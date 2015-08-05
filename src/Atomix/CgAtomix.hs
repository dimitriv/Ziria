{-
   Copyright (c) Microsoft Corporation
   All rights reserved.

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -Werror #-}
module CgAtomix where


import AstName
import AstExpr
-- import AstComp
import AstUnlabelled
import Utils
-- import Control.Applicative ( (<$>) )
import Outputable 
-- import Data.Maybe ( fromJust, isJust )
-- import qualified Data.Set as S

import Data.Map.Strict ( Map ) 
import qualified Data.Map.Strict as Map

import qualified Language.C.Syntax as C
import Language.C.Quote.C

-- import Control.Monad.Identity
import Control.Monad ( unless, void, when )
import Data.Loc
import Text.PrettyPrint.HughesPJ
-- import qualified GenSym as GS
-- import Data.List ( nub )
-- import CtExpr 
-- import CtComp
-- import TcRename 
import AtomComp

-- import Analysis.DataFlow 
import Opts
import CgMonad
import CgTypes
import CgFun
import CgExpr
-- import CgCmdDom

import AtomixCompTransform 
import AutomataModel
import AtomInstantiation


cgRnSt :: DynFlags -> RnSt -> Cg a -> Cg a
cgRnSt dfs (RnSt { st_bound_vars = bound
                 , st_fundefs    = fdefs
                 , st_structs    = sdefs }) action
  = cg_sdefs sdefs $
    cg_fdefs fdefs $
    cg_bound bound $ action
  where
    cg_sdefs [] m = m
    cg_sdefs ((_,sd):sds) m
      = cg_sdefs sds (cgStructDef sd m)
    cg_fdefs [] m = m
    cg_fdefs ((_,(fun,clos)):fds) m
      | is_external fun
      = cg_fdefs fds (cgFunExternal dfs (funLoc fun) fun m)
      | otherwise
      = cg_fdefs fds (cgFunDefined_clos dfs (funLoc fun) fun clos m)
    cg_bound [] m = m
    cg_bound (x:xs) m
      = cg_bound xs (cgGlobMutBind dfs noLoc x m)

    is_external (MkFun (MkFunExternal {}) _ _) = True
    is_external _ = False


lblOfNid :: Int -> CLabel
lblOfNid x = "BLOCK_" ++ show x

wiredAtomNameOfLbl :: String -> SrcLoc -> Ty -> EId
wiredAtomNameOfLbl str loc ty = toName (alphaNumStr str) loc ty Imm


mkCastTyArray :: Int -> Ty -> Ty
mkCastTyArray 1 t = t
mkCastTyArray n (TArray (Literal m) t) = TArray (Literal (m*n)) t
mkCastTyArray n t                      = TArray (Literal n) t

-- | Datatype with queue information of this automaton 
data QueueInfo 
  = QI { qi_inch   :: EId                   -- ^ THE Input queue 
       , qi_outch  :: EId                   -- ^ THE Output queue
       , qi_interm :: Map.Map EId (QId,Int) -- ^ Intermediate queues, 
                                            -- ^ their ids and max sizes
       }

newtype QId = QId { unQId :: Int }
data QueueId = QIn | QOut | QMid QId

qiQueueId :: EId -> QueueInfo -> Maybe QueueId
-- | Return Just the QueueId, if this variable is a queue; Nothing otherwise.
qiQueueId x qs | x == qi_inch qs  = Just QIn
qiQueueId x qs | x == qi_outch qs = Just QOut
qiQueueId x qs | Just (qid,_) <- Map.lookup x (qi_interm qs) = Just (QMid qid)
qiQueueId _ _ = Nothing


-- | Declare queues 
cgDeclQueues :: DynFlags -> QueueInfo -> Cg a -> Cg a
cgDeclQueues dfs qs action 
  = let numqs = Map.size (qi_interm qs)
    in if numqs == 0 then do 
           bind_queue_vars action (qi_inch qs : qi_outch qs : []) 
        else do 
          let my_sizes_decl = [cdecl|typename size_t my_sizes[$int:(numqs)];|]
              my_slots_decl = [cdecl|int my_slots[$int:(numqs)];|]
              q_init_stmts 
                = [cstm| stq_init($int:(numqs),my_sizes,my_slots); |]
             
              my_sizes_inits 
                = concat $
                  Map.elems $ 
                  Map.mapWithKey (\qvar (QId i,siz) -- ignore slots
                         -> let orig = "qvar type = " ++ show (nameTyp qvar)
                            in [ [cstm| ORIGIN($string:orig);|]
                               , [cstm|
                                  my_sizes[$int:i] = 
                                        $(tySizeOf_C (nameTyp qvar));|]
                               , [cstm| my_slots[$int:i] = $int:siz;|]
                               ]) (qi_interm qs)

             -- Append top declarations  
          appendTopDecl my_sizes_decl
          appendTopDecl my_slots_decl

             -- Add code to initialize queues in wpl global init
          _ <- mapM addGlobalWplAllocated (my_sizes_inits ++ [q_init_stmts])

          bind_queue_vars action $ 
              (qi_inch qs : qi_outch qs : Map.keys (qi_interm qs))

  where
    bind_queue_vars m [] = m
    bind_queue_vars m (q1:qss)
      = do cgIO $ putStrLn ("Declaring temp variable for queue: " ++ show q1)
           cgGlobMutBind dfs noLoc q1 (bind_queue_vars m qss)

-- | Extract all introduced queues, and for each calculate the maximum
-- ^ size we should allocate it with.
extractQueues :: CfgAuto SymAtom Int -> QueueInfo
extractQueues auto 
  = QI { qi_inch   = auto_inchan auto
       , qi_outch  = auto_outchan auto
       , qi_interm = interms
       }
  where
    interms = Map.fromList $ 
               zipWith (\(k,siz) n -> (k,(QId n,siz))) 
                       (Map.toList (unions_with max pre)) [0..]

    unions_with f = foldl (Map.unionWith f) Map.empty

    pre = map (extract_queue . node_kind) (Map.elems (auto_graph auto))
    extract_queue (CfgAction atoms _ init_pipes) = update_pipes atoms init_pipes
    extract_queue _ = Map.empty

    update_pipes :: [WiredAtom SymAtom] -> Map Chan Int -> Map Chan Int
    update_pipes watoms pipes 
      = Map.map snd $ foldl upd (Map.map (\r -> (r,r)) pipes) watoms
      where upd ps wa = 
               Map.mapWithKey (\q (cur,m) -> 
                   let cur' = cur + countWrites q wa - countReads q wa
                       m'   = max m cur'
                   in (cur',m')) ps

-- | Main driver for code generation
cgAutomaton :: DynFlags 
            -> RnSt                 -- ^ Records all variables (but no queues)
            -> CfgAuto SymAtom Int  -- ^ The automaton
            -> Cg CLabel            -- ^ Label of starting state we jump to
cgAutomaton dfs st auto@(Automaton { auto_graph   = graph
                                   , auto_start   = start })
  = do { cgRnSt dfs st $ cgDeclQueues dfs queues (cg_define_atoms cg_automaton)
       ; return (lblOfNid start) }
  where 
   queues = extractQueues auto

   cg_automaton = do 
      frameVar <- freshVar "mem_idx"
      appendDecl [cdecl| unsigned int $id:frameVar; |]
      mapM_ (cg_node frameVar) (Map.elems graph)

   cg_node frameVar (Node nid nk) = appendLabeledBlockNoScope (lblOfNid nid) (cg_nkind frameVar nk)
   cg_nkind _ CfgDone        = appendStmt [cstm| exit(0); |]
   cg_nkind _ (CfgLoop next) = appendStmt [cstm| goto $id:(lblOfNid next);|]
   cg_nkind _ (CfgBranch c l r _) = do
     cc <- lookupVarEnv c
     appendStmt 
        [cstm| if ($cc) goto $id:(lblOfNid l); else goto $id:(lblOfNid r); |]
   cg_nkind frameVar (CfgAction atoms next _pipes) = do 
     inAllocFrame' frameVar noLoc $ pushAllocFrame $ mapM_ (cgCallAtom dfs) atoms
     appendStmt [cstm| goto $id:(lblOfNid next);|]


   cg_define_atoms       = cg_def_atoms (Map.elems graph)
   cg_def_atoms [] m     = m
   cg_def_atoms (n:ns) m = cg_def_node n (cg_def_atoms ns m)
   cg_def_node (Node _nid nk)    = cg_def_nkind nk
   cg_def_nkind CfgDone m        = m
   cg_def_nkind (CfgLoop _n) m   = m
   cg_def_nkind (CfgBranch {}) m = m
   cg_def_nkind (CfgAction atoms _ _) m = cg_def_atoms' atoms m 

   cg_def_atoms' [] m = m
   cg_def_atoms' (a:as) m = cgDefAtom dfs queues a (cg_def_atoms' as m)

{---------------------- Defining and calling an atom -------------------------}

cgDefAtom :: DynFlags
          -> QueueInfo
          -> WiredAtom SymAtom
          -> Cg a
          -> Cg a
cgDefAtom dfs qs w@(WiredAtom win wout the_atom)
  = case the_atom of
      SACast _ inty outty ->
        let inwire  = head win
            outwire = head wout
        in cg_def_atom dfs wid $ cgACastBody dfs qs inwire outwire inty outty

      SADiscard _ inty 
        -> cg_def_atom dfs wid $ cgADiscBody dfs qs (head win) inty

      SAExp aexp 
        -> cg_def_atom dfs wid $ cgAExpBody dfs qs win wout aexp

      SARollback qvar n 
        -> cg_def_atom dfs wid $ cgARollbackBody dfs qs qvar n

      SAClear qsns      
        -> cg_def_atom dfs wid $ cgAClearBody dfs qs qsns

      SAAssert b 
        -> cg_def_atom dfs wid $ cgAAssertBody dfs qs (head win) b

  where wid = wiredAtomId w

cg_def_atom :: DynFlags 
            -> String   -- atom identifier
            -> Cg C.Exp -- Body 
            -> Cg a     -- Continuation
            -> Cg a
cg_def_atom _dfs aid mbody mkont
  = do mb <- lookupExpFunEnv_maybe fun_name
       case mb of { Just {} -> mkont; Nothing -> cg_define_atom }
  where
    fun_name = wiredAtomNameOfLbl aid noLoc (TArrow [] TUnit)
    cg_define_atom = do 
      (cdecls,cstmts,cbody) <- inNewBlock (pushAllocFrame mbody)

      -- Prototype
      appendTopDecl [cdecl| $ty:(codeGenTyOcc TUnit) $id:(name fun_name)();|]

      -- Implementation 
      appendTopDef $
        [cedecl| $ty:(codeGenTyOcc TUnit)
              $id:(name fun_name)() {
                      $decls:cdecls    // Define things needed for body
                      $stms:cstmts     // Emit rest of body
                      return $(cbody);                          
             } |]

      -- Extend environment and go on
      extendExpFunEnv fun_name (fun_name, [], False) mkont


cgCallAtom :: DynFlags
           -> WiredAtom SymAtom 
           -> Cg ()
cgCallAtom dfs w = codeGenExp dfs call >>= \_ -> return ()
     -- Can safely always discard call result 
     -- because it is going to be UNIT, see CgCall.hs 
  where
    fun_name = wiredAtomNameOfLbl aid noLoc (TArrow [] TUnit)
    call     = eCall noLoc fun_name []
    aid      = wiredAtomId w



{---------------------------- Assert atom implementation ----------------------}

-- NB: In the future we can define this one too. 
cgAAssertBody :: DynFlags
              -> QueueInfo
              -> (Int,EId)
              -> Bool
              -> Cg C.Exp
cgAAssertBody dfs qs (_,inwire) b = do 
      let loc = nameLoc inwire
      storage <- CgMonad.freshName "assert_storage" TBool Mut
      cgMutBind dfs noLoc storage Nothing $ do
         cgInWiring dfs qs [(1,inwire)] [storage]
         let err   = eError loc TUnit "Atomix dead code assertion violation!"
             vunit = eVal loc TUnit VUnit
         _ <- codeGenExp dfs $ 
              if b then eIf loc (eUnOp loc Not (eVar loc storage)) err vunit
                   else eIf loc (eVar loc storage) err vunit
         return [cexp|UNIT|]


{---------------------------- Rollback atom implementation --------------------}

cgARollbackBody :: DynFlags
                -> QueueInfo
                -> EId
                -> Int
                -> Cg C.Exp
cgARollbackBody _dfs _qs x n =
   panic $ vcat [ text "SARollback not implemented yet"
                , nest 2 $ text "queue = " <+> ppr x
                , nest 2 $ text "n     = " <+> ppr n 
                ]

{------------------------------- Cast atom implementation ---------------------}

cgACastBody :: DynFlags 
            -> QueueInfo
            -> (Int,EId) -- ^ inwire
            -> (Int,EId) -- ^ outwire
            -> (Int,Ty)  -- ^ inty
            -> (Int,Ty)  -- ^ outty
            -> Cg C.Exp
cgACastBody dfs qs (n,inwire) 
                      (m,outwire) 
                      (n',inty) 
                      (m',_outty) 
  = assert "cgCastAtom" (n == n' && m == m') $ do
    case (qiQueueId inwire qs, qiQueueId outwire qs) of 
      (Nothing, _) -> assert "cgCastAtom" (n == 1) $ do 
                      cgOutWiring dfs qs [(m,outwire)] [inwire]
                      return [cexp|UNIT|]
      (_,Nothing)  -> assert "cgCastAtom" (m == 1) $ do 
                      cgInWiring dfs qs [(n,inwire)] [outwire]
                      return [cexp|UNIT|]
      _ -> let storage_in_ty  = mkCastTyArray n inty 
           in do storage <- CgMonad.freshName "cast_storage" storage_in_ty Mut
                 cgMutBind dfs noLoc storage Nothing $ do 
                    cgInWiring dfs qs [(n,inwire) ] [storage]
                    cgOutWiring dfs qs [(m,outwire)] [storage]
                    return [cexp|UNIT|]

{---------------------- Discard/Clear atom implementation ---------------------}

cgADiscBody :: DynFlags
            -> QueueInfo
            -> (Int,EId) -- ^ inwire
            -> (Int,Ty)  -- ^ inty
            -> Cg C.Exp
cgADiscBody dfs qs (n,inwire) (n',inty) 
  = assert "cgDiscAtom" (n == n') $ do
 
      let storage_in_ty = mkCastTyArray n inty
      storage <- CgMonad.freshName "disc_storage" storage_in_ty Mut

      cgMutBind dfs noLoc storage Nothing $ do 
         cgInWiring dfs qs [(n,inwire)] [storage]
         return [cexp|UNIT|]

cgAClearBody :: DynFlags 
             -> QueueInfo
             -> Map EId Int
             -> Cg C.Exp
cgAClearBody dfs qs qmap = do 
  -- For now implement clear via discard
  mapM_ (\(q,n) -> cgADiscBody dfs qs (n,q) (n,nameTyp q)) (Map.toList qmap)
  return [cexp|UNIT|]

{--------------------------- AExp atom implementation -------------------------}

cgAExpBody :: DynFlags 
           -> QueueInfo
           -> [(Int,EId)] -- ^ in wires
           -> [(Int,EId)] -- ^ out wires
           -> AExp ()
           -> Cg C.Exp
cgAExpBody dfs qs wins wouts aexp
  = when (isDynFlagSet dfs Verbose) dbg_m >> 
    do { cgInWiring dfs qs wins wires_in       -- wiring 
       ; b <- codeGenExp dfs fun_body          -- body
       ; cgOutWiring dfs qs wouts  wires_out   -- wiring
       ; return b 
       }
  where 
    dbg_m = cgIO $ print $ vcat [ text "cgAExpAtom:" <+> ppr fun_name
                                , text "ivs       :" <+> ppr wires_in
                                , text "ovs       :" <+> ppr wires_out
                                , text "fun_ty    :" <+> ppr (nameTyp fun_name)
                                , text "body was  :" <+> ppr (aexp_exp aexp)
                                ]
    loc  = expLoc (aexp_exp aexp)
    -- input wires
    wires_in  = map snd wins
    -- output wires
    wires_out = map snd wouts
    fun_name  = wiredAtomNameOfLbl (aexp_lbl aexp) loc (TArrow [] TUnit)
    fun_body  = aexp_exp aexp   


{- Wiring and infrastructure
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}



{--------------- Reading from TS queues ---------------------------------------}

q_read_n :: QId -> Int -> C.Exp -> C.Stm
q_read_n (QId qid) n ptr
  = [cstm| stq_getMany($int:qid,$int:n,$ptr); |]

q_read_n_bits :: QId -> Int -> C.Exp -> C.Stm
q_read_n_bits (QId qid) n ptr
  = [cstm| stq_getManyBits($int:qid,$int:n,$ptr); |]


readFromTs :: DynFlags 
           -> Int -> Ty -- # number of reads, and type of each read
           -> QId       -- Queue id
           -> C.Exp     -- Target variable to store to
           -> Cg ()
readFromTs _ n TBit q ptr = appendStmt $ q_read_n_bits q n ptr

readFromTs dfs n (TArray (Literal m) TBit) q@(QId qid) ptr
  | m `mod` 8 == 0 = appendStmt $ q_read_n q n ptr
  | n == 1         = appendStmt $ q_read_n q 1 ptr
  | otherwise -- Not byte-aligned byte array, a bit sad and slow
  = do x <- CgMonad.freshName "q_rdx" (TArray (Literal m) TBit) Mut
       i <- CgMonad.freshName "q_idx" tint Mut
       cgMutBind dfs noLoc i Nothing $ do 
       cgMutBind dfs noLoc x Nothing $ do
         cx <- lookupVarEnv x
         ci <- lookupVarEnv i
         appendStmt $ [cstm| for ($ci = 0; $ci < $int:n; $ci++) {
                                stq_get($int:qid,(char *) $cx);
                                bitArrWrite((typename BitArrPtr) $cx,
                                              $ci*$int:m,
                                              $int:m,
                                            (typename BitArrPtr) $ptr);
                             } |]
readFromTs _ n _ q ptr = appendStmt $ q_read_n q n ptr


          
cgInWiring :: DynFlags 
           -> QueueInfo 
           -> [(Int,EId)] -- Input wires
           -> [EId]       -- Where to store things
           -> Cg ()
cgInWiring dfs qs ws vs = mapM_ (uncurry (cgInWire dfs qs)) (zip ws vs)

cgInWire :: DynFlags -> QueueInfo -> (Int,EId) -> EId -> Cg ()
cgInWire dfs qs (n,qvar) v = 
  case qiQueueId qvar qs of

    -- Not a queue
    Nothing 
       -> assert "cgInWire/non-queue" (n == 1) $
          unless (qvar == v) $
          void $ codeGenExp dfs $
                 eAssign noLoc (eVar noLoc v) (eVar noLoc qvar)
        
    -- Some intermediate SORA queue
    Just (QMid q) -> do
      cv <- lookupVarEnv v
      let ptr = if isPtrType (nameTyp v) 
                then [cexp| (char *) $cv|] else [cexp| (char *) & $cv|]
      readFromTs dfs n (nameTyp qvar) q ptr

    -- Unexpected output queue
    Just QOut -> panicStr "cg_in_wire: encountered output queue!"


    -- TODO: the main input queue
    Just QIn -> do
      let qtype = squashedQueueType n (nameTyp qvar)
      
      let (bufget_suffix, getLen) = getTyPutGetInfo qtype
      let bufGetF = "buf_get" ++ bufget_suffix

      buf_context  <- getBufContext
      global_params <- getGlobalParams

      cv <- lookupVarEnv v
      let ptr = if isPtrType (nameTyp v) then [cexp| $cv|] else [cexp| & $cv|]
      if isArrayTy qtype 
        then appendStmt [cstm| if ($id:(bufGetF)($id:global_params,
                                   $id:buf_context,
                                   $ptr, $(getLen)) == GS_EOF) exit(0); |]
        else appendStmt [cstm| if ($id:(bufGetF)($id:global_params,
                                   $id:buf_context,
                                   $ptr) == GS_EOF) exit(0); |]


squashedQueueType :: Int -> Ty -> Ty
-- | Squash a queue type to expose an array of base elements
squashedQueueType 1 t = t
squashedQueueType n (TArray (Literal m) bt) = TArray (Literal (n*m)) bt
squashedQueueType n t = TArray (Literal n) t



{--------------- Writing to TS queues -----------------------------------------}

q_write_n :: QId -> Int -> C.Exp -> C.Stm
q_write_n (QId qid) n ptr = [cstm| stq_putMany($int:qid,$int:n,$ptr); |]

q_write_n_bits :: QId -> Int -> C.Exp -> C.Stm
q_write_n_bits (QId qid) n ptr = [cstm| stq_putManyBits($int:qid,$int:n,$ptr); |]


writeToTs :: DynFlags 
           -> Int -> Ty -- # number of writes, and type of each write
           -> QId       -- Queue id
           -> C.Exp     -- Source variable to read from
           -> Cg ()
writeToTs _ n TBit q ptr = appendStmt $ q_write_n_bits q n ptr
writeToTs dfs n (TArray (Literal m) TBit) q@(QId qid) ptr
  | m `mod` 8 == 0 = appendStmt $ q_write_n q n ptr
  | n == 1 = appendStmt $ q_write_n q 1 ptr
  | otherwise -- Not byte-aligned byte array, a bit sad and slow
  = do x <- CgMonad.freshName "q_rdx" (TArray (Literal m) TBit) Mut 
       i <- CgMonad.freshName "q_idx" tint Mut
       cgMutBind dfs noLoc i Nothing $ do 
       cgMutBind dfs noLoc x Nothing $ do
         cx <- lookupVarEnv x
         ci <- lookupVarEnv i
         appendStmt $ [cstm| for ($ci = 0; $ci < $int:n; $ci++) {
                                bitArrRead((typename BitArrPtr) $ptr,
                                            $ci*$int:m,
                                            $int:m,
                                           (typename BitArrPtr) $cx);
                                stq_put($int:qid,(char *) $cx);
                             }|]

writeToTs _ n _ q ptr = appendStmt $ q_write_n q n ptr


cgOutWiring :: DynFlags 
            -> QueueInfo
            -> [(Int,EId)] -- Output wires
            -> [EId]       -- Where to read from
            -> Cg ()
cgOutWiring dfs qs ws vs = mapM_ (uncurry (cgOutWire dfs qs)) (zip ws vs)


cgOutWire :: DynFlags -> QueueInfo -> (Int,EId) -> EId -> Cg ()
cgOutWire dfs qs (n,qvar) v
  = case qiQueueId qvar qs of

      -- Not a queue
      Nothing ->
         assert "cgOutWire/non-queue" (n == 1) $
         unless (qvar == v) $
         void $ codeGenExp dfs (eAssign noLoc (eVar noLoc qvar) (eVar noLoc v))

      -- Some intermediate SORA queue
      Just (QMid q) -> do
        cv <- lookupVarEnv v
        let ptr = if isPtrType (nameTyp v)
                  then [cexp| (char *) $cv|] else [cexp| (char *) & $cv|]
        writeToTs dfs n (nameTyp qvar) q ptr

      -- Unexpected input queue
      Just QIn -> panicStr "cg_out_wire: encountered the input queue!"

      -- The actual output queue
      Just QOut -> do

        let qtype = squashedQueueType n (nameTyp qvar)

        cgIO $ print $ vcat [ text "Out-Wiring:"
                            , text "Queue type:" <+> ppr (nameTyp qvar)
                            , text "  Squashed:" <+> ppr qtype
                            , text "  Var type:" <+> ppr (nameTyp v) ]


        let (bufput_suffix, putLen) = getTyPutGetInfo qtype
        let bufPutF = "buf_put" ++ bufput_suffix

        buf_context   <- getBufContext
        global_params <- getGlobalParams

        cv <- lookupVarEnv v
        let ptr = if isPtrType (nameTyp v)
                  then [cexp| $cv|] else [cexp| & $cv|]

        if isArrayTy qtype 
          then 
            appendStmt [cstm| $id:(bufPutF)($id:global_params,
                                            $id:buf_context,
                                            $ptr, $(putLen)); |]
          else
            appendStmt [cstm| $id:(bufPutF)($id:global_params,
                                            $id:buf_context,
                                            *$ptr); |]
        
