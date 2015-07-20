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

-- import qualified Language.C.Syntax as C
import Language.C.Quote.C

-- import Control.Monad.Identity
import Control.Monad ( unless, void )
import Data.Loc
import Text.PrettyPrint.HughesPJ
-- import qualified GenSym as GS
import Data.List ( nub )
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
      = cg_bound xs (cgMutBind dfs noLoc x Nothing m)

    is_external (MkFun (MkFunExternal {}) _ _) = True
    is_external _ = False


lblOfNid :: Int -> CLabel
lblOfNid x = "BLOCK_" ++ show x


mkCastTyArray :: Int -> Ty -> Ty
mkCastTyArray 1 t = t
mkCastTyArray n (TArray (Literal m) t) = TArray (Literal (m*n)) t
mkCastTyArray n t                      = TArray (Literal n) t

-- | Datatype with queue information of this automaton 
data QueueInfo 
  = QI { qi_inch   :: EId                   -- ^ THE Input queue 
       , qi_outch  :: EId                   -- ^ THE Output queue
       , qi_interm :: Map.Map EId (QId,Int) -- ^ Intermediate queues, their ids and max sizes
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
    in if numqs == 0 then action else do 
        let my_sizes_decl = [cdecl|typename size_t my_sizes[$int:(numqs)];|]
            my_slots_decl = [cdecl|int my_slots[$int:(numqs)];|]
            ts_init_stmts = [cstm| ts_init_var($int:(numqs),my_sizes,my_slots); |]
            my_sizes_inits 
              = concat $
                Map.elems $ 
                Map.mapWithKey (\qvar (QId i,siz) -- ignore slots
                       -> [ [cstm| my_sizes[$int:i] = 
                                   $(tySizeOf_C (nameTyp qvar));|]
                          , [cstm| my_slots[$int:i] = $int:siz;|]
                          ]) (qi_interm qs)

           -- Append top declarations  
        appendTopDecl my_sizes_decl
        appendTopDecl my_slots_decl

           -- Add code to initialize queues in wpl global init
        _ <- mapM addGlobalWplAllocated (my_sizes_inits ++ [ts_init_stmts])

        bind_queue_vars action (qi_inch qs : qi_outch qs : Map.keys (qi_interm qs))

  where bind_queue_vars m [] = m
        bind_queue_vars m (q1:qss)
          = cgMutBind dfs noLoc q1 Nothing (bind_queue_vars m qss)


-- | Extract all introduced queues, and for each calculate the maximum
-- size we should allocate it with.
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


cgAutomaton :: DynFlags 
            -> RnSt                 -- ^ Records all variables (but no queues)
            -> CfgAuto SymAtom Int  -- ^ The automaton
            -> Cg CLabel            -- ^ Label of starting state we jump to
cgAutomaton dfs st auto@(Automaton { auto_graph   = graph
                                   , auto_start   = start })
  = do { cgRnSt dfs st $
         cgDeclQueues dfs queues $
         cg_define_atoms $
         cg_automaton
       ; return (lblOfNid start) }
  where 
   queues = extractQueues auto

   cg_automaton             = mapM_ cg_node (Map.elems graph)
   cg_node (Node nid nk)    = appendLabeledBlock (lblOfNid nid) (cg_nkind nk)
   cg_nkind CfgDone         = appendStmt [cstm| return 0; |]
   cg_nkind (CfgLoop next)  = appendStmt [cstm| goto $id:(lblOfNid next);|]
   cg_nkind (CfgBranch c l r _) = do
     cc <- lookupVarEnv c
     appendStmt [cstm| if ($cc) { goto $id:(lblOfNid l); } 
                       else { goto $id:(lblOfNid r); } |]
   cg_nkind (CfgAction atoms next _pipes) = do 
     inAllocFrame noLoc $ pushAllocFrame $ mapM_ (cgAtom dfs queues) atoms
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
   cg_def_atoms' (a:as) m = cgDefAtom dfs a (cg_def_atoms' as m)


cgDefAtom :: DynFlags
          -> WiredAtom SymAtom
          -> Cg a
          -> Cg a
cgDefAtom dfs (WiredAtom win wout the_atom) m
  = case the_atom of
      SAAssert {}   -> m
      SACast {}     -> m
      SADiscard {}  -> m
      SARollback {} -> m
      SAExp aexp    -> cgAExpDefAtom dfs win wout aexp m

-- | Code generation for an atom
cgAtom :: DynFlags
       -> QueueInfo         -- ^ Queues of this program
       -> WiredAtom SymAtom -- ^ The wired atom
       -> Cg ()
cgAtom dfs qnfo (WiredAtom win wout the_atom) 
  = case the_atom of
      SAAssert b -> 
        assert "cgAtom/Assert" (singleton win) $
        cgAssertAtom dfs qnfo (head win) b

      SAExp aexp -> 
        cgAExpAtom dfs qnfo win wout aexp
      SACast intty outty  -> 
        assert "cgAtom/Cast" (singleton win)  $
        assert "cgAtom/Cast" (singleton wout) $
        cgCastAtom dfs qnfo (head win) (head wout) intty outty
      SADiscard inty -> 
        assert "cgAtom/Cast" (singleton win) $
        assert "cgAtom/Discard" (null wout)  $
        cgDiscAtom dfs qnfo (head win) inty
      SARollback _queue _n ->
        fail "NOT IMPLEMENTED!!"

  where singleton [_] = True
        singleton _   = False
        

cgAssertAtom :: DynFlags
             -> QueueInfo
             -> (Int,EId)
             -> Bool
             -> Cg ()
cgAssertAtom dfs qs (_,inwire) b = do 
      let loc = nameLoc inwire

      storage <- CgMonad.freshName "assert_storage" TBool Mut

      cgMutBind dfs noLoc storage Nothing $ do 

         cgInWiring dfs qs [(1,inwire)] [storage]
         
         let err   = eError loc TUnit "Atomix dead code assertion violation!"
             vunit = eVal loc TUnit VUnit

         _ <- codeGenExp dfs $ 
              if b then eIf loc (eUnOp loc Neg (eVar loc storage)) err vunit
                   else eIf loc (eVar loc storage) err vunit

         return ();

cgCastAtom :: DynFlags 
           -> QueueInfo
           -> (Int,EId) -- ^ inwire
           -> (Int,EId) -- ^ outwire
           -> (Int,Ty)  -- ^ inty
           -> (Int,Ty)  -- ^ outty
           -> Cg ()
cgCastAtom dfs qs (n,inwire) 
                  (m,outwire) 
                  (n',inty) 
                  (m',_outty)
  = assert "cgCastAtom" (n == n' && m == m') $ do 
    case (qiQueueId inwire qs, qiQueueId outwire qs) of 
      (Nothing, _) -> assert "cgCastAtom" (n == 1) $
                      cgOutWiring dfs qs [(m,outwire)] [inwire]
      (_,Nothing)  -> assert "cgCastAtom" (m == 1) $
                      cgInWiring dfs qs [(n,inwire)] [outwire]
      _ -> let storage_in_ty  = mkCastTyArray n inty 
           in do storage <- CgMonad.freshName "cast_storage" storage_in_ty Mut
                 cgMutBind dfs noLoc storage Nothing $ do 
                    cgInWiring dfs qs [(n,inwire) ] [storage]
                    cgOutWiring dfs qs [(m,outwire)] [storage]

cgDiscAtom :: DynFlags
           -> QueueInfo
           -> (Int,EId) -- ^ inwire
           -> (Int,Ty)  -- ^ inty
           -> Cg ()
cgDiscAtom dfs qs (n,inwire) (n',inty) 
  = assert "cgDiscAtom" (n == n') $ do
 
      let storage_in_ty = mkCastTyArray n inty
      storage <- CgMonad.freshName "disc_storage" storage_in_ty Mut

      cgMutBind dfs noLoc storage Nothing $
         cgInWiring dfs qs [(n,inwire)] [storage]


mkAExpFunName :: [EId] -> [EId] -> AExp () -> (EId,[EId])
mkAExpFunName wires_in wires_out aexp = (fun_name, params)
  where 
    loc  = expLoc (aexp_exp aexp)
    input_only x = (x `elem` wires_in) && not (x `elem` wires_out)
    args   = nub (wires_in ++ wires_out)
    params = map (\x -> if input_only x then x { nameMut = Imm } 
                        else x { nameMut = Mut }) args
    argtys = map (\x -> GArgTy (nameTyp x) 
                               (if isMutable x then Mut else Imm)) params

    resty     = TUnit
    funty  = TArrow argtys resty
    fun_name = toName (alphaNumStr $ aexp_lbl aexp) loc funty Imm

mkAExpFunDef :: [EId] -> [EId] -> AExp () -> Fun
mkAExpFunDef wires_in wires_out aexp = mkFunDefined loc fun_name params fun_body
  where 
    (fun_name,params) = mkAExpFunName wires_in wires_out aexp
    loc      = expLoc fun_body
    fun_body = aexp_exp aexp

cgAExpDefAtom :: DynFlags 
              -> [(Int,EId)] -- ^ in wires
              -> [(Int,EId)] -- ^ out wires
              -> AExp ()
              -> Cg a
              -> Cg a
cgAExpDefAtom dfs wins wouts aexp m
  = do mb <- lookupExpFunEnv_maybe fun_name
       case mb of Just {} -> m
                  Nothing -> cgFunDefined_clos dfs loc fun [] (dbg_m >> m)
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
    fun_name = funName fun
    fun = mkAExpFunDef wires_in wires_out aexp

cgAExpAtom :: DynFlags
           -> QueueInfo
           -> [(Int,EId)] -- ^ in wires
           -> [(Int,EId)] -- ^ out wires
           -> AExp ()
           -> Cg ()
cgAExpAtom dfs qs wins wouts aexp 
  = assert "cgAExpAtom" (all (\x -> fst x == 1) wins)  $
    assert "cgAExpAtom" (all (\x -> fst x == 1) wouts) $ 
    do  cgInWiring dfs qs wins (map snd wins) 

        _ccall <- codeGenExp dfs call

        -- Can safely always discard _ccal because it is going to be UNIT, 
        -- see CgCall.hs 

        cgOutWiring dfs qs wouts (map snd wouts)

  where
    loc  = expLoc body
    body = aexp_exp aexp
    -- input wires
    wires_in  = map snd wins
    -- output wires
    wires_out = map snd wouts

    fun_name = fst $ mkAExpFunName wires_in wires_out aexp
    args = nub (wires_in ++ wires_out)
    call = eCall loc fun_name (map (eVar loc) args)


calcTsLen :: Int -> Ty -> Int
calcTsLen 1 TBit = 1
calcTsLen n TBit = (n+7) `div` 8
calcTsLen n (TArray (Literal m) TBit)
  | m `mod` 8 == 0
  = (n * (m `div` 8))
  | otherwise 
  = error $ "Unaligned ts_get/put: not implemented yet, n =" ++ show n ++ ", m =" ++ show m
calcTsLen n _ = n

          
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
    Just (QMid (QId qid)) -> do
      cv <- lookupVarEnv v
      let realn = calcTsLen n (nameTyp qvar)
      let ptr = if isPtrType (nameTyp v)
                then [cexp| (char *) $cv|] else [cexp| (char *) & $cv|]
      appendStmt [cstm| if (ts_getManyBlocking($int:qid,$int:realn,$ptr) != $int:realn) 
                          return 3; |]

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
                                   $ptr, $(getLen)) == GS_EOF) return 3; |]
        else appendStmt [cstm| if ($id:(bufGetF)($id:global_params,
                                   $id:buf_context,
                                   $ptr) == GS_EOF) return 3; |]


squashedQueueType :: Int -> Ty -> Ty
-- | Squash a queue type to expose an array of base elements
squashedQueueType 1 t = t
squashedQueueType n (TArray (Literal m) bt) = TArray (Literal (n*m)) bt
squashedQueueType n t = TArray (Literal n) t


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
      Just (QMid (QId qid)) -> do
        cv <- lookupVarEnv v
        let realn = calcTsLen n (nameTyp qvar)
        let ptr = if isPtrType (nameTyp v)
                  then [cexp| (char *) $cv|] else [cexp| (char *) & $cv|]
        appendStmt [cstm| ts_putMany($int:qid,$int:realn,$ptr);|]

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
        
