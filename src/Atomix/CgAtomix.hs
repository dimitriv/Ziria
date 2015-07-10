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
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -Werror #-}
module CgAtomix where


import AstExpr
import AstComp
import AstUnlabelled
import Utils
import Control.Applicative ( (<$>) )
import Outputable 
import Data.Maybe ( fromJust, isJust )
import qualified Data.Set as S
import Control.Monad.Identity
import Control.Monad.State
import Data.Loc
import Text.PrettyPrint.HughesPJ
import qualified GenSym as GS
import Data.List ( nub )
import CtExpr 
import CtComp
import TcRename 
import AtomComp

import Analysis.DataFlow 

import Opts

import CgMonad

type CLabel = String


cgRnSt :: RnStr -> Cg a -> Cg a 
cgRnSt = error "foo"


lblOfNid :: Int -> CLabel
lblOfNid x = "BLOCK_" ++ show x

-- | Datatype with queue information of this automaton 
data QueueInfo 
  = QI { qi_inch   :: EId          -- ^ Input queue 
       , qi_outch  :: EId          -- ^ Output queue
       , qi_interm :: Map EId Int  -- ^ Intermediate queues and max sizes
       }

-- | Declare queues 
cgDeclQueues :: QueueInfo -> Cg a -> Cg a
cgDeclQueues = error "foo" 

-- | Extract all introduced queues, and for each calculate the maximum
-- size we should allocate it with.
extractQueues :: Automaton SymAtom Int -> QueueInfo
extractQueues auto 
  = QI { qi_inchan  = auto_inchan auto
       , qi_outchan = auto_outchan auto
       , qi_interm  = unionsWith max pre
       }
  where pre = map extract_queue (Map.elems (auto_graph auto))
        extract_queue (Action atoms _ init_pipes) = update_pipes atoms init_pipes
        extract_queue _ = Map.empty

        update_pipes :: [WiredAtom SymAtom] -> Map Chan Int -> Map Chan Int
        update_pipes watoms pipes 
          = Map.map snd $ foldl upd_one (Map.map (\r -> (r,r)) pipes) watoms
          where upd ps wa = 
                   Map.mapWithKey (\q (cur,m) -> 
                       let cur' = cur + countWrite q wa - countRead q wa
                           m'   = max m cur'
                       in (cur',m')) ps


cgAutomaton :: DynFlags 
            -> RnSt                   -- ^ Records all variables (but no queues)
            -> Automaton SymAtom Int  -- ^ The automaton
            -> Cg CLabel              -- ^ Label of starting state we jump to
cgAutomaton dfs st auto@(Automaton { auto_graph   = graph
                                   , auto_inchan  = inch
                                   , auto_outchan = outch 
                                   , auto_start   = start })
  = do cgRnSt st $ cgDeclareQueues queues $ cg_automaton
       return (lblOfNid start)
  where 
   queues = extractQueues auto

   cg_automaton = Map.map cg_node graph
   cg_node (Node nid nk) = appendLabeledBlock (lblOfNid nid) (cg_nkind nk)
   cg_nkind Done         = appendStmt [cstm| return 0; |]
   cg_nkind (Loop next)  = appendStmt [cstm| goto $id:(lblOfNid next);|]
   cg_nkind (Branch c l r _) = do
     cc <- lookupVarEnv c
     appendStmt [cstm| if ($cc) goto $id:(lblOfNid l) else goto $id:(lblOfNid r);|]

   cg_nkind (Action atoms next pipes) = do 
     let qs = Map.keys pipes
     mapM (cgAtom dfs queues) atoms
     appendStmt [cstm| goto $id:(lblOfNid next);|]


-- | Code generation for an atom
cgAtom :: DynFlags
       -> QueueInfo         -- ^ Queues of this program
       -> WiredAtom SymAtom -- ^ The wired atom
       -> Cg ()
cgAtom dfs qnfo (WiredAtom win wout the_atom) 
  = case the_atom of
      SAExp aexp          -> cgAExpAtom dfs qnfo win wout False aexp
      SAExpIgnoreRet aexp -> cgAExpAtom dfs qnfo win wout True  aexp
      SACast intty outty  -> cgCastAtom dfs qnfo win wout inty outty
      SADiscard inty      -> cgDiscAtom dfs qnfo win wout inty 

******** here ************* 
cgAExpAtom :: DynFlags
           -> QueueInfo
           -> [(Int,EId)] -- ^ in wires
           -> [(Int,EId)] -- ^ out wires
           -> Bool        -- ^ True <-> IgnoreRet
           -> AExp ()
           -> Cg ()
cgAExpAtom dfs qnfo wins wouts aexp = 


= do
   -- Create a new function declaration and get back C name 
   ziria_nm <- cg_atom the_atom   
   cg_wiring qs win wout (cg_mk_call ziria_nm)

cgAtom dfs qs (WiredAtom [(n,x)] [(m,y)] (SACast (n',inty) (m',outty)) = do
  assert (n == n') && (m == m') $ 
  cq <- lookupVarEnv (queueNameOf x)
  cy <- lookupVarEnv y
  appendSmtt [cstm| read_buf(cy,cq,n,...)|]
  appendStmt [cstm| put_buf(cy ...............
  ... 

{- How to initialize Sora queues

   Initialization time: 

   (A) declare:   size_t queue_sizes[NUM_OF_QUEUES];
   (B) set    :   queue_sizes[i] = tySizeOf_C (corresponding queue type);
   (C) call   :   ts_init(NUM_OF_QUEUES,queue_sizes)

   How to use sora queues. 
   (A) putting:
         -- while(ts_isFull(i)) ;
         ts_put (i, (char *) ...)

   (B) getting:
         -- while(ts_isEmpty(i)) {
         --   if (ts_isFinished($id:buf_id)) return 3
         -- }         
         ts_get(i, (char *) ...)

 -------------------------------------------------------------------------------------}


  

cg_mk_call zir_nm (Cast (n,t1) (m,t2)) = 


  let exp_to_compile = if care_about_result then 
                         eAssign ret_var (eCall zir_nm (nub $ invars ++ outvars))
                       else 
                         eCall zir_nm (nub $ invars++ outvars)
  in cgExp exp_to_compile

    

cg_wiring qs win wout m = do
  cg_wire_inputs win >> m cg_wire_outputs win

cg_wire_inputs qs win = mapM cg_wire_input win
  where cg_wire_input (n,x)
         | x `elem` qs = do
          cx <- lookupVarEnv x
          cq <- lookupVarEnv (queueNameOf x)
          appendStmt [cstm| read_buf(cx,cq,$int:n, sizeof(...))|]
         | otherwise = return ()

cg_wire_outputs qs win = mapM cg_wire_output win
  where cg_wire_output (n,x)
         | x `elem` qs = do
          cx <- lookupVarEnv x
          cq <- lookupVarEnv (queueNameOf x)
          appendStmt [cstm| put_buf(cx,cq,$int:n,sizeof(...))|]
         | otherwise = return ()


  

{------





    arr[512] int x; 

    read_buf(xq,x);

    f(x)

    put_buf(xq,x);

----}



cg_aexp_atom :: AExp () 
             -> Bool      -- True <-> Use return value
             -> Cg C.Id,
cg_aexp_atom (MkAExp blk_lbl body ivs ovs ret_ty) b = do
  there_already <- lookupExpFunEnv_maybe blk_lbl
  case there_already of 
    Just (cfn,_,_) -> return $ C.Id (name cfn) loc
    Nothing  -> do -- Not there, we need to declare


  
  
  where 

    fun   = mkFunDefined loc fun_name params body'

    fun_name = ...

    body' = if b then body 
            else eSeq loc body (eVal loc TUnit VUnit)
    loc = expLoc body 




   





   
