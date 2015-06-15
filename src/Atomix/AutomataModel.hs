module AutomataModel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)

import Data.Graph.Inductive.Graph  ( Node )
import qualified Data.Graph.Inductive.Graph  as G
import Data.Graph.Inductive.PatriciaTree as G

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

import AtomComp -- simplified Ziria model
import AstExpr (nameTyp)
import Opts


type LNode = G.LNode NodeLabel
type Chan = Var

data AtomixFun a 
  = AtomixFun { in_tys   :: [Ty]
              , out_tys  :: [Ty]
              , fun_code :: a
              }


data NodeLabel
  = NodeLabel { node_id   :: Node
              , node_kind :: NodeKind
              }

data NodeKind
  = Action { action_in     :: [Chan]
           , action_out    :: [Chan] -- NB: includes the 'return' channel
           , action_code   :: AtomixFun
           }

  | Loop { loop_times :: Maybe Int }
 
 
  | Branch { branch_in   :: Set Chan 
           , branch_code :: SymFun
           , branch_next :: (Node,Node)
           }

data Automaton
  = Automaton { autom_init  :: Node
              , autom_final :: Set Node }

type ZirGraph = Gr NodeLabel ()

type GraphM a = StateT ZirGraph (ReaderT (CompEnv ()) IO) a


mkNode :: NodeKind -> GraphM LNode
mkNode kind = do
  g <- get
  let [node] = G.newNodes 1 g
  let lnode = (node, NodeLabel node kind)
  put (G.insNode lnode g)
  return lnode

--mkDummyNode :: GraphM LNode
--mkDummyNode = mkNode $ Action Set.empty Set.empty (eint32 0) []

--mkDummyAutomaton :: GraphM Automaton
--mkDummyAutomaton = do
--  (node,_) <- mkDummyNode
--  return $ Automaton node (Set.singleton node)

singletonAutomaton :: Node -> Automaton
singletonAutomaton node = Automaton node (Set.singleton node)

--extendChan :: (EId,Chan) -> GraphM a -> GraphM a
--extendChan (x,c) = local add_bnd
--  where add_bnd (ZirEnv binds sym) = ZirEnv ((x,c):binds) sym

----lookupChan :: EId -> GraphM Chan
----lookupChan x = do
----   env <- ask
----   case lookup x (chan_binds env) of
----      Nothing   -> fail ("Automata generation: unbound variable " ++ (show x))
----      Just chan -> return chan

---- TODO: use proper exception
--lookupChan :: ZirEnv -> EId -> Chan
--lookupChan env x =
--   case lookup x (chan_binds env) of
--      Nothing   -> assert False undefined
--      Just chan -> chan

--freshChan :: EId -> GraphM Chan
--freshChan x = do
--  u <- liftIO . GS.genSymStr =<< asks chan_gensym
--  return $ x { uniqId = MkUniq u }

--runGraphM :: GraphM a -> IO (a,ZirGraph)
--runGraphM m = do
--   new_sym <- GS.initGenSym "automata"
--   runReaderT (runStateT m (G.empty)) (ZirEnv [] new_sym)

{-------------------- Translation --------------------}

concatAutomata :: Automaton -> Automaton -> GraphM Automaton
concatAutomata a1 a2 = do
    let edges = map (\x -> (x, autom_init a2, ())) (Set.toList $ autom_final a1)
    modify (G.insEdges edges)
    return $ Automaton (autom_init a1) (autom_final a2)

data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }


-- need to define some standard functions
-- discard t : t -> () = NOP
-- identity t : t -> t = Var
-- 

-- builds discard function of the appropriate type
mkDiscard :: Ty -> GraphM FunName
mkDiscard t = fail "not implemented"

-- builds identity function of the appropriate type
mkIdentity :: Ty -> GraphM FunName
mkIdentity t = fail "not implemented"


mkAutomaton :: DynFlags -> Channels -> Comp a b -> GraphM Automaton
mkAutomaton dfs chans comp = go chans (unComp comp)
  where
    go chans (Take1 t) = do
      let inp = Set.singleton (in_chan chans)
      case ctrl_chan chans of
        Nothing -> do
          let outp = Set.empty
              code = SFDiscard t
          (node,_) <- mkNode (Action inp outp code)
          return $ singletonAutomaton node
        Just ctrlc -> do
          let outp = Set.singleton ctrlc
              code = SFId t
          (node,_) <- mkNode (Action inp outp code)
          return $ singletonAutomaton node

    go chans (TakeN _ n) = fail "not implemented"

    go chans (Emit1 x) = do
      let inp = Set.singleton x
      let outp = Set.singleton (out_chan chans)
          code = SFId (nameTyp x)
      (node,_) <- mkNode (Action inp outp code)
      return $ singletonAutomaton node

    go chans (EmitN x) = fail "not implemented" 

    go chans (Return (MkExp (ExpApp fn args) _ _)) = do
      let TArrow argtys _res = nameTyp fn
          inp  = Set.fromList args
          ctr  = maybeToList (ctrl_chan chans)
          outp = Set.fromList (ctr : outargs) 
          outargs = concat $ 
                    zipWith (\arg argty -> 
                      if argty_mut argty == Mut then [arg] else []) args argtys

      (node,_) <- mkNode (Action inp outp fn args)
      return $ singletonAutomaton node
      

********
      




      let ctr = maybeToList (ctrl_chan chans) 
      let inp = Set.fromList $ case e of ExpVar x -> [x]
                                         ExpApp _ args -> args
      let out = Set.fromList $ case e of 
           ExpVar x -> ctr
           ExpApp fn args -> 
             let TArrow argtys _res = nameTyp fn
                 tmp = zipWith (\arg argty -> if argty_mut argty == Mut then [arg] else []) 
                               args argtys
             in concat tmp



                                           
                            


ctr ++ ... (look at the type of f and filter) 
                                         -- (map fst . filter (\(_,k) -> k==Mut) $ args)

      fail "TBD"
      --let code = 
      --  case (e,ctr) of (ExpApp f _, _) -> f
      --                  (ExpVar _, []) ->


    go chans (NewVar x_spec c) = mkAutomaton dfs chans c -- NOP for now

    -- go chans (Bind x c) = mkAutomaton dfs (chans { ctrl_chan = Just x }) c

    go chans (Bind mbx c1 c2) = do
      a1 <- mkAutomaton dfs (chans { ctrl_chan = mbx }) c1
      a2 <- mkAutomaton dfs chans c2
      concatAutomata a1 a2

    -- go chans 

    -- go chans (Seq c1 c2) = do
    --   a1 <- mkAutomaton dfs chans c1
    --   a2 <- mkAutomaton dfs chans c2
    --   concatAutomata a1 a2

    go chans (Par _ c1 c2) = fail "not implemented" 

    go chans (AtomComp.Branch x c1 c2) = fail "not implemented" 

    go chans (RepeatN n c) = fail "not implemented" 
    go chans (Repeat c) = fail "not implemented"

    go chans (While x c) = fail "not implemented"
    go chans (Until x c) = fail "not implemented"
