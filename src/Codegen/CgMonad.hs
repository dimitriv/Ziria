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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CgMonad
  ( IfThenElse(..)

  , Cg
  , CgError(..)
  , LUTGenInfo (..)
  , evalCg
  , emptyEnv
  , emptyState

  , Code(..)
  , inNewBlock
  , inNewBlock_
  , collectDefinitions
  , collectDefinitions_
  , collectStmts
  , collectStmts_

  , inAllocFrame, isInsideAllocFrame, pushAllocFrame
  , getGlobalWplAllocated
  , addGlobalWplAllocated

  , getFunDefs

  , collect

  , Handle

  , CompInfo(..)
  , mkCompInfo

  , CompKont(..)
  , finalCompKont

  , CompFunGen(..)
  , CodeGen            -- type of expression code generator

  , genSym
  , getNames
  , setNames

  , freshName          -- new Ziria-level EId (typed name)
  , freshVar           -- new C-level var (just a string)
  , CLabel, freshLabel -- new C-level block id

    -- TODO: Get rid of the CLabel id management
    -- through nameStack ASAP ncluding the next
    -- three functions
  , nextName
  , pushName
  , printNames


  , getLUTHashes
  , setLUTHashes

  , appendTopDef
  , appendTopDefs
  , appendTopDecl
  , appendTopDecls
  , appendDecl
  , appendDecls
  , appendStmt
  , appendStmts
  , appendLabeledBlock
  , appendStructDef

  , codeStmt
  , codeStmts
  , codeDecl
  , codeDecls
  , getCode
  , emitCode


  , extendFunEnv
  , extendVarEnv
  , extendExpFunEnv
  , extendCompEnv
  , extendTyDefEnv

  , withThreadId

  , withClonedState

  , lookupCompFunCode
  , lookupExpFunEnv
  , lookupExpFunEnv_maybe
  , lookupVarEnv
  , lookupCompCode
  , lookupTyDefEnv
  , getBoundVars

  , newHeapAlloc
  , getMaxStackAlloc

  , getTyPutGetInfo

  , cgTIntName
  , cgIO
  , cMAX_STACK_ALLOC

  , withDisabledBC
  , withDisabledBCWhen
  , isDisabledBC

  , withModuleName
  , getHeapContext
  , getBufContext
  , getGlobalParams
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (toUpper)
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Loc (noLoc)
import Data.Loc
import qualified Data.Symbol
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ hiding ( (<>) )
import Data.Maybe

import Rebindables
import AstExpr
import NameEnv

import AstComp
import Outputable
import PpComp
import PpExpr
import qualified GenSym as GS
import CgHeader

import Opts

instance IfThenElse Bool a where
    ifThenElse True  th _  = th
    ifThenElse False _  el = el

instance IfThenElse C.Exp (Cg ()) where
    ifThenElse ce mth mel = do
        (th_decls, th_stms) <- inNewBlock_ mth
        (el_decls, el_stms) <- inNewBlock_ mel
        appendStmt [cstm|if ($ce) {
                           $decls:th_decls
                           $stms:th_stms
                         } else {
                           $decls:el_decls
                           $stms:el_stms
                         }|]


instance IfThenElse C.Exp Code where
    ifThenElse ce mth mel =
        let (th_defs,th_decls,th_stms) = getCode mth
            (el_defs,el_decls,el_stms) = getCode mel
            s = [cstm|if ($ce) {
                           $decls:th_decls
                           $stms:th_stms
                         } else {
                           $decls:el_decls
                           $stms:el_stms
                         }|]
        in Code (DL.fromList (th_defs ++ el_defs))
                (DL.empty)
                (DL.singleton s)


------------------------------------------------------------------------------
-- | Continuations
------------------------------------------------------------------------------

type Handle = String

------------------------------------------------------------------------------
-- | Code Generation Monad Cg
------------------------------------------------------------------------------

-- The handlers to a component.
data CompInfo = CompInfo
    { compGenInit :: Code   -- Initialization stuff
    , tickHdl     :: Handle
    , procHdl     :: Handle
    , canTick     :: Bool
    }


-- Create handles to this component's init/tick/process
mkCompInfo :: Handle -> Bool -> CompInfo
mkCompInfo pref = CompInfo mempty pref pref

-- Note [Continuation Invariants]
--
-- The following three continuations do not overlap. Importantly: they are the
-- last thing that happens inside the tick()/process() functions.
-- NB: Although they are Cg () stuff they never create labelled blocks
--
data CompKont = CompKont
    { kontDone    :: Cg () -- Note [kontDone]
                           -- What to do in the body of tick() or process() once/if
                           -- they set the whatIs flag to Done (and the doneVal to something)

    , kontYield   :: Cg () -- Note [kontYield]
                           -- What to do in the body of tick() or process() once/if
                           -- they set the whatIs flag to Yield (and the yldVal to something)

                           -- E.g. if you are the upstream process() or tick() function in a >>> composition
                           -- you must (i) push your yldVal to the downstream process() function instead of
                           -- returning (IMMEDIATE if you were tick(), or void + setting whatIs=YLD if you
                           -- were process) and (ii) return (IMMEIDATE if you were tick() otherwise void).

    , kontConsume :: Cg () -- Note [kontConsume]
                           -- Code to execute from inside tick() whenever we need to consume.

                           -- E.g. you are the downstream component in a >>> composition, and
                           -- instead of returning 'CONSUME', you call the upstream tick()
                           -- But if you have no upstream component then you'd simply return
                           -- CONSUME.

    , doneHdl  :: Handle   -- where to write our done value
    , yieldHdl :: Handle   -- where to write our yield value
    , inHdl    :: Handle   -- where to read our input values
    }

finalCompKont :: String -> CompKont
finalCompKont tid = CompKont
    { kontDone    = do appendStmt [cstm|$id:globalWhatIs = DONE;|]
                       appendStmt [cstm|goto l_IMMEDIATE;|]
    , kontYield   = do appendStmt [cstm|$id:globalWhatIs = YIELD;|]
                       appendStmt [cstm|goto l_IMMEDIATE;|]
    , kontConsume = appendStmt [cstm|goto l_CONSUME;|]
    , doneHdl     = threadIdOf tid globalDoneHdl
    , yieldHdl    = threadIdOf tid globalYldHdl
    , inHdl       = threadIdOf tid globalInHdl
    }

type ExpGen = C.Exp

type CompGen = CompKont -> Cg CompInfo

type CompFunGen = [CallArg Exp Comp] -> CompKont -> Cg CompInfo

data CgEnv = CgEnv
    {
      -- | Parameterized ST computations
      compFunEnv :: NameEnv CTy CompFunGen

      -- | TODO: doc
    , compEnv :: NameEnv CTy CompGen

      -- | Type definitions
    , tyDefEnv :: M.Map TyName StructDef

      -- | TODO: doc
    , varEnv :: NameEnv Ty ExpGen

      -- | The code generator lifts local functions to top-level, introducing
      -- additional function parameters for any free variables in the (local)
      -- function body. For each local function we record the mapping from the
      -- local function name to the unique name of the lifted global fiunction,
      -- as well as the list of additional "closure" parameters that we
      -- introduced.
    , funEnv :: NameEnv Ty (GName Ty, [GName Ty],Bool)
      -- | Boolean flag: True => returns an already allocated LUT, False => otherwise

      -- | Reference to a symbol, for gensym'ing
    , symEnv :: GS.Sym

      -- | When true, no bound checks are emitted overriding any user
      -- preferences. Useful for external functions that are "trust me"
    , disableBC  :: Bool

      -- | This is to be added to global variables, to allow us to link
      -- multiple Ziria modules in the same C project
    , moduleName :: String

      -- Yes means we run no danger of leaking, someone will free memory
    , inAllocatorFrame :: Bool 
    }

emptyEnv :: GS.Sym -> CgEnv
emptyEnv sym =
    CgEnv { compFunEnv = neEmpty
          , compEnv    = neEmpty
          , tyDefEnv   = M.fromList primComplexStructs
          , varEnv     = neEmpty
          , funEnv     = neEmpty
          , symEnv     = sym
          , disableBC  = False
          , moduleName = ""
          , inAllocatorFrame = False 
          }



-- Note [CodeGen Invariants]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- (1) In tick() or process(), whenever we produce a Result
-- (yldVal/doneVal + whatIs) we must set the global /data/ pointers
-- (global_yldVal/global_doneVal + global_whatIs) to our result.
--
-- (2) We respect the continuation invariants,
-- see Note [Continuation Invariants]

data LUTGenInfo
  = LUTGenInfo { lgi_lut_var        :: C.Exp              -- the lut table variable
               , lgi_lut_gen        :: C.Stm              -- call to the lut initializer
               , lgi_masked_outvars :: [(EId, Maybe EId)] -- The outvars plus their mask BitArrPtr vars
               }
  deriving Show

data CgState = CgState {
      nameStack :: [CLabel]

      -- | Number of heap-allocated variables
    , numAllocs :: Int

      -- | Hashed LUT-ted exprs, along with the C variable for the LUT
    , maxStackAlloc :: Int

    , lutHashes  :: [(Int,LUTGenInfo)]

      -- | Number of already defined structs
      --
      -- if > than this, then allocate on the heap
    , structDefs :: [TyName]
    
    , funDefs    :: [EId]

    , globalWplAllocated :: [C.Stm]

    }
  deriving Show

emptyState = CgState [] 0 Opts.cMAX_STACK_ALLOC [] [] [] []


data Code = Code
    { -- Top-level definitions
      defs     :: !(DList C.Definition)
      -- Local declarations
    , decls    :: !(DList C.InitGroup)
      -- Local statements
    , stmts   :: !(DList C.Stm)
    }

getCode :: Code -> ([C.Definition],[C.InitGroup],[C.Stm])
getCode (Code df dc st) = (DL.toList df, DL.toList dc, DL.toList st)


instance Monoid Code where
    mempty = Code { defs     = mempty
                  , decls    = mempty
                  , stmts    = mempty
                  }

    a `mappend` b = Code { defs  = defs a <> defs b
                         , decls = decls a <> decls b
                         , stmts = stmts a <> stmts b
                         }

data CgError = GenericError String
  deriving (Show)

newtype Cg a = Cg { runCg :: CgEnv
                          -> CgState
                          -> IO (Either CgError (a, Code, CgState)) }

evalCg :: GS.Sym -> Int -> Cg () -> IO (Either CgError [C.Definition])
evalCg sym stack_alloc_threshold m = do
    res <- runCg m (emptyEnv sym)
                   (emptyState { maxStackAlloc = stack_alloc_threshold })
    case res of
      Left err -> return $ Left err
      Right (_, code, _) -> return $ Right $ DL.toList (defs code)

instance Monad Cg where
    return x = Cg $ \rho s -> return (Right (x, mempty, s))

    (>>=) m f =
        Cg $ \rho s -> do res1 <- runCg m rho s
                          case res1 of
                            Left err -> return (Left err)
                            Right (x, w1, s') -> do res2 <- runCg (f x) rho s'
                                                    case res2 of
                                                      Left err -> return (Left err)
                                                      Right (y, w2, s'') ->
                                                          let w = w1 <> w2
                                                          in
                                                            w `seq` return (Right (y, w, s''))

    (>>) m1  m2 =
        Cg $ \rho s -> do res1 <- runCg m1 rho s
                          case res1 of
                            Left err -> return (Left err)
                            Right (_, w1, s') -> do res2 <- runCg m2 rho s'
                                                    case res2 of
                                                      Left err -> return (Left err)
                                                      Right (y, w2, s'') ->
                                                          let w = w1 <> w2
                                                          in
                                                            w `seq` return (Right (y, w, s''))

    fail msg = throwError $ GenericError msg

instance MonadIO Cg where
    liftIO m = Cg $ \_ s -> m >>= \a -> return (Right (a, mempty, s))

cgIO :: IO a -> Cg a
cgIO = liftIO

instance Functor Cg where
    fmap f x = x >>= return . f

instance Applicative Cg where
    pure   = return
    (<*>)  = ap

instance MonadError CgError Cg where
    throwError e = Cg $ \_ _ -> return (Left e)

    catchError m f =
        Cg $ \rho s -> do res <- runCg m rho s
                          case res of
                            Left err -> runCg (f err) rho s
                            Right x  -> return $ Right x

instance MonadReader CgEnv Cg where
    -- | Fetch the value of the environment.
    ask = Cg $ \r s -> return (Right (r, mempty, s))

    -- | Execute a computation in a modified environment.
    local f m = Cg $ \r s -> runCg m (f r) s

instance MonadState CgState Cg where
    get   = Cg $ \_ s -> return (Right (s, mempty, s))
    put s = Cg $ \_ _ -> return (Right ((), mempty, s))

instance MonadWriter Code Cg where
    -- | @'tell' w@ is an action that produces the output @w@.
    tell w = Cg $ \_ s -> return (Right ((), w, s))

    -- | @'listen' m @is an action that executes the action @m@ and adds its
    -- output to the value of the computation.
    listen m = Cg $ \r s -> do res <- runCg m r s
                               case res of
                                 Left err -> return $ Left err
                                 Right (x, w, s') -> return $ Right ((x, w), w, s')

    -- | @'pass' m@ is an action that executes the action @m@, which returns a
    -- value and a function, and returns the value, applying the function to the
    -- output.
    pass m = Cg $ \r s -> do res <- runCg m r s
                             case res of
                               Left err -> return $ Left err
                               Right ((x, f), w, s') -> return $ Right (x, f w, s')

-- | @'collect' m@ executes the action @m@ and captures its output without
-- adding it to the output of the outer computation.
collect :: MonadWriter w m => m a -> m (a, w)
collect m = censor (const mempty) $ listen m

inNewBlock :: Cg a -> Cg ([C.InitGroup], [C.Stm], a)
inNewBlock m
 = do { (x, code) <- collect m
      ; tell code { decls = mempty, stmts  = mempty }
      ; let decls' = DL.toList (decls code)
            stmts' = DL.toList (stmts code)
      ; return (decls',stmts',x)
      }

inNewBlock_ :: Cg () -> Cg ([C.InitGroup], [C.Stm])
inNewBlock_ m = do
    (decls, stms, _) <- inNewBlock m
    return (decls, stms)

-- | Execute this action in an allocation frame. Use with moderation!
inAllocFrame :: SrcLoc -> Cg a -> Cg a
inAllocFrame _origin action
  = do { idx <- freshVar "mem_idx"
       ; heap_context <- getHeapContext
       ; appendDecl [cdecl| unsigned int $id:idx; |]
       ; appendStmt
           [cstm| $id:idx = wpl_get_free_idx($id:heap_context); |]
       ; x <- action 
       ; appendStmt
           [cstm| wpl_restore_free_idx($id:heap_context, $id:idx); |]
       ; return x }

pushAllocFrame :: Cg a -> Cg a 
-- | To be used when generating code for a function
pushAllocFrame action 
  = local (\rho -> rho { inAllocatorFrame = True }) action

isInsideAllocFrame :: Cg Bool
isInsideAllocFrame = asks inAllocatorFrame

collectDefinitions :: Cg a -> Cg ([C.Definition], a)
collectDefinitions m = do
    (x, code) <- collect m
    tell code { defs = mempty }
    return (DL.toList (defs code), x)

collectDefinitions_ :: Cg () -> Cg ([C.Definition])
collectDefinitions_ m = do
    (defs, _) <- collectDefinitions m
    return defs

collectStmts :: Cg a -> Cg ([C.Stm], a)
collectStmts m = do
    (x, code) <- collect m
    tell code { stmts = mempty }
    return (DL.toList (stmts code), x)

collectStmts_ :: Cg () -> Cg ([C.Stm])
collectStmts_ m = do
    (stms, _) <- collectStmts m
    return stms

genSym :: String -> Cg String
genSym prefix = do
    sym       <- asks symEnv
    str       <- liftIO $ GS.genSymStr sym
    return $ prefix ++ str


-- TODO: remove this nameStack management of component ids!
getNames :: Cg [CLabel]
getNames = gets nameStack

setNames :: [CLabel] -> Cg ()
setNames names' = modify $ \s -> s { nameStack = names' }

nextName :: String -> Cg CLabel
nextName prefix = do
  names <- getNames
  case names of
    [] -> genSym prefix
    nm : names' -> setNames names' >> return nm

pushName :: CLabel -> Cg ()
pushName nm = modify $ \s -> s { nameStack = nm : nameStack s }


getLUTHashes :: Cg [(Int,LUTGenInfo)]
getLUTHashes
  = gets lutHashes

setLUTHashes :: [(Int,LUTGenInfo)] -> Cg ()
setLUTHashes hs
  = modify $ \s -> s { lutHashes = hs }

-- | {get/add}GlobalWplAllocated
-- ^ NB: statements in reverse order
getGlobalWplAllocated :: Cg [C.Stm]
getGlobalWplAllocated = reverse <$> gets globalWplAllocated

addGlobalWplAllocated :: C.Stm -> Cg ()
addGlobalWplAllocated stm
  = modify $ \s -> s { globalWplAllocated = stm : (globalWplAllocated s) }


newHeapAlloc :: Cg ()
newHeapAlloc = modify $ \s ->
   s { numAllocs = numAllocs s + 1 }


getMaxStackAlloc :: Cg Int
getMaxStackAlloc = gets maxStackAlloc


freshName :: String -> Ty -> MutKind -> Cg EId
-- A new name of a given type
freshName prefix ty mk
  = do { s' <- genSym prefix
       ; return $ toName s' noLoc ty mk
       }

type CLabel = String

freshLabel :: String -> Cg CLabel
-- A new C-level label (e.g. block label)
freshLabel prefix
  = genSym prefix


freshVar :: String -> Cg String
-- A new C-level variable
freshVar prefix
  = genSym prefix

printNames :: Cg ()
printNames = do
  names <- getNames
  liftIO $ mapM_ putStrLn names

printState :: Cg ()
printState = do
  s <- get
  liftIO $ putStrLn $ show s

appendTopDef :: C.Definition -> Cg ()
appendTopDef newDef =
  tell mempty { defs = DL.singleton newDef }

appendTopDefs :: [C.Definition] -> Cg ()
appendTopDefs newDefs =
  tell mempty { defs = DL.fromList newDefs }

appendStructDef :: TyName -> C.InitGroup -> Cg ()
-- Structs can't shadow each other in Blink, but we may end up
-- in a situation where we translate 2 structs at the same time
-- due to pipelining. But we should only emit the struct once.
appendStructDef tyname sdef
  = do { sdefs <- gets structDefs
       ; if tyname `elem` sdefs then return ()
         else do { modify $ \s -> s { structDefs = tyname : (structDefs s) }
                 ; appendTopDefs [cunit|
$esc:("#ifndef " ++ define)
$esc:("#define " ++ define)
$decl:sdef;
$esc:("#endif")
|]
                 }
       }
  where
    define :: String
    define = "STRUCT_" ++ map toUpper tyname

appendTopDecl :: C.InitGroup -> Cg ()
appendTopDecl newDecl =
  tell mempty { defs = DL.singleton (C.DecDef newDecl noLoc) }

appendTopDecls :: [C.InitGroup] -> Cg ()
appendTopDecls newDecls =
  tell mempty { defs = DL.fromList [C.DecDef decl noLoc | decl <- newDecls] }

appendDecl :: C.InitGroup -> Cg ()
appendDecl newDecl = tell (codeDecl newDecl)

appendDecls :: [C.InitGroup] -> Cg ()
appendDecls newDecls = tell (codeDecls newDecls)

codeStmt :: C.Stm -> Code
codeStmt newStmt = mempty { stmts = DL.singleton newStmt }

codeStmts :: [C.Stm] -> Code
codeStmts newStmts = mempty { stmts = DL.fromList newStmts }

codeDecl :: C.InitGroup -> Code
codeDecl newDecl = mempty { decls = DL.singleton newDecl }

codeDecls :: [C.InitGroup] -> Code
codeDecls newDecls = mempty { decls = DL.fromList newDecls }


emitCode :: Code -> Cg ()
emitCode cd
  = do { let (defs,decls,stms) = getCode cd
       ; appendTopDefs defs
       ; appendDecls decls
       ; appendStmts stms }

appendStmt :: C.Stm -> Cg ()
appendStmt newStmt = tell (codeStmt newStmt)

appendStmts :: [C.Stm] -> Cg ()
appendStmts newStmts = tell (codeStmts newStmts)

{- Original:
appendLabeledBlock :: C.ToIdent i => i -> Cg () -> Cg ()
appendLabeledBlock i m
  = do { (decls, stms) <- inNewBlock_ m
       ; appendStmt [cstm|$id:i: { $decls:decls $stms:stms }|] }
-}

-- Modified
appendLabeledBlock :: C.ToIdent i => i -> Cg () -> Cg ()
appendLabeledBlock i m
  = do { (decls, stms) <- inNewBlock_ m
       ; appendDecls decls -- Propagate those out!
       ; appendStmt [cstm|$id:i: { $stms:stms }|] }


extendFunEnv :: GName CTy -> CompFunGen -> Cg a -> Cg a
extendFunEnv nm fn =
    local $ \rho -> rho { compFunEnv = neExtend nm fn (compFunEnv rho) }

extendVarEnv :: [(GName Ty, ExpGen)] -> Cg a -> Cg a
-- BUG: This seems wrong: We can't inline a potentially
-- imperative expression anywhere we want! What is this ExpGen stuff????
extendVarEnv binds a = do
    -- mapM_ shadow_warn binds
    -- We need to bind array lengths of polymorphic array as well
    local (\rho -> rho { varEnv = 
      neExtendMany (binds ++ convTy binds) (varEnv rho) }) a
  where
    -- NOTE: This is the point where we introduce GNames for LenVars.
    getPolymArrTy :: (GName Ty, ExpGen) -> Maybe (GName Ty, ExpGen)
    getPolymArrTy (n, e) = case nameTyp n of
      TArray (NVar nv) ta -> Just (toName nv noLoc tint Imm, [cexp|$id:nv|])
      _                   -> Nothing

    convTy :: [(GName Ty, ExpGen)] -> [(GName Ty, ExpGen)]
    convTy binds = catMaybes (map getPolymArrTy binds)

    shadow_warn (n,_) = do 
       mb <- asks (neLookup n . varEnv)
       case mb of 
         Just {} -> cgIO $ putStrLn ("Code generation shadowing: " ++ show n)
         Nothing -> return ()


extendExpFunEnv :: GName Ty -> (GName Ty,[GName Ty],Bool) -> Cg a -> Cg a
extendExpFunEnv nm bind action = do
   modify (\s -> s { funDefs = nm : funDefs s })
   local  (\rho -> rho { funEnv = neExtend nm bind (funEnv rho) }) action


getFunDefs :: Cg [GName Ty]
getFunDefs = gets funDefs


extendTyDefEnv :: TyName -> StructDef -> Cg a -> Cg a
extendTyDefEnv nm bind =
   local $ \rho -> rho { tyDefEnv = M.insert nm bind (tyDefEnv rho) }


extendCompEnv :: GName CTy -> CompGen -> Cg a -> Cg a
extendCompEnv nm bind =
    local $ \rho -> rho { compEnv = neExtend nm bind (compEnv rho) }


withDisabledBC :: Cg a -> Cg a
withDisabledBC =
    local $ \rho -> rho { disableBC = True }

withDisabledBCWhen :: Bool -> Cg a -> Cg a
withDisabledBCWhen True  x = withDisabledBC x
withDisabledBCWhen False x = x

isDisabledBC :: Cg Bool
isDisabledBC = asks $ \rho -> disableBC rho


withModuleName :: String -> Cg a -> Cg a
withModuleName str =
    local $ \rho -> rho { moduleName = str }

getHeapContext :: Cg String
getHeapContext = asks $ \rho -> "pheap_ctx" ++ (moduleName rho)

getBufContext :: Cg String
getBufContext = asks $ \rho -> "pbuf_ctx" ++ (moduleName rho)

getGlobalParams :: Cg String
getGlobalParams = asks $ \rho -> "params" ++ (moduleName rho)

withThreadId :: String -> Cg a -> Cg a
withThreadId _ m = m

withClonedState :: Cg a -> Cg a
withClonedState cg = do
    st0 <- get
    sym <- asks symEnv
    n   <- liftIO $ GS.getSym sym
    (ret, _) <- collect cg
    liftIO $ GS.setSym sym n
    put st0
    return ret

lookupCompFunCode :: GName CTy -> Cg CompFunGen
lookupCompFunCode nm = do
    maybe_gen <- asks $ \rho -> neLookup nm (compFunEnv rho)
    case maybe_gen of
      Nothing  -> fail ("CodeGen: unbound computation function: " ++ show nm)
      Just gen -> return gen

lookupExpFunEnv :: GName Ty -> Cg (GName Ty, [GName Ty],Bool)
lookupExpFunEnv nm  = do
    maybe_x <- asks $ \rho -> neLookup nm (funEnv rho)
    case maybe_x of
      Nothing -> do funs <- asks funEnv
                    let bound = map (show . ppName) (neKeys funs)
                    fail ("Unbound function in code generation: "  ++ show (ppName nm) ++
                          " bound = " ++ show bound ++ " pos = " ++ (displayLoc . locOf . nameLoc) nm)
      Just x  -> return x

lookupExpFunEnv_maybe :: GName Ty -> Cg (Maybe (GName Ty, [GName Ty],Bool))
lookupExpFunEnv_maybe nm  =
    asks $ \rho -> neLookup nm (funEnv rho)


getBoundVars :: Cg [GName Ty]
getBoundVars = asks (neKeys . varEnv)

lookupVarEnv :: GName Ty -> Cg ExpGen
lookupVarEnv nm = do
    maybe_x <- asks $ \rho -> neLookup nm (varEnv rho)
    case maybe_x of
      Nothing -> do vars <- asks varEnv
                    let bound = map ppNameUniq (neKeys vars)
                    -- TODO: This still does not render properly because we do a show on this string.
                    -- It has to do with the type of failOnError in Main.hs. The right thing to do
                    -- would be to simply make Cg return a Doc in the case of an error, not a string.
                    
                    liftIO $ 
                     print (vcat [ text ("Unbound variable in code generation: "  ++ show (ppNameUniq nm))
                                 , text "bound = "
                                 , nest 2 $ vcat bound
                                 , text ("pos = " ++ (displayLoc . locOf . nameLoc) nm) ])
                    fail "Aborting compilation."

      Just x  -> return x

lookupCompCode :: GName CTy -> Cg CompGen
lookupCompCode nm = do
    maybe_e <- asks $ \rho -> neLookup nm (compEnv rho)
    case maybe_e of
      Nothing -> fail $ "CodeGen: Unbound computation var " ++ show nm ++ " detected!"
      Just e  -> return e

lookupTyDefEnv :: TyName -> Cg StructDef
lookupTyDefEnv nm = do
    (maybe_e,env) <- asks $ \rho -> (M.lookup nm (tyDefEnv rho), tyDefEnv rho)
    case maybe_e of
      Nothing -> fail $ 
        "CodeGen: Unbound struct type " ++ show nm ++ " detected!" ++ "\nBound are: " ++ show (M.keys env)
      Just e  -> return e


-- Precondition: t is an array type w/ known size
getLitArrSz :: Ty -> Int
getLitArrSz (TArray (Literal n) t) = n
getLitArrSz _                      = error "CodeGen: Got array size of an unresolved array type!"

getTyPutGetInfo :: Ty -> (String,Int)
-- Get a description of the type that matches the driver description
getTyPutGetInfo ty = (buf_typ ty, buf_siz ty)
  where buf_siz (TArray (Literal n) _) = n
        buf_siz (TBuff (IntBuf t)) = buf_siz t
        buf_siz _other     = 1
        buf_typ ty
          = case ty of

              -- Bit buffers
              TBit -> "bit"
              TBool -> "bit"

              -- Complex buffers
              TStruct nm _ | nm == complexTyName   -> "complex32"
              TStruct nm _ | nm == complex32TyName -> "complex32"
              TStruct nm _ | nm == complex16TyName -> "complex16"
              TStruct nm _ | nm == complex8TyName  -> "complex8"

              -- Arrays should be just fine
              TArray _ bty -> "arr" ++ buf_typ bty

              TInt bw sg   -> cgTIntName bw sg

              -- internal synchronization queue buffers
              (TBuff (IntBuf t)) -> buf_typ t

              -- Arbitrary structure - we use generic buffer read/write called buf_getchunk/buf_puchunk
              TStruct nm _ -> "chunk"

              -- Others ... Not sure how well-supported they are
              otherty
                -> error $ "Code generation does not yet support buffers for type:" ++ show otherty




cgTIntName :: BitWidth -> Signedness -> String
cgTIntName BW8  Signed          = "int8"
cgTIntName BW16 Signed          = "int16"
cgTIntName BW32 Signed          = "int32"
cgTIntName BW64 Signed          = "int64"
cgTIntName (BWUnknown _) Signed = "int32" -- Defaulting to 32 bits
cgTIntName BW8  Unsigned        = "uint8"
cgTIntName BW16 Unsigned        = "uint16"
cgTIntName BW32 Unsigned        = "uint32"
cgTIntName BW64 Unsigned        = "uint64"
cgTIntName (BWUnknown _) Unsigned = "uint32" 


type CodeGen = DynFlags -> Exp -> Cg C.Exp
