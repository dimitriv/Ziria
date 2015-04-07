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
{-# LANGUAGE GADTs, RankNTypes, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, RecordWildCards, 
    TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module AstComp where

import Prelude hiding (pi, mapM)
import Control.Arrow ((***))
import Control.Monad (forM, liftM)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Functor.Identity ( Identity (..) )
import Data.Monoid
import Data.Set (Set)
import Data.Maybe ( isJust )
import Data.Traversable (mapM)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Parsec.Pos
import Text.Show.Pretty (PrettyVal)
import qualified Data.Set as S

import AstExpr
import PpExpr ()
import Utils

{-------------------------------------------------------------------------------
  Comp types

  Although we do not distinguish between source and internal comp types, we
  _do_ distinguish between source and internal expression types, and since
  comp types include expression types, we parameterize by the expression type.
-------------------------------------------------------------------------------}

-- | Computation type variables
type CTyVar = String

data GCTy ty where
  CTVar   :: CTyVar -> GCTy ty
  CTComp  :: ty -> ty -> ty -> GCTy ty
  CTTrans :: ty -> ty -> GCTy ty
  CTArrow :: [CallArg (GArgTy ty) (GCTy ty)] -> GCTy ty -> GCTy ty
  deriving (Generic, Typeable, Data)

{-------------------------------------------------------------------------------
  AST parameterized by type (see "AstExpr")
-------------------------------------------------------------------------------}

{- TODO -- replace all the let constructs with simpler binding forms
   NOTE: A similar data type exists in the parse as LetDecl.
data Bind a b
  = BindC Name (Comp a b)           -- Bind computation:       let comp x = ...
  | BindStruct String StructDef     -- Structure definition:   struct x = { ... }
  | BindE Name (Exp b)              -- Bind expression:        let x = ...
  | BindExtFun Name (Fun b)         -- Bind external function: let external f(...) : ty
  | BindFun Name (Fun b)            -- Bind ordinary function: let f(...) = ...
  | BindFunC Name CParams Locals    -- Bind comp. function:    let comp f(...) = ...
  | BindMonadic Name (Comp a b)     -- Bind from a monad:      x <- c
-}

data GComp0 tc t a b where
  -- | Variables
  Var :: GName tc -> GComp0 tc t a b

  -- | Monadic bind
  --
  -- This represents a chain of monadic binds, which is a good representation for
  -- flattening out stuff and nested binds etc. In other words
  --
  -- > x1 <- c1; x2 <- c2; ... xn <- cn
  --
  -- is represented as:
  --
  -- > BindMany c1 [(x1,c2), (x2,c3) .... (x_(n-1),cn)]
  BindMany :: GComp tc t a b -> [(GName t,GComp tc t a b)] -> GComp0 tc t a b

  -- | Sequential composition
  --
  -- TODO: The type checker translates this to `BindMany`. If we had a more
  -- complete distinction between source and internal syntax this could
  -- probably go (#71).
  Seq :: GComp tc t a b -> GComp tc t a b -> GComp0 tc t a b

  -- | Parallel composition
  --
  -- We can compose to transformers
  --
  -- > c1 :: ST T a b   c2 :: ST T b c
  -- > -------------------------------
  -- >     c1 >>> c2 :: ST T a c
  --
  -- or a transformer and a computer
  --
  -- > c1 :: ST (C u) a b   c2 :: ST T b c
  -- > -----------------------------------
  -- >      c1 >>> c2 :: ST (C u) a c
  --
  -- > c1 :: ST T a b   c2 :: ST (C u) b c
  -- > -----------------------------------
  -- >      c1 >>> c2 :: ST (C u) a c
  Par :: ParInfo -> GComp tc t a b -> GComp tc t a b -> GComp0 tc t a b

  -- | Bind a computation
  Let :: GName tc  -> GComp tc t a b -> GComp tc t a b -> GComp0 tc t a b

  -- | Bind an expression
  LetE :: GName t -> ForceInline -> GExp t b -> GComp tc t a b -> GComp0 tc t a b

  -- | Bind a mutable variable
  LetERef :: GName t -> Maybe (GExp t b) -> GComp tc t a b -> GComp0 tc t a b

  -- | Bind an expression function
  LetHeader :: GFun t b -> GComp tc t a b -> GComp0 tc t a b

  -- | Bind a computation function
  LetFunC :: GName tc
          -> [GName (CallArg t tc)] -- params (could include computation types)
          -> GComp tc t a b         -- body
          -> GComp tc t a b         -- rhs
          -> GComp0 tc t a b

  -- | Bind a struct definition
  LetStruct :: GStructDef t -> GComp tc t a b -> GComp0 tc t a b

  -- | Function call
  Call :: GName tc -> [CallArg (GExp t b) (GComp tc t a b)] -> GComp0 tc t a b

  -- | Emit a value to the output stream
  --
  -- >         e :: b
  -- > -----------------------
  -- > emit e :: ST (C ()) TVoid b
  --
  -- Since the argument to `emit` does not determine `a`, we add it an an extra
  -- parameter to `Emit`.
  Emit :: GExp t b -> GComp0 tc t a b

  -- | Emit an array of values to the output stream
  --
  -- >      e :: arr[n] b
  -- > ------------------------
  -- > emits e :: ST (C ()) TVoid b
  --
  -- Since the argument to `emits` does not determine `a`, we add it an an extra
  -- parameter to `Emit`.
  Emits :: GExp t b -> GComp0 tc t a b

  -- | Return a value
  --
  -- >         e :: u
  -- > ------------------------
  -- > return e :: ST (C u) TVoid TVoid
  --
  -- Since the argument to `return` does not determine `a` and `b` we add these
  -- as extra arguments.
  Return :: ForceInline -> GExp t b -> GComp0 tc t a b

  -- | Interleave
  --
  -- > c1 :: T a b    c2 :: T a b
  -- > --------------------------
  -- > Interleave c1 c2 :: T a b
  --
  -- TODO: We don't have source syntax for this?
  -- TODO: Not currently actually implemented in codegen?
  Interleave :: GComp tc t a b -> GComp tc t a b -> GComp0 tc t a b

  -- | Conditional
  Branch :: GExp t b -> GComp tc t a b -> GComp tc t a b -> GComp0 tc t a b

  -- | Take a value from the input stream
  --
  -- > --------------------
  -- > take :: ST (C a) a TVoid
  --
  -- Since `take` has no arguments we record both `a` and `b` as parameters.
  Take1 :: t -> GComp0 tc t a b

  -- | Take multiple values from the input stream
  --
  -- > --------------------------------
  -- > takes n :: ST (C (arr[n] a)) a TVoid
  --
  -- Since `takes` has no arguments we record both `a` and `b` as parameters.
  Take :: t -> Int -> GComp0 tc t a b

  -- | Iteration
  --
  -- > e :: Bool   c :: ST (C u) a b
  -- > -----------------------------
  -- >   until e c :: ST (C u) a b
  Until :: GExp t b -> GComp tc t a b -> GComp0 tc t a b

  -- | Iteration
  --
  -- > e :: Bool   c :: ST (C u) a b
  -- > -----------------------------
  -- >   while e c :: ST (C u) a b
  While :: GExp t b -> GComp tc t a b -> GComp0 tc t a b

  -- | Iteration
  --
  -- > e :: int<bw>   elen :: int<bw>   nm :: int<bw> |- c :: ST (C u) a b
  -- > -------------------------------------------------------------------
  -- >          times <unroll-info> e elen nm c :: ST (C u) a b
  --
  -- TODO: Replace with
  --
  -- > For :: GName ty -> GExp t b -> GExp t b -> GComp tc t a b -> GComp tc t 0 a a
  Times :: UnrollInfo -> GExp t b -> GExp t b -> GName t -> GComp tc t a b -> GComp0 tc t a b

  -- | Repeat a computer (to get a transformer)
  --
  -- >  c :: ST (C ()) a b
  -- > --------------------
  -- > repeat c :: ST T a b
  --
  -- Accepts an optional vectorization width annotation
  Repeat :: Maybe VectAnn -> GComp tc t a b -> GComp0 tc t a b

  -- | A computer annotated with vectorization width information.
  --
  -- NB: It must be a computer (not transformer).  Also notice we allow only
  -- rigid vectorization annotations here, the programmer must know what they
  -- are doing.
  VectComp :: (Int,Int) -> GComp tc t a b -> GComp0 tc t a b

  -- | Construct a transformer from a pure function
  --
  -- >    f :: a -> b
  -- > -----------------
  -- > map f :: ST T a b
  --
  -- Accepts an optional vectorization width annotation.
  Map :: Maybe VectAnn -> GName t -> GComp0 tc t a b

  -- | Filter an input stream
  --
  -- >     f :: a -> Bool
  -- > --------------------
  -- > filter f :: ST T a a
  Filter :: GName t -> GComp0 tc t a b

  -- | Read source
  --
  -- > ------------------------------
  -- > ReadSrc a :: ST T (ExtBuf a) a
  ReadSrc :: t -> GComp0 tc t a b

  -- | Write sink
  --
  -- > -------------------------------
  -- > WriteSnk a :: ST T a (ExtBuf a)
  WriteSnk :: t -> GComp0 tc t a b

  -- | Read from thread separator queue
  --
  -- > ---------------------------------
  -- > ReadInternal :: ST T (IntBuf a) a
  --
  -- Since this is polymorphic in `a` we add `a` as a parameter.
  --
  -- See Note [Standalone Reads] for `ReadType` (TODO: Where is this note?)
  ReadInternal :: t -> BufId -> ReadType -> GComp0 tc t a b

  -- | Write to thread separator queue
  --
  -- > ----------------------------------
  -- > WriteInternal :: ST T a (IntBuf a)
  --
  -- Since this is polymorphic in `a` we add `a` as a parameter.
  WriteInternal :: t -> BufId -> GComp0 tc t a b

  -- | Standalone computations (forked onto another core)
  --
  -- >      c :: ST T a b
  -- > ------------------------
  -- > standalone c :: ST T a b
  Standalone :: GComp tc t a b -> GComp0 tc t a b

  -- | Downgrade or upgrade the rate of components.
  --
  -- > n1, n2 > 1    (n2 `divides` n1 || n1 `divides` n2)
  -- > --------------------------------------------------
  -- >  Mitigate a n1 n2 :: ST T (arr[n1] a) (arr[n2] a)
  --
  -- >             n2 > 1
  -- > -------------------------------------
  -- > Mitigate a 1 n2 :: ST T a (arr[n2] a)
  --
  -- >             n1 > 1
  -- > -------------------------------------
  -- > Mitigate a n1 1 :: ST T (arr[n1] a) a
  --
  -- > --------------------------
  -- > Mitigate a 1 1 :: ST T a a
  Mitigate :: String  -- just for debugging 
           -> t -> Int -> Int -> GComp0 tc t a b

data VectAnn = Rigid Bool (Int,Int) -- True == allow mitigations up, False == disallow mitigations up
             | UpTo  Bool (Int,Int)
  deriving (Generic, Typeable, Data)


-- Call argument information
data CallArg a b
  = CAExp  { unCAExp  :: a }
  | CAComp { unCAComp :: b }
  deriving (Generic, Typeable, Data)


-- | Separate computation from expression arguments
mkHOCompSubst ::
     [(GName (CallArg t tc))]                  -- Original params
  -> [CallArg (GExp t b) (GComp tc t a b)]     -- Original arguments
  -> ( [(GName (CallArg t tc))]                -- Only expression params
     , [(GName tc, GComp tc t a b)]            -- Bindings for comp arguments
     , [CallArg (GExp t b) (GComp tc t a b)] ) -- Only expression arguments
mkHOCompSubst fprms fargs = go fprms fargs ([],[],[])
  where 
    go [] [] acc = rev_acc acc
    go (nm:nms) (arg@(CAExp _ae):args)  (eprms,cbinds,eargs)
       = go nms args (nm:eprms, cbinds, arg:eargs)
    go (nm:nms) ((CAComp ac):args) (eprms,cbinds,eargs)
       = go nms args (eprms,(to_comp_nm nm, ac):cbinds,eargs)
    go _ _ _ = panicStr "mkHOCompSubst"

    rev_acc (x,y,z) = (reverse x, reverse y, reverse z)

    to_comp_nm nm = updNameTy nm (callArg (error "mkHOCompSubst") id (nameTyp nm))


checkCAArgMut :: -- Function argument types (expected)
                 [CallArg ArgTy CTy]
                 -- Actual arguments
              -> [CallArg (GExp t b) (GComp tc t a b)] -> Bool
checkCAArgMut fun_tys args = all check_mut (zip fun_tys args)
  where 
   check_mut (CAExp (GArgTy _ Mut), CAExp earg) 
     = isJust $ isMutGDerefExp earg
   check_mut _other = True



-- A view of some Pars as a list
data GParListView tc t a b
  = ParListView { plv_loc  :: CompLoc
                , plv_nfo  :: a
                , plv_head :: GComp tc t a b
                , plv_rest :: [(ParInfo,GComp tc t a b)]
                }

data PlInfo where
  AlwaysPipeline ::    Int -- use this thread id for c1
                    -> Int -- use this thread id for c2
                    -> PlInfo
  NeverPipeline  :: PlInfo
  MaybePipeline  :: PlInfo
  deriving (Generic, Typeable, Data)

data ParInfo
  = ParInfo { plInfo     :: PlInfo
            , inBurstSz  :: Maybe Int
            , outBurstSz :: Maybe Int }
  deriving (Generic, Typeable, Data)

-- See Note [Standalone Reads]
data ReadType
  = SpinOnEmpty
  | JumpToConsumeOnEmpty
  deriving (Generic, Typeable, Data)

type CompLoc = Maybe SourcePos

data GComp tc t a b
  = MkComp { unComp   :: !(GComp0 tc t a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data GProg tc t a b
  = MkProg { progComp :: GComp tc t a b }

{-------------------------------------------------------------------------------
  Specialization of the AST to Ty (internal types)

  These types are used everywhere in the compiler except in the front-end.
-------------------------------------------------------------------------------}

type CTy   = GCTy   Ty
type CId   = GName  CTy

type Comp0 = GComp0 CTy Ty () ()
type Comp  = GComp  CTy Ty () ()
type Prog  = GProg  CTy Ty () ()

type ParListView = GParListView CTy Ty () ()

{-------------------------------------------------------------------------------
  Specializations of the AST to SrcTy (source level types)

  These types are only used in the parser and the renamer, and as input
  to the type checker (which translates from SrcTy to Ty).
-------------------------------------------------------------------------------}

data SrcCTy = SrcCTyUnknown | SrcCTyKnown (GCTy SrcTy)
  deriving (Generic, Typeable, Data)

type SrcComp = GComp SrcCTy SrcTy () ()
type SrcProg = GProg SrcCTy SrcTy () ()

{-------------------------------------------------------------------------------
  Smart constructors
-------------------------------------------------------------------------------}

mkBind :: GComp tc t a b -> (GName t, GComp tc t a b) -> GComp0 tc t a b
mkBind c1 (n,c2) = mkBindMany c1 [(n,c2)]

mkBindMany :: GComp tc t a b -> [(GName t,GComp tc t a b)] -> GComp0 tc t a b
-- First push all the bindmany's on the list
mkBindMany = go
   where
     go (MkComp (BindMany c0 c0s) _ _) c1s = mkBindMany c0 (c0s++c1s)
     -- Now we know that 'c' is not a BindMany, empty continuation: just return
     go c [] = unComp c
     -- We know that 'c' is not a BindMany, but we do have a continuation, so
     -- recurse into the continuation to flatten the continuation this time.
     go c ((n,c1):cs)
       = case mkBindMany c1 cs of
           BindMany c1' c1s' -> BindMany c ((n,c1'):c1s')
           c1' -> BindMany c [(n, MkComp c1' (compLoc c1) (compInfo c1))]

{-------------------------------------------------------------------------------
  Various map functions

  Since these are used on both source terms and internal terms they have the
  general types where possible.
-------------------------------------------------------------------------------}

-- | General form of mapping over computations
--
-- NOTE: Not binding aware.

mapCompM :: forall tc tc' t t' a a' b b' m. Monad m
         => (tc -> m tc')                  -- ^ On comp types
         -> (t -> m t')                    -- ^ On expression types
         -> (a -> m a')                    -- ^ On comp annotations
         -> (b -> m b')                    -- ^ On expression annotations
         -> (GExp t b -> m (GExp t' b'))   -- ^ On expressions
         -> (GComp tc' t' a' b' -> m (GComp tc' t' a' b')) -- ^ Combine results
         -> GComp tc t a b
         -> m (GComp tc' t' a' b')
mapCompM onCTyp onETyp onCAnn onEAnn onExp f 
  = mapCompM_env onCTyp onETyp onCAnn onEAnn onExp f (const id) (const id)

mapCompM_env :: forall tc tc' t t' a a' b b' m. Monad m
         => (tc -> m tc')                  -- ^ On comp types
         -> (t -> m t')                    -- ^ On expression types
         -> (a -> m a')                    -- ^ On comp annotations
         -> (b -> m b')                    -- ^ On expression annotations
         -> (GExp t b -> m (GExp t' b'))   -- ^ On expressions
         -> (GComp tc' t' a' b' -> m (GComp tc' t' a' b')) -- ^ Combine results
         -> (forall i. GName t -> m i -> m i)  -- extending environments
         -> (forall i. GName tc -> m i -> m i) -- extending environments
         -> GComp tc t a b
         -> m (GComp tc' t' a' b')
mapCompM_env onCTyp onETyp onCAnn onEAnn onExp f extE extC = goComp
  where
    goBind c xs_cs = do 
        c' <- goComp c
        xs_cs' <- go_binds xs_cs
        return $ mkBindMany c' xs_cs'
    
    go_binds [] = return []
    go_binds ((x,c1):xs_cs) = do
      x' <- mapNameM onETyp x
      c1' <- extE x $ goComp c1
      xs_cs' <- extE x $ go_binds xs_cs
      return $ (x',c1'):xs_cs'

    goComp :: GComp tc t a b  -> m (GComp tc' t' a' b')
    goComp MkComp{..} = do
      unComp'   <- goComp0 unComp
      compInfo' <- onCAnn compInfo
      f MkComp{unComp = unComp', compInfo = compInfo', ..}

    goComp0 :: GComp0 tc t a b  -> m (GComp0 tc' t' a' b')
    goComp0 (Var x) = do
       x' <- mapNameM onCTyp x
       return $ Var x'

    goComp0 (BindMany c1 xs_cs) = goBind c1 xs_cs

    goComp0 (Seq c1 c2) = do
      c1' <- goComp c1
      c2' <- goComp c2
      return $ Seq c1' c2'
    goComp0 (Par pi c1 c2) = do
      c1' <- goComp c1
      c2' <- goComp c2
      return $ Par pi c1' c2'
    goComp0 (Let x c1 c2) = do
      x'  <- mapNameM onCTyp x
      c1' <- goComp c1
      c2' <- extC x $ goComp c2
      return $ Let x' c1' c2'
    goComp0 (LetStruct sdef c1) = do
      sdef' <- goStructDef sdef
      c1'   <- goComp c1
      return $ LetStruct sdef' c1'
    goComp0 (LetE x fi e c1) = do
      x'  <- mapNameM onETyp x
      e'  <- onExp e
      c1' <- extE x $ goComp c1
      return $ LetE x' fi e' c1'
    goComp0 (LetERef x me c1) = do
      x' <- mapNameM onETyp x
      me' <- mapM onExp me
      c1' <- extE x $ goComp c1
      return $ LetERef x' me' c1'

    -- Header 
    goComp0 (LetHeader fun c1) = do
      let on_body prms = extParams (map CAExp prms) . onExp
      fun' <- mapFunM_env onETyp onEAnn on_body fun
      c1'  <- extFun fun $ goComp c1
      return $ LetHeader fun' c1'

    goComp0 (LetFunC nm params c1 c2) = do
      nm'     <- mapNameM onCTyp nm
      params' <- mapM (mapNameM goCallArgT) params
      c1'     <- goComp c1
      c2'     <- extParams (map paramToName params) $ 
                 extC nm (goComp c2)
      return $ LetFunC nm' params' c1' c2'

    goComp0 (Call nm args) = do
      nm'   <- mapNameM onCTyp nm
      args' <- mapM goCallArg args
      return $ Call nm' args'
    goComp0 (Emit e) = do
      e' <- onExp e
      return $ Emit e'
    goComp0 (Return fi e) = do
      e' <- onExp e
      return $ Return fi e'
    goComp0 (Emits e) = do
      e' <- onExp e
      return $ Emits e'
    goComp0 (Interleave c1 c2) = do
      c1' <- goComp c1
      c2' <- goComp c2
      return $ Interleave c1' c2'
    goComp0 (Branch e c1 c2) = do
      e'  <- onExp e
      c1' <- goComp c1
      c2' <- goComp c2
      return $ Branch e' c1' c2'
    goComp0 (Take1 a) = do
      a' <- onETyp a
      return $ Take1 a'
    goComp0 (Take a n) = do
      a' <- onETyp a
      return $ Take a' n
    goComp0 (Until e c1) = do
      e'  <- onExp e
      c1' <- goComp c1
      return $ Until e' c1'
    goComp0 (While e c1) = do
      e'  <- onExp e
      c1' <- goComp c1
      return $ While e' c1'
    goComp0 (Times ui e elen nm c1) = do
      e'    <- onExp e
      elen' <- onExp elen
      nm'   <- mapNameM onETyp nm
      c1'   <- extE nm $ goComp c1
      return $ Times ui e' elen' nm' c1'
    goComp0 (Repeat wdth c1) = do
      c1' <- goComp c1
      return $ Repeat wdth c1'
    goComp0 (VectComp wdth c1) = do
      c1' <- goComp c1
      return $ VectComp wdth c1'
    goComp0 (Map wdth nm) = do
      nm' <- mapNameM onETyp nm
      return $ Map wdth nm'
    goComp0 (Filter nm) = do
      nm' <- mapNameM onETyp nm
      return $ Filter nm'
    goComp0 (ReadSrc a) = do
      a' <- onETyp a
      return $ ReadSrc a'
    goComp0 (WriteSnk a) = do
      a' <- onETyp a
      return $ WriteSnk a'
    goComp0 (ReadInternal a bid rt) = do
      a' <- onETyp a
      return $ ReadInternal a' bid rt
    goComp0 (WriteInternal a bid) = do
      a' <- onETyp a
      return $ WriteInternal a' bid
    goComp0 (Standalone c1) = do
      c1' <- goComp c1
      return $ Standalone c1'
    goComp0 (Mitigate s t n1 n2) = do
      t' <- onETyp t
      return $ Mitigate s t' n1 n2

    extParams :: forall i. [(CallArg (GName t) (GName tc))] -> m i -> m i 
    extParams [] act = act
    extParams ((CAExp nm):prms) act  = extE nm (extParams prms act)
    extParams ((CAComp nm):prms) act = extC nm (extParams prms act)

    extFun :: forall i. GFun t b -> m i -> m i
    extFun (MkFun (MkFunDefined nm prms _) _ _) act
      = extE nm $ extParams (map CAExp prms) act
    extFun (MkFun (MkFunExternal nm _ _) _ _) act = extE nm act


    goCallArg :: CallArg (GExp t b) (GComp tc t a b) -> m (CallArg (GExp t' b') (GComp tc' t' a' b'))
    goCallArg (CAExp  e) = CAExp  `liftM` onExp e
    goCallArg (CAComp c) = CAComp `liftM` goComp c

    goCallArgT :: CallArg t tc -> m (CallArg t' tc')
    goCallArgT (CAExp  e) = CAExp  `liftM` onETyp e
    goCallArgT (CAComp c) = CAComp `liftM` onCTyp c

    goStructDef :: GStructDef t -> m (GStructDef t')
    goStructDef StructDef{..} = do
      struct_flds' <- forM struct_flds $ \(fld, t) -> do
                        t' <- onETyp t
                        return (fld, t')
      return StructDef{struct_flds = struct_flds', ..}

{-------------------------------------------------------------------------------
  Pure mapping functions
-------------------------------------------------------------------------------}

mapComp :: (tc -> tc')                  -- ^ On comp types
        -> (t -> t')                    -- ^ On expression types
        -> (a -> a')                    -- ^ On comp annotations
        -> (b -> b')                    -- ^ On expression annotations
        -> (GExp t b -> GExp t' b')     -- ^ On expressions
        -> (GComp tc' t' a' b' -> GComp tc' t' a' b') -- ^ Combine results
        -> GComp tc t a b
        -> GComp tc' t' a' b'
mapComp onCTyp onETyp onCAnn onEAnn onExp f =
    runIdentity . mapCompM (Identity . onCTyp)
                           (Identity . onETyp)
                           (Identity . onCAnn)
                           (Identity . onEAnn)
                           (Identity . onExp)
                           (Identity . f)

{-------------------------------------------------------------------------------
  Erase annotations
-------------------------------------------------------------------------------}

eraseComp :: GComp tc t a b -> GComp tc t () ()
eraseComp = mapComp id id (const ()) (const ()) eraseExp id

eraseCallArg :: CallArg (GExp t b) (GComp tc t a b) -> CallArg (GExp t ()) (GComp tc t () ())
eraseCallArg (CAExp  e) = CAExp  $ eraseExp e
eraseCallArg (CAComp c) = CAComp $ eraseComp c

{-------------------------------------------------------------------------------
  Free variables
-------------------------------------------------------------------------------}

type CompFVs tc t = (GNameSet t, GNameSet tc) -- Alread an instance of Monoid

tdeleting :: CompFVs tc t -> GName t -> CompFVs tc t
tdeleting (t,c) n = (S.delete n t, c)

cdeleting :: CompFVs tc t -> GName tc -> CompFVs tc t
cdeleting (t,c) n = (t, S.delete n c)

-- | Compute the free variables in a computation
--
-- NOTE: We collect in a bottom-up fashion, and assume that we are working with
-- a uniqued (renamed) and correctly scoped term; in other words, we assume
-- that variable names don't occur in subexpressions where they are not in
-- scope.
compFVs :: forall tc t a b. GComp tc t a b -> CompFVs tc t
compFVs = goComp
  where 
    goComp = goComp0 . unComp 

    goComp0 (Var nm)                 = (mempty,S.singleton nm)
    goComp0 (BindMany c1 xcs)        = goBind c1 xcs 
    goComp0 (Let nm c1 c2)           = goComp c1 `mappend` (goComp c2 `cdeleting` nm)
    goComp0 (LetE nm _ e c2)         = goExp e   `mappend` (goComp c2 `tdeleting` nm)
    goComp0 (LetERef nm Nothing c2)  = goComp c2 `tdeleting` nm
    goComp0 (LetERef nm (Just e) c2) = goExp e `mappend` (goComp c2 `tdeleting` nm)
    goComp0 (LetHeader fun c)        = goFun fun `mappend` (goComp c `tdeleting` funName fun)

    goComp0 (ReadSrc {})        = mempty
    goComp0 (WriteSnk {})       = mempty
    goComp0 (ReadInternal {})   = mempty
    goComp0 (WriteInternal {})  = mempty
    goComp0 (Mitigate {})       = mempty
    goComp0 (Take1 {})          = mempty
    goComp0 (Take {})           = mempty
    goComp0 (Repeat _ c)        = goComp c
    goComp0 (VectComp _ c)      = goComp c
    goComp0 (Standalone c)      = goComp c

    goComp0 (Map _v nm)         = (S.singleton nm, mempty)
    goComp0 (Filter nm)         = (S.singleton nm, mempty)

    goComp0 (Seq c1 c2)         = goComp c1 `mappend` goComp c2
    goComp0 (Par _p c1 c2)      = goComp c1 `mappend` goComp c2

    goComp0 (LetStruct _ c)     = goComp c
    goComp0 (Emit e)            = goExp e
    goComp0 (Emits e)           = goExp e
    goComp0 (Return _ e)        = goExp e
    goComp0 (Interleave c1 c2)  = goComp c1 `mappend` goComp c2
    goComp0 (Branch e c1 c2)    = goExp e `mappend ` goComp c1 `mappend` goComp c2

   
    goComp0 (Call nm args)        = (mempty,S.singleton nm) `mappend` goCallArgs args
    goComp0 (LetFunC nm ps c1 c2) = (goComp c1 `del_params` ps') `mappend` (goComp c2 `cdeleting` nm)
      where ps' = map paramToName ps

    goComp0 (Times _ui e elen nm c1) = goExp e `mappend` goExp elen `mappend` (goComp c1 `tdeleting` nm)
    goComp0 (Until e c1) = goExp e `mappend` goComp c1
    goComp0 (While e c1) = goExp e `mappend` goComp c1

    goExp e = (exprFVs e, mempty)
    goFun f = (funFVs f, mempty)
    goCallArg (CAExp e)  = goExp e
    goCallArg (CAComp c) = goComp c
    goCallArgs = mconcat . map goCallArg

    goBind c1 [] = goComp c1
    goBind c1 ((x,c2):rest) = goComp c1 `mappend` (goBind c2 rest `tdeleting` x)
    
    del_params st [] = st
    del_params st ((CAExp nm):rest)  = del_params (st `tdeleting` nm) rest
    del_params st ((CAComp nm):rest) = del_params (st `cdeleting` nm) rest


compFVs_all :: [GComp tc t a b] -> CompFVs tc t
compFVs_all = (S.unions *** S.unions) . unzip . map compFVs

compCFVs :: GComp tc t a b -> GNameSet tc
compCFVs = snd . compFVs

compEFVs :: GComp tc t a b -> GNameSet t
compEFVs = fst . compFVs

{-------------------------------------------------------------------------------
  Free _type_ variables
-------------------------------------------------------------------------------}

data TyVars = TyVars {
    tyVarsTy  :: Set TyVar
  , tyVarsCTy :: Set CTyVar
  , tyVarsLen :: Set LenVar
  , tyVarsBW  :: Set BWVar
  }

instance Monoid TyVars where
  mempty        = TyVars { tyVarsTy  = S.empty
                         , tyVarsCTy = S.empty
                         , tyVarsLen = S.empty
                         , tyVarsBW  = S.empty
                         }
  a `mappend` b = TyVars { tyVarsTy  = tyVarsTy  a `S.union` tyVarsTy  b
                         , tyVarsCTy = tyVarsCTy a `S.union` tyVarsCTy b
                         , tyVarsLen = tyVarsLen a `S.union` tyVarsLen b
                         , tyVarsBW  = tyVarsBW  a `S.union` tyVarsBW  b
                         }

-- | Are two sets of type variables entirely disjoint
tyVarsDisjoint :: TyVars -> TyVars -> Bool
tyVarsDisjoint a b = and [
      disjoint (tyVarsTy  a) (tyVarsTy  b)
    , disjoint (tyVarsCTy a) (tyVarsCTy b)
    , disjoint (tyVarsLen a) (tyVarsLen b)
    , disjoint (tyVarsBW  a) (tyVarsBW  b)
    ]
  where
    disjoint :: Ord a => Set a -> Set a -> Bool
    disjoint xs ys = S.null (xs `S.intersection` ys)

tyFVs :: Ty -> TyVars
tyFVs (TVar x)            = mempty { tyVarsTy = S.singleton x }
tyFVs TUnit               = mempty
tyFVs TBit                = mempty
tyFVs TBool               = mempty
tyFVs TString             = mempty
tyFVs (TArray n t)        = numExprFVs n `mappend` tyFVs t
tyFVs (TInt bw)           = bitWidthFVs bw
tyFVs TDouble             = mempty
tyFVs (TStruct _ flds)    = mconcat (map (tyFVs . snd) flds)
tyFVs (TInterval _)       = mempty
tyFVs (TArrow args res)   = mconcat (map tyFVs (res:(map argty_ty args)))
tyFVs (TBuff (IntBuf ty)) = tyFVs ty
tyFVs (TBuff (ExtBuf ty)) = tyFVs ty
tyFVs TVoid               = mempty


ctyFVs :: CTy -> TyVars
ctyFVs (CTVar x)          = mempty { tyVarsCTy = S.singleton x }
ctyFVs (CTComp u a b)     = mconcat (map tyFVs [u, a, b])
ctyFVs (CTTrans a b)      = mconcat (map tyFVs [a, b])
ctyFVs (CTArrow args res) = mconcat (ctyFVs res : map caFVs args)
  where
    caFVs :: CallArg ArgTy CTy -> TyVars
    caFVs = callArg (tyFVs . argty_ty) ctyFVs

numExprFVs :: NumExpr -> TyVars
numExprFVs (Literal _) = mempty
numExprFVs (NVar x)    = mempty { tyVarsLen = S.singleton x }

bitWidthFVs :: BitWidth -> TyVars
bitWidthFVs BW8  = mempty
bitWidthFVs BW16 = mempty
bitWidthFVs BW32 = mempty
bitWidthFVs BW64 = mempty
bitWidthFVs (BWUnknown x) = mempty { tyVarsBW = S.singleton x }

-- | Find all length variables in a set of types
gatherPolyVars :: [Ty] -> [LenVar]
gatherPolyVars = S.toList . tyVarsLen . mconcat . map tyFVs

{-------------------------------------------------------------------------------
  Substitution
-------------------------------------------------------------------------------}

substCTy :: [(LenVar, NumExpr)] -> CTy -> CTy
substCTy slen = goCTy
  where
    goCTy :: CTy -> CTy
    goCTy (CTVar x)          = CTVar x
    goCTy (CTComp u a b)     = CTComp (goTy u) (goTy a) (goTy b)
    goCTy (CTTrans a b)      = CTTrans (goTy a) (goTy b)
    goCTy (CTArrow args res) = CTArrow (map goCA args) (goCTy res)

    goTy :: Ty -> Ty
    goTy = substTy slen

    goCA :: CallArg ArgTy CTy -> CallArg ArgTy CTy
    goCA = callArg (\(GArgTy t mk) -> CAExp (GArgTy (goTy t) mk)) (CAComp . goCTy)

-- | Apply substitution to a computation
--
-- See comments about idempotency in `substExp`.
substComp :: [(LenVar, NumExpr)]
          -> [(GName Ty, GExp Ty b)]
          -> [(GName CTy, GComp CTy Ty a b)]
          -> GComp CTy Ty a b -> GComp CTy Ty a b
substComp slen sexp scomp arg
  = mapCompM_env on_cty on_ty (\l _ -> l) (\l _ -> l)
                 on_exp aux ext_e ext_c arg (sexp, scomp)
  where 
    on_cty x _      = substCTy slen x
    on_ty  x _      = substTy slen x
    on_exp x (es,_) = substExp slen es x
  
    ext_c nm act (s,c) = act (s, filter (\(snm,_) -> snm /= nm) c)
    ext_e nm act (s,c) = act (filter (\(snm,_) -> snm /= nm) s, c)

    aux c (_,sc)
     | Var x <- unComp c
     , Just c' <- lookup x sc = c'
     | otherwise = c

{-------------------------------------------------------------------------------
  Dealing with CallArgs
-------------------------------------------------------------------------------}

callArg :: (a -> c) -> (b -> c) -> CallArg a b -> c
callArg f _ (CAExp  a) = f a
callArg _ g (CAComp b) = g b

nameCallArgTy :: GName (CallArg a b) -> CallArg (GArgTy a) b
nameCallArgTy nm = callArg (\t -> CAExp (GArgTy t (nameMut nm))) CAComp typ
  where typ = nameTyp nm


paramToName :: GName (CallArg a b) -> CallArg (GName a) (GName b)
paramToName nm = callArg (\t  -> CAExp  nm { nameTyp = t })
                         (\ct -> CAComp nm { nameTyp = ct }) (nameTyp nm)

partitionCallArgs :: [CallArg a b] -> ([a], [b])
partitionCallArgs = partitionEithers . map (callArg Left Right)

partitionParams :: [GName (CallArg a b)] -> ([GName a], [GName b])
partitionParams = partitionEithers . map classify
  where
    classify :: GName (CallArg a b) -> Either (GName a) (GName b)
    classify nm = case nameTyp nm of
                     CAExp  t -> Left  nm{nameTyp = t}
                     CAComp t -> Right nm{nameTyp = t}

callArgExp :: CallArg a b -> Maybe a
callArgExp = callArg Just (const Nothing)

callArgComp :: CallArg a b -> Maybe b
callArgComp = callArg (const Nothing) Just

{-------------------------------------------------------------------------------
  Bindings contexts
-------------------------------------------------------------------------------}

-- | Binding context (for multiple threads)
--
-- We may have to pipeline under a shared context and CompCtxt simply records
-- this shared context. We are not really using this context in AstComp but it
-- seems the right place to define this datatype.
data GCompCtxt tc t a b
  = Hole
  | CLet       CompLoc (GName tc) (GComp tc t a b) (GCompCtxt tc t a b)
  | CLetE      CompLoc (GName t) ForceInline (GExp t b) (GCompCtxt tc t a b)
  | CLetERef   CompLoc (GName t) (Maybe (GExp t b)) (GCompCtxt tc t a b)
  | CLetHeader CompLoc (GFun t b) (GCompCtxt tc t a b)
  | CLetFunC   CompLoc (GName tc) [GName (CallArg t tc)]       -- params
                                  (GComp tc t a b)             -- body
                                  (GCompCtxt tc t a b)
  | CLetStruct CompLoc (GStructDef t) (GCompCtxt tc t a b)

type CompCtxt = GCompCtxt CTy Ty () ()

{-------------------------------------------------------------------------------
  Mutable variables
-------------------------------------------------------------------------------}

-- | Mutable variables (those declared in ELetRef and LetERef)
data GMutVar t a b = MutVar {
    mutVar  :: GName t
  , mutInit :: Maybe (GExp t b)
  , mutLoc  :: CompLoc
  , mutInfo :: a
  }
  deriving (Generic)

type MutVar = GMutVar Ty () ()

-- | Apply substitution to local variables
substLocal :: [(LenVar, NumExpr)]
           -> [(GName Ty, GExp Ty b)]
           -> GMutVar Ty a b -> GMutVar Ty a b
substLocal slen sexp (MutVar x minit loc info) = MutVar {
      mutVar  = mapName (substTy slen) x
    , mutInit = fmap (substExp slen sexp) minit
    , mutLoc  = loc
    , mutInfo = info
    }

setMutVarName :: String -> GMutVar t a b -> GMutVar t a b
setMutVarName nm mv = mv { mutVar = (mutVar mv) { name = nm } }

-- | Extract local variables from a computation
--
-- Invariant:
--
-- > uncurry insertCMutVars (extractCMutVars c) == c
extractCMutVars :: GComp tc t a b -> ([GMutVar t a b], GComp tc t a b)
extractCMutVars (MkComp (LetERef x minit c) loc info) =
    let (locals, c') = extractCMutVars c
    in (MutVar x minit loc info : locals, c')
extractCMutVars c =
    ([], c)

-- | Insert locals into a computation
--
-- See also `extractMutVars`.
insertCMutVars :: [GMutVar t a b] -> GComp tc t a b -> GComp tc t a b
insertCMutVars [] c =
    c
insertCMutVars (MutVar x minit loc info:ls) c =
    MkComp (LetERef x minit (insertCMutVars ls c)) loc info

-- | Extract locals from an expression
--
-- Invariant:
--
-- > uncurry insertCMutVars (extractCMutVars c) == c
extractEMutVars :: GExp t a -> ([GMutVar t a a], GExp t a)
extractEMutVars (MkExp (ELetRef x minit e) loc info) =
    let (locals, e') = extractEMutVars e
    in (MutVar x minit loc info : locals, e')
extractEMutVars e =
    ([], e)

-- | Insert locals into an expression
--
-- See also `extractMutVars`.
--
-- NOTE: Locals extracted from a computation can be inserted into an
-- expression provided that the types of the labels matches.
insertEMutVars :: [GMutVar t a a] -> GExp t a -> GExp t a
insertEMutVars [] e =
    e
insertEMutVars (MutVar x minit loc info:ls) e =
    MkExp (ELetRef x minit (insertEMutVars ls e)) loc info

-- | Equivalent of `extractCMutVars` for `GCompCtxt`
--
-- NOTE: Since we don't record labels in GCompCtxt (TODO: why not?) the labels
-- on the locals are unit.
extractCtxtMutVars :: GCompCtxt tc t a b -> ([GMutVar t () b], GCompCtxt tc t a b)
extractCtxtMutVars (CLetERef loc x minit ctxt) =
    let (locals, ctxt') = extractCtxtMutVars ctxt
    in (MutVar x minit loc () : locals, ctxt')
extractCtxtMutVars ctxt =
    ([], ctxt)

-- | Inverse of `extractCtxtMutVars`
insertCtxtMutVars :: [GMutVar t () b] -> GCompCtxt tc t a b -> GCompCtxt tc t a b
insertCtxtMutVars [] ctxt =
    ctxt
insertCtxtMutVars (MutVar x minit loc ():ls) ctxt =
    CLetERef loc x minit (insertCtxtMutVars ls ctxt)

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

isMaybeOrNever :: PlInfo -> Bool
isMaybeOrNever nfo =
  case nfo of
    AlwaysPipeline _ _ -> False
    NeverPipeline      -> True
    MaybePipeline      -> True


mkParInfo :: PlInfo -> ParInfo
mkParInfo nfo = ParInfo nfo Nothing Nothing

pnever :: ParInfo
pnever = mkParInfo NeverPipeline



parsToParList :: GComp tc t a b -> GParListView tc t a b
parsToParList c
  = ParListView { plv_loc  = compLoc c
                , plv_nfo  = compInfo c
                , plv_head = cfst
                , plv_rest = crest
                }
  where (cfst,crest) = go c
        go :: GComp tc t a b -> (GComp tc t a b, [(ParInfo, GComp tc t a b)])
        go (MkComp (Par p c1 c2) _loc _nfo)
          = let (c1fst,c1rest) = go c1
                (c2fst,c2rest) = go c2
            in (c1fst, c1rest ++ (p,c2fst) : c2rest)
        go cother = (cother, [])

readJumpToConsumeOnEmpty :: ReadType -> Bool
readJumpToConsumeOnEmpty JumpToConsumeOnEmpty = True
readJumpToConsumeOnEmpty _ = False


compShortName :: GComp tc t a b -> String
compShortName = go . unComp
  where
    go (Var n             ) = "Var(" ++ name n ++ ")"
    go (BindMany        {}) = "BindMany"
    go (Seq             {}) = "Seq"
    go (Par             {}) = "Par"
    go (Let             {}) = "Let"
    go (LetE            {}) = "LetE"
    go (LetERef         {}) = "LetERef"
    go (LetHeader fun _   ) = "LetHeader(" ++ name (funName fun) ++ ")"
    go (LetFunC nm _ _ _  ) = "LetFunC(" ++ name nm ++ ")"
    go (LetStruct       {}) = "Struct"
    go (Call n _          ) = "Call(" ++ name n ++ ")"
    go (Emit            {}) = "Emit"
    go (Emits           {}) = "Emits"
    go (Return          {}) = "Return"
    go (Interleave      {}) = "Interleave"
    go (Branch          {}) = "Branch"
    go (Take1           {}) = "Take1"
    go (Take            {}) = "Take"
    go (Until           {}) = "Until"
    go (While           {}) = "While"
    go (Times           {}) = "Times"
    go (Repeat          {}) = "Repeat"
    go (VectComp        {}) = "VectComp"
    go (Map             {}) = "Map"
    go (Filter          {}) = "Filter"
    go (ReadSrc         {}) = "ReadSrc"
    go (WriteSnk        {}) = "WriteSnk"
    go (ReadInternal    {}) = "ReadInternal"
    go (WriteInternal   {}) = "WriteInternal"
    go (Standalone      {}) = "Standalone"
    go (Mitigate        {}) = "Mitigate"

-- | Type of the input stream
--
-- Panics if it's not a computer or transformer
inTyOfCTy :: CTy -> Ty
inTyOfCTy (CTComp _ x _) = x
inTyOfCTy (CTTrans  x _) = x
inTyOfCTy _ = panicStr "inTyOfCTy: not a computer or transformer"

-- | Type of the output stream
--
-- Panics if it's not a computer or transformer
yldTyOfCTy  :: CTy -> Ty
yldTyOfCTy (CTComp _ _ x) = x
yldTyOfCTy (CTTrans  _ x) = x
yldTyOfCTy _ = panicStr "yldTyOfCTy: not a compute or transformer"

-- | Result type (if it's a computer) or Nothing if it's a transformer
--
-- Panics if it's not a computer or transformer
doneTyOfCTy :: CTy -> Maybe Ty
doneTyOfCTy (CTComp x _ _) = Just x
doneTyOfCTy (CTTrans  _ _) = Nothing
doneTyOfCTy _ = panicStr "doneTyOfCTy: not a computer or transformer"

-- | Check if something is a computer or transformer
--
-- Panics when it sees a type variable
isCompOrTrans :: CTy -> Bool
isCompOrTrans (CTComp  {}) = True
isCompOrTrans (CTTrans {}) = True
isCompOrTrans (CTArrow {}) = False
isCompOrTrans (CTVar   {}) = panicStr "isCompOrTrans: type variable"

-- | Check if something is a computer
--
-- Panics when it sees a type variable
isComputer :: CTy -> Bool
isComputer (CTComp  {}) = True
isComputer (CTTrans {}) = False
isComputer (CTArrow {}) = False
isComputer (CTVar   {}) = panicStr "isComputer: type variable"


toComp :: a -> GComp0 tc t a b -> GComp tc t a b
toComp a c0 = MkComp c0 Nothing a

toCompPos :: a -> SourcePos -> GComp0 tc t a b -> GComp tc t a b
toCompPos a pos c0 = MkComp c0 (Just pos) a




-- TODO: The cases for Repeat, VectComp, Interleave and Standalone look
-- suspicious? Why no +1? Fix or document.
compSize :: GComp tc t a b -> Int
compSize c = case unComp c of
  Var _nm                   -> 1
  BindMany c1 xs_cs         -> foldr (\(_x,c') _s -> compSize c') (compSize c1) xs_cs
  Seq c1 c2                 -> 1 + compSize c1 + compSize c2
  Par _ c1 c2               -> 1 + compSize c1 + compSize c2
  Let _nm c1 c2             -> 1 + compSize c1 + compSize c2
  LetE _nm _ _e c1          -> 2 + compSize c1
  LetERef _nm (Just _) c1   -> 2 + compSize c1
  LetERef _nm Nothing  c1   -> 2 + compSize c1
  LetHeader _f c1           -> 2 + compSize c1
  LetStruct _sdef c1        -> 1 + compSize c1
  LetFunC _nm _params c1 c2 -> 1 + compSize c1 + compSize c2
  Call _nm es               -> 1 + sum (map callArgSize es)
  Emit {}                   -> 1
  Emits {}                  -> 1
  Return {}                 -> 1
  Branch _e c1 c2           -> 1 + compSize c1 + compSize c2
  Take1 {}                  -> 1
  Take {}                   -> 1
  Until _e c1               -> 1 + compSize c1
  While _e c1               -> 1 + compSize c1
  Times _ui _e1 _e2 _nm c1  -> 1 + compSize c1
  Repeat _ c1               -> compSize c1
  VectComp _ c1             -> compSize c1
  Map _ _nm                 -> 1
  Filter _e                 -> 1
  Interleave c1 c2          -> compSize c1 + compSize c2
  ReadSrc  {}               -> 1
  WriteSnk {}               -> 1
  ReadInternal  {}          -> 1
  WriteInternal {}          -> 1
  Standalone c1             -> compSize c1
  Mitigate {}               -> 1

callArgSize :: CallArg (GExp t b) (GComp tc t a b) -> Int
callArgSize (CAExp _)  = 0
callArgSize (CAComp _) = 1

{-------------------------------------------------------------------------------
  PrettyVal instances (used for dumping the AST)
-------------------------------------------------------------------------------}

instance PrettyVal ParInfo
instance PrettyVal PlInfo
instance PrettyVal ReadType
instance PrettyVal VectAnn

instance PrettyVal t => PrettyVal (GCTy t)

instance (PrettyVal a, PrettyVal b) => PrettyVal (CallArg a b)


-- instance (PrettyVal t, PrettyVal a, PrettyVal b) => PrettyVal (GMutVar t a b)
-- instance (PrettyVal tc, PrettyVal t, PrettyVal a, PrettyVal b) => PrettyVal (GComp0 tc t a b)
-- instance (PrettyVal tc, PrettyVal t, PrettyVal a, PrettyVal b) => PrettyVal (GComp tc t a b)
-- instance (PrettyVal tc, PrettyVal t, PrettyVal a, PrettyVal b) => PrettyVal (GProg tc t a b)

instance PrettyVal SrcCTy

-- Note [Standalone reads]
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- Reads arising from standalone pipelining are slightly problematic.
-- Normally, if a read from a thread-separator cannot be executed because
-- the queue is empty we could return SKIP and jump to this threads main
-- control loop. That's all very fine.
--
-- *Except* when this thread is the main thread (i.e. /not/ a standalone).
-- Example:
--    main thread: read >>> c1 >>> write(s) >>> read(q) >>> c2 >>> write
--    standalone : read(s) >>> s >>> write(q)
--
-- If the 'read(q)' returns SKIP and jumps back to the main threads
-- loop, next time around we will ask again to read(q), which will
-- also be empty.  Our only chance of making progress, is to jump to
-- the *consume* continuation which is the consume continuation of the
-- write(s) which will eventually cause the 's' queue to be filled in,
-- give a chance to the standalone thread to write to 'q', and then
-- give a chance to the main thread to read(q) successfully!
--
--
-- Hence, we pass a flag down to ReadInternal that will allow us to choose
-- one of the two behaviours for reading.
--
-- TODO: We might want to revisit this design later, or even deprecate the
-- standalone pipelining which does not perform great anyway, but at the time
-- of the writing of this comment (10/01/2014), this is not done.
