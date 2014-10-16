{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RebindableSyntax, FlexibleInstances, MultiParamTypeClasses #-}
-- | Free monads for convenient construction of AST terms
--
-- NOTE: Importing modules will probably want to enable the `RebindableSyntax`
-- language extension and import `Rebindables` to get if-then-else syntax.
module AstFreeMonad (
    -- * Free expression monad
    FreeExp -- opaque
  , feVal
  , feVar
  , feBinOp
  , feArrRead
  , feAssign
  , feError
  , feAnnot
  , feLift
    -- * Convenience wrappers for the expression monad
  , ToFreeExp(..)
  , (.:=)
  , (.*)
  , (./)
  , (.%)
  , (.=)
  , (.<)
  , (.<=)
  , (.+)
  , (.-)
  , (.::)
  , XRange(..)
  , (.!)
    -- * Free computation monad
  , FreeComp -- opaque
  , fVar
  , fLetE
  , fLetERef
  , fBind
  , fTake1
  , fTake
  , fEmit
  , fEmits
  , fReturn
  , fBranch
  , fTimes
  , fRepeat
  , fError
  , fLift
    -- ** Type annotations on computations
  , returning
  , taking
  , emitting
    -- * Translation out of the free monad
  , unFreeExp
  , unFreeComp
  ) where

import Prelude
import Control.Applicative
import Control.Monad (liftM, ap, (>=>))
import Data.Maybe (fromJust)
import Data.Foldable (forM_)
import Text.Parsec.Pos (SourcePos)

import AstComp hiding (GComp0(..))
import AstExpr hiding (GExp0(..))
import AstUnlabelled
import CtComp (ctComp)
import CtExpr (ctExp)
import Rebindables
import TcMonad (TcM)
import TcUnify (unify)
import TcErrors (ErrCtx(InternalTypeChecking))
import qualified GenSym  as GS
import qualified TcMonad as TcM

{-------------------------------------------------------------------------------
  Free expression monad
-------------------------------------------------------------------------------}

data FreeExp a =
    EVal Val (FreeExp a)
  | EVar (GName Ty) (FreeExp a)
  | EBinOp BinOp (FreeExp ()) (FreeExp ()) (FreeExp a)
  | EArrRead (FreeExp ()) (FreeExp ()) LengthInfo (FreeExp a)
  | EArrWrite (FreeExp ()) (FreeExp ()) LengthInfo (FreeExp ()) (FreeExp a)
  | EAssign (FreeExp ()) (FreeExp ()) (FreeExp a)
  | EError String (FreeExp a)
  | ELift Exp (FreeExp a)
  | EAnnot (FreeExp ()) Ty (FreeExp a)
  | EPure a

instance Functor FreeExp where
  fmap = liftM

instance Applicative FreeExp where
  pure  = return
  (<*>) = ap

instance Monad FreeExp where
  return = EPure
  EVal v                k >>= f = EVal v                (k >>= f)
  EVar x                k >>= f = EVar x                (k >>= f)
  EBinOp op e1 e2       k >>= f = EBinOp op e1 e2       (k >>= f)
  EArrRead arr e li     k >>= f = EArrRead arr e li     (k >>= f)
  EArrWrite arr e li e' k >>= f = EArrWrite arr e li e' (k >>= f)
  EAssign x e           k >>= f = EAssign x e           (k >>= f)
  EError str            k >>= f = EError str            (k >>= f)
  EAnnot e ty           k >>= f = EAnnot e ty           (k >>= f)
  ELift e               k >>= f = ELift e               (k >>= f)
  EPure a                 >>= f = f a

feVal :: Val -> FreeExp ()
feVal v = EVal v (EPure ())

feVar :: GName Ty -> FreeExp ()
feVar x = EVar x (EPure ())

feBinOp :: BinOp -> FreeExp () -> FreeExp () -> FreeExp ()
feBinOp op e1 e2 = EBinOp op e1 e2 (EPure ())

feArrRead :: FreeExp () -> FreeExp () -> LengthInfo -> FreeExp ()
feArrRead arr estart li = EArrRead arr estart li (EPure ())

feArrWrite :: FreeExp () -> FreeExp () -> LengthInfo -> FreeExp () -> FreeExp ()
feArrWrite arr estart li e = EArrWrite arr estart li e (EPure ())

feAssign :: FreeExp () -> FreeExp () -> FreeExp ()
feAssign x e = EAssign x e (EPure ())

feError :: String -> FreeExp ()
feError str = EError str (EPure ())

-- | Annotate an expression with a type (@e :: ty@)
feAnnot :: FreeExp () -> Ty -> FreeExp ()
feAnnot e ty = EAnnot e ty (EPure ())

feLift :: Exp -> FreeExp ()
feLift e = ELift e (EPure ())

{-------------------------------------------------------------------------------
  Some convenience wrappers around the free expressions
-------------------------------------------------------------------------------}

class ToFreeExp a where
  toFreeExp :: a -> FreeExp ()

instance ToFreeExp (FreeExp ()) where
  toFreeExp = id

instance ToFreeExp (GName Ty) where
  toFreeExp = feVar

instance ToFreeExp Int where
  toFreeExp = feVal . VInt . fromIntegral

instance ToFreeExp Bool where
  toFreeExp = feVal . VBool

instance ToFreeExp () where
  toFreeExp _ = feVal VUnit

instance ToFreeExp Exp where
  toFreeExp = feLift

-- | Write to a variable _or_ to an array
(.:=) :: (ToFreeExp x, ToFreeExp e) => x -> e -> FreeExp ()
(.:=) x e = case toFreeExp x of
  EArrRead earr start len (EPure ()) -> feArrWrite earr start len (toFreeExp e)
  _otherwise                         -> feAssign (toFreeExp x)    (toFreeExp e)

(.*) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.*) e1 e2 = feBinOp Mult (toFreeExp e1) (toFreeExp e2)

(./) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(./) e1 e2 = feBinOp Div (toFreeExp e1) (toFreeExp e2)

(.%) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.%) e1 e2 = feBinOp Rem (toFreeExp e1) (toFreeExp e2)

(.=) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.=) e1 e2 = feBinOp Eq (toFreeExp e1) (toFreeExp e2)

(.<) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.<) e1 e2 = feBinOp Lt (toFreeExp e1) (toFreeExp e2)

(.<=) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.<=) e1 e2 = feBinOp Leq (toFreeExp e1) (toFreeExp e2)

(.+) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.+) e1 e2 = feBinOp Add (toFreeExp e1) (toFreeExp e2)

(.-) :: (ToFreeExp e1, ToFreeExp e2) => e1 -> e2 -> FreeExp ()
(.-) e1 e2 = feBinOp Add (toFreeExp e1) (toFreeExp e2)

(.::) :: ToFreeExp e => e -> Ty -> FreeExp ()
(.::) e ty = feAnnot (toFreeExp e) ty

infix  1 .:=
infix  2 .::

-- Precedence follows Haskell conventions

infixl 7 .*
infixl 7 ./
infixl 7 .%
infix  4 .=
infix  4 .<
infix  4 .<=
infixl 6 .+
infixl 6 .-

{-------------------------------------------------------------------------------
  XRange
-------------------------------------------------------------------------------}

class XRange a where
 toRange :: a -> (FreeExp (), LengthInfo)

instance XRange Int where
  toRange a = (toFreeExp a, LISingleton)

instance XRange (GName Ty) where
  toRange a = (toFreeExp a, LISingleton)

instance ToFreeExp a => XRange (a, Int) where
  toRange (a,b) = (toFreeExp a, LILength b)

(.!) :: (ToFreeExp arr, XRange y) => arr -> y -> FreeExp ()
(.!) earr y = let (xe, li) = toRange y in feArrRead (toFreeExp earr) xe li

{-------------------------------------------------------------------------------
  Free computation monad
-------------------------------------------------------------------------------}

-- | Free computation monad
--
-- NOTE: The cases for Bind, LetE and LetERef scope the variable over the
-- _entire_ remaining computation; i.e., something like
--
-- > let e = expr in (c1 ; c2)
--
-- It is not possible to construct something of the form
--
-- > (let e = expr in c1) ; c2
--
-- If that is undesirable we have to give both constructors an additional
-- continuation argument (similar to Times), but then they would be less
-- convenient to use.
data FreeComp a =
    Var (GName CTy) (FreeComp a)
  | Bind (FreeComp ()) (GName Ty -> FreeComp a)
  | LetE ForceInline (FreeExp ()) (GName Ty -> FreeComp a)
  | LetERef (Either (FreeExp ()) Ty) (GName Ty -> FreeComp a)
  | Take1 (FreeComp a)
  | Take Int (FreeComp a)
  | Emit (FreeExp ()) (FreeComp a)
  | Emits (FreeExp ()) (FreeComp a)
  | Return ForceInline (FreeExp ()) (FreeComp a)
  | Branch (FreeExp ()) (FreeComp ()) (FreeComp ()) (FreeComp a)
  | Times UnrollInfo (FreeExp ()) (FreeExp ()) (GName Ty -> FreeComp ()) (FreeComp a)
  | Repeat (Maybe VectAnn) (FreeComp ()) (FreeComp a)
  | Annot (FreeComp ()) (Maybe Ty) (Maybe Ty) (Maybe Ty) (FreeComp a)
  | Lift Comp (FreeComp a)
  | Pure a

instance Functor FreeComp where
  fmap = liftM

instance Applicative FreeComp where
  pure  = return
  (<*>) = ap

instance Monad FreeComp where
  return = Pure
  Var x            k >>= f = Var x            (k >>= f)
  LetE fi e        k >>= f = LetE fi e        (k >=> f)
  LetERef e        k >>= f = LetERef e        (k >=> f)
  Bind c           k >>= f = Bind c           (k >=> f)
  Take1            k >>= f = Take1            (k >>= f)
  Take n           k >>= f = Take n           (k >>= f)
  Emit e           k >>= f = Emit e           (k >>= f)
  Emits e          k >>= f = Emits e          (k >>= f)
  Return fi e      k >>= f = Return fi e      (k >>= f)
  Branch e c1 c2   k >>= f = Branch e c1 c2   (k >>= f)
  Times ui e1 e2 b k >>= f = Times ui e1 e2 b (k >>= f)
  Repeat ann c     k >>= f = Repeat ann c     (k >>= f)
  Annot c u a b    k >>= f = Annot c u a b    (k >>= f)
  Lift c           k >>= f = Lift c           (k >>= f)
  Pure a             >>= f = f a

fVar :: GName CTy -> FreeComp ()
fVar x = Var x (Pure ())

fLetE :: ForceInline -> FreeExp () -> FreeComp (GName Ty)
fLetE fi e = LetE fi e Pure

fLetERef :: Either (FreeExp ()) Ty -> FreeComp (GName Ty)
fLetERef e = LetERef e Pure

fBind :: FreeComp () -> FreeComp (GName Ty)
fBind c = Bind c Pure

fTake1 :: FreeComp ()
fTake1 = Take1 (Pure ())

fTake :: Int -> FreeComp (GName Ty)
fTake n = fBind (Take n (Pure ()))

fEmit :: FreeExp () -> FreeComp ()
fEmit e = Emit e (Pure ())

fEmits :: FreeExp () -> FreeComp ()
fEmits e = Emits e (Pure ())

fReturn :: ToFreeExp e => ForceInline -> e -> FreeComp ()
fReturn fi e = Return fi (toFreeExp e) (Pure ())

fBranch :: FreeExp () -> FreeComp () -> FreeComp () -> FreeComp ()
fBranch e c1 c2  = Branch e c1 c2 (Pure ())

fTimes :: UnrollInfo -> FreeExp () -> FreeExp () -> (GName Ty -> FreeComp ()) -> FreeComp ()
fTimes ui estart elen body = Times ui estart elen body (Pure ())

fRepeat :: Maybe VectAnn -> FreeComp () -> FreeComp ()
fRepeat ann body = Repeat ann body (Pure ())

fError :: String -> FreeComp ()
fError str = fReturn AutoInline (feError str)

fLift :: Comp -> FreeComp ()
fLift c = Lift c (Pure ())

instance IfThenElse (FreeExp ()) (FreeComp ()) where
  ifThenElse = fBranch

{-------------------------------------------------------------------------------
  Type annotations on computations
-------------------------------------------------------------------------------}

-- | Annotate the "done type" of a computation
returning :: FreeComp () -> Ty -> FreeComp ()
returning c ty = Annot c (Just ty) Nothing Nothing (Pure ())

-- | Annotate the type of the input source
taking :: FreeComp () -> Ty -> FreeComp ()
taking c ty = Annot c Nothing (Just ty) Nothing (Pure ())

-- | Annotate the type of the output source
emitting :: FreeComp () -> Ty -> FreeComp ()
emitting c ty = Annot c Nothing Nothing (Just ty) (Pure ())

{-------------------------------------------------------------------------------
  Translate back to normal computations
-------------------------------------------------------------------------------}

unFreeExp :: Maybe SourcePos -> FreeExp () -> TcM Exp
unFreeExp loc e = TcM.zonkExpr =<< unEFree loc e

unFreeComp :: Maybe SourcePos -> FreeComp () -> TcM Comp
unFreeComp loc c = TcM.zonkComp =<< unFree loc c

unEFree :: Maybe SourcePos -> FreeExp () -> TcM Exp
unEFree loc = liftM fromJust . go
  where
    go :: FreeExp () -> TcM (Maybe Exp)
    go (EVal v k) = do
      a  <- newTyVar
      k' <- go k
      return $ eVal loc a v `mSeq` k'
    go (EVar nm k) = do
      k' <- go k
      return $ eVar loc nm `mSeq` k'
    go (EBinOp op e1 e2 k) = do
      Just e1' <- go e1
      Just e2' <- go e2
      k'       <- go k
      return $ eBinOp loc op e1' e2' `mSeq` k'
    go (EArrRead arr estart li k) = do
      Just arr'    <- go arr
      Just estart' <- go estart
      k'           <- go k
      return $ eArrRead loc arr' estart' li `mSeq` k'
    go (EArrWrite arr estart li e k) = do
      Just arr'    <- go arr
      Just estart' <- go estart
      Just e'      <- go e
      k'           <- go k
      return $ eArrWrite loc arr' estart' li e' `mSeq` k'
    go (EAssign x e k) = do
      Just x' <- go x
      Just e' <- go e
      k'      <- go k
      return $ eAssign loc x' e' `mSeq` k'
    go (EError str k) = do
      a  <- newTyVar
      k' <- go k
      return $ eError loc a str `mSeq` k'
    go (EAnnot e ty k) = do
      Just e' <- go e
      k'      <- go k
      unify loc (ctExp e') ty
      return $ e' `mSeq` k'
    go (ELift e k) = do
      k' <- go k
      return $ e `mSeq` k'
    go (EPure ()) =
      return Nothing

    mSeq :: Exp -> Maybe Exp -> Maybe Exp
    mSeq e Nothing   = Just $ e
    mSeq e (Just e') = Just $ eSeq loc e e'

unFree :: Maybe SourcePos -> FreeComp () -> TcM Comp
unFree loc = liftM fromJust . go
  where
    go :: FreeComp () -> TcM (Maybe Comp)
    go (Var x k) = do
      k' <- go k
      return $ cVar loc x `mSeq` k'
    go (LetE fi e k) = do
      e'      <- unEFree loc e
      nm      <- newName loc (ctExp e')
      Just k' <- go (k nm)
      return $ Just $ cLetE loc nm fi e' k'
    go (LetERef (Left e) k) = do
      e'      <- unEFree loc e
      nm      <- newName loc (ctExp e')
      Just k' <- go (k nm)
      return $ Just $ cLetERef loc nm (Just e') k'
    go (LetERef (Right ty) k) = do
      nm      <- newName loc ty
      Just k' <- go (k nm)
      return $ Just $ cLetERef loc nm Nothing k'
    go (Bind c k) = do
      Just c' <- go c
      nm      <- newName loc (fromJust . doneTyOfCTyBase . ctComp $ c')
      Just k' <- go (k nm)
      return $ Just $ cBindMany loc c' [(nm, k')]
    go (Take1 k) = do
      a  <- newTyVar
      b  <- newTyVar
      k' <- go k
      return $ cTake1 loc a b `mSeq` k'
    go (Take n k) = do
      a  <- newTyVar
      b  <- newTyVar
      k' <- go k
      return $ cTake loc a b n `mSeq` k'
    go (Emit e k) = do
      e' <- unEFree loc e
      a  <- newTyVar
      k' <- go k
      return $ cEmit loc a e' `mSeq` k'
    go (Emits e k) = do
      e' <- unEFree loc e
      a  <- newTyVar
      k' <- go k
      return $ cEmits loc a e' `mSeq` k'
    go (Return fi e k) = do
      e' <- unEFree loc e
      a  <- newTyVar
      b  <- newTyVar
      k' <- go k
      return $ cReturn loc a b fi e' `mSeq` k'
    go (Branch e c1 c2 k) = do
      e'       <- unEFree loc e
      Just c1' <- go c1
      Just c2' <- go c2
      k'       <- go k
      return $ cBranch loc e' c1' c2' `mSeq` k'
    go (Times ui estart elen body k) = do
      estart'    <- unEFree loc estart
      elen'      <- unEFree loc elen
      nm         <- newName loc (ctExp estart')
      Just body' <- go (body nm)
      k'         <- go k
      return $ cTimes loc ui estart' elen' nm body' `mSeq` k'
    go (Repeat ann body k) = do
      Just body' <- go body
      k'         <- go k
      return $ cRepeat loc ann body' `mSeq` k'
    go (Annot c mu ma mb k) = do
      Just c' <- go c
      let CTBase cty0 = ctComp c'
      forM_ mu $ unify loc (fromJust (doneTyOfCTy cty0))
      forM_ ma $ unify loc ( inTyOfCTy cty0)
      forM_ mb $ unify loc (yldTyOfCTy cty0)
      k'      <- go k
      return $ c' `mSeq` k'
    go (Lift c k) = do
      k' <- go k
      return $ c `mSeq` k'
    go (Pure ()) =
      return Nothing

    mSeq :: Comp -> Maybe Comp -> Maybe Comp
    mSeq c Nothing   = Just $ c
    mSeq c (Just c') = Just $ cSeq loc c c'

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

newName :: Maybe SourcePos -> Ty -> TcM (GName Ty)
newName loc ty = do
  str <- TcM.genSym "_x"
  return $ toName ("_x" ++ str) loc ty

newTyVar :: TcM Ty
newTyVar = TVar <$> TcM.newTyVar "_a"

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

_test' :: IO ()
_test' = do
  -- TODO: I'm assigning incorrect types to these variables. I'm not sure
  -- what the actual ziria snippet in _test is doing.
  let tmp_idx     = toName "tmp_idx"     Nothing tint32
      is_empty    = toName "is_empty"    Nothing TBool
      in_buff     = toName "in_buff"     Nothing tint32
      in_buff_idx = toName "in_buff_idx" Nothing tint32
      c           = _test tmp_idx is_empty in_buff in_buff_idx 1 2
  sym <- GS.initGenSym ""
  mc  <- TcM.runTcM (unFreeComp Nothing c)
                    (TcM.mkTyDefEnv [])
                    (TcM.mkEnv      [])
                    (TcM.mkCEnv     [])
                    sym
                    InternalTypeChecking
                    TcM.emptyTcMState
  case mc of
    Left err      -> print err
    Right (c', _) -> print c'

_test :: GName Ty -> GName Ty -> GName Ty -> GName Ty -> Int -> Int -> FreeComp ()
_test tmp_idx is_empty in_buff in_buff_idx finalin n0 = do
  if is_empty .= True
    then do x <- fBind (fTake1 `returning` tint32)
            fReturn AutoInline $ do
              in_buff     .:= x
              is_empty    .:= False
              in_buff_idx .:= (0 :: Int)
    else fReturn AutoInline ()
  if finalin .= in_buff_idx .+ n0
    then do fReturn ForceInline $ do
              is_empty .:= True
              in_buff  .! (in_buff_idx, n0)
    else if in_buff_idx .+ n0 .< finalin
           then fReturn ForceInline $ do
                  tmp_idx     .:= in_buff_idx
                  in_buff_idx .:= in_buff_idx .+ n0
                  in_buff .! (tmp_idx, n0)
           else -- The only reason this is an error is the lack of memcpy as a
                -- primitive but probably we will not encounter any such
                -- misaligned annotations.
                do -- Just a dummy return to make code generator happy ...
                   -- Fix this at some point by providing a proper
                   -- computer-level error function
                   _u <- fBind (fError "rewrite_take: unaligned!")
                   fReturn ForceInline $ in_buff .! (0 :: Int, n0)
  -- This wasn't in the original example. Just testing array assignment
  fReturn AutoInline $ in_buff .! (in_buff_idx, n0) .:= (123 :: Int)
