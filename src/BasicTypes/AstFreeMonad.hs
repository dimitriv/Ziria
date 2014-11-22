{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RebindableSyntax, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes #-}
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
  , fLiftSrc
    -- ** Type annotations on computations
  , returning
  , taking
  , emitting
    -- * Translation out of the free monad
  , unFreeExp
  , unFreeComp
--  , _test'
  ) where

import Prelude
import Control.Applicative
import Control.Exception
import Control.Monad hiding (forM_)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid
import Data.Foldable (forM_)
import Text.Parsec.Pos
import qualified Data.Set as S

import AstComp
import AstExpr
import AstUnlabelled
import CtComp (ctComp)
import CtExpr (ctExp)
import Lint (lint)
import Rebindables
import TcMonad
import TcUnify (unify)
import Typecheck (tyCheckComp)
import qualified GenSym as GS

-- Imports for the example only:
-- import AstQuasiQuote (zcomp)

{-------------------------------------------------------------------------------
  Free expression monad
-------------------------------------------------------------------------------}

data FreeExp a =
    FEVal Val (FreeExp a)
  | FEVar (GName Ty) (FreeExp a)
  | FEBinOp BinOp (FreeExp ()) (FreeExp ()) (FreeExp a)
  | FEArrRead (FreeExp ()) (FreeExp ()) LengthInfo (FreeExp a)
  | FEArrWrite (FreeExp ()) (FreeExp ()) LengthInfo (FreeExp ()) (FreeExp a)
  | FEAssign (FreeExp ()) (FreeExp ()) (FreeExp a)
  | FEError String (FreeExp a)
  | FELift Exp (FreeExp a)
  | FEAnnot (FreeExp ()) Ty (FreeExp a)
  | FEPure a

instance Functor FreeExp where
  fmap = liftM

instance Applicative FreeExp where
  pure  = return
  (<*>) = ap

instance Monad FreeExp where
  return = FEPure
  FEVal v                k >>= f = FEVal v                (k >>= f)
  FEVar x                k >>= f = FEVar x                (k >>= f)
  FEBinOp op e1 e2       k >>= f = FEBinOp op e1 e2       (k >>= f)
  FEArrRead arr e li     k >>= f = FEArrRead arr e li     (k >>= f)
  FEArrWrite arr e li e' k >>= f = FEArrWrite arr e li e' (k >>= f)
  FEAssign x e           k >>= f = FEAssign x e           (k >>= f)
  FEError str            k >>= f = FEError str            (k >>= f)
  FEAnnot e ty           k >>= f = FEAnnot e ty           (k >>= f)
  FELift e               k >>= f = FELift e               (k >>= f)
  FEPure a                 >>= f = f a

feVal :: Val -> FreeExp ()
feVal v = FEVal v (FEPure ())

feVar :: GName Ty -> FreeExp ()
feVar x = FEVar x (FEPure ())

feBinOp :: BinOp -> FreeExp () -> FreeExp () -> FreeExp ()
feBinOp op e1 e2 = FEBinOp op e1 e2 (FEPure ())

feArrRead :: FreeExp () -> FreeExp () -> LengthInfo -> FreeExp ()
feArrRead arr estart li = FEArrRead arr estart li (FEPure ())

feArrWrite :: FreeExp () -> FreeExp () -> LengthInfo -> FreeExp () -> FreeExp ()
feArrWrite arr estart li e = FEArrWrite arr estart li e (FEPure ())

feAssign :: FreeExp () -> FreeExp () -> FreeExp ()
feAssign x e = FEAssign x e (FEPure ())

feError :: String -> FreeExp ()
feError str = FEError str (FEPure ())

-- | Annotate an expression with a type (@e :: ty@)
feAnnot :: FreeExp () -> Ty -> FreeExp ()
feAnnot e ty = FEAnnot e ty (FEPure ())

feLift :: Exp -> FreeExp ()
feLift e = FELift e (FEPure ())

{-------------------------------------------------------------------------------
  Some convenience wrappers around the free expressions
-------------------------------------------------------------------------------}

-- | Overloading of free expressions
--
-- NOTE: We do NOT support overloading for actual Haskell values. We used to
-- have `ToFreeExp` instances for `Int`, `Bool` and `()`, but the instance for
-- `Int` is not particularly useful because we'd still to write `(0 :: Int)`
-- anyway to avoid ambiguity errors, and the instance for `()` is actually
-- harmful, because if _intended_ to write
--
-- > x <- fBind fTake1
-- > fReturn $ y .:= x
--
-- but actually wrote
--
-- > x <- fTake1
-- > fReturn $ y .:= x
--
-- then this would still be type correct because x now has `()` type and we
-- would be assigning `()` to `y`, rather than the value of `x`.
--
-- More generally, we don't want to confuse the return type of Haskell values
-- with Ziria values, which is why we have a `ToFreeExp` instance for `Val`
-- (Ziria values) but not for Haskell level values.
class ToFreeExp a where
  toFreeExp :: a -> FreeExp ()

instance ToFreeExp (FreeExp ()) where
  toFreeExp = id

instance ToFreeExp (GName Ty) where
  toFreeExp = feVar

instance ToFreeExp Val where
  toFreeExp = feVal

instance ToFreeExp Exp where
  toFreeExp = feLift

-- | Write to a variable _or_ to an array
(.:=) :: (ToFreeExp x, ToFreeExp e) => x -> e -> FreeExp ()
(.:=) x e = case toFreeExp x of
  FEArrRead earr start len (FEPure ()) -> feArrWrite earr start len (toFreeExp e)
  _otherwise                           -> feAssign (toFreeExp x)    (toFreeExp e)

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

instance XRange Val where
  toRange a = (toFreeExp a, LISingleton)

instance XRange (GName Ty) where
  toRange a = (toFreeExp a, LISingleton)

instance (ToFreeExp a, Integral b) => XRange (a, b) where
  toRange (a,b) = (toFreeExp a, LILength (fromIntegral b))

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
    FVar (GName CTy) (FreeComp a)
  | FBind (FreeComp ()) (GName Ty -> FreeComp a)
  | FLetE ForceInline (FreeExp ()) (GName Ty -> FreeComp a)
  | FLetERef (Either (FreeExp ()) Ty) (GName Ty -> FreeComp a)
  | FTake1 (FreeComp a)
  | FTake Int (FreeComp a)
  | FEmit (FreeExp ()) (FreeComp a)
  | FEmits (FreeExp ()) (FreeComp a)
  | FReturn ForceInline (FreeExp ()) (FreeComp a)
  | FBranch (FreeExp ()) (FreeComp ()) (FreeComp ()) (FreeComp a)
  | FTimes UnrollInfo (FreeExp ()) (FreeExp ()) (GName Ty -> FreeComp ()) (FreeComp a)
  | FRepeat (Maybe VectAnn) (FreeComp ()) (FreeComp a)
  | FAnnot (FreeComp ()) (Maybe Ty) (Maybe Ty) (Maybe Ty) (FreeComp a)
  | FLift Comp (FreeComp a)
  | FLiftSrc TyDefEnv SrcComp (FreeComp a)
  | FPure a

instance Functor FreeComp where
  fmap = liftM

instance Applicative FreeComp where
  pure  = return
  (<*>) = ap

instance Monad FreeComp where
  return = FPure
  FVar x            k >>= f = FVar x            (k >>= f)
  FLetE fi e        k >>= f = FLetE fi e        (k >=> f)
  FLetERef e        k >>= f = FLetERef e        (k >=> f)
  FBind c           k >>= f = FBind c           (k >=> f)
  FTake1            k >>= f = FTake1            (k >>= f)
  FTake n           k >>= f = FTake n           (k >>= f)
  FEmit e           k >>= f = FEmit e           (k >>= f)
  FEmits e          k >>= f = FEmits e          (k >>= f)
  FReturn fi e      k >>= f = FReturn fi e      (k >>= f)
  FBranch e c1 c2   k >>= f = FBranch e c1 c2   (k >>= f)
  FTimes ui e1 e2 b k >>= f = FTimes ui e1 e2 b (k >>= f)
  FRepeat ann c     k >>= f = FRepeat ann c     (k >>= f)
  FAnnot c u a b    k >>= f = FAnnot c u a b    (k >>= f)
  FLift c           k >>= f = FLift c           (k >>= f)
  FLiftSrc env c    k >>= f = FLiftSrc env c    (k >>= f)
  FPure a             >>= f = f a

fVar :: GName CTy -> FreeComp ()
fVar x = FVar x (FPure ())

fLetE :: ForceInline -> FreeExp () -> FreeComp (GName Ty)
fLetE fi e = FLetE fi e FPure

fLetERef :: Either (FreeExp ()) Ty -> FreeComp (GName Ty)
fLetERef e = FLetERef e FPure

fBind :: FreeComp () -> FreeComp (GName Ty)
fBind c = FBind c FPure

fTake1 :: FreeComp ()
fTake1 = FTake1 (FPure ())

fTake :: Int -> FreeComp ()
fTake n = FTake n (FPure ())

fEmit :: ToFreeExp e => e -> FreeComp ()
fEmit e = FEmit (toFreeExp e) (FPure ())

fEmits :: ToFreeExp e => e -> FreeComp ()
fEmits e = FEmits (toFreeExp e) (FPure ())

fReturn :: ToFreeExp e => ForceInline -> e -> FreeComp ()
fReturn fi e = FReturn fi (toFreeExp e) (FPure ())

fBranch :: ToFreeExp e => e -> FreeComp () -> FreeComp () -> FreeComp ()
fBranch e c1 c2  = FBranch (toFreeExp e) c1 c2 (FPure ())

fTimes :: (ToFreeExp a, ToFreeExp b)
       => UnrollInfo -> a -> b -> (GName Ty -> FreeComp ()) -> FreeComp ()
fTimes ui estart elen body = FTimes ui (toFreeExp estart)
                                       (toFreeExp elen) body (FPure ())

fRepeat :: Maybe VectAnn -> FreeComp () -> FreeComp ()
fRepeat ann body = FRepeat ann body (FPure ())

fError :: String -> FreeComp ()
fError str = fReturn AutoInline (feError str)

fLift :: Comp -> FreeComp ()
fLift c = FLift c (FPure ())

-- | For use with the quasi-quoter (which generates source terms)
--
-- In order to be able to translate source terms in isolation we need the
-- definition of any types (structs) that might be used inside the snippet.
fLiftSrc :: TyDefEnv -> SrcComp -> FreeComp ()
fLiftSrc tydefenv c = FLiftSrc tydefenv c (FPure ())

instance IfThenElse (FreeExp ()) (FreeComp ()) where
  ifThenElse = fBranch

{-------------------------------------------------------------------------------
  Type annotations on computations
-------------------------------------------------------------------------------}

-- | Annotate the "done type" of a computation
returning :: FreeComp () -> Ty -> FreeComp ()
returning c ty = FAnnot c (Just ty) Nothing Nothing (FPure ())

-- | Annotate the type of the input source
taking :: FreeComp () -> Ty -> FreeComp ()
taking c ty = FAnnot c Nothing (Just ty) Nothing (FPure ())

-- | Annotate the type of the output source
emitting :: FreeComp () -> Ty -> FreeComp ()
emitting c ty = FAnnot c Nothing Nothing (Just ty) (FPure ())

{-------------------------------------------------------------------------------
  Top-level entry point to the translation out of the free monad
-------------------------------------------------------------------------------}

-- | Translate out of the free expression monad
--
-- The snippet is translated and linted.
--
-- See remark for `unFreeComp` about independent type checking.
unFreeExp :: GS.Sym -> FreeExp () -> IO Exp
unFreeExp sym fexp = do
    result <- runTcM (lint =<< unEFree Nothing fexp)
                     (mkTyDefEnv [])
                     (mkEnv [])
                     (mkCEnv [])
                     sym
                     TopLevelErrCtx
                     emptyUnifier
    case result of
      Left err ->
        throwIO (userError (show err))
      Right (final, unifiers) -> do
        let efvs         = exprFVs final
            tyVarsInEnv  = mconcat (map (tyFVs . nameTyp) (S.toList efvs))
            udom         = unifierDom unifiers
        unless (tyVarsInEnv `tyVarsDisjoint` udom) $
          throwIO (userError ("Invalid type assumptions in snippet"))
        return final

-- | Translate out of the free monad
--
-- The snippet is translated to a `Comp` and linted.
--
-- NOTE: If type checking the snippet may NOT require unificaiton of any of the
-- type variables in the free term (expression or computation) variables. This
-- guarantees that we can typecheck snippets independently. We verify this and
-- throw an exception if this assumption is violated. (Such free type variables
-- may exist because we might be rewriting in the body of a polymorphic
-- function.)
unFreeComp :: GS.Sym -> FreeComp () -> IO Comp
unFreeComp sym fcomp = do
    result <- runTcM (lint =<< unFree Nothing fcomp)
                     (mkTyDefEnv [])
                     (mkEnv [])
                     (mkCEnv [])
                     sym
                     TopLevelErrCtx
                     emptyUnifier
    case result of
      Left err ->
        throwIO (userError (show err))
      Right (final, unifiers) -> do
        let (cfvs, efvs) = compFVs final
            tyVarsInEnv  = mconcat (map (tyFVs . nameTyp) (S.toList efvs))
                          `mappend`
                           mconcat (map (ctyFVs  . nameTyp) (S.toList cfvs))
            udom         = unifierDom unifiers
        unless (tyVarsInEnv `tyVarsDisjoint` udom) $
          throwIO (userError ("Invalid type assumptions in snippet"))
        return final

{-------------------------------------------------------------------------------
  Translation out of the free monad
-------------------------------------------------------------------------------}

unEFree :: Maybe SourcePos -> FreeExp () -> TcM Exp
unEFree loc = liftM fromJust . go
  where
    go :: FreeExp () -> TcM (Maybe Exp)
    go (FEVal v k) = do
      a  <- freshTy "a"
      k' <- go k
      return $ eVal loc a v `mSeq` k'
    go (FEVar nm k) = do
      k' <- go k
      return $ eVar loc nm `mSeq` k'
    go (FEBinOp op e1 e2 k) = do
      Just e1' <- go e1
      Just e2' <- go e2
      k'       <- go k
      return $ eBinOp loc op e1' e2' `mSeq` k'
    go (FEArrRead arr estart li k) = do
      Just arr'    <- go arr
      Just estart' <- go estart
      k'           <- go k
      return $ eArrRead loc arr' estart' li `mSeq` k'
    go (FEArrWrite arr estart li e k) = do
      Just arr'    <- go arr
      Just estart' <- go estart
      Just e'      <- go e
      k'           <- go k
      return $ eArrWrite loc arr' estart' li e' `mSeq` k'
    go (FEAssign x e k) = do
      Just x' <- go x
      Just e' <- go e
      k'      <- go k
      return $ eAssign loc x' e' `mSeq` k'
    go (FEError str k) = do
      a  <- freshTy "a"
      k' <- go k
      return $ eError loc a str `mSeq` k'
    go (FEAnnot e ty k) = do
      Just e' <- go e
      k'      <- go k
      unify loc (ctExp e') ty
      return $ e' `mSeq` k'
    go (FELift e k) = do
      k' <- go k
      return $ e `mSeq` k'
    go (FEPure ()) =
      return Nothing

    mSeq :: Exp -> Maybe Exp -> Maybe Exp
    mSeq e Nothing   = Just $ e
    mSeq e (Just e') = Just $ eSeq loc e e'

unFree :: Maybe SourcePos -> FreeComp () -> TcM Comp
unFree loc = liftM fromJust . go
  where
    go :: FreeComp () -> TcM (Maybe Comp)
    go (FVar x k) = do
      k' <- go k
      return $ cVar loc x `mSeq` k'
    go (FLetE fi e k) = do
      e'      <- unEFree loc e
      nm      <- newName loc (ctExp e')
      Just k' <- go (k nm)
      return $ Just $ cLetE loc nm fi e' k'
    go (FLetERef (Left e) k) = do
      e'      <- unEFree loc e
      nm      <- newName loc (ctExp e')
      Just k' <- go (k nm)
      return $ Just $ cLetERef loc nm (Just e') k'
    go (FLetERef (Right ty) k) = do
      nm      <- newName loc ty
      Just k' <- go (k nm)
      return $ Just $ cLetERef loc nm Nothing k'
    go (FBind c k) = do
      Just c' <- go c
      nm      <- newName loc (fromJust . doneTyOfCTy . ctComp $ c')
      Just k' <- go (k nm)
      return $ Just $ cBindMany loc c' [(nm, k')]
    go (FTake1 k) = do
      a  <- freshTy "a"
      b  <- freshTy "b"
      k' <- go k
      return $ cTake1 loc a b `mSeq` k'
    go (FTake n k) = do
      a  <- freshTy "a"
      b  <- freshTy "b"
      k' <- go k
      return $ cTake loc a b n `mSeq` k'
    go (FEmit e k) = do
      e' <- unEFree loc e
      a  <- freshTy "a"
      k' <- go k
      return $ cEmit loc a e' `mSeq` k'
    go (FEmits e k) = do
      e' <- unEFree loc e
      a  <- freshTy "a"
      k' <- go k
      return $ cEmits loc a e' `mSeq` k'
    go (FReturn fi e k) = do
      e' <- unEFree loc e
      a  <- freshTy "a"
      b  <- freshTy "b"
      k' <- go k
      return $ cReturn loc a b fi e' `mSeq` k'
    go (FBranch e c1 c2 k) = do
      e'       <- unEFree loc e
      Just c1' <- go c1
      Just c2' <- go c2
      k'       <- go k
      return $ cBranch loc e' c1' c2' `mSeq` k'
    go (FTimes ui estart elen body k) = do
      estart'    <- unEFree loc estart
      elen'      <- unEFree loc elen
      nm         <- newName loc (ctExp estart')
      Just body' <- go (body nm)
      k'         <- go k
      return $ cTimes loc ui estart' elen' nm body' `mSeq` k'
    go (FRepeat ann body k) = do
      Just body' <- go body
      k'         <- go k
      return $ cRepeat loc ann body' `mSeq` k'
    go (FAnnot c mu ma mb k) = do
      Just c' <- go c
      let cty = ctComp c'
      forM_ mu $ unify loc (fromJust (doneTyOfCTy cty))
      forM_ ma $ unify loc ( inTyOfCTy cty)
      forM_ mb $ unify loc (yldTyOfCTy cty)
      k'      <- go k
      return $ c' `mSeq` k'
    go (FLift c k) = do
      k' <- go k
      return $ c `mSeq` k'
    go (FLiftSrc env c k) = do
      c' <- extendTDefEnv' (mkTyDefEnv primComplexStructs) $
              extendTDefEnv' env $
                extendEnv (extractTypeEnv c) $
                  tyCheckComp c
      k' <- go k
      return $ c' `mSeq` k'
    go (FPure ()) =
      return Nothing

    mSeq :: Comp -> Maybe Comp -> Maybe Comp
    mSeq c Nothing   = Just $ c
    mSeq c (Just c') = Just $ cSeq loc c c'

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

newName :: Maybe SourcePos -> Ty -> TcM (GName Ty)
newName loc ty = do
  str <- genSym "_x"
  return $ toName ("_x" ++ str) loc ty

-- This environment is used by the renamer, which is indexing variables by their
-- _name_, rather than by their uniqId.
extractTypeEnv :: SrcComp -> [(String, GName Ty)]
extractTypeEnv = catMaybes . map aux . S.toList . compEFVs
  where
    aux :: GName SrcTy -> Maybe (String, GName Ty)
    aux nm | SrcInject ty <- nameTyp nm =
      Just (name nm, nm { nameTyp = ty })
    aux _ =
      Nothing

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

{- 
_test' :: IO ()
_test' = do
    go _test1
    go _test2
  where
    go test = do
      let tmp_idx     = toName "tmp_idx"     Nothing tint32
          is_empty    = toName "is_empty"    Nothing TBool
          in_buff     = toName "in_buff"     Nothing (TArray (Literal 16) tint32)
          in_buff_idx = toName "in_buff_idx" Nothing tint32

      let c = test tmp_idx is_empty in_buff in_buff_idx 1 2

      sym <- GS.initGenSym ""
      c'  <- unFreeComp sym c
      putStrLn (replicate 80 '-')
      print c'

_test1 :: GName Ty -> GName Ty -> GName Ty -> GName Ty -> Integer -> Integer -> FreeComp ()
_test1 tmp_idx is_empty in_buff in_buff_idx finalin n0 = do
  if is_empty .= VBool True
    then do x <- fBind (fTake1 `returning` nameTyp in_buff) -- optional type annotation
            fReturn AutoInline $ do
              in_buff     .:= x
              is_empty    .:= VBool False
              in_buff_idx .:= VInt 0
    else fReturn AutoInline VUnit
  if VInt finalin .= in_buff_idx .+ VInt n0
    then do fReturn ForceInline $ do
              is_empty .:= VBool True
              in_buff  .! (in_buff_idx, n0)
    else if in_buff_idx .+ VInt n0 .< VInt finalin
           then fReturn ForceInline $ do
                  tmp_idx     .:= in_buff_idx
                  in_buff_idx .:= in_buff_idx .+ VInt n0
                  in_buff .! (tmp_idx, n0)
           else -- The only reason this is an error is the lack of memcpy as a
                -- primitive but probably we will not encounter any such
                -- misaligned annotations.
                do -- Just a dummy return to make code generator happy ...
                   -- Fix this at some point by providing a proper
                   -- computer-level error function
                   _u <- fBind (fError "rewrite_take: unaligned!")
                   fReturn ForceInline $ in_buff .! (VInt 0, n0)

-}

{- AstQuasiQote needs to be brought up to speed ... 
_test2 :: GName Ty -> GName Ty -> GName Ty -> GName Ty -> Integer -> Integer -> FreeComp ()
_test2 tmp_idx is_empty in_buff in_buff_idx finalin n0 = fLiftSrc (mkTyDefEnv []) [zcomp| {
    if is_empty == true 
    then { x <- take
         ; do { in_buff     := x
              ; is_empty    := false
              ; in_buff_idx := 0
              }
         }
    else return ();
    do { if finalin == in_buff_idx + n0
         then is_empty := true
         else if in_buff_idx + n0 < finalin
              then { tmp_idx     := in_buff_idx
                   ; in_buff_idx := in_buff_idx + n0
                   }
         else error "rewrite_take: unaligned";
       };
    return [forceinline] in_buff[tmp_idx,n0];
  }|]


-}
