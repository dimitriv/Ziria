{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RebindableSyntax, 
             FlexibleInstances, 
             MultiParamTypeClasses, QuasiQuotes, GADTs, 
             TypeFamilies, TypeOperators, ExistentialQuantification,
             EmptyDataDecls #-}

-- | Free monads for convenient construction of AST terms
--
-- NOTE: Importing modules will probably want to enable the `RebindableSyntax`
-- language extension and import `Rebindables` to get if-then-else syntax.
module AstFM where

import Prelude

import Control.Applicative

-- import Control.Exception
-- import Control.Monad hiding (forM_)
-- import Data.Maybe (fromJust, catMaybes)
-- import Data.Monoid
-- import Data.Foldable (forM_)

import Text.Parsec.Pos
-- import qualified Data.Set as S

import AstComp
import AstExpr
import AstUnlabelled

import CtComp ( ctDoneTyOfComp )

-- import Tc ( tc )
-- import TcMonad
-- import TcUnify (unify)
-- import Typecheck (tyCheckComp)
import qualified GenSym as GS

{- 
import CtExpr ( ctExp )
import Rebindables
import Utils ( uncurry4 )
-}


-- Imports for the example only:
-- import AstQuasiQuote (zcomp)


{------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

data FExp where
  FEVal     :: Ty -> Val -> FExp
  FEVar     :: GName Ty -> FExp
  FEBinOp   :: BinOp -> FExp -> FExp -> FExp
  FELift    :: Exp -> FExp
  FEArrRead :: FExp -> FExp -> LengthInfo -> FExp
  FEError   :: Ty -> String -> FExp 

-- Value-like things 
class FValy v where 
  toVal :: v -> Val

instance FValy Bool where toVal b = VBool b
instance FValy Int      where toVal i = VInt $ fromIntegral i
instance FValy Integer  where toVal i = VInt $ fromIntegral i

instance FValy ()   where toVal _ = VUnit
instance FValy Val  where toVal v = v

-- Expression-like things
class FExpy a where
  toFExp :: a -> FExp

-- Annotations 
data v ::: t = v ::: t 
    
instance FExpy FExp       where toFExp   = id
instance FExpy (GName Ty) where toFExp   = FEVar 
instance FExpy Exp        where toFExp   = FELift 
instance FExpy Bool       where toFExp b = FEVal TBool (VBool b)
instance FExpy Int        where toFExp i = FEVal tint (VInt $ fromIntegral i)
instance FExpy Integer    where toFExp i = FEVal tint (VInt $ fromIntegral i)

instance FExpy ()         where toFExp _ = FEVal TUnit VUnit
-- Instance for annotated values
instance FValy v => FExpy (v ::: Ty) where 
  toFExp (v ::: t) = FEVal t (toVal v)

-- Convenience combinators
(.*) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.*) e1 e2 = FEBinOp Mult (toFExp e1) (toFExp e2)

(./) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(./) e1 e2 = FEBinOp Div (toFExp e1) (toFExp e2)

(.%) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.%) e1 e2 = FEBinOp Rem (toFExp e1) (toFExp e2)

(.=) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.=) e1 e2 = FEBinOp Eq (toFExp e1) (toFExp e2)

(.<) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.<) e1 e2 = FEBinOp Lt (toFExp e1) (toFExp e2)

(.<=) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.<=) e1 e2 = FEBinOp Leq (toFExp e1) (toFExp e2)

(.+) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.+) e1 e2 = FEBinOp Add (toFExp e1) (toFExp e2)

(.-) :: (FExpy e1, FExpy e2) => e1 -> e2 -> FExp
(.-) e1 e2 = FEBinOp Add (toFExp e1) (toFExp e2)

infixl 7 .*
infixl 7 ./
infixl 7 .%
infix  4 .=
infix  4 .<
infix  4 .<=
infixl 6 .+
infixl 6 .-

class Rng e where toRng :: e -> (FExp, LengthInfo)

data e :+ i = e :+ i
instance Rng FExp                   where toRng e      = (toFExp e, LISingleton)
instance Rng (GName Ty)             where toRng e      = (toFExp e, LISingleton)
instance Rng Exp                    where toRng e      = (toFExp e, LISingleton)
instance Rng Int                    where toRng e      = (toFExp e, LISingleton)
instance Rng Integer                where toRng e      = (toFExp e, LISingleton)
instance FValy v => Rng (v ::: Ty)  where toRng e      = (toFExp e, LISingleton)
instance FExpy e => Rng (e :+ Int)  where toRng (e:+i) = (toFExp e, LILength i )
instance FExpy e => Rng (e :+ Integer) where 
  toRng (e:+i) = (toFExp e, LILength $ fromIntegral i)


(.!) :: (FExpy earr, Rng r) => earr -> r -> FExp
(.!) earr r 
  = FEArrRead (toFExp earr) estart len
  where (estart,len) = toRng r


-- Interpret expressions
interpE :: FExpy e => Maybe SourcePos -> e -> Exp
interpE p fe  = go (toFExp fe)
  where 
    go (FEVal t v) = eVal p t v
    go (FEVar nm)  = eVar p nm
    go (FEBinOp bop f1 f2) = eBinOp p bop (go f1) (go f2) 
    go (FELift e) = e 
    go (FEArrRead f1 f2 li) = eArrRead p (go f1) (go f2) li
    go (FEError t s) = eError p t s


{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

 
-- Statements
data FStmt s where 
  FEArrWrite :: FExp -> FExp -> LengthInfo -> FExp -> FStmt s -> FStmt s
  FEAssign   :: FExp -> FExp -> FStmt s -> FStmt s
  FEReturn   :: v -> FStmt v

instance Monad FStmt where 
  return s = FEReturn s
  (FEArrWrite fe1 fe2 li fe ss) >>= f = FEArrWrite fe1 fe2 li fe (ss >>= f)
  (FEAssign fe1 fe2 ss) >>= f         = FEAssign fe1 fe2 (ss >>= f)
  (FEReturn v) >>= f                  = f v

interpS_FExpy :: FExpy e => Maybe SourcePos -> FStmt e -> Exp
interpS_FExpy p = interpS_aux p (interpE p . toFExp)

interpS_Bindable :: Bindable e => Maybe SourcePos -> FStmt e -> Exp
interpS_Bindable p = interpS_aux p genexp

interpS_aux :: Maybe SourcePos -> (v -> Exp) -> FStmt v -> Exp
interpS_aux p on_ret = go 
  where 
    go (FEReturn e) = on_ret e
    go (FEArrWrite fe1 fe2 li fe3 s)
      = case go s of 
          es | EVal TUnit VUnit <- unExp es 
            -> earrwrite
             | otherwise
             -> eSeq p earrwrite es
      where earrwrite = eArrWrite p e1 e2 li e3
            e1 = interpE p fe1
            e2 = interpE p fe2
            e3 = interpE p fe3
    go (FEAssign fe1 fe2 s) 
      = case go s of
          es | EVal TUnit VUnit <- unExp es 
             -> eassign 
             | otherwise
             -> eSeq p eassign es
      where eassign = eAssign p e1 e2
            e1 = interpE p fe1
            e2 = interpE p fe2



{-------------------------------------------------------------------------------
  Computations
-------------------------------------------------------------------------------}

data Void -- empty
data ZTy  -- domain of Ziria types

data BndRes v = BndResB EId v | BndResU v 

gen_name :: GS.Sym -> Ty -> IO EId
gen_name sym ty = gen_name_pref sym "" ty 

gen_name_pref :: GS.Sym -> String -> Ty -> IO EId
gen_name_pref sym x ty = do
    suff <- GS.genSymStr sym
    return $ toName (x ++ "_free_" ++ suff) Nothing ty


class Bindable v where
  genbnd :: GS.Sym -> Ty -> IO (BndRes v)
  genexp :: v -> Exp

instance Bindable EId where
  genbnd sym ty = do
    nm <- gen_name sym ty
    return $ BndResB nm nm
  genexp nm = eVar Nothing nm

instance Bindable FExp where
  genbnd sym t
    = do nm <- gen_name sym t
         return $ BndResB nm (FEVar nm)
  genexp fe = interpE Nothing fe

instance Bindable () where
  genbnd _sym _ty = return $ BndResU ()
  genexp _ = eVal Nothing TUnit VUnit

instance Bindable Void where 
  genbnd _sym _ 
    = error "Bindable: Cannot bind result of transformer (Void)"
  genexp _ 
    = error "Bindable: Cannot bind result of transformer (Void)"

data Zr v where 
 FTakeOne  :: Ty -> Zr EId
 FTakeMany :: Ty -> Int -> Zr EId 
 FEmit     :: FExpy v => v -> Zr ()
 FEmits    :: FExpy v => v -> Zr ()
 FReturn   :: ForceInline -> v -> Zr v
 FBind     :: Bindable v => Zr v -> (v -> Zr w) -> Zr w
 FParA     :: Bindable v => ParInfo -> Zr Void -> Zr v     -> Zr v
 FParB     :: Bindable v => ParInfo -> Zr v    -> Zr Void  -> Zr v
 FRepeat   :: Maybe VectAnn -> Zr () -> Zr Void
 -- Like return but for statements
 FExec     :: Bindable v => FStmt v  -> Zr v

 FLetE    :: FExpy e 
          => ForceInline -> (String ::: Ty) -> e -> (EId -> Zr v) -> Zr v
 FLetERef :: FExpy e 
          => (String ::: Ty) -> Maybe e -> (EId -> Zr v) -> Zr v
 FBranch  :: FExpy e => e -> Zr v -> Zr v -> Zr v
 FTimes   :: (FExpy e1, FExpy e2) 
          => UnrollInfo -> e1 -> e2 -> (EId -> Zr ()) -> Zr ()
 


instance Monad Zr where
  return e                      = FReturn AutoInline e
  (>>=) (FTakeOne t)  f         = FBind (FTakeOne t) f
  (>>=) (FTakeMany t i) f       = FBind (FTakeMany t i) f
  (>>=) (FReturn _ v) f         = f v
  (>>=) (FEmit v) f             = FBind (FEmit v) f
  (>>=) (FEmits v) f            = FBind (FEmits v) f
  (>>=) (FBind m g)   f         = FBind m (\v -> g v >>= f)
  (>>=) (FRepeat v c) f         = FBind (FRepeat v c) f 
  (>>=) (FParA p m1 m2) f       = FBind (FParA p m1 m2) f
  (>>=) (FParB p m1 m2) f       = FBind (FParB p m1 m2) f
  (>>=) (FExec stms)  f         = FBind (FExec stms) f
  (>>=) (FTimes ui fe1 fe2 k) f = FBind (FTimes ui fe1 fe2 k) f

  -- Floating Let/Letrefs/Branches just /only/ because it's easier 
  -- otherwise we would have to introduce a Bindable instance.
  (>>=) (FLetE fi sty fe k) f   = FLetE fi sty fe (\v -> k v >>= f)
  (>>=) (FLetERef sty me k) f   = FLetERef sty me (\v -> k v >>= f)
  (>>=) (FBranch e c1 c2) f     = FBranch e (c1 >>= f) (c2 >>= f) 

 
interpC :: Bindable v => GS.Sym -> Maybe SourcePos -> Zr v -> IO Comp
interpC sym loc = go
  where 
    go :: forall v. Bindable v => Zr v -> IO Comp -- Polymorphic recursion
    go (FTakeOne t)    = return $ cTake1 loc t
    go (FTakeMany t i) = return $ cTake loc t i
    go (FEmit e)       = return $ cEmit loc (interpE loc e)
    go (FEmits e)      = return $ cEmits loc (interpE loc e)
    go (FReturn fi e)  = return $ cReturn loc fi (genexp e)
    go (FExec stmts)   = return $ 
                         cReturn loc AutoInline (interpS_Bindable loc stmts)
    go (FBind st1 f) = do 
      c1 <- go st1
      let t = ctDoneTyOfComp c1
      bnd <- genbnd sym t
      case bnd of
        BndResB nm farg -> do c2 <- go (f farg)
                              return $ cBindMany loc c1 [(nm,c2)]
        BndResU farg    -> cSeq loc c1 <$> go (f farg)

    go (FRepeat v stc)   = cRepeat loc v <$> go stc
    go (FParA p st1 st2) = do c1 <- go st1 
                              c2 <- go st2
                              return $ cPar loc p c1 c2
    go (FParB p st1 st2) = do c1 <- go st1 
                              c2 <- go st2
                              return $ cPar loc p c1 c2

    go (FLetE fi (x ::: ty) fe k) = do
       nm <- gen_name_pref sym x ty
       c <- go (k nm)
       let e = interpE loc $ toFExp fe
       return $ cLetE loc nm fi e c
    go (FLetERef (x ::: ty) mfe k) = do
       nm <- gen_name_pref sym x ty
       let me = fmap (interpE loc . toFExp ) mfe
       c <- go (k nm) 
       return $ cLetERef loc nm me c
    go (FBranch fe s1 s2) = do 
       let e = interpE loc (toFExp fe)
       c1 <- go s1
       c2 <- go s2
       return $ cBranch loc e c1 c2
    go (FTimes ui fe1 fe2 k) = do
       let e1 = interpE loc (toFExp fe1)
           e2 = interpE loc (toFExp fe2)
       nm <- gen_name_pref sym "idx" tint
       c <- go (k nm) 
       return $ cTimes loc ui e1 e2 nm c
 


{-------------------------------------------------------------------------------
  Computation API
-------------------------------------------------------------------------------}




{------------ DV this is almost working .......

data FComp a 
  = FComp0 FComp0                    (GName Ty -> FComp a)
  | FLetE ForceInline String Ty FExp (GName Ty -> FComp a) -- String there for debug purposes only
  | FLetERef String Ty (Maybe FExp)  (GName Ty -> FComp a) -- String there for debug purposes only
  | FPure a

data FComp0 
  = FVar CId                                          
  | FTake1 Ty                                       
  | FTake Ty Int                                    
  | FEmit FExp                                      
  | FEmits FExp                                     
  | FReturn ForceInline FExp                        
  | FStmts (MStmt ())                               
  | FLift Comp                                      
  | forall b. FBranch FExp (FComp b) (FComp b)         
  | forall b. FTimes UnrollInfo FExp FExp (GName Ty -> FComp b)        
  | forall b. FRepeat (Maybe VectAnn) (FComp b)
  | forall b c. FPar (FComp b) (FComp c)
  -- It's convenient to just have naked statements here ...
  | FCEArrWrite FExp FExp LengthInfo FExp
  | FCEAssign FExp FExp


instance Monad FComp where
  return a = FPure a
  (FPure a)     >>= f = f a 
  (FComp0 c0 k) >>= f = FComp0 c0 (\n -> k n >>= f)
  (FLetE fi s t fe k) >>= f = FLetE fi s t fe (\n -> k n >>= f)
  (FLetERef s t me k) >>= f = FLetERef s t me (\n -> k n >>= f)
 
-- API 
type FComp a = FComp EId

fTakeOne :: Ty -> FComp a 
fTakeOne ty = FComp0 (FTake1 ty) FPure

fTakeMany :: Ty -> Int -> FComp a
fTakeMany ty i = FComp0 (FTake ty i) FPure

fVar :: GName CTy -> FComp a
fVar x = FComp0 (FVar x) FPure

fEmit :: FExpy e => e -> FComp a
fEmit e = FComp0 (FEmit (toFExp e)) FPure

fEmits :: FExpy e => e -> FComp a
fEmits e = FComp0 (FEmits (toFExp e)) FPure

fBranch :: FExpy e => e -> FComp a -> FComp a -> FComp a
fBranch e c1 c2 = FComp0 (FBranch (toFExp e) c1 c2) (const $ FPure (error "bomb!"))

instance IfThenElse FExp (FComp a) where
  ifThenElse = fBranch

fRet :: FExpy e => ForceInline -> e -> FComp ()
fRet fi e = FComp0 (FReturn fi (toFExp e)) (const $ FPure ())

fTimes :: (FExpy a, FExpy b)
       => UnrollInfo -> a -> b -> (GName Ty -> FComp a) -> FComp a
fTimes ui estart elen body = FComp0 (FTimes ui fs fl body) FPure
  where fs = toFExp estart
        fl = toFExp elen 

fRepeat :: Maybe VectAnn -> FComp a -> FComp a
fRepeat ann body = FComp0 (FRepeat ann body) FPure

fLetE :: FExpy e => ForceInline -> String -> Ty -> e -> FComp a
fLetE fi s ty e = FLetE fi s ty (toFExp e) FPure

fLetERef :: String -> Ty -> FComp a
fLetERef s ty = FLetERef s ty Nothing FPure

fError :: Ty -> String -> FComp ()
fError t str = fRet AutoInline (FEError t str)

fExec :: MStmt () -> FComp a 
fExec stms = FComp0 (FStmts stms) FPure


_aI,_fI,_nI :: ForceInline 
_aI = AutoInline
_fI = ForceInline
_nI = NoInline

-- Overload statements to be commands too
class Cmd a where 
 toCmd :: Either (FExp,FExp,LengthInfo,FExp) (FExp,FExp) -> a 

instance Cmd (MStmt a) where 
 toCmd (Left (earr,start,len,rhs)) 
   = Free $ FEArrWrite earr start len rhs (Pure (error "bomb!"))
 toCmd (Right (elhs,erhs))
   = Free $ FEAssign elhs erhs (Pure (error "bomb!"))

instance Cmd (FComp a) where
 toCmd (Left bundle)  = FComp0 (uncurry4 FCEArrWrite bundle) (const $ FPure (error "Bomb!"))
 toCmd (Right bundle) = FComp0 (uncurry FCEAssign bundle)    (const $ FPure (error "Bomb!"))

-- | Write to a variable _or_ to an array
(.:=) :: (Cmd s, FExpy x, FExpy e) => x -> e -> s
(.:=) x e 
  = case toFExp x of
      FEArrRead earr start len 
        -> let fearr  = toFExp earr
               fstart = toFExp start
           in toCmd (Left (fearr,fstart,len,frhs))
      fx -> toCmd (Right (fx,frhs))
  where frhs = toFExp e

infix  1 .:=



_test1 :: GName Ty -> GName Ty 
       -> GName Ty -> GName Ty 
       -> Integer -> Integer -> FComp ()
_test1 tmp_idx is_empty in_buff in_buff_idx finalin n0 = do

  if is_empty .= True
    then do x <- fTakeOne (nameTyp in_buff)
            in_buff     .:= x
            is_empty    .:= False
            in_buff_idx .:= (0::Int)
    else fRet _aI ()

  if finalin .= in_buff_idx .+ n0
    then do fExec $ is_empty .:= True
            fRet _fI $ in_buff  .! (in_buff_idx :+ n0)
    else if in_buff_idx .+ n0 .< finalin
         then do tmp_idx     .:= in_buff_idx
                 in_buff_idx .:= in_buff_idx .+ n0
                 fRet _fI $ in_buff .! (tmp_idx :+ n0)
         else do fError TUnit "rewrite_take: unaligned"
                 fRet _fI $ in_buff .! ((0::Int) :+ n0)



newName :: GS.Sym -> String -> Maybe SourcePos -> Ty -> IO EId
newName sym orig loc ty = do
  suff <- GS.genSymStr sym
  return $ toName (orig ++ "_free_" ++ suff) loc ty


------------------ almost-working ---------------}


{- DV: TODO-TODO-TODO
 - ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interpFComp :: GS.Sym -> Maybe SourcePos -> FComp a -> IO (Maybe Comp)
interpFComp sym p m = go m 
 where
   go (FLetE fi x ty fe k) = do
     e' <- interpFExp p e
     nm <- newName p x ty
     Just comp <- go (k nm)
     return $ Just $ cLetE p nm fi e' comp

     
     

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
      k' <- go k
      return $ cTake1 loc a `mSeq` k'
    go (FTake n k) = do
      a  <- freshTy "a"
      k' <- go k
      return $ cTake loc a n `mSeq` k'
    go (FEmit e k) = do
      e' <- unEFree loc e
      k' <- go k
      return $ cEmit loc e' `mSeq` k'
    go (FEmits e k) = do
      e' <- unEFree loc e
      k' <- go k
      return $ cEmits loc e' `mSeq` k'
    go (FReturn fi e k) = do
      e' <- unEFree loc e
      k' <- go k
      return $ cReturn loc fi e' `mSeq` k'
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



-}






{-


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
    result <- runTcM (tc =<< unEFree Nothing fexp)
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
    result <- runTcM (tc =<< unFree Nothing fcomp)
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
      k' <- go k
      return $ cTake1 loc a `mSeq` k'
    go (FTake n k) = do
      a  <- freshTy "a"
      k' <- go k
      return $ cTake loc a n `mSeq` k'
    go (FEmit e k) = do
      e' <- unEFree loc e
      k' <- go k
      return $ cEmit loc e' `mSeq` k'
    go (FEmits e k) = do
      e' <- unEFree loc e
      k' <- go k
      return $ cEmits loc e' `mSeq` k'
    go (FReturn fi e k) = do
      e' <- unEFree loc e
      k' <- go k
      return $ cReturn loc fi e' `mSeq` k'
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

-}

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
