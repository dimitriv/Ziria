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

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RebindableSyntax, 
             FlexibleInstances, 
             MultiParamTypeClasses, QuasiQuotes, GADTs, 
             TypeFamilies, TypeOperators, ExistentialQuantification,
             FunctionalDependencies, FlexibleContexts,
             ExtendedDefaultRules, 
             EmptyDataDecls #-}

-- | Free monads for convenient construction of AST terms
--
-- NOTE: Importing modules will probably want to enable the `RebindableSyntax`
-- language extension and import `Rebindables` to get if-then-else syntax.
module AstFM where

import Prelude

import Control.Applicative

import Text.Parsec.Pos

import AstComp
import AstExpr
import AstUnlabelled
import CtComp ( ctDoneTyOfComp )
import qualified GenSym as GS
import Rebindables


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

-- Removing that to avoid confusion with meta-language unit 
-- instance FValy ()   where toVal _ = VUnit

instance FValy Val  where toVal v = v

-- Expression-like things
class FExpy a where
  toFExp :: a -> FExp

-- Annotations 
data v ::: t = v ::: t 
    
instance FExpy FExp where toFExp   = id
instance FExpy EId  where toFExp   = FEVar 
instance FExpy Exp  where toFExp   = FELift 
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
(.-) e1 e2 = FEBinOp Sub (toFExp e1) (toFExp e2)


instance Num FExp where
  fromInteger i = FEVal tint (VInt i)
  (+) = FEBinOp Add 
  (-) = FEBinOp Sub
  (*) = FEBinOp Mult
  negate = error "todo"
  abs    = error "todo"
  signum = error "todo"


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

data BndRes v = BndResB { bnd_res_id :: EId 
                        , bnd_res_val :: v } 
              | BndResU { bnd_res_val :: v }

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
 FReturn   :: Bindable v => ForceInline -> v -> Zr v
 FBind     :: Bindable v => Zr v -> (v -> Zr w) -> Zr w
 FParA     :: Bindable v => ParInfo -> Zr Void -> Zr v     -> Zr v
 FParB     :: Bindable v => ParInfo -> Zr v    -> Zr Void  -> Zr v
 FRepeat   :: Maybe VectAnn -> Zr () -> Zr Void
 -- Like return but for statements
 FExec     :: Bindable v => FStmt v  -> Zr v

 FLetE     :: Bindable v
           => ForceInline -> String -> Ty -> FExp -> (EId -> Zr v) -> Zr v
 FLetERef  :: (Bindable v)
           => String -> Ty -> Maybe FExp -> (EId -> Zr v) -> Zr v
 FBranch   :: (Bindable v, FExpy e) => e -> Zr v -> Zr v -> Zr v
 FTimes    :: (FExpy e1, FExpy e2) 
           => UnrollInfo -> e1 -> e2 -> (EId -> Zr ()) -> Zr ()

 FPure     :: v -> Zr v

 FEmbed    :: Bindable v => IO Comp -> Zr v

instance Monad Zr where
  return e                      = FPure e 
  (>>=) (FPure v) f             = f v

  (>>=) (FTakeOne t)  f         = FBind (FTakeOne t) f
  (>>=) (FTakeMany t i) f       = FBind (FTakeMany t i) f
  (>>=) (FReturn fi v) f        = FBind (FReturn fi v) f
  (>>=) (FEmit v) f             = FBind (FEmit v) f
  (>>=) (FEmits v) f            = FBind (FEmits v) f
  (>>=) (FBind m g)   f         = FBind m (\v -> g v >>= f)
  (>>=) (FRepeat v c) f         = FBind (FRepeat v c) f 
  (>>=) (FParA p m1 m2) f       = FBind (FParA p m1 m2) f
  (>>=) (FParB p m1 m2) f       = FBind (FParB p m1 m2) f
  (>>=) (FExec stms)  f         = FBind (FExec stms) f
  (>>=) (FTimes ui fe1 fe2 k) f = FBind (FTimes ui fe1 fe2 k) f

  (>>=) (FLetE fi x ty fe k) f  = FBind (FLetE fi x ty fe k) f 
  (>>=) (FLetERef x ty me k) f  = FBind (FLetERef x ty me k) f
  (>>=) (FBranch e c1 c2) f     = FBind (FBranch e c1 c2) f
  (>>=) (FEmbed io_c) f         = FBind (FEmbed io_c) f

interpC :: Bindable v => GS.Sym -> Maybe SourcePos -> Zr v -> IO Comp
interpC sym loc = go
  where 
    -- NB: requires polymorphic recursion
    go :: forall v. Bindable v => Zr v -> IO Comp
    go (FPure _e)      = error "FPure!?" 
                         -- FPure in isolation is just bad.
    go (FEmbed io_c)   = io_c
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
      let fc2 = f (bnd_res_val bnd)
      case (fc2,bnd) of 
        -- If continuation is pure then just return c1 
        -- Invariant: we must be returning whatever c1 returns
        (FPure {},_)     -> return c1
        (_,BndResB nm _) -> do c2 <- go fc2 
                               return $ cBindMany loc c1 [(nm,c2)]
        (_,BndResU _)    -> cSeq loc c1 <$> go fc2

    go (FRepeat v stc)   = cRepeat loc v <$> go stc
    go (FParA p st1 st2) = do c1 <- go st1 
                              c2 <- go st2
                              return $ cPar loc p c1 c2
    go (FParB p st1 st2) = do c1 <- go st1 
                              c2 <- go st2
                              return $ cPar loc p c1 c2
    go (FLetE fi x ty fe k) = do
       nm <- gen_name_pref sym x ty
       c <- go (k nm)
       let e = interpE loc fe
       return $ cLetE loc nm fi e c
    go (FLetERef x ty mfe k) = do
       nm <- gen_name_pref sym x ty
       let me = fmap (interpE loc) mfe
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

ftake :: Ty -> Zr EId
ftake = FTakeOne

ftakes :: Ty -> Int -> Zr EId
ftakes = FTakeMany 

femit :: FExpy v => v -> Zr ()
femit = FEmit

femits :: FExpy v => v -> Zr ()
femits = FEmits

freturn :: Bindable v => ForceInline -> v -> Zr v
freturn = FReturn 

class ArrComp a b c | a b -> c where 
  (>->) :: ParInfo -> Zr a -> Zr b -> Zr c

instance Bindable v => ArrComp Void v v where (>->) = FParA 
instance Bindable v => ArrComp v Void v where (>->) = FParB

frepeat :: Zr () -> Zr Void
frepeat = FRepeat Nothing

fexec :: Bindable v => FStmt v -> Zr v
fexec = FExec 

ftimes :: (FExpy e1, FExpy e2) => e1 -> e2 -> (EId -> Zr ()) -> Zr ()
ftimes = FTimes AutoUnroll

flete :: (Bindable v, FExpy e) 
      => ForceInline -> (String ::: Ty ::= e) -> (EId -> Zr v) -> Zr v
flete fi (x ::: t ::= e) = FLetE fi x t (toFExp e)


fembed :: Bindable v => IO Comp -> Zr v
fembed = FEmbed

instance Bindable v => IfThenElse FExp (Zr v) where
  ifThenElse = FBranch

ferror :: Ty -> String -> Zr ()
ferror t str = do _x <- freturn _aI (FEError t str)
                  return ()

data b ::= e = b ::= e

class MutBind c where
  mutbind :: c -> (String,Ty,Maybe FExp)
instance MutBind (String ::: Ty) where
  mutbind (x ::: t) = (x,t,Nothing)
instance FExpy e => MutBind (String ::: Ty ::= e) where
  mutbind (x ::: t ::= e) = (x,t, Just $ toFExp e)

fleteref :: (Bindable v, MutBind c) => c -> (EId -> Zr v) -> Zr v
fleteref c = FLetERef x t mb
  where (x,t,mb) = mutbind c


_aI,_fI,_nI :: ForceInline 
_aI = AutoInline
_fI = ForceInline
_nI = NoInline


{-------------------------------------------------------------------------------
  Command API
-------------------------------------------------------------------------------}
class Cmd s where 
  fromStmt :: FStmt () -> s ()

-- A command is either a statement or a statement lifted into computations
instance Cmd FStmt where fromStmt s = s 
instance Cmd Zr    where fromStmt s = fexec s 

(.:=) :: (Cmd s, FExpy x, FExpy e) => x -> e -> s ()
(.:=) x e = fromStmt stm
  where 
    stm = case toFExp x of
           FEArrRead earr start len 
             | let fearr  = toFExp earr
                   fstart = toFExp start
             -> FEArrWrite fearr fstart len frhs (return ())
           fx -> FEAssign fx frhs (return ())
    frhs = toFExp e


infix  1 .:=


data I = I Int
instance FExpy I where toFExp (I i) = toFExp i


-- An example, like those typically found in the vectorizer
_test1 :: Int -> Int -> Zr FExp
_test1 finalin n0 
  = let tarr = TArray (Literal finalin) tint
    in 
    fleteref ("tmp_idx"  ::: tint ) $ \tmp_idx  ->
    fleteref ("is_empty" ::: TBool) $ \is_empty ->
    fleteref ("in_buff"  ::: tarr ) $ \in_buff  ->
    flete _aI ("in_buff_idx" ::: tint ::= I(0)) $ \in_buff_idx ->
    do if is_empty .= True
          then do x <- ftake (nameTyp in_buff)
                  in_buff     .:= x
                  is_empty    .:= False
                  in_buff_idx .:= I(0)
          else freturn _aI ()
       if finalin .= in_buff_idx .+ n0
        then do fexec $ is_empty .:= True
                freturn _fI $ (in_buff  .! (in_buff_idx :+ n0))
        else if in_buff_idx .+ n0 .< finalin
             then do tmp_idx     .:= in_buff_idx
                     in_buff_idx .:= in_buff_idx .+ n0
                     freturn _fI $ in_buff .! (tmp_idx :+ n0)
             else do ferror TUnit "rewrite_take: unaligned"
                     freturn _fI $ in_buff .! (I(0) :+ n0)
