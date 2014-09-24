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
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module AstExpr where

import {-# SOURCE #-} Analysis.Range

import Control.Monad.State
import Data.Functor.Identity ( Identity (..) )
import Data.Map (Map)
import Data.Maybe
import GHC.Generics (Generic)
import Text.Parsec.Pos
import Text.PrettyPrint.Mainland
import Text.Show.Pretty (PrettyVal)
import qualified Data.Set as S

import Orphans

data Precision
     = Full
     | Fixed Int
     | Unknown Name
  deriving (Generic, Show, Eq)

data UnOp =
    NatExp
  | Neg
  | Not
  | BwNeg
  | Cast Ty   -- Cast to this target type
  | ALength
  deriving (Generic, Eq)

data BinOp =
  -- arithmetic operators
    Add
  | Sub
  | Mult
  | Div
  | Rem
  | Expon
  -- bitwise operators
  | ShL
  | ShR
  | BwAnd
  | BwOr
  | BwXor
  -- comparison operators
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or
  deriving (Generic, Eq, Show)

data Val where
  VBit    :: Bool    -> Val
  VInt    :: Integer -> Val
  VDouble :: Precision -> Double -> Val
  VBool   :: Bool -> Val
  VString :: String -> Val
  VUnit   :: Val
  deriving (Generic, Show, Eq)


data Name
  = MkName { name      :: String
           , uniqId    :: String
           , mbtype    :: Maybe Ty
           , nameLoc :: Maybe SourcePos }
  deriving (Generic)



instance Eq Name where
  nm1 == nm2 = (name nm1 == name nm2) && (uniqId nm1 == uniqId nm2)

instance Ord Name where
  nm1 <= nm2 = (uniqId nm1 <= uniqId nm2)

instance Show Name where
  show (MkName x id _ loc)    = x

instance Pretty Name where
    ppr = string . show


toName :: String -> Maybe SourcePos -> Maybe Ty -> Name
-- This is our only function to create new names
toName s mpos mty
  = MkName { name    = s
           , uniqId  = s
           , nameLoc = mpos
           , mbtype  = mty }

updNameId id nm = nm { uniqId = id }

getLnNumInStr csp
   = case csp of
       Just l  -> "ln" ++ (show $ sourceLine l) ++ "_"
       Nothing -> "ln_"

getNameWithUniq :: Name -> String
getNameWithUniq nm = name nm ++ "_blk" ++ uniqId nm

data LengthInfo
     = LISingleton
     | LILength Int -- Invariant: > 0
  deriving (Generic, Eq)

data UnrollInfo
  = Unroll        -- force unroll
  | NoUnroll      -- force no-unroll
  | AutoUnroll    -- do whatever the compiler would do (no annotation)
  deriving (Generic, Eq)

-- If true, the binding should be forced to be inlined.
-- This is used by e.g. the vectorizer to bind inlinable
-- sub-arrays of the input array.

data ForceInline
  = ForceInline   -- Always inline
  | NoInline      -- Never inline
  | AutoInline    -- Let the compiler decide
  deriving (Generic)

data Exp0 a where
  EVal :: Val -> Exp0 a
  EValArr :: [Val] -> Exp0 a
  EVar :: Name -> Exp0 a
  EUnOp :: UnOp -> Exp a -> Exp0 a
  EBinOp :: BinOp -> Exp a -> Exp a -> Exp0 a

  EAssign :: Exp a -> Exp a -> Exp0 a

  -- EArrRead ex ei j.
  -- Read a subarray of 'ex' starting at index ei of
  -- length j and going as long as LengthInfo says.
  -- Similarly for EArrWrite.
  -- If LengthInfo is LISingleton, then we are supposed to only read
  -- at a single position and return a scalar. Otherwise we return an array.
  EArrRead :: Exp a -> Exp a -> LengthInfo -> Exp0 a
  EArrWrite :: Exp a -> Exp a -> LengthInfo -> Exp a -> Exp0 a

  EIter :: Name -> Name -> Exp a -> Exp a -> Exp0 a

  EFor :: UnrollInfo -> Name -> Exp a -> Exp a -> Exp a -> Exp0 a


  EWhile :: Exp a -> Exp a -> Exp0 a


  ELet :: Name -> ForceInline -> Exp a -> Exp a -> Exp0 a

  -- Potentially initialized read/write variable
  ELetRef :: Name -> Either Ty (Exp a) -> Exp a -> Exp0 a

  ESeq :: Exp a -> Exp a -> Exp0 a
  ECall :: Exp a -> [Exp a] -> Exp0 a
  EIf :: Exp a -> Exp a -> Exp a -> Exp0 a

  -- Print any expression, for debugging
  EPrint :: Bool -> Exp a -> Exp0 a

  -- Generate runtime failure, with error report
  EError :: String -> Exp0 a
  ELUT :: Map Name Range -> Exp a -> Exp0 a

  -- Permute a bit array: In the long run this should probably
  -- become a generalized array read but for now I am keeping it as
  -- is.
  --  e1 : arr[N] bit   e2 : arr[N] int
  --  ------------------------------------
  --   EBPerm e1 e2  : arr[N] bit
  EBPerm :: Exp a -> Exp a -> Exp0 a

  -- Constructing structs
  EStruct :: TyName -> [(String,Exp a)] -> Exp0 a
  -- Project field out of a struct
  EProj   :: Exp a -> String -> Exp0 a
  deriving Generic


isEVal :: Exp a -> Bool
isEVal e
  | EVal {} <- unExp e
  = True
isEVal _
  = False

-- Convenience constructors

vint :: Int -> Val
-- Auxiliary function for use in the vectorizer
vint n = VInt (fromIntegral n)

eVal :: Maybe SourcePos -> a -> Val -> Exp a
eVal loc a v = MkExp (EVal v) loc a
eValArr :: Maybe SourcePos -> a ->  [Val] -> Exp a
eValArr loc a v = MkExp (EValArr v) loc a
eVar :: Maybe SourcePos -> a ->  Name -> Exp a
eVar loc a v = MkExp (EVar v) loc a
eUnOp :: Maybe SourcePos -> a -> UnOp -> Exp a -> Exp a
eUnOp loc a o v = MkExp (EUnOp o v) loc a
eBinOp :: Maybe SourcePos -> a -> BinOp -> Exp a -> Exp a -> Exp a
eBinOp loc a b x y = MkExp (EBinOp b x y) loc a
eAssign :: Maybe SourcePos -> a ->  Exp a -> Exp a -> Exp a
eAssign loc a x y = MkExp (EAssign x y) loc a
eArrRead :: Maybe SourcePos -> a ->  Exp a -> Exp a -> LengthInfo -> Exp a
eArrRead loc a x y l = MkExp (EArrRead x y l) loc a
eArrWrite :: Maybe SourcePos -> a ->  Exp a -> Exp a -> LengthInfo -> Exp a -> Exp a
eArrWrite loc a x y l e = MkExp (EArrWrite x y l e) loc a
eIter :: Maybe SourcePos -> a -> Name -> Name -> Exp a -> Exp a -> Exp a
eIter loc a x y e1 e2 = MkExp (EIter x y e1 e2) loc a
eFor :: Maybe SourcePos -> a -> UnrollInfo -> Name -> Exp a -> Exp a -> Exp a -> Exp a
eFor loc a ui n e1 e2 e3 = MkExp (EFor ui n e1 e2 e3) loc a
eLet :: Maybe SourcePos -> a ->  Name -> ForceInline -> Exp a -> Exp a -> Exp a
eLet loc a x fi e1 e2 = MkExp (ELet x fi e1 e2) loc a
eLetRef :: Maybe SourcePos -> a ->  Name -> Either Ty (Exp a) -> Exp a -> Exp a
eLetRef loc a nm x e = MkExp (ELetRef nm x e) loc a
eSeq :: Maybe SourcePos -> a ->  Exp a -> Exp a -> Exp a
eSeq loc a e1 e2 = MkExp (ESeq e1 e2) loc a
eCall :: Maybe SourcePos -> a ->  Exp a -> [Exp a] -> Exp a
eCall loc a e es = MkExp (ECall e es) loc a
eIf :: Maybe SourcePos -> a ->  Exp a -> Exp a -> Exp a -> Exp a
eIf loc a e1 e2 e3 = MkExp (EIf e1 e2 e3) loc a
ePrint :: Maybe SourcePos -> a ->  Bool -> Exp a -> Exp a
ePrint loc a b e = MkExp (EPrint b e) loc a
eError :: Maybe SourcePos -> a ->  String -> Exp a
eError loc a s = MkExp (EError s) loc a
eLUT :: Maybe SourcePos -> a ->  Map Name Range -> Exp a -> Exp a
eLUT loc a m e = MkExp (ELUT m e) loc a
eBPerm :: Maybe SourcePos -> a ->  Exp a -> Exp a -> Exp a
eBPerm loc a e1 e2 = MkExp (EBPerm e1 e2) loc a
eStruct :: Maybe SourcePos -> a ->  TyName -> [(String,Exp a)] -> Exp a
eStruct loc a tn es = MkExp (EStruct tn es) loc a
eProj :: Maybe SourcePos -> a ->  Exp a -> String -> Exp a
eProj loc a e s = MkExp (EProj e s) loc a


eWhile :: Maybe SourcePos -> a -> Exp a -> Exp a -> Exp a
eWhile loc a econd ebody = MkExp (EWhile econd ebody) loc a


isArrTy :: Ty -> Bool
isArrTy (TArr n _) = True
isArrTy _          = False

getArrTy :: Ty -> Ty
getArrTy (TArr n t) = t
getArrTy t          = t


expEq :: Exp a -> Exp a -> Bool
-- Are these two expressions /definitely/ equal?
expEq e e' = expEq0 (unExp e) (unExp e')
  where
    expEq0 (EVal v) (EVal v') = v == v'
    expEq0 (EVar x) (EVar y)  = x == y
    expEq0 (EArrRead e1 e2 li) (EArrRead e1' e2' li')
      = expEq e1 e1' && expEq e2 e2' && (li == li')
    expEq0 (EUnOp u1 e1) (EUnOp u1' e1')
      = (u1 == u1') && expEq e1 e1'
    expEq0 (EBinOp b e1 e2) (EBinOp b' e1' e2')
      = (b == b') && expEq e1 e1' && expEq e2 e2'
    expEq0 _e _e' = False

data Exp a
  = MkExp { unExp :: Exp0 a
          , expLoc :: Maybe SourcePos
          , info :: a }
  deriving (Generic)

type SrcExp = Exp ()


toExp :: a -> Exp0 a -> Exp a
toExp a e = MkExp { unExp = e, expLoc = Nothing, info = a }

toExpPos :: a -> SourcePos -> Exp0 a -> Exp a
toExpPos a pos e = MkExp { unExp = e, expLoc = Just pos, info = a }

binopList :: BinOp -> a -> Exp a -> [Exp a] -> Exp a
binopList op a e0 []       = e0
binopList op a e0 (e : es) = toExp a $ EBinOp op e (binopList op a e0 es)

data NumExpr where
  Literal :: Int -> NumExpr

  -- NVar: Length to be inferred from the context
  -- Int parameter denotes the max length seen
  -- and is used as a return of length function on polymorphic arrays
  -- to denote the max array size that can occur in the program
  NVar :: Name -> Int -> NumExpr

  -- NArr: Length is the same as the length of the array of the given name
  NArr :: Name -> NumExpr

  deriving (Generic, Eq)

data BitWidth
  = BW8
  | BW16
  | BW32
  | BW64
  | BWUnknown BWVar
  deriving (Generic, Eq, Show)

type BWVar = String


tint64  = TInt BW64
tint32  = TInt BW32
tint16  = TInt BW16
tint8   = TInt BW8
tint    = tint32

tdouble = TDouble Full

data Ty where
  -- TVars are just strings since they don't appear in user programs
  TVar      :: TyVar -> Ty
  TUnit     :: Ty
  TBit      :: Ty
  TBool     :: Ty
  TString   :: Ty                       -- Currently we have very limited supports for strings -
                                        -- they can only be printed
  TArr      :: NumExpr -> Ty -> Ty
  TInt      :: BitWidth -> Ty
  TDouble   :: Precision -> Ty
  TStruct   :: TyName -> Ty
  TInterval :: Int -> Ty

  -- Arrow and buffer types
  TArrow :: [Ty] -> Ty -> Ty
  TBuff  :: BufTy -> Ty

  deriving (Generic, Eq)

data BufTy
  = IntBuf { bufty_ty   :: Ty }   -- Identifier and type of buffer
  | ExtBuf { bufty_base :: Ty }   -- *Base* type of buffer
  deriving (Generic, Eq)

type BufId  = String
type TyName = String


-- For now
type TyVar = String


-- Structure definitions
data StructDef
  = StructDef { struct_name :: String
              , struct_flds :: [(String,Ty)] }
  deriving (Generic)

dotDotName = toName "..." Nothing Nothing

-- Just for debugging purposes
unknownTArr :: Ty
unknownTArr = TArr (NVar dotDotName 0) (TVar (name dotDotName))

unknownTArrOfBase :: Ty -> Ty
unknownTArrOfBase t = TArr (NVar dotDotName 0) t

unknownTFun :: Int -> Ty
unknownTFun n = TArrow (replicate n (TVar (name dotDotName)))
                       (TVar (name dotDotName))


-- Observations/questions about the following code
-- (a) Excludes Bit. Not sure why. (TODO)
-- (b) Type variables are scalars but after subst they may not be?
-- (c) Structs are assumed to be scalars. Probably that's fine as they
--     can be treated in a CBV fashion.
isScalarTy :: Ty -> Bool
isScalarTy ty =
  case ty of
    TVar {}      -> True
    TUnit        -> True
    TBit         -> False
    TInt {}      -> True
    TDouble {}   -> True
    TStruct {}   -> True
    TBool        -> True
    TString      -> True
    TInterval {} -> False
    TArr {}      -> False
    TArrow {}    -> False
    TBuff {}     -> False



-- Does this type support arithmetic operations?
supportsArithTy :: Ty -> Bool
supportsArithTy ty =
  case ty of
    TVar {}      -> True
    TInt {}      -> True
    TDouble {}   -> True
    TStruct {}   -> isComplexTy ty
    _other       -> False


-- Does this type support direct comparison (= in C)
supportsEqTy :: Ty -> Bool
supportsEqTy ty =
  case ty of
    TVar {}      -> True
    TUnit        -> True
    TBit         -> True
    TInt {}      -> True
    TDouble {}   -> True
    TStruct {}   -> True
    TBool        -> True
    TString      -> False
    _other       -> False

-- Does this type support <, <= etc?
supportsCmpTy :: Ty -> Bool
supportsCmpTy ty =
  case ty of
    TVar {}      -> True
    TInt {}      -> True
    TDouble {}   -> True
    TBool        -> True
    TString      -> False
    _other       -> False


isComplexTy :: Ty -> Bool
isComplexTy (TStruct tn)
  = any (== tn) [ complexTyName
                , complex8TyName
                , complex16TyName
                , complex32TyName
                ]
isComplexTy _other = False

complexTyName :: TyName
complexTyName = "complex"
complex8TyName :: TyName
complex8TyName = "complex8"
complex16TyName :: TyName
complex16TyName = "complex16"
complex32TyName :: TyName
complex32TyName = "complex32"
complex64TyName :: TyName
complex64TyName = "complex64"


tcomplex :: Ty
tcomplex = TStruct complex32TyName
tcomplex8 :: Ty
tcomplex8 = TStruct complex8TyName
tcomplex16 :: Ty
tcomplex16 = TStruct complex16TyName
tcomplex32 :: Ty
tcomplex32 = TStruct complex32TyName

tcomplex64 :: Ty
tcomplex64 = TStruct complex64TyName

-- Primitive complex structures
primComplexStructs :: [(TyName,StructDef)]
primComplexStructs
  = [ (complex8TyName,
          StructDef complex8TyName  [("re", TInt BW8),  ("im", TInt BW8)])
    , (complex16TyName,
          StructDef complex16TyName [("re", TInt BW16), ("im", TInt BW16)])
    , (complex32TyName,
          StructDef complex32TyName [("re", TInt BW32), ("im", TInt BW32)])
    , (complex64TyName,
          StructDef complex64TyName [("re", TInt BW64), ("im", TInt BW64)])
    ]

data Fun0 a where
  MkFunDefined  :: Name                       -- name
                -> [(Name, Ty)]               -- params
                -> [(Name,Ty,Maybe (Exp a))]  -- locals
                -> Exp a                      -- body
                -> Fun0 a
  MkFunExternal :: Name                       -- name
                -> [(Name, Ty)]               -- params
                -> Ty                         -- return type
                -> Fun0 a
  deriving (Generic)

{- TODO plug this in at some point
data FunDef a body
  = FunDef { funName   :: Name
           , funParams :: [(Name,Ty)]
           , funLocals :: [(Name,Ty,Maybe (Exp a))]
           , funDef    :: body }
-}

data Fun a
  = MkFun { unFun   :: Fun0 a
          , funLoc  :: Maybe SourcePos
          , funInfo :: a }
  deriving (Generic)

toFunPos a pos fn = MkFun fn (Just pos) a

funName :: Fun a -> Name
funName fn = get (unFun fn)
  where get (MkFunDefined nm params locals body) = nm
        get (MkFunExternal nm _ _) = nm


exprFVs' take_funs e
  = snd $ runState (mapExpM_aux return on_exp_action e) S.empty
  where on_exp_action x = on_exp (unExp x) >> return x
        -- NB: We collect the state in a bottom-up fashion
        on_exp (EVar nm)              = modify (\s -> S.union (S.singleton nm) s)
        on_exp (EFor _ x _e1 _e2 _e3) = modify (\s -> s S.\\ S.singleton x)
        on_exp (ELet x _fi _e1 _e2)   = modify (\s -> s S.\\ S.singleton x)
        on_exp (ELetRef x _e1 _e2)    = modify (\s -> s S.\\ S.singleton x)
        on_exp (EIter x v _e1 _e2)
          = do { modify (\s -> s S.\\ S.singleton x)
               ; modify (\s -> s S.\\ S.singleton v) }
        on_exp z@(ECall (MkExp (EVar nm) _ _) _es)
          | take_funs = return ()
          | otherwise = modify (\s -> s S.\\ S.singleton nm)
        on_exp _z = return ()


-- NB: Take function variables (hence True)
exprFVs = exprFVs' True
-- NB: Don't take function variables when computing fvs of closure
exprFVsClos = exprFVs' False

funFVs :: Fun a -> S.Set Name
funFVs f = case unFun f of
  MkFunDefined nm params locals body ->
    -- NB: important that we use foldr here instead of foldl
    (foldr (\(nm,_,me) s ->
             let se = case me of
                   Just e  -> S.union (exprFVs e) s
                   Nothing -> s
             in se S.\\ (S.singleton nm)) (exprFVs body) locals) S.\\
    (S.fromList $ map fst params)
  MkFunExternal nm params ty -> S.empty

funFVsClos :: Fun a -> S.Set Name
funFVsClos f = case unFun f of
  MkFunDefined nm params locals body ->
    -- NB: important that we use foldr here instead of foldl
    (foldr (\(nm,_,me) s ->
             let se = case me of
                   Just e  -> S.union (exprFVsClos e) s
                   Nothing -> s
             in se S.\\ (S.singleton nm)) (exprFVsClos body) locals) S.\\
    (S.fromList $ (map fst params) ++ (convTy params))
  MkFunExternal nm params ty -> S.empty
  -- We need to remove array length variables from the closure
  where getPolymArrTy (n, t)
          | (TArr (NVar nv s) ta) <- t = [nv]
          | (TArr (NArr nv) ta) <- t = [nv]
          | otherwise = []
        convTy (h:t) = (getPolymArrTy h) ++ (convTy t)
        convTy [] = []

mapTyM :: Monad m => (Ty -> m Ty) -> Ty -> m Ty
mapTyM f ty = go ty
  where go (TVar s)      = f (TVar s)
        go TUnit         = f TUnit
        go TBit          = f TBit
        go TBool         = f TBool
        go TString       = f TString
        go (TInt bw)     = f (TInt bw)
        go (TStruct tn)  = f (TStruct tn)
        go (TInterval n) = f (TInterval n)
        go (TDouble p)   = f (TDouble p)
        go (TArr n t)    = go t >>= \t' -> f (TArr n t')
        go (TArrow ts t) = do { ts' <- mapM go ts
                              ; t'  <- go t
                              ; f (TArrow ts' t') }
        go (TBuff (IntBuf bt)) = go bt >>= \bt' -> f (TBuff (IntBuf bt'))
        go (TBuff (ExtBuf bt)) = go bt >>= \bt' -> f (TBuff (ExtBuf bt'))

mapTy :: (Ty -> Ty) -> Ty -> Ty
mapTy f ty = runIdentity (mapTyM (Identity . f) ty)


mapExpM_aux :: Monad m
            => (a -> m b)           -- What to do on types
            -> (Exp b -> m (Exp b)) -- How to combine results
            -> Exp a
            -> m (Exp b)
mapExpM_aux on_ty f e = go e
  where
    go e
      = do { let loc = expLoc e
           ; nfo <- on_ty (info e)
           ; case unExp e of

              EVal v       -> f (eVal loc nfo v)

              EValArr varr -> f (eValArr loc nfo varr)

              EVar x       -> f (eVar loc nfo x)

              EUnOp op e1 ->
                do e1' <- go e1
                   f (eUnOp loc nfo op e1')

              EBinOp op e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eBinOp loc nfo op e1' e2')

              EAssign e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eAssign loc nfo e1' e2')

              EArrRead e1 e2 r ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eArrRead loc nfo e1' e2' r)

              EArrWrite e1 e2 r e3  ->
                do e1' <- go e1
                   e2' <- go e2
                   e3' <- go e3
                   f (eArrWrite loc nfo e1' e2' r e3')

              EIter nm1 nm2 e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eIter loc nfo nm1 nm2 e1' e2')

              EFor ui nm1 e1 e2 e3 ->
                do e1' <- go e1
                   e2' <- go e2
                   e3' <- go e3
                   f (eFor loc nfo ui nm1 e1' e2' e3')


              EWhile e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eWhile loc nfo e1' e2')

              ELet nm1 fi e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eLet loc nfo nm1 fi e1' e2')

              ELetRef nm1 (Left x) e2 ->
                do e2' <- go  e2
                   f (eLetRef loc nfo nm1 (Left x) e2')

              ELetRef nm1 (Right e1) e2 ->
                do e1' <- go  e1
                   e2' <- go  e2
                   f (eLetRef loc nfo nm1 (Right e1') e2')

              ESeq e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eSeq loc nfo e1' e2')

              ECall e1 es      ->
                do e1' <- go e1
                   es' <- mapM go es
                   f (eCall loc nfo e1' es')

              EIf e1 e2 e3 ->
                do e1' <- go e1
                   e2' <- go e2
                   e3' <- go e3
                   f (eIf loc nfo e1' e2' e3')

              EPrint nl e1 ->
                do e1' <- go  e1
                   f (ePrint loc nfo nl e1')

              EError err -> f (eError loc nfo err)

              ELUT r e1 ->
                do e1'  <- go e1
                   f (eLUT loc nfo r e1')

              EBPerm e1 e2 ->
                do e1' <- go e1
                   e2' <- go e2
                   f (eBPerm loc nfo e1' e2')

              EStruct tn tfs ->
                do { let do_fld (t,e) = go e >>= \e' -> return (t,e')
                   ; tfs' <- mapM do_fld tfs
                   ; f (eStruct loc nfo tn tfs') }

              EProj e1 fn ->
                do { e1' <- go e1
                   ; f (eProj loc nfo e1' fn) }
           }

mapExpM_ :: Monad m => (Exp a -> m (Exp a)) -> Exp a -> m (Exp a)
mapExpM_ f = mapExpM_aux return f


mapLocalsM :: Monad m
           => (Exp a -> m (Exp b))
           -> [(Name,Ty,Maybe (Exp a))]
           -> m [(Name,Ty,Maybe (Exp b))]
mapLocalsM g locs = mapLocalsAndTysM g return locs

mapLocalsAndTysM :: Monad m
                 => (Exp a -> m (Exp b))
                 -> (Ty -> m Ty)
                 -> [(Name,Ty,Maybe (Exp a))]
                 -> m [(Name,Ty,Maybe (Exp b))]
mapLocalsAndTysM g on_ty locs = mapM do_loc locs
  where do_loc (x,ty,Nothing)
          = do { ty' <- on_ty ty
               ; return (x,ty',Nothing)
               }
        do_loc (x,ty,Just e)
            = do { e' <- g e
                 ; ty' <- on_ty ty
                 ; return (x,ty',Just e')
                 }


mapFunAndTysM :: Monad m
        => (a -> m b)           -- on types
        -> (Ty -> m Ty)         -- on annotated types
        -> (Exp a -> m (Exp b)) -- on expressions
        -> Fun a
        -> m (Fun b)
mapFunAndTysM on_ty on_concrete_ty on_exp fe
  = case unFun fe of
     MkFunDefined nm params locals body
       -> do { params' <- mapM on_param params
             ; locals' <- mapLocalsAndTysM on_exp on_concrete_ty locals
             ; body' <- on_exp body
             ; info' <- on_ty (funInfo fe)
             ; return $
               MkFun (MkFunDefined nm params' locals' body')
                     (funLoc fe) info'
             }
     MkFunExternal nm params res_ty
       -> do { info' <- on_ty (funInfo fe)
             ; params' <- mapM on_param params
             ; res_ty' <- on_concrete_ty res_ty
             ; return $
               MkFun (MkFunExternal nm params' res_ty') (funLoc fe) info'
             }
  where on_param (pn,pt) = do { pt' <- on_concrete_ty pt; return (pn,pt') }

mapFunM :: Monad m
        => (a -> m b)           -- on types
        -> (Exp a -> m (Exp b)) -- on expressions
        -> Fun a
        -> m (Fun b)
mapFunM on_ty on_exp fe = mapFunAndTysM on_ty return on_exp fe

substExp :: Monad m => (Name, Exp a) -> Exp a -> m (Exp a)
substExp (nm,e') e = mapExpM_ subst_var e
  where subst_var e0
          | EVar nm' <- unExp e0
          = if nm == nm' then return e' else return e0
          | otherwise
          = return e0

substLength :: Monad m => (Name, NumExpr) -> Exp Ty -> m (Exp Ty)
-- Substitute lengths through
substLength (nm,numexpr) e
  = mapExpM_aux (substLengthTy (nm,numexpr)) return e

substLengthTy :: Monad m => (Name,NumExpr) -> Ty -> m Ty
substLengthTy (nm,numexpr) = mapTyM on_ty
  where on_ty (TArr (NVar nm' m) ty)
          | nm == nm'
          = return (TArr numexpr ty)
        on_ty ty_other
          = return ty_other

substAllLengthTy :: Monad m => [(Name,NumExpr)] -> Ty -> m Ty
substAllLengthTy substs t
  = foldM (\x p -> substLengthTy p x) t substs

substAllTyped :: Monad m
              => [(Name, Exp Ty)]
              -> [(Name,NumExpr)]
              -> Exp Ty
              -> m (Exp Ty)
substAllTyped substs len_substs e
  = do { e' <- foldM (\x p -> substLength p x) e len_substs
       ; foldM (\x p -> substExp p x) e' substs
       }


substAll :: Monad m => [(Name, Exp a)] -> Exp a -> m (Exp a)
substAll substs e = foldM (\x p -> substExp p x) e substs


eraseExp :: Exp a -> Exp ()
eraseExp e
   = runIdentity $
     mapExpM_aux (\t -> return ()) return e

eraseFun f
   = runIdentity $ mapFunM (\t -> return ()) (return . eraseExp) f

eraseLocals locs
   = runIdentity $ mapLocalsM (return . eraseExp) locs


isArithBinOp Add   = True
isArithBinOp Sub   = True
isArithBinOp Mult  = True
isArithBinOp Div   = True
isArithBinOp Rem   = True
isArithBinOp Expon = True
isArithBinOp _     = False

isShiftBinOp ShL = True
isShiftBinOp ShR = True
isShiftBinOp _   = False


isLogicalBinOp BwAnd = True
isLogicalBinOp BwOr  = True
isLogicalBinOp BwXor = True
isLogicalBinOp _     = False

isEqualityBinOp Eq  = True
isEqualityBinOp Neq = True
isEqualityBinOp _   = False


isRelBinOp Lt  = True
isRelBinOp Leq = True
isRelBinOp Gt  = True
isRelBinOp Geq = True
isRelBinOp _   = False

isBoolBinOp And = True
isBoolBinOp Or  = True
isBoolBinOp _   = False


-- Can this expression potentially change the state?
-- A super conservative side-effect analysis
mutates_state :: Exp a -> Bool
mutates_state e = case unExp e of
  EVal _                -> False
  EValArr _             -> False
  EVar nm               -> False
  EUnOp _ e'            -> mutates_state e'
  EBinOp _ e1 e2        -> any mutates_state [e1,e2]
  EAssign e1 e2         -> True

  EArrRead e1 e2 LISingleton   -> any mutates_state [e1,e2]

  EArrRead e1 e2 (LILength {}) -> any mutates_state [e1,e2]

  EArrWrite e1 e2 _r e3 -> True

  EIter _ _ e1 e2       -> any mutates_state [e1,e2]
  EFor _ _ e1 e2 e3     -> any mutates_state [e1,e2,e3]
  EWhile e1 e2          -> any mutates_state [e1,e2]

  ELet nm _fi e1 e2     -> any mutates_state [e1,e2]
  ELetRef nm (Right e1) e2 -> any mutates_state [e1,e2]
  ELetRef nm (Left {}) e2  -> mutates_state e2

  ESeq e1 e2     -> any mutates_state [e1,e2]
  ECall e' es    -> True
  EIf e1 e2 e3   -> any mutates_state [e1,e2,e3]

  EPrint nl e    -> True -- See Note [IOEffects]
  EError _       -> True

  ELUT _ e       -> mutates_state e
  EBPerm e1 e2   -> any mutates_state [e1,e2]

  EStruct tn tfs -> any mutates_state (map snd tfs)
  EProj e0 f     -> mutates_state e0


{-
Note [IOEffects]
~~~~~~~~~~~~~~~~

If an expression does not mutate state then we would like to not
execute it at all.  For instance, if we have:

     let _ = x[0:256]
     in (y+3)


then we should be able to rewrite this code to just (y+3) (and not
have to copy a useless value of 256 integers!)


So function mutates_state gives a conservative analysis about when can
an expression mutate or affect in some way the state of the
program. E.g. assignments and array writes do mutate state.


Also, 'error' and 'print' statements do affect the state by means of
writing to the console or terminating the program hence mutate_state
gives True for those.

-}




{-
Note [Polymorphic Arrays]
~~~~~~~~~~~~~~~~~~~~~~~~~

Arrays that may be polymorphic in their length arise from user code of
the form:

      (var x : arr int) <- ...

Now, it may turn out that such arrays end up with lengths that are
resolved. E.g. by the following assignment:

      x = y;   // where y is declared to be (arr[42] int)

we learn that the type of 'x' is an array of static size 42. But not
necessarily so, i.e. the length of 'x' may remain unconstrained until
we typecheck a function fully.

Moreover, arguments to functions may be polymorhic:

    f(x : arr int) { ...  }

This has the same effect. If x is not used in a way that fixes its
length in the body of the function then this remains polymorphic,
otherwise the length is fixed.


Note [Polymorphic Array Code Generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When (after type checking) we are in the process of generating code
for a declaration of a variable of array type, there exist several
cases:

(a) The variable has a length that has been statically resolved. This
    is easy, we simply declare the variable.

(b) The variable has length that has not been statically resolved yet,
    e.g. it's still a length variable.  Then it will be ensured that that
    variable is in scope (by type checking, see Note [Array Length Unification])
    and we will do a stack allocation for that variable of the required length.

Finally you can also say something like:

     (x : arr [length(y)] int)

meaning that x gets an array type with the same length as y, which
must be in scope at the program point where we refer to y. For instance
you can have:

     f(x : arr int, y : arr [length(x)] int) { ... }

Meaning: I don't know what is the size of 'x', in fact 'f' may be even
polymorphic in it, but I know that 'y' has that same length.

Note [Array Length Unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(a) Generation of length unification variables: An occurence of type
    (arr int) gives rise to a new unification variable for the length
    e.g. becomes (arr[l_32] int).

    NB: This is currently hardcoded in the AST and done by the parser
    which is unsatisfactory but not the end of the world. It'd be nicer
    if unification variables were born only in the type checker.

    TODO: Probably will do this.


(b) We wish to type check functions and let bindings in isolation so we
    maintain a map from length variables lvar |-> Literal n | lvar

    Whenever we meet an equation:  (arr [lvar] t) ~ (arr [42] t) we simply
    update the map unifying lvar to 42. Similarly for equations between lvars.

    Whenever we meet an equation:
                (arr [lvar] t) ~ (arr [length(y)] t)
    we pick up the type of 'y' from the environment, it must be a
   (TArr numexpr _) and we simply recurse: arr [lvar] t ~ arr numexpr t

    This ensures that 'y' must be in the environment. Moreover,
    whenever we introduce a new type in the environment we check it is
    well formed, by checking if the 'length' variables are bound. This
    will happen when we check parameter binding, annotated let-bound
    definitions, and annotated monadic bind.

(c) At generalization boundaries (function definitions,
    let-bound expressions) we make sure that the only 'length' variables
    that we generalize over (i.e. locally generated - outside-generated) are
    bound in the function parameters. (For ordinary let-bindings I am thinking
    to not generalize over anything). Fail otherwise.


Q:

   How to generalize this to arbitrary numerical expressions instead
   of just 'length' of a previously scoped variable? How to specify the
   return type of a function, if it is polymorphic? Outs ...

         f(x : arr int) : arr (length(x)) int ???

   'x' is typically considered out of scope in typical programming
   languages.  But need not, i.e. it is in scope in hybrid type systems.

-}

{-------------------------------------------------------------------------------
  PrettyVal instances (used for dumping the AST)
-------------------------------------------------------------------------------}

instance PrettyVal BitWidth
instance PrettyVal Ty
instance PrettyVal BufTy
instance PrettyVal NumExpr
instance PrettyVal Precision
instance PrettyVal Name
instance PrettyVal BinOp
instance PrettyVal ForceInline
instance PrettyVal LengthInfo
instance PrettyVal UnOp
instance PrettyVal UnrollInfo
instance PrettyVal Val
instance PrettyVal StructDef

instance PrettyVal a => PrettyVal (Exp0 a)
instance PrettyVal a => PrettyVal (Exp a)
instance PrettyVal a => PrettyVal (Fun a)
instance PrettyVal a => PrettyVal (Fun0 a)
