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
{-
{-# LANGUAGE GADTs, RankNTypes, KindSignatures, ImpredicativeTypes,
    DataKinds, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ConstraintKinds #-}
-}
{-# LANGUAGE FlexibleInstances, GADTs #-}
-- | Define combinators to generate fragments of untyped terms
module AstCombinator where

import Text.Parsec.Pos (SourcePos)

import AstExpr
import AstComp
import AstUnlabelled  -- We don't offer combinators for adding labels

{-------------------------------------------------------------------------------
  XExpy
-------------------------------------------------------------------------------}

type XExp  = Maybe SourcePos -> Exp
type XComp = Maybe SourcePos -> Comp

class XExpy a where
  toXExp :: a -> XExp

instance XExpy XExp where
 toXExp x = x

instance XExpy (GName Ty) where
 toXExp nm loc = eVar loc nm

-- Instance for Int disabled, because we don't know what type we want the
-- int to be at
-- instance XExpy Int where
--  toXExp i loc = eVal loc ??? (VInt (fromIntegral i))

instance XExpy Bool where
 toXExp b loc = eVal loc TBool (VBool b)

instance XExpy () where
 toXExp () loc = eVal loc TUnit VUnit

instance XExpy Exp where
 toXExp e _loc = e -- Keep the expression location

{-------------------------------------------------------------------------------
  Generalized expression combinators (relies on XExpy)
-------------------------------------------------------------------------------}

(.:=) :: (XExpy a, XExpy b) => a -> b -> XExp
(.:=) e1 e2 loc = case unExp $ toXExp e1 loc of
    EArrRead earr start len -> eArrWrite loc earr start len (toXExp e2 loc)
    _other                  -> eAssign loc (toXExp e1 loc) (toXExp e2 loc)

(.*) :: (XExpy a, XExpy b) => a -> b -> XExp
(.*) xe1 xe2 loc = eBinOp loc Mult (toXExp xe1 loc) (toXExp xe2 loc)

(./) :: (XExpy a, XExpy b) => a -> b -> XExp
(./) xe1 xe2 loc = eBinOp loc Div (toXExp xe1 loc) (toXExp xe2 loc)

(.%) :: (XExpy a, XExpy b) => a -> b -> XExp
(.%) xe1 xe2 loc = eBinOp loc Rem (toXExp xe1 loc) (toXExp xe2 loc)

(.=) :: (XExpy a, XExpy b) => a -> b -> XExp
(.=) xe1 xe2 loc = eBinOp loc Eq (toXExp xe1 loc) (toXExp xe2 loc)

(.<) :: (XExpy a, XExpy b) => a -> b -> XExp
(.<) xe1 xe2 loc = eBinOp loc Lt (toXExp xe1 loc) (toXExp xe2 loc)

(.<=) :: (XExpy a, XExpy b) => a -> b -> XExp
(.<=) xe1 xe2 loc = eBinOp loc Leq (toXExp xe1 loc) (toXExp xe2 loc)

(.+) :: (XExpy a, XExpy b) => a -> b -> XExp
(.+) xe1 xe2 loc = eBinOp loc Add (toXExp xe1 loc) (toXExp xe2 loc)

(.-) :: (XExpy a, XExpy b) => a -> b -> XExp
(.-) xe1 xe2 loc = eBinOp loc Sub (toXExp xe1 loc) (toXExp xe2 loc)

{-------------------------------------------------------------------------------
  XCompy
-------------------------------------------------------------------------------}

class XCompy a where
  toXComp :: a -> XComp

instance XCompy XComp where
  toXComp x = x

instance XCompy Comp where
  toXComp x _loc = x

{-------------------------------------------------------------------------------
  Generalized computation combinators (relies on XExpy and XCompy)
-------------------------------------------------------------------------------}

xTakes :: Ty -> Ty -> Int -> XComp
xTakes a b n loc = cTake loc a b n

xEmit :: XExpy a => Ty -> a -> XComp
xEmit a xe loc = cEmit loc a (toXExp xe loc)

xEmits :: XExpy a => Ty -> a -> XComp
xEmits a xe loc = cEmits loc a (toXExp xe loc)

xTake :: Ty -> Ty -> XComp
xTake a b loc = cTake1 loc a b

xIf :: XExpy a => a -> XComp -> XComp -> XComp
xIf xe xc1 xc2 loc
  = cBranch loc (toXExp xe loc) (xc1 loc) (xc2 loc)

xLetE :: XExpy a => GName Ty -> ForceInline -> a -> XComp -> XComp
xLetE nm fi xe xcomp loc = cLetE loc nm fi (toXExp xe loc) (xcomp loc)

xReturn :: XExpy a => Ty -> Ty -> ForceInline -> a -> XComp
xReturn a b fi xe loc = cReturn loc a b fi (toXExp xe loc)

xDo :: XExpy a => Ty -> Ty -> ForceInline -> a -> XComp
xDo a b fi = xReturn a b fi

xTimes :: (XExpy a, XExpy b) => GName Ty -> a -> b -> XComp -> XComp
xTimes nm xs xl xc loc
  = cTimes loc AutoUnroll (toXExp xs loc) (toXExp xl loc) nm (xc loc)

xRepeat :: XComp -> XComp
xRepeat xc loc = cRepeat loc Nothing (xc loc)

xError :: Ty -> Ty -> Ty -> String -> XComp
xError a b u s loc = cReturn loc a b AutoInline (eError loc u s)

xBind :: XComp -> GName Ty -> XComp -> XComp
xBind c1 nm c2 loc = cBindMany loc (c1 loc) [(nm,c2 loc)]

{-------------------------------------------------------------------------------
  XRange
-------------------------------------------------------------------------------}

class XRange a where
 toRange :: a -> (XExp, LengthInfo)

-- instance XRange Int where
--   toRange a = (toXExp a, LISingleton)

instance XRange (GName Ty) where
  toRange a = (toXExp a, LISingleton)

instance XExpy a => XRange (a, Int) where
  toRange (a,b) = (toXExp a, LILength b)

(.!) :: (XExpy arr, XRange y) => arr -> y -> XExp
(.!) earr y loc =
    let (xe,li) = toRange y
    in eArrRead loc (toXExp earr loc) (xe loc) li

{-------------------------------------------------------------------------------
  BindLike
-------------------------------------------------------------------------------}

data ASTBind where
  ASTBind :: GName Ty -> XComp -> ASTBind
  ASTComp :: XComp -> ASTBind

class BindLike a where
  toASTBind :: a -> ASTBind

instance BindLike ASTBind where
  toASTBind x = x

instance BindLike XComp where
  toASTBind x = ASTComp x

instance BindLike Comp where
  toASTBind x = ASTComp (\_ -> x)

-- instance BindLike XExp where
--   toASTBind x = ASTComp (xReturn AutoInline x)

data BindLikeT where
  CMD :: BindLike a => a -> BindLikeT

xSeq :: [BindLikeT] -> XComp
xSeq = xSeq_ . ast_binds
  where
    ast_binds [] = []
    ast_binds ((CMD a):xs) = (toASTBind a : ast_binds xs)

    xSeq_ :: [ASTBind] -> XComp
    xSeq_ [ASTComp c]       loc = c loc
    xSeq_ (ASTBind x c1:cs) loc = cBindMany loc (c1 loc) [(x, xSeq_ cs loc)]
    xSeq_ (ASTComp c1  :cs) loc = cSeq loc (c1 loc) (xSeq_ cs loc)
    xSeq_ _                 _   = error "Empty sequence!"


(<:-) :: XCompy a => GName Ty -> a -> ASTBind
(<:-) n a = ASTBind n (toXComp a)

{- A test
test nm = xSeq [ CMD $ nm <:- xTakes (43 :: Int)
               , CMD $ xEmit nm
               , CMD $ xReturn (34::Int)
               ]
-}
