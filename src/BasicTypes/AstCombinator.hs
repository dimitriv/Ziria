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

{-# LANGUAGE GADTs, RankNTypes, KindSignatures, ImpredicativeTypes,
    DataKinds, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ConstraintKinds #-}

module AstCombinator where

import Text.Parsec.Pos
import qualified Data.Set as S

import AstExpr
import AstComp
import PpExpr
import PpComp

import Data.Maybe
import Data.Int

import Data.Functor.Identity ( Identity (..) )

import GHC.TypeLits


-- Define combinators to generate fragments of untyped terms
type XComp = Maybe SourcePos -> Comp () ()
type XExp  = Maybe SourcePos -> Exp ()

class XExpy a where
 toXExp :: a -> XExp 

instance XExpy Name where
 toXExp nm loc = eVar loc () nm

instance XExpy Int where
 toXExp i loc = eVal loc () (VInt i)

instance XExpy Bool where
 toXExp b loc = eVal loc () (VBool b)

instance XExpy () where
 toXExp () loc = eVal loc () VUnit

instance XExpy XExp where
 toXExp x = x 

instance XExpy (Exp a) where
 -- Keep the expression location
 toXExp e = \_ -> eraseExp e 

class XRange a where
 toRange :: a -> (XExp, LengthInfo)

instance XRange Int where
  toRange a = (toXExp a, LISingleton)

instance XRange Name where
  toRange a = (toXExp a, LISingleton)

instance XExpy a => XRange (a,Int) where
  toRange (a,b) = (toXExp a, LILength b)


(.!) :: (XExpy arr, XRange y) => arr -> y -> XExp
(.!) earr y loc = 
    let (xe,li) = toRange y
    in eArrRead loc () (toXExp earr loc) (xe loc) li

(.:=) :: (XExpy a, XExpy b) => a -> b -> XExp
(.:=) e1 e2 loc
  = case unExp $ toXExp e1 loc of
      EArrRead earr start len ->
         eArrWrite loc () earr start len (toXExp e2 loc)
      _other -> 
         eAssign loc () (toXExp e1 loc) (toXExp e2 loc)

(.*) :: (XExpy a, XExpy b) => a -> b -> XExp
(.*) xe1 xe2 loc 
  = eBinOp loc () Mult (toXExp xe1 loc) (toXExp xe2 loc)

(./) :: (XExpy a, XExpy b) => a -> b -> XExp
(./) xe1 xe2 loc 
  = eBinOp loc () Div (toXExp xe1 loc) (toXExp xe2 loc)

(.%) :: (XExpy a, XExpy b) => a -> b -> XExp
(.%) xe1 xe2 loc 
  = eBinOp loc () Rem (toXExp xe1 loc) (toXExp xe2 loc)

(.=) :: (XExpy a, XExpy b) => a -> b -> XExp
(.=) xe1 xe2 loc 
  = eBinOp loc () Eq (toXExp xe1 loc) (toXExp xe2 loc)

(.<) :: (XExpy a, XExpy b) => a -> b -> XExp
(.<) xe1 xe2 loc 
  = eBinOp loc () Lt (toXExp xe1 loc) (toXExp xe2 loc)

(.<=) :: (XExpy a, XExpy b) => a -> b -> XExp
(.<=) xe1 xe2 loc 
  = eBinOp loc () Leq (toXExp xe1 loc) (toXExp xe2 loc)

(.+) :: (XExpy a, XExpy b) => a -> b -> XExp
(.+) xe1 xe2 loc 
  = eBinOp loc () Add (toXExp xe1 loc) (toXExp xe2 loc)

(.-) :: (XExpy a, XExpy b) => a -> b -> XExp
(.-) xe1 xe2 loc 
  = eBinOp loc () Sub (toXExp xe1 loc) (toXExp xe2 loc)


xTakes :: XExpy a => a -> XComp
xTakes xe loc = cTake loc () (toXExp xe loc)

xEmit :: XExpy a => a -> XComp
xEmit xe loc = cEmit loc () (toXExp xe loc) 

xEmits :: XExpy a => a -> XComp
xEmits xe loc = cEmits loc () (toXExp xe loc)

xTake :: XComp
xTake loc = cTake1 loc ()

xIf :: XExpy a => a -> XComp -> XComp -> XComp 
xIf xe xc1 xc2 loc 
  = cBranch loc () (toXExp xe loc) (xc1 loc) (xc2 loc)

xLetE :: XExpy a => Name -> a -> XComp -> XComp
xLetE nm xe xcomp loc = cLetE loc () nm (toXExp xe loc) (xcomp loc)

xReturn :: XExpy a => a -> XComp
xReturn xe loc = cReturn loc () (toXExp xe loc)

xDo :: XExpy a => a -> XComp
xDo = xReturn 

xTimes :: (XExpy a, XExpy b) => Name -> a -> b -> XComp -> XComp
xTimes nm xs xl xc loc 
  = cTimes loc () (toXExp xs loc) (toXExp xl loc) nm (xc loc)

xError :: String -> XComp
xError s loc = cReturn loc () (eError loc () s)

xBind :: XComp -> Name -> XComp -> XComp
xBind c1 nm c2 loc = cBindMany loc () (c1 loc) [(nm,c2 loc)]

data ASTBind where
  ASTBind :: Name -> XComp -> ASTBind
  ASTComp :: XComp -> ASTBind

xSeq_ :: [ASTBind] -> XComp
xSeq_ [ASTComp c] loc = c loc 
xSeq_ ((ASTBind x c1):cs) loc = cBindMany loc () (c1 loc) [(x, xSeq_ cs loc)]
xSeq_ ((ASTComp c1):cs) loc   = cSeq loc () (c1 loc) (xSeq_ cs loc)
xSeq_ _ _ = error "Empty sequence!"

data BindLikeT where
  CMD :: BindLike a => a -> BindLikeT

xSeq :: [BindLikeT] -> XComp
xSeq as = xSeq_ (ast_binds as)
  where ast_binds [] = []
        ast_binds ((CMD a):xs) = (toASTBind a : ast_binds xs)


class BindLike a where
  toASTBind :: a -> ASTBind

instance BindLike ASTBind where
  toASTBind x = x 

instance BindLike XComp where 
  toASTBind x = ASTComp x

instance BindLike (Comp () ()) where
  toASTBind x = ASTComp (\_ -> x)

instance BindLike XExp where
  toASTBind x = ASTComp (xReturn x)

class XCompy a where 
  toXComp :: a -> XComp

instance XCompy XComp where
  toXComp x = x 

instance XCompy (Comp () ()) where
  toXComp x = \_ -> x

(<:-) :: XCompy a => Name -> a -> ASTBind
(<:-) n a = ASTBind n (toXComp a)

{- A test 
test nm = xSeq [ CMD $ nm <:- xTakes (43 :: Int)
               , CMD $ xEmit nm
               , CMD $ xReturn (34::Int)
               ]
-}

