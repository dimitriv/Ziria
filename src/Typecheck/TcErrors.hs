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
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
 
module TcErrors where

import AstExpr
import AstComp
import PpExpr
import PpComp

import Text.PrettyPrint.HughesPJ
import qualified Control.Monad.Error as E
import Text.Parsec.Pos

import qualified Text.Parsec as PS


import Data.Maybe ( maybe )

data ErrCtx 
  = CompErrCtx (Comp () ()) 
  | ExprErrCtx (Exp ())
  | SolvingInCts 
  | SolvingOutCts
  | GlobalDefs

data TyErr 
  = TyErr { err_ctxt     :: ErrCtx
          , err_pos      :: Maybe SourcePos
          , err_msg      :: Doc 
          , err_var_ctxt :: Doc }

ppTyErr :: TyErr -> Doc 
ppTyErr (TyErr ctxt pos msg var_ctxt)
  = vcat [ msg
         , pp_ctxt ctxt
         , text "At location:" <+> 
           text (maybe (error "BUG: Unknown location!") show pos)
         , var_ctxt
         ]
  where 
    pp_ctxt (CompErrCtx c) 
      = vcat [ text "When type checking computation:"
             , nest 2 $ ppComp c ] 
    pp_ctxt (ExprErrCtx e) 
      = vcat [ text "When type checking expression:"
             , nest 2 $ ppExp e ]
    pp_ctxt (SolvingInCts)
      = text "When solving constraints arising from read(s)"
    pp_ctxt (SolvingOutCts)
      = text "When solving constraints arising from write(s)"
    pp_ctxt (GlobalDefs)
      = text "" -- Not particularly helpful



expActualErr :: Ty -> Ty -> Exp a -> Doc
expActualErr exp_ty actual_ty exp
  = vcat [ text "Couldn't match expected type:" <+> ppTy exp_ty 
         , text "with actual type:            " <+> ppTy actual_ty
         , text "for expression:" <+> ppExp exp ]

nonFullAppErr :: Comp a b -> Doc
nonFullAppErr comp
  = vcat [ text "Computer/transformer not fully applied:" 
         , nest 2 $ ppComp comp
         ]

expectedButFound :: String -> String -> Comp a b -> Doc
expectedButFound expected found c
  = vcat [ text "Expected" <+> text expected 
                           <+> text "but found" 
                           <+> text found <> colon
         , nest 2 $ ppComp c
         ]

