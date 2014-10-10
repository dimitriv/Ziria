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
{-# LANGUAGE RecordWildCards #-}
module TcErrors (
    ErrCtx(..)
  , TyErr(TyErr)
  , ppTyErr
  , expActualErr
  , nonFullAppErr
  , expectedButFound
  ) where

import Prelude hiding (exp)
import Text.Parsec.Pos
import Text.PrettyPrint.HughesPJ

import AstComp
import AstExpr
import Outputable
import PpComp ()
import PpExpr ()

data ErrCtx
  = CompErrCtx SrcComp
  | ExprErrCtx SrcExp
  | GlobalDefs -- TODO: Remove once we remove globals

data TyErr
  = TyErr { err_ctxt     :: ErrCtx
          , err_pos      :: Maybe SourcePos
          , err_msg      :: Doc
          , err_var_ctxt :: Doc }

ppTyErr :: TyErr -> Doc
ppTyErr TyErr{..}
  = vcat [ err_msg
         , pp_ctxt err_ctxt
         , text "At location:" <+>
           text (maybe (error "BUG: Unknown location!") show err_pos)
         , err_var_ctxt
         ]
  where
    pp_ctxt (CompErrCtx c)
      = vcat [ text "When type checking computation:"
             , nest 2 $ ppr c ]
    pp_ctxt (ExprErrCtx e)
      = vcat [ text "When type checking expression:"
             , nest 2 $ ppr e ]
    pp_ctxt (GlobalDefs)
      = text "" -- Not particularly helpful



expActualErr :: Ty -> Ty -> SrcExp -> Doc
expActualErr exp_ty actual_ty exp
  = vcat [ text "Couldn't match expected type:" <+> ppr exp_ty
         , text "with actual type:            " <+> ppr actual_ty
         , text "for expression:" <+> ppr exp ]

nonFullAppErr :: SrcComp -> Doc
nonFullAppErr comp
  = vcat [ text "Computer/transformer not fully applied:"
         , nest 2 $ ppr comp
         ]

expectedButFound :: String -> String -> SrcComp -> Doc
expectedButFound expected found c
  = vcat [ text "Expected" <+> text expected
                           <+> text "but found"
                           <+> text found <> colon
         , nest 2 $ ppr c
         ]

