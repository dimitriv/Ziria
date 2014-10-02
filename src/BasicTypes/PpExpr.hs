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
{-# LANGUAGe FlexibleContexts #-}
{-# LANGUAGe FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
-- | Pretty-printing type classes instances
module PpExpr (nestingDepth, ppName, ppDecls, ppEs, ppIx) where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.Mainland (Pretty)
import qualified Text.PrettyPrint.Mainland as Mainland

import AstExpr
import Outputable

nestingDepth :: Int
nestingDepth = 2

{-------------------------------------------------------------------------------
  Outputable instances
-------------------------------------------------------------------------------}

instance Outputable UnOp where
  ppr op = case op of
    NatExp  -> text "exp"
    Neg     -> text "-"
    Not     -> text "not"
    BwNeg   -> text "~"
    Cast ty -> ppr ty
    ALength -> text "length"

instance Outputable BinOp where
  ppr op = case op of
    Add   -> text "+"
    Sub   -> text "-"
    Mult  -> text "*"
    Div   -> text "/"
    Rem   -> text "%"
    Expon -> text "**"

    ShL   -> text "<<"
    ShR   -> text ">>"
    BwAnd -> text "&"
    BwOr  -> text "|"
    BwXor -> text "^"

    Eq    -> text "=="
    Neq   -> text "!="
    Lt    -> text "<"
    Gt    -> text ">"
    Leq   -> text "<="
    Geq   -> text ">="
    And   -> text "&&"
    Or    -> text "||"

instance Outputable Val where
  ppr v = case v of
    VBit b       -> text $ if b then "'1" else "'0"
    VInt n       -> integer n
    VDouble _p d -> double d
    VBool b      -> if b then text "true" else text "false"
    VString s    -> text s
    VUnit        -> text "tt"

instance Outputable (Exp0 a) where
  ppr e = case e of
    EVal v          -> ppr v
    EValArr v       -> text "{" <> pprArr v <> text "}"
    EVar x          -> ppName x
    EUnOp op e      -> ppr op <> parens (ppr e)
    EBinOp op e1 e2 -> ppr e1 <> ppr op <> ppr e2
    EAssign e1 e2   -> assign ":=" (ppr e1) (ppr e2)

    EArrRead earr eix LISingleton  -> ppr earr <> brackets (ppr eix)
    EArrRead earr eix (LILength n) -> ppr earr <> brackets ((ppr eix) <> text ":+" <> int n)

    EArrWrite earr eix LISingleton eval ->
      ppr earr <> assign ":=" (brackets (ppr eix)) (ppr eval)

    EArrWrite earr eix (LILength r) eval ->
      ppr earr <> assign ":=" (brackets $ (ppr eix) <> text ":+" <> int r) (ppr eval)

    EFor ui ix estart elen ebody ->
      ppr ui <+>
       (text "for" <+>
         ppIx ix <+> text "in" <+> brackets (ppr estart <> comma <+> ppr elen) <+> text "{" $$
         nest nestingDepth (ppr ebody) $$
         text "}"
       )

    EWhile econd ebody ->
      text "while" <+> parens (ppr econd) <+> text "{" $$
      nest nestingDepth (ppr ebody) $$
      text "}"

    EIter ix val earr ebody ->
      text "for" <+>
        ppIx ix <> comma <+> text (name val) <+>
        text "in" <+> ppr earr <+> text "{" $$
          nest nestingDepth (ppr ebody) $$
        text "}"

    ELet x _fi e1 e2 ->
      text "let" <+> assign "=" (ppName x) (ppr e1) $$
      text "in" $$
      ppr e2

    ELetRef x ty Nothing e2 ->
      text "letref" <+> ppName x <+> colon <+> ppr ty $$
      text "in" $$
      ppr e2

    -- TODO: We should pretty-print the type here
    ELetRef x _ty (Just e1) e2 ->
      text "letref" <+> assign ":=" (ppName x) (ppr e1) $$
      text "in" $$
      ppr e2

    ESeq e1 e2       -> ppr e1 <> semi $$ ppr e2
    ECall f eargs    -> ppr f <> parens (ppEs ppr comma eargs)
    EIf be e1 e2     -> text "if" <+> ppr be <+> text "{" $$
                          nest nestingDepth (ppr e1) $$
                        text "}" <+> text "else" <+> text "{" $$
                          nest nestingDepth (ppr e2) $$
                        text "}"
    EPrint True e1   -> text "printl" <+> ppr e1
    EPrint False e1  -> text "print" <+> ppr e1
    EError str       -> text "error " <+> text str
    ELUT _ e1        -> text "LUT" <+> ppr e1
    EBPerm e1 e2     -> text "bperm " <> parens (ppr e1 <> comma <> ppr e2)
    EProj e fn       -> ppr e <> text "." <> text fn

    EStruct tn tfs   ->
      let ppfe (fn,fe) = text fn <+> text "=" <+> ppr fe
      in text tn <+> braces (hsep (punctuate comma (map ppfe tfs)))

    where assign s e1 e2 = e1 <+> text s <+> e2

instance Outputable UnrollInfo where
  ppr Unroll     = text "unroll"
  ppr NoUnroll   = text "nounroll"
  ppr AutoUnroll = empty

instance Outputable (Exp a) where
  ppr = ppr . unExp

instance Outputable BitWidth where
  ppr bw = case bw of
    BW8  -> text "8"
    BW16 -> text "16"
    BW32 -> text "32"
    BW64 -> text "64"
    BWUnknown _nm -> text ""
    -- Or maybe print the name?

instance Outputable Ty where
  ppr ty = case ty of
    TVar x               -> text "?" <> text x
    TUnit                -> text "()"
    TBit                 -> text "bit"
    TInt bw              -> text "int" <> ppr bw
    TDouble p            -> text "double:" <> ppr p
    TBool                -> text "bool"
    TString              -> text "string"
    TArr (Literal n) ty' -> text "arr" <> brackets (int n) <+> ppr ty'
    TArr (NVar n m) ty'  -> text "arr" <> brackets (text (show n)) <+> text "(max: " <+> int m <+> text ")" <+> ppr ty'
    TArr (NArr n) ty'    -> text "arr" <> brackets (text ("arr " ++ (show n))) <+> ppr ty'
    TArrow tys tyres     -> parens (hsep (punctuate comma (map ppr tys))) <+> text "->" <+> ppr tyres
    TInterval n          -> text "interval" <> brackets (int n)
    TBuff (IntBuf t)     -> parens $ text "INTBUF" <> brackets (ppr t)
    TBuff (ExtBuf bt)    -> parens $ text "EXTBUF" <> brackets (text "base=" <> ppr bt)
    TStruct tyname       -> text tyname

instance Outputable (Fun a) where
  ppr fn = case unFun fn of
    MkFunDefined f params decls ebody ->
      ppName f <> parens (ppParams params) <+> text "=" $$
          nest nestingDepth (ppDecls decls) $$
          nest nestingDepth (ppr ebody)
    MkFunExternal f params ty ->
      text (name f) <> parens (ppParams params) <+> text ":" <+> ppr ty

instance Outputable NumExpr where
  ppr ne = case ne of
    Literal i -> int i
    NVar n m  -> text (show (name n)) <+> (text " (max:") <+> int m <+> text ") "
    NArr n    -> text ("arr " ++ (show (name n)))

instance Outputable Precision where
  ppr ne = case ne of
    Full      -> text "full"
    Fixed p   -> text ("fixed " ++ (show p))
    Unknown n -> text ("unknown " ++ (show n))

{-------------------------------------------------------------------------------
  Utility

  Many of these are used in the Comp pretty-printer too.
-------------------------------------------------------------------------------}

pprArr :: [Val] -> Doc
pprArr (h:[]) = ppr h
pprArr (h:t)  = ppr h <> comma <+> pprArr t
pprArr []     = empty

ppEs :: (a -> Doc) -> Doc -> [a] -> Doc
ppEs f sep eargs = case eargs of
    []         -> empty
    e : []     -> f e
    e : eargs' -> f e <> sep <+> ppEs f sep eargs'

ppIx :: Name -> Doc
ppIx ix = case mbtype ix of
    Nothing -> ppName ix
    Just ty -> parens (ppName ix <+> char ':' <+> ppr ty)

ppName :: Name -> Doc
ppName nm = text (name nm) -- only in debug mode we want this: <> braces (text $ uniqId nm)

ppDecls :: [(Name, Ty, Maybe (Exp a))] -> Doc
ppDecls decls =
  case decls of
    [] -> empty
    (x,ty,Just einit) : decls' ->
      text "var" <+>
        ppName x <> text ":" <+> ppr ty <+> text ":=" <+> ppr einit <> semi $$
      ppDecls decls'
    (x,ty,Nothing) : decls' ->
      text "var" <+> ppName x <> text ":" <+> ppr ty <> semi $$
      ppDecls decls'

ppParams :: [(Name, Ty)] -> Doc
ppParams params =
  case params of
    [] -> empty
    (x,ty) : [] -> ppName x <> text ":" <+> ppr ty
    (x,ty) : params' ->
      ppName x <> text ":" <+> ppr ty <> comma <+> ppParams params'

{-------------------------------------------------------------------------------
  Show instances
-------------------------------------------------------------------------------}

instance Show (NumExpr) where show = render . ppr
instance Show (Fun a)   where show = render . ppr
instance Show UnOp      where show = render . ppr
instance Show Ty        where show = render . ppr
instance Show (Exp0 a)  where show = render . ppr
instance Show (Exp a)   where show = render . ppr

{-------------------------------------------------------------------------------
  Pretty instances
-------------------------------------------------------------------------------}

instance Pretty (Fun a) where
    ppr = Mainland.string . show

instance Pretty Ty where
    ppr = Mainland.string . show

instance Pretty Ty => Pretty (Exp Ty) where
    ppr = Mainland.string . show
