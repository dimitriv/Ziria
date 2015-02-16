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
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Pretty-printing type classes instances
module PpExpr (nestingDepth, ppName, ppNameUniq, ppEs, ppBind) where

import Text.PrettyPrint.HughesPJ

import AstExpr
import Outputable

nestingDepth :: Int
nestingDepth = 2

{-------------------------------------------------------------------------------
  Outputable instances
-------------------------------------------------------------------------------}

instance Outputable ty => Outputable (GUnOp ty) where
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
    VBit b    -> text $ if b then "'1" else "'0"
    VInt n    -> integer n
    VDouble d -> double d
    VBool b   -> if b then text "true" else text "false"
    VString s -> text s
    VUnit     -> text "tt"

instance Outputable ForceInline where
  ppr ForceInline = brackets $ text "ForceInline"
  ppr NoInline    = brackets $ text "NoInline"
  ppr AutoInline  = text ""

instance Outputable ty => Outputable (GExp0 ty a) where
  ppr e = case e of
    EVal _ v        -> ppr v
    EValArr v       -> text "{" <> pprArr v <> text "}"
    EVar x          -> ppName x
    EUnOp op e      -> ppr op <> parens (ppr e)
    EBinOp op e1 e2 -> ppr e1 <> ppr op <> ppr e2
    EAssign e1 e2   -> ppr e1 <+> text ":=" $$ 
                       nest 2 (ppr e2)

    EArrRead earr eix LISingleton  -> ppr earr <> brackets (ppr eix)
    EArrRead earr eix (LILength n) -> ppr earr <> brackets ((ppr eix) <> text ":+" <> int n)
    EArrRead earr eix (LIMeta   x) -> ppr earr <> brackets ((ppr eix) <> text ":+" <> text  x)

    EArrWrite earr eix LISingleton eval ->
      ppr earr <> (brackets (ppr eix)) <+> text ":=" $$ 
      nest 2 (ppr eval)

    EArrWrite earr eix (LILength r) eval ->
      ppr earr <> (brackets $ (ppr eix) <> text ":+" <> int r) <+> text ":=" $$
      nest 2 (ppr eval)  

    EArrWrite earr eix (LIMeta x) eval ->
      ppr earr <> assign ":=" (brackets $ (ppr eix) <> text ":+" <> text x) (ppr eval)

    EFor ui ix estart elen ebody ->
      ppr ui <+>
       (text "for" <+>
         ppr ix <+> text "in" <+> brackets (ppr estart <> comma <+> ppr elen) <+> text "{" $$
         nest nestingDepth (ppr ebody) $$
         text "}"
       )

    EWhile econd ebody ->
      text "while" <+> parens (ppr econd) <+> text "{" $$
      nest nestingDepth (ppr ebody) $$
      text "}"

    ELet x fi e1 e2 ->
      text "let" <> ppr fi <+> ppBind x <+> text "=" $$
      nest 2 (ppr e1) $$
      text "in" $$
      ppr e2

    ELetRef x Nothing e2 ->
      text "var" <+> ppBind x <+> text "in" $$
      ppr e2

    ELetRef x (Just e1) e2 ->
      text "var" <+> assign ":=" (ppBind x) (ppr e1) <+> text "in" $$
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
    EError _ str     -> text "error " <+> text str
    ELUT _ e1        -> text "LUT" <+> ppr e1
    EProj e fn       -> ppr e <> text "." <> text fn

    EStruct t tfs ->
      let ppfe (fn,fe) = text fn <+> text "=" <+> ppr fe
      in ppr t <+> braces (hsep (punctuate comma (map ppfe tfs)))

    where assign s e1 e2 = e1 <+> text s <+> e2

instance Outputable UnrollInfo where
  ppr Unroll     = text "unroll"
  ppr NoUnroll   = text "nounroll"
  ppr AutoUnroll = empty

instance Outputable ty => Outputable (GExp ty a) where
  ppr = ppr . unExp

instance Outputable BitWidth where
  ppr bw = case bw of
    BW8  -> text "8"
    BW16 -> text "16"
    BW32 -> text "32"
    BW64 -> text "64"
    BWUnknown _nm -> text ""
    -- Or maybe print the name?

instance Outputable SrcBitWidth where
  ppr bw = case bw of
    SrcBW8  -> text "8"
    SrcBW16 -> text "16"
    SrcBW32 -> text "32"
    SrcBW64 -> text "64"

instance Outputable MutKind where
  ppr mk = text (show mk)

instance Outputable t => Outputable (GArgTy t) where
  ppr (GArgTy t m) = parens (ppr m <+> ppr t)

instance Outputable Ty where
  ppr ty = case ty of
    TVar x                 -> text "?" <> text x
    TUnit                  -> text "()"
    TBit                   -> text "bit"
    TInt bw                -> text "int" <> ppr bw
    TDouble                -> text "double"
    TBool                  -> text "bool"
    TString                -> text "string"
    TArray (Literal n) ty' -> text "arr" <> brackets (int n) <+> ppr ty'
    TArray (NVar n)    ty' -> text "arr" <> brackets (text (show n)) <+> ppr ty'
    TArrow tys tyres       -> parens (hsep (punctuate comma (map ppr tys))) <+> text "->" <+> ppr tyres
    TInterval n            -> text "interval" <> brackets (int n)
    TBuff (IntBuf t)       -> parens $ text "INTBUF" <> brackets (ppr t)
    TBuff (ExtBuf bt)      -> parens $ text "EXTBUF" <> brackets (text "base=" <> ppr bt)
    TStruct tyname _       -> text tyname 
    -- NOTE: If we change this to be the full type the instance for EStruct breaks

    TVoid                  -> text "void"

instance Outputable ty => Outputable (GStructDef ty) where
  -- TODO: Perhaps it would make more sense to show the entire thing
  ppr (StructDef nm _) = text nm

instance Outputable SrcTy where
  ppr ty = case ty of
    SrcTUnit      -> text "()"
    SrcTBit       -> text "bit"
    SrcTInt bw    -> text "int" <> ppr bw
    SrcTDouble    -> text "double"
    SrcTBool      -> text "bool"
    SrcTStruct nm -> text nm
    SrcInject  ty -> ppr ty
    SrcTyUnknown  -> empty
    SrcTArray (SrcLiteral n) ty'
      -> text "arr" <> brackets (int n) <+> ppr ty'
    SrcTArray (SrcNVar _loc) ty'
      -> text "arr" <> brackets empty <+> ppr ty'
    SrcTArray (SrcNArr n) ty'
      -> text "arr" <> brackets (text "length" <> parens (ppName n)) <+> ppr ty'

instance Outputable ty => Outputable (GFun ty a) where
  ppr fn = case unFun fn of
    MkFunDefined f params ebody ->
      ppName f <> parens (ppParams params) <+> text "=" $$
          nest nestingDepth (ppr ebody)
    MkFunExternal f params ty ->
      text (name f) <> parens (ppParams params) <+> text ":" <+> ppr ty

instance Outputable NumExpr where
  ppr ne = case ne of
    Literal i -> int i
    NVar n    -> text n
    -- TODO: here and elsewhere, are the quotes around the name intentional?

instance Outputable ty => Outputable (GName ty) where
  ppr ix = ppName ix

{-------------------------------------------------------------------------------
  Utility

  Many of these are used in the Comp pretty-printer too.
-------------------------------------------------------------------------------}

pprArr :: Outputable ty => [GExp ty a] -> Doc
pprArr (h:[]) = ppr h
pprArr (h:t)  = ppr h <> comma <+> pprArr t
pprArr []     = empty

ppEs :: (a -> Doc) -> Doc -> [a] -> Doc
ppEs f sep eargs = case eargs of
    []         -> empty
    e : []     -> f e
    e : eargs' -> f e <> sep <+> ppEs f sep eargs'

ppName :: GName ty -> Doc
ppName nm = text (name nm) -- <> braces (text $ uniqId nm)

ppNameUniq :: GName ty -> Doc
ppNameUniq nm = text (name nm) <> braces (text $ show $ uniqId nm)

ppBind :: Outputable ty => GName ty -> Doc
ppBind nm = ppName nm <+> colon <+> ppr (nameTyp nm)

ppParams :: Outputable ty => [GName ty] -> Doc
ppParams params =
  case params of
    []          -> empty
    x : []      -> ppMut (nameMut x) <> ppBind x 
    x : params' -> ppMut (nameMut x) <> ppBind x <> comma <+> ppParams params'

ppMut :: MutKind -> Doc
ppMut Imm = empty
ppMut Mut = text "var " 

{-------------------------------------------------------------------------------
  Show instances
-------------------------------------------------------------------------------}

instance Show (NumExpr) where show = render . ppr
instance Show Ty        where show = render . ppr
instance Outputable ty => Show (GStructDef ty) where show = render . ppr

instance Outputable ty => Show (GFun ty a)  where show = render . ppr
instance Outputable ty => Show (GUnOp ty)   where show = render . ppr
instance Outputable ty => Show (GExp0 ty a) where show = render . ppr
instance Outputable ty => Show (GExp ty a)  where show = render . ppr

