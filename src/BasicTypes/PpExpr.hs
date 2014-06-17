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

module PpExpr where

import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.Mainland (Pretty(..), string)
import AstExpr

import Outputable 

nestingDepth = 2

instance Outputable UnOp where 
  ppr = ppUnOp

ppUnOp op =
  case op of
    NatExp -> text "exp"
    Neg -> text "-"    
    Not -> text "not"
    BwNeg -> text "~"
    Cast ty -> ppTy ty 
    ALength -> text "length"    


instance Outputable BinOp where 
  ppr = ppBinOp

ppBinOp op =
  case op of
    Add -> text "+"
    Sub -> text "-"
    Mult -> text "*"
    Div -> text "/"
    Rem -> text "%"    
    Expon -> text "**"

    ShL -> text "<<"
    ShR -> text ">>"
    BwAnd -> text "&"
    BwOr -> text "|"
    BwXor -> text "^"

    Eq -> text "=="
    Neq -> text "!="
    Lt -> text "<"
    Gt -> text ">"
    Leq -> text "<="
    Geq -> text ">="
    And -> text "&&"
    Or -> text "||"

instance Outputable Val where
  ppr = ppVal 


ppUI Unroll     c = text "unroll" <+> c
ppUI NoUnroll   c = text "nounroll" <+> c
ppUI AutoUnroll c = c

instance Outputable UnrollInfo where
  ppr x = ppUI x empty 


ppVal v =
  case v of
    VBit b -> text $ if b then "'1" else "'0"
    VInt n -> int n
    VDouble p d -> double d
    VBool b -> if b then text "true" else text "false"
    VString s -> text s
    VUnit -> text "tt"

ppValArr (h:[]) = ppVal h
ppValArr (h:t) = ppVal h <> comma <+> ppValArr t
ppValArr [] = empty


ppEs f sep eargs =
  case eargs of
    [] -> empty
    e : [] -> f e
    e : eargs' -> f e <> sep <+> ppEs f sep eargs'

ppExp0 e =
  case e of
    EVal v -> ppVal v
    EValArr v -> text "{" <> ppValArr v <> text "}"
    EVar x -> ppName x
    EUnOp op e -> ppUnOp op <> parens (ppExp e)
    EBinOp op e1 e2 -> ppExp e1 <> ppBinOp op <> ppExp e2
    EAssign e1 e2 -> assign ":=" (ppExp e1) (ppExp e2)
    EArrRead earr eix LISingleton -> ppExp earr <> brackets (ppExp eix)
    EArrRead earr eix (LILength n) -> ppExp earr <> brackets ((ppExp eix) <> text ":+" <> int n)

    EArrWrite earr eix LISingleton eval ->
      ppExp earr <> assign ":=" (brackets (ppExp eix)) (ppExp eval)

    EArrWrite earr eix (LILength r) eval ->
      ppExp earr <> assign ":=" (brackets $ (ppExp eix) <> text ":+" <> int r) (ppExp eval)

    EFor ui ix estart elen ebody ->
      ppUI ui $ 
       (text "for" <+>
         ppIx ix <+> text "in" <+> brackets (ppExp estart <> comma <+> ppExp elen) <+> text "{" $$
         nest nestingDepth (ppExp ebody) $$
         text "}"
       )

    EWhile econd ebody ->
      text "while" <+> parens (ppExp econd) <+> text "{" $$
      nest nestingDepth (ppExp ebody) $$
      text "}" 

    EIter ix val earr ebody ->
      text "for" <+>
        ppIx ix <> comma <+> text (name val) <+>
        text "in" <+> ppExp earr <+> text "{" $$
          nest nestingDepth (ppExp ebody) $$
        text "}" 
    ELet x e1 e2 ->
      text "let" <+> assign "=" (ppName x) (ppExp e1) $$
      text "in" <+> ppExp e2

    ELetRef x (Left ty) e2 ->
      text "letref" <+> ppName x <+> colon <+> ppTy ty $$ 
      text "in" <+> ppExp e2

    ELetRef x (Right e1) e2 ->
      text "letref" <+> assign ":=" (ppName x) (ppExp e1) $$
      text "in" <+> ppExp e2

    ESeq e1 e2 -> ppExp e1 <> semi $$ ppExp e2
    ECall f eargs -> ppExp f <> parens (ppEs ppExp comma eargs)
    EIf be e1 e2 -> text "if" <+> ppExp be <+> text "{" $$
                      nest nestingDepth (ppExp e1) $$
                    text "}" <+> text "else" <+> text "{" $$
                      nest nestingDepth (ppExp e2) $$
                    text "}"
    EPrint True e1   -> text "printl" <+> ppExp e1
    EPrint False e1  -> text "print" <+> ppExp e1
    EError str       -> text "error " <+> text str
    ELUT _ e1        -> text "LUT" <+> ppExp e1
    EBPerm e1 e2     -> text "bperm " <> parens (ppExp e1 <> comma <> ppExp e2)
    EProj e fn       -> ppExp e <> text "." <> text fn
    EStruct tn tfs   ->
      let ppfe (fn,fe) = text fn <+> text "=" <+> ppExp fe 
      in text tn <+> braces (hsep (punctuate comma (map ppfe tfs)))

    where assign s e1 e2 = e1 <+> text s <+> e2

ppIx :: Name -> Doc
ppIx ix =
    case mbtype ix of
      Nothing -> ppName ix 
      Just ty -> parens (ppName ix <+> char ':' <+> ppTy ty)

ppExp = ppExp0 . unExp


ppExpLoc e =
  (case expLoc e of
    Just pos -> text (show pos) 
    Nothing -> empty) 

ppBW bw = 
  case bw of BW8  -> text "8"
             BW16 -> text "16"
             BW32 -> text "32"
             BWUnknown nm -> text "" 
             -- Or maybe print the name?

ppTy ty =
  case ty of
    TVar x -> text "?" <> text x
    TUnit -> text "()"
    TBit -> text "bit"    
    TInt bw -> text "int" <> ppBW bw
    TDouble p -> text "double:" <> ppPrecision p
    TBool -> text "bool"
    TString -> text "string"
    TArr (Literal n) ty' 
       -> text "arr" <> brackets (int n) <+> ppTy ty' 
    TArr (NVar n m) ty' 
       -> text "arr" <> brackets (text (show n)) <+> text "(max: " <+> int m <+> text ")" <+> ppTy ty'
    TArr (NArr n) ty' -> text "arr" <> brackets (text ("arr " ++ (show n))) <+> ppTy ty' 
    TArrow tys tyres -> parens (hsep (punctuate comma (map ppTy tys))) <+> text "->" <+> ppTy tyres
    TInterval n -> text "interval" <> brackets (int n)
    TBuff (IntBuf t)  -> parens $ text "INTBUF" <> brackets (ppTy t)
    TBuff (ExtBuf bt) -> parens $ text "EXTBUF" <> brackets (text "base=" <> ppTy bt)

    TStruct tyname -> text tyname

ppName nm = text (name nm) -- only in debug mode we want this: <> braces (text $ uniqId nm)
 
ppDecls decls =
  case decls of
    [] -> empty
    (x,ty,Just einit) : decls' ->
      text "var" <+>
        ppName x <> text ":" <+> ppTy ty <+> text ":=" <+> ppExp einit <> semi $$
      ppDecls decls'
    (x,ty,Nothing) : decls' ->
      text "var" <+> ppName x <> text ":" <+> ppTy ty <> semi $$
      ppDecls decls'

ppParams params =
  case params of
    [] -> empty
    (x,ty) : [] -> ppName x <> text ":" <+> ppTy ty
    (x,ty) : params' -> 
      ppName x <> text ":" <+> ppTy ty <> comma <+> ppParams params'

ppList f sep l =
  case l of
    [] -> empty
    x : [] -> f x 
    x : l' -> sep (f x) (ppList f sep l')

ppFun fn =
  case unFun fn of
    MkFunDefined f params decls ebody ->
      ppName f <> parens (ppParams params) <+> text "=" $$
          nest nestingDepth (ppDecls decls) $$
          nest nestingDepth (ppExp ebody)
    MkFunExternal f params ty ->
      text (name f) <> parens (ppParams params) <+> text ":" <+> ppTy ty

ppNumExpr ne = 
  case ne of
    Literal i -> int i
    NVar n m  -> text (show (name n)) <+> (text " (max:") <+> int m <+> text ") "
    NArr n    -> text ("arr " ++ (show (name n)))

ppPrecision ne = 
  case ne of
    Full      -> text "full"
    Fixed p   -> text ("fixed " ++ (show p))
    Unknown n -> text ("unknown " ++ (show n))

    
instance Show (Exp0 a) where
  show e = render $ ppExp0 e

instance Show (Exp a) where
  show e = render $ ppExp e

instance Pretty Ty => Pretty (Exp Ty) where
    ppr = string . show

instance Show Ty where
  show ty = render $ ppTy ty

instance Show UnOp where 
  show up = render $ ppUnOp up

instance Pretty Ty where
    ppr = string . show

instance Show (Fun a) where
  show n = render $ ppFun n

instance Pretty (Fun a) where
    ppr = string . show

instance Show (NumExpr) where
  show n = render $ ppNumExpr n

