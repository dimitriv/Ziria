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
module PpComp where

import Text.PrettyPrint.HughesPJ

import AstExpr
import AstComp
import PpExpr

import Outputable

instance Outputable CTy0 where
  ppr = ppCTy0

ppCTy0 cty =
  case cty of
    TComp t1 t2 t3 ->
      text "ST" <+> parens (text "C" <+> ppTy t1) <+> ppTy t2 <+> ppTy t3
    TTrans t1 t2 ->
      text "ST" <+> text "T" <+> ppTy t1 <+> ppTy t2

instance Outputable CTy where
  ppr = ppCTy

ppCTy cty =
  case cty of
    CTBase cty -> ppCTy0 cty
    CTArrow tys cty
     -> parens (hsep (punctuate comma (map ppCParamTy tys))) <+>
        text "->" <+>
        ppCTy0 cty

ppCParamTy (CAExp ty)    = ppTy ty
ppCParamTy (CAComp cty0) = ppCTy0 cty0

myFromMaybe f d ma =
  case ma of
    Nothing -> d
    Just a  -> f a

ppParInfo info =
  let comb = case plInfo info of
               AlwaysPipeline _ _ -> text "|>>>|"
               NeverPipeline -> text ".>>>."
               MaybePipeline -> text ">>>"
      inBurst  = myFromMaybe (\sz -> int sz <> text "_") empty (inBurstSz info)
      outBurst = myFromMaybe (\sz -> int sz <> text "_") empty (outBurstSz info)
  in inBurst <> comb <> outBurst

ppTypedName nm =
  case mbtype nm of
    Just ty -> parens $ ppName nm <+> text ":" <+> ppTy ty
    Nothing -> ppName nm

ppComp0 ppComp printtypes ignorelet ignoreexp c =
  case c of
    Var x ->
      ppName x
    BindMany c1 xs_cs ->

         let go_pp c [] = ppComp c
             go_pp c ((x,c2):rest) =
               ppTypedName x <+> text "<-" <+> ppComp c <> semi $$
               go_pp c2 rest
         in text "seq" <+> vcat [ text "{" <+> go_pp c1 xs_cs
                                , text "}" ]

    Seq c1 c2 ->
      ppComp c1 <> semi $$
      ppComp c2

    Par parInfo c1 c2 ->
{-
      parens ( ppComp c1 <+> ppParInfo parInfo $$
               ppComp c2
             )
-}
      ppComp c1 <+> ppParInfo parInfo $$
      ppComp c2

    Let x c1 c2
      | ignorelet
      -> ppComp c2
      | otherwise
      -> text "let comp" <+> ppTypedName x <+> text "=" <+> ppComp c1 $$
         text "in" $$ ppComp c2

    LetStruct sdef c1
      | ignorelet || ignoreexp
      -> ppComp c1
      | otherwise
      -> text "struct " <+> text (struct_name sdef) <+> text "=" <+>
            (hsep $ punctuate (text ",") (map (\(f,t) ->
               text f <> colon <+> ppTy t) (struct_flds sdef))) $$
         text "in" $$
         ppComp c1

    LetE x _ e c
      | ignorelet || ignoreexp
      -> ppComp c
      | otherwise
      -> text "let" <+> ppTypedName x <+> text "=" <+> ppExp e $$
         text "in" $$
         ppComp c

    -- CL
    LetERef x ty Nothing c
       | ignorelet || ignoreexp
       -> ppComp c
       | otherwise
       -> text "letref" <+> ppName x <+> colon <+> ppTy ty $$
          text "in" $$
          ppComp c

    -- TODO: We should pretty-print the type annotation
    LetERef x _ty (Just e) c
       | ignorelet || ignoreexp
       -> ppComp c
       | otherwise
       -> text "letref" <+> assign ":=" (ppName x) (ppExp e) $$
          text "in" $$
          ppComp c

    LetHeader _ fn c
      | ignorelet || ignoreexp
      -> ppComp c
      | otherwise
      -> text "let" <+> ppFun fn $$
         text "in" $$
         ppComp c
    --
    LetFunC f params locls c1 c2
      | ignorelet || (ignoreexp && not (nested_letfuns c1))
      -> ppComp c2
      | otherwise
      -> text "let comp" <+> ppName f <+>
                           parens (ppCompParams params) <+> text "=" $$
           (if not ignoreexp then (nest nestingDepth (ppDecls locls))
            else empty) $$
           nest nestingDepth (ppComp c1) $$
         text "in" $$
         ppComp c2
    Call f eargs ->
      ppName f <+> parens (ppEs ppCallArg comma eargs)
    Emit e
      | ignoreexp -> text "emit ..."
      | otherwise -> text "emit" <+> ppExp e
    Emits e
      | ignoreexp -> text "emits ..."
      | otherwise -> text "emits" <+> ppExp e
    Return _ e
      | ignoreexp -> text "return ..."
      | otherwise -> text "return" <+> ppExp e
    Interleave c1 c2 ->
      ppComp c1 <+> text "<|>" <+> ppComp c2
    Branch e c1 c2 ->
      text "if" <+> ppExp e $$
      text "then" <+> ppComp c1 $$
      text "else" <+> ppComp c2
    Take1 ->
      text "take"
    Take e ->
      text "takes" <+> ppExp e
    Until e c ->
      text "until" <+> parens (ppExp e) <+> ppComp c
    While e c ->
      text "while" <+> parens (ppExp e) <+> ppComp c

    Times ui estart elen ix c ->
      ppUI ui $
       text "for" <+>
         ppIx ix <+> text "in" <+>
            brackets (ppExp estart <> comma <+> ppExp elen) $$
         nest nestingDepth (ppComp c)

    Repeat wdth c ->
      text "repeat" <> myFromMaybe ppVectWidth empty wdth <+>
                       ppComp c

    VectComp (n1,n2) c ->
      text "repeat" <> ppWidth (n1,n2) <+>
                       ppComp c

    Map wdth nm ->
      text "map" <> myFromMaybe ppVectWidth empty wdth <+> ppName nm
    Filter e ->
      text "filter" <+> ppExp e

    ReadSrc {} ->
      text "read"
    WriteSnk {} ->
      text "write"

    ReadInternal bid _typ ->
      text "read_internal[sq]" <> parens (text bid)
    WriteInternal bid ->
      text "write_internal[sq]" <> parens (text bid)

    Standalone c1 ->
      text "standalone" <> braces (ppComp c1)

    Mitigate t n1 n2 ->
      int n1 <> text "-mitigate" <> brackets (ppTy t) <> text "-" <> int n2
    -- CL
    where assign s e1 e2 = e1 <+> text s <+> e2
    --

ppVectWidth (Rigid _r ann) = ppWidth ann
ppVectWidth (UpTo _r ann)  = text "<=" <> ppWidth ann

ppWidth (i,j) = brackets $ (int i) <> text "," <> (int j)

ppCallArg (CAExp e)  = ppExp e
ppCallArg (CAComp c) = ppComp c

ppComp = ppComp0 ppComp False False False . unComp

ppCompPipeline = ppComp0 ppCompPipeline False True False . unComp

ppCompShortFold = ppComp0 ppCompShortFold False False True . unComp


ppCompAst cmp =
   brackets (text $ compShortName cmp) <+>
   ppComp0 (\c -> brackets (text $ compShortName c) <+> ppComp c)
           False False False (unComp cmp)

ppCompLoc c =
  (case compLoc c of
    Just pos -> text (show pos)
    Nothing -> empty)

ppProg p =
  case p of
    MkProg globs c ->
      ppDecls globs $$
      ppComp c

ppCompTyped x =
  let p1 = ppComp0 ppCompTyped True False False $ unComp x
      pty = ppCTy (compInfo x)
  in parens (p1 <+> text "::" <+> pty)

ppCompParams params =
  case params of
    [] -> empty
    (x, ty) : [] -> ppName x <> text ":" <+> ppCParamTy ty
    (x, ty) : params' ->
       ppName x <> text ":" <+> ppCParamTy ty <> comma <+> ppCompParams params'

ppCompTypedVect x =
  let p1  = ppComp0 ppCompTypedVect False True True $ unComp x
      cty  = compInfo x
      inty  = inTyOfCTyBase cty
      yldty = yldTyOfCTyBase cty
      arity (TArr (Literal n) _) = show n
      arity _                    = "1"
      ain   = arity inty
      ayld  = arity yldty

  in if isSimplComp (unComp x)
     then text ain <> text "-" <> braces p1 <> text "-" <> text ayld
     else p1

isSimplComp (Var {})      = True
isSimplComp (Call {})     = True
isSimplComp (Emit {})     = True
isSimplComp (Return {})   = True
isSimplComp (Take1 {})    = True
isSimplComp (Take {})     = True
isSimplComp (Map {})      = True
isSimplComp (Filter {})   = True
isSimplComp (ReadSrc {})  = True
isSimplComp (WriteSnk {}) = True
isSimplComp _             = False


nested_letfuns c
  = case mapCompM_ return aux c of
      Nothing -> True
      Just _  -> False
  where aux c | LetFunC {} <- unComp c = Nothing
              | Let {}     <- unComp c = Nothing
              | otherwise              = Just c


instance Show CTy0 where
  show cty = render $ ppCTy0 cty

instance Show CTy where
  show cty = render $ ppCTy cty

instance Show (Comp0 a b) where
  show c = render $ ppComp0 ppComp False False False c

instance Show (Comp a b) where
  show = render . ppComp

instance Show (Prog a b) where
  show = render . ppProg

