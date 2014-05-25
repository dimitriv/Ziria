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

ppComp0 ppComp printtypes ignorelet c =
  case c of
    Var x ->
      ppName x
    BindMany c1 xs_cs -> 
--  Just for debugging the structure of the AST
{- 
           ppComp c1 <+>
             (nest 2 $ brackets $ 
              vcat (map (\(x,cx) -> parens (ppTypedName x <> text "," <+> ppComp cx)) xs_cs))
-}
         
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
      ppComp c1 <+> ppParInfo parInfo $$ 
      ppComp c2
    Let x c1 c2
      | ignorelet 
      -> ppComp c2
      | otherwise
      -> text "let comp" <+> ppTypedName x <+> text "=" <+> ppComp c1 $$
         text "in" $$ ppComp c2

    LetStruct sdef c1
      | ignorelet
      -> ppComp c1
      | otherwise
      -> text "struct " <+> text (struct_name sdef) <+> text "=" <+> (hsep $ punctuate (text ",") (map (\(f,t) -> text f <> colon <+> ppTy t) (struct_flds sdef))) $$
         text "in" $$ ppComp c1

    LetE x e c
      | ignorelet 
      -> ppComp c
      | otherwise
      -> text "let" <+> ppTypedName x <+> text "=" <+> ppExp e $$
         text "in" $$ ppComp c
    LetFun _ fn c 
      | ignorelet 
      -> ppComp c
      | otherwise 
      -> text "let" <+> ppFun fn $$
         text "in" $$
         ppComp c
    LetExternal _ fn c
      | ignorelet
      -> ppComp c
      | otherwise
      -> text "let external" <+> ppFun fn $$
         text "in" $$
         ppComp c
    LetFunC f params locls c1 c2 
      | ignorelet
      -> ppComp c2
      | otherwise
      -> text "let comp" <+> ppName f <+> parens (ppCompParams params) <+> text "=" $$
           nest nestingDepth (ppDecls locls) $$
           nest nestingDepth (ppComp c1) $$
         text "in" $$
         ppComp c2
    Call f eargs ->
      ppName f <+> parens (ppEs ppCallArg comma eargs)
    Emit e ->
      text "emit" <+> ppExp e
    Emits e ->
      text "emits" <+> ppExp e 
    Return e ->
      text "return" <+> ppExp e
    Interleave c1 c2 ->
      ppComp c1 <+> text "<|>" <+> ppComp c2
    Branch e c1 c2 ->
      text "if" <+> ppExp e $$
      text "then" <+> ppComp c1 $$
      text "else" <+> ppComp c2
    Take1 ->
      text "take" 
    Take e ->
      text "takes"
    Until e c ->
      text "until" <+> parens (ppExp e) <+> ppComp c
    While e c ->
      text "while" <+> parens (ppExp e) <+> ppComp c

{- 
    Times e x c ->
      text "times" <+> ppExp e <+> parens (text "\\" <> text (name x) <> (text ".") <+> ppComp c)
-}

    Times estart elen ix c ->
      text "for" <+>
        ppIx ix <+> text "in" <+> brackets (ppExp estart <> comma <+> ppExp elen) $$ 
        nest nestingDepth (ppComp c)

    Repeat wdth c ->
      text "repeat" <> myFromMaybe (\n -> brackets (int (fst n) <> text "," <> int (snd n))) empty wdth <+> 
                      ppComp c
    Map wdth e ->
      text "map" <> myFromMaybe (\n -> brackets (int (fst n) <> text "," <> int (snd n))) empty wdth <+> ppExp e 
    Filter e ->
      text "filter" <+> ppExp e

    ReadSrc {} ->
      text "read"  -- <> parens (text bid)
    WriteSnk {} -> 
      text "write" -- <> parens (text bid)

    -- ReadSrc (ExternalBufId bid) ->
    --   text "read[sq]" <> parens (text bid)
    -- WriteSnk (ExternalBufId bid) -> 
    --   text "write[sq]" <> parens (text bid)

    ReadInternal bid _typ ->
      text "read_internal[sq]" <> parens (text bid)
    WriteInternal bid -> 
      text "write_internal[sq]" <> parens (text bid)

    Standalone c1 ->
      text "standalone" <> braces (ppComp c1)


ppCallArg (CAExp e)  = ppExp e
ppCallArg (CAComp c) = ppComp c

ppComp = ppComp0 ppComp False False . unComp

ppCompPipeline = ppComp0 ppCompPipeline False True . unComp

ppCompAst cmp = 
   brackets (text $ compShortName cmp) <+>
   ppComp0 (\c -> brackets (text $ compShortName c) <+> ppComp c) False False (unComp cmp)

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
  let p1 = ppComp0 ppCompTyped True False $ unComp x
      pty = ppCTy (compInfo x)
  in parens (p1 <+> text "::" <+> pty) 

ppCompParams params =
  case params of
    [] -> empty
    (x, ty) : [] -> ppName x <> text ":" <+> ppCParamTy ty
    (x, ty) : params' -> 
       ppName x <> text ":" <+> ppCParamTy ty <> comma <+> ppCompParams params'

ppCompTypedVect x =
  let p1  = ppComp0 ppCompTypedVect False True $ unComp x
      cty  = compInfo x
      inty  = inTyOfCTyBase cty
      yldty = yldTyOfCTyBase cty
      arity (TArr (Literal n) _) = show n 
      arity _                    = "1"
      ain   = arity inty
      ayld  = arity yldty

  in if isSimplComp (unComp x) then (text ain <> text "-" <> braces p1 <> text "-" <> text ayld)
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



instance Show CTy0 where
  show cty = render $ ppCTy0 cty

instance Show CTy where
  show cty = render $ ppCTy cty

instance Show (Comp0 a b) where
  show c = render $ ppComp0 ppComp False False c 

instance Show (Comp a b) where
  show = render . ppComp

instance Show (Prog a b) where
  show = render . ppProg

