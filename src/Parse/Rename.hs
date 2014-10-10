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
module Rename (runRenM, renameProg) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Monad.State ()
import Data.Traversable (mapM)
import Data.Maybe (fromJust)
-- import Text.Show.Pretty (dumpStr)

import AstComp
import AstExpr
import Utils
import qualified GenSym as GS

-- A uniq environment mapping names to unique binding identifiers
type UniqEnv = [(GName SrcTy,Name)]

data RenM a = RenM { runRenM :: GS.Sym
                             -> UniqEnv
                             -> IO a }

instance Functor RenM where
  fmap f (RenM c) = RenM $ \sym env -> fmap f (c sym env)

instance Applicative RenM where
  pure = RenM . const . const . pure
  (RenM f) <*> (RenM x) = RenM $ \sym env -> f sym env <*> x sym env

instance Monad RenM where
  (>>=) m1 m2 =
    RenM $ \sym env ->
      do b <- runRenM m1 sym env
         runRenM (m2 b) sym env
  return x = RenM $ \_sym _env -> return x
  fail = failRenM

extendUniqEnv :: GName SrcTy -> Name -> RenM a -> RenM a
extendUniqEnv nm uniq (RenM action)
  = RenM (\sym env -> action sym ((nm,uniq):env))

extendUniqEnvMany :: [(GName SrcTy,Name)] -> RenM a -> RenM a
extendUniqEnvMany nms (RenM action)
  = RenM (\sym env -> action sym (nms ++ env))

renMIO :: IO a -> RenM a
renMIO m = RenM (\_ _ -> m)

get_RenMSym :: RenM GS.Sym
get_RenMSym = RenM (\sym _ -> return sym)

newUniq :: RenM String
newUniq = do { sym <- get_RenMSym
             ; str <- renMIO $ GS.genSymStr sym
             ; return ("_r" ++ str)
             }

lkupUniqEnv :: GName SrcTy -> RenM Name
lkupUniqEnv nm =
   RenM $ \_ env ->
     case lookup nm env of
       Just uniq -> return uniq
       Nothing   -> do { putStrLn $ "Unbound identifier: " ++ (name nm)
                       ; putStrLn $ "Location: " ++ (show (nameLoc nm))
                       ; error "Failing to compile." }

failRenM :: String -> RenM a
failRenM msg = renMIO $ putStrLn msg >> error "Failure"

renameBoundName :: GName SrcTy -> RenM Name
renameBoundName nm = do
  uniqId' <- newUniq
  mbtype' <- mapM renameType (mbtype nm)
  return MkName{ name    = name    nm
               , nameLoc = nameLoc nm
               , uniqId  = uniqId'
               , mbtype  = mbtype'
               }

renameFreeName :: GName SrcTy -> RenM Name
renameFreeName nm = do
  nm'     <- lkupUniqEnv nm
  mbtype' <- mapM renameType (mbtype nm)
  return MkName{ name    = name    nm
               , nameLoc = nameLoc nm
               , uniqId  = uniqId nm'
               , mbtype  = mbtype'
               }

renameType :: SrcTy -> RenM Ty
renameType SrcTUnit             = return TUnit
renameType SrcTBit              = return TBit
renameType SrcTBool             = return TBool
renameType (SrcTArr numExpr ty) = TArr <$> renameNumExpr numExpr <*> renameType ty
renameType (SrcTInt bw)         = TInt <$> renameBitWidth bw
renameType SrcTDouble           = return TDouble
renameType (SrcTStruct nm)      = return $ TStruct nm

renameNumExpr :: SrcNumExpr -> RenM NumExpr
renameNumExpr (SrcLiteral n) =
  return $ Literal n
renameNumExpr (SrcNArr nm) = do
  nm' <- lkupUniqEnv nm
  -- TODO: Really this belongs in the type checker, especially because of the
  -- first case. See #66.
  ne <- case mbtype nm' of
    Nothing                 -> fail "length of unknown array"
    Just (TArr numExpr _ty) -> return numExpr
    Just _                  -> fail "length of non-array type"
  return ne
renameNumExpr (SrcNVar loc) = do
  uniq <- newUniq
  -- This variable ranges over the _length_ of the array, and we assume that
  -- array lengths have type `tint`. This is _not_ the type of the _elements_
  -- of the array.
  let nm = toName uniq (Just loc) (Just tint)
  return $ NVar nm 0

renameBitWidth :: SrcBitWidth -> RenM BitWidth
renameBitWidth SrcBW8  = return BW8
renameBitWidth SrcBW16 = return BW16
renameBitWidth SrcBW32 = return BW32
renameBitWidth SrcBW64 = return BW64

renameCompType :: GCTy0 SrcTy -> RenM CTy0
renameCompType (TComp v a b) = TComp  <$> renameType v <*> renameType a <*> renameType b
renameCompType (TTrans a b)  = TTrans <$> renameType a <*> renameType b

renameStructDef :: GStructDef SrcTy -> RenM StructDef
renameStructDef StructDef{..} = do
  struct_flds' <- mapM (\(fld, ty) -> do ty' <- renameType ty ; return (fld, ty')) struct_flds
  return StructDef { struct_name = struct_name
                   , struct_flds = struct_flds'
                   }

renameRWTypeAnn :: GRWTypeAnn SrcTy -> RenM RWTypeAnn
renameRWTypeAnn (RWBaseTyAnn ty) = RWBaseTyAnn <$> renameType ty
renameRWTypeAnn (RWRealTyAnn ty) = RWRealTyAnn <$> renameType ty
renameRWTypeAnn RWNoTyAnn        = return RWNoTyAnn

renameExpr :: GExp SrcTy a -> RenM (Exp a)
renameExpr e
  = case unExp e of
    EVal v         -> return $ eVal eloc enfo v
    EValArr vs     -> return $ eValArr eloc enfo vs
    EVar nm        -> do { nm' <- renameFreeName nm
                         ; return $ eVar eloc enfo nm'
                         }
    EUnOp op e1    ->
      do { e1' <- renameExpr e1
         ; return $ eUnOp eloc enfo op e1'
         }
    EBinOp op e1 e2 ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         return $ eBinOp eloc enfo op e1' e2'

    EAssign e1 e2   ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         return $ eAssign eloc enfo e1' e2'
    EArrRead e1 e2 r ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         return $ eArrRead eloc enfo e1' e2' r

    EArrWrite e1 e2 r e3 ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         e3' <- renameExpr e3
         return $ eArrWrite eloc enfo e1' e2' r e3'
    EIter nm1 nm2 e1 e2 ->
      do { nm1' <- renameBoundName nm1
         ; nm2' <- renameBoundName nm2
         ; extendUniqEnv nm2 nm2' $
           extendUniqEnv nm1 nm1' $
              do e1' <- renameExpr e1
                 e2' <- renameExpr e2
                 return $ eIter eloc enfo nm1' nm2' e1' e2'
         }
    EFor ui nm1 e1 e2 e3   ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         nm1' <- renameBoundName nm1
         extendUniqEnv nm1 nm1' $
           do { e3' <- renameExpr e3
              ; return $ eFor eloc enfo ui nm1' e1' e2' e3'
              }
    EWhile e1 e2 ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         return $ eWhile eloc enfo e1' e2'

    ELet nm1 fi e1 e2   ->
      do e1' <- renameExpr e1
         nm1' <- renameBoundName nm1
         extendUniqEnv nm1 nm1' $
           do { e2' <- renameExpr e2
              ; return $ eLet eloc enfo nm1' fi e1' e2'
              }

    ELetRef nm1 _ty e1 e2   ->
      do e1' <- mapM renameExpr e1
         nm1' <- renameBoundName nm1
         -- ty'  <- renameType ty -- NOTE [Redundant types]
         let ty' = fromJust (mbtype nm1')
         extendUniqEnv nm1 nm1' $
           do { e2' <- renameExpr e2
              ; return $ eLetRef eloc enfo nm1' ty' e1' e2' }

    ESeq e1 e2       ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         return $ eSeq eloc enfo e1' e2'
    ECall e1 es      ->
      do e1' <- renameExpr e1
         es' <- mapM renameExpr es
         return $ eCall eloc enfo e1' es'
    EIf e1 e2 e3     ->
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         e3' <- renameExpr e3
         return $ eIf eloc enfo e1' e2' e3'
    EPrint nl e1   ->
      do e1' <- renameExpr e1
         return $ ePrint eloc enfo nl e1'
    EError str       -> return $ eError eloc enfo str
    ELUT r e1 ->
      do r' <- mapKeysM renameFreeName r
         e1' <- renameExpr e1
         return $ eLUT eloc enfo r' e1'
    EBPerm e1 e2 -> do
      e1' <- renameExpr e1
      e2' <- renameExpr e2
      return $ eBPerm eloc enfo e1' e2'

    EStruct tn tfs ->
      do { tfs' <- mapM (\(f,e') -> renameExpr e' >>= \e'' -> return (f,e'')) tfs
         ; return $ eStruct eloc enfo tn tfs' }

    EProj e1 fn ->
      do { e1' <- renameExpr e1
         ; return $ eProj eloc enfo e1' fn }
  where
    eloc = expLoc e
    enfo = info e


renameBinds :: [(GName SrcTy,GComp SrcTy a b)] -> RenM [(Name,Comp a b)]
renameBinds [] = return []
renameBinds ((nm,c):bnds) =
  do { nm' <- renameBoundName nm
     ; (c',bnds') <- extendUniqEnv nm nm' $
                     do { c' <- renameComp c
                        ; bnds' <- renameBinds bnds
                        ; return (c',bnds') }
    ; return $ (nm',c'):bnds' }

renameFun :: GFun SrcTy a -> RenM (Name, Fun a)
renameFun fe = case unFun fe of
  MkFunDefined nm params locals body ->
    do { nm'     <- renameBoundName nm
       ; (env1', params') <- renameParams params
       ; (env2', locals') <- extendUniqEnvMany env1' $ renameLocals locals
       ; body' <- extendUniqEnvMany (env2' ++ env1') $ renameExpr body
       ; return $ (nm', MkFun (MkFunDefined nm' params' locals' body')
                              (funLoc fe)
                              (funInfo fe))
      }

  MkFunExternal nm params retTy ->
    do { nm'     <- renameBoundName nm
       ; (env', params') <- renameParams params
       ; retTy'  <- extendUniqEnvMany env' $ renameType retTy
       ; return $ (nm', MkFun (MkFunExternal nm' params' retTy')
                              (funLoc fe)
                              (funInfo fe))
      }

-- | Rename function parameter list
--
-- Returns both the renamed parameter list and the name mapping.
--
-- NOTE: This is not a simply map, because the recursive calls happen inside
-- extended environments (these lists where later types can refer to earlier
-- types are sometimes called "telescopes").
renameParams :: [(GName SrcTy, SrcTy)]
             -> RenM ([(GName SrcTy, Name)], [(Name, Ty)])
renameParams []                         = return ([], [])
renameParams ((parNm, _parTy) : params) = do
    parNm' <- renameBoundName parNm
    -- parTy' <- renameType parTy -- NOTE [Redundant types]
    let parTy' = fromJust (mbtype parNm')
    (env, params') <- extendUniqEnv parNm parNm' $ renameParams params
    return ((parNm, parNm') : env, (parNm', parTy') : params')

-- | Like renameParams, but for comp functions.
renameCParams :: [(GName SrcTy, CallArg SrcTy (GCTy0 SrcTy))]
              -> RenM ([(GName SrcTy, Name)], [(Name, CallArg Ty CTy0)])
renameCParams []                        = return ([], [])
renameCParams ((parNm, parTy) : params) = do
    parNm' <- renameBoundName parNm
    -- NOTE [Redundant types]
    parTy' <- case parTy of
                CAExp  ty  -> CAExp  <$> renameType ty
                CAComp cty -> CAComp <$> renameCompType cty
    (env, params') <- extendUniqEnv parNm parNm' $ renameCParams params
    return ((parNm, parNm') : env, (parNm', parTy') : params')

-- | Rename locals list.
--
-- Returns both the renamed locals and the name mapping.
renameLocals :: [(GName SrcTy,SrcTy,Maybe (GExp SrcTy a))]
             -> RenM ([(GName SrcTy, Name)], [(Name,Ty,Maybe (Exp a))])
renameLocals []                                 = return ([], [])
renameLocals ((locNm, _locTy, locExp) : locals) = do
    locNm'  <- renameBoundName locNm
    -- locTy'  <- renameType locTy -- NOTE [Redundant types]
    let locTy' = fromJust (mbtype locNm')
    locExp' <- mapM renameExpr locExp
    (env, locals') <- extendUniqEnv locNm locNm' $ renameLocals locals
    return ((locNm, locNm') : env, (locNm', locTy', locExp') : locals')

renameComp :: GComp SrcTy a b -> RenM (Comp a b)
renameComp c =
  case unComp c of
    Var nm ->
      do { nm' <- renameFreeName nm
         ; return $ cVar cloc cnfo nm'
         }
    BindMany c1 xs_cs ->
      do { c1' <- renameComp c1
         ; xs_cs' <- renameBinds xs_cs
         ; return $ MkComp (mkBindMany c1' xs_cs') cloc cnfo
         }

    Seq c1 c2 ->
      do { c1' <- renameComp c1
         ; c2' <- renameComp c2
         ; return $ cSeq cloc cnfo c1' c2'
         }
    Par parInfo c1 c2 ->
      do { c1' <- renameComp c1
         ; c2' <- renameComp c2
         ; return $ cPar cloc cnfo parInfo c1' c2'
         }
    Let x c1 c2 ->
      do { c1' <- renameComp c1
         ; x'  <- renameBoundName x
         ; c2' <- extendUniqEnv x x' $ renameComp c2
         ; return $ cLet cloc cnfo x' c1' c2'
         }
    LetStruct sdef c1 ->
      do { c1'   <- renameComp c1
         ; sdef' <- renameStructDef sdef
         ; return $ cLetStruct cloc cnfo sdef' c1'
         }
    LetE x fi e c1 ->
      do { e'  <- renameExpr e
         ; x' <- renameBoundName x
         ; c1' <- extendUniqEnv x x' $ renameComp c1
         ; return $ cLetE cloc cnfo x' fi e' c1'
         }

    -- CL
    LetERef nm1 _ty e c1  ->
      do e'   <- mapM renameExpr e
         nm1' <- renameBoundName nm1
         -- ty'  <- renameType ty -- NOTE [Redundant types]
         let ty' = fromJust (mbtype nm1')
         extendUniqEnv nm1 nm1' $
           do { c1' <- renameComp c1
              ; return $ cLetERef cloc cnfo nm1' ty' e' c1' }

    LetHeader nm fun c2 ->
      do { (nm',fun')  <- renameFun fun
         ; c2' <- extendUniqEnv nm nm' $ renameComp c2
         ; return $ cLetHeader cloc cnfo nm' fun' c2'
         }

    LetFunC nm params locals c1 c2 ->
      do { (env1', params') <- renameCParams params
         ; (env2', locals') <- extendUniqEnvMany env1' $ renameLocals locals
         ; c1' <- extendUniqEnvMany (env2' ++ env1') $ renameComp c1
         ; nm' <- renameBoundName nm
         ; c2' <- extendUniqEnv nm nm' $ renameComp c2
         ; return $ cLetFunC cloc cnfo nm' params' locals' c1' c2'
         }

    Call nm es ->
      do { es' <- mapM renameCallArg es
         ; nm' <- renameFreeName nm
         ; return $ cCall cloc cnfo nm' es'
         }
    Emit e ->
      do { e' <- renameExpr e
         ; return $ cEmit cloc cnfo e'
         }
    Return fi e ->
      do { e' <- renameExpr e
         ; return $ cReturn cloc cnfo fi e'
         }
    Emits e ->
      do { e' <- renameExpr e
         ; return $ cEmits cloc cnfo e'
         }
    Interleave c1 c2 ->
      do { c1' <- renameComp c1
         ; c2' <- renameComp c2
         ; return $ cInterleave cloc cnfo c1' c2'
         }
    Branch e c1 c2 ->
      do { e'  <- renameExpr e
         ; c1' <- renameComp c1
         ; c2' <- renameComp c2
         ; return $ cBranch cloc cnfo e' c1' c2'
         }
    Take1 -> return $ cTake1 cloc cnfo
    Take e ->
      do { e' <- renameExpr e
         ; return $ cTake cloc cnfo e'
         }
    Until e c' ->
      do { e'  <- renameExpr e
         ; c'' <- renameComp c'
         ; return $ cUntil cloc cnfo e' c''
         }

    While e c' ->
      do { e'  <- renameExpr e
         ; c'' <- renameComp c'
         ; return $ cWhile cloc cnfo e' c''
         }

    Times ui e elen nm c' ->
      do { e'  <- renameExpr e
         ; elen' <- renameExpr elen
         ; nm' <- renameBoundName nm
         ; c'' <- extendUniqEnv nm nm' $ renameComp c'
         ; return $ cTimes cloc cnfo ui e' elen' nm' c''
         }
    Repeat wdth c' ->
      do { c'' <- renameComp c'
         ; return $ cRepeat cloc cnfo wdth c''
         }
    VectComp wdth c' ->
      do { c'' <- renameComp c'
         ; return $ cVectComp cloc cnfo wdth c''
         }
    Map wdth nm ->
      do { nm' <- renameFreeName nm
         ; return $ cMap cloc cnfo wdth nm'
         }
    Filter e ->
      do { e' <- renameExpr e
         ; return $ cFilter cloc cnfo e'
         }

    ReadSrc  ann -> do ann' <- renameRWTypeAnn ann
                       return $ cReadSrc cloc cnfo ann'
    WriteSnk ann -> do ann' <- renameRWTypeAnn ann
                       return $ cWriteSnk cloc cnfo ann'

    ReadInternal  s typ mq -> return $ cReadInternal cloc cnfo s typ mq
    WriteInternal s mq     -> return $ cWriteInternal cloc cnfo s mq

    Standalone c' ->
      do { c'' <- renameComp c'
         ; return $ cStandalone cloc cnfo c''
         }

    Sync {} -> taskControlRenamerBug
    Mitigate ty n1 n2 -> do
      ty' <- renameType ty
      return $ cMitigate cloc cnfo ty' n1 n2

  where cloc = compLoc c
        cnfo = compInfo c

taskControlRenamerBug :: a
taskControlRenamerBug =
  error "BUG: renamer hit a task sync node!"

renameCallArg :: CallArg (GExp SrcTy a) (GComp SrcTy a1 b)
              -> RenM (CallArg (Exp a) (Comp a1 b))
renameCallArg (CAExp e)  = do { e' <- renameExpr e ; return (CAExp e')  }
renameCallArg (CAComp c) = do { c' <- renameComp c ; return (CAComp c') }

renameProg :: GProg SrcTy a b -> RenM (Prog a b)
renameProg (MkProg globals comp) = do
  (initEnv, globals') <- renameLocals globals
  comp' <- extendUniqEnvMany initEnv $ renameComp comp
  return $ MkProg globals' comp'

{-
NOTE [Redundant types]

The AST contains redundant types in a number of places:

* As part of parameter lists for expression functions
* As part of locals for functions and the top-level program
* As part of var-declarations, both in the comp and in the expr language

In all these cases the source language contains a _single_ type annotation, say

    var x : int

but the AST contains the type _twice_: once as part of the name (`x`) as once
as an explicit type in the AST. This is a problem for the renamer in the case
of implicit variables; for instance, when the user writes

    var x : arr int

then this "arr int" (with unspecified length) expression occurs, again, twice
in the AST. Then when the renamer passes over the tree and starts to introduce
type variables, the result will be something like

    LetERef (Name "x_1" <arr[_r3] int>) <arr[_r4] int> ...

because the renamer sees two array types with an unspecified size and does
not know that they are related.

There are two possible solutions to this. One is to modify the type checker to
unify the two types in the type checking phase. This might make sense because
it means that if we later introduce incompatible types in these places in the
AST we would find it when we type check again. However, since we are planning
to remove this redundancy from the AST anyway, for now we've simply _ignored_
the redundant types and instead we do something like for function parameters:

``` Haskell
aux (parNm, _parTy) = do
  parNm' <- renameBoundName parNm
  -- parTy' <- renameType parTy -- NOTE [Redundant types]
  let parTy' = fromJust (mbtype parNm')
  return ((parNm, parNm'), (parNm', parTy'))
```

and likewise for the other cases.

NOTE: For some strange reason we do NOT record the type on the name of comp
function parameters, so we don't do this trick in `renameCParams`.
-}

