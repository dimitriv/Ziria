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
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, FlexibleInstances #-}
module Rename (RenM, runRenM, renProg, renComp, renExp, extend) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Monad.Reader hiding (mapM)
import Data.Traversable (mapM)

import AstComp
import AstExpr
import AstUnlabelled
import Utils
import qualified GenSym as GS

{-------------------------------------------------------------------------------
  Renamer monad
-------------------------------------------------------------------------------}

data RenEnv = RenEnv {
    renSym     :: GS.Sym
  , renUniqEnv :: [(String, String)] -- ^ Map names to unique IDs
  }

newtype RenM a = RenM (ReaderT RenEnv IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader RenEnv
           , MonadIO
           )

runRenM :: GS.Sym -> RenM a -> IO a
runRenM renSym (RenM ren) = do
  let renUniqEnv = []
  runReaderT ren RenEnv{..}

newUniq :: RenM String
newUniq = do
  sym <- asks renSym
  str <- liftIO $ GS.genSymStr sym
  return ("_r" ++ str)

extendUniq :: String -> String -> RenM a -> RenM a
extendUniq nm uniqId = local $ \env -> env {
      renUniqEnv = (nm, uniqId) : renUniqEnv env
    }

lookupUniq :: GName ty -> RenM String
lookupUniq nm = do
  env <- asks renUniqEnv
  case lookup (name nm) env of
    Just uniq -> return uniq
    Nothing   -> liftIO $ do
      putStrLn $ "Unbound identifier: " ++ name nm
      putStrLn $ "Location: " ++ show (nameLoc nm)
      error "Failing to compile."

_failRenM :: String -> RenM a
_failRenM msg = liftIO $ putStrLn msg >> error "Failure"

{-------------------------------------------------------------------------------
  The heart of renaming: renaming variables
-------------------------------------------------------------------------------}

renBound :: (ty -> RenM ty) -> GName ty -> RenM (GName ty)
renBound renty MkName{..} = do
  uniqId'  <- newUniq
  nameTyp' <- renty nameTyp
  return MkName{uniqId = uniqId', nameTyp = nameTyp', ..}

renFree :: (ty -> RenM ty) -> GName ty -> RenM (GName ty)
renFree renty nm@MkName{..} = do
  uniqId'  <- lookupUniq nm
  nameTyp' <- renty nameTyp
  return MkName{uniqId = uniqId', nameTyp = nameTyp', ..}

{-------------------------------------------------------------------------------
  Interacting with the environment
-------------------------------------------------------------------------------}

class Extend a where
  extend :: a -> a -> RenM b -> RenM b

instance Extend (GName ty) where
  extend nm nm' = extendUniq (name nm) (uniqId nm')

instance Extend (GName ty, a) where -- Locals, monadic bind
  extend (nm, _) (nm', _) = extend nm nm'

instance Extend SrcFun where
  extend fun fun' = extend (funName fun) (funName fun')

instance Extend a => Extend [a] where
  extend []     []     act = act
  extend (x:xs) (y:ys) act = extend x y $ extend xs ys act
  extend _      _      _   = fail "extend: length mismatch"

{-------------------------------------------------------------------------------
  Telescopes
-------------------------------------------------------------------------------}

-- | Rename a telescope
--
-- A /telescope/ is a list of binding sites where later items in the list
-- can refer to earlier items the list. A typical example is a list of
-- parameters, where we might have
--
-- > fun comp f(arr int x, arr[length(x)] int y) { .. }
renTelescope :: Extend a => (a -> RenM a) -> [a] -> RenM [a]
renTelescope f = go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- f x
      xs' <- extend x x' $ go xs
      return (x':xs')

-- | Local variables
renLocal :: (GName (Maybe SrcTy), Maybe SrcExp)
            -> RenM (GName (Maybe SrcTy), Maybe SrcExp)
renLocal (x, me) = do
    x'  <- renBound renTyAnn x
    me' <- mapM renExp me -- No recursion!
    return (x', me')

-- | Monadic bind
renBind :: (GName (Maybe SrcTy), SrcComp)
           -> RenM (GName (Maybe SrcTy), SrcComp)
renBind (x, c) = do
    x' <- renBound renTyAnn x
    c' <- extend x x' $ renComp c
    return (x', c')

{-------------------------------------------------------------------------------
  Renaming proper
-------------------------------------------------------------------------------}

{-
-- | Renaming does not change the type, just assigns unique IDs to variables.
class Rename a where
  ren :: a -> RenM a

instance Rename a => Rename (Maybe a) where
  ren = mapM ren

instance (Rename a, Rename b) => Rename (CallArg a b) where
  ren (CAExp  e) = CAExp  <$> ren e
  ren (CAComp c) = CAComp <$> ren c
-}

{-------------------------------------------------------------------------------
  Renaming types

  We have to ren types because types can refer to term variables (length(x)).
-------------------------------------------------------------------------------}

renTyAnn :: Maybe SrcTy -> RenM (Maybe SrcTy)
renTyAnn Nothing   = return Nothing
renTyAnn (Just ty) = Just <$> renTy ty

renCTyAnn :: Maybe (GCTy SrcTy) -> RenM (Maybe (GCTy SrcTy))
renCTyAnn Nothing    = return Nothing
renCTyAnn (Just cty) = Just <$> renCTy cty

-- Internal types injected into the source language are used for quasi-quoting
-- only, and therefore do not have to be rend.
renTy :: SrcTy -> RenM SrcTy
renTy SrcTUnit               = return SrcTUnit
renTy SrcTBit                = return SrcTBit
renTy SrcTBool               = return SrcTBool
renTy (SrcTArray numExpr ty) = SrcTArray <$> renNumExpr numExpr <*> renTy ty
renTy (SrcTInt bw)           = SrcTInt   <$> renBitWidth bw
renTy SrcTDouble             = return SrcTDouble
renTy (SrcTStruct nm)        = return $ SrcTStruct nm
renTy (SrcInject ty)         = return $ SrcInject ty

renNumExpr :: SrcNumExpr -> RenM SrcNumExpr
renNumExpr (SrcLiteral n) = return $ SrcLiteral n
renNumExpr (SrcNArr nm)   = SrcNArr <$> renFree renTyAnn nm
renNumExpr (SrcNVar loc)  = return $ SrcNVar loc

renBitWidth :: SrcBitWidth -> RenM SrcBitWidth
renBitWidth = return -- (source) bitwidths don't contain variable names

renCTy :: GCTy SrcTy -> RenM (GCTy SrcTy)
renCTy (CTVar _)          = panicStr "Unexpected type variable in source type"
renCTy (CTComp u a b)     = CTComp  <$> renTy u <*> renTy a <*> renTy b
renCTy (CTTrans a b)      = CTTrans <$> renTy a <*> renTy b
renCTy (CTArrow args res) = CTArrow <$> mapM renTyCTy args <*> renCTy res

renTyCTy :: CallArg SrcTy (GCTy SrcTy) -> RenM (CallArg SrcTy (GCTy SrcTy))
renTyCTy = callArg (liftM CAExp . renTy) (liftM CAComp . renCTy)

renTyCTyAnn :: CallArg (Maybe SrcTy) (Maybe (GCTy SrcTy))
            -> RenM (CallArg (Maybe SrcTy) (Maybe (GCTy SrcTy)))
renTyCTyAnn = callArg (liftM CAExp . renTyAnn) (liftM CAComp . renCTyAnn)

renExpComp :: CallArg SrcExp SrcComp -> RenM (CallArg SrcExp SrcComp)
renExpComp = callArg (liftM CAExp . renExp) (liftM CAComp . renComp)

renStructDef :: GStructDef (Maybe SrcTy) -> RenM (GStructDef (Maybe SrcTy))
renStructDef StructDef{..} = do
    struct_flds' <- forM struct_flds $ \(fld, ty) -> do
                      ty' <- renTyAnn ty
                      return (fld, ty')
    return StructDef { struct_name = struct_name
                     , struct_flds = struct_flds'
                     }

{-------------------------------------------------------------------------------
  Renaming computations
-------------------------------------------------------------------------------}

renProg :: SrcProg -> RenM SrcProg
renProg (MkProg globals comp) = do
    globals' <- renTelescope renLocal globals
    comp'    <- extend globals globals' $ renComp comp
    return $ MkProg globals' comp'

renFun :: SrcFun -> RenM SrcFun
renFun fun = case unFun fun of
    MkFunDefined nm params locals body -> do
      nm'     <- renBound renTyAnn nm
      params' <- renTelescope (renBound renTyAnn) params
      locals' <- extend params params' $ renTelescope renLocal locals
      body'   <- extend params params' $ extend locals locals' $ renExp body
      return $ mkFunDefined (funLoc fun) nm' params' locals' body'
    MkFunExternal nm params retTy -> do
      nm'     <- renBound renTyAnn nm
      params' <- renTelescope (renBound renTyAnn) params
      retTy'  <- extend params params' $ renTyAnn retTy
      return $ mkFunExternal (funLoc fun) nm' params' retTy'

renComp :: SrcComp -> RenM SrcComp
renComp c = case unComp c of
      Var nm -> do
        nm' <- renFree renCTyAnn nm
        return $ cVar cloc nm'
      BindMany c1 xs_cs -> do
        c1' <- renComp c1
        xs_cs' <- renTelescope renBind $ xs_cs
        return $ cBindMany cloc c1' xs_cs'
      Seq c1 c2 -> do
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cSeq cloc c1' c2'
      Par parInfo c1 c2 -> do
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cPar cloc parInfo c1' c2'
      Let x c1 c2 -> do
        c1' <- renComp c1
        x'  <- renBound renCTyAnn x
        c2' <- extend x x' $ renComp c2
        return $ cLet cloc x' c1' c2'
      LetStruct sdef c1 -> do
        c1'   <- renComp c1
        sdef' <- renStructDef sdef
        return $ cLetStruct cloc sdef' c1'
      LetE x fi e c1 -> do
        e'  <- renExp e
        x'  <- renBound renTyAnn x
        c1' <- extend x x' $ renComp c1
        return $ cLetE cloc x' fi e' c1'
      LetERef nm1 e c1 -> do
        e'   <- mapM renExp e
        nm1' <- renBound renTyAnn nm1
        extend nm1 nm1' $ do
          c1' <- renComp c1
          return $ cLetERef cloc nm1' e' c1'
      LetHeader fun c2 -> do
        fun' <- renFun fun
        c2'  <- extend fun fun' $ renComp c2
        return $ cLetHeader cloc fun' c2'
      LetFunC nm params locals c1 c2 -> do
        nm'     <- renBound renCTyAnn nm
        params' <- renTelescope (renBound renTyCTyAnn) params
        locals' <- extend params params' $ renTelescope renLocal locals
        c1'     <- extend params params' $ extend locals locals' $ renComp c1
        c2'     <- extend nm nm' $ renComp c2
        return $ cLetFunC cloc nm' params' locals' c1' c2'
      Call nm es -> do
        es' <- mapM renExpComp es
        nm' <- renFree renCTyAnn nm
        return $ cCall cloc nm' es'
      Emit a e -> do
        a' <- renTyAnn a
        e' <- renExp e
        return $ cEmit cloc a' e'
      Return a b fi e -> do
        a' <- renTyAnn a
        b' <- renTyAnn b
        e' <- renExp e
        return $ cReturn cloc a' b' fi e'
      Emits a e -> do
        a' <- renTyAnn a
        e' <- renExp e
        return $ cEmits cloc a' e'
      Interleave c1 c2 -> do
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cInterleave cloc c1' c2'
      Branch e c1 c2 -> do
        e'  <- renExp e
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cBranch cloc e' c1' c2'
      Take1 a b -> do
        a' <- renTyAnn a
        b' <- renTyAnn b
        return $ cTake1 cloc a' b'
      Take a b n -> do
        a' <- renTyAnn a
        b' <- renTyAnn b
        return $ cTake cloc a' b' n
      Until e c' -> do
        e'  <- renExp e
        c'' <- renComp c'
        return $ cUntil cloc e' c''
      While e c' -> do
        e'  <- renExp e
        c'' <- renComp c'
        return $ cWhile cloc e' c''
      Times ui e elen nm c' -> do
        e'    <- renExp e
        elen' <- renExp elen
        nm'   <- renBound renTyAnn nm
        c''   <- extend nm nm' $ renComp c'
        return $ cTimes cloc ui e' elen' nm' c''
      Repeat wdth c' -> do
        c'' <- renComp c'
        return $ cRepeat cloc wdth c''
      VectComp wdth c' -> do
        c'' <- renComp c'
        return $ cVectComp cloc wdth c''
      Map wdth nm -> do
        nm' <- renFree renTyAnn nm
        return $ cMap cloc wdth nm'
      Filter f -> do
        f' <- renFree renTyAnn f
        return $ cFilter cloc f'
      ReadSrc ann -> do
        ann' <- renTyAnn ann
        return $ cReadSrc cloc ann'
      WriteSnk ann -> do
        ann' <- renTyAnn ann
        return $ cWriteSnk cloc ann'
      ReadInternal a s typ -> do
        a' <- renTyAnn a
        return $ cReadInternal cloc a' s typ
      WriteInternal a s -> do
        a' <- renTyAnn a
        return $ cWriteInternal cloc a' s
      Standalone c' -> do
        c'' <- renComp c'
        return $ cStandalone cloc c''
      Mitigate ty n1 n2 -> do
        ty' <- renTyAnn ty
        return $ cMitigate cloc ty' n1 n2
    where
      cloc = compLoc c

{-------------------------------------------------------------------------------
  Renaming expressions
-------------------------------------------------------------------------------}

renExp :: SrcExp -> RenM SrcExp
renExp e = case unExp e of
      EVal t v -> do
        t' <- renTyAnn t
        return $ eVal eloc t' v
      EValArr t vs -> do
        t' <- renTyAnn t
        return $ eValArr eloc t' vs
      EVar nm -> do
        nm' <- renFree renTyAnn nm
        return $ eVar eloc nm'
      EUnOp op e1 -> do
        e1' <- renExp e1
        return $ eUnOp eloc op e1'
      EBinOp op e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eBinOp eloc op e1' e2'
      EAssign e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eAssign eloc e1' e2'
      EArrRead e1 e2 r -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eArrRead eloc e1' e2' r
      EArrWrite e1 e2 r e3 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        e3' <- renExp e3
        return $ eArrWrite eloc e1' e2' r e3'
      EIter nm1 nm2 e1 e2 -> do
        nm1' <- renBound renTyAnn nm1
        nm2' <- renBound renTyAnn nm2
        extend [nm2, nm1] [nm2', nm1'] $ do
          e1' <- renExp e1
          e2' <- renExp e2
          return $ eIter eloc nm1' nm2' e1' e2'
      EFor ui nm1 e1 e2 e3 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        nm1' <- renBound renTyAnn nm1
        extend nm1 nm1' $ do
          e3' <- renExp e3
          return $ eFor eloc ui nm1' e1' e2' e3'
      EWhile e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eWhile eloc e1' e2'
      ELet nm1 fi e1 e2 -> do
        e1' <- renExp e1
        nm1' <- renBound renTyAnn nm1
        extend nm1 nm1' $ do
          e2' <- renExp e2
          return $ eLet eloc nm1' fi e1' e2'
      ELetRef nm1 e1 e2 -> do
        e1'  <- mapM renExp e1
        nm1' <- renBound renTyAnn nm1
        extend nm1 nm1' $ do
          e2' <- renExp e2
          return $ eLetRef eloc nm1' e1' e2'
      ESeq e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eSeq eloc e1' e2'
      ECall f es -> do
        f'  <- renFree renTyAnn f
        es' <- mapM renExp es
        return $ eCall eloc f' es'
      EIf e1 e2 e3 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        e3' <- renExp e3
        return $ eIf eloc e1' e2' e3'
      EPrint nl e1 -> do
        e1' <- renExp e1
        return $ ePrint eloc nl e1'
      EError a str -> do
        a' <- renTyAnn a
        return $ eError eloc a' str
      ELUT r e1 -> do
        r'  <- mapKeysM (renFree renTyAnn) r
        e1' <- renExp e1
        return $ eLUT eloc r' e1'
      EBPerm e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eBPerm eloc e1' e2'
      EStruct tn tfs -> do
        tfs' <- mapM (\(f,e') -> renExp e' >>= \e'' -> return (f,e'')) tfs
        return $ eStruct eloc tn tfs'
      EProj e1 fn -> do
        e1' <- renExp e1
        return $ eProj eloc e1' fn
    where
      eloc = expLoc e
