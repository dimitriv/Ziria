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
module Rename (RenM, runRenM, rename, extend) where

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

renameBound :: Rename ty => GName ty -> RenM (GName ty)
renameBound MkName{..} = do
  uniqId'  <- newUniq
  nameTyp' <- rename nameTyp
  return MkName{uniqId = uniqId', nameTyp = nameTyp', ..}

renameFree :: Rename ty => GName ty -> RenM (GName ty)
renameFree nm@MkName{..} = do
  uniqId'  <- lookupUniq nm
  nameTyp' <- rename nameTyp
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
renameTelescope :: Extend a => (a -> RenM a) -> [a] -> RenM [a]
renameTelescope f = go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- f x
      xs' <- extend x x' $ go xs
      return (x':xs')

-- | Local variables
renameLocal :: (GName (Maybe SrcTy), Maybe SrcExp)
            -> RenM (GName (Maybe SrcTy), Maybe SrcExp)
renameLocal (x, me) = do
    x'  <- renameBound x
    me' <- mapM rename me -- No recursion!
    return (x', me')

-- | Monadic bind
renameBind :: (GName (Maybe SrcTy), SrcComp)
           -> RenM (GName (Maybe SrcTy), SrcComp)
renameBind (x, c) = do
    x' <- renameBound x
    c' <- extend x x' $ rename c
    return (x', c')

{-------------------------------------------------------------------------------
  Renaming proper
-------------------------------------------------------------------------------}

-- | Renaming does not change the type, just assigns unique IDs to variables.
class Rename a where
  rename :: a -> RenM a

instance Rename a => Rename (Maybe a) where
  rename = mapM rename

instance (Rename a, Rename b) => Rename (CallArg a b) where
  rename (CAExp  e) = CAExp  <$> rename e
  rename (CAComp c) = CAComp <$> rename c

{-------------------------------------------------------------------------------
  Renaming types

  We have to rename types because types can refer to term variables (length(x)).
-------------------------------------------------------------------------------}

instance Rename SrcTy where
  rename SrcTUnit               = return SrcTUnit
  rename SrcTBit                = return SrcTBit
  rename SrcTBool               = return SrcTBool
  rename (SrcTArray numExpr ty) = SrcTArray <$> rename numExpr <*> rename ty
  rename (SrcTInt bw)           = SrcTInt   <$> rename bw
  rename SrcTDouble             = return SrcTDouble
  rename (SrcTStruct nm)        = return $ SrcTStruct nm

  -- Internal types injected into the source language are used for quasi-quoting
  -- only, and therefore do not have to be renamed.
  rename (SrcInject ty)         = return $ SrcInject ty

instance Rename SrcNumExpr where
  rename (SrcLiteral n) = return $ SrcLiteral n
  rename (SrcNArr nm)   = SrcNArr <$> renameFree nm
  rename (SrcNVar loc)  = return $ SrcNVar loc

instance Rename SrcBitWidth where
  rename = return -- (source) bitwidths don't contain variable names

instance Rename ty => Rename (GCTy ty) where
  rename (CTVar _)          = panicStr "Unexpected type variable in source type"
  rename (CTComp u a b)     = CTComp  <$> rename u <*> rename a <*> rename b
  rename (CTTrans a b)      = CTTrans <$> rename a <*> rename b
  rename (CTArrow args res) = CTArrow <$> mapM rename args <*> rename res

instance Rename ty => Rename (GStructDef ty) where
  rename StructDef{..} = do
    struct_flds' <- forM struct_flds $ \(fld, ty) -> do
                      ty' <- rename ty
                      return (fld, ty')
    return StructDef { struct_name = struct_name
                     , struct_flds = struct_flds'
                     }

{-------------------------------------------------------------------------------
  Renaming computations
-------------------------------------------------------------------------------}

instance Rename SrcProg where
  rename (MkProg globals comp) = do
    globals' <- renameTelescope renameLocal globals
    comp'    <- extend globals globals' $ rename comp
    return $ MkProg globals' comp'

instance Rename SrcFun where
  rename fun = case unFun fun of
    MkFunDefined nm params locals body -> do
      nm'     <- renameBound nm
      params' <- renameTelescope renameBound params
      locals' <- extend params params' $ renameTelescope renameLocal locals
      body'   <- extend params params' $ extend locals locals' $ rename body
      return $ mkFunDefined (funLoc fun) nm' params' locals' body'

    MkFunExternal nm params retTy -> do
      nm'     <- renameBound nm
      params' <- renameTelescope renameBound params
      retTy'  <- extend params params' $ rename retTy
      return $ mkFunExternal (funLoc fun) nm' params' retTy'

instance Rename SrcComp where
  rename c = case unComp c of
      Var nm -> do
        nm' <- renameFree nm
        return $ cVar cloc nm'
      BindMany c1 xs_cs -> do
        c1' <- rename c1
        xs_cs' <- renameTelescope renameBind $ xs_cs
        return $ cBindMany cloc c1' xs_cs'
      Seq c1 c2 -> do
        c1' <- rename c1
        c2' <- rename c2
        return $ cSeq cloc c1' c2'
      Par parInfo c1 c2 -> do
        c1' <- rename c1
        c2' <- rename c2
        return $ cPar cloc parInfo c1' c2'
      Let x c1 c2 -> do
        c1' <- rename c1
        x'  <- renameBound x
        c2' <- extend x x' $ rename c2
        return $ cLet cloc x' c1' c2'
      LetStruct sdef c1 -> do
        c1'   <- rename c1
        sdef' <- rename sdef
        return $ cLetStruct cloc sdef' c1'
      LetE x fi e c1 -> do
        e'  <- rename e
        x' <- renameBound x
        c1' <- extend x x' $ rename c1
        return $ cLetE cloc x' fi e' c1'
      LetERef nm1 e c1 -> do
        e'   <- mapM rename e
        nm1' <- renameBound nm1
        extend nm1 nm1' $ do
          c1' <- rename c1
          return $ cLetERef cloc nm1' e' c1'
      LetHeader fun c2 -> do
        fun' <- rename fun
        c2'  <- extend fun fun' $ rename c2
        return $ cLetHeader cloc fun' c2'
      LetFunC nm params locals c1 c2 -> do
        nm'     <- renameBound nm
        params' <- renameTelescope renameBound params
        locals' <- extend params params' $ renameTelescope renameLocal locals
        c1'     <- extend params params' $ extend locals locals' $ rename c1
        c2'     <- extend nm nm' $ rename c2
        return $ cLetFunC cloc nm' params' locals' c1' c2'
      Call nm es -> do
        es' <- mapM rename es
        nm' <- renameFree nm
        return $ cCall cloc nm' es'
      Emit a e -> do
        a' <- rename a
        e' <- rename e
        return $ cEmit cloc a' e'
      Return a b fi e -> do
        a' <- rename a
        b' <- rename b
        e' <- rename e
        return $ cReturn cloc a' b' fi e'
      Emits a e -> do
        a' <- rename a
        e' <- rename e
        return $ cEmits cloc a' e'
      Interleave c1 c2 -> do
        c1' <- rename c1
        c2' <- rename c2
        return $ cInterleave cloc c1' c2'
      Branch e c1 c2 -> do
        e'  <- rename e
        c1' <- rename c1
        c2' <- rename c2
        return $ cBranch cloc e' c1' c2'
      Take1 a b -> do
        a' <- rename a
        b' <- rename b
        return $ cTake1 cloc a' b'
      Take a b n -> do
        a' <- rename a
        b' <- rename b
        return $ cTake cloc a' b' n
      Until e c' -> do
        e'  <- rename e
        c'' <- rename c'
        return $ cUntil cloc e' c''
      While e c' -> do
        e'  <- rename e
        c'' <- rename c'
        return $ cWhile cloc e' c''
      Times ui e elen nm c' -> do
        e'    <- rename e
        elen' <- rename elen
        nm'   <- renameBound nm
        c''   <- extend nm nm' $ rename c'
        return $ cTimes cloc ui e' elen' nm' c''
      Repeat wdth c' -> do
        c'' <- rename c'
        return $ cRepeat cloc wdth c''
      VectComp wdth c' -> do
        c'' <- rename c'
        return $ cVectComp cloc wdth c''
      Map wdth nm -> do
        nm' <- renameFree nm
        return $ cMap cloc wdth nm'
      Filter f -> do
        f' <- renameFree f
        return $ cFilter cloc f'
      ReadSrc ann -> do
        ann' <- rename ann
        return $ cReadSrc cloc ann'
      WriteSnk ann -> do
        ann' <- rename ann
        return $ cWriteSnk cloc ann'
      ReadInternal a s typ -> do
        a' <- rename a
        return $ cReadInternal cloc a' s typ
      WriteInternal a s -> do
        a' <- rename a
        return $ cWriteInternal cloc a' s
      Standalone c' -> do
        c'' <- rename c'
        return $ cStandalone cloc c''
      Mitigate ty n1 n2 -> do
        ty' <- rename ty
        return $ cMitigate cloc ty' n1 n2
    where
      cloc = compLoc c

{-------------------------------------------------------------------------------
  Renaming expressions
-------------------------------------------------------------------------------}

instance Rename SrcExp where
  rename e = case unExp e of
      EVal t v -> do
        t' <- rename t
        return $ eVal eloc t' v
      EValArr t vs -> do
        t' <- rename t
        return $ eValArr eloc t' vs
      EVar nm -> do
        nm' <- renameFree nm
        return $ eVar eloc nm'
      EUnOp op e1 -> do
        e1' <- rename e1
        return $ eUnOp eloc op e1'
      EBinOp op e1 e2 -> do
        e1' <- rename e1
        e2' <- rename e2
        return $ eBinOp eloc op e1' e2'
      EAssign e1 e2 -> do
        e1' <- rename e1
        e2' <- rename e2
        return $ eAssign eloc e1' e2'
      EArrRead e1 e2 r -> do
        e1' <- rename e1
        e2' <- rename e2
        return $ eArrRead eloc e1' e2' r
      EArrWrite e1 e2 r e3 -> do
        e1' <- rename e1
        e2' <- rename e2
        e3' <- rename e3
        return $ eArrWrite eloc e1' e2' r e3'
      EIter nm1 nm2 e1 e2 -> do
        nm1' <- renameBound nm1
        nm2' <- renameBound nm2
        extend [nm2, nm1] [nm2', nm1'] $ do
          e1' <- rename e1
          e2' <- rename e2
          return $ eIter eloc nm1' nm2' e1' e2'
      EFor ui nm1 e1 e2 e3 -> do
        e1' <- rename e1
        e2' <- rename e2
        nm1' <- renameBound nm1
        extend nm1 nm1' $ do
          e3' <- rename e3
          return $ eFor eloc ui nm1' e1' e2' e3'
      EWhile e1 e2 -> do
        e1' <- rename e1
        e2' <- rename e2
        return $ eWhile eloc e1' e2'
      ELet nm1 fi e1 e2 -> do
        e1' <- rename e1
        nm1' <- renameBound nm1
        extend nm1 nm1' $ do
          e2' <- rename e2
          return $ eLet eloc nm1' fi e1' e2'
      ELetRef nm1 e1 e2 -> do
        e1'  <- mapM rename e1
        nm1' <- renameBound nm1
        extend nm1 nm1' $ do
          e2' <- rename e2
          return $ eLetRef eloc nm1' e1' e2'
      ESeq e1 e2 -> do
        e1' <- rename e1
        e2' <- rename e2
        return $ eSeq eloc e1' e2'
      ECall f es -> do
        f'  <- renameFree f
        es' <- mapM rename es
        return $ eCall eloc f' es'
      EIf e1 e2 e3 -> do
        e1' <- rename e1
        e2' <- rename e2
        e3' <- rename e3
        return $ eIf eloc e1' e2' e3'
      EPrint nl e1 -> do
        e1' <- rename e1
        return $ ePrint eloc nl e1'
      EError a str -> do
        a' <- rename a
        return $ eError eloc a' str
      ELUT r e1 -> do
        r'  <- mapKeysM renameFree r
        e1' <- rename e1
        return $ eLUT eloc r' e1'
      EBPerm e1 e2 -> do
        e1' <- rename e1
        e2' <- rename e2
        return $ eBPerm eloc e1' e2'
      EStruct tn tfs -> do
        tfs' <- mapM (\(f,e') -> rename e' >>= \e'' -> return (f,e'')) tfs
        return $ eStruct eloc tn tfs'
      EProj e1 fn -> do
        e1' <- rename e1
        return $ eProj eloc e1' fn
    where
      eloc = expLoc e
