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
module Rename where

import AstExpr
import AstComp
import PpComp
import qualified GenSym as GS

import qualified Data.Set as S
import Control.Monad.State

import Data.List as M

-- A uniq environment mapping names to unique binding identifiers
type UniqEnv = [(Name,String)]

data RenM a = RenM { runRenM :: GS.Sym
                             -> UniqEnv
                             -> IO a }

instance Monad RenM where
  (>>=) m1 m2 = 
    RenM $ \sym env -> 
      do b <- runRenM m1 sym env
         runRenM (m2 b) sym env
  return x = RenM $ \sym env -> return x

extendUniqEnv :: Name -> String -> RenM a -> RenM a
extendUniqEnv nm uniq (RenM action)
  = RenM (\sym env -> action sym ((nm,uniq):env))

extendUniqEnvMany :: [(Name,String)] -> RenM a -> RenM a
extendUniqEnvMany nms (RenM action)
  = RenM (\sym env -> action sym (nms ++ env)) 

renMIO :: IO a -> RenM a 
renMIO m = RenM (\_ _ -> m)

get_RenMSym :: RenM GS.Sym
get_RenMSym = RenM (\sym _ -> return sym)

newUniq :: RenM String
newUniq = do { sym <- get_RenMSym 
             ; (u, str) <- renMIO $ GS.genSym sym
             ; return ("_r" ++ show u ++ str) 
             }

lkupUniqEnv :: Name -> RenM String
lkupUniqEnv nm = 
   RenM $ \_ env -> 
     case lookup nm env of 
       Just uniq -> return uniq
       Nothing   -> do { putStrLn $ "Unbound identifier: " ++ (name nm)
                       ; putStrLn $ "Location: " ++ (show (nameLoc nm))
                       ; error "Failing to compile." }

failRenM :: String -> RenM a
failRenM msg = renMIO $ putStrLn msg >> error "Failure"


renameExpr :: Exp a -> RenM (Exp a)
renameExpr e 
  = case unExp e of 
    EVal _         -> return e
    EValArr _      -> return e
    EVar nm        -> do { uniq <- lkupUniqEnv nm 
                         ; let nm' = nm { uniqId = uniq } 
                         ; return $ MkExp (EVar nm') (expLoc e) (info e) 
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
      do { u1 <- newUniq 
         ; u2 <- newUniq
         ; extendUniqEnv nm2 u2 $ 
           extendUniqEnv nm1 u1 $
              do e1' <- renameExpr e1                        
                 e2' <- renameExpr e2
                 return $ eIter eloc enfo nm1 { uniqId = u1 } 
                                          nm2 { uniqId = u2 } e1' e2'
         }
    EFor ui nm1 e1 e2 e3   ->
      do e1' <- renameExpr e1                        
         e2' <- renameExpr e2
         u1 <- newUniq
         extendUniqEnv nm1 u1 $ 
           do { e3' <- renameExpr e3                            
              ; return $ eFor eloc enfo ui nm1 { uniqId = u1 } e1' e2' e3'
              }
    EWhile e1 e2 -> 
      do e1' <- renameExpr e1
         e2' <- renameExpr e2
         return $ eWhile eloc enfo e1' e2'

    ELet nm1 e1 e2   -> 
      do e1' <- renameExpr e1
         u1 <- newUniq
         extendUniqEnv nm1 u1 $ 
           do { e2' <- renameExpr e2
              ; return $ eLet eloc enfo nm1 { uniqId = u1 } e1' e2' 
              }

    ELetRef nm1 e1 e2   -> 
      do e1' <- renameMbExpr e1
         u1 <- newUniq
         extendUniqEnv nm1 u1 $ 
           do { e2' <- renameExpr e2
              ; return $ eLetRef eloc enfo nm1 { uniqId = u1 } e1' e2' }

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
    EError _       -> return e
    ELUT r e1 -> 
      do e1' <- renameExpr e1
         return $ eLUT eloc enfo r e1'
    EBPerm e1 e2 -> do
      e1' <- renameExpr e1
      e2' <- renameExpr e2
      return $ eBPerm eloc enfo e1' e2'

    EStruct tn tfs -> 
      do { tfs' <- mapM (\(f,e) -> renameExpr e >>= \e' -> return (f,e')) tfs
         ; return $ eStruct eloc enfo tn tfs' }

    EProj e fn -> 
      do { e' <- renameExpr e
         ; return $ eProj eloc enfo e' fn }
  where
    eloc = expLoc e
    enfo = info e
    
renameMbExpr :: Either Ty (Exp e) -> RenM (Either Ty (Exp e))
renameMbExpr (Left x)  = return (Left x)
renameMbExpr (Right e) = renameExpr e >>= (return . Right)


renameBinds :: [(Name,Comp a b)] -> RenM [(Name,Comp a b)] 
renameBinds [] = return []
renameBinds ((nm,c):bnds) = 
  do { u <- newUniq 
     ; (c',bnds') <- extendUniqEnv nm u $
                     do { c' <- renameComp c 
                        ; bnds' <- renameBinds bnds
                        ; return (c',bnds') }
    ; return $ (nm { uniqId = u },c'):bnds' }

renameFun :: Fun a -> RenM (Name, Fun a)
renameFun fe 
  | MkFunDefined nm params locals body <- unFun fe
  = do { u <- newUniq -- For the name of the function: 
                      -- assuming non-recursive for now
       ; usp <- mapM (\_ -> newUniq) params
       ; usl <- mapM (\_ -> newUniq) locals
       ; let env1' = zipWith (\(nm,_) u -> (nm,u)) params usp
       ; let env2' = zipWith (\(nm,_,_) u -> (nm,u)) locals usl
       ; let params' 
              = zipWith (\(nm,a) u -> (nm { uniqId = u }, a)) params usp

       ; locals'  <- extendUniqEnvMany env1' $ renameLocals locals usl

       ; body' <- extendUniqEnvMany (env2' ++ env1') $ renameExpr body
       ; let nm' = nm { uniqId = u } 
       ; return $ (nm', MkFun (MkFunDefined nm' params' locals' body')
                              (funLoc fe)
                              (funInfo fe))
      }

  | MkFunExternal nm _ _ <- unFun fe -- External
  = return (nm, fe)
  | otherwise
  = error "renameFun: can't happen!" 


renameLocals :: [(Name,Ty,Maybe (Exp a))] 
             -> [String] 
             -> RenM [(Name,Ty,Maybe (Exp a))]
-- Accepts: locals to rename, and an already generated list of uniqs to use
renameLocals [] [] = return []
renameLocals ((nm,a,Nothing):lcls) (u1:u1s) 
  = do { rest <- extendUniqEnv nm u1 $ renameLocals lcls u1s 
       ; return $ ((nm{uniqId=u1},a,Nothing):rest) }
renameLocals ((nm,a,Just e):lcls) (u1:u1s) 
  = do { rest <- extendUniqEnv nm u1 $ renameLocals lcls u1s
       ; e' <- renameExpr e
       ; return $ ((nm{uniqId=u1},a,Just e'):rest) }
renameLocals _ _ = error "Can't happen" 


renameComp :: Comp a b -> RenM (Comp a b)
renameComp c = 
  case unComp c of
    Var nm -> 
      do { uniq <- lkupUniqEnv nm 
         ; let nm' = nm { uniqId = uniq } 
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
         ; u <- newUniq
         ; c2' <- extendUniqEnv x u $ renameComp c2 
         ; return $ cLet cloc cnfo x { uniqId = u } c1' c2'
         }

    LetStruct sdef c1 ->
      do { c1' <- renameComp c1
         ; return $ cLetStruct cloc cnfo sdef c1'
         }

    LetE x e c' ->
      do { e'  <- renameExpr e
         ; u <- newUniq
         ; c'' <- extendUniqEnv x u $ renameComp c'
         ; return $ cLetE cloc cnfo x { uniqId = u } e' c''
         }
    LetFun nm e c2 -> -- Not renaming functions for now
      do { (nm',e')  <- renameFun e
         ; c2' <- extendUniqEnv nm (uniqId nm') $ renameComp c2
         ; return $ cLetFun cloc cnfo nm' e' c2'
         }
    LetExternal nm e c2 ->
      do { (_,e')  <- renameFun e -- No renaming happens for externals for now
         ; c2' <- extendUniqEnv nm (uniqId nm) $ renameComp c2 -- Not renaming externals
         ; return $ cLetExternal cloc cnfo nm e' c2'
         }
    LetFunC nm params locals c1 c2 ->
      do { usp <- mapM (\_ -> newUniq) params
         ; usl <- mapM (\_ -> newUniq) locals

         ; let env1' = zipWith (\(nm,_) u -> (nm,u)) params usp
         ; let env2' = zipWith (\(nm,_,_) u -> (nm,u)) locals usl
         ; let params' 
                 = zipWith (\(nm,a) u -> (nm{uniqId=u}, a)) params usp 
      
         ; locals' <- extendUniqEnvMany env1' $ renameLocals locals usl 

         ; c1' <- extendUniqEnvMany (env2' ++ env1') $ renameComp c1

         ; u <- newUniq 
         ; c2' <- extendUniqEnv nm u $ renameComp c2 
         ; return $ 
           cLetFunC cloc cnfo nm {uniqId = u } params' locals' c1' c2'
         }

    Call nm es ->
      do { es' <- mapM renameCallArg es
         ; u <- lkupUniqEnv nm 
         ; return $ cCall cloc cnfo nm { uniqId = u } es'
         }
    Emit e ->
      do { e' <- renameExpr e
         ; return $ cEmit cloc cnfo e'
         }
    Return e -> 
      do { e' <- renameExpr e
         ; return $ cReturn cloc cnfo e'
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
    Take1 -> return c
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
         ; u <- newUniq
         ; c'' <- extendUniqEnv nm u $ renameComp c' 
         ; return $ cTimes cloc cnfo ui e' elen' nm { uniqId = u } c''
         }
    Repeat wdth c' ->
      do { c'' <- renameComp c'
         ; return $ cRepeat cloc cnfo wdth c''
         }
    VectComp wdth c' -> 
      do { c'' <- renameComp c'
         ; return $ cVectComp cloc cnfo wdth c''
         }
    Map wdth e ->
      do { e' <- renameExpr e
         ; return $ cMap cloc cnfo wdth e'
         }
    Filter e -> 
      do { e' <- renameExpr e
         ; return $ cFilter cloc cnfo e'
         }

    ReadSrc {}  -> return c
    WriteSnk {} -> return c

    ReadInternal _s _typ -> return c
    WriteInternal _s     -> return c

    Standalone c' -> 
      do { c'' <- renameComp c'
         ; return $ cStandalone cloc cnfo c''
         }

  where cloc = compLoc c
        cnfo = compInfo c

renameCallArg (CAExp e)  = do { e' <- renameExpr e ; return (CAExp e')  }
renameCallArg (CAComp c) = do { c' <- renameComp c ; return (CAComp c') }


renameProg :: Prog a b -> RenM (Prog a b)
renameProg (MkProg globals comp)
  = let init_env = map (\(nm,_,_) -> (nm, uniqId nm)) globals
    in do { comp' <- extendUniqEnvMany init_env $ 
                     renameComp comp
          ; return (MkProg globals comp') 
          }
