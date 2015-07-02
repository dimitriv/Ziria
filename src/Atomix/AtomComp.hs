module AtomComp  where 

import AstExpr 
import AstComp


import Outputable
import Text.PrettyPrint.HughesPJ

import qualified GenSym as GS
import Data.Loc
import qualified Data.List as List
import Control.Monad.State

data AExp b
  = MkAExp { aexp_lbl :: String  -- ^ unique label
           , aexp_exp :: Exp     -- ^ the expression
           , aexp_ivs :: [EId]   -- ^ all variables
           , aexp_ovs :: [EId]   -- ^ output variables
           , aexp_ret :: Ty      -- ^ return type
           , aexp_nfo :: b }     -- ^ other stuff

instance Outputable (AExp b) where
  ppr ae = vcat [ ppr (aexp_lbl ae) <> colon
                , nest 2 $ ppr (aexp_exp ae)
                , text "invars :" <+> ppr (aexp_ivs ae)
                , text "outvars:" <+> ppr (aexp_ovs ae) 
                ]

instance Show (AExp b) where
  show e = render (ppr e)

data AComp a b
  = MkAComp { acomp_loc  :: !(CompLoc)
            , acomp_nfo  :: a 
            , acomp_comp :: !(AComp0 a b) }


data AComp0 a b
  = ATake1 Ty
  | ATakeN Ty Int
  | AEmit1 EId
  | AEmitN EId
  | AMitigate String Ty Int Int

  | AReturn (AExp b)


  | ABind (Maybe EId) (AComp a b) (AComp a b) 

  | APar ParInfo (AComp a b) (AComp a b)

  | ABranch EId (AComp a b) (AComp a b)

  | ARepeatN Int (AComp a b)
  | ARepeat (AComp a b)

  | AWhile EId (AComp a b)
  | AUntil EId (AComp a b)

  | AReadSrc Ty
  | AWriteSnk Ty 


aTake1 :: SrcLoc -> a -> Ty -> AComp a b 
aTake1 loc a x = MkAComp loc a (ATake1 x) 

aTakeN :: SrcLoc -> a -> Ty -> Int -> AComp a b
aTakeN loc a t n = MkAComp loc a (ATakeN t n)

aEmit1 :: SrcLoc -> a -> EId -> AComp a b
aEmit1 loc a x = MkAComp loc a (AEmit1 x)

aEmitN :: SrcLoc -> a -> EId -> AComp a b
aEmitN loc a x = MkAComp loc a (AEmitN x)

aMitigate :: SrcLoc -> a -> String -> Ty -> Int -> Int -> AComp a b
aMitigate loc a s t n1 n2 = MkAComp loc a (AMitigate s t n1 n2)

aReturn   :: SrcLoc -> a -> AExp b -> AComp a b
aReturn loc a e = MkAComp loc a (AReturn e)

aBind :: SrcLoc -> a -> Maybe EId -> AComp a b -> AComp a b -> AComp a b
aBind loc a mx c1 c2 = MkAComp loc a (ABind mx c1 c2)

aPar :: SrcLoc -> a -> ParInfo -> AComp a b -> AComp a b -> AComp a b
aPar loc a p c1 c2 = MkAComp loc a (APar p c1 c2)

aBranch :: SrcLoc -> a -> EId -> AComp a b -> AComp a b -> AComp a b
aBranch loc a x c1 c2 = MkAComp loc a (ABranch x c1 c2)

aRepeatN :: SrcLoc -> a -> Int -> AComp a b -> AComp a b
aRepeatN loc a n c = MkAComp loc a (ARepeatN n c)

aRepeat :: SrcLoc -> a -> AComp a b -> AComp a b
aRepeat loc a c = MkAComp loc a (ARepeat c)

aWhile :: SrcLoc -> a -> EId -> AComp a b -> AComp a b
aWhile loc a x c = MkAComp loc a (AWhile x c)

aUntil :: SrcLoc -> a -> EId -> AComp a b -> AComp a b
aUntil loc a x c = MkAComp loc a (AUntil x c)

aReadSrc :: SrcLoc -> a -> Ty -> AComp a b
aReadSrc loc a t = MkAComp loc a (AReadSrc t)

aWriteSnk :: SrcLoc -> a -> Ty  -> AComp a b
aWriteSnk loc a t = MkAComp loc a (AWriteSnk t)


ppAComp  :: (Outputable a, Outputable b) => AComp a b -> Doc
ppAComp ac = ppAComp0 (acomp_comp ac)

ppAComp0 :: (Outputable a, Outputable b) => AComp0 a b -> Doc
ppAComp0 (ATake1 t)   = text "take" <> brackets (ppr t)
ppAComp0 (ATakeN t n) = text "takes" <+> int n
ppAComp0 (AEmit1 x)   = text "emit" <+> ppr x
ppAComp0 (AEmitN x)   = text "emits" <+> ppr x
ppAComp0 (AMitigate s t n1 n2) 
  = int n1 <> text "-mitigate" <> parens (text s) <>
    brackets (ppr t) <> text "-" <> int n2
ppAComp0 (ABind mx c1 c2) = vcat [ ppr mx <+> text "<-" <+> ppr c1 
                                 , ppr c2 ]
ppAComp0 (AReturn e) = text "ret" <+> braces (ppr e) 
ppAComp0 (APar p c1 c2) = ppr c1 <+> text ">>>" <+> ppr c2
ppAComp0 (ABranch x c1 c2) 
  = text "if" <+> ppr x $$
         text "then" <+> ppr c1 $$
         text "else" <+> ppr c2
ppAComp0 (ARepeatN n c) 
  = text "repeat" <> brackets (text "n =" <> (int n)) <> braces (ppr c)
ppAComp0 (ARepeat c)    = text "repeat" <> braces (ppr c)
ppAComp0 (AWhile x c)   = text "while" <> parens (ppr x) <> braces (ppr c)
ppAComp0 (AUntil x c)   = text "do-unitl" <> parens (ppr x) <> braces (ppr c)
ppAComp0 (AReadSrc t)   = text "read"
ppAComp0 (AWriteSnk t)  = text "write"


instance (Outputable a, Outputable b) => Outputable (AComp a b) where
  ppr = ppAComp

instance (Outputable a, Outputable b) => Show (AComp a b) where
  show = render . ppr 

{-
data SymAtom = SAExp (Exp ())
             | SAId Ty
             | SADiscard Ty
-}

-- data SymFun = SFFun FunName
--             | SFId Ty
--             | SFDiscard Ty 


-- -- Notes. Here we may want to add:
-- --   SFCompose (SymFun a) (SymFun a) ...
-- --   SFFunFun  ... some functional representation of actual functions ...

-- data CompEnv a = CompEnv { fun_binds  :: [(FunName, Fun a)]
--                          , funGenSym :: GS.Sym
--                          , varGenSym :: GS.Sym
--                          }

-- type CompM a = StateT (CompEnv a) IO

-- freshVar :: String -> Ty -> MutKind -> CompM a Var
-- freshVar pretty_name t mk = do
--   env <- get
--   uniq <- liftIO $ GS.genSymStr (varGenSym env)
--   return (MkName pretty_name (MkUniq $ pretty_name ++ "$" ++ uniq) t noLoc mk)

-- freshFun :: String -> [ArgTy] -> Ty -> CompM a Var
-- freshFun pretty_name arg_tys out_ty = do
--   env <- get
--   uniq <- liftIO $ GS.genSymStr (funGenSym env)
--   let t = TArrow arg_tys out_ty
--   return (MkName pretty_name (MkUniq $ pretty_name ++ "$" ++ uniq) t noLoc undefined)


-- mkCompOfAst :: AstC.GComp tc t a b -> CompM a (Comp a b)
-- mkCompOfAst c = fail "not implemented"

-- runCompM :: CompM a b -> IO (b, CompEnv a)
-- runCompM m = do
--   fungen <- GS.initGenSym "f"
--   vargen <- GS.initGenSym "x"
--   let env = CompEnv [] fungen vargen
--   runStateT m env
