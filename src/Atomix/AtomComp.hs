module AtomComp  where 

import AstExpr 
import AstComp


import Outputable
import Text.PrettyPrint.HughesPJ

import qualified GenSym as GS
import Data.Loc
import Control.Monad.State

import AtomixCompTransform

data AExp b
  = MkAExp { aexp_lbl :: String  -- ^ unique label
           , aexp_exp :: Exp     -- ^ the expression
           , aexp_ivs :: [EId]   -- ^ all variables
           , aexp_ovs :: [EId]   -- ^ output variables
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
  = MkAComp { acomp_comp :: !(AComp0 a b)
            , acomp_loc  :: !(CompLoc)
            , acomp_nfo  :: a }


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


ppAComp  :: (Outputable a, Outputable b) => AComp a b -> Doc
ppAComp ac = ppAComp0 (acomp_comp ac)

ppAComp0 :: (Outputable a, Outputable b) => AComp0 a b -> Doc
ppAComp0 (ATake1 t)   = text "take" <> brackets (ppr t)
ppAComp0 (ATakeN t n) = text "takes" <+> int n
ppAComp0 (AEmit1 x)   = text "emit" <+> ppr x
ppAComp0 (AEmitN x)   = text "emits" <+> ppr x
ppAComp0 (AMitigate s t n1 n2) = int n1 <> text "-mitigate" <> parens (text s) <>
                                 brackets (ppr t) <> text "-" <> int n2

ppAComp0 (ABind mx c1 c2) = vcat [ ppr mx <+> text "<-" <+> ppr c1 
                                 , ppr c2 ]
ppAComp0 (AReturn e) = text "ret" <+> braces (ppr e) 
ppAComp0 (APar p c1 c2) = ppr c1 <+> text ">>>" <+> ppr c2
ppAComp0 (ABranch x c1 c2) 
  = text "if" <+> ppr x $$
         text "then" <+> ppr c1 $$
         text "else" <+> ppr c2
ppAComp0 (ARepeatN n c) = text "repeat" <> brackets (text "n =" <> (int n)) <> braces (ppr c)
ppAComp0 (ARepeat c)    = text "repeat" <> braces (ppr c)
ppAComp0 (AWhile x c)   = text "while" <> parens (ppr x) <> braces (ppr c)
ppAComp0 (AUntil x c)   = text "do-unitl" <> parens (ppr x) <> braces (ppr c)

instance (Outputable a, Outputable b) => Outputable (AComp a b) where
  ppr = ppAComp

instance (Outputable a, Outputable b) => Show (AComp a b) where
  show = render . ppr 



-- Translate Ziria programs to atomix ziria programs

zirToAtomZir :: GS.Sym -> Comp -> IO (AComp (), RnSt)
zirToAtomZir sym comp = do
  -- Closure convert and lift
  (comp0,rnst) <- atomixCompTransform sym comp 
  -- Transform lifted
  let acomp = transLifted comp0
  return (acomp,rnst)


transLifted :: Comp -> AComp
transLifted comp = go (unComp comp)
  where 
    loc = compLoc comp
    go (BindMany c1 []) = transLifted c1

    go (BindMany c1 ((x:c2):xscs)) 
       = aBind loc (Just x) (transLifted c1) (go (BindMAny c2 xscs))

    go (Seq c1 c2)
       = aBind loc Nothing (transLifted c1) (transLifted c2)

    go (Par p c1 c2)
       = aPar p (transLifted c1) (transLifted c2)

    go (Emit (MkExp (EVar x) _ _)) = aEmit1 loc x

    go (Emits (MkExp (EVar x) _ _)) = aEmitN loc x

    go (Take1 t)  = aTake1 loc t n
    go (Take t n) = aTakeN loc t n

    go (Until (MkExp (EVar x) _ _) c) = aUntil loc x c
    go (While (MkExp (EVar x) _ _) c) = aWhile loc x c
    go (Times _ estrt (MkExp (EVal _ (VInt i _)) _ _) cnt c) =

       cnt := estart
       repeatM i (c; cnt := cnt + 1)

       ....
       ....
    go (Repeat _ c) = aRepeat (transLifted c)
    go (Mitigate s t n1 n2) = aMitigate loc s t n1 n2
    go (Standalone c) = transLifted c

    go (Branch (MkExp (EVar x)) c1 c2) = aBranch loc x (transLifted c1) (transLifted c2)

    go (ReadSrc t)  = aReadSrc loc t
    go (WriteSnk t) = aWriteSnk loc t









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

-- freshEId :: String -> Ty -> MutKind -> CompM a EId
-- freshEId pretty_name t mk = do
--   env <- get
--   uniq <- liftIO $ GS.genSymStr (varGenSym env)
--   return (MkName pretty_name (MkUniq $ pretty_name ++ "$" ++ uniq) t noLoc mk)

-- freshFun :: String -> [ArgTy] -> Ty -> CompM a EId
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
  
