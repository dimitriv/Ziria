module AtomComp  where 

import AstExpr 
import AstUnlabelled ( eVar )
import AstComp

import Outputable
import Text.PrettyPrint.HughesPJ
import Data.Loc

data AExp b
  = MkAExp { aexp_lbl :: String  -- ^ unique label
           , aexp_exp :: Exp     -- ^ the expression
           , aexp_ivs :: [EId]   -- ^ all variables
           , aexp_ovs :: [EId]   -- ^ output variables
           , aexp_ret :: Ty      -- ^ return type
           , aexp_nfo :: b }     -- ^ other stuff
  deriving Eq

substAExp :: EId -> EId -> AExp b -> AExp b
substAExp x t (MkAExp lbl e ivs ovs ret nfo) = MkAExp lbl e' ivs' ovs' ret nfo
  where e'   = substExp [] [(x, eVar (nameLoc t) t)] e
        ivs' = map (\v -> if v == x then t else v) ivs
        ovs' = map (\v -> if v == x then t else v) ovs

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
  = ATake1 String Ty               -- String is a uniq label
  | ATakeN String Ty Int           -- String is a uniq label
  | AEmitN String Ty Int EId       -- AEmitN t n e invariant:  e : arr[n] 
  | ACast String (Int,Ty) (Int,Ty) -- ACast (n1,t1) (n2,t2)
                                   -- takes n1*t1 and emits n2*t2 
                                   --   where n1*sizeof(t1) = n2*sizeof(t2))

  | AReturn (AExp b)       -- Maybe EId -> AExp b
  | AEmit1 (AExp b)        -- EId -> AExp b

  | ATakeEmit   EId (AExp b)
  | ATakeReturn EId (AExp b)

  | ABind (Maybe EId) (AComp a b) (AComp a b)
  | APar ParInfo (AComp a b) Ty (AComp a b)
  | ABranch EId (AComp a b) (AComp a b)
  | ARepeatN Int (AComp a b)
  | ARepeat (AComp a b)

  | AWhile EId (AComp a b)
  | AUntil EId (AComp a b)



aTake1 :: SrcLoc -> a -> String -> Ty -> AComp a b 
aTake1 loc a s x = MkAComp loc a (ATake1 s x) 

aTakeN :: SrcLoc -> a -> String -> Ty -> Int -> AComp a b
aTakeN loc a s t n = MkAComp loc a (ATakeN s t n)

aEmit1 :: SrcLoc -> a -> AExp b -> AComp a b
aEmit1 loc a e = MkAComp loc a (AEmit1 e)

aEmitN :: SrcLoc -> a -> String -> Ty -> Int -> EId -> AComp a b
aEmitN loc a s t n x = MkAComp loc a (AEmitN s t n x)

aCast :: SrcLoc -> a -> String -> (Int,Ty) -> (Int,Ty) -> AComp a b
aCast loc a s (n1,t1) (n2,t2) = MkAComp loc a (ACast s (n1,t1) (n2,t2))

aReturn   :: SrcLoc -> a -> AExp b -> AComp a b
aReturn loc a e = MkAComp loc a (AReturn e)

aBind :: SrcLoc -> a -> Maybe EId -> AComp a b -> AComp a b -> AComp a b
aBind loc a mx c1 c2 
  | ATake1 {}   <- acomp_comp c1
  , Just x      <- mx
  , AEmit1 aexp <- acomp_comp c2
  = MkAComp loc a (ATakeEmit x aexp)
  | ATake1 {}    <- acomp_comp c1
  , Just x       <- mx
  , AReturn aexp <- acomp_comp c2
  = MkAComp loc a (ATakeReturn x aexp)
  | otherwise
  = MkAComp loc a (ABind mx c1 c2)

aPar :: SrcLoc -> a -> ParInfo -> AComp a b -> Ty -> AComp a b -> AComp a b
aPar loc a p c1 t c2 = MkAComp loc a (APar p c1 t c2)

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



ppAComp  :: AComp a b -> Doc
ppAComp ac = ppAComp0 (acomp_comp ac)

ppAComp0 :: AComp0 a b -> Doc
ppAComp0 (ATake1 _s t)      = text "take" <> brackets (ppr t)
ppAComp0 (ATakeN _s _t n)   = text "takes" <+> int n
ppAComp0 (AEmit1 e)         = text "emit" <+> braces (ppr e)
ppAComp0 (AEmitN _ _t _n x) = text "emits" <+> ppr x
ppAComp0 (ACast s (n1,t1) (n2,t2))
  = brackets (int n1 <> (text "*") <> parens (ppr t1)) <> 
    (text "-cast") <> parens (text s) <> (text "-") <> 
    brackets (int n2 <> (text "*") <> parens (ppr t2))

ppAComp0 (ATakeEmit x e)   = text "take-emit" <+> ppr x <+> braces (ppr e)
ppAComp0 (ATakeReturn x e) = text "take-retn" <+> ppr x <+> braces (ppr e)

ppAComp0 (ABind mx c1 c2) = vcat [ ppr mx <+> text "<-" <+> ppr c1 
                                 , ppr c2 ]
ppAComp0 (AReturn e) = text "ret" <+> braces (ppr e) 
ppAComp0 (APar _p c1 _ c2) = ppr c1 <+> text ">>>" <+> ppr c2
ppAComp0 (ABranch x c1 c2) 
  = text "if" <+> ppr x $$
         text "then" <+> ppr c1 $$
         text "else" <+> ppr c2
ppAComp0 (ARepeatN n c) 
  = text "repeat" <> brackets (text "n =" <> (int n)) <> braces (ppr c)
ppAComp0 (ARepeat c)    = text "repeat" <> braces (ppr c)
ppAComp0 (AWhile x c)   = text "while" <> parens (ppr x) <> braces (ppr c)
ppAComp0 (AUntil x c)   = text "do-unitl" <> parens (ppr x) <> braces (ppr c)


instance Outputable (AComp a b) where
  ppr = ppAComp

instance Show (AComp a b) where
  show = render . ppr
