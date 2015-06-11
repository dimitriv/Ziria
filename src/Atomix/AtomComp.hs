data Exp t b = ExpFun { expFunNm :: Name t, expFunArgs :: [GName t] }
             | ExpVar (Name t)

data Comp tc t a b 
  = MkComp { unComp   :: !(Comp0 tc t a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data Comp0 tc t a b where
  Take1 :: t -> Comp0 tc t a b
  TakeN :: t -> Int -> Comp0 tc t a b
  Emit :: GName t -> Comp0 tc t a b
  Emits :: GName t -> Comp0 tc t a b
  Return :: GName t -> Comp0 tc t a b

  Bind :: GName t -> Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b
  Let  :: GName t -> Exp t b -> Comp tc t a b -> Comp0 tc t a b
  NewRef :: GName t -> Comp tc t a b -> Comp0 tc t a b

  Seq :: Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b
  Par :: ParInfo -> Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b

  Branch :: GName t -> Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b

  RepeatN :: Int -> Comp tc t a b -> Comp0 tc t a b
  Repeat :: Comp tc t a b -> Comp0 tc t a b

  While :: GName t -> Comp tc t a b -> Comp0 tc t a b
  Until :: GName t -> Comp tc t a b -> Comp0 tc t a b

  ----------------------------------------------
  Standalone :: Comp tc t a b -> Comp0 tc t a b
  Mitigate :: String  -- just for debugging 
           -> t -> Int -> Int -> Comp0 tc t a b