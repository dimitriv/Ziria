data Comp tc t a b 
  = MkComp { unComp   :: !(Comp0 tc t a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data Exp t b = ExpFun { expFunNm :: Name t, expFunArgs :: [GName t] }
             | ExpVar (Name t)

data Comp0 tc t a b where

  -- | Monadic bind
  --
  -- This represents a chain of monadic binds, which is a good representation for
  -- flattening out stuff and nested binds etc. In other words
  --
  -- > x1 <- c1; x2 <- c2; ... xn <- cn
  --
  -- is represented as:
  --
  -- > BindMany c1 [(x1,c2), (x2,c3) .... (x_(n-1),cn)]
  Bind :: GName t -> Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b
  Let  :: GName t -> Exp t b -> Comp tc t a b -> Comp0 tc t a b
  -- | Bind a mutable variable
  NewRef :: GName t -> Comp tc t a b -> Comp0 tc t a b

  -- | Sequential composition
  --
  -- TODO: The type checker translates this to `BindMany`. If we had a more
  -- complete distinction between source and internal syntax this could
  -- probably go (#71).
  Seq :: Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b

  -- | Parallel composition
  --
  -- We can compose to transformers
  --
  -- > c1 :: ST T a b   c2 :: ST T b c
  -- > -------------------------------
  -- >     c1 >>> c2 :: ST T a c
  --
  -- or a transformer and a computer
  --
  -- > c1 :: ST (C u) a b   c2 :: ST T b c
  -- > -----------------------------------
  -- >      c1 >>> c2 :: ST (C u) a c
  --
  -- > c1 :: ST T a b   c2 :: ST (C u) b c
  -- > -----------------------------------
  -- >      c1 >>> c2 :: ST (C u) a c
  Par :: ParInfo -> Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b



  -- | Emit a value to the output stream
  --
  -- >         e :: b
  -- > -----------------------
  -- > emit e :: ST (C ()) TVoid b
  --
  -- Since the argument to `emit` does not determine `a`, we add it an an extra
  -- parameter to `Emit`.
  Emit :: GName t -> Comp0 tc t a b

  -- | Emit an array of values to the output stream
  --
  -- >      e :: arr[n] b
  -- > ------------------------
  -- > emits e :: ST (C ()) TVoid b
  --
  -- Since the argument to `emits` does not determine `a`, we add it an an extra
  -- parameter to `Emit`.
  Emits :: GName t -> Comp0 tc t a b

  -- | Return a value
  --
  -- >         e :: u
  -- > ------------------------
  -- > return e :: ST (C u) TVoid TVoid
  --
  -- Since the argument to `return` does not determine `a` and `b` we add these
  -- as extra arguments.
  Return :: GName t -> Comp0 tc t a b

  -- | Conditional
  Branch :: GName t -> Comp tc t a b -> Comp tc t a b -> Comp0 tc t a b

  -- | Take a value from the input stream
  --
  -- > --------------------
  -- > take :: ST (C a) a TVoid
  --
  -- Since `take` has no arguments we record both `a` and `b` as parameters.
  Take1 :: t -> Comp0 tc t a b

  -- | Take multiple values from the input stream
  --
  -- > --------------------------------
  -- > takes n :: ST (C (arr[n] a)) a TVoid
  --
  -- Since `takes` has no arguments we record both `a` and `b` as parameters.
  Take :: t -> Int -> Comp0 tc t a b

  -- | Iteration
  --
  -- > e :: Bool   c :: ST (C u) a b
  -- > -----------------------------
  -- >   until e c :: ST (C u) a b
  Until :: GName t -> Comp tc t a b -> Comp0 tc t a b

  -- | Iteration
  --
  -- > e :: Bool   c :: ST (C u) a b
  -- > -----------------------------
  -- >   while e c :: ST (C u) a b
  While :: GName t -> Comp tc t a b -> Comp0 tc t a b

  -- | Iteration
  --
  -- > e :: int<bw>   elen :: int<bw>   nm :: int<bw> |- c :: ST (C u) a b
  -- > -------------------------------------------------------------------
  -- >          for nm e elen  c :: ST (C u) a b
  --
  -- for (nm = e; nm < e + elen; nm++) c
  -- TODO: Replace with
  --
  -- > For :: GName ty -> Exp t b -> Exp t b -> Comp tc t a b -> Comp tc t 0 a a
  RepeatN :: Int -> Comp tc t a b -> Comp0 tc t a b

  -- | Repeat a computer (to get a transformer)
  --
  -- >  c :: ST (C ()) a b
  -- > --------------------
  -- > repeat c :: ST T a b
  --
  Repeat :: Comp tc t a b -> Comp0 tc t a b

  -- | Standalone computations (forked onto another core)
  --
  -- >      c :: ST T a b
  -- > ------------------------
  -- > standalone c :: ST T a b
  Standalone :: Comp tc t a b -> Comp0 tc t a b

  -- | Downgrade or upgrade the rate of components.
  --
  -- > n1, n2 > 1    (n2 `divides` n1 || n1 `divides` n2)
  -- > --------------------------------------------------
  -- >  Mitigate a n1 n2 :: ST T (arr[n1] a) (arr[n2] a)
  --
  -- >             n2 > 1
  -- > -------------------------------------
  -- > Mitigate a 1 n2 :: ST T a (arr[n2] a)
  --
  -- >             n1 > 1
  -- > -------------------------------------
  -- > Mitigate a n1 1 :: ST T (arr[n1] a) a
  --
  -- > --------------------------
  -- > Mitigate a 1 1 :: ST T a a
  Mitigate :: String  -- just for debugging 
           -> t -> Int -> Int -> Comp0 tc t a b