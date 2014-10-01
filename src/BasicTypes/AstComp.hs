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
{-# LANGUAGE GADTs, RankNTypes, DataKinds, DeriveGeneric #-}

module AstComp where

import Data.Functor.Identity ( Identity (..) )
import GHC.Generics (Generic)
import Text.Parsec.Pos
import Text.Show.Pretty (PrettyVal)
import qualified Data.Set as S

import AstExpr
import PpExpr

data PlInfo where
  AlwaysPipeline ::    Int -- use this thread id for c1
                    -> Int -- use this thread id for c2
                    -> PlInfo
  NeverPipeline  :: PlInfo
  MaybePipeline  :: PlInfo
  deriving (Generic)

isMaybeOrNever plInfo =
  case plInfo of
    AlwaysPipeline _ _ -> False
    NeverPipeline      -> True
    MaybePipeline      -> True

data ParInfo
  = ParInfo { plInfo     :: PlInfo
            , inBurstSz  :: Maybe Int
            , outBurstSz :: Maybe Int }
  deriving (Generic)

mkParInfo plInfo = ParInfo plInfo Nothing Nothing

pnever = mkParInfo NeverPipeline

type CParams  = [(Name, CallArg Ty CTy0)]
type Locals b = [(Name, Maybe (Exp b))]

-- | Identifier for tasks.
type TaskID = Int


{- TODO -- replace all the let constructs with simpler binding forms
data Bind a b
  = BindC Name (Comp a b)           -- Bind computation:       let comp x = ...
  | BindStruct String StructDef     -- Structure definition:   struct x = { ... }
  | BindE Name (Exp b)              -- Bind expression:        let x = ...
  | BindExtFun Name (Fun b)         -- Bind external function: let external f(...) : ty
  | BindFun Name (Fun b)            -- Bind ordinary function: let f(...) = ...
  | BindFunC Name CParams Locals    -- Bind comp. function:    let comp f(...) = ...
  | BindMonadic Name (Comp a b)     -- Bind from a monad:      x <- c
-}


data Comp0 a b where
  Var      :: Name -> Comp0 a b
  BindMany :: Comp a b -> [(Name,Comp a b)] -> Comp0 a b

  Seq      :: Comp a b -> Comp a b -> Comp0 a b
  Par      :: ParInfo -> Comp a b -> Comp a b -> Comp0 a b

  Let :: Name  -> Comp a b -> Comp a b -> Comp0 a b
  LetE :: Name -> ForceInline -> Exp b -> Comp a b -> Comp0 a b

  -- CL
  LetERef :: Name -> Either Ty (Exp b) -> Comp a b -> Comp0 a b

  LetHeader :: Name -> Fun b -> Comp a b -> Comp0 a b
  --

  LetFunC     :: Name
              -> [(Name, CallArg Ty CTy0)] -- params (could include computation types)
              -> [(Name,Ty,Maybe (Exp b))] -- locals
              -> Comp a b                  -- body
              -> Comp a b                  -- rhs
              -> Comp0 a b

  -- Struct definition
  LetStruct :: StructDef -> Comp a b -> Comp0 a b

  Call   :: Name -> [CallArg (Exp b) (Comp a b)] -> Comp0 a b
  Emit   :: Exp b -> Comp0 a b
  Emits  :: Exp b -> Comp0 a b
  Return :: ForceInline -> Exp b -> Comp0 a b
  Interleave :: Comp a b -> Comp a b -> Comp0 a b
  Branch     :: Exp b -> Comp a b -> Comp a b -> Comp0 a b
  Take1      :: Comp0 a b

  Take       :: Exp b -> Comp0 a b -- I think we only use this with EVal !!! Replace Exp b --> Int.

  Until      :: Exp b -> Comp a b -> Comp0 a b

  While      :: Exp b -> Comp a b -> Comp0 a b

  -- times N count_var computation
  Times      :: UnrollInfo -> Exp b -> Exp b -> Name -> Comp a b -> Comp0 a b
  -- First is starting value, second is length

  -- This will replace times eventually
  -- For        :: Name -> Exp b -> Exp b -> Comp a b -> Comp 0 a b


  Repeat :: Maybe VectAnn -- optional vectorization width annotation
         -> Comp a b -> Comp0 a b

  -- A computer annotated with vectorization width
  -- information.
  -- NB: it must be a computer -- not transformer
  -- Also notice we allow only rigid vectorization
  -- annotations here, the programmer must know what they are doing.
  VectComp :: (Int,Int) -> Comp a b -> Comp0 a b

  Map    :: Maybe VectAnn -- optional vectorization width annotation
         -> Name -> Comp0 a b

  Filter :: Exp b -> Comp0 a b

  -- Source/sink primitives, see Note [RW Annotations]
  ReadSrc  :: RWTypeAnn -> Comp0 a b
  WriteSnk :: RWTypeAnn -> Comp0 a b

  -- Read/write from thread separator queues
  -- What is ReadType? See Note [Standalone Reads]
  ReadInternal  :: BufId -> ReadType -> Comp0 a b
  WriteInternal :: BufId -> Comp0 a b

  -- Pipelining primitives
  Standalone :: Comp a b -> Comp0 a b

  Mitigate :: Ty -> Int -> Int -> Comp0 a b
  -- Mitigate t n1 n2
  -- Pre: n1, n2 > 1
  -- This is a transformer of type:  ST T (arr t n1) (arr t : n2)

  -- | Activate a task. Only ever inserted by the execution planner
  --   for parallel compilation.
  --   ActivateTask may be generated as part of a BindMany. If this
  --   happens, the Maybe Name argument should be filled with the name
  --   of the computation's input var.
  ActivateTask :: TaskID -> Maybe Name -> Comp0 a b

  -- | Deactivate the currently running task. Only ever inserted by
  --   the execution planner.
  DeactivateSelf :: Comp0 a b
  deriving (Generic)

data VectAnn = Rigid Bool (Int,Int) -- True == allow mitigations up, False == disallow mitigations up
             | UpTo  Bool (Int,Int)
  deriving (Generic)


-- Call argument information
data CallArg a b
  = CAExp  { unCAExp  :: a }
  | CAComp { unCAComp :: b }
  deriving (Generic)


-- A view of some Pars as a list 
data ParListView a b 
  = ParListView { plv_loc  :: CompLoc 
                , plv_nfo  :: a 
                , plv_head :: Comp a b 
                , plv_rest :: [(ParInfo,Comp a b)] 
                }

parsToParList :: Comp a b -> ParListView a b 
parsToParList c
  = ParListView { plv_loc  = compLoc c
                , plv_nfo  = compInfo c
                , plv_head = cfst
                , plv_rest = crest 
                }
  where (cfst,crest) = go c 
        go :: Comp a b -> (Comp a b, [(ParInfo, Comp a b)])
        go c@(MkComp (Par p c1 c2) loc nfo)
          = let (cfst,c1rest)  = go c1
                (c2fst,c2rest) = go c2
            in (cfst, c1rest ++ (p,c2fst) : c2rest)
        go cother = (cother, [])


cVar :: Maybe SourcePos -> a -> Name -> Comp a b
cVar loc a x = MkComp (Var x) loc a

cBindMany :: Maybe SourcePos -> a -> Comp a b -> [(Name,Comp a b)] -> Comp a b
cBindMany loc a c cs = MkComp (mkBindMany c cs) loc a -- NB: using smar constructor

cSeq :: Maybe SourcePos -> a -> Comp a b -> Comp a b -> Comp a b
cSeq loc a c1 c2 = MkComp (Seq c1 c2) loc a

cPar :: Maybe SourcePos -> a -> ParInfo -> Comp a b -> Comp a b -> Comp a b
cPar loc a pi c1 c2 = MkComp (Par pi c1 c2) loc a

cLet :: Maybe SourcePos -> a -> Name ->
        Comp a b -> Comp a b -> Comp a b
cLet loc a x c1 c2 = MkComp (Let x c1 c2) loc a

cLetE :: Maybe SourcePos -> a -> Name -> ForceInline ->
         Exp b -> Comp a b -> Comp a b
cLetE loc a x fi e c = MkComp (LetE x fi e c) loc a

-- CL
cLetERef :: Maybe SourcePos -> a -> Name -> Either Ty (Exp b) -> Comp a b -> Comp a b
cLetERef loc a x y c = MkComp (LetERef x y c) loc a

cLetHeader :: Maybe SourcePos -> a -> Name -> Fun b -> Comp a b -> Comp a b
cLetHeader loc a x f c = MkComp (LetHeader x f c) loc a
--

cLetFunC :: Maybe SourcePos -> a -> Name -> [(Name, CallArg Ty CTy0)]
         -> [(Name,Ty,Maybe (Exp b))] -> Comp a b -> Comp a b -> Comp a b
cLetFunC loc a x args locs c1 c2 = MkComp (LetFunC x args locs c1 c2) loc a

cLetStruct :: Maybe SourcePos -> a -> StructDef -> Comp a b -> Comp a b
cLetStruct loc a sd c = MkComp (LetStruct sd c) loc a

cCall :: Maybe SourcePos -> a -> Name -> [CallArg (Exp b) (Comp a b)] -> Comp a b
cCall loc a x es = MkComp (Call x es) loc a

cEmit :: Maybe SourcePos -> a -> Exp b -> Comp a b
cEmit loc a e = MkComp (Emit e) loc a

cEmits :: Maybe SourcePos -> a -> Exp b -> Comp a b
cEmits loc a e = MkComp (Emits e) loc a

cReturn :: Maybe SourcePos -> a -> ForceInline -> Exp b -> Comp a b
cReturn loc a fi e = MkComp (Return fi e) loc a

cInterleave :: Maybe SourcePos -> a -> Comp a b -> Comp a b -> Comp a b
cInterleave loc a c1 c2 = MkComp (Interleave c1 c2) loc a

cBranch :: Maybe SourcePos -> a -> Exp b -> Comp a b -> Comp a b -> Comp a b
cBranch loc a e c1 c2 = MkComp (Branch e c1 c2) loc a

cTake1 :: Maybe SourcePos -> a -> Comp a b
cTake1 loc a = MkComp Take1 loc a

cTake :: Maybe SourcePos -> a -> Exp b -> Comp a b
cTake loc a e = MkComp (Take e) loc a

cUntil :: Maybe SourcePos -> a -> Exp b -> Comp a b -> Comp a b
cUntil loc a e c = MkComp (Until e c) loc a

cWhile :: Maybe SourcePos -> a -> Exp b -> Comp a b -> Comp a b
cWhile loc a e c = MkComp (While e c) loc a

cTimes :: Maybe SourcePos -> a -> UnrollInfo -> Exp b -> Exp b -> Name -> Comp a b -> Comp a b
cTimes loc a ui es elen x c = MkComp (Times ui es elen x c) loc a

cRepeat :: Maybe SourcePos -> a -> Maybe VectAnn -> Comp a b -> Comp a b
cRepeat loc a ann c = MkComp (Repeat ann c) loc a

cVectComp :: Maybe SourcePos -> a -> (Int,Int) -> Comp a b -> Comp a b
cVectComp loc a ann c = MkComp (VectComp ann c) loc a

cMap :: Maybe SourcePos -> a -> Maybe VectAnn -> Name -> Comp a b
cMap loc a ann nm = MkComp (Map ann nm) loc a

cFilter :: Maybe SourcePos -> a -> Exp b -> Comp a b
cFilter loc a e = MkComp (Filter e) loc a

cReadSrc  :: Maybe SourcePos -> a -> RWTypeAnn -> Comp a b
cReadSrc loc a ann = MkComp (ReadSrc ann) loc a

cWriteSnk :: Maybe SourcePos -> a -> RWTypeAnn -> Comp a b
cWriteSnk loc a ann = MkComp (WriteSnk ann) loc a

cReadInternal  :: Maybe SourcePos -> a -> BufId -> ReadType -> Comp a b
cReadInternal  loc a b rt = MkComp (ReadInternal b rt) loc a

cWriteInternal :: Maybe SourcePos -> a -> BufId -> Comp a b
cWriteInternal loc a b = MkComp (WriteInternal b) loc a

cStandalone :: Maybe SourcePos -> a -> Comp a b -> Comp a b
cStandalone loc a c = MkComp (Standalone c) loc a


cMitigate :: Maybe SourcePos -> a -> Ty -> Int -> Int -> Comp a b
cMitigate loc a t n1 n2 = MkComp (Mitigate t n1 n2) loc a


{- Note [RW Annotations]
   ~~~~~~~~~~~~~~~~~~~~~
   Annotations on ReadSrc and WriteSnk are of three forms
    (a) either the actual type (as given by a user annotation)
    (b) or the base type (which will be inserted there by the vectorizer
    (c) or no annotations whatsoever
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

data RWTypeAnn =
       RWBaseTyAnn Ty
     | RWRealTyAnn Ty
     | RWNoTyAnn
  deriving (Generic, Show)


-- Note [Standalone reads]
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- Reads arising from standalone pipelining are slightly problematic.
-- Normally, if a read from a thread-separator cannot be executed because
-- the queue is empty we could return SKIP and jump to this threads main
-- control loop. That's all very fine.
--
-- *Except* when this thread is the main thread (i.e. /not/ a standalone).
-- Example:
--    main thread: read >>> c1 >>> write(s) >>> read(q) >>> c2 >>> write
--    standalone : read(s) >>> s >>> write(q)
--
-- If the 'read(q)' returns SKIP and jumps back to the main threads
-- loop, next time around we will ask again to read(q), which will
-- also be empty.  Our only chance of making progress, is to jump to
-- the *consume* continuation which is the consume continuation of the
-- write(s) which will eventually cause the 's' queue to be filled in,
-- give a chance to the standalone thread to write to 'q', and then
-- give a chance to the main thread to read(q) successfully!
--
--
-- Hence, we pass a flag down to ReadInternal that will allow us to choose
-- one of the two behaviours for reading.
--
-- TODO: We might want to revisit this design later, or even deprecate the
-- standalone pipelining which does not perform great anyway, but at the time
-- of the writing of this comment (10/01/2014), this is not done.


-- See Note [Standalone Reads]
data ReadType
  = SpinOnEmpty
  | JumpToConsumeOnEmpty
  deriving (Generic)

readJumpToConsumeOnEmpty JumpToConsumeOnEmpty = True
readJumpToConsumeOnEmpty _ = False


compShortName :: Comp a b -> String
compShortName c = go (unComp c)
  where go (  Var n          ) = "Var(" ++ name n ++ ")"
        go (  BindMany    {} ) = "BindMany"
        go (  Seq         {} ) = "Seq"
        go (  Par         {} ) = "Par"
        go (  Let         {} ) = "Let"
        go (  LetE        {} ) = "LetE"
        -- CL
        go (  LetERef     {} ) = "LetERef"
        go (  LetHeader nm (MkFun (MkFunDefined _ _ _ _) _ _) _  ) = "LetHeader(" ++ name nm ++ ")"
        go (  LetHeader {} ) = "LetHeader"
        --
        go (  LetFunC nm _ _ _ _ ) = "LetFunC(" ++ name nm ++ ")"
        go (  LetStruct   {} ) = "Struct"
        go (  Call n _       ) = "Call(" ++ name n ++ ")"
        go (  Emit        {} ) = "Emit"
        go (  Emits       {} ) = "Emits"
        go (  Return      {} ) = "Return"
        go (  Interleave  {} ) = "Interleave"
        go (  Branch      {} ) = "Branch"
        go (  Take1       {} ) = "Take1"
        go (  Take        {} ) = "Take"
        go (  Until       {} ) = "Until"
        go (  While       {} ) = "While"
        go (  Times       {} ) = "Times"
        go (  Repeat      {} ) = "Repeat"
        go (  VectComp    {} ) = "VectComp"
        go (  Map         {} ) = "Map"
        go (  Filter      {} ) = "Filter"

        go (  ReadSrc         {} ) = "ReadSrc"
        go (  WriteSnk        {} ) = "WriteSnk"
        go (  ReadInternal    {} ) = "ReadInternal"
        go (  WriteInternal   {} ) = "WriteInternal"
        go (  Standalone      {} ) = "Standalone"
        go (  Mitigate        {} ) = "Mitigate"


type CompLoc = Maybe SourcePos

data Comp a b
  = MkComp { unComp   :: Comp0 a b
           , compLoc  :: CompLoc
           , compInfo :: a }
  deriving (Generic)

type SrcComp = Comp () ()

data Prog a b
  = MkProg { globals :: [(Name,Ty,Maybe (Exp b))]
           , comp :: Comp a b }
  deriving (Generic)



-- Just A binding context (for multiple threads)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We may have to pipeline under a shared context and CompCtxt simply
-- records this shared context. We are not really using this context
-- in AstComp but it seems the right place to define this datatype.
data CompCtxt
  = Hole
  | CLet  CompLoc Name (Comp CTy Ty) CompCtxt
  | CLetE CompLoc Name ForceInline (Exp Ty) CompCtxt
  -- CL
  | CLetERef CompLoc Name (Either Ty (Exp Ty)) CompCtxt
  --
  | CLetHeader CompLoc Name (Fun Ty) CompCtxt
  | CLetFunC CompLoc Name [(Name, CallArg Ty CTy0)]  -- params
                          [(Name,Ty,Maybe (Exp Ty))] -- locals
                          (Comp CTy Ty) CompCtxt     -- body
  | CLetStruct CompLoc StructDef CompCtxt


data CTy0 where
  TComp :: Ty -> Ty -> Ty -> CTy0
  TTrans :: Ty -> Ty -> CTy0
  deriving Generic

inTyOfCTy   :: CTy0 -> Ty
inTyOfCTy (TComp _ x _) = x
inTyOfCTy (TTrans x _)  = x

yldTyOfCTy  :: CTy0 -> Ty
yldTyOfCTy (TComp _ _ x)  = x
yldTyOfCTy (TTrans _ x)   = x


inTyOfCTyBase :: CTy -> Ty
inTyOfCTyBase (CTBase ct) = inTyOfCTy ct
inTyOfCTyBase _ = error "inTyOfCTyBase: not a base type!"

yldTyOfCTyBase (CTBase ct) = yldTyOfCTy ct
yldTyOfCTyBase _ = error "yldTyOfCTyBase: not a base type!"

doneTyOfCTyBase (CTBase ct) = doneTyOfCTy ct
doneTyOfCTyBase _ = error "doneTyOfCTyBase: not a base type!"


isCTyBase :: CTy -> Bool
isCTyBase (CTBase {}) = True
isCTyBase _ = False


doneTyOfCTy :: CTy0 -> Maybe Ty
-- NOTE: transformers have no doneTy
doneTyOfCTy (TComp x _ _) = Just x
doneTyOfCTy (TTrans _ _) = Nothing

hasDoneTyBase (CTBase ct) = hasDoneTy ct
hasDoneTyBase _ = error "hasDoneTyBase: not a base type!"

hasDoneTy :: CTy0 -> Bool
hasDoneTy cty
  | Just _ <- doneTyOfCTy cty = True
  | otherwise                 = False

isCompCTy :: CTy0 -> Bool
isCompCTy (TComp {}) = True
isCompCTy _ = False

data CTy where
  CTBase :: CTy0 -> CTy
  -- Invariant: non-empty list and CTy0 is not arrow
  CTArrow :: [CallArg Ty CTy0] -> CTy0 -> CTy


-- Composing transformers and computers
parCompose (CTBase (TTrans t1 _))
           (CTBase (TTrans _ t3))  = CTBase (TTrans t1 t3)
parCompose (CTBase (TTrans t1 _))
           (CTBase (TComp v _ t3)) = CTBase (TComp v t1 t3)
parCompose (CTBase (TComp v t1 _))
           (CTBase (TTrans _ t3))  = CTBase (TComp v t1 t3)
parCompose (CTBase (TComp v t1 _))
           (CTBase (TComp _ _ t3)) = CTBase (TComp v t1 t3)
parCompose _ct1 _cty2
  = error "Type checking bug: revealed in parCompose!"


toComp :: a -> Comp0 a b -> Comp a b
toComp a c0 = MkComp c0 Nothing a

toCompPos :: a -> SourcePos -> Comp0 a b -> Comp a b
toCompPos a pos c0 = MkComp c0 (Just pos) a

mkBind :: Comp a b -> (Name, Comp a b) -> Comp0 a b
mkBind c1 (n,c2) = mkBindMany c1 [(n,c2)]

mkBindMany :: Comp a b -> [(Name,Comp a b)] -> Comp0 a b
-- First push all the bindmany's on the list
mkBindMany comp args = go comp args
   where
     go (MkComp (BindMany c0 c0s) _ _) c1s = mkBindMany c0 (c0s++c1s)
     -- Now we know that 'c' is not a BindMany, empty continuation: just return
     go c [] = unComp c
     -- We know that 'c' is not a BindMany, but we do have a continuation, so
     -- recurse into the continuation to flatten the continuation this time.
     go c ((n,c1):cs)
       = case mkBindMany c1 cs of
           BindMany c1' c1s' -> BindMany c ((n,c1'):c1s')
           c1' -> BindMany c [(n, MkComp c1' (compLoc c1) (compInfo c1))]


data BindView a b
  = BindView (Comp a b) Name (Comp a b)
  | SeqView (Comp a b) (Comp a b)
  | NotSeqOrBind (Comp a b)


bindSeqView :: Comp a b -> BindView a b
bindSeqView comp = mk_view comp
  where mk_view comp@(MkComp (BindMany c1 c2s) cloc cinfo)
          = case c2s of
              (nm,c2):crest ->
                 BindView c1 nm (MkComp (mkBindMany c2 crest) cloc cinfo)
              [] ->
                 NotSeqOrBind comp
        mk_view comp@(MkComp (Seq c1 c2) cloc cinfo) = SeqView c1 c2
        mk_view comp = NotSeqOrBind comp

compFVs :: Comp a b -> S.Set Name
compFVs c = case unComp c of
  Var nm -> S.singleton nm
  BindMany c1 xs_cs ->
    -- NB: important that we `foldr' here since `x' may appear free
    -- in rest of `xs_cs'
    foldr (\(x,c) s -> (s `S.union` compFVs c) S.\\ (S.singleton x))
      (compFVs c1) xs_cs
  Seq c1 c2      -> compFVs c1 `S.union` compFVs c2
  Par _ c1 c2    -> compFVs c1 `S.union` compFVs c2
  Let nm c1 c2 -> (compFVs c1 `S.union` compFVs c2) S.\\ (S.singleton nm)
  LetE nm _ e c1 -> (exprFVs e `S.union` compFVs c1) S.\\ (S.singleton nm)
  -- CL
  LetERef nm (Right e) c1
     -> (exprFVs e `S.union` compFVs c1) S.\\ (S.singleton nm)
  LetERef nm (Left _) c1 -> (compFVs c1) S.\\ (S.singleton nm)
  LetHeader nm f c1 -> (funFVs f `S.union` compFVs c1) S.\\ (S.singleton nm)
  --
  LetFunC nm params locals c1 c2 ->
    let funVars = (foldr (\(nm,_,me) s ->
                           let se = case me of
                                 Just e  -> S.union (exprFVs e) s
                                 Nothing -> s
                           in se S.\\ (S.singleton nm)) (compFVs c1) locals) S.\\
                  (S.fromList . map fst) params
    in (funVars `S.union` compFVs c2) S.\\ (S.singleton nm)

  LetStruct _sdef c1 -> compFVs c1

  Call nm es      -> foldl (\s e -> callArgFVs e `S.union` s)
                           (S.singleton nm) es
  Emit e          -> exprFVs e
  Emits e         -> exprFVs e
  Return _ e      -> exprFVs e
  Branch e c1 c2  -> exprFVs e `S.union` compFVs c1 `S.union` compFVs c2
  Take1           -> S.empty
  Take e          -> exprFVs e
  Until e c1      -> exprFVs e `S.union` compFVs c1
  While e c1      -> exprFVs e `S.union` compFVs c1
  Times ui es elen nm c1
     -> exprFVs es `S.union` exprFVs elen `S.union` compFVs c1 S.\\ (S.singleton nm)
  Repeat _ c1     -> compFVs c1
  VectComp _ c1   -> compFVs c1
  Map _ nm         -> S.singleton nm
  Filter e        -> exprFVs e
  Interleave c1 c2 -> compFVs c1 `S.union` compFVs c2


  ReadSrc  {} -> S.empty
  WriteSnk {} -> S.empty
  ReadInternal  {} -> S.empty
  WriteInternal {} -> S.empty
  Standalone c1  -> compFVs c1

  Mitigate {} -> S.empty

callArgFVs :: CallArg (Exp b) (Comp a b) -> S.Set Name
callArgFVs (CAExp e)  = exprFVs e
callArgFVs (CAComp c) = compFVs c

compFVs_all :: [Comp a b] -> S.Set Name
compFVs_all = S.unions . map compFVs

compSize :: Comp a b -> Int
compSize c = case unComp c of
  Var nm -> 1
  BindMany c1 xs_cs ->
    foldr (\(x,c) s -> compSize c) (compSize c1) xs_cs
  Seq c1 c2      -> 1 + compSize c1 + compSize c2
  Par _ c1 c2    -> 1 + compSize c1 + compSize c2
  Let nm c1 c2 -> 1 + compSize c1 + compSize c2
  LetE nm _ e c1 -> 2 + compSize c1
  -- CL
  LetERef nm (Right _) c1 -> 2 + compSize c1
  LetERef nm (Left _) c1 -> 2 + compSize c1
  LetHeader nm f c1 -> 2 + compSize c1
  --
  LetStruct _sdef c1 -> 1 + compSize c1
  LetFunC nm params locals c1 c2 -> 1 + compSize c1 + compSize c2
  Call nm es       -> 1 + sum (map callArgSize es)
  Emit e           -> 1
  Emits e          -> 1
  Return _ e       -> 1
  Branch e c1 c2   -> 1 + compSize c1 + compSize c2
  Take1            -> 1
  Take e           -> 1
  Until e c1        -> 1 + compSize c1
  While e c1        -> 1 + compSize c1
  Times ui e1 e2 nm c1 -> 1 + compSize c1
  Repeat _ c1    -> compSize c1
  VectComp _ c1  -> compSize c1
  Map _ nm        -> 1
  Filter e       -> 1
  Interleave c1 c2 -> compSize c1 + compSize c2

  ReadSrc  {} -> 1
  WriteSnk {} -> 1
  ReadInternal  {} -> 1
  WriteInternal {} -> 1
  Standalone c1  -> compSize c1

  Mitigate {} -> 1

callArgSize :: CallArg (Exp b) (Comp a b) -> Int
callArgSize (CAExp x)  = 0
callArgSize (CAComp x) = 1

mapCompM_aux :: Monad m
             => (b -> m d)                 -- What to do on expression types
             -> (Exp b -> m (Exp d))       -- What to do on expressions
             -> (a -> m c)                 -- What to do on computation types
             -> (Comp c d -> m (Comp c d)) -- How to combine results
             -> Comp a b -> m (Comp c d)
-- NB: Ignores binding, if your monadic actions should be
-- binding-aware then you should be using some different function
mapCompM_aux on_ty on_exp on_cty g c = go c
  where
   go c = do { let cloc = compLoc c
             ; cnfo <- on_cty (compInfo c)
             ; case unComp c of
                 Var x -> g (cVar cloc cnfo x)
                 BindMany c1 xs_cs ->
                   do { c1' <- go c1
                      ; xs_cs' <- mapM (\(x,c) ->
                          do { c' <- go c
                             ; return (x,c') }) xs_cs
                      ; g (cBindMany cloc cnfo c1' xs_cs') }
                 Seq c1 c2 ->
                   do { c1' <- go c1
                      ; c2' <- go c2
                      ; g (cSeq cloc cnfo c1' c2') }
                 Par pi c1 c2 ->
                   do { c1' <- go c1
                      ; c2' <- go c2
                      ; g (cPar cloc cnfo pi c1' c2') }
                 Let x c1 c2 ->
                   do { c1' <- go c1
                      ; c2' <- go c2
                      ; g (cLet cloc cnfo x c1' c2') }
                 LetStruct sdef c1 ->
                   do { c1' <- go c1
                      ; g (cLetStruct cloc cnfo sdef c1') }
                 LetE x fi e c1 ->
                   do { e' <- on_exp e
                      ; c1' <- go c1
                      ; g (cLetE cloc cnfo x fi e' c1') }
                 -- CL
                 LetERef x (Left y) c1 ->
                    do {c1' <- go c1
                       ; g (cLetERef cloc cnfo x (Left y) c1') }
                 LetERef x (Right e) c1 ->
                    do { e' <- on_exp e
                       ; c1' <- go c1
                       ; g (cLetERef cloc cnfo x (Right e') c1') }
                 LetHeader nm fun c1 ->
                   do { fun' <- mapFunM on_ty on_exp fun
                      ; c1' <- go c1
                      ; g (cLetHeader cloc cnfo nm fun' c1') }
                 --
                 LetFunC nm params locals c1 c2 ->
                   do { locals' <- mapLocalsM on_exp locals
                      ; c1' <- go c1
                      ; c2' <- go c2
                      ; g (cLetFunC cloc cnfo nm params locals' c1' c2')
                      }
                 Call nm args ->
                   do { args' <- mapM (mapCallArgM on_exp go) args
                      ; g (cCall cloc cnfo nm args') }
                 Emit e ->
                   do { e' <- on_exp e
                      ; g (cEmit cloc cnfo e') }
                 Return fi e ->
                   do { e' <- on_exp e
                      ; g (cReturn cloc cnfo fi e') }
                 Emits e ->
                   do { e' <- on_exp e
                      ; g (cEmits cloc cnfo e') }
                 Interleave c1 c2 ->
                   do { c1' <- go c1
                      ; c2' <- go c2
                      ; g (cInterleave cloc cnfo c1' c2') }
                 Branch e c1 c2 ->
                   do { e'  <- on_exp e
                      ; c1' <- go c1
                      ; c2' <- go c2
                      ; g (cBranch cloc cnfo e' c1' c2') }

                 Take1 -> g (cTake1 cloc cnfo)

                 Take e ->
                   do { e' <- on_exp e
                      ; g (cTake cloc cnfo e') }
                 Until e c1 ->
                   do { e' <- on_exp e
                      ; c1' <- go c1
                      ; g (cUntil cloc cnfo e' c1') }
                 While e c1 ->
                   do { e' <- on_exp e
                      ; c1' <- go c1
                      ; g (cWhile cloc cnfo e' c1') }
                 Times ui e elen nm c1 ->
                   do { e' <- on_exp e
                      ; elen' <- on_exp elen
                      ; c1' <- go c1
                      ; g (cTimes cloc cnfo ui e' elen' nm c1') }

                 Repeat wdth c1 ->
                   do { c1' <- go c1
                      ; g (cRepeat cloc cnfo wdth c1') }
                 VectComp wdth c1 ->
                   do { c1' <- go c1
                      ; g (cVectComp cloc cnfo wdth c1') }
                 Map wdth nm -> g (cMap cloc cnfo wdth nm)

                 Filter e ->
                   do { e' <- on_exp e
                      ; g (cFilter cloc cnfo e') }
                 ReadSrc b         -> g (cReadSrc cloc cnfo b)
                 WriteSnk b        -> g (cWriteSnk cloc cnfo b)
                 ReadInternal b rt -> g (cReadInternal cloc cnfo b rt)
                 WriteInternal b   -> g (cWriteInternal cloc cnfo b)

                 Standalone c1 ->
                   do { c1' <- go c1
                      ; g (cStandalone cloc cnfo c1') }

                 Mitigate t n1 n2 -> g (cMitigate cloc cnfo t n1 n2)
             }

mapCallArgM :: Monad m
            => (Exp b -> m (Exp d))
            -> (Comp a b -> m (Comp c d))
            -> CallArg (Exp b) (Comp a b)
            -> m (CallArg (Exp d) (Comp c d))
mapCallArgM on_exp on_comp arg = do_arg arg
  where
    do_arg (CAExp e)  = on_exp e  >>= \e' -> return (CAExp e')
    do_arg (CAComp c) = on_comp c >>= \c' -> return (CAComp c')



mapCompM_ :: Monad m
         => (Exp b -> m (Exp b))
         -> (Comp a b -> m (Comp a b))
         -> Comp a b
         -> m (Comp a b)
mapCompM_ f g = mapCompM_aux return f return g



substComp :: Monad m => (Name, Comp a b) -> Comp a b -> m (Comp a b)
substComp (nm,c') c = mapCompM_ return aux c
  where aux c0
         | Var nm' <- unComp c0
         = if nm == nm' then return c' else return c0
         | otherwise
         = return c0


substExpComp :: Monad m => (Name, Exp b) -> Comp a b -> m (Comp a b)
substExpComp (nm,e') c = mapCompM_ aux return c
  where aux e0 = substExp (nm,e') e0


substAllComp :: Monad m => [(Name, Exp b)] -> Comp a b -> m (Comp a b)
substAllComp substs c
  | [] <- substs          = return c
  | s : substs' <- substs =
    do c' <- substAllComp substs' c
       substExpComp s c'
  | otherwise = error "substAllComp"


eraseComp :: Comp a b -> Comp () ()
eraseComp c
  = runIdentity $
    mapCompM_aux (\t -> return ())
                 (return . eraseExp)
                 (\t -> return ()) return c

eraseCallArg (CAExp e)  = CAExp (eraseExp e)
eraseCallArg (CAComp c) = CAComp (eraseComp c)

{-------------------------------------------------------------------------------
  PrettyVal instances (used for dumping the AST)
-------------------------------------------------------------------------------}

instance PrettyVal CTy0
instance PrettyVal ParInfo
instance PrettyVal PlInfo
instance PrettyVal RWTypeAnn
instance PrettyVal ReadType
instance PrettyVal VectAnn

instance (PrettyVal a, PrettyVal b) => PrettyVal (CallArg a b)
instance (PrettyVal a, PrettyVal b) => PrettyVal (Comp0 a b)
instance (PrettyVal a, PrettyVal b) => PrettyVal (Comp a b)
instance (PrettyVal a, PrettyVal b) => PrettyVal (Prog a b)
