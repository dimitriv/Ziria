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

module PassPipeline ( 
   runPipeLine
 , ThreadId
 , PipelineRetPkg (..) ) where

import AstExpr
import AstComp
import PpComp

import qualified Data.Set as S
import Control.Monad.State

import TcMonad ( updYldTy, updInTy )


-- A split records 
--  (a) the enclosing context, 
--  (b) the mediating buffer types, and
--  (c) the actual threads
data SplitInfo 
  = SplitInfo { split_contxt  :: CompCtxt
              , split_buffers :: [Ty]
              , split_threads :: [Comp CTy Ty] 
              }



stripLetCtxt :: Comp CTy Ty -> (CompCtxt, Comp CTy Ty)
-- Find the CompContext of a computation
stripLetCtxt c = go c
  where 
    -- It would have been nicer to build this bottom up
    go c
      = case unComp c of
          Let nm c1 c2 -> 
            let (ctx,c0) = go c2 in (CLet cloc nm c1 ctx, c0)
          LetE nm e1 c2 -> 
            let (ctx,c0) = go c2 in (CLetE cloc nm e1 ctx, c0)
          LetHeader nm fun@(MkFun (MkFunDefined {}) _ _) c2 ->
            let (ctx,c0) = go c2 in (CLetHeader cloc nm fun ctx, c0)
          LetHeader nm fun@(MkFun (MkFunExternal {}) _ _) c2 -> 
            let (ctx,c0) = go c2 in (CLetHeader cloc nm fun ctx,c0)
          LetFunC nm ps ls c1 c2 -> 
            let (ctx,c0) = go c2 in (CLetFunC cloc nm ps ls c1 ctx, c0)
          LetStruct sdef c2 -> 
            let (ctx,c0) = go c2 in (CLetStruct cloc sdef ctx, c0)
          _other -> (Hole,c)
      where cloc = compLoc c


pipeLineBase :: Comp CTy Ty -> IO ([Ty], [Comp CTy Ty])
-- Pipeline a computation that has no defs (CompCtxt) around it
-- Pre: stripLetCtxt gives empty context and the same computation back
pipeLineBase c
  = do { floated <- mapCompM_ return float_pipe c -- float |>>>| to the top
       -- ; putStrLn $ "floated = " ++ show (ppComp floated)

       ; let splits = create_splits floated       -- create splits
       ; return $ insertBufs splits [0..]         -- insert buffers 
       }

  where float_pipe c
            -- c1 >>> (c21 |>>>| c22) ~~> (c1 >>> c21) |>>>| c22
          | MkComp c0 cloc cnfo <- c
          , Par p c1 c2 <- c0
          , Par p2 c21 c22 <- unComp c2
          , AlwaysPipeline {} <- plInfo p2
          , let c21cty = compInfo c21
          , let c1cty  = compInfo c1
          , let cty    = c1cty `parCompose` c21cty
          = do { return $ cPar cloc cnfo p2 (cPar cloc cty p c1 c21) c22 }

            -- (c11 |>>>| c12) >>> c2 ~~> c11 |>>>| (c12 >>> c2)
          | MkComp c0 cloc cnfo <- c
          , Par p c1 c2 <- c0
          , Par p1 c11 c12 <- unComp c1
          , AlwaysPipeline {} <- plInfo p1
          , let c12cty = compInfo c12
          , let c2cty  = compInfo c2
          , let cty    = c12cty `parCompose` c2cty
          = do { return $ cPar cloc cnfo p1 c11 (cPar  cloc cty p c12 c2) }

          | otherwise
          = return c

        -- Just return a list of the splits 
        create_splits :: Comp CTy Ty -> [Comp CTy Ty] 
        create_splits c
          | MkComp c0 cloc cnfo <- c
          , Par p c1 c2 <- c0
          , AlwaysPipeline {} <- plInfo p
          = let c1s = create_splits c1
                c2s = create_splits c2 
            in c1s ++ c2s
          | otherwise
          = [c]


insertBufs :: [Comp CTy Ty] -> [Int] -> ([Ty], [Comp CTy Ty])
-- Takes the splits and a sequence of buffer 
-- ids, and insers read/writes where needed.
insertBufs [c1] _bids = ([],[c1])
insertBufs (c1:c2:cs) (bid:bids)
  = let buf_m   = show bid              
        c1cty   = compInfo c1            
        c1yldty = yldTyOfCTyBase c1cty
        bufty   = TBuff (IntBuf c1yldty)      
        pnfo    = mkParInfo MaybePipeline -- why not 'never'?

        c1loc   = compLoc c1
        c2loc   = compLoc c2 

        -- c1' = c1 >>> writeInternal buf_m
        cwrite = cWriteInternal c1loc (CTBase (TTrans c1yldty bufty)) buf_m
        c1' = cPar c1loc (updYldTy bufty c1cty) pnfo c1 cwrite

        -- c2' = readInternal buf_m >>> c2 
        -- NB: 'SpinOnEmpty' relevant? (had to do with Standalone Reads)
        cread = cReadInternal c2loc (CTBase (TTrans bufty c1yldty)) buf_m $ 
                SpinOnEmpty                
        c2' = cPar c2loc (updInTy bufty $ compInfo c2) pnfo cread c2 

        -- Recursively call in the rest of the splits 
        (btys, cs') = insertBufs (c2' : cs) bids
    in
    (c1yldty : btys, c1' : cs')

insertBufs _ _ 
  = error "BUG: insertBufs!"


type ThreadId = String

data PipelineRetPkg =
  MkPipelineRetPkg { context :: CompCtxt
                   , threads :: [(ThreadId, Comp CTy Ty)]
                   , buf_tys :: [Ty] }

runPipeLine :: Bool -> Comp CTy Ty -> IO PipelineRetPkg
runPipeLine dumpPipeline c
  = do { let (ctx,cbase) = stripLetCtxt c       -- get the context
--       ; putStrLn $ "cbase = " ++ show (ppCompShortFold cbase) 
       ; (buftys,splits) <- pipeLineBase cbase  -- pipeline base computation
       ; let count = length splits
       ; when (dumpPipeline && count > 1) $ 
         putStrLn ("Pipeline pass created: " ++ 
                            show count ++ " top-level splits.")
       ; let threads = map mk_thread (zip [0..] splits)
       ; return $ MkPipelineRetPkg ctx threads buftys }
  where mk_thread (tid,c) = ("thread" ++ show tid, c)

