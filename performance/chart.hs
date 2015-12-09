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

import Control.Applicative                       ((<*), (<$>), (*>))
import Control.Arrow                             ((&&&))
import Control.Lens                              ((.~))
import Data.Default.Class                        (def)
import Data.List                                 (sort, nub, transpose, isPrefixOf)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import System.Directory                          (getDirectoryContents)
import System.Environment                        (getArgs)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char        (digit, space, string, anyChar)

-------------------------------------------------------------------------------
-- BEGIN PARSER

-- | The RadioMode can be in Transmission, Reception, or CCA
data RadioMode = TX | RX deriving (Read, Show, Eq, Ord)

-- | Data type holding the test results
data TestResult = TestResult { dummySamples :: Double
                             , timeElapsed :: Double
                             , bytesCopied :: Double
			     , mode :: RadioMode
			     , bitrate :: Double
                             } deriving (Show, Eq)

instance Ord TestResult where
  (TestResult { mode = m, bitrate = br })
    `compare` (TestResult { mode = m2, bitrate = br2 }) =
       (m,br) `compare` (m2, br2)

-- | Parse TX | RX with the bitrate, or CCA
parseRadioMode :: Parser (String, String)
parseRadioMode = do
  mode <- choice [string "TX", string "RX"]
  bitrate <- try (many1 digit) <* string "Mbps" <|> string "CCA"
  if bitrate == "CCA" then return (mode, "0") else return (mode, bitrate)

-- | Parse all TestResult's and backtrack at EOF
parseManyTests :: Parser [TestResult]
parseManyTests = do
  xs <- many (try (manyTill anyChar (try $ string "./test")
                   *> parseTestResult))
          <|> parseEOF
  return xs

-- | Parser for each test result. Very crude implementation.
parseTestResult :: Parser TestResult
parseTestResult = do
  -- string "./test"
  (mode, bitrate) <- parseRadioMode
  string ".out --input=dummy \\\n"
  many space
  string "--dummy-samples="
  ds <- many1 digit
  string " \\\n"
  many space
  string "--output=dummy"
  manyTill anyChar (try $ (string "Time Elapsed: "))
  -- many space
  -- string "Time Elapsed: "
  te <- many1 digit
  units <- optional $ try $ space *> string "us"
  many space
  string "Bytes copied: "
  sign <- optional $ char '-'
  bc <- many1 digit
  -- char '\n'
  return $ TestResult (read ds :: Double)
                      (read te :: Double)
                      (read bc :: Double)
		      (read mode :: RadioMode)
		      (read bitrate :: Double)

-- | Always return an empty list at EOF.
parseEOF :: Parser [TestResult]
parseEOF = do
  manyTill anyChar eof
  return []

-- | Parse the list of TestResults from the file.
-- This function throws an error if the parse failed.
parseFile :: String -> IO [TestResult]
parseFile file = do
  stringReadFromFile <- readFile file
  case parse parseManyTests "" stringReadFromFile of
    Left err -> error $ "Parse error in file: " ++ file
    Right xs -> return $ (nub . reverse . sort) xs

-- | Grab performance numbers, where performance is determined
-- by (bytesCopied / timeElapsed). Higher is better.
getPerformance :: [[TestResult]] -> [[(Double, String)]]
getPerformance = map (map ((\(d,t,m,r) -> ((d / t), (show m ++ show r)))
	       . (\x -> (dummySamples x, timeElapsed x, mode x, bitrate x))))

-- END PARSER
-------------------------------------------------------------------------------

perfDir :: FilePath
perfDir = "perf"

-- | Parse the text files, and render the diagram to an SVG
main :: IO ()
main = do
  -- get directory contents
  fs <- filter (`notElem` [".",".."]) <$> getDirectoryContents perfDir
  -- perpend folder root
  let pfs = fmap (\x -> perfDir ++ "/" ++ x) fs
  -- parse the files and extract throughput
  nss <- getPerformance . transpose <$> mapM parseFile pfs
  -- output the result
  _ <- renderableToFile def "output.svg" (renderableContext nss fs)
  -- and we are done
  return ()

-------------------------------------------------------------------------------
-- BEGIN CHART

-- | Helper function to create plot
createPlot values fs = plot_bars_titles .~ fs
                     $ plot_bars_values .~ addIndexes values
                     $ plot_bars_style .~ BarsClustered
                     $ def

-- | Helper function to create layout
layout title pb labels =
    layout_title .~ title
  $ layout_plots .~ [plotBars pb]
  $ layout_x_axis . laxis_generate .~ autoIndexAxis labels
  $ layout_y_axis . laxis_override .~ axisGridHide
  $ layout_left_axis_visibility . axis_show_ticks .~ False
  $ layout_y_axis . laxis_title .~ "Throughput in Mbps"
  $ def

-- | First plot
plot1 nss fs = createPlot nss fs

-- | First layout
layout1 nss fs = layout "RX Performance comparison" (plot1 dss fs) lns
  where dss = take nElems . map (map fst) $ nss
        ls = map (head . (map snd)) nss
	nElems = (length . takeWhile (isPrefixOf "RX") $ ls) - 1
        lns = take nElems ls

-- | Second plot
plot2 nss fs = createPlot nss fs

-- | Second layout
layout2 nss fs = layout "RXCCA + TX Performance comparison" (plot2 dss fs) lns
  where dss = drop nElems . map (map fst) $ nss
        ls = map (head . (map snd)) nss
	nElems = (length . takeWhile (isPrefixOf "RX") $ ls) - 1
	lns = drop nElems ls

-- | Helper function to stack layouts
mkStack ls = renderStackedLayouts
           $ slayouts_layouts .~ ls
	   $ slayouts_compress_legend .~ True
           $ def

-- | Helper function to create renderable context
renderableContext :: [[(Double, String)]] -> [FilePath] -> Renderable ()
renderableContext nss fs = mkStack [ StackedLayout (layout1 nss fs)
                                   , StackedLayout (layout2 nss fs)
				   ]

-- END CHART
-------------------------------------------------------------------------------

