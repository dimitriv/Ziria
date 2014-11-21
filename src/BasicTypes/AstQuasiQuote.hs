{-# OPTIONS_GHC -Wall -Wwarn #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module AstQuasiQuote where -- (zexpr, zcomp) where

import Data.Data (Data)
import Data.Set (Set)
import Data.Generics (extQ)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec (setPosition)
import Text.Parsec.Pos (SourcePos, newPos)
import qualified Data.Set as Set

import AstExpr
import AstComp
import AstUnlabelled
import BlinkParseExpr (parseExpr)
import BlinkParseComp (parseComp, parseTopLevel)
import BlinkParseM (BlinkParser, runParseM)

{-------------------------------------------------------------------------------
  Quasi-quoters
-------------------------------------------------------------------------------}

zexpr :: QuasiQuoter
zexpr = genericQQ "zexpr QQ" exprFVs (parseTopLevel parseExpr)

zcomp :: QuasiQuoter
zcomp = genericQQ "zcomp QQ" compEFVs (parseTopLevel parseComp)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

genericQQ :: forall a. (Data a)
          => String
          -> (a -> Set (GName (Maybe SrcTy)))        -- ^ Free variables
          -> BlinkParser a
          -> QuasiQuoter
genericQQ src freeVars parser = QuasiQuoter {
      quoteExp  = \str -> do a <- parseQ str ; dataToExpQ (overrideExp a) a
    , quotePat  = error "cannot use this quasi-quoter to write patterns"
    , quoteType = error "cannot use this quasi-quoter to write types"
    , quoteDec  = error "cannot use this quasi-quoter to write declarations"
    }
  where
    parseQ :: String -> Q a
    parseQ str = do
      pos <- getPositionQ
      let parser' = setPosition pos >> parser
      me <- runIO $ runParseM parser' src str
      case me of
        Left err -> fail (show err)
        Right e  -> return e

    overrideExp :: a -> forall b. Data b => b -> Maybe ExpQ
    overrideExp a = const Nothing `extQ` overrideMetaExp fvs
                                  `extQ` overrideMetaLI
      where
        fvs = Set.toList (freeVars a)

    overrideMetaExp :: [GName (Maybe SrcTy)] -> SrcExp -> Maybe ExpQ
    overrideMetaExp fvs e =
      case unExp e of
        EVar nm | any (\nm' -> name nm == name nm') fvs ->
          Just [| toSrcExp $(varE (mkName (name nm))) |]
        _ -> Nothing

    overrideMetaLI :: LengthInfo -> Maybe ExpQ
    overrideMetaLI (LIMeta x) =
      Just [| LILength (fromIntegral $(varE (mkName x))) |]
    overrideMetaLI _ =
      Nothing

-- | Get current position in the source file, but in parsec format
getPositionQ :: Q SourcePos
getPositionQ = fmap transPos location
  where
    transPos loc = newPos (loc_filename loc)
                          (fst (loc_start loc))
                          (snd (loc_start loc))

{-------------------------------------------------------------------------------
  Inject stuff from the internal language into the external language
-------------------------------------------------------------------------------}

class ToSrcExp a where
  toSrcExp :: a -> SrcExp

instance ToSrcExp (GName Ty) where
  toSrcExp nm = eVar Nothing $ nm { nameTyp = Just (SrcInject (nameTyp nm)) }

instance ToSrcExp Integer where
  toSrcExp = eVal Nothing Nothing . VInt
