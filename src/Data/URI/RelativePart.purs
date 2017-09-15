module Data.URI.RelativePart where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI (RelativePart(..))
import Data.URI.Authority as Authority
import Data.URI.Path (printPath, parsePathNoScheme, parsePathAbsolute, parsePathAbEmpty)
import Text.Parsing.StringParser (Parser)

parser ∷ Parser RelativePart
parser = withAuth <|> withoutAuth
  where

  withAuth =
    RelativePart
      <$> Just <$> Authority.parser
      <*> parsePathAbEmpty

  withoutAuth = RelativePart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute)
    <|> (Just <$> parsePathNoScheme)
    <|> pure Nothing

print ∷ RelativePart → String
print (RelativePart a p) =
  S.joinWith "" $
    catMaybes
      [ Authority.print <$> a
      , printPath <$> p
      ]
