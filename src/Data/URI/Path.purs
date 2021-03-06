module Data.URI.Path
  ( parsePathAbEmpty
  , parsePathAbsolute
  , parsePathNoScheme
  , parsePathRootless
  , parseSegment
  , parseSegmentNonZero
  , Path(..)
  , printPath
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.String as Str
import Data.URI.Common (PCTEncoded, decodePCT, joinWith, parsePCTEncoded, parsePChar, parseSubDelims, parseUnreserved)
import Global (encodeURI)
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.Combinators (many, many1)
import Text.Parsing.StringParser.String (string)

newtype Path = Path String
derive instance newtypePath ∷ Newtype Path _
derive instance genericPath ∷ Generic Path _
derive instance eqPath ∷ Eq Path
derive instance ordPath ∷ Ord Path
instance showPath ∷ Show Path where
  show = genericShow


parsePathAbEmpty ∷ Parser (Maybe Path)
parsePathAbEmpty
  = try (Just <<< Path <$> (joinWith "" <$> many1 (append <$> string "/" <*> parseSegment)))
  <|> pure Nothing

parsePathAbsolute ∷ Parser Path
parsePathAbsolute = Path <$> do
  try pa <|> string "/"
 where
  pa = do
    _ <- string "/"
    start ← parseSegmentNonZero
    rest ← joinWith "" <$> many (append <$> string "/" <*> parseSegment)
    pure $ "/" <> start <> rest

parsePathNoScheme ∷ Parser Path
parsePathNoScheme =
  Path <$> p
 where
  p =
    append
      <$> parseSegmentNonZeroNoColon
      <*> (joinWith "" <$> many (append <$> string "/" <*> parseSegment))

parsePathRootless ∷ Parser Path
parsePathRootless =
  Path <$> p
 where
  p =
    append
      <$> parseSegmentNonZero
      <*> (joinWith "" <$> many (append <$> string "/" <*> parseSegment))


parseSegment ∷ Parser String
parseSegment = joinWith "" <$> many (parsePChar decoder)

parseSegmentNonZero ∷ Parser String
parseSegmentNonZero = joinWith "" <$> many1 (parsePChar decoder)

parseSegmentNonZeroNoColon ∷ Parser String
parseSegmentNonZeroNoColon =
  joinWith "" <$> many1
    (parseUnreserved <|> parsePCTEncoded decoder <|> parseSubDelims <|> string "@")

decoder ∷ PCTEncoded → String
decoder = Str.replaceAll (Str.Pattern "%23") (Str.Replacement "#") <<< decodePCT

printPath ∷ Path → String
printPath (Path p) =
    escape p
  where
    escape =
      Str.replaceAll (Str.Pattern "#") (Str.Replacement "%23") <<< encodeURI
