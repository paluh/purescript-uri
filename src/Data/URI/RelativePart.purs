module Data.URI.RelativePart
  ( RelativePart(..)
  , parser
  , print
  , _authority
  , _path
  , module Data.URI.Authority
  , module Data.URI.Path
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.Authority (Authority(..), Host(..), Port(..), UserInfo(..))
import Data.URI.Authority as Authority
import Data.URI.Path (printPath, parsePathNoScheme, parsePathAbsolute, parsePathAbEmpty, Path(..))
import Text.Parsing.StringParser (Parser)

-- | The "relative part" of a relative reference.
data RelativePart = RelativePart (Maybe Authority) (Maybe Path)

derive instance eqRelativePart ∷ Eq RelativePart
derive instance ordRelativePart ∷ Ord RelativePart
derive instance genericRelativePart ∷ Generic RelativePart _
instance showRelativePart ∷ Show RelativePart where show = genericShow

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

_authority ∷ Lens' RelativePart (Maybe Authority)
_authority =
  lens
    (\(RelativePart a _) → a)
    (\(RelativePart _ p) a → RelativePart a p)

_path ∷ Lens' RelativePart (Maybe Path)
_path =
  lens
    (\(RelativePart _ p) → p)
    (\(RelativePart a _) p → RelativePart a p)
