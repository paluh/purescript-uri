module Data.URI.HierarchicalPart
  ( HierarchicalPart(..)
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
import Data.URI.Authority (Authority)
import Data.URI.Authority as Authority
import Data.URI.Path (Path, printPath, parsePathRootless, parsePathAbsolute, parsePathAbEmpty)
import Data.URI.Path as Path
import Text.Parsing.StringParser (Parser)

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart = HierarchicalPart (Maybe Authority) (Maybe Path)

derive instance eqHierarchicalPart ∷ Eq HierarchicalPart
derive instance ordHierarchicalPart ∷ Ord HierarchicalPart
derive instance genericHierarchicalPart ∷ Generic HierarchicalPart _
instance showHierarchicalPart ∷ Show HierarchicalPart where show = genericShow

parser ∷ Parser HierarchicalPart
parser = withAuth <|> withoutAuth
  where
  withAuth =
    HierarchicalPart <<< Just
      <$> Authority.parser
      <*> parsePathAbEmpty

  withoutAuth = HierarchicalPart Nothing <$> noAuthPath

  noAuthPath
      = (Just <$> parsePathAbsolute)
    <|> (Just <$> parsePathRootless)
    <|> pure Nothing

print ∷ HierarchicalPart → String
print (HierarchicalPart a p) =
  S.joinWith "" (catMaybes [Authority.print <$> a, Path.printPath <$> p])

_authority ∷ Lens' HierarchicalPart (Maybe Authority)
_authority =
  lens
    (\(HierarchicalPart a _) → a)
    (\(HierarchicalPart _ p) a → HierarchicalPart a p)

_path ∷ Lens' HierarchicalPart (Maybe Path)
_path =
  lens
    (\(HierarchicalPart _ p) → p)
    (\(HierarchicalPart a _) p → HierarchicalPart a p)
