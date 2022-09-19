module Types where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Foldable (for_)
import Data.Traversable (for)
import Text.Show.Pretty (ppShow)

import Data.UUID (UUID)


data Cargo = Cargo { cId:: CargoId, cOwner :: Person, cGoods :: Goods}
  deriving stock Show

data CargoId = CargoId { getId :: UUID }
  deriving stock (Eq, Ord, Show)

newtype Goods = Goods {goodsList :: [String]}
  deriving stock Show

data Person = Person {name :: String}
  deriving stock Show