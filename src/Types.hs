module Types where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Foldable (for_)
import Data.Traversable (for)
import Text.Show.Pretty (ppShow)

newtype CargoId = CargoId Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Enum

data Cargo = Cargo { cOwner :: Person, cGoods :: Goods}
  deriving stock Show

data RegisteredCargo = RegisteredCargo {rcId :: CargoId, rcCargo :: Cargo}
  deriving stock Show

newtype Goods = Goods {goodsList :: [String]}
  deriving stock Show

data Person = Person {name :: String}
  deriving stock Show