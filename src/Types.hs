module Types where

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import Data.UUID (UUID)
import Text.Show.Pretty (ppShow)

data Cargo = Cargo
  { cId :: CargoId,
    cOwner :: Person,
    cGoods :: Goods
  }
  deriving stock (Show)

data CargoId = CargoId {getId :: UUID}
  deriving stock (Eq, Ord, Show)

newtype Goods = Goods
  { goodsList :: [String]
  }
  deriving stock (Show)

data Person = Person
  { name :: String
  , phone :: UserPhone -- used for indetification 
  }
  deriving stock (Show)

type UserPhone = String