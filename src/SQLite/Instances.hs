module SQLite.Instances where

import Database.SQLite.Simple (FromRow (fromRow), ToRow (toRow), field)
import GHC.Generics (Generic)
import Types (Person (Person))

instance FromRow Person where
  fromRow = Person <$> field <*> field

instance ToRow Person where
  toRow (Person n p) = toRow (n, p)