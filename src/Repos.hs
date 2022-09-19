module Repos where

import Data.UUID (UUID)

import Types

data CargoRegistryError 
  = SomeCargoRegError String
  deriving stock Show

class Monad m => CargoRegistry m where
  -- nextRegId :: m CargoId
  addCargo :: Cargo -> m (Either CargoRegistryError ())
  allCargos :: m (Either CargoRegistryError [Cargo])

data IdServiceError
  = SomeIdServiceErr String
  deriving stock Show

class Monad m => IdService m where
  nextCargoId :: m (Either IdServiceError CargoId)
