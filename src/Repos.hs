module Repos where

import Types

class CargoRegistry m where
  -- nextRegId :: m CargoId
  addCargo :: Cargo -> m ()
  allCargos :: m [RegisteredCargo]