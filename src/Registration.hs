module Registration where

import Types
import Repos


registerCargo :: (Monad m, CargoRegistry m) => Person -> Goods -> m ()
registerCargo p gs = do
  -- newId <- nextRegId
  let newCargo = Cargo p gs
  addCargo newCargo
  pure ()

listRegistered :: CargoRegistry m => m [RegisteredCargo]
listRegistered = 
  allCargos

