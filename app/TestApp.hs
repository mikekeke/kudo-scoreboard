module TestApp where

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Functor.Identity
import Control.Monad.State.Class

import Types
import Repos

type LastId = CargoId
type CargoStorage = (LastId, Map CargoId RegisteredCargo)

newtype TestCargoApp a = TestCA {runTestCA :: StateT CargoStorage Identity a}
  deriving newtype (Functor, Applicative, Monad, MonadState CargoStorage )

runTa app = runIdentity $ evalStateT (runTestCA app) (CargoId 0, mempty)


instance CargoRegistry TestCargoApp where

  addCargo c = do
    modify (\(lId, cs) -> let newId = succ lId
                              newRegCargo = RegisteredCargo newId c
                          in (newId, Map.insert newId newRegCargo cs)
           )

  allCargos =
    gets (Map.elems . snd)
