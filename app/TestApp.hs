module TestApp where

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Functor.Identity
import Control.Monad.State.Class
import System.IO.Unsafe (unsafePerformIO)

import Data.UUID (UUID, nil, fromString, toString)
import Data.UUID.V4 qualified as UUID
import Data.Bifunctor (first, second)
import Data.Maybe (fromJust)

import Types
import Repos

type CargoStorage = (UUID, Map CargoId Cargo)

newtype TestCargoApp a = TestCA {runTestCA :: StateT CargoStorage Identity a}
  deriving newtype (Functor, Applicative, Monad, MonadState CargoStorage )

runTa app = runIdentity $ evalStateT (runTestCA app) (nil, mempty)


instance CargoRegistry TestCargoApp where

  addCargo c = 
    case cGoods c of
      (Goods [x]) -> pure $ Left (SomeCargoRegError "Test error: registry unavailable")
      _ -> Right <$> modify (second (Map.insert (cId c) c))
    

  allCargos =
    Right <$> gets (Map.elems . snd)

instance IdService TestCargoApp where
  nextCargoId = do
    lastUuid <- gets fst
    let newLast = 
          fromJust 
          . fromString 
          . (\(h:t) -> succ h : t)
          . toString 
          $ lastUuid
    modify (first (const newLast))
    pure . Right $ CargoId newLast
