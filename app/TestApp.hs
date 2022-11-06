module TestApp where

import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Bifunctor (first, second)
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString, nil, toString)
import Data.UUID.V4 qualified as UUID
import Repos
    ( CargoRegistry(..),
      CargoRegistryError(SomeCargoRegError),
      IdService(..),
      UserRegistry(..),
      UserRepoError(..)
     )
import System.IO.Unsafe (unsafePerformIO)
import Types ( Cargo(cGoods, cId), CargoId(CargoId), Goods(Goods), Person(phone) )

data TestAppState = TestAppState 
 { taCurrentUid :: UUID
 , taCargos :: Map CargoId Cargo
 , taUsers :: [Person]
 }

newtype TestCargoApp a = TestCA
  { runTestCA :: StateT TestAppState Identity a
  }
  deriving newtype (Functor, Applicative, Monad, MonadState TestAppState)

runTa :: TestCargoApp a -> a
runTa app = 
  runIdentity $ evalStateT (runTestCA app) 
  (TestAppState nil mempty mempty)

instance CargoRegistry TestCargoApp where
  addCargo c = do
    appS <- get
    case cGoods c of
      (Goods [x]) -> pure $ Left (SomeCargoRegError "Test error: registry unavailable")
      _ -> Right <$> put (appS {taCargos = Map.insert (cId c) c (taCargos appS)})

  allCargos =
    Right <$> gets (Map.elems . taCargos)

instance IdService TestCargoApp where
  nextCargoId = do
    appS <- get
    let lastUuid = taCurrentUid appS
        newLast =
          fromJust
            . fromString
            . (\(h : t) -> succ h : t)
            . toString
            $ lastUuid
    put appS {taCurrentUid = newLast}
    pure . Right $ CargoId newLast

instance UserRegistry TestCargoApp where
  addUser p = 
    get >>= 
      \s -> put s{taUsers = p : taUsers s}
      >>= pure . Right

  getUser ph = do
    users <- gets $ filter ((ph ==) . phone) . taUsers
    case users of
      [u] -> pure $ Right u
      (_:_) -> pure $ Left (ManyUsersFound ph)
      [] -> pure $ Left (UserNotFound ph)

  allUsers = gets (Right . taUsers)