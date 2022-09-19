{-# LANGUAGE ScopedTypeVariables #-}

module Registration where

import Control.Monad.Trans.Either
import Debug.Trace

import Types
import Repos

data RegistrationError
  = CargoRegErr CargoRegistryError
  | IdServiceErr IdServiceError
  deriving stock Show

registerCargo :: (CargoRegistry m, IdService m) => Person -> Goods -> m (Either RegistrationError ())
registerCargo p gs = runEitherT $ do
  newCargoId <- nextCargoId `withErrorAs` IdServiceErr
  let newCargo = Cargo newCargoId p gs
  res <- addCargo newCargo `withErrorAs` CargoRegErr
  pure res


listRegistered :: (CargoRegistry m) => m (Either RegistrationError [Cargo])
listRegistered = runEitherT $ do 
  allCargos `withErrorAs` CargoRegErr

withErrorAs ::
  Monad m =>
  m (Either e a) ->
  (e -> RegistrationError) ->
  EitherT RegistrationError m a
withErrorAs action errorCons = firstEitherT errorCons $ newEitherT action 
