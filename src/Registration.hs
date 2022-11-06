{-# LANGUAGE ScopedTypeVariables #-}

module Registration where

import Control.Monad.Trans.Either
import Debug.Trace
import Repos
  ( CargoRegistry (..),
    CargoRegistryError,
    IdService (..),
    IdServiceError,
    UserRegistry,
  )
import Types (Cargo (Cargo), Goods, Person)
import Users (UserServiceError, registerUser)

data RegistrationError
  = CargoRegErr CargoRegistryError
  | IdServiceErr IdServiceError
  | UserSvcError UserServiceError
  deriving stock (Show)

registerCargo ::
  (CargoRegistry m, IdService m, UserRegistry m) =>
  Person ->
  Goods ->
  m (Either RegistrationError ())
registerCargo p gs = runEitherT $ do
  newCargoId <- getNexCargoId
  let newCargo = Cargo newCargoId p gs
  res <- addNewCargo newCargo
  registerUserOfNotFound
  pure res
  where
    getNexCargoId = nextCargoId `rethrow` IdServiceErr
    addNewCargo c = addCargo c `rethrow` CargoRegErr
    registerUserOfNotFound = registerUser p `rethrow` UserSvcError

listRegistered :: (CargoRegistry m) => m (Either RegistrationError [Cargo])
listRegistered = runEitherT $ do
  allCargos `rethrow` CargoRegErr

rethrow ::
  Monad m =>
  m (Either e a) ->
  (e -> RegistrationError) ->
  EitherT RegistrationError m a
rethrow action errorCons =
  firstEitherT errorCons $ newEitherT action
