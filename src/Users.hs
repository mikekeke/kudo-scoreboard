module Users where

import Control.Arrow (left)
import Control.Monad.Trans.Either
  ( EitherT,
    firstEitherT,
    newEitherT,
    runEitherT,
  )
import Repos
  ( UserRegistry (addUser, allUsers, getUser),
    UserRepoError (UserNotFound),
  )
import Types (Person (phone))

-- data UserServiceError
--   = AlreadyRegistered Person String
--   deriving stock (Eq, Show)

-- registerUser ::
--   (UserRegistry m) =>
--   Person ->
--   m (Either UserServiceError ())
-- registerUser p = do
--   eUser <- getUser (phone p)
--   case eUser of
--     Right _ -> pure $ Right ()
--     Left (UserNotFound _) -> addUser p `rethrow` someErr
--     Left e -> pure $ Left (someErr e)
--   where
--     someErr :: Show a => a -> UserServiceError
--     someErr = SomeUserSvcError . show

-- -- contragents ::
-- --   (UserRegistry m) =>
-- --   m (Either UserServiceError [Person])
-- -- contragents = allUsers `rethrow` (SomeUserSvcError . show)

-- rethrow ::
--   (Monad m) =>
--   m (Either e a) ->
--   (e -> UserServiceError) ->
--   m (Either UserServiceError a)
-- rethrow action errCons = left errCons <$> action
