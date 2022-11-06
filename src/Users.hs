module Users where
import Types (Person (phone))
import Repos (UserRegistry (getUser, addUser, allUsers), UserRepoError (UserNotFound))
import Control.Monad.Trans.Either (runEitherT, EitherT, firstEitherT, newEitherT)
import Control.Arrow (left)

data UserServiceError
  = SomeUserSvcError String
  deriving stock Show

registerUser ::
  (UserRegistry m) =>
  Person ->
  m (Either UserServiceError ())
registerUser p = do
  eUser <- getUser (phone p)
  case eUser of
    Right _ -> pure $ Right ()
    Left (UserNotFound _) -> addUser p `rethrow` someErr
    Left e -> pure $ Left (someErr e)
  where
    someErr :: Show a => a ->  UserServiceError
    someErr = SomeUserSvcError . show

contragents ::
 (UserRegistry m) =>
 m (Either UserServiceError [Person])
contragents = allUsers `rethrow` (SomeUserSvcError . show)

rethrow :: 
  (Monad m) => 
  m (Either e a) ->
  (e -> UserServiceError) ->
  m (Either UserServiceError a)
rethrow action errCons = action >>=  pure . left errCons
-- wrapErr ::
--   (Monad m, Show e) =>
--   m (Either e a) ->
--   EitherT UserServiceError m a
-- wrapErr action = 
--   firstEitherT (SomeUserSvcError . show) $ newEitherT action 