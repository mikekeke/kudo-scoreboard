module DbApp where

import Control.Arrow (left)
import Control.Exception (IOException, try)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), asks, runReader)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Database.SQLite.Simple (Connection, execute, execute_, open, query_, SQLError)
import Repos (UserRegistry (addUser, allUsers, getUser), UserRepoError (OtherUSerRepoErr, UserNotFound))
import SQLite.Instances
import Types (Person (Person))

newtype DbApp a = DbApp {unDbApp :: ReaderT DbAppEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader DbAppEnv, MonadIO)

data DbAppEnv = DbAppEnv
  { dbConn :: Connection
  }

-- data DbEnv = DbEnv
--   {
--   }
data DbAppConfig = DbAppConfig

bpPath :: String
bpPath = "app.db"

setup :: DbAppConfig -> IO DbAppEnv
setup _ = do
  conn <- open bpPath
  execute_ conn "CREATE TABLE IF NOT EXISTS person (name TEXT, phone TEXT)"
  return (DbAppEnv conn)

run = runReaderT . unDbApp

instance UserRegistry DbApp where
  addUser = addUser_

  getUser _ = undefined -- pure $ Left (UserNotFound "test no found phone")

  allUsers = allUsers_

addUser_ :: Person -> DbApp (Either UserRepoError ())
addUser_ p = do
  conn <- asks dbConn
  tryAdd conn
    <&> left (OtherUSerRepoErr . show)
  where
    tryAdd conn =
      liftIO $
        try @SQLError $
          execute
            conn
            "INSERT INTO person (name, phone) VALUES (?,?)"
            p

allUsers_ :: DbApp (Either UserRepoError [Person])
allUsers_ = do
  conn <- asks dbConn
  tryQueryAll conn
    <&> left (OtherUSerRepoErr . show)
  where
    tryQueryAll c = 
      liftIO . try @SQLError $
        (query_ c "SELECT * FROM person" :: IO [Person])