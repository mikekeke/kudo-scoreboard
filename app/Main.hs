module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Data.Char
import Data.IORef
import DbApp qualified
import Usecase.Registration qualified as Reg
import SQLite.DebugSQLite qualified as DSQL
import TestApp qualified as TA
import Text.Show.Pretty (ppShow)
import Types
import Users qualified as Users

main :: IO ()
main = do
  putStrLn $ replicate 50 '*'
  let conf = DbApp.DbAppConfig
  res <- DbApp.setup conf >>= DbApp.run app
  print res

-- putStrLn . ppShow $ TA.runTa someApp
-- withCapitalizer $ \upperer -> putStrLn (upperer "test")

app =
  Users.registerUser (Person "Bob" "3344")
    >>= either (pure . Left) (const Users.contragents)

-- appRun :: Monad m => m [RegisteredCargo]
-- TODO: test cargo registration adds user to contragents
-- someApp =
--   (,,,)
--   <$> Reg.registerCargo (Person "Bob" "3344") (Goods ["Bob's shit"])
--   <*> Reg.registerCargo (Person "Tom" "22111") (Goods ["bread", "pitt"])
--   <*> Reg.listRegistered
--   <*> Users.contragents

withCapitalizer :: ((String -> String) -> IO ()) -> IO ()
withCapitalizer act = do
  act (map toUpper)
