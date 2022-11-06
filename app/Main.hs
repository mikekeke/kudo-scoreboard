module Main where

import Types
import Text.Show.Pretty (ppShow)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.IORef
import TestApp qualified as TA
import Registration qualified as Reg
import Users qualified as Users
import Data.Char

main :: IO ()
main = do
  putStrLn $ replicate 50 '*'
  putStrLn . ppShow $ TA.runTa someApp
  withCapitalizer $ \upperer -> putStrLn (upperer "test")


-- appRun :: Monad m => m [RegisteredCargo]
-- TODO: test cargo registration adds user to contragents
someApp = 
  (,,,)
  <$> Reg.registerCargo (Person "Bob" "3344") (Goods ["Bob's shit"])
  <*> Reg.registerCargo (Person "Tom" "22111") (Goods ["bread", "pitt"])
  <*> Reg.listRegistered
  <*> Users.contragents


withCapitalizer :: ((String -> String) -> IO ()) -> IO ()
withCapitalizer act = do
  act (map toUpper)


