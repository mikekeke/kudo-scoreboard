module Main where

import Types
import Text.Show.Pretty (ppShow)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.IORef
import TestApp qualified as TA
import Registration qualified as Reg

main :: IO ()
main = do
  putStrLn $ replicate 50 '*'
  putStrLn . ppShow $ TA.runTa someApp


-- appRun :: Monad m => m [RegisteredCargo]
someApp = do
  Reg.registerCargo (Person "Bob") (Goods ["Bob's shit"])
  Reg.registerCargo (Person "Tom") (Goods ["bread", "pitt"])
  Reg.listRegistered