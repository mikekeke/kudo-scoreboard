module Main where

import BoardN
import BoardTF
import TFBoard
import Types
import Text.Show.Pretty (ppShow)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.IORef

main :: IO ()
main = do
  someFunc
  putStrLn $ replicate 50 '*'
  putStrLn $ replicate 50 '*'
  newBoard <- newIORef emptyBoard
  runTfApp newBoard tfSomeFunc

tfSomeFunc :: (MonadIO m, ScoreB m) => m ()
tfSomeFunc = do
  registerPointTf (WhitePoint U)
  registerPointTf (WhitePoint K)
  sb <- currentScoreTf
  liftIO $ putStrLn $ ppShow sb
