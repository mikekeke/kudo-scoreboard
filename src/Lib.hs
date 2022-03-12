module Lib
  ( someFunc,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
-- import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import UnliftIO.STM

data BoardState = BS Int

up (BS i) = BS (succ i)

someFunc :: IO ()
someFunc = do
  static <- getStaticDir
  newState <- newTVarIO (BS 0)
  startGUI defaultConfig {jsStatic = Just static} (setup newState)
  where
    getStaticDir = pure "static"

setup :: TVar BoardState -> Window -> UI ()
setup bs w = void $ do
  return w # set title "Board"
  let ps = (mkScoreBlock <$> "hkuw")
  getBody w #+
   [row  ps]



mkScoreBlock :: Char -> UI Element
mkScoreBlock name = do
  bs <- newTVarIO (BS 0)
  (BS i) <- readTVarIO bs
  button <- UI.button #. "button" #+ [string "+"]
  nameLbl <- UI.label #. "label 1" #+ [string (show name)]
  lbl <- UI.label #. "label 1" #+ [string (show i)]
  view <- UI.div #+
            [ UI.p #+ [element nameLbl]
            , UI.p #+ [element lbl]
            , UI.p #+ [element button]
            ]
  on UI.click button $ \_ -> do
    atomically $ modifyTVar' bs up 
    BS i' <- readTVarIO bs
    element lbl # set text (show i')
  return view