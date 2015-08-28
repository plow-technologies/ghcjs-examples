{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Textbox where

import           Prelude                        hiding (div, sequence)

import           Control.Monad                  (void)
import           Data.Maybe
import           Data.Text                      (Text, pack, unpack)
import qualified Data.Sequence                  as S

-- VDOM, GHCJS, Valentine
import           Control.Concurrent.STM.Notify
import           GHCJS.VDOM
import           LiveVDom.Adapter.Types
import           LiveVDom.Components
import           LiveVDom.Event
import           LiveVDom.Message
import           LiveVDom.Render
import           LiveVDom.Types
import           Valentine

runTextboxDefault :: IO ()
runTextboxDefault = do
  container <- createContainer
  runConfiguration container

runConfiguration :: DOMNode -> IO ()
runConfiguration container = do
  -- spawn event info
  (modifyTextEnv, modifyTextAddr) <- spawnIO $ (Unfired :: Event String)
  tMb <- spawnIO $ S.empty
  forkModifyText modifyTextEnv tMb

  runDomI container (return ()) $ return (displayText modifyTextAddr tMb)

forkModifyText :: STMEnvelope (Event String) -> STMMailbox (S.Seq Text) -> IO ()
forkModifyText textBoxEnv (_,addr) = void $ forkOnChange textBoxEnv $ \_ -> do
  eCurrentText <- recvIO textBoxEnv
  case eCurrentText of
    Unfired -> return ()
    Fired currentText -> do 
      sendIO addr $ S.singleton (pack currentText)
      return ()

displayText :: Address (Event String) -> STMMailbox (S.Seq Text) -> LiveVDom JSEvent
displayText modifyTextAddr tMb = [valentine|
<div>
  Input Text:
  <div>
    &{forEach tMb (displayLine tMb)}
  <div>
    !{return $ textBox modifyTextAddr [] Nothing}
|]

displayLine :: STMMailbox (S.Seq Text) -> Text -> ((Maybe Text) -> Message ()) -> LiveVDom JSEvent
displayLine _ t _ = [valentine|
<span>
  #{return . unpack $ t}
|]
