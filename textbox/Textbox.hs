{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Textbox where

-- import           Data.Aeson
-- Base
import           Control.Applicative
-- import           Control.Lens
import           Data.Maybe
import           Data.Traversable
import           Prelude                        hiding (div, sequence)

import           Control.Concurrent.STM.Message
import           Control.Concurrent.STM.Notify
import           Control.Concurrent.STM.TMVar

-- GHCJS/VDom/Ophelia
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Marshal
import           GHCJS.Types
import           GHCJS.VDOM

import           Shakespeare.Dynamic.Components
import           Shakespeare.Dynamic.Event
import           Shakespeare.Dynamic.Render
import qualified VDOM.Adapter                   as VDA

import           Control.Concurrent
import           Control.Monad                  (join, void, when)
import           Control.Monad.STM
import           Data.Text                      (Text, pack, unpack)
import           Shakespeare.Ophelia
import           Text.Read

import qualified Data.Sequence                  as S
import           VDOM.Adapter

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

displayText :: Address (Event String) -> STMMailbox (S.Seq Text) -> LiveVDom VDA.JSEvent
displayText modifyTextAddr tMb = [gertrude|
<div>
  Input Text:
  <div>
    &{forEach tMb (displayLine tMb)}
  <div>
    !{return $ textBox modifyTextAddr [] Nothing}
|]

displayLine :: STMMailbox (S.Seq Text) -> Text -> ((Maybe Text) -> Message ()) -> LiveVDom VDA.JSEvent
displayLine tMb t updateT = [gertrude|
<span>
  #{return . unpack $ t}
|]
