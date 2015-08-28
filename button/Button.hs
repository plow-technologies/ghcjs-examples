{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Button where

import           Prelude                        hiding (div, sequence)

import           Control.Monad                  (void)
import           Data.Maybe
import           Data.Text                      (Text, concat, pack, unpack)
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


runButtonDefault :: IO ()
runButtonDefault = do
  container <- createContainer
  runConfiguration container

runConfiguration :: DOMNode -> IO ()
runConfiguration container = do
  -- spawn event info
  (sbmtEnv  , sbmtAddr  ) <- spawnIO $ Unfired
  (deleteEnv, deleteAddr) <- spawnIO $ Unfired
  
  textLines <- spawnIO $ S.empty
  
  forkSubmitButton sbmtEnv   textLines
  forkDeleteButton deleteEnv textLines

  runDomI container (return ()) $ return (displayButton sbmtAddr deleteAddr textLines)

forkSubmitButton :: STMEnvelope (Event ()) -> STMMailbox (S.Seq Text) -> IO ()
forkSubmitButton buttonEnv (env,addr) = void $ forkOnChange buttonEnv $ \_ -> do
  previousLines <- recvIO env
  let ordinal = show . (+1) . S.length $ previousLines
  sendIO addr $ previousLines S.|> (Data.Text.concat [(pack ordinal),". new line of text"])

forkDeleteButton :: STMEnvelope (Event ()) -> STMMailbox (S.Seq Text) -> IO ()
forkDeleteButton buttonEnv (_,addr) = void $ forkOnChange buttonEnv $ \_ -> do
  sendIO addr $ S.empty

displayButton ::  Address (Event ()) -> Address (Event ()) -> STMMailbox (S.Seq Text) -> LiveVDom JSEvent
displayButton sbmtBttnAddr deleteAddr ls = [valentine|
<div>
  <h2>
    Button Test
  <div>
    &{forEach ls (displayLine ls)}
  <div>
    !{return $ button sbmtBttnAddr [] "Add a Line"}
    !{return $ button deleteAddr   [] "Delete All"}
|]

displayLine :: STMMailbox (S.Seq Text) -> Text -> ((Maybe Text) -> Message ()) -> LiveVDom JSEvent
displayLine _ t _ = [valentine|
<div>
  #{return . unpack $ t}
|]