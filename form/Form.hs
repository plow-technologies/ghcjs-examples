{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Form where

import           Control.Applicative

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

runFormDefault :: IO ()
runFormDefault = do
  container <- createContainer
  runForm container

-- multiple entries in form
-- if required entries not filled then notify user
-- otherwise submit form
-- clear all form items

data ProfileConfig = ProfileConfig {
  profileFirstName    :: String
, profileLastName     :: String
, profileEmail        :: String
, profileHomeAddress  :: String
} deriving (Eq, Show, Ord)

runForm :: DOMNode -> IO ()
runForm container = do
  -- spawn event info
  (profileEnv  , profileAddr  ) <- spawnIO $ (Unfired :: Event (String,String))
  p <- spawnIO $ S.singleton $ ProfileConfig "" "" "" ""
  
  forkModifyT profileEnv p

  runDomI container (return ()) $ return (displayForm profileAddr p)

forkModifyT :: STMEnvelope (Event (String,String)) -> STMMailbox (S.Seq ProfileConfig) -> IO ()
forkModifyT textBoxEnv (profileEnv, addr) = void $ forkOnChange textBoxEnv $ \_ -> do
  eTextboxMessage <- recvIO textBoxEnv
  case eTextboxMessage of
    Unfired -> return ()
    Fired textboxMessage -> do
      sCurrentProfile <- recvIO profileEnv
      let currentProfile = S.index sCurrentProfile 0
      let tbId = fst textboxMessage
      let uv = snd textboxMessage
      let newProfile = updateProfile tbId uv currentProfile
      print newProfile
      sendIO addr $ S.singleton newProfile

updateProfile :: String -> String -> ProfileConfig -> ProfileConfig
updateProfile "firstname"    uv pc = ProfileConfig uv (profileLastName pc) (profileEmail pc) (profileHomeAddress pc)
updateProfile "lastname"     uv pc = ProfileConfig (profileFirstName pc) uv (profileEmail pc) (profileHomeAddress pc)
updateProfile "emailaddress" uv pc = ProfileConfig (profileFirstName pc) (profileLastName pc) uv (profileHomeAddress pc)
updateProfile "homeaddress"  uv pc = ProfileConfig (profileFirstName pc) (profileLastName pc) (profileEmail pc) uv
updateProfile _              _  pc = pc

displayForm :: Address (Event (String, String)) -> STMMailbox (S.Seq ProfileConfig) -> LiveVDom VDA.JSEvent
displayForm profileAddr pMb = [gertrude|
<div>
  <div>
    First Name:
    <span>
      !{return $ textBoxWithId "firstname" profileAddr [] Nothing}
  <div>
    Last Name:
    <span>
      !{return $ textBoxWithId "lastname" profileAddr [] Nothing}
  <div>
    Email Address:
    <span>
      !{return $ textBoxWithId "emailaddress" profileAddr [] Nothing}
  <div>
    Home Address: 
    <span>
      !{return $ textBoxWithId "homeaddress" profileAddr [] Nothing}
|]

textBoxWithId :: String -> Address (Event (String,String)) -> [Property] -> Maybe String -> LiveVDom JSEvent
textBoxWithId i addr = textBoxWith (\str -> sendMessage addr $ Fired (i,str))


{-
Not sure how to make delete button set ProfileConfig to clear and then update textBoxes in displayForm

displayControls :: Address (Event ()) -> Address (Event ()) -> LiveVDOM VDA.JSEvent
displayControls a b = [gertrude|
<div>
  !{return $ button ? [] "Sign up"}
  !{return $ button ? [] "Clear All"}
|]

-}
