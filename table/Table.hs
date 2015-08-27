{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Table where

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

runTableDefault :: IO ()
runTableDefault = do
  container <- createContainer
  runTable container

data TableRow = TableRow {
  tableRowUserId   :: String
, tableRowUserName :: String
, tableRowEmail    :: String
, tableRowGroup    :: String
} deriving (Show,Eq,Ord)

runTable :: DOMNode -> IO ()
runTable container = do
  -- spawn event info
  (profileEnv  , profileAddr  ) <- spawnIO $ (Unfired :: Event (String,String))
  p <- spawnIO $ S.fromList $ [(TableRow "1" "Saint IGNUcius" "saint@gnu.com" "GNU Linux"),(TableRow "2" "Donald Trump" "donald@trump.com" "Trump Enterprises")]
  
  -- forkModifyT profileEnv p

  runDomI container (return ()) $ return (displayTable p)

-- Address (Event (String, String)) -> 

displayTable :: STMMailbox (S.Seq TableRow) -> LiveVDom VDA.JSEvent
displayTable pMb = [gertrude|
<table>
  <tr>
    <th>
      User ID  
    <th>
      User Name
    <th> 
      Email
    <th>
      Group
  &{forEach pMb (displayRow pMb)}
|]

displayRow :: STMMailbox (S.Seq TableRow) -> TableRow -> ((Maybe TableRow) -> Message ()) -> LiveVDom VDA.JSEvent
displayRow tMb t updateT = [gertrude|
<tr>
  <td>
    #{return . tableRowUserId $ t}
  <td>
    #{return . tableRowUserName $ t}
  <td>
    #{return . tableRowEmail $ t}
  <td>
    #{return . tableRowGroup $ t}
|]


{-
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

displayRow :: LiveVDom VDA.JSEvent
displayRow p p = [gertrude|
<tr>
  <td>
    tableRowUserId
  <td>
    tableRowUserName
  <td>
    tableRowEmail
  <td>
    tableRowGroup
|]

 <table style="width:100%">
  <tr>
    <th>Firstname</th>
    <th>Lastname</th>
    <th>Points</th>
  </tr>
  <tr>
    <td>Eve</td>
    <td>Jackson</td>
    <td>94</td>
  </tr>
</table> 
-}


