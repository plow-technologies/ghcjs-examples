{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Table where

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


displayTable :: STMMailbox (S.Seq TableRow) -> LiveVDom JSEvent
displayTable pMb = [valentine|
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

displayRow :: STMMailbox (S.Seq TableRow) -> TableRow -> ((Maybe TableRow) -> Message ()) -> LiveVDom JSEvent
displayRow tMb t updateT = [valentine|
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



