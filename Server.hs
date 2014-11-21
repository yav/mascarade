{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import           Snap.Http.Server ( quickHttpServe )
import           Snap.Core ( Snap, setContentType, getParam )
import qualified Snap.Core as Snap
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap ( runWebSocketsSnap )
import           Control.Monad ( forM_, unless )
import           Data.Map (Map)
import qualified Data.Map as Map

import           Protocol
import           Data.Text ( Text )
import qualified Data.Text.Encoding as Text

import           Control.Concurrent.STM
import           Control.Concurrent(MVar, newMVar, modifyMVar_)
import qualified Control.Exception as X


type ServerState = Map Text Room

data Room = Room
  { roomName       :: !Text
  , roomNextPlayer :: !PlayerId
  , roomPlayers    :: ![ (PlayerId, Conn, Bool) ]
  }

emptyRoom :: Text -> Room
emptyRoom roomName = Room { roomNextPlayer = 0
                          , roomPlayers    = []
                          , .. }




main :: IO ()
main =
  do s <- newMVar Map.empty
     quickHttpServe $ Snap.route
       [ ("/",  sendHtml "index.html")
       , ("jquery.js",   sendJS   "jquery.js")
       , ("client.js",   sendJS   "client.js")
       , ("newConn",     newClient s)
 --      , ("test",        doTest s)
       ]


-- | Start a new client, and insert it into one of the rooms.
newClient :: MVar ServerState -> Snap ()
newClient s =
  do mb <- getParam "room"
     let room = case mb of
                  Just p  -> Text.decodeUtf8 p
                  Nothing -> "default"
     runWebSocketsSnap (client room)
  where
  client room pending =
    do conn <- WS.acceptRequest pending
       done <- atomically (newTVar False)

       let pConn = Conn
            { stop    = atomically (writeTVar done True)
            , stopped = atomically (readTVar done)
            , send    = \x -> WS.sendTextData conn (toClient x)
                                  `X.catch` \X.SomeException {} -> stop pConn
            , recv = do lbs <- WS.receiveData conn
                        case fromClient lbs of
                          Just msg -> return msg
                          Nothing  -> return ISentGarbage
                      `X.catch` \X.SomeException {} ->
                                   do stop pConn
                                      return IDisconnected
            }

       modifyMVar_ s $ \roomMap ->
          do let Room { .. } = Map.findWithDefault (emptyRoom room) room roomMap
                 r = Room { roomPlayers    = (roomNextPlayer, pConn, False)
                                           : roomPlayers
                          , roomNextPlayer = 1 + roomNextPlayer
                          , roomName       = room
                          }
             announceRoomUpdate r
             return $! Map.insert room r roomMap




       atomically $ do isDone <- readTVar done
                       unless isDone retry


-- | Send the current state of a room to all participants.
announceRoomUpdate :: Room -> IO ()
announceRoomUpdate Room { .. } =
  forM_ roomPlayers $ \(pid,conn,_) ->
    send conn $ RoomUpdate
              $ RoomInfo { riName    = roomName
                         , riYouAre  = pid
                         , riPlayers = [ (p,r) | (p,_,r) <- roomPlayers ]
                         }


-- | Send a static HTML file.
sendHtml :: FilePath -> Snap ()
sendHtml file = do Snap.modifyResponse (setContentType "text/html")
                   Snap.sendFile file

-- | Send a static Javascript file.
sendJS :: FilePath -> Snap ()
sendJS file = do Snap.modifyResponse (setContentType "application/javascript")
                 Snap.sendFile file


