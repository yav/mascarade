{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import           Snap.Http.Server ( quickHttpServe )
import           Snap.Core ( Snap, setContentType, getParam )
import qualified Snap.Core as Snap
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap ( runWebSocketsSnap )
import           Control.Monad ( forM_, unless )
import           Control.Monad.IO.Class (liftIO)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Protocol
import           Data.Text ( Text )
import qualified Data.Text.Encoding as Text

import           Control.Concurrent.STM
import qualified Control.Exception as X


type ServerState = Map Text Room

data Room = Room
  { roomName       :: !Text
  , roomNextPlayer :: !PlayerId
  , roomPlayers    :: ![ (PlayerId, Conn, Bool) ]
  }




main :: IO ()
main =
  do s <- atomically (newTVar Map.empty)
     quickHttpServe $ Snap.route
       [ ("index.html",  sendHtml "index.html")
       , ("jquery.js",   sendJS   "jquery.js")
       , ("client.js",   sendJS   "client.js")
       , ("newConn",     newClient s)
 --      , ("test",        doTest s)
       ]
{-
doTest :: ServerState -> Snap ()
doTest s =
  liftIO $
  do cs <- atomically (readTVar s)
     forM_ cs $ \c -> send c $ ChoosePlayer [3]
-}


-- | Start a new client.
newClient :: TVar ServerState -> Snap ()
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


           updateRoom (Just Room { .. }) =
             Just Room { roomPlayers = (roomNextPlayer, pConn, False)
                                                                  : roomPlayers
                       , roomNextPlayer = 1 + roomNextPlayer
                       , .. }

           updateRoom Nothing =
             Just Room { roomNextPlayer = 1
                       , roomPlayers    = [ (1, pConn, False) ]
                       , roomName       = room
                       }

       atomically $ modifyTVar' s $ Map.alter updateRoom room

       atomically $ do isDone <- readTVar done
                       unless isDone retry


announceRoomUpdate :: Room -> IO ()
announceRoomUpdate Room { .. } =
  forM_ roomPlayers $ \(pid,conn) ->
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


