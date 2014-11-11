{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import           Snap.Http.Server ( quickHttpServe )
import           Snap.Core ( Snap, setContentType )
import qualified Snap.Core as Snap
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap ( runWebSocketsSnap )
import           Control.Monad ( forM_, unless )
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JS

import           Protocol
import           Data.Text ( Text )

import           Control.Concurrent.STM
import qualified Control.Exception as X

type ServerState = TVar [Conn]

data Room = Room
  { roomName       :: !Text
  , roomNextPlayer :: !PlayerId
  , roomPlayers    :: ![ (PlayerId, Conn) ]
  }




main :: IO ()
main =
  do s <- newServerState
     quickHttpServe $ Snap.route
       [ ("index.html",  sendHtml "index.html")
       , ("jquery.js",   sendJS   "jquery.js")
       , ("client.js",   sendJS   "client.js")
       , ("newConn",     newClient s)
       , ("test",        doTest s)
       ]

doTest :: ServerState -> Snap ()
doTest s =
  liftIO $
  do cs <- atomically (readTVar s)
     forM_ cs $ \c -> send c $ ChoosePlayer [3]

newServerState :: IO ServerState
newServerState = atomically (newTVar [])


-- | Start a new client.
newClient :: ServerState -> Snap ()
newClient s = runWebSocketsSnap client
  where
  client pending =
    do conn <- WS.acceptRequest pending
       done <- atomically (newTVar False)

       let stop   = atomically (writeTVar done True)

           stopped = atomically (readTVar done)

           send x = WS.sendTextData conn (toClient x)
                      `X.catch` \X.SomeException {} -> stop

           recv = do lbs <- WS.receiveData conn
                     case fromClient lbs of
                       Just msg -> return msg
                       Nothing  -> return ISentGarbage
                   `X.catch` \X.SomeException {} ->
                                do stop
                                   return IDisconnected

       atomically (modifyTVar s (Conn { .. } :))
       atomically $ do isDone <- readTVar done
                       unless isDone retry


-- | Send a static HTML file.
sendHtml :: FilePath -> Snap ()
sendHtml file = do Snap.modifyResponse (setContentType "text/html")
                   Snap.sendFile file

-- | Send a static Javascript file.
sendJS :: FilePath -> Snap ()
sendJS file = do Snap.modifyResponse (setContentType "application/javascript")
                 Snap.sendFile file


