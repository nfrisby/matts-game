{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import GHC.Generics

import Yesod
import Yesod.Core.Dispatch (toWaiAppPlain)
import Yesod.Default.Main (defaultDevelApp)
import Yesod.Default.Config (loadDevelopmentConfig,appPort)

import qualified Data.ByteString as BL
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as JS

import Network.Wai (remoteHost)
import Crypto.MAC.SipHash (SipKey(..),SipHash(..),hash)
import System.CPUTime (getCPUTime)

import Data.IORef (IORef,newIORef,readIORef,writeIORef,modifyIORef)
import Control.Concurrent (MVar,newMVar,takeMVar,putMVar,modifyMVar_,readMVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Control.Exception (fromException)
import Control.Monad (when,liftM,forM_,forever,(>=>))

import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsIntercept)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS



type Nickname = Text
type Room = Text

data ClientMessage = ChangeNick Nickname | Say Room Text | NewRoom | JoinRoom Room | LeaveRoom Room deriving Generic
instance FromJSON ClientMessage
instance ToJSON ClientMessage
clientCtors = [julius|
function ChangeNick(nickname) {
  return {tag:'ChangeNick',contents:[nickname]}
}
function Say(room,text) {
  return {tag:'Say',contents:[room,text]}
}
var NewRoom = {tag: 'NewRoom',contents:[]};
function JoinRoom(room) {
  return {tag:'JoinRoom',contents:[room]}
}
function LeaveRoom(room) {
  return {tag:'LeaveRoom',contents:[room]}
}
|]

data ServerMessage = JoinedRoom Room [Nickname] | Door Room Bool Nickname | WasSaid Room Nickname Text | ServerError Text deriving Generic
instance FromJSON ServerMessage
instance ToJSON ServerMessage

data Client = Client { clientName :: IORef Nickname , clientSink :: WS.Sink WS.Hybi00 }
type AppState = MVar (Map Room (MVar [Client]))
data App = App { _rooms :: AppState , _port :: Int }
instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
/js JSR GET
|]

getJSR :: Handler Html
getJSR = do
  contents <- liftIO $ BL.readFile $ "js/chat.js"
  sendResponse (T.encodeUtf8 "text/javascript",toContent contents)

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  port <- _port `liftM` getYesod

  setTitle "Title"
  addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js"
  addScript JSR

  toWidget [julius|
var wsPort = #{toJSON port};
^{clientCtors}
|]
  toWidget [whamlet|
$newline never
<textarea #box>
<div #messages>
  <ul .messages>
  <ul .others>
|]

tshow :: Show a => a -> T.Text
tshow = T.pack . show

serverMessage msg = WS.sendTextData $ JS.encode msg
serverError text = serverMessage $ ServerError text

wsHandler :: AppState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsHandler rooms req = do
  WS.acceptRequest req
  sink <- WS.getSink

  let withRoom room kN kJ = do
        room <- liftIO $ Map.lookup room `liftM` readMVar rooms
        case room of
          Nothing -> kN
          Just room -> kJ room
      broadcast msg clients =
        forM_ clients $ \(Client _ sink') ->
          when (sink /= sink') $ serverMessage msg

  let firstLoop = do
        bs <- WS.receiveData
        case JS.decode bs of
          Just (ChangeNick nick) -> liftIO $ newIORef nick
          _ -> (>> firstLoop) $ serverError "Ignoring messages except for ChangeNick"
  nameRef <- firstLoop
  roomsRef <- liftIO $ newIORef []

  let loop = WS.receiveData >>= \bs -> case JS.decode bs of
        Nothing -> serverError "Could not parse command"
        Just msg -> handle msg
      handle msg = case msg of
        ChangeNick name -> liftIO $ writeIORef nameRef name
        Say room text -> do
          name <- liftIO $ readIORef nameRef
          withRoom room (return ()) $ liftIO . readMVar >=> broadcast (WasSaid room name text)
        NewRoom -> do
          now <- liftIO getCPUTime
          ip <- return "ip" -- how to determine client IP?
          let SipHash tag = hash (SipKey 0 0) $ BC.pack $ show now ++ ip
          newRoom (tshow tag)
        JoinRoom room -> do
          name <- liftIO $ readIORef nameRef
          withRoom room (newRoom room) $ \mvar -> do
            clients <- liftIO $ takeMVar mvar
            liftIO $ putMVar mvar $ (Client nameRef sink):clients
            liftIO (mapM (readIORef . clientName) clients) >>= serverMessage . JoinedRoom room
            liftIO $ modifyIORef roomsRef $ ((room,mvar):)
            broadcast (Door room True name) clients
        LeaveRoom room -> do
          name <- liftIO $ readIORef nameRef
          withRoom room (return ()) $ leaveRoom name room
          liftIO $ modifyIORef roomsRef $ filter ((/=room) . fst)

      newRoom room = do
        roomV <- liftIO $ newMVar [Client nameRef sink]
        liftIO $ modifyMVar_ rooms $ return . Map.insert room roomV
        liftIO $ modifyIORef roomsRef $ ((room,roomV):)
        serverMessage $ JoinedRoom room []
      leaveRoom name room roomV = do
        clients <- liftIO $ filter ((/=sink) . clientSink) `liftM` takeMVar roomV
        liftIO $ putMVar roomV clients
        broadcast (Door room False name) clients

  WS.catchWsError (forever loop) $ \e -> case fromException e of
     Just x -> case x of
       WS.ConnectionClosed -> do
         name <- liftIO $ readIORef nameRef
         liftIO (readIORef roomsRef) >>= mapM_ (uncurry (leaveRoom name))
       _ -> serverError $ T.append "FATAL " $ tshow x
     Nothing -> WS.throwWsError e

main :: IO ()
main = do
  mvar <- newMVar Map.empty
  (port,waiApp) <- defaultDevelApp loadDevelopmentConfig $ \cfg -> toWaiAppPlain $ App mvar (appPort cfg)
  runSettings defaultSettings
    { settingsPort = port
    , settingsIntercept = WaiWS.intercept (wsHandler mvar)
    } waiApp
