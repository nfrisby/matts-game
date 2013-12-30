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

import Network.Wai (remoteHost)
import Crypto.MAC.SipHash (SipKey(..),SipHash(..),hash)
import System.CPUTime (getCPUTime)

import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Control.Concurrent (MVar,newMVar,modifyMVar_,readMVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Control.Exception (fromException)
import Control.Monad (when,liftM)

import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsIntercept)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS



type Nickname = Text

data ClientMessage = ChangeNick Nickname | Say Text | NewRoom | JoinRoom Word64 deriving Generic
instance FromJSON ClientMessage
instance ToJSON ClientMessage
clientCtors = [julius|
function ChangeNick(nickname) {
  return {tag:'ChangeNick',contents:[nickname]}
}
function Say(text) {
  return {tag:'Say',contents:[text]}
}
var NewRoom = {tag: 'NewRoom',contents:[]};
function JoinRoom(tag) {
  return {tag:'JoinRoom',contents:[tag]}
}
|]

data ServerMessage = JoinedRoom Word64 [Nickname] | SayHiTo Nickname | WasSaid Text | ServerError Text deriving Generic
instance FromJSON ServerMessage
instance ToJSON ServerMessage
serverScott = [julius|
function serverMessage(message,joinedRoom,sayHiTo,wasSaid,serverError) {
  var dispatch = {};
  dispatch['JoinedRoom'] = joinedRoom;
  dispatch['SayHiTo'] = sayHiTo;
  dispatch['WasSaid'] = wasSaid;
  dispatch['ServerError'] = serverError;
  var tag = message.tag;
  var contents = message.contents;
  delete message.tag;
  delete message.contents; // now message just has the record fields, if any were present
  return dispatch[tag].apply(message,contents)
}
|]

data Client = Client (IORef Text) (WS.Sink WS.Hybi00)
type AppState = MVar (Map Word64 (MVar [Client]))
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
var wsPort = #{toJSON port}
|]
  toWidget [whamlet|
$newline never
<textarea #box>
<ol #messages>
|]

wsHandler :: AppState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsHandler rooms req = do
  WS.acceptRequest req
  sink <- WS.getSink

  now <- liftIO getCPUTime
  ip <- return "ip" -- fmap (show . remoteHost . reqWaiRequest) getRequest
  let SipHash tag = hash (SipKey 0 0) $ BC.pack $ show now ++ ip

  nameRef <- liftIO $ newIORef ""
  room <- liftIO $ newMVar [Client nameRef sink]
  liftIO $ modifyMVar_ rooms $ return . Map.insert tag room

  let tshow :: Show a => a -> T.Text
      tshow = T.pack . show

  WS.sendTextData $ T.append "MSG Joined room " $ tshow tag

  let loop name = do
        text <- WS.receiveData
        case text of
          _ | Just name <- T.stripPrefix "/nick " text -> do
                liftIO $ writeIORef nameRef name
                loop name
          _ | "/" `T.isPrefixOf` text -> do
                WS.sendTextData ("MSG Unknown command." :: T.Text)
                loop name
          _ | otherwise -> do
                let (tag,msg) = T.break (==' ') text
                (liftIO (readMVar room) >>=) $ mapM_ $ \(Client _ sink') ->
                  when (sink /= sink') $ WS.sendTextData $ T.concat ["SAID ",name,": ",msg]
                loop name

  WS.catchWsError (loop "") $ \e -> case fromException e of
     Just x -> case x of
       WS.ConnectionClosed -> return ()
       _ -> WS.sendTextData $ T.append "FATAL " $ tshow x
     Nothing -> WS.throwWsError e

main :: IO ()
main = do
  mvar <- newMVar Map.empty
  (port,waiApp) <- defaultDevelApp loadDevelopmentConfig $ \cfg -> toWaiAppPlain $ App mvar (appPort cfg)
  runSettings defaultSettings
    { settingsPort = port
    , settingsIntercept = WaiWS.intercept (wsHandler mvar)
    } waiApp
