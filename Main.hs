{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, connection) -> WS.sendTextData connection message
  -- loop through clients, $ - current client

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state client = forever $ do
    msg <- WS.receiveData conn
    liftIO $ readMVar state >>= broadcast
      (msg)

serverApp :: MVar ServerState -> WS.ServerApp
serverApp state pendingConn = do
    conn <- WS.acceptRequest pendingConn
    let client = ("", conn)
    liftIO $ modifyMVar_ state $ \s -> do 
      let s1 = addClient client s
      -- s is state; first parameter to modifyMVar_
      -- s1 is new state
      return s1
    talk conn state client

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 8080 $ serverApp state
  -- current $ pendingConn
