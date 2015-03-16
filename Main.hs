{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forM_, forever)
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

meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", meow"

serverApp :: MVar ServerState -> WS.ServerApp
serverApp state pendingConn = do
    conn <- WS.acceptRequest pendingConn
    meow conn

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 8080 $ serverApp state
