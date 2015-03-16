{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

connections :: [WS.Connection]
connections = []

addConnection :: WS.Connection -> [WS.Connection]
addConnection conn = conn : connections

meow :: WS.Connection -> [WS.Connection] -> IO ()
meow conn conns = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData (head conns) $ msg `T.append` ", meow"

serverApp :: WS.PendingConnection -> IO ()
serverApp pendingConn = do
    conn <- WS.acceptRequest pendingConn
    meow conn (addConnection conn)

main :: IO ()
main = WS.runServer "127.0.0.1" 8080 serverApp
