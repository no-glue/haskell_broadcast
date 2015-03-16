{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forever)
import Control.Concurrent
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", meow"

serverApp :: WS.PendingConnection -> IO ()
serverApp pendingConn = do
    conn <- WS.acceptRequest pendingConn
    meow conn

main :: IO ()
main = WS.runServer "127.0.0.1" 8080 serverApp
