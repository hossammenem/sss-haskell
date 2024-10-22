{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as S
import Control.Monad (forever)
import Control.Exception (finally)


-- what's unless?
-- do notation? and <-
-- bind the return of a monadic action to a variable
-- $ ???
-- what's () after IO

setupServer :: Int -> IO ()
setupServer = runTcpServer Nothing 3000 talk
	where
		talk s = do
			msg <- recv s 1024
			unless (S.null msg) $ do
				sendAll s msg
				talk s


connectToClient :: Int -> IO ()
connectToClient = Error "not implemented"

handleClient :: Socket -> IO ()
handleClient conn = do
    msg <- recv conn 1024
    putStrLn $ "Received: " ++ show msg

    sendAll conn "Hello, Client!"
