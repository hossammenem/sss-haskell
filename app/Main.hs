module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import qualified Data.ByteString as S
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

import Control.Concurrent (forkFinally)

import Control.Monad (forever, unless, void)
import Control.Exception


main :: IO ()
main = putStrLn "fuckoff"

setupServer :: Maybe String -> [Char] -> IO ()
setupServer mhostN port = runTCPServer mhostN port talk
	where
		talk s = do
			msg <- recv s 1024
			unless (S.null msg) $ do
				sendAll s msg
				talk s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do -- this withSocketDo is for windows, cuz windows is gay
    addr <- resolve
    E.bracket (open addr) close loop
		where
			resolve = do
				let hints = defaultHints {
					addrFlags = [AI_PASSIVE]
						, addrSocketType = Stream
				}
				NE.head <$> getAddrInfo (Just hints) mhost (Just port)
			open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
				setSocketOption sock ReuseAddr 1
				withFdSocket sock setCloseOnExecIfNeeded
				bind sock $ addrAddress addr
				listen sock 1024
				return sock
			loop sock = forever $ E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> void $
				-- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
				-- but 'E.bracketOnError' above will be necessary if some
				-- non-atomic setups (e.g. spawning a subprocess to handle
				-- @conn@) before proper cleanup of @conn@ is your case
				forkFinally (server conn) (const $ gracefulClose conn 5000)


connectToClient :: Int -> IO ()
connectToClient _ = error "not implemented"

-- handleClient :: Socket -> IO ()
-- handleClient conn = do
--     msg <- recv conn 1024
--     putStrLn $ "Received: " ++ show msg
-- 
--     sendAll conn "Hello, Client!"
