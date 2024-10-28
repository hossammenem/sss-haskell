module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll, send)

import qualified Data.ByteString as S
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

import Control.Concurrent (forkFinally)
import Control.Monad (forever, unless, void)
import Control.Exception

import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8(pack,unpack)


main :: IO ()
main = putStrLn "..."

setupServer :: Maybe String -> [Char] -> IO ()
setupServer mhostN port = runTCPServer mhostN port loop 
    where
        loop s = do
            print "Start of loop"
            
            -- Alert part - send message
            _ <- sendAll s $ pack "Hi!!"
            print $ "got new socket " ++ show s
            
            -- Talk part - receive message
            msg <- recv s 1024
            unless (S.null msg) $ do
                print $ "Received: " ++ show msg
                -- Echo back the received message
                sendAll s msg
            
            print "End of loop"
            loop s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do -- this withSocketDo is for windows, cuz windows is gay
    addr <- resolve
    E.bracket (open addr) close loop
		where
			resolve = do
				let hints = defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Stream }
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


-- TODO:
-- send msgs with input typeshit

connectClient :: Maybe String -> [Char] -> IO ()
connectClient Nothing _ = error "???"
connectClient mhostName port = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol

    let hints = defaultHints { addrSocketType = Stream }
    let hostname = fromMaybe "localhost" mhostName
    let portNum = fromIntegral (read port :: Int) :: PortNumber

    addrInfos <- getAddrInfo (Just hints) mhostName (Just port)

    case addrInfos of
			(addr:_) -> do
				-- Create socket
				sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
				putStrLn $ "Created socket: " ++ show sock
				putStrLn $ "Address: " ++ show (addrAddress addr)

				-- Try to connect
				connect sock (addrAddress addr)

				forever $ loop
				where
					loop = do
						msg <- recv sock 1024
						unless (S.null msg) $ do
							putStrLn $ "Received from server: " ++ show msg

				-- close sock
			[] -> putStrLn "Could not resolve address"


-- handleClient :: Socket -> IO ()
-- handleClient conn = do
--     msg <- recv conn 1024
--     putStrLn $ "Received: " ++ show msg
-- 
--     sendAll conn "Hello, Client!"







--- helper funcs
hostnameToWord32 :: Maybe String -> IO Word32
hostnameToWord32 mhost = do
    let hostname = fromMaybe "localhost" mhost
    -- Get address info
    addrInfos <- getAddrInfo 
                    (Just defaultHints { addrSocketType = Stream }) 
                    (Just hostname) 
                    Nothing
    -- Take the first address
    case addrInfos of
        (addrInfo:_) -> case addrAddress addrInfo of
            SockAddrInet _ hostAddr -> return hostAddr
            _ -> return 0  -- fallback for non-IPv4 addresses
        [] -> return 0    -- fallback if no address found
