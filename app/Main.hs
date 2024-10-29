{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll, send)

import Database.SQLite.Simple as SS
import Database.SQLite.Simple.FromRow
import Data.Time.Clock (UTCTime, getCurrentTime)

import qualified Data.ByteString as S
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

import Control.Concurrent (forkFinally)
import Control.Monad (forever, unless, void)

import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Data.ByteString.Char8(pack, unpack)


main :: IO ()
main = putStrLn "..."

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

testDB :: IO ()
testDB = do
 conn <- open "test.db"
 -- execute_ conn "CREATE TABLE test (id INTEGER PRIMARY KEY, str text);"
 putStrLn "printing table test..."

 r <- query_ conn "SELECT * from test" :: IO [TestField]
 mapM_ print r

 putStrLn "printing table test done, closing..."

 SS.close conn


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
    E.bracket (open addr) NS.close loop
		where
			resolve = do
				let hints = defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Stream }
				NE.head <$> getAddrInfo (Just hints) mhost (Just port)

			open addr = E.bracketOnError (openSocket addr) NS.close $ \sock -> do
				setSocketOption sock ReuseAddr 1
				withFdSocket sock setCloseOnExecIfNeeded
				NS.bind sock $ addrAddress addr
				listen sock 1024
				return sock

			loop sock = forever $ E.bracketOnError (accept sock) (NS.close . fst) $ \(conn, _peer) -> void $
				-- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
				-- but 'E.bracketOnError' above will be necessary if some
				-- non-atomic setups (e.g. spawning a subprocess to handle
				-- @conn@) before proper cleanup of @conn@ is your case
				forkFinally (server conn) (const $ gracefulClose conn 5000)


-- take input from user, keep taking the input from the user
-- send this message, sendAll typeshit
chat :: Socket -> IO ()
chat s = do
	putStr "Enter Message: "
	msg <- getLine
	sendAll s $ pack msg


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

				forever $ loop sock
				where
					loop sock = do
						msg <- recv sock 1024
						unless (S.null msg) $ do
							putStrLn $ "Received from server: " ++ show msg
						chat sock


				-- close sock
			[] -> putStrLn "Could not resolve address"


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
