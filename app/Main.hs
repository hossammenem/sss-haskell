{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll, send)

import Database.SQLite.Simple as SS
import Database.SQLite.Simple.FromRow

import qualified Data.ByteString as S
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

import Control.Concurrent (forkFinally)
import Control.Monad (forever, unless, void)

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)

main :: IO ()
main = do
    putStrLn "Enter a command (init_server, connect_client, create_room, join_room):"
    opt <- getLine 
    case opt of
        "init_server" -> setupServer (Just "localhost") "8000"
        "connect_client" -> connectClient (Just "localhost") "8000" 
        "create_room" -> createRoom
        "join_room" -> getRoom
        _ -> putStrLn "Invalid options"
    where 
        getRoom = do 
            putStrLn "\nRoom name: "
            roomName <- getLine
            joinRoom -- roomName

setupServer :: Maybe String -> [Char] -> IO ()
setupServer mhostN port = runTCPServer mhostN port loop 
	where
		loop s = do
			putStrLn "\n================="
			putStrLn "Start of a loop"
			putStrLn "=================\n"

			-- basically what i have in mind is that 
			-- a socket joins
			-- we listen for that
			-- something finally joins!
			-- send it to all
			(conn) <- accept s

			unless(null conn) $ do
				print $ "Handling new socket " ++ show conn
				sendAll s $ pack "Hi!"

			-- when recievce, echo message back
			msg <- recv s 1024
			unless (S.null msg) $ do
					print $ "Received: " ++ (show . unpack) msg
					sendAll s msg
			
			putStrLn "\n================="
			putStrLn "end of the loop"
			putStrLn "=================\n"

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

connectClient :: Maybe String -> [Char] -> IO ()
connectClient mhostName port = withSocketsDo $ do
    let hints = defaultHints { addrSocketType = Stream }

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
								chat sock

								msg <- recv sock 1024
								unless (S.null msg) $ do 
									putStrLn $ "Received from server: " ++ show msg

					-- close sock
        [] -> putStrLn "Could not resolve address"

-- take input from user, keep taking the input from the user
-- send this message, sendAll typeshit
chat :: Socket -> IO ()
chat s = do
    putStr "Enter Message: "
    msg <- getLine
    sendAll s $ pack msg

createRoom :: IO ()
createRoom = error "not implemented"

joinRoom :: IO ()
joinRoom = error "not implemented"

-------------------------
-- DB stuff
-------------------------
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
