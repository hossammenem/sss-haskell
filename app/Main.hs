{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll, send)

import Database.SQLite.Simple as SS
import Database.SQLite.Simple.FromRow

import qualified Data.ByteString as S
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

import Control.Concurrent (forkFinally, forkIO)
import Control.Monad (forever, unless, void)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)

import System.IO (hFlush, stdout)


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
            joinRoom

setupServer :: Maybe String -> [Char] -> IO ()
setupServer mhostN port = runTCPServer mhostN port loop 
    where
        loop s = forever $ do
            putStrLn "\n================="
            putStrLn "Waiting for connection..."
            putStrLn "=================\n"
            
            (clientSock, clientAddr) <- accept s
            -- Handle each client in a separate thread, so now the accept function doesn't block for all clients
            void $ forkIO $ handleClient clientSock clientAddr
            
handleClient :: Socket -> SockAddr -> IO ()
handleClient sock addr = do
    putStrLn $ "New client connected: " ++ show addr
    sendAll sock $ pack "Hi!"
    
    forever $ do
        msg <- recv sock 1024
        unless (S.null msg) $ do
            print $ "Received: " ++ (show . unpack) msg
            sendAll sock msg
    `E.catch` \(e :: E.IOException) -> do
        putStrLn $ "Client disconnected: " ++ show addr
        NS.close sock

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
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

        loop sock = server sock

connectClient :: Maybe String -> [Char] -> IO ()
connectClient mhostName port = withSocketsDo $ do
    let hints = defaultHints { addrSocketType = Stream }
    addrInfos <- getAddrInfo (Just hints) mhostName (Just port)
    case addrInfos of
        (addr:_) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            putStrLn $ "Created socket: " ++ show sock
            putStrLn $ "Address: " ++ show (addrAddress addr)
            connect sock (addrAddress addr)
            
            -- Create MVar to coordinate output
            outputLock <- newMVar ()
            
            -- Handle receiving in a separate thread
            void $ forkIO $ forever $ do
                msg <- recv sock 1024
                unless (S.null msg) $ do 
                    withMVar outputLock $ \_ -> do
                        clearLine
                        putStrLn $ "Received: " ++ show (unpack msg)
                        putStr "Enter Message: "
                        hFlush stdout
            
            -- Handle sending in main thread
            forever $ chatWithLock outputLock sock
            
        [] -> putStrLn "Could not resolve address"

chatWithLock :: MVar () -> Socket -> IO ()
chatWithLock lock s = do
    withMVar lock $ \_ -> do
        putStr "Enter Message: "
        hFlush stdout
    msg <- getLine
    sendAll s $ pack msg

clearLine :: IO ()
clearLine = do
    putStr "\r"           -- Return to start of line
    putStr (replicate 80 ' ')  -- Clear with spaces
    putStr "\r"           -- Return to start again

createRoom :: IO ()
createRoom = error "not implemented"

joinRoom :: IO ()
joinRoom = error "not implemented"

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

testDB :: IO ()
testDB = do
    conn <- open "test.db"
    putStrLn "printing table test..."
    r <- query_ conn "SELECT * from test" :: IO [TestField]
    mapM_ print r
    putStrLn "printing table test done, closing..."
    SS.close conn
