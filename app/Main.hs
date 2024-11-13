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
import Control.Monad (forever, unless, void, forM_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, readMVar, modifyMVar_, modifyMVar)

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)

import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map
import Data.List (delete)

-- Define types for room management
type ClientId = Int
type RoomId = Int
type RoomName = String
type ClientSocket = Socket
type Clients = Map.Map ClientId (ClientSocket, RoomName)

-- Global state to track room memberships
data ServerState = ServerState 
    { roomClients :: Map.Map RoomName [ClientId]  -- Maps room names to list of client IDs
    , clientMap :: Clients                        -- Maps client IDs to their sockets and room
    , nextClientId :: ClientId                    -- Counter for generating client IDs
    }

-- Initialize server state
initServerState :: ServerState
initServerState = ServerState 
    { roomClients = Map.empty
    , clientMap = Map.empty
    , nextClientId = 0
    }

main :: IO ()
main = do
    putStrLn "Enter a command (init_server, init_db, connect_client, create_room, join_room):"
    opt <- getLine 
    case opt of
        "init_server" -> setupServer (Just "localhost") "8000"
        "init_db"     -> main >> initDB  -- we initDB then run main again
        "connect_client" -> connectClient (Just "localhost") "8000" >>= startChatting
        "create_room" -> getRoom "create" 
        "join_room"   -> getRoom "join"
        _            -> putStrLn "Invalid options"
    where 
        getRoom action = do 
            putStrLn "\nRoom name: "
            roomName <- getLine
            s <- connectClient (Just "localhost") "8000" 
            if action == "join"
                then joinRoom roomName s
                else main >> void (createRoom roomName)

setupServer :: Maybe String -> [Char] -> IO ()
setupServer mhostN port = do
    state <- newMVar initServerState
    runTCPServer mhostN port (loop state)
    where
        loop state s = forever $ do
            putStrLn "\n================="
            putStrLn "Waiting for connection..."
            putStrLn "=================\n"
            
            (clientSock, clientAddr) <- accept s
            void $ forkIO $ handleClient state clientSock clientAddr
            
handleClient :: MVar ServerState -> Socket -> SockAddr -> IO ()
handleClient state sock addr = do
    putStrLn $ "New client connected: " ++ show addr
    sendAll sock $ pack "Welcome! Please join a room using JOIN <room_name>"
    
    -- Wait for JOIN command
    msg <- recv sock 1024
    let command = words $ unpack msg
    
    case command of
        ("JOIN":roomName:_) -> do
            clientId <- joinClientToRoom state sock roomName
            broadcastToRoom state roomName $ 
                pack $ "New client joined the room!"
            
            -- Handle messages in room
            handleMessages state clientId sock
        _ -> sendAll sock $ pack "Invalid command. Use: JOIN <room_name>"


runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) NS.close loop
    where
        resolve = do
            let hints = defaultHints 
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream 
                    }
            NE.head <$> getAddrInfo (Just hints) mhost (Just port)

        open addr = E.bracketOnError (openSocket addr) NS.close $ \sock -> do
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            NS.bind sock $ addrAddress addr
            listen sock 1024
            return sock

        loop sock = server sock

connectClient :: Maybe String -> [Char] -> IO Socket
connectClient mhostName port = withSocketsDo $ do
    let hints = defaultHints { addrSocketType = Stream }
    addrInfos <- getAddrInfo (Just hints) mhostName (Just port)
    case addrInfos of
        (addr:_) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            putStrLn $ "Created socket: " ++ show sock
            putStrLn $ "Address: " ++ show (addrAddress addr)
            connect sock (addrAddress addr)
            return sock
        [] -> error "Could not resolve address"

startChatting :: Socket -> IO ()
startChatting sock = do
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

chatWithLock :: MVar () -> Socket -> IO ()
chatWithLock lock s = do
    withMVar lock $ \_ -> do
        putStr "Enter Message: "
        hFlush stdout
    msg <- getLine
    sendAll s $ pack msg

clearLine :: IO ()
clearLine = do
    putStr "\r"                    -- Return to start of line
    putStr (replicate 80 ' ')      -- Clear with spaces
    putStr "\r"                    -- Return to start again

-- Handle messages from a client
handleMessages :: MVar ServerState -> ClientId -> Socket -> IO ()
handleMessages state clientId sock = forever $ do
    msg <- recv sock 1024
    unless (S.null msg) $ do
        serverState <- readMVar state
        case Map.lookup clientId (clientMap serverState) of
            Just (_, roomName) -> 
                broadcastToRoom state roomName $ 
                    pack $ "Client " ++ show clientId ++ ": " ++ unpack msg
            Nothing -> 
                sendAll sock $ pack "You're not in any room!"
    `E.catch` \(e :: E.IOException) -> do
        removeClientFromRoom state clientId
        NS.close sock

-- Join a client to a room
joinClientToRoom :: MVar ServerState -> Socket -> RoomName -> IO ClientId
joinClientToRoom state sock roomName = modifyMVar state $ \s -> do
    let clientId = nextClientId s
        newClients = Map.insert clientId (sock, roomName) (clientMap s)
        newRoomClients = Map.insertWith (++) roomName [clientId] (roomClients s)
        newState = s { clientMap = newClients
                    , roomClients = newRoomClients
                    , nextClientId = clientId + 1
                    }
    
    -- Update the database
    conn <- open "chat.db"
    execute conn "INSERT INTO room_members (room_id, client_id) VALUES ((SELECT room_id FROM rooms WHERE room_name = ?), ?)" 
           (roomName, clientId)
    SS.close conn
    
    return (newState, clientId)

-- Remove a client from their room
removeClientFromRoom :: MVar ServerState -> ClientId -> IO ()
removeClientFromRoom state clientId = modifyMVar_ state $ \s -> do
    case Map.lookup clientId (clientMap s) of
        Just (_, roomName) -> do
            let newRoomClients = Map.adjust (delete clientId) roomName (roomClients s)
                newClientMap = Map.delete clientId (clientMap s)
            
            -- Update database
            conn <- open "chat.db"
            execute conn "DELETE FROM room_members WHERE client_id = ?" (Only clientId)
            SS.close conn
            
            return s { roomClients = newRoomClients
                    , clientMap = newClientMap
                    }
        Nothing -> return s

-- Broadcast a message to all clients in a room
broadcastToRoom :: MVar ServerState -> RoomName -> S.ByteString -> IO ()
broadcastToRoom state roomName msg = do
    serverState <- readMVar state
    case Map.lookup roomName (roomClients serverState) of
        Just clientIds -> 
            forM_ clientIds $ \clientId -> 
                case Map.lookup clientId (clientMap serverState) of
                    Just (sock, _) -> 
                        sendAll sock msg `E.catch` \(_ :: E.IOException) -> 
                            return ()
                    Nothing -> return ()
        Nothing -> return ()

--------------------------------
---- DB Stuff
--------------------------------

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance FromRow Room where
    fromRow = Room <$> field <*> field <*> field

data Room = Room 
    { roomId :: Int
    , roomName :: String
    , maxClients :: Int  -- Optional: limit room size
    } deriving (Show)

data Client = Client
    { clientId :: Int
    , clientName :: String  -- Optional: for identifying users
    } deriving (Show)

initDB :: IO ()
initDB = do
    conn <- open "chat.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS rooms \
                 \(room_id INTEGER PRIMARY KEY AUTOINCREMENT, \
                 \room_name TEXT NOT NULL UNIQUE, \
                 \max_clients INTEGER DEFAULT 10, \
                 \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
                 
    execute_ conn "CREATE TABLE IF NOT EXISTS clients \
                 \(client_id INTEGER PRIMARY KEY AUTOINCREMENT, \
                 \client_name TEXT, \
                 \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
                 
    execute_ conn "CREATE TABLE IF NOT EXISTS room_members \
                 \(room_id INTEGER, \
                 \client_id INTEGER, \
                 \joined_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, \
                 \PRIMARY KEY (room_id, client_id), \
                 \FOREIGN KEY (room_id) REFERENCES rooms(room_id), \
                 \FOREIGN KEY (client_id) REFERENCES clients(client_id))"
    
    putStrLn "Database initialized successfully"
    SS.close conn

-- Create a new room
createRoom :: String -> IO Room
createRoom roomName = do
    conn <- open "chat.db"
    execute conn "INSERT INTO rooms (room_name) VALUES (?)" (Only roomName)
    rowId <- lastInsertRowId conn
    SS.close conn
    return $ Room (fromIntegral rowId) roomName 10

-- Modified joinRoom function to initiate joining process
joinRoom :: String -> Socket -> IO ()
joinRoom roomName sock = do
    -- Verify room exists
    conn <- open "chat.db"
		-- this next line is fucked up, do some types here
    rooms <- query conn "SELECT * FROM rooms WHERE room_name = ?" (Only roomName) :: IO [Room]
    SS.close conn
    
    case rooms of
        [] -> sendAll sock $ pack "Error: Room does not exist"
        _  -> do
            -- Send JOIN command to server
            sendAll sock $ pack $ "JOIN " ++ roomName
            startChatting sock  -- Reuse existing chat functionality
