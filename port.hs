import Control.Monad
import Data.List
import Data.Maybe
import System
import System.IO
import System.Process
import Text.ParserCombinators.Parsec

data Addr =
    Addr { ip   :: String,
           port :: String
         } deriving Show
         
data Conn =
    Conn { protocol   :: String,
           localAddr  :: Addr,
           remoteAddr :: Addr,
           state'     :: String,
           pId'       :: Int
         }
         
data Task =
    Task { imageName :: String,
           pId       :: Int,
           sName     :: String,
           sNumber   :: Int,
           memUsage  :: String
         } deriving Show

main = do
    p <- getPort
    c <- getConn p
    t <- getTask $ pId' c
    showTask t

getPort :: IO String
getPort = do
    args <- getArgs
    when (length args /= 1) $ do putStrLn "Usage: port.exe <port>" >> exitWith ExitSuccess
    let port = last args
    return port

getConn :: String -> IO Conn
getConn port = do
    raw <- getNetStat
    let conns = case parse parseConns "netstat" raw of
            Left err -> error $ "Input:\n" ++ show raw ++ "\nError:\n" ++ show err
            Right result -> result
    let c = find (filterPort port) conns
    when (isNothing c) $ do putStrLn (port ++ ": Port not in use") >> exitWith ExitSuccess
    return $ fromJust c
    
getTask :: Int -> IO Task
getTask pId = do
    raw <- getTaskList
    let tasks = case parse parseTasks "tasklist" raw of
            Left err -> error $ "Input:\n" ++ show raw ++ "\nError:\n" ++ show err
            Right result -> result
    let t = find (filterTask pId) tasks
    return $ fromJust t
    
filterPort :: String -> Conn -> Bool
--filterPort port (Conn{localAddr=Addr{port=p}}) = port == p
filterPort p c = p == (port $ localAddr c)
    
filterTask :: Int -> Task -> Bool
filterTask p t = p == pId t
    
showTask :: Task -> IO ()
showTask t = do putStrLn $ "Name: " ++ (imageName t) ++ "\nPID: " ++ (show $ pId t)
    
getNetStat :: IO String
getNetStat = readProcess "netstat" ["-ano"] []

getTaskList :: IO String
getTaskList = readProcess "tasklist" ["/nh", "/fo", "csv"] []
   
parseConns :: GenParser Char st [Conn]
parseConns = do 
    connections <- many conn
    eof
    return connections

conn :: GenParser Char st Conn
conn = do 
    spaces
    protocol <- anyChar `manyTill` space
    spaces
    localAddr <- readAddr
    spaces
    remoteAddr <- readAddr
    spaces
    state' <- optionMaybe $ try state
    spaces
    pId <- digit `manyTill` newline
    return (Conn protocol localAddr remoteAddr (fromMaybe "" state') (read pId))

state :: GenParser Char st [Char]
state = do
    result <- many $ letter <|> digit <|> char '_'
    notFollowedBy newline
    return result

parseTasks :: GenParser Char () [Task]    
parseTasks = do 
    optional (many newline)
    tasks <- many task
    eof :: Parser ()
    return tasks
     
task :: GenParser Char st Task
task = do
    imageName <- quotedString
    char ','
    pId <- quotedString
    char ','
    sessionName <- optionMaybe quotedString
    char ','
    session <- quotedString
    char ','
    memUsage<- quotedString
    newline
    return (Task imageName (read pId) (fromMaybe "" sessionName) (read session) memUsage)

quotedString :: GenParser Char st [Char]
quotedString = between quote quote $ many $ noneOf "\""

quote :: CharParser st Char
quote = char '"'
    
readAddr :: GenParser Char st Addr
readAddr = do
    ip <- anyChar `manyTill` (char ':')
    port <- anyChar `manyTill` space
    return (Addr ip port)
