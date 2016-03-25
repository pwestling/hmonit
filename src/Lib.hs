{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}



module Lib where


import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Lens
import           Control.Monad

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text                 as Text
import           GHC.Generics
import           Network.Wreq

import           Data.String.Utils
import           Network.Socket
import qualified Snap
import           System.Environment
import           System.IO
import           System.Posix.Signals
import           System.Process
import           System.Random
import           Text.Regex.PCRE

import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy.UTF8 as Utf8

data SSHHost = SSHHost{host :: String, remoteport :: Integer} deriving (Show, Generic, FromJSON, ToJSON)
data SSHConfig = SSH{username :: String, hosts :: [SSHHost]} deriving (Show, Generic, FromJSON, ToJSON)

data Config =
  Config{ ssh :: Maybe SSHConfig } deriving (Show, Generic, FromJSON, ToJSON)

data Address = Address String String deriving Show

sshTunnelCmd :: Integer -> SSHHost -> String -> String
sshTunnelCmd localport (SSHHost host remoteport) username = "ssh -M -N -L " ++ show localport ++ ":localhost:" ++ show remoteport ++ " " ++ username ++ "@" ++ host

sshAddress :: Integer -> SSHHost -> Address
sshAddress localport SSHHost{host} = Address ("http://127.0.0.1:" ++ show localport) host

randomPort :: IO Integer
randomPort = do
  sock <- socket AF_INET Stream defaultProtocol
  bound <- bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
  port <- socketPort sock
  close sock
  return $ fromIntegral port

connectToSSH :: String -> SSHHost -> IO (Address, ProcessHandle)
connectToSSH username host = do
  port <- randomPort
  handle <- spawnCommand (sshTunnelCmd port host username)
  let address = sshAddress port host
  return (address, handle)

toAddresses :: Lib.Config -> IO ([Address],[ProcessHandle])
toAddresses (config@Config{ssh = (Just (SSH username hosts))}) = do
  connections <- mapM (connectToSSH username) hosts
  let output = unzip connections
  return output

getResult :: Result a -> IO ()
getResult (Error r) = print r
getResult (Success r) = return ()

getLink :: Address -> String
getLink (Address link _) = link

relink :: Address -> String -> String
relink (Address a h) page = sub page "href='(.*?)'" ("href='host/" ++ h ++ "/$1'")

getA :: String -> IO String
getA link = do
  let options = defaults & auth ?~ basicAuth "admin" "monit"
  rsp <- getWith options link
  let responseString =  show $ rsp ^. responseBody
  return responseString

getPlain :: Address -> IO String
getPlain (Address link h) = getA link

getRelinked :: Address -> IO String
getRelinked a@(Address link host) = do
  page <- getA link
  return $ relink a page

sysRegex :: String
sysRegex = "(<tr>.*?System.*?</tr>)(.*?)(</table>)"
serviceRegex :: String
serviceRegex = "(<tr>.*?Process.*?</tr>)(.*?)(</table>)"

grabEntries :: String -> String -> String
grabEntries regex html = secondEl groups where
  (a,b,c,groups) = html =~ regex :: (String, String, String, [String])

grabSystemEntries :: String -> String
grabSystemEntries = grabEntries sysRegex
grabServiceEntries :: String -> String
grabServiceEntries = grabEntries serviceRegex

concatStrings :: [String] -> String
concatStrings = concat

secondEl :: [a] -> a
secondEl = head . tail

thirdEl :: [a] -> a
thirdEl = head . tail .tail

replacementFunctions :: Int -> [String] -> [String -> String]
replacementFunctions i (s : ss) = replace ("$"++show i) s : replacementFunctions (i+1) ss
replacementFunctions _ [] = []

iterApply :: [a -> a] -> a -> a
iterApply fns val = foldr ($) val fns

sub :: String -> String -> String -> String
sub "" _ _ = ""
sub text pat replaceStr = subMatch text t pat replaceStr where
  t = text =~ pat :: (String, String, String, [String])

subMatch :: String -> (String, String, String, [String]) -> String -> String -> String
subMatch text (_, "",_,[]) _ _ = text
subMatch text (start,match,end,groups) pat replaceStr = start ++ replaceval ++ sub end pat replaceStr where
  replacements = replacementFunctions 1 groups
  replaceval = iterApply replacements replaceStr

matchGroup :: String -> String -> String
matchGroup regex text  = head groups where
  (start,match,end,groups) = text =~ regex :: (String, String, String, [String])

allMatches :: String -> String -> [String]
allMatches _ "" = []
allMatches regex text
  | match == "" = []
  | otherwise = match : allMatches regex end
  where
    (start,match,end,groups) = text =~ regex :: (String, String, String, [String])

addSystemColumn :: Address -> String -> String
addSystemColumn (Address _ host) tr = sub tr "</td></tr>" ("</td><td align='right' >" ++ host ++"</td></tr>")

addSystemHeader :: String -> String
addSystemHeader page = sub page "(<tr>.*?Process.*?</th>)</tr>" "$1<th align='right'>Host</th></tr>"

rows = "<tr.*?>.*?</tr>"
linkDest = "<a.*?>(.*?)</a>"

sortRowsByLink :: String -> String
sortRowsByLink tableBlob = concatStrings $ sortOn (matchGroup linkDest) $ allMatches rows tableBlob

createWebPage :: [Address] -> IO String
createWebPage as = do
  pageHTMLSWithErrors <- mapM getRelinked as
  let pageStrings = pageHTMLSWithErrors
  let baseHTML = head pageStrings
  let subTables = zipWith addSystemColumn as $ map grabServiceEntries pageStrings
  let serviceEntries = sortRowsByLink $ concatStrings subTables
  let systemEntries =  sortRowsByLink $ concatStrings $ map grabSystemEntries pageStrings
  let inter = sub baseHTML sysRegex ("$1" ++ systemEntries ++ "$3")
  let subHTML =  addSystemHeader $ sub inter serviceRegex ("$1" ++ serviceEntries ++ "$3")
  return subHTML

findByHost :: String -> [Address] -> Address
findByHost h [] = error $ "No host " ++ (show h)
findByHost h (add@(Address link host) : as)
  | h == host = add
  | otherwise = findByHost h as

hostroute :: [Address] -> Snap.Snap ()
hostroute as  = do
  host <- Snap.getParam "host"
  dest <- Snap.getParam "dest"
  let realhost = C.unpack $ fromJust host
  let realdest = maybe "" C.unpack dest
  let address = findByHost realhost as
  fmap read (liftIO (getA (getLink address ++ "/" ++ realdest))) >>= Snap.writeBS

toproute as = fmap read (liftIO $ createWebPage as) >>= Snap.writeBS

server :: [Address] -> Snap.Snap ()
server as =  Snap.ifTop (toproute as)
  <|> Snap.route [("host/:host", hostroute as),
                  ("host/:host/:dest", hostroute as)]

handler :: ThreadId -> [ProcessHandle] -> IO ()
handler tid handles = do
  mapM terminateProcess handles
  putStrLn "Terminating tunnels"
  throwTo tid E.UserInterrupt
  return ()

someFunc :: IO ()
someFunc = do
  tid <- myThreadId
  args <- getArgs
  configString <- readFile $ head args
  let Just config = decode $ Utf8.fromString configString :: Maybe Config
  addressAndHandles <- toAddresses config
  installHandler keyboardSignal (Catch (handler tid (snd addressAndHandles))) Nothing
  threadDelay 5000000
  putStrLn "Ready"
  Snap.quickHttpServe $ server (fst addressAndHandles)
  return ()
