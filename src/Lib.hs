{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}



module Lib where


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
import qualified Snap
import           System.Environment
import           System.IO
import           System.Posix.Signals
import           System.Process
import           Text.Regex.PCRE

import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy.UTF8 as Utf8

data SSHHost = SSHHost{host :: String, localport :: Integer, remoteport :: Integer} deriving (Show, Generic, FromJSON, ToJSON)
data SSHConfig = SSH{username :: String, hosts :: [SSHHost]} deriving (Show, Generic, FromJSON, ToJSON)

data Config =
  Config{ ssh :: Maybe SSHConfig } deriving (Show, Generic, FromJSON, ToJSON)

data Address = Address String String

sshTunnelCmd :: SSHHost -> String -> String
sshTunnelCmd (SSHHost host localport remoteport) username = "ssh -M -f -N -L " ++ show localport ++ ":localhost:" ++ show remoteport ++ " " ++ username ++ "@" ++ host

sshAddress :: SSHHost -> Address
sshAddress SSHHost{localport,host} = Address ("http://127.0.0.1:" ++ show localport) host


connectToSSH :: String -> SSHHost -> IO (Address, ProcessHandle)
connectToSSH username host = do
  handle <- spawnCommand (sshTunnelCmd host username)
  let address = sshAddress host
  return (address, handle)

toAddresses :: Lib.Config -> IO ([Address],[ProcessHandle])
toAddresses (config@Config{ssh = (Just (SSH username hosts))}) = do
  connections <- mapM (connectToSSH username) hosts
  let output = unzip connections
  return output

getResult :: Result a -> IO ()
getResult (Error r) = print r
getResult (Success r) = return ()



relink :: String -> String -> String
relink a page = sub page "href='(.*?)'" ("href='" ++ a ++ "/$1'")

getA :: Address -> IO String
getA (Address a h) = do
  let options = defaults & auth ?~ basicAuth "admin" "monit"
  rsp <- getWith options a
  let responseString = relink a $ show $ rsp ^. responseBody
  return responseString

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

addSystemColumn :: Address -> String -> String
addSystemColumn (Address _ host) tr = sub tr "</td></tr>" ("</td><td align='right' >" ++ host ++"</td></tr>")

addSystemHeader :: String -> String
addSystemHeader page = sub page "(<tr>.*?Process.*?</th>)</tr>" "$1<th align='right'>Host</th></tr>"

createWebPage :: [Address] -> IO String
createWebPage as = do
  pageHTMLSWithErrors <- mapM getA as
  let pageStrings = pageHTMLSWithErrors
  let baseHTML = head pageStrings
  let serviceEntries = concatStrings $ sort $ zipWith addSystemColumn as $ map grabServiceEntries pageStrings
  let systemEntries = concatStrings $ map grabSystemEntries pageStrings
  let inter = sub baseHTML sysRegex ("$1" ++ systemEntries ++ "$3")
  let subHTML =  addSystemHeader $ sub inter serviceRegex ("$1" ++ serviceEntries ++ "$3")
  return subHTML

server :: [Address] -> Snap.Snap ()
server as =  fmap read (liftIO $ createWebPage as) >>= Snap.writeBS

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
  threadDelay 10000000
  putStrLn "Ready"
  Snap.quickHttpServe $ server (fst addressAndHandles)
  return ()
