{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Network where

import qualified Control.Exception         as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.String.Utils
import           Debug.Trace
import           Network.Socket
import           Network.Wreq
import qualified Snap
import           System.Environment
import           System.IO
import           System.Posix.Signals
import           System.Process



import           Types
import           Util

import qualified Data.ByteString.Char8     as C

import qualified Data.ByteString.Lazy.UTF8 as Utf8

sshTunnelCmd :: Integer -> SSHHost -> String -> String
sshTunnelCmd localport (SSHHost host remoteport) username = "ssh -M -N -F ~" ++ username ++"/.ssh/config -L" ++ show localport ++ ":localhost:" ++ show remoteport ++ " " ++ username ++ "@" ++ host

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

toAddresses :: Config -> IO ([Address],[ProcessHandle])
toAddresses (config@Config{ssh = (Just (SSH username hosts))}) = do
  connections <- mapM (connectToSSH username) hosts
  let output = unzip connections
  return output

unsafeGetA :: String -> IO String
unsafeGetA link = do
  let options = defaults & auth ?~ basicAuth "admin" "monit"
  rsp <- getWith options link
  let responseString =  show $ fromMaybe "" (rsp ^? responseBody)
  return responseString

getA :: String -> IO (Maybe String)
getA link = E.catch (verify <$> unsafeGetA link) ((\e -> return Nothing) :: E.SomeException -> IO (Maybe String))

getPlain :: Address -> IO (Maybe String)
getPlain (Address link h) = getA link

paramToForm :: C.ByteString -> [C.ByteString] -> [FormParam] -> [FormParam]
paramToForm k vs fs = newparams ++ fs where
  newparams = map ((:=) k) vs

paramsToForm :: Snap.Params -> [FormParam]
paramsToForm = M.foldrWithKey paramToForm []

postA :: String -> [FormParam] -> IO ()
postA link params = do
  let options = defaults & auth ?~ basicAuth "admin" "monit"
  postWith options link params
  return ()
