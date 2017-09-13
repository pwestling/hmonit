{-# LANGUAGE DeriveAnyClass    #-}
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
import qualified Data.Map                  as M
import           Data.Maybe
import qualified Data.Text                 as Text

import qualified Snap
import           System.Environment
import           System.IO
import           System.Posix.Signals
import           System.Process

import           Safe                      as X (headMay, headNote, initMay,
                                                 tailMay)

import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy.UTF8 as Utf8
import qualified Data.Text                 as T
import qualified Text.Read                 as R

import           Debug.Trace               (traceShow)


import           Html
import           Network
import           Regex
import           Types
import           Util



createWebPage :: String -> [Address] -> Maybe Rgx -> IO String
createWebPage root as serviceFilter = do
  pageHTMLSWithErrors <-  mapM (getRelinked root) as
  let _ = traceShow (filter (isNothing . fst) $ zip pageHTMLSWithErrors as) Nothing
  let validPagesAndAddress = filter (isJust . fst) $ zip pageHTMLSWithErrors as
  let pageStrings = map (fromJust. fst) validPagesAndAddress
  let addresses = map snd validPagesAndAddress
  let baseHTML = headNote "Every page returned an error" pageStrings
  let serviceEntryRawRows = filterRow (maybeMatch serviceFilter) $ extractRows grabServiceEntries addresses pageStrings
  print $ length serviceEntryRawRows
  let systemEntryRawRows = extractRows grabSystemEntries addresses pageStrings
  let serviceEntries = asTable $ mapRows recolorRow $ mapRows sortRowsByLink $ mapMeta (addressMeta addSystemColumn) serviceEntryRawRows
  let systemEntries =  asTable $ mapRows recolorRow $ mapRows sortRowsByLink $ mapMeta (pageMeta addUptime) systemEntryRawRows
  let htmlWithRows = insertToHtml baseHTML
        [(sysRegex, systemEntries),
          (serviceRegex, serviceEntries)]
  let finalHTML = fixHomeLink root $ removeUptimeMessage $ addUptimeHeader $ addSystemHeader htmlWithRows
  return (if finalHTML == "" then "Data Filtered Out" else finalHTML)

findByHost :: String -> [Address] -> Address
findByHost h [] = error $ "No host " ++ show h
findByHost h (add@(Address link host) : as)
  | h == host = add
  | otherwise = findByHost h as

determineAddress :: [Address] -> Snap.Snap (Address, String, String)
determineAddress as = do
  host <- Snap.getParam "host"
  dest <- Snap.getParam "dest"
  let realhost = C.unpack $ fromMaybe "Please specify a host" host
  let realdest = maybe "" C.unpack dest
  let address = findByHost realhost as
  let link = getLink address ++ "/" ++ realdest
  return (address, link, realdest)

snapToPage :: Maybe String -> Snap.Snap ()
snapToPage page = fmap pack' (return $ fromMaybe "Data not found for this page" page) >>= Snap.writeBS

hostroute :: String -> [Address] -> Snap.Snap ()
hostroute root as  = do
  (address, link, dest) <- determineAddress as
  rawPage <- liftIO $ getA (getLink address ++ "/" ++ dest)
  let adjustPage = fixHomeLink root . relink root address
  let page = fmap adjustPage rawPage
  snapToPage page

hostpostroute :: [Address] -> Snap.Snap ()
hostpostroute as  = do
  (address, link, dest) <- determineAddress as
  postParams <- Snap.getPostParams
  liftIO $ postA link (paramsToForm postParams)
  page <- liftIO  (getA link)
  snapToPage page

toproute root as = fmap pack' (liftIO $ createWebPage root as Nothing) >>= Snap.writeBS

serviceroute root as = do
  service <- Snap.getParam "service"
  fmap pack' (liftIO $ createWebPage root as (Just $ Rgx $ C.unpack $ fromMaybe "Please set service param" service)) >>= Snap.writeBS

server :: String -> [Address] -> Snap.Snap ()
server root as =  Snap.ifTop (toproute root as)
  <|> Snap.route [("host/:host/:dest", Snap.method Snap.POST (hostpostroute as)),
                  ("host/:host", hostroute root as),
                  ("host/:host/:dest", hostroute root as),
                  ("service/:service", serviceroute root as)]

handler :: ThreadId -> [ProcessHandle] -> IO ()
handler tid handles = do
  mapM_ terminateProcess handles
  putStrLn "Terminating tunnels"
  throwTo tid E.UserInterrupt
  return ()

someFunc :: IO ()
someFunc = do
  tid <- myThreadId
  args <- getArgs
  let check = if length args /= 3 then error "Required arguments are bindAddr, port, configJson" else ()
  let bindAddr = headNote "Please supply the correct args: [bindAddr, port, configJson]" args
  let port = (read $ fromMaybe "8000" (secondEl args)) :: Int
  configString <- readFile $ fromMaybe "" (thirdEl args)
  let Just config = decode $ Utf8.fromString configString :: Maybe Config
  addressAndHandles <- toAddresses config
  installHandler keyboardSignal (Catch (handler tid (snd addressAndHandles))) Nothing
  threadDelay 1000000
  putStrLn "Ready"
  let config = Snap.setPort port mempty
  Snap.httpServe config $ server (bindAddr ++ ":" ++ show port) (fst addressAndHandles)
  return ()
