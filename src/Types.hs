{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           GHC.Generics



data SSHHost = SSHHost{host :: String, remoteport :: Integer} deriving (Show, Generic, FromJSON, ToJSON)
data SSHConfig = SSH{username :: String, hosts :: [SSHHost]} deriving (Show, Generic, FromJSON, ToJSON)

data Config =
  Config{ ssh :: Maybe SSHConfig } deriving (Show, Generic, FromJSON, ToJSON)

data Address = Address String String deriving Show

getLink :: Address -> String
getLink (Address link _) = link
