{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Monad         as Ctrl (join)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)
import           Safe                  as X (headMay, headNote, initMay,
                                             tailMay)
import qualified Text.Read             as R




concatStrings :: [String] -> String
concatStrings = concat

secondEl :: [a] -> Maybe a
secondEl = join . fmap headMay . tailMay

thirdEl :: [a] -> Maybe a
thirdEl = join . fmap headMay . join . fmap tailMay . tailMay

iterApply :: [a -> a] -> a -> a
iterApply fns val = foldr ($) val fns


appendEach :: [a] -> [[b]] -> [[(a,b)]]
appendEach _ [] = []
appendEach [] _ = []
appendEach (a:as) (b:bs) = map (\x -> (a,x)) b : appendEach as bs

onSnd :: (a -> b) -> ((c,a) -> (c,b))
onSnd f (c,a) = (c, f a)

pack' :: String -> C.ByteString
pack' = encodeUtf8 . T.pack

verify :: String -> Maybe String
verify = fmap C.unpack . R.readMaybe
