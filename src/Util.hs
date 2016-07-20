module Util where

import           Control.Monad as Ctrl (join)
import           Safe          as X (headMay, headNote, initMay, tailMay)

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
