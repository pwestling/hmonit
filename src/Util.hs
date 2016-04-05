module Util where



concatStrings :: [String] -> String
concatStrings = concat

secondEl :: [a] -> a
secondEl = head . tail

thirdEl :: [a] -> a
thirdEl = head . tail .tail

iterApply :: [a -> a] -> a -> a
iterApply fns val = foldr ($) val fns


appendEach :: [a] -> [[b]] -> [[(a,b)]]
appendEach _ [] = []
appendEach [] _ = []
appendEach (a:as) (b:bs) = map (\x -> (a,x)) b : appendEach as bs

onSnd :: (a -> b) -> ((c,a) -> (c,b))
onSnd f (c,a) = (c, f a)
