module Regex where

  import           Data.String.Utils
  import           Text.Regex.PCRE
  import           Util

  import           Safe                      as X (headMay, headNote, initMay,
                                                   tailMay)


  newtype Rgx = Rgx String
  newtype Replacement = Replacement String

  type Target = String
  type Match = String

  rgxStr :: Rgx -> String
  rgxStr (Rgx str) = str

  rplStr :: Replacement -> String
  rplStr (Replacement str) = str

  (=~*) :: String -> Rgx -> (String, String, String, [String])
  (=~*) t r = (t =~ rgxStr r) :: (String, String, String, [String])

  (=~?) :: String -> Rgx -> Bool
  (=~?) t r = (t =~ rgxStr r) :: Bool

  maybeMatch :: Maybe Rgx -> String -> Bool
  maybeMatch (Just r) s = s =~? r
  maybeMatch Nothing s = True

  replacementFunctions :: Int -> [Replacement] -> [String -> String]
  replacementFunctions i (s : ss) = replace ("$"++show i) (rplStr s) : replacementFunctions (i+1) ss
  replacementFunctions _ [] = []

  sub :: Rgx -> Target -> Replacement -> String
  sub _ "" _ = ""
  sub pat text replaceStr = subMatch pat text t replaceStr where
    t = text =~* pat

  subMatch :: Rgx -> Target -> (String, String, String, [String]) ->  Replacement -> String
  subMatch _ text (_, "",_,[]) _ = text
  subMatch pat text (start,match,end,groups) replaceStr = start ++ cleanedReplace ++ sub pat end  replaceStr where
    replacements = replacementFunctions 1 (map Replacement groups)
    replaceval =  iterApply replacements (rplStr replaceStr)
    cleanedReplace = sub (Rgx "[$][0-9]") replaceval  (Replacement "")

  matchGroup :: Rgx -> Target -> Maybe String
  matchGroup regex text = headMay $ matchGroups regex text

  matchGroups :: Rgx -> Target -> [String]
  matchGroups regex text  = groups where
    (start,match,end,groups) = text =~* regex :: (String, String, String, [String])

  allMatches :: Rgx -> Target -> [Match]
  allMatches _ "" = []
  allMatches regex text
    | match == "" = []
    | otherwise = match : allMatches regex end
    where
      (start,match,end,groups) = text =~* regex
