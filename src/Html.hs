module Html where

import           Data.List
import           Data.Maybe
import           Network
import           Regex
import           Types
import           Util


relink :: String -> Address -> String -> String
relink root (Address a h) page = sub  (Rgx $ "href='(http://"++root++"/host/"++ h ++"/)?(.*?)'") page (Replacement $ "href='http://"++root++"/host/" ++ h ++ "/$2'")

getRelinked :: String -> Address -> IO (Maybe String)
getRelinked root a@(Address link host) = do
  page <- getA link
  return $ relink root a <$> page

sysRegex :: Rgx
sysRegex = Rgx "(<tr>.*?System.*?</tr>)(.*?)(</table>)"
serviceRegex :: Rgx
serviceRegex = Rgx "(<tr>.*?Process.*?</tr>)(.*?)(</table>)"

grabEntries :: Rgx -> String -> [String]
grabEntries regex html = maybe [""] asRows (secondEl (matchGroups regex html))

grabSystemEntries :: String -> [String]
grabSystemEntries = grabEntries sysRegex
grabServiceEntries :: String -> [String]
grabServiceEntries = grabEntries serviceRegex

addSystemColumn :: Address -> String -> String
addSystemColumn (Address _ host) tr = sub (Rgx "</td></tr>") tr (Replacement ("</td><td align='right' ><a href='../../host/"++host++"'>" ++ host ++"</a></td></tr>"))

addSystemHeader :: Page -> Page
addSystemHeader page = sub (Rgx "(<tr>.*?Process.*?</th>)</tr>") page (Replacement "$1<th align='right'>Host</th></tr>")

addUptimeHeader :: Page -> Page
addUptimeHeader page = sub (Rgx "(<tr>.*?System.*?</th>)</tr>") page (Replacement "$1<th align='right'>Uptime</th></tr>")

removeUptimeMessage :: Page -> Page
removeUptimeMessage page = sub (Rgx "<p align='center'.*?Monit is .*?</p>") page (Replacement "")

rows = Rgx "<tr.*?>.*?</tr>"
linkDest = Rgx "<a.*?>(.*?)</a>"

asRows :: String -> [String]
asRows = allMatches rows

sortRowsByLink :: [String] -> [String]
sortRowsByLink =  sortOn (matchGroup linkDest)

recolorRow :: [String] -> [String]
recolorRow = recolor True where
  recolor _ [] = []
  recolor True (r : rows) = sub (Rgx "<tr([^>]*?)(class='[^>]*?')?([^>]*?)>") r (Replacement "<tr$1 class='stripe' $3>") : recolor False rows
  recolor False (r : rows)= sub (Rgx "<tr([^>]*?)class='[^>]*?'([^>]*?)>") r (Replacement "<tr$1$3>") : recolor True rows

type Page = String
type Row = String

extractRows :: (Page -> [Row]) -> [Address] -> [Page] -> Rows
extractRows entryExtractor as pages = Rows (concat $ appendEach (zip as pages) $ map entryExtractor pages)

data Rows = Rows [((Address,Page), Row)] deriving Show

mapRow :: (Row -> Row) -> Rows -> Rows
mapRow f (Rows l)= Rows $ map (onSnd f) l

type RowTransformer = (((Address,Page),Row) -> Row)

allMeta :: (Address -> Page -> Row -> Row) -> RowTransformer
allMeta f ((a,p),r) = f a p r

addressMeta :: (Address -> Row -> Row) -> RowTransformer
addressMeta f ((a,_),r) = f a r

pageMeta :: (Page -> Row -> Row) -> RowTransformer
pageMeta f ((_,p),r) = f p r

mapMeta :: (((Address,Page),Row) -> Row) -> Rows -> Rows
mapMeta f (Rows l)= Rows (mapR f l) where
  mapR _ [] = []
  mapR f (t@(a,s) : ls) = (a, f t) : mapR f ls

mapRows :: ([String] -> [String]) -> Rows -> Rows
mapRows f (Rows l) = Rows $ zip (map fst l) (f $ map snd l)

filterRow :: (String -> Bool) -> Rows -> Rows
filterRow f (Rows l) = Rows $ filter filt l where
  filt = f . snd

asTable :: Rows -> String
asTable (Rows l) = concatStrings $ map snd l

addUptime :: Page -> Row -> Row
addUptime page row = sub (Rgx "</td></tr>") row (Replacement $ "</td><td align='right'>"++ uptimeStr ++ "</td></tr>") where
  uptimeStr = fromMaybe "" $ matchGroup (Rgx "<i>uptime, (.*?)</i>") page

fixHomeLink :: String -> Page -> String
fixHomeLink root page = sub (Rgx "<a href='.*?'>Home</a>") page (Replacement $ "<a href='http://"++ root ++ "'>Home</a>")

insertToHtml :: String -> [(Rgx,String)] -> String
insertToHtml page [] = page
insertToHtml page ((regex,insert) : regexes) = insertToHtml (sub regex page (Replacement $ "$1" ++ insert ++ "$3")) regexes
