import Data.List ( foldl' )
import qualified Data.Map as M
import System.Environment ( getArgs )
import System.Random

type Appearances = M.Map String Int
type FollowingWords = M.Map String Appearances
data Choices = Choices 
    { getList :: [(Int, String)]
    , getListLength :: Int 
    } deriving ( Show )
type ChoiceMap = M.Map String Choices

tr :: [(Char, String)] -> String -> String
tr m = concatMap trChar
    where
      d = M.fromList m
      trChar c = case c `M.lookup` d of
          Nothing -> [c]
          Just s  -> s

addSpacesAround :: String -> String -> String
addSpacesAround ds = tr rs
    where
      ds' = map (\c -> ' ':c:" ") ds
      rs = zip ds ds'

preprocess :: String -> String
preprocess = addSpacesAround ",.!?:[]#*_;\"'-()/\\"

buildFollowingWordMap :: [String] -> FollowingWords
buildFollowingWordMap = go M.empty
    where
      go :: FollowingWords -> [String] -> FollowingWords
      go m (w:w2:ws) = go m' (w2:ws)
          where
            m' = case w `M.lookup` m of
                   Nothing -> M.insert w (M.insert w2 1 $ M.empty) m
                   Just m2 -> M.update (const . Just $ M.alter updateAdder w2 m2) w m
            updateAdder Nothing = Just 1
            updateAdder (Just n) = Just $ n+1
      go m _ = m

buildChoiceMap :: FollowingWords -> ChoiceMap
buildChoiceMap = M.map appearancesToChoices
    where
      appearancesToChoices :: Appearances -> Choices
      appearancesToChoices apps = Choices as' (fst . last $ as')
          where
            as = map (\(a, b) -> (b, a)) $ M.assocs apps
            as' = reverse . fst $ foldl' (\(l, rt) (i, s) -> ((i+rt, s):l, rt+i)) ([], 0) as

rollNSidedDice :: Int -> IO Int
rollNSidedDice n = getStdRandom (randomR (1, n))

getRandomWeightedChoice :: Choices -> IO String
getRandomWeightedChoice c = do
  n <- rollNSidedDice (getListLength c)
  return . snd . head $ filter (\(i, s) -> i>=n) (getList c)

main :: IO ()
main = do
  (fn:_) <- getArgs
  txt <- readFile fn
  let wl = words . preprocess $ txt
  let cm = buildChoiceMap . buildFollowingWordMap $ wl
  let (Just vah) = M.lookup "the" cm
  getRandomWeightedChoice vah >>= print
