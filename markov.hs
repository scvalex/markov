import qualified Data.Map as M
import System.Environment ( getArgs )

type Appearances = M.Map String Int
type FollowingWords = M.Map String Appearances

tr :: [(Char, String)] -> String -> String
tr m s = concatMap trChar s
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
buildFollowingWordMap ws = go M.empty ws
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

main :: IO ()
main = do
  (fn:_) <- getArgs
  txt <- readFile fn
  let txt' = preprocess txt
  let wl = words txt'
  let fwm = buildFollowingWordMap wl
  print $ M.lookup "." fwm
