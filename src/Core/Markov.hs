module Core.Markov where

import Data.List (foldl')
import Data.Hashable
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Control.Monad.Random
import System.Random

-- Code from:  https://www.kovach.me/posts/2014-08-04-markov-chains.html
-- by Benjamin Kovach

-------------------------------- Intermediate Rep ------------------------------
type Weight = Rational
type Edge a  = (a, Weight)

-- | We use an intermediate representation that maps a node a, to a edge with a
-- weight and a Maybe because Rands cannot be empty
type MarkovI a = M.HashMap a (Maybe [Edge a])

class (Hashable a, Eq a) => Markovable a

insertToMarkov :: (Hashable a, Eq a) =>
  Rational -> a -> a -> MarkovI a -> MarkovI a
insertToMarkov r k v mkv = M.insert k (Just $ case M.lookup k mkv of
                                          Nothing -> [(v, r)]
                                          Just xs -> case xs of
                                            Nothing -> [(v,r)]
                                            Just ys -> (v, r):ys) mkv

insertEnd :: (Hashable a, Eq a) => a -> MarkovI a -> MarkovI a
insertEnd k = M.insert k Nothing

-------------------------------- Final Rep ------------------------------

newtype Markov g a = Markov {getMarkov :: M.HashMap a (Maybe (Rand g a)) }

fromMarkovI :: RandomGen g => MarkovI a -> Markov g a
fromMarkovI = Markov . fmap (fromList <$>)

type Err = String
data Output g a = Error Err
                | Value g a
                | End
                deriving (Show, Eq)

step :: (RandomGen g, Hashable a, Eq a) => Markov g a -> g -> a -> Output g a
step mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Error "Could not find value"
  Just rs -> case (flip runRand gen <$> rs) of
    Nothing -> End
    Just (a, g) -> Value g a

runMarkov :: (RandomGen g, Hashable a, Eq a) =>
  Integer -> Markov g a -> g -> a -> Either Err [a]
runMarkov n mkv gen x = go n
  where go m | m <= 0 = Right []
             | otherwise = (x:) <$> case step mkv gen x of
                 Value g a -> runMarkov (n-1) mkv g a
                 End       -> Right []
                 Error err -> Left err

mkMarkov :: (Hashable a, Eq a, RandomGen g) =>
  [(a, [(a, Rational)])] -> Markov g a
mkMarkov = Markov . foldl' (flip $ uncurry ins) M.empty
  where ins a b m = case b of
          [] -> M.insert a Nothing m
          _  -> M.insert a (Just $ fromList b) m

example :: (RandomGen g) => Markov g String
example = mkMarkov [("E", [("E", 3), ("A", 7)]), ("A", [("E", 4), ("A", 6)])]

-- run with
-- g <- newStdGen
-- runMarkov 15 example g "E"

-------------------------------- Making Sentences ------------------------------
wordPairs :: T.Text -> [(T.Text, T.Text)]
wordPairs = (zip <*> tail) . T.words

insertSentence :: MarkovI T.Text -> T.Text -> MarkovI T.Text
insertSentence mkv = insertMkvPairsInto mkv . wordPairs

insertMkvPairsInto :: (Hashable a, Eq a) => MarkovI a -> [(a, a)] -> MarkovI a
insertMkvPairsInto mkv [] = mkv
insertMkvPairsInto mkv ps = insertEnd lst $
  foldl' (flip (uncurry (insertToMarkov 1))) mkv ps
  where lst = snd $ last ps

fromSentences :: RandomGen g => [T.Text] -> Markov g T.Text
fromSentences = fromMarkovI . foldl' insertSentence M.empty

runFromSentences :: Integer -> [T.Text] -> IO (Either Err T.Text)
runFromSentences n sentences = do
  g <- newStdGen
  let hds = map (head . T.words) sentences
  seed <- uniform hds
  return $ T.unwords <$> runMarkov n (fromSentences sentences) g seed

test :: [T.Text]
test = [ "I am a monster."
       , "I am a rock star."
       , "I want to go to Hawaii."
       , "I want to eat a hamburger."
       , "I have a really big headache."
       , "Haskell is a fun language!"
       , "Go eat a big hamburger!"
       , "Markov chains are fun to use!"
       ]
