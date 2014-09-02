{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeFamilies, OverloadedLists #-}
module Main where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Data.String
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Data.Monoid

import GHC.Exts

import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

import Control.Applicative
import Control.Arrow
import Control.Monad

import Debug.Trace

newtype Name = Name (V.Vector Letter) deriving (Eq, Show, Read)
type Names = [Name]
data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z  deriving (Eq, Enum, Ord, Show, Read)

vowel :: Letter -> Bool
vowel l = l `elem` [A,E,I,O,U]

approxSyllables :: Name -> Int
approxSyllables (Name ls) = go 0 False ls
    where go n v ll = 
            if V.null ll 
                then n 
                else if (not v) && vowel (V.head ll)
                    then go (n+1) True (V.tail ll)
                    else if not $ vowel (V.head ll)
                        then go n False (V.tail ll)
                        else go n v (V.tail ll)

instance IsString Name where
    fromString "" = Name V.empty
    fromString x = Name . V.fromList . map (read . return . toUpper) . filter isAlpha . head . words $ x

instance IsString [Name] where
    fromString = map (Name . V.fromList . map (read . return . toUpper)) . filter (/="") . map (filter isAlpha) . words

type Probability = Float
newtype Distribution a = Distribution (M.Map a Probability) deriving (Eq, Show, Read)
newtype Transition a b = Transition (M.Map a (Distribution b)) deriving (Eq, Show, Read)

roll :: Probability -> Distribution a -> Maybe a
roll p (Distribution ds) = go p (M.toList ds)
    where
        go _ [] = Nothing
        go p ((x,p'):ds) = if p <= p' then Just x else go (p-p') ds

distribute :: Ord a => [a] -> Distribution a
distribute xs = Distribution $ fmap (/(genericLength xs)) $ execState (forM_ xs $ \x -> do
    m <- get
    unless (x `M.member` m) $ modify (M.insert x 0)
    modify $ M.mapWithKey $ \k -> if k == x then (+1) else id
    ) M.empty 

randomFrom :: MonadRandom m => Distribution a -> m (Maybe a)
randomFrom d = do
    p <- getRandomR (0,1)
    return $ roll p d

makeTransition :: (Ord a, Ord b) => [(a,b)] -> Transition a b
makeTransition ps = Transition . fmap (\(m,n) -> Distribution . fmap (/n) $ m) $ flip execState M.empty $ forM_ ps $ \(x,y) -> do
    m <- get 
    unless (x `M.member` m) $ modify (M.insert x (M.empty,0))
    modify $ M.mapWithKey $ \k a -> if k /= x then a else flip execState a $ do
        (m',_) <- get
        unless (y `M.member` m') $ modify $ first (M.insert y 0)
        modify $ first $ M.mapWithKey $ \k' -> if k' /= y then id else (+1)
        modify $ second (+1)

randomTransit :: (MonadRandom m, Ord a) => Transition a b -> a -> m (Maybe b)
randomTransit (Transition t) x = do
    let md = M.lookup x t
    case md of 
        Nothing -> return Nothing
        Just d -> randomFrom d


makePairs :: Name -> [((Maybe Letter, Maybe Letter), Maybe Letter)]
makePairs (Name ls) = V.toList $ V.zipWith3 (\a b c -> ((a,b),c)) 
    (V.fromList [Nothing, Nothing] V.++ mls)
    (V.singleton Nothing V.++ mls)
    (mls V.++ V.singleton Nothing)
    where
        mls = Just <$> ls

type NameTransition = Transition (Maybe Letter, Maybe Letter) (Maybe Letter)

nameTransition :: Names -> NameTransition
nameTransition ns = makeTransition pairs 
    where
        pairs = ns >>= makePairs

randomNameStarts :: MonadRandom m => NameTransition -> Name -> m Name
randomNameStarts t (Name b) = do
    let go n = do
            let l1 = n V.! 0
                l2 = n V.! 1
            next <- randomTransit t (l2,l1)
            case next of
                Nothing -> return n
                Just l -> go $ V.singleton l V.++ n
    mname <- go $ V.reverse $ V.fromList [Nothing, Nothing] V.++ fmap Just b
    let name = fmap fromJust . V.filter isJust $ mname
    return . Name . V.reverse $ name

randomName t = randomNameStarts t (Name V.empty)

data NameCondition = NameCondition [Name -> Bool]
noCondition :: NameCondition
noCondition = NameCondition []
instance Monoid NameCondition where
    mempty = noCondition
    mappend = both
both :: NameCondition -> NameCondition -> NameCondition
both (NameCondition fs) (NameCondition gs) = NameCondition (fs ++ gs)
lots :: [NameCondition] -> NameCondition
lots = mconcat

instance IsList NameCondition where
    type Item NameCondition = Name -> Bool
    fromList cs = NameCondition cs
    toList (NameCondition cs) = cs

satisfies :: Name -> NameCondition -> Bool
satisfies n (NameCondition cs) = all id $ cs <*> [n]

nameLength :: (Int -> Bool) -> NameCondition
nameLength n = NameCondition [\(Name name) -> n $ V.length name]

ends :: Name -> NameCondition
ends (Name b) = let n = V.length b in both (nameLength (> n)) $ [\(Name name) -> V.reverse b == V.take n (V.reverse name)]

syllables :: (Int -> Bool) -> NameCondition
syllables n = [\name -> n $ approxSyllables name]

with :: Monad m => m Name -> NameCondition -> m Name
with ac con = do
    n <- ac
    if n `satisfies` con
        then return n
        else ac `with` con

randomNameWith :: MonadRandom m => NameTransition -> NameCondition -> m Name
randomNameWith t c = do
    n <- randomName t
    if n `satisfies` c
        then return n
        else randomNameWith t c

showName :: Name -> String
showName (Name ls) = map toLower . concat . map show . V.toList $ ls

main = do
    ns <- fromString <$> readFile "/usr/share/dict/cracklib-small"
    let t = nameTransition ns
    print t
