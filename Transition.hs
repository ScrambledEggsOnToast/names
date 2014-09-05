module Transition where

import qualified Data.Map as M (
    Map (..), toList, lookup, unionWith, unionsWith, foldl
  )
import Control.Applicative ((<$>))
import Control.Monad.Random (MonadRandom, getRandomR)

type Probability = Double

newtype Distribution a = Distribution (M.Map a Probability) 
    deriving (Eq, Show, Read)

newtype Transition a b = Transition (M.Map a (Distribution b))
    deriving (Eq, Show, Read)

roll :: Probability -> Distribution a -> Maybe a
roll p (Distribution ds) = go p (M.toList ds)
    where
        go _ [] = Nothing
        go p ((x,p'):ds) = if p <= p' then Just x else go (p-p') ds

randomFrom :: MonadRandom m => Distribution a -> m (Maybe a)
randomFrom d = do
    p <- getRandomR (0,1)
    return $ roll p d

randomTransit :: (MonadRandom m, Ord a) => Transition a b -> a -> m (Maybe b)
randomTransit (Transition t) x = do
    let md = M.lookup x t
    case md of 
        Nothing -> return Nothing
        Just d -> randomFrom d

transitionMap :: Transition a b -> M.Map a (Distribution b)
transitionMap (Transition m) = m

distributionMap :: Distribution a -> M.Map a Probability
distributionMap (Distribution m) = m

transitionDistance :: (Ord a, Ord b) => Transition a b -> Transition a b 
                        -> Double
transitionDistance (Transition m1) (Transition m2) = sqrt sumSqds
  where
    sqds = M.unionWith (fmap (**2) .: M.unionWith (-)) 
        (distributionMap <$> m1) (distributionMap <$> m2)
    (.:) = (.).(.)

    sumSqds = M.foldl (+) 0 $ fmap (M.foldl (+) 0) sqds

averageTransition :: (Ord a, Ord b) => [Transition a b] -> Transition a b
averageTransition ts = 
    Transition
  . M.unionsWith (\a b -> Distribution $ 
        M.unionWith (+) (distributionMap a) (distributionMap b))
  . map transitionMap 
  $ ts
