module Dendrogram where

import Names
import Transition

import Data.List (intercalate)
import Data.Function (on)

import Data.Clustering.Hierarchical (
    Dendrogram (..), Distance, dendrogram, Linkage (CompleteLinkage), elements
  , cutAt
  )

makeDendrogram :: [(String, NameTransition)] 
            -> Dendrogram (String, NameTransition)
makeDendrogram transs = 
    dendrogram CompleteLinkage transs (transitionDistance `on` snd)

timeTravel' :: Distance -> ([a] -> a) -> (a -> a -> Distance) 
            -> Dendrogram a -> Dendrogram a
timeTravel' t cc df = 
    flip (dendrogram CompleteLinkage) df . map (cc . elements) . (`cutAt` t)

timeTravel :: Distance -> Dendrogram (String, NameTransition) 
            -> Dendrogram (String, NameTransition)
timeTravel t = timeTravel' t 
    (\sts -> (intercalate ", " (map fst sts), averageTransition (map snd sts))) 
    (transitionDistance `on` snd)

