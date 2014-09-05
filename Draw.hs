module Draw where

import Data.Clustering.Hierarchical

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.SVGFonts

renderDendrogram' :: Double -> Dendrogram String -> Diagram B R2

renderDendrogram' fs (Leaf s) = 
        stroke (textSVG s fs) # fc black 
                              # lcA transparent 
                              # rotateBy 0.25 
                              # alignT 
                              # translate (r2 (0,-fs/4))
     <> fromVertices (map p2 [(-fs/2,0),(fs/2,0)]) # lwO 2

renderDendrogram' fs (Branch dist a b) = mconcat [
        thisDiag
      , aDiag # translate (r2 (-branchWidth/2, -aHeight))
      , bDiag # translate (r2 (branchWidth/2, -bHeight))
      ]
  where
    circ = circle (fs/5) # lcA transparent # fc black 
    
    thisDiag = circ <> fromVertices (map p2 [
        (-branchWidth/2,-aHeight)
      , (-branchWidth/2,0)
      , (branchWidth/2,0)
      , (branchWidth/2, -bHeight)
      ]) # lwO 1.5

    aDiag = renderDendrogram' fs a
    bDiag = renderDendrogram' fs b

    Just (_,aR) = extentX aDiag
    Just (bL,_) = extentX bDiag

    branchWidth = ((*0.2) . exp $ dist/10) + (aR - bL)

    aHeight = case a of
        Leaf _ -> dist
        Branch aDist _ _ -> dist - aDist
    bHeight = case b of
        Leaf _ -> dist
        Branch bDist _ _ -> dist - bDist

renderDendrogram :: Dendrogram String -> Diagram B R2
renderDendrogram dendro@(Branch d _ _) = 
    rect (dendroWidth * 1.1) (dendroHeight * 1.1) <> center dendroDiag
  where
    dendroDiag = renderDendrogram' 1 dendro
    dendroWidth = width dendroDiag
    dendroHeight = height dendroDiag

writeDendrogramSVG :: FilePath -> Double -> Dendrogram String -> IO ()
writeDendrogramSVG fp w dendro = 
    renderSVG fp (Width w) $ renderDendrogram dendro # rotateBy (-0.25)
                                                     # pad 1.05
