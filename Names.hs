{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Names where

import Draw
import Transition

import qualified Data.Map.Strict as M (member, insert, empty, mapWithKey, foldr)
import qualified Data.Vector as V (
    Vector(..), empty, fromList, (!), singleton, reverse, (++), filter, toList
  )

import Data.Function (on)
import Data.Maybe (isJust, fromJust)
import Data.Char (
    toLower, toUpper, isAlpha, generalCategory
  , GeneralCategory (NonSpacingMark)
  )
import Data.String (IsString(..))

import Text.Read (readMaybe)

import qualified Data.Text as T (unpack, filter)
import qualified Data.Text.Normal.NFD as NFD (toText)

import Control.Monad (when, unless) 
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.State (execState, get, put, modify)

import Text.Printf (printf)
import System.IO (
    openFile, IOMode (ReadMode), stdin, hClose, hIsEOF, hGetChar, hFileSize
  , hTell
  )
import System.Console.ANSI (setCursorColumn)

import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)

newtype Name = Name (V.Vector Letter) deriving (Eq, Show, Read)

instance IsString Name where
    fromString "" = Name V.empty
    fromString s = Name . V.fromList . map (read . return . toUpper)
                 . filter isAlpha . head . words . stripAccents $ s

data Letter = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O 
            | P | Q | R | S | T | U | V | W | X | Y | Z | Æ | Ð | Ø | Þ 
            deriving (Eq, Enum, Ord, Show, Read)

type NameTransition = Transition (Maybe Letter, Maybe Letter) (Maybe Letter)

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

showName :: Name -> String
showName (Name ls) = map toLower . concatMap show . V.toList $ ls

generateTransition :: Maybe FilePath -> IO NameTransition
generateTransition mDictFp = do
    dictHandle <- case mDictFp of
        Just fp -> openFile fp ReadMode
        Nothing -> return stdin
    
    dictSize <- if isJust mDictFp then hFileSize dictHandle else return 1
    
    transRef <- newIORef (Transition M.empty)
    l1Ref <- newIORef Nothing
    l2Ref <- newIORef Nothing
    
    let process = do
            when (isJust mDictFp) $ do
                pos <- if isJust mDictFp then hTell dictHandle else return 1
                let p = 100 * fromIntegral pos / fromIntegral dictSize :: Double
                putStr $ printf "%.2f" p ++ "%"
                setCursorColumn 0
            eof <- hIsEOF dictHandle
            if eof 
                then when (isJust mDictFp) $ hClose dictHandle
                else do
                    ch <- hGetChar dictHandle
                    if ch `elem` " \n"
                        then do
                            l1 <- readIORef l1Ref
                            l2 <- readIORef l2Ref
                            when (isJust l2) $ modifyIORef' transRef $ 
                                addCombo (l1,l2) Nothing
                            writeIORef l1Ref Nothing
                            writeIORef l2Ref Nothing
                        else do
                            let ml = readMaybe . stripAccents 
                                   . return . toUpper $ ch
                            when (isJust ml) $ do
                                let l = fromJust ml
                                l1 <- readIORef l1Ref
                                l2 <- readIORef l2Ref
                                modifyIORef' transRef $ 
                                    addCombo (l1,l2) (Just l)
                                writeIORef l1Ref l2
                                writeIORef l2Ref (Just l)
                    process
    
    process

    nonNormal <- readIORef transRef

    putStrLn ""

    return $ normalize nonNormal

----------------------

stripAccents :: String -> String
stripAccents = T.unpack . T.filter ((/=NonSpacingMark) . generalCategory) 
             . NFD.toText . fromString . map (\c -> case c of
                     'ß' -> 's'
                     x -> x)

addCombo :: (Maybe Letter, Maybe Letter) -> Maybe Letter 
            -> NameTransition -> NameTransition
addCombo x l transition = flip execState transition $ do
    (Transition m) <- get
    unless (x `M.member` m) $ 
        put $ Transition . M.insert x (Distribution M.empty) $ m
    modify $ \(Transition t) -> Transition $ flip M.mapWithKey t $
        \k a -> if k /= x then a else flip execState a $ do
            (Distribution m') <- get
            unless (l `M.member` m') $ modify $ 
                \(Distribution d) -> Distribution (M.insert l 0 d)
            modify $ \(Distribution d) -> Distribution $ flip M.mapWithKey d $ 
                \k' -> if k' /= l then id else (+1)

normalize :: NameTransition -> NameTransition
normalize (Transition m) = 
    Transition $ flip fmap m $ \(Distribution d) -> Distribution $ 
        let total = M.foldr (+) 0 d in fmap (/total) d

