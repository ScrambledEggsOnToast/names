{-# LANGUAGE DeriveDataTypeable, RecordWildCards, BangPatterns, TupleSections #-}
module Main where

import Prelude hiding (words)

import Names
import Dendrogram
import Draw

import Data.Maybe (fromMaybe, isJust, fromJust)
import Text.Read (readMaybe)
import Control.Applicative ((<$>))
import Control.Monad (when, forM, replicateM_)
import Control.Arrow (first, second)

import System.IO (
    openFile, hClose, hPutStr, hPutStrLn, hGetContents, stdin, stdout
  , IOMode (ReadMode, WriteMode)
  )
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

import System.Console.CmdArgs (
    def, Data, Typeable, (&=), typFile, typDir, help, modes, versionArg
  , summary, helpArg, program, explicit, name, cmdArgs
  )

data Options = 
    Analyse {
        wordsIn :: Maybe FilePath
      , transitionOut :: Maybe FilePath
    }
  | Words {
        transitionIn :: Maybe FilePath
      , wordsOut :: Maybe FilePath
      , count :: Int
  }
  | Dendrogram {
        transitionDir :: Maybe FilePath
      , svgOut :: FilePath
  }
    deriving (Data, Typeable, Show)


analyseMode = Analyse {
    wordsIn = def &= typFile
  , transitionOut = def &= typFile
} &= help "Convert a list of words to a letter transition matrix."

wordsMode = Words {
    transitionIn = def &= typFile
  , wordsOut = def &= typFile
  , count = 1 &= help "Defaults to 1" 
} &= help "Generate random words from a letter transition matrix."

dendrogramMode = Dendrogram {
    transitionDir = def &= name "T" &= typDir &= help "Defaults to pwd"
  , svgOut = "dendrogram.svg" &= typFile
} &= help "Render a dendrogram from a letter transition matrix."

programName = "names"
programInfo = "names v0.1.0"
copyright = "(C) Josh Kirklin 2014"
programAbout = "All file options but svg-out default to stdin/stdout"

main = do
    o <- cmdArgs $ modes [analyseMode, wordsMode, dendrogramMode]
        &= versionArg [explicit, name "version", name "v", summary programInfo]
        &= summary (programInfo ++ ", " ++ copyright)
        &= help programAbout
        &= helpArg [explicit, name "help", name "h"]
        &= program programName
    exec o

exec :: Options -> IO ()

exec Analyse{..} = do
    t <- generateTransition wordsIn
    outHandle <- case transitionOut of
        Just fp -> openFile fp WriteMode
        Nothing -> return stdout

    hPutStr outHandle . show $ t
    
    when (isJust transitionOut) $ hClose outHandle

exec Words{..} = do
    inHandle <- case transitionIn of
        Just fp -> openFile fp ReadMode
        Nothing -> return stdin

    !t <- read <$> hGetContents inHandle

    outHandle <- case wordsOut of
        Just fp -> openFile fp WriteMode
        Nothing -> return stdout

    replicateM_ count $ do
        w <- randomName t
        hPutStrLn outHandle $ showName w

    when (isJust wordsOut) $ hClose outHandle

exec Dendrogram{..} = do
    let jTransitionDir = fromMaybe "." transitionDir

    transDirFps <- getDirectoryContents jTransitionDir
    transFps <- map fst . filter snd <$> 
        mapM (\fp -> (fp,) <$> doesFileExist (jTransitionDir </> fp)) 
        transDirFps

    !ts <- map (second fromJust) . filter (isJust . snd) <$> 
        forM transFps (\fp -> do
            !t <- readMaybe <$> readFile (jTransitionDir </> fp)
            return (fp, t))
    
    let d = makeDendrogram . map (first $ takeWhile (/='.')) $ ts
    
    writeDendrogramSVG svgOut 700 $ fst <$> d

