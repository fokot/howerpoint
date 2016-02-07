{-# LANGUAGE ScopedTypeVariables #-}

module Howepoint where

import System.IO
import Control.Monad
import System.Process
import Data.List.Split
import Data.IORef
import Control.DeepSeq (deepseq)

f = "/Users/Frantisek/haskell/howerpoint/test/resources/test_navigation.txt"

type Slide = [String]


data PresentationSettings = PresentationSettings {
  width :: Int,
  height :: Int
--   slides :: [Slide],
--   currentSlide :: Int
}

h = do
  putStrLn "To start presentation run:"
  putStrLn "  :set -XRecordWildCards"
  putStrLn "  Presentation {..} <- loadPresentation \"PATH-TO-PRESENTATION\""


data Presentation = Presentation {
  n :: IO (),
  p :: IO ()
}

parseSlides :: String -> IO [Slide]
parseSlides path = do
    h <- openFile path ReadMode
    slides <- (splitOn ["---"]) . lines <$> hGetContents h
    slides `deepseq` hClose h
    pure slides

-- showSlide :: Slide -> IO ()
-- showSlide = mapM (putStrLn . formatSlide)

moveSlide :: [Slide] -> IORef Int -> (Int -> Int) -> IO ()
moveSlide slides currentSlideRef f = do
  modifyIORef currentSlideRef f
  n <- readIORef currentSlideRef
  let slide :: Slide = formatSlide (PresentationSettings 50 20) (slides !! n)
  mapM putStrLn slide
  pure ()


loadPresentation :: String -> IO Presentation
loadPresentation path = do
  slides <- parseSlides path
  currentSlide <- newIORef 0
  let slide :: Slide = formatSlide (PresentationSettings 50 20) (slides !! 0)
  mapM putStrLn slide
  let nextSlide = moveSlide slides currentSlide (\x -> if x == (length slides - 1) then x else x + 1)
  let previousSlide = moveSlide slides currentSlide (\x -> if x == 0 then x else x - 1)
  return $ Presentation nextSlide previousSlide

-- |calculates left-right or top-bottom padding from the maximum size and content size
padding :: Int -> Int -> (Int, Int)
padding size contentSize = (half, if odd (size - contentSize) then half + 1 else half)
  where half = floor $ (fromIntegral (size - contentSize)) / 2


repeatN n = take n . repeat

formatSlide :: PresentationSettings -> Slide -> Slide
formatSlide ps content =
  line :  (repeatN topPadding emptyLine) ++ map centerLine content ++ repeatN bottomPadding emptyLine ++ [line]
  where (topPadding, bottomPadding) = padding (height ps) (length content)
        line = take (width ps) $ repeat '*'
        emptyLine = '*' : (repeatN ((width ps) -2) ' ') ++ "*"
        centerLine :: String -> String
        centerLine lineContent =
          let (leftPadding, rightPadding) = padding (width ps) (length lineContent)
          in '*' :
             (repeatN (leftPadding - 1) ' ') ++
             lineContent ++
             (repeatN (rightPadding - 1) ' ') ++
             "*"