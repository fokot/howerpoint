{-# LANGUAGE ScopedTypeVariables #-}

module Howepoint where

import System.IO
import Data.List.Split
import Data.IORef
import Control.DeepSeq (deepseq)
import System.Console.Terminal.Size (size)

f = "/Users/Frantisek/haskell/howerpoint/test/resources/howerpoint.txt"

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
  p :: IO (),
  nn :: Int -> IO (),
  pp :: Int -> IO (),
  g :: Int -> IO ()
--   resetSize :: IO (),
--   setSize :: Int -> Int -> IO (),
--   showSize :: IO ()
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
  let maxSlides = length slides
  let nextSlide = moveSlide slides currentSlide (\x -> if x == maxSlides - 1 then x else x + 1)
  let previousSlide = moveSlide slides currentSlide (\x -> if x == 0 then x else x - 1)
  let advanceSlides = \i -> moveSlide slides currentSlide (\x -> if x + i >= maxSlides - 1 then maxSlides - 1 else x + i)
  let goBackSlides = \i -> moveSlide slides currentSlide (\x -> if x - i <= 0 then 0 else x - i)
  let goToSlide = \i -> moveSlide slides currentSlide (\x -> if i > 0 && i <= maxSlides then i - 1 else x)
  return $ Presentation {
    n = nextSlide,
    p = previousSlide,
    nn = advanceSlides,
    pp = goBackSlides,
    g = goToSlide
  }

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
          let (colorizedContent, len) = colorize lineContent
              (leftPadding, rightPadding) = padding (width ps) len
          in '*' :
             (repeatN (leftPadding - 1) ' ') ++
             colorizedContent ++
             (repeatN (rightPadding - 1) ' ') ++
             "*"

colorSet x = case x of
  'k' -> toColor 30 -- Black
  'r' -> toColor 31 -- Red
  'g' -> toColor 32 -- Green
  'y' -> toColor 33 -- Yellow
  'b' -> toColor 34 -- Blue
  'm' -> toColor 35 -- Magenta
  'c' -> toColor 36 -- Cyan
  'w' -> toColor 37 -- White
  'a' -> [x]   -- No coloring
  where toColor c = "\x1b[" ++ show c ++ "m"

colorReset = "\x1b[0m"

-- it also takes care of escaping to ;\\k' will be just '\k' and not black color
colorize :: String -> (String, Int)
colorize line = if elem '\\' line then colorize' line [] (length line) else (line, length line)
  where colorize' [] acc len = (acc ++ colorReset, len)
        colorize' ('\\':x:xs) acc len = let c = colorSet x
                                        in colorize' xs (acc ++ c) (if c == [x] then len else len - 2)
        colorize' (x:xs) acc len = colorize' xs (acc ++ [x]) len


-- gets the terminal size
-- import System.Console.Terminal.Size (size)