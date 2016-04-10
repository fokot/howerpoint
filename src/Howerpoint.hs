{-# LANGUAGE ScopedTypeVariables #-}

module Howerpoint where

import System.IO
import Data.List.Split
import Data.IORef
import Control.DeepSeq (deepseq)
import qualified System.Console.Terminal.Size as T
import Data.Char (isLower, toLower)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.List (isPrefixOf)
import Control.Applicative (liftA2)

type Slide = [String]

data PresentationState = PresentationState {
  slides :: [Slide],
  width :: Int,
  height :: Int,
  currentSlide :: Int
}

getCurrentSlide :: PresentationState -> Slide
getCurrentSlide ps = (slides ps) !! (currentSlide ps)

sanitizeCode :: String -> String
sanitizeCode = concatMap (\c -> if c == '"' then "\\\"" else [c])

filterCode :: Slide -> [String]
filterCode = catMaybes . map (\line ->
  if isPrefixOf ">>>" line then Just $ drop 3 line
  else if isPrefixOf "L>>" line then Just $ "let " ++ drop 3 line
  else if isPrefixOf "H>>" line then Just $ drop 3 line
  else Nothing)

isCode :: String -> Bool
isCode = liftA2 (||) (isPrefixOf ">>>") (isPrefixOf "L>>")

isHiddenCode = isPrefixOf "H>>"

maxCodeLength :: Slide -> Int
maxCodeLength = maximum . map (\line -> if isCode line then length line - 3 else 0)

printCodeAndExecute :: [String] -> String
printCodeAndExecute code = unlines (map (\x -> "putStrLn \"> " ++ (sanitizeCode x) ++ "\"") code) ++ unlines code

codeFromAllPreviousSlides :: IO String
codeFromAllPreviousSlides = pure "letn y = 100"

help = (sequence $ map (putStrLn . fst . colorize)
 ["\\k\\WTo load test presentation run \\Y:loadTestPresentation",
  "\\k\\WTo load your own presentation run \\Y:loadPresentation <PATH-TO-YOUR-PRESENTATION>"])
  >> pure ()


data Presentation = Presentation {
  n :: IO (),
  p :: IO (),
  nn :: Int -> IO (),
  pp :: Int -> IO (),
  g :: Int -> IO (),
  resetSize :: IO (),
  setSize :: Int -> Int -> IO (),
  showSize :: IO (),
  codeFromSlide :: IO [String]
}

parseSlides :: String -> IO [Slide]
parseSlides path = do
    h <- openFile path ReadMode
    slides <- (splitOn ["---"]) . lines <$> hGetContents h
    slides `deepseq` hClose h
    pure slides

displaySlide :: IORef PresentationState -> IO ()
displaySlide ps = do
  ps <- readIORef ps
  let slide = getCurrentSlide ps
  mapM putStrLn (formatSlide ps slide) >> pure ()

moveSlide :: IORef PresentationState -> (Int -> Int) -> IO ()
moveSlide ps f = do
  modifyIORef ps (\p -> p {currentSlide = f (currentSlide p)})
  displaySlide ps

getTerminalSize :: IO (Int, Int)
getTerminalSize = T.size >>= (\s -> pure $ maybe (50, 20) (\w -> (T.width w, T.height w)) s)

loadPresentation :: String -> IO Presentation
loadPresentation path = do
  slides <- parseSlides path
  (w, h) <- getTerminalSize
  ioRef <- newIORef $ PresentationState slides w (h - 3) 0
  displaySlide ioRef
  let maxSlides = length slides
  let nextSlide = moveSlide ioRef (\x -> if x == maxSlides - 1 then x else x + 1)
  let previousSlide = moveSlide ioRef (\x -> if x == 0 then x else x - 1)
  let advanceSlides = \i -> moveSlide ioRef (\x -> if x + i >= maxSlides - 1 then maxSlides - 1 else x + i)
  let goBackSlides = \i -> moveSlide ioRef (\x -> if x - i <= 0 then 0 else x - i)
  let goToSlide = \i -> moveSlide ioRef (\x -> if i > 0 && i <= maxSlides then i - 1 else x)
  let resetSize = getTerminalSize >>= (\(w, h) -> modifyIORef ioRef (\p -> p {width = w, height = h - 3} ) ) >> displaySlide ioRef
  let setSize w h = modifyIORef ioRef (\p -> p {width = w, height = h}) >> displaySlide ioRef
  let showSize = readIORef ioRef >>= (\ps -> putStrLn $ (show $ width ps) ++ "x" ++ (show $ height ps) )
  let codeFromSlide = filterCode . getCurrentSlide <$> readIORef ioRef
  return $ Presentation {
    n = nextSlide,
    p = previousSlide,
    nn = advanceSlides,
    pp = goBackSlides,
    g = goToSlide,
    resetSize = resetSize,
    setSize = setSize,
    showSize = showSize,
    codeFromSlide = codeFromSlide
  }

-- |calculates left-right or top-bottom padding from the maximum size and content size
padding :: Int -> Int -> (Int, Int)
padding size contentSize = (half, if odd (size - contentSize) then half + 1 else half)
  where half = floor $ (fromIntegral (size - contentSize)) / 2

repeatN n = take n . repeat

formatSlide :: PresentationState -> Slide -> Slide
formatSlide ps content =
  lineOfStars : (repeatN topPadding emptyLine) ++ map centerLine showableContent ++ repeatN bottomPadding emptyLine ++ [lineOfStars]
  where showableContent = filter (not . isHiddenCode) content
        (topPadding, bottomPadding) = padding (height ps) (length showableContent)
        lineOfStars = take (width ps) $ repeat '*'
        emptyLine = '*' : (repeatN ((width ps) -2) ' ') ++ "*"
        (leftCodePadding, _) = padding (width ps) (maxCodeLength showableContent - 3)
        trimCode :: String -> String
        trimCode = drop 3
        centerLine :: String -> String
        centerLine lineContent =
          let (colorizedContent, len) = if isCode lineContent then colorize (trimCode lineContent) else colorize lineContent
              (leftPadding, rightPadding) = if isCode lineContent then (leftCodePadding, (width ps) - leftCodePadding - len) else padding (width ps) len
          in '*' :
             (repeatN (leftPadding - 1) ' ') ++
             colorizedContent ++
             (repeatN (rightPadding - 1) ' ') ++
             "*"

-- foreground colors 30-37, background colors 40-47
color :: Char -> Maybe String
color x = fmap (\c -> "\x1b[" ++ show (if isLower x then c + 30 else c + 40) ++ "m" )
  (case toLower x of  'k' -> Just 0 -- Black
                      'r' -> Just 1 -- Red
                      'g' -> Just 2 -- Green
                      'y' -> Just 3 -- Yellow
                      'b' -> Just 4 -- Blue
                      'm' -> Just 5 -- Magenta
                      'c' -> Just 6 -- Cyan
                      'w' -> Just 7 -- White
                      _   -> Nothing -- No coloring
  )

colorReset = "\x1b[0m"

-- it also takes care of escaping to ;\\k' will be just '\k' and not black color
colorize :: String -> (String, Int)
colorize line = if elem '\\' line then colorize' line [] (length line) else (line, length line)
  where colorize' [] acc len = (acc ++ colorReset, len)
        colorize' ('\\':x:xs) acc len = let maybeColor = color x
                                            colorizedChar = fromMaybe [x] maybeColor
                                        in  colorize' xs (acc ++ colorizedChar) (if isJust maybeColor then len - 2 else len)
        colorize' (x:xs) acc len = colorize' xs (acc ++ [x]) len