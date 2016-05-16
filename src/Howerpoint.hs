{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Howerpoint where

import           Control.DeepSeq              (deepseq)
import           Control.Monad                ((>=>), liftM)
import           Data.Char                    (isLower, toLower)
import           Data.IORef
import           Data.List                    (isPrefixOf)
import           Data.List.Split
import           Data.Maybe                   (fromMaybe, isJust, mapMaybe)
import qualified System.Console.Terminal.Size as T
import           System.IO

type Slide = [String]

data Zipper a = Zipper [a] a [a]

extract :: Zipper a -> a
extract (Zipper _ x _) = x

previous :: Zipper a -> [a]
previous (Zipper x _ _) = x

fromList :: [a] -> Zipper a
fromList (x : xs)
  = Zipper [] x xs
fromList []
  = error "Empty list"

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Zipper (l : ls) x rs)
  = Just $ Zipper ls l (x : rs)
moveLeft z
  = Nothing

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Zipper ls x (r : rs))
  = Just $ Zipper (x : ls) r rs
moveRight z
  = Nothing

data PresentationState = PresentationState {
  slides       :: Zipper Slide,
  width        :: Int,
  height       :: Int,
  currentSlide :: Int
}

getCurrentSlide :: PresentationState -> Slide
getCurrentSlide = extract . slides

getPreviousSlides :: PresentationState -> [Slide]
getPreviousSlides = previous . slides

sanitizeCode :: String -> String
sanitizeCode = concatMap (\c -> if c == '"' then "\\\"" else [c])

filterCode :: Slide -> [String]
filterCode = mapMaybe processLine
  where
    processLine :: String -> Maybe String
    processLine line
      | ">>>" `isPrefixOf` line = Just $ drop 3 line
      | "L>>" `isPrefixOf` line = Just $ "let " ++ drop 3 line
      | "H>>" `isPrefixOf` line = Just $ drop 3 line
      | otherwise               = Nothing

isCode :: String -> Bool
isCode s = isPrefixOf ">>>" s || isPrefixOf "L>>" s

isHiddenCode :: String -> Bool
isHiddenCode = isPrefixOf "H>>"

maxCodeLength :: Slide -> Int
maxCodeLength = maximum . map (subtract 3 . length) . filter isCode

printCodeAndExecute :: [String] -> String
printCodeAndExecute code = unlines (map (\x -> "putStrLn \"> " ++ sanitizeCode x ++ "\"") code) ++ unlines code

help :: IO ()
help = mapM_ (putStrLn . fst . colorize)
       [ "\\k\\WTo load test presentation run \\Y:loadTestPresentation"
       , "\\k\\WTo load your own presentation run \\Y:loadPresentation <PATH-TO-YOUR-PRESENTATION>"]


data Presentation = Presentation {
  n                      :: IO (),
  p                      :: IO (),
  nn                     :: Int -> IO (),
  pp                     :: Int -> IO (),
  g                      :: Int -> IO (),
  resetSize              :: IO (),
  setSize                :: Int -> Int -> IO (),
  showSize               :: IO (),
  codeFromSlide          :: IO [String],
  codeFromPreviousSlides :: IO [[String]]
}

parseSlides :: String -> IO (Zipper Slide)
parseSlides path = do
    h <- openFile path ReadMode
    slides' <- splitOn ["---"] . lines <$> hGetContents h
    slides' `deepseq` hClose h
    return $ fromList slides'

displaySlide :: IORef PresentationState -> IO ()
displaySlide ps = do
  ps' <- readIORef ps
  mapM_ putStrLn (formatSlide ps' (getCurrentSlide ps'))

moveSlide :: (Int -> Int) -> PresentationState -> PresentationState
moveSlide f ps
  = maybe ps (\ss -> ps {slides = ss, currentSlide = newpos}) slides'
  where ntimes n fun = foldr (>=>) return (replicate n fun)
        pos = currentSlide ps
        newpos = f pos
        dir = if pos < newpos
              then moveRight
              else moveLeft
        slides' = ntimes (abs $ newpos - pos) dir (slides ps)

getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  s <- T.size
  return $ maybe (50, 20) (\w -> (T.width w, T.height w)) s

loadPresentation :: String -> IO Presentation
loadPresentation path = do
  slides' <- parseSlides path
  (w, h) <- getTerminalSize
  ioRef <- newIORef $ PresentationState slides' w (h - 3) 0

  displaySlide ioRef
  let
    g  = moveWith . const
    nn = moveWith . (+)
    pp = moveWith . subtract
    p  = pp 1
    n  = nn 1

    moveWith f
      = modifyIORef ioRef (moveSlide f) >> displaySlide ioRef

    resetSize = do
      (w', h') <- getTerminalSize
      setSize w' (h' - 3)

    setSize w' h' = do
      modifyIORef ioRef (\p' -> p' {width = w', height = h'})
      displaySlide ioRef

    showSize = do
      ps <- readIORef ioRef
      putStrLn $ show (width ps) ++ "x" ++ show (height ps)

    codeFromSlide = filterCode . getCurrentSlide <$> readIORef ioRef

    codeFromPreviousSlides = liftM (map filterCode) $ getPreviousSlides <$> readIORef ioRef

  return Presentation {..}

-- |calculates left-right or top-bottom padding from the maximum size and content size
padding :: Int -> Int -> (Int, Int)
padding size contentSize = (half, if odd (size - contentSize) then half + 1 else half)
  where half = (size - contentSize) `div` 2

formatSlide :: PresentationState -> Slide -> Slide
formatSlide ps content =
  lineOfStars : replicate topPadding emptyLine ++ map centerLine showableContent ++ replicate bottomPadding emptyLine ++ [lineOfStars]
  where showableContent = filter (not . isHiddenCode) content
        (topPadding, bottomPadding) = padding (height ps) (length showableContent)
        lineOfStars = replicate (width ps) '*'
        emptyLine = '*' : replicate (width ps - 2) ' ' ++ "*"
        (leftCodePadding, _) = padding (width ps) (maxCodeLength showableContent - 3)
        trimCode :: String -> String
        trimCode = drop 3
        centerLine :: String -> String
        centerLine lineContent =
          let (colorizedContent, len) =
                if isCode lineContent
                    then colorize (trimCode lineContent)
                    else colorize lineContent
              (leftPadding, rightPadding) =
                if isCode lineContent
                  then (leftCodePadding, width ps - leftCodePadding - len)
                  else padding (width ps) len
          in '*' :
             replicate (leftPadding - 1) ' ' ++
             colorizedContent ++
             replicate (rightPadding - 1) ' ' ++
             "*"

-- foreground colors 30-37, background colors 40-47
color :: Char -> Maybe String
color '0'
  = Just colorReset
color x
  = fmap (\c -> "\x1b[" ++ show (if isLower x then c + 30 else c + 40) ++ "m")
         (lookup (toLower x) colors)
  where
    colors :: [(Char, Int)]
    colors =
      [ ('k', 0) -- Black
      , ('r', 1) -- Red
      , ('g', 2) -- Green
      , ('y', 3) -- Yellow
      , ('b', 4) -- Blue
      , ('m', 5) -- Magenta
      , ('c', 6) -- Cyan
      , ('w', 7) -- White
      ]

colorReset :: String
colorReset = "\x1b[0m"

-- it also takes care of escaping to '\\k' will be just '\k' and not black color
colorize :: String -> (String, Int)
colorize line =
  if '\\' `elem` line
    then colorize' line [] (length line)
    else (line, length line)
  where colorize' [] acc len = (acc ++ colorReset, len)
        colorize' ('\\':x:xs) acc len =
          let maybeColor = color x
              colorizedChar = fromMaybe [x] maybeColor
              len' = if isJust maybeColor then len - 2 else len
          in  colorize' xs (acc ++ colorizedChar) len'
        colorize' (x:xs) acc len = colorize' xs (acc ++ [x]) len
