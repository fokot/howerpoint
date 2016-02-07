{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import Control.Monad
-- import Data.List.Split
--  >>= (\x -> return $ splitOn "\n" x)

f = "/Users/Frantisek/haskell/howerpoint/test/resources/test_navigation.txt"

data PresentationSetting = PresentationSetting {
  width :: Int,
  height :: Int
}

padding :: Int -> Int -> (Int, Int)
padding size contentSize = (half, if odd (size - contentSize) then half + 1 else half)
  where half = floor $ (fromIntegral (size - contentSize)) / 2


repeatN n = take n . repeat

drawFrame :: PresentationSetting -> [String] -> [String]
drawFrame ps content =
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


loadFile :: String -> IO ()
loadFile path = withFile path ReadMode (\handle -> do
                content :: [String] <- liftM lines $ hGetContents handle
--                 let slides :: [[String]]= content
                let frame :: [String] = drawFrame (PresentationSetting 50 20) content
                mapM putStrLn frame
                return ())
main = putStr "hi"