module Main where

import ConwayGame
import Peggy2Board
import System.Random
import Control.Concurrent
import Control.Monad
import Data.List.Split

main :: IO ()
main = forever $ do
    putStrLn "Starting new game..."
    g <- newStdGen
    let steps = take 100 (iterate step (randomBoard g 25 25))
    sequence (fmap (\board@(Board dta) -> do
            drawBool (concat dta)
            threadDelay 200000        
        ) steps)
    putStrLn "Game Finished"

