module Main where

import Data.List (sortOn)
import Log (logInfo)
import Simulation.Model (runSimulation)
import Simulation.Types
  ( CustomerType (Blue, Red, Yellow),
    Result (..)
  )

main = do
  let count = 10000
      runSimulation' customerType = do
        res <- runSimulation count customerType
        pure (customerType, res)
  results <- mapM runSimulation' [Yellow, Red, Blue]
  let [yellow, red, blue] = map snd results
      close (c, r) = (c, (maxWaitTime r) - (avgWaitTime r))
      results' = map close results
      closest = fst . head . sortOn snd $ results'

  logInfo $ "Given only yellow customers "
  logInfo $ "Average customer waiting time is " ++ (show $ avgWaitTime yellow)
  logInfo $ "Maximum customer waiting time is " ++ (show $ maxWaitTime yellow)
  logInfo $ "Given only red customers "
  logInfo $ "Average queue lengths in-front of the teller is " ++ (show $ avgQueueLength red)
  logInfo $ "Maximum queue lengths in-front of the teller is " ++ (show $ maxQueueLength red)
  logInfo $ "The type of customer that gives the closest value between"
  logInfo $ "the average and maximum customer waiting times is " ++ (show closest)
