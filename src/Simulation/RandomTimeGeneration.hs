module Simulation.RandomTimeGeneration where

import Control.Monad (replicateM)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import Simulation.Types ( Time, CustomerType(..) )

-- We know cumulative distribution function (CDF) is F(t) = 1 - e ^ ( -t / a)   a = 100
-- Probability density function (PDF) is f (t) = (1/a) * e ^ ( -t / a)

-- pdfInterArrivalTime t =  (1/a) * e ^ ( -t / a)
--   where a = 100

pdfProcessingTime :: (Integral b1, Integral b2, Num a) => b1 -> b2 -> a -> a
pdfProcessingTime a b x = p * x ^ (a - 1) * (1 - x) ^ (b - 1)
  where
    p = 200

-- generateInterArrivalTime
generateInterArrivalTime :: Int -> IO [Time]
generateInterArrivalTime n = do
  gen <- MWC.create
  times <- replicateM n $ MWC.exponential 100 gen
  pure times

generateProcessingTime :: Int -> Int -> Int -> IO [Time]
generateProcessingTime alpha beta n = do
  gen <- MWC.create
  times <- replicateM n $ MWC.uniform gen
  pure $ pdfProcessingTime alpha beta <$> times

generateCustomerProcessingTime :: Int -> CustomerType -> IO [Time]
generateCustomerProcessingTime n customerType = case customerType of
  Yellow -> generateProcessingTime 2 5 n
  Red -> generateProcessingTime 2 2 n
  Blue -> generateProcessingTime 5 1 n