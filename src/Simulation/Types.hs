{-# LANGUAGE TemplateHaskell #-}

module Simulation.Types where

import Lens.Micro ( (^.) )
import Lens.Micro.TH ( makeLenses )

data CustomerType = Yellow | Red | Blue deriving (Show)

data Result = Result
  { avgWaitTime :: Double,
    maxWaitTime :: Time,
    avgQueueLength :: Double,
    maxQueueLength :: Int
  }
  deriving (Show)

type Time = Double

data Customer = Customer
  { arrivalTime :: Time,
    processingTime :: Time
  }
  deriving (Show)

makeLenses ''Customer

data BankState = BankState
  { _customersWaiting :: [Customer],
    _numberServedCustomers :: Int,
    _sumLengthOfQueue :: Int,
    _maxLengthOfQueue :: Int,
    _sumWaitingTime :: Time,
    _maxWaitingTime :: Time
  } --deriving (Show)

makeLenses ''BankState

instance Show BankState where
  show bs = unlines info
    where
      info =
        [ "",
          "Waiting Customers: " <> show (length $ bs ^. customersWaiting),
          "Number Of Served Customers: " <> show (bs ^. numberServedCustomers)
        ]

data SimulationState = SimulationState
  { _generatedCustomers :: [Customer],
    _timeNow :: Time,
    _bankState :: BankState
  } --deriving (Show)

makeLenses ''SimulationState

instance Show SimulationState where
  show bs = unlines info
    where
      info =
        [ "",
          "Generated Customers: " <> show (length $ bs ^. generatedCustomers),
          "Time Now: " <> show (bs ^. timeNow),
          "Bank state:" <> show (bs ^. bankState)
        ]