{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simulation.Model where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
  ( atomically,
    newTVar,
    readTVar,
    writeTVar,
  )
import qualified Control.Concurrent.Thread as T (forkIO)
import Control.Monad (forever)
import Lens.Micro ((%~), (&), (.~), (^.))
import Log (logInfo)
import Simulation.RandomTimeGeneration
  ( generateCustomerProcessingTime,
    generateInterArrivalTime,
  )
import Types
    ( Customer(Customer, processingTime, arrivalTime),
      Time,
      Result(..),
      CustomerType,
      BankState(..),
      SimulationState(SimulationState, _generatedCustomers, _timeNow,
                      _bankState),
      customersWaiting,
      maxLengthOfQueue,
      maxWaitingTime,
      numberServedCustomers,
      sumLengthOfQueue,
      sumWaitingTime,
      bankState,
      generatedCustomers,
      timeNow )

initBankState :: BankState
initBankState =
  BankState
    { _customersWaiting = [],
      _numberServedCustomers = 0,
      _sumLengthOfQueue = 0,
      _maxLengthOfQueue = 0,
      _sumWaitingTime = 0,
      _maxWaitingTime = 0
    }

runSimulation :: Int -> CustomerType -> IO Result
runSimulation count customerType = do
  -- generate customers
  iaTimes :: [Time] <- generateInterArrivalTime count
  -- calculate arrival time by using scanl1 function
  let aTimes = scanl1 (\acc x -> acc + x) iaTimes
  pTimes :: [Time] <- generateCustomerProcessingTime count customerType
  let customers = zipWith Customer aTimes pTimes

  -- initialize state
  sharedState <-
    atomically $
      newTVar $
        SimulationState
          { _generatedCustomers = customers,
            _timeNow = 0,
            _bankState = initBankState
          }

  -- parallel process CustomerService
  pid2 <- forkIO $
    forever $ do
      atomically $ do
        s <- readTVar sharedState
        s' <-
          if null (s ^. bankState ^. customersWaiting)
            then do
              if null (s ^. generatedCustomers)
                then pure s
                else do
                  let customer = s ^. generatedCustomers & head
                  pure $
                    s
                      & timeNow .~ (arrivalTime customer)
                      & generatedCustomers %~ drop 1
                      & (bankState . customersWaiting) %~ ((++) [customer])
            else do
              let customer = s ^. bankState ^. customersWaiting & head
              let waitingTime = (s ^. timeNow) - (arrivalTime customer)              
              let lengthOfQueue = s ^. bankState ^. customersWaiting & length
              pure $
                s
                  & timeNow %~ ((+) (processingTime customer))
                  & (bankState . sumWaitingTime) %~ ((+) waitingTime)
                  & (bankState . maxWaitingTime) %~ ((max waitingTime))
                  & (bankState . sumLengthOfQueue) %~ ((+) lengthOfQueue)
                  & (bankState . maxLengthOfQueue) %~ (max lengthOfQueue)
                  & (bankState . customersWaiting) %~ (drop 1)
                  & (bankState . numberServedCustomers) %~ ((+) 1)

        writeTVar sharedState s'

  -- parallel process CustomerArrival
  pid1 <- forkIO $
    forever $ do
      atomically $ do
        s <- readTVar sharedState
        s' <-
          if null (s ^. generatedCustomers)
            then pure s
            else do
              let customer = s ^. generatedCustomers & head
              let lengthOfQueue = s ^. bankState ^. customersWaiting & length
              if (arrivalTime customer) > (s ^. timeNow)
                then pure s
                else
                  pure $
                    s
                      & generatedCustomers %~ drop 1
                      & (bankState . customersWaiting) %~ (++ [customer])
        writeTVar sharedState s'

  let loop = do
        s <- atomically $ readTVar sharedState
        let stopSimulationBool = null (s ^. generatedCustomers) && null (s ^. bankState ^. customersWaiting)
        if stopSimulationBool
          then do
            killThread pid1
            killThread pid2
            pure $ summary (s ^. bankState)
          else do
            loop

  -- parallel process StopSimulation
  (pid, resultIO) <- T.forkIO $ loop

  result <- resultIO
  case result of
    Right r -> pure r
    Left l -> error $ show l

summary :: BankState -> Result
summary BankState {..} =
  Result
    { avgWaitTime = _sumWaitingTime / numberServedCustomers,
      maxWaitTime = _maxWaitingTime,
      avgQueueLength = (fromIntegral _sumLengthOfQueue) / numberServedCustomers,
      maxQueueLength = _maxLengthOfQueue
    }
  where
    numberServedCustomers = fromIntegral _numberServedCustomers
