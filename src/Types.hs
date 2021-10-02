{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE DeriveFunctor #-}

module Types where

import Data.Time.Clock (UTCTime)

newtype EventId = EventId String deriving (Show, Eq)

newtype RunId = RunId String deriving (Show, Eq)

newtype Version = Version Int deriving (Show, Eq)

newtype WorkflowError = WorkflowError String deriving (Show, Eq)

data Event a = Event { id :: EventId
                     , runId :: RunId
                     , version :: Version
                     , timestanp :: UTCTime
                     , payload :: WorflowEvent a
                     } deriving (Eq, Show, Functor)

data WorflowEvent a =
    Started (StartedPayload a)                                 |
    Suspended                                                  |
    Resumed                                                    |
    Completed                                                  |
    Failed                                                     |
    CompensationStarted                                        |
    CompensationCompleted                                      |
    CompensationFailed                                         |
    StepStarted StepStartedPayload                             |
    StepCompleted (StepCompletedPayload a)                     |
    StepFailed StepFailedPayload                               |
    StepCompensationStarted StepCompensationStartedPayload     |
    StepCompensationCompleted StepCompensationCompletedPayload |
    StepCompensationFailed StepCompensationFailedPayload
    deriving (Eq, Show, Functor)

data StartedPayload a = StartedPayload { worklow :: String
                                       , input :: a
                                       } deriving (Show, Eq, Functor)

data StepStartedPayload = StepStartedPayload { step :: String
                                             , correlationId :: EventId
                                             } deriving (Show, Eq)

data StepCompletedPayload a = StepCompletedPayload { step :: String
                                                   , stepStartedId :: EventId
                                                   , payload :: a
                                                   } deriving (Show, Eq, Functor)

data StepFailedPayload = StepFailedPayload { step :: String
                                           , stepStartedId :: EventId
                                           , errorMessage :: WorkflowError
                                           } deriving (Show, Eq)

data StepCompensationStartedPayload = StepCompensationStartedPayload { step :: String
                                                                     , stepStartedId :: EventId
                                                                     } deriving (Show, Eq)

data StepCompensationCompletedPayload = StepCompensationCompletedPayload { step :: String
                                                                         , stepStartedId :: EventId
                                                                         } deriving (Show, Eq)

data StepCompensationFailedPayload = StepCompensationFailedPayload { step :: String
                                                                   , stepStartedId :: EventId
                                                                   , errorMessage :: WorkflowError
                                                                   } deriving (Show, Eq)
