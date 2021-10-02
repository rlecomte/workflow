{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Workflow where

import Types

newtype WorkflowName = WorkflowName String deriving (Eq, Show)

newtype StepName = StepName String deriving (Eq, Show)

newtype Compensation = Compensation (IO ())

data VersionConflict = VersionConflict { expected :: Version, actual :: Version } deriving (Eq, Show)

data WorkflowInstr e a where
    Step :: Payload e a => StepName -> IO (Compensation, a) -> WorkflowInstr e a

data Workflow e input output = Workflow { name :: WorkflowName
                                        , program :: input -> WorkflowInstr e output
                                        }

class Payload e a where
    encodePayload :: a -> e
    decodePayload :: e -> Either String a

data StoreInstr e a where
    WorkflowStarted   :: Payload e input => WorkflowName
                                         -> input
                                         -> StoreInstr e (RunId, EventId)

    WorkflowCompleted :: RunId
                      -> StoreInstr e EventId

    WorkflowResumed   :: RunId
                      -> Version
                      -> StoreInstr e (Either VersionConflict EventId)

    WorkflowFailed    :: RunId
                      -> StoreInstr e EventId

    CompensationStarted :: RunId
                        -> StoreInstr e EventId

    CompensationFailed :: RunId
                       -> StoreInstr e EventId

    CompensationCompleted :: RunId
                          -> StoreInstr e EventId

    StepStarted       :: RunId
                      -> StepName
                      -> EventId -- parent event
                      -> StoreInstr e EventId

    StepCompleted     :: Payload e a => RunId
                                     -> StepName
                                     -> a
                                     -> EventId -- parent event
                                     -> StoreInstr e EventId

    StepFailed        :: RunId
                      -> StepName
                      -> WorkflowError
                      -> StoreInstr e EventId

    StepCompensationStarted :: RunId
                            -> StepName
                            -> StoreInstr e EventId

    StepCompensationFailed :: RunId
                           -> StepName
                           -> WorkflowError
                           -> StoreInstr e EventId

    StepCompensationCompleted :: RunId
                              -> StepName
                              -> StoreInstr e EventId
