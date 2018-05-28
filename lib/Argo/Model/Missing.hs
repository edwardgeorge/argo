{-# LANGUAGE DeriveDataTypeable #-}
module Argo.Model.Missing where
import Data.Aeson as A
import Data.Typeable (Typeable)

data WorkflowStatus = WorkflowStatus
  deriving (Eq, Show, Typeable)

instance A.ToJSON WorkflowStatus where
  toJSON WorkflowStatus = object []

instance A.FromJSON WorkflowStatus where
  parseJSON _ = pure WorkflowStatus
