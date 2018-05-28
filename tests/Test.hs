{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Argo.Model
import Argo.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 5) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy Arguments)
      propMimeEq MimeJSON (Proxy :: Proxy Artifact)
      propMimeEq MimeJSON (Proxy :: Proxy ArtifactLocation)
      propMimeEq MimeJSON (Proxy :: Proxy ArtifactoryArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy ArtifactoryAuth)
      propMimeEq MimeJSON (Proxy :: Proxy DAGTask)
      propMimeEq MimeJSON (Proxy :: Proxy DAGTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy GitArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy HTTPArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy Inputs)
      propMimeEq MimeJSON (Proxy :: Proxy Item)
      propMimeEq MimeJSON (Proxy :: Proxy Metadata)
      propMimeEq MimeJSON (Proxy :: Proxy NodeStatus)
      propMimeEq MimeJSON (Proxy :: Proxy Outputs)
      propMimeEq MimeJSON (Proxy :: Proxy Parameter)
      propMimeEq MimeJSON (Proxy :: Proxy RawArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy ResourceTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy RetryStrategy)
      propMimeEq MimeJSON (Proxy :: Proxy S3Artifact)
      propMimeEq MimeJSON (Proxy :: Proxy S3Bucket)
      propMimeEq MimeJSON (Proxy :: Proxy ScriptTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy Sidecar)
      propMimeEq MimeJSON (Proxy :: Proxy SuspendTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy Template)
      propMimeEq MimeJSON (Proxy :: Proxy ValueFrom)
      propMimeEq MimeJSON (Proxy :: Proxy Workflow)
      propMimeEq MimeJSON (Proxy :: Proxy WorkflowList)
      propMimeEq MimeJSON (Proxy :: Proxy WorkflowSpec)
      propMimeEq MimeJSON (Proxy :: Proxy WorkflowStatus)
      propMimeEq MimeJSON (Proxy :: Proxy WorkflowStep)
      
