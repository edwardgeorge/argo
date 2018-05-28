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
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Arguments)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Artifact)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1ArtifactLocation)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1ArtifactoryArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1ArtifactoryAuth)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1DAGTask)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1DAGTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1GitArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1HTTPArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Inputs)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Item)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Metadata)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Outputs)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Parameter)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1RawArtifact)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1ResourceTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1RetryStrategy)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1S3Artifact)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1S3Bucket)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1ScriptTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Sidecar)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1SuspendTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Template)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1ValueFrom)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1Workflow)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1WorkflowList)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1WorkflowSpec)
      propMimeEq MimeJSON (Proxy :: Proxy V1alpha1WorkflowStep)
      
