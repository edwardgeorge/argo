{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import Argo.Model
import Argo.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary Arguments where
  arbitrary =
    Arguments
      <$> arbitrary -- argumentsArtifacts :: Maybe [Artifact]
      <*> arbitrary -- argumentsParameters :: Maybe [Parameter]
    
instance Arbitrary Artifact where
  arbitrary =
    Artifact
      <$> arbitrary -- artifactArtifactory :: Maybe ArtifactoryArtifact
      <*> arbitrary -- artifactFrom :: Maybe Text
      <*> arbitrary -- artifactGit :: Maybe GitArtifact
      <*> arbitrary -- artifactGlobalName :: Maybe Text
      <*> arbitrary -- artifactHttp :: Maybe HTTPArtifact
      <*> arbitrary -- artifactMode :: Maybe Int
      <*> arbitrary -- artifactName :: Text
      <*> arbitrary -- artifactPath :: Maybe Text
      <*> arbitrary -- artifactRaw :: Maybe RawArtifact
      <*> arbitrary -- artifactS3 :: Maybe S3Artifact
    
instance Arbitrary ArtifactLocation where
  arbitrary =
    ArtifactLocation
      <$> arbitrary -- artifactLocationArtifactory :: Maybe ArtifactoryArtifact
      <*> arbitrary -- artifactLocationGit :: Maybe GitArtifact
      <*> arbitrary -- artifactLocationHttp :: Maybe HTTPArtifact
      <*> arbitrary -- artifactLocationRaw :: Maybe RawArtifact
      <*> arbitrary -- artifactLocationS3 :: Maybe S3Artifact
    
instance Arbitrary ArtifactoryArtifact where
  arbitrary =
    ArtifactoryArtifact
      <$> arbitrary -- artifactoryArtifactPasswordSecret :: Maybe V1SecretKeySelector
      <*> arbitrary -- artifactoryArtifactUrl :: Text
      <*> arbitrary -- artifactoryArtifactUsernameSecret :: Maybe V1SecretKeySelector
    
instance Arbitrary ArtifactoryAuth where
  arbitrary =
    ArtifactoryAuth
      <$> arbitrary -- artifactoryAuthPasswordSecret :: Maybe V1SecretKeySelector
      <*> arbitrary -- artifactoryAuthUsernameSecret :: Maybe V1SecretKeySelector
    
instance Arbitrary DAGTask where
  arbitrary =
    DAGTask
      <$> arbitrary -- dAGTaskArguments :: Maybe Arguments
      <*> arbitrary -- dAGTaskDependencies :: Maybe [Text]
      <*> arbitrary -- dAGTaskName :: Text
      <*> arbitrary -- dAGTaskTemplate :: Text
    
instance Arbitrary DAGTemplate where
  arbitrary =
    DAGTemplate
      <$> arbitrary -- dAGTemplateTarget :: Maybe Text
      <*> arbitrary -- dAGTemplateTasks :: [DAGTask]
    
instance Arbitrary GitArtifact where
  arbitrary =
    GitArtifact
      <$> arbitrary -- gitArtifactPasswordSecret :: Maybe V1SecretKeySelector
      <*> arbitrary -- gitArtifactRepo :: Text
      <*> arbitrary -- gitArtifactRevision :: Maybe Text
      <*> arbitrary -- gitArtifactUsernameSecret :: Maybe V1SecretKeySelector
    
instance Arbitrary HTTPArtifact where
  arbitrary =
    HTTPArtifact
      <$> arbitrary -- hTTPArtifactUrl :: Text
    
instance Arbitrary Inputs where
  arbitrary =
    Inputs
      <$> arbitrary -- inputsArtifacts :: Maybe [Artifact]
      <*> arbitrary -- inputsParameters :: Maybe [Parameter]
    
instance Arbitrary Item where
  arbitrary =
    Item <$> arbitrary
instance Arbitrary Metadata where
  arbitrary =
    Metadata
      <$> arbitrary -- metadataAnnotations :: Maybe (Map.Map String Text)
      <*> arbitrary -- metadataLabels :: Maybe (Map.Map String Text)
    
instance Arbitrary NodeStatus where
  arbitrary =
    
    pure NodeStatus
     
instance Arbitrary Outputs where
  arbitrary =
    Outputs
      <$> arbitrary -- outputsArtifacts :: Maybe [Artifact]
      <*> arbitrary -- outputsParameters :: Maybe [Parameter]
      <*> arbitrary -- outputsResult :: Maybe Text
    
instance Arbitrary Parameter where
  arbitrary =
    Parameter
      <$> arbitrary -- parameterDefault :: Maybe Text
      <*> arbitrary -- parameterGlobalName :: Maybe Text
      <*> arbitrary -- parameterName :: Text
      <*> arbitrary -- parameterValue :: Maybe Text
      <*> arbitrary -- parameterValueFrom :: Maybe ValueFrom
    
instance Arbitrary RawArtifact where
  arbitrary =
    RawArtifact
      <$> arbitrary -- rawArtifactData :: Text
    
instance Arbitrary ResourceTemplate where
  arbitrary =
    ResourceTemplate
      <$> arbitrary -- resourceTemplateAction :: Text
      <*> arbitrary -- resourceTemplateFailureCondition :: Maybe Text
      <*> arbitrary -- resourceTemplateManifest :: Text
      <*> arbitrary -- resourceTemplateSuccessCondition :: Maybe Text
    
instance Arbitrary RetryStrategy where
  arbitrary =
    RetryStrategy
      <$> arbitrary -- retryStrategyLimit :: Maybe Int
    
instance Arbitrary S3Artifact where
  arbitrary =
    S3Artifact
      <$> arbitrary -- s3ArtifactAccessKeySecret :: V1SecretKeySelector
      <*> arbitrary -- s3ArtifactBucket :: Text
      <*> arbitrary -- s3ArtifactEndpoint :: Text
      <*> arbitrary -- s3ArtifactInsecure :: Maybe Bool
      <*> arbitrary -- s3ArtifactKey :: Text
      <*> arbitrary -- s3ArtifactRegion :: Maybe Text
      <*> arbitrary -- s3ArtifactSecretKeySecret :: V1SecretKeySelector
    
instance Arbitrary S3Bucket where
  arbitrary =
    S3Bucket
      <$> arbitrary -- s3BucketAccessKeySecret :: V1SecretKeySelector
      <*> arbitrary -- s3BucketBucket :: Text
      <*> arbitrary -- s3BucketEndpoint :: Text
      <*> arbitrary -- s3BucketInsecure :: Maybe Bool
      <*> arbitrary -- s3BucketRegion :: Maybe Text
      <*> arbitrary -- s3BucketSecretKeySecret :: V1SecretKeySelector
    
instance Arbitrary ScriptTemplate where
  arbitrary =
    ScriptTemplate
      <$> arbitrary -- scriptTemplateArgs :: Maybe [Text]
      <*> arbitrary -- scriptTemplateCommand :: Maybe [Text]
      <*> arbitrary -- scriptTemplateEnv :: Maybe [V1EnvVar]
      <*> arbitrary -- scriptTemplateEnvFrom :: Maybe [V1EnvFromSource]
      <*> arbitrary -- scriptTemplateImage :: Maybe Text
      <*> arbitrary -- scriptTemplateImagePullPolicy :: Maybe Text
      <*> arbitrary -- scriptTemplateLifecycle :: Maybe V1Lifecycle
      <*> arbitrary -- scriptTemplateLivenessProbe :: Maybe V1Probe
      <*> arbitrary -- scriptTemplateName :: Text
      <*> arbitrary -- scriptTemplatePorts :: Maybe [V1ContainerPort]
      <*> arbitrary -- scriptTemplateReadinessProbe :: Maybe V1Probe
      <*> arbitrary -- scriptTemplateResources :: Maybe V1ResourceRequirements
      <*> arbitrary -- scriptTemplateSecurityContext :: Maybe V1SecurityContext
      <*> arbitrary -- scriptTemplateSource :: Text
      <*> arbitrary -- scriptTemplateStdin :: Maybe Bool
      <*> arbitrary -- scriptTemplateStdinOnce :: Maybe Bool
      <*> arbitrary -- scriptTemplateTerminationMessagePath :: Maybe Text
      <*> arbitrary -- scriptTemplateTerminationMessagePolicy :: Maybe Text
      <*> arbitrary -- scriptTemplateTty :: Maybe Bool
      <*> arbitrary -- scriptTemplateVolumeDevices :: Maybe [V1VolumeDevice]
      <*> arbitrary -- scriptTemplateVolumeMounts :: Maybe [V1VolumeMount]
      <*> arbitrary -- scriptTemplateWorkingDir :: Maybe Text
    
instance Arbitrary Sidecar where
  arbitrary =
    Sidecar
      <$> arbitrary -- sidecarArgs :: Maybe [Text]
      <*> arbitrary -- sidecarCommand :: Maybe [Text]
      <*> arbitrary -- sidecarEnv :: Maybe [V1EnvVar]
      <*> arbitrary -- sidecarEnvFrom :: Maybe [V1EnvFromSource]
      <*> arbitrary -- sidecarImage :: Maybe Text
      <*> arbitrary -- sidecarImagePullPolicy :: Maybe Text
      <*> arbitrary -- sidecarLifecycle :: Maybe V1Lifecycle
      <*> arbitrary -- sidecarLivenessProbe :: Maybe V1Probe
      <*> arbitrary -- sidecarMirrorVolumeMounts :: Maybe Bool
      <*> arbitrary -- sidecarName :: Text
      <*> arbitrary -- sidecarPorts :: Maybe [V1ContainerPort]
      <*> arbitrary -- sidecarReadinessProbe :: Maybe V1Probe
      <*> arbitrary -- sidecarResources :: Maybe V1ResourceRequirements
      <*> arbitrary -- sidecarSecurityContext :: Maybe V1SecurityContext
      <*> arbitrary -- sidecarStdin :: Maybe Bool
      <*> arbitrary -- sidecarStdinOnce :: Maybe Bool
      <*> arbitrary -- sidecarTerminationMessagePath :: Maybe Text
      <*> arbitrary -- sidecarTerminationMessagePolicy :: Maybe Text
      <*> arbitrary -- sidecarTty :: Maybe Bool
      <*> arbitrary -- sidecarVolumeDevices :: Maybe [V1VolumeDevice]
      <*> arbitrary -- sidecarVolumeMounts :: Maybe [V1VolumeMount]
      <*> arbitrary -- sidecarWorkingDir :: Maybe Text
    
instance Arbitrary SuspendTemplate where
  arbitrary =
    
    pure SuspendTemplate
     
instance Arbitrary Template where
  arbitrary =
    Template
      <$> arbitrary -- templateActiveDeadlineSeconds :: Maybe Integer
      <*> arbitrary -- templateAffinity :: Maybe V1Affinity
      <*> arbitrary -- templateArchiveLocation :: Maybe ArtifactLocation
      <*> arbitrary -- templateContainer :: Maybe V1Container
      <*> arbitrary -- templateDaemon :: Maybe Bool
      <*> arbitrary -- templateDag :: Maybe DAGTemplate
      <*> arbitrary -- templateInputs :: Maybe Inputs
      <*> arbitrary -- templateMetadata :: Maybe Metadata
      <*> arbitrary -- templateName :: Text
      <*> arbitrary -- templateNodeSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- templateOutputs :: Maybe Outputs
      <*> arbitrary -- templateParallelism :: Maybe Integer
      <*> arbitrary -- templateResource :: Maybe ResourceTemplate
      <*> arbitrary -- templateRetryStrategy :: Maybe RetryStrategy
      <*> arbitrary -- templateScript :: Maybe ScriptTemplate
      <*> arbitrary -- templateSidecars :: Maybe [Sidecar]
      <*> arbitrary -- templateSteps :: Maybe [[WorkflowStep]]
      <*> arbitrary -- templateSuspend :: Maybe SuspendTemplate
      <*> arbitrary -- templateTolerations :: Maybe [V1Toleration]
    
instance Arbitrary ValueFrom where
  arbitrary =
    ValueFrom
      <$> arbitrary -- valueFromJqFilter :: Maybe Text
      <*> arbitrary -- valueFromJsonPath :: Maybe Text
      <*> arbitrary -- valueFromParameter :: Maybe Text
      <*> arbitrary -- valueFromPath :: Maybe Text
    
instance Arbitrary Workflow where
  arbitrary =
    Workflow
      <$> arbitrary -- workflowApiVersion :: Maybe Text
      <*> arbitrary -- workflowKind :: Maybe Text
      <*> arbitrary -- workflowMetadata :: V1ObjectMeta
      <*> arbitrary -- workflowSpec :: WorkflowSpec
      <*> arbitrary -- workflowStatus :: Maybe WorkflowStatus
    
instance Arbitrary WorkflowList where
  arbitrary =
    WorkflowList
      <$> arbitrary -- workflowListApiVersion :: Maybe Text
      <*> arbitrary -- workflowListItems :: [Workflow]
      <*> arbitrary -- workflowListKind :: Maybe Text
      <*> arbitrary -- workflowListMetadata :: V1ListMeta
    
instance Arbitrary WorkflowSpec where
  arbitrary =
    WorkflowSpec
      <$> arbitrary -- workflowSpecAffinity :: Maybe V1Affinity
      <*> arbitrary -- workflowSpecArguments :: Maybe Arguments
      <*> arbitrary -- workflowSpecEntrypoint :: Text
      <*> arbitrary -- workflowSpecImagePullSecrets :: Maybe [V1LocalObjectReference]
      <*> arbitrary -- workflowSpecNodeSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- workflowSpecOnExit :: Maybe Text
      <*> arbitrary -- workflowSpecParallelism :: Maybe Integer
      <*> arbitrary -- workflowSpecServiceAccountName :: Maybe Text
      <*> arbitrary -- workflowSpecSuspend :: Maybe Bool
      <*> arbitrary -- workflowSpecTemplates :: [Template]
      <*> arbitrary -- workflowSpecTolerations :: Maybe [V1Toleration]
      <*> arbitrary -- workflowSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
      <*> arbitrary -- workflowSpecVolumes :: Maybe [V1Volume]
    
instance Arbitrary WorkflowStatus where
  arbitrary =
    WorkflowStatus
      <$> arbitrary -- workflowStatusPhase :: Maybe Text
      <*> arbitrary -- workflowStatusStartedAt :: Maybe DateTime
      <*> arbitrary -- workflowStatusFinishedAt :: Maybe DateTime
      <*> arbitrary -- workflowStatusMessage :: Maybe Text
      <*> arbitrary -- workflowStatusNodes :: Maybe (Map.Map String NodeStatus)
      <*> arbitrary -- workflowStatusPersistentVolumeClaims :: Maybe [V1Volume]
      <*> arbitrary -- workflowStatusOutputs :: Maybe Outputs
    
instance Arbitrary WorkflowStep where
  arbitrary =
    WorkflowStep
      <$> arbitrary -- workflowStepArguments :: Maybe Arguments
      <*> arbitrary -- workflowStepName :: Maybe Text
      <*> arbitrary -- workflowStepTemplate :: Maybe Text
      <*> arbitrary -- workflowStepWhen :: Maybe Text
      <*> arbitrary -- workflowStepWithItems :: Maybe [Item]
      <*> arbitrary -- workflowStepWithParam :: Maybe Text
    



instance Arbitrary E'Phase where
  arbitrary = arbitraryBoundedEnum
