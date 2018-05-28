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
 
instance Arbitrary V1alpha1Arguments where
  arbitrary =
    V1alpha1Arguments
      <$> arbitrary -- v1alpha1ArgumentsArtifacts :: Maybe [V1alpha1Artifact]
      <*> arbitrary -- v1alpha1ArgumentsParameters :: Maybe [V1alpha1Parameter]
    
instance Arbitrary V1alpha1Artifact where
  arbitrary =
    V1alpha1Artifact
      <$> arbitrary -- v1alpha1ArtifactArtifactory :: Maybe V1alpha1ArtifactoryArtifact
      <*> arbitrary -- v1alpha1ArtifactFrom :: Maybe Text
      <*> arbitrary -- v1alpha1ArtifactGit :: Maybe V1alpha1GitArtifact
      <*> arbitrary -- v1alpha1ArtifactGlobalName :: Maybe Text
      <*> arbitrary -- v1alpha1ArtifactHttp :: Maybe V1alpha1HTTPArtifact
      <*> arbitrary -- v1alpha1ArtifactMode :: Maybe Int
      <*> arbitrary -- v1alpha1ArtifactName :: Text
      <*> arbitrary -- v1alpha1ArtifactPath :: Maybe Text
      <*> arbitrary -- v1alpha1ArtifactRaw :: Maybe V1alpha1RawArtifact
      <*> arbitrary -- v1alpha1ArtifactS3 :: Maybe V1alpha1S3Artifact
    
instance Arbitrary V1alpha1ArtifactLocation where
  arbitrary =
    V1alpha1ArtifactLocation
      <$> arbitrary -- v1alpha1ArtifactLocationArtifactory :: Maybe V1alpha1ArtifactoryArtifact
      <*> arbitrary -- v1alpha1ArtifactLocationGit :: Maybe V1alpha1GitArtifact
      <*> arbitrary -- v1alpha1ArtifactLocationHttp :: Maybe V1alpha1HTTPArtifact
      <*> arbitrary -- v1alpha1ArtifactLocationRaw :: Maybe V1alpha1RawArtifact
      <*> arbitrary -- v1alpha1ArtifactLocationS3 :: Maybe V1alpha1S3Artifact
    
instance Arbitrary V1alpha1ArtifactoryArtifact where
  arbitrary =
    V1alpha1ArtifactoryArtifact
      <$> arbitrary -- v1alpha1ArtifactoryArtifactPasswordSecret :: Maybe V1SecretKeySelector
      <*> arbitrary -- v1alpha1ArtifactoryArtifactUrl :: Text
      <*> arbitrary -- v1alpha1ArtifactoryArtifactUsernameSecret :: Maybe V1SecretKeySelector
    
instance Arbitrary V1alpha1ArtifactoryAuth where
  arbitrary =
    V1alpha1ArtifactoryAuth
      <$> arbitrary -- v1alpha1ArtifactoryAuthPasswordSecret :: Maybe V1SecretKeySelector
      <*> arbitrary -- v1alpha1ArtifactoryAuthUsernameSecret :: Maybe V1SecretKeySelector
    
instance Arbitrary V1alpha1DAGTask where
  arbitrary =
    V1alpha1DAGTask
      <$> arbitrary -- v1alpha1DAGTaskArguments :: Maybe V1alpha1Arguments
      <*> arbitrary -- v1alpha1DAGTaskDependencies :: Maybe [Text]
      <*> arbitrary -- v1alpha1DAGTaskName :: Text
      <*> arbitrary -- v1alpha1DAGTaskTemplate :: Text
    
instance Arbitrary V1alpha1DAGTemplate where
  arbitrary =
    V1alpha1DAGTemplate
      <$> arbitrary -- v1alpha1DAGTemplateTarget :: Maybe Text
      <*> arbitrary -- v1alpha1DAGTemplateTasks :: [V1alpha1DAGTask]
    
instance Arbitrary V1alpha1GitArtifact where
  arbitrary =
    V1alpha1GitArtifact
      <$> arbitrary -- v1alpha1GitArtifactPasswordSecret :: Maybe V1SecretKeySelector
      <*> arbitrary -- v1alpha1GitArtifactRepo :: Text
      <*> arbitrary -- v1alpha1GitArtifactRevision :: Maybe Text
      <*> arbitrary -- v1alpha1GitArtifactUsernameSecret :: Maybe V1SecretKeySelector
    
instance Arbitrary V1alpha1HTTPArtifact where
  arbitrary =
    V1alpha1HTTPArtifact
      <$> arbitrary -- v1alpha1HTTPArtifactUrl :: Text
    
instance Arbitrary V1alpha1Inputs where
  arbitrary =
    V1alpha1Inputs
      <$> arbitrary -- v1alpha1InputsArtifacts :: Maybe [V1alpha1Artifact]
      <*> arbitrary -- v1alpha1InputsParameters :: Maybe [V1alpha1Parameter]
    
instance Arbitrary V1alpha1Item where
  arbitrary =
    V1alpha1Item <$> arbitrary
instance Arbitrary V1alpha1Metadata where
  arbitrary =
    V1alpha1Metadata
      <$> arbitrary -- v1alpha1MetadataAnnotations :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1alpha1MetadataLabels :: Maybe (Map.Map String Text)
    
instance Arbitrary V1alpha1Outputs where
  arbitrary =
    V1alpha1Outputs
      <$> arbitrary -- v1alpha1OutputsArtifacts :: Maybe [V1alpha1Artifact]
      <*> arbitrary -- v1alpha1OutputsParameters :: Maybe [V1alpha1Parameter]
      <*> arbitrary -- v1alpha1OutputsResult :: Maybe Text
    
instance Arbitrary V1alpha1Parameter where
  arbitrary =
    V1alpha1Parameter
      <$> arbitrary -- v1alpha1ParameterDefault :: Maybe Text
      <*> arbitrary -- v1alpha1ParameterGlobalName :: Maybe Text
      <*> arbitrary -- v1alpha1ParameterName :: Text
      <*> arbitrary -- v1alpha1ParameterValue :: Maybe Text
      <*> arbitrary -- v1alpha1ParameterValueFrom :: Maybe V1alpha1ValueFrom
    
instance Arbitrary V1alpha1RawArtifact where
  arbitrary =
    V1alpha1RawArtifact
      <$> arbitrary -- v1alpha1RawArtifactData :: Text
    
instance Arbitrary V1alpha1ResourceTemplate where
  arbitrary =
    V1alpha1ResourceTemplate
      <$> arbitrary -- v1alpha1ResourceTemplateAction :: Text
      <*> arbitrary -- v1alpha1ResourceTemplateFailureCondition :: Maybe Text
      <*> arbitrary -- v1alpha1ResourceTemplateManifest :: Text
      <*> arbitrary -- v1alpha1ResourceTemplateSuccessCondition :: Maybe Text
    
instance Arbitrary V1alpha1RetryStrategy where
  arbitrary =
    V1alpha1RetryStrategy
      <$> arbitrary -- v1alpha1RetryStrategyLimit :: Maybe Int
    
instance Arbitrary V1alpha1S3Artifact where
  arbitrary =
    V1alpha1S3Artifact
      <$> arbitrary -- v1alpha1S3ArtifactAccessKeySecret :: V1SecretKeySelector
      <*> arbitrary -- v1alpha1S3ArtifactBucket :: Text
      <*> arbitrary -- v1alpha1S3ArtifactEndpoint :: Text
      <*> arbitrary -- v1alpha1S3ArtifactInsecure :: Maybe Bool
      <*> arbitrary -- v1alpha1S3ArtifactKey :: Text
      <*> arbitrary -- v1alpha1S3ArtifactRegion :: Maybe Text
      <*> arbitrary -- v1alpha1S3ArtifactSecretKeySecret :: V1SecretKeySelector
    
instance Arbitrary V1alpha1S3Bucket where
  arbitrary =
    V1alpha1S3Bucket
      <$> arbitrary -- v1alpha1S3BucketAccessKeySecret :: V1SecretKeySelector
      <*> arbitrary -- v1alpha1S3BucketBucket :: Text
      <*> arbitrary -- v1alpha1S3BucketEndpoint :: Text
      <*> arbitrary -- v1alpha1S3BucketInsecure :: Maybe Bool
      <*> arbitrary -- v1alpha1S3BucketRegion :: Maybe Text
      <*> arbitrary -- v1alpha1S3BucketSecretKeySecret :: V1SecretKeySelector
    
instance Arbitrary V1alpha1ScriptTemplate where
  arbitrary =
    V1alpha1ScriptTemplate
      <$> arbitrary -- v1alpha1ScriptTemplateArgs :: Maybe [Text]
      <*> arbitrary -- v1alpha1ScriptTemplateCommand :: Maybe [Text]
      <*> arbitrary -- v1alpha1ScriptTemplateEnv :: Maybe [V1EnvVar]
      <*> arbitrary -- v1alpha1ScriptTemplateEnvFrom :: Maybe [V1EnvFromSource]
      <*> arbitrary -- v1alpha1ScriptTemplateImage :: Maybe Text
      <*> arbitrary -- v1alpha1ScriptTemplateImagePullPolicy :: Maybe Text
      <*> arbitrary -- v1alpha1ScriptTemplateLifecycle :: Maybe V1Lifecycle
      <*> arbitrary -- v1alpha1ScriptTemplateLivenessProbe :: Maybe V1Probe
      <*> arbitrary -- v1alpha1ScriptTemplateName :: Text
      <*> arbitrary -- v1alpha1ScriptTemplatePorts :: Maybe [V1ContainerPort]
      <*> arbitrary -- v1alpha1ScriptTemplateReadinessProbe :: Maybe V1Probe
      <*> arbitrary -- v1alpha1ScriptTemplateResources :: Maybe V1ResourceRequirements
      <*> arbitrary -- v1alpha1ScriptTemplateSecurityContext :: Maybe V1SecurityContext
      <*> arbitrary -- v1alpha1ScriptTemplateSource :: Text
      <*> arbitrary -- v1alpha1ScriptTemplateStdin :: Maybe Bool
      <*> arbitrary -- v1alpha1ScriptTemplateStdinOnce :: Maybe Bool
      <*> arbitrary -- v1alpha1ScriptTemplateTerminationMessagePath :: Maybe Text
      <*> arbitrary -- v1alpha1ScriptTemplateTerminationMessagePolicy :: Maybe Text
      <*> arbitrary -- v1alpha1ScriptTemplateTty :: Maybe Bool
      <*> arbitrary -- v1alpha1ScriptTemplateVolumeDevices :: Maybe [V1VolumeDevice]
      <*> arbitrary -- v1alpha1ScriptTemplateVolumeMounts :: Maybe [V1VolumeMount]
      <*> arbitrary -- v1alpha1ScriptTemplateWorkingDir :: Maybe Text
    
instance Arbitrary V1alpha1Sidecar where
  arbitrary =
    V1alpha1Sidecar
      <$> arbitrary -- v1alpha1SidecarArgs :: Maybe [Text]
      <*> arbitrary -- v1alpha1SidecarCommand :: Maybe [Text]
      <*> arbitrary -- v1alpha1SidecarEnv :: Maybe [V1EnvVar]
      <*> arbitrary -- v1alpha1SidecarEnvFrom :: Maybe [V1EnvFromSource]
      <*> arbitrary -- v1alpha1SidecarImage :: Maybe Text
      <*> arbitrary -- v1alpha1SidecarImagePullPolicy :: Maybe Text
      <*> arbitrary -- v1alpha1SidecarLifecycle :: Maybe V1Lifecycle
      <*> arbitrary -- v1alpha1SidecarLivenessProbe :: Maybe V1Probe
      <*> arbitrary -- v1alpha1SidecarMirrorVolumeMounts :: Maybe Bool
      <*> arbitrary -- v1alpha1SidecarName :: Text
      <*> arbitrary -- v1alpha1SidecarPorts :: Maybe [V1ContainerPort]
      <*> arbitrary -- v1alpha1SidecarReadinessProbe :: Maybe V1Probe
      <*> arbitrary -- v1alpha1SidecarResources :: Maybe V1ResourceRequirements
      <*> arbitrary -- v1alpha1SidecarSecurityContext :: Maybe V1SecurityContext
      <*> arbitrary -- v1alpha1SidecarStdin :: Maybe Bool
      <*> arbitrary -- v1alpha1SidecarStdinOnce :: Maybe Bool
      <*> arbitrary -- v1alpha1SidecarTerminationMessagePath :: Maybe Text
      <*> arbitrary -- v1alpha1SidecarTerminationMessagePolicy :: Maybe Text
      <*> arbitrary -- v1alpha1SidecarTty :: Maybe Bool
      <*> arbitrary -- v1alpha1SidecarVolumeDevices :: Maybe [V1VolumeDevice]
      <*> arbitrary -- v1alpha1SidecarVolumeMounts :: Maybe [V1VolumeMount]
      <*> arbitrary -- v1alpha1SidecarWorkingDir :: Maybe Text
    
instance Arbitrary V1alpha1SuspendTemplate where
  arbitrary =
    
    pure V1alpha1SuspendTemplate
     
instance Arbitrary V1alpha1Template where
  arbitrary =
    V1alpha1Template
      <$> arbitrary -- v1alpha1TemplateActiveDeadlineSeconds :: Maybe Integer
      <*> arbitrary -- v1alpha1TemplateAffinity :: Maybe V1Affinity
      <*> arbitrary -- v1alpha1TemplateArchiveLocation :: Maybe V1alpha1ArtifactLocation
      <*> arbitrary -- v1alpha1TemplateContainer :: Maybe V1Container
      <*> arbitrary -- v1alpha1TemplateDaemon :: Maybe Bool
      <*> arbitrary -- v1alpha1TemplateDag :: Maybe V1alpha1DAGTemplate
      <*> arbitrary -- v1alpha1TemplateInputs :: Maybe V1alpha1Inputs
      <*> arbitrary -- v1alpha1TemplateMetadata :: Maybe V1alpha1Metadata
      <*> arbitrary -- v1alpha1TemplateName :: Text
      <*> arbitrary -- v1alpha1TemplateNodeSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1alpha1TemplateOutputs :: Maybe V1alpha1Outputs
      <*> arbitrary -- v1alpha1TemplateParallelism :: Maybe Integer
      <*> arbitrary -- v1alpha1TemplateResource :: Maybe V1alpha1ResourceTemplate
      <*> arbitrary -- v1alpha1TemplateRetryStrategy :: Maybe V1alpha1RetryStrategy
      <*> arbitrary -- v1alpha1TemplateScript :: Maybe V1alpha1ScriptTemplate
      <*> arbitrary -- v1alpha1TemplateSidecars :: Maybe [V1alpha1Sidecar]
      <*> arbitrary -- v1alpha1TemplateSteps :: Maybe [[V1alpha1WorkflowStep]]
      <*> arbitrary -- v1alpha1TemplateSuspend :: Maybe V1alpha1SuspendTemplate
      <*> arbitrary -- v1alpha1TemplateTolerations :: Maybe [V1Toleration]
    
instance Arbitrary V1alpha1ValueFrom where
  arbitrary =
    V1alpha1ValueFrom
      <$> arbitrary -- v1alpha1ValueFromJqFilter :: Maybe Text
      <*> arbitrary -- v1alpha1ValueFromJsonPath :: Maybe Text
      <*> arbitrary -- v1alpha1ValueFromParameter :: Maybe Text
      <*> arbitrary -- v1alpha1ValueFromPath :: Maybe Text
    
instance Arbitrary V1alpha1Workflow where
  arbitrary =
    V1alpha1Workflow
      <$> arbitrary -- v1alpha1WorkflowApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowKind :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowMetadata :: V1ObjectMeta
      <*> arbitrary -- v1alpha1WorkflowSpec :: V1alpha1WorkflowSpec
      <*> arbitrary -- v1alpha1WorkflowStatus :: V1alpha1WorkflowStatus
    
instance Arbitrary V1alpha1WorkflowList where
  arbitrary =
    V1alpha1WorkflowList
      <$> arbitrary -- v1alpha1WorkflowListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowListItems :: [V1alpha1Workflow]
      <*> arbitrary -- v1alpha1WorkflowListKind :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowListMetadata :: V1ListMeta
    
instance Arbitrary V1alpha1WorkflowSpec where
  arbitrary =
    V1alpha1WorkflowSpec
      <$> arbitrary -- v1alpha1WorkflowSpecAffinity :: Maybe V1Affinity
      <*> arbitrary -- v1alpha1WorkflowSpecArguments :: Maybe V1alpha1Arguments
      <*> arbitrary -- v1alpha1WorkflowSpecEntrypoint :: Text
      <*> arbitrary -- v1alpha1WorkflowSpecImagePullSecrets :: Maybe [V1LocalObjectReference]
      <*> arbitrary -- v1alpha1WorkflowSpecNodeSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1alpha1WorkflowSpecOnExit :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowSpecParallelism :: Maybe Integer
      <*> arbitrary -- v1alpha1WorkflowSpecServiceAccountName :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowSpecSuspend :: Maybe Bool
      <*> arbitrary -- v1alpha1WorkflowSpecTemplates :: [V1alpha1Template]
      <*> arbitrary -- v1alpha1WorkflowSpecTolerations :: Maybe [V1Toleration]
      <*> arbitrary -- v1alpha1WorkflowSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
      <*> arbitrary -- v1alpha1WorkflowSpecVolumes :: Maybe [V1Volume]
    
instance Arbitrary V1alpha1WorkflowStep where
  arbitrary =
    V1alpha1WorkflowStep
      <$> arbitrary -- v1alpha1WorkflowStepArguments :: Maybe V1alpha1Arguments
      <*> arbitrary -- v1alpha1WorkflowStepName :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowStepTemplate :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowStepWhen :: Maybe Text
      <*> arbitrary -- v1alpha1WorkflowStepWithItems :: Maybe [V1alpha1Item]
      <*> arbitrary -- v1alpha1WorkflowStepWithParam :: Maybe Text
    


