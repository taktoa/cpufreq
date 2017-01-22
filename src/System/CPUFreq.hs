{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module System.CPUFreq where

import           Data.Semigroup

import           Control.Monad
import           Control.Monad.Catch

import           Foreign.C.Error
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable

import           Data.Word

import           Data.Set                (Set)
import qualified Data.Set                as Set

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS

import           GHC.Generics            (Generic)

import           System.CPUFreq.Internal

type CPUIndex    = Int
type Frequency   = Int
type Latency     = Int
type Governor    = ByteString
type Driver      = ByteString
type ElapsedTime = Word64

data Policy
  = MkPolicy
    { minFreq  :: Frequency
    , maxFreq  :: Frequency
    , governor :: Governor
    }
  deriving (Eq, Show, Read)

type Stats = (Frequency, ElapsedTime)

-- | FIXME: doc
cpuExists :: CPUIndex -> IO Bool
cpuExists idx = do errno <- cf_cpu_exists $ fromIntegral idx
                   pure $ errno == eOK

-- | FIXME: doc
getKernelFrequency :: CPUIndex -> IO Frequency
getKernelFrequency = fmap coerceI . cf_get_frequency_kernel . coerceI

-- | FIXME: doc
getHardwareFrequency :: CPUIndex -> IO Frequency
getHardwareFrequency = fmap coerceI . cf_get_frequency_hardware . coerceI

-- | FIXME: doc
getTransitionLatency :: CPUIndex -> IO Latency
getTransitionLatency = fmap coerceI . cf_get_frequency_hardware . coerceI

-- | FIXME: doc
getHardwareLimits :: CPUIndex -> IO (Frequency, Frequency)
getHardwareLimits idx = do minFP <- mallocForeignPtr
                           maxFP <- mallocForeignPtr
                           withForeignPtr minFP $ \minP ->
                             withForeignPtr maxFP $ \maxP -> helper minP maxP
  where
    helper :: Ptr CFFreq -> Ptr CFFreq -> IO (Frequency, Frequency)
    helper minP maxP = do
      checkErrno "System.CPUFreq.getHardwareLimits"
        $ cf_get_hardware_limits (coerceI idx) minP maxP
      minFreq <- peek minP
      maxFreq <- peek maxP
      pure (coerceI minFreq, coerceI maxFreq)

-- | FIXME: doc
getDriver :: CPUIndex -> IO ByteString
getDriver idx = do
  ptr <- cf_get_driver $ coerceI idx
  driver <- BS.packCString ptr
  cf_put_driver ptr
  pure driver

-- | FIXME: doc
getPolicy :: CPUIndex -> IO Policy
getPolicy idx = do
  ptr <- cf_get_policy (coerceI idx)
  (CFPolicy {..}) <- peek ptr
  let (!minFreq, !maxFreq) = (coerceI _p_min, coerceI _p_max)
  !governor <- BS.packCString _p_governor
  let !policy = MkPolicy {..}
  cf_put_policy ptr
  pure policy

-- | FIXME: doc
availableGovernors :: CPUIndex -> IO (Set Governor)
availableGovernors idx = do ptr <- cf_get_available_governors (coerceI idx)
                            res <- helper ptr []
                            cf_put_available_governors ptr
                            pure $ Set.fromList res
  where
    helper :: Ptr CFAvailableGovernors -> [Governor] -> IO [Governor]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAvailableGovernors {..}) <- peek ptr
                               !gov <- BS.packCString _ag_governor
                               helper _ag_next (gov : soFar)

availableFrequencies :: CPUIndex -> IO (Set Frequency)
availableFrequencies idx = do ptr <- cf_get_available_frequencies (coerceI idx)
                              res <- helper ptr []
                              cf_put_available_frequencies ptr
                              pure $ Set.fromList res
  where
    helper :: Ptr CFAvailableFrequencies -> [Frequency] -> IO [Frequency]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAvailableFrequencies {..}) <- peek ptr
                               let freq = coerceI _af_frequency
                               helper _af_next (freq : soFar)

-- | FIXME: doc
affectedCPUs :: CPUIndex -> IO (Set CPUIndex)
affectedCPUs idx = do ptr <- cf_get_affected_cpus (coerceI idx)
                      res <- helper ptr []
                      cf_put_affected_cpus ptr
                      pure $ Set.fromList res
  where
    helper :: Ptr CFAffectedCPUs -> [CPUIndex] -> IO [CPUIndex]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAffectedCPUs {..}) <- peek ptr
                               let cpu = coerceI _ac_cpu
                               helper _ac_next (cpu : soFar)

-- | FIXME: doc
relatedCPUs :: CPUIndex -> IO (Set CPUIndex)
relatedCPUs idx = do ptr <- cf_get_related_cpus (coerceI idx)
                     res <- helper ptr []
                     cf_put_related_cpus ptr
                     pure $ Set.fromList res
  where
    helper :: Ptr CFAffectedCPUs -> [CPUIndex] -> IO [CPUIndex]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAffectedCPUs {..}) <- peek ptr
                               let cpu = coerceI _ac_cpu
                               helper _ac_next (cpu : soFar)

-- | FIXME: doc
cpuStatistics :: CPUIndex -> ElapsedTime -> IO [Stats]
cpuStatistics idx tot = do ptr <- cf_get_stats (coerceI idx) (coerceI tot)
                           res <- helper ptr []
                           cf_put_stats ptr
                           pure res
  where
    helper :: Ptr CFStats -> [Stats] -> IO [Stats]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFStats {..}) <- peek ptr
                               let freq = coerceI _s_frequency
                               let time = coerceI _s_time_in_state
                               helper _s_next ((freq, time) : soFar)

-- | FIXME: doc
setPolicy :: CPUIndex -> Policy -> IO ()
setPolicy idx (MkPolicy {..}) = checkErrno "System.CPUFreq.setPolicy"
                                $ BS.useAsCString governor helper
  where
    helper govCS = do
      ptr <- newStablePtr $ CFPolicy (coerceI minFreq) (coerceI maxFreq) govCS
      res <- cf_set_policy (coerceI idx) (castPtr (castStablePtrToPtr ptr))
      freeStablePtr ptr
      pure res

-- | FIXME: doc
setMinFreq :: CPUIndex -> Frequency -> IO ()
setMinFreq idx freq = checkErrno "System.CPUFreq.setMinFreq"
                      $ cf_modify_policy_min (coerceI idx) (coerceI freq)

-- | FIXME: doc
setMaxFreq :: CPUIndex -> Frequency -> IO ()
setMaxFreq idx freq = checkErrno "System.CPUFreq.setMaxFreq"
                      $ cf_modify_policy_max (coerceI idx) (coerceI freq)

-- | FIXME: doc
setGovernor :: CPUIndex -> Governor -> IO ()
setGovernor idx gov = checkErrno "System.CPUFreq.setGovernor"
                      $ BS.useAsCString gov
                      $ cf_modify_policy_governor (coerceI idx)

-- | FIXME: doc
setFrequency :: CPUIndex -> Frequency -> IO ()
setFrequency idx freq = checkErrno "System.CPUFreq.setFrequency"
                        $ cf_set_frequency (coerceI idx) (coerceI freq)

--------------------------------------------------------------------------------

checkErrno :: String -> IO Errno -> IO ()
checkErrno loc m = do
  err <- m
  let (hdl, fname) = (Nothing, Nothing)
  when (err /= eOK) $ throwM $ errnoToIOError loc err hdl fname

coerceI :: (Integral i, Integral i') => i -> i'
coerceI = fromIntegral
