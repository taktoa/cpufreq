{-# LANGUAGE CPP                      #-}
{-# LANGUAGE Safe                     #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.CPUFreq.Internal where

import Foreign.C
import Foreign.C.Error
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

#include <cpufreq.h>
  
type CFIndex    = CUInt
type CFFreq     = CULong
type CFLatency  = CULong
type CFGovernor = CString
type CFDriver   = CString
type CFResult   = Errno

data CFPolicy
  = CFPolicy
    { _p_min      :: {-# UNPACK #-} !CFFreq
    , _p_max      :: {-# UNPACK #-} !CFFreq
    , _p_governor :: {-# UNPACK #-} !CFGovernor
    }
  deriving (Eq, Show)

data CFAvailableGovernors
  = CFAvailableGovernors
    { _ag_governor :: {-# UNPACK #-} !CFGovernor
    , _ag_next     :: {-# UNPACK #-} !(Ptr CFAvailableGovernors)
    , _ag_first    :: {-# UNPACK #-} !(Ptr CFAvailableGovernors)
    }
  deriving (Eq, Show)

data CFAvailableFrequencies
  = CFAvailableFrequencies
    { _af_frequency :: {-# UNPACK #-} !CFFreq
    , _af_next      :: {-# UNPACK #-} !(Ptr CFAvailableFrequencies)
    , _af_first     :: {-# UNPACK #-} !(Ptr CFAvailableFrequencies)
    }
  deriving (Eq, Show)

data CFAffectedCPUs
  = CFAffectedCPUs
    { _ac_cpu   :: {-# UNPACK #-} !CFIndex
    , _ac_next  :: {-# UNPACK #-} !(Ptr CFAffectedCPUs)
    , _ac_first :: {-# UNPACK #-} !(Ptr CFAffectedCPUs)
    }
  deriving (Eq, Show)

data CFStats
  = CFStats
    { _s_frequency     :: {-# UNPACK #-} !CFFreq
    , _s_time_in_state :: {-# UNPACK #-} !CULLong
    , _s_next          :: {-# UNPACK #-} !(Ptr CFStats)
    , _s_first         :: {-# UNPACK #-} !(Ptr CFStats)
    }
  deriving (Eq, Show)

foreign import ccall "cpufreq.h cpufreq_cpu_exists"
  cf_cpu_exists :: CFIndex -> IO CFResult
foreign import ccall "cpufreq.h cpufreq_get_freq_kernel"
  cf_get_frequency_kernel :: CFIndex -> IO CFFreq
foreign import ccall "cpufreq.h cpufreq_get_freq_hardware"
  cf_get_frequency_hardware :: CFIndex -> IO CFFreq
foreign import ccall "cpufreq.h cpufreq_get_transition_latency"
  cf_get_transition_latency :: CFIndex -> IO CFLatency
foreign import ccall "cpufreq.h cpufreq_get_hardware_limits"
  cf_get_hardware_limits :: CFIndex -> Ptr CFFreq -> Ptr CFFreq -> IO CFResult
foreign import ccall "cpufreq.h cpufreq_get_driver"
  cf_get_driver :: CFIndex -> IO CFDriver
foreign import ccall "cpufreq.h cpufreq_put_driver"
  cf_put_driver :: CFDriver -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_policy"
  cf_get_policy :: CFIndex -> IO (Ptr CFPolicy)
foreign import ccall "cpufreq.h cpufreq_put_policy"
  cf_put_policy :: Ptr CFPolicy -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_available_governors"
  cf_get_available_governors :: CFIndex -> IO (Ptr CFAvailableGovernors)
foreign import ccall "cpufreq.h cpufreq_put_available_governors"
  cf_put_available_governors :: Ptr CFAvailableGovernors -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_available_frequencies"
  cf_get_available_frequencies :: CFIndex -> IO (Ptr CFAvailableFrequencies)
foreign import ccall "cpufreq.h cpufreq_put_available_frequencies"
  cf_put_available_frequencies :: Ptr CFAvailableFrequencies -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_affected_cpus"
  cf_get_affected_cpus :: CFIndex -> IO (Ptr CFAffectedCPUs)
foreign import ccall "cpufreq.h cpufreq_put_affected_cpus"
  cf_put_affected_cpus :: Ptr CFAffectedCPUs -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_related_cpus"
  cf_get_related_cpus :: CFIndex -> IO (Ptr CFAffectedCPUs)
foreign import ccall "cpufreq.h cpufreq_put_related_cpus"
  cf_put_related_cpus :: Ptr CFAffectedCPUs -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_stats"
  cf_get_stats :: CFIndex -> CULLong -> IO (Ptr CFStats)
foreign import ccall "cpufreq.h cpufreq_put_stats"
  cf_put_stats :: Ptr CFStats -> IO ()
foreign import ccall "cpufreq.h cpufreq_get_transitions"
  cf_get_transitions :: CFIndex -> IO CULong
foreign import ccall "cpufreq.h cpufreq_set_policy"
  cf_set_policy :: CFIndex -> Ptr CFPolicy -> IO CFResult
foreign import ccall "cpufreq.h cpufreq_modify_policy_min"
  cf_modify_policy_min :: CFIndex -> CFFreq -> IO CFResult
foreign import ccall "cpufreq.h cpufreq_modify_policy_max"
  cf_modify_policy_max :: CFIndex -> CFFreq -> IO CFResult
foreign import ccall "cpufreq.h cpufreq_modify_policy_governor"
  cf_modify_policy_governor :: CFIndex -> CString -> IO CFResult
foreign import ccall "cpufreq.h cpufreq_set_frequency"
  cf_set_frequency :: CFIndex -> CFFreq -> IO CFResult

instance Storable CFPolicy where
  alignment _ = {# alignof cpufreq_policy #}
  sizeOf _ = {# sizeof cpufreq_policy #}
  peek p = CFPolicy
           <$> ({# get cpufreq_policy.min      #} p)
           <*> ({# get cpufreq_policy.max      #} p)
           <*> ({# get cpufreq_policy.governor #} p)
  poke p (CFPolicy {..}) = do
    ({# set cpufreq_policy.min      #} p _p_min)
    ({# set cpufreq_policy.max      #} p _p_max)
    ({# set cpufreq_policy.governor #} p _p_governor)

instance Storable CFAvailableGovernors where
  alignment _ = {# alignof cpufreq_available_governors #}
  sizeOf _ = {# sizeof cpufreq_available_governors #}
  peek p = CFAvailableGovernors
           <$> (id      <$> {# get cpufreq_available_governors.governor #} p)
           <*> (castPtr <$> {# get cpufreq_available_governors.next     #} p)
           <*> (castPtr <$> {# get cpufreq_available_governors.first    #} p)
  poke p (CFAvailableGovernors {..}) = do
    ({# set cpufreq_available_governors.governor #} p _ag_governor)
    ({# set cpufreq_available_governors.next     #} p (castPtr _ag_next))
    ({# set cpufreq_available_governors.first    #} p (castPtr _ag_first))

instance Storable CFAvailableFrequencies where
  alignment _ = {# alignof cpufreq_available_frequencies #}
  sizeOf _ = {# sizeof cpufreq_available_frequencies #}
  peek p = CFAvailableFrequencies
           <$> (id      <$> {# get cpufreq_available_frequencies.frequency #} p)
           <*> (castPtr <$> {# get cpufreq_available_frequencies.next      #} p)
           <*> (castPtr <$> {# get cpufreq_available_frequencies.first     #} p)
  poke p (CFAvailableFrequencies {..}) = do
    ({# set cpufreq_available_frequencies.frequency #} p _af_frequency)
    ({# set cpufreq_available_frequencies.next      #} p (castPtr _af_next))
    ({# set cpufreq_available_frequencies.first     #} p (castPtr _af_first))

instance Storable CFAffectedCPUs where
  alignment _ = {# alignof cpufreq_affected_cpus #}
  sizeOf _ = {# sizeof cpufreq_affected_cpus #}
  peek p = CFAffectedCPUs
           <$> (id      <$> {# get cpufreq_affected_cpus.cpu   #} p)
           <*> (castPtr <$> {# get cpufreq_affected_cpus.next  #} p)
           <*> (castPtr <$> {# get cpufreq_affected_cpus.first #} p)
  poke p (CFAffectedCPUs {..}) = do
    ({# set cpufreq_affected_cpus.cpu   #} p _ac_cpu)
    ({# set cpufreq_affected_cpus.next  #} p (castPtr _ac_next))
    ({# set cpufreq_affected_cpus.first #} p (castPtr _ac_first))

instance Storable CFStats where
  alignment _ = {# alignof cpufreq_stats #}
  sizeOf _ = {# sizeof cpufreq_stats #}
  peek p = CFStats
           <$> (id      <$> {# get cpufreq_stats.frequency     #} p)
           <*> (id      <$> {# get cpufreq_stats.time_in_state #} p)
           <*> (castPtr <$> {# get cpufreq_stats.next          #} p)
           <*> (castPtr <$> {# get cpufreq_stats.first         #} p)
  poke p (CFStats {..}) = do
    ({# set cpufreq_stats.frequency     #} p _s_frequency)
    ({# set cpufreq_stats.time_in_state #} p _s_time_in_state)
    ({# set cpufreq_stats.next          #} p (castPtr _s_next))
    ({# set cpufreq_stats.first         #} p (castPtr _s_first))
