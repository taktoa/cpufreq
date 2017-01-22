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

type FPtr a = ForeignPtr a
  
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

-- FIXME: add cpufreq_stats

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

{-

instance Storable CPamMessage where
    alignment _ = alignment (undefined :: CDouble)
    sizeOf _ = {# sizeof pam_message #}
    peek p = CPamMessage <$> ({#get pam_message.msg_style #} p)
                         <*> ({#get pam_message.msg #} p)
    poke p (CPamMessage ms m) = do
        {#set pam_message.msg_style #} p ms
        {#set pam_message.msg #} p m

data CPamResponse = CPamResponse { resp :: CString
                                 , resp_retcode :: CInt
                                 }
                                 deriving (Show,Eq)

instance Storable CPamResponse where
    alignment _ = alignment (undefined :: CDouble)
    sizeOf _ = {# sizeof pam_response #}
    peek p = CPamResponse <$> ({#get pam_response.resp #} p)
                          <*> ({#get pam_response.resp_retcode #} p)
    poke p (CPamResponse r rc) = do
        {#set pam_response.resp #} p r
        {#set pam_response.resp_retcode #} p rc

data CPamConv = CPamConv { conv        :: FunPtr ConvFunc
                         , appdata_ptr :: Ptr () }

type ConvFunc = CInt -> Ptr ( Ptr ()) -> Ptr ( Ptr ()) -> Ptr () -> IO CInt
foreign import ccall "wrapper" mkconvFunc :: ConvFunc -> IO (FunPtr ConvFunc)

instance Storable CPamConv where
    alignment _ = alignment (undefined :: CDouble)
    sizeOf _ = {# sizeof pam_conv #}
    peek p = CPamConv <$> ({#get pam_conv.conv #} p)
                      <*> ({#get pam_conv.appdata_ptr #} p)
    poke p (CPamConv c ap) = do
        {#set pam_conv.conv #} p c
        {#set pam_conv.appdata_ptr #} p ap

type CPamHandleT = ()

foreign import ccall "security/pam_appl.h pam_start"         c_pam_start
  :: CString -> CString -> Ptr CPamConv -> Ptr (Ptr CPamHandleT) -> IO CInt
foreign import ccall "security/pam_appl.h pam_authenticate"  c_pam_authenticate
  :: {# type pam_handle_t #} -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_setcred"       c_pam_setcred
  :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_acct_mgmt"     c_pam_acct_mgmt
  :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_open_session"  c_pam_open_session
  :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_close_session" c_pam_close_session
  :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_end"           c_pam_end
  :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_set_item"      c_pam_set_item
  :: Ptr CPamHandleT -> CInt -> Ptr () -> IO CInt
foreign import ccall "security/pam_appl.h pam_getenvlist"    c_pam_getenvlist
  :: {# type pam_handle_t #} -> IO (Ptr (CString))

-}


-- // cpufreq.h - definitions for libcpufreq
--
-- #ifndef _CPUFREQ_H
-- #define _CPUFREQ_H 1
--
-- struct cpufreq_policy {
--	unsigned long min;
--	unsigned long max;
--	char *governor;
-- };
--
-- struct cpufreq_available_governors {
--	char *governor;
--	struct cpufreq_available_governors *next;
--	struct cpufreq_available_governors *first;
-- };
--
-- struct cpufreq_available_frequencies {
--	unsigned long frequency;
--	struct cpufreq_available_frequencies *next;
--	struct cpufreq_available_frequencies *first;
-- };
--
--
-- struct cpufreq_affected_cpus {
--	unsigned int cpu;
--	struct cpufreq_affected_cpus *next;
--	struct cpufreq_affected_cpus *first;
-- };
--
-- struct cpufreq_stats {
--	unsigned long frequency;
--	unsigned long long time_in_state;
--	struct cpufreq_stats *next;
--	struct cpufreq_stats *first;
-- };
--
-- #ifdef __cplusplus
-- extern "C" {
-- #endif
--
-- //! returns 0 if the specified CPU is present (it doesn't say
-- //! whether it is online!), and an error value if not.
--
-- extern int cpufreq_cpu_exists(unsigned int cpu);
--
-- //! determine current CPU frequency
-- //! - _kernel variant means kernel's opinion of CPU frequency
-- //! - _hardware variant means actual hardware CPU frequency,
-- //!	  which is only available to root.
-- //!
-- //! returns 0 on failure, else frequency in kHz.
-- extern unsigned long cpufreq_get_freq_kernel(unsigned int cpu);
--
-- extern unsigned long cpufreq_get_freq_hardware(unsigned int cpu);
--
-- #define cpufreq_get(cpu) cpufreq_get_freq_kernel(cpu);
--
--
-- //! determine CPU transition latency
-- //!
-- //! returns 0 on failure, else transition latency in 10^(-9) s = nanoseconds
-- extern unsigned long cpufreq_get_transition_latency(unsigned int cpu);
--
--
-- //! determine hardware CPU frequency limits
-- //!
-- //! These may be limited further by thermal, energy or other
-- //! considerations by cpufreq policy notifiers in the kernel.
-- extern int cpufreq_get_hardware_limits(unsigned int cpu,
--				       unsigned long *min,
--				       unsigned long *max);
--
--
-- //! determine CPUfreq driver used
-- //!
-- //! Remember to call cpufreq_put_driver when no longer needed
-- //! to avoid memory leakage, please.
--
-- extern char * cpufreq_get_driver(unsigned int cpu);
--
-- extern void	 cpufreq_put_driver(char * ptr);
--
--
-- //! determine CPUfreq policy currently used
-- //!
-- //! Remember to call cpufreq_put_policy when no longer needed
-- //! to avoid memory leakage, please.
--
-- extern struct cpufreq_policy * cpufreq_get_policy(unsigned int cpu);
--
-- extern void cpufreq_put_policy(struct cpufreq_policy *policy);
--
-- //! determine CPUfreq governors currently available
-- //!
-- //! may be modified by modprobe'ing or rmmod'ing other governors. Please
-- //! free allocated memory by calling cpufreq_put_available_governors
-- //! after use.
--
--
-- extern struct cpufreq_available_governors * cpufreq_get_available_governors(unsigned int cpu);
--
-- extern void cpufreq_put_available_governors(struct cpufreq_available_governors *first);
--
--
-- //! determine CPU frequency states available
-- //!
-- //! only present on _some_ ->target() cpufreq drivers. For information purposes
-- //! only. Please free allocated memory by calling cpufreq_put_available_frequencies
-- //! after use.
--
-- extern struct cpufreq_available_frequencies * cpufreq_get_available_frequencies(unsigned int cpu);
--
-- extern void cpufreq_put_available_frequencies(struct cpufreq_available_frequencies *first);
--
--
-- //! determine affected CPUs
-- //!
-- //! Remember to call cpufreq_put_affected_cpus when no longer needed
-- //! to avoid memory leakage, please.
--
-- extern struct cpufreq_affected_cpus * cpufreq_get_affected_cpus(unsigned int cpu);
--
-- extern void cpufreq_put_affected_cpus(struct cpufreq_affected_cpus *first);
--
--
-- //! determine related CPUs
-- //!
-- //! Remember to call cpufreq_put_related_cpus when no longer needed
-- //! to avoid memory leakage, please.
--
-- extern struct cpufreq_affected_cpus * cpufreq_get_related_cpus(unsigned int cpu);
--
-- extern void cpufreq_put_related_cpus(struct cpufreq_affected_cpus *first);
--
--
-- //! determine stats for cpufreq subsystem
-- //!
-- //! This is not available in all kernel versions or configurations.
--
-- extern struct cpufreq_stats * cpufreq_get_stats(unsigned int cpu, unsigned long long *total_time);
--
-- extern void cpufreq_put_stats(struct cpufreq_stats *stats);
--
-- extern unsigned long cpufreq_get_transitions(unsigned int cpu);
--
--
-- //! set new cpufreq policy
-- //!
-- //! Tries to set the passed policy as new policy as close as possible,
-- //! but results may differ depending e.g. on governors being available.
--
-- extern int cpufreq_set_policy(unsigned int cpu, struct cpufreq_policy *policy);
--
--
-- //! modify a policy by only changing min/max freq or governor
-- //!
-- //! Does not check whether result is what was intended.
--
-- extern int cpufreq_modify_policy_min(unsigned int cpu, unsigned long min_freq);
-- extern int cpufreq_modify_policy_max(unsigned int cpu, unsigned long max_freq);
-- extern int cpufreq_modify_policy_governor(unsigned int cpu, char *governor);
--
--
-- //! set a specific frequency
-- //!
-- //! Does only work if userspace governor can be used and no external
-- //! interference (other calls to this function or to set/modify_policy)
-- //! occurs. Also does not work on ->range() cpufreq drivers.
--
-- extern int cpufreq_set_frequency(unsigned int cpu, unsigned long target_frequency);
--
-- #ifdef __cplusplus
-- }
-- #endif
--
-- #endif /* _CPUFREQ_H */
