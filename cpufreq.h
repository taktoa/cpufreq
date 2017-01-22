// cpufreq.h - definitions for libcpufreq

#ifndef _CPUFREQ_H
#define _CPUFREQ_H 1

typedef struct cpufreq_policy {
	unsigned long min;
	unsigned long max;
	char* governor;
} cf_policy;

typedef struct cpufreq_available_governors {
	char* governor;
	struct cpufreq_available_governors* next;
	struct cpufreq_available_governors* first;
} cf_available_governors;

typedef struct cpufreq_available_frequencies {
	unsigned long frequency;
	struct cpufreq_available_frequencies* next;
	struct cpufreq_available_frequencies* first;
} cf_available_frequencies;

typedef struct cpufreq_affected_cpus {
	unsigned int cpu;
	struct cpufreq_affected_cpus* next;
	struct cpufreq_affected_cpus* first;
} cf_affected_cpus;

typedef struct cpufreq_stats {
	unsigned long frequency;
	unsigned long long time_in_state;
	struct cpufreq_stats* next;
	struct cpufreq_stats* first;
} cf_stats;

//! CPU index
typedef unsigned int cpu_index;

//! CPU frequency in kHz
typedef unsigned long cpu_freq;

//! CPU transition latency in nanoseconds
typedef unsigned long cpu_transition_latency;


#ifdef __cplusplus
extern "C" {
#endif

//! returns 0 if the specified CPU is present (it doesn't say
//! whether it is online!), and an error value if not.

extern int
cpufreq_cpu_exists(cpu_index cpu);

//! determine current CPU frequency
//! - _kernel variant means kernel's opinion of CPU frequency
//! - _hardware variant means actual hardware CPU frequency,
//!	  which is only available to root.
//!
//! returns 0 on failure, else frequency in kHz.

extern cpu_freq
cpufreq_get_freq_kernel(cpu_index cpu);

extern cpu_freq
cpufreq_get_freq_hardware(cpu_index cpu);

#define cpufreq_get(cpu) cpufreq_get_freq_kernel(cpu);

//! determine CPU transition latency
//!
//! returns 0 on failure, else transition latency in 10^(-9) s = nanoseconds

extern cpu_transition_latency
cpufreq_get_transition_latency(cpu_index cpu);


//! determine hardware CPU frequency limits
//!
//! These may be limited further by thermal, energy or other
//! considerations by cpufreq policy notifiers in the kernel.

extern int
cpufreq_get_hardware_limits(cpu_index cpu,
							cpu_freq* min,
							cpu_freq* max);

//! determine CPUfreq driver used
//!
//! Remember to call cpufreq_put_driver when no longer needed
//! to avoid memory leakage, please.

extern char*
cpufreq_get_driver(cpu_index cpu);

extern void
cpufreq_put_driver(char* ptr);

//! determine CPUfreq policy currently used
//!
//! Remember to call cpufreq_put_policy when no longer needed
//! to avoid memory leakage, please.

extern cf_policy*
cpufreq_get_policy(cpu_index cpu);

extern void
cpufreq_put_policy(cf_policy* policy);

//! determine CPUfreq governors currently available
//!
//! may be modified by modprobe'ing or rmmod'ing other governors. Please
//! free allocated memory by calling cpufreq_put_available_governors
//! after use.


extern cf_available_governors*
cpufreq_get_available_governors(cpu_index cpu);

extern void
cpufreq_put_available_governors(cf_available_governors* first);


//! determine CPU frequency states available
//!
//! only present on _some_ ->target() cpufreq drivers. For information purposes
//! only. Please free allocated memory by calling cpufreq_put_available_frequencies
//! after use.

extern cf_available_frequencies*
cpufreq_get_available_frequencies(cpu_index cpu);

extern void
cpufreq_put_available_frequencies(cf_available_frequencies* first);


//! determine affected CPUs
//!
//! Remember to call cpufreq_put_affected_cpus when no longer needed
//! to avoid memory leakage, please.

extern cf_affected_cpus*
cpufreq_get_affected_cpus(cpu_index cpu);

extern void
cpufreq_put_affected_cpus(cf_affected_cpus* first);


//! determine related CPUs
//!
//! Remember to call cpufreq_put_related_cpus when no longer needed
//! to avoid memory leakage, please.

extern cf_affected_cpus*
cpufreq_get_related_cpus(cpu_index cpu);

extern void
cpufreq_put_related_cpus(cf_affected_cpus* first);

//! set new cpufreq policy
//!
//! Tries to set the passed policy as new policy as close as possible,
//! but results may differ depending e.g. on governors being available.

extern int
cpufreq_set_policy(cpu_index cpu, cf_policy* policy);

//! modify a policy by only changing min/max freq or governor
//!
//! Does not check whether result is what was intended.

extern int
cpufreq_modify_policy_min(cpu_index cpu, unsigned long min_freq);

extern int
cpufreq_modify_policy_max(cpu_index cpu, unsigned long max_freq);

extern int
cpufreq_modify_policy_governor(cpu_index cpu, char* governor);

//! set a specific frequency
//!
//! Does only work if userspace governor can be used and no external
//! interference (other calls to this function or to set/modify_policy)
//! occurs. Also does not work on ->range() cpufreq drivers.

extern int cpufreq_set_frequency(cpu_index cpu, cpu_freq target_frequency);

#ifdef __cplusplus
}
#endif

#endif /* _CPUFREQ_H */
