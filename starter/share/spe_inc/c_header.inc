#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd  || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32
#include <sys\types.h>
#define _FCALL 
#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/times.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#define _FCALL
#endif
