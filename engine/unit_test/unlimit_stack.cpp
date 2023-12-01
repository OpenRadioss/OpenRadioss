#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

extern "C" {
    void set_stack_unlimited() {
        struct rlimit rl;
        rl.rlim_cur = RLIM_INFINITY;
        rl.rlim_max = RLIM_INFINITY;
        setrlimit(RLIMIT_STACK, &rl);
    }
}

