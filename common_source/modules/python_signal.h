#include <csignal>
#include <functional>
#include <iostream>

// Signal handler type
using SignalHandler = std::function<void(int)>;

// Global pointer to store the custom handler
static SignalHandler* custom_handler_ptr = nullptr;

// Wrapper to call the custom handler
void signal_dispatcher(int signum) {
    if (custom_handler_ptr && *custom_handler_ptr) {
        (*custom_handler_ptr)(signum);
    }
}

// Activate signal catching
void activate_signal_handling(SignalHandler handler, int signal = SIGINT) {
    static SignalHandler custom_handler; // Keep the handler alive
    custom_handler = std::move(handler);
    custom_handler_ptr = &custom_handler;
    std::signal(signal, signal_dispatcher);
}


void restore_default_signal_handling(int signal = SIGINT) {
    std::signal(signal, SIG_DFL);
}
