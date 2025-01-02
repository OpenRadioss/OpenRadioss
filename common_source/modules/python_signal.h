//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
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
