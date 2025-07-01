#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cstring>
#include <iostream>
#include <algorithm>
#include <cctype>
#include <limits>
#include <cerrno>

extern "C"
{
    void cpp_precice_read_cpl(char *input_filename, char *participant_name, char *config_file, char *mesh_name, char *write_name, char *read_name, int *grnod_id)
    {
        // Input validation - check for null pointers first
        if (!input_filename) {
            std::cout << "Error: input_filename is null pointer" << std::endl;
            return;
        }
        
        if (!participant_name || !config_file || !mesh_name || !write_name || !read_name || !grnod_id) {
            std::cout << "Error: One or more output parameters are null pointers" << std::endl;
            return;
        }
        
        // Debug: Print pointer address for debugging
        std::cout << "Debug: input_filename pointer = " << static_cast<void*>(input_filename) << std::endl;
        
        // Handle Fortran string: may not be null-terminated and has limited buffer
        std::string filename_str;
        
        try {
            // Conservative approach: assume buffer might be small
            // Try to detect actual buffer size by careful probing
            const int max_safe_read = 256; // Conservative limit
            char temp_char;
            int i = 0;
            bool found_null = false;
            
            // Probe memory very carefully
            while (i < max_safe_read) {
                // Try to read one byte, but be prepared for segfault
                bool can_read = true;
                try {
                    temp_char = input_filename[i];
                } catch (...) {
                    // Memory access failed - we've hit the buffer limit
                    can_read = false;
                }
                
                if (!can_read) {
                    std::cout << "Debug: Buffer limit detected at position " << i << std::endl;
                    break;
                }
                
                // Check for null terminator
                if (temp_char == '\0') {
                    found_null = true;
                    std::cout << "Debug: Null terminator found at position " << i << std::endl;
                    break;
                }
                
                // Check for reasonable characters (printable ASCII + common whitespace)
                if ((temp_char >= 32 && temp_char <= 126) || 
                    temp_char == '\t' || temp_char == '\n' || temp_char == '\r') {
                    filename_str += temp_char;
                } else if (temp_char == ' ') {
                    // Space might be padding from Fortran - we'll handle this later
                    filename_str += temp_char;
                } else {
                    // Non-printable character might indicate end of string or garbage
                    std::cout << "Debug: Non-printable character at position " << i 
                              << " (ASCII " << static_cast<int>(temp_char) << ") - assuming end of string" << std::endl;
                    break;
                }
                
                i++;
            }
            
            // For Fortran strings, remove trailing spaces (common padding)
            filename_str.erase(filename_str.find_last_not_of(" \t\r\n") + 1);
            
            if (filename_str.empty()) {
                std::cout << "Error: Empty filename after processing" << std::endl;
                return;
            }
            
            std::cout << "Debug: Successfully extracted filename from " << i << " byte buffer" << std::endl;
            if (!found_null) {
                std::cout << "Debug: No null terminator found - assuming Fortran fixed-length string" << std::endl;
            }
            
        } catch (const std::exception& e) {
            std::cout << "Error: Exception while processing filename: " << e.what() << std::endl;
            return;
        } catch (...) {
            std::cout << "Error: Unknown exception while processing filename" << std::endl;
            return;
        }
        
        std::cout << "Debug: Processed filename = '" << filename_str << "' (length: " << filename_str.length() << ")" << std::endl;
        
        // Open file with error checking
        std::ifstream file;
        try {
            file.open(filename_str.c_str());
            if (!file.is_open()) {
                std::cout << "Error: Cannot open file: " << filename_str << std::endl;
                return;
            }
        } catch (const std::exception& e) {
            std::cout << "Error: Exception while opening file: " << e.what() << std::endl;
            return;
        }
        
        // Parse file
        std::string line;
        std::vector<std::vector<std::string>> lines;
        
        try {
            while (std::getline(file, line)) {
                // Check if line starts with "/PRECICE" (case-insensitive)
                if (line.length() >= 8) {
                    std::string prefix = line.substr(0, 8);
                    std::transform(prefix.begin(), prefix.end(), prefix.begin(), ::toupper);
                    
                    if (prefix == "/PRECICE") {
                        std::vector<std::string> words;
                        std::string word;
                        std::istringstream ss(line);
                        
                        // Split by '/' delimiter
                        while (std::getline(ss, word, '/')) {
                            // Trim whitespace
                            word.erase(0, word.find_first_not_of(" \t\r\n"));
                            word.erase(word.find_last_not_of(" \t\r\n") + 1);
                            words.push_back(word);
                        }
                        
                        // Only add if we have enough tokens
                        if (words.size() >= 4) {
                            lines.push_back(words);
                        } else {
                            std::cout << "Warning: Malformed PRECICE line (insufficient tokens): " << line << std::endl;
                        }
                    }
                }
            }
        } catch (const std::exception& e) {
            std::cout << "Error: Exception while reading file: " << e.what() << std::endl;
            file.close();
            return;
        }
        
        file.close();
        
        if (lines.empty()) {
            std::cout << "Warning: No valid PRECICE configuration lines found in file" << std::endl;
            return;
        }
        
        // Initialize output parameters to safe defaults - check each pointer first
        try {
            participant_name[0] = '\0';
            config_file[0] = '\0';
            mesh_name[0] = '\0';
            write_name[0] = '\0';
            read_name[0] = '\0';
            *grnod_id = 0;
        } catch (...) {
            std::cout << "Error: Cannot initialize output parameters - invalid pointers" << std::endl;
            return;
        }
        
        // Safe string copy function
        auto safe_string_copy = [](char* dest, const std::string& src, const char* param_name) {
            try {
                if (src.length() >= 50) {
                    std::cout << "Warning: " << param_name << " too long, truncating: " << src << std::endl;
                    strncpy(dest, src.c_str(), 49);
                    dest[49] = '\0';
                } else {
                    strcpy(dest, src.c_str());
                }
            } catch (...) {
                std::cout << "Error: Failed to copy " << param_name << std::endl;
            }
        };
        
        // Process parsed lines
        for (const auto &words : lines) {
            // Ensure we have at least 4 elements
            if (words.size() < 4) {
                continue;
            }
            
            // Convert key to uppercase for case-insensitive comparison
            std::string key = words[2];
            std::transform(key.begin(), key.end(), key.begin(), ::toupper);
            
            try {
                if (key == "PARTICIPANT_NAME") {
                    safe_string_copy(participant_name, words[3], "PARTICIPANT_NAME");
                }
                else if (key == "CONFIG_FILE") {
                    safe_string_copy(config_file, words[3], "CONFIG_FILE");
                }
                else if (key == "MESH_NAME") {
                    safe_string_copy(mesh_name, words[3], "MESH_NAME");
                }
                else if (key == "WRITE") {
                    safe_string_copy(write_name, words[3], "WRITE");
                }
                else if (key == "READ") {
                    safe_string_copy(read_name, words[3], "READ");
                }
                else if (key == "INTERFACE") {
                    // Safe integer conversion
                    char* endptr;
                    errno = 0;
                    long val = strtol(words[3].c_str(), &endptr, 10);
                    
                    if (errno != 0) {
                        std::cout << "Error: Failed to convert INTERFACE value to integer: " << words[3] << std::endl;
                        *grnod_id = 0;
                    } else if (endptr == words[3].c_str()) {
                        std::cout << "Error: INTERFACE value is not a valid integer: " << words[3] << std::endl;
                        *grnod_id = 0;
                    } else if (val < std::numeric_limits<int>::min() || val > std::numeric_limits<int>::max()) {
                        std::cout << "Error: INTERFACE value out of integer range: " << words[3] << std::endl;
                        *grnod_id = 0;
                    } else {
                        *grnod_id = static_cast<int>(val);
                    }
                }
            } catch (const std::exception& e) {
                std::cout << "Error: Exception while processing configuration line: " << e.what() << std::endl;
                continue;
            }
        }
        
        // Print summary of what was found
        std::cout << "preCICE configuration loaded successfully:" << std::endl;
        std::cout << "  Participant: " << (strlen(participant_name) > 0 ? participant_name : "NOT SET") << std::endl;
        std::cout << "  Config file: " << (strlen(config_file) > 0 ? config_file : "NOT SET") << std::endl;
        std::cout << "  Mesh name: " << (strlen(mesh_name) > 0 ? mesh_name : "NOT SET") << std::endl;
        std::cout << "  Write name: " << (strlen(write_name) > 0 ? write_name : "NOT SET") << std::endl;
        std::cout << "  Read name: " << (strlen(read_name) > 0 ? read_name : "NOT SET") << std::endl;
        std::cout << "  Interface ID: " << *grnod_id << std::endl;
    }
}