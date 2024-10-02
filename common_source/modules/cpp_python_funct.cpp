//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
//
#ifndef __aarch64__
#define _GLIBCXX_USE_CXX11_ABI 0
#endif
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstring>
#include <vector>
#include <set>
#include <map>
#include <regex>
#ifndef PYTHON_DISABLED
#ifdef _WIN32
/* Windows includes */
#include <windows.h>
#else
#include <dlfcn.h>
#include <dirent.h>
#endif

#ifdef MYREAL8
// double precision define my_real as double
typedef double my_real;
#else
typedef float my_real;
#endif

#include "cpp_python_funct.h"

// Note on the python library used:
//
//- We check that RAD_PYTHON_PATH is defined and that it points to a valid python library. This variable must contain the full path of the *.[so|dll] file
// - if the previous check fails, we check that PYTHONHOME is defined and that it points to a valid python library (that contains /lib/libpython*.[so|dll]])
// - For Linux if the previous check fails, we look for a python library in the default locations (LD_LIBRARY_PATH)

// load a function from a dynamic library
#ifdef _WIN32
HMODULE handle = NULL;
HMODULE python_exec = NULL;
#else
void* handle = nullptr; 
#endif


// global variables
PyObject *pDict = nullptr;
bool python_initialized = false;

// user ids of the nodes that are used in the python functions
std::set<int> nodes_uid;
// mapping between user ids and local ids
std::map<int, int> nodes_uid_to_local_id;

KeywordPairs element_variables;

template <std::size_t N>
std::string element_parenthesis_to_underscore(const std::string &input, const std::array<const char *, N> &keywords)
{
    std::string output = input;
    for (const auto &keyword : keywords)
    {
        std::regex pattern(std::string(keyword) + R"(\(\s*(\d+)\s*\))");
        output = std::regex_replace(output, pattern, std::string(keyword) + "_$1");
    }
    return output;
}

std::string node_parenthesis_to_underscore(const std::string &input)
{
    std::regex pattern(R"(\b(DX|DY|DZ|AX|AY|AZ|CX|CY|CZ|VX|VY|VZ|ARX|ARY|ARZ|VRX|VRY|VRZ|DRX|DRY|DRZ)\b\s*\(\s*(\d+)\s*\))");
    std::string result = std::regex_replace(input, pattern, "$1_$2");
    return result;
}

std::string parenthesis_to_underscore(const std::string &input)
{
    std::string result = node_parenthesis_to_underscore(input);
    return element_parenthesis_to_underscore(result, ELEMENT_KEYWORDS);
}

// Function to extract numbers based on the pattern and fill the global set
void extract_node_uid(const std::string &input)
{
    //    Coordinates     CX_n,  CY_n,  CZ_n
    //    Displacement    DX_n,  DY_n,  DZ_n
    //    Acceleration    AX_n,  AY_n,  AZ_n
    //    DisplacementR  DRX_n, DRY_n, DRZ_n
    //    Velocity        VX_n,  VY_n , VZ_n
    //    VelocityR      VRX_n, VRY_n, VRZ_n
    //    AccelerationR  ARX_n, ARY_n, ARZ_n
    // Regex pattern: non-alphanumeric or start of line, followed by A[XYZ] and underscore and numbers
    // std::cout<<"input: "<<input<<std::endl;
    std::regex pattern(R"((?:[^a-zA-Z0-9]|^)[ACDV][R]*[XYZ]_[0-9]+)");

    auto begin = std::sregex_iterator(input.begin(), input.end(), pattern);
    auto end = std::sregex_iterator();

    for (auto i = begin; i != end; ++i)
    {
	
        auto match = *i;
        std::string match_str = match.str();
        size_t underscore_pos = match_str.find('_');
        if (underscore_pos != std::string::npos)
        {
            int number = std::stoi(match_str.substr(underscore_pos + 1));
            nodes_uid.insert(number);
        }
    }
}

// Function to extract unique pairs of <int id, const char * keyword> from the string
void extract_element_keywords(const std::string &input)
{
    for (const auto &keyword : ELEMENT_KEYWORDS)
    {
        // std::regex pattern(std::string(keyword) + R"(_(\d+))");
        std::regex pattern(std::string(R"((?:[^a-zA-Z0-9]|^))") + std::string(keyword) + R"(_(\d+))");
        auto words_begin = std::sregex_iterator(input.begin(), input.end(), pattern);
        auto words_end = std::sregex_iterator();
        for (std::sregex_iterator i = words_begin; i != words_end; ++i)
        {
            std::smatch match = *i;
            int id = std::stoi(match.str(1));
            element_variables.emplace(id, keyword);
            // std::cout<<"[PYTHON] keyword found: "<<keyword<<" id: "<<id<<std::endl;
        }
    }
}

// Here is the list of the function that are loaded from the Python library
// The template is there only to have one version of the code for both Windows and Linux
template <typename T>
void load_functions(T h, bool &python_initialized)
{
    python_initialized = true;
    load_function(h, "Py_Initialize", My_Initialize, python_initialized);
    load_function(h, "Py_IsInitialized", My_IsInitialized, python_initialized);
    load_function(h, "Py_Finalize", My_Finalize, python_initialized);
    load_function(h, "PyDict_GetItemString", MyDict_GetItemString, python_initialized);
    load_function(h, "PyCallable_Check", MyCallable_Check, python_initialized);
    load_function(h, "PyTuple_New", MyTuple_New, python_initialized);
    load_function(h, "PyFloat_FromDouble", MyFloat_FromDouble, python_initialized);
    load_function(h, "PyObject_CallObject", MyObject_CallObject, python_initialized);
    load_function(h, "PyImport_AddModule", MyImport_AddModule, python_initialized);
    load_function(h, "PyModule_GetDict", MyModule_GetDict, python_initialized);
    load_function(h, "PyRun_SimpleString", MyRun_SimpleString, python_initialized);
    load_function(h, "PyTuple_SetItem", MyTuple_SetItem, python_initialized);
    load_function(h, "Py_DecRef", My_DecRef, python_initialized);
    load_function(h, "PyFloat_AsDouble", MyFloat_AsDouble, python_initialized);
    load_function(h, "PyDict_SetItemString", MyDict_SetItemString, python_initialized);
    load_function(h, "PyErr_Fetch", MyErr_Fetch, python_initialized);
    load_function(h, "PyErr_Display", MyErr_Display, python_initialized);
    load_function(h, "PyErr_Occurred", MyErr_Occurred, python_initialized);
}

// call a python function with a list of arguments
PyObject *call_python_function(const char *func_name, double *args, int num_args)
{
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = static_cast<PyObject *>(MyDict_GetItemString(pDict, func_name));
    if (MyCallable_Check(pFunc))
    {
        pArgs = static_cast<PyObject *>(MyTuple_New(num_args));
        for (size_t i = 0; i < num_args; i++)
        {
            MyTuple_SetItem(pArgs, i, static_cast<PyObject *>(MyFloat_FromDouble(args[i])));
        }
        pValue = static_cast<PyObject *>(MyObject_CallObject(pFunc, pArgs));
        My_DecRef(pArgs);

        if (pValue != nullptr)
        {
            // Function executed successfully
            // Add your code here to handle the result
        }
        else
        {
            //  convet func_name to a string
            std::string func_name_str(func_name);
            std::cout << "ERROR in Python function " << func_name_str << ": function execution failed" << std::endl;
            if (MyErr_Occurred())
            {
                // Fetch the error
                PyObject *pType, *pValue, *pTraceback;
                MyErr_Fetch(&pType, &pValue, &pTraceback);

                // Print the error
                MyErr_Display(pType, pValue, pTraceback);

                // Decrement reference counts for the error objects
                My_DecRef(pType);
                My_DecRef(pValue);
                My_DecRef(pTraceback);
            }
        }

        return pValue;
    }
    std::cout << "ERROR in Python function: cannot call function: " << func_name << std::endl;
    return nullptr;
}

void python_execute_code(const std::string &code)
{
    MyRun_SimpleString(code.c_str());
}

// returns the function name from the function signature, or an empty string if the function name is not found
std::string extract_function_name(const std::string &signature)
{
    if (signature.substr(0, 3) != "def")
    {
        std::cout << "ERROR in Python function: signature does not start with 'def'" << std::endl;
        return "";
    }
    std::size_t startPos = signature.find_first_not_of(" ", 3);
    if (startPos == std::string::npos)
    {
        std::cout << "ERROR in Python function: function name not found" << std::endl;
        return "";
    }
    // Find the index of the opening parenthesis
    std::size_t endPos = signature.find("(", startPos);
    if (endPos == std::string::npos)
    {
        std::cout << "ERROR in Python function: opening parenthesis not found" << std::endl;
        return "";
    }
    // Extract the function name
    return signature.substr(startPos, endPos - startPos);
}

// Search for the Python library in the directory specified by the environment variable RAD_PYTHON_PATH
// If not found, look for PYTHONHOME and search for the library in PYTHONHOME/lib
#ifdef _WIN32
// Wndows version
void python_load_library()
{
    python_initialized = true;

    // Get the string from the environment variable RAD_PYTHON_PATH
    char python_path[20000];
    int path_size = GetEnvironmentVariable("RAD_PYTHON_PATH", python_path, 20000);

    if (path_size == 0)
    {
        python_initialized = false;
    }
    else
    {
        handle = LoadLibrary(python_path);
        if (!handle)
        {
            python_initialized = false;
        }
        else
        {
            load_functions(handle, python_initialized);
        }
    }

    if (!python_initialized)
    {

        python_exec = LoadLibrary("python.exe");

        if (python_exec == NULL)
        {
            std::cout << "ERROR: No python installation found." << std::endl;
            std::cout << "       Set PATH to Python Installation or set RAD_PYTHON_PATH to the Python library" << std::endl;
            python_initialized = false;
            return;
        }

        char python_filename[2048];
        DWORD filelen = GetModuleFileName(python_exec, python_filename, 2048);
        int i = filelen;
        while (i > 0 && python_filename[i - 1] != '\\')
            --i;
        python_filename[i] = '\0';

        strcpy_s(python_path, 20000, python_filename);
        FreeLibrary(python_exec);

        std::string dir_path = std::string(python_path);

        WIN32_FIND_DATA find_file_data;
        HANDLE hFind = FindFirstFile((dir_path + "python*.dll").c_str(), &find_file_data);

        if (hFind == INVALID_HANDLE_VALUE)
        {
            std::cout << "ERROR: Could not find any python*.dll files in " << dir_path << std::endl;
            python_initialized = false;
            return;
        }

        do
        {
            std::string full_dll_path = dir_path + find_file_data.cFileName;
            handle = LoadLibrary(full_dll_path.c_str());

            if (handle)
            {
                python_initialized = true;
                // std::cout << "Trying python library: " << full_dll_path << std::endl;
                load_functions(handle, python_initialized);
                if (python_initialized)
                {
                    // std::cout << "Python library found at " << full_dll_path << std::endl;
                    FindClose(hFind);
                    My_Initialize();
                    return;
                }
                FreeLibrary(handle);
            }
        } while (FindNextFile(hFind, &find_file_data) != 0);
        FindClose(hFind);
    }
    else
    {
        My_Initialize();
    }
}

#else

// Linux only: try to load the python library at the specified path
bool try_load_library(const std::string& path) {
    bool python_initialized = false;
    handle = dlopen(path.c_str(), RTLD_LAZY);
    if (handle) {
        std::cout << "Trying python library: " << path << std::endl;
        load_functions(handle, python_initialized);
        if (python_initialized) {
            std::cout << "INFO: Python library found at " << path << std::endl;
            My_Initialize();
            if (!My_IsInitialized()) {
                std::cout << "ERROR: My_Initialize failed" << std::endl;
                python_initialized = false;
            }
        }
    }
    return python_initialized;
}

void python_load_library()
{
    python_initialized = false;
    handle = nullptr;
    // clear the previous errors in dlerror:
    dlerror();
    // Get the string from the environment variable RAD_PYTHON_PATH
    const char *python_path = getenv("RAD_PYTHON_PATH");
    if (python_path == nullptr)
    {
        python_initialized = false;
    }
    else
    {
        python_initialized = try_load_library(python_path);
    }

    // if RAD_PYTHON_PATH was not found, or if the library was not loaded, try find PYTHONHOME
    if (!python_initialized)
    {
        python_path = getenv("PYTHONHOME");
        if (python_path)
        {
            std::cout << "INFO: searching for python library in PYTHONHOME" << std::endl;
            std::vector<std::string> possible_dirs = {"/lib64/", "/lib/", "/usr/lib64/", "/usr/lib/","/usr/lib/x86_64-linux-gnu/"};
            for (const auto &dir : possible_dirs)
            {
                std::string dir_path = std::string(python_path) + dir;
                DIR *d = opendir(dir_path.c_str());
                if (d)
                {
                    struct dirent *entry;
                    while ((entry = readdir(d)) != nullptr)
                    {
                        std::string filename(entry->d_name);
                        if (filename.find("libpython") == 0 && filename.find(".so") != std::string::npos)
                        {
                            python_initialized = try_load_library(dir_path + filename);
                            if (python_initialized)
                            {
                                closedir(d);
                                return;
                            }
                        }
                    }
                    closedir(d);
                }
            }
        }
    }
    // if we reach this point, we did not find any python library
    // we look into some default locations
    std::cout << " INFO: searching for python library in default locations LD_LIBRARY_PATH" << std::endl;
    std::vector<std::string> possible_names = {
        "libpython3.12.so",
        "libpython3.11.so",
        "libpython3.10.so",
        "libpython3.9.so",
        "libpython3.8.so",
        "libpython3.7.so",
        "libpython3.6.so",
        "libpython3.5.so",
        "libpython3.4.so",
        "libpython3.3.so",
        "libpython3.2.so",
        "libpython3.1.so",
        "libpython3.so",
        "libpython3.0.so",
        "libpython2.7.so"};
    for (const auto &name : possible_names)
    {
        std::string libname = name;
        python_initialized = try_load_library(libname);
        if(python_initialized)
        {
            return;
        }
    }
}
#endif

// C++ functions that can be called from Fortran
extern "C"
{
    void cpp_python_initialize(int *ierror)
    {
        // if ierror = 1 on entry, then "-python" is missing from the starter command line, and we will not execute any python code
        if (*ierror == 1)
            return;
        *ierror = 1;
        // Load Python dynamic library
        python_load_library();
        if (python_initialized)
        {
            pDict = MyModule_GetDict(MyImport_AddModule("__main__")); // Get the main module dictionary
            if (!pDict)
            {
                std::cout << "ERROR in Python: fetching main module dictionary" << std::endl;
                return;
            }
            MyRun_SimpleString("import math"); // Import the math module for sin and other functions
            *ierror = 0;
        }
    }
    void cpp_python_finalize()
    {
        My_Finalize();
    }
    void cpp_python_execute_code(const char *code)
    {
        MyRun_SimpleString(code);
    }

    void cpp_python_initiazlize_global_variables()
    {
        if (!python_initialized)
        {
            return;
        }
        // initialize TIME and DT to 0

        PyObject *py_TIME = static_cast<PyObject *>(MyFloat_FromDouble(0.0));
        PyObject *py_DT = static_cast<PyObject *>(MyFloat_FromDouble(0.0));
        MyDict_SetItemString(pDict, "TIME", py_TIME);
        MyDict_SetItemString(pDict, "DT", py_DT);

        // loop over the set of nodes
        for (auto node_uid : nodes_uid)
        {
            std::string entity_names[] = {"C", "D", "V", "A", "VR", "AR", "DR"};
            for (auto name : entity_names)
            {
                const double x_values = static_cast<double>(1);
                const double y_values = static_cast<double>(1);
                const double z_values = static_cast<double>(1);
                PyObject *py_x_values = static_cast<PyObject *>(MyFloat_FromDouble(x_values));
                PyObject *py_y_values = static_cast<PyObject *>(MyFloat_FromDouble(y_values));
                PyObject *py_z_values = static_cast<PyObject *>(MyFloat_FromDouble(z_values));
                if (!py_x_values || !py_y_values || !py_z_values)
                {
                    std::cout << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
                    return;
                }
                std::string x_name = name + "X_" + std::to_string(node_uid);
                std::string y_name = name + "Y_" + std::to_string(node_uid);
                std::string z_name = name + "Z_" + std::to_string(node_uid);
                // std::cout<<" write to python: "<<x_name<<" "<<y_name<<" "<<z_name<<std::endl;
                MyDict_SetItemString(pDict, x_name.c_str(), py_x_values);
                MyDict_SetItemString(pDict, y_name.c_str(), py_y_values);
                MyDict_SetItemString(pDict, z_name.c_str(), py_z_values);
                // Release the Python objects
                if (py_x_values != nullptr)
                    My_DecRef(py_x_values);
                if (py_y_values != nullptr)
                    My_DecRef(py_y_values);
                if (py_z_values != nullptr)
                    My_DecRef(py_z_values);
            }
        }
        // Initialize all elements keywords to 1
        for (auto p : element_variables)
        {
            double v = static_cast<double>(1);
            PyObject *py_value = static_cast<PyObject *>(MyFloat_FromDouble(v));
            if (!py_value)
            {
                std::cout << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
            }
            // convert p.second (const char *) to std::string
            std::string keyword(p.second);
            std::string sname = keyword + "_" + std::to_string(p.first);
            MyDict_SetItemString(pDict, sname.c_str(), py_value);
            if (py_value != nullptr)
                My_DecRef(py_value);
        }
    }

    // register a function in the python dictionary
    void cpp_python_register_function(char *name, char code[], int num_lines)
    {
        std::string tmp_string;
        int current_line = 0;
        std::stringstream function_code;

        if (!python_initialized)
        {
            std::cout << "ERROR: Python not initialized" << std::endl;
            std::cout << "Make sure that the following python code is safe" << std::endl;
            std::cout << "and rerun the starter with the -python option." << std::endl;
        }

        for (int i = 0; current_line < num_lines;)
        {
            tmp_string.clear();
            // Extract characters into the temporary string until a null character is found
            while (code[i] != '\0')
            {
                tmp_string += code[i];
                i++;
            }
            if (current_line == 0)
            {
                std::string function_name = extract_function_name(tmp_string);
                if (function_name.empty())
                {
                    std::cout << "ERROR: function name not found in function signature" << std::endl;
                    return;
                }
// copy function_name into argument name
#ifdef _WIN64
                strcpy_s(name, max_line_length, function_name.c_str());
#else
                strcpy(name, function_name.c_str());
#endif
                // add the null char at the end of the string
                name[function_name.size()] = '\0';
            }

            const std::string s = parenthesis_to_underscore(tmp_string);
            extract_node_uid(s);


            extract_element_keywords(s);
            function_code << s << std::endl; // Add the line to the function code
            i++;                             // Move past the null character
            current_line++;
        }
        if (python_initialized)
        {
            // initialize the global variables found in the python function
            cpp_python_initiazlize_global_variables();
            python_execute_code(function_code.str());
        }
        else
        {
            // print the python function to stdout and stderr
            std::cout << function_code.str() << std::endl;
        }
    }

    // works for functions with 2 arguments and 1 return value
    void cpp_python_call_function(char *name, int num_args, double *args, int num_return, double *return_values)
    {
        if (!python_initialized)
        {
            return;
        }
        PyObject *result = call_python_function(name, args, num_args);
        if (result)
        {
            return_values[0] = MyFloat_AsDouble(result);
            My_DecRef(result);
        }
    }
    // this function checks if a function exists in the python dictionary
    void cpp_python_check_function(char *name, int *error)
    {
        PyObject *pFunc;
        //        std::cout << "Checking if function exists: " << name << std::endl;
        if (python_initialized)
        {
            pFunc = static_cast<PyObject *>(MyDict_GetItemString(pDict, name));
            if (MyCallable_Check(pFunc))
            {
                *error = 0;
            }
            else
            {
                *error = 1;
            }
        }
        else
        {
            *error = 1;
        }
        //        std::cout << "Function exists? " << *error << std::endl;
    }

    void cpp_python_update_time(my_real TIME, my_real DT)
    {
        if (!python_initialized)
        {
            // std::cerr << "ERROR: Python is not initialized." << std::endl;
            return;
        }

        if (!pDict)
        {
            std::cerr << "ERROR: Python main module dictionary not initialized." << std::endl;
            return;
        }
        // donvert TIME and DT to double precision in TIME2 and DT2
        double TIME2 = static_cast<double>(TIME);
        double DT2 = static_cast<double>(DT);
        // Convert C++ doubles to Python objects
        PyObject *py_TIME = static_cast<PyObject *>(MyFloat_FromDouble(TIME2));
        PyObject *py_DT = static_cast<PyObject *>(MyFloat_FromDouble(DT2));

        if (!py_TIME || !py_DT)
        {
            std::cerr << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
            return;
        }

        // Set the Python global variables in the main module's dictionary
        MyDict_SetItemString(pDict, "TIME", py_TIME);
        MyDict_SetItemString(pDict, "DT", py_DT);

        // Release the Python objects
        if (py_TIME != nullptr)
            My_DecRef(py_TIME);
        if (py_DT != nullptr)
            My_DecRef(py_DT);
    }
    // return the number of nodes that are used in the python functions
    void cpp_python_get_number_of_nodes(int *num_nodes)
    {
        if (!python_initialized)
        {
            *num_nodes = 0;
        }
        else
        {
            *num_nodes = nodes_uid.size();
        }
    }

    // return the list of nodes (user ids) that are used in the python functions
    void cpp_python_get_nodes(int *nodes_uid_array)
    {
        if (python_initialized)
        {
            int i = 0;
            for (auto node_uid : nodes_uid)
            {
                // std::cout << "Node uid: " << node_uid << std::endl;
                nodes_uid_array[i] = node_uid;
                i++;
            }
        }
    }
    // itab(i) = uid of node local node i
    void cpp_python_create_node_mapping(int *itab, int *num_nodes)
    {
        // print number of nodes and python_initialized
        // std::cout << "Number of nodes: " << *num_nodes << " python_initialized" << python_initialized << std::endl;
        if (python_initialized)
        {
            // loop over the set
            for (auto node_uid : nodes_uid)
            {
                // find i such that itab[i] = node_uid
                bool found = false;
                for (int i = 0; i < *num_nodes; i++)
                {
                    if (itab[i] == node_uid)
                    {
                        nodes_uid_to_local_id[node_uid] = i;
                        // std::cout << "Node uid: " << node_uid << " local id: " << i << std::endl;
                        found = true;
                        break;
                    }
                }
                if (!found)
                {
                    std::cout << "Node uid: " << node_uid << " not found in itab" << std::endl;
                }
            }
        }
    }

    // values is an array of size (3*numnod) containing the values of the nodal entities
    void cpp_python_update_nodal_entity(int numnod, int name_len, char *name, my_real *values)
    {
        if (!python_initialized)
        {
            return;
        }
        double x_values, y_values, z_values;
        // loop over the map nodes_uid_to_local_id
        for (auto it = nodes_uid_to_local_id.begin(); it != nodes_uid_to_local_id.end(); ++it)
        {
            int node_uid = it->first;
            int local_id = it->second;
            x_values = static_cast<double>(values[3 * local_id]);
            y_values = static_cast<double>(values[3 * local_id + 1]);
            z_values = static_cast<double>(values[3 * local_id + 2]);
            PyObject *py_x_values = static_cast<PyObject *>(MyFloat_FromDouble(x_values));
            PyObject *py_y_values = static_cast<PyObject *>(MyFloat_FromDouble(y_values));
            PyObject *py_z_values = static_cast<PyObject *>(MyFloat_FromDouble(z_values));
            if (!py_x_values || !py_y_values || !py_z_values)
            {
                std::cout << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
                return;
            }
            // Set the Python global variables in the main module's dictionary
            std::string x_name = std::string(name) + "X_" + std::to_string(node_uid);
            std::string y_name = std::string(name) + "Y_" + std::to_string(node_uid);
            std::string z_name = std::string(name) + "Z_" + std::to_string(node_uid);
            // std::cout<<" write to python: "<<x_name<<" "<<y_name<<" "<<z_name<<std::endl;
            MyDict_SetItemString(pDict, x_name.c_str(), py_x_values);
            MyDict_SetItemString(pDict, y_name.c_str(), py_y_values);
            MyDict_SetItemString(pDict, z_name.c_str(), py_z_values);
            // Release the Python objects
            if (py_x_values != nullptr)
                My_DecRef(py_x_values);
            if (py_y_values != nullptr)
                My_DecRef(py_y_values);
            if (py_z_values != nullptr)
                My_DecRef(py_z_values);
        }
    }

    // update values for elemental entities found in the Python function
    void cpp_python_update_elemental_entity(char *name, double value, int uid)
    {
        double v = static_cast<double>(value);
        PyObject *py_value = static_cast<PyObject *>(MyFloat_FromDouble(v));
        if (!py_value)
        {
            std::cout << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
        }
        std::string sname = std::string(name) + "_" + std::to_string(uid);
        // std::cout<<"[PYTHON] update elemental entity: "<<sname<<" value: "<<v<<std::endl;
        MyDict_SetItemString(pDict, sname.c_str(), py_value);
        if (py_value != nullptr)
            My_DecRef(py_value);
    }

    // return the size of the KeywordPairs element_variables
    void cpp_python_get_number_elemental_entities(int *nb)
    {
        *nb = element_variables.size();
    }

    void cpp_python_get_elemental_entity(int nb, char *name, int *uid)
    {
        size_t n = static_cast<size_t>(nb - 1);
        KeywordPair p = get_keyword_pair(element_variables, n);
        for (int i = 0; i < max_variable_length; i++)
        {
            name[i] = '\0';
        }
        // copy user id
        *uid = p.first;
        // copy variable name
#ifdef _WIN64
                strcpy_s(name, max_variable_length, p.second);
#else
        strcpy(name, p.second);
#endif
    }

} // extern "C"

#else
// dummy functions
extern "C"
{
    void cpp_python_initialize(int *ok)
    {
        // ok = 0; if not ok
        *ok = 0;
        //        std::cout << "ERROR: python not enabled" << std::endl;
    }
    void cpp_python_finalize()
    {
        //        std::cout << "ERROR: python not enabled" << std::endl;
    }
    void cpp_python_execute_code(const char *code)
    {
        //        std::cout << "ERROR: python not enabled" << std::endl;
    }
    void cpp_python_register_function(char *name, char code[500][1000], int num_lines)
    {
        //        std::cout << "ERROR: python not enabled" << std::endl;
    }
    void cpp_python_call_function(char *name, int num_args, double *args, int num_return, double *return_values)
    {
        std::cout << "ERROR: python not enabled" << std::endl;
    }
    void cpp_python_check_function(char *name, int *error)
    {
        //        std::cout << "ERROR: python not enabled" << std::endl;
    }
    void cpp_python_update_time(double TIME, double DT) {}
    void cpp_python_get_number_of_nodes(int *num_nodes) {}
    // return the list of nodes (user ids) that are used in the python functions
    void cpp_python_get_nodes(int *nodes_uid_array) {}
    void cpp_python_create_node_mapping(int *itab, int *num_nodes) {}
    void cpp_python_update_nodal_entity(int numnod, int name_len, char *name, my_real *values){}
    void cpp_python_update_elemental_entity(char *name, my_real value, int uid){}
    void cpp_python_get_number_elemental_entities(int *nb){}
    void cpp_python_get_elemental_entity(int nb, char *name, int *uid){}

}

#endif
