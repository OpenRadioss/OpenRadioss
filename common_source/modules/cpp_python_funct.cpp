//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
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
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstring>
#include <vector>
#ifndef PYTHON_DISABLED
//#include <Python.h>
#ifdef _WIN32
/* Windows includes */
#include <windows.h>
#else
#include <dlfcn.h>
#include <dirent.h>
#endif
typedef void* PyObject;

typedef PyObject (*T_PyDict_GetItemString)(PyObject *, const char*);
typedef int (*T_PyCallable_Check)(PyObject *);
typedef PyObject (*T_PyTuple_New)(int);
typedef PyObject (*T_PyFloat_FromDouble)(double);
typedef PyObject (*T_PyObject_CallObject)(PyObject *, PyObject*);
typedef void (*T_Py_Initialize)();
typedef void (*T_Py_Finalize)();
typedef PyObject* (*T_PyImport_AddModule)(const char *);
typedef PyObject* (*T_PyModule_GetDict)(PyObject *);
typedef int (*T_PyRun_SimpleString)(const char *);
typedef int (*T_PyTuple_SetItem)(PyObject *, int, PyObject *);
typedef void (*T_Py_DecRef)(PyObject *);
typedef double (*T_PyFloat_AsDouble)(PyObject *);

// Note on the python library used:
//
//- We check that RAD_PYTHON_PATH is defined and that it points to a valid python library. This variable must contain the full path of the *.[so|dll] file
// - if the previous check fails, we check that PYTHONHOME is defined and that it points to a valid python library (that contains /lib/libpython*.[so|dll]])
// - For Linux if the previous check fails, we look for a python library in the default locations (LD_LIBRARY_PATH)

// load a function from a dynamic library
#ifdef _WIN32
template<typename T>
void load_function(HMODULE handle, const std::string& func_name, T& func_ptr, bool& python_initialized) {
    func_ptr = reinterpret_cast<T>(GetProcAddress(handle, func_name.c_str()));
    if (func_ptr == nullptr) {
        std::cout << "Could not load " << func_name << ": " << GetLastError() << std::endl;
        python_initialized = false;
    }
}

#else
template<typename T>
void load_function(void* handle, const std::string& func_name, T& func_ptr, bool& python_initialized) {
    func_ptr = reinterpret_cast<T>(dlsym(handle, func_name.c_str()));
    if (func_ptr == nullptr) {
        const char* dlsym_error = dlerror();
        std::cout << "Could not load " << func_name << ": " << dlsym_error << std::endl;
        python_initialized = false;
    }
}
#endif


T_Py_Initialize  Py_Initialize;
T_Py_Finalize Py_Finalize;
T_PyDict_GetItemString PyDict_GetItemString;
T_PyCallable_Check PyCallable_Check;
T_PyTuple_New PyTuple_New;
T_PyFloat_FromDouble PyFloat_FromDouble;
T_PyObject_CallObject PyObject_CallObject;
T_PyImport_AddModule PyImport_AddModule;
T_PyModule_GetDict PyModule_GetDict;
T_PyRun_SimpleString PyRun_SimpleString;
T_PyTuple_SetItem PyTuple_SetItem;
T_Py_DecRef Py_DecRef;
T_PyFloat_AsDouble PyFloat_AsDouble;

// global variables
PyObject *pDict = nullptr;
bool python_initialized = false;


// Here is the list of the function that are loaded from the Python library
// The template is there only to have one version of the code for both Windows and Linux
template<typename T>
void load_functions(T handle, bool& python_initialized) {
    python_initialized = true;
    load_function(handle, "Py_Initialize", Py_Initialize, python_initialized);
    load_function(handle, "Py_Finalize", Py_Finalize, python_initialized);
    load_function(handle, "PyDict_GetItemString", PyDict_GetItemString, python_initialized);
    load_function(handle, "PyCallable_Check", PyCallable_Check, python_initialized);
    load_function(handle, "PyTuple_New", PyTuple_New, python_initialized);
    load_function(handle, "PyFloat_FromDouble", PyFloat_FromDouble, python_initialized);
    load_function(handle, "PyObject_CallObject", PyObject_CallObject, python_initialized);
    load_function(handle, "PyImport_AddModule", PyImport_AddModule, python_initialized);
    load_function(handle, "PyModule_GetDict", PyModule_GetDict, python_initialized);
    load_function(handle, "PyRun_SimpleString", PyRun_SimpleString, python_initialized);
    load_function(handle, "PyTuple_SetItem", PyTuple_SetItem, python_initialized);
    load_function(handle, "Py_DecRef", Py_DecRef, python_initialized);
    load_function(handle, "PyFloat_AsDouble", PyFloat_AsDouble, python_initialized);
}


// call a python function with a list of arguments
PyObject* call_python_function(const char* func_name, double * args, int num_args) {
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = static_cast<PyObject*>(PyDict_GetItemString(pDict, func_name));
    if (PyCallable_Check(pFunc)) {
        pArgs = static_cast<PyObject*>(PyTuple_New(num_args));
        for( size_t i = 0; i < num_args; i++ ) {
            PyTuple_SetItem(pArgs, i, static_cast<PyObject*>(PyFloat_FromDouble(args[i])));
        }
        pValue = static_cast<PyObject*>(PyObject_CallObject(pFunc, pArgs));
        Py_DecRef(pArgs);
        return pValue;
    }
    std::cout << "ERROR in Python function: cannot call function:"<<func_name << std::endl;
    return nullptr;
}

void python_execute_code(const std::string& code) {
    PyRun_SimpleString(code.c_str());
}

// returns the function name from the function signature, or an empty string if the function name is not found
std::string extract_function_name(const std::string& signature) {
    if (signature.substr(0, 3) != "def") {
        std::cout<<"ERROR in Python function: signature does not start with 'def'"<<std::endl;
        return "";  
    }
    std::size_t startPos = signature.find_first_not_of(" ", 3);
    if(startPos == std::string::npos) {
        std::cout<<"ERROR in Python function: function name not found"<<std::endl;
        return ""; 
    }
    // Find the index of the opening parenthesis
    std::size_t endPos = signature.find("(", startPos);
    if(endPos == std::string::npos) {
        std::cout<<"ERROR in Python function: opening parenthesis not found"<<std::endl;
        return "";
    }
    // Extract the function name
    return signature.substr(startPos, endPos - startPos);
}

// Search for the Python library in the directory specified by the environment variable RAD_PYTHON_PATH
// If not found, look for PYTHONHOME and search for the library in PYTHONHOME/lib
#ifdef _WIN32
//Wndows version
void python_load_library()
{
    python_initialized = true;
    HMODULE handle = NULL;




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


        path_size = GetEnvironmentVariable("PYTHONHOME", python_path, 20000);

        if (path_size == 0)
        {
            std::cout << "ERROR: Could not find any python library in PYTHONHOME= " << python_path << std::endl;
            python_initialized = false;
            return;
        }

        std::string dir_path = std::string(python_path) + "\\lib\\";

        WIN32_FIND_DATA find_file_data;
        HANDLE hFind = FindFirstFile((dir_path + "python*.dll").c_str(), &find_file_data);

        if (hFind == INVALID_HANDLE_VALUE)
        {
            std::cout << "ERROR: Could not find any python*.dll files in " << dir_path << std::endl;
            python_initialized = false;
            return;
        }

        do {
            std::string full_dll_path = dir_path + find_file_data.cFileName;
            handle = LoadLibrary(full_dll_path.c_str());

            if (handle)
            {
                python_initialized = true;
                std::cout << "Trying python library: " << full_dll_path << std::endl;
                load_functions(handle, python_initialized);
                if (python_initialized)
                {
                    std::cout << "Python library found at " << full_dll_path << std::endl;
                    FindClose(hFind);
                    Py_Initialize();
                    return;
                }
                FreeLibrary(handle);
            }
        } while (FindNextFile(hFind, &find_file_data) != 0);
        FindClose(hFind);
    }
    else
    {
        Py_Initialize();
    }
}

#else
void python_load_library()
{
    python_initialized = false;
    void *handle = nullptr;
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
        handle = dlopen(python_path, RTLD_LAZY | RTLD_GLOBAL);
        if (!handle)
        {
            python_initialized = false;
            std::cout << "WARNING: Could not find any python library in RAD_PYTHON_PATH= " << python_path << std::endl;

        }
        else
        {
            load_functions(handle, python_initialized);
        }
    }

    // if RAD_PYTHON_PATH was not found, or if the library was not loaded, try find PYTHONHOME
    if (python_initialized == false)
    {
        python_path = getenv("PYTHONHOME");
        // Construct the directory path
        if (python_path != nullptr)
        {
            std::cout << "INFO: searching for python library in PYTHONHOME" << std::endl;
            std::string dir_path = std::string(python_path) + "/lib/";
            // Open the directory
            DIR *dir = opendir(dir_path.c_str());
            if (dir == nullptr)
            {
                std::cout << "WARNING: Could not find any python library in PYTHONHOME= " << dir_path << std::endl;
                python_initialized = false;
            }
            else
            {
                struct dirent *entry;

                while ((entry = readdir(dir)) != nullptr)
                {
                    std::string filename(entry->d_name);
                    if (filename.find("libpython") == 0 && filename.find(".so") != std::string::npos)
                    {
                        std::string full_so_path = dir_path + filename;
                        handle = dlopen(full_so_path.c_str(), RTLD_LAZY | RTLD_GLOBAL);
                        if (handle)
                        {
                            std::cout << "Trying python library: " << full_so_path << std::endl;
                            load_functions(handle, python_initialized);
                            if (python_initialized)
                            {
                                std::cout << "Python library found at " << full_so_path << std::endl;
                                closedir(dir);
                                Py_Initialize();
                                return;
                            }
                            dlclose(handle);
                        }
                    }
                }
                closedir(dir);
            }
        }
        // if we reach this point, we did not find any python library
        // we look into some default locations
        std::cout<<" INFO: searching for python library in default locations LD_LIBRARY_PATH"<<std::endl;
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
            "libpython3.0.so",
            "libpython2.7.so"};
        for (const auto &name : possible_names)
        {
            void *handle = dlopen(name.c_str(), RTLD_LAZY);
            if (handle != nullptr)
            {
                load_functions(handle, python_initialized);
                if (python_initialized)
                {
                    std::cout << "Python library found " << name << std::endl;
                    Py_Initialize();
                    return;
                }
                dlclose(handle);
            }
        }
    }
    else // if python_initialized == true we have found the library in RAD_PYTHON_PATH
    {
        Py_Initialize();
    }
}
#endif

// C++ functions that can be called from Fortran
extern "C"
{
    void cpp_python_initialize(int *ierror)
    {
        // if ierror = 1 on entry, then "-python" is missing from the starter command line, and we will not execute any python code
        if(*ierror == 1) return;
        *ierror = 1;
        // Load Python dynamic library
        python_load_library();
        if (python_initialized)
        {
            pDict = PyModule_GetDict(PyImport_AddModule("__main__")); // Get the main module dictionary
            if (!pDict)
            {
                std::cout << "ERROR in Python: fetching main module dictionary" << std::endl;
                return;
            }
            PyRun_SimpleString("import math"); // Import the math module for sin and other functions
            *ierror = 0;
        }
    }
    void cpp_python_finalize()
    {
        Py_Finalize();
    }
    void cpp_python_execute_code(const char *code)
    {
        PyRun_SimpleString(code);
    }

    // register a function in the python dictionary
    void cpp_python_register_function(char *name, char code[], int num_lines)
    {
        std::string tmp_string;
        int current_line = 0;
        std::stringstream function_code;

        if(!python_initialized){
            std::cout << "ERROR: Python not initialized" << std::endl;
            std::cout << "Make sure that the following python code is safe" << std::endl;
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
                //                std::cout << "Registering function: " << function_name << std::endl;
                // copy function_name into argument name
                strcpy(name, function_name.c_str());
                // add the null char at the end of the string
                name[function_name.size()] = '\0';
            }
            //            std::cout << tmp_string << std::endl;  // Print the line (optional)
            function_code << tmp_string << std::endl; // Add the line to the function code
            i++;                                      // Move past the null character
            current_line++;
        }
        if(python_initialized)
        {
            python_execute_code(function_code.str());
        }else
        {
            // print the python function to stdout and stderr
            std::cout << function_code.str() << std::endl;
//            std::cout << function_code.str() << std::endl;
        }
        //        std::cout<<" function registered"<<std::endl;
    }

    // works for functions with 2 arguments and 1 return value
    void cpp_python_call_function(char *name, int num_args, double *args, int num_return, double *return_values)
    {
        //        std::cout<<"Calling function: "<<name<<std::endl;
        if( !python_initialized ) {
            return;
        }
        PyObject *result = call_python_function(name, args, num_args);
        if (result)
        {
            return_values[0] = PyFloat_AsDouble(result);
            Py_DecRef(result);
        }
    }
    // this function checks if a function exists in the python dictionary
    void cpp_python_check_function(char *name, int *error)
    {
        PyObject *pFunc;
        //        std::cout << "Checking if function exists: " << name << std::endl;
        if (python_initialized)
        {
            pFunc = static_cast<PyObject *>(PyDict_GetItemString(pDict, name));
            if (PyCallable_Check(pFunc))
            {
                *error = 0;
            }
            else
            {
                *error = 1;
            }
        } else
        {
            *error = 1;
        }
        //        std::cout << "Function exists? " << *error << std::endl;
    }
}

#else
// dummy functions
extern "C"
{
    void cpp_python_initialize(int * ok)
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
}

#endif
