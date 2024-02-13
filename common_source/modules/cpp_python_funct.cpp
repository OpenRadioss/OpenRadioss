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
#include "radioss_py.hpp"
#ifndef PYTHON_DISABLED

// Note on the python library used:
//
//- We check that RAD_PYTHON_PATH is defined and that it points to a valid python library. This variable must contain the full path of the *.[so|dll] file
// - if the previous check fails, we check that PYTHONHOME is defined and that it points to a valid python library (that contains /lib/libpython*.[so|dll]])
// - For Linux if the previous check fails, we look for a python library in the default locations (LD_LIBRARY_PATH)

//====================================================
//                   Global Variables
//====================================================
PyObject *pDict = nullptr;
bool python_initialized = false;

// user ids of the nodes that are used in the python functions
std::set<int> nodes_uid;

// mapping between user ids and local ids
std::map<int, int> nodes_uid_to_local_id;

// pointers to Fortran variables
my_real *C; // coordinates
my_real *D; // displacement
my_real *V;
my_real *A;
my_real *DR;
my_real *VR;
my_real *AR;

//====================================================
//                   Internal Functions
//====================================================
// load a function from a dynamic library
#ifdef _WIN32
template <typename T>
void load_function(HMODULE handle, const std::string &func_name, T &func_ptr, bool &python_initialized)
{
    func_ptr = reinterpret_cast<T>(GetProcAddress(handle, func_name.c_str()));
    if (func_ptr == nullptr)
    {
        // std::cout << "Could not load " << func_name << ": " << GetLastError() << std::endl;
        python_initialized = false;
    }
}

#else
template <typename T>
void load_function(void *handle, const std::string &func_name, T &func_ptr, bool &python_initialized)
{
    func_ptr = reinterpret_cast<T>(dlsym(handle, func_name.c_str()));
    if (func_ptr == nullptr)
    {
        const char *dlsym_error = dlerror();
        std::cout << "Could not load " << func_name << ": " << dlsym_error << std::endl;
        python_initialized = false;
    }
}
#endif

// Function to extract numbers based on the pattern and fill the global set
// void extract_uid(const std::string &input)
//{
//    Coordinates     CX_n,  CY_n,  CZ_n
//    Displacement    DX_n,  DY_n,  DZ_n
//    DisplacementR  DRX_n, DRY_n, DRZ_n
//    Velocity        VX_n,  VY_n , VZ_n
//    VelocityR      VRX_n, VRY_n, VRZ_n
//    Acceleration    AX_n,  AY_n,  AZ_n
//    AccelerationR  ARX_n, ARY_n, ARZ_n
// Regex pattern: non-alphanumeric or start of line, followed by A[XYZ] and underscore and numbers
// std::cout<<"input: "<<input<<std::endl;
//    std::regex pattern(R"((?:[^a-zA-Z0-9]|^)[ACDV][R]*[XYZ]_[0-9]+)");
//
//    auto begin = std::sregex_iterator(input.begin(), input.end(), pattern);
//    auto end = std::sregex_iterator();
//
//    for (auto i = begin; i != end; ++i)
//    {
//        auto match = *i;
//        std::string match_str = match.str();
//        size_t underscore_pos = match_str.find('_');
//        if (underscore_pos != std::string::npos)
//        {
//            int number = std::stoi(match_str.substr(underscore_pos + 1));
//            nodes_uid.insert(number);
//            // std::cout<<"[PYTHON] uid found: "<<number<<std::endl;
//        }
//    }
//
//}

void extract_uid(const std::string &input)
{

    std::regex pattern_variable(R"((?:[^a-zA-Z0-9]|^)[ACDV][R]*[XYZ]_[0-9]+)");
    std::regex pattern_decorator(R"(@requires_node\([^,]+,\s*(\d+)\))");

    std::smatch matches;

    // Check for old pattern
    auto begin_variable = std::sregex_iterator(input.begin(), input.end(), pattern_variable);
    auto end_variable = std::sregex_iterator();
    for (auto i = begin_variable; i != end_variable; ++i)
    {
        std::string match_str = i->str();
        size_t underscore_pos = match_str.find('_');
        if (underscore_pos != std::string::npos)
        {
            int number = std::stoi(match_str.substr(underscore_pos + 1));
            nodes_uid.insert(number);
            std::cout << "[PYTHON] uid found: " << number << std::endl;
        }
    }

    // Check for new pattern
    auto begin_decorator = std::sregex_iterator(input.begin(), input.end(), pattern_decorator);
    auto end_decorator = std::sregex_iterator();
    for (auto i = begin_decorator; i != end_decorator; ++i)
    {
        int number = std::stoi(i->str(1));
        nodes_uid.insert(number);
        std::cout << "[PYTHON] uid found: " << number << std::endl;
    }
}

// Here is the list of the function that are loaded from the Python library
// The template is there only to have one version of the code for both Windows and Linux
template <typename T>
void load_functions(T handle, bool &python_initialized)
{
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
    load_function(handle, "PyDict_SetItemString", PyDict_SetItemString, python_initialized);
    load_function(handle, "PyErr_Fetch", PyErr_Fetch, python_initialized);
    load_function(handle, "PyErr_Display", PyErr_Display, python_initialized);
    load_function(handle, "PyErr_Occurred", PyErr_Occurred, python_initialized);
    load_function(handle, "PyArg_ParseTuple", PyArg_ParseTuple, python_initialized);
    // load_function(handle, "Py_BuildValue", Py_BuildValue, python_initialized);
    // load_function(handle, "PyCFunction_New", PyCFunction_New, python_initialized);
    // load_function(handle, "PyObject_SetAttrString", PyObject_SetAttrString, python_initialized);
}

// call a python function with a list of arguments
PyObject *call_python_function(const char *func_name, double *args, int num_args)
{
    PyObject *pFunc, *pArgs, *pValue;
    pFunc = static_cast<PyObject *>(PyDict_GetItemString(pDict, func_name));
    if (PyCallable_Check(pFunc))
    {
        pArgs = static_cast<PyObject *>(PyTuple_New(num_args));
        for (size_t i = 0; i < num_args; i++)
        {
            PyTuple_SetItem(pArgs, i, static_cast<PyObject *>(PyFloat_FromDouble(args[i])));
        }
        pValue = static_cast<PyObject *>(PyObject_CallObject(pFunc, pArgs));
        Py_DecRef(pArgs);

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
            if (PyErr_Occurred())
            {
                // Fetch the error
                PyObject *pType, *pValue, *pTraceback;
                PyErr_Fetch(&pType, &pValue, &pTraceback);

                // Print the error
                PyErr_Display(pType, pValue, pTraceback);

                // Decrement reference counts for the error objects
                Py_DecRef(pType);
                Py_DecRef(pValue);
                Py_DecRef(pTraceback);
            }
        }

        return pValue;
    }
    std::cout << "ERROR in Python function: cannot call function: " << func_name << std::endl;
    return nullptr;
}

void python_execute_code(const std::string &code)
{
    PyRun_SimpleString(code.c_str());
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

// static PyObject *getEntity(PyObject *self, PyObject *args)
//{
//     const char *name;
//     int node_id;
//     std::cout<<"Entering getEntity"<<std::endl;
//
//     if (!PyArg_ParseTuple(args, "si", &name, &node_id))
//     {
//         std::cout<<" Error parsing arguments"<<std::endl;
//         return Py_BuildValue("d", 0.0);
//     }
//     std::cout<<"arguments parsed"<<std::endl;
//     auto it = nodes_uid_to_local_id.find(node_id);
//     if (it == nodes_uid_to_local_id.end())
//     {
//         std::cout << " ERROR in Python function: node with id " << node_id << " is not available" << std::endl;
//         return Py_BuildValue("d", 0.0);
//         //return NULL;
//     }
//     int node_local_id = it->second;
//     int pos = 3 * node_local_id;
//     std::cout<<"Local id ="<<node_local_id<<std::endl;
//
//     if (strcmp(name, "displacement") == 0 || strcmp(name, "D") == 0) // Displacement
//     {
//         return Py_BuildValue("(ddd)", D[pos], D[pos + 1], D[pos + 2]);
//     }
//     else if (strcmp(name, "velocity") == 0 || strcmp(name, "V") == 0)
//     {
//         return Py_BuildValue("(ddd)", V[pos], V[pos + 1], V[pos + 2]);
//     }
//     else if(strcmp(name, "acceleration") == 0 || strcmp(name, "A") == 0)
//     {
//         return Py_BuildValue("(ddd)", A[pos], A[pos + 1], A[pos + 2]);
//     }
//     else if (strcmp(name, "coordinates") == 0 || strcmp(name, "C") == 0)
//     {
//         return Py_BuildValue("(ddd)", C[pos], C[pos + 1], C[pos + 2]);
//     }
//     else if (strcmp(name, "displacement R") == 0 || strcmp(name, "DR") == 0)
//     {
//         return Py_BuildValue("(ddd)", DR[pos], DR[pos + 1], DR[pos + 2]);
//     }
//     else if (strcmp(name, "velocity R") == 0 || strcmp(name, "VR") == 0)
//     {
//         return Py_BuildValue("(ddd)", VR[pos], VR[pos + 1], VR[pos + 2]);
//     }
//     else if (strcmp(name, "acceleration R") == 0 || strcmp(name, "AR") == 0)
//     {
//         return Py_BuildValue("(ddd)", AR[pos], AR[pos + 1], AR[pos + 2]);
//     }
//     else if (strcmp(name, "displacement X") == 0 || strcmp(name, "DX") == 0)
//     {
//         return Py_BuildValue("d", D[pos]);
//     } else if(strcmp(name, "displacement Y") == 0 || strcmp(name, "DY") == 0)
//     {
//         return Py_BuildValue("d", D[pos + 1]);
//     } else if(strcmp(name, "displacement Z") == 0 || strcmp(name, "DZ") == 0)
//     {
//         return Py_BuildValue("d", D[pos + 2]);
//     }
//     else if (strcmp(name, "velocity X") == 0 || strcmp(name, "VX") == 0)
//     {
//         return Py_BuildValue("d", V[pos]);
//     } else if(strcmp(name, "velocity Y") == 0 || strcmp(name, "VY") == 0)
//     {
//         return Py_BuildValue("d", V[pos + 1]);
//     } else if(strcmp(name, "velocity Z") == 0 || strcmp(name, "VZ") == 0)
//     {
//         return Py_BuildValue("d", V[pos + 2]);
//     }
//     else if (strcmp(name, "acceleration X") == 0 || strcmp(name, "AX") == 0)
//     {
//         return Py_BuildValue("d", A[pos]);
//     } else if(strcmp(name, "acceleration Y") == 0 || strcmp(name, "AY") == 0)
//     {
//         return Py_BuildValue("d", A[pos + 1]);
//     } else if(strcmp(name, "acceleration Z") == 0 || strcmp(name, "AZ") == 0)
//     {
//         return Py_BuildValue("d", A[pos + 2]);
//     }
//     else if (strcmp(name, "coordinates X") == 0 || strcmp(name, "CX") == 0)
//     {
//         return Py_BuildValue("d", C[pos]);
//     } else if(strcmp(name, "coordinates Y") == 0 || strcmp(name, "CY") == 0)
//     {
//         return Py_BuildValue("d", C[pos + 1]);
//     } else if(strcmp(name, "coordinates Z") == 0 || strcmp(name, "CZ") == 0)
//     {
//         return Py_BuildValue("d", C[pos + 2]);
//     }
//     else if (strcmp(name, "displacement R X") == 0 || strcmp(name, "DRX") == 0)
//     {
//         return Py_BuildValue("d", DR[pos]);
//     } else if (strcmp(name, "displacement R Y") == 0 || strcmp(name, "DRY") == 0)
//     {
//         return Py_BuildValue("d", DR[pos + 1]);
//     } else if (strcmp(name, "displacement R Z") == 0 || strcmp(name, "DRZ") == 0)
//     {
//         return Py_BuildValue("d", DR[pos + 2]);
//     }
//     else if (strcmp(name, "velocity R X") == 0 || strcmp(name, "VRX") == 0)
//     {
//         return Py_BuildValue("d", VR[pos]);
//     } else if (strcmp(name, "velocity R Y") == 0 || strcmp(name, "VRY") == 0)
//     {
//         return Py_BuildValue("d", VR[pos + 1]);
//     } else if (strcmp(name, "velocity R Z") == 0 || strcmp(name, "VRZ") == 0)
//     {
//         return Py_BuildValue("d", VR[pos + 2]);
//     }
//     else if (strcmp(name, "acceleration R X") == 0 || strcmp(name, "ARX") == 0)
//     {
//         return Py_BuildValue("d", AR[pos]);
//     } else if (strcmp(name, "acceleration R Y") == 0 || strcmp(name, "ARY") == 0)
//     {
//         return Py_BuildValue("d", AR[pos + 1]);
//     } else if (strcmp(name, "acceleration R Z") == 0 || strcmp(name, "ARZ") == 0)
//     {
//         return Py_BuildValue("d", AR[pos + 2]);
//     }
//     else
//     {
//         std::cout << "ERROR in Python function: invalid entity name" << std::endl;
//         return Py_BuildValue("d", 0.0);
//     }
//// ... continue for all other conditions
//
//    std::cout << "ERROR in Python function: invalid entity name" << std::endl;
//    return Py_BuildValue("d", 0.0);
//}

void initialize_python()
{
    Py_Initialize();
    //    std::cout<<" Python initialized"<<std::endl;
    //// Create a Python function object from the C++ function
    //    PyMethodDef methodDef = {"getEntity", getEntity, METH_VARARGS, "Get entity data"};
    //    std::cout<<"methodDef done"<<std::endl;
    //
    //    PyObject *pythonFunction = PyCFunction_New(&methodDef, NULL);
    //    std::cout<<"PyCFunction_New done"<<std::endl;
    //    PyObject * mainModule = PyImport_AddModule("__main__");
    //    PyObject_SetAttrString(mainModule, "getEntity", pythonFunction);
    //    std::cout<<" getEntity function registered in Python"<<std::endl;
    //        PyRun_SimpleString(
    //        "import sys\n"
    //        "print('Python version')\n"
    //        "print(sys.version)\n"
    //        "print('Version info.')\n"
    //        "print(sys.version_info)\n"
    //        "result = getEntity('displacement', 100)\n"
    //        "print(result)"
    //    );
    //    std::cout<<"Success: getEntity function called in Python"<<std::endl;

    // definition for the decorator @requires_node
    //        std::cout<<"Entering requires_node"<<std::endl;
    PyRun_SimpleString(
        "def requires_node(name, node_id):\n"
        "    def decorator(func):\n"
        "        def wrapper(*args, **kwargs):\n"
        "            kwargs[name] = node_id\n"
        "            return func(*args, **kwargs)\n"
        "        return wrapper\n"
        "    return decorator\n");
    //    COORDINATES     CX_n,  CY_n,  CZ_n
    //    DISPLACEMENTS    DX_n,  DY_n,  DZ_n
    //    DISPLACEMENTS_R  DRX_n, DRY_n, DRZ_n
    //    VELOCITIES        VX_n,  VY_n , VZ_n
    //    VELOCITIES_R     VRX_n, VRY_n, VRZ_n
    //    ACCELERATIONS    AX_n,  AY_n,  AZ_n
    //    ACCELERATIONS_R  ARX_n, ARY_n, ARZ_n

    PyRun_SimpleString(
        "def getEntity(name, node_id):\n"
        "    if(node_id not in DISPLACEMENTS ):\n"
        "        raise ValueError('Node with id {} is not available'.format(node_id))\n"
        "    elif(name == 'coordinates'):\n"
        "        return COORDINATES[node_id]\n"
        "    elif(name == 'displacements'):\n"
        "        return DISPLACEMENTS[node_id]\n"
        "    elif(name == 'velocities'):\n"
        "        return VELOCITIES[node_id]\n"
        "    elif(name == 'accelerations'):\n"
        "        return ACCELERATIONS[node_id]\n"
        "    elif(name == 'rotational displacements'):\n"
        "        return DISPLACEMENTS_R[node_id]\n"
        "    elif(name == 'rotational velocities'):\n"
        "        return VELOCTIES_R[node_id]\n"
        "    elif(name == 'rotational accelerations'):\n"
        "        return ACCELERATION_R[node_id]\n"
        "    elif(name == 'X displacement'):\n"
        "        return DISPLACEMENTS[node_id][0]\n"
        "    elif(name == 'Y displacement'):\n"
        "        return DISPLACEMENTS[node_id][1]\n"
        "    elif(name == 'Z displacement'):\n"
        "        return DISPLACEMENTS[node_id][2]\n"
        "    elif(name == 'X velocity'):\n"
        "        return VELOCITIES[node_id][0]\n"
        "    elif(name == 'Y velocity'):\n"
        "        return VELOCITIES[node_id][1]\n"
        "    elif(name == 'Z velocity'):\n"
        "        return VELOCITIES[node_id][2]\n"
        "    elif(name == 'X acceleration'):\n"
        "        return ACCELERATIONS[node_id][0]\n"
        "    elif(name == 'Y acceleration'):\n"
        "        return ACCELERATIONS[node_id][1]\n"
        "    elif(name == 'Z acceleration'):\n"
        "        return ACCELERATIONS[node_id][2]\n"
        "    elif(name == 'X coordinate'):\n"
        "        return COORDINATES[node_id][0]\n"
        "    elif(name == 'Y coordinate'):\n"
        "        return COORDINATES[node_id][1]\n"
        "    elif(name == 'Z coordinate'):\n"
        "        return COORDINATES[node_id][2]\n"
        "    else:\n"
        "        raise ValueError('Invalid entity name: {}'.format(name))\n" 
    );
    PyRun_SimpleString(
        "COORDINATES = {}\n"
        "DISPLACEMENTS = {}\n"
        "VELOCITIES = {}\n"
        "ACCELERATIONS = {}\n"
        "DISPLACEMENTS_R = {}\n"
        "VELOCITIES_R = {}\n"
        "ACCELERATIONS_R ={}\n");

}

// Search for the Python library in the directory specified by the environment variable RAD_PYTHON_PATH
// If not found, look for PYTHONHOME and search for the library in PYTHONHOME/lib
#ifdef _WIN32
// Wndows version
void python_load_library()
{
    python_initialized = true;
    HMODULE handle = NULL;
    HMODULE python_exec = NULL;

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
                    initialize_python();
                    return;
                }
                FreeLibrary(handle);
            }
        } while (FindNextFile(hFind, &find_file_data) != 0);
        FindClose(hFind);
    }
    else
    {
        initialize_python();
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
                                initialize_python();
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
                    initialize_python();
                    return;
                }
                dlclose(handle);
            }
        }
    }
    else // if python_initialized == true we have found the library in RAD_PYTHON_PATH
    {
        initialize_python();
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

    void cpp_python_initiazlize_global_variables()
    {
        if (!python_initialized)
        {
            return;
        }
        // initialize TIME and DT to 0

        PyObject *py_TIME = static_cast<PyObject *>(PyFloat_FromDouble(0.0));
        PyObject *py_DT = static_cast<PyObject *>(PyFloat_FromDouble(0.0));
        PyDict_SetItemString(pDict, "TIME", py_TIME);
        PyDict_SetItemString(pDict, "DT", py_DT);
        std::string initCommand =
            std::string("COORDINATES = {}\n") +
            "DISPLACEMENTS = {}\n" +
            "VELOCITIES = {}\n" +
            "ACCELERATIONS = {}\n" +
            "DISPLACEMENTS_R = {}\n" +
            "VELOCITIES_R = {}\n" +
            "ACCELERATIONS_R = {}\n";
        // loop over the set of nodes
        for (auto node_uid : nodes_uid)
        {
            std::string entity_names[] = {"C", "D", "V", "A", "VR", "AR", "DR"};
            for (auto name : entity_names)
            {
                const double x_values = static_cast<double>(1);
                const double y_values = static_cast<double>(1);
                const double z_values = static_cast<double>(1);
                PyObject *py_x_values = static_cast<PyObject *>(PyFloat_FromDouble(x_values));
                PyObject *py_y_values = static_cast<PyObject *>(PyFloat_FromDouble(y_values));
                PyObject *py_z_values = static_cast<PyObject *>(PyFloat_FromDouble(z_values));
                if (!py_x_values || !py_y_values || !py_z_values)
                {
                    std::cout << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
                    return;
                }
                std::string x_name = name + "X_" + std::to_string(node_uid);
                std::string y_name = name + "Y_" + std::to_string(node_uid);
                std::string z_name = name + "Z_" + std::to_string(node_uid);
                // std::cout<<" write to python: "<<x_name<<" "<<y_name<<" "<<z_name<<std::endl;
                PyDict_SetItemString(pDict, x_name.c_str(), py_x_values);
                PyDict_SetItemString(pDict, y_name.c_str(), py_y_values);
                PyDict_SetItemString(pDict, z_name.c_str(), py_z_values);
                // Release the Python objects
                if (py_x_values != nullptr)
                    Py_DecRef(py_x_values);
                if (py_y_values != nullptr)
                    Py_DecRef(py_y_values);
                if (py_z_values != nullptr)
                    Py_DecRef(py_z_values);
            }
            initCommand +=
                "COORDINATES["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "COORDINATES["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "DISPLACEMENTS["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "VELOCITIES["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "ACCELERATIONS["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "DISPLACEMENTS_R["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "VELOCITIES_R["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n" +
                "ACCELERATIONS_R["+std::to_string(node_uid)+"] = [0.0, 0.0, 0.0]\n";

        }
        PyRun_SimpleString(initCommand.c_str());
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
        }

        bool signature_found = false;
        for (int i = 0; current_line < num_lines;)
        {
            tmp_string.clear();
            // Extract characters into the temporary string until a null character is found
            while (code[i] != '\0')
            {
                tmp_string += code[i];
                i++;
            }
            if (!signature_found && tmp_string.find("def") == 0)
            {
                signature_found = true;
                // print tmp_string
                std::cout << "tmp_string: " << tmp_string << std::endl;
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
            extract_uid(tmp_string);
            //            std::cout << tmp_string << std::endl;  // Print the line (optional)
            function_code << tmp_string << std::endl; // Add the line to the function code
            i++;                                      // Move past the null character
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
        PyObject *py_TIME = static_cast<PyObject *>(PyFloat_FromDouble(TIME2));
        PyObject *py_DT = static_cast<PyObject *>(PyFloat_FromDouble(DT2));

        if (!py_TIME || !py_DT)
        {
            std::cerr << "ERROR: Failed to create Python objects from C++ doubles." << std::endl;
            return;
        }

        // Set the Python global variables in the main module's dictionary
        PyDict_SetItemString(pDict, "TIME", py_TIME);
        PyDict_SetItemString(pDict, "DT", py_DT);

        // Release the Python objects
        if (py_TIME != nullptr)
            Py_DecRef(py_TIME);
        if (py_DT != nullptr)
            Py_DecRef(py_DT);
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
        std::string command;
        std::string full_name;
        std::string short_name = std::string(name);
        if (short_name == "A")
        {
            full_name = "ACCELERATIONS";
        }
        else if (short_name == "V")
        {
            full_name = "VELOCITIES";
        }
        else if (short_name == "D")
        {
            full_name = "DISPLACEMENTS";
        }
        else if (short_name == "DR")
        {
            full_name = "DISPLACEMENTS_R";
        }
        else if (short_name == "VR")
        {
            full_name = "VELOCITIES_R";
        }
        else if (short_name == "AR")
        {
            full_name = "ACCELERATIONS_R";
        }
        else if (short_name == "C")
        {
            full_name = "COORDINATES";
        }
        else
        {
            std::cout << "PYTHON ERROR: invalid entity name: " << name << std::endl;
            return;
        }

        for (auto it = nodes_uid_to_local_id.begin(); it != nodes_uid_to_local_id.end(); ++it)
        {
            int node_uid = it->first;
            int local_id = it->second;
            x_values = static_cast<double>(values[3 * local_id]);
            y_values = static_cast<double>(values[3 * local_id + 1]);
            z_values = static_cast<double>(values[3 * local_id + 2]);
            PyObject *py_x_values = static_cast<PyObject *>(PyFloat_FromDouble(x_values));
            PyObject *py_y_values = static_cast<PyObject *>(PyFloat_FromDouble(y_values));
            PyObject *py_z_values = static_cast<PyObject *>(PyFloat_FromDouble(z_values));
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
            PyDict_SetItemString(pDict, x_name.c_str(), py_x_values);
            PyDict_SetItemString(pDict, y_name.c_str(), py_y_values);
            PyDict_SetItemString(pDict, z_name.c_str(), py_z_values);

            // A[local_id] = [AX_n, AY_n, AZ_n]
            command += full_name + "[" + std::to_string(node_uid) +
                       "] = [" + std::string(name) + "X_" + std::to_string(node_uid) +
                       ", " + std::string(name) + "Y_" + std::to_string(node_uid) +
                       ", " + std::string(name) + "Z_" + std::to_string(node_uid) +
                       "]\n";
           // std::cout<<command<<std::endl;

            // Release the Python objects
            if (py_x_values != nullptr)
                Py_DecRef(py_x_values);
            if (py_y_values != nullptr)
                Py_DecRef(py_y_values);
            if (py_z_values != nullptr)
                Py_DecRef(py_z_values);
        }
        PyRun_SimpleString(command.c_str());
    }

    // initialize the global variables found in the python function
    void cpp_python_copy_pointers(int *numnod, my_real *x, my_real *d, my_real *v, my_real *a, my_real *dr, my_real *vr, my_real *ar)
    {
        C = x;
        D = d;
        V = v;
        A = a;
        DR = dr;
        VR = vr;
        AR = ar;
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

    void cpp_python_copy_pointers(int *numnod, my_real *x, my_real *d, my_real *v, my_real *a, my_real *dr, my_real *vr, my_real *ar) {] }
}

#endif
