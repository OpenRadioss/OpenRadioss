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

// the maximum length of a line of code of python function
#ifndef __aarch64__
#define _GLIBCXX_USE_CXX11_ABI 0
#endif
#include <iostream>
#include <string>
#include <regex>
#include <array>
#include <set>
#include <array>
#include <set>
#include <utility>
#include <vector> 
#include <limits>
#include <cmath>


#include <stdio.h>

#if defined(_WIN64)
#include <BaseTsd.h>
typedef SSIZE_T My_ssize_t;
#else
#include <stddef.h>
typedef ssize_t My_ssize_t;
#endif

#define max_line_length 500
// the maximum number of lines of python function
#define max_num_lines 1000
#define max_code_length max_line_length *max_num_lines
#define max_variable_length 100

constexpr int sensor_result_size  = 2;

typedef void *PyObject;
// Corrected typedefs for Python C API function pointers
typedef PyObject *(*T_PyDict_GetItemString)(PyObject *, const char *);
typedef int (*T_PyCallable_Check)(PyObject *);
typedef PyObject *(*T_PyTuple_New)(My_ssize_t);  // Use Py_ssize_t for size
typedef PyObject *(*T_PyFloat_FromDouble)(double);
typedef PyObject *(*T_PyObject_CallObject)(PyObject *, PyObject *);
typedef void (*T_Py_Initialize)();
typedef void (*T_Py_Finalize)();
typedef PyObject *(*T_PyImport_AddModule)(const char *);
typedef PyObject *(*T_PyModule_GetDict)(PyObject *);
typedef int (*T_PyRun_SimpleString)(const char *);
typedef int (*T_PyTuple_SetItem)(PyObject *, My_ssize_t, PyObject *);  // Use Py_ssize_t for index
typedef void (*T_Py_DecRef)(PyObject *);
typedef double (*T_PyFloat_AsDouble)(PyObject *);
typedef int (*T_PyDict_SetItemString)(PyObject *, const char *, PyObject *);
typedef void (*T_PyErr_Fetch)(PyObject **, PyObject **, PyObject **);
typedef void (*T_PyErr_Display)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*T_PyErr_Occurred)();
typedef int (*T_Py_IsInitialized)();
typedef PyObject *(*T_PyObject_Str)(PyObject *);
typedef const char *(*T_PyUnicode_AsUTF8)(PyObject *);
typedef PyObject *(*T_PyDict_New)();
typedef PyObject *(*T_PyList_New)(My_ssize_t);  // Use Py_ssize_t for size
typedef int (*T_PyList_SetItem)(PyObject *, My_ssize_t, PyObject *);



// Python library handle
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
void load_function(void *h, const std::string &func_name, T &func_ptr, bool &python_initialized)
{
    func_ptr = reinterpret_cast<T>(dlsym(h, func_name.c_str()));
    if (func_ptr == nullptr)
    {
        const char *dlsym_error = dlerror();
        std::cout << "Could not load " << func_name << ": " << dlsym_error << std::endl;
        python_initialized = false;
    }
}
#endif

T_Py_Initialize My_Initialize;
T_Py_Finalize My_Finalize;
T_PyDict_GetItemString MyDict_GetItemString;
T_PyCallable_Check MyCallable_Check;
T_PyTuple_New MyTuple_New;
T_PyFloat_FromDouble MyFloat_FromDouble;
T_PyObject_CallObject MyObject_CallObject;
T_PyImport_AddModule MyImport_AddModule;
T_PyModule_GetDict MyModule_GetDict;
T_PyRun_SimpleString MyRun_SimpleString;
T_PyTuple_SetItem MyTuple_SetItem;
T_Py_DecRef My_DecRef;
T_PyFloat_AsDouble MyFloat_AsDouble;
T_PyDict_SetItemString MyDict_SetItemString;
T_PyErr_Fetch MyErr_Fetch;
T_PyErr_Display MyErr_Display;
T_PyErr_Occurred MyErr_Occurred;
T_Py_IsInitialized My_IsInitialized;
T_PyObject_Str MyObject_Str;
T_PyUnicode_AsUTF8 MyUnicode_AsUTF8;
T_PyDict_New MyDict_New;
T_PyList_New MyList_New;
T_PyList_SetItem MyList_SetItem;


constexpr std::array<const char*, 89> ELEMENT_KEYWORDS = {
"ALPHA",
"AMS",
"BFRAC",
"BULK",
"COLOR",
"DAM1",
"DAM2",
"DAM3",
"DAMA",
"DAMG",
"DAMINI",
"DENS",
"DOMAIN",
"DT",
"EINT",
"EINTM",
"EINTV",
"ENER",
"ENTH",
"ENTHM",
"ENTHV",
"EPSD",
"EPSP",
"FAIL",
"FAILURE",
"FILL",
"FLAY",
"FLDF",
"FLDZ",
"GROUP",
"HC_DSSE_F",
"HC_DSSE_Z",
"HOURGLASS",
"K",
"M151DENS",
"M151ENER",
"M151PRES",
"M151VFRAC",
"MACH",
"MASS",
"MOMX",
"MOMXY",
"MOMXZ",
"MOMY",
"MOMYZ",
"MOMZ",
"NL_EPSD",
"NL_EPSP",
"NXTF",
"OFF",
"P",
"PHI",
"SCHLIEREN",
"SIGEQ",
"SIGX",
"SIGXY",
"SIGY",
"SIGYZ",
"SIGZ",
"SIGZX",
"SSP",
"TDEL",
"TDET",
"TEMP",
"THICK",
"THIN",
"TILLOTSON",
"TSAIWU",
"TVIS",
"VDAM1",
"VDAM2",
"VDAM3",
"VELX",
"VELXY",
"VELXZ",
"VELY",
"VELYZ",
"VELZ",
"VFRAC1",
"VFRAC2",
"VFRAC3",
"VFRAC4",
"VOLU",
"VONM",
"VORT",
"VORTX",
"VORTY",
"VORTZ",
"WPLA"
};

using KeywordPair = std::pair<int, const char*>;
// Comparator for the set to order pairs by keyword and id
struct ComparePairs {
    bool operator() (const KeywordPair& lhs, const KeywordPair& rhs) const {
        if (lhs.second == rhs.second) {
            return lhs.first < rhs.first;
        }
        return std::string(lhs.second) < std::string(rhs.second);
    }
};
using KeywordPairs = std::set<std::pair<int, const char*>, ComparePairs>;

// Get a pair of keyword, id from an index: is ineficient but called only at initialization
KeywordPair get_keyword_pair(const KeywordPairs& keywordPairs, size_t n) {
    if (n >= keywordPairs.size()) {
        std::cout<<"ERROR: Out of range access in elemental value for Python function."<<std::endl;
        std::cout<<"Index: "<<n<<" Size: "<<keywordPairs.size()<<std::endl;
    }
    auto it = std::next(keywordPairs.begin(), n);
    return *it;
}
