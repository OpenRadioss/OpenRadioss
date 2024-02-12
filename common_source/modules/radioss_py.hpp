#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cstring>
#include <vector>
#include <set>
#include <map>
#include <unordered_map>
#include <regex>
#ifdef _WIN32
/* Windows includes */
#include <windows.h>
#else
#include <dlfcn.h>
#include <dirent.h>
#endif

#ifdef MYREAL8 
//double precision define my_real as double
typedef double my_real;
#else
typedef float my_real;
#endif
// the maximum length of a line of code of python function
#define max_line_length 500
// the maximum number of lines of python function
#define max_num_lines 1000
#define max_code_length max_line_length *max_num_lines

#ifndef PYTHON_DISABLED


constexpr int METH_VARARGS = 0x0001;
typedef void *PyObject;

typedef PyObject* (*PyCFunction)(PyObject *, PyObject *);
typedef struct PyMethodDef {
    const char  *ml_name;   // The name of the built-in function/method
    PyCFunction  ml_meth;   // The C function that implements it
    int          ml_flags;  // Combination of METH_xxx flags
    const char  *ml_doc;    // The __doc__ attribute, or NULL
} PyMethodDef;


typedef PyObject (*T_PyDict_GetItemString)(PyObject *, const char *);
typedef int (*T_PyCallable_Check)(PyObject *);
typedef PyObject (*T_PyTuple_New)(int);
typedef PyObject (*T_PyFloat_FromDouble)(double);
typedef PyObject (*T_PyObject_CallObject)(PyObject *, PyObject *);
typedef void (*T_Py_Initialize)();
typedef void (*T_Py_Finalize)();
typedef PyObject *(*T_PyImport_AddModule)(const char *);
typedef PyObject *(*T_PyModule_GetDict)(PyObject *);
typedef int (*T_PyRun_SimpleString)(const char *);
typedef int (*T_PyTuple_SetItem)(PyObject *, int, PyObject *);
typedef void (*T_Py_DecRef)(PyObject *);
typedef double (*T_PyFloat_AsDouble)(PyObject *);
typedef int (*T_PyDict_SetItemString)(PyObject *, const char *, PyObject *);
typedef void (*T_PyErr_Fetch)(PyObject **, PyObject **, PyObject **);
typedef void (*T_PyErr_Display)(PyObject *, PyObject *, PyObject *);
typedef int (*T_PyArg_ParseTuple)(PyObject *, const char *, ...);
typedef PyObject *(*T_Py_BuildValue)(const char *, ...);
typedef PyObject *(*T_PyErr_Occurred)();
typedef PyObject* (*T_PyCFunction_New)(PyMethodDef *, PyObject *);
typedef int (*T_PyObject_SetAttrString)(PyObject *, const char *, PyObject *);

T_Py_Initialize Py_Initialize;
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
T_PyDict_SetItemString PyDict_SetItemString;
T_PyErr_Fetch PyErr_Fetch;
T_PyErr_Display PyErr_Display;
T_PyErr_Occurred PyErr_Occurred;
T_PyArg_ParseTuple PyArg_ParseTuple;
T_Py_BuildValue Py_BuildValue;
T_PyCFunction_New PyCFunction_New;
T_PyObject_SetAttrString PyObject_SetAttrString;

#endif