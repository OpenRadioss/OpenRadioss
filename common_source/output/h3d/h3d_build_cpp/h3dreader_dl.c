//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 2026 Siemens
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Simcenter Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
//Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
//Copyright>        commercial version may interest you: 
//Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
//    
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#define _FCALL

#ifdef _WIN32
/* Windows includes */
#include <windows.h>
#include <process.h>
#include <io.h>
#include <sys\types.h>
#include <sys/stat.h>

#elif 1
/* Linux includes */
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <stdbool.h>

#endif

/* Opaque handle for H3D reader file (same underlying type as writer H3DFileInfo) */
typedef void H3DReaderInfo;

/* Message / error callback types */
typedef void (*H3DMessageFunctionType)(H3DReaderInfo* context, const char* msg);
typedef void (*H3DErrorFunctionType)(H3DReaderInfo* context, const char* msg);


#ifdef _WIN32
char * h3dreaderlib = "h3dreader.dll";
char   libh3dreaderpath[20000];
char   h3dreader_hwarch[200];
HINSTANCE h3dreaderhandle;
#elif 1
char * h3dreaderlib = "libh3dreader.so";
char * libh3dreaderpath;
void  *h3dreaderhandle;
#endif
char h3dreader_load_libname[20000];

/* -----------------------------------------------------------------------
 * Function pointer declarations for libh3dreader public API
 * Only Open/Close are declared here; all callback-based API functions
 * are resolved on-demand via h3dreader_get_handle() + dlsym in C++ code.
 * ----------------------------------------------------------------------- */

H3DReaderInfo* (*DLHyper3DImportOpen)(const char* filename,
                                      H3DMessageFunctionType mFunc,
                                      H3DErrorFunctionType   eFunc);

bool (*DLHyper3DImportClose)(H3DReaderInfo* h3d_file);

bool (*DLHyper3DLookupString)(H3DReaderInfo* h3d_file, uint32_t str_id, const char** string);


/* -----------------------------------------------------------------------
 * h3dreaderlib_load_
 *   Called from Fortran to load libh3dreader.so at runtime via dlopen.
 *   Pattern mirrors h3dlib_load_ in h3d_dl.c (writer).
 *
 *   Returns (IERROR):
 *     0  = success
 *     1  = dlopen failure (library not found)
 *     >1 = dlopen success but one or more dlsym resolutions failed
 * ----------------------------------------------------------------------- */
#ifdef _WIN32
void h3dreaderlib_load_(int * IERROR)
{
  int ierr, dllpath_size, arch_size;
  h3dreaderhandle = NULL;
  *IERROR = 0;
  ierr    = 0;
  memset(h3dreader_load_libname, 0, sizeof(h3dreader_load_libname));

  dllpath_size = GetEnvironmentVariable("RAD_H3D_PATH", libh3dreaderpath, 20000);
  if (dllpath_size > 0) {
    /* First trial: RAD_H3D_PATH */
    strcpy_s(h3dreader_load_libname, 20000, libh3dreaderpath);
    strcat_s(h3dreader_load_libname, 20000, "\\");
    strcat_s(h3dreader_load_libname, 20000, h3dreaderlib);
    h3dreaderhandle = LoadLibrary(TEXT(h3dreader_load_libname));
  }

  if (!h3dreaderhandle) {
    /* Second trial: current working directory */
    dllpath_size = GetCurrentDirectory(20000, libh3dreaderpath);
    strcpy_s(h3dreader_load_libname, 20000, libh3dreaderpath);
    strcat_s(h3dreader_load_libname, 20000, "\\");
    strcat_s(h3dreader_load_libname, 20000, h3dreaderlib);
    h3dreaderhandle = LoadLibrary(TEXT(h3dreader_load_libname));
  }

  if (!h3dreaderhandle) {
    /* Third trial: %ALTAIR_HOME%\hwsolvers\common\bin\%ARCH% */
    dllpath_size = GetEnvironmentVariable("ALTAIR_HOME", libh3dreaderpath, 20000);
    if (dllpath_size > 0) {
      arch_size = GetEnvironmentVariable("ARCH", h3dreader_hwarch, 200);
      if (arch_size > 0) {
        strcpy_s(h3dreader_load_libname, 20000, libh3dreaderpath);
        strcat_s(h3dreader_load_libname, 20000, "\\hwsolvers\\common\\bin\\");
        strcat_s(h3dreader_load_libname, 20000, h3dreader_hwarch);
        strcat_s(h3dreader_load_libname, 20000, "\\");
        strcat_s(h3dreader_load_libname, 20000, h3dreaderlib);
        h3dreaderhandle = LoadLibrary(TEXT(h3dreader_load_libname));
      }
    }
  }

  if (!h3dreaderhandle) {
    /* Fourth trial: PATH settings */
    dllpath_size = GetEnvironmentVariable("PATH", libh3dreaderpath, 20000);
    SetDllDirectory(libh3dreaderpath);
    h3dreaderhandle = LoadLibrary(TEXT(h3dreader_load_libname));
  }

  if (h3dreaderhandle) {

    DLHyper3DImportOpen = (void*)GetProcAddress(h3dreaderhandle, "Hyper3DImportOpen");
    if (!DLHyper3DImportOpen) ierr++;

    DLHyper3DImportClose = (void*)GetProcAddress(h3dreaderhandle, "Hyper3DImportClose");
    if (!DLHyper3DImportClose) ierr++;

    DLHyper3DLookupString    = (void*)GetProcAddress(h3dreaderhandle, "Hyper3DLookupString");

    if (!DLHyper3DLookupString) ierr++;

    if (ierr > 0) { *IERROR = ierr + 1; }

  } else {
    *IERROR = 1;
  }
}

#elif 1
void h3dreaderlib_load_(int * IERROR)
{
  int ierr;
  h3dreaderhandle = NULL;
  *IERROR = 0;
  ierr    = 0;
  memset(h3dreader_load_libname, 0, sizeof(h3dreader_load_libname));

  if ((libh3dreaderpath = getenv("RAD_H3D_PATH")) != NULL) {
    /* First trial: RAD_H3D_PATH environment variable */
    strcat(h3dreader_load_libname, getenv("RAD_H3D_PATH"));
    strcat(h3dreader_load_libname, "/");
    strcat(h3dreader_load_libname, h3dreaderlib);
    h3dreaderhandle = dlopen(h3dreader_load_libname, RTLD_LAZY | RTLD_GLOBAL);
    if (!h3dreaderhandle)
      fprintf(stderr, "Warning: libh3dreader.so not found in $RAD_H3D_PATH:\n %s\n", dlerror());
  }

  if (!h3dreaderhandle) {
    /* Second trial: current working directory */
    memset(h3dreader_load_libname, 0, sizeof(h3dreader_load_libname));
    getcwd(h3dreader_load_libname, 20000);
    strcat(h3dreader_load_libname, "/");
    strcat(h3dreader_load_libname, h3dreaderlib);
    h3dreaderhandle = dlopen(h3dreader_load_libname, RTLD_LAZY | RTLD_GLOBAL);
    if (!h3dreaderhandle)
      fprintf(stderr, "Warning: libh3dreader.so not found in current directory:\n %s\n", dlerror());
    else
      fprintf(stderr, "Success: libh3dreader.so found in current directory\n");
  }

  if (!h3dreaderhandle && (libh3dreaderpath = getenv("ALTAIR_HOME")) != NULL
      && (libh3dreaderpath = getenv("ARCH")) != NULL) {
    /* Third trial: $ALTAIR_HOME/hwsolvers/common/bin/$ARCH */
    memset(h3dreader_load_libname, 0, sizeof(h3dreader_load_libname));
    strcpy(h3dreader_load_libname, getenv("ALTAIR_HOME"));
    strcat(h3dreader_load_libname, "/hwsolvers/common/bin/");
    strcat(h3dreader_load_libname, getenv("ARCH"));
    strcat(h3dreader_load_libname, "/");
    strcat(h3dreader_load_libname, h3dreaderlib);
    h3dreaderhandle = dlopen(h3dreader_load_libname, RTLD_LAZY | RTLD_GLOBAL);
    if (!h3dreaderhandle)
      fprintf(stderr, "Warning: libh3dreader.so not found in $ALTAIR_HOME:\n %s\n", dlerror());
  }

  if (!h3dreaderhandle) {
    /* Fourth trial: LD_LIBRARY_PATH settings - simple dlopen */
    memset(h3dreader_load_libname, 0, sizeof(h3dreader_load_libname));
    strcpy(h3dreader_load_libname, h3dreaderlib);
    h3dreaderhandle = dlopen(h3dreader_load_libname, RTLD_LAZY | RTLD_GLOBAL);
    if (!h3dreaderhandle)
      fprintf(stderr, "Warning: libh3dreader.so not found in $LD_LIBRARY_PATH:\n %s\n", dlerror());
    else
      fprintf(stderr, "Success: libh3dreader.so found in $LD_LIBRARY_PATH\n");
  }

  /* Library load sequence terminated */
  if (h3dreaderhandle) {

    DLHyper3DImportOpen = dlsym(h3dreaderhandle, "Hyper3DImportOpen");
    if (!DLHyper3DImportOpen) ierr++;

    DLHyper3DImportClose = dlsym(h3dreaderhandle, "Hyper3DImportClose");
    if (!DLHyper3DImportClose) ierr++;

    DLHyper3DLookupString    = dlsym(h3dreaderhandle, "Hyper3DLookupString");

    if (!DLHyper3DLookupString) ierr++;

    if (ierr > 0) { *IERROR = ierr + 1; }

  } else {
    *IERROR = 1;
  }
}
#endif


/* -----------------------------------------------------------------------
 * Wrapper functions callable from C/Fortran code
 * ----------------------------------------------------------------------- */

H3DReaderInfo* Hyper3DImportOpen(const char* filename,
                                 H3DMessageFunctionType mFunc,
                                 H3DErrorFunctionType   eFunc)
{
  return DLHyper3DImportOpen(filename, mFunc, eFunc);
}

bool Hyper3DImportClose(H3DReaderInfo* h3d_file)
{
  return DLHyper3DImportClose(h3d_file);
}

bool Hyper3DLookupString(H3DReaderInfo* h3d_file, uint32_t str_id, const char** string)
{
  return DLHyper3DLookupString(h3d_file, str_id, string);
}

/* End of H3D reader wrappers. */

