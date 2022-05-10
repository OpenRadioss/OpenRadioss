//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>

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

#include "h3dpublic_defs.h"


#ifdef _WIN32
char * h3dlib="h3dwriter.dll";
char  libh3dpath[20000];
char  hwarch[200];
HINSTANCE h3dhandle;
#elif 1
char * h3dlib="libh3dwriter.so";
char * libh3dpath;
void *h3dhandle;
#endif
char load_libname[20000];
char * H3D_open_file="Hyper3DExportOpen";


/**************/
/* The Basics */
/**************/
 H3DFileInfo* (*DLHyper3DExportOpen) (const char* filename, H3D_FileMode mode, 
                    H3DMessageFunctionType mFunc, H3DErrorFunctionType eFunc);

 bool (*DLHyper3DWriteTOC) (H3DFileInfo* h3d_file);

 bool (*DLHyper3DExportClearError)  (H3DFileInfo* h3d_file);

 bool (*DLHyper3DExportClose)  (H3DFileInfo* h3d_file);

/***********************/
/* String Table Blocks */
/***********************/

 bool (*DLHyper3DAddString)  (H3DFileInfo* h3d_file,
                    const char* const string, H3D_ID* const str_id);

/* File Information Block - REQUIRED */

 bool (*DLHyper3DFileInfoBegin)  (H3DFileInfo* h3d_file, 
                    const char* creating_appl, const char* creation_date,
                    const char* solver_name);

 bool (*DLHyper3DFileInfoAddModelFile)  (H3DFileInfo* h3d_file, 
                    const char* model_file);

 bool (*DLHyper3DFileInfoAddResultFile)  (H3DFileInfo* h3d_file, 
                    const char* result_file);

 bool (*DLHyper3DFileInfoAddComment)  (H3DFileInfo* h3d_file, 
                    const char* comment);

 bool (*DLHyper3DFileInfoEnd)  (H3DFileInfo* h3d_file);

/****************/
/* Model Blocks */
/****************/
                      
 bool (*DLHyper3DModelBegin) (H3DFileInfo* h3d_file, unsigned int count);

 bool (*DLHyper3DModelWrite)  (H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, bool tabular, H3D_TRIBOOL adaptive);

 bool (*DLHyper3DModelEnd)  (H3DFileInfo* h3d_file);

 bool (*DLHyper3DSetModelToWrite)  (H3DFileInfo* h3d_file, H3D_ID id, 
                                      bool tabular);
/* Assemblies Blocks */

 bool (*DLHyper3DAssemblyBegin)  (H3DFileInfo* h3d_file, unsigned int count, 
	   H3D_ID poolname_id, H3D_ID parent_poolname_id);
 bool (*DLHyper3DAssemblyWrite)  (H3DFileInfo* h3d_file, const char* label, 
	   H3D_ID id, H3D_ID parent_id);
 bool (*DLHyper3DAssemblyEnd)  (H3DFileInfo* h3d_file);

/*********************/
/* Components Blocks */
/*********************/

 bool (*DLHyper3DComponentBegin)  (H3DFileInfo* h3d_file, unsigned int count,
	   H3D_ID poolname_id, H3D_ID parent_poolname_id);

 bool (*DLHyper3DComponentWrite)  (H3DFileInfo* h3d_file, const char* label, 
	   H3D_ID id, H3D_ID node_poolname_id, H3D_ID parent_id);

 bool (*DLHyper3DComponentEnd)  (H3DFileInfo* h3d_file);

/*********************/
/* System Blocks     */
/*********************/
          
 bool (*DLHyper3DSystemBegin)  (H3DFileInfo* h3d_file, unsigned int count);

 bool (*DLHyper3DSystemWrite)  (H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_SYSTEM_TYPE type, float* global_origin, 
                    float* global_axis);

 bool (*DLHyper3DSystemEnd)  (H3DFileInfo* h3d_file);

/***************/
/* Node blocks */
/***************/

 bool (*DLHyper3DPositionBegin)  (H3DFileInfo* h3d_file, unsigned int count,
 	   H3D_ID poolname_id);

 bool (*DLHyper3DPositionWrite)  (H3DFileInfo* h3d_file, H3D_ID id, 
 	   float* coords, H3D_ID refsys_id, H3D_ID analysis_id);

 bool (*DLHyper3DPositionEnd)  (H3DFileInfo* h3d_file);

/******************/
/* Element Blocks */
/******************/

 bool (*DLHyper3DElementBegin)  (H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_ID poolname_id, H3D_ElementConfig config, 
                    H3D_ID parent_id, H3D_ID parent_poolname_id, 
                    H3D_ID node_poolname_id);

 bool (*DLHyper3DElementWrite)  (H3DFileInfo* h3d_file, 
 	   H3D_ID id, H3D_ID* connectivity);

 bool (*DLHyper3DElementEnd)  (H3DFileInfo* h3d_file);

 bool (*DLHyper3DElement2Begin)  (H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_ID poolname_id, H3D_ElementConfig config, 
           H3D_ID parent_id, H3D_ID parent_poolname_id, 
           H3D_ID node_poolname_id);

 bool (*DLHyper3DElement2Write)  (H3DFileInfo* h3d_file, H3D_ID id, 
                    unsigned int* inode, int* idof, double* icoeff, 
                    unsigned int num_inodes,
                    unsigned int* dnode, int* ddof, double* dcoeff, 
                    unsigned int num_dnodes);

 bool (*DLHyper3DElement2End)  (H3DFileInfo* h3d_file);

/*************************************/
/*  Model Attribute Export Functions */
/*************************************/
/* Eroded Data Blocks */
/* * all pool names are specified per block *  
                              */
 bool (*DLHyper3DErodeBegin) (H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_ID element_poolname_id, H3D_SIM_IDX idx, 
 	   H3D_ID subcase_id);

 bool (*DLHyper3DErodeElement) (H3DFileInfo* h3d_file, H3D_ID id);

 bool (*DLHyper3DErodeEnd) (H3DFileInfo* h3d_file);

/*****************/
/* Result Blocks */
/*****************/ 

 bool (*DLHyper3DResultBegin)  (H3DFileInfo* h3d_file, unsigned int count);

 bool (*DLHyper3DResultWrite)  (H3DFileInfo* h3d_file, const char* label, 
 	   H3D_DS_SHELL_METHOD method, unsigned int num_systems);

 bool (*DLHyper3DResultAddSystem)  (H3DFileInfo* h3d_file, 
                    H3D_ID poolname_id, int system_id, 
                    H3D_ID_POOL_TYPE sysType);

 bool (*DLHyper3DResultEnd)  (H3DFileInfo* h3d_file); 

/*****************************/ 
/* Subcase (Loadcase) Blocks */
/*****************************/   

 bool (*DLHyper3DSimSubcaseBegin)  (H3DFileInfo* h3d_file, unsigned int count);

 bool (*DLHyper3DSimSubcaseWrite)  (H3DFileInfo* h3d_file, const char* label,
 	   H3D_ID id, H3D_ANALYSIS_TYPE atype,  
 	   unsigned int num_datatypes, H3D_ID* datatype_ids, 
 	   H3D_NODAL_DATA_TYPE anim_type);

 bool (*DLHyper3DSimSubcaseAnimationGroups)  (H3DFileInfo* h3d_file, 
 	   H3D_ID id, 
 	   unsigned int num_groups, H3D_ID* grp_datatype_ids, 
 	   unsigned int* num_dts_per_grp, H3D_ID* datatype_ids);

 bool (*DLHyper3DSimSubcaseEnd)  (H3DFileInfo* h3d_file);

/*********************/ 
/* Simulation Blocks */
/*********************/ 

 bool (*DLHyper3DSimulationBegin)  (H3DFileInfo* h3d_file, unsigned int count,
 	   H3D_ID subcase_id);

 bool (*DLHyper3DSimulationWrite)  (H3DFileInfo* h3d_file, H3D_SIM_IDX idx, 
 	   const char* label, float syncValue); 

 bool (*DLHyper3DSimulationEnd)  (H3DFileInfo* h3d_file);

/*******************/ 
/* Datatype Blocks */
/*******************/ 

 bool (*DLHyper3DDatatypeBegin)  (H3DFileInfo* h3d_file, unsigned int count);

 bool (*DLHyper3DDatatypeWrite) (H3DFileInfo* h3d_file, const char* label, 
           H3D_ID dt_id, H3D_DS_FORMAT format, H3D_DS_TYPE type, 
 	   unsigned int num_pools);

 bool (*DLHyper3DDatatypeDescriptionWrite)  (H3DFileInfo* h3d_file, 
 	   H3D_ID dt_id, const char* description);

 bool (*DLHyper3DDatatypePools)  (H3DFileInfo* h3d_file, H3D_ID dt_id, 
 	   H3D_ID poolname_id, unsigned int num_layers, 
 	   H3D_ID* layername_ids, bool corners,
 	   H3D_TENSOR_TYPE tensor_type, float poisson);

 bool (*DLHyper3DDatatypeEnd)  (H3DFileInfo* h3d_file);

/******************/ 
/* Dataset Blocks */
/******************/ 

 bool (*DLHyper3DDatasetBegin)  (H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_SIM_IDX idx, H3D_ID subcase_id, 
 	   H3D_DS_TYPE type, H3D_DS_FORMAT format, 
 	   unsigned int num_corners, unsigned int num_modes, 
 	   H3D_ID dt_id, int layer_idx, H3D_ID data_poolname_id,
 	   bool complex);

 bool (*DLHyper3DDatasetWriteParent)  (H3DFileInfo* h3d_file, H3D_ID comp_id, 
 	   H3D_ID component_poolname_id);

 bool (*DLHyper3DDatasetWrite)  (H3DFileInfo* h3d_file, H3D_ID id,
 	   const float* data);

 bool (*DLHyper3DDatasetWriteWithSystem)  (H3DFileInfo* h3d_file, H3D_ID id, 
 	   const float* data, H3D_ID sys_id, H3D_ANALYSIS_SYSTEM system_flag);

 bool (*DLHyper3DDatasetDoubleWrite)  (H3DFileInfo* h3d_file, H3D_ID id,
 	   const double* data);

 bool (*DLHyper3DDatasetDoubleWriteWithSystem)  (H3DFileInfo* h3d_file, H3D_ID id, 
 	   const double* data, H3D_ID sys_id, H3D_ANALYSIS_SYSTEM system_flag);

 bool (*DLHyper3DDatasetEnd)  (H3DFileInfo* h3d_file);

 
/* h3dlib_load                                                       */
/* Loads the H3D dynamic library and retrieves every routine address */
/*-------------------------------------------------------------------*/
/* Returns                                                           */
/* IERROR = 0 success                                                */
/*        = 1 Load Library Failure                                   */
/*          2 or more dlsym issue                                    */
/*            could not obtain all function adresses                 */
/*                                                                   */
/* CARE : Routine is OS Specific Linux & Windows                     */
/*-------------------------------------------------------------------*/
#ifdef _WIN32
void h3dlib_load_(int * IERROR)
{
  int ierr,dllpath_size,arch_size;
  h3dhandle=NULL;
  *IERROR=0;
  ierr=0;

  dllpath_size=GetEnvironmentVariable("RAD_H3D_PATH",libh3dpath,20000);
  if(dllpath_size > 0) {
  /* First trial get RAD_H3D_PATH environment variable */
      strcpy(load_libname,libh3dpath);
      strcat(load_libname,"\\");
      strcat(load_libname,h3dlib);
      h3dhandle = LoadLibrary(TEXT(load_libname));
  }
  if(!h3dhandle) {
  /* Second trial : current working directory */
      dllpath_size=GetCurrentDirectory(20000,libh3dpath);
      strcpy(load_libname,libh3dpath);
      strcat(load_libname,"\\");
      strcat(load_libname,h3dlib);
      h3dhandle = LoadLibrary(TEXT(load_libname));
  }
  
  if(!h3dhandle) {
  /* Third Trial - %ALTAIR_HOME\hwsolvers\common\bin\%ARCH%  */
      dllpath_size=GetEnvironmentVariable("ALTAIR_HOME",libh3dpath,20000);
      if(dllpath_size > 0) {
          arch_size = GetEnvironmentVariable("ARCH",hwarch,200);
          if(arch_size >0) {
              strcpy(load_libname,libh3dpath);
              strcat(load_libname,"\\hwsolvers\\common\\bin\\");
              strcat(load_libname,hwarch);
              strcat(load_libname,"\\");
              strcat(load_libname,h3dlib);
              h3dhandle = LoadLibrary(TEXT(load_libname));
          }
      }
  }
  
       if (!h3dhandle){
  /* Fourth trial : $LD_LIBRARY_PATH settings - simple dlopen*/
       dllpath_size=GetEnvironmentVariable("PATH",libh3dpath,20000);
       SetDllDirectory(libh3dpath);
       h3dhandle = LoadLibrary(TEXT(load_libname));
       }
  
    /* Library Load sequence terminated */
 if (h3dhandle){
  
/**************/ 
/* The Basics */
/**************/
 
  DLHyper3DExportOpen=GetProcAddress(h3dhandle,"Hyper3DExportOpen");
  if( !DLHyper3DExportOpen) ierr=ierr+1;
  
  DLHyper3DWriteTOC=GetProcAddress(h3dhandle,"Hyper3DWriteTOC");
  if( !DLHyper3DWriteTOC) ierr=ierr+1;

  DLHyper3DExportClearError=GetProcAddress(h3dhandle,"Hyper3DExportClearError");
  if( !DLHyper3DExportClearError) ierr=ierr+1;

  DLHyper3DExportClose=GetProcAddress(h3dhandle,"Hyper3DExportClose");
  if( !DLHyper3DExportClose) ierr=ierr+1;

/***********************/ 
/* String Table Blocks */
/***********************/ 

  DLHyper3DAddString=GetProcAddress(h3dhandle,"Hyper3DAddString");
  if( !DLHyper3DAddString) ierr=ierr+1;

/*************************************/ 
/* File Information Block - REQUIRED */
/*************************************/ 

  DLHyper3DFileInfoBegin=GetProcAddress(h3dhandle,"Hyper3DFileInfoBegin");
  if( !DLHyper3DFileInfoBegin) ierr=ierr+1;

  DLHyper3DFileInfoAddModelFile=GetProcAddress(h3dhandle,"Hyper3DFileInfoAddModelFile");
  if( !DLHyper3DFileInfoAddModelFile) ierr=ierr+1;

  DLHyper3DFileInfoAddResultFile=GetProcAddress(h3dhandle,"Hyper3DFileInfoAddResultFile");
  if( !DLHyper3DFileInfoAddResultFile) ierr=ierr+1;

  DLHyper3DFileInfoAddComment=GetProcAddress(h3dhandle,"Hyper3DFileInfoAddComment");
  if( !DLHyper3DFileInfoAddComment) ierr=ierr+1;

  DLHyper3DFileInfoEnd=GetProcAddress(h3dhandle,"Hyper3DFileInfoEnd");
  if( !DLHyper3DFileInfoEnd) ierr=ierr+1;

/****************/ 
/* Model Blocks */
/****************/ 

  DLHyper3DModelBegin=GetProcAddress(h3dhandle,"Hyper3DModelBegin");
  if( !DLHyper3DModelBegin) ierr=ierr+1;

  DLHyper3DModelWrite=GetProcAddress(h3dhandle,"Hyper3DModelWrite");
  if( !DLHyper3DModelWrite) ierr=ierr+1;

  DLHyper3DModelEnd=GetProcAddress(h3dhandle,"Hyper3DModelEnd");
  if( !DLHyper3DModelEnd) ierr=ierr+1;

  DLHyper3DSetModelToWrite=GetProcAddress(h3dhandle,"Hyper3DSetModelToWrite");
  if( !DLHyper3DSetModelToWrite) ierr=ierr+1;

/*********************/ 
/* Assemblies Blocks */
/*********************/ 

  DLHyper3DAssemblyBegin=GetProcAddress(h3dhandle,"Hyper3DAssemblyBegin");
  if( !DLHyper3DAssemblyBegin) ierr=ierr+1;

  DLHyper3DAssemblyWrite=GetProcAddress(h3dhandle,"Hyper3DAssemblyWrite");
  if( !DLHyper3DAssemblyWrite) ierr=ierr+1;

  DLHyper3DAssemblyEnd=GetProcAddress(h3dhandle,"Hyper3DAssemblyEnd");
  if( !DLHyper3DAssemblyEnd) ierr=ierr+1;

/*********************/ 
/* Components Blocks */
/*********************/ 

  DLHyper3DComponentBegin=GetProcAddress(h3dhandle,"Hyper3DComponentBegin");
  if( !DLHyper3DComponentBegin) ierr=ierr+1;

  DLHyper3DComponentWrite=GetProcAddress(h3dhandle,"Hyper3DComponentWrite");
  if( !DLHyper3DComponentWrite) ierr=ierr+1;

  DLHyper3DComponentEnd=GetProcAddress(h3dhandle,"Hyper3DComponentEnd");
  if( !DLHyper3DComponentEnd) ierr=ierr+1;

/*********************/
/* System Blocks     */
/*********************/

  DLHyper3DSystemBegin=GetProcAddress(h3dhandle,"Hyper3DSystemBegin");
  if( !DLHyper3DSystemBegin) ierr=ierr+1;

  DLHyper3DSystemWrite=GetProcAddress(h3dhandle,"Hyper3DSystemWrite");
  if( !DLHyper3DSystemWrite) ierr=ierr+1;

  DLHyper3DSystemEnd=GetProcAddress(h3dhandle,"Hyper3DSystemEnd");
  if( !DLHyper3DSystemEnd) ierr=ierr+1;

/***************/ 
/* Node blocks */
/***************/ 

  DLHyper3DPositionBegin=GetProcAddress(h3dhandle,"Hyper3DPositionBegin");
  if( !DLHyper3DPositionBegin) ierr=ierr+1;

  DLHyper3DPositionWrite=GetProcAddress(h3dhandle,"Hyper3DPositionWrite");
  if( !DLHyper3DPositionWrite) ierr=ierr+1;

  DLHyper3DPositionEnd=GetProcAddress(h3dhandle,"Hyper3DPositionEnd");
  if( !DLHyper3DPositionEnd) ierr=ierr+1;

/******************/ 
/* Element Blocks */
/******************/ 

  DLHyper3DElementBegin=GetProcAddress(h3dhandle,"Hyper3DElementBegin");
  if( !DLHyper3DElementBegin) ierr=ierr+1;

  DLHyper3DElementWrite=GetProcAddress(h3dhandle,"Hyper3DElementWrite");
  if( !DLHyper3DElementWrite) ierr=ierr+1;

  DLHyper3DElementEnd=GetProcAddress(h3dhandle,"Hyper3DElementEnd");
  if( !DLHyper3DElementEnd) ierr=ierr+1;

  DLHyper3DElement2Begin=GetProcAddress(h3dhandle,"Hyper3DElement2Begin");
  if( !DLHyper3DElement2Begin) ierr=ierr+1;

  DLHyper3DElement2Write=GetProcAddress(h3dhandle,"Hyper3DElement2Write");
  if( !DLHyper3DElement2Write) ierr=ierr+1;

  DLHyper3DElement2End=GetProcAddress(h3dhandle,"Hyper3DElement2End");
  if( !DLHyper3DElement2End) ierr=ierr+1;

/*************************************/
/*  Model Attribute Export Functions */
/*************************************/
/* Eroded Data Blocks */
/* * all pool names are specified per block *  
                              */
  DLHyper3DErodeBegin=GetProcAddress(h3dhandle,"Hyper3DErodeBegin");
  if( !DLHyper3DErodeBegin) ierr=ierr+1;

  DLHyper3DErodeElement=GetProcAddress(h3dhandle,"Hyper3DErodeElement");
  if( !DLHyper3DErodeElement) ierr=ierr+1;

  DLHyper3DErodeEnd=GetProcAddress(h3dhandle,"Hyper3DErodeEnd");
  if( !DLHyper3DErodeEnd) ierr=ierr+1;

/*****************/ 
/* Result Blocks */
/*****************/ 

  DLHyper3DResultBegin=GetProcAddress(h3dhandle,"Hyper3DResultBegin");
  if( !DLHyper3DResultBegin) ierr=ierr+1;

  DLHyper3DResultWrite=GetProcAddress(h3dhandle,"Hyper3DResultWrite");
  if( !DLHyper3DResultWrite) ierr=ierr+1;

  DLHyper3DResultAddSystem=GetProcAddress(h3dhandle,"Hyper3DResultAddSystem");
  if( !DLHyper3DResultAddSystem) ierr=ierr+1;

  DLHyper3DResultEnd=GetProcAddress(h3dhandle,"Hyper3DResultEnd");
  if( !DLHyper3DResultEnd) ierr=ierr+1;

/*****************************/ 
/* Subcase (Loadcase) Blocks */
/*****************************/ 

  DLHyper3DSimSubcaseBegin=GetProcAddress(h3dhandle,"Hyper3DSimSubcaseBegin");
  if( !DLHyper3DSimSubcaseBegin) ierr=ierr+1;

  DLHyper3DSimSubcaseWrite=GetProcAddress(h3dhandle,"Hyper3DSimSubcaseWrite");
  if( !DLHyper3DSimSubcaseWrite) ierr=ierr+1;

  DLHyper3DSimSubcaseAnimationGroups=GetProcAddress(h3dhandle,"Hyper3DSimSubcaseAnimationGroups");
  if( !DLHyper3DSimSubcaseAnimationGroups) ierr=ierr+1;

  DLHyper3DSimSubcaseEnd=GetProcAddress(h3dhandle,"Hyper3DSimSubcaseEnd");
  if( !DLHyper3DSimSubcaseEnd) ierr=ierr+1;

/*********************/ 
/* Simulation Blocks */
/*********************/ 

  DLHyper3DSimulationBegin=GetProcAddress(h3dhandle,"Hyper3DSimulationBegin");
  if( !DLHyper3DSimulationBegin) ierr=ierr+1;

  DLHyper3DSimulationWrite=GetProcAddress(h3dhandle,"Hyper3DSimulationWrite");
  if( !DLHyper3DSimulationWrite) ierr=ierr+1;

  DLHyper3DSimulationEnd=GetProcAddress(h3dhandle,"Hyper3DSimulationEnd");
  if( !DLHyper3DSimulationEnd) ierr=ierr+1;

/*******************/ 
/* Datatype Blocks */
/*******************/ 

  DLHyper3DDatatypeBegin=GetProcAddress(h3dhandle,"Hyper3DDatatypeBegin");
  if( !DLHyper3DDatatypeBegin) ierr=ierr+1;

  DLHyper3DDatatypeWrite=GetProcAddress(h3dhandle,"Hyper3DDatatypeWrite");
  if( !DLHyper3DDatatypeWrite) ierr=ierr+1;

  DLHyper3DDatatypeDescriptionWrite=GetProcAddress(h3dhandle,"Hyper3DDatatypeDescriptionWrite");
  if( !DLHyper3DDatatypeDescriptionWrite) ierr=ierr+1;

  DLHyper3DDatatypePools=GetProcAddress(h3dhandle,"Hyper3DDatatypePools");
  if( !DLHyper3DDatatypePools) ierr=ierr+1;

  DLHyper3DDatatypeEnd=GetProcAddress(h3dhandle,"Hyper3DDatatypeEnd");
  if( !DLHyper3DDatatypeEnd) ierr=ierr+1;

/******************/ 
/* Dataset Blocks */
/******************/ 

  DLHyper3DDatasetBegin=GetProcAddress(h3dhandle,"Hyper3DDatasetBegin");
  if( !DLHyper3DDatasetBegin) ierr=ierr+1;

  DLHyper3DDatasetWriteParent=GetProcAddress(h3dhandle,"Hyper3DDatasetWriteParent");
  if( !DLHyper3DDatasetWriteParent) ierr=ierr+1;

  DLHyper3DDatasetWrite=GetProcAddress(h3dhandle,"Hyper3DDatasetWrite");
  if( !DLHyper3DDatasetWrite) ierr=ierr+1;

  DLHyper3DDatasetWriteWithSystem=GetProcAddress(h3dhandle,"Hyper3DDatasetWriteWithSystem");
  if( !DLHyper3DDatasetWriteWithSystem) ierr=ierr+1;

  DLHyper3DDatasetDoubleWrite=GetProcAddress(h3dhandle,"Hyper3DDatasetDoubleWrite");
  if( !DLHyper3DDatasetDoubleWrite) ierr=ierr+1;

  DLHyper3DDatasetDoubleWriteWithSystem=GetProcAddress(h3dhandle,"Hyper3DDatasetDoubleWriteWithSystem");
  if( !DLHyper3DDatasetDoubleWriteWithSystem) ierr=ierr+1;

  DLHyper3DDatasetEnd=GetProcAddress(h3dhandle,"Hyper3DDatasetEnd");
  if( !DLHyper3DDatasetEnd) ierr=ierr+1;

  
  if (ierr > 0){*IERROR=ierr+1;}

 }

 else
   { *IERROR=1; }
 /* return 0;*/

}
#elif 1 
void h3dlib_load_(int * IERROR)
{
  int ierr;
  h3dhandle=NULL;
  *IERROR=0;
  ierr=0;

  if((libh3dpath = getenv("RAD_H3D_PATH")) != NULL) {
  /* First trial get RAD_H3D_PATH environment variable */
    strcat(load_libname,getenv("RAD_H3D_PATH"));
    strcat(load_libname,"/");
    strcat(load_libname,h3dlib);
    h3dhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
    if(!h3dhandle) fprintf(stderr,"Warning: libh3dwriter.so not found in $RAD_H3D_PATH:\n %s \n",dlerror());

  }

 
  if(!h3dhandle) {
  /* Second trial : current working directory */

    getcwd(load_libname,20000);
    strcat(load_libname,"/");
    strcat(load_libname,h3dlib);
    h3dhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
    if(!h3dhandle) {fprintf(stderr,"Warning: libh3dwriter.so not found in current directory:\n %s\n",dlerror());}
    else {fprintf(stderr,"success: libh3dwriter.so was found in current directory\n");}

  }


  if(!h3dhandle && (libh3dpath = getenv("ALTAIR_HOME")) != NULL 
     &&  (libh3dpath = getenv("ARCH")) != NULL  ) {
  /* Third trial : $ALTAIR_HOME/hwsolvers/common/bin/$ARCH*/
    strcpy(load_libname,getenv("ALTAIR_HOME"));
    strcat(load_libname,"/hwsolvers/common/bin/");
    strcat(load_libname,getenv("ARCH"));
    strcat(load_libname,"/");
    strcat(load_libname,h3dlib);
    h3dhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
    if(!h3dhandle) {fprintf(stderr,"Warning: libh3dwriter.so not found in $ALTAIR_HOME:\n %s\n",dlerror());}
  }


  if(!h3dhandle) {
  /* Fourth trial : $LD_LIBRARY_PATH settings - simple dlopen*/
    strcpy(load_libname,h3dlib);
    h3dhandle = dlopen(load_libname,RTLD_LAZY|RTLD_GLOBAL|RTLD_DEEPBIND);
    if(!h3dhandle) {fprintf(stderr,"Warning: libh3dwriter.so not found in $LD_LIBRARY_PATH:\n %s\n",dlerror());}
    else {fprintf(stderr,"success: libh3dwriter.so was found in $LD_LIBRARY_PATH\n");}

  }


/* Library Load sequence terminated */
 if (h3dhandle){

/**************/ 
/* The Basics */
/**************/
 
  DLHyper3DExportOpen=dlsym(h3dhandle,"Hyper3DExportOpen");
  if( !DLHyper3DExportOpen) ierr=ierr+1;
  
  DLHyper3DWriteTOC=dlsym(h3dhandle,"Hyper3DWriteTOC");
  if( !DLHyper3DWriteTOC) ierr=ierr+1;

  DLHyper3DExportClearError=dlsym(h3dhandle,"Hyper3DExportClearError");
  if( !DLHyper3DExportClearError) ierr=ierr+1;

  DLHyper3DExportClose=dlsym(h3dhandle,"Hyper3DExportClose");
  if( !DLHyper3DExportClose) ierr=ierr+1;

/***********************/ 
/* String Table Blocks */
/***********************/ 

  DLHyper3DAddString=dlsym(h3dhandle,"Hyper3DAddString");
  if( !DLHyper3DAddString) ierr=ierr+1;

/*************************************/ 
/* File Information Block - REQUIRED */
/*************************************/ 

  DLHyper3DFileInfoBegin=dlsym(h3dhandle,"Hyper3DFileInfoBegin");
  if( !DLHyper3DFileInfoBegin) ierr=ierr+1;

  DLHyper3DFileInfoAddModelFile=dlsym(h3dhandle,"Hyper3DFileInfoAddModelFile");
  if( !DLHyper3DFileInfoAddModelFile) ierr=ierr+1;

  DLHyper3DFileInfoAddResultFile=dlsym(h3dhandle,"Hyper3DFileInfoAddResultFile");
  if( !DLHyper3DFileInfoAddResultFile) ierr=ierr+1;

  DLHyper3DFileInfoAddComment=dlsym(h3dhandle,"Hyper3DFileInfoAddComment");
  if( !DLHyper3DFileInfoAddComment) ierr=ierr+1;

  DLHyper3DFileInfoEnd=dlsym(h3dhandle,"Hyper3DFileInfoEnd");
  if( !DLHyper3DFileInfoEnd) ierr=ierr+1;

/****************/ 
/* Model Blocks */
/****************/ 

  DLHyper3DModelBegin=dlsym(h3dhandle,"Hyper3DModelBegin");
  if( !DLHyper3DModelBegin) ierr=ierr+1;

  DLHyper3DModelWrite=dlsym(h3dhandle,"Hyper3DModelWrite");
  if( !DLHyper3DModelWrite) ierr=ierr+1;

  DLHyper3DModelEnd=dlsym(h3dhandle,"Hyper3DModelEnd");
  if( !DLHyper3DModelEnd) ierr=ierr+1;

  DLHyper3DSetModelToWrite=dlsym(h3dhandle,"Hyper3DSetModelToWrite");
  if( !DLHyper3DSetModelToWrite) ierr=ierr+1;

/*********************/ 
/* Assemblies Blocks */
/*********************/ 

  DLHyper3DAssemblyBegin=dlsym(h3dhandle,"Hyper3DAssemblyBegin");
  if( !DLHyper3DAssemblyBegin) ierr=ierr+1;

  DLHyper3DAssemblyWrite=dlsym(h3dhandle,"Hyper3DAssemblyWrite");
  if( !DLHyper3DAssemblyWrite) ierr=ierr+1;

  DLHyper3DAssemblyEnd=dlsym(h3dhandle,"Hyper3DAssemblyEnd");
  if( !DLHyper3DAssemblyEnd) ierr=ierr+1;

/*********************/ 
/* Components Blocks */
/*********************/ 

  DLHyper3DComponentBegin=dlsym(h3dhandle,"Hyper3DComponentBegin");
  if( !DLHyper3DComponentBegin) ierr=ierr+1;

  DLHyper3DComponentWrite=dlsym(h3dhandle,"Hyper3DComponentWrite");
  if( !DLHyper3DComponentWrite) ierr=ierr+1;

  DLHyper3DComponentEnd=dlsym(h3dhandle,"Hyper3DComponentEnd");
  if( !DLHyper3DComponentEnd) ierr=ierr+1;

/*********************/
/* System Blocks     */
/*********************/

  DLHyper3DSystemBegin=dlsym(h3dhandle,"Hyper3DSystemBegin");
  if( !DLHyper3DSystemBegin) ierr=ierr+1;

  DLHyper3DSystemWrite=dlsym(h3dhandle,"Hyper3DSystemWrite");
  if( !DLHyper3DSystemWrite) ierr=ierr+1;

  DLHyper3DSystemEnd=dlsym(h3dhandle,"Hyper3DSystemEnd");
  if( !DLHyper3DSystemEnd) ierr=ierr+1;

/***************/ 
/* Node blocks */
/***************/ 

  DLHyper3DPositionBegin=dlsym(h3dhandle,"Hyper3DPositionBegin");
  if( !DLHyper3DPositionBegin) ierr=ierr+1;

  DLHyper3DPositionWrite=dlsym(h3dhandle,"Hyper3DPositionWrite");
  if( !DLHyper3DPositionWrite) ierr=ierr+1;

  DLHyper3DPositionEnd=dlsym(h3dhandle,"Hyper3DPositionEnd");
  if( !DLHyper3DPositionEnd) ierr=ierr+1;

/******************/ 
/* Element Blocks */
/******************/ 

  DLHyper3DElementBegin=dlsym(h3dhandle,"Hyper3DElementBegin");
  if( !DLHyper3DElementBegin) ierr=ierr+1;

  DLHyper3DElementWrite=dlsym(h3dhandle,"Hyper3DElementWrite");
  if( !DLHyper3DElementWrite) ierr=ierr+1;

  DLHyper3DElementEnd=dlsym(h3dhandle,"Hyper3DElementEnd");
  if( !DLHyper3DElementEnd) ierr=ierr+1;

  DLHyper3DElement2Begin=dlsym(h3dhandle,"Hyper3DElement2Begin");
  if( !DLHyper3DElement2Begin) ierr=ierr+1;

  DLHyper3DElement2Write=dlsym(h3dhandle,"Hyper3DElement2Write");
  if( !DLHyper3DElement2Write) ierr=ierr+1;

  DLHyper3DElement2End=dlsym(h3dhandle,"Hyper3DElement2End");
  if( !DLHyper3DElement2End) ierr=ierr+1;

/*************************************/
/*  Model Attribute Export Functions */
/*************************************/
/* Eroded Data Blocks */
/* * all pool names are specified per block *  
                              */
  DLHyper3DErodeBegin=dlsym(h3dhandle,"Hyper3DErodeBegin");
  if( !DLHyper3DErodeBegin) ierr=ierr+1;

  DLHyper3DErodeElement=dlsym(h3dhandle,"Hyper3DErodeElement");
  if( !DLHyper3DErodeElement) ierr=ierr+1;

  DLHyper3DErodeEnd=dlsym(h3dhandle,"Hyper3DErodeEnd");
  if( !DLHyper3DErodeEnd) ierr=ierr+1;

/*****************/ 
/* Result Blocks */
/*****************/ 

  DLHyper3DResultBegin=dlsym(h3dhandle,"Hyper3DResultBegin");
  if( !DLHyper3DResultBegin) ierr=ierr+1;

  DLHyper3DResultWrite=dlsym(h3dhandle,"Hyper3DResultWrite");
  if( !DLHyper3DResultWrite) ierr=ierr+1;

  DLHyper3DResultAddSystem=dlsym(h3dhandle,"Hyper3DResultAddSystem");
  if( !DLHyper3DResultAddSystem) ierr=ierr+1;

  DLHyper3DResultEnd=dlsym(h3dhandle,"Hyper3DResultEnd");
  if( !DLHyper3DResultEnd) ierr=ierr+1;

/*****************************/ 
/* Subcase (Loadcase) Blocks */
/*****************************/ 

  DLHyper3DSimSubcaseBegin=dlsym(h3dhandle,"Hyper3DSimSubcaseBegin");
  if( !DLHyper3DSimSubcaseBegin) ierr=ierr+1;

  DLHyper3DSimSubcaseWrite=dlsym(h3dhandle,"Hyper3DSimSubcaseWrite");
  if( !DLHyper3DSimSubcaseWrite) ierr=ierr+1;

  DLHyper3DSimSubcaseAnimationGroups=dlsym(h3dhandle,"Hyper3DSimSubcaseAnimationGroups");
  if( !DLHyper3DSimSubcaseAnimationGroups) ierr=ierr+1;

  DLHyper3DSimSubcaseEnd=dlsym(h3dhandle,"Hyper3DSimSubcaseEnd");
  if( !DLHyper3DSimSubcaseEnd) ierr=ierr+1;

/*********************/ 
/* Simulation Blocks */
/*********************/ 

  DLHyper3DSimulationBegin=dlsym(h3dhandle,"Hyper3DSimulationBegin");
  if( !DLHyper3DSimulationBegin) ierr=ierr+1;

  DLHyper3DSimulationWrite=dlsym(h3dhandle,"Hyper3DSimulationWrite");
  if( !DLHyper3DSimulationWrite) ierr=ierr+1;

  DLHyper3DSimulationEnd=dlsym(h3dhandle,"Hyper3DSimulationEnd");
  if( !DLHyper3DSimulationEnd) ierr=ierr+1;

/*******************/ 
/* Datatype Blocks */
/*******************/ 

  DLHyper3DDatatypeBegin=dlsym(h3dhandle,"Hyper3DDatatypeBegin");
  if( !DLHyper3DDatatypeBegin) ierr=ierr+1;

  DLHyper3DDatatypeWrite=dlsym(h3dhandle,"Hyper3DDatatypeWrite");
  if( !DLHyper3DDatatypeWrite) ierr=ierr+1;

  DLHyper3DDatatypeDescriptionWrite=dlsym(h3dhandle,"Hyper3DDatatypeDescriptionWrite");
  if( !DLHyper3DDatatypeDescriptionWrite) ierr=ierr+1;

  DLHyper3DDatatypePools=dlsym(h3dhandle,"Hyper3DDatatypePools");
  if( !DLHyper3DDatatypePools) ierr=ierr+1;

  DLHyper3DDatatypeEnd=dlsym(h3dhandle,"Hyper3DDatatypeEnd");
  if( !DLHyper3DDatatypeEnd) ierr=ierr+1;

/******************/ 
/* Dataset Blocks */
/******************/ 

  DLHyper3DDatasetBegin=dlsym(h3dhandle,"Hyper3DDatasetBegin");
  if( !DLHyper3DDatasetBegin) ierr=ierr+1;

  DLHyper3DDatasetWriteParent=dlsym(h3dhandle,"Hyper3DDatasetWriteParent");
  if( !DLHyper3DDatasetWriteParent) ierr=ierr+1;

  DLHyper3DDatasetWrite=dlsym(h3dhandle,"Hyper3DDatasetWrite");
  if( !DLHyper3DDatasetWrite) ierr=ierr+1;

  DLHyper3DDatasetWriteWithSystem=dlsym(h3dhandle,"Hyper3DDatasetWriteWithSystem");
  if( !DLHyper3DDatasetWriteWithSystem) ierr=ierr+1;

  DLHyper3DDatasetDoubleWrite=dlsym(h3dhandle,"Hyper3DDatasetDoubleWrite");
  if( !DLHyper3DDatasetDoubleWrite) ierr=ierr+1;

  DLHyper3DDatasetDoubleWriteWithSystem=dlsym(h3dhandle,"Hyper3DDatasetDoubleWriteWithSystem");
  if( !DLHyper3DDatasetDoubleWriteWithSystem) ierr=ierr+1;

  DLHyper3DDatasetEnd=dlsym(h3dhandle,"Hyper3DDatasetEnd");
  if( !DLHyper3DDatasetEnd) ierr=ierr+1;


  if (ierr > 0){*IERROR=ierr+1;}

 }

 else
   { *IERROR=1; }
 /* return 0;*/

}
#endif

void _FCALL H3DLIB_LOAD(int *IERROR)
{h3dlib_load_ (IERROR);}

void h3dlib_load__ (int *IERROR)
{h3dlib_load_ (IERROR);}

void h3dlib_load (int * IERROR)
{h3dlib_load_ (IERROR);}


/**************/ 
/* The Basics */
/**************/ 
H3DFileInfo* Hyper3DExportOpen(const char* filename, H3D_FileMode mode, 
                    H3DMessageFunctionType mFunc, H3DErrorFunctionType eFunc)
{
    H3DFileInfo* return_value;
    return_value = DLHyper3DExportOpen(filename,  mode,   mFunc,  eFunc);
    return return_value ;
}

 bool Hyper3DWriteTOC(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DWriteTOC(h3d_file);
   return return_value ;
}
 bool Hyper3DExportClearError(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DExportClearError(h3d_file);
   return return_value ;
}

 bool Hyper3DExportClose(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DExportClose(h3d_file);
   return return_value ;
}

/***********************/ 
/* String Table Blocks */
/***********************/ 

 bool Hyper3DAddString(H3DFileInfo* h3d_file,
                    const char* const string, H3D_ID* const str_id)
{  bool return_value;
   return_value = DLHyper3DAddString(h3d_file,string,str_id);
   return return_value ;
}

/*************************************/ 
/* File Information Block - REQUIRED */
/*************************************/ 

 bool Hyper3DFileInfoBegin(H3DFileInfo* h3d_file, 
                    const char* creating_appl, const char* creation_date, 
                    const char* solver_name)
{  bool return_value;
   return_value = DLHyper3DFileInfoBegin(h3d_file,creating_appl,creation_date,
                                         solver_name );
   return return_value ;
}

 bool Hyper3DFileInfoAddModelFile(H3DFileInfo* h3d_file, 
                    const char* model_file)
{  bool return_value;
   return_value = DLHyper3DFileInfoAddModelFile(h3d_file,model_file);
   return return_value ;
}

 bool Hyper3DFileInfoAddResultFile(H3DFileInfo* h3d_file, 
                    const char* result_file)
{  bool return_value;
   return_value = DLHyper3DFileInfoAddResultFile(h3d_file,result_file);
   return return_value ;
}

 bool Hyper3DFileInfoAddComment(H3DFileInfo* h3d_file, 
                    const char* comment)
{  bool return_value;
   return_value = DLHyper3DFileInfoAddComment(h3d_file,comment);
   return return_value ;
}

 bool Hyper3DFileInfoEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DFileInfoEnd(h3d_file);
   return return_value ;
}

/****************/ 
/* Model Blocks */
/****************/ 

 bool Hyper3DModelBegin(H3DFileInfo* h3d_file, unsigned int count)
{  bool return_value;
   return_value = DLHyper3DModelBegin(h3d_file, count);
   return return_value ;
}


 bool Hyper3DModelWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, bool tabular, H3D_TRIBOOL adaptive)
{  bool return_value;
   return_value = DLHyper3DModelWrite(h3d_file,label,id,tabular,adaptive);
   return return_value ;
}

 bool Hyper3DModelEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DModelEnd(h3d_file);
   return return_value ;
}


 bool Hyper3DSetModelToWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                                      bool tabular)
{  bool return_value;
   return_value = DLHyper3DSetModelToWrite(h3d_file, id, tabular);
   return return_value ;
}

/*********************/ 
/* Assemblies Blocks */
/*********************/ 

 bool  Hyper3DAssemblyBegin(H3DFileInfo* h3d_file, unsigned int count, 
	   H3D_ID poolname_id, H3D_ID parent_poolname_id)

{  bool return_value;
   return_value = DLHyper3DAssemblyBegin(h3d_file, count, poolname_id, parent_poolname_id);
   return return_value ;
}

 bool  Hyper3DAssemblyWrite(H3DFileInfo* h3d_file, const char* label, 
	   H3D_ID id, H3D_ID parent_id)

{  bool return_value;
   return_value = DLHyper3DAssemblyWrite(h3d_file, label, id, parent_id);
   return return_value ;
}

 bool  Hyper3DAssemblyEnd(H3DFileInfo* h3d_file)

{  bool return_value;
   return_value = DLHyper3DAssemblyEnd(h3d_file);
   return return_value ;
}

/*********************/ 
/* Components Blocks */
/*********************/ 

 bool Hyper3DComponentBegin(H3DFileInfo* h3d_file, unsigned int count,
	   H3D_ID poolname_id, H3D_ID parent_poolname_id)
{  bool return_value;
   return_value = DLHyper3DComponentBegin(h3d_file, count, poolname_id, parent_poolname_id);
   return return_value ;
}

 bool Hyper3DComponentWrite(H3DFileInfo* h3d_file, const char* label, 
	   H3D_ID id, H3D_ID node_poolname_id, H3D_ID parent_id)
{  bool return_value;
   return_value = DLHyper3DComponentWrite(h3d_file,label, id, node_poolname_id, parent_id);
   return return_value ;
}

 bool Hyper3DComponentEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DComponentEnd(h3d_file);
   return return_value ;
}

/*********************/
/* System Blocks     */
/*********************/
          
 bool Hyper3DSystemBegin(H3DFileInfo* h3d_file, unsigned int count)
{  bool return_value;
   return_value = DLHyper3DSystemBegin(h3d_file, count);
   return return_value ;
}

 bool Hyper3DSystemWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_SYSTEM_TYPE type, float* global_origin, 
                    float* global_axis)
{  bool return_value;
   return_value = DLHyper3DSystemWrite(h3d_file, id, type, global_origin, global_axis);
   return return_value ;
}

 bool Hyper3DSystemEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DSystemEnd(h3d_file);
   return return_value ;
}

/***************/ 
/* Node blocks */
/***************/ 

 bool Hyper3DPositionBegin(H3DFileInfo* h3d_file, unsigned int count,
 	   H3D_ID poolname_id)
{  bool return_value;
   return_value = DLHyper3DPositionBegin(h3d_file, count, poolname_id);
   return return_value ;
}

 bool Hyper3DPositionWrite(H3DFileInfo* h3d_file, H3D_ID id, 
 	   float* coords, H3D_ID refsys_id, H3D_ID analysis_id)
{  bool return_value;
   return_value = DLHyper3DPositionWrite(h3d_file, id, coords, refsys_id, analysis_id);
   return return_value ;
}

 bool Hyper3DPositionEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DPositionEnd(h3d_file);
   return return_value ;
}

/******************/ 
/* Element Blocks */
/******************/ 

 bool Hyper3DElementBegin(H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_ID poolname_id, H3D_ElementConfig config, 
 	   H3D_ID parent_id, H3D_ID parent_poolname_id, 
 	   H3D_ID node_poolname_id)
{  bool return_value;
   return_value = DLHyper3DElementBegin(h3d_file, count, poolname_id, config, 
 	   parent_id, parent_poolname_id, node_poolname_id);
   return return_value ;
}

 bool Hyper3DElementWrite(H3DFileInfo* h3d_file, 
 	   H3D_ID id, H3D_ID* connectivity)
{  bool return_value;
   return_value = DLHyper3DElementWrite(h3d_file, id, connectivity);
   return return_value ;
}

 bool Hyper3DElementEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DElementEnd(h3d_file);
   return return_value ;
}

 bool Hyper3DElement2Begin(H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_ID poolname_id, H3D_ElementConfig config, 
 	   H3D_ID parent_id, H3D_ID parent_poolname_id, 
 	   H3D_ID node_poolname_id)
{  bool return_value;
   return_value = DLHyper3DElement2Begin(h3d_file, count, poolname_id, config, 
 	   parent_id, parent_poolname_id, node_poolname_id);
   return return_value ;
}

 bool Hyper3DElement2Write(H3DFileInfo* h3d_file, H3D_ID id, 
                    unsigned int* inode, int* idof, double* icoeff, 
                    unsigned int num_inodes,
                    unsigned int* dnode, int* ddof, double* dcoeff, 
                    unsigned int num_dnodes)
{  bool return_value;
   return_value = DLHyper3DElement2Write(h3d_file, id, inode, idof, icoeff, 
                    num_inodes, dnode, ddof, dcoeff, num_dnodes);
   return return_value ;
}

 bool Hyper3DElement2End(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DElement2End(h3d_file);
   return return_value ;
}

/*************************************/
/*  Model Attribute Export Functions */
/*************************************/
/* Eroded Data Blocks */

 bool Hyper3DErodeBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID element_poolname_id, H3D_SIM_IDX idx, 
                    H3D_ID subcase_id)
{  bool return_value;
   return_value = DLHyper3DErodeBegin(h3d_file, count, element_poolname_id, idx, subcase_id);
   return return_value ;
}

 bool Hyper3DErodeElement(H3DFileInfo* h3d_file, H3D_ID id)
{  bool return_value;
   return_value = DLHyper3DErodeElement(h3d_file, id);
   return return_value ;
}



 bool Hyper3DErodeEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DErodeEnd(h3d_file);
   return return_value ;
}

/*****************/ 
/* Result Blocks */
/*****************/ 

 bool Hyper3DResultBegin(H3DFileInfo* h3d_file, unsigned int count)
{  bool return_value;
   return_value = DLHyper3DResultBegin(h3d_file, count);
   return return_value ;
}

 bool Hyper3DResultWrite(H3DFileInfo* h3d_file, const char* label, 
       H3D_DS_SHELL_METHOD method, unsigned int num_systems)
{  bool return_value;
   return_value = DLHyper3DResultWrite(h3d_file, label, method, num_systems);
   return return_value ;
}

 bool Hyper3DResultAddSystem(H3DFileInfo* h3d_file, 
                    H3D_ID poolname_id, int system_id, 
                    H3D_ID_POOL_TYPE sysType)
{  bool return_value;
   return_value =  DLHyper3DResultAddSystem(h3d_file, poolname_id, system_id, sysType);
   return return_value ;
}

 bool Hyper3DResultEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value =  DLHyper3DResultEnd(h3d_file);
   return return_value ;
}

/*****************************/ 
/* Subcase (Loadcase) Blocks */
/*****************************/ 

 bool Hyper3DSimSubcaseBegin(H3DFileInfo* h3d_file, unsigned int count)
{  bool return_value;
   return_value =  DLHyper3DSimSubcaseBegin(h3d_file, count);
   return return_value ;
}

 bool Hyper3DSimSubcaseWrite(H3DFileInfo* h3d_file, const char* label,
 	   H3D_ID id, H3D_ANALYSIS_TYPE atype,  
 	   unsigned int num_datatypes, H3D_ID* datatype_ids, 
 	   H3D_NODAL_DATA_TYPE anim_type)
{  bool return_value;
   return_value =  DLHyper3DSimSubcaseWrite(h3d_file,label, id, atype, 
 	    num_datatypes, datatype_ids, anim_type);
   return return_value ;
}

 bool Hyper3DSimSubcaseAnimationGroups(H3DFileInfo* h3d_file, 
 	   H3D_ID id, 
 	   unsigned int num_groups, H3D_ID* grp_datatype_ids, 
 	   unsigned int* num_dts_per_grp, H3D_ID* datatype_ids)
{  bool return_value;
   return_value =  DLHyper3DSimSubcaseAnimationGroups(h3d_file, 
 	   id, num_groups, grp_datatype_ids, num_dts_per_grp, datatype_ids);
   return return_value ;
}

 bool Hyper3DSimSubcaseEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value =  DLHyper3DSimSubcaseEnd(h3d_file);
   return return_value ;
}

/*********************/ 
/* Simulation Blocks */
/*********************/ 

 bool Hyper3DSimulationBegin(H3DFileInfo* h3d_file, unsigned int count,
 	   H3D_ID subcase_id)
{  bool return_value;
   return_value =  DLHyper3DSimulationBegin(h3d_file, count, subcase_id);
   return return_value ;
}

 bool Hyper3DSimulationWrite(H3DFileInfo* h3d_file, H3D_SIM_IDX idx, 
 	   const char* label, float syncValue)
{  bool return_value;
   return_value =  DLHyper3DSimulationWrite(h3d_file, idx, label, syncValue);
   return return_value ;
}

 bool Hyper3DSimulationEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value =  DLHyper3DSimulationEnd(h3d_file);
   return return_value ;
}


/*******************/ 
/* Datatype Blocks */
/*******************/ 

 bool Hyper3DDatatypeBegin(H3DFileInfo* h3d_file, unsigned int count)
{  bool return_value;
   return_value = DLHyper3DDatatypeBegin(h3d_file, count);
   return return_value ;
}

 bool Hyper3DDatatypeWrite(H3DFileInfo* h3d_file, const char* label, 
 	   H3D_ID dt_id, H3D_DS_FORMAT format, H3D_DS_TYPE type, 
 	   unsigned int num_pools)
{  bool return_value;
   return_value = DLHyper3DDatatypeWrite(h3d_file, label, dt_id, format, type, num_pools);
   return return_value ;
}

 bool Hyper3DDatatypeDescriptionWrite(H3DFileInfo* h3d_file, 
 	   H3D_ID dt_id, const char* description)
{  bool return_value;
   return_value = DLHyper3DDatatypeDescriptionWrite(h3d_file, dt_id, description);
   return return_value ;
}

 bool Hyper3DDatatypePools(H3DFileInfo* h3d_file, H3D_ID dt_id, 
 	   H3D_ID poolname_id, unsigned int num_layers, 
 	   H3D_ID* layername_ids, bool corners,
 	   H3D_TENSOR_TYPE tensor_type, float poisson)
{  bool return_value;
   return_value = DLHyper3DDatatypePools(h3d_file, dt_id, poolname_id, num_layers, 
 	   layername_ids, corners, tensor_type, poisson);
   return return_value ;
}

 bool Hyper3DDatatypeEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DDatatypeEnd(h3d_file);
   return return_value ;
}

/******************/ 
/* Dataset Blocks */
/******************/ 


 bool Hyper3DDatasetBegin(H3DFileInfo* h3d_file, unsigned int count, 
 	   H3D_SIM_IDX idx, H3D_ID subcase_id, 
 	   H3D_DS_TYPE type, H3D_DS_FORMAT format, 
 	   unsigned int num_corners, unsigned int num_modes, 
 	   H3D_ID dt_id, int layer_idx, H3D_ID data_poolname_id,
 	   bool complex)
{  bool return_value;
   return_value = DLHyper3DDatasetBegin(h3d_file,count, idx,subcase_id, 
 	   type, format, num_corners, num_modes, 
 	   dt_id, layer_idx, data_poolname_id, complex);
   return return_value ;
}

 bool Hyper3DDatasetWriteParent(H3DFileInfo* h3d_file, H3D_ID comp_id, 
 	   H3D_ID component_poolname_id)
{  bool return_value;
   return_value = DLHyper3DDatasetWriteParent(h3d_file, comp_id, component_poolname_id);
   return return_value ;
}

 bool Hyper3DDatasetWrite(H3DFileInfo* h3d_file, H3D_ID id,
 	   const float* data)
{  bool return_value;
   return_value = DLHyper3DDatasetWrite(h3d_file, id, data);
   return return_value ;
}

 bool Hyper3DDatasetWriteWithSystem(H3DFileInfo* h3d_file, H3D_ID id, 
 	   const float* data, H3D_ID sys_id, H3D_ANALYSIS_SYSTEM system_flag)
{  bool return_value;
   return_value = DLHyper3DDatasetWriteWithSystem(h3d_file, id, data, sys_id, system_flag);
   return return_value ;
}

 bool Hyper3DDatasetDoubleWrite(H3DFileInfo* h3d_file, H3D_ID id,
 	   const double* data)
{  bool return_value;
   return_value = DLHyper3DDatasetDoubleWrite(h3d_file, id, data);
   return return_value ;
}

 bool Hyper3DDatasetDoubleWriteWithSystem(H3DFileInfo* h3d_file, H3D_ID id, 
 	   const double* data, H3D_ID sys_id, H3D_ANALYSIS_SYSTEM system_flag)
{  bool return_value;
   return_value = DLHyper3DDatasetDoubleWriteWithSystem(h3d_file, id, data, sys_id, system_flag);
   return return_value ;
}

 bool Hyper3DDatasetEnd(H3DFileInfo* h3d_file)
{  bool return_value;
   return_value = DLHyper3DDatasetEnd(h3d_file);
   return return_value ;
}












