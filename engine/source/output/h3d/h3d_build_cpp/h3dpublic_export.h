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
/*
** ------------------------------------------------------------------
** Copyright (c) 2001-07 Altair Engineering Inc. All Rights Reserved
** Contains trade secrets of Altair Engineering, Inc.  Copyright notice 
** does not imply publication.  Decompilation or disassembly of this 
** software is strictly prohibited.
** ------------------------------------------------------------------
*/
#ifndef ALTAIR_HYPER3DAPI_PUBLIC_EXPORT_INCLUDED 
#define ALTAIR_HYPER3DAPI_PUBLIC_EXPORT_INCLUDED

/* H3D API Lexicon:                                                          */
/*      model           -  a collection of assemblies, components,           */
/*                          primitives, elements, positional and simulation  */ 
/*                          data                                             */

/*      assembly        - a grouping of components that will be              */
/*                          reorientation as a group                         */
/*      component       - a collection elements and positional data that     */
/*                          share common attributes                          */
/*      primitive       - a non-finite element representation of a           */
/*                          specialized component                            */
/*      eroded element  - an element that is removed from the result space   */
/*                          at a specific simulation                         */

/*      result          - a collection of subcases that share common         */
/*                          analysis properties                              */
/*      subcase         - (aka loadcase) grouping of simulations with the    */
/*                          same physical attributes                         */
/*      simulation      - a snapshot in time of a finite element system      */
/*      datatype        - a specific type of result data (stress, strain,    */
/*                          velocity) computed by a FE solver                */
/*      data set        - data of a specific data type                       */
/*      animation group - listing of datatypes required to visualize         */
/*                          animation of result data                         */

/*      count           - number of items in block                           */
/*      id              - id of model, must be greater than 0 (zero)         */
/*      pool            - a collection of objects whose id values are        */
/*                          unique within the group                          */
/*      tabular format  - entire pool specified in one block, item order     */
/*                          is always maintained                             */
/*      adaptive        - a node pool that grows atspecific simulations,     */
/*                          these position pools must be tabular             */
/*      initial transform - orientation data that is applied to model        */
/*                          display and all simulations                      */
/*      model transform   - orientation data that is applied to the model    */
/*                          or a specified simulation                        */
/*      x, y, z         - orientation translation vector                     */
/*      e1, e2, e3      - orientation rotation vector in radians             */
/*      sim_idx         - index value of specific simulation to which data   */
/*                          applies: H3D_MODEL_SIM_IDX,                      */
/*                                   H3D_GENERIC_RESULT_SIM_IDX              */
/*                                   or other positive value                 */

/*      complex data is to be in magnitude/phase form                        */
/*          (i.e. Complex Vector data (MagX, MagY, MagZ, PhX, PhY, PhZ)      */

#include "h3dpublic_defs.h"

#if !defined(OS_UNIX)
#   define DllExport __declspec( dllexport )
#else
#   define DllExport
#endif

#if defined (__cplusplus)
extern "C"
{
#endif

/* The Basics */
DllExport H3DFileInfo* Hyper3DExportOpen(const char* filename, H3D_FileMode mode, 
                    H3DMessageFunctionType mFunc, H3DErrorFunctionType eFunc);
DllExport bool Hyper3DWriteTOC(H3DFileInfo* h3d_file);
DllExport bool Hyper3DExportClearError(H3DFileInfo* h3d_file);
DllExport bool Hyper3DExportClose(H3DFileInfo* h3d_file);

/* String Table Blocks */
DllExport bool Hyper3DLookupStringID(H3DFileInfo* h3d_file, 
                    const char* const string, H3D_ID* const str_id);
DllExport bool Hyper3DLookupString(H3DFileInfo* h3d_file, H3D_ID str_id,
                    const char** string);
DllExport bool Hyper3DAddString(H3DFileInfo* h3d_file,
                    const char* const string, H3D_ID* const str_id);

/* alternate messaging APIs */
DllExport void Hyper3DRegisterExportErrorFunction(H3DFileInfo* h3d_file, 
                    H3DErrorFunctionType func);
DllExport void Hyper3DRegisterExportMessageFunction(H3DFileInfo* h3d_file, 
                    H3DMessageFunctionType func);

/* File Information Block - REQUIRED */
/*      creating_appl - name of application creating this file               */
/*      creation_date - date of file creation                                */
/*      solver_name   - name of solver that created data                     */
/*      model_file - file from which model data originated                   */
/*      result_file - file from which result data originated                 */
DllExport bool Hyper3DFileInfoBegin(H3DFileInfo* h3d_file, 
                    const char* creating_appl, const char* creation_date,
                    const char* solver_name);
/*DllExport bool Hyper3DFileInfoBegin(H3DFileInfo* h3d_file, 
                    const char* creating_appl, const char* creation_date, 
                    const char* solver_name);      commented sinon erreur            */
DllExport bool Hyper3DFileInfoAddModelFile(H3DFileInfo* h3d_file, 
                    const char* model_file);
DllExport bool Hyper3DFileInfoAddResultFile(H3DFileInfo* h3d_file, 
                    const char* result_file);
DllExport bool Hyper3DFileInfoAddComment(H3DFileInfo* h3d_file, 
                    const char* comment);
DllExport bool Hyper3DFileInfoEnd(H3DFileInfo* h3d_file);


/**************************/
/*  Model Data Functions  */
/**************************/

/* Model Blocks */
/*      name - name of model, may be NULL                                    */
DllExport bool Hyper3DModelBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DModelWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, bool tabular, H3D_TRIBOOL adaptive);
DllExport bool Hyper3DModelEnd(H3DFileInfo* h3d_file);

/* Usage REQUIRED for all for APIs defined below this point */ 
DllExport bool Hyper3DSetModelToWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                                      bool tabular);

/* Assemblies Blocks */
/* * all pool names are specified per block *                                */
/*   AssemblyWrite must be called before corresponding Transform APIs        */
/*                                                                           */
/*      parent_id - id of another assembly                                   */
/*      parent_poolname_id may be H3D_NULL_ID if parent_id == 0  (no parent) */
DllExport bool Hyper3DAssemblyBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID poolname_id, H3D_ID parent_poolname_id);
DllExport bool Hyper3DAssemblyWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, H3D_ID parent_id);
DllExport bool Hyper3DAssemblySetInitialTransform(H3DFileInfo* h3d_file, H3D_ID id, 
                    float x, float y, float z, float e1, float e2, float e3);
DllExport bool Hyper3DAssemblySetModelTransform(H3DFileInfo* h3d_file, H3D_ID id, 
                    float x, float y, float z, float e1, float e2, float e3);
DllExport bool Hyper3DAssemblyEnd(H3DFileInfo* h3d_file);

/* Components Blocks */
/* * all pool names are specified per block *                                */
/*                                                                           */
/*      parent_id - id of an assembly                                        */
/*      parent_poolname_id may be H3D_NULL_ID if parent_id == 0  (no parent) */
DllExport bool Hyper3DComponentBegin(H3DFileInfo* h3d_file, unsigned int count,
                    H3D_ID poolname_id, H3D_ID parent_poolname_id);
DllExport bool Hyper3DComponentWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, H3D_ID node_poolname_id, H3D_ID parent_id);
DllExport bool Hyper3DComponentSetInitialTransform(H3DFileInfo* h3d_file, H3D_ID id,
                    float x, float y, float z, float e1, float e2, float e3);
DllExport bool Hyper3DComponentSetModelTransform(H3DFileInfo* h3d_file, H3D_ID id, 
                    float x, float y, float z, float e1, float e2, float e3);
DllExport bool Hyper3DComponentEnd(H3DFileInfo* h3d_file);

/* Primitives Blocks */
/* * all pool names are specified per block *                                */
/*                                                                           */
/*      name may be NULL, default is supplied by reader                      */
/*      parent_id - id of an assembly                                        */
/*      parent_poolname_id may be H3D_NULL_ID if parent_id == 0  (no parent) */
DllExport bool Hyper3DPrimitiveBegin(H3DFileInfo* h3d_file, unsigned int count,
                    H3D_ID poolname_id, H3D_ID parent_poolname_id);
DllExport bool Hyper3DBoxWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, float xsize, float ysize, float zsize, 
                    bool centered, float x, float y, float z, 
                    float e1, float e2, float e3, H3D_ID parent_id);
DllExport bool Hyper3DMarkerWrite(H3DFileInfo* h3d_file, const char* label,
                    H3D_ID id, float x, float y, float z, 
                    float e1, float e2, float e3, H3D_ID parent_id);
DllExport bool Hyper3DCylinderWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, int angle, int segments_in_yz, 
                    float sweep_angle, H3D_PRIMITIVE_CAPS cap, float semi_x, 
                    float semi_y1, float semi_y2, float semi_z1, float semi_z2,
                    bool centered, float x, float y, float z, 
                    float e1, float e2, float e3, H3D_ID parent_id);
DllExport bool Hyper3DEllipsoidWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, int angle, 
                    int segments_in_z, int segments_in_xy,
                    float semi_x, float semi_y, float semi_z,
                    float x, float y, float z, 
                    float e1, float e2, float e3, H3D_ID parent_id);
DllExport bool Hyper3DLinesWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, unsigned int* elem_conn, 
                    unsigned int conn_length, float* nodes, 
                    unsigned int num_nodes, float x, float y, float z, 
                    float e1, float e2, float e3, H3D_ID parent_id);
DllExport bool Hyper3DMeshWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, unsigned int* conn, unsigned int conn_length, 
                    float* nodes, unsigned int num_nodes,
                    float x, float y, float z, float e1, float e2, float e3,
                    H3D_ID parent_id);
DllExport bool Hyper3DOutlineWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, unsigned int num_markers, H3D_ID* marker_ids, 
                    H3D_ID* marker_poolname_ids, H3D_ID parent_id);
DllExport bool Hyper3DSpringWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id,
                    H3D_ID marker1_id, H3D_ID marker1_poolname_id,
                    H3D_ID marker2_id, H3D_ID marker2_poolname_id,
                    float offset_marker1_damper, float offset_marker2_damper,
                    float height_damper1, float height_damper2,
                    float diameter_spring, 
                    float diameter_damper1, float diameter_damper2,
                    int helix_coils, int helix_segs, H3D_ID parent_id);
DllExport bool Hyper3DPrimitiveSetInitialTransform(H3DFileInfo* h3d_file, 
                    H3D_ID id, float x, float y, float z, 
                    float e1, float e2, float e3);
DllExport bool Hyper3DPrimitiveEnd(H3DFileInfo* h3d_file);

/* System Blocks */
/* at this time H3D supports only one system pool                            */
/*      and systems may not be nested (or parented)                          */
DllExport bool Hyper3DSystemBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DSystemWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_SYSTEM_TYPE type, float* global_origin, 
                    float* global_axis);
DllExport bool Hyper3DSystemEnd(H3DFileInfo* h3d_file);

/* Node blocks */
/* * all pool names are specified per block                                 */
/*   PositionR API is used only when suppling time indexed positions         */
/*      as opposed to time indexed nodal displacement results                */
/*   PositionR API may only be used when defining a tabular model            */
/*   PositionR is specified per block                                        */
DllExport bool Hyper3DPositionBegin(H3DFileInfo* h3d_file, unsigned int count,
                    H3D_ID poolname_id);
DllExport bool Hyper3DPositionWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    float* coords, H3D_ID refsys_id, H3D_ID analysis_id);
DllExport bool Hyper3DPositionEnd(H3DFileInfo* h3d_file);

DllExport bool Hyper3DPositionRBegin(H3DFileInfo* h3d_file, 
                    unsigned int count, H3D_ID poolname_id, 
                    H3D_SIM_IDX idx, H3D_ID subcase_id); 
DllExport bool Hyper3DPositionRWrite(H3DFileInfo* h3d_file, float* coords);
DllExport bool Hyper3DPositionREnd(H3DFileInfo* h3d_file);

/* Position Pool blocks */
/*  this block is used to specify a position data pool in tabular models     */
/*  this block also expresses the id order that a result data blocks for     */
/*      the named pool will have                                             */
/*  at this time it is recommended that only one pool be stored per block    */
/*      as the ammount of memory required to read this data can be very      */
/*      large per pool                                                       */
DllExport bool Hyper3DPositionPoolsBegin(H3DFileInfo* h3d_file, 
                    unsigned int count);
DllExport bool Hyper3DPositionPoolWrite(H3DFileInfo* h3d_file, 
                    H3D_ID poolname_id, unsigned int num_nodes, H3D_ID* ids);
DllExport bool Hyper3DPositionPoolEnd(H3DFileInfo* h3d_file);

/* Element Blocks */
/* * all pool names are specified per block *                                */
/*                                                                           */
/*      parent_id - id of a component                                        */
/*      connectivity will be node ids,                                       */
/*          except in tabluar models when it will be node pool indexes       */
/*                                                                           */
/*      any elem_pname begining with "HW_LOD_" is an pool of DAB data        */
/*          from Hyperworks and is not considered part of the actual         */
/*          FE Model definition                                              */
DllExport bool Hyper3DElementBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID poolname_id, H3D_ElementConfig config, 
                    H3D_ID parent_id, H3D_ID parent_poolname_id, 
                    H3D_ID node_poolname_id);
DllExport bool Hyper3DElementRBegin(H3DFileInfo* h3d_file, 
                    unsigned int count, H3D_ID poolname_id, 
                    H3D_ElementConfig config, H3D_ID parent_id, 
                    H3D_ID parent_poolname_id, H3D_ID node_poolname_id, 
                    H3D_SIM_IDX idx, H3D_ID subcase_id);
DllExport bool Hyper3DElementWrite(H3DFileInfo* h3d_file, 
                    H3D_ID id, H3D_ID* connectivity);
DllExport bool Hyper3DElementEnd(H3DFileInfo* h3d_file);

/* Elements (second form) */
/* * all pool names are specified per block *                                */
/*                                                                           */
/*      parent_id - id of a component                                        */
/*      connectivity will be node ids,                                       */
/*          except in tabluar models when it will be node pool indexes       */
/*                                                                           */
/*      inode, idof, icoeff, num_inodes are the independant nodes of an      */
/*          element, while dnode, ddof, dcoeff, num_dnodes are the           */
/*          dependant nodes                                                  */
DllExport bool Hyper3DElement2Begin(H3DFileInfo* h3d_file,  unsigned int count,
                    H3D_ID poolname_id, H3D_ElementConfig config, 
                    H3D_ID parent_id, H3D_ID parent_poolname_id, 
                    H3D_ID node_poolname_id);
DllExport bool Hyper3DElement2RBegin(H3DFileInfo* h3d_file,  
                    unsigned int count, H3D_ID poolname_id, 
                    H3D_ElementConfig config, H3D_ID parent_id, 
                    H3D_ID parent_poolname_id, H3D_ID node_poolname_id, 
                    H3D_SIM_IDX idx, H3D_ID subcase_id);
DllExport bool Hyper3DElement2Write(H3DFileInfo* h3d_file, H3D_ID id, 
                    unsigned int* inode, int* idof, double* icoeff, 
                    unsigned int num_inodes,
                    unsigned int* dnode, int* ddof, double* dcoeff, 
                    unsigned int num_dnodes);
DllExport bool Hyper3DElement2End(H3DFileInfo* h3d_file);


/* Elements by Pool blocks */
/*  this block is used to specify a element data pool in tabular models      */
/*  this block also expresses the id order that a result data blocks for     */
/*      the named pool will have                                             */
/*  as this block is used for tabluar model only, the element connectivity   */
/*      is to be specified by the position pool index                        */
DllExport bool Hyper3DElementPoolBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID poolname_id, H3D_ID parent_poolname_id, 
                    H3D_ID node_poolname_id);
DllExport bool Hyper3DElementPoolRBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID poolname_id, H3D_ID parent_poolname_id, 
                    H3D_ID node_poolname_id, H3D_SIM_IDX idx, H3D_ID subcase_id);
DllExport bool Hyper3DElementPoolWrite(H3DFileInfo* h3d_file, 
                    H3D_ElementConfig config, H3D_ID id, H3D_ID parent_id, 
                    H3D_ID* connectivity);
DllExport bool Hyper3DElementPoolEnd(H3DFileInfo* h3d_file);

/* Entity Set Blocks */
DllExport bool Hyper3DEntitySetBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID poolname_id);
DllExport bool Hyper3DEntitySetWrite(H3DFileInfo* h3d_file, const char* label,
                    H3D_ID id, H3D_ID_POOL_TYPE ent_type);
DllExport bool Hyper3DEntitySetAdd(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID_POOL_TYPE ent_type, H3D_ID ent_poolname_id, 
                    H3D_ID* ids, unsigned int num_ids);
DllExport bool Hyper3DEntitySetAddRange(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID_POOL_TYPE ent_type, H3D_ID ent_poolname_id, 
                    H3D_ID first_id, H3D_ID last_id);
DllExport bool Hyper3DEntitySetRemove(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID_POOL_TYPE ent_type, H3D_ID ent_poolname_id, 
                    H3D_ID* ids, unsigned int num_ids);
DllExport bool Hyper3DEntitySetRemoveRange(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID_POOL_TYPE ent_type, H3D_ID ent_poolname_id, 
                    H3D_ID first_id, H3D_ID last_id);
DllExport bool Hyper3DEntitySetEnd(H3DFileInfo* h3d_file);


/*************************************/
/*  Model Attribute Export Functions */
/*************************************/

/* Model Attributes Blocks */
DllExport bool Hyper3DModelAttribBegin(H3DFileInfo* h3d_file, 
                    unsigned int count);
DllExport bool Hyper3DModelAttribWrite(H3DFileInfo* h3d_file, H3D_ID id,
                    H3D_MODEL_ATTRIBS flags, unsigned char* meta, 
                    unsigned int meta_size);
DllExport bool Hyper3DModelAttribColor(H3DFileInfo* h3d_file, H3D_ID id, 
                    unsigned char color[3]);
DllExport bool Hyper3DModelAttribEnd(H3DFileInfo* h3d_file);

/* Assembly Attributes Blocks */
/* * all pool names are specified per block *                                */
DllExport bool Hyper3DAssemblyAttribBegin(H3DFileInfo* h3d_file, 
                    unsigned int count, H3D_ID poolname_id);
DllExport bool Hyper3DAssemblyAttribWrite(H3DFileInfo* h3d_file,
                    H3D_ID id, H3D_ASSEMBLY_ATTRIBS flags);
DllExport bool Hyper3DAssemblyAttribColor(H3DFileInfo* h3d_file, 
                    H3D_ID id, unsigned char color[3]);
DllExport bool Hyper3DAssemblyAttribEnd(H3DFileInfo* h3d_file);

/* Component Attributes Blocks */
/* * all pool names are specified per block *                                */
/*   AttribWrite must be called before any other corresponding APIs          */
/*                                                                           */
/*      color - RGB values                                                   */
DllExport bool Hyper3DComponentAttribBegin(H3DFileInfo* h3d_file, 
                    unsigned int count, H3D_ID poolname_id);
DllExport bool Hyper3DComponentAttribWrite(H3DFileInfo* h3d_file, 
                    H3D_ID id, H3D_COMPONENT_ATTRIBS flags);
DllExport bool Hyper3DComponentAttribColor(H3DFileInfo* h3d_file, 
                    H3D_ID id, unsigned char color[3]);
DllExport bool Hyper3DComponentAttribFeatureAngle(H3DFileInfo* h3d_file, 
                    H3D_ID id, float featureAngle);
DllExport bool Hyper3DComponentAttribEnd(H3DFileInfo* h3d_file);

/* System Attributes Blocks */
/*      system_pname - this agruement should be NULL and will be ignored     */
/*      color - RGB values                                                   */
DllExport bool Hyper3DSystemAttribBegin(H3DFileInfo* h3d_file, 
                    unsigned int count, H3D_ID poolname_id);
DllExport bool Hyper3DSystemAttribWrite(H3DFileInfo* h3d_file, H3D_ID id,                                                 
                    H3D_SYSTEM_ATTRIBS flags, unsigned char color[3]);
DllExport bool Hyper3DSystemAttribEnd(H3DFileInfo* h3d_file);

/* Entity Set Attributes Blocks */
/*      color - RGB values                                                   */
/*      draw_size - use default size, set to 0                               */
DllExport bool Hyper3DEntitySetAttribBegin(H3DFileInfo* h3d_file, 
                    unsigned int count, H3D_ID poolname_id);
DllExport bool Hyper3DEntitySetAttribWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ENTITY_SET_ATTRIBS flags, unsigned int draw_size,
                    unsigned char color[3]);
DllExport bool Hyper3DEntitySetAttribEnd(H3DFileInfo* h3d_file);

/* Eroded Data Blocks */
/* * all pool names are specified per block *                                */
DllExport bool Hyper3DErodeBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID element_poolname_id, H3D_SIM_IDX idx, 
                    H3D_ID subcase_id);
DllExport bool Hyper3DErodeElement(H3DFileInfo* h3d_file, H3D_ID id);
DllExport bool Hyper3DErodeEnd(H3DFileInfo* h3d_file);

/* Mask Blocks */
/* * all pool names are specified per block *                                */
/* * only one entity type allowed per block *                                */
/*  HyperView's default is to mask all markers and entity sets when imported,*/
/*      thus markers and entity sets are only unmasked (or made visible)     */
DllExport bool Hyper3DMaskBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_ID poolname_id);
DllExport bool Hyper3DMaskComponent(H3DFileInfo* h3d_file, H3D_ID id);
DllExport bool Hyper3DMaskSystem(H3DFileInfo* h3d_file, H3D_ID id);
DllExport bool Hyper3DUnmaskMarker(H3DFileInfo* h3d_file, H3D_ID id);
DllExport bool Hyper3DUnmaskEntitySet(H3DFileInfo* h3d_file, H3D_ID id);
DllExport bool Hyper3DMaskElementRange(H3DFileInfo* h3d_file, 
                    H3D_ID first_id, H3D_ID last_id);
DllExport bool Hyper3DMaskElement(H3DFileInfo* h3d_file, H3D_ID id);
DllExport bool Hyper3DMaskEnd(H3DFileInfo* h3d_file);

/* Text blocks */
DllExport bool Hyper3DFATXMLWrite(H3DFileInfo* h3d_file, unsigned int text_size, 
                    char* text_buffer);
DllExport bool Hyper3DTextWrite(H3DFileInfo* h3d_file, char* text_tag, 
                    H3D_SIM_IDX idx, H3D_ID subcase_id, 
                    unsigned int text_size, char* text_buffer);

/* View Blocks */
DllExport bool Hyper3DViewBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DViewWrite(H3DFileInfo* h3d_file, char* label, 
                    H3D_VIEW_TYPE type, float* view_matrix);
DllExport bool Hyper3DViewOrthoWrite(H3DFileInfo* h3d_file, char* label,
                    float xmin, float xmax, float ymin, float ymax, 
                    float zmin, float zmax);
DllExport bool Hyper3DViewPerspectiveWrite(H3DFileInfo* h3d_file, char* label,
                    float xmin, float xmax, float ymin, float ymax, 
                    float zmin, float zmax, float fov, float z_offset);
DllExport bool Hyper3DViewLensWrite(H3DFileInfo* h3d_file, char* label,
                    float angle, float focal_len, float sensor_ht, 
                    float x_orient, float y_orient, float z_orient,
                    float x_offset, float y_offset, float z_offset);
DllExport bool Hyper3DViewEnd(H3DFileInfo* h3d_file);


/**********************************/
/*  Result Data Export Functions  */
/**********************************/

/* Result Blocks */
/*  the purpose of this block is to establish defaults for interpreting      */
/*      the result data contained in this file                               */
/*                                                                           */
/*      count   - please use only 1 at this time                             */
/*      name    - may be NULL                                                */
DllExport bool Hyper3DResultBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DResultWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_DS_SHELL_METHOD method, unsigned int num_systems);
DllExport bool Hyper3DResultAddSystem(H3DFileInfo* h3d_file, 
                    H3D_ID poolname_id, int system_id, 
                    H3D_ID_POOL_TYPE sysType);
DllExport bool Hyper3DResultEnd(H3DFileInfo* h3d_file);

/* Subcase (Loadcase) Blocks */
/*  animation groups must be defined in order for the reader to determine    */
/*      which vector data types can be animated                              */
/*  HyperView's default is to animate none                                   */
DllExport bool Hyper3DSimSubcaseBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DSimSubcaseWrite(H3DFileInfo* h3d_file, const char* label,
                    H3D_ID id, H3D_ANALYSIS_TYPE atype,  
                    unsigned int num_datatypes, H3D_ID* datatype_ids, 
                    H3D_NODAL_DATA_TYPE anim_type);
DllExport bool Hyper3DSimSubcaseAnimationGroups(H3DFileInfo* h3d_file, 
                    H3D_ID id, 
                    unsigned int num_groups, H3D_ID* grp_datatype_ids, 
                    unsigned int* num_dts_per_grp, H3D_ID* datatype_ids);
DllExport bool Hyper3DSimSubcaseEnd(H3DFileInfo* h3d_file);

/* Derived Subcase Blocks */
DllExport bool Hyper3DDerivedSubcaseBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DDerivedSubcaseWrite(H3DFileInfo* h3d_file, const char* label,
                    H3D_ID id, H3D_SUBCASE_TYPE type, unsigned int num_sims);
DllExport bool Hyper3DDerivedSubcaseSimWrite(H3DFileInfo* h3d_file, const char* label,
                    H3D_ID id, H3D_ID ref_subcase_id, H3D_SIM_IDX ref_sim_idx,
                    float scale);
DllExport bool Hyper3DDerivedSubcaseEnd(H3DFileInfo* h3d_file);

/* Simulation Blocks */
/* * all time steps in this block are per subcase                            */
DllExport bool Hyper3DSimulationBegin(H3DFileInfo* h3d_file, unsigned int count,
                    H3D_ID subcase_id);
DllExport bool Hyper3DSimulationWrite(H3DFileInfo* h3d_file, H3D_SIM_IDX idx, 
                    const char* label, float syncValue); 
DllExport bool Hyper3DSimulationTabularWrite(H3DFileInfo* h3d_file, 
                    H3D_SIM_IDX idx, const char* label, float syncValue, 
                    bool adaptive, bool eroded); 
DllExport bool Hyper3DSimulationEnd(H3DFileInfo* h3d_file);

/* Datatype Blocks */
/*   DatatypeWrite must be called before corresponding DatatypePool API      */
/*                                                                           */
/*  an example:                                                              */
/*      2 calculations of Von Mises Stress data to be available, Z1 & Z2     */
/*      create a data type labeled "Stress:Von Mises" with 2 layers          */
/*      the ':' is defined as H3D_DT_DELIMITER                               */
/*      there can be multiple Stress:.... data types, they will be collated  */
DllExport bool Hyper3DDatatypeBegin(H3DFileInfo* h3d_file, unsigned int count);
DllExport bool Hyper3DDatatypeWrite(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID dt_id, H3D_DS_FORMAT format, H3D_DS_TYPE type, 
                    unsigned int num_pools);
DllExport bool Hyper3DDatatypeDescriptionWrite(H3DFileInfo* h3d_file, 
                    H3D_ID dt_id, const char* description);
DllExport bool Hyper3DDatatypePools(H3DFileInfo* h3d_file, H3D_ID dt_id, 
                    H3D_ID poolname_id, unsigned int num_layers, 
                    H3D_ID* layername_ids, bool corners,
                    H3D_TENSOR_TYPE tensor_type, float poisson);
DllExport bool Hyper3DDatatypeEnd(H3DFileInfo* h3d_file);

/* Dataset Blocks */
/* * all pool names are specified per block *                                */
/*  WriteParent is used when specifying nodal data and the node is on a      */
/*      component boundary and shared with another component                 */
/*  WriteWithSystem is used when this data is associated to a system         */
/*      different from that described in the Result Block default for the    */
/*      specified pool                                                       */
/*  Write is used when all defaults are to be applied                        */
/*                                                                           */
/*      num_corners - element centroidal and corner data must be specified   */
/*                      in separate blocks                                   */
/*      num_modes   - should be zero, this field only used by MotionSolve    */
/*      dt_id       - data type id associated with this set                  */
/*      layer_idx   - specific layer index of dt_id of this set              */
/*                      note: H3D_DS_NO_LAYER is specified if there is no    */
/*                            layer associated of the dt_id to this pool     */
/*      data_poolname_id  - poolname id where entity IDs are defined         */
/*      complex data is to be in magnitude/phase form                        */
/*          (i.e. Complex Vector data (MagX, MagY, MagZ, PhX, PhY, PhZ)      */
DllExport bool Hyper3DDatasetBegin(H3DFileInfo* h3d_file, unsigned int count, 
                    H3D_SIM_IDX idx, H3D_ID subcase_id, 
                    H3D_DS_TYPE type, H3D_DS_FORMAT format, 
                    unsigned int num_corners, unsigned int num_modes, 
                    H3D_ID dt_id, int layer_idx, H3D_ID data_poolname_id,
                    bool complex);
DllExport bool Hyper3DDatasetWriteParent(H3DFileInfo* h3d_file, H3D_ID comp_id, 
                    H3D_ID component_poolname_id);
DllExport bool Hyper3DDatasetWrite(H3DFileInfo* h3d_file, H3D_ID id,
                    const float* data);
DllExport bool Hyper3DDatasetWriteWithSystem(H3DFileInfo* h3d_file, H3D_ID id, 
                    const float* data, H3D_ID sys_id, H3D_ANALYSIS_SYSTEM system_flag);
DllExport bool Hyper3DDatasetDoubleWrite(H3DFileInfo* h3d_file, H3D_ID id,
                    const double* data);
DllExport bool Hyper3DDatasetDoubleWriteWithSystem(H3DFileInfo* h3d_file, H3D_ID id, 
                    const double* data, H3D_ID sys_id, H3D_ANALYSIS_SYSTEM system_flag);
DllExport bool Hyper3DDatasetEnd(H3DFileInfo* h3d_file);


/* Flexbody Datasets */
DllExport bool Hyper3DFlexDatasetBegin(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID poolname_id, H3D_FLEX_TYPE type,
                    unsigned int count, H3D_ID data_poolname_id, 
                    H3D_SIM_IDX idx, H3D_ID subcase_id);
DllExport bool Hyper3DFlexDatasetWithIdsBegin(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID poolname_id, H3D_FLEX_TYPE type, 
                    unsigned int count, H3D_ID* data_ids, H3D_ID data_poolname_id, 
                    H3D_SIM_IDX sim_idx, H3D_ID subcase_id);
DllExport bool Hyper3DFlexDatasetWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_FLEX_TYPE type, void* data);
DllExport bool Hyper3DFlexDatasetEnd(H3DFileInfo* h3d_file);

/* Flexbody Mode Blocked Datasets */
DllExport bool Hyper3DFlexBlockDatasetCompressionLevel(H3DFileInfo* h3d_file, unsigned int level);
DllExport bool Hyper3DFlexBlockDatasetBegin(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID poolname_id, H3D_FLEX_TYPE type, unsigned int num_blocks, 
                    H3D_ID* data_ids, H3D_ID data_poolname_id, unsigned int id_count, 
                    unsigned int data_count, H3D_SIM_IDX sim_idx, H3D_ID subcase_id);
DllExport bool Hyper3DFlexBlockDatasetWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_FLEX_TYPE type, unsigned int block_idx, void* data);
DllExport bool Hyper3DFlexBlockDatasetEnd(H3DFileInfo* h3d_file);

/* Matrix Datasets */
DllExport bool Hyper3DMatrixBegin(H3DFileInfo* h3d_file, const char* label, 
                    H3D_ID id, H3D_ID poolname_id, H3D_MTX_TYPE type, 
                    H3D_MTX_FORMAT mxt_type, H3D_MTX_VALUE_FORMAT val_type,
                    unsigned int num_columns, unsigned int num_rows,
                    uint64_t nz_count, 
                    H3D_ID parent_id, H3D_ID parent_poolname_id,
                    H3D_SIM_IDX idx, H3D_ID subcase_id);

/* all matrix labels must be written before            */
/*      ANY of the matrix data write commands are used */
DllExport bool Hyper3DMatrixLabelWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID column_id, H3D_ID row_id, H3D_ID label_id, 
                    H3D_ID entity_id, H3D_ID ent_poolname_id);

DllExport bool Hyper3DDenseMtxWrite(H3DFileInfo* h3d_file, H3D_ID id,
                    unsigned int start_column_id, 
                    unsigned int num_cols, void* data);

/* For a sparse mon matrix, M, we need to store three one-dimensional        */
/* arrays. Let NNZ denote the number of nonzero entries of M.                */
/* The first array is A, which is of length NNZ, and holds all nonzero       */
/* entries of M in top-to-bottom left-to-right order.                        */
/* The second array is JA, which is of length n + 1. JA(i) contains the      */
/* index in A of the first nonzero element of column i. Column i of the      */
/* original matrix extends from A(JA(i)) to A(JA(i+1)-1).                    */
/* The third array, IA, contains the row index of each element of A,         */
/* so it also is of length NNZ.                                              */
DllExport bool Hyper3DSparseMtxJaWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    unsigned int* data);
DllExport bool Hyper3DSparseMtxIaAaWrite(H3DFileInfo* h3d_file, H3D_ID id, 
                    H3D_ID start_column_id, unsigned int num_columns, 
                    unsigned int* dataJa, unsigned int* dataIa, void* dataAa);
DllExport bool Hyper3DMatrixEnd(H3DFileInfo* h3d_file);


#if defined (__cplusplus)
}
#endif

#endif /* ALTAIR_HYPER3DAPI_PUBLIC_EXPORT_INCLUDED */
