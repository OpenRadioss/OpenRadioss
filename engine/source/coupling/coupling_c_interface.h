//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
#ifndef COUPLING_C_INTERFACE_H
#define COUPLING_C_INTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif

// C interface for Fortran binding
// Should be kept generic enough to work with any coupling adapter (CWIPI, preCICE, etc.)

// Object management
void* coupling_adapter_create();
void coupling_adapter_destroy(void* adapter);

// Configuration
int coupling_adapter_configure(void* adapter, const char* filename);
void coupling_adapter_set_nodes(void* adapter, const int* nodeIds, int numNodes);

// Initialization
int coupling_adapter_initialize(void* adapter, const double* coordinates,int n2d, 
                               int totalNodes, int mpiRank, int mpiSize);

// Data exchange
void coupling_adapter_write_data(void* adapter, const double* values, 
                                int totalNodes, double dt, int dataType);
void coupling_adapter_read_data(void* adapter, double* values, 
                               int totalNodes, double dt, int dataType);

// Simulation control
void coupling_adapter_advance(void* adapter, double* dt);
int coupling_adapter_is_coupling_ongoing(void* adapter);
int coupling_adapter_requires_writing_checkpoint(void* adapter);
int coupling_adapter_requires_reading_checkpoint(void* adapter);

// Finalization
void coupling_adapter_finalize(void* adapter);

// Getters
int coupling_adapter_is_active(void* adapter);
double coupling_adapter_get_max_time_step_size(void* adapter);
int coupling_adapter_get_num_coupling_nodes(void* adapter);
int coupling_adapter_get_group_node_id(void* adapter);
int coupling_adapter_get_surface_id(void* adapter);
int coupling_adapter_get_communicator(void* adapter);
void coupling_adapter_get_coupled_data(void* adapter, int* rd, int* wd);


#ifdef __cplusplus
}
#endif

#endif 
