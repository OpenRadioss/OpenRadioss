//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
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
#include "coupling_c_interface.h"
#include "coupling_factory.h"
#include "coupling.h"
#include <string>
#include <vector>
#include <iostream>
#ifdef WITH_CWIPI
#include "cwipi_coupling_adapter.h"
#endif

extern "C" {

// Factory function: creates and returns a new coupling adapter instance.
// - If WITH_PRECICE: returns new PreciceCouplingAdapter()
// - If WITH_CWIPI: returns new CwipiCouplingAdapter()
// - Otherwise: returns DummyCouplingAdapter()
// No direct preCICE or CWIPI function is called here; just object construction.
void* coupling_adapter_create() {
    return createCouplingAdapter();
}

// Deletes the adapter instance created by coupling_adapter_create.
// - For both preCICE and CWIPI: calls the destructor of the adapter, which in turn calls finalize() if active.
//   - PreciceCouplingAdapter::~PreciceCouplingAdapter():  finalize()
//   - CwipiCouplingAdapter::~CwipiCouplingAdapter(): finalize()
void coupling_adapter_destroy(void* adapter) {
    delete static_cast<CouplingAdapter*>(adapter);
}

// Configures the adapter using a configuration file.
// - preCICE: calls PreciceCouplingAdapter::configure(const std::string& configFile)
//   - Parses config, sets up participant, mesh, data types, etc.
// - CWIPI: calls CwipiCouplingAdapter::configure(const std::string& configFile)
//   - Parses config, sets up application/coupling names, data types, etc.
//      cwipi_init
int coupling_adapter_configure(void* adapter, const char* filename) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->configure(std::string(filename)) ? 1 : 0;
}

// Sets the list of node IDs to be used for coupling.
// - preCICE: calls PreciceCouplingAdapter::setNodes(const std::vector<int>& nodeIds)
//   - Allocates buffers, stores node IDs, prepares for mesh setup.
// - CWIPI: calls CwipiCouplingAdapter::setNodes(const std::vector<int>& nodeIds)
//   - Allocates buffers, stores node IDs, prepares for mesh setup.


void coupling_adapter_set_nodes(void* adapter, const int* nodeIds, int numNodes) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    std::vector<int> nodes(nodeIds, nodeIds + numNodes);
    ca->setNodes(nodes);
}

// Initializes the coupling adapter with node coordinates and MPI info.
// - preCICE: calls PreciceCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize)
//   - Creates preCICE participant, sets mesh vertices, initializes coupling.
//     PreciceCouplingAdapter::initialize()
//     precice::Participant constructor
//     precice_->setMeshVertices()
//     precice_->writeData() (if initial data required)
//     precice_->initialize()
//     precice_->getMaxTimeStepSize()
// - CWIPI: calls CwipiCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize)
//   - Initializes CWIPI, creates coupling, defines mesh, locates points.
//     CwipiCouplingAdapter::initialize()
//     cwipi_create_coupling()
//     cwipi_define_mesh() or cwipi_ho_define_mesh()
//     cwipi_locate()
//     cwipi_get_n_not_located_points()
int coupling_adapter_initialize(void* adapter, const double* coordinates, 
                               int totalNodes, int mpiRank, int mpiSize) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->initialize(coordinates, totalNodes, mpiRank, mpiSize) ? 1 : 0;
}

// Writes data (e.g., displacements, forces, positions) to the coupling interface.
// - preCICE: calls PreciceCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType)
//   - Extracts node data, calls preCICE::writeData() for the mesh/data type.
// - CWIPI: calls CwipiCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType)
//   - Extracts node data, calls cwipi_issend() and cwipi_wait_issend() for async send.
void coupling_adapter_write_data(void* adapter, const double* values, 
                                int totalNodes, double dt, int dataType) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->writeData(values, totalNodes, dt, dataType);
}

// Reads data (e.g., displacements, forces, positions) from the coupling interface.
// - preCICE: calls PreciceCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType)
//   - Calls preCICE::readData() for the mesh/data type, injects into global arrays.
// - CWIPI: calls CwipiCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType)
//   - Calls cwipi_irecv() and cwipi_wait_irecv() for async receive, injects into global arrays.
void coupling_adapter_read_data(void* adapter, double* values, 
                               int totalNodes, double dt, int dataType) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->readData(values, totalNodes, dt, dataType);
}

// Advances the coupling interface by a time step.
// - preCICE: calls PreciceCouplingAdapter::advance(double& dt)
//   - Calls preCICE::advance(), updates max time step size.
// - CWIPI: calls CwipiCouplingAdapter::advance(double& dt)
//   - Calls cwipi_locate(), checks for not located points.
void coupling_adapter_advance(void* adapter, double* dt) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->advance(*dt);
}

// Checks if the coupling is still ongoing.
// - preCICE: calls PreciceCouplingAdapter::isCouplingOngoing()
//   - Returns preCICE::isCouplingOngoing().
// - CWIPI: calls CwipiCouplingAdapter::isCouplingOngoing()
//   - Returns true if active and initialized (CWIPI does not have a strict ongoing state).
int coupling_adapter_is_coupling_ongoing(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->isCouplingOngoing() ? 1 : 0;
}

// Checks if a checkpoint write is required.
// - preCICE: calls PreciceCouplingAdapter::requiresWritingCheckpoint()
//   - Returns preCICE::requiresWritingCheckpoint().
// - CWIPI: calls CwipiCouplingAdapter::requiresWritingCheckpoint()
//   - Always returns false (not required for CWIPI).
int coupling_adapter_requires_writing_checkpoint(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->requiresWritingCheckpoint() ? 1 : 0;
}

// Checks if a checkpoint read is required.
// - preCICE: calls PreciceCouplingAdapter::requiresReadingCheckpoint()
//   - Returns preCICE::requiresReadingCheckpoint().
// - CWIPI: calls CwipiCouplingAdapter::requiresReadingCheckpoint()
//   - Always returns false (not required for CWIPI).
int coupling_adapter_requires_reading_checkpoint(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->requiresReadingCheckpoint() ? 1 : 0;
}

// Finalizes and cleans up the coupling adapter.
// preCICE: PreciceCouplingAdapter::finalize() → precice_->finalize()
// CWIPI: CwipiCouplingAdapter::finalize() → cwipi_delete_coupling(), cwipi_finalize()
void coupling_adapter_finalize(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->finalize();
}

// Returns whether the adapter is active.
int coupling_adapter_is_active(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->isActive() ? 1 : 0;
}

// Returns the maximum allowed time step size for the coupling.
// - preCICE: calls PreciceCouplingAdapter::getMaxTimeStepSize()
double coupling_adapter_get_max_time_step_size(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getMaxTimeStepSize();
}

// Returns the number of coupling nodes.
int coupling_adapter_get_num_coupling_nodes(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getNumberOfCouplingNodes();
}

// Returns the group node ID used for coupling.
int coupling_adapter_get_group_node_id(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getGroupNodeId();
}

// Returns the surface ID used for coupling (not always used).
int coupling_adapter_get_surface_id(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getSurfaceId();
}

// Returns the MPI communicator used by the adapter.
// - preCICE: calls PreciceCouplingAdapter::getCommunicator()
//   - Returns 0 (default, not used).
// - CWIPI: calls CwipiCouplingAdapter::getCommunicator()
//   - Returns Fortran MPI communicator handle (via MPI_Comm_c2f).
int coupling_adapter_get_communicator(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getCommunicator();
}


// Sets the mesh connectivity for the coupling (only used for CWIPI).
// - CWIPI: calls CwipiCouplingAdapter::setMesh(const int* elem_node_offsets, const int* elem_node_indices, int num_elements)
//   - Stores mesh connectivity for later use in mesh definition.
void coupling_adapter_set_mesh(void* adapter, const int* elem_node_offsets, const int* elem_node_indices, int num_elements) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    
    // Try to cast to CwipiCouplingAdapter to call setMesh
#ifdef WITH_CWIPI
    CwipiCouplingAdapter* cwipi_adapter = dynamic_cast<CwipiCouplingAdapter*>(ca);
    if (cwipi_adapter) {
        cwipi_adapter->setMesh(elem_node_offsets, elem_node_indices, num_elements);
        return;
    }
#endif
    
}

} // extern "C"
