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
#include "cwipi_coupling_adapter.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>

#ifdef WITH_CWIPI

CwipiCouplingAdapter::CwipiCouplingAdapter() 
    : active_(false)
    , maxTimeStepSize_(1e30)
    , initialized_(false)
    , dimension_(3)
    , tolerance_(0.1)
    , order_(1)
    , numElements_(0)
    , applicationName_("Radioss")
    , coupledAppName_("Radioss")
    , couplingName_("Coupling")
    , exchangeName_("Exchange")
{
}

CwipiCouplingAdapter::~CwipiCouplingAdapter() {
    if (active_) {
        finalize();
    }
}

bool CwipiCouplingAdapter::configure(const std::string& configFile) {
     // Expected format of the configuration file:
     //  /CWIPI/APPLICATION_NAME/SolverOne
     //  /CWIPI/COUPLED_APP_NAME/SolverTwo
     //  /CWIPI/READ/FORCES
     //  /CWIPI/WRITE/POSITIONS
     //  /CWIPI/SURF/8
    std::cout << "Configuring CwipiCouplingAdapter with file: " << configFile << std::endl;
    std::ifstream file(configFile);
    if (!file.is_open()) {
        std::cout << "No " << configFile << " file found in the current directory" << std::endl;
        std::cout << "CWIPI will not be active" << std::endl;
        active_ = false;
        return false;
    }
    
    std::string line;
    while (std::getline(file, line)) {
        // Remove leading/trailing whitespace
        line.erase(0, line.find_first_not_of(" \t"));
        line.erase(line.find_last_not_of(" \t") + 1);
        
        if (line.empty() || line[0] == '#') continue;
        
        // Parse CWIPI configuration
        // Expected format: /CWIPI/KEY/VALUE
        std::vector<std::string> parts;
        std::istringstream iss(line);
        std::string part;
        
        while (std::getline(iss, part, '/')) {
            if (!part.empty()) {
                parts.push_back(part);
            }
        }
        
        if (parts.size() >= 3 && parts[0] == "CWIPI") {
            std::string key = parts[1];
            std::string value = parts[2];
            
            // Handle cases where VALUE might contain additional path segments
            if (parts.size() > 3) {
                for (size_t i = 3; i < parts.size(); ++i) {
                    value += "/" + parts[i];
                }
            }
            if (key == "APPLICATION_NAME") {
                std::cout << "Setting applicationName_ to " << value << std::endl;
                applicationName_ = value;
            } else if (key == "COUPLED_APP_NAME") {
                std::cout << "Setting coupledAppName_ to " << value << std::endl;
                coupledAppName_ = value;
            } else if (key == "COUPLING_NAME") {
                couplingName_ = value;
            } else if (key == "EXCHANGE_NAME") {
                std::cout << "Setting exchangeName_ to " << value << std::endl;
                exchangeName_ = value;
            } else if (key == "DIMENSION") {
                dimension_ = std::stoi(value);
            } else if (key == "TOLERANCE") {
                tolerance_ = std::stod(value);
            } else if (key == "ORDER") {
                order_ = std::stoi(value);
            } else if (key == "READ") {
                DataType dataType = stringToDataType(value);
                if (dataType == DataType::NOTHING) {
                    std::cout << "Warning: Unknown data type '" << value << "' in read configuration." << std::endl;
                    continue;
                }
                readData_[static_cast<size_t>(dataType)].isActive = true;
                if (DataType::POSITIONS == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::REPLACE;
                } else if (DataType::FORCES == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::ADD;
                }
//                std::cout << "Setting readData_[" << static_cast<size_t>(dataType) 
//                          << "].isActive to true for data type " << value << std::endl;
            } else if (key == "WRITE") {
                DataType dataType = stringToDataType(value);
                if (dataType == DataType::NOTHING) {
                    std::cout << "Warning: Unknown data type '" << value << "' in write configuration." << std::endl;
                    continue;
                }
                writeData_[static_cast<size_t>(dataType)].isActive = true;
 //               std::cout << "Setting writeData_[" << static_cast<size_t>(dataType) 
 //                         << "].isActive to true for data type " << value << std::endl;
            } else if (key == "GRNOD") {
                // should not be used, interface should be defined by /CWIPI/SURF instead of a grnod with cwipi
                setGroupNodeId(std::stoi(value));
                std::cout << "Setting group_node_id_ to " << value << std::endl;
            }
            else if (key == "SURF") {
                //surface_id_ = std::stoi(value);
                setSurfaceId(std::stoi(value));
                std::cout<<" Setting surface_id_ to " << value << std::endl;
            } else {
                std::cout << "Warning: Unknown CWIPI configuration key '" << key << "'." << std::endl;
            }
        }
    }
    // call initialize to set up the CWIPI environment 
    std::cout << "Initializing CWIPI with application name: " << applicationName_ << std::endl;
    cwipi_init(MPI_COMM_WORLD, applicationName_.c_str(), &localComm_);
    
    int currentRank, localCommSize; 
    MPI_Comm_rank(localComm_, &currentRank);
    MPI_Comm_size(localComm_, &localCommSize);
    std::cout << "CWIPI initialized on rank " << currentRank 
              << " with local communicator size: " << localCommSize << std::endl;


    active_ = true;
    return true;
}


void CwipiCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    couplingNodeIds_ = nodeIds;
    
    // Allocate buffers for 3D data
    int bufferSize = couplingNodeIds_.size() * 3;
    
    for (size_t i = 0; i < readData_.size(); ++i) {
        if (readData_[i].isActive) {
            readData_[i].buffer.resize(bufferSize);
        }
    }
    
    for (size_t i = 0; i < writeData_.size(); ++i) {
        if (writeData_[i].isActive) {
            writeData_[i].buffer.resize(bufferSize);
        }
    }
}

void CwipiCouplingAdapter::setMesh(const int* elem_node_offsets, const int* elem_node_indices, int num_elements) {
    numElements_ = num_elements;
    eltsConnecPointer_.assign(elem_node_offsets, elem_node_offsets + num_elements + 1);
    eltsConnec_.assign(elem_node_indices, elem_node_indices + elem_node_offsets[num_elements]);
}

bool CwipiCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
    if (!active_) return false;
    
    try {
        // 1. Initialize CWIPI
        std::cout << "Initializing CWIPI coupling adapter..." << std::endl; 
        // 2. Create coupling
        cwipi_create_coupling(couplingName_.c_str(),
                             CWIPI_COUPLING_PARALLEL_WITH_PARTITIONING,
                             coupledAppName_.c_str(),
                             dimension_,
                             tolerance_,
                             CWIPI_STATIC_MESH,
                             CWIPI_SOLVER_CELL_VERTEX,
                             -1,  // No postprocessing
                             "EnSight Gold",
                             "text");
        
        // 3. Set up mesh vertices for coupling nodes
        std::vector<double> meshVertices;
        meshVertices.reserve(couplingNodeIds_.size() * 3);
        
        for (int nodeId : couplingNodeIds_) {
            // Convert from 1-based Fortran indexing to 0-based C++ indexing
            int idx = nodeId - 1;
            meshVertices.push_back(coordinates[idx * 3]);
            meshVertices.push_back(coordinates[idx * 3 + 1]);
            meshVertices.push_back(coordinates[idx * 3 + 2]);
        }
        
        // 4. Define mesh
        if (order_ == 1) {
            std::cout << "Defining CWIPI mesh with linear elements." << std::endl;
            // couplingNodeIds_.size() should be equal to meshVertices.size() / 3
            //std::cout << "Number of coupling nodes: " << couplingNodeIds_.size() 
            //          << ", Number of mesh vertices: " << meshVertices.size() / 3 << std::endl;
            cwipi_define_mesh(couplingName_.c_str(),
                             couplingNodeIds_.size(),
                             numElements_,
                             meshVertices.data(),
                             eltsConnecPointer_.data(),
                             eltsConnec_.data());
        } else {
            std::cout << "Defining CWIPI mesh with higher-order elements." << std::endl;
            cwipi_ho_define_mesh(couplingName_.c_str(),
                                couplingNodeIds_.size(),
                                numElements_,
                                order_,
                                meshVertices.data(),
                                eltsConnecPointer_.data(),
                                eltsConnec_.data());
        }
        
        initialized_ = true;
        cwipi_locate(couplingName_.c_str());
        int nNotLocated = cwipi_get_n_not_located_points(couplingName_.c_str());
        if (nNotLocated > 0) {
            std::cout << "Warning: " << nNotLocated << " points not located in coupling " 
                      << couplingName_ << std::endl;
        }
 
    } catch (const std::exception& e) {
        std::cerr << "Error initializing CWIPI: " << e.what() << std::endl;
        return false;
    }
    
    return true;
}

void CwipiCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType) {
    if (!active_ || !initialized_) return;
    
    if (!writeData_[static_cast<size_t>(dataType)].isActive) {
        return;
    }
    
    // Extract data for coupling nodes
    extractNodeData(values, totalNodes, dataType);
    
    // Start async send
    std::string fieldName = getFieldName(static_cast<DataType>(dataType));
    int tag = getTag(static_cast<DataType>(dataType));
    std::cout << "Sending data for field: " << fieldName 
              << " with tag: " << tag << std::endl; 
    cwipi_issend(couplingName_.c_str(),
                exchangeName_.c_str(),
                tag,
                1,    // time step (user controlled)
                1,    // n_step
                0.0,  // physical_time (user controlled)
                fieldName.c_str(),
                writeData_[dataType].buffer.data(),
                &writeData_[dataType].sendRequest);

    // Wait for send completion
//  std::cout << "Waiting for data send completion for field: " << fieldName << std::endl;
    cwipi_wait_issend(couplingName_.c_str(), writeData_[dataType].sendRequest);


//    // Wait for any pending sends to complete
//    for (size_t i = 0; i < writeData_.size(); ++i) {
//        if (writeData_[i].isActive && writeData_[i].sendRequest != -1) {
//            std::cout << "Waiting for send completion for field: " 
//                      << getFieldName(static_cast<DataType>(i)) << std::endl;
//            cwipi_wait_issend(couplingName_.c_str(), writeData_[i].sendRequest);
//            writeData_[i].sendRequest = -1;  // Reset request
//        }
//    }
// 
}

void CwipiCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType) {
    if (!active_ || !initialized_) return;
    
    if (!readData_[static_cast<size_t>(dataType)].isActive) {
        return;
    }
    
    // Start async receive
    std::string fieldName = getFieldName(static_cast<DataType>(dataType));
    int tag = getTag(static_cast<DataType>(dataType));
//  std::cout << "Receiving data for field: " << fieldName 
//            << " with tag: " << tag << std::endl; 
    cwipi_irecv(couplingName_.c_str(),
               exchangeName_.c_str(),
               tag,
               1,    // time step
               1,    // n_step
               0.0,  // physical_time
               fieldName.c_str(),
               readData_[dataType].buffer.data(),
               &readData_[dataType].recvRequest);
    
    // Wait for receive completion
//  std::cout << "Waiting for data reception for field: " << fieldName << std::endl;
    cwipi_wait_irecv(couplingName_.c_str(), readData_[dataType].recvRequest);
//  std::cout<<"COMPLETED irecv for " << fieldName << std::endl;
    //std::cout << "Checking for not located points in coupling: " << couplingName_ << std::endl;
   
    // Inject received data into global arrays
    injectNodeData(values, totalNodes, dataType);
}

void CwipiCouplingAdapter::advance(double& dt) {
    if (!active_ || !initialized_) return;
    
    // Trigger location/interpolation update
//  std::cout << "Advancing CWIPI coupling adapter with dt: " << dt << std::endl;
    cwipi_locate(couplingName_.c_str());
    int nNotLocated = cwipi_get_n_not_located_points(couplingName_.c_str());
    if (nNotLocated > 0) {
        std::cout << "Warning: " << nNotLocated << " points not located in coupling " 
                  << couplingName_ << std::endl;
    }
 
}

bool CwipiCouplingAdapter::isCouplingOngoing() const {
    if (!active_ || !initialized_)
    {
        std::cout << "Warning: Coupling is not active or not initialized." << std::endl;
        std::cout<<" active_: " << active_ << ", initialized_: " << initialized_ << std::endl;
        return false; // Not active or not initialized
    } 
    
    // CWIPI doesn't have an inherent "ongoing" state like preCICE
    // Return true if coupling is active and initialized
    return true;
}

bool CwipiCouplingAdapter::requiresWritingCheckpoint() const {
    return false; // CWIPI typically doesn't require checkpoints
}

bool CwipiCouplingAdapter::requiresReadingCheckpoint() const {
    return false; // CWIPI typically doesn't require checkpoints
}

void CwipiCouplingAdapter::finalize() {
    if (!active_) return;
    
    if (initialized_) {
        // Delete coupling
        std::cout << "Finalizing CWIPI coupling adapter..." << std::endl;
        cwipi_delete_coupling(couplingName_.c_str());
        
        // Finalize CWIPI
        cwipi_finalize();
        initialized_ = false;
    }
    
    // Clear all data structures
    couplingNodeIds_.clear();
    eltsConnecPointer_.clear();
    eltsConnec_.clear();
    
    for (auto& data : readData_) {
        data.buffer.clear();
        data.isActive = false;
        data.sendRequest = -1;
        data.recvRequest = -1;
    }
    for (auto& data : writeData_) {
        data.buffer.clear();
        data.isActive = false;
        data.sendRequest = -1;
        data.recvRequest = -1;
    }
    
    active_ = false;
    return;
}

bool CwipiCouplingAdapter::isActive() const {
    return active_;
}

double CwipiCouplingAdapter::getMaxTimeStepSize() const {
    return maxTimeStepSize_;
}

int CwipiCouplingAdapter::getNumberOfCouplingNodes() const {
    return couplingNodeIds_.size();
}

void CwipiCouplingAdapter::extractNodeData(const double* globalValues, int totalNodes, int dataType) {
    if (!writeData_[dataType].isActive) {
        return;
    }
    
    for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
        int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
        writeData_[dataType].buffer[i * 3] = globalValues[nodeId * 3];
        writeData_[dataType].buffer[i * 3 + 1] = globalValues[nodeId * 3 + 1];
        writeData_[dataType].buffer[i * 3 + 2] = globalValues[nodeId * 3 + 2];
    }
}

void CwipiCouplingAdapter::injectNodeData(double* globalValues, int totalNodes, int dataType) {
    if (!readData_[dataType].isActive) {
        std::cout << "Warning: Attempted to read data for inactive data type " 
                  << getFieldName(static_cast<DataType>(dataType)) << std::endl;
        return;
    }
    
    if (readData_[dataType].mode == Mode::ADD) {
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] += readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] += readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] += readData_[dataType].buffer[i * 3 + 2];
        }
    } else if (readData_[dataType].mode == Mode::REPLACE) {
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] = readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] = readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] = readData_[dataType].buffer[i * 3 + 2];
        }
    }

}

std::string CwipiCouplingAdapter::getFieldName(DataType type) {
    switch(type) {
        case DataType::POSITIONS: return "positions";
        case DataType::FORCES: return "forces";
        case DataType::DISPLACEMENTS: return "displacements";
        default: return "unknown";
    }
}

int CwipiCouplingAdapter::getTag(DataType type) {
    return static_cast<int>(type);
}

#endif
