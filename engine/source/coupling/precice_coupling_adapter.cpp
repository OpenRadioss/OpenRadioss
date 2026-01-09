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
#include "precice_coupling_adapter.h"
#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#ifdef WITH_PRECICE

PreciceCouplingAdapter::PreciceCouplingAdapter() 
    : active_(false)
    , maxTimeStepSize_(0.0)
    , precice_(nullptr)
{
}

PreciceCouplingAdapter::~PreciceCouplingAdapter() {
    if (active_) {
        finalize();
    }
}

bool PreciceCouplingAdapter::configure(const std::string& configFile) {
     // Expected format of the configuration file:
     // /PRECICE/PARTICIPANT_NAME/SolverOne
     // /PRECICE/CONFIG_FILE/precice-config.xml
     // /PRECICE/MESH_NAME/MeshOne
     // /PRECICE/READ/DISPLACEMENTS
     // /PRECICE/WRITE/FORCES
     // /PRECICE/GRNOD/grnodID
    std::ifstream file(configFile);
    if (!file.is_open()) {
        std::cout << "No " << configFile << " file found in the current directory" << std::endl;
        std::cout << "preCICE will not be active" << std::endl;
        active_ = false;
        return false;
    }
    
    std::string line;
    while (std::getline(file, line)) {
        // Remove leading/trailing whitespace
        line.erase(0, line.find_first_not_of(" \t"));
        line.erase(line.find_last_not_of(" \t") + 1);
        
        if (line.empty() || line[0] == '#') continue;
        
        // Split by '/' and expect format: /PRECICE/KEY/VALUE
        std::vector<std::string> parts;
        std::istringstream iss(line);
        std::string part;
        
        while (std::getline(iss, part, '/')) {
            if (!part.empty()) {  // Skip empty parts (from leading '/')
                parts.push_back(part);
            }
        }
        
        // Expected format: parts[0] = "PRECICE", parts[1] = "KEY", parts[2] = "VALUE"
        if (parts.size() >= 3 && parts[0] == "PRECICE") {
            const auto& key = parts[1];
            auto value = parts[2];
            
            // Handle cases where VALUE might contain additional path segments
            if (parts.size() > 3) {
                for (size_t i = 3; i < parts.size(); ++i) {
                    value += "/" + parts[i];
                }
            }
            
            if (key == "PARTICIPANT_NAME") {
                participantName_ = value;
            } else if (key == "CONFIG_FILE") {
                configFile_ = value;
            } else if (key == "MESH_NAME") {
                meshName_ = value;
            } else if (key == "READ") {
                const auto dataType = stringToDataType(value);
                if (dataType == DataType::NOTHING) {
                    std::cerr << "Error: Unknown read data type '" << value << "' in the preCICE adapter configuration file " << configFile << std::endl;
                    return false;
                }
                readData_[static_cast<size_t>(dataType)].isActive = true;
                if (DataType::POSITIONS == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::REPLACE;
                } else if (DataType::FORCES == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::ADD;
                } else if (DataType::DISPLACEMENTS == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::REPLACE;
                }
            } else if (key == "WRITE") {
                const auto dataType = stringToDataType(value);
                if (dataType == DataType::NOTHING) {
                    std::cerr << "Error: Unknown write data type '" << value << "' in the preCICE adapter configuration file " << configFile << std::endl;
                    return false;
                }
                writeData_[static_cast<size_t>(dataType)].isActive = true;
            } else if (key == "GRNOD") {
                try {
                    setGroupNodeId(std::stoi(value));
                } catch (const std::exception& e) {
                    std::cerr << "Error: Invalid nodes group GRNOD '" << value << "' in the preCICE adapter configuration file " << configFile << ": " << e.what() << std::endl;
                    return false;
                }
            }
        } else {
            std::cout << "Warning: Ignoring malformed line: " << line << std::endl;
        }
    }
    
    active_ = true;
    return true;
}

void PreciceCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    couplingNodeIds_ = nodeIds;
    
    // Allocate buffers for 3D data
    constexpr auto dimensions = getDimensions();
    const auto bufferSize = couplingNodeIds_.size() * dimensions;
    vertexIds_.resize(couplingNodeIds_.size());
    
    // Loop over readData_
    for (auto& data : readData_) {
        if (data.isActive) {
            data.buffer.resize(bufferSize);
        }
    }
    
    // Loop over writeData_
    for (auto& data : writeData_) {
        if (data.isActive) {
            data.buffer.resize(bufferSize);
        }
    }
}

bool PreciceCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
    if (!active_) return false;
    if (!coordinates) {
        std::cerr << "Error: coordinates pointer is null" << std::endl;
        return false;
    }
    if (totalNodes <= 0) {
        std::cerr << "Error: totalNodes must be positive, got " << totalNodes << std::endl;
        return false;
    }
    
    try {
        // Create preCICE participant
        precice_ = std::make_unique<precice::Participant>(participantName_, configFile_, mpiRank, mpiSize);
        
        // Set up mesh vertices
        std::vector<double> meshVertices;
        meshVertices.reserve(couplingNodeIds_.size() * 3);
        
        for (const auto nodeId : couplingNodeIds_) {
            if (!isNodeIdValid(nodeId, totalNodes)) {
                std::cerr << "Error: Node ID " << nodeId << " out of bounds [1, " << totalNodes << "]" << std::endl;
                return false;
            }
            // Convert from 1-based Fortran indexing to 0-based C++ indexing
            const auto idx = nodeId - 1;
            constexpr auto dimensions = getDimensions();
            meshVertices.push_back(coordinates[idx * dimensions]);
            meshVertices.push_back(coordinates[idx * dimensions + 1]);
            meshVertices.push_back(coordinates[idx * dimensions + 2]);
        }
        // copy initial positions in meshVertices to x0_
        x0_ = meshVertices;
        
        // Set mesh vertices
        precice_->setMeshVertices(meshName_, meshVertices, vertexIds_);
        
        // Check if initial data is required
        if (precice_->requiresInitialData()) {
            for (size_t i = 0; i < writeData_.size(); ++i) {
                if (writeData_[i].isActive) {
                    const auto& dataName = dataTypeToString(static_cast<DataType>(i));
                    precice_->writeData(meshName_, dataName, vertexIds_, writeData_[i].buffer);
                }
            }
        }
        
        // Initialize preCICE
        precice_->initialize();
        maxTimeStepSize_ = precice_->getMaxTimeStepSize();
        
    } catch (const std::exception& e) {
        std::cerr << "Error initializing preCICE: " << e.what() << std::endl;
        return false;
    }
    
    return true;
}

void PreciceCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType) {
    if (!active_) return;
    if (!precice_) return;
    if (!precice_->isCouplingOngoing()) return;
    if (!values) {
        std::cerr << "Error: values pointer is null in writeData" << std::endl;
        return;
    }

    const auto& writeDataName = dataTypeToString(static_cast<DataType>(dataType));
    if (!writeData_[static_cast<size_t>(dataType)].isActive) {
        return;
    }
    
    // Extract data for coupling nodes
    extractNodeData(values, totalNodes, dataType);
    precice_->writeData(meshName_, writeDataName, vertexIds_, writeData_[dataType].buffer);
}

void PreciceCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType) {
    if (!active_) return;
    if (!precice_) return;
    if (!precice_->isCouplingOngoing()) return;
    if (!values) {
        std::cerr << "Error: values pointer is null in readData" << std::endl;
        return;
    }
    
    const auto& readDataName = dataTypeToString(static_cast<DataType>(dataType));
    if (!readData_[static_cast<size_t>(dataType)].isActive) {
        return;
    }
    
    const auto maxTimeStepSize = precice_->getMaxTimeStepSize();
    const auto cdt = std::min(dt, maxTimeStepSize);

    // Read data from preCICE
    precice_->readData(meshName_, readDataName, vertexIds_, cdt, readData_[dataType].buffer);
   
    // Inject data into global arrays
    injectNodeData(values, totalNodes, dataType);
}

void PreciceCouplingAdapter::advance(double& dt) {
    if (!active_) return;
    if (!precice_) return;
    if (!precice_->isCouplingOngoing()) return;
    
    const auto dtToUse = std::min(dt, maxTimeStepSize_);
    
    // Advance preCICE
    precice_->advance(dtToUse);
    
    // Update max time step size
    maxTimeStepSize_ = precice_->getMaxTimeStepSize();
    dt = dtToUse;
}

bool PreciceCouplingAdapter::isCouplingOngoing() const {
    if (!active_) return false;
    if (!precice_) return false;
    return precice_->isCouplingOngoing();
}

bool PreciceCouplingAdapter::requiresWritingCheckpoint() const {
    if (!active_) return false;
    if (!precice_) return false;
    return precice_->requiresWritingCheckpoint();
}

bool PreciceCouplingAdapter::requiresReadingCheckpoint() const {
    if (!active_) return false;
    if (!precice_) return false;
    return precice_->requiresReadingCheckpoint();
}

void PreciceCouplingAdapter::finalize() {
    if (!active_) return;
    
    if (precice_) {
        precice_->finalize();
        precice_.reset();
    }
    
    // Clear all data structures
    couplingNodeIds_.clear();
    vertexIds_.clear();
    for (auto& data : readData_) {
        data.buffer.clear();
        data.isActive = false;
    }
    for (auto& data : writeData_) {
        data.buffer.clear();
        data.isActive = false;
    }
    
    active_ = false;
}

bool PreciceCouplingAdapter::isActive() const {
    return active_;
}

double PreciceCouplingAdapter::getMaxTimeStepSize() const {
    return maxTimeStepSize_;
}

int PreciceCouplingAdapter::getNumberOfCouplingNodes() const {
    return static_cast<int>(couplingNodeIds_.size());
}

void PreciceCouplingAdapter::extractNodeData(const double* globalValues, int totalNodes, int dataType) {
    if (!writeData_[dataType].isActive) {
        return;
    }
    constexpr auto dimensions = getDimensions();
    for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
        const auto nodeId = couplingNodeIds_[i];
        if (!isNodeIdValid(nodeId, totalNodes)) {
            std::cerr << "Error: Node ID " << nodeId << " out of bounds in extractNodeData" << std::endl;
            continue;
        }
        const auto idx = nodeId - 1; // Convert to 0-based indexing
        writeData_[dataType].buffer[i * dimensions] = globalValues[idx * dimensions];
        writeData_[dataType].buffer[i * dimensions + 1] = globalValues[idx * dimensions + 1];
        writeData_[dataType].buffer[i * dimensions + 2] = globalValues[idx * dimensions + 2];
    }
}

void PreciceCouplingAdapter::injectNodeData(double* globalValues, int totalNodes, int dataType) {
    if (!readData_[dataType].isActive) {
        return;
    }
    if (readData_[dataType].mode == Mode::ADD) {
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] += readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] += readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] += readData_[dataType].buffer[i * 3 + 2];
        }
    }  else if (readData_[dataType].mode == Mode::REPLACE) {
        // write a debug message, with the name of the data type being injected and the participant name
        std::cout << "READ " << dataTypeToString(static_cast<DataType>(dataType))
                  << " for participant " << participantName_ << std::endl;
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] = readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] = readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] = readData_[dataType].buffer[i * 3 + 2];
        }
    } else {
        std::cout << "Warning: Unknown mode for data type " << dataTypeToString(static_cast<DataType>(dataType))
                  << ", skipping injection." << std::endl;
    }
}

#endif
