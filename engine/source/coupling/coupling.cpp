#include "coupling.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cstring>

// =============================================================================
// Dummy Coupling Adapter Implementation (always available)
// =============================================================================

bool DummyCouplingAdapter::configure(const std::string& configFile) {
    std::cout << "No coupling library available - coupling disabled" << std::endl;
    return true;
}

void DummyCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    // Do nothing
}

bool DummyCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
    std::cout << "Dummy coupling adapter initialized (no actual coupling)" << std::endl;
    return true;
}

void DummyCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType) {
    // Do nothing
}

void DummyCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataTypee) {
    // Do nothing
}

void DummyCouplingAdapter::advance(double& dt) {
    // Do nothing - dt remains unchanged
}

bool DummyCouplingAdapter::isCouplingOngoing() const {
    return false; // No coupling, so never ongoing
}

bool DummyCouplingAdapter::requiresWritingCheckpoint() const {
    return false;
}

bool DummyCouplingAdapter::requiresReadingCheckpoint() const {
    return false;
}

void DummyCouplingAdapter::finalize() {
    // Do nothing
}

bool DummyCouplingAdapter::isActive() const {
    return false;
}

double DummyCouplingAdapter::getMaxTimeStepSize() const {
    return 1e30; // No time step limit
}

int DummyCouplingAdapter::getNumberOfCouplingNodes() const {
    return 0;
}

// =============================================================================
// preCICE Coupling Adapter Implementation (only if WITH_PRECICE is defined)
// =============================================================================

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
        
        // Debug output
        //std::cout << "Reading line: " << line << std::endl;
        
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
            std::string key = parts[1];
            std::string value = parts[2];
            
            // Handle cases where VALUE might contain additional path segments
            if (parts.size() > 3) {
                for (size_t i = 3; i < parts.size(); ++i) {
                    value += "/" + parts[i];
                }
            }
            
            //std::cout << "Key: " << key << ", Value: " << value << std::endl;
            
            if (key == "PARTICIPANT_NAME") {
                participantName_ = value;
            } else if (key == "CONFIG_FILE") {
                configFile_ = value;
            } else if (key == "MESH_NAME") {
                meshName_ = value;
            } else if (key == "READ") {
                DataType dataType = stringToDataType(value);
                if(dataType == DataType::NOTHING) {
                    std::cout << "Warning: Unknown data type '" << value << "' in read configuration." << std::endl;
                    continue;
                }
                readData_[static_cast<size_t>(dataType)].isActive = true;
                if(DataType::POSITIONS == dataType) {
                    // Special case for positions, set mode to REPLACE
                    readData_[static_cast<size_t>(dataType)].mode = Mode::REPLACE;
                } else if (DataType::FORCES== dataType) {
                    // Special case for velocities, set mode to ADD
                    readData_[static_cast<size_t>(dataType)].mode = Mode::ADD;
                }
            } else if (key == "WRITE") {
                DataType dataType = stringToDataType(value);
                if(dataType == DataType::NOTHING) {
                    std::cout << "Warning: Unknown data type '" << value << "' in write configuration." << std::endl;
                    continue;
                }
                writeData_[static_cast<size_t>(dataType)].isActive = true;
            } else if (key == "INTERFACE") {
                setGroupNodeId(std::stoi(value));
            }
        } else {
            std::cout << "Warning: Ignoring malformed line: " << line << std::endl;
        }
    }
    
    // Print the configuration
    //std::cout << "Participant Name: " << participantName_ << std::endl;
    //std::cout << "Config File: " << configFile_ << std::endl;
    //std::cout << "Mesh Name: " << meshName_ << std::endl;   
    
    active_ = true;
    return true;
}


void PreciceCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    couplingNodeIds_ = nodeIds;
    
    // Allocate buffers for 3D data
    int bufferSize = couplingNodeIds_.size() * 3;
    vertexIds_.resize(couplingNodeIds_.size());
    // loop over readData_
    for(size_t i = 0; i < readData_.size(); ++i) {
        if (readData_[i].isActive) {
            readData_[i].buffer.resize(bufferSize);
        }
    }
    // loop over writeData_
    for(size_t i = 0; i < writeData_.size(); ++i) {
        if (writeData_[i].isActive) {
            writeData_[i].buffer.resize(bufferSize);
        }
    }
}

bool PreciceCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
    if (!active_) return false;
    
    //std::cout << "participant=" << participantName_ << std::endl;
    //std::cout << "config_file=" << configFile_ << std::endl;
    //std::cout << "mpi_rank=" << mpiRank << " mpi_size=" << mpiSize << std::endl;
    
    try {
        // Create preCICE participant
        precice_ = std::make_unique<precice::Participant>(participantName_, configFile_, mpiRank, mpiSize);
        
        // Set up mesh vertices
        std::vector<double> meshVertices;
        meshVertices.reserve(couplingNodeIds_.size() * 3);
        
        for (int nodeId : couplingNodeIds_) {
            // Convert from 1-based Fortran indexing to 0-based C++ indexing
            int idx = nodeId - 1;
            meshVertices.push_back(coordinates[idx * 3]);
            meshVertices.push_back(coordinates[idx * 3 + 1]);
            meshVertices.push_back(coordinates[idx * 3 + 2]);
        }
        
        // Set mesh vertices
        precice_->setMeshVertices(meshName_, meshVertices, vertexIds_);
        
        // Check if initial data is required
        if (precice_->requiresInitialData()) {
            //std::cout << participantName_ << " writing initial data" << std::endl;
            for(size_t i = 0; i < writeData_.size(); ++i) {
                if (writeData_[i].isActive) {
                  const std::string & dataName = dataTypeToString(static_cast<DataType>(i));
                  precice_->writeData(meshName_, dataName , vertexIds_, writeData_[i].buffer);
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
    if (!active_ ) return;
    if (!precice_) return;
    if(!precice_->isCouplingOngoing()) {
        return;
    }

        // Check if the data type is active 
    const std::string& writeDataName = dataTypeToString(static_cast<DataType>(dataType));
    if (!writeData_[static_cast<size_t>(dataType)].isActive) {
//       std::cout << "Data type " << writeDataName << " is not active, skipping write." << std::endl;
        return;
    }
    double maxTimeStepSize = precice_->getMaxTimeStepSize();
    //std::cout << "maxTimeStepSize=" << maxTimeStepSize << std::endl;
    //std::cout << "dt=" << dt << std::endl;
    //std::cout << participantName_ << " writing data " << writeDataName << std::endl;
    // Write data to preCICE
    // Extract data for coupling nodes
    extractNodeData(values, totalNodes, dataType);
    precice_->writeData(meshName_, writeDataName, vertexIds_, writeData_[dataType].buffer);
}

void PreciceCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType) {
    if (!active_) return;
    if (!precice_) return;
    if(!precice_->isCouplingOngoing()) {
        return;
    }
    const std::string& readDataName = dataTypeToString(static_cast<DataType>(dataType));
    if(!readData_[static_cast<size_t>(dataType)].isActive) {
 //       std::cout << "Data type " << readDataName << " is not active, skipping read." << std::endl;
        return;
    }
   double maxTimeStepSize = precice_->getMaxTimeStepSize();

    //std::cout << participantName_ << " reading data " << readDataName << std::endl;                           
    //std::cout << "maxTimeStepSize=" << maxTimeStepSize << std::endl;
    //std::cout << "dt=" << dt << std::endl;
    double cdt = std::min(dt, maxTimeStepSize);


    // Read data from preCICE
    precice_->readData(meshName_, readDataName, vertexIds_, cdt, readData_[dataType].buffer);
    // Inject data into global arrays
    injectNodeData(values, totalNodes, dataType);
}

void PreciceCouplingAdapter::advance(double& dt) {
    if (!active_) return;
    if (!precice_) return;
    if(!precice_->isCouplingOngoing()) {
        return;
    }
    
    double dtToUse = std::min(dt, maxTimeStepSize_);
    // Advance preCICE
    precice_->advance(dtToUse);
    // Update max time step size
    maxTimeStepSize_ = precice_->getMaxTimeStepSize();
    //std::cout << " advance " << dt << " " << dtToUse << " " << maxTimeStepSize_ << std::endl;
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
    return couplingNodeIds_.size();
}


void PreciceCouplingAdapter::extractNodeData(const double* globalValues, int totalNodes, int dataType) {
   if(!writeData_[dataType].isActive) {
        //std::cout << "Data type " << dataTypeToString(static_cast<DataType>(dataType))
        //          << " is not active, skipping extraction." << std::endl;
        return;
    }
    for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
        int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
         writeData_[dataType].buffer[i * 3] = globalValues[nodeId * 3];
         writeData_[dataType].buffer[i * 3 + 1] = globalValues[nodeId * 3 + 1];
         writeData_[dataType].buffer[i * 3 + 2] = globalValues[nodeId * 3 + 2];
    }
}

void PreciceCouplingAdapter::injectNodeData(double *globalValues, int totalNodes, int dataType)
{
    if (!readData_[dataType].isActive)
    {
        //std::cout << "Data type " << dataTypeToString(static_cast<DataType>(dataType))
        //          << " is not active, skipping injection." << std::endl;
        return;
    }
    //std::cout<<"Injection mode for data type " << dataTypeToString(static_cast<DataType>(dataType))
    //         << " is " << static_cast<size_t>(readData_[dataType].mode)<<std::endl;
    if (readData_[dataType].mode == Mode::ADD)
    {
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i)
        {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] += readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] += readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] += readData_[dataType].buffer[i * 3 + 2];
        }
    }
    else if (readData_[dataType].mode == Mode::REPLACE)
    {
        // Replacement mode
        //std::cout << "coupling nodes=" << couplingNodeIds_.size() << std::endl;
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i)
        {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] = readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] = readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] = readData_[dataType].buffer[i * 3 + 2];
            //if(i==0 && dataType == static_cast<int>(DataType::POSITIONS)) {
            //    std::cout << "Replacing data for node " << couplingNodeIds_[i] 
            //              << " with values: (" 
            //              << readData_[dataType].buffer[i * 3] << ", "
            //              << readData_[dataType].buffer[i * 3 + 1] << ", "
            //              << readData_[dataType].buffer[i * 3 + 2] << ")" << std::endl;
            //}
        }
    }
    else
    {
          std::cout << "Warning: Unknown mode for data type " << dataTypeToString(static_cast<DataType>(dataType))
                    << ", skipping injection." << std::endl;
    }
}

#endif // WITH_PRECICE

// =============================================================================
// Factory Function
// =============================================================================

CouplingAdapter* createCouplingAdapter() {
#ifdef WITH_PRECICE
    return new PreciceCouplingAdapter();
#else
    return new DummyCouplingAdapter();
#endif
}

// =============================================================================
// C Interface Implementation
// =============================================================================

extern "C" {
    void* coupling_adapter_create() {
        return createCouplingAdapter();
    }
    
    void coupling_adapter_destroy(void* adapter) {
        delete static_cast<CouplingAdapter*>(adapter);
    }
    
    int coupling_adapter_configure(void* adapter, const char* filename) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->configure(std::string(filename)) ? 1 : 0;
    }
    
    void coupling_adapter_set_nodes(void* adapter, const int* nodeIds, int numNodes) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        std::vector<int> nodes(nodeIds, nodeIds + numNodes);
        ca->setNodes(nodes);
    }
    
    int coupling_adapter_initialize(void* adapter, const double* coordinates, 
                                   int totalNodes, int mpiRank, int mpiSize) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->initialize(coordinates, totalNodes, mpiRank, mpiSize) ? 1 : 0;
    }
    
    void coupling_adapter_write_data(void* adapter, const double* values, 
                                    int totalNodes, double dt, int dataType) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        ca->writeData(values, totalNodes, dt, dataType);
    }
    
    void coupling_adapter_read_data(void* adapter, double* values, 
                                   int totalNodes, double dt, int dataType) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        ca->readData(values, totalNodes, dt, dataType);
    }
    
    void coupling_adapter_advance(void* adapter, double* dt) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        ca->advance(*dt);
    }
    
    int coupling_adapter_is_coupling_ongoing(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->isCouplingOngoing() ? 1 : 0;
    }
    
    int coupling_adapter_requires_writing_checkpoint(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->requiresWritingCheckpoint() ? 1 : 0;
    }
    
    int coupling_adapter_requires_reading_checkpoint(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->requiresReadingCheckpoint() ? 1 : 0;
    }
    
    void coupling_adapter_finalize(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        ca->finalize();
    }
    
    int coupling_adapter_is_active(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->isActive() ? 1 : 0;
    }
    
    double coupling_adapter_get_max_time_step_size(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->getMaxTimeStepSize();
    }
    
    int coupling_adapter_get_num_coupling_nodes(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->getNumberOfCouplingNodes();
    }

    int coupling_adapter_get_group_node_id(void* adapter) {
        CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
        return ca->getGroupNodeId();
    }
}
