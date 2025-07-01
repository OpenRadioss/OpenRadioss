#ifndef ADAPTER_H
#define ADAPTER_H
#include <string>
#include <vector>
#include <memory>
#include <array>

// Abstract base class for coupling adapters
class CouplingAdapter {
private:
    int groupNodeId_; // the group node ID for this adapter. 
    int surfaceId_; // the surface ID for this adapter, not used yet

public:
    virtual ~CouplingAdapter() = default;
    
    // Configuration
    virtual bool configure(const std::string& configFile) = 0;
    virtual void setNodes(const std::vector<int>& nodeIds) = 0;
    
    // Initialization
    virtual bool initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) = 0;
    
    // Data exchange
    virtual void writeData(const double* values, int totalNodes, double dt, int dataType) = 0;
    virtual void readData(double* values, int totalNodes, double dt, int dataType) = 0;
    
    // Simulation control
    virtual void advance(double& dt) = 0;
    virtual bool isCouplingOngoing() const = 0;
    virtual bool requiresWritingCheckpoint() const = 0;
    virtual bool requiresReadingCheckpoint() const = 0;
    
    // Finalization
    virtual void finalize() = 0;
    
    // Getters
    virtual bool isActive() const = 0;
    virtual double getMaxTimeStepSize() const = 0;
    virtual int getNumberOfCouplingNodes() const = 0;
    int getGroupNodeId() const {
        return groupNodeId_;
    }
    void setGroupNodeId(int id) {
        groupNodeId_ = id;
    }

    // data that can be exchanged during the coupling process, can be extended                                                   
    enum class DataType {
         NOTHING = 0,
         DISPLACEMENTS = 1,
         FORCES = 2,
         POSITIONS = 3,
         DATA_COUNT = 4 // Total number of data types
    };
    // What to do with the recieved data:
    enum class Mode {
         SKIP = 0,
         REPLACE = 1, // For positions, replace the existing data
         ADD = 2 // For forces, we add to the existing data
     };

     // helpper functions to convert between strings and DataType enums
    static DataType stringToDataType(const std::string& str) {
        // Order by most common usage for better branch prediction
        if (str == "DISPLACEMENTS") return DataType::DISPLACEMENTS;
        if (str == "FORCES") return DataType::FORCES;
        if (str == "POSITIONS") return DataType::POSITIONS;
        return DataType::NOTHING;
    }
    static std::string dataTypeToString(DataType type) {
        switch (type) {
            case DataType::DISPLACEMENTS: return "DISPLACEMENTS";
            case DataType::FORCES: return "FORCES";
            case DataType::POSITIONS: return "POSITIONS";
            case DataType::NOTHING: return "NOTHING";
            case DataType::DATA_COUNT: break; // Don't convert this
        }
        return "NOTHING";
    }
    

};

// Dummy adapter for when no coupling library is available
class DummyCouplingAdapter : public CouplingAdapter {
public:
    bool configure(const std::string& configFile) override;
    void setNodes(const std::vector<int>& nodeIds) override;
    bool initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) override;
    void writeData(const double* values, int totalNodes, double dt, int dataType) override;
    void readData(double* values, int totalNodes, double dt, int dataType) override;
    void advance(double& dt) override;
    bool isCouplingOngoing() const override;
    bool requiresWritingCheckpoint() const override;
    bool requiresReadingCheckpoint() const override;
    void finalize() override;
    bool isActive() const override;
    double getMaxTimeStepSize() const override;
    int getNumberOfCouplingNodes() const override;
};

// =========================================================
// preCICE coupling adapter
// =========================================================
#ifdef WITH_PRECICE
#include "precice/precice.hpp"

// preCICE implementation of coupling adapter
class PreciceCouplingAdapter : public CouplingAdapter {
public:
    PreciceCouplingAdapter();
    ~PreciceCouplingAdapter() override;
    
    // Implement abstract interface
    bool configure(const std::string& configFile) override;
    void setNodes(const std::vector<int>& nodeIds) override;
    bool initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) override;
    void writeData(const double* values, int totalNodes, double dt, int dataType) override;
    void readData(double* values, int totalNodes, double dt, int dataType) override;
    void advance(double& dt) override;
    bool isCouplingOngoing() const override;
    bool requiresWritingCheckpoint() const override;
    bool requiresReadingCheckpoint() const override;
    void finalize() override;
    bool isActive() const override;
    double getMaxTimeStepSize() const override;
    int getNumberOfCouplingNodes() const override;

    struct CouplingData {
      bool isActive = false; // Whether this data type is active
      Mode mode = Mode::SKIP; // Mode for this data type
      std::vector<double> buffer;
    };

private:
    // Configuration data
    bool active_;
    std::string participantName_;
    std::string configFile_;
    std::string meshName_;

    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> readData_; // Store coupling data for different types
    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> writeData_; // Store coupling data for different types
    // Coupling data
    std::vector<int> couplingNodeIds_;  // Radioss node IDs
    std::vector<int> vertexIds_;        // preCICE vertex IDs
    double maxTimeStepSize_;
    std::unique_ptr<precice::Participant> precice_;
    // Helper functions
    void extractNodeData(const double* globalValues, int totalNodes, int dataType);
    void injectNodeData(double* globalValues, int totalNodes, int dataType);

};

#endif // WITH_PRECICE

// Factory function to create the appropriate adapter
CouplingAdapter* createCouplingAdapter();


// C interface for Fortran binding
// Should be kept generic enough to work with any coupling adapter (CWIPI, preCICE, etc.)
extern "C" {
    // Object management
    void* coupling_adapter_create();
    void coupling_adapter_destroy(void* adapter);
    
    // Configuration
    int coupling_adapter_configure(void* adapter, const char* filename);
    void coupling_adapter_set_nodes(void* adapter, const int* nodeIds, int numNodes);
    
    // Initialization
    int coupling_adapter_initialize(void* adapter, const double* coordinates, 
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
}

#endif
