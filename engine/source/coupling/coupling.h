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
#ifndef COUPLING_H
#define COUPLING_H

#include <string>
#include <vector>
#include <array>

// Abstract base class for coupling adapters
class CouplingAdapter {
private:
    int groupNodeId_; // the group node ID for this adapter. 
    int surfaceId_; // the surface ID for this adapter, not used yet
    int n2d_; // 0 for 3D, 1 for axisymmetric, 2 for plane strain
    int dimensions_; // dimension of the interface mesh

public:
    virtual ~CouplingAdapter() = default;
    
    // Configuration
    virtual bool configure(const std::string& configFile) = 0;
    virtual void setNodes(const std::vector<int>& nodeIds) = 0;
    
    // Initialization ; n2d = 0 for 3D, n2d = 2 for plane strain ; n2d = 1 for axi-symmetric
    virtual bool initialize(const double* coordinates, int n2d, int totalNodes, int mpiRank, int mpiSize) = 0;

    
    // Data exchange
    virtual void writeData(const double* values, int totalNodes, double dt, int dataType) = 0;
    virtual void readData(double* values, int totalNodes, double dt, int dataType) = 0;
    
    // Simulation control
    virtual void advance(double& dt) = 0;
    virtual bool isCouplingOngoing() const = 0;
    virtual bool requiresWritingCheckpoint() const {return false; };
    virtual bool requiresReadingCheckpoint() const {return false; };
    
    // Finalization
    virtual void finalize() = 0;
    
    // Getters
    virtual bool isActive() const = 0;
    virtual double getMaxTimeStepSize() const = 0;
    virtual int getNumberOfCouplingNodes() const = 0;
    int getGroupNodeId() const { return groupNodeId_; }
    void setGroupNodeId(int id) { groupNodeId_ = id; }
    int getSurfaceId() const { return surfaceId_; }
    void setSurfaceId(int id) { surfaceId_ = id; }
    virtual int getCommunicator() const { return 0; }
    virtual void get_coupled_data(int* rd, int* wd) const { 
        // Fill rd and wd fortran arrays with zeros
        for (int i = 0; i < static_cast<int>(DataType::DATA_COUNT) - 1; ++i) {
            rd[i] = 0;
            wd[i] = 0;
        }
    }
    int getN2D() const { return n2d_; }
    void setN2D(int n2d) { n2d_ = n2d; }
    int getDimensions() const { return dimensions_; }
    void setDimensions(int dimensions) { dimensions_ = dimensions; }

    // Data types that can be exchanged during the coupling process
    enum class DataType {
         NOTHING = 0,
         DISPLACEMENTS = 1,
         FORCES = 2,
         POSITIONS = 3,
         TEMPERATURE = 4,
         DATA_COUNT = 5 // Total number of data types
    };

    // Returns the number of components for a given data type (3 for vector fields, 1 for scalar fields)
    static int dataDimensions(DataType type) {
        switch (type) {
            case DataType::TEMPERATURE: return 1;
            case DataType::DISPLACEMENTS:
            case DataType::FORCES:
            case DataType::POSITIONS:
                return 3;
            default: return 0;
        }
    }
    
    // What to do with the received data
    enum class Mode {
         SKIP = 0,
         REPLACE = 1, // For positions, replace the existing data
         ADD = 2      // For forces, we add to the existing data
     };
 

     // Helper functions to convert between strings and DataType enums
    static DataType stringToDataType(const std::string& str) {
        if (str == "DISPLACEMENTS") return DataType::DISPLACEMENTS;
        if (str == "FORCES") return DataType::FORCES;
        if (str == "POSITIONS") return DataType::POSITIONS;
        if (str == "TEMPERATURE") return DataType::TEMPERATURE;
        return DataType::NOTHING;
    }
    
    static std::string dataTypeToString(DataType type) {
        switch (type) {
            case DataType::DISPLACEMENTS: return "DISPLACEMENTS";
            case DataType::FORCES: return "FORCES";
            case DataType::POSITIONS: return "POSITIONS";
            case DataType::TEMPERATURE: return "TEMPERATURE";
            case DataType::NOTHING: return "NOTHING";
            case DataType::DATA_COUNT: break; // Don't convert this
        }
        return "NOTHING";
    }
};

#endif
