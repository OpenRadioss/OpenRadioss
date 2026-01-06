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

    // Data types that can be exchanged during the coupling process
    enum class DataType {
         NOTHING = 0,
         DISPLACEMENTS = 1,
         FORCES = 2,
         POSITIONS = 3,
         DATA_COUNT = 4 // Total number of data types
    };
    
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

#endif
