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
#ifndef CWIPI_COUPLING_ADAPTER_H
#define CWIPI_COUPLING_ADAPTER_H

#include "coupling.h"
#include <memory>
#include <array>

#ifdef WITH_CWIPI
#include "cwipi.h"
#include <mpi.h>

class CwipiCouplingAdapter : public CouplingAdapter
{
public:
    CwipiCouplingAdapter();
    ~CwipiCouplingAdapter() override;

    // Implement abstract interface
    bool configure(const std::string &configFile) override;
    void setNodes(const std::vector<int> &nodeIds) override;
    void setMesh(const int *elem_node_offsets, const int *elem_node_indices, int num_elements);
    bool initialize(const double *coordinates, int n2d, int totalNodes, int mpiRank, int mpiSize) override;
    void writeData(const double *values, int totalNodes, double dt, int dataType) override;
    void readData(double *values, int totalNodes, double dt, int dataType) override;
    void advance(double &dt) override;
    bool isCouplingOngoing() const override;
    bool requiresWritingCheckpoint() const override;
    bool requiresReadingCheckpoint() const override;
    void finalize() override;
    bool isActive() const override;
    double getMaxTimeStepSize() const override;
    int getNumberOfCouplingNodes() const override;

    struct CouplingData
    {
        bool isActive = false;  // Whether this data type is active
        Mode mode = Mode::SKIP; // Mode for this data type
        std::vector<double> buffer;
        int sendRequest = -1; // Track async send request
        int recvRequest = -1; // Track async receive request
    };

private:
    // Configuration data
    bool active_;
    std::string applicationName_;
    std::string coupledAppName_;
    std::string couplingName_;
    std::string exchangeName_;
    int dimension_;
    double tolerance_;
    int order_;

    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> readData_;
    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> writeData_;

    // Coupling data
    std::vector<int> couplingNodeIds_;
    double maxTimeStepSize_;
    bool initialized_;

    // CWIPI specific data
    MPI_Comm localComm_;

    // Mesh connectivity storage
    std::vector<int> eltsConnecPointer_;
    std::vector<int> eltsConnec_;
    int numElements_;

    // Helper functions
    void extractNodeData(const double *globalValues, int totalNodes, int dataType);
    void injectNodeData(double *globalValues, int totalNodes, int dataType);
    std::string getFieldName(DataType type);
    int getTag(DataType type);
    // override getCommuncator to return localComm_
    int getCommunicator() const override
    {
        int FortranComm = MPI_Comm_c2f(localComm_);
        return FortranComm;
    }

public:
    void get_coupled_data(int *rd, int *wd) const
    {
        // rd is the readData_ status (1: active)
        // wd is the writeData_status
        for (size_t i = 0; i < 3; ++i)
        {
            wd[i] = 0;
            rd[i] = 0;
        }
        for (size_t i = 1; i < static_cast<size_t>(DataType::DATA_COUNT); ++i)
        {
            if (readData_[i].isActive)
            {
                rd[i - 1] = 1;
            }
            if (writeData_[i].isActive)
            {
                wd[i - 1] = 1;
            }
        }
    }
};

#endif
#endif
