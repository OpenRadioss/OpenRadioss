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
#ifndef DUMMY_COUPLING_ADAPTER_H
#define DUMMY_COUPLING_ADAPTER_H

#include "coupling.h"

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

#endif // DUMMY_COUPLING_ADAPTER_H
