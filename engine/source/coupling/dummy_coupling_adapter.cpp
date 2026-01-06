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
#include "dummy_coupling_adapter.h"
#include <iostream>

bool DummyCouplingAdapter::configure(const std::string& configFile) {
//    std::cout << "No coupling library available - coupling disabled" << std::endl;
    return false;
}

void DummyCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    // Do nothing
}

bool DummyCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
//    std::cout << "Dummy coupling adapter initialized (no actual coupling)" << std::endl;
    return true;
}

void DummyCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType) {
    // Do nothing
}

void DummyCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType) {
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
