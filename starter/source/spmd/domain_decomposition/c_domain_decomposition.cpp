//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#include <iostream>
#include <vector>
#include <map>

#define _FCALL


// global vector of hash table
std::vector<std::vector<int>> clusters;

extern "C" {
        void c_prevent_decomposition_(int * clusterSize, int * elements )
        {
                const int cs = (*clusterSize);
                clusters.emplace_back(elements, elements+cs);
        }

        void c_enforce_constraints_(int * cep)
        {
                for(const auto & c : clusters)
                {
                        const int domain = cep[c[0]];
                        for(const auto & v : c)
                        {
//                                std::cout<<"element "<<v<<" moved from "<<cep[v-1]<<" to "<<domain<<std::endl;
                                cep[v-1] = domain; 
                        }
                }
                clusters.clear();
        }


        // Fortran 2 C porting
        void _FCALL C_PREVENT_DECOMPOSITION(int * clusterSize, int * elements)
        {
                c_prevent_decomposition_(clusterSize,elements);
        }
        void c_prvent_decomposition__(int * clusterSize, int * elements)
        {
                c_prevent_decomposition_(clusterSize,elements);
        }
        void c_prevent_decomposition(int * clusterSize,int * elements)
        {
                c_prevent_decomposition_(clusterSize,elements);
        }

        void _FCALL C_ENFORCE_CONSTRAINTS(int * cep)
        {
                c_enforce_constraints_(cep);
        }
        void c_enforce_constraints__(int * cep)
        {
                c_enforce_constraints_(cep);
        }
        void c_enforce_constraints(int * cep)
        {
                c_enforce_constraints_(cep);
        }

}
