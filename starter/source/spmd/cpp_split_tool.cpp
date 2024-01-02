//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
#define _FCALL
#include <list>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <deque>
#include <fstream>
#include <iostream>
#include <limits>
#include <cfloat>
#include <set>
#include <map>


#ifndef NDEBUG
#define NDEBUG
#endif
#include <assert.h>


//Type definition
//
// Partition[domain][global Id] = local Id 
typedef std::vector<std::unordered_map<int,int>> Partition;

//vector of candidates
typedef std::vector<std::pair<int,int>> Candidates;

//vector of pointers to pairs
typedef std::vector<std::pair<int,int>*> VectOfCandPtr; 

typedef std::vector<std::vector<int>> Remotes;

void build_partition(int *cep, int *size, const int nspmd, Partition & partition)
{ // partition element according to the CEP

// create a vector of size nspmd initialized at 0
        std::vector<int> count(nspmd,0);
        partition.reserve(nspmd);
        for(int i = 0 ; i < *size ; i++)
        {
                assert( cep[i] >= 0);
                assert( cep[i] < nspmd);
                partition[cep[i]][i]=count[cep[i]];
                count[cep[i]]++; 
        }
}

extern "C" {


//Restriction : secondary objects must belong to one and only one processor
//
void cpp_count_candidates(int *nbCand, int *sizeM, int *cepM, int *localIdM, int *candM, int *sizeS, int *cepS, int *localIdS, int *candS,  int * nspmd, int * secondaryRemoteCount, int * localIdx)
{
        // nbCand: 
        //      IN: number of candidates
        // sizeM: 
        //      IN: number of Main objects
        // cepM: 
        //      IN: domain of main objects, starts at 0
        // LocalIdM
        //      IN: local ID of the main object on its owner domain
        // candM
        //      IN: global  id of main object
        //      OUT: id of main object local to domain
        // sizeS: number of Main objects
        // cepS: 
        //      IN: secondary of main objects, starts at 0
        // LocalIdM
        //      IN: local ID of the main object on its owner domain
        // candS
        //      IN: global  id of secondary object, starts at 1
        //      OUT: id of secondary object: 
        //              if CandS[i] > 0 => CandS[i] is the local ID on domain CepM[i]
        //              if CandS[i] < 0 => -CandS[i] is the ID in the remote (FI) structure
        //              indexes starts at 1
        // nspmd:
        //      IN: number of domains
        // secondaryRemoteCount:
        //      OUT: array of size nspmd*nspmd that contains the number of remotes per pairs of processors
        // localIdx: 
        //      OUT: array of size nbCand, localIdx[i] = local index of the CandS[i] in the domain CepS[i] 
        //
        // Description:
        // split based on the domains of the main and the secondary element


        const int nspmd2 = (*nspmd)*(*nspmd);
        std::vector<Candidates> candidates(nspmd2, std::vector<std::pair<int,int>>());
        Remotes secondaryRemotes(nspmd2, std::vector<int>());

        for(int i = 0 ; i < *nbCand ; i++)
        {
                // Fortran indexes start at 1
                const int currentM = candM[i] - 1;
                const int currentS = candS[i] - 1;
                assert(currentM < *sizeM);
                assert(currentM >= 0 );
                assert(currentS < *sizeS);
                assert(currentS >= 0 );
                const int domainM = cepM[currentM];
                const int domainS = cepS[currentS];
                const int oneD = domainM * (*nspmd) + domainS;

//                const auto itrS = partitionS[domainS].find(currentS);
//                const auto itrM = partitionM[domainM].find(currentM);
//                const int localIdS = itrS->second;
//                const int localIdM = itrM->second;
//                const int localIdS = std::distance(partitionM[domainM].begin(),itrM) + 1; //idx starts at 1 (back to Fortran)
//                const int localIdM = std::distance(partitionS[domainS].begin(),itrS) + 1;

                //DEBUG 
                assert(domainM >= 0);
                assert(domainM < *nspmd);
                assert(domainS >=0);
                assert(domainS < *nspmd);
                if(domainS != domainM)
                { 
                      //  candidates[oneD].push_back(std::make_pair(localIdM[currentM],localIdS[currentS]));
                        candidates[oneD].push_back(std::make_pair(i,localIdS[currentS]));

                }
                //replace candidates global id by local ids
                candM[i] = localIdM[currentM];
                candS[i] = localIdS[currentS];    // if remote, will be replace by id in FI
                localIdx[i] = localIdS[currentS]; // keep local ID on owner domain

        }

        for(int domainM = 0 ; domainM < *nspmd ; domainM++)
        {
                int offset = 0 ;
                for(int domainS = 0 ; domainS < *nspmd ; domainS++)
                {
                        // 2D to 1D index
                        const int oneD = domainM * (*nspmd) + domainS;
                        secondaryRemoteCount[oneD] = 0;
                        secondaryRemotes[oneD] = {} ;
                        if(domainS != domainM)
                        {
                                //  SecondaryToCand[ global secondary id] => vector of pointers to candidates that 
                                //  have that secondary object
                                std::map<int,VectOfCandPtr> secondaryToCand; 

                                // Sets automatically exclude duplicates
                                std::set<int> secondarySet;

                                // Find all remote secondary objects
                                // They may be duplicated,multiple candidate pairs having the same secondary objects
                                // SecondaryToCand keep tracks of those candidates pairs
                                for(auto & c : candidates[oneD])
                                {
                                        auto it = secondarySet.insert(c.second);
                                        secondaryToCand[c.second].push_back(&(c));
                                }
                                // convert set into vector 
                                secondaryRemotes[oneD].assign(secondarySet.begin(),secondarySet.end());
                                // sort the vector - not needed since set are aumatically sorted
                                // would be needed if set was replaced by unordered_set
                                // std::sort(secondaryRemotes[oneD].begin(),secondaryRemotes[oneD].end();
                                secondaryRemoteCount[oneD] = secondaryRemotes[oneD].size();
                                int idxInFi = offset; // index in Fi structure
                
                                // For remote candidate, convert:
                                // id of the secondary object (local to the owner domain)  => index in the FI structure 
                                for(auto & sc : secondaryToCand)
                                {
                                        idxInFi++;
                                        for(auto & ptr : sc.second)
                                        {
                                                // ptr = ptr to the pair of candidates
                                                (*ptr).second = -( idxInFi); // index starts at 1 
                                             //   std::cout<<"candS["<<(*ptr).first<<"]="<< -(idxInFi) << std::endl;   
                                                candS[(*ptr).first] = -(idxInFi);
                                        }
                                }

                        } else
                        {
                                secondaryRemoteCount[oneD] = 0;
                        }
                        offset+= secondaryRemoteCount[oneD];
                }
        }

//        for(int domainM = 0 ; domainM < *nspmd ; domainM++)
//        {
//                for(int domainS = 0 ; domainS < *nspmd ; domainS++)
//                {
//                        const int oneD = domainM * (*nspmd) + domainS;
//                        std::cout<<secondaryRemoteCount[oneD]<<" ";
//                }
//                std::cout<<std::endl;
//        }
}


void _FCALL  CPP_COUNT_CANDIDATES(int *nbCand, int *sizeM, int *cepM, int *localIdM,  int *candM, int *sizeS, int *cepS,int *localIdS,  int *candS, int * nspmd, int * secondaryRemoteCount, int * localIdx)
{
     cpp_count_candidates(nbCand, sizeM, cepM, localIdM, candM, sizeS, cepS, localIdS,  candS,  nspmd, secondaryRemoteCount, localIdx);
}
void cpp_count_candidates__(int *nbCand, int *sizeM, int *cepM, int *localIdM,  int *candM, int *sizeS, int *cepS,int *localIdS,  int *candS, int * nspmd, int * secondaryRemoteCount, int * localIdx)
{
     cpp_count_candidates(nbCand, sizeM, cepM, localIdM, candM, sizeS, cepS, localIdS,  candS,  nspmd, secondaryRemoteCount, localIdx);
}
void cpp_count_candidates_(int *nbCand, int *sizeM, int *cepM, int *localIdM,  int *candM, int *sizeS, int *cepS,int *localIdS,  int *candS, int * nspmd, int * secondaryRemoteCount, int * localIdx)
{
     cpp_count_candidates(nbCand, sizeM, cepM, localIdM, candM, sizeS, cepS, localIdS,  candS,  nspmd, secondaryRemoteCount, localIdx);
}

} //extern C

