//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
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
std::vector<std::vector<int>> rbody;


/* ***************************************************************
c_prevent_decomposition_rbody : create a list of element
input : * rbodysize : size of the list (type = integer)
        * elements : list of element (type = integer)
*************************************************************** */
extern "C" {
        void c_prevent_decomposition_rbody_(int * rbodysize, int * elements )
        {
                const int cs = (*rbodysize);
                rbody.emplace_back(elements, elements+cs);
        }

/* ***************************************************************
c_enforce_constraints_rbody : stick a list of element on a given processor
         2 strategies : * stick all the elements of the rigid body on the processor P
                          with the lowest total number of element
                        * stick all the elements of the rigid body on the processor P
                          with the lowest total number of rigid body
                             
input : * cep : connectivity processor / element (type = integer)
        * nspmd : number of MPI processor (type = integer)
        * nrby : number of rigid body (type = integer)
*************************************************************** */
        void c_enforce_constraints_rbody_(int * cep, int  * nspmd, int * nrby )
        {


            int const number_proc = *nspmd ;

            std::vector<int> compteur(*nspmd,0);
            std::vector<int> compteur_2(*nspmd,0);

                for(const auto & c : rbody)
                {

                        for(int i=0 ; i<number_proc ; i++)
                        {
                            compteur_2[i] = 0 ;
                        }

                        for(const auto & v : c)
                        {
                            compteur_2[ cep[v-1]-1 ] += 1 ;
                        }

                        int id_proc, number_element,number_rby ;
                        id_proc = -1 ;
                        number_element= -1 ;
                        number_rby= *nrby + 1  ;
//  ----------------------------------------------------
// stick all the elements of the rigid body on the processor P
// with the lowest total number of element

/*                        for(int i=0 ; i<number_proc ; i++){
                            if(number_element<compteur_2[i]){
                                id_proc = i+1 ;
                                number_element=compteur_2[i] ;
                            }
                        }
//  ----------------------------------------------------
*/
//  ----------------------------------------------------
// stick all the elements of the rigid body on the processor P
// with the lowest total number of rigid body
                        // find id_proc : the processor ID with the lowest number of /RBODY
                        for(int i=0 ; i<number_proc ; i++){
                            if(compteur_2[i]>0 && compteur[i]<number_rby){
                                id_proc = i+1 ;
                                number_rby = compteur[i] ;
                            }
                        }
                        // stick the element v-1 to id_proc
                        int domain = id_proc;
                        compteur[domain-1] =  compteur[domain-1] + 1 ;
                        for(const auto & v : c)
                        {
                                cep[v-1] = domain; 
                        }
//  ----------------------------------------------------
                }
                rbody.clear();
        }


        // Fortran 2 C porting
        void _FCALL C_PREVENT_DECOMPOSITION_RBODY(int * rbodysize, int * elements)
        {
                c_prevent_decomposition_rbody_(rbodysize,elements);
        }
        void c_prvent_decomposition_rbody__(int * rbodysize, int * elements)
        {
                c_prevent_decomposition_rbody_(rbodysize,elements);
        }
        void c_prevent_decomposition_rbody(int * rbodysize,int * elements)
        {
                c_prevent_decomposition_rbody_(rbodysize,elements);
        }

        void _FCALL C_ENFORCE_CONSTRAINTS_RBODY(int * cep, int * nspmd, int * nrby)
        {
                c_enforce_constraints_rbody_(cep,nspmd,nrby);
        }
        void c_enforce_constraints_rbody__(int * cep , int * nspmd, int * nrby )
        {
                c_enforce_constraints_rbody_(cep,nspmd,nrby);
        }
        void c_enforce_constraints_rbody(int * cep, int * nspmd, int * nrby)
        {
                c_enforce_constraints_rbody_(cep,nspmd,nrby);
        }

}
