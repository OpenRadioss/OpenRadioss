/*Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/

#include "GlobalModelSDI.h"

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <map>
#include <dll_settings.h>

#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;

extern "C" 
{

CDECL void cpp_sh3n_read_(int *IXTG, int *NIXTG, int *IPARTTG, double *ANGLE, double *THK, int *SUBID_SH3N, int *UID_SH3N)
{
    SelectionElementRead elems(g_pModelViewSDI, "/SH3N");
    int i=0;
    unsigned int partId = UINT_MAX, includeId = UINT_MAX, unitId = UINT_MAX;
    unsigned int submodelId=0;
    sdiUIntList aNodeId;
    sdiIdentifier identifier_phi("PHI_Z");
    sdiIdentifier identifier_thick("Thick");
    sdiIdentifier identifier("PHI_Z");
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiValue val;


    sdiValue phival;
    sdiValue unitIdval;
//
// Elem loop
//
    while(elems.Next())
    {
// Get Submodel Id
            submodelId=0;
            includeId=0;
            HandleRead hInclude(elems->GetInclude());
            while(0 == submodelId && hInclude.IsValid())
            {
                 EntityRead parent(g_pModelViewSDI, hInclude);
                 parent.GetValue(sdiIdentifier("shadow_submodelid"), val);
                 val.GetValue(submodelId);
                 if(submodelId == 0) hInclude = parent.GetInclude();
            }
            includeId = (int) hInclude.GetId(g_pModelViewSDI);
            SUBID_SH3N[i] = includeId;
// Get Elem Id
            IXTG[*NIXTG * i + *NIXTG - 1] = elems->GetId();
// Get Uid
            elems->GetValue(identifier_unitid,val);
            sdiValueEntity unit;
            val.GetValue(unit);
            UID_SH3N[i] = unit.GetId();
// Get Part Id
            IPARTTG[i] = elems->GetOwnerId();
// Get Elem Connectivity
            elems->GetNodeIds(aNodeId);
            for(size_t j=0; j < 3 ; ++j) IXTG[*NIXTG * i + j + 1] = aNodeId[j];
// Get Elem Phi Value
            elems->GetValue(identifier_phi,val);
            double phi = 0;
            val.GetValue(ANGLE[i]);
// Get Elem Thickness Value
            double thickness = 0;
            elems->GetValue(identifier_thick,val);
            val.GetValue(THK[i]);
// next     
            aNodeId.resize(0);   
            i++;
        }
}

CDECL void CPP_SH3N_READ(int *IXTG, int *NIXTG, int *IPARTTG, double *ANGLE, double *THK, int *SUBID_SH3N, int *UID_SH3N)
{cpp_sh3n_read_ (IXTG,NIXTG,IPARTTG,ANGLE,THK,SUBID_SH3N,UID_SH3N);}

CDECL void cpp_sh3n_read__(int *IXTG, int *NIXTG, int *IPARTTG, double *ANGLE, double *THK, int *SUBID_SH3N, int *UID_SH3N)
{cpp_sh3n_read_ (IXTG,NIXTG,IPARTTG,ANGLE,THK,SUBID_SH3N,UID_SH3N);}

CDECL void cpp_sh3n_read(int *IXTG, int *NIXTG, int *IPARTTG, double *ANGLE, double *THK, int *SUBID_SH3N, int *UID_SH3N)
{cpp_sh3n_read_ (IXTG,NIXTG,IPARTTG,ANGLE,THK,SUBID_SH3N,UID_SH3N);}

}


