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

CDECL void cpp_tetra4_read_(int *IXS, int *NIXS, int *NUMBRICK, int *IPARTS, int *SUBID_SOL)
{
    SelectionElementRead elems(g_pModelViewSDI, "/TETRA4");
    unsigned int partId = UINT_MAX, includeId = UINT_MAX, unitId = UINT_MAX;

    int i=*NUMBRICK;
    unsigned int submodelId=0;
    sdiUIntList aNodeId;
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiValue val;
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
        SUBID_SOL[i] = includeId;
        elems->GetValue(identifier_unitid,val);
        sdiValueEntity unit;
        val.GetValue(unit);
// Get Elem Id
        IXS[*NIXS * i + *NIXS - 1] = elems->GetId();
// Get Part Id
        IPARTS[i] = elems->GetOwnerId();
// Get Elem Connectivity
        elems->GetNodeIds(aNodeId);
        for(size_t j=0; j < 4 ; ++j) IXS[*NIXS * i + j + 1] = aNodeId[j];
// next     
        i++; 
        aNodeId.resize(0);
    }
}


CDECL void CPP_TETRA4_READ(int *IXS, int *NIXS, int *NUMBRICK, int *IPARTS, int *SUBID_SOL)
{cpp_tetra4_read_ (IXS,NIXS,NUMBRICK,IPARTS,SUBID_SOL);}

CDECL void cpp_tetra4_read__(int *IXS, int *NIXS, int *NUMBRICK, int *IPARTS, int *SUBID_SOL)
{cpp_tetra4_read_ (IXS,NIXS,NUMBRICK,IPARTS,SUBID_SOL);}

CDECL void cpp_tetra4_read(int *IXS, int *NIXS, int *NUMBRICK, int *IPARTS, int *SUBID_SOL)
{cpp_tetra4_read_ (IXS,NIXS,NUMBRICK,IPARTS,SUBID_SOL);}


}
