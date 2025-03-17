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

CDECL void cpp_sphcel_read_(int *KXSP, int *NISP, int *IPARTSP, int *SUBID_SPH, int *TYPE, double *MASS, int *UID_SPHCEL)
{
    SelectionElementRead elems(g_pModelViewSDI, "/SPHCEL");
    int i=0;
    unsigned int partId = UINT_MAX, includeId = UINT_MAX, unitId = UINT_MAX;
    unsigned int submodelId=0;
    sdiUIntList aNodeId;
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiIdentifier identifier_flag("Flag");
    sdiIdentifier identifier_mass("MASS");
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
            SUBID_SPH[i] = includeId;
// Get Elem Id
            KXSP[*NISP * i + *NISP - 1] = elems->GetId();
// Get Part Id
            IPARTSP[i] = elems->GetOwnerId();
// Get Uid
            elems->GetValue(identifier_unitid,val);
            sdiValueEntity unit;
            val.GetValue(unit);
            UID_SPHCEL[i] = unit.GetId();
// Get Elem Connectivity (NodeId = ElemId)
            KXSP[*NISP * i + 2] = elems->GetId();;
            KXSP[*NISP * i + 1] = 1;
// Get Elem type Value
            elems->GetValue(identifier_flag,val);
            val.GetValue(TYPE[i]);
// Get Elem MASS Value
            elems->GetValue(identifier_mass,val);
            val.GetValue(MASS[i]);
// next      
            i++;
        }
} 

CDECL void CPP_SPHCEL_READ(int *KXSP, int *NISP, int *IPARTSP, int *SUBID_SPH, int *TYPE, double *MASS, int *UID_SPHCEL)
{cpp_sphcel_read_ (KXSP,NISP,IPARTSP,SUBID_SPH,TYPE,MASS,UID_SPHCEL);}

CDECL void cpp_sphcel_read__(int *KXSP, int *NISP, int *IPARTSP, int *SUBID_SPH, int *TYPE, double *MASS, int *UID_SPHCEL)
{cpp_sphcel_read_ (KXSP,NISP,IPARTSP,SUBID_SPH,TYPE,MASS,UID_SPHCEL);}

CDECL void cpp_sphcel_read(int *KXSP, int *NISP, int *IPARTSP, int *SUBID_SPH, int *TYPE, double *MASS, int *UID_SPHCEL)
{cpp_sphcel_read_ (KXSP,NISP,IPARTSP,SUBID_SPH,TYPE,MASS,UID_SPHCEL);}



}
