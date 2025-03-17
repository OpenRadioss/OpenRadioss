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

CDECL void cpp_spring_read_(int *IXR, int *NIXR, int *IXR_KJ, int *NIXR_KJ,int *IPARTR, int *SUBID_SPRING, int *SKEWID)
{
    SelectionElementRead elems(g_pModelViewSDI, "/SPRING");
    int i=0;
    unsigned int partId = UINT_MAX, includeId = UINT_MAX, unitId = UINT_MAX;
    unsigned int submodelId=0;
    sdiUIntList aNodeId;
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiIdentifier identifier_skew("Skew_ID");
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
        SUBID_SPRING[i] = includeId;
// Get Elem Id
        IXR[*NIXR * i + *NIXR - 1] = elems->GetId()  ;
// Get Part Id
        IPARTR[i] = elems->GetOwnerId();
// Get Elem Connectivity IXR
        elems->GetNodeIds(aNodeId);
        for(size_t j=0; j < 3 ; ++j) IXR[*NIXR * i + j + 1] = aNodeId[j];

        if(aNodeId.size() > 3 ) 
        {
            for(size_t j=0; j < aNodeId.size()-3 ; ++j) IXR_KJ[*NIXR_KJ * i + j] = aNodeId[j+3];
        }
// Get Skew Id
        sdiValueEntity skew_loc;
        elems->GetValue(identifier_skew,val);
        val.GetValue(skew_loc);
        SKEWID[i] = skew_loc.GetId();
// next     
        i++;
        aNodeId.resize(0);
    }
}

CDECL void CPP_SPRING_READ(int *IXR, int *NIXR, int *IXR_KJ, int *NIXR_KJ, int *IPARTR, int *SUBID_SPRING, int *SKEWID)
{cpp_spring_read_ (IXR,NIXR,IXR_KJ,NIXR_KJ,IPARTR,SUBID_SPRING,SKEWID);}

CDECL void cpp_spring_read__(int *IXR, int *NIXR, int *IXR_KJ, int *NIXR_KJ, int *IPARTR, int *SUBID_SPRING, int *SKEWID)
{cpp_spring_read_ (IXR,NIXR,IXR_KJ,NIXR_KJ,IPARTR,SUBID_SPRING,SKEWID);}

CDECL void cpp_spring_read(int *IXR, int *NIXR, int *IXR_KJ, int *NIXR_KJ, int *IPARTR, int *SUBID_SPRING, int *SKEWID)
{cpp_spring_read_ (IXR,NIXR,IXR_KJ,NIXR_KJ,IPARTR,SUBID_SPRING,SKEWID);}


}
