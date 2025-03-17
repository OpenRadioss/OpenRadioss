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
#include <float.h>
using namespace sdi;
using namespace std;

extern "C" 
{

CDECL void cpp_beam_read_(int *IXP, int *NIXP, int *IPARTP, int *SUBID_BEAM, double *VX, double *VY, double *VZ)
{
    SelectionElementRead elems(g_pModelViewSDI, "/BEAM");
    int i=0;
    unsigned int partId = UINT_MAX, includeId = UINT_MAX, unitId = UINT_MAX;
    unsigned int submodelId=0;
    sdiUIntList aNodeId;
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiValue val;
    sdiIdentifier identifier_vx("Vx");
    sdiIdentifier identifier_vy("Vy");
    sdiIdentifier identifier_vz("Vz");
    double dvx, dvy, dvz;
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
        SUBID_BEAM[i] = includeId;
// Get Elem Id
        IXP[*NIXP * i + *NIXP - 1] = elems->GetId()  ;
// Get Part Id
        IPARTP[i] = elems->GetOwnerId();
// Get Elem Connectivity
        elems->GetNodeIds(aNodeId);
        for(size_t j=0; j < 3 ; ++j) IXP[*NIXP * i + j + 1] = aNodeId[j];
// Get Elem Vx Vy Vz
        sdiValue valV(dvx);
        elems->GetValue(identifier_vx,valV);
        valV.GetValue(dvx);
        elems->GetValue(identifier_vy,valV);
        valV.GetValue(dvy);
        elems->GetValue(identifier_vz,valV);
        valV.GetValue(dvz);

        
        if(DBL_MAX == dvx) dvx = 0;
        if(DBL_MAX == dvy) dvy = 0;
        if(DBL_MAX == dvz) dvz = 0;

        VX[i] = dvx;
        VY[i] = dvy;
        VZ[i] = dvz;
        // next
        aNodeId.resize(0);   
        i++;
    }
}

CDECL void CPP_BEAM_READ(int *IXP, int *NIXP, int *IPARTP, int *SUBID_BEAM, double *VX, double *VY, double *VZ)
{cpp_beam_read_ (IXP,NIXP,IPARTP,SUBID_BEAM,VX,VY,VZ);}

CDECL void cpp_beam_read__(int *IXP, int *NIXP, int *IPARTP, int *SUBID_BEAM, double *VX, double *VY, double *VZ)
{cpp_beam_read_ (IXP,NIXP,IPARTP,SUBID_BEAM,VX,VY,VZ);}

CDECL void cpp_beam_read(int *IXP, int *NIXP, int *IPARTP, int *SUBID_BEAM, double *VX, double *VY, double *VZ)
{cpp_beam_read_ (IXP,NIXP,IPARTP,SUBID_BEAM,VX,VY,VZ);}

}


