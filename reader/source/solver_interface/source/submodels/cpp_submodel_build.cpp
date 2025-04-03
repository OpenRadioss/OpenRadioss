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
#include <dll_settings.h>

#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;

extern "C" 
{

CDECL void cpp_submodel_build_(int *IFATHER, int *NOSUBMOD, int *LEVEL, int *OFFSETS, int *UID_SUB)
{
    unsigned int submodelId;
    unsigned int fatherId;
    unsigned int fatherIndex;
    unsigned int IncludeId;
    unsigned int offset=0;
    int i = 0;
    sdiIdentifier identifier_unitid("unitid");
    sdiValue unitIdval;

    //SelectionIncludeFileRead entities(g_pModelViewSDI);

    SelectionRead entities(g_pModelViewSDI, "#include");

//
// Elem entities
//
    while(entities.Next())
    {
        submodelId=0;
        fatherId=0;
        fatherIndex=0;
        IncludeId=0;
// Get Id
        sdiValue value;
        entities->GetValue(sdiIdentifier("shadow_submodelid"), value);
        value.GetValue(submodelId);
// Get OFFSETS
        NOSUBMOD[i] = submodelId;
        HandleRead hSubmodel;
        HandleRead hInclude = entities->GetHandle();
        entities->GetEntityHandle(sdiIdentifier("shadow_submodel"), hSubmodel);

        EntityRead rSubmodel(g_pModelViewSDI, hSubmodel);
        EntityRead rInclude(g_pModelViewSDI, hInclude);

        rSubmodel.GetValue(sdiIdentifier("alloptionoffset"), value);
        value.GetValue(OFFSETS[7*i]);
        rSubmodel.GetValue(sdiIdentifier("nodeoffset"), value);
        value.GetValue(OFFSETS[7*i+1]);
        rSubmodel.GetValue(sdiIdentifier("elementoffset"), value);
        value.GetValue(OFFSETS[7*i+2]);
        rSubmodel.GetValue(sdiIdentifier("componentoffset"), value);
        value.GetValue(OFFSETS[7*i+3]);
        rSubmodel.GetValue(sdiIdentifier("materialoffset"), value);
        value.GetValue(OFFSETS[7*i+4]);
        rSubmodel.GetValue(sdiIdentifier("propertyoffset"), value);
        value.GetValue(OFFSETS[7*i+5]);
//         submodel.GetValue(sdiIdentifier("submodeloffset"), value);
//         value.GetValue(OFFSETS[7*i+6]);
// Get Uid
        rSubmodel.GetValue(sdiIdentifier("unitid"), unitIdval);
        sdiValueEntity unit;
        unitIdval.GetValue(unit);
        UID_SUB[i] = unit.GetId();

// Get father
        fatherId = 0;
        fatherIndex = 0;
        hInclude = rInclude.GetInclude();
        if(hInclude.IsValid())
        {
            EntityRead parent(g_pModelViewSDI, hInclude);
            parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
            value.GetValue(fatherId);
            hInclude = parent.GetInclude();
            fatherIndex = parent.GetId();

            while(0 == fatherId  && hInclude.IsValid())
            {
                EntityRead parent1(g_pModelViewSDI, hInclude);
                parent1.GetValue(sdiIdentifier("shadow_submodelid"), value);
                value.GetValue(fatherId);
                fatherIndex = parent1.GetId();
                hInclude = parent1.GetInclude();
            }
        }
        IFATHER[i] = fatherIndex;
        i++;
     }

}
CDECL void CPP_SUBMODEL_BUILD(int *IFATHER, int *NOSUBMOD, int *LEVEL, int *OFFSETS, int *UID_SUB)
{cpp_submodel_build_ (IFATHER,NOSUBMOD,LEVEL,OFFSETS,UID_SUB);}

CDECL void cpp_submodel_build__(int *IFATHER, int *NOSUBMOD, int *LEVEL, int *OFFSETS, int *UID_SUB)
{cpp_submodel_build_ (IFATHER,NOSUBMOD,LEVEL,OFFSETS,UID_SUB);}

CDECL void cpp_submodel_build(int *IFATHER, int *NOSUBMOD, int *LEVEL, int *OFFSETS, int *UID_SUB)
{cpp_submodel_build_ (IFATHER,NOSUBMOD,LEVEL,OFFSETS,UID_SUB);}


}
