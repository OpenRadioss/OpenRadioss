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


CDECL void cpp_elem_sub_tag_(int *elemType, int *TAGELEMSUB)
{
    char* config ;     

    switch (*elemType)
    {
    case 21:
        config = "/SPRING";
        break;

    case 60:
        config = "/BEAM";
        break;

    case 61:
        config = "/TRUSS";
        break;

    case 104:
        config = "/SHELL";
        break;

    case 103:
        config = "/SH3N";
        break;

    case 208:
        config = "/BRICK";
        break;

    
    default:
        config = "";
        break;
    }
 

    SelectionElementRead elems(g_pModelViewSDI,config);
    unsigned int submodelId=0;
    unsigned int includeId=0;
    int i=0;
    sdiValue val;

    while(elems.Next())
    {
// Get Submodel Id
        submodelId=0;
        includeId=0;

        HandleRead hInclude = elems->GetInclude();
        while(0 == submodelId && hInclude.IsValid())
        {
            sdiValue value;
            EntityRead include(g_pModelViewSDI, hInclude);
            include.GetValue(sdiIdentifier("shadow_submodelid"), value);
            value.GetValue(submodelId);
            includeId = include.GetId();
            if(submodelId == 0) hInclude = include.GetInclude();
        }
        TAGELEMSUB[i] = includeId;
        i++;
    }
}

CDECL void CPP_ELEM_SUB_TAG(int *elemType, int *TAGELEMSUB)
{cpp_elem_sub_tag_ (elemType, TAGELEMSUB);}

CDECL void cpp_elem_sub_tag__(int *elemType,int *TAGELEMSUB)
{cpp_elem_sub_tag_ (elemType, TAGELEMSUB);}

CDECL void cpp_elem_sub_tag(int *elemType,int *TAGELEMSUB)
{cpp_elem_sub_tag_ (elemType, TAGELEMSUB);}

}
