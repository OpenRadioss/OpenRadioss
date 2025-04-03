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

CDECL void cpp_node_sub_tag_(int *TAGNODSUB)
{
    SelectionRead nodes(g_pModelViewSDI,"/NODE");
    unsigned int submodelId=0;
    unsigned int includeId=0;
    int i=0;
//
// Nodes loop
//
    while(nodes.Next())
    {
// Get Submodel Id
        sdiValue value;
        HandleRead hInclude(nodes->GetInclude());
        unsigned int submodelId = 0;
        includeId = hInclude.GetId(g_pModelViewSDI);
        hInclude.GetValue(g_pModelViewSDI, sdiIdentifier("shadow_submodelid"), value);
        value.GetValue(submodelId);
        while(0 == submodelId && hInclude.IsValid())
        {
            EntityRead include(g_pModelViewSDI, hInclude);
            hInclude = include.GetInclude();
            includeId = hInclude.GetId(g_pModelViewSDI);
            hInclude.GetValue(g_pModelViewSDI, sdiIdentifier("shadow_submodelid"), value);
            value.GetValue(submodelId);
        }
        TAGNODSUB[i] = includeId;
        i++;
    }
}

CDECL void CPP_NODE_SUB_TAG(int *TAGNODSUB)
{cpp_node_sub_tag_ (TAGNODSUB);}

CDECL void c_node_sub_tag__(int *TAGNODSUB)
{cpp_node_sub_tag_ (TAGNODSUB);}

CDECL void c_node_sub_tag(int *TAGNODSUB)
{cpp_node_sub_tag_ (TAGNODSUB);}


CDECL void cpp_node_sub_tag_dyna_(int *TAGNODSUB, int *IDNOD)
{
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    unsigned int submodelId=0;
    unsigned int includeId=0;
    int i=0;
//
// Nodes loop
//
    while(nodes.Next())
    {
// Get Submodel IdValue value;
        sdiValue value;
        HandleRead hInclude(nodes->GetInclude());
        unsigned int submodelId = 0;
        includeId = hInclude.GetId(g_pModelViewSDI);
        hInclude.GetValue(g_pModelViewSDI, sdiIdentifier("shadow_submodelid"), value);
        value.GetValue(submodelId);
        while(0 == submodelId && hInclude.IsValid())
        {
            EntityRead include(g_pModelViewSDI, hInclude);
            hInclude = include.GetInclude();
            includeId = hInclude.GetId(g_pModelViewSDI);
            hInclude.GetValue(g_pModelViewSDI, sdiIdentifier("shadow_submodelid"), value);
            value.GetValue(submodelId);
        }
        TAGNODSUB[i] = includeId;
        IDNOD[i] = nodes->GetId();
        i++;
    }
}

CDECL void CPP_NODE_SUB_TAG_DYNA(int *TAGNODSUB, int *IDNOD)
{cpp_node_sub_tag_dyna_ (TAGNODSUB,IDNOD);}

CDECL void c_node_sub_tag_dyna__(int *TAGNODSUB, int *IDNOD)
{cpp_node_sub_tag_dyna_ (TAGNODSUB,IDNOD);}

CDECL void c_node_sub_tag_dyna(int *TAGNODSUB, int *IDNOD)
{cpp_node_sub_tag_dyna_ (TAGNODSUB,IDNOD);}


}
