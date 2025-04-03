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

CDECL void cpp_node_read_(int *ITAB, double *X, double *W, int *SUBID_NOD, int *UID_NOD)
{
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    unsigned int includeId=0;
    unsigned int submodelId=0;
    int i=0;
    sdiValue unitIdval;
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiIdentifier search_value_identifier("Search_Value");
    sdiValue value;
    sdiValue search_value_val;
    bool is_cnode;
//
// Nodes loop
//
    while(nodes.Next())
    {
        is_cnode = nodes->GetValue(search_value_identifier, search_value_val);
        if(!is_cnode)
        {
// Get Submodel Id
            submodelId=0;
            includeId=0;
            HandleRead hInclude(nodes->GetInclude());
            while(0 == submodelId && hInclude.IsValid())
            {
                 EntityRead parent(g_pModelViewSDI, hInclude);
                 parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
                 value.GetValue(submodelId);
                 if(submodelId == 0) hInclude = parent.GetInclude();
            }
            includeId = (int) hInclude.GetId(g_pModelViewSDI);
            SUBID_NOD[i] = includeId;
// Get Uid
            nodes->GetValue(identifier_unitid, unitIdval);
            sdiValueEntity unit;
            unitIdval.GetValue(unit);
            UID_NOD[i] = unit.GetId();
// Get Id
            ITAB[i] = nodes->GetId() ;
// Get coords
        sdiTriple pos = nodes->GetPosition();
            X[3 * i] = pos.X();
            X[3 * i + 1] = pos.Y();
            X[3 * i + 2] = pos.Z();
// next        
            i++;
        }
    }
}

CDECL void CPP_NODE_READ(int *ITAB, double *X, double *W, int *SUBID_NOD, int *UID_NOD)
{cpp_node_read_ (ITAB, X, W, SUBID_NOD, UID_NOD);}

CDECL void cpp_node_read__(int *ITAB, double *X, double *W, int *SUBID_NOD, int *UID_NOD)
{cpp_node_read_ (ITAB, X, W, SUBID_NOD, UID_NOD);}

CDECL void c_node_read(int *ITAB, double *X, double *W, int *SUBID_NOD, int *UID_NOD)
{cpp_node_read_ (ITAB, X, W, SUBID_NOD, UID_NOD);}


CDECL void cpp_nodes_read_(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    unsigned int includeId=0;
    unsigned int submodelId=0;
    int cptNodes=0;
    int cptCnodes=0;
    double dmerge=0.;
    sdiValue unitIdval;
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiIdentifier search_value_identifier("Search_Value");
    sdiValue value;
    sdiValue search_value_val;
    bool is_cnode;
//
// Nodes loop
//
    while(nodes.Next())
    {
        is_cnode = nodes->GetValue(search_value_identifier, search_value_val);
// Get Submodel Id
        submodelId=0;
        includeId=0;
        HandleRead hInclude(nodes->GetInclude());
        while(0 == submodelId && hInclude.IsValid())
        {
             EntityRead parent(g_pModelViewSDI, hInclude);
             parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
             value.GetValue(submodelId);
             if(submodelId == 0) hInclude = parent.GetInclude();
        }
        includeId = (int) hInclude.GetId(g_pModelViewSDI);
        SUBID_NOD[cptNodes] = includeId;
// Get Uid
        nodes->GetValue(identifier_unitid, unitIdval);
        sdiValueEntity unit;
        unitIdval.GetValue(unit);
        UID_NOD[cptNodes] = unit.GetId();
// Get Id
        ITAB[cptNodes] = nodes->GetId() ;
// Get coords
        sdiTriple pos = nodes->GetPosition();
        X[3 * cptNodes] = pos.X();
        X[3 * cptNodes + 1] = pos.Y();
        X[3 * cptNodes + 2] = pos.Z();
// next      
        cptNodes = cptNodes + 1;
        if(is_cnode)
        {   
            search_value_val.GetValue(dmerge);
            CMERGE[cptCnodes] = dmerge ;
            cptCnodes = cptCnodes + 1;
        }
    }
}

CDECL void CPP_NODES_READ(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{cpp_nodes_read_ (ITAB, X, CMERGE, SUBID_NOD, UID_NOD);}

CDECL void cpp_nodes_read__(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{cpp_nodes_read_ (ITAB, X, CMERGE, SUBID_NOD, UID_NOD);}

CDECL void c_nodes_read(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{cpp_nodes_read_ (ITAB, X, CMERGE, SUBID_NOD, UID_NOD);}



CDECL void cpp_node_id_read_(int *ITAB, int *SUBID_NOD)
{
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    unsigned int includeId=0;
    unsigned int submodelId=0;
    int i=0;
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiValue value;
//
// Nodes loop
//
    while(nodes.Next())
    {
// Get Submodel Id
        submodelId=0;
        includeId=0;
        HandleRead hInclude(nodes->GetInclude());
        while(0 == submodelId && hInclude.IsValid())
        {
             EntityRead parent(g_pModelViewSDI, hInclude);
             parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
             value.GetValue(submodelId);
             if(submodelId == 0) hInclude = parent.GetInclude();
        }
        includeId = (int) hInclude.GetId(g_pModelViewSDI);
        SUBID_NOD[i] = includeId;
// Get Id
        ITAB[i] = nodes->GetId() ;
// next        
        i++;
    }
}

CDECL void CPP_NODE_ID_READ(int *ITAB, int *SUBID_NOD)
{cpp_node_id_read_ (ITAB,SUBID_NOD);}

CDECL void cpp_node_id_read__(int *ITAB, int *SUBID_NOD)
{cpp_node_id_read_ (ITAB,SUBID_NOD);}

CDECL void cpp_node_id_read(int *ITAB, int *SUBID_NOD)
{cpp_node_id_read_ (ITAB,SUBID_NOD);}




CDECL void cpp_cnode_read_(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    unsigned int includeId=0;
    unsigned int submodelId=0;
    int cptCnodes=0;
    double dmerge=0.;
    sdiValue unitIdval;
    sdiIdentifier identifier_unitid("unitid");
    sdiIdentifier identifier_shadow_submodelid("shadow_submodelid");
    sdiIdentifier search_value_identifier("Search_Value");
    sdiValue value;
    sdiValue search_value_val;
    bool is_cnode;
//
// Nodes loop
//
    while(nodes.Next())
    {
        is_cnode = nodes->GetValue(search_value_identifier, search_value_val);

        if(is_cnode)
        {   
// Get Submodel Id
            submodelId=0;
            includeId=0;
            HandleRead hInclude(nodes->GetInclude());
            while(0 == submodelId && hInclude.IsValid())
            {
                 EntityRead parent(g_pModelViewSDI, hInclude);
                 parent.GetValue(sdiIdentifier("shadow_submodelid"), value);
                 value.GetValue(submodelId);
                 if(submodelId == 0) hInclude = parent.GetInclude();
            }
            includeId = (int) hInclude.GetId(g_pModelViewSDI);
            SUBID_NOD[cptCnodes] = includeId;
// Get Uid
            nodes->GetValue(identifier_unitid, unitIdval);
            sdiValueEntity unit;
            unitIdval.GetValue(unit);
            UID_NOD[cptCnodes] = unit.GetId();
// Get Id
            ITAB[cptCnodes] = nodes->GetId() ;
// Get coords
            sdiTriple pos = nodes->GetPosition();
            X[3 * cptCnodes] = pos.X();
            X[3 * cptCnodes + 1] = pos.Y();
            X[3 * cptCnodes + 2] = pos.Z();
// next      
            search_value_val.GetValue(dmerge);
            CMERGE[cptCnodes] = dmerge ;
            cptCnodes = cptCnodes + 1;
        }
    }
}

CDECL void CPP_CNODE_READ(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{cpp_cnode_read_ (ITAB, X, CMERGE, SUBID_NOD, UID_NOD);}

CDECL void cpp_cnode_read__(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{cpp_cnode_read_ (ITAB, X, CMERGE, SUBID_NOD, UID_NOD);}

CDECL void c_cnode_read(int *ITAB, double *X, double *CMERGE, int *SUBID_NOD, int *UID_NOD)
{cpp_cnode_read_ (ITAB, X, CMERGE, SUBID_NOD, UID_NOD);}




}
