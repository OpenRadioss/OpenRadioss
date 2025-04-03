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

#include <dll_settings.h>

#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;


extern "C" 
{


CDECL void cpp_node_count_(int *nbNodes)
{
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    *nbNodes = nodes.Count();
}
CDECL void CPP_NODE_COUNT(int *nbNodes)
{cpp_node_count_ (nbNodes);}

CDECL void cpp_node_count__ (int *nbNodes)
{cpp_node_count_ (nbNodes);}

CDECL void CPP_node_count (int *nbNodes)
{cpp_node_count_ (nbNodes);}



CDECL void cpp_nodes_count_(int *nbNodes, int *nbCnodes)
{
    sdiValue search_value_val;
    SelectionNodeRead nodes(g_pModelViewSDI, "/NODE");
    sdiIdentifier search_value_identifier("Search_Value");
    bool is_cnode;
    int cpt = 0;
    int cpt1 = 0;
    while(nodes.Next())
    {
        is_cnode = nodes->GetValue(search_value_identifier, search_value_val);
        if(!is_cnode)
        {
            cpt = cpt + 1;
        }
        else
        {
            cpt1 = cpt1 + 1;
        }
    }
    *nbNodes = cpt;
    *nbCnodes = cpt1;
}
CDECL void CPP_NODES_COUNT(int *nbNodes, int *nbCnodes)
{cpp_nodes_count_ (nbNodes,nbCnodes);}

CDECL void cpp_nodes_count__ (int *nbNodes, int *nbCnodes)
{cpp_nodes_count_ (nbNodes,nbCnodes);}

CDECL void cpp_nodes_count (int *nbNodes, int *nbCnodes)
{cpp_nodes_count_ (nbNodes,nbCnodes);}



}
