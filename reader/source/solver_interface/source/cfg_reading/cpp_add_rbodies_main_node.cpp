//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.

#include "GlobalModelSDI.h"

#include <stdio.h>
#include <string.h>
#include <dll_settings.h>

using namespace std;

extern "C" 
{

CDECL void cpp_add_rbody_main_node_(int *addedNodeId)
{
    GlobalEntitySDIRbodiesCreateMainNode(addedNodeId);
}

CDECL void CPP_ADD_RBODY_MAIN_NODE(int *addedNodeId)
{cpp_add_rbody_main_node_ (addedNodeId);}

CDECL void cpp_add_rbody_main_node__(int *addedNodeId)
{cpp_add_rbody_main_node_ (addedNodeId);}

CDECL void cpp_add_rbody_main_node(int *addedNodeId)
{cpp_add_rbody_main_node_ (addedNodeId);}

}
