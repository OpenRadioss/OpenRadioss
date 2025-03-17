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

CDECL void cpp_elem_count_(char *elemType, int *s_elemType, int *nbElems, int *isDyna)
{ 
// Char fortran -> c++
    char *cname;
    int cname_len;
    int i;
    bool isSlash = true;
    cname_len = *s_elemType + 1;

    if (strncmp(elemType,"/",1) != 0) 
    {
        isSlash = false;
        cname_len++;
    }
    cname=(char*) malloc(sizeof(char)*cname_len);
   
    if (isSlash == false ) 
    { 
        cname[0] = '/';
        for(i=0;i<*s_elemType;i++)  cname[i+1] = elemType[i];
    } 
    else
    { 
        for(i=0;i<*s_elemType;i++)  cname[i] = elemType[i];
    } 
    cname[cname_len - 1]='\0';

//    Element::Config config ;     

/*    if (strncmp(cname,"/SPRING",7) == 0 )
    {    
        config = Element::HW_ELEMENT_CONFIG_SPRING;    
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }   
    if (strncmp(cname,"/XELEM",6) == 0 && *isDyna == 0)   
    {    
        config = Element::HW_ELEMENT_CONFIG_XELEM;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }     
    else if (strncmp(cname,"/BEAM",5) == 0 )   
    {    
        config = Element::HW_ELEMENT_CONFIG_BAR;    
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }      
    else if (strncmp(cname,"/TRUSS",6) == 0 )   
    {    
        config = Element::HW_ELEMENT_CONFIG_ROD;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }   
    else if (strncmp(cname,"/SH3N",5) == 0 )   
    {    
        config = Element::HW_ELEMENT_CONFIG_TRIA3;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }     
    else if (strncmp(cname,"/SHELL",6) == 0 )   
    {    
        config = Element::HW_ELEMENT_CONFIG_QUAD4;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }
    else if (strncmp(cname,"/TETRA4",7) == 0 )   
    {    
        config = Element::HW_ELEMENT_CONFIG_TETRA4;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }      
    else if (strncmp(cname,"/PENTA6",7) == 0  && *isDyna == 0)   
    {    
        config = Element::HW_ELEMENT_CONFIG_PENTA6;  
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();

        printf("%s nb elem penta6 %d","/PENTA6",*nbElems);
        fflush(stdout);
    }   
*/  
    if (strncmp(cname,"/BRICK20",8) == 0  && *isDyna == 0)   
    {    
        SelectionElementRead elems(g_pModelViewSDI, "/BRIC20"); 
        *nbElems = elems.Count();
    }/*
    else if (strncmp(cname,"/BRICK",6) == 0 )   
    {    
        config = Element::HW_ELEMENT_CONFIG_HEX8;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    }      
    else if (strncmp(cname,"/TETRA10",8) == 0  && *isDyna == 0)   
    {    
        config = Element::HW_ELEMENT_CONFIG_TETRA10;   
        SelectionElementRead elems(g_pModelView,FilterElementConfig(config));
        *nbElems = elems.Count();
    } */
    else   
    {    
        SelectionElementRead elems(g_pModelViewSDI, cname); 
        *nbElems = elems.Count();
    } 
     

//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_INVALID;
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_MASS;       
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_PLOT;       
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_WELD;       
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_RIGID;         
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_SPRING3;    
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_SPRING4;       
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_GAP;        
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_BAR3;       
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_JOINT;      
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_RIGIDLINK;  
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_RBE3;       
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_ICE;         
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_TRIA6;      
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_QUAD8;      
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_PENTA5;      
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_PENTA13;    
//    if (strncmp(cname,"",1) == 0 )   config = Element::HW_ELEMENT_CONFIG_PENTA15;    
    
}
CDECL void CPP_ELEM_COUNT(char *elemType, int *s_elemType, int *nbElems, int *isDyna)
{cpp_elem_count_ (elemType,s_elemType,nbElems,isDyna);}

CDECL void cpp_elem_count__ (char *elemType, int *s_elemType, int *nbElems, int *isDyna)
{cpp_elem_count_ (elemType,s_elemType,nbElems,isDyna);}

CDECL void cpp_elem_count (char *elemType, int *s_elemType, int *nbElems, int *isDyna)
{cpp_elem_count_ (elemType,s_elemType,nbElems,isDyna);}

}


