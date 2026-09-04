//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 2026 Siemens
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Simcenter Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
//Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
//Copyright>        commercial version may interest you:
//Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.

#include <stdlib.h>

#define _FCALL

#include "h3dpublic_defs.h"
#include "h3dpublic_export.h"
#include "h3d_values.h"

extern "C"
{

/*=================================================================*/
/*        C_H3D_ADD_STRING                                         */
/*=================================================================*/

void c_h3d_add_string_(char *name, int *size)
{
    char *cname = (char*) malloc(sizeof(char) * (*size + 1));
    for (int i = 0; i < *size; i++) cname[i] = name[i];
    cname[*size] = '\0';

    H3D_ID string_id = H3D_NULL_ID;
    rc = Hyper3DAddString(h3d_file, cname, &string_id);
    free(cname);
}

void _FCALL C_H3D_ADD_STRING(char *name, int *size)
{c_h3d_add_string_ (name, size);}

void c_h3d_add_string__ (char *name, int *size)
{c_h3d_add_string_ (name, size);}

void c_h3d_add_string (char *name, int *size)
{c_h3d_add_string_ (name, size);}

}