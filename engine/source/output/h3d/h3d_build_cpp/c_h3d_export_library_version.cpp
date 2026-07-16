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
//    
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>



#ifdef _WIN32
/* Windows includes */
#include <sys\types.h>
#elif 1
/* Linux includes */
#include <sys/types.h>
#endif

#include "h3dpublic_defs.h"
#include "h3dpublic_export.h"

#define _FCALL 

#include "h3d_values.h"

#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real double
#endif

extern "C" 
/*=================================================================*/
{

/*=================================================================*/

void c_h3d_export_library_version_(int *major_version, int *minor_version)
{
    try {   
        Hyper3DExportLibraryVersion((uint32_t*)major_version, (uint32_t*)minor_version);

    } // end of try

    catch(...)    {
        Hyper3DExportClearError(h3d_file);
        
    } // end of try

}

void _FCALL  C_H3D_EXPORT_LIBRARY_VERSION(int *major_version, int *minor_version)
{c_h3d_export_library_version_(major_version, minor_version);}

void c_h3d_export_library_version__ (int *major_version, int *minor_version)
{c_h3d_export_library_version_(major_version, minor_version);}

void c_h3d_export_library_version (int *major_version, int *minor_version)
{c_h3d_export_library_version_(major_version, minor_version);}


}
