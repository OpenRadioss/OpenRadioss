//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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


#include "hardware.inc"

/*** Includes from sortie1/sortie1_c.c ***/
#ifdef _WIN64

#include <sys\types.h>
#define _FCALL

#elif 1

#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#define _FCALL

#endif


/*********************************************************************
 *
 * C->Fortran calls : routines prototyping
 *
 ********************************************************************/
#ifdef _WIN64

   #define wistdo WISTDO 
   #define wiout WIOUT
   #define anend ANEND
   #define anaderr ANADERR
   #define anadwar ANADWAR
   #define angetnb ANGETNB

   int _FCALL WISTDO(int * length, int * tabint);
   int _FCALL WIOUT(int * length, int * tabint);
   int _FCALL ANEND();
   int _FCALL ANADERR();
   int _FCALL ANADWAR();
   int _FCALL ANGETNB(int * nberr, int * nbwar);
#else

   #define wistdo wistdo_
   #define wiout wiout_
   #define anaderr anaderr_
   #define anend anend_
   #define anadwar anadwar_
   #define angetnb angetnb_

   int wistdo_(int * length, int * tabint);
   int wiout_(int * length, int * tabint);
   int anend_();
   int anaderr_();
   int anadwar_();
   int angetnb(int * nberr, int * nbwar);

#endif

