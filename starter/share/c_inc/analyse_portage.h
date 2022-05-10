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
/* 
   Cls41l32 : Create File 
*/
#include "hardware.inc"

/*** Includes from sortie1/sortie1_c.c ***/
#if ((CPP_mach == CPP_w95) || (CPP_mach == CPP_win64_spmd) || (CPP_mach == CPP_p4win64_spmd) || (CPP_mach == CPP_p4win64) || (CPP_mach == CPP_p4win32) || (CPP_mach == CPP_wnt) || (CPP_mach == CPP_wmr))
#include <sys\types.h>
/* el41m2 +1 #define _FCALL __stdcall*/
#define _FCALL
/* Celdev032 : definie dans fctnl (pgi) ou stdlib (visual f) */
/* extern int _fmode; */
#elif 1
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#define _FCALL
#endif


/* Communication C->Fortran */
#if  ( (CPP_mach == CPP_hp9) || (CPP_mach == CPP_hp9_spmd) || (CPP_mach == CPP_hp10) || (CPP_mach == CPP_hp10_spmd) || ((CPP_mach == CPP_hp11) && (CPP_rel != 640) && (CPP_rel != 600) ) || (CPP_mach == CPP_ppc) || (CPP_mach == CPP_sp2) || (CPP_mach == CPP_rs7) || (CPP_mach == CPP_rs9) || (CPP_mach == CPP_pwr4) || (CPP_mach == CPP_pwr4_spmd ) )
#define anaderr anaderr
#define anadwar anadwar
#define angetnb angetnb
#define anend anend
#define wiout wiout
#define wistdo wistdo
/*  Cls41l31+1 */
#elif ( (CPP_mach == CPP_ymp) || (CPP_mach == CPP_ymp_spmd) || (CPP_mach == CPP_c90) || (CPP_mach == CPP_t90) || (CPP_mach == CPP_t90_i3e) || (CPP_mach == CPP_p4win64_spmd) || (CPP_mach == CPP_p4win64)  || (CPP_mach == CPP_p4win32) )
#define anaderr ANADERR
#define anadwar ANADWAR
#define angetnb ANGETNB
#define anend ANEND
#define wiout WIOUT
#define wistdo WISTDO
#else
#define anaderr anaderr_
#define anadwar anadwar_
#define angetnb angetnb_
#define anend anend_
#define wiout wiout_
#define wistdo wistdo_
#endif
