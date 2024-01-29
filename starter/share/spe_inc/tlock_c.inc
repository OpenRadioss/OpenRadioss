Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Cow41r2 #if CPP_mach == CPP_deca_mp || (CPP_mach == CPP_sgi6 && _MIPS_SZLONG == 64)||(CPP_mach == CPP_hp11 && CPP_rel == 640) || (CPP_mach == CPP_hp11 && CPP_rel == 600) || (CPP_mach == CPP_sx4_i3e && defined(_OPENMP)) || CPP_mach == CPP_pwr4  
#if CPP_mach == CPP_deca_mp || (CPP_mach == CPP_sgi6 && _MIPS_SZLONG == 64)||(CPP_mach == CPP_hp11 && CPP_rel == 640)
#define tlock INTEGER*8
#elif (CPP_mach == CPP_hp11 && CPP_rel == 600) || (CPP_mach == CPP_sx4_i3e && defined(_OPENMP)) || (CPP_mach == CPP_pwr4) || (CPP_mach == CPP_sun25 && CPP_rel == 580) || (CPP_mach == CPP_ppw) || CPP_mach == CPP_cx1 || CPP_mach == CPP_il
#define tlock INTEGER*8
#elif CPP_mach == CPP_ppc
#define tlock type(f_pthread_mutex_t) ::
#elif (CPP_mach == CPP_sun25 && CPP_rel == 1000)
#define tlock INTEGER*8
#elif 1
#define tlock INTEGER
#endif
 
