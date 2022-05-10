Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
       INTEGER MVSIZ
       PARAMETER (MVSIZ = 512)

Cow50c3 #include "hardware.inc"
Cow50c3       INTEGER MVSIZ
Cow50c3 #if CPP_mach == CPP_sgi6 || CPP_mach == CPP_sgi6_spmd
Cow50c3       PARAMETER (MVSIZ = 128)
Cow50c3 #elif CPP_mach == CPP_cvx
Cow50c3       PARAMETER (MVSIZ = 256)
Cow50c3 #elif CPP_mach == CPP_ymp || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_c90 || CPP_mach == CPP_t90 || CPP_mach == CPP_t90_i3e
Cow50c3       PARAMETER (MVSIZ = 512)
Cow50c3 #elif CPP_mach == CPP_sx4 || CPP_mach == CPP_vpp || CPP_mach == CPP_vpp_spmd
Cow50c3       PARAMETER (MVSIZ = 1024)
Cow41r1+++
Cow50c3 #elif  CPP_mach == CPP_sx4_i3e
Cow50c3 
Cow50c3 #if CPP_rel == 60 
Cow50c3       PARAMETER (MVSIZ = 4095)
Cow50c3 #elif 1
Cow50c3       PARAMETER (MVSIZ = 2047)
Cow50c3 #endif
Cow50c3 #elif CPP_mach == CPP_sx5_spmd
Cow50c3       PARAMETER (MVSIZ = 4095)
Cow50c3 Cow41r1 #elif  CPP_mach == CPP_sx4_i3e || CPP_mach == CPP_sx5_spmd
Cow50c3 Cow41r1      PARAMETER (MVSIZ = 2047)
Cow50c3 
Cow50c3 #elif CPP_mach == CPP_hp11 || CPP_mach == CPP_hp11_spmd
Cow50c3 Cow41r3+++
Cow50c3 #if CPP_rel == 600
Cow50c3       PARAMETER (MVSIZ = 256)
Cow50c3 #elif 1
Cow50c3       PARAMETER (MVSIZ = 64)
Cow50c3 #endif
Cow50c3 Cow41r3---
Cow50c3 #elif CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd
Cow50c3       PARAMETER (MVSIZ = 32)
Cow50c3 #elif CPP_mach == CPP_ppw_spmd || CPP_mach == CPP_ppw || CPP_mach == CPP_linux64_spmd
Cow50c3       PARAMETER (MVSIZ = 64)
Cow50c3 Cow41r5 #elif CPP_mach == CPP_ppw_spmd
Cow50c3 #elif CPP_mach == CPP_il_spmd || CPP_mach == CPP_il
Cow50c3       PARAMETER (MVSIZ = 256)
Cow50c3 #elif CPP_mach == CPP_cx1_spmd || CPP_mach == CPP_cx1
Cow50c3       PARAMETER (MVSIZ = 511)
Cow50c3 #elif 1
Cow50c3 C else ne marche pas sur sgi
Cow50c3       PARAMETER (MVSIZ = 32)
Cow50c3 #endif
