      !Copyright>        OpenRadioss
      !Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
      !Copyright>    
      !Copyright>        This program is free software: you can redistribute it and/or modify
      !Copyright>        it under the terms of the GNU Affero General Public License as published by
      !Copyright>        the Free Software Foundation, either version 3 of the License, or
      !Copyright>        (at your option) any later version.
      !Copyright>    
      !Copyright>        This program is distributed in the hope that it will be useful,
      !Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
      !Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      !Copyright>        GNU Affero General Public License for more details.
      !Copyright>    
      !Copyright>        You should have received a copy of the GNU Affero General Public License
      !Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
      !Copyright>    
      !Copyright>    
      !Copyright>        Commercial Alternative: Altair Radioss Software 
      !Copyright>    
      !Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
      !Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
      !Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
#include "hardware.inc"
      INTEGER MVSIZ
#if CPP_mach == CPP_sgi6 || CPP_mach == CPP_sgi6_spmd 
      PARAMETER (MVSIZ = 129)
#elif CPP_mach == CPP_cvx
      PARAMETER (MVSIZ = 256)
#elif CPP_mach == CPP_ymp || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_c90 || CPP_mach == CPP_t90 || CPP_mach == CPP_t90_i3e
      PARAMETER (MVSIZ = 513)
#elif CPP_mach == CPP_sx4 || CPP_mach == CPP_vpp || CPP_mach == CPP_vpp_spmd
      PARAMETER (MVSIZ = 1025)

#elif  CPP_mach == CPP_sx4_i3e 
#if CPP_rel == 60
      PARAMETER (MVSIZ = 4095)
#elif 1 
      PARAMETER (MVSIZ = 2047)
#endif
#elif CPP_mach == CPP_sx5_spmd
      PARAMETER (MVSIZ = 4095)
#elif CPP_mach == CPP_hp11 || CPP_mach == CPP_hp11_spmd

#if CPP_rel == 600
      PARAMETER (MVSIZ = 257)
#elif 1
       PARAMETER (MVSIZ = 65)
#endif

#elif CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd
      PARAMETER (MVSIZ = 33)
#elif CPP_mach == CPP_ppw || CPP_mach == CPP_ppw_spmd  || CPP_mach == CPP_p4win32 || CPP_mach == CPP_p4linux932
      PARAMETER (MVSIZ = 65)
#elif CPP_mach == CPP_linux64_spmd || CPP_mach == CPP_p4linux964_spmd || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd|| CPP_mach == CPP_p4win64 || CPP_mach == CPP_linux964 || CPP_mach == CPP_p4linux964 || CPP_mach == CPP_macosx64
      PARAMETER (MVSIZ = 129)
#elif CPP_mach == CPP_il_spmd || CPP_mach == CPP_il
      PARAMETER (MVSIZ = 257)
#elif CPP_mach == CPP_cx1 || CPP_mach == CPP_cx1_spmd
      PARAMETER (MVSIZ = 511)
#elif CPP_mach==CPP_sun25 && CPP_rel==1000 || CPP_mach == CPP_sol10x64_spmd
      PARAMETER (MVSIZ = 129)
#elif 1
C (elif 1) remplace (else) qui ne marche pas sur sgi6 avec des (elif)
      PARAMETER (MVSIZ = 33)
#endif
