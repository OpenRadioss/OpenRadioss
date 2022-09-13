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
Chd|====================================================================
Chd|  IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|-- called by -----------
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I21FOR3                       source/interfaces/int21/i21for3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|        I3FOR2                        source/interfaces/inter2d/i3for2.F
Chd|        I3FOR3                        source/interfaces/inter3d/i3for3.F
Chd|        I5FOR3                        source/interfaces/inter3d/i5for3.F
Chd|        I7FOR3                        source/interfaces/int07/i7for3.F
Chd|        I8FOR3                        source/interfaces/inter3d/i8for3.F
Chd|        SPMD_FIADD20F_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD25E_POFF            source/mpi/interfaces/spmd_fiadd25e_poff.F
Chd|        SPMD_FIADD25E_PON             source/mpi/interfaces/spmd_fiadd25e_pon.F
Chd|        SPMD_FIADD_POFF               source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD_PON                source/mpi/interfaces/spmd_i7tool.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IBCOFF(IBC,ICODT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBC, ICODT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J1, L, J2, J3, K1, K2, K3, ICODTN
C-----------------------------------------------
C
      J1=ICODT/4
      L=ICODT-4*J1
      J2=L/2
      J3=L-2*J2
      K1=IBC/4
      L=IBC-4*K1
      K2=L/2
      K3=L-2*K2
C     CALL MY_LOCK
C      ICODT=4*MAX0(0,J1-K1)+2*MAX0(0,J2-K2)+MAX0(0,J3-K3)
C     CALL MY_FREE
      ICODTN = 4*MAX0(0,J1-K1)+2*MAX0(0,J2-K2)+MAX0(0,J3-K3)
      IF (ICODTN/=ICODT) THEN
#include "lockon.inc"
        ICODT = ICODTN
#include "lockoff.inc"
C var pour le SPMD
        IEXICODT = 1
      ENDIF
      RETURN
      END
