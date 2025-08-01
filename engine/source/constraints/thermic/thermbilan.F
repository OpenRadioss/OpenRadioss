Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    thermbilan            ../engine/source/constraints/thermic/thermbilan.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    spmd_barrier          ../engine/source/mpi/spmd_mod.F90
!||    spmd_glob_rsum_poff   ../engine/source/mpi/generic/spmd_glob_rsum_poff.F
!||--- uses       -----------------------------------------------------
!||    glob_therm_mod        ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    spmd_mod              ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE THERMBILAN(GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      use spmd_mod, only : SPMD_BARRIER
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: ARRAY(5)
!=======================================================================
      ARRAY(1) =  GLOB_THERM%HEAT_FFLUX
      ARRAY(2) =  GLOB_THERM%HEAT_MECA
      ARRAY(3) =  GLOB_THERM%HEAT_CONV
      ARRAY(4) =  GLOB_THERM%HEAT_RADIA
      ARRAY(5) =  GLOB_THERM%HEAT_STORED
C      WRITE(6,*) __FILE__,"-",ISPMD,ARRAY(5)
      CALL SPMD_BARRIER
      IF (NSPMD > 1) CALL SPMD_GLOB_RSUM_POFF(ARRAY,5)
C      WRITE(6,*) __FILE__,"+",ISPMD,ARRAY(5)
      CALL SPMD_BARRIER
C    
      IF(ISPMD == 0) THEN
        WRITE(IOUT,'(/1X,A,I10,A)')'** THERMAL ANALYSIS **'
        WRITE(IOUT,'(4X,A,G15.8)') 'IMPOSED FLUX_DENSITY HEAT............:',ARRAY(1)
        WRITE(IOUT,'(4X,A,G15.8)') 'HEAT CONVERTED FROM STRAIN ENERGY....:',ARRAY(2)
        WRITE(IOUT,'(4X,A,G15.8)') 'CONVECTION HEAT......................:',ARRAY(3)
        WRITE(IOUT,'(4X,A,G15.8)') 'RADIATION HEAT.......................:',ARRAY(4)
        WRITE(IOUT,'(4X,A,G15.8)') 'HEAT STORED..........................:',ARRAY(5)
      ENDIF
C
      RETURN
      END

