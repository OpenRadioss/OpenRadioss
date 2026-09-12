Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    eig_mod      ../engine/share/modules/eig_mod.F
!||--- called by ------------------------------------------------------
!||    eigrrest     ../engine/source/output/restart/rdresb.F
!||    leceig       ../engine/source/input/leceig.F
!||    lecfxinp     ../engine/source/input/lecfxinp.F
!||    resol_head   ../engine/source/engine/resol_head.F
!||    restalloc    ../engine/source/output/restart/arralloc.F
!||====================================================================
      MODULE EIG_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: EIGIPM

      INTEGER, DIMENSION(:), ALLOCATABLE :: EIGIBUF
       
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: EIGRPM
     
C Public eigenmode result hand-off.  The matrix solver populates these arrays
C and the animation writer consumes them before the /EIG control path exits
C RESOL.  SLEPc vectors remain in backend-owned distributed storage and only
C the active Nbloc batch is materialized for constraint recovery.
      INTEGER :: PUBLIC_EIG_NMODE = 0
      INTEGER :: PUBLIC_EIG_NNODE = 0
      INTEGER, DIMENSION(:), ALLOCATABLE :: PUBLIC_EIG_NODE
      my_real, DIMENSION(:), ALLOCATABLE :: PUBLIC_EIG_FREQ
      my_real, DIMENSION(:,:), ALLOCATABLE :: PUBLIC_EIG_REDUCED
      INTEGER*8 :: PUBLIC_EIG_SOLVER_HANDLE = 0
      INTEGER :: PUBLIC_EIG_EXPORT_NLOCAL = 0
      INTEGER, DIMENSION(:), ALLOCATABLE :: PUBLIC_EIG_EXPORT_ROW
      LOGICAL :: PUBLIC_EIG_STREAMED = .FALSE.
      LOGICAL :: PUBLIC_EIG_READY = .FALSE.

      CONTAINS

      SUBROUTINE CLEAR_PUBLIC_EIG_RESULTS()
        IF (ALLOCATED(PUBLIC_EIG_NODE)) DEALLOCATE(PUBLIC_EIG_NODE)
        IF (ALLOCATED(PUBLIC_EIG_FREQ)) DEALLOCATE(PUBLIC_EIG_FREQ)
        IF (ALLOCATED(PUBLIC_EIG_REDUCED))
     .    DEALLOCATE(PUBLIC_EIG_REDUCED)
        IF (ALLOCATED(PUBLIC_EIG_EXPORT_ROW))
     .    DEALLOCATE(PUBLIC_EIG_EXPORT_ROW)
        PUBLIC_EIG_NMODE=0
        PUBLIC_EIG_NNODE=0
        PUBLIC_EIG_EXPORT_NLOCAL=0
        PUBLIC_EIG_STREAMED=.FALSE.
        PUBLIC_EIG_READY=.FALSE.
      END SUBROUTINE CLEAR_PUBLIC_EIG_RESULTS

C-----------------------------------------------
      END MODULE EIG_MOD
