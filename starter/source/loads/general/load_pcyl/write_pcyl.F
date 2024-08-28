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
      !||====================================================================
      !||    write_pcyl      ../starter/source/loads/general/load_pcyl/write_pcyl.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit         ../starter/source/restart/ddsplit/ddsplit.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod     ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE WRITE_PCYL(LOAD_CYL ,NLOAD_CYL ,NODLOCAL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE PLOAD_CYL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "tabsiz_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NLOAD_CYL
      TYPE (PRESS_CYL_) ,DIMENSION(NLOAD_CYL) ,INTENT(IN) :: LOAD_CYL
      INTEGER , DIMENSION(*) ,INTENT(IN) :: NODLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,N1,N2,N3,N4,NSEG,LINT,IAD,OFF
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITMP
      my_real, DIMENSION(3) :: RTMP
C-----------------------------------------------
c         -> PCYL_ID
c         -> NSEG
c         -> SEGNOD(NSEG,4)  (N1,N2,NB3,N4) by segment
c         -> FRAME_ID
c         -> SENS_ID
c         -> TABLE_ID
c         -> XSCALE_R
c         -> XSCALE_T
c         -> YSCALE_P
c=======================================================================

      ! --------------
      ! loop over the /load/pcyl
      DO I=1,NLOAD_CYL        
        NSEG = LOAD_CYL(I)%NSEG ! get the number of segment 
        LINT = 5 + NSEG*4
        ALLOCATE(ITMP(LINT))
        RTMP(1) = LOAD_CYL(I)%XSCALE_R
        RTMP(2) = LOAD_CYL(I)%XSCALE_T
        RTMP(3) = LOAD_CYL(I)%YSCALE
c
        ITMP(1) = NSEG
        ITMP(2) = LOAD_CYL(I)%ID
        ITMP(3) = LOAD_CYL(I)%ISENS
        ITMP(4) = LOAD_CYL(I)%ITABLE
        ITMP(5) = LOAD_CYL(I)%IFRAME
        IAD     = 5
        ! --------------
        ! loop over the segment to convert global node id into local node id
        DO J=1,LOAD_CYL(I)%NSEG
            N1 = LOAD_CYL(I)%SEGNOD(J,1) ! get the global node id N1
            N2 = LOAD_CYL(I)%SEGNOD(J,2) ! get the global node id N2
            N3 = LOAD_CYL(I)%SEGNOD(J,3) ! get the global node id N3
            N4 = LOAD_CYL(I)%SEGNOD(J,4) ! get the global node id N4
            ITMP(IAD+1) = NODLOCAL(N1) ! global --> local for N1
            ITMP(IAD+2) = NODLOCAL(N2) ! global --> local for N2
            ITMP(IAD+3) = NODLOCAL(N3) ! global --> local for N3
            ! global --> local for N4 : for triangle, N4 = 0, for shell N4/=0
            IF (N4 == 0) THEN
                ITMP(IAD+4) = 0
            ELSE
                ITMP(IAD+4) = NODLOCAL(N4)
            ENDIF
            IAD = IAD + 4
        ENDDO
        ! --------------

        ! --------------        
        ! write the data related to the load/pcyl
        CALL WRITE_I_C(ITMP,IAD)
        CALL WRITE_DB(RTMP,3)
        ! /LOAD/PCYL option : adress for parith/on 
        CALL WRITE_I_C(LOAD_CYL(I)%SEGMENT_ADRESS,4*LOAD_CYL(I)%S_SEGMENT_ADRESS)
        ! --------------
                
        DEALLOCATE(ITMP)
      ENDDO
      ! --------------


      RETURN
      END
