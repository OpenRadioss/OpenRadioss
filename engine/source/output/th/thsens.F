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
!||    thsens       ../engine/source/output/th/thsens.F
!||--- called by ------------------------------------------------------
!||    hist2        ../engine/source/output/th/hist2.F
!||--- calls      -----------------------------------------------------
!||    wrtdes       ../engine/source/output/th/wrtdes.F
!||--- uses       -----------------------------------------------------
!||    sensor_mod   ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE THSENS(SENSOR_TAB,NSENSOR ,
     .           J1   ,J2   ,L1   ,L2   ,ITHBUF,
     .           WA   ,IFORM,SITHBUF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SITHBUF
      INTEGER ,INTENT(IN) :: IFORM,J1,J2,L1,L2,NSENSOR
      INTEGER ,DIMENSION(SITHBUF) ,INTENT(IN)  :: ITHBUF
      my_real ,DIMENSION(SWA)     ,INTENT(OUT) :: WA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,II
c=======================================================================
c     smpd : gather already done, write only by proc 0
c-----------------------------------------------------------------------
      IF (ISPMD == 0) THEN
        II = 0
        DO J=J1,J2
          DO L=L1,L2
            II=II+1
            WA(II) = ZERO
          ENDDO
        ENDDO
C
        II = 0
        DO J = J1,J2
          I = ITHBUF(J)
          DO L=L1,L2
             K = ITHBUF(L) 
             II= II+1
             IF (K == 1) WA(II) = SENSOR_TAB(I)%STATUS
          ENDDO
        ENDDO
        IF (II > 0) CALL WRTDES(WA,WA,II,IFORM,1)
      ENDIF
c-----------
      RETURN
      END
