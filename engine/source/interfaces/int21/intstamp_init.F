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
Chd|====================================================================
Chd|  INTSTAMP_INIT                 source/interfaces/int21/intstamp_init.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
      SUBROUTINE INTSTAMP_INIT(INTSTAMP,ICODR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "intstamp_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODR(*)
C     REAL
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, NN, MSR, IROT, K
C     REAL
      my_real
     .   VI(3)
C-----------------------------------------------
c      IF(NCYCLE==0)THEN
c        DO NN=1,NINTSTAMP
c          MSR=INTSTAMP(NN)%MSR
c          IF(ICODR(MSR) < 7) THEN
c            INTSTAMP(NN)%IROT=1
c          ELSE
c            INTSTAMP(NN)%IROT=0
c          END IF
c        END DO
c      END IF
C-----
      DO NN=1,NINTSTAMP
C
        INTSTAMP(NN)%FC(1)=ZERO
        INTSTAMP(NN)%FC(2)=ZERO
        INTSTAMP(NN)%FC(3)=ZERO
        INTSTAMP(NN)%STF=ZERO
C
        INTSTAMP(NN)%MC(1)=ZERO
        INTSTAMP(NN)%MC(2)=ZERO
        INTSTAMP(NN)%MC(3)=ZERO
        INTSTAMP(NN)%STR=ZERO
C
        IF(IPARIT/=0)THEN
          DO J=1,3
           DO K=1,6
           INTSTAMP(NN)%FC6(K,J)=ZERO
           INTSTAMP(NN)%MC6(K,J)=ZERO
           END DO
          END DO
          DO K=1,6
          INTSTAMP(NN)%ST6(K) =ZERO
          INTSTAMP(NN)%STR6(K)=ZERO
          END DO
        END IF
C
      ENDDO
C
      RETURN
      END
