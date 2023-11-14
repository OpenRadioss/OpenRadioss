Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  R23SENS3                      source/elements/spring/r23sens3.F
Chd|-- called by -----------
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE R23SENS3(
     1   GEO,       OFF,       SENSOR_TAB,DX,
     2   DY,        DZ,        X0,        Y0,
     3   Z0,        RX,        RY,        RZ,
     4   IGEO,      PID,       NEL,       NSENSOR)
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NSENSOR
      INTEGER IGEO(NPROPGI,*),PID(*)
      my_real :: GEO(NPROPG,*), OFF(*), DX(*),DY(*),DZ(*),X0(*),Y0(*),Z0(*),RX(*),RY(*),RZ(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ISENS,K,USENS,IFL
C-----------------------------------------------
      DO I=1,NEL
        ISENS=0
        USENS=IGEO(3,PID(I))
        IFL=IGEO(5,PID(I))
        DO K=1,NSENSOR
          IF (ABS(USENS) == SENSOR_TAB(K)%SENS_ID) ISENS=K
        ENDDO
        IF (IFL == 2) THEN
C
C                  _________           _________
C       __________/         \_________/   \______
C
C  OFF    -10.         1.       -10.       1.        
C                        \_  rupture eventuelle du ressort => 0. 
C                            (pas de reactivation possible)
C
          IF (TT > SENSOR_TAB(ISENS)%TSTART .AND. OFF(I) == -TEN) THEN
            OFF(I) = ONE
            X0(I) = X0(I) + DX(I)
            Y0(I) = Y0(I) + DY(I)
            Z0(I) = Z0(I) + DZ(I)
            DX(I) = ZERO
            DY(I) = ZERO
            DZ(I) = ZERO
            RX(I) = ZERO
            RY(I) = ZERO
            RZ(I) = ZERO    
          ELSEIF (TT < SENSOR_TAB(ISENS)%TSTART .AND. OFF(I) == ONE) THEN
            OFF(I) = ZERO    
          ENDIF
        ELSEIF (USENS > 0) THEN
C
C                  _________
C       __________/         
C
C  OFF    -10.         1.       
C                        \_  rupture eventuelle du ressort => 0. 
C                            (pas de reactivation possible)
C
            IF (TT > SENSOR_TAB(ISENS)%TSTART .AND. OFF(I) == -TEN)
     .          OFF(I) = ONE
        ELSEIF (USENS < 0) THEN
C
C        _________           
C                  \_________
C
C  OFF     1.                       
C            \_  rupture eventuelle du ressort => 0. 
C
C
            IF (TT > SENSOR_TAB(ISENS)%TSTART) OFF(I) =ZERO
        ENDIF
      ENDDO
C
      RETURN
      END
