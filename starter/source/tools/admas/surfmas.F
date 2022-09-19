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
Chd|  SURFMAS                       source/tools/admas/surfmas.F  
Chd|-- called by -----------
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SURFMAS(MS,IBUFN,ITY,AMASU,X,ID,ADDMAS,
     .                   ADMID,TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUFN(*),ITY,ID,ADMID
C     REAL
      my_real
     .   MS(*),AMASU,X(3,*),ADDMAS
      CHARACTER TITR*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K
C     REAL
      my_real
     .   X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   X21,Y21,Z21,X31,Y31,Z31,X42,Y42,Z42,
     .   X32,Y32,Z32,E3X,E3Y,E3Z,SMASS,SUM,AREA,
     .   A2,B2,C2,AA,BB,CC,ANG1,ANG2,ANG3
C=======================================================================
      IF(ITY /= 7)THEN
C----------------------------------------------
C     MASSES ELEMENTS /4
C----------------------------------------------
        X1=X(1,IBUFN(1))
        Y1=X(2,IBUFN(1))
        Z1=X(3,IBUFN(1))
        X2=X(1,IBUFN(2))
        Y2=X(2,IBUFN(2))
        Z2=X(3,IBUFN(2))
        X3=X(1,IBUFN(3))
        Y3=X(2,IBUFN(3))
        Z3=X(3,IBUFN(3))
        X4=X(1,IBUFN(4))
        Y4=X(2,IBUFN(4))
        Z4=X(3,IBUFN(4))
C
        X21=X2-X1
        Y21=Y2-Y1
        Z21=Z2-Z1
        X31=X3-X1
        Y31=Y3-Y1
        Z31=Z3-Z1
        X42=X4-X2
        Y42=Y4-Y2
        Z42=Z4-Z2
C
        E3X=Y31*Z42-Z31*Y42
        E3Y=Z31*X42-X31*Z42
        E3Z=X31*Y42-Y31*X42
        SUM=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        AREA=HALF*SUM
C
C        EMS = RHO * THK * AREA
C
        SMASS = AMASU * AREA  ! THK is already included in the input AMASU ( = MASS/AREA = RHO * THK)
C nodal mass
        MS(IBUFN(1))=MS(IBUFN(1)) + SMASS * FOURTH
        MS(IBUFN(2))=MS(IBUFN(2)) + SMASS * FOURTH
        MS(IBUFN(3))=MS(IBUFN(3)) + SMASS * FOURTH
        MS(IBUFN(4))=MS(IBUFN(4)) + SMASS * FOURTH
C
        ADDMAS = ADDMAS + SMASS
C
      ELSE IF(ITY == 7)THEN
C----------------------------------------------
C     MASSES ELEMENTS * ANGLE
C----------------------------------------------
        X1=X(1,IBUFN(1))
        Y1=X(2,IBUFN(1))
        Z1=X(3,IBUFN(1))
        X2=X(1,IBUFN(2))
        Y2=X(2,IBUFN(2))
        Z2=X(3,IBUFN(2))
        X3=X(1,IBUFN(3))
        Y3=X(2,IBUFN(3))
        Z3=X(3,IBUFN(3))
C
        X21=X2-X1
        Y21=Y2-Y1
        Z21=Z2-Z1
        X31=X3-X1
        Y31=Y3-Y1
        Z31=Z3-Z1
        X32=X3-X2
        Y32=Y3-Y2
        Z32=Z3-Z2
C
        A2 = X21**2 + Y21**2 + Z21**2
        B2 = X32**2 + Y32**2 + Z32**2
        C2 = X31**2 + Y31**2 + Z31**2
        AA = SQRT(A2)
        BB = SQRT(B2)
        CC = SQRT(C2)
C
        ANG1 = ACOS((A2 + C2 - B2)/(TWO * AA * CC)) / PI
        ANG2 = ACOS((A2 + B2 - C2)/(TWO * AA * BB)) / PI
        ANG3 = ACOS((B2 + C2 - A2)/(TWO * BB * CC)) / PI
C
        IF ( ( (A2 + C2 - B2)/(2. * AA * CC) <= -ONE ) .OR.
     .       ( (A2 + C2 - B2)/(2. * AA * CC) >=  ONE ) .OR.
     .       ( (A2 + B2 - C2)/(2. * AA * BB) <= -ONE ) .OR.
     .       ( (A2 + B2 - C2)/(2. * AA * BB) >=  ONE ) .OR.
     .       ( (B2 + C2 - A2)/(2. * BB * CC) <= -ONE ) .OR.
     .       ( (B2 + C2 - A2)/(2. * BB * CC) >=  ONE ) ) THEN
          CALL ANCMSG(MSGID=880,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ADMID,
     .                C1=TITR,I2=ID)
        ENDIF
C
        E3X=Y21*Z31-Z21*Y31
        E3Y=Z21*X31-X21*Z31
        E3Z=X21*Y31-Y21*X31
        SUM=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        AREA=HALF*SUM
C
        SMASS = AMASU * AREA
C
        MS(IBUFN(1))=MS(IBUFN(1)) + SMASS * ANG1
        MS(IBUFN(2))=MS(IBUFN(2)) + SMASS * ANG2
        MS(IBUFN(3))=MS(IBUFN(3)) + SMASS * ANG3
C
C        ADDMAS = ADDMAS + SMASS * (ANG1 + ANG2 + ANG3)
C    ANG1 + ANG2 + ANG3 = 1.
        ADDMAS = ADDMAS + SMASS
      END IF
C=======================================================================
      RETURN
      END
