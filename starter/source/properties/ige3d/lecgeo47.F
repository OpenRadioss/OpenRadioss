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
Chd|  LECG47                        source/properties/ige3d/lecgeo47.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECG47(IIN   ,IOUT ,PARGEO,IGEO,KNOT,IAD_KNOT)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "ige3d_c.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI)
      INTEGER IIN,IOUT
      my_real   
     .        PARGEO(*),KNOT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,NUVAR,NPG,JCVT,
     .        D1,D2,D3,N1,N2,N3,IAD_KNOT,J,INTRULE,
     .        I,NKNOT1,NKNOT2,NKNOT3,RAFRULE
      my_real
     .   R5(5)
C=======================================================================
      READ(IIN,ERR=999,FMT='(A)')LINE
      READ(LINE,ERR=999,FMT=FMT_2I) INTRULE,RAFRULE
      READ(IIN,ERR=999,FMT='(A)')LINE
      READ(LINE,ERR=999,FMT=FMT_6I)
     . D1,D2,D3,N1,N2,N3
      IGEO(40) = IAD_KNOT
      IGEO(41) = D1+1
      IGEO(42) = D2+1
      IGEO(43) = D3+1
      IGEO(44) = N1
      IGEO(45) = N2
      IGEO(46) = N3
      DEG_MAX=MAX(DEG_MAX,D1+2,D2+2,D3+2)
      NKNOT1 = N1+D1+1
      NKNOT2 = N2+D2+1
      NKNOT3 = N3+D3+1
      DO I=1,((N1+D1)/5)+1
        READ(IIN,ERR=999,FMT='(A)')LINE
        KLINE=LINE
        READ(LINE,ERR=999,FMT=FMT_5F) R5
        DO J=1,5
         IF(IAD_KNOT < NKNOT1+IGEO(40))THEN
           IAD_KNOT = IAD_KNOT + 1
           KNOT(IAD_KNOT) = R5(J)
         ENDIF
        ENDDO
      ENDDO
C
      DO I=1,((N2+D2)/5)+1
        READ(IIN,ERR=999,FMT='(A)')LINE
        KLINE=LINE
        READ(LINE,ERR=999,FMT=FMT_5F) R5
        DO J=1,5
         IF(IAD_KNOT < NKNOT1+NKNOT2+IGEO(40))THEN
           IAD_KNOT = IAD_KNOT + 1
           KNOT(IAD_KNOT) = R5(J)
         ENDIF
        ENDDO
      ENDDO
C
      DO I=1,((N3+D3)/5)+1
        READ(IIN,ERR=999,FMT='(A)')LINE
        KLINE=LINE
        READ(LINE,ERR=999,FMT=FMT_5F) R5
        DO J=1,5
         IF(IAD_KNOT < NKNOT1+NKNOT2+NKNOT3+IGEO(40))THEN
           IAD_KNOT = IAD_KNOT + 1
           KNOT(IAD_KNOT) = R5(J)
         ENDIF
        ENDDO
      ENDDO
C
      SKNOT=IAD_KNOT
C
C-----------
     
      WRITE(IOUT,1000) 
      RETURN
C-----------
 1000 FORMAT(
     & 5X,'ISO GEO PROPERTY . . . . . . . . . . .)')
C-----------
 999  CALL FREERR(3)
      RETURN
      END SUBROUTINE LECG47

