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
Chd|  S4COORK                       source/elements/solid/solide4/s4coork.F
Chd|-- called by -----------
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4COORK(
     1   X,       IXS,     X1,      X2,
     2   X3,      X4,      Y1,      Y2,
     3   Y3,      Y4,      Z1,      Z2,
     4   Z3,      Z4,      OFFG,    OFF,
     5   SAV,     NC1,     NC2,     NC3,
     6   NC4,     NGL,     MXT,     NGEO,
     7   K11,     K12,     K13,     K14,
     8   K22,     K23,     K24,     K33,
     9   K34,     K44,     NEL,     ISMSTR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NEL
      my_real
     .   X(3,*),
     .   X1(*), X2(*), X3(*), X4(*),  
     .   Y1(*), Y2(*), Y3(*), Y4(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), 
     .   OFFG(*), OFF(*)
      DOUBLE PRECISION
     .  SAV(NEL,9)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)  ,K14(9,*)    ,K22(9,*)  ,
     .   K23(9,*)   ,K24(9,*)   ,K33(9,*)   ,K34(9,*)   ,K44(9,*)     
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,MXT_1
C     REAL
C-----------------------------------------------
C
      MXT_1 = IXS(1,1)
      DO I=1,NEL
      NGEO(I)=IXS(10,I)
      NGL(I)=IXS(11,I)
      MXT(I)=MXT_1
      NC1(I)=IXS(2,I)
      NC2(I)=IXS(4,I)
      NC3(I)=IXS(7,I)
      NC4(I)=IXS(6,I)
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO I=1,NEL
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
      ENDDO
C-----------
      IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
       DO I=1,NEL
       IF(ABS(OFFG(I))>ONE)THEN
        X1(I)=SAV(I,1)
        Y1(I)=SAV(I,2)
        Z1(I)=SAV(I,3)
        X2(I)=SAV(I,4)
        Y2(I)=SAV(I,5)
        Z2(I)=SAV(I,6)
        X3(I)=SAV(I,7)
        Y3(I)=SAV(I,8)
        Z3(I)=SAV(I,9)
        X4(I)=ZERO
        Y4(I)=ZERO
        Z4(I)=ZERO
        OFF(I) = ABS(OFFG(I))-ONE
       ELSE
        OFF(I) = OFFG(I)
       ENDIF
       ENDDO
C
      ENDIF
C-----------
      DO J=1,9      
      DO I=1,NEL
        K11(J,I)=ZERO
        K12(J,I)=ZERO
        K13(J,I)=ZERO
        K14(J,I)=ZERO
        K22(J,I)=ZERO
        K23(J,I)=ZERO
        K24(J,I)=ZERO
        K33(J,I)=ZERO
        K34(J,I)=ZERO
        K44(J,I)=ZERO
      ENDDO
      ENDDO
C-----------
      RETURN
      END
