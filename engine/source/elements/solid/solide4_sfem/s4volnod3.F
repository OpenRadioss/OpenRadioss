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
Chd|  S4VOLNOD3                     source/elements/solid/solide4_sfem/s4volnod3.F
Chd|-- called by -----------
Chd|        S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|====================================================================
      SUBROUTINE S4VOLNOD3(
     1   VOLNOD6, X,       NC1,     NC2,
     2   NC3,     NC4,     OFFG,    XDP,
     3   NEL,     ISMSTR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*),NEL
      DOUBLE PRECISION
     .   VOLNOD6(6,2*NUMNOD)
      DOUBLE PRECISION , DIMENSION(3,SXDP/3), INTENT(IN) :: XDP     
C     REAL
      my_real
     .   X(3,*),OFFG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K
C     REAL

      my_real
     .   DET(MVSIZ)
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,
     .   B1, C1, D1
      DOUBLE PRECISION
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)

      DOUBLE PRECISION
     .   DET6(6,MVSIZ),DETDP
C----------------------------
C     NODAL COORDINATES     
C----------------------------
      IF (ISMSTR==1.OR.ISMSTR==11) RETURN
      IF(IRESP == 1) THEN
        IF((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3)THEN
          DO I=1,NEL
            IF(ABS(OFFG(I))>ONE) CYCLE
            X1(I)=XDP(1,NC1(I))
            Y1(I)=XDP(2,NC1(I))
            Z1(I)=XDP(3,NC1(I))
            X2(I)=XDP(1,NC2(I))
            Y2(I)=XDP(2,NC2(I))
            Z2(I)=XDP(3,NC2(I))
            X3(I)=XDP(1,NC3(I))
            Y3(I)=XDP(2,NC3(I))
            Z3(I)=XDP(3,NC3(I))
            X4(I)=XDP(1,NC4(I))
            Y4(I)=XDP(2,NC4(I))
            Z4(I)=XDP(3,NC4(I))
          ENDDO
        ELSE
          DO I=1,NEL
            X1(I)=XDP(1,NC1(I))
            Y1(I)=XDP(2,NC1(I))
            Z1(I)=XDP(3,NC1(I))
            X2(I)=XDP(1,NC2(I))
            Y2(I)=XDP(2,NC2(I))
            Z2(I)=XDP(3,NC2(I))
            X3(I)=XDP(1,NC3(I))
            Y3(I)=XDP(2,NC3(I))
            Z3(I)=XDP(3,NC3(I))
            X4(I)=XDP(1,NC4(I))
            Y4(I)=XDP(2,NC4(I))
            Z4(I)=XDP(3,NC4(I))
          ENDDO
        ENDIF
      ELSE
        IF((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3)THEN
          DO I=1,NEL
            IF(ABS(OFFG(I))>ONE) CYCLE
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
          ENDDO
        ELSE
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
          ENDDO
        ENDIF
      END IF !(IRESP == 1) THEN
C----------------------------
C     TETRAHEDRON VOLUME
C----------------------------
      DO I=1,NEL
       IF (OFFG(I) == ZERO .OR. ABS(OFFG(I))>ONE) THEN
         DET(I)= ZERO
       ELSE
         X43 = X4(I) - X3(I)
         Y43 = Y4(I) - Y3(I)
         Z43 = Z4(I) - Z3(I)
         X41 = X4(I) - X1(I)
         Y41 = Y4(I) - Y1(I)
         Z41 = Z4(I) - Z1(I)
         X42 = X4(I) - X2(I)
         Y42 = Y4(I) - Y2(I)
         Z42 = Z4(I) - Z2(I)
C
         B1 =  Y43*Z42 - Y42*Z43
         C1 =  Z43*X42 - Z42*X43
         D1 =  X43*Y42 - X42*Y43
C
         DETDP= (X41*B1 + Y41*C1 + Z41*D1)*ONE_OVER_6
         DET(I)= DETDP
       ENDIF
      ENDDO

      !Parith-On treatment
      CALL FOAT_TO_6_FLOAT(1  ,NEL  ,DET ,DET6 )

      DO I=1,NEL
C----------------------------------
C VOLUME <0 SERA TESTE DANS S4DERI3
C----------------------------------
        !Parith-On treatment
        DO K=1,6
          VOLNOD6(K,NC1(I)) = VOLNOD6(K,NC1(I)) + DET6(K,I)
          VOLNOD6(K,NC2(I)) = VOLNOD6(K,NC2(I)) + DET6(K,I)
          VOLNOD6(K,NC3(I)) = VOLNOD6(K,NC3(I)) + DET6(K,I)
          VOLNOD6(K,NC4(I)) = VOLNOD6(K,NC4(I)) + DET6(K,I)
        ENDDO
      ENDDO
C
      RETURN
      END
