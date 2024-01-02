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
Chd|  SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SCHKJABT3                     source/elements/solid/solide4/schkjabt3.F
Chd|====================================================================
      SUBROUTINE SZDERI3(
     1   OFF,     DET,     NGL,     ISMSTR,
     2   X1,      X2,      X3,      X4,
     3   X5,      X6,      X7,      X8,
     4   Y1,      Y2,      Y3,      Y4,
     5   Y5,      Y6,      Y7,      Y8,
     6   Z1,      Z2,      Z3,      Z4,
     7   Z5,      Z6,      Z7,      Z8,
     8   PX1,     PX2,     PX3,     PX4,
     9   PY1,     PY2,     PY3,     PY4,
     A   PZ1,     PZ2,     PZ3,     PZ4,
     B   PX1H1,   PX1H2,   PX1H3,   PX1H4,
     C   PX2H1,   PX2H2,   PX2H3,   PX2H4,
     D   PX3H1,   PX3H2,   PX3H3,   PX3H4,
     E   PX4H1,   PX4H2,   PX4H3,   PX4H4,
     F   JAC1,    JAC2,    JAC3,    JAC4,
     G   JAC5,    JAC6,    JAC9,    SAV,
     H   OFFG,    NEL,     VOLDP,   JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JLAG
      INTEGER :: ISMSTR,NEL,NGL(*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   SAV(NEL,21),VOLDP(*)
C
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),JAC9(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NNEGA,INDEX(MVSIZ)
C
      my_real
     .   DETT ,  JAC7(MVSIZ), JAC8(MVSIZ) , 
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35 ,
     .   HX,HY,HZ
      DOUBLE PRECISION
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46
C-----------------------------------------------
      DO I=1,NEL
        X17=X7(I)-X1(I)
        X28=X8(I)-X2(I)
        X35=X5(I)-X3(I)
        X46=X6(I)-X4(I)
        Y17=Y7(I)-Y1(I)
        Y28=Y8(I)-Y2(I)
        Y35=Y5(I)-Y3(I)
        Y46=Y6(I)-Y4(I)
        Z17=Z7(I)-Z1(I)
        Z28=Z8(I)-Z2(I)
        Z35=Z5(I)-Z3(I)
        Z46=Z6(I)-Z4(I)
C
        JAC4(I)=X17+X28-X35-X46
        JAC5(I)=Y17+Y28-Y35-Y46
        JAC6(I)=Z17+Z28-Z35-Z46
        X_17_46=X17+X46
        X_28_35=X28+X35
        Y_17_46=Y17+Y46
        Y_28_35=Y28+Y35
        Z_17_46=Z17+Z46
        Z_28_35=Z28+Z35
        JAC7(I)=X_17_46+X_28_35
        JAC8(I)=Y_17_46+Y_28_35
        JAC9(I)=Z_17_46+Z_28_35
        JAC1(I)=X_17_46-X_28_35
        JAC2(I)=Y_17_46-Y_28_35
        JAC3(I)=Z_17_46-Z_28_35
      ENDDO
C
      DO I=1,NEL
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=1,NEL
      VOLDP(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DET(I)=VOLDP(I)
      END DO
C
      CALL SCHKJABT3(
     1   OFF,     DET,     NGL,     OFFG,
     2   NNEGA,   INDEX,   NEL,     ISMSTR,
     3   JLAG)
      IF (NNEGA>0) THEN
        IF (ISMSTR==10.OR.ISMSTR==12) THEN
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
            X1(I)=SAV(I,1)
            Y1(I)=SAV(I,8)
            Z1(I)=SAV(I,15)
            X2(I)=SAV(I,2)
            Y2(I)=SAV(I,9)
            Z2(I)=SAV(I,16)
            X3(I)=SAV(I,3)
            Y3(I)=SAV(I,10)
            Z3(I)=SAV(I,17)
            X4(I)=SAV(I,4)
            Y4(I)=SAV(I,11)
            Z4(I)=SAV(I,18)
            X5(I)=SAV(I,5)
            Y5(I)=SAV(I,12)
            Z5(I)=SAV(I,19)
            X6(I)=SAV(I,6)
            Y6(I)=SAV(I,13)
            Z6(I)=SAV(I,20)
            X7(I)=SAV(I,7)
            Y7(I)=SAV(I,14)
            Z7(I)=SAV(I,21)
            X8(I)=ZERO
            Y8(I)=ZERO
            Z8(I)=ZERO
         ENDDO
        ELSE
         DO J=1,NNEGA
          I = INDEX(J)
            X1(I)=SAV(I,1)
            Y1(I)=SAV(I,2)
            Z1(I)=SAV(I,3)
            X2(I)=SAV(I,4)
            Y2(I)=SAV(I,5)
            Z2(I)=SAV(I,6)
            X3(I)=SAV(I,7)
            Y3(I)=SAV(I,8)
            Z3(I)=SAV(I,9)
            X4(I)=SAV(I,10)
            Y4(I)=SAV(I,11)
            Z4(I)=SAV(I,12)
            X5(I)=SAV(I,13)
            Y5(I)=SAV(I,14)
            Z5(I)=SAV(I,15)
            X6(I)=SAV(I,16)
            Y6(I)=SAV(I,17)
            Z6(I)=SAV(I,18)
            X7(I)=SAV(I,19)
            Y7(I)=SAV(I,20)
            Z7(I)=SAV(I,21)
            X8(I)=ZERO
            Y8(I)=ZERO
            Z8(I)=ZERO
         ENDDO
        END IF
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
C
            X17=X7(I)-X1(I)
            X28=X8(I)-X2(I)
            X35=X5(I)-X3(I)
            X46=X6(I)-X4(I)
            Y17=Y7(I)-Y1(I)
            Y28=Y8(I)-Y2(I)
            Y35=Y5(I)-Y3(I)
            Y46=Y6(I)-Y4(I)
            Z17=Z7(I)-Z1(I)
            Z28=Z8(I)-Z2(I)
            Z35=Z5(I)-Z3(I)
            Z46=Z6(I)-Z4(I)
C
            JAC4(I)=X17+X28-X35-X46
            JAC5(I)=Y17+Y28-Y35-Y46
            JAC6(I)=Z17+Z28-Z35-Z46
            JAC7(I)=X17+X46+X28+X35
            JAC8(I)=Y17+Y46+Y28+Y35
            JAC9(I)=Z17+Z46+Z28+Z35
            JAC1(I)=X17+X46-X28-X35
            JAC2(I)=Y17+Y46-Y28-Y35
            JAC3(I)=Z17+Z46-Z28-Z35
C      
            JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
            JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
            JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C
           VOLDP(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
           DET(I)=VOLDP(I)
           OFFG(I) = TWO
         ENDDO
      END IF
C
C Jacobian matrix inverse and shape functions derivatives Pij
      DO I=1,NEL
         DETT=ONE_OVER_64/DET(I)
         JACI1=DETT*JAC_59_68(I)
         JACI4=DETT*JAC_67_49(I)
         JACI7=DETT*JAC_48_57(I)
         JACI2=DETT*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
         JACI5=DETT*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
         JACI8=DETT*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
         JACI3=DETT*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
         JACI6=DETT*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
         JACI9=DETT*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))

         JACI12=JACI1-JACI2
         JACI45=JACI4-JACI5
         JACI78=JACI7-JACI8
         PX2(I)= JACI12-JACI3
         PY2(I)= JACI45-JACI6
         PZ2(I)= JACI78-JACI9
         PX4(I)=-JACI12-JACI3
         PY4(I)=-JACI45-JACI6
         PZ4(I)=-JACI78-JACI9

         JACI12=JACI1+JACI2
         JACI45=JACI4+JACI5
         JACI78=JACI7+JACI8
         PX1(I)=-JACI12-JACI3
         PY1(I)=-JACI45-JACI6
         PZ1(I)=-JACI78-JACI9
         PX3(I)=JACI12-JACI3
         PY3(I)=JACI45-JACI6
         PZ3(I)=JACI78-JACI9
       ENDDO
C
       DO I=1,NEL
C 1 -1 1 -1 1 -1 1 -1
         HX=ONE_OVER_8*(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))   
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         HX=ONE_OVER_8*(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY=ONE_OVER_8*(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ=ONE_OVER_8*(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))   
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         HX=ONE_OVER_8*(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))   
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         HX=ONE_OVER_8*(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))   
         PX1H4(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H4(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H4(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H4(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
      RETURN
C
      END SUBROUTINE SZDERI3
Chd|====================================================================
Chd|  SZDERIT3                      source/elements/solid/solidez/szderi3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SCHKJAB3                      source/elements/solid/solide/schkjab3.F
Chd|====================================================================
      SUBROUTINE SZDERIT3(
     1   OFF,     DET,     NGL,     NEL,
     2   X1,      X2,      X3,      X4,
     3   X5,      X6,      X7,      X8,
     4   Y1,      Y2,      Y3,      Y4,
     5   Y5,      Y6,      Y7,      Y8,
     6   Z1,      Z2,      Z3,      Z4,
     7   Z5,      Z6,      Z7,      Z8,
     8   PX1,     PX2,     PX3,     PX4,
     9   PY1,     PY2,     PY3,     PY4,
     A   PZ1,     PZ2,     PZ3,     PZ4,
     B   PX1H1,   PX1H2,   PX1H3,   PX1H4,
     C   PX2H1,   PX2H2,   PX2H3,   PX2H4,
     D   PX3H1,   PX3H2,   PX3H3,   PX3H4,
     E   PX4H1,   PX4H2,   PX4H3,   PX4H4,
     F   JAC1,    JAC2,    JAC3,    JAC4,
     G   JAC5,    JAC6,    JAC9,    JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JLAG
      INTEGER :: NEL,NGL(*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),JAC9(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C                                                                     12
      my_real
     .   DETT ,  JAC7(MVSIZ), JAC8(MVSIZ) , 
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35 ,
     .   HX,HY,HZ
C-----------------------------------------------
C
C Jacobian matrix
      DO I=1,NEL
       X17=X7(I)-X1(I)
       X28=X8(I)-X2(I)
       X35=X5(I)-X3(I)
       X46=X6(I)-X4(I)
       Y17=Y7(I)-Y1(I)
       Y28=Y8(I)-Y2(I)
       Y35=Y5(I)-Y3(I)
       Y46=Y6(I)-Y4(I)
       Z17=Z7(I)-Z1(I)
       Z28=Z8(I)-Z2(I)
       Z35=Z5(I)-Z3(I)
       Z46=Z6(I)-Z4(I)
C
       JAC4(I)=X17+X28-X35-X46
       JAC5(I)=Y17+Y28-Y35-Y46
       JAC6(I)=Z17+Z28-Z35-Z46
C
       X_17_46=X17+X46
       X_28_35=X28+X35
       Y_17_46=Y17+Y46
       Y_28_35=Y28+Y35
       Z_17_46=Z17+Z46
       Z_28_35=Z28+Z35
C
       JAC7(I)=X_17_46+X_28_35
       JAC8(I)=Y_17_46+Y_28_35
       JAC9(I)=Z_17_46+Z_28_35
       JAC1(I)=X_17_46-X_28_35
       JAC2(I)=Y_17_46-Y_28_35
       JAC3(I)=Z_17_46-Z_28_35
      ENDDO
C
      DO I=1,NEL
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=1,NEL
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      END DO
C
      CALL SCHKJAB3(
     1   OFF,     DET,     NGL,     NEL)
C
C Jacobian matrix inverse and shape functions derivatives Pij
      DO I=1,NEL
        DETT=ONE_OVER_64/DET(I)
        JACI1=DETT*JAC_59_68(I)
        JACI4=DETT*JAC_67_49(I)
        JACI7=DETT*JAC_48_57(I)
        JACI2=DETT*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JACI5=DETT*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JACI8=DETT*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JACI3=DETT*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JACI6=DETT*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JACI9=DETT*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
C
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8

        PX2(I)= JACI12-JACI3
        PY2(I)= JACI45-JACI6
        PZ2(I)= JACI78-JACI9
        PX4(I)=-JACI12-JACI3
        PY4(I)=-JACI45-JACI6
        PZ4(I)=-JACI78-JACI9

        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8

        PX1(I)=-JACI12-JACI3
        PY1(I)=-JACI45-JACI6
        PZ1(I)=-JACI78-JACI9
        PX3(I)=JACI12-JACI3
        PY3(I)=JACI45-JACI6
        PZ3(I)=JACI78-JACI9
      ENDDO
C
      DO I=1,NEL
C 1 -1 1 -1 1 -1 1 -1
         HX=ONE_OVER_8*(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))   
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
      END DO
C 1 1 -1 -1 -1 -1 1 1
      DO I=1,NEL
         HX=ONE_OVER_8*(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY=ONE_OVER_8*(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ=ONE_OVER_8*(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))   
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
      END DO
C 1 -1 -1 1 -1 1 1 -1
      DO I=1,NEL
         HX=ONE_OVER_8*(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))   
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
      END DO
C -1 1 -1 1 1 -1 1 -1
      DO I=1,NEL
         HX=ONE_OVER_8*(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))   
         PX1H4(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H4(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H4(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H4(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
      END DO
      RETURN
C
      END SUBROUTINE SZDERIT3
Chd|====================================================================
Chd|  SZDERITO3                     source/elements/solid/solidez/szderi3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZDERITO3(
     1   OFF,     DET,     PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     JAC_I,   NEL,
     5   JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JLAG
      INTEGER :: NEL
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C                                                                     12
      my_real
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   JACI12, JACI45, JACI78
C-----------------------------------------------
C----JAC_I is calculated based on H8, permutation :[t,r,s]:[r,s,t]
      DO I=1,NEL
        JACI1=JAC_I(3,I)
        JACI4=JAC_I(6,I)
        JACI7=JAC_I(9,I)
        JACI2=JAC_I(1,I)
        JACI5=JAC_I(4,I)
        JACI8=JAC_I(7,I)
        JACI3=JAC_I(2,I)
        JACI6=JAC_I(5,I)
        JACI9=JAC_I(8,I)
        DET(I) =JAC_I(10,I)
C      
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
        PX2(I)= JACI12-JACI3
        PY2(I)= JACI45-JACI6
        PZ2(I)= JACI78-JACI9
        PX4(I)=-JACI12-JACI3
        PY4(I)=-JACI45-JACI6
        PZ4(I)=-JACI78-JACI9
C
        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8
        PX1(I)=-JACI12-JACI3
        PY1(I)=-JACI45-JACI6
        PZ1(I)=-JACI78-JACI9
        PX3(I)= JACI12-JACI3
        PY3(I)= JACI45-JACI6
        PZ3(I)= JACI78-JACI9
      ENDDO
C
      RETURN
C
      END SUBROUTINE SZDERITO3
