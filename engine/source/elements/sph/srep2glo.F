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
Chd|  SREP2GLO                      source/elements/sph/srep2glo.F
Chd|-- called by -----------
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|-- calls ---------------
Chd|        SCORTHO3                      source/elements/thickshell/solidec/scortho3.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREPISOT3                     source/elements/solid/solide/srepisot3.F
Chd|====================================================================
      SUBROUTINE SREP2GLO(
     1   X,       IXS,     GAMA,    RX,
     2   RY,      RZ,      SX,      SY,
     3   SZ,      TX,      TY,      TZ,
     4   R11,     R12,     R13,     R21,
     5   R22,     R23,     R31,     R32,
     6   R33,     T11,     T12,     T13,
     7   T21,     T22,     T23,     T31,
     8   T32,     T33,     JR0,     JS0,
     9   JT0,     NEL,     LFT,     LLT,
     A   JHBE,    JCVT,    ISORTH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: JHBE
      INTEGER, INTENT(INOUT) :: JCVT
      INTEGER, INTENT(INOUT) :: ISORTH
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*), GAMA(NEL,6),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   T11(MVSIZ),T12(MVSIZ),T13(MVSIZ),
     .   T21(MVSIZ),T22(MVSIZ),T23(MVSIZ),
     .   T31(MVSIZ),T32(MVSIZ),T33(MVSIZ),
     .   RX(MVSIZ),SX(MVSIZ),TX(MVSIZ),
     .   RY(MVSIZ),SY(MVSIZ),TY(MVSIZ),
     .   RZ(MVSIZ),SZ(MVSIZ),TZ(MVSIZ),
     .   JR0(MVSIZ),JS0(MVSIZ),JT0(MVSIZ)
      INTEGER IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   S11,S22,S33,S12,S21,S23,S32,S13,S31,RR,XL,YL,ZL
      INTEGER NC1, NC2, NC3, NC4, 
     .        NC5, NC6, NC7, NC8, I
C-----------------------------------------------
      DO I=LFT,LLT
        NC1=IXS(2,I)
        NC2=IXS(3,I)
        NC3=IXS(4,I)
        NC4=IXS(5,I)
        NC5=IXS(6,I)
        NC6=IXS(7,I)
        NC7=IXS(8,I)
        NC8=IXS(9,I)
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
        X1(I)=X(1,NC1)
        Y1(I)=X(2,NC1)
        Z1(I)=X(3,NC1)
        X2(I)=X(1,NC2)
        Y2(I)=X(2,NC2)
        Z2(I)=X(3,NC2)
        X3(I)=X(1,NC3)
        Y3(I)=X(2,NC3)
        Z3(I)=X(3,NC3)
        X4(I)=X(1,NC4)
        Y4(I)=X(2,NC4)
        Z4(I)=X(3,NC4)
        X5(I)=X(1,NC5)
        Y5(I)=X(2,NC5)
        Z5(I)=X(3,NC5)
        X6(I)=X(1,NC6)
        Y6(I)=X(2,NC6)
        Z6(I)=X(3,NC6)
        X7(I)=X(1,NC7)
        Y7(I)=X(2,NC7)
        Z7(I)=X(3,NC7)
        X8(I)=X(1,NC8)
        Y8(I)=X(2,NC8)
        Z8(I)=X(3,NC8)
      END DO
C-----------
C isoparametric system (non orthomalized)
      CALL SREPISOT3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      X7,      X8,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Y7,      Y8,
     5   Z1,      Z2,      Z3,      Z4,
     6   Z5,      Z6,      Z7,      Z8,
     7   RX,      RY,      RZ,      SX,
     8   SY,      SZ,      TX,      TY,
     9   TZ,      NEL)

C-----------
C GLOBAL => Corotational
      IF (JHBE == 14 .OR. JHBE == 24) THEN
        CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R12,     R13,     R11,
     4   R22,     R23,     R21,     R32,
     5   R33,     R31,     NEL)
      ELSEIF (JHBE == 15 ) THEN
        CALL SCORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     NEL)
      ELSEIF(JCVT /= 0)THEN
        CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     NEL)
      ENDIF
C-----------
C orthomalized isoparametric system
      DO I=LFT,LLT
       RR   =SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)
       RX(I)=RX(I)/MAX(EM30,RR)
       RY(I)=RY(I)/MAX(EM30,RR)
       RZ(I)=RZ(I)/MAX(EM30,RR)
      END DO
      DO I=LFT,LLT
       TX(I)=RY(I)*SZ(I)-RZ(I)*SY(I)
       TY(I)=RZ(I)*SX(I)-RX(I)*SZ(I)
       TZ(I)=RX(I)*SY(I)-RY(I)*SX(I)
       RR   =SQRT(TX(I)**2+TY(I)**2+TZ(I)**2)
       TX(I)=TX(I)/MAX(EM30,RR)
       TY(I)=TY(I)/MAX(EM30,RR)
       TZ(I)=TZ(I)/MAX(EM30,RR)
      END DO
      DO I=LFT,LLT
       SX(I)=TY(I)*RZ(I)-TZ(I)*RY(I)
       SY(I)=TZ(I)*RX(I)-TX(I)*RZ(I)
       SZ(I)=TX(I)*RY(I)-TY(I)*RX(I)
       RR   =SQRT(SX(I)**2+SY(I)**2+SZ(I)**2)
       SX(I)=SX(I)/MAX(EM30,RR)
       SY(I)=SY(I)/MAX(EM30,RR)
       SZ(I)=SZ(I)/MAX(EM30,RR)
      END DO
C-----------
C       REPERE ORTHOTROPE.
C-----------
      IF (ISORTH /= 0) THEN
        IF (JCVT == 0) THEN
          DO I=LFT,LLT
            G11=GAMA(I,1)
            G21=GAMA(I,2)
            G31=GAMA(I,3)
            G12=GAMA(I,4)
            G22=GAMA(I,5)
            G32=GAMA(I,6)
            G13=G21*G32-G31*G22
            G23=G31*G12-G11*G32
            G33=G11*G22-G21*G12
C           MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
            T11(I)=RX(I)*G11+SX(I)*G21+TX(I)*G31
            T12(I)=RX(I)*G12+SX(I)*G22+TX(I)*G32
            T13(I)=RX(I)*G13+SX(I)*G23+TX(I)*G33
            T21(I)=RY(I)*G11+SY(I)*G21+TY(I)*G31
            T22(I)=RY(I)*G12+SY(I)*G22+TY(I)*G32
            T23(I)=RY(I)*G13+SY(I)*G23+TY(I)*G33
            T31(I)=RZ(I)*G11+SZ(I)*G21+TZ(I)*G31
            T32(I)=RZ(I)*G12+SZ(I)*G22+TZ(I)*G32
            T33(I)=RZ(I)*G13+SZ(I)*G23+TZ(I)*G33
          END DO
        ELSE
          DO I=LFT,LLT
            G11=GAMA(I,1)
            G21=GAMA(I,2)
            G31=GAMA(I,3)
            G12=GAMA(I,4)
            G22=GAMA(I,5)
            G32=GAMA(I,6)
            G13=G21*G32-G31*G22
            G23=G31*G12-G11*G32
            G33=G11*G22-G21*G12
C           MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
            T11(I)=R11(I)*G11+R12(I)*G21+R13(I)*G31
            T12(I)=R11(I)*G12+R12(I)*G22+R13(I)*G32
            T13(I)=R11(I)*G13+R12(I)*G23+R13(I)*G33
            T21(I)=R21(I)*G11+R22(I)*G21+R23(I)*G31
            T22(I)=R21(I)*G12+R22(I)*G22+R23(I)*G32
            T23(I)=R21(I)*G13+R22(I)*G23+R23(I)*G33
            T31(I)=R31(I)*G11+R32(I)*G21+R33(I)*G31
            T32(I)=R31(I)*G12+R32(I)*G22+R33(I)*G32
            T33(I)=R31(I)*G13+R32(I)*G23+R33(I)*G33
          END DO
        ENDIF
      ENDIF
C
      IF (JHBE==24) THEN
        DO I=LFT,LLT      
                XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
                YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
                ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
                X1(I)=XL
                Y1(I)=YL
                Z1(I)=ZL
                XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
                YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
                ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
                X2(I)=XL
                Y2(I)=YL
                Z2(I)=ZL
                XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
                YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
                ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
                X3(I)=XL
                Y3(I)=YL
                Z3(I)=ZL
                XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
                YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
                ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
                X4(I)=XL
                Y4(I)=YL
                Z4(I)=ZL
                XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
                YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
                ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
                X5(I)=XL
                Y5(I)=YL
                Z5(I)=ZL
                XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
                YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
                ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
                X6(I)=XL
                Y6(I)=YL
                Z6(I)=ZL
                XL=R11(I)*X7(I)+R21(I)*Y7(I)+R31(I)*Z7(I)
                YL=R12(I)*X7(I)+R22(I)*Y7(I)+R32(I)*Z7(I)
                ZL=R13(I)*X7(I)+R23(I)*Y7(I)+R33(I)*Z7(I)
                X7(I)=XL
                Y7(I)=YL
                Z7(I)=ZL
                XL=R11(I)*X8(I)+R21(I)*Y8(I)+R31(I)*Z8(I)
                YL=R12(I)*X8(I)+R22(I)*Y8(I)+R32(I)*Z8(I)
                ZL=R13(I)*X8(I)+R23(I)*Y8(I)+R33(I)*Z8(I)
                X8(I)=XL
                Y8(I)=YL
                Z8(I)=ZL 
        ENDDO
C
        DO I=LFT,LLT
          JR0(I) = -X1(I)+X2(I)+X3(I)-
     .                 X4(I)-X5(I)+X6(I)+
     .                 X7(I)-X8(I)
          JS0(I) = -Y1(I)-Y2(I)+Y3(I)+
     .                 Y4(I)-Y5(I)-Y6(I)+
     .                 Y7(I)+Y8(I)
          JT0(I) = -Z1(I)-Z2(I)-Z3(I)-
     .                 Z4(I)+Z5(I)+Z6(I)+
     .                 Z7(I)+Z8(I)
        ENDDO
      ENDIF
C-----------
      RETURN
      END
