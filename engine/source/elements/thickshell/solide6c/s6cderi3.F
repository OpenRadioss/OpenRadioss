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
Chd|  S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S6CDERI3(
     1   OFF,     DET,     NGL,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      Y1,      Y2,      Y3,
     4   Y4,      Y5,      Y6,      Z1,
     5   Z2,      Z3,      Z4,      Z5,
     6   Z6,      PX1,     PX2,     PX3,
     7   PX4,     PY1,     PY2,     PY3,
     8   PY4,     PZ1,     PZ2,     PZ3,
     9   PZ4,     PX1H,    PX2H,    PX3H,
     A   PY1H,    PY2H,    PY3H,    PZ1H,
     B   PZ2H,    PZ3H,    JACOB5,  JACOB6,
     C   JACOB4,  JACOB8,  JACOB9,  JACOB7,
     D   JACI33,  B1X,     B1Y,     B2Y,
     E   B2X,     B1122,   B1221,   B2212,
     F   B1121,   B1XH,    B1YH,    B2XH,
     G   B2YH,    B1122H,  B1221H,  B2212H,
     H   B1121H,  VZL,     VOLG,    SAV,
     I   OFFG,    NEL,     ISMSTR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER :: NEL, NGL(*)
C
      my_real
     .   OFF(*),DET(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*),   
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H(*), PX2H(*), PX3H(*),  
     .   PY1H(*), PY2H(*), PY3H(*),   
     .   PZ1H(*), PZ2H(*), PZ3H(*),   
     .   JACOB7(*),JACOB8(*),JACOB9(*),
     .   JACOB4(*),JACOB5(*),JACOB6(*),
     .   JACI33(*),B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   B1122(*),B1221(*),B2212(*),B1121(*),
     .   B1122H(*),B1221H(*),B2212H(*),B1121H(*),
     .   VZL(*),VOLG(*),OFFG(*)
      DOUBLE PRECISION
     .   SAV(NEL,15)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, J,ICOR,NNEGA,INDEX(MVSIZ)
C                                                                     12
      my_real
     .   DETT(MVSIZ), JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ), 
     .   JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     .   JAC7(MVSIZ), JAC8(MVSIZ), JAC9(MVSIZ),
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X21(MVSIZ) , X31(MVSIZ) , X54(MVSIZ) , X64(MVSIZ),
     .   Y21(MVSIZ) , Y31(MVSIZ) , Y54(MVSIZ) , Y64(MVSIZ),
     .   Z21(MVSIZ) , Z31(MVSIZ) , Z54(MVSIZ) , Z64(MVSIZ),
     .   X41(MVSIZ) , Y41(MVSIZ) , Z41(MVSIZ) , 
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   FAC
C-----------------------------------------------
      NNEGA=0
C
      DO I=1,NEL
      X21(I)=X2(I)-X1(I)
      X31(I)=X3(I)-X1(I)
      X41(I)=X4(I)-X1(I)
      X54(I)=X5(I)-X4(I)
      X64(I)=X6(I)-X4(I)
      Y21(I)=Y2(I)-Y1(I)
      Y31(I)=Y3(I)-Y1(I)
      Y41(I)=Y4(I)-Y1(I)
      Y54(I)=Y5(I)-Y4(I)
      Y64(I)=Y6(I)-Y4(I)
      Z21(I)=Z2(I)-Z1(I)
      Z31(I)=Z3(I)-Z1(I)
      Z41(I)=Z4(I)-Z1(I)
      Z54(I)=Z5(I)-Z4(I)
      Z64(I)=Z6(I)-Z4(I)
      END DO
C
C Jacobian matrix
      DO I=1,NEL
C  -------ri.xi---->ksi--------
       JAC1(I)=X21(I)+X54(I)
       JAC2(I)=Y21(I)+Y54(I)
       JAC3(I)=Z21(I)+Z54(I)
      END DO
      DO I=1,NEL
C  -------si.xi--->eta--------
       JAC4(I)=X31(I)+X64(I)
       JAC5(I)=Y31(I)+Y64(I)
       JAC6(I)=Z31(I)+Z64(I)
C  -------ti.xi----zeta-------
       JAC7(I)=THIRD*(X41(I)+X5(I)-X2(I)+X6(I)-X3(I))
       JAC8(I)=THIRD*(Y41(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
       JAC9(I)=THIRD*(Z41(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
      END DO
C
      DO I=1,NEL
       JACOB4(I)=JAC4(I)
       JACOB5(I)=JAC5(I)
       JACOB6(I)=JAC6(I)
       JACOB7(I)=JAC7(I)
       JACOB8(I)=JAC8(I)
       JACOB9(I)=JAC9(I)
      ENDDO 
C
      DO I=1,NEL
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=1,NEL
      DET(I)=ONE_OVER_8*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      END DO
C
      IF(IDTMIN(1)==1)THEN
        ICOR = 0
        DO I=1,NEL
           IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
          DO I=1,NEL
           IF(OFF(I)/=ZERO)THEN
             IF(DET(I)<=VOLMIN)THEN
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
             ELSEIF(DET(I)<=ZERO)THEN
               CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                     I1=NGL(I))
               MSTOP = 1
             ENDIF
           ENDIF
          ENDDO
        ENDIF
      ELSEIF(IDTMIN(1)==2)THEN
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
           ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
          DO I=1,NEL
            IF((OFF(I)/=ZERO).AND.
     .         (DET(I)<=VOLMIN.OR.DET(I)<=ZERO))THEN
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
              IDEL7NOK = 1
            ENDIF
          ENDDO
        ENDIF
      ELSEIF (ISMSTR /=4 ) THEN
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
        DO I=1,NEL
          IF(OFF(I) == ZERO)THEN
            DET(I)=ONE
          ELSEIF(OFFG(I) > ONE)THEN
            
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            NNEGA=NNEGA+1
              INDEX(NNEGA)=I
#include "lockon.inc"
            WRITE(ISTDO,3000) NGL(I)
            WRITE(IOUT ,3000) NGL(I)
#include "lockoff.inc"
          ENDIF
        ENDDO
          IF (INEG_V==0) THEN
           CALL ANCMSG(MSGID=280,ANMODE=ANINFO)
           MSTOP = 1
          ENDIF
        END IF !(ICOR>0) THEN
      ELSE
C
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF(DET(I)<=ZERO)THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
          DO I=1,NEL
           IF(OFF(I)/=ZERO)THEN
            IF(DET(I)<=ZERO)THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
              MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ENDIF
C
C Projection is not changed---
      IF (NNEGA>0) THEN
#include "vectorize.inc"
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
            X6(I)=ZERO
            Y6(I)=ZERO
            Z6(I)=ZERO
C
           X21(I)=X2(I)-X1(I)
           X31(I)=X3(I)-X1(I)
           X41(I)=X4(I)-X1(I)
           X54(I)=X5(I)-X4(I)
           X64(I)=X6(I)-X4(I)
           Y21(I)=Y2(I)-Y1(I)
           Y31(I)=Y3(I)-Y1(I)
           Y41(I)=Y4(I)-Y1(I)
           Y54(I)=Y5(I)-Y4(I)
           Y64(I)=Y6(I)-Y4(I)
           Z21(I)=Z2(I)-Z1(I)
           Z31(I)=Z3(I)-Z1(I)
           Z41(I)=Z4(I)-Z1(I)
           Z54(I)=Z5(I)-Z4(I)
           Z64(I)=Z6(I)-Z4(I)
C
           JAC1(I)=X21(I)+X54(I)
           JAC2(I)=Y21(I)+Y54(I)
           JAC3(I)=Z21(I)+Z54(I)
C----
           JAC4(I)=X31(I)+X64(I)
           JAC5(I)=Y31(I)+Y64(I)
           JAC6(I)=Z31(I)+Z64(I)
           JAC7(I)=THIRD*(X41(I)+X5(I)-X2(I)+X6(I)-X3(I))
           JAC8(I)=THIRD*(Y41(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
           JAC9(I)=THIRD*(Z41(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
C
           JACOB4(I)=JAC4(I)
           JACOB5(I)=JAC5(I)
           JACOB6(I)=JAC6(I)
           JACOB7(I)=JAC7(I)
           JACOB8(I)=JAC8(I)
           JACOB9(I)=JAC9(I)
C
           JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
           JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
           JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C
           DET(I)=ONE_OVER_8*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
           OFFG(I) = TWO
         ENDDO
      END IF
C
C Jacobian matrix inverse
      DO I=1,NEL
        DETT(I)=ONE_OVER_8/DET(I)
C
        JACI1=DETT(I)*JAC_59_68(I)
        JACI4=DETT(I)*JAC_67_49(I)
        JACI7=DETT(I)*JAC_48_57(I)
        JACI2=DETT(I)*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JACI5=DETT(I)*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JACI8=DETT(I)*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JACI3=DETT(I)*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JACI6=DETT(I)*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JACI9=DETT(I)*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
C
        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8
C
C Symmetry (a b c a b c)->P1-P3, anti-symmetry(-1 -1 -1 1 1 1)->P4
        PX1(I)=-JACI12
        PY1(I)=-JACI45
        PZ1(I)=-JACI78
        PX2(I)=JACI1
        PY2(I)=JACI4
        PZ2(I)=JACI7
        PX3(I)=JACI2
        PY3(I)=JACI5
        PZ3(I)=JACI8
        PX4(I)=THIRD*JACI3
        PY4(I)=THIRD*JACI6
        PZ4(I)=THIRD*JACI9
        JACI33(I) = JACI9*ONE_OVER_12
      ENDDO
C
C For shear traitement----------
      DO I=1,NEL
       FAC = DETT(I)*ONE_OVER_12
       B1X(I,1)=-FAC*JAC1(I)*JAC2(I)
       B1X(I,2)=-FAC*JAC4(I)*JAC5(I)
       B1Y(I,1)=-FAC*JAC2(I)*JAC2(I)
       B1Y(I,2)=-FAC*JAC5(I)*JAC5(I)
       B2X(I,1)=-FAC*JAC1(I)*JAC1(I)
       B2X(I,2)=-FAC*JAC4(I)*JAC4(I)
       B2Y(I,1)=B1X(I,1)
       B2Y(I,2)=B1X(I,2)
       FAC = FAC*2.0
       B1122(I)=FAC*JAC1(I)*JAC5(I)
       B1221(I)=FAC*JAC2(I)*JAC4(I)
       B2212(I)=FAC*JAC5(I)*JAC2(I)
       B1121(I)=B2212(I)
C
       B1XH(I,1)=-FAC*(X54(I)*Y54(I)-X21(I)*Y21(I))
       B1XH(I,2)=-FAC*(X64(I)*Y64(I)-X31(I)*Y31(I))
       B1YH(I,1)=-FAC*(Y54(I)*Y54(I)-Y21(I)*Y21(I))
       B1YH(I,2)=-FAC*(Y64(I)*Y64(I)-Y31(I)*Y31(I))
       B2XH(I,1)=-FAC*(X54(I)*X54(I)-X21(I)*X21(I))
       B2XH(I,2)=-FAC*(X64(I)*X64(I)-X31(I)*X31(I))
       B2YH(I,1)=B1XH(I,1)
       B2YH(I,2)=B1XH(I,2)
       FAC = FAC*TWO
       B1122H(I)=FAC*(X54(I)*Y64(I)-X21(I)*Y31(I))
       B1221H(I)=FAC*(X64(I)*Y54(I)-X31(I)*Y21(I))
       B2212H(I)=FAC*(Y54(I)*Y64(I)-Y21(I)*Y31(I))
       B1121H(I)=B2212H(I)
      ENDDO
C Non constant part
      DO I=1,NEL
       JAC1(I)=-X21(I)+X54(I)
       JAC2(I)=-Y21(I)+Y54(I)
       JAC3(I)=-Z21(I)+Z54(I)
       JAC4(I)=-X31(I)+X64(I)
       JAC5(I)=-Y31(I)+Y64(I)
       JAC6(I)=-Z31(I)+Z64(I)
      ENDDO
C
      DO I=1,NEL
       JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
       JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
       JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
       JACI1=DETT(I)*JAC_59_68(I)
       JACI4=DETT(I)*JAC_67_49(I)
       JACI7=DETT(I)*JAC_48_57(I)
       JACI2=DETT(I)*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
       JACI5=DETT(I)*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
       JACI8=DETT(I)*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
C
       JACI12=JACI1+JACI2
       JACI45=JACI4+JACI5
       JACI78=JACI7+JACI8
C
C Symmetry(a b c a b c)->P1-P3
       PX1H(I)=-JACI12
       PY1H(I)=-JACI45
       PZ1H(I)=-JACI78
       PX2H(I)=JACI1
       PY2H(I)=JACI4
       PZ2H(I)=JACI7
       PX3H(I)=JACI2
       PY3H(I)=JACI5
       PZ3H(I)=JACI8
      ENDDO
C                                                                     12
       DO I=1,NEL
        VZL(I) = FOURTH*(JACOB9(I)*(
     .          X54(I)*Y64(I)-X21(I)*Y31(I)-X64(I)*Y54(I)+X31(I)*Y21(I))
     .                  -JACOB8(I)*(
     .          X54(I)*Z64(I)+X31(I)*Z21(I)-X21(I)*Z31(I)-X64(I)*Z54(I))
     .                  +JACOB7(I)*(
     .          Y54(I)*Z64(I)+Y31(I)*Z21(I)-Y21(I)*Z31(I)-Y64(I)*Z54(I))
     .                )
        VOLG(I)= DET(I)
       ENDDO
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
 3000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +    ' SOLID-SHELL ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/) 
      END