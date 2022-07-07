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
Chd|  S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S6CKEL3                       source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S6CDERI3(OFF,DET,NGL,
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H, PX2H, PX3H, 
     .   PY1H, PY2H, PY3H, 
     .   PZ1H, PZ2H, PZ3H, 
     .   AAJ5,AAJ6,AAJ4,
     .   AAJ8,AAJ9,AAJ7,
     .   JI33,B1X,B1Y,B2Y,B2X,B1122,B1221,B2212,B1121,
     .   B1XH,B1YH,B2XH,B2YH,B1122H,B1221H,B2212H,B1121H,
     .   VZL,VOLG,SAV,OFFG,NEL)
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
#include      "vect01_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
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
     .   AAJ7(*),AAJ8(*),AAJ9(*),
     .   AAJ4(*),AAJ5(*),AAJ6(*),
     .   JI33(*),B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   B1122(*),B1221(*),B2212(*),B1121(*),
     .   B1122H(*),B1221H(*),B2212H(*),B1121H(*),
     .   VZL(*),VOLG(*),OFFG(*)
      DOUBLE PRECISION
     .   SAV(NEL,15)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J,ICOR,NNEGA,INDEX(MVSIZ)
C     REAL
C                                                                     12
      my_real
     .   DETT(MVSIZ) ,  AJ1(MVSIZ), AJ2(MVSIZ) ,AJ3(MVSIZ), 
     .   AJ4(MVSIZ)  ,  AJ5(MVSIZ), AJ6(MVSIZ),
     .   AJ7(MVSIZ)  ,  AJ8(MVSIZ), AJ9(MVSIZ),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   X21(MVSIZ) , X31(MVSIZ) , X54(MVSIZ) , X64(MVSIZ),
     .   Y21(MVSIZ) , Y31(MVSIZ) , Y54(MVSIZ) , Y64(MVSIZ),
     .   Z21(MVSIZ) , Z31(MVSIZ) , Z54(MVSIZ) , Z64(MVSIZ),
     .   X41(MVSIZ) , Y41(MVSIZ) , Z41(MVSIZ) , 
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   AJ12, AJ45, AJ78,
     .   FAC
C-----------------------------------------------
      NNEGA=0
C
      DO 10 I=LFT,LLT
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
   10 Z64(I)=Z6(I)-Z4(I)
C
      DO 20 I=LFT,LLT
C  -------ri.xi---->ksi--------
      AJ1(I)=X21(I)+X54(I)
      AJ2(I)=Y21(I)+Y54(I)
   20 AJ3(I)=Z21(I)+Z54(I)
      DO 30 I=LFT,LLT
C  -------si.xi--->eta--------
      AJ4(I)=X31(I)+X64(I)
      AJ5(I)=Y31(I)+Y64(I)
      AJ6(I)=Z31(I)+Z64(I)
C  -------ti.xi----zeta-------
      AJ7(I)=THIRD*(X41(I)+X5(I)-X2(I)+X6(I)-X3(I))
      AJ8(I)=THIRD*(Y41(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
   30 AJ9(I)=THIRD*(Z41(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
      DO I=LFT,LLT
       AAJ4(I)=AJ4(I)
       AAJ5(I)=AJ5(I)
       AAJ6(I)=AJ6(I)
       AAJ7(I)=AJ7(I)
       AAJ8(I)=AJ8(I)
       AAJ9(I)=AJ9(I)
      ENDDO 
C
C     JACOBIAN
C
      DO 100 I=LFT,LLT
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
  100 JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
      DO 110 I=LFT,LLT
  110 DET(I)=ONE_OVER_8*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
C
      IF(IDTMIN(1).EQ.1)THEN
        ICOR = 0
        DO I=LFT,LLT
           IF(OFF(I) .EQ.ZERO)THEN
            DET(I)=ONE
          ELSEIF((DET(I).LE.VOLMIN).OR.(DET(I).LE.ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR.GT.0) THEN
          DO I=LFT,LLT
           IF(OFF(I).NE.ZERO)THEN
             IF(DET(I).LE.VOLMIN)THEN
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
             ELSEIF(DET(I).LE.ZERO)THEN
               CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                     I1=NGL(I))
               MSTOP = 1
             ENDIF
           ENDIF
          ENDDO
        ENDIF
      ELSEIF(IDTMIN(1).EQ.2)THEN
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) .EQ.ZERO)THEN
            DET(I)=ONE
           ELSEIF((DET(I).LE.VOLMIN).OR.(DET(I).LE.ZERO))THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR.GT.0) THEN
          DO I=LFT,LLT
            IF((OFF(I).NE.ZERO).AND.
     .         (DET(I).LE.VOLMIN.OR.DET(I).LE.ZERO))THEN
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
        DO I=LFT,LLT
          IF(OFF(I) .EQ.ZERO)THEN
            DET(I)=ONE
          ELSEIF((DET(I).LE.VOLMIN).OR.(DET(I).LE.ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR.GT.0) THEN
        DO I=LFT,LLT
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
        END IF !(ICOR.GT.0) THEN
      ELSE
C
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) .EQ.ZERO)THEN
            DET(I)=ONE
          ELSEIF(DET(I).LE.ZERO)THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR.GT.0) THEN
          DO I=LFT,LLT
           IF(OFF(I).NE.ZERO)THEN
            IF(DET(I).LE.ZERO)THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
              MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ENDIF
C------------projection is not changed---
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
           AJ1(I)=X21(I)+X54(I)
           AJ2(I)=Y21(I)+Y54(I)
           AJ3(I)=Z21(I)+Z54(I)
C----
           AJ4(I)=X31(I)+X64(I)
           AJ5(I)=Y31(I)+Y64(I)
           AJ6(I)=Z31(I)+Z64(I)
           AJ7(I)=THIRD*(X41(I)+X5(I)-X2(I)+X6(I)-X3(I))
           AJ8(I)=THIRD*(Y41(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
           AJ9(I)=THIRD*(Z41(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
C
           AAJ4(I)=AJ4(I)
           AAJ5(I)=AJ5(I)
           AAJ6(I)=AJ6(I)
           AAJ7(I)=AJ7(I)
           AAJ8(I)=AJ8(I)
           AAJ9(I)=AJ9(I)
C
           JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
           JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
           JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
           DET(I)=ONE_OVER_8*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
	   OFFG(I) = TWO
         ENDDO
      END IF
C      
      DO I=LFT,LLT
        DETT(I)=ONE_OVER_8/DET(I)
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
        AJI1=DETT(I)*JAC_59_68(I)
        AJI4=DETT(I)*JAC_67_49(I)
        AJI7=DETT(I)*JAC_48_57(I)
        AJI2=DETT(I)*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT(I)*(AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT(I)*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT(I)*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT(I)*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT(I)*(AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8
C
C ----------symtrie(a b c a b c)->P1-P3,anti-symtrie(-1 -1 -1 1 1 1)->P4
        PX1(I)=-AJ12
        PY1(I)=-AJ45
        PZ1(I)=-AJ78
        PX2(I)=AJI1
        PY2(I)=AJI4
        PZ2(I)=AJI7
        PX3(I)=AJI2
        PY3(I)=AJI5
        PZ3(I)=AJI8
        PX4(I)=THIRD*AJI3
        PY4(I)=THIRD*AJI6
        PZ4(I)=THIRD*AJI9
       JI33(I) = AJI9*ONE_OVER_12
      ENDDO
C ----------for shear traitement----------
      DO I=LFT,LLT
       FAC = DETT(I)*ONE_OVER_12
       B1X(I,1)=-FAC*AJ1(I)*AJ2(I)
       B1X(I,2)=-FAC*AJ4(I)*AJ5(I)
       B1Y(I,1)=-FAC*AJ2(I)*AJ2(I)
       B1Y(I,2)=-FAC*AJ5(I)*AJ5(I)
       B2X(I,1)=-FAC*AJ1(I)*AJ1(I)
       B2X(I,2)=-FAC*AJ4(I)*AJ4(I)
       B2Y(I,1)=B1X(I,1)
       B2Y(I,2)=B1X(I,2)
       FAC = FAC*2.0
       B1122(I)=FAC*AJ1(I)*AJ5(I)
       B1221(I)=FAC*AJ2(I)*AJ4(I)
       B2212(I)=FAC*AJ5(I)*AJ2(I)
C         B1121(I)=FAC*AJ1(I)*AJ4(I)
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
C         B1121H(I)=FAC*(X64(I)*Y54(I)-X31(I)*Y21(I))
       B1121H(I)=B2212H(I)
      ENDDO
C  --------non constante part--------
      DO I=LFT,LLT
       AJ1(I)=-X21(I)+X54(I)
       AJ2(I)=-Y21(I)+Y54(I)
       AJ3(I)=-Z21(I)+Z54(I)
       AJ4(I)=-X31(I)+X64(I)
       AJ5(I)=-Y31(I)+Y64(I)
       AJ6(I)=-Z31(I)+Z64(I)
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
       AJI1=DETT(I)*JAC_59_68(I)
       AJI4=DETT(I)*JAC_67_49(I)
       AJI7=DETT(I)*JAC_48_57(I)
       AJI2=DETT(I)*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       AJI5=DETT(I)*(AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       AJI8=DETT(I)*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
C
       AJ12=AJI1+AJI2
       AJ45=AJI4+AJI5
       AJ78=AJI7+AJI8
C
C ----------symtrie(a b c a b c)->P1-P3
       PX1H(I)=-AJ12
       PY1H(I)=-AJ45
       PZ1H(I)=-AJ78
       PX2H(I)=AJI1
       PY2H(I)=AJI4
       PZ2H(I)=AJI7
       PX3H(I)=AJI2
       PY3H(I)=AJI5
       PZ3H(I)=AJI8
      ENDDO
C                                                                     12
       DO I=LFT,LLT
        VZL(I) = FOURTH*(AAJ9(I)*(
     .          X54(I)*Y64(I)-X21(I)*Y31(I)-X64(I)*Y54(I)+X31(I)*Y21(I))
     .                -AAJ8(I)*(
     .          X54(I)*Z64(I)+X31(I)*Z21(I)-X21(I)*Z31(I)-X64(I)*Z54(I))
     .                +AAJ7(I)*(
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
