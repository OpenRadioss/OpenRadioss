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
Chd|  SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCDERI3(OFF,DET,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H1, PX1H2, PX1H3, PX1H4,
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4,
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   JAC1,JAC2,JAC3,
     .   JAC4,JAC5,JAC6,
     .   RX0,RY0,SX0,SY0,VZL,VOLG,SAV,OFFG,NEL)
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
      INTEGER :: NEL,NGL(*)
C
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   SAV(NEL,21)
     
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
     .   JAC4(*),JAC5(*),JAC6(*),
     .   RX0(*),RY0(*),SX0(*),SY0(*),VZL(*),VOLG(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NNEGA,INDEX(MVSIZ)
C                                                                     12
      my_real
     .   DETT ,  JAC7(MVSIZ), JAC8(MVSIZ) ,JAC9(MVSIZ), 
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),JAC_19_37(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35 ,AREA(MVSIZ) ,
     .   HX,HY,HZ,A_I,
     .   H1X(MVSIZ),H1Y(MVSIZ),H2X(MVSIZ),H2Y(MVSIZ),
     .   H1Z(MVSIZ),H2Z(MVSIZ),ICOR
C=======================================================================
C
      NNEGA=0
      DO I=1,NEL
      X17(I)=X7(I)-X1(I)
      X28(I)=X8(I)-X2(I)
      X35(I)=X5(I)-X3(I)
      X46(I)=X6(I)-X4(I)
      Y17(I)=Y7(I)-Y1(I)
      Y28(I)=Y8(I)-Y2(I)
      Y35(I)=Y5(I)-Y3(I)
      Y46(I)=Y6(I)-Y4(I)
      Z17(I)=Z7(I)-Z1(I)
      Z28(I)=Z8(I)-Z2(I)
      Z35(I)=Z5(I)-Z3(I)
      Z46(I)=Z6(I)-Z4(I)
      END DO
C
C Jacobian matrix
      DO I=1,NEL
C  -------ri.xi-----------
        JAC3(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC1(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC2(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
C-------
        X_17_46=X17(I)+X46(I)
        X_28_35=X28(I)+X35(I)
        Y_17_46=Y17(I)+Y46(I)
        Y_28_35=Y28(I)+Y35(I)
        Z_17_46=Z17(I)+Z46(I)
        Z_28_35=Z28(I)+Z35(I)
C  -------si.xi-----------
        JAC6(I)=X_17_46+X_28_35
        JAC4(I)=Y_17_46+Y_28_35
        JAC5(I)=Z_17_46+Z_28_35
C  -------ti.xi-----------
        JAC9(I)=X_17_46-X_28_35
        JAC7(I)=Y_17_46-Y_28_35
        JAC8(I)=Z_17_46-Z_28_35
      ENDDO
C
      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_19_37(I)=JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
       AREA(I) = ONE_OVER_64*JAC_19_37(I)
       DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      ENDDO
C
      IF(IDTMIN(1)==1)THEN
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            AREA(I) = ONE
            DET(I)=ONE
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO)
     .           .OR.(AREA(I)<=ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
          DO I=1,NEL
           IF(OFF(I)/=ZERO)THEN
             IF(DET(I)<=VOLMIN)THEN
              AREA(I) = ONE
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
            ELSEIF(DET(I)<=ZERO.OR.(AREA(I)<=ZERO))THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
             MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ELSEIF(IDTMIN(1)==2)THEN
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            AREA(I) = ONE
            DET(I)=ONE
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO)
     .           .OR.(AREA(I)<=ZERO))THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
          DO I=1,NEL
            IF((OFF(I)/=0.).AND.
     .         (DET(I)<=VOLMIN.OR.DET(I)<=ZERO)
     .           .OR.(AREA(I)<=ZERO))THEN
              AREA(I) = ONE
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
            ENDIF
          ENDDO
        ENDIF
      ELSEIF (ISMSTR /=4 ) THEN
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF((DET(I)<=VOLMIN).OR.(AREA(I)<=ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR>0) THEN
        DO I=1,NEL
          IF(OFF(I) == ZERO)THEN
            DET(I)=ONE
            AREA(I) = ONE
          ELSEIF(OFFG(I) > ONE)THEN
	  
          ELSEIF((DET(I)<=VOLMIN).OR.(AREA(I)<=ZERO))THEN
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
        ICOR = 0
        DO I=1,NEL
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
            AREA(I) = ONE
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
            X6(I)=SAV(I,16)
            Y6(I)=SAV(I,17)
            Z6(I)=SAV(I,18)
            X7(I)=SAV(I,19)
            Y7(I)=SAV(I,20)
            Z7(I)=SAV(I,21)
            X8(I)=ZERO
            Y8(I)=ZERO
            Z8(I)=ZERO
C
C---------
            JAC3(I)=X17(I)+X28(I)-X35(I)-X46(I)
            JAC1(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
            JAC2(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
C-------
            X_17_46=X17(I)+X46(I)
            X_28_35=X28(I)+X35(I)
            Y_17_46=Y17(I)+Y46(I)
            Y_28_35=Y28(I)+Y35(I)
            Z_17_46=Z17(I)+Z46(I)
            Z_28_35=Z28(I)+Z35(I)
C----------
            JAC6(I)=X_17_46+X_28_35
            JAC4(I)=Y_17_46+Y_28_35
            JAC5(I)=Z_17_46+Z_28_35
            JAC9(I)=X_17_46-X_28_35
            JAC7(I)=Y_17_46-Y_28_35
            JAC8(I)=Z_17_46-Z_28_35
C
            JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
            JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
            JAC_19_37(I)=JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I)
            JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C
            AREA(I) = ONE_OVER_64*JAC_19_37(I)
            DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
            OFFG(I) = TWO
         ENDDO
      END IF
C
C Jacobian matrix inverse 
      DO I=1,NEL
        DETT=ONE_OVER_64/DET(I)
        JACI1=DETT*JAC_59_68(I)
        JACI4=DETT*JAC_67_49(I)
        JACI7=DETT*JAC_48_57(I)
        JACI2=DETT*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JACI5=DETT*JAC_19_37(I)
        JACI8=DETT*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JACI3=DETT*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JACI6=DETT*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JACI9=DETT*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
C
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
C
        PY3(I)= JACI12+JACI3
        PZ3(I)= JACI45+JACI6
        PX3(I)= JACI78+JACI9
        PY4(I)= JACI12-JACI3
        PZ4(I)= JACI45-JACI6
        PX4(I)= JACI78-JACI9

        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8

        PY1(I)=-JACI12-JACI3
        PZ1(I)=-JACI45-JACI6
        PX1(I)=-JACI78-JACI9
        PY2(I)=-JACI12+JACI3
        PZ2(I)=-JACI45+JACI6
        PX2(I)=-JACI78+JACI9
      ENDDO
C
       DO I=1,NEL
C   h3
C 1 -1 1 -1 1 -1 1 -1
         HX=ONE_OVER_8*(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=ONE_OVER_8*(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=ONE_OVER_8*(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
C   h1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         H1X(I)=X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I)
         H1Y(I)=Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I)
         H1Z(I)=Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I)
         HX=ONE_OVER_8*H1X(I)
         HY=ONE_OVER_8*H1Y(I)
         HZ=ONE_OVER_8*H1Z(I)
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
C   h2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         H2X(I)=X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I)
         H2Y(I)=Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I)
         H2Z(I)=Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I)
         HX=ONE_OVER_8*H2X(I)
         HY=ONE_OVER_8*H2Y(I)
         HZ=ONE_OVER_8*H2Z(I)	 
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       END DO
C   h4
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
C
       DO I=1,NEL
        A_I = ONE_OVER_8/AREA(I)
        RX0(I) = JAC9(I)*A_I
        RY0(I) = JAC7(I)*A_I
        SX0(I) = JAC3(I)*A_I
        SY0(I) = JAC1(I)*A_I
        VZL(I) = ONE_OVER_64*JAC5(I)*(
     .          JAC9(I)*H1Y(I)+JAC1(I)*H2X(I)-JAC3(I)*H2Y(I)-JAC7(I)*H1X(I))
     .         + ONE_OVER_64*JAC4(I)*(
     .          JAC3(I)*H2Z(I)+JAC8(I)*H1X(I)-JAC9(I)*H1Z(I)-JAC2(I)*H2X(I))
     .         + ONE_OVER_64*JAC6(I)*(
     .          JAC7(I)*H1Z(I)+JAC2(I)*H2Y(I)-JAC1(I)*H2Z(I)-JAC8(I)*H1Y(I))
C
        VOLG(I)= DET(I)
       ENDDO
C-----------
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
 3000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +    ' SOLID-SHELL ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/) 
C-----------
      END SUBROUTINE SCDERI3