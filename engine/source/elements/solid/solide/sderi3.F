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
Chd|  SDERI3                        source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        MULTI_COMPUTEVOLUME           source/multifluid/multi_computevolume.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SCHKJABT3                     source/elements/solid/solide4/s4deri3.F
Chd|====================================================================
      SUBROUTINE SDERI3(OFF,DET,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H1, PX1H2, PX1H3,
     .   PX2H1, PX2H2, PX2H3,
     .   PX3H1, PX3H2, PX3H3,
     .   PX4H1, PX4H2, PX4H3,
     .   AJ1,AJ2,AJ3,
     .   AJ4,AJ5,AJ6,SAV,OFFG ,NEL,VOLDP)
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
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   SAV(NEL,21) ,VOLDP(*)  
      my_real
     .   OFF(*),DET(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),  
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),HXP,HYP,HZP,OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J,NNEGA,INDEX(MVSIZ)
C     REAL ou REAL*8
      my_real
     .   DETT ,  AJ7(MVSIZ), AJ8(MVSIZ) , AJ9(MVSIZ),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   AJ12, AJ45, AJ78,
     .   A17 , A28 ,
     .   B17 , B28 ,
     .   C17 , C28 ,
     .   HX,HY,HZ,ICOR
      DOUBLE PRECISION
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46
C-----------------------------------------------
C
      DO I=LFT,LLT
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
c             
        AJ1(I)=X17+X28-X35-X46
        AJ2(I)=Y17+Y28-Y35-Y46
        AJ3(I)=Z17+Z28-Z35-Z46
        A17=X17+X46
        A28=X28+X35
        B17=Y17+Y46
        B28=Y28+Y35
        C17=Z17+Z46
        C28=Z28+Z35

        AJ4(I)=A17+A28
        AJ5(I)=B17+B28
        AJ6(I)=C17+C28
        AJ7(I)=A17-A28
        AJ8(I)=B17-B28
        AJ9(I)=C17-C28
      ENDDO
C                  
C     JACOBIAN
C
      DO 100 I=LFT,LLT      
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
  100 JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
      DO I=LFT,LLT    
       VOLDP(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
       DET(I)=VOLDP(I)
      ENDDO
        
      CALL SCHKJABT3(OFF,DET,NGL,OFFG,NNEGA,INDEX)
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
            AJ1(I)=X17+X28-X35-X46
            AJ2(I)=Y17+Y28-Y35-Y46
            AJ3(I)=Z17+Z28-Z35-Z46
            A17=X17+X46
            A28=X28+X35
            B17=Y17+Y46
            B28=Y28+Y35
            C17=Z17+Z46
            C28=Z28+Z35
C      
            AJ4(I)=A17+A28
            AJ5(I)=B17+B28
            AJ6(I)=C17+C28
            AJ7(I)=A17-A28
            AJ8(I)=B17-B28
            AJ9(I)=C17-C28
C	    
            JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
            JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
            JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
           VOLDP(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
           DET(I)=VOLDP(I)
c
	   OFFG(I) = TWO
         ENDDO
      END IF
C
      DO I=LFT,LLT
        DETT=ONE_OVER_64/DET(I)
C       INVERSE DE LA MATRICE JACOBIENNE
        AJI1=DETT*JAC_59_68(I)
        AJI4=DETT*JAC_67_49(I)
        AJI7=DETT*JAC_48_57(I)
        AJI2=DETT*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1-AJI2
        AJ45=AJI4-AJI5
        AJ78=AJI7-AJI8
        PX3(I)= AJ12+AJI3
        PY3(I)= AJ45+AJI6
        PZ3(I)= AJ78+AJI9
        PX4(I)= AJ12-AJI3
        PY4(I)= AJ45-AJI6
        PZ4(I)= AJ78-AJI9
!
        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8
!
        PX1(I)=-AJ12-AJI3
        PY1(I)=-AJ45-AJI6
        PZ1(I)=-AJ78-AJI9
        PX2(I)=-AJ12+AJI3
        PY2(I)=-AJ45+AJI6
        PZ2(I)=-AJ78+AJI9
      ENDDO
C
      IF(JHBE.NE.0)THEN
        DO200 I=LFT,LLT
C 1 -1 1 -1 1 -1 1 -1
         HX=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
  200   CONTINUE
c
C 1 1 -1 -1 -1 -1 1 1
        DO201 I=LFT,LLT
         HX=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
  201   CONTINUE
c
C 1 -1 -1 1 -1 1 1 -1
        DO202 I=LFT,LLT
         HX=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
  202   CONTINUE
      ENDIF
c-----------
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        FDERI3                        source/elements/solid/solide/fderi3.F
Chd|        S4DERIT3                      source/elements/solid/solide4/s4deri3.F
Chd|        S8EDERICT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderi3.F
Chd|        SDERIT3                       source/elements/solid/solide/sderi3.F
Chd|        SZDERIT3                      source/elements/solid/solidez/szderi3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCHKJAB3(OFF,DET,NGL)
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*)
C     REAL ou REAL*8
      my_real
     .   OFF(*),DET(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J ,ICOR
C     REAL ou REAL*8
C-----------------------------------------------
      IF(IDTMIN(1).EQ.1)THEN
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) .EQ.ZERO)THEN
            DET(I)=1.
          ELSEIF((DET(I).LE.VOLMIN).OR.(DET(I).LE.ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR.GT.0.AND.IMCONV.EQ.1) THEN
          DO I=LFT,LLT
           IF(OFF(I).NE.ZERO)THEN
            IF(DET(I).LE.VOLMIN)THEN
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
              IDEL7NOK = 1
            ELSEIF(DET(I).LE.ZERO)THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
              DET(I)= ONE
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
        IF (ICOR.GT.0.AND.IMCONV.EQ.1) THEN
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
      ELSE
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) .EQ.ZERO)THEN
            DET(I)=ONE
          ELSEIF(DET(I).LE.ZERO)THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR.GT.0.AND.IMCONV.EQ.1) THEN
          DO I=LFT,LLT
           IF(OFF(I).NE.ZERO)THEN
            IF(DET(I).LE.ZERO)THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
              DET(I)= ONE
              MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ENDIF
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
Chd|====================================================================
Chd|  SDERIT3                       source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|====================================================================
      SUBROUTINE SDERIT3(OFF,DET,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H1, PX1H2, PX1H3,
     .   PX2H1, PX2H2, PX2H3,
     .   PX3H1, PX3H2, PX3H3,
     .   PX4H1, PX4H2, PX4H3,
     .   AJ1,AJ2,AJ3,
     .   AJ4,AJ5,AJ6)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)   
 
      my_real
     .   OFF(*),DET(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),  
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),HXP,HYP,HZP
     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J

      my_real
     .   DETT ,  AJ7(MVSIZ), AJ8(MVSIZ) , AJ9(MVSIZ),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   AJ12, AJ45, AJ78,
     .   A17 , A28 ,
     .   B17 , B28 ,
     .   C17 , C28 ,
     .   HX,HY,HZ,    
     .   ICOR
C-----------------------------------------------
C
      DO I=LFT,LLT
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
        AJ1(I)=X17+X28-X35-X46
        AJ2(I)=Y17+Y28-Y35-Y46
        AJ3(I)=Z17+Z28-Z35-Z46
        A17=X17+X46
        A28=X28+X35
        B17=Y17+Y46      
        B28=Y28+Y35
        C17=Z17+Z46
        C28=Z28+Z35
!         
        AJ4(I)=A17+A28
        AJ5(I)=B17+B28
        AJ6(I)=C17+C28
        AJ7(I)=A17-A28             
        AJ8(I)=B17-B28
        AJ9(I)=C17-C28
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
  110 DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))   
C

      CALL SCHKJAB3(OFF,DET,NGL)
C
      DO I=LFT,LLT
        DETT=ONE_OVER_64/DET(I)
C     INVERSE DE LA MATRICE JACOBIENNE
        AJI1=DETT*JAC_59_68(I)
        AJI4=DETT*JAC_67_49(I)
        AJI7=DETT*JAC_48_57(I)
        AJI2=DETT*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1-AJI2
        AJ45=AJI4-AJI5
        AJ78=AJI7-AJI8

        PX3(I)= AJ12+AJI3
        PY3(I)= AJ45+AJI6
        PZ3(I)= AJ78+AJI9
        PX4(I)= AJ12-AJI3
        PY4(I)= AJ45-AJI6
        PZ4(I)= AJ78-AJI9

        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8

        PX1(I)=-AJ12-AJI3
        PY1(I)=-AJ45-AJI6
        PZ1(I)=-AJ78-AJI9
        PX2(I)=-AJ12+AJI3
        PY2(I)=-AJ45+AJI6
        PZ2(I)=-AJ78+AJI9
      ENDDO
C
      IF(JHBE.NE.0)THEN
        DO200 I=LFT,LLT
         HX=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
  200   CONTINUE
C
        DO201 I=LFT,LLT
         HX=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
  201   CONTINUE
c
        DO202 I=LFT,LLT
         HX=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
  202   CONTINUE
      ENDIF
      RETURN
C-----------
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
Chd|====================================================================
Chd|  SDERITO3                      source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDERITO3(OFF,DET,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   JAC_I)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL ou REAL*8
C                                                                     12
      my_real
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   AJ12, AJ45, AJ78
C-----------------------------------------------
      DO I=LFT,LLT
        AJI1=JAC_I(1,I)
        AJI4=JAC_I(4,I)
        AJI7=JAC_I(7,I)
        AJI2=JAC_I(2,I)
        AJI5=JAC_I(5,I)
        AJI8=JAC_I(8,I)
        AJI3=JAC_I(3,I)
        AJI6=JAC_I(6,I)
        AJI9=JAC_I(9,I)
        DET(I) =JAC_I(10,I)
C      
        AJ12=AJI1-AJI2
        AJ45=AJI4-AJI5
        AJ78=AJI7-AJI8

        PX3(I)= AJ12+AJI3
        PY3(I)= AJ45+AJI6
        PZ3(I)= AJ78+AJI9
        PX4(I)= AJ12-AJI3
        PY4(I)= AJ45-AJI6
        PZ4(I)= AJ78-AJI9

        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8

        PX1(I)=-AJ12-AJI3
        PY1(I)=-AJ45-AJI6
        PZ1(I)=-AJ78-AJI9
        PX2(I)=-AJ12+AJI3
        PY2(I)=-AJ45+AJI6
        PZ2(I)=-AJ78+AJI9
      ENDDO
C
      RETURN
C
      END
            
