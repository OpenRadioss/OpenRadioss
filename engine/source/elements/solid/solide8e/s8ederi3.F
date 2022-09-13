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
Chd|  S8EDERI3                      source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERI3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    HX,    HY,    HZ,   
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    AJ4,AJ5,AJ6,
     .    AJ7,AJ8,AJ9,
     .    SMAX,DELTAX,ICP ,IDEG,NU  )
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   SMAX(*),DELTAX(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A1PR1(MVSIZ),A1PR3(MVSIZ),A1PR5(MVSIZ),A1PR7(MVSIZ),
     .   A4PR1(MVSIZ),A4PR3(MVSIZ),A4PR5(MVSIZ),A4PR7(MVSIZ),
     .   A7PR1(MVSIZ),A7PR3(MVSIZ),A7PR5(MVSIZ),A7PR7(MVSIZ),
     .   A2PS1(MVSIZ),A2PS2(MVSIZ),A2PS5(MVSIZ),A2PS6(MVSIZ),
     .   A5PS1(MVSIZ),A5PS2(MVSIZ),A5PS5(MVSIZ),A5PS6(MVSIZ),
     .   A8PS1(MVSIZ),A8PS2(MVSIZ),A8PS5(MVSIZ),A8PS6(MVSIZ),
     .   A3PT1(MVSIZ),A3PT2(MVSIZ),A3PT3(MVSIZ),A3PT4(MVSIZ),
     .   A6PT1(MVSIZ),A6PT2(MVSIZ),A6PT3(MVSIZ),A6PT4(MVSIZ),
     .   A9PT1(MVSIZ),A9PT2(MVSIZ),A9PT3(MVSIZ),A9PT4(MVSIZ),
     .   PR(8),PS(8),PT(8),RP, SP, TP, RM, SM, TM,BXHI,BYHI,BZHI,TMP
C-----------------------------------------------
C
      DO I=LFT,LLT
       AJ1(I)=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       AJ2(I)=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       AJ3(I)=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       AJ4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       AJ5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       AJ6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       AJ7(I)=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       AJ8(I)=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       AJ9(I)=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
       JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
       JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
       JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      VOL(I)= WI*DET(I)
      ENDDO
C
      ICOR = 0
      DO I=LFT,LLT
        OFF(I)=OFFG(I)
        IF(OFF(I) ==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) VOL(I)=ONE
        ELSEIF (VOL(I)<=ZERO) THEN
         VOL(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=LFT,LLT
          IF(VOL(I)<=ZERO.AND.OFF(I)/=ZERO)THEN
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV==1) THEN
#include "lockon.inc"
            WRITE(ISTDO,2000) NGL(I)
            WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1   
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
      DO I=LFT,LLT
        DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C
      DO I=LFT,LLT
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C------NI,ksi-------
      RP = ONE + KSI
      SP = ONE + ETA
      TP = ONE + ZETA
      RM = ONE - KSI
      SM = ONE - ETA
      TM = ONE - ZETA
      PR(1)=-SM*TM
      PR(2)=-PR(1)
      PR(3)= SP*TM
      PR(4)=-PR(3)
      PR(5)=-SM*TP
      PR(6)=-PR(5)
      PR(7)= SP*TP
      PR(8)=-PR(7)
      PS(1)=-RM*TM
      PS(2)=-RP*TM
      PS(3)=-PS(2)
      PS(4)=-PS(1)
      PS(5)=-RM*TP
      PS(6)=-RP*TP
      PS(7)=-PS(6)
      PS(8)=-PS(5)
      PT(1)=-RM*SM
      PT(2)=-RP*SM
      PT(3)=-RP*SP
      PT(4)=-RM*SP
      PT(5)=-PT(1)
      PT(6)=-PT(2)
      PT(7)=-PT(3)
      PT(8)=-PT(4)
cc
      DO I=LFT,LLT
        A1PR1(I)=AJI1(I)*PR(1)
        A1PR3(I)=AJI1(I)*PR(3)
        A1PR5(I)=AJI1(I)*PR(5)
        A1PR7(I)=AJI1(I)*PR(7)
        A4PR1(I)=AJI4(I)*PR(1)
        A4PR3(I)=AJI4(I)*PR(3)
        A4PR5(I)=AJI4(I)*PR(5)
        A4PR7(I)=AJI4(I)*PR(7)
        A7PR1(I)=AJI7(I)*PR(1)
        A7PR3(I)=AJI7(I)*PR(3)
        A7PR5(I)=AJI7(I)*PR(5)
        A7PR7(I)=AJI7(I)*PR(7)
      ENDDO
      DO I=LFT,LLT
        A2PS1(I)=AJI2(I)*PS(1)
        A2PS2(I)=AJI2(I)*PS(2)
        A2PS5(I)=AJI2(I)*PS(5)
        A2PS6(I)=AJI2(I)*PS(6)
        A5PS1(I)=AJI5(I)*PS(1)
        A5PS2(I)=AJI5(I)*PS(2)
        A5PS5(I)=AJI5(I)*PS(5)
        A5PS6(I)=AJI5(I)*PS(6)
        A8PS1(I)=AJI8(I)*PS(1)
        A8PS2(I)=AJI8(I)*PS(2)
        A8PS5(I)=AJI8(I)*PS(5)
        A8PS6(I)=AJI8(I)*PS(6)
      ENDDO
C
      DO I=LFT,LLT
        A3PT1(I)=AJI3(I)*PT(1)
        A3PT2(I)=AJI3(I)*PT(2)
        A3PT3(I)=AJI3(I)*PT(3)
        A3PT4(I)=AJI3(I)*PT(4)
        A6PT1(I)=AJI6(I)*PT(1)
        A6PT2(I)=AJI6(I)*PT(2)
        A6PT3(I)=AJI6(I)*PT(3)
        A6PT4(I)=AJI6(I)*PT(4)
        A9PT1(I)=AJI9(I)*PT(1)
        A9PT2(I)=AJI9(I)*PT(2)
        A9PT3(I)=AJI9(I)*PT(3)
        A9PT4(I)=AJI9(I)*PT(4)
      ENDDO
C
      DO I=LFT,LLT
       PX1(I)= A1PR1(I)+A2PS1(I)+A3PT1(I)
       PX2(I)=-A1PR1(I)+A2PS2(I)+A3PT2(I)
       PX3(I)= A1PR3(I)-A2PS2(I)+A3PT3(I)
       PX4(I)=-A1PR3(I)-A2PS1(I)+A3PT4(I)
       PX5(I)= A1PR5(I)+A2PS5(I)-A3PT1(I)
       PX6(I)=-A1PR5(I)+A2PS6(I)-A3PT2(I)
       PX7(I)= A1PR7(I)-A2PS6(I)-A3PT3(I)
       PX8(I)=-A1PR7(I)-A2PS5(I)-A3PT4(I)
C
       PY1(I)= A4PR1(I)+A5PS1(I)+A6PT1(I)
       PY2(I)=-A4PR1(I)+A5PS2(I)+A6PT2(I)
       PY3(I)= A4PR3(I)-A5PS2(I)+A6PT3(I)
       PY4(I)=-A4PR3(I)-A5PS1(I)+A6PT4(I)
       PY5(I)= A4PR5(I)+A5PS5(I)-A6PT1(I)
       PY6(I)=-A4PR5(I)+A5PS6(I)-A6PT2(I)
       PY7(I)= A4PR7(I)-A5PS6(I)-A6PT3(I)
       PY8(I)=-A4PR7(I)-A5PS5(I)-A6PT4(I)
C
       PZ1(I)= A7PR1(I)+A8PS1(I)+A9PT1(I)
       PZ2(I)=-A7PR1(I)+A8PS2(I)+A9PT2(I)
       PZ3(I)= A7PR3(I)-A8PS2(I)+A9PT3(I)
       PZ4(I)=-A7PR3(I)-A8PS1(I)+A9PT4(I)
       PZ5(I)= A7PR5(I)+A8PS5(I)-A9PT1(I)
       PZ6(I)=-A7PR5(I)+A8PS6(I)-A9PT2(I)
       PZ7(I)= A7PR7(I)-A8PS6(I)-A9PT3(I)
       PZ8(I)=-A7PR7(I)-A8PS5(I)-A9PT4(I)
      ENDDO
C
      DO I=LFT,LLT
        IF (IDTS6==0.OR.IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    NU  )
      ENDIF
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
Chd|====================================================================
Chd|  S8EDERIC3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SCHKJABT3                     source/elements/solid/solide4/s4deri3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERIC3(OFF,DET,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX1H1, PX1H2, PX1H3,PX1H4,  
     .   PX2H1, PX2H2, PX2H3,PX2H4,  
     .   PX3H1, PX3H2, PX3H3,PX3H4,  
     .   PX4H1, PX4H2, PX4H3,PX4H4,  
     .   HX,    HY,    HZ,   
     .   JR_1,   JS_1,   JT_1,   
     .   AJ1,AJ2,AJ3,
     .   AJ4,AJ5,AJ6,
     .   AJ7,AJ8,AJ9,SMAX,SAV,OFFG,NEL)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
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
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   JR_1(*),JS_1(*),JT_1(*),   
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),SMAX(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,NNEGA,INDEX(MVSIZ)
C     REAL
C                                                                     12
      my_real
     .   DETT(MVSIZ) ,   
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ)
C-----------------------------------------------
      DO 10 I=LFT,LLT
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
   10 Z46(I)=Z6(I)-Z4(I)
C
      DO 20 I=LFT,LLT
      AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      A17(I)=X17(I)+X46(I)
      A28(I)=X28(I)+X35(I)
      B17(I)=Y17(I)+Y46(I)
      B28(I)=Y28(I)+Y35(I)
      C17(I)=Z17(I)+Z46(I)
   20 C28(I)=Z28(I)+Z35(I)
      DO 30 I=LFT,LLT
      AJ7(I)=A17(I)+A28(I)
      AJ8(I)=B17(I)+B28(I)
      AJ9(I)=C17(I)+C28(I)
      AJ1(I)=A17(I)-A28(I)
      AJ2(I)=B17(I)-B28(I)
   30 AJ3(I)=C17(I)-C28(I)
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
C
            AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
            AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
            AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
            A17(I)=X17(I)+X46(I)
            A28(I)=X28(I)+X35(I)
            B17(I)=Y17(I)+Y46(I)
            B28(I)=Y28(I)+Y35(I)
            C17(I)=Z17(I)+Z46(I)
            C28(I)=Z28(I)+Z35(I)
            AJ7(I)=A17(I)+A28(I)
            AJ8(I)=B17(I)+B28(I)
            AJ9(I)=C17(I)+C28(I)
            AJ1(I)=A17(I)-A28(I)
            AJ2(I)=B17(I)-B28(I)
            AJ3(I)=C17(I)-C28(I)
C	    
            JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
            JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
            JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
           DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
	   OFFG(I) = TWO
         ENDDO
      END IF
C      
      DO I=LFT,LLT
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      END DO
C
      DO 140 I=LFT,LLT
  140 DETT(I)=ONE_OVER_64/DET(I)
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO 150 I=LFT,LLT
      AJI1(I)=DETT(I)*JAC_59_68(I)
      AJI4(I)=DETT(I)*JAC_67_49(I)
      AJI7(I)=DETT(I)*JAC_48_57(I)
      AJI2(I)=DETT(I)*JAC_38_29(I)
      AJI5(I)=DETT(I)*JAC_19_37(I)
      AJI8(I)=DETT(I)*JAC_27_18(I)
      AJI3(I)=DETT(I)*JAC_26_35(I)
      AJI6(I)=DETT(I)*JAC_34_16(I)
      AJI9(I)=DETT(I)*JAC_15_24(I)
  150 CONTINUE
C
      DO 160 I=LFT,LLT
      AJ12(I)=AJI1(I)-AJI2(I)
      AJ45(I)=AJI4(I)-AJI5(I)
  160 AJ78(I)=AJI7(I)-AJI8(I)
      DO 170 I=LFT,LLT
      PX2(I)= AJ12(I)-AJI3(I)
      PY2(I)= AJ45(I)-AJI6(I)
      PZ2(I)= AJ78(I)-AJI9(I)
      PX4(I)=-AJ12(I)-AJI3(I)
      PY4(I)=-AJ45(I)-AJI6(I)
  170 PZ4(I)=-AJ78(I)-AJI9(I)
      DO 180 I=LFT,LLT
      AJ12(I)=AJI1(I)+AJI2(I)
      AJ45(I)=AJI4(I)+AJI5(I)
  180 AJ78(I)=AJI7(I)+AJI8(I)
      DO 190 I=LFT,LLT
      PX1(I)=-AJ12(I)-AJI3(I)
      PY1(I)=-AJ45(I)-AJI6(I)
      PZ1(I)=-AJ78(I)-AJI9(I)
      PX3(I)=AJ12(I)-AJI3(I)
      PY3(I)=AJ45(I)-AJI6(I)
  190 PZ3(I)=AJ78(I)-AJI9(I)
C
C---- for f_vis  
      JR_1(LFT:LLT)=AJI2(LFT:LLT)
      JS_1(LFT:LLT)=AJI6(LFT:LLT)
      JT_1(LFT:LLT)=AJI7(LFT:LLT)
C
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=LFT,LLT
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H1(I)=PX1(I)*HX(I,1)+ PY1(I)*HY(I,1)+PZ1(I)*HZ(I,1)
         PX2H1(I)=PX2(I)*HX(I,1)+ PY2(I)*HY(I,1)+PZ2(I)*HZ(I,1)
         PX3H1(I)=PX3(I)*HX(I,1)+ PY3(I)*HY(I,1)+PZ3(I)*HZ(I,1)
         PX4H1(I)=PX4(I)*HX(I,1)+ PY4(I)*HY(I,1)+PZ4(I)*HZ(I,1)
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=LFT,LLT
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H2(I)=PX1(I)*HX(I,2)+ PY1(I)*HY(I,2)+PZ1(I)*HZ(I,2)
         PX2H2(I)=PX2(I)*HX(I,2)+ PY2(I)*HY(I,2)+PZ2(I)*HZ(I,2)
         PX3H2(I)=PX3(I)*HX(I,2)+ PY3(I)*HY(I,2)+PZ3(I)*HZ(I,2)
         PX4H2(I)=PX4(I)*HX(I,2)+ PY4(I)*HY(I,2)+PZ4(I)*HZ(I,2)
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=LFT,LLT
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX(I,3)+ PY1(I)*HY(I,3)+PZ1(I)*HZ(I,3)
         PX2H3(I)=PX2(I)*HX(I,3)+ PY2(I)*HY(I,3)+PZ2(I)*HZ(I,3)
         PX3H3(I)=PX3(I)*HX(I,3)+ PY3(I)*HY(I,3)+PZ3(I)*HZ(I,3)
         PX4H3(I)=PX4(I)*HX(I,3)+ PY4(I)*HY(I,3)+PZ4(I)*HZ(I,3)
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=LFT,LLT
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H4(I)=PX1(I)*HX(I,4)+ PY1(I)*HY(I,4)+PZ1(I)*HZ(I,4)
         PX2H4(I)=PX2(I)*HX(I,4)+ PY2(I)*HY(I,4)+PZ2(I)*HZ(I,4)
         PX3H4(I)=PX3(I)*HX(I,4)+ PY3(I)*HY(I,4)+PZ3(I)*HZ(I,4)
         PX4H4(I)=PX4(I)*HX(I,4)+ PY4(I)*HY(I,4)+PZ4(I)*HZ(I,4)
       ENDDO
C----surface max mediane-- *16
       DO I=LFT,LLT
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
       DO I=LFT,LLT
         IF(SMAX(I)<=ZERO)THEN
           CALL ANCMSG(MSGID=173,ANMODE=ANINFO,
     .                 I1=NGL(I))
           CALL ARRET(2)
         ENDIF
         SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
Chd|====================================================================
Chd|  S8EDERICT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERICT3(OFF,DET,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   HX,    HY,    HZ,   
     .   AJ1,AJ2,AJ3,
     .   AJ4,AJ5,AJ6,
     .   AJ7,AJ8,AJ9,SMAX)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C     REAL   
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),SMAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT(MVSIZ) ,   
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ)
C-----------------------------------------------
      DO 10 I=LFT,LLT
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
   10 Z46(I)=Z6(I)-Z4(I)
C
      DO 20 I=LFT,LLT
      AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      A17(I)=X17(I)+X46(I)
      A28(I)=X28(I)+X35(I)
      B17(I)=Y17(I)+Y46(I)
      B28(I)=Y28(I)+Y35(I)
      C17(I)=Z17(I)+Z46(I)
   20 C28(I)=Z28(I)+Z35(I)
      DO 30 I=LFT,LLT
      AJ7(I)=A17(I)+A28(I)
      AJ8(I)=B17(I)+B28(I)
      AJ9(I)=C17(I)+C28(I)
      AJ1(I)=A17(I)-A28(I)
      AJ2(I)=B17(I)-B28(I)
   30 AJ3(I)=C17(I)-C28(I)
C
C     JACOBIAN
C
      DO 100 I=LFT,LLT
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
  100 JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
      DO 110 I=LFT,LLT
  110 DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
C
      CALL SCHKJAB3(OFF,DET,NGL)
C
      DO 140 I=LFT,LLT
  140 DETT(I)=ONE_OVER_64/DET(I)
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO 150 I=LFT,LLT
      AJI1(I)=DETT(I)*JAC_59_68(I)
      AJI4(I)=DETT(I)*JAC_67_49(I)
      AJI7(I)=DETT(I)*JAC_48_57(I)
      AJI2(I)=DETT(I)*JAC_38_29(I)
      AJI5(I)=DETT(I)*JAC_19_37(I)
      AJI8(I)=DETT(I)*JAC_27_18(I)
      AJI3(I)=DETT(I)*JAC_26_35(I)
      AJI6(I)=DETT(I)*JAC_34_16(I)
      AJI9(I)=DETT(I)*JAC_15_24(I)
  150 CONTINUE
C
      DO 160 I=LFT,LLT
      AJ12(I)=AJI1(I)-AJI2(I)
      AJ45(I)=AJI4(I)-AJI5(I)
  160 AJ78(I)=AJI7(I)-AJI8(I)
      DO 170 I=LFT,LLT
      PX2(I)= AJ12(I)-AJI3(I)
      PY2(I)= AJ45(I)-AJI6(I)
      PZ2(I)= AJ78(I)-AJI9(I)
      PX4(I)=-AJ12(I)-AJI3(I)
      PY4(I)=-AJ45(I)-AJI6(I)
  170 PZ4(I)=-AJ78(I)-AJI9(I)
      DO 180 I=LFT,LLT
      AJ12(I)=AJI1(I)+AJI2(I)
      AJ45(I)=AJI4(I)+AJI5(I)
  180 AJ78(I)=AJI7(I)+AJI8(I)
      DO 190 I=LFT,LLT
      PX1(I)=-AJ12(I)-AJI3(I)
      PY1(I)=-AJ45(I)-AJI6(I)
      PZ1(I)=-AJ78(I)-AJI9(I)
      PX3(I)=AJ12(I)-AJI3(I)
      PY3(I)=AJ45(I)-AJI6(I)
  190 PZ3(I)=AJ78(I)-AJI9(I)
C
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=LFT,LLT
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=LFT,LLT
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=LFT,LLT
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=LFT,LLT
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
       ENDDO
C       
C----surface max mediane-- *16
       DO I=LFT,LLT
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
       DO I=LFT,LLT
         IF(SMAX(I)<=ZERO)THEN
           CALL ANCMSG(MSGID=173,ANMODE=ANINFO,
     .                 I1=NGL(I))
           CALL ARRET(2)
         ENDIF
         SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END 
Chd|====================================================================
Chd|  S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EDERI3                      source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERIO3                     source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERIOM3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERIOT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERI_2                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8BICP11(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    NU  )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C     REAL
C                                                                     12
      my_real
     .   BXHI,BYHI,BZHI,TEMP
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
        TEMP=HALF
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX1(I)-PXC1(I))
        BYHI=TEMP*(PY1(I)-PYC1(I))
        BZHI=TEMP*(PZ1(I)-PZC1(I))
c        PX1(I)=PXC1(I)+TWO*BXHI
c        PY1(I)=PYC1(I)+TWO*BYHI
c        PZ1(I)=PZC1(I)+TWO*BZHI
        BXY1(I)=-BXHI
        BXZ1(I)=-BXHI
        BYX1(I)=-BYHI
        BYZ1(I)=-BYHI
        BZX1(I)=-BZHI
        BZY1(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX2(I)-PXC2(I))
        BYHI=TEMP*(PY2(I)-PYC2(I))
        BZHI=TEMP*(PZ2(I)-PZC2(I))
c        PX2(I)=PXC2(I)+TWO*BXHI
c        PY2(I)=PYC2(I)+TWO*BYHI
c        PZ2(I)=PZC2(I)+TWO*BZHI
        BXY2(I)=-BXHI
        BXZ2(I)=-BXHI
        BYX2(I)=-BYHI
        BYZ2(I)=-BYHI
        BZX2(I)=-BZHI
        BZY2(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX3(I)-PXC3(I))
        BYHI=TEMP*(PY3(I)-PYC3(I))
        BZHI=TEMP*(PZ3(I)-PZC3(I))
c        PX3(I)=PXC3(I)+TWO*BXHI
c        PY3(I)=PYC3(I)+TWO*BYHI
c        PZ3(I)=PZC3(I)+TWO*BZHI
        BXY3(I)=-BXHI
        BXZ3(I)=-BXHI
        BYX3(I)=-BYHI
        BYZ3(I)=-BYHI
        BZX3(I)=-BZHI
        BZY3(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX4(I)-PXC4(I))
        BYHI=TEMP*(PY4(I)-PYC4(I))
        BZHI=TEMP*(PZ4(I)-PZC4(I))
c        PX4(I)=PXC4(I)+TWO*BXHI
c        PY4(I)=PYC4(I)+TWO*BYHI
c        PZ4(I)=PZC4(I)+TWO*BZHI
        BXY4(I)=-BXHI
        BXZ4(I)=-BXHI
        BYX4(I)=-BYHI
        BYZ4(I)=-BYHI
        BZX4(I)=-BZHI
        BZY4(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX5(I)+PXC3(I))
        BYHI=TEMP*(PY5(I)+PYC3(I))
        BZHI=TEMP*(PZ5(I)+PZC3(I))
c        PX5(I)=-PXC3(I)+TWO*BXHI
c        PY5(I)=-PYC3(I)+TWO*BYHI
c        PZ5(I)=-PZC3(I)+TWO*BZHI
        BXY5(I)=-BXHI
        BXZ5(I)=-BXHI
        BYX5(I)=-BYHI
        BYZ5(I)=-BYHI
        BZX5(I)=-BZHI
        BZY5(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX6(I)+PXC4(I))
        BYHI=TEMP*(PY6(I)+PYC4(I))
        BZHI=TEMP*(PZ6(I)+PZC4(I))
c        PX6(I)=-PXC4(I)+TWO*BXHI
c        PY6(I)=-PYC4(I)+TWO*BYHI
c        PZ6(I)=-PZC4(I)+TWO*BZHI
        BXY6(I)=-BXHI
        BXZ6(I)=-BXHI
        BYX6(I)=-BYHI
        BYZ6(I)=-BYHI
        BZX6(I)=-BZHI
        BZY6(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX7(I)+PXC1(I))
        BYHI=TEMP*(PY7(I)+PYC1(I))
        BZHI=TEMP*(PZ7(I)+PZC1(I))
c        PX7(I)=-PXC1(I)+TWO*BXHI
c        PY7(I)=-PYC1(I)+TWO*BYHI
c        PZ7(I)=-PZC1(I)+TWO*BZHI
        BXY7(I)=-BXHI
        BXZ7(I)=-BXHI
        BYX7(I)=-BYHI
        BYZ7(I)=-BYHI
        BZX7(I)=-BZHI
        BZY7(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
c        TEMP=NU(I)
        BXHI=TEMP*(PX8(I)+PXC2(I))
        BYHI=TEMP*(PY8(I)+PYC2(I))
        BZHI=TEMP*(PZ8(I)+PZC2(I))
c        PX8(I)=-PXC2(I)+TWO*BXHI
c        PY8(I)=-PYC2(I)+TWO*BYHI
c        PZ8(I)=-PZC2(I)+TWO*BZHI
        BXY8(I)=-BXHI
        BXZ8(I)=-BXHI
        BYX8(I)=-BYHI
        BYZ8(I)=-BYHI
        BZX8(I)=-BZHI
        BZY8(I)=-BZHI
       ENDDO
       DO I=LFT,LLT
        BXX1(I) = PX1(I)
        BYY1(I) = PY1(I)
        BZZ1(I) = PZ1(I)
        BXX2(I) = PX2(I)
        BYY2(I) = PY2(I)
        BZZ2(I) = PZ2(I)
        BXX3(I) = PX3(I)
        BYY3(I) = PY3(I)
        BZZ3(I) = PZ3(I)
        BXX4(I) = PX4(I)
        BYY4(I) = PY4(I)
        BZZ4(I) = PZ4(I)
        BXX5(I) = PX5(I)
        BYY5(I) = PY5(I)
        BZZ5(I) = PZ5(I)
        BXX6(I) = PX6(I)
        BYY6(I) = PY6(I)
        BZZ6(I) = PZ6(I)
        BXX7(I) = PX7(I)
        BYY7(I) = PY7(I)
        BZZ7(I) = PZ7(I)
        BXX8(I) = PX8(I)
        BYY8(I) = PY8(I)
        BZZ8(I) = PZ8(I)
       ENDDO
C
      RETURN
      END 
Chd|====================================================================
Chd|  S8EJACIP3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EJACIP3(
     .    HX,    HY,    HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    AJ1,AJ2,AJ3,
     .    AJ4,AJ5,AJ6,
     .    AJ7,AJ8,AJ9)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   AJ1(MVSIZ,8),AJ2(MVSIZ,8),AJ3(MVSIZ,8),
     .   AJ4(MVSIZ,8),AJ5(MVSIZ,8),AJ6(MVSIZ,8),
     .   AJ7(MVSIZ,8),AJ8(MVSIZ,8),AJ9(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IP
C     REAL
C                                                                     12
      my_real
     .   HX1PG(MVSIZ), HX2PG(MVSIZ), HX3PG(MVSIZ),HX4PG2(MVSIZ),
     .   HY1PG(MVSIZ), HY2PG(MVSIZ), HY3PG(MVSIZ),HY4PG2(MVSIZ),
     .   HZ1PG(MVSIZ), HZ2PG(MVSIZ), HZ3PG(MVSIZ),HZ4PG2(MVSIZ),
     .   PG2,PG
       PARAMETER (PG=.577350269189625D0)
C-----------------------------------------------
      PG2=PG*PG
C
      DO I=LFT,LLT
       HX1PG(I)=HX(I,1)*PG
       HX2PG(I)=HX(I,2)*PG
       HX3PG(I)=HX(I,3)*PG
       HX4PG2(I)=HX(I,4)*PG2
       HY1PG(I)=HY(I,1)*PG
       HY2PG(I)=HY(I,2)*PG
       HY3PG(I)=HY(I,3)*PG
       HY4PG2(I)=HY(I,4)*PG2
       HZ1PG(I)=HZ(I,1)*PG
       HZ2PG(I)=HZ(I,2)*PG
       HZ3PG(I)=HZ(I,3)*PG
       HZ4PG2(I)=HZ(I,4)*PG2
      ENDDO
      IP=1
C - - -	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=2	  
C + - -	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=3	  
C - + -	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=4	  
C + + -	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=5	  
C - - +	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=6	  
C + - +	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=7	  
C - + +	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=8	  
C + + +	  
      DO I=LFT,LLT
       AJ1(I,IP)=CJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EDERIO3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERIO3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    HX,    HY,    HZ,   
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    AJP1,AJP2,AJP3,
     .    AJP4,AJP5,AJP6,
     .    AJP7,AJP8,AJP9,
     .    AJ4,AJ5,AJ6,
     .    AJ7,AJ8,AJ9,
     .    PR ,PS ,PT ,
     .    SMAX,DELTAX,ICP ,IDEG,NU  )
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJP1(*),AJP2(*),AJP3(*),
     .   AJP4(*),AJP5(*),AJP6(*),
     .   AJP7(*),AJP8(*),AJP9(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   PR(8),PS(8),PT(8),
     .   SMAX(*),DELTAX(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4,
     .   BXHI,BYHI,BZHI,TMP,DETI
C-----------------------------------------------
       AJ1(LFT:LLT)=AJP1(LFT:LLT)
       AJ2(LFT:LLT)=AJP2(LFT:LLT)
       AJ3(LFT:LLT)=AJP3(LFT:LLT)
       AJ4(LFT:LLT)=AJP4(LFT:LLT)
       AJ5(LFT:LLT)=AJP5(LFT:LLT)
       AJ6(LFT:LLT)=AJP6(LFT:LLT)
       AJ7(LFT:LLT)=AJP7(LFT:LLT)
       AJ8(LFT:LLT)=AJP8(LFT:LLT)
       AJ9(LFT:LLT)=AJP9(LFT:LLT)

C     JACOBIAN
C
      DO I=LFT,LLT
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
       JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
       JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
       JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      VOL(I)= WI*DET(I)
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C
      ICOR = 0
      DO I=LFT,LLT
        OFF(I)=OFFG(I)
        IF(OFF(I) ==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) VOL(I)=ONE
        ELSEIF (VOL(I)<=ZERO) THEN
         VOL(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=LFT,LLT
          IF(VOL(I)<=ZERO.AND.OFF(I)/=ZERO)THEN
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV==1) THEN
#include "lockon.inc"
            WRITE(ISTDO,2000) NGL(I)
            WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1   
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
c      DO I=LFT,LLT
c        DETT(I)=ONE_OVER_512/DET(I)
c      ENDDO
C
      DO I=LFT,LLT
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
cc-----pxI
      DO I=LFT,LLT
        A1PR1=AJI1(I)*PR(1)
        A1PR3=AJI1(I)*PR(3)
        A1PR5=AJI1(I)*PR(5)
        A1PR7=AJI1(I)*PR(7)
        A2PS1=AJI2(I)*PS(1)
        A2PS2=AJI2(I)*PS(2)
        A2PS5=AJI2(I)*PS(5)
        A2PS6=AJI2(I)*PS(6)
        A3PT1=AJI3(I)*PT(1)
        A3PT2=AJI3(I)*PT(2)
        A3PT3=AJI3(I)*PT(3)
        A3PT4=AJI3(I)*PT(4)
       PX1(I)= A1PR1+A2PS1+A3PT1
       PX2(I)=-A1PR1+A2PS2+A3PT2
       PX3(I)= A1PR3-A2PS2+A3PT3
       PX4(I)=-A1PR3-A2PS1+A3PT4
       PX5(I)= A1PR5+A2PS5-A3PT1
       PX6(I)=-A1PR5+A2PS6-A3PT2
       PX7(I)= A1PR7-A2PS6-A3PT3
       PX8(I)=-A1PR7-A2PS5-A3PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A4PR1=AJI4(I)*PR(1)
        A4PR3=AJI4(I)*PR(3)
        A4PR5=AJI4(I)*PR(5)
        A4PR7=AJI4(I)*PR(7)
        A5PS1=AJI5(I)*PS(1)
        A5PS2=AJI5(I)*PS(2)
        A5PS5=AJI5(I)*PS(5)
        A5PS6=AJI5(I)*PS(6)
        A6PT1=AJI6(I)*PT(1)
        A6PT2=AJI6(I)*PT(2)
        A6PT3=AJI6(I)*PT(3)
        A6PT4=AJI6(I)*PT(4)
       PY1(I)= A4PR1+A5PS1+A6PT1
       PY2(I)=-A4PR1+A5PS2+A6PT2
       PY3(I)= A4PR3-A5PS2+A6PT3
       PY4(I)=-A4PR3-A5PS1+A6PT4
       PY5(I)= A4PR5+A5PS5-A6PT1
       PY6(I)=-A4PR5+A5PS6-A6PT2
       PY7(I)= A4PR7-A5PS6-A6PT3
       PY8(I)=-A4PR7-A5PS5-A6PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A7PR1=AJI7(I)*PR(1)
        A7PR3=AJI7(I)*PR(3)
        A7PR5=AJI7(I)*PR(5)
        A7PR7=AJI7(I)*PR(7)
        A8PS1=AJI8(I)*PS(1)
        A8PS2=AJI8(I)*PS(2)
        A8PS5=AJI8(I)*PS(5)
        A8PS6=AJI8(I)*PS(6)
        A9PT1=AJI9(I)*PT(1)
        A9PT2=AJI9(I)*PT(2)
        A9PT3=AJI9(I)*PT(3)
        A9PT4=AJI9(I)*PT(4)
       PZ1(I)= A7PR1+A8PS1+A9PT1
       PZ2(I)=-A7PR1+A8PS2+A9PT2
       PZ3(I)= A7PR3-A8PS2+A9PT3
       PZ4(I)=-A7PR3-A8PS1+A9PT4
       PZ5(I)= A7PR5+A8PS5-A9PT1
       PZ6(I)=-A7PR5+A8PS6-A9PT2
       PZ7(I)= A7PR7-A8PS6-A9PT3
       PZ8(I)=-A7PR7-A8PS5-A9PT4
      ENDDO
C
      IF (IDTS6==0) THEN
       DO I=LFT,LLT
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
       ENDDO
      ELSE
      DO I=LFT,LLT
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      END IF
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    NU  )
      ENDIF
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
Chd|====================================================================
Chd|  S8EDERIPR3                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERIPR3(OFFG,VOLDP,NGL,WI,
     .    AJ1,AJ2,AJ3,
     .    AJ4,AJ5,AJ6,
     .    AJ7,AJ8,AJ9,
     .    AJI1,AJI2,AJI3,
     .    AJI4,AJI5,AJI6,
     .    AJI7,AJI8,AJI9,NNEGA,INDEX,IPT)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNEGA,INDEX(*),IPT
C     REAL
      my_real
     .   OFFG(*),WI,
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   AJI1(*), AJI2(*), AJI3(*),
     .   AJI4(*), AJI5(*), AJI6(*),
     .   AJI7(*), AJI8(*), AJI9(*)
      DOUBLE PRECISION 
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   VOL(MVSIZ),DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ)
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
C
      DO I=LFT,LLT
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
       JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
       JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
       JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
      DETDP=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DET(I)=DETDP
      VOLDP(I)= WI*DETDP
      VOL(I)= VOLDP(I)
      ENDDO
C
      ICOR = 0
      DO I=LFT,LLT
        IF(OFFG(I)==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) VOL(I)=ONE
        ELSEIF (VOL(I)<=ZERO ) THEN
          ICOR=1
        ENDIF
      ENDDO
      IF (ICOR>0.AND.INCONV==1) THEN
       DO I=LFT,LLT
        IF (OFFG(I) /= TWO .AND.OFFG(I) /= ZERO ) THEN
         NNEGA=NNEGA+1
         INDEX(NNEGA)=I
         OFFG(I) = TWO
        END IF
       ENDDO
      END IF
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=LFT,LLT
          IF(VOL(I)<=ZERO)THEN
            VOLDP(I)= EM20
            DET(I)= EM20
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV==1.AND.ABS(OFFG(I))/=TWO) THEN
c#include "lockon.inc"
c            WRITE(ISTDO,2000) NGL(I)
c            WRITE(IOUT ,2000) NGL(I)
c#include "lockoff.inc"
c            IDEL7NOK = 1   
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
C
      DO I=LFT,LLT
       DETT(I)=ONE_OVER_512/DET(I)
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
Chd|====================================================================
Chd|  S8EDERIOM3                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERIOM3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    AJI1,AJI2,AJI3,
     .    AJI4,AJI5,AJI6,
     .    AJI7,AJI8,AJI9,
     .    AJ4,AJ5,AJ6,AJ7,AJ8,AJ9,
     .    RX       ,RY      ,RZ      ,SX      ,SY      ,SZ      ,
     .    PR ,PS ,PT ,
     .    SMAX,DELTAX,ICP ,IDEG,NU  ,VOLP)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJI1(*), AJI2(*), AJI3(*),
     .   AJI4(*), AJI5(*), AJI6(*),
     .   AJI7(*), AJI8(*), AJI9(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   RX(*)    ,RY(*)    ,RZ(*)    ,
     .   SX(*)    ,SY(*)    ,SZ(*)    ,
     .   PR(8),PS(8),PT(8),
     .   SMAX(*),DELTAX(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*),VOLP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4,
     .   BXHI,BYHI,BZHI,TMP,DETI
C-----------------------------------------------
      DO I=LFT,LLT
       RX(I) = AJ4(I)
       RY(I) = AJ5(I)
       RZ(I) = AJ6(I)
       SX(I) = AJ7(I)
       SY(I) = AJ8(I)
       SZ(I) = AJ9(I)
      ENDDO
cc-----pxI
      DO I=LFT,LLT
        A1PR1=AJI1(I)*PR(1)
        A1PR3=AJI1(I)*PR(3)
        A1PR5=AJI1(I)*PR(5)
        A1PR7=AJI1(I)*PR(7)
        A2PS1=AJI2(I)*PS(1)
        A2PS2=AJI2(I)*PS(2)
        A2PS5=AJI2(I)*PS(5)
        A2PS6=AJI2(I)*PS(6)
        A3PT1=AJI3(I)*PT(1)
        A3PT2=AJI3(I)*PT(2)
        A3PT3=AJI3(I)*PT(3)
        A3PT4=AJI3(I)*PT(4)
       PX1(I)= A1PR1+A2PS1+A3PT1
       PX2(I)=-A1PR1+A2PS2+A3PT2
       PX3(I)= A1PR3-A2PS2+A3PT3
       PX4(I)=-A1PR3-A2PS1+A3PT4
       PX5(I)= A1PR5+A2PS5-A3PT1
       PX6(I)=-A1PR5+A2PS6-A3PT2
       PX7(I)= A1PR7-A2PS6-A3PT3
       PX8(I)=-A1PR7-A2PS5-A3PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A4PR1=AJI4(I)*PR(1)
        A4PR3=AJI4(I)*PR(3)
        A4PR5=AJI4(I)*PR(5)
        A4PR7=AJI4(I)*PR(7)
        A5PS1=AJI5(I)*PS(1)
        A5PS2=AJI5(I)*PS(2)
        A5PS5=AJI5(I)*PS(5)
        A5PS6=AJI5(I)*PS(6)
        A6PT1=AJI6(I)*PT(1)
        A6PT2=AJI6(I)*PT(2)
        A6PT3=AJI6(I)*PT(3)
        A6PT4=AJI6(I)*PT(4)
       PY1(I)= A4PR1+A5PS1+A6PT1
       PY2(I)=-A4PR1+A5PS2+A6PT2
       PY3(I)= A4PR3-A5PS2+A6PT3
       PY4(I)=-A4PR3-A5PS1+A6PT4
       PY5(I)= A4PR5+A5PS5-A6PT1
       PY6(I)=-A4PR5+A5PS6-A6PT2
       PY7(I)= A4PR7-A5PS6-A6PT3
       PY8(I)=-A4PR7-A5PS5-A6PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A7PR1=AJI7(I)*PR(1)
        A7PR3=AJI7(I)*PR(3)
        A7PR5=AJI7(I)*PR(5)
        A7PR7=AJI7(I)*PR(7)
        A8PS1=AJI8(I)*PS(1)
        A8PS2=AJI8(I)*PS(2)
        A8PS5=AJI8(I)*PS(5)
        A8PS6=AJI8(I)*PS(6)
        A9PT1=AJI9(I)*PT(1)
        A9PT2=AJI9(I)*PT(2)
        A9PT3=AJI9(I)*PT(3)
        A9PT4=AJI9(I)*PT(4)
       PZ1(I)= A7PR1+A8PS1+A9PT1
       PZ2(I)=-A7PR1+A8PS2+A9PT2
       PZ3(I)= A7PR3-A8PS2+A9PT3
       PZ4(I)=-A7PR3-A8PS1+A9PT4
       PZ5(I)= A7PR5+A8PS5-A9PT1
       PZ6(I)=-A7PR5+A8PS6-A9PT2
       PZ7(I)= A7PR7-A8PS6-A9PT3
       PZ8(I)=-A7PR7-A8PS5-A9PT4
      ENDDO
C
      DO I=LFT,LLT
       OFF(I) = OFFG(I)
       VOL(I) = VOLP(I)
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*VOL(I)*SMAX(I)/WI)
      ENDDO
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    NU  )
      ENDIF
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
Chd|====================================================================
Chd|  S8EDERICM3                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERICM3(DET,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   HX,    HY,    HZ,   
     .   AJ1,AJ2,AJ3,
     .   AJ4,AJ5,AJ6,
     .   AJ7,AJ8,AJ9,SMAX,SAV,OFFG,NNEGA,INDEX,NEL)
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
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNEGA,INDEX(MVSIZ),NEL
C     REAL
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   SAV(NEL,21)
     
      my_real
     .   DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),SMAX(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT(MVSIZ) ,   
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ)
C-----------------------------------------------
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
#include "lockon.inc"
           IF(ISMSTR<10) THEN
              CALL ANCMSG(MSGID=260,ANMODE=ANINFO,
     .                    I1=NGL(I))
	   ELSE
              CALL ANCMSG(MSGID=262,ANMODE=ANINFO,
     .                    I1=NGL(I))
	   END IF
#include "lockoff.inc"
C
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
C
            AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
            AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
            AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
            A17(I)=X17(I)+X46(I)
            A28(I)=X28(I)+X35(I)
            B17(I)=Y17(I)+Y46(I)
            B28(I)=Y28(I)+Y35(I)
            C17(I)=Z17(I)+Z46(I)
            C28(I)=Z28(I)+Z35(I)
            AJ7(I)=A17(I)+A28(I)
            AJ8(I)=B17(I)+B28(I)
            AJ9(I)=C17(I)+C28(I)
            AJ1(I)=A17(I)-A28(I)
            AJ2(I)=B17(I)-B28(I)
            AJ3(I)=C17(I)-C28(I)
C	    
            JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
            JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
            JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
           DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
         ENDDO
      END IF
C      
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
          JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
          JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
          JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
          JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
          JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
          JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
          DETT(I)=ONE_OVER_64/DET(I)
          AJI1(I)=DETT(I)*JAC_59_68(I)
          AJI4(I)=DETT(I)*JAC_67_49(I)
          AJI7(I)=DETT(I)*JAC_48_57(I)
          AJI2(I)=DETT(I)*JAC_38_29(I)
          AJI5(I)=DETT(I)*JAC_19_37(I)
          AJI8(I)=DETT(I)*JAC_27_18(I)
          AJI3(I)=DETT(I)*JAC_26_35(I)
          AJI6(I)=DETT(I)*JAC_34_16(I)
          AJI9(I)=DETT(I)*JAC_15_24(I)
          AJ12(I)=AJI1(I)-AJI2(I)
          AJ45(I)=AJI4(I)-AJI5(I)
          AJ78(I)=AJI7(I)-AJI8(I)
          PX2(I)= AJ12(I)-AJI3(I)
          PY2(I)= AJ45(I)-AJI6(I)
          PZ2(I)= AJ78(I)-AJI9(I)
          PX4(I)=-AJ12(I)-AJI3(I)
          PY4(I)=-AJ45(I)-AJI6(I)
          PZ4(I)=-AJ78(I)-AJI9(I)
          AJ12(I)=AJI1(I)+AJI2(I)
          AJ45(I)=AJI4(I)+AJI5(I)
          AJ78(I)=AJI7(I)+AJI8(I)
          PX1(I)=-AJ12(I)-AJI3(I)
          PY1(I)=-AJ45(I)-AJI6(I)
          PZ1(I)=-AJ78(I)-AJI9(I)
          PX3(I)=AJ12(I)-AJI3(I)
          PY3(I)=AJ45(I)-AJI6(I)
          PZ3(I)=AJ78(I)-AJI9(I)
         END DO
C
C mode 1-4
C 1 1 -1 -1 -1 -1 1 1
#include "vectorize.inc"
       DO J=1,NNEGA
         I = INDEX(J)
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
C	 
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
C	 
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
C	 
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
C       
      RETURN
C
 3000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : 3D-ELEMENT NB:',I10/,
     +        ' ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/) 
      END
Chd|====================================================================
Chd|  S8EDERICTO3                   source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERICTO3(OFF,DET,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   JAC_I)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL   
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ)
C-----------------------------------------------
C  
      DO I=LFT,LLT
      AJI1(I)=JAC_I(1,I)
      AJI4(I)=JAC_I(4,I)
      AJI7(I)=JAC_I(7,I)
      AJI2(I)=JAC_I(2,I)
      AJI5(I)=JAC_I(5,I)
      AJI8(I)=JAC_I(8,I)
      AJI3(I)=JAC_I(3,I)
      AJI6(I)=JAC_I(6,I)
      AJI9(I)=JAC_I(9,I)
      DET(I) =JAC_I(10,I)
      END DO
C
      DO 160 I=LFT,LLT
      AJ12(I)=AJI1(I)-AJI2(I)
      AJ45(I)=AJI4(I)-AJI5(I)
  160 AJ78(I)=AJI7(I)-AJI8(I)
      DO 170 I=LFT,LLT
      PX2(I)= AJ12(I)-AJI3(I)
      PY2(I)= AJ45(I)-AJI6(I)
      PZ2(I)= AJ78(I)-AJI9(I)
      PX4(I)=-AJ12(I)-AJI3(I)
      PY4(I)=-AJ45(I)-AJI6(I)
  170 PZ4(I)=-AJ78(I)-AJI9(I)
      DO 180 I=LFT,LLT
      AJ12(I)=AJI1(I)+AJI2(I)
      AJ45(I)=AJI4(I)+AJI5(I)
  180 AJ78(I)=AJI7(I)+AJI8(I)
      DO 190 I=LFT,LLT
      PX1(I)=-AJ12(I)-AJI3(I)
      PY1(I)=-AJ45(I)-AJI6(I)
      PZ1(I)=-AJ78(I)-AJI9(I)
      PX3(I)=AJ12(I)-AJI3(I)
      PY3(I)=AJ45(I)-AJI6(I)
  190 PZ3(I)=AJ78(I)-AJI9(I)
C       
      RETURN
      END 
Chd|====================================================================
Chd|  S8EDERIOT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERIOT3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    HX,    HY,    HZ,   
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    AJP1,AJP2,AJP3,
     .    AJP4,AJP5,AJP6,
     .    AJP7,AJP8,AJP9,
     .    AJ4,AJ5,AJ6,
     .    AJ7,AJ8,AJ9,
     .    PR ,PS ,PT ,
     .    SMAX,DELTAX,ICP ,IDEG,NU  ,JAC_I)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJP1(*),AJP2(*),AJP3(*),
     .   AJP4(*),AJP5(*),AJP6(*),
     .   AJP7(*),AJP8(*),AJP9(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   PR(8),PS(8),PT(8),
     .   SMAX(*),DELTAX(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*),JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4,
     .   BXHI,BYHI,BZHI,TMP,DETI
C-----------------------------------------------
      IF (ISMSTR==11) THEN
       AJ1(LFT:LLT)=AJP1(LFT:LLT)
       AJ2(LFT:LLT)=AJP2(LFT:LLT)
       AJ3(LFT:LLT)=AJP3(LFT:LLT)
       AJ4(LFT:LLT)=AJP4(LFT:LLT)
       AJ5(LFT:LLT)=AJP5(LFT:LLT)
       AJ6(LFT:LLT)=AJP6(LFT:LLT)
       AJ7(LFT:LLT)=AJP7(LFT:LLT)
       AJ8(LFT:LLT)=AJP8(LFT:LLT)
       AJ9(LFT:LLT)=AJP9(LFT:LLT)

C     JACOBIAN
C
      DO I=LFT,LLT
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
       JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
       JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
       JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      VOL(I)= WI*DET(I)
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C
      DO I=LFT,LLT
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C
      DO I=LFT,LLT
       IF (IDTS6==0.OR.IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
C------ ISMSTR =10       
      ELSE
      DO I=LFT,LLT
       AJI1(I)=JAC_I(1,I)
       AJI4(I)=JAC_I(4,I)
       AJI7(I)=JAC_I(7,I)
       AJI2(I)=JAC_I(2,I)
       AJI5(I)=JAC_I(5,I)
       AJI8(I)=JAC_I(8,I)
       AJI3(I)=JAC_I(3,I)
       AJI6(I)=JAC_I(6,I)
       AJI9(I)=JAC_I(9,I)
       VOL(I) =JAC_I(10,I)
      END DO
      END IF !(ISMSTR==11)
cc-----pxI
      DO I=LFT,LLT
        A1PR1=AJI1(I)*PR(1)
        A1PR3=AJI1(I)*PR(3)
        A1PR5=AJI1(I)*PR(5)
        A1PR7=AJI1(I)*PR(7)
        A2PS1=AJI2(I)*PS(1)
        A2PS2=AJI2(I)*PS(2)
        A2PS5=AJI2(I)*PS(5)
        A2PS6=AJI2(I)*PS(6)
        A3PT1=AJI3(I)*PT(1)
        A3PT2=AJI3(I)*PT(2)
        A3PT3=AJI3(I)*PT(3)
        A3PT4=AJI3(I)*PT(4)
       PX1(I)= A1PR1+A2PS1+A3PT1
       PX2(I)=-A1PR1+A2PS2+A3PT2
       PX3(I)= A1PR3-A2PS2+A3PT3
       PX4(I)=-A1PR3-A2PS1+A3PT4
       PX5(I)= A1PR5+A2PS5-A3PT1
       PX6(I)=-A1PR5+A2PS6-A3PT2
       PX7(I)= A1PR7-A2PS6-A3PT3
       PX8(I)=-A1PR7-A2PS5-A3PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A4PR1=AJI4(I)*PR(1)
        A4PR3=AJI4(I)*PR(3)
        A4PR5=AJI4(I)*PR(5)
        A4PR7=AJI4(I)*PR(7)
        A5PS1=AJI5(I)*PS(1)
        A5PS2=AJI5(I)*PS(2)
        A5PS5=AJI5(I)*PS(5)
        A5PS6=AJI5(I)*PS(6)
        A6PT1=AJI6(I)*PT(1)
        A6PT2=AJI6(I)*PT(2)
        A6PT3=AJI6(I)*PT(3)
        A6PT4=AJI6(I)*PT(4)
       PY1(I)= A4PR1+A5PS1+A6PT1
       PY2(I)=-A4PR1+A5PS2+A6PT2
       PY3(I)= A4PR3-A5PS2+A6PT3
       PY4(I)=-A4PR3-A5PS1+A6PT4
       PY5(I)= A4PR5+A5PS5-A6PT1
       PY6(I)=-A4PR5+A5PS6-A6PT2
       PY7(I)= A4PR7-A5PS6-A6PT3
       PY8(I)=-A4PR7-A5PS5-A6PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A7PR1=AJI7(I)*PR(1)
        A7PR3=AJI7(I)*PR(3)
        A7PR5=AJI7(I)*PR(5)
        A7PR7=AJI7(I)*PR(7)
        A8PS1=AJI8(I)*PS(1)
        A8PS2=AJI8(I)*PS(2)
        A8PS5=AJI8(I)*PS(5)
        A8PS6=AJI8(I)*PS(6)
        A9PT1=AJI9(I)*PT(1)
        A9PT2=AJI9(I)*PT(2)
        A9PT3=AJI9(I)*PT(3)
        A9PT4=AJI9(I)*PT(4)
       PZ1(I)= A7PR1+A8PS1+A9PT1
       PZ2(I)=-A7PR1+A8PS2+A9PT2
       PZ3(I)= A7PR3-A8PS2+A9PT3
       PZ4(I)=-A7PR3-A8PS1+A9PT4
       PZ5(I)= A7PR5+A8PS5-A9PT1
       PZ6(I)=-A7PR5+A8PS6-A9PT2
       PZ7(I)= A7PR7-A8PS6-A9PT3
       PZ8(I)=-A7PR7-A8PS5-A9PT4
      ENDDO
C
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    NU  )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EPRST_INI                   source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EPRST_INI(PR ,PS ,PT )
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
C     REAL
      my_real
     .   PR(8,8),PS(8,8),PT(8,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IP
C     REAL
C                                                                     12
      my_real
     .   PG2,KSI(8),ETA(8),ZETA(8),ETAZETA(8),KSIETA(8),KSIZETA(8),PG,MPG
       PARAMETER (PG=.577350269189625D0)
       PARAMETER (MPG=-PG)
      DATA KSI / MPG, PG, MPG, PG, MPG, PG, MPG, PG/
      DATA ETA / MPG,MPG,  PG, PG, MPG, MPG, PG, PG/
      DATA ZETA / MPG,MPG, MPG, MPG, PG, PG, PG, PG/
C-----------------------------------------------
      PG2=PG*PG
      ETAZETA(1) = PG2
      ETAZETA(2) = PG2
      ETAZETA(3) = -PG2
      ETAZETA(4) = -PG2
      ETAZETA(5) = -PG2
      ETAZETA(6) = -PG2
      ETAZETA(7) = PG2
      ETAZETA(8) = PG2
C
      KSIETA(1) = PG2
      KSIETA(2) = -PG2
      KSIETA(3) = -PG2
      KSIETA(4) = PG2
      KSIETA(5) = PG2
      KSIETA(6) = -PG2
      KSIETA(7) = -PG2
      KSIETA(8) = PG2
C
      KSIZETA(1) = PG2
      KSIZETA(2) = -PG2
      KSIZETA(3) = PG2
      KSIZETA(4) = -PG2
      KSIZETA(5) = -PG2
      KSIZETA(6) = PG2
      KSIZETA(7) = -PG2
      KSIZETA(8) = PG2
C------NI,ksi-----ETA(IP),ZETA(IP),KSIETA(IP).....--
      DO IP = 1,8
       PR(1,IP)=-(ONE-ETA(IP)-ZETA(IP)+ETAZETA(IP))
       PR(2,IP)=-PR(1,IP)
       PR(3,IP)= ONE+ETA(IP)-ZETA(IP)-ETAZETA(IP)
       PR(4,IP)=-PR(3,IP)
       PR(5,IP)=-(ONE-ETA(IP)+ZETA(IP)-ETAZETA(IP))
       PR(6,IP)=-PR(5,IP)
       PR(7,IP)= ONE+ETA(IP)+ZETA(IP)+ETAZETA(IP)
       PR(8,IP)=-PR(7,IP)
       PS(1,IP)=-(ONE-KSI(IP)-ZETA(IP)+KSIZETA(IP))
       PS(2,IP)=-(ONE+KSI(IP)-ZETA(IP)-KSIZETA(IP))
       PS(3,IP)=-PS(2,IP)
       PS(4,IP)=-PS(1,IP)
       PS(5,IP)=-(ONE-KSI(IP)+ZETA(IP)-KSIZETA(IP))
       PS(6,IP)=-(ONE+KSI(IP)+ZETA(IP)+KSIZETA(IP))
       PS(7,IP)=-PS(6,IP)
       PS(8,IP)=-PS(5,IP)
       PT(1,IP)=-(ONE-KSI(IP)-ETA(IP)+KSIETA(IP))
       PT(2,IP)=-(ONE+KSI(IP)-ETA(IP)-KSIETA(IP))
       PT(3,IP)=-(ONE+KSI(IP)+ETA(IP)+KSIETA(IP))
       PT(4,IP)=-(ONE-KSI(IP)+ETA(IP)-KSIETA(IP))
       PT(5,IP)=-PT(1,IP)
       PT(6,IP)=-PT(2,IP)
       PT(7,IP)=-PT(3,IP)
       PT(8,IP)=-PT(4,IP)
      END DO !IP = 1,8
      
      RETURN
      END
Chd|====================================================================
Chd|  S8EDERIG3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERIG3(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    AJI1,AJI2,AJI3,
     .    AJI4,AJI5,AJI6,
     .    AJI7,AJI8,AJI9,
     .    PR ,PS ,PT )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJI1(*), AJI2(*), AJI3(*),
     .   AJI4(*), AJI5(*), AJI6(*),
     .   AJI7(*), AJI8(*), AJI9(*),
     .   PR(8),PS(8),PT(8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C     REAL
C                                                                     12
      my_real
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4
C-----------------------------------------------
cc-----pxI
      DO I=LFT,LLT
        A1PR1=AJI1(I)*PR(1)
        A1PR3=AJI1(I)*PR(3)
        A1PR5=AJI1(I)*PR(5)
        A1PR7=AJI1(I)*PR(7)
        A2PS1=AJI2(I)*PS(1)
        A2PS2=AJI2(I)*PS(2)
        A2PS5=AJI2(I)*PS(5)
        A2PS6=AJI2(I)*PS(6)
        A3PT1=AJI3(I)*PT(1)
        A3PT2=AJI3(I)*PT(2)
        A3PT3=AJI3(I)*PT(3)
        A3PT4=AJI3(I)*PT(4)
       PX1(I)= A1PR1+A2PS1+A3PT1
       PX2(I)=-A1PR1+A2PS2+A3PT2
       PX3(I)= A1PR3-A2PS2+A3PT3
       PX4(I)=-A1PR3-A2PS1+A3PT4
       PX5(I)= A1PR5+A2PS5-A3PT1
       PX6(I)=-A1PR5+A2PS6-A3PT2
       PX7(I)= A1PR7-A2PS6-A3PT3
       PX8(I)=-A1PR7-A2PS5-A3PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A4PR1=AJI4(I)*PR(1)
        A4PR3=AJI4(I)*PR(3)
        A4PR5=AJI4(I)*PR(5)
        A4PR7=AJI4(I)*PR(7)
        A5PS1=AJI5(I)*PS(1)
        A5PS2=AJI5(I)*PS(2)
        A5PS5=AJI5(I)*PS(5)
        A5PS6=AJI5(I)*PS(6)
        A6PT1=AJI6(I)*PT(1)
        A6PT2=AJI6(I)*PT(2)
        A6PT3=AJI6(I)*PT(3)
        A6PT4=AJI6(I)*PT(4)
       PY1(I)= A4PR1+A5PS1+A6PT1
       PY2(I)=-A4PR1+A5PS2+A6PT2
       PY3(I)= A4PR3-A5PS2+A6PT3
       PY4(I)=-A4PR3-A5PS1+A6PT4
       PY5(I)= A4PR5+A5PS5-A6PT1
       PY6(I)=-A4PR5+A5PS6-A6PT2
       PY7(I)= A4PR7-A5PS6-A6PT3
       PY8(I)=-A4PR7-A5PS5-A6PT4
      ENDDO
C
      DO I=LFT,LLT
C
        A7PR1=AJI7(I)*PR(1)
        A7PR3=AJI7(I)*PR(3)
        A7PR5=AJI7(I)*PR(5)
        A7PR7=AJI7(I)*PR(7)
        A8PS1=AJI8(I)*PS(1)
        A8PS2=AJI8(I)*PS(2)
        A8PS5=AJI8(I)*PS(5)
        A8PS6=AJI8(I)*PS(6)
        A9PT1=AJI9(I)*PT(1)
        A9PT2=AJI9(I)*PT(2)
        A9PT3=AJI9(I)*PT(3)
        A9PT4=AJI9(I)*PT(4)
       PZ1(I)= A7PR1+A8PS1+A9PT1
       PZ2(I)=-A7PR1+A8PS2+A9PT2
       PZ3(I)= A7PR3-A8PS2+A9PT3
       PZ4(I)=-A7PR3-A8PS1+A9PT4
       PZ5(I)= A7PR5+A8PS5-A9PT1
       PZ6(I)=-A7PR5+A8PS6-A9PT2
       PZ7(I)= A7PR7-A8PS6-A9PT3
       PZ8(I)=-A7PR7-A8PS5-A9PT4
      ENDDO
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EA2BP8                      source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EDERISH2                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EA2BP8(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    P0X1, P0X2, P0X3, P0X4, P0X5, P0X6, P0X7, P0X8)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   P0X1(*), P0X2(*), P0X3(*), P0X4(*),
     .   P0X5(*), P0X6(*), P0X7(*), P0X8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C      
        PX1(LFT:LLT) = P0X1(LFT:LLT)  
        PX2(LFT:LLT) = P0X2(LFT:LLT)  
        PX3(LFT:LLT) = P0X3(LFT:LLT)  
        PX4(LFT:LLT) = P0X4(LFT:LLT)  
        PX5(LFT:LLT) = P0X5(LFT:LLT)  
        PX6(LFT:LLT) = P0X6(LFT:LLT)  
        PX7(LFT:LLT) = P0X7(LFT:LLT)  
        PX8(LFT:LLT) = P0X8(LFT:LLT)  
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EDERI_2                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERI_2(OFFG,OFF,VOL,
     .    KSI,ETA,ZETA,WI,
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    AJ4,AJ5,AJ6,AJ7,AJ8,AJ9,
     .    RX ,RY ,RZ ,SX ,SY ,SZ ,
     .    SMAX,DELTAX,ICP ,IDEG,NU  ,VOLP)
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
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   RX(*)    ,RY(*)    ,RZ(*)    ,
     .   SX(*)    ,SY(*)    ,SZ(*)    ,
     .   SMAX(*),DELTAX(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*)
      DOUBLE PRECISION 
     .   VOLP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4,
     .   BXHI,BYHI,BZHI,TMP,DETI
C-----------------------------------------------
      DO I=LFT,LLT
       RX(I) = AJ4(I)
       RY(I) = AJ5(I)
       RZ(I) = AJ6(I)
       SX(I) = AJ7(I)
       SY(I) = AJ8(I)
       SZ(I) = AJ9(I)
      ENDDO
C
      DO I=LFT,LLT
       OFF(I) = OFFG(I)
       VOL(I) = VOLP(I)
      ENDDO
      IF (IDTS6==0) THEN
       DO I=LFT,LLT
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*VOL(I)*SMAX(I)/WI)
       ENDDO
      ELSE
      DO I=LFT,LLT
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*VOL(I)*SMAX(I)/WI)
      ENDDO
      END IF
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    NU  )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EDERISH                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERISH(
     .    KSI,ETA,ZETA,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    AJI1,AJI2,AJI3,
     .    AJI4,AJI5,AJI6,
     .    AJI7,AJI8,AJI9,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   KSI,ETA,ZETA,
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   D1X(MVSIZ) , D2X(MVSIZ) , D3X(MVSIZ) , D4X(MVSIZ) ,
     .   D1Y(MVSIZ) , D2Y(MVSIZ) , D3Y(MVSIZ) , D4Y(MVSIZ) ,
     .   D1Z(MVSIZ) , D2Z(MVSIZ) , D3Z(MVSIZ) , D4Z(MVSIZ) ,
     .   XG1(MVSIZ), XG2(MVSIZ), XG3(MVSIZ), XG4(MVSIZ),  
     .   YG1(MVSIZ), YG2(MVSIZ), YG3(MVSIZ), YG4(MVSIZ),   
     .   ZG1(MVSIZ), ZG2(MVSIZ), ZG3(MVSIZ), ZG4(MVSIZ),   
     .   F1,F2,F3,XS,XAS,YS,YAS,ZS,ZAS,CS,CAS ,BXHI,BYHI,BZHI
C-----------------------------------------------
       F1 = ETA*ZETA
       F2 = KSI*ZETA
       F3 = KSI*ETA
      DO I=LFT,LLT
       D1X(I)=AJI3(I)*ETA +AJI2(I)*ZETA
       D2X(I)=AJI1(I)*ZETA +AJI3(I)*KSI
       D3X(I)=AJI2(I)*KSI +AJI1(I)*ETA
       D4X(I)=AJI1(I)*F1+AJI2(I)*F2+AJI3(I)*F3
C
       D1Y(I)=AJI6(I)*ETA +AJI5(I)*ZETA
       D2Y(I)=AJI4(I)*ZETA +AJI6(I)*KSI
       D3Y(I)=AJI5(I)*KSI +AJI4(I)*ETA
       D4Y(I)=AJI4(I)*F1+AJI5(I)*F2+AJI6(I)*F3
C
       D1Z(I)=AJI9(I)*ETA +AJI8(I)*ZETA
       D2Z(I)=AJI7(I)*ZETA +AJI9(I)*KSI
       D3Z(I)=AJI8(I)*KSI +AJI7(I)*ETA
       D4Z(I)=AJI7(I)*F1+AJI8(I)*F2+AJI9(I)*F3
      ENDDO
C-------Node 1,7-----
      DO I=LFT,LLT
       XG1(I) = PX1H1(I)*D1X(I)
       XG2(I) = PX1H2(I)*D2X(I)
       XG3(I) = PX1H3(I)*D3X(I)
       XG4(I) = PX1H4(I)*D4X(I)
       YG1(I) = PX1H1(I)*D1Y(I)
       YG2(I) = PX1H2(I)*D2Y(I)
       YG3(I) = PX1H3(I)*D3Y(I)
       YG4(I) = PX1H4(I)*D4Y(I)
       ZG1(I) = PX1H1(I)*D1Z(I)
       ZG2(I) = PX1H2(I)*D2Z(I)
       ZG3(I) = PX1H3(I)*D3Z(I)
       ZG4(I) = PX1H4(I)*D4Z(I)
      ENDDO
C
      DO I=LFT,LLT
C
       YS =D1Y(I)+D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC1(I)
       PXY1(I)=YS-YAS
       PXY7(I)=YS+YAS
       XS =D1X(I)+D2X(I)
       XAS =XG1(I)+XG2(I)-PXC1(I)
       PYX1(I)=XS-XAS
       PYX7(I)=XS+XAS
       ZS =D1Z(I)+D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC1(I)
       PXZ1(I)=ZS-ZAS
       PXZ7(I)=ZS+ZAS
       XS =D1X(I)+D3X(I)
       XAS =XG1(I)+XG3(I)-PXC1(I)
       PZX1(I)=XS-XAS
       PZX7(I)=XS+XAS
       ZS =D2Z(I)+D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC1(I)
       PYZ1(I)=ZS-ZAS
       PYZ7(I)=ZS+ZAS
       YS =D2Y(I)+D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC1(I)
       PZY1(I)=YS-YAS
       PZY7(I)=YS+YAS
      ENDDO
C-------Node 2,8-----
      DO I=LFT,LLT
       XG1(I) = PX2H1(I)*D1X(I)
       XG2(I) = PX2H2(I)*D2X(I)
       XG3(I) = PX2H3(I)*D3X(I)
       XG4(I) = PX2H4(I)*D4X(I)
       YG1(I) = PX2H1(I)*D1Y(I)
       YG2(I) = PX2H2(I)*D2Y(I)
       YG3(I) = PX2H3(I)*D3Y(I)
       YG4(I) = PX2H4(I)*D4Y(I)
       ZG1(I) = PX2H1(I)*D1Z(I)
       ZG2(I) = PX2H2(I)*D2Z(I)
       ZG3(I) = PX2H3(I)*D3Z(I)
       ZG4(I) = PX2H4(I)*D4Z(I)
      ENDDO
C
      DO I=LFT,LLT
C
       YS =D1Y(I)-D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC2(I)
       PXY2(I)=YS-YAS
       PXY8(I)=YS+YAS
       XS =D1X(I)-D2X(I)
       XAS =XG1(I)+XG2(I)-PXC2(I)
       PYX2(I)=XS-XAS
       PYX8(I)=XS+XAS
       ZS =D1Z(I)-D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC2(I)
       PXZ2(I)=ZS-ZAS
       PXZ8(I)=ZS+ZAS
       XS =D1X(I)-D3X(I)
       XAS =XG1(I)+XG3(I)-PXC2(I)
       PZX2(I)=XS-XAS
       PZX8(I)=XS+XAS
       ZS =-D2Z(I)-D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC2(I)
       PYZ2(I)=ZS-ZAS
       PYZ8(I)=ZS+ZAS
       YS =-D2Y(I)-D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC2(I)
       PZY2(I)=YS-YAS
       PZY8(I)=YS+YAS
      ENDDO
C-------Node 3,5-----
      DO I=LFT,LLT
       XG1(I) = PX3H1(I)*D1X(I)
       XG2(I) = PX3H2(I)*D2X(I)
       XG3(I) = PX3H3(I)*D3X(I)
       XG4(I) = PX3H4(I)*D4X(I)
       YG1(I) = PX3H1(I)*D1Y(I)
       YG2(I) = PX3H2(I)*D2Y(I)
       YG3(I) = PX3H3(I)*D3Y(I)
       YG4(I) = PX3H4(I)*D4Y(I)
       ZG1(I) = PX3H1(I)*D1Z(I)
       ZG2(I) = PX3H2(I)*D2Z(I)
       ZG3(I) = PX3H3(I)*D3Z(I)
       ZG4(I) = PX3H4(I)*D4Z(I)
      ENDDO
C
      DO I=LFT,LLT
C
       YS =-D1Y(I)-D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC3(I)
       PXY3(I)=YS-YAS
       PXY5(I)=YS+YAS
       XS =-D1X(I)-D2X(I)
       XAS =XG1(I)+XG2(I)-PXC3(I)
       PYX3(I)=XS-XAS
       PYX5(I)=XS+XAS
       ZS =-D1Z(I)+D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC3(I)
       PXZ3(I)=ZS-ZAS
       PXZ5(I)=ZS+ZAS
       XS =-D1X(I)+D3X(I)
       XAS =XG1(I)+XG3(I)-PXC3(I)
       PZX3(I)=XS-XAS
       PZX5(I)=XS+XAS
       ZS =-D2Z(I)+D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC3(I)
       PYZ3(I)=ZS-ZAS
       PYZ5(I)=ZS+ZAS
       YS =-D2Y(I)+D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC3(I)
       PZY3(I)=YS-YAS
       PZY5(I)=YS+YAS
      ENDDO
C-------Node 4,6-----
      DO I=LFT,LLT
       XG1(I) = PX4H1(I)*D1X(I)
       XG2(I) = PX4H2(I)*D2X(I)
       XG3(I) = PX4H3(I)*D3X(I)
       XG4(I) = PX4H4(I)*D4X(I)
       YG1(I) = PX4H1(I)*D1Y(I)
       YG2(I) = PX4H2(I)*D2Y(I)
       YG3(I) = PX4H3(I)*D3Y(I)
       YG4(I) = PX4H4(I)*D4Y(I)
       ZG1(I) = PX4H1(I)*D1Z(I)
       ZG2(I) = PX4H2(I)*D2Z(I)
       ZG3(I) = PX4H3(I)*D3Z(I)
       ZG4(I) = PX4H4(I)*D4Z(I)
      ENDDO
C
      DO I=LFT,LLT
C
       YS =-D1Y(I)+D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC4(I)
       PXY4(I)=YS-YAS
       PXY6(I)=YS+YAS
       XS =-D1X(I)+D2X(I)
       XAS =XG1(I)+XG2(I)-PXC4(I)
       PYX4(I)=XS-XAS
       PYX6(I)=XS+XAS
       ZS =-D1Z(I)-D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC4(I)
       PXZ4(I)=ZS-ZAS
       PXZ6(I)=ZS+ZAS
       XS =-D1X(I)-D3X(I)
       XAS =XG1(I)+XG3(I)-PXC4(I)
       PZX4(I)=XS-XAS
       PZX6(I)=XS+XAS
       ZS =D2Z(I)-D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC4(I)
       PYZ4(I)=ZS-ZAS
       PYZ6(I)=ZS+ZAS
       YS =D2Y(I)-D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC4(I)
       PZY4(I)=YS-YAS
       PZY6(I)=YS+YAS
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8EJACIT3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EJACIT3(VOLDP,
     .    KSI,ETA,ZETA,WI,
     .    AJP1,AJP2,AJP3,
     .    AJP4,AJP5,AJP6,
     .    AJP7,AJP8,AJP9,
     .    AJI1, AJI2, AJI3,
     .    AJI4, AJI5, AJI6,
     .    AJI7, AJI8, AJI9,
     .    JAC_I)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   KSI,ETA,ZETA,WI,
     .   AJP1(*),AJP2(*),AJP3(*),
     .   AJP4(*),AJP5(*),AJP6(*),
     .   AJP7(*),AJP8(*),AJP9(*),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   JAC_I(10,MVSIZ)
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJ4(MVSIZ),AJ5(MVSIZ),AJ6(MVSIZ),
     .   AJ7(MVSIZ),AJ8(MVSIZ),AJ9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ)
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
      IF (ISMSTR==11) THEN
       AJ1(LFT:LLT)=AJP1(LFT:LLT)
       AJ2(LFT:LLT)=AJP2(LFT:LLT)
       AJ3(LFT:LLT)=AJP3(LFT:LLT)
       AJ4(LFT:LLT)=AJP4(LFT:LLT)
       AJ5(LFT:LLT)=AJP5(LFT:LLT)
       AJ6(LFT:LLT)=AJP6(LFT:LLT)
       AJ7(LFT:LLT)=AJP7(LFT:LLT)
       AJ8(LFT:LLT)=AJP8(LFT:LLT)
       AJ9(LFT:LLT)=AJP9(LFT:LLT)

C     JACOBIAN
C
      DO I=LFT,LLT
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
       JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
       JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
       JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
      DETDP=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DET(I) = DETDP
      VOLDP(I)= WI*DETDP
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C
      DO I=LFT,LLT
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C---------not foget this for Ismstr11 and R,S,T
c      DO I=LFT,LLT
c       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
c      ENDDO
C------ ISMSTR =10       
      ELSE
      DO I=LFT,LLT
       AJI1(I)=JAC_I(1,I)
       AJI4(I)=JAC_I(4,I)
       AJI7(I)=JAC_I(7,I)
       AJI2(I)=JAC_I(2,I)
       AJI5(I)=JAC_I(5,I)
       AJI8(I)=JAC_I(8,I)
       AJI3(I)=JAC_I(3,I)
       AJI6(I)=JAC_I(6,I)
       AJI9(I)=JAC_I(9,I)
       VOLDP(I) =JAC_I(10,I)
      END DO
      END IF !(ISMSTR==11)
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ESELECSH                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ESELECSH(NPTR,NPTS,NPTT,NNPT,
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .   P0XY1,P0XY2,P0XY3,P0XY4,P0XY5,P0XY6,P0XY7,P0XY8,
     .   P0YX1,P0YX2,P0YX3,P0YX4,P0YX5,P0YX6,P0YX7,P0YX8,
     .   P0XZ1,P0XZ2,P0XZ3,P0XZ4,P0XZ5,P0XZ6,P0XZ7,P0XZ8,
     .   P0ZX1,P0ZX2,P0ZX3,P0ZX4,P0ZX5,P0ZX6,P0ZX7,P0ZX8,
     .   P0YZ1,P0YZ2,P0YZ3,P0YZ4,P0YZ5,P0YZ6,P0YZ7,P0YZ8,
     .   P0ZY1,P0ZY2,P0ZY3,P0ZY4,P0ZY5,P0ZY6,P0ZY7,P0ZY8)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPTR,NPTS,NPTT,NNPT 
C     REAL
      my_real
     .  PX1(MVSIZ,NNPT),PX2(MVSIZ,NNPT),PX3(MVSIZ,NNPT),PX4(MVSIZ,NNPT),
     .  PX5(MVSIZ,NNPT),PX6(MVSIZ,NNPT),PX7(MVSIZ,NNPT),PX8(MVSIZ,NNPT),
     .  PY1(MVSIZ,NNPT),PY2(MVSIZ,NNPT),PY3(MVSIZ,NNPT),PY4(MVSIZ,NNPT),
     .  PY5(MVSIZ,NNPT),PY6(MVSIZ,NNPT),PY7(MVSIZ,NNPT),PY8(MVSIZ,NNPT),
     .  PZ1(MVSIZ,NNPT),PZ2(MVSIZ,NNPT),PZ3(MVSIZ,NNPT),PZ4(MVSIZ,NNPT),
     .  PZ5(MVSIZ,NNPT),PZ6(MVSIZ,NNPT),PZ7(MVSIZ,NNPT),PZ8(MVSIZ,NNPT),
     .   P0XY1(MVSIZ,2),P0XY2(MVSIZ,2),P0XY3(MVSIZ,2),P0XY4(MVSIZ,2),
     .   P0XY5(MVSIZ,2),P0XY6(MVSIZ,2),P0XY7(MVSIZ,2),P0XY8(MVSIZ,2),
     .   P0YX1(MVSIZ,2),P0YX2(MVSIZ,2),P0YX3(MVSIZ,2),P0YX4(MVSIZ,2),
     .   P0YX5(MVSIZ,2),P0YX6(MVSIZ,2),P0YX7(MVSIZ,2),P0YX8(MVSIZ,2),
     .   P0XZ1(MVSIZ,2),P0XZ2(MVSIZ,2),P0XZ3(MVSIZ,2),P0XZ4(MVSIZ,2),
     .   P0XZ5(MVSIZ,2),P0XZ6(MVSIZ,2),P0XZ7(MVSIZ,2),P0XZ8(MVSIZ,2),
     .   P0ZX1(MVSIZ,2),P0ZX2(MVSIZ,2),P0ZX3(MVSIZ,2),P0ZX4(MVSIZ,2),
     .   P0ZX5(MVSIZ,2),P0ZX6(MVSIZ,2),P0ZX7(MVSIZ,2),P0ZX8(MVSIZ,2),
     .   P0YZ1(MVSIZ,2),P0YZ2(MVSIZ,2),P0YZ3(MVSIZ,2),P0YZ4(MVSIZ,2),
     .   P0YZ5(MVSIZ,2),P0YZ6(MVSIZ,2),P0YZ7(MVSIZ,2),P0YZ8(MVSIZ,2),
     .   P0ZY1(MVSIZ,2),P0ZY2(MVSIZ,2),P0ZY3(MVSIZ,2),P0ZY4(MVSIZ,2),
     .   P0ZY5(MVSIZ,2),P0ZY6(MVSIZ,2),P0ZY7(MVSIZ,2),P0ZY8(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IR,IS,IT,IP,I1,I2,I3,I4
      INTEGER IXY(4,2),IXZ(4,2),IYZ(4,2)
      DATA IXY/1,5,3,7,2,6,4,8/
      DATA IXZ/1,3,2,4,5,7,6,8/
      DATA IYZ/1,5,2,6,3,7,4,8/
C------ moyen in 3 directions
      DO IR=1,NPTR
        I1 = IXY(1,IR)
        I2 = IXY(2,IR)
        I3 = IXY(3,IR)
        I4 = IXY(4,IR)
       DO I=LFT,LLT
       P0YX1(I,IR)=FOURTH*(PX1(I,I1)+PX1(I,I2)+PX1(I,I3)+PX1(I,I4))
       P0YX2(I,IR)=FOURTH*(PX2(I,I1)+PX2(I,I2)+PX2(I,I3)+PX2(I,I4))
       P0YX3(I,IR)=FOURTH*(PX3(I,I1)+PX3(I,I2)+PX3(I,I3)+PX3(I,I4))
       P0YX4(I,IR)=FOURTH*(PX4(I,I1)+PX4(I,I2)+PX4(I,I3)+PX4(I,I4))
       P0YX5(I,IR)=FOURTH*(PX5(I,I1)+PX5(I,I2)+PX5(I,I3)+PX5(I,I4))
       P0YX6(I,IR)=FOURTH*(PX6(I,I1)+PX6(I,I2)+PX6(I,I3)+PX6(I,I4))
       P0YX7(I,IR)=FOURTH*(PX7(I,I1)+PX7(I,I2)+PX7(I,I3)+PX7(I,I4))
       P0YX8(I,IR)=FOURTH*(PX8(I,I1)+PX8(I,I2)+PX8(I,I3)+PX8(I,I4))
       ENDDO
       DO I=LFT,LLT
       P0XY1(I,IR)=FOURTH*(PY1(I,I1)+PY1(I,I2)+PY1(I,I3)+PY1(I,I4))
       P0XY2(I,IR)=FOURTH*(PY2(I,I1)+PY2(I,I2)+PY2(I,I3)+PY2(I,I4))
       P0XY3(I,IR)=FOURTH*(PY3(I,I1)+PY3(I,I2)+PY3(I,I3)+PY3(I,I4))
       P0XY4(I,IR)=FOURTH*(PY4(I,I1)+PY4(I,I2)+PY4(I,I3)+PY4(I,I4))
       P0XY5(I,IR)=FOURTH*(PY5(I,I1)+PY5(I,I2)+PY5(I,I3)+PY5(I,I4))
       P0XY6(I,IR)=FOURTH*(PY6(I,I1)+PY6(I,I2)+PY6(I,I3)+PY6(I,I4))
       P0XY7(I,IR)=FOURTH*(PY7(I,I1)+PY7(I,I2)+PY7(I,I3)+PY7(I,I4))
       P0XY8(I,IR)=FOURTH*(PY8(I,I1)+PY8(I,I2)+PY8(I,I3)+PY8(I,I4))
       ENDDO
      ENDDO
C      
      DO IT=1,NPTT
        I1 = IXZ(1,IT)
        I2 = IXZ(2,IT)
        I3 = IXZ(3,IT)
        I4 = IXZ(4,IT)
       DO I=LFT,LLT
       P0ZX1(I,IT)=FOURTH*(PX1(I,I1)+PX1(I,I2)+PX1(I,I3)+PX1(I,I4))
       P0ZX2(I,IT)=FOURTH*(PX2(I,I1)+PX2(I,I2)+PX2(I,I3)+PX2(I,I4))
       P0ZX3(I,IT)=FOURTH*(PX3(I,I1)+PX3(I,I2)+PX3(I,I3)+PX3(I,I4))
       P0ZX4(I,IT)=FOURTH*(PX4(I,I1)+PX4(I,I2)+PX4(I,I3)+PX4(I,I4))
       P0ZX5(I,IT)=FOURTH*(PX5(I,I1)+PX5(I,I2)+PX5(I,I3)+PX5(I,I4))
       P0ZX6(I,IT)=FOURTH*(PX6(I,I1)+PX6(I,I2)+PX6(I,I3)+PX6(I,I4))
       P0ZX7(I,IT)=FOURTH*(PX7(I,I1)+PX7(I,I2)+PX7(I,I3)+PX7(I,I4))
       P0ZX8(I,IT)=FOURTH*(PX8(I,I1)+PX8(I,I2)+PX8(I,I3)+PX8(I,I4))
       ENDDO
       DO I=LFT,LLT
       P0XZ1(I,IT)=FOURTH*(PZ1(I,I1)+PZ1(I,I2)+PZ1(I,I3)+PZ1(I,I4))
       P0XZ2(I,IT)=FOURTH*(PZ2(I,I1)+PZ2(I,I2)+PZ2(I,I3)+PZ2(I,I4))
       P0XZ3(I,IT)=FOURTH*(PZ3(I,I1)+PZ3(I,I2)+PZ3(I,I3)+PZ3(I,I4))
       P0XZ4(I,IT)=FOURTH*(PZ4(I,I1)+PZ4(I,I2)+PZ4(I,I3)+PZ4(I,I4))
       P0XZ5(I,IT)=FOURTH*(PZ5(I,I1)+PZ5(I,I2)+PZ5(I,I3)+PZ5(I,I4))
       P0XZ6(I,IT)=FOURTH*(PZ6(I,I1)+PZ6(I,I2)+PZ6(I,I3)+PZ6(I,I4))
       P0XZ7(I,IT)=FOURTH*(PZ7(I,I1)+PZ7(I,I2)+PZ7(I,I3)+PZ7(I,I4))
       P0XZ8(I,IT)=FOURTH*(PZ8(I,I1)+PZ8(I,I2)+PZ8(I,I3)+PZ8(I,I4))
       ENDDO
      ENDDO
C     
      DO IS=1,NPTS
        I1 = IYZ(1,IS)
        I2 = IYZ(2,IS)
        I3 = IYZ(3,IS)
        I4 = IYZ(4,IS)
       DO I=LFT,LLT
       P0ZY1(I,IS)=FOURTH*(PY1(I,I1)+PY1(I,I2)+PY1(I,I3)+PY1(I,I4))
       P0ZY2(I,IS)=FOURTH*(PY2(I,I1)+PY2(I,I2)+PY2(I,I3)+PY2(I,I4))
       P0ZY3(I,IS)=FOURTH*(PY3(I,I1)+PY3(I,I2)+PY3(I,I3)+PY3(I,I4))
       P0ZY4(I,IS)=FOURTH*(PY4(I,I1)+PY4(I,I2)+PY4(I,I3)+PY4(I,I4))
       P0ZY5(I,IS)=FOURTH*(PY5(I,I1)+PY5(I,I2)+PY5(I,I3)+PY5(I,I4))
       P0ZY6(I,IS)=FOURTH*(PY6(I,I1)+PY6(I,I2)+PY6(I,I3)+PY6(I,I4))
       P0ZY7(I,IS)=FOURTH*(PY7(I,I1)+PY7(I,I2)+PY7(I,I3)+PY7(I,I4))
       P0ZY8(I,IS)=FOURTH*(PY8(I,I1)+PY8(I,I2)+PY8(I,I3)+PY8(I,I4))
       ENDDO
       DO I=LFT,LLT
       P0YZ1(I,IS)=FOURTH*(PZ1(I,I1)+PZ1(I,I2)+PZ1(I,I3)+PZ1(I,I4))
       P0YZ2(I,IS)=FOURTH*(PZ2(I,I1)+PZ2(I,I2)+PZ2(I,I3)+PZ2(I,I4))
       P0YZ3(I,IS)=FOURTH*(PZ3(I,I1)+PZ3(I,I2)+PZ3(I,I3)+PZ3(I,I4))
       P0YZ4(I,IS)=FOURTH*(PZ4(I,I1)+PZ4(I,I2)+PZ4(I,I3)+PZ4(I,I4))
       P0YZ5(I,IS)=FOURTH*(PZ5(I,I1)+PZ5(I,I2)+PZ5(I,I3)+PZ5(I,I4))
       P0YZ6(I,IS)=FOURTH*(PZ6(I,I1)+PZ6(I,I2)+PZ6(I,I3)+PZ6(I,I4))
       P0YZ7(I,IS)=FOURTH*(PZ7(I,I1)+PZ7(I,I2)+PZ7(I,I3)+PZ7(I,I4))
       P0YZ8(I,IS)=FOURTH*(PZ8(I,I1)+PZ8(I,I2)+PZ8(I,I3)+PZ8(I,I4))
       ENDDO
      ENDDO
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EDERISH2                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        S8EA2BP8                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERISH2(
     .   PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .   PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .   PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .   PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .   PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .   PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .   P0XY1,P0XY2,P0XY3,P0XY4,P0XY5,P0XY6,P0XY7,P0XY8,
     .   P0YX1,P0YX2,P0YX3,P0YX4,P0YX5,P0YX6,P0YX7,P0YX8,
     .   P0XZ1,P0XZ2,P0XZ3,P0XZ4,P0XZ5,P0XZ6,P0XZ7,P0XZ8,
     .   P0ZX1,P0ZX2,P0ZX3,P0ZX4,P0ZX5,P0ZX6,P0ZX7,P0ZX8,
     .   P0YZ1,P0YZ2,P0YZ3,P0YZ4,P0YZ5,P0YZ6,P0YZ7,P0YZ8,
     .   P0ZY1,P0ZY2,P0ZY3,P0ZY4,P0ZY5,P0ZY6,P0ZY7,P0ZY8)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   P0XY1(*),P0XY2(*),P0XY3(*),P0XY4(*),
     .   P0XY5(*),P0XY6(*),P0XY7(*),P0XY8(*),
     .   P0YX1(*),P0YX2(*),P0YX3(*),P0YX4(*),
     .   P0YX5(*),P0YX6(*),P0YX7(*),P0YX8(*),
     .   P0XZ1(*),P0XZ2(*),P0XZ3(*),P0XZ4(*),
     .   P0XZ5(*),P0XZ6(*),P0XZ7(*),P0XZ8(*),
     .   P0ZX1(*),P0ZX2(*),P0ZX3(*),P0ZX4(*),
     .   P0ZX5(*),P0ZX6(*),P0ZX7(*),P0ZX8(*),
     .   P0YZ1(*),P0YZ2(*),P0YZ3(*),P0YZ4(*),
     .   P0YZ5(*),P0YZ6(*),P0YZ7(*),P0YZ8(*),
     .   P0ZY1(*),P0ZY2(*),P0ZY3(*),P0ZY4(*),
     .   P0ZY5(*),P0ZY6(*),P0ZY7(*),P0ZY8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C                                                                     12
          CALL S8EA2BP8(
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    P0XY1, P0XY2, P0XY3, P0XY4,
     .    P0XY5, P0XY6, P0XY7, P0XY8)
          CALL S8EA2BP8(
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    P0YX1,P0YX2,P0YX3,P0YX4,
     .    P0YX5,P0YX6,P0YX7,P0YX8)
          CALL S8EA2BP8(
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    P0XZ1,P0XZ2,P0XZ3,P0XZ4,
     .    P0XZ5,P0XZ6,P0XZ7,P0XZ8)
          CALL S8EA2BP8(
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    P0ZX1,P0ZX2,P0ZX3,P0ZX4,
     .    P0ZX5,P0ZX6,P0ZX7,P0ZX8)
          CALL S8EA2BP8(
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    P0YZ1,P0YZ2,P0YZ3,P0YZ4,
     .    P0YZ5,P0YZ6,P0YZ7,P0YZ8)
          CALL S8EA2BP8(
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    P0ZY1,P0ZY2,P0ZY3,P0ZY4,
     .    P0ZY5,P0ZY6,P0ZY7,P0ZY8)
C
      RETURN
       END
Chd|====================================================================
Chd|  S8EDERI_BIJ                   source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERI_BIJ(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    NU  ,NU1 )
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   NU(*)  ,NU1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   X1,X3,Y1,Y2,Z2,Z3,X24,Y34,Z14
C-----------------------------------------------
C---PX_I=X1234_I+PXC_I; PYX_I=X23_I+PXC_I; PZX_I=X12_I+PXC_I
C-----X2_I=0->X1_I = X12_I ; X3_I = X23_I X24_I =X1234_I-X1_I-X3_I
C-----verifie valeur de X4_I otherwise to be calculated
C---PY_I=Y1234_I+PYC_I; PXY_I=Y23_I+PYC_I; PZY_I=Y13_I+PYC_I
C-----Y3_I=0->Y1_I = Y13_I ; Y2_I = Y23_I Y34_I =Y1234_I-Y1_I-Y2_I
C---PZ_I=Z1234_I+PZC_I; PXZ_I=Z12_I+PZC_I; PYZ_I=Z13_I+PZC_I
C-----Z1_I=0->Z2_I = Z12_I ; Z3_I = Z13_I Z14_I =Z1234_I-Z2_I-Z3_I
C-------|0 BYX BZX |
C-------|BXY 0 BZY |  same than H8A
C-------|BXZ BYZ 0 |
C-BXY_I=-nu1*X1_I-nu*X24_I
C-BXZ_I=-nu1*X3_I-nu*X24_I
C-BYX_I=-nu1*Y1_I-nu*Y34_I
C-BYZ_I=-nu1*Y2_I-nu*Y34_I
C-BZX_I=-nu1*Z2_I-nu*Z14_I
C-BZY_I=-nu1*Z3_I-nu*Z14_I
      DO I=LFT,LLT
       X1 = PZX1(I)-PXC1(I)
       X3 = PYX1(I)-PXC1(I)
       Y1 = PZY1(I)-PYC1(I)
       Y2 = PXY1(I)-PYC1(I)
       Z2 = PXZ1(I)-PZC1(I)
       Z3 = PYZ1(I)-PZC1(I)
       X24= NU(I)*(PX1(I)-PXC1(I)-X1-X3)
       Y34= NU(I)*(PY1(I)-PYC1(I)-Y1-Y2)
       Z14= NU(I)*(PZ1(I)-PZC1(I)-Z2-Z3)
       BXY1(I)=-NU1(I)*X1-X24
       BXZ1(I)=-NU1(I)*X3-X24
       BYX1(I)=-NU1(I)*Y1-Y34
       BYZ1(I)=-NU1(I)*Y2-Y34
       BZX1(I)=-NU1(I)*Z3-Z14
       BZY1(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX2(I)-PXC2(I)
       X3 = PYX2(I)-PXC2(I)
       Y1 = PZY2(I)-PYC2(I)
       Y2 = PXY2(I)-PYC2(I)
       Z2 = PXZ2(I)-PZC2(I)
       Z3 = PYZ2(I)-PZC2(I)
       X24= NU(I)*(PX2(I)-PXC2(I)-X1-X3)
       Y34= NU(I)*(PY2(I)-PYC2(I)-Y1-Y2)
       Z14= NU(I)*(PZ2(I)-PZC2(I)-Z2-Z3)
       BXY2(I)=-NU1(I)*X1-X24
       BXZ2(I)=-NU1(I)*X3-X24
       BYX2(I)=-NU1(I)*Y1-Y34
       BYZ2(I)=-NU1(I)*Y2-Y34
       BZX2(I)=-NU1(I)*Z3-Z14
       BZY2(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX3(I)-PXC3(I)
       X3 = PYX3(I)-PXC3(I)
       Y1 = PZY3(I)-PYC3(I)
       Y2 = PXY3(I)-PYC3(I)
       Z2 = PXZ3(I)-PZC3(I)
       Z3 = PYZ3(I)-PZC3(I)
       X24= NU(I)*(PX3(I)-PXC3(I)-X1-X3)
       Y34= NU(I)*(PY3(I)-PYC3(I)-Y1-Y2)
       Z14= NU(I)*(PZ3(I)-PZC3(I)-Z2-Z3)
       BXY3(I)=-NU1(I)*X1-X24
       BXZ3(I)=-NU1(I)*X3-X24
       BYX3(I)=-NU1(I)*Y1-Y34
       BYZ3(I)=-NU1(I)*Y2-Y34
       BZX3(I)=-NU1(I)*Z3-Z14
       BZY3(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX4(I)-PXC4(I)
       X3 = PYX4(I)-PXC4(I)
       Y1 = PZY4(I)-PYC4(I)
       Y2 = PXY4(I)-PYC4(I)
       Z2 = PXZ4(I)-PZC4(I)
       Z3 = PYZ4(I)-PZC4(I)
       X24= NU(I)*(PX4(I)-PXC4(I)-X1-X3)
       Y34= NU(I)*(PY4(I)-PYC4(I)-Y1-Y2)
       Z14= NU(I)*(PZ4(I)-PZC4(I)-Z2-Z3)
       BXY4(I)=-NU1(I)*X1-X24
       BXZ4(I)=-NU1(I)*X3-X24
       BYX4(I)=-NU1(I)*Y1-Y34
       BYZ4(I)=-NU1(I)*Y2-Y34
       BZX4(I)=-NU1(I)*Z3-Z14
       BZY4(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX5(I)+PXC3(I)
       X3 = PYX5(I)+PXC3(I)
       Y1 = PZY5(I)+PYC3(I)
       Y2 = PXY5(I)+PYC3(I)
       Z2 = PXZ5(I)+PZC3(I)
       Z3 = PYZ5(I)+PZC3(I)
       X24= NU(I)*(PX5(I)+PXC3(I)-X1-X3)
       Y34= NU(I)*(PY5(I)+PYC3(I)-Y1-Y2)
       Z14= NU(I)*(PZ5(I)+PZC3(I)-Z2-Z3)
       BXY5(I)=-NU1(I)*X1-X24
       BXZ5(I)=-NU1(I)*X3-X24
       BYX5(I)=-NU1(I)*Y1-Y34
       BYZ5(I)=-NU1(I)*Y2-Y34
       BZX5(I)=-NU1(I)*Z3-Z14
       BZY5(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX6(I)+PXC4(I)
       X3 = PYX6(I)+PXC4(I)
       Y1 = PZY6(I)+PYC4(I)
       Y2 = PXY6(I)+PYC4(I)
       Z2 = PXZ6(I)+PZC4(I)
       Z3 = PYZ6(I)+PZC4(I)
       X24= NU(I)*(PX6(I)+PXC4(I)-X1-X3)
       Y34= NU(I)*(PY6(I)+PYC4(I)-Y1-Y2)
       Z14= NU(I)*(PZ6(I)+PZC4(I)-Z2-Z3)
       BXY6(I)=-NU1(I)*X1-X24
       BXZ6(I)=-NU1(I)*X3-X24
       BYX6(I)=-NU1(I)*Y1-Y34
       BYZ6(I)=-NU1(I)*Y2-Y34
       BZX6(I)=-NU1(I)*Z3-Z14
       BZY6(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX7(I)+PXC1(I)
       X3 = PYX7(I)+PXC1(I)
       Y1 = PZY7(I)+PYC1(I)
       Y2 = PXY7(I)+PYC1(I)
       Z2 = PXZ7(I)+PZC1(I)
       Z3 = PYZ7(I)+PZC1(I)
       X24= NU(I)*(PX7(I)+PXC1(I)-X1-X3)
       Y34= NU(I)*(PY7(I)+PYC1(I)-Y1-Y2)
       Z14= NU(I)*(PZ7(I)+PZC1(I)-Z2-Z3)
       BXY7(I)=-NU1(I)*X1-X24
       BXZ7(I)=-NU1(I)*X3-X24
       BYX7(I)=-NU1(I)*Y1-Y34
       BYZ7(I)=-NU1(I)*Y2-Y34
       BZX7(I)=-NU1(I)*Z3-Z14
       BZY7(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=LFT,LLT
       X1 = PZX8(I)+PXC2(I)
       X3 = PYX8(I)+PXC2(I)
       Y1 = PZY8(I)+PYC2(I)
       Y2 = PXY8(I)+PYC2(I)
       Z2 = PXZ8(I)+PZC2(I)
       Z3 = PYZ8(I)+PZC2(I)
       X24= NU(I)*(PX8(I)+PXC2(I)-X1-X3)
       Y34= NU(I)*(PY8(I)+PYC2(I)-Y1-Y2)
       Z14= NU(I)*(PZ8(I)+PZC2(I)-Z2-Z3)
       BXY8(I)=-NU1(I)*X1-X24
       BXZ8(I)=-NU1(I)*X3-X24
       BYX8(I)=-NU1(I)*Y1-Y34
       BYZ8(I)=-NU1(I)*Y2-Y34
       BZX8(I)=-NU1(I)*Z3-Z14
       BZY8(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8EGETPIJ                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EGETPIJ(
     .    PX1, PX2, PX3, PX4,
     .    PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4,
     .    PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4,
     .    PZ5, PZ6, PZ7, PZ8,
     .    PIJ,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
      INTEGER NEL
C     REAL
      my_real
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PIJ(NEL,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

C-----------------------------------------------
       DO I=1,NEL
        PX1(I) = PIJ(I,1) 
        PY1(I) = PIJ(I,2) 
        PZ1(I) = PIJ(I,3) 
        PX2(I) = PIJ(I,4) 
        PY2(I) = PIJ(I,5) 
        PZ2(I) = PIJ(I,6) 
        PX3(I) = PIJ(I,7) 
        PY3(I) = PIJ(I,8) 
        PZ3(I) = PIJ(I,9) 
        PX4(I) = PIJ(I,10) 
        PY4(I) = PIJ(I,11) 
        PZ4(I) = PIJ(I,12) 
        PX5(I) = PIJ(I,13) 
        PY5(I) = PIJ(I,14) 
        PZ5(I) = PIJ(I,15) 
        PX6(I) = PIJ(I,16) 
        PY6(I) = PIJ(I,17) 
        PZ6(I) = PIJ(I,18) 
        PX7(I) = PIJ(I,19) 
        PY7(I) = PIJ(I,20) 
        PZ7(I) = PIJ(I,21) 
        PX8(I) = PIJ(I,22) 
        PY8(I) = PIJ(I,23) 
        PZ8(I) = PIJ(I,24) 
       END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8EGETBIJ                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EGETBIJ(
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    PIJ,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
      INTEGER NEL
C     REAL
      my_real
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   PIJ(NEL,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K

C-----------------------------------------------
       DO I=1,NEL
C       
        BXY1(I) = PIJ(I,25) 
        BYX1(I) = PIJ(I,26) 
        BXZ1(I) = PIJ(I,27) 
        BZX1(I) = PIJ(I,28) 
        BYZ1(I) = PIJ(I,29) 
        BZY1(I) = PIJ(I,30) 
C                
        BXY2(I) = PIJ(I,31) 
        BYX2(I) = PIJ(I,32) 
        BXZ2(I) = PIJ(I,33) 
        BZX2(I) = PIJ(I,34) 
        BYZ2(I) = PIJ(I,35) 
        BZY2(I) = PIJ(I,36) 
C                
        BXY3(I) = PIJ(I,37) 
        BYX3(I) = PIJ(I,38) 
        BXZ3(I) = PIJ(I,39) 
        BZX3(I) = PIJ(I,40) 
        BYZ3(I) = PIJ(I,41) 
        BZY3(I) = PIJ(I,42) 
C                
        BXY4(I) = PIJ(I,43) 
        BYX4(I) = PIJ(I,44) 
        BXZ4(I) = PIJ(I,45) 
        BZX4(I) = PIJ(I,46) 
        BYZ4(I) = PIJ(I,47) 
        BZY4(I) = PIJ(I,48) 
C                
        BXY5(I) = PIJ(I,49) 
        BYX5(I) = PIJ(I,50) 
        BXZ5(I) = PIJ(I,51) 
        BZX5(I) = PIJ(I,52) 
        BYZ5(I) = PIJ(I,53) 
        BZY5(I) = PIJ(I,54) 
C                
        BXY6(I) = PIJ(I,55) 
        BYX6(I) = PIJ(I,56) 
        BXZ6(I) = PIJ(I,57) 
        BZX6(I) = PIJ(I,58) 
        BYZ6(I) = PIJ(I,59) 
        BZY6(I) = PIJ(I,60) 
C                
        BXY7(I) = PIJ(I,61) 
        BYX7(I) = PIJ(I,62) 
        BXZ7(I) = PIJ(I,63) 
        BZX7(I) = PIJ(I,64) 
        BYZ7(I) = PIJ(I,65) 
        BZY7(I) = PIJ(I,66) 
C                
        BXY8(I) = PIJ(I,67) 
        BYX8(I) = PIJ(I,68) 
        BXZ8(I) = PIJ(I,69) 
        BZX8(I) = PIJ(I,70) 
        BYZ8(I) = PIJ(I,71) 
        BZY8(I) = PIJ(I,72) 
       END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8E_SIG_A                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8E_SIG_A(C1,G ,OFF,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    PIJ, SIG_A,EINT,VOL,D1,D2,D3, SIG, S1 , S2 ,S3 ,NEL,OFFG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   PIJ(NEL,*),C1,G,SIG_A(MVSIZ,3),OFF(*),EINT(*),
     .   VOL(*),D1(*),D2(*),D3(*),SIG(NEL,6),S1(*),S2(*),S3(*),OFFG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   DC1,DG2,DXX(MVSIZ),DYY(MVSIZ),DZZ(MVSIZ),EKK,DAV,
     .   DT05,E1,E2,E3,DE,P2
C PIJ(NEL,25-27) are used call stress with v-terms
C-----------------------------------------------
       IF (TT==ZERO) PIJ(1:NEL,25:27)=ZERO
C----1,compute Dii with v-terms, compute incremental stress
       DO I=LFT,LLT
	     IF (OFFG(I) >ONE) CYCLE
           DYY(I) =BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +              +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +              +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +              +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
     +              +BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +              +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +              +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +              +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
           DZZ(I) =BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +              +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +              +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +              +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
     +              +BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +              +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +              +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +              +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
           DXX(I) =BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +              +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +              +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +              +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
     +              +BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +              +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +              +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +              +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
       ENDDO
       DC1=C1*DT1
       DG2=TWO*G*DT1
       SIG_A(1:NEL,1:3)=PIJ(1:NEL,25:27)
       DO I=LFT,LLT
	     IF (OFFG(I) >ONE) CYCLE
         EKK=(DXX(I)+DYY(I)+DZZ(I))*DC1
c	     IF (OFFG(I) >ONE) EKK=ZERO
         DAV=-THIRD*(DXX(I)+DYY(I)+DZZ(I))
         PIJ(I,25)=PIJ(I,25)*OFF(I)+EKK+DG2*(DXX(I)+DAV)
         PIJ(I,26)=PIJ(I,26)*OFF(I)+EKK+DG2*(DYY(I)+DAV)
         PIJ(I,27)=PIJ(I,27)*OFF(I)+EKK+DG2*(DZZ(I)+DAV)
       END DO
       DT05 =HALF*DT1
      DO I=LFT,LLT
	    IF (OFFG(I) >ONE) CYCLE
       E1=(DXX(I)+D1(I))*(SIG_A(I,1)+PIJ(I,25))+DXX(I)*(S1(I)+SIG(I,1))
       E2=(DYY(I)+D2(I))*(SIG_A(I,2)+PIJ(I,26))+DYY(I)*(S2(I)+SIG(I,2))
       E3=(DZZ(I)+D3(I))*(SIG_A(I,3)+PIJ(I,27))+DZZ(I)*(S3(I)+SIG(I,3))
       DE= (E1+E2+E3)*DT05*OFF(I) 
       EINT(I)=EINT(I)+DE 
       SIG_A(I,1:3)=PIJ(I,25:27)
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8ZJAC_IC                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8_IS17JAC_I                  source/elements/solid/solide8e/s8ederi3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE S8ZJAC_IC(
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              AJ1  ,AJ2  ,AJ3  ,
     .              AJ4  ,AJ5  ,AJ6  ,
     .              AJ7  ,AJ8  ,AJ9  ,
     .              HX, HY, HZ,
     .              PXC1, PXC2, PXC3, PXC4, 
     .              PYC1, PYC2, PYC3, PYC4, 
     .              PZC1, PZC2, PZC3, PZC4,
     .              JAC_I,INDEX,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,INDEX(*)
      my_real
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),JAC_I(10,*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*), 
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*), 
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*) 
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ)
C                                                                     12
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJI1, AJI2, AJI3,AJI4, AJI5, AJI6,AJI7, AJI8, AJI9,
     .   AJ12, AJ45, AJ78,AJ12P, AJ45P, AJ78P,
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) ,JAC_1(10,MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
      X17(I)=XD7(I)-XD1(I)
      X28(I)=XD8(I)-XD2(I)
      X35(I)=XD5(I)-XD3(I)
      X46(I)=XD6(I)-XD4(I)
      Y17(I)=YD7(I)-YD1(I)
      Y28(I)=YD8(I)-YD2(I)
      Y35(I)=YD5(I)-YD3(I)
      Y46(I)=YD6(I)-YD4(I)
      Z17(I)=ZD7(I)-ZD1(I)
      Z28(I)=ZD8(I)-ZD2(I)
      Z35(I)=ZD5(I)-ZD3(I)
      Z46(I)=ZD6(I)-ZD4(I)
      END DO
C
      DO I=1,NEL
      AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      A17(I)=X17(I)+X46(I)
      A28(I)=X28(I)+X35(I)
      B17(I)=Y17(I)+Y46(I)
      B28(I)=Y28(I)+Y35(I)
      C17(I)=Z17(I)+Z46(I)
      C28(I)=Z28(I)+Z35(I)
C
      AJ7(I)=A17(I)+A28(I)
      AJ8(I)=B17(I)+B28(I)
      AJ9(I)=C17(I)+C28(I)
      AJ1(I)=A17(I)-A28(I)
      AJ2(I)=B17(I)-B28(I)
      AJ3(I)=C17(I)-C28(I)
      END DO
C
C     JACOBIAN
C
      DO I=1,NEL
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
      DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_64/DET(I)
      ENDDO
C
       DO I=1,NEL
         AJI1=DETT(I)*JAC_59_68(I)
         AJI4=DETT(I)*JAC_67_49(I)
         AJI7=DETT(I)*JAC_48_57(I)
         AJI2=DETT(I)*JAC_38_29(I)
         AJI5=DETT(I)*JAC_19_37(I)
         AJI8=DETT(I)*JAC_27_18(I)
         AJI3=DETT(I)*JAC_26_35(I)
         AJI6=DETT(I)*JAC_34_16(I)
         AJI9=DETT(I)*JAC_15_24(I)
         AJ12=AJI1-AJI2
         AJ45=AJI4-AJI5
         AJ78=AJI7-AJI8
         PXC2(I)= AJ12-AJI3
         PYC2(I)= AJ45-AJI6
         PZC2(I)= AJ78-AJI9
         PXC4(I)=-AJ12-AJI3
         PYC4(I)=-AJ45-AJI6
         PZC4(I)=-AJ78-AJI9
         AJ12P=AJI1+AJI2
         AJ45P=AJI4+AJI5
         AJ78P=AJI7+AJI8
         PXC1(I)=-AJ12P-AJI3
         PYC1(I)=-AJ45P-AJI6
         PZC1(I)=-AJ78P-AJI9
         PXC3(I)=AJ12P-AJI3
         PYC3(I)=AJ45P-AJI6
         PZC3(I)=AJ78P-AJI9
         JAC_1(1,I)=AJI1
         JAC_1(4,I)=AJI4
         JAC_1(7,I)=AJI7
         JAC_1(2,I)=AJI2
         JAC_1(5,I)=AJI5
         JAC_1(8,I)=AJI8
         JAC_1(3,I)=AJI3
         JAC_1(6,I)=AJI6
         JAC_1(9,I)=AJI9
         JAC_1(10,I)=DET(I)
       ENDDO
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         HX(1,I)=(XD1(I)+XD2(I)-XD3(I)-XD4(I)-XD5(I)-XD6(I)+XD7(I)+XD8(I))
         HY(1,I)=(YD1(I)+YD2(I)-YD3(I)-YD4(I)-YD5(I)-YD6(I)+YD7(I)+YD8(I))
         HZ(1,I)=(ZD1(I)+ZD2(I)-ZD3(I)-ZD4(I)-ZD5(I)-ZD6(I)+ZD7(I)+ZD8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         HX(2,I)=(XD1(I)-XD2(I)-XD3(I)+XD4(I)-XD5(I)+XD6(I)+XD7(I)-XD8(I))
         HY(2,I)=(YD1(I)-YD2(I)-YD3(I)+YD4(I)-YD5(I)+YD6(I)+YD7(I)-YD8(I))
         HZ(2,I)=(ZD1(I)-ZD2(I)-ZD3(I)+ZD4(I)-ZD5(I)+ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=1,NEL
         HX(3,I)=(XD1(I)-XD2(I)+XD3(I)-XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(3,I)=(YD1(I)-YD2(I)+YD3(I)-YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(3,I)=(ZD1(I)-ZD2(I)+ZD3(I)-ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         HX(4,I)=(-XD1(I)+XD2(I)-XD3(I)+XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(4,I)=(-YD1(I)+YD2(I)-YD3(I)+YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(4,I)=(-ZD1(I)+ZD2(I)-ZD3(I)+ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
#include "vectorize.inc"
        DO J=1,NEL
          I = INDEX(J)
          JAC_I(1:10,I) = JAC_1(1:10,J)
        ENDDO      
C       
      RETURN
C
      END
Chd|====================================================================
Chd|  S8ZJAC_I3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8_IS17JAC_I                  source/elements/solid/solide8e/s8ederi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZJAC_I3(
     .    KSI,ETA,ZETA,WI,
     .    HX, HY, HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC_I,L_PIJ,PIJ,SIG,SIGL,INDEX,NB,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER L_PIJ,NEL,INDEX(*),NB
      my_real
     .   KSI,ETA,ZETA,WI,
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC_I(10,*),PIJ(NEL,24),SIG(NEL,6),SIGL(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      my_real
     .   DET(MVSIZ) ,  DETT(MVSIZ) ,
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJ4(MVSIZ),AJ5(MVSIZ),AJ6(MVSIZ),
     .   AJ7(MVSIZ),AJ8(MVSIZ),AJ9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ)
      my_real
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   A1PR1(MVSIZ),A1PR3(MVSIZ),A1PR5(MVSIZ),A1PR7(MVSIZ),
     .   A4PR1(MVSIZ),A4PR3(MVSIZ),A4PR5(MVSIZ),A4PR7(MVSIZ),
     .   A7PR1(MVSIZ),A7PR3(MVSIZ),A7PR5(MVSIZ),A7PR7(MVSIZ),
     .   A2PS1(MVSIZ),A2PS2(MVSIZ),A2PS5(MVSIZ),A2PS6(MVSIZ),
     .   A5PS1(MVSIZ),A5PS2(MVSIZ),A5PS5(MVSIZ),A5PS6(MVSIZ),
     .   A8PS1(MVSIZ),A8PS2(MVSIZ),A8PS5(MVSIZ),A8PS6(MVSIZ),
     .   A3PT1(MVSIZ),A3PT2(MVSIZ),A3PT3(MVSIZ),A3PT4(MVSIZ),
     .   A6PT1(MVSIZ),A6PT2(MVSIZ),A6PT3(MVSIZ),A6PT4(MVSIZ),
     .   A9PT1(MVSIZ),A9PT2(MVSIZ),A9PT3(MVSIZ),A9PT4(MVSIZ),
     .   PR(8),PS(8),PT(8),RP, SP, TP, RM, SM, TM,
     .   PX1, PX2, PX3, PX4,PX5, PX6, PX7, PX8,
     .   PY1, PY2, PY3, PY4,PY5, PY6, PY7, PY8,
     .   PZ1, PZ2, PZ3, PZ4,PZ5, PZ6, PZ7, PZ8,
     .   JAC_1(10,MVSIZ),PIJ12(24,NEL)
C-----------------------------------------------
      DO I=1,NB
       AJ1(I)=CJ1(I)+HX(3,I)*ETA+(HX(2,I)+HX(4,I)*ETA)*ZETA
       AJ2(I)=CJ2(I)+HY(3,I)*ETA+(HY(2,I)+HY(4,I)*ETA)*ZETA
       AJ3(I)=CJ3(I)+HZ(3,I)*ETA+(HZ(2,I)+HZ(4,I)*ETA)*ZETA
C
       AJ4(I)=CJ4(I)+HX(1,I)*ZETA+(HX(3,I)+HX(4,I)*ZETA)*KSI
       AJ5(I)=CJ5(I)+HY(1,I)*ZETA+(HY(3,I)+HY(4,I)*ZETA)*KSI
       AJ6(I)=CJ6(I)+HZ(1,I)*ZETA+(HZ(3,I)+HZ(4,I)*ZETA)*KSI
C
       AJ7(I)=CJ7(I)+HX(2,I)*KSI+(HX(1,I)+HX(4,I)*KSI)*ETA
       AJ8(I)=CJ8(I)+HY(2,I)*KSI+(HY(1,I)+HY(4,I)*KSI)*ETA
       AJ9(I)=CJ9(I)+HZ(2,I)*KSI+(HZ(1,I)+HZ(4,I)*KSI)*ETA
      ENDDO
C
C     JACOBIAN
C
      DO I=1,NB
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=1,NB
      DET(I)=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C      
      IF (L_PIJ ==0) THEN
      DO I=1,NB
       JAC_1(1,I)=DETT(I)*JAC_59_68(I)
       JAC_1(4,I)=DETT(I)*JAC_67_49(I)
       JAC_1(7,I)=DETT(I)*JAC_48_57(I)
       JAC_1(2,I)=DETT(I)*JAC_38_29(I)
       JAC_1(5,I)=DETT(I)*JAC_19_37(I)
       JAC_1(8,I)=DETT(I)*JAC_27_18(I)
       JAC_1(3,I)=DETT(I)*JAC_26_35(I)
       JAC_1(6,I)=DETT(I)*JAC_34_16(I)
       JAC_1(9,I)=DETT(I)*JAC_15_24(I)
       JAC_1(10,I)=DET(I)
      ENDDO
#include "vectorize.inc"
          DO J=1,NB
            I = INDEX(J)
            JAC_I(1:10,I) = JAC_1(1:10,J)
          ENDDO      
      ELSE
C--------calculate Pij----      
      DO I=1,NB
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C------NI,ksi-------
      RP = ONE + KSI
      SP = ONE + ETA
      TP = ONE + ZETA
      RM = ONE - KSI
      SM = ONE - ETA
      TM = ONE - ZETA
      PR(1)=-SM*TM
      PR(2)=-PR(1)
      PR(3)= SP*TM
      PR(4)=-PR(3)
      PR(5)=-SM*TP
      PR(6)=-PR(5)
      PR(7)= SP*TP
      PR(8)=-PR(7)
      PS(1)=-RM*TM
      PS(2)=-RP*TM
      PS(3)=-PS(2)
      PS(4)=-PS(1)
      PS(5)=-RM*TP
      PS(6)=-RP*TP
      PS(7)=-PS(6)
      PS(8)=-PS(5)
      PT(1)=-RM*SM
      PT(2)=-RP*SM
      PT(3)=-RP*SP
      PT(4)=-RM*SP
      PT(5)=-PT(1)
      PT(6)=-PT(2)
      PT(7)=-PT(3)
      PT(8)=-PT(4)
cc
      DO I=1,NB
        A1PR1(I)=AJI1(I)*PR(1)
        A1PR3(I)=AJI1(I)*PR(3)
        A1PR5(I)=AJI1(I)*PR(5)
        A1PR7(I)=AJI1(I)*PR(7)
        A4PR1(I)=AJI4(I)*PR(1)
        A4PR3(I)=AJI4(I)*PR(3)
        A4PR5(I)=AJI4(I)*PR(5)
        A4PR7(I)=AJI4(I)*PR(7)
        A7PR1(I)=AJI7(I)*PR(1)
        A7PR3(I)=AJI7(I)*PR(3)
        A7PR5(I)=AJI7(I)*PR(5)
        A7PR7(I)=AJI7(I)*PR(7)
      ENDDO
      DO I=1,NB
        A2PS1(I)=AJI2(I)*PS(1)
        A2PS2(I)=AJI2(I)*PS(2)
        A2PS5(I)=AJI2(I)*PS(5)
        A2PS6(I)=AJI2(I)*PS(6)
        A5PS1(I)=AJI5(I)*PS(1)
        A5PS2(I)=AJI5(I)*PS(2)
        A5PS5(I)=AJI5(I)*PS(5)
        A5PS6(I)=AJI5(I)*PS(6)
        A8PS1(I)=AJI8(I)*PS(1)
        A8PS2(I)=AJI8(I)*PS(2)
        A8PS5(I)=AJI8(I)*PS(5)
        A8PS6(I)=AJI8(I)*PS(6)
      ENDDO
C
      DO I=1,NB
        A3PT1(I)=AJI3(I)*PT(1)
        A3PT2(I)=AJI3(I)*PT(2)
        A3PT3(I)=AJI3(I)*PT(3)
        A3PT4(I)=AJI3(I)*PT(4)
        A6PT1(I)=AJI6(I)*PT(1)
        A6PT2(I)=AJI6(I)*PT(2)
        A6PT3(I)=AJI6(I)*PT(3)
        A6PT4(I)=AJI6(I)*PT(4)
        A9PT1(I)=AJI9(I)*PT(1)
        A9PT2(I)=AJI9(I)*PT(2)
        A9PT3(I)=AJI9(I)*PT(3)
        A9PT4(I)=AJI9(I)*PT(4)
      ENDDO
C
      DO I=1,NB
       PX1= A1PR1(I)+A2PS1(I)+A3PT1(I)       
       PX2=-A1PR1(I)+A2PS2(I)+A3PT2(I)
       PX3= A1PR3(I)-A2PS2(I)+A3PT3(I)
       PX4=-A1PR3(I)-A2PS1(I)+A3PT4(I)
       PX5= A1PR5(I)+A2PS5(I)-A3PT1(I)
       PX6=-A1PR5(I)+A2PS6(I)-A3PT2(I)
       PX7= A1PR7(I)-A2PS6(I)-A3PT3(I)
       PX8=-A1PR7(I)-A2PS5(I)-A3PT4(I)
C
       PY1= A4PR1(I)+A5PS1(I)+A6PT1(I)
       PY2=-A4PR1(I)+A5PS2(I)+A6PT2(I)
       PY3= A4PR3(I)-A5PS2(I)+A6PT3(I)
       PY4=-A4PR3(I)-A5PS1(I)+A6PT4(I)
       PY5= A4PR5(I)+A5PS5(I)-A6PT1(I)
       PY6=-A4PR5(I)+A5PS6(I)-A6PT2(I)
       PY7= A4PR7(I)-A5PS6(I)-A6PT3(I)
       PY8=-A4PR7(I)-A5PS5(I)-A6PT4(I)
C
       PZ1= A7PR1(I)+A8PS1(I)+A9PT1(I)
       PZ2=-A7PR1(I)+A8PS2(I)+A9PT2(I)
       PZ3= A7PR3(I)-A8PS2(I)+A9PT3(I)
       PZ4=-A7PR3(I)-A8PS1(I)+A9PT4(I)
       PZ5= A7PR5(I)+A8PS5(I)-A9PT1(I)
       PZ6=-A7PR5(I)+A8PS6(I)-A9PT2(I)
       PZ7= A7PR7(I)-A8PS6(I)-A9PT3(I)
       PZ8=-A7PR7(I)-A8PS5(I)-A9PT4(I)
C
       PIJ12(1,I) = PX1
       PIJ12(2,I) = PY1
       PIJ12(3,I) = PZ1
       PIJ12(4,I) = PX2
       PIJ12(5,I) = PY2
       PIJ12(6,I) = PZ2
       PIJ12(7,I) = PX3
       PIJ12(8,I) = PY3
       PIJ12(9,I) = PZ3
       PIJ12(10,I) = PX4
       PIJ12(11,I) = PY4
       PIJ12(12,I) = PZ4
       PIJ12(13,I) = PX5
       PIJ12(14,I) = PY5
       PIJ12(15,I) = PZ5
       PIJ12(16,I) = PX6
       PIJ12(17,I) = PY6
       PIJ12(18,I) = PZ6
       PIJ12(19,I) = PX7
       PIJ12(20,I) = PY7
       PIJ12(21,I) = PZ7
       PIJ12(22,I) = PX8
       PIJ12(23,I) = PY8
       PIJ12(24,I) = PZ8
      ENDDO
#include "vectorize.inc"
          DO J=1,NB
            I = INDEX(J)
            PIJ(I,1:24) = PIJ12(1:24,J)
          ENDDO      
      END IF !(L_JAC >0) THEN
#include "vectorize.inc"
          DO J=1,NB
            I = INDEX(J)
            SIGL(I,1:6) = SIG(I,1:6)
          ENDDO      
C
      RETURN
      END
Chd|====================================================================
Chd|  S8_IS17JAC_I                  source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8FUPD11T12                   source/elements/solid/solide8e/s8fupd11t12.F
Chd|-- calls ---------------
Chd|        S8ZJAC_I3                     source/elements/solid/solide8e/s8ederi3.F
Chd|        S8ZJAC_IC                     source/elements/solid/solide8e/s8ederi3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8_IS17JAC_I(ELBUF_TAB,
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              INDEX,NCH  ,NEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C------------------------------------------------------
C------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
C     REAL
      INTEGER INDEX(*),NCH   ,NEL
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   X7(*), X8(*), Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*),
     .   Y8(*), Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N   
      INTEGER IP,IR, IS, IT,JHBE,IREP,MPT,NLAY,NPTR,NPTS,NPTT,NUVAR,
     .       L_PLA,L_SIGB,NSPHDIR, NCELF, NCELL,LLPIJ,L_JAC,NNPT
C     REAL
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      my_real
     .   AJC1(MVSIZ) , AJC2(MVSIZ) , AJC3(MVSIZ) ,
     .   AJC4(MVSIZ) , AJC5(MVSIZ) , AJC6(MVSIZ) ,
     .   AJC7(MVSIZ) , AJC8(MVSIZ) , AJC9(MVSIZ) ,
     .   HX(4,MVSIZ) , HY(4,MVSIZ), HZ(4,MVSIZ),
     .   PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .   PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .   PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ)
      my_real
     .   WT,ZR,ZS,ZT,ZZ,WI
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
c---
     1 2.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 1.D0               ,1.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     3 0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 0.347854845137454D0,0.652145154862546D0,0.652145154862546D0,
     4 0.347854845137454D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 0.236926885056189D0,0.478628670499366D0,0.568888888888889D0,
     5 0.478628670499366D0,0.236926885056189D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 0.171324492379170D0,0.360761573048139D0,0.467913934572691D0,
     6 0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 0.129484966168870D0,0.279705391489277D0,0.381830050505119D0,
     7 0.417959183673469D0,0.381830050505119D0,0.279705391489277D0,
     7 0.129484966168870D0,0.D0               ,0.D0               ,
     8 0.101228536290376D0,0.222381034453374D0,0.313706645877887D0,
     8 0.362683783378362D0,0.362683783378362D0,0.313706645877887D0,
     8 0.222381034453374D0,0.101228536290376D0,0.D0               ,
     9 0.081274388361574D0,0.180648160694857D0,0.260610696402935D0,
     9 0.312347077040003D0,0.330239355001260D0,0.312347077040003D0,
     9 0.260610696402935D0,0.180648160694857D0,0.081274388361574D0/
c------------------------------------------------------------
      DATA A_GAUSS / 
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 -.577350269189625D0,0.577350269189625D0,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               , 	
     3 -.774596669241483D0,0.D0               ,0.774596669241483D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 -.861136311594053D0,-.339981043584856D0,0.339981043584856D0,
     4 0.861136311594053D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 -.906179845938664D0,-.538469310105683D0,0.D0               ,
     5 0.538469310105683D0,0.906179845938664D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 -.932469514203152D0,-.661209386466265D0,-.238619186083197D0,
     6 0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 -.949107912342759D0,-.741531185599394D0,-.405845151377397D0,
     7 0.D0               ,0.405845151377397D0,0.741531185599394D0,
     7 0.949107912342759D0,0.D0               ,0.D0               ,
     8 -.960289856497536D0,-.796666477413627D0,-.525532409916329D0,
     8 -.183434642495650D0,0.183434642495650D0,0.525532409916329D0,
     8 0.796666477413627D0,0.960289856497536D0,0.D0               ,
     9 -.968160239507626D0,-.836031107326636D0,-.613371432700590D0,
     9 -.324253423403809D0,0.D0               ,0.324253423403809D0,
     9 0.613371432700590D0,0.836031107326636D0,0.968160239507626D0/
C
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
C   cas GBUF%JAC_I  for all case
        CALL S8ZJAC_IC(
     .           X1  ,X2  ,X3  ,X4  ,X5  ,X6  ,X7  ,X8   ,
     .           Y1  ,Y2  ,Y3  ,Y4  ,Y5  ,Y6  ,Y7  ,Y8   ,
     .           Z1  ,Z2  ,Z3  ,Z4  ,Z5  ,Z6  ,Z7  ,Z8   ,
     .           AJC1 ,AJC2 ,AJC3 ,
     .           AJC4 ,AJC5 ,AJC6 ,
     .           AJC7 ,AJC8 ,AJC9 ,
     .           HX, HY, HZ,
     .           PXC1, PXC2, PXC3, PXC4, 
     .           PYC1, PYC2, PYC3, PYC4, 
     .           PZC1, PZC2, PZC3, PZC4,
     .           GBUF%JAC_I,INDEX,NCH )
        NPTR = ELBUF_TAB%NPTR
        NPTS = ELBUF_TAB%NPTS
        NPTT = ELBUF_TAB%NPTT
        LLPIJ = ELBUF_TAB%BUFLY(1)%L_PIJ
C        
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
          LBUF => ELBUF_TAB%BUFLY(1)%LBUF(IR,IS,IT)
c
           ZR = A_GAUSS(IR,NPTR)
           ZS = A_GAUSS(IS,NPTS)
           ZT = A_GAUSS(IT,NPTT)
           WT = W_GAUSS(IT,NPTT)
           IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
           WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
C   cas LBUF%L_PIJ=24 global system w/o assumed strain for Isolid=17,18 only
          CALL S8ZJAC_I3(
     .        ZR,ZS,ZT,WI,
     .        HX, HY, HZ,
     .        AJC1,AJC2,AJC3,
     .        AJC4,AJC5,AJC6,
     .        AJC7,AJC8,AJC9,LBUF%JAC_I,LLPIJ,LBUF%PIJ,LBUF%SIG,LBUF%SIGL,
     .        INDEX,NCH,NEL)
c
        ENDDO
        ENDDO
        ENDDO
C       
      RETURN
C
      END


      
