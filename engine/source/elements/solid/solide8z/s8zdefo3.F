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
Chd|  S8ZDEFO3                      source/elements/solid/solide8z/s8zdefo3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDEFO3(
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4, 
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .    WXX, WYY, WZZ,VOLO, OFF,EINT, OFFS,DSV,
     .    ICP,ICR,ICS,ICT,DTI,DSR,DSS,DST,NGL,IDEG,VOL0DP)
C                                                                     12
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
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,ICR,ICS,ICT,NGL(*),IDEG(*)
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),  
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
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), OFFS(*),DSV(*),
     .   VOLO(*),OFF(*),EINT(*),DSR(*),DSS(*),DST(*),DTI
      DOUBLE PRECISION 
     .   VOL0DP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ICOR
C     REAL
      my_real
     .   DVC(MVSIZ),DVCA(MVSIZ),DT1D2,TOL,DT1D
      DOUBLE PRECISION 
     .   DVP,DV,DV1
C                                                                     12
C-----------------------------------------------
       TOL = ONE-EM20
       DXX(LFT:LLT) =PX1(LFT:LLT)*VX1(LFT:LLT)+PX2(LFT:LLT)*VX2(LFT:LLT)+PX3(LFT:LLT)*VX3(LFT:LLT)+PX4(LFT:LLT)*VX4(LFT:LLT)
     .        +PX5(LFT:LLT)*VX5(LFT:LLT)+PX6(LFT:LLT)*VX6(LFT:LLT)+PX7(LFT:LLT)*VX7(LFT:LLT)+PX8(LFT:LLT)*VX8(LFT:LLT)
       DYY(LFT:LLT) =PY1(LFT:LLT)*VY1(LFT:LLT)+PY2(LFT:LLT)*VY2(LFT:LLT)+PY3(LFT:LLT)*VY3(LFT:LLT)+PY4(LFT:LLT)*VY4(LFT:LLT)
     .        +PY5(LFT:LLT)*VY5(LFT:LLT)+PY6(LFT:LLT)*VY6(LFT:LLT)+PY7(LFT:LLT)*VY7(LFT:LLT)+PY8(LFT:LLT)*VY8(LFT:LLT)
       DZZ(LFT:LLT) =PZ1(LFT:LLT)*VZ1(LFT:LLT)+PZ2(LFT:LLT)*VZ2(LFT:LLT)+PZ3(LFT:LLT)*VZ3(LFT:LLT)+PZ4(LFT:LLT)*VZ4(LFT:LLT)
     .        +PZ5(LFT:LLT)*VZ5(LFT:LLT)+PZ6(LFT:LLT)*VZ6(LFT:LLT)+PZ7(LFT:LLT)*VZ7(LFT:LLT)+PZ8(LFT:LLT)*VZ8(LFT:LLT)
       DXY(LFT:LLT) =PXY1(LFT:LLT)*VX1(LFT:LLT)+PXY2(LFT:LLT)*VX2(LFT:LLT)
     +        +PXY3(LFT:LLT)*VX3(LFT:LLT)+PXY4(LFT:LLT)*VX4(LFT:LLT)
     +        +PXY5(LFT:LLT)*VX5(LFT:LLT)+PXY6(LFT:LLT)*VX6(LFT:LLT)
     +        +PXY7(LFT:LLT)*VX7(LFT:LLT)+PXY8(LFT:LLT)*VX8(LFT:LLT)
       DXZ(LFT:LLT) =PXZ1(LFT:LLT)*VX1(LFT:LLT)+PXZ2(LFT:LLT)*VX2(LFT:LLT)
     +        +PXZ3(LFT:LLT)*VX3(LFT:LLT)+PXZ4(LFT:LLT)*VX4(LFT:LLT)
     +        +PXZ5(LFT:LLT)*VX5(LFT:LLT)+PXZ6(LFT:LLT)*VX6(LFT:LLT)
     +        +PXZ7(LFT:LLT)*VX7(LFT:LLT)+PXZ8(LFT:LLT)*VX8(LFT:LLT)
       DYX(LFT:LLT) =PYX1(LFT:LLT)*VY1(LFT:LLT)+PYX2(LFT:LLT)*VY2(LFT:LLT)
     +        +PYX3(LFT:LLT)*VY3(LFT:LLT)+PYX4(LFT:LLT)*VY4(LFT:LLT)
     +        +PYX5(LFT:LLT)*VY5(LFT:LLT)+PYX6(LFT:LLT)*VY6(LFT:LLT)
     +        +PYX7(LFT:LLT)*VY7(LFT:LLT)+PYX8(LFT:LLT)*VY8(LFT:LLT)
       DYZ(LFT:LLT) =PYZ1(LFT:LLT)*VY1(LFT:LLT)+PYZ2(LFT:LLT)*VY2(LFT:LLT)
     +        +PYZ3(LFT:LLT)*VY3(LFT:LLT)+PYZ4(LFT:LLT)*VY4(LFT:LLT)
     +        +PYZ5(LFT:LLT)*VY5(LFT:LLT)+PYZ6(LFT:LLT)*VY6(LFT:LLT)
     +        +PYZ7(LFT:LLT)*VY7(LFT:LLT)+PYZ8(LFT:LLT)*VY8(LFT:LLT)
       DZX(LFT:LLT) =PZX1(LFT:LLT)*VZ1(LFT:LLT)+PZX2(LFT:LLT)*VZ2(LFT:LLT)
     +        +PZX3(LFT:LLT)*VZ3(LFT:LLT)+PZX4(LFT:LLT)*VZ4(LFT:LLT)
     +        +PZX5(LFT:LLT)*VZ5(LFT:LLT)+PZX6(LFT:LLT)*VZ6(LFT:LLT)
     +        +PZX7(LFT:LLT)*VZ7(LFT:LLT)+PZX8(LFT:LLT)*VZ8(LFT:LLT)
       DZY(LFT:LLT) =PZY1(LFT:LLT)*VZ1(LFT:LLT)+PZY2(LFT:LLT)*VZ2(LFT:LLT)
     +        +PZY3(LFT:LLT)*VZ3(LFT:LLT)+PZY4(LFT:LLT)*VZ4(LFT:LLT)
     +        +PZY5(LFT:LLT)*VZ5(LFT:LLT)+PZY6(LFT:LLT)*VZ6(LFT:LLT)
     +        +PZY7(LFT:LLT)*VZ7(LFT:LLT)+PZY8(LFT:LLT)*VZ8(LFT:LLT)
        DVC(LFT:LLT) =(DXX(LFT:LLT)+DYY(LFT:LLT)+DZZ(LFT:LLT))*DT1
C  
      IF (ICP ==1) THEN
        DVCA(LFT:LLT)=PXC1(LFT:LLT)*(VX1(LFT:LLT)-VX7(LFT:LLT))+PXC2(LFT:LLT)*(VX2(LFT:LLT)-VX8(LFT:LLT))
     .        +PXC3(LFT:LLT)*(VX3(LFT:LLT)-VX5(LFT:LLT))+PXC4(LFT:LLT)*(VX4(LFT:LLT)-VX6(LFT:LLT))
     .        +PYC1(LFT:LLT)*(VY1(LFT:LLT)-VY7(LFT:LLT))+PYC2(LFT:LLT)*(VY2(LFT:LLT)-VY8(LFT:LLT))
     .        +PYC3(LFT:LLT)*(VY3(LFT:LLT)-VY5(LFT:LLT))+PYC4(LFT:LLT)*(VY4(LFT:LLT)-VY6(LFT:LLT))
     .        +PZC1(LFT:LLT)*(VZ1(LFT:LLT)-VZ7(LFT:LLT))+PZC2(LFT:LLT)*(VZ2(LFT:LLT)-VZ8(LFT:LLT))
     .        +PZC3(LFT:LLT)*(VZ3(LFT:LLT)-VZ5(LFT:LLT))+PZC4(LFT:LLT)*(VZ4(LFT:LLT)-VZ6(LFT:LLT))
        DVCA(LFT:LLT)=DVCA(LFT:LLT)*DT1
      ELSE
        DYY(LFT:LLT) =DYY(LFT:LLT)+BXY1(LFT:LLT)*VX1(LFT:LLT)+BXY2(LFT:LLT)*VX2(LFT:LLT)
     +        +BXY3(LFT:LLT)*VX3(LFT:LLT)+BXY4(LFT:LLT)*VX4(LFT:LLT)
     +        +BXY5(LFT:LLT)*VX5(LFT:LLT)+BXY6(LFT:LLT)*VX6(LFT:LLT)
     +        +BXY7(LFT:LLT)*VX7(LFT:LLT)+BXY8(LFT:LLT)*VX8(LFT:LLT)
        DZZ(LFT:LLT) =DZZ(LFT:LLT)+BXZ1(LFT:LLT)*VX1(LFT:LLT)+BXZ2(LFT:LLT)*VX2(LFT:LLT)
     +        +BXZ3(LFT:LLT)*VX3(LFT:LLT)+BXZ4(LFT:LLT)*VX4(LFT:LLT)
     +        +BXZ5(LFT:LLT)*VX5(LFT:LLT)+BXZ6(LFT:LLT)*VX6(LFT:LLT)
     +        +BXZ7(LFT:LLT)*VX7(LFT:LLT)+BXZ8(LFT:LLT)*VX8(LFT:LLT)
        DXX(LFT:LLT) =DXX(LFT:LLT)+BYX1(LFT:LLT)*VY1(LFT:LLT)+BYX2(LFT:LLT)*VY2(LFT:LLT)
     +        +BYX3(LFT:LLT)*VY3(LFT:LLT)+BYX4(LFT:LLT)*VY4(LFT:LLT)
     +        +BYX5(LFT:LLT)*VY5(LFT:LLT)+BYX6(LFT:LLT)*VY6(LFT:LLT)
     +        +BYX7(LFT:LLT)*VY7(LFT:LLT)+BYX8(LFT:LLT)*VY8(LFT:LLT)
        DZZ(LFT:LLT) =DZZ(LFT:LLT)+BYZ1(LFT:LLT)*VY1(LFT:LLT)+BYZ2(LFT:LLT)*VY2(LFT:LLT)
     +        +BYZ3(LFT:LLT)*VY3(LFT:LLT)+BYZ4(LFT:LLT)*VY4(LFT:LLT)
     +        +BYZ5(LFT:LLT)*VY5(LFT:LLT)+BYZ6(LFT:LLT)*VY6(LFT:LLT)
     +        +BYZ7(LFT:LLT)*VY7(LFT:LLT)+BYZ8(LFT:LLT)*VY8(LFT:LLT)
        DXX(LFT:LLT) =DXX(LFT:LLT)+BZX1(LFT:LLT)*VZ1(LFT:LLT)+BZX2(LFT:LLT)*VZ2(LFT:LLT)
     +        +BZX3(LFT:LLT)*VZ3(LFT:LLT)+BZX4(LFT:LLT)*VZ4(LFT:LLT)
     +        +BZX5(LFT:LLT)*VZ5(LFT:LLT)+BZX6(LFT:LLT)*VZ6(LFT:LLT)
     +        +BZX7(LFT:LLT)*VZ7(LFT:LLT)+BZX8(LFT:LLT)*VZ8(LFT:LLT)
        DYY(LFT:LLT) =DYY(LFT:LLT)+BZY1(LFT:LLT)*VZ1(LFT:LLT)+BZY2(LFT:LLT)*VZ2(LFT:LLT)
     +        +BZY3(LFT:LLT)*VZ3(LFT:LLT)+BZY4(LFT:LLT)*VZ4(LFT:LLT)
     +        +BZY5(LFT:LLT)*VZ5(LFT:LLT)+BZY6(LFT:LLT)*VZ6(LFT:LLT)
     +        +BZY7(LFT:LLT)*VZ7(LFT:LLT)+BZY8(LFT:LLT)*VZ8(LFT:LLT)
        DVCA(LFT:LLT) =(DXX(LFT:LLT)+DYY(LFT:LLT)+DZZ(LFT:LLT))*DT1
      ENDIF
C-------------Constant stress traitement------
C-------------R--direction------
      IF (ICR==1) THEN
       DO I=LFT,LLT
        DXX(I)= DXX(I)-DSR(I)*DTI
        IF(ISMDISP==0.AND.OFFS(I)/=TWO)DVCA(I)= DVCA(I)-DSR(I)
       ENDDO
      ENDIF 
C--------------S--direction--
      IF (ICS==1) THEN
       DO I=LFT,LLT
        DYY(I)= DYY(I)-DSS(I)*DTI
        IF(ISMDISP==0.AND.OFFS(I)/=TWO)DVCA(I)= DVCA(I)-DSS(I)
       ENDDO
      ENDIF 
C--------------T--direction
      IF (ICT==1) THEN
       DO I=LFT,LLT
        DZZ(I)= DZZ(I)-DST(I)*DTI
        IF(ISMDISP==0.AND.OFFS(I)/=TWO)DVCA(I)= DVCA(I)-DST(I)
       ENDDO
      ENDIF 
C-------------Volume traitement------
      DO I=LFT,LLT
        DVP=DVCA(I)-DVC(I)
        DV =DVP*OFF(I)
        IF(IDEG(I)>10) DV = ZERO
        DSV(I) = DV
      ENDDO
      IF (ISMDISP>0.AND.(ICR+ICS+ICT)==0.AND.ICP/=1) THEN
       DO I=LFT,LLT
        DSV(I) = ZERO
       ENDDO
      ELSEIF (ISMSTR==10.AND.ICP==1) THEN
       DO I=LFT,LLT
        DSV(I) = ZERO
       ENDDO
      ELSEIF (ISMSTR==12.AND.ICP==1) THEN
       DO I=LFT,LLT
        IF(OFFS(I)<=ONE) DSV(I) = ZERO
       ENDDO
      ELSEIF ((ICR+ICS+ICT)>0 ) THEN
       DO I=LFT,LLT
        IF(OFFS(I)==TWO.OR.ISMSTR==11) THEN
         IF (ICP==1) THEN
          DSV(I) =DVCA(I)-(DXX(I)+DYY(I)+DZZ(I))*DT1
         ELSE
          DSV(I) = ZERO
         END IF
        END IF
       ENDDO
      END IF 
      DO I=LFT,LLT
        IF(OFFS(I)==TWO.OR.ISMSTR==11) CYCLE
        DV=DSV(I)
        IF (ABS(DV)>TOL.AND.OFF(I)/=ZERO) THEN
          DV =ZERO
C----generally element will be switched to smstr 	  
C          OFF(I)=ZERO
C         IF (IMCONV==1) THEN
C#include "lockon.inc"
C          WRITE(ISTDO,2000) NGL(I)
C          WRITE(IOUT ,2000) NGL(I)
C#include "lockoff.inc"
C          IDEL7NOK = 1
C         ENDIF 
        ELSEIF(ABS(DV)>TOL)THEN
          DV =ZERO
        ENDIF
        IF(ISMDISP==0) THEN
         DV1 = ONE-DV
         VOLO(I) = VOLO(I)*DV1
         IF (IRESP==1) VOL0DP(I) = VOL0DP(I)*DV1
         EINT(I) = EINT(I)/DV1
        ENDIF
      ENDDO
C
      DT1D2=HALF*DT1
      IF (ISCAU>0 )DT1D2=DT1
      DT1D=TWO*DT1D2
      IF (JCVT/=0) THEN
       IF (ISMDISP>0.AND.ISCAU==0) THEN
         D4(LFT:LLT)   = DXY(LFT:LLT)+DYX(LFT:LLT)
         D5(LFT:LLT)   = DYZ(LFT:LLT)+DZY(LFT:LLT)
         D6(LFT:LLT)   = DXZ(LFT:LLT)+DZX(LFT:LLT)
         WXX(LFT:LLT)=ZERO
         WYY(LFT:LLT)=ZERO
         WZZ(LFT:LLT)=ZERO
       ELSE
        D4(LFT:LLT)   = DXY(LFT:LLT)+DYX(LFT:LLT)
     .           -DT1D*(DXX(LFT:LLT)*DXY(LFT:LLT)+DYX(LFT:LLT)*DYY(LFT:LLT)+DZX(LFT:LLT)*DZY(LFT:LLT))
        D5(LFT:LLT)   = DYZ(LFT:LLT)+DZY(LFT:LLT)
     .           -DT1D*(DYY(LFT:LLT)*DYZ(LFT:LLT)+DZY(LFT:LLT)*DZZ(LFT:LLT)+DXY(LFT:LLT)*DXZ(LFT:LLT))
        D6(LFT:LLT)   = DXZ(LFT:LLT)+DZX(LFT:LLT)
     .           -DT1D*(DZZ(LFT:LLT)*DZX(LFT:LLT)+DXZ(LFT:LLT)*DXX(LFT:LLT)+DYZ(LFT:LLT)*DYX(LFT:LLT))
        DXX(LFT:LLT)  = DXX(LFT:LLT)
     .           -DT1D2*(DXX(LFT:LLT)*DXX(LFT:LLT)+DYX(LFT:LLT)*DYX(LFT:LLT)+DZX(LFT:LLT)*DZX(LFT:LLT))
        DYY(LFT:LLT)  = DYY(LFT:LLT)
     .           -DT1D2*(DYY(LFT:LLT)*DYY(LFT:LLT)+DZY(LFT:LLT)*DZY(LFT:LLT)+DXY(LFT:LLT)*DXY(LFT:LLT))
        DZZ(LFT:LLT)  = DZZ(LFT:LLT)
     .           -DT1D2*(DZZ(LFT:LLT)*DZZ(LFT:LLT)+DXZ(LFT:LLT)*DXZ(LFT:LLT)+DYZ(LFT:LLT)*DYZ(LFT:LLT))
        WXX(LFT:LLT)=ZERO
        WYY(LFT:LLT)=ZERO
        WZZ(LFT:LLT)=ZERO
       ENDIF 
      ELSE
        D4(LFT:LLT)=DXY(LFT:LLT)+DYX(LFT:LLT)
        D5(LFT:LLT)=DYZ(LFT:LLT)+DZY(LFT:LLT)
        D6(LFT:LLT)=DXZ(LFT:LLT)+DZX(LFT:LLT)
        WZZ(LFT:LLT)=DT1D2*(DYX(LFT:LLT)-DXY(LFT:LLT))
        WYY(LFT:LLT)=DT1D2*(DXZ(LFT:LLT)-DZX(LFT:LLT))
        WXX(LFT:LLT)=DT1D2*(DZY(LFT:LLT)-DYZ(LFT:LLT))
      ENDIF
C
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I8/)
      RETURN
      END
Chd|====================================================================
Chd|  S8ZDEFOT3                     source/elements/solid/solide8z/s8zdefo3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|====================================================================
      SUBROUTINE S8ZDEFOT3(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ,ICP,
     .    DET0,JFAC,NU ,IDEG)
C                                                                     12
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
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
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
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*) ,DET0(*),JFAC(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   FAC,DET(MVSIZ),BASE
C-----------------------------------------------
       DXX(LFT:LLT) =PX1(LFT:LLT)*VX1(LFT:LLT)+PX2(LFT:LLT)*VX2(LFT:LLT)+PX3(LFT:LLT)*VX3(LFT:LLT)+PX4(LFT:LLT)*VX4(LFT:LLT)
     .        +PX5(LFT:LLT)*VX5(LFT:LLT)+PX6(LFT:LLT)*VX6(LFT:LLT)+PX7(LFT:LLT)*VX7(LFT:LLT)+PX8(LFT:LLT)*VX8(LFT:LLT)
       DYY(LFT:LLT) =PY1(LFT:LLT)*VY1(LFT:LLT)+PY2(LFT:LLT)*VY2(LFT:LLT)+PY3(LFT:LLT)*VY3(LFT:LLT)+PY4(LFT:LLT)*VY4(LFT:LLT)
     .        +PY5(LFT:LLT)*VY5(LFT:LLT)+PY6(LFT:LLT)*VY6(LFT:LLT)+PY7(LFT:LLT)*VY7(LFT:LLT)+PY8(LFT:LLT)*VY8(LFT:LLT)
       DZZ(LFT:LLT) =PZ1(LFT:LLT)*VZ1(LFT:LLT)+PZ2(LFT:LLT)*VZ2(LFT:LLT)+PZ3(LFT:LLT)*VZ3(LFT:LLT)+PZ4(LFT:LLT)*VZ4(LFT:LLT)
     .        +PZ5(LFT:LLT)*VZ5(LFT:LLT)+PZ6(LFT:LLT)*VZ6(LFT:LLT)+PZ7(LFT:LLT)*VZ7(LFT:LLT)+PZ8(LFT:LLT)*VZ8(LFT:LLT)
       DXY(LFT:LLT) =PXY1(LFT:LLT)*VX1(LFT:LLT)+PXY2(LFT:LLT)*VX2(LFT:LLT)
     +        +PXY3(LFT:LLT)*VX3(LFT:LLT)+PXY4(LFT:LLT)*VX4(LFT:LLT)
     +        +PXY5(LFT:LLT)*VX5(LFT:LLT)+PXY6(LFT:LLT)*VX6(LFT:LLT)
     +        +PXY7(LFT:LLT)*VX7(LFT:LLT)+PXY8(LFT:LLT)*VX8(LFT:LLT)
       DXZ(LFT:LLT) =PXZ1(LFT:LLT)*VX1(LFT:LLT)+PXZ2(LFT:LLT)*VX2(LFT:LLT)
     +        +PXZ3(LFT:LLT)*VX3(LFT:LLT)+PXZ4(LFT:LLT)*VX4(LFT:LLT)
     +        +PXZ5(LFT:LLT)*VX5(LFT:LLT)+PXZ6(LFT:LLT)*VX6(LFT:LLT)
     +        +PXZ7(LFT:LLT)*VX7(LFT:LLT)+PXZ8(LFT:LLT)*VX8(LFT:LLT)
       DYX(LFT:LLT) =PYX1(LFT:LLT)*VY1(LFT:LLT)+PYX2(LFT:LLT)*VY2(LFT:LLT)
     +        +PYX3(LFT:LLT)*VY3(LFT:LLT)+PYX4(LFT:LLT)*VY4(LFT:LLT)
     +        +PYX5(LFT:LLT)*VY5(LFT:LLT)+PYX6(LFT:LLT)*VY6(LFT:LLT)
     +        +PYX7(LFT:LLT)*VY7(LFT:LLT)+PYX8(LFT:LLT)*VY8(LFT:LLT)
       DYZ(LFT:LLT) =PYZ1(LFT:LLT)*VY1(LFT:LLT)+PYZ2(LFT:LLT)*VY2(LFT:LLT)
     +        +PYZ3(LFT:LLT)*VY3(LFT:LLT)+PYZ4(LFT:LLT)*VY4(LFT:LLT)
     +        +PYZ5(LFT:LLT)*VY5(LFT:LLT)+PYZ6(LFT:LLT)*VY6(LFT:LLT)
     +        +PYZ7(LFT:LLT)*VY7(LFT:LLT)+PYZ8(LFT:LLT)*VY8(LFT:LLT)
       DZX(LFT:LLT) =PZX1(LFT:LLT)*VZ1(LFT:LLT)+PZX2(LFT:LLT)*VZ2(LFT:LLT)
     +        +PZX3(LFT:LLT)*VZ3(LFT:LLT)+PZX4(LFT:LLT)*VZ4(LFT:LLT)
     +        +PZX5(LFT:LLT)*VZ5(LFT:LLT)+PZX6(LFT:LLT)*VZ6(LFT:LLT)
     +        +PZX7(LFT:LLT)*VZ7(LFT:LLT)+PZX8(LFT:LLT)*VZ8(LFT:LLT)
       DZY(LFT:LLT) =PZY1(LFT:LLT)*VZ1(LFT:LLT)+PZY2(LFT:LLT)*VZ2(LFT:LLT)
     +        +PZY3(LFT:LLT)*VZ3(LFT:LLT)+PZY4(LFT:LLT)*VZ4(LFT:LLT)
     +        +PZY5(LFT:LLT)*VZ5(LFT:LLT)+PZY6(LFT:LLT)*VZ6(LFT:LLT)
     +        +PZY7(LFT:LLT)*VZ7(LFT:LLT)+PZY8(LFT:LLT)*VZ8(LFT:LLT)
C  
      IF (ICP==1) THEN
       CALL JACOB_J33(DET,
     .                DXX, DXY, DXZ,
     .                DYX, DYY, DYZ,
     .                DZX, DZY, DZZ)
C-----F'=FAC*F=MF'+[1]-->MF'=FAC*MF+(FAC-ONE)*[1]
        DO I=LFT,LLT
         FAC=TWO_THIRD*NU(I)
         BASE = DET0(I)/MAX(EM20,DET(I))
         JFAC(I)=EXP(FAC*LOG(MAX(EM20,BASE)))
         IF (IDEG(I)>10) JFAC(I) =ONE 
        ENDDO
        DXX(LFT:LLT) = JFAC(LFT:LLT)*DXX(LFT:LLT)+JFAC(LFT:LLT)-ONE
        DYY(LFT:LLT) = JFAC(LFT:LLT)*DYY(LFT:LLT)+JFAC(LFT:LLT)-ONE
        DZZ(LFT:LLT) = JFAC(LFT:LLT)*DZZ(LFT:LLT)+JFAC(LFT:LLT)-ONE
        DXY(LFT:LLT) = JFAC(LFT:LLT)*DXY(LFT:LLT)
        DYX(LFT:LLT) = JFAC(LFT:LLT)*DYX(LFT:LLT)
        DZX(LFT:LLT) = JFAC(LFT:LLT)*DZX(LFT:LLT)
        DXZ(LFT:LLT) = JFAC(LFT:LLT)*DXZ(LFT:LLT)
        DYZ(LFT:LLT) = JFAC(LFT:LLT)*DYZ(LFT:LLT)
        DZY(LFT:LLT) = JFAC(LFT:LLT)*DZY(LFT:LLT)
      ELSE
        DYY(LFT:LLT) =DYY(LFT:LLT)+BXY1(LFT:LLT)*VX1(LFT:LLT)+BXY2(LFT:LLT)*VX2(LFT:LLT)
     +        +BXY3(LFT:LLT)*VX3(LFT:LLT)+BXY4(LFT:LLT)*VX4(LFT:LLT)
     +        +BXY5(LFT:LLT)*VX5(LFT:LLT)+BXY6(LFT:LLT)*VX6(LFT:LLT)
     +        +BXY7(LFT:LLT)*VX7(LFT:LLT)+BXY8(LFT:LLT)*VX8(LFT:LLT)
        DZZ(LFT:LLT) =DZZ(LFT:LLT)+BXZ1(LFT:LLT)*VX1(LFT:LLT)+BXZ2(LFT:LLT)*VX2(LFT:LLT)
     +        +BXZ3(LFT:LLT)*VX3(LFT:LLT)+BXZ4(LFT:LLT)*VX4(LFT:LLT)
     +        +BXZ5(LFT:LLT)*VX5(LFT:LLT)+BXZ6(LFT:LLT)*VX6(LFT:LLT)
     +        +BXZ7(LFT:LLT)*VX7(LFT:LLT)+BXZ8(LFT:LLT)*VX8(LFT:LLT)
        DXX(LFT:LLT) =DXX(LFT:LLT)+BYX1(LFT:LLT)*VY1(LFT:LLT)+BYX2(LFT:LLT)*VY2(LFT:LLT)
     +        +BYX3(LFT:LLT)*VY3(LFT:LLT)+BYX4(LFT:LLT)*VY4(LFT:LLT)
     +        +BYX5(LFT:LLT)*VY5(LFT:LLT)+BYX6(LFT:LLT)*VY6(LFT:LLT)
     +        +BYX7(LFT:LLT)*VY7(LFT:LLT)+BYX8(LFT:LLT)*VY8(LFT:LLT)
        DZZ(LFT:LLT) =DZZ(LFT:LLT)+BYZ1(LFT:LLT)*VY1(LFT:LLT)+BYZ2(LFT:LLT)*VY2(LFT:LLT)
     +        +BYZ3(LFT:LLT)*VY3(LFT:LLT)+BYZ4(LFT:LLT)*VY4(LFT:LLT)
     +        +BYZ5(LFT:LLT)*VY5(LFT:LLT)+BYZ6(LFT:LLT)*VY6(LFT:LLT)
     +        +BYZ7(LFT:LLT)*VY7(LFT:LLT)+BYZ8(LFT:LLT)*VY8(LFT:LLT)
        DXX(LFT:LLT) =DXX(LFT:LLT)+BZX1(LFT:LLT)*VZ1(LFT:LLT)+BZX2(LFT:LLT)*VZ2(LFT:LLT)
     +        +BZX3(LFT:LLT)*VZ3(LFT:LLT)+BZX4(LFT:LLT)*VZ4(LFT:LLT)
     +        +BZX5(LFT:LLT)*VZ5(LFT:LLT)+BZX6(LFT:LLT)*VZ6(LFT:LLT)
     +        +BZX7(LFT:LLT)*VZ7(LFT:LLT)+BZX8(LFT:LLT)*VZ8(LFT:LLT)
        DYY(LFT:LLT) =DYY(LFT:LLT)+BZY1(LFT:LLT)*VZ1(LFT:LLT)+BZY2(LFT:LLT)*VZ2(LFT:LLT)
     +        +BZY3(LFT:LLT)*VZ3(LFT:LLT)+BZY4(LFT:LLT)*VZ4(LFT:LLT)
     +        +BZY5(LFT:LLT)*VZ5(LFT:LLT)+BZY6(LFT:LLT)*VZ6(LFT:LLT)
     +        +BZY7(LFT:LLT)*VZ7(LFT:LLT)+BZY8(LFT:LLT)*VZ8(LFT:LLT)
      ENDIF !IF (ICP==1) THEN
C
      RETURN
      END
