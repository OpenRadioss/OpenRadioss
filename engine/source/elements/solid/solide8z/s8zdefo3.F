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
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDEFO3(
     1   PXC1,    PXC2,    PXC3,    PXC4,
     2   PYC1,    PYC2,    PYC3,    PYC4,
     3   PZC1,    PZC2,    PZC3,    PZC4,
     4   PX1,     PX2,     PX3,     PX4,
     5   PX5,     PX6,     PX7,     PX8,
     6   PY1,     PY2,     PY3,     PY4,
     7   PY5,     PY6,     PY7,     PY8,
     8   PZ1,     PZ2,     PZ3,     PZ4,
     9   PZ5,     PZ6,     PZ7,     PZ8,
     A   PXY1,    PXY2,    PXY3,    PXY4,
     B   PXY5,    PXY6,    PXY7,    PXY8,
     C   PYX1,    PYX2,    PYX3,    PYX4,
     D   PYX5,    PYX6,    PYX7,    PYX8,
     E   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     F   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     G   PZX1,    PZX2,    PZX3,    PZX4,
     H   PZX5,    PZX6,    PZX7,    PZX8,
     I   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     J   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     K   PZY1,    PZY2,    PZY3,    PZY4,
     L   PZY5,    PZY6,    PZY7,    PZY8,
     M   BXY1,    BXY2,    BXY3,    BXY4,
     N   BXY5,    BXY6,    BXY7,    BXY8,
     O   BYX1,    BYX2,    BYX3,    BYX4,
     P   BYX5,    BYX6,    BYX7,    BYX8,
     Q   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     R   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     S   BZX1,    BZX2,    BZX3,    BZX4,
     T   BZX5,    BZX6,    BZX7,    BZX8,
     U   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     V   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     W   BZY1,    BZY2,    BZY3,    BZY4,
     X   BZY5,    BZY6,    BZY7,    BZY8,
     Y   VX1,     VX2,     VX3,     VX4,
     Z   VX5,     VX6,     VX7,     VX8,
     1   VY1,     VY2,     VY3,     VY4,
     2   VY5,     VY6,     VY7,     VY8,
     3   VZ1,     VZ2,     VZ3,     VZ4,
     4   VZ5,     VZ6,     VZ7,     VZ8,
     5   DXX,     DXY,     DXZ,     DYX,
     6   DYY,     DYZ,     DZX,     DZY,
     7   DZZ,     D4,      D5,      D6,
     8   WXX,     WYY,     WZZ,     VOLO,
     9   OFF,     EINT,    OFFS,    DSV,
     A   ICP,     ICR,     ICS,     ICT,
     B   DTI,     DSR,     DSS,     DST,
     C   NGL,     IDEG,    VOL0DP,  NEL,
     D   ISMSTR,  JCVT)
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
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JCVT
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
       DXX(1:NEL) =PX1(1:NEL)*VX1(1:NEL)+PX2(1:NEL)*VX2(1:NEL)+PX3(1:NEL)*VX3(1:NEL)+PX4(1:NEL)*VX4(1:NEL)
     .        +PX5(1:NEL)*VX5(1:NEL)+PX6(1:NEL)*VX6(1:NEL)+PX7(1:NEL)*VX7(1:NEL)+PX8(1:NEL)*VX8(1:NEL)
       DYY(1:NEL) =PY1(1:NEL)*VY1(1:NEL)+PY2(1:NEL)*VY2(1:NEL)+PY3(1:NEL)*VY3(1:NEL)+PY4(1:NEL)*VY4(1:NEL)
     .        +PY5(1:NEL)*VY5(1:NEL)+PY6(1:NEL)*VY6(1:NEL)+PY7(1:NEL)*VY7(1:NEL)+PY8(1:NEL)*VY8(1:NEL)
       DZZ(1:NEL) =PZ1(1:NEL)*VZ1(1:NEL)+PZ2(1:NEL)*VZ2(1:NEL)+PZ3(1:NEL)*VZ3(1:NEL)+PZ4(1:NEL)*VZ4(1:NEL)
     .        +PZ5(1:NEL)*VZ5(1:NEL)+PZ6(1:NEL)*VZ6(1:NEL)+PZ7(1:NEL)*VZ7(1:NEL)+PZ8(1:NEL)*VZ8(1:NEL)
       DXY(1:NEL) =PXY1(1:NEL)*VX1(1:NEL)+PXY2(1:NEL)*VX2(1:NEL)
     +        +PXY3(1:NEL)*VX3(1:NEL)+PXY4(1:NEL)*VX4(1:NEL)
     +        +PXY5(1:NEL)*VX5(1:NEL)+PXY6(1:NEL)*VX6(1:NEL)
     +        +PXY7(1:NEL)*VX7(1:NEL)+PXY8(1:NEL)*VX8(1:NEL)
       DXZ(1:NEL) =PXZ1(1:NEL)*VX1(1:NEL)+PXZ2(1:NEL)*VX2(1:NEL)
     +        +PXZ3(1:NEL)*VX3(1:NEL)+PXZ4(1:NEL)*VX4(1:NEL)
     +        +PXZ5(1:NEL)*VX5(1:NEL)+PXZ6(1:NEL)*VX6(1:NEL)
     +        +PXZ7(1:NEL)*VX7(1:NEL)+PXZ8(1:NEL)*VX8(1:NEL)
       DYX(1:NEL) =PYX1(1:NEL)*VY1(1:NEL)+PYX2(1:NEL)*VY2(1:NEL)
     +        +PYX3(1:NEL)*VY3(1:NEL)+PYX4(1:NEL)*VY4(1:NEL)
     +        +PYX5(1:NEL)*VY5(1:NEL)+PYX6(1:NEL)*VY6(1:NEL)
     +        +PYX7(1:NEL)*VY7(1:NEL)+PYX8(1:NEL)*VY8(1:NEL)
       DYZ(1:NEL) =PYZ1(1:NEL)*VY1(1:NEL)+PYZ2(1:NEL)*VY2(1:NEL)
     +        +PYZ3(1:NEL)*VY3(1:NEL)+PYZ4(1:NEL)*VY4(1:NEL)
     +        +PYZ5(1:NEL)*VY5(1:NEL)+PYZ6(1:NEL)*VY6(1:NEL)
     +        +PYZ7(1:NEL)*VY7(1:NEL)+PYZ8(1:NEL)*VY8(1:NEL)
       DZX(1:NEL) =PZX1(1:NEL)*VZ1(1:NEL)+PZX2(1:NEL)*VZ2(1:NEL)
     +        +PZX3(1:NEL)*VZ3(1:NEL)+PZX4(1:NEL)*VZ4(1:NEL)
     +        +PZX5(1:NEL)*VZ5(1:NEL)+PZX6(1:NEL)*VZ6(1:NEL)
     +        +PZX7(1:NEL)*VZ7(1:NEL)+PZX8(1:NEL)*VZ8(1:NEL)
       DZY(1:NEL) =PZY1(1:NEL)*VZ1(1:NEL)+PZY2(1:NEL)*VZ2(1:NEL)
     +        +PZY3(1:NEL)*VZ3(1:NEL)+PZY4(1:NEL)*VZ4(1:NEL)
     +        +PZY5(1:NEL)*VZ5(1:NEL)+PZY6(1:NEL)*VZ6(1:NEL)
     +        +PZY7(1:NEL)*VZ7(1:NEL)+PZY8(1:NEL)*VZ8(1:NEL)
        DVC(1:NEL) =(DXX(1:NEL)+DYY(1:NEL)+DZZ(1:NEL))*DT1
C  
      IF (ICP ==1) THEN
        DVCA(1:NEL)=PXC1(1:NEL)*(VX1(1:NEL)-VX7(1:NEL))+PXC2(1:NEL)*(VX2(1:NEL)-VX8(1:NEL))
     .        +PXC3(1:NEL)*(VX3(1:NEL)-VX5(1:NEL))+PXC4(1:NEL)*(VX4(1:NEL)-VX6(1:NEL))
     .        +PYC1(1:NEL)*(VY1(1:NEL)-VY7(1:NEL))+PYC2(1:NEL)*(VY2(1:NEL)-VY8(1:NEL))
     .        +PYC3(1:NEL)*(VY3(1:NEL)-VY5(1:NEL))+PYC4(1:NEL)*(VY4(1:NEL)-VY6(1:NEL))
     .        +PZC1(1:NEL)*(VZ1(1:NEL)-VZ7(1:NEL))+PZC2(1:NEL)*(VZ2(1:NEL)-VZ8(1:NEL))
     .        +PZC3(1:NEL)*(VZ3(1:NEL)-VZ5(1:NEL))+PZC4(1:NEL)*(VZ4(1:NEL)-VZ6(1:NEL))
        DVCA(1:NEL)=DVCA(1:NEL)*DT1
      ELSE
        DYY(1:NEL) =DYY(1:NEL)+BXY1(1:NEL)*VX1(1:NEL)+BXY2(1:NEL)*VX2(1:NEL)
     +        +BXY3(1:NEL)*VX3(1:NEL)+BXY4(1:NEL)*VX4(1:NEL)
     +        +BXY5(1:NEL)*VX5(1:NEL)+BXY6(1:NEL)*VX6(1:NEL)
     +        +BXY7(1:NEL)*VX7(1:NEL)+BXY8(1:NEL)*VX8(1:NEL)
        DZZ(1:NEL) =DZZ(1:NEL)+BXZ1(1:NEL)*VX1(1:NEL)+BXZ2(1:NEL)*VX2(1:NEL)
     +        +BXZ3(1:NEL)*VX3(1:NEL)+BXZ4(1:NEL)*VX4(1:NEL)
     +        +BXZ5(1:NEL)*VX5(1:NEL)+BXZ6(1:NEL)*VX6(1:NEL)
     +        +BXZ7(1:NEL)*VX7(1:NEL)+BXZ8(1:NEL)*VX8(1:NEL)
        DXX(1:NEL) =DXX(1:NEL)+BYX1(1:NEL)*VY1(1:NEL)+BYX2(1:NEL)*VY2(1:NEL)
     +        +BYX3(1:NEL)*VY3(1:NEL)+BYX4(1:NEL)*VY4(1:NEL)
     +        +BYX5(1:NEL)*VY5(1:NEL)+BYX6(1:NEL)*VY6(1:NEL)
     +        +BYX7(1:NEL)*VY7(1:NEL)+BYX8(1:NEL)*VY8(1:NEL)
        DZZ(1:NEL) =DZZ(1:NEL)+BYZ1(1:NEL)*VY1(1:NEL)+BYZ2(1:NEL)*VY2(1:NEL)
     +        +BYZ3(1:NEL)*VY3(1:NEL)+BYZ4(1:NEL)*VY4(1:NEL)
     +        +BYZ5(1:NEL)*VY5(1:NEL)+BYZ6(1:NEL)*VY6(1:NEL)
     +        +BYZ7(1:NEL)*VY7(1:NEL)+BYZ8(1:NEL)*VY8(1:NEL)
        DXX(1:NEL) =DXX(1:NEL)+BZX1(1:NEL)*VZ1(1:NEL)+BZX2(1:NEL)*VZ2(1:NEL)
     +        +BZX3(1:NEL)*VZ3(1:NEL)+BZX4(1:NEL)*VZ4(1:NEL)
     +        +BZX5(1:NEL)*VZ5(1:NEL)+BZX6(1:NEL)*VZ6(1:NEL)
     +        +BZX7(1:NEL)*VZ7(1:NEL)+BZX8(1:NEL)*VZ8(1:NEL)
        DYY(1:NEL) =DYY(1:NEL)+BZY1(1:NEL)*VZ1(1:NEL)+BZY2(1:NEL)*VZ2(1:NEL)
     +        +BZY3(1:NEL)*VZ3(1:NEL)+BZY4(1:NEL)*VZ4(1:NEL)
     +        +BZY5(1:NEL)*VZ5(1:NEL)+BZY6(1:NEL)*VZ6(1:NEL)
     +        +BZY7(1:NEL)*VZ7(1:NEL)+BZY8(1:NEL)*VZ8(1:NEL)
        DVCA(1:NEL) =(DXX(1:NEL)+DYY(1:NEL)+DZZ(1:NEL))*DT1
      ENDIF
C-------------Constant stress traitement------
C-------------R--direction------
      IF (ICR==1) THEN
       DO I=1,NEL
        DXX(I)= DXX(I)-DSR(I)*DTI
        IF(ISMDISP==0.AND.OFFS(I)/=TWO)DVCA(I)= DVCA(I)-DSR(I)
       ENDDO
      ENDIF 
C--------------S--direction--
      IF (ICS==1) THEN
       DO I=1,NEL
        DYY(I)= DYY(I)-DSS(I)*DTI
        IF(ISMDISP==0.AND.OFFS(I)/=TWO)DVCA(I)= DVCA(I)-DSS(I)
       ENDDO
      ENDIF 
C--------------T--direction
      IF (ICT==1) THEN
       DO I=1,NEL
        DZZ(I)= DZZ(I)-DST(I)*DTI
        IF(ISMDISP==0.AND.OFFS(I)/=TWO)DVCA(I)= DVCA(I)-DST(I)
       ENDDO
      ENDIF 
C-------------Volume traitement------
      DO I=1,NEL
        DVP=DVCA(I)-DVC(I)
        DV =DVP*OFF(I)
        IF(IDEG(I)>10) DV = ZERO
        DSV(I) = DV
      ENDDO
      IF (ISMDISP>0.AND.(ICR+ICS+ICT)==0.AND.ICP/=1) THEN
       DO I=1,NEL
        DSV(I) = ZERO
       ENDDO
      ELSEIF (ISMSTR==10.AND.ICP==1) THEN
       DO I=1,NEL
        DSV(I) = ZERO
       ENDDO
      ELSEIF (ISMSTR==12.AND.ICP==1) THEN
       DO I=1,NEL
        IF(OFFS(I)<=ONE) DSV(I) = ZERO
       ENDDO
      ELSEIF ((ICR+ICS+ICT)>0 ) THEN
       DO I=1,NEL
        IF(OFFS(I)==TWO.OR.ISMSTR==11) THEN
         IF (ICP==1) THEN
          DSV(I) =DVCA(I)-(DXX(I)+DYY(I)+DZZ(I))*DT1
         ELSE
          DSV(I) = ZERO
         END IF
        END IF
       ENDDO
      END IF 
      DO I=1,NEL
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
         D4(1:NEL)   = DXY(1:NEL)+DYX(1:NEL)
         D5(1:NEL)   = DYZ(1:NEL)+DZY(1:NEL)
         D6(1:NEL)   = DXZ(1:NEL)+DZX(1:NEL)
         WXX(1:NEL)=ZERO
         WYY(1:NEL)=ZERO
         WZZ(1:NEL)=ZERO
       ELSE
        D4(1:NEL)   = DXY(1:NEL)+DYX(1:NEL)
     .           -DT1D*(DXX(1:NEL)*DXY(1:NEL)+DYX(1:NEL)*DYY(1:NEL)+DZX(1:NEL)*DZY(1:NEL))
        D5(1:NEL)   = DYZ(1:NEL)+DZY(1:NEL)
     .           -DT1D*(DYY(1:NEL)*DYZ(1:NEL)+DZY(1:NEL)*DZZ(1:NEL)+DXY(1:NEL)*DXZ(1:NEL))
        D6(1:NEL)   = DXZ(1:NEL)+DZX(1:NEL)
     .           -DT1D*(DZZ(1:NEL)*DZX(1:NEL)+DXZ(1:NEL)*DXX(1:NEL)+DYZ(1:NEL)*DYX(1:NEL))
        DXX(1:NEL)  = DXX(1:NEL)
     .           -DT1D2*(DXX(1:NEL)*DXX(1:NEL)+DYX(1:NEL)*DYX(1:NEL)+DZX(1:NEL)*DZX(1:NEL))
        DYY(1:NEL)  = DYY(1:NEL)
     .           -DT1D2*(DYY(1:NEL)*DYY(1:NEL)+DZY(1:NEL)*DZY(1:NEL)+DXY(1:NEL)*DXY(1:NEL))
        DZZ(1:NEL)  = DZZ(1:NEL)
     .           -DT1D2*(DZZ(1:NEL)*DZZ(1:NEL)+DXZ(1:NEL)*DXZ(1:NEL)+DYZ(1:NEL)*DYZ(1:NEL))
        WXX(1:NEL)=ZERO
        WYY(1:NEL)=ZERO
        WZZ(1:NEL)=ZERO
       ENDIF 
      ELSE
        D4(1:NEL)=DXY(1:NEL)+DYX(1:NEL)
        D5(1:NEL)=DYZ(1:NEL)+DZY(1:NEL)
        D6(1:NEL)=DXZ(1:NEL)+DZX(1:NEL)
        WZZ(1:NEL)=DT1D2*(DYX(1:NEL)-DXY(1:NEL))
        WYY(1:NEL)=DT1D2*(DXZ(1:NEL)-DZX(1:NEL))
        WXX(1:NEL)=DT1D2*(DZY(1:NEL)-DYZ(1:NEL))
      ENDIF
C
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I8/)
      RETURN
      END
