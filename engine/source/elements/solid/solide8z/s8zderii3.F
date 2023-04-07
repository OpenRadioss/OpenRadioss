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
Chd|  S8ZDERII3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERII3(
     1   OFFG,    OFF,     VOL,     NGL,
     2   KSI,     ETA,     ZETA,    WI,
     3   PXC1,    PXC2,    PXC3,    PXC4,
     4   PYC1,    PYC2,    PYC3,    PYC4,
     5   PZC1,    PZC2,    PZC3,    PZC4,
     6   PX1H1,   PX1H2,   PX1H3,   PX1H4,
     7   PX2H1,   PX2H2,   PX2H3,   PX2H4,
     8   PX3H1,   PX3H2,   PX3H3,   PX3H4,
     9   PX4H1,   PX4H2,   PX4H3,   PX4H4,
     A   HX,      HY,      HZ,      PX1,
     B   PX2,     PX3,     PX4,     PX5,
     C   PX6,     PX7,     PX8,     PY1,
     D   PY2,     PY3,     PY4,     PY5,
     E   PY6,     PY7,     PY8,     PZ1,
     F   PZ2,     PZ3,     PZ4,     PZ5,
     G   PZ6,     PZ7,     PZ8,     PXY1,
     H   PXY2,    PXY3,    PXY4,    PXY5,
     I   PXY6,    PXY7,    PXY8,    PYX1,
     J   PYX2,    PYX3,    PYX4,    PYX5,
     K   PYX6,    PYX7,    PYX8,    PXZ1,
     L   PXZ2,    PXZ3,    PXZ4,    PXZ5,
     M   PXZ6,    PXZ7,    PXZ8,    PZX1,
     N   PZX2,    PZX3,    PZX4,    PZX5,
     O   PZX6,    PZX7,    PZX8,    PYZ1,
     P   PYZ2,    PYZ3,    PYZ4,    PYZ5,
     Q   PYZ6,    PYZ7,    PYZ8,    PZY1,
     R   PZY2,    PZY3,    PZY4,    PZY5,
     S   PZY6,    PZY7,    PZY8,    BXY1,
     T   BXY2,    BXY3,    BXY4,    BXY5,
     U   BXY6,    BXY7,    BXY8,    BYX1,
     V   BYX2,    BYX3,    BYX4,    BYX5,
     W   BYX6,    BYX7,    BYX8,    BXZ1,
     X   BXZ2,    BXZ3,    BXZ4,    BXZ5,
     Y   BXZ6,    BXZ7,    BXZ8,    BZX1,
     Z   BZX2,    BZX3,    BZX4,    BZX5,
     1   BZX6,    BZX7,    BZX8,    BYZ1,
     2   BYZ2,    BYZ3,    BYZ4,    BYZ5,
     3   BYZ6,    BYZ7,    BYZ8,    BZY1,
     4   BZY2,    BZY3,    BZY4,    BZY5,
     5   BZY6,    BZY7,    BZY8,    CJ1,
     6   CJ2,     CJ3,     CJ4,     CJ5,
     7   CJ6,     CJ7,     CJ8,     CJ9,
     8   JAC4,    JAC5,    JAC6,    JAC7,
     9   JAC8,    JAC9,    SMAX,    DELTAX,
     A   NU,      ICP,     IDEG,    NEL,
     B   MTN,     ISMSTR,  JHBE)
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JHBE
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
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
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC4,JAC5,JAC6,
     .   JAC7,JAC8,JAC9,
     .   SMAX(*),DELTAX(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT , NU1(MVSIZ),  
     .   JAC1,JAC2,JAC3,
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JACI12(MVSIZ), JACI45(MVSIZ), JACI78(MVSIZ),
     .   D1X(MVSIZ) , D2X(MVSIZ) , D3X(MVSIZ) , D4X(MVSIZ) ,
     .   D1Y(MVSIZ) , D2Y(MVSIZ) , D3Y(MVSIZ) , D4Y(MVSIZ) ,
     .   D1Z(MVSIZ) , D2Z(MVSIZ) , D3Z(MVSIZ) , D4Z(MVSIZ) ,
     .   XG1(MVSIZ), XG2(MVSIZ), XG3(MVSIZ), XG4(MVSIZ),  
     .   YG1(MVSIZ), YG2(MVSIZ), YG3(MVSIZ), YG4(MVSIZ),   
     .   ZG1(MVSIZ), ZG2(MVSIZ), ZG3(MVSIZ), ZG4(MVSIZ), 
     .   F1,F2,F3,XS,XAS,YS,YAS,ZS,ZAS,CS,CAS ,BXHI,BYHI,BZHI
C-----------------------------------------------
C--debug for termes PXYj... consisting w/ S8ZFORC3----
      DO I=1,NEL
       JAC1=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       JAC7=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       JAC8=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       JAC9=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
C
C     JACOBIAN
C
       JAC_59_68(I)=JAC5*JAC9-JAC6*JAC8
       JAC_67_49(I)=JAC6*JAC7-JAC4*JAC9
       JAC_38_29(I)=(-JAC2*JAC9+JAC3*JAC8)
       JAC_19_37(I)=( JAC1*JAC9-JAC3*JAC7)
       JAC_27_18(I)=(-JAC1*JAC8+JAC2*JAC7)
       JAC_26_35(I)=( JAC2*JAC6-JAC3*JAC5)
       JAC_34_16(I)=(-JAC1*JAC6+JAC3*JAC4)
       JAC_15_24(I)=( JAC1*JAC5-JAC2*JAC4)
       JAC_48_57(I)=JAC4*JAC8-JAC5*JAC7
C 
       DET(I)=ONE_OVER_512*(JAC1*JAC_59_68(I)+JAC2*JAC_67_49(I)+JAC3*JAC_48_57(I))
       VOL(I)= WI*DET(I)
      ENDDO
C
      ICOR = 0
      DO I=1,NEL
        OFF(I)=OFFG(I)
        IF(OFF(I)==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) VOL(I)=ONE
        ELSEIF (VOL(I)<=ZERO ) THEN
         VOL(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=1,NEL
          IF(VOL(I)<=ZERO.AND.OFF(I)/=ZERO)THEN
            VOL(I)= EM20
            OFF(I) =ZERO 
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
      F1 = ETA*ZETA
      F2 = KSI*ZETA
      F3 = KSI*ETA
      DO I=1,NEL
        DETT=ONE_OVER_512/DET(I)
       JACI1=DETT*JAC_59_68(I)
       JACI4=DETT*JAC_67_49(I)
       JACI7=DETT*JAC_48_57(I)
       JACI2=DETT*JAC_38_29(I)
       JACI5=DETT*JAC_19_37(I)
       JACI8=DETT*JAC_27_18(I)
       JACI3=DETT*JAC_26_35(I)
       JACI6=DETT*JAC_34_16(I)
       JACI9=DETT*JAC_15_24(I)
C
       NU1(I) = NU(I)/(ONE - NU(I))
C
       D1X(I)=JACI3*ETA +JACI2*ZETA
       D2X(I)=JACI1*ZETA +JACI3*KSI
       D3X(I)=JACI2*KSI +JACI1*ETA
       D4X(I)=JACI1*F1+JACI2*F2+JACI3*F3
C
       D1Y(I)=JACI6*ETA +JACI5*ZETA
       D2Y(I)=JACI4*ZETA +JACI6*KSI
       D3Y(I)=JACI5*KSI +JACI4*ETA
       D4Y(I)=JACI4*F1+JACI5*F2+JACI6*F3
C
       D1Z(I)=JACI9*ETA +JACI8*ZETA
       D2Z(I)=JACI7*ZETA +JACI9*KSI
       D3Z(I)=JACI8*KSI +JACI7*ETA
       D4Z(I)=JACI7*F1+JACI8*F2+JACI9*F3
      ENDDO
C-------Node 1,7-----
      DO I=1,NEL
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
      DO I=1,NEL
       XS =D1X(I)+D2X(I)+D3X(I)
       XAS =D4X(I)+XG1(I)+XG2(I)+XG3(I)+XG4(I)-PXC1(I)
       PX1(I)=XS-XAS
       PX7(I)=XS+XAS
       YS =D1Y(I)+D2Y(I)+D3Y(I)
       YAS =D4Y(I)+YG1(I)+YG2(I)+YG3(I)+YG4(I)-PYC1(I)
       PY1(I)=YS-YAS
       PY7(I)=YS+YAS
       ZS =D1Z(I)+D2Z(I)+D3Z(I)
       ZAS =D4Z(I)+ZG1(I)+ZG2(I)+ZG3(I)+ZG4(I)-PZC1(I)
       PZ1(I)=ZS-ZAS
       PZ7(I)=ZS+ZAS
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
C  
      IF (ICP /= 1 .AND. ICP /= 11) THEN
       DO I=1,NEL
       CAS =-NU(I)*(D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY1(I)=-XAS
       BXY7(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ1(I)=-XAS
       BXZ7(I)=XAS
       CAS =-NU(I)*(D4Y(I)+YG2(I)+YG4(I))
C      
       YAS =-NU1(I)*YG3(I)+CAS
       BYX1(I)=-YAS
       BYX7(I)=YAS
       YAS =-NU1(I)*YG1(I)+CAS
       BYZ1(I)=-YAS
       BYZ7(I)=YAS
       CAS =-NU(I)*(D4Z(I)+ZG3(I)+ZG4(I))
       ZAS =-NU1(I)*ZG2(I)+CAS
       BZX1(I)=-ZAS
       BZX7(I)=ZAS
       ZAS =-NU1(I)*ZG1(I)+CAS
       BZY1(I)=-ZAS
       BZY7(I)=ZAS
       ENDDO
      ENDIF
C-------Node 2,8-----
      DO I=1,NEL
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
      DO I=1,NEL
       XS =D1X(I)-D2X(I)-D3X(I)
       XAS =-D4X(I)+XG1(I)+XG2(I)+XG3(I)+XG4(I)-PXC2(I)
       PX2(I)=XS-XAS
       PX8(I)=XS+XAS
       YS =D1Y(I)-D2Y(I)-D3Y(I)
       YAS =-D4Y(I)+YG1(I)+YG2(I)+YG3(I)+YG4(I)-PYC2(I)
       PY2(I)=YS-YAS
       PY8(I)=YS+YAS
       ZS =D1Z(I)-D2Z(I)-D3Z(I)
       ZAS =-D4Z(I)+ZG1(I)+ZG2(I)+ZG3(I)+ZG4(I)-PZC2(I)
       PZ2(I)=ZS-ZAS
       PZ8(I)=ZS+ZAS
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
C
      IF (ICP /= 1 .AND. ICP /= 11) THEN
      DO I=1,NEL
       CAS =-NU(I)*(-D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY2(I)=-XAS
       BXY8(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ2(I)=-XAS
       BXZ8(I)=XAS
       CAS =-NU(I)*(-D4Y(I)+YG2(I)+YG4(I))
       YAS =-NU1(I)*YG3(I)+CAS
       BYX2(I)=-YAS
       BYX8(I)=YAS
       YAS =-NU1(I)*YG1(I)+CAS
       BYZ2(I)=-YAS
       BYZ8(I)=YAS
       CAS =-NU(I)*(-D4Z(I)+ZG3(I)+ZG4(I))
       ZAS =-NU1(I)*ZG2(I)+CAS
       BZX2(I)=-ZAS
       BZX8(I)=ZAS
       ZAS =-NU1(I)*ZG1(I)+CAS
       BZY2(I)=-ZAS
       BZY8(I)=ZAS
      ENDDO
      ENDIF
C
C-------Node 3,5-----
      DO I=1,NEL
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
      DO I=1,NEL
       XS =-D1X(I)-D2X(I)+D3X(I)
       XAS =D4X(I)+XG1(I)+XG2(I)+XG3(I)+XG4(I)-PXC3(I)
       PX3(I)=XS-XAS
       PX5(I)=XS+XAS
       YS =-D1Y(I)-D2Y(I)+D3Y(I)
       YAS =D4Y(I)+YG1(I)+YG2(I)+YG3(I)+YG4(I)-PYC3(I)
       PY3(I)=YS-YAS
       PY5(I)=YS+YAS
       ZS =-D1Z(I)-D2Z(I)+D3Z(I)
       ZAS =D4Z(I)+ZG1(I)+ZG2(I)+ZG3(I)+ZG4(I)-PZC3(I)
       PZ3(I)=ZS-ZAS
       PZ5(I)=ZS+ZAS
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
C  
      IF (ICP /= 1 .AND. ICP /= 11) THEN
      DO I=1,NEL
       CAS =-NU(I)*(D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY3(I)=-XAS
       BXY5(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ3(I)=-XAS
       BXZ5(I)=XAS
       CAS =-NU(I)*(D4Y(I)+YG2(I)+YG4(I))
       YAS =-NU1(I)*YG3(I)+CAS
       BYX3(I)=-YAS
       BYX5(I)=YAS
       YAS =-NU1(I)*YG1(I)+CAS
       BYZ3(I)=-YAS
       BYZ5(I)=YAS
       CAS =-NU(I)*(D4Z(I)+ZG3(I)+ZG4(I))
       ZAS =-NU1(I)*ZG2(I)+CAS
       BZX3(I)=-ZAS
       BZX5(I)=ZAS
       ZAS =-NU1(I)*ZG1(I)+CAS
       BZY3(I)=-ZAS
       BZY5(I)=ZAS
      ENDDO
      ENDIF
C-------Node 4,6-----
      DO I=1,NEL
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
      DO I=1,NEL
       XS =-D1X(I)+D2X(I)-D3X(I)
       XAS =-D4X(I)+XG1(I)+XG2(I)+XG3(I)+XG4(I)-PXC4(I)
       PX4(I)=XS-XAS
       PX6(I)=XS+XAS
       YS =-D1Y(I)+D2Y(I)-D3Y(I)
       YAS =-D4Y(I)+YG1(I)+YG2(I)+YG3(I)+YG4(I)-PYC4(I)
       PY4(I)=YS-YAS
       PY6(I)=YS+YAS
       ZS =-D1Z(I)+D2Z(I)-D3Z(I)
       ZAS =-D4Z(I)+ZG1(I)+ZG2(I)+ZG3(I)+ZG4(I)-PZC4(I)
       PZ4(I)=ZS-ZAS
       PZ6(I)=ZS+ZAS
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
      IF (ICP /= 1 .AND. ICP /= 11) THEN
       DO I=1,NEL
        CAS =-NU(I)*(-D4X(I)+XG1(I)+XG4(I))
        XAS =-NU1(I)*XG3(I)+CAS
        BXY4(I)=-XAS
        BXY6(I)=XAS
        XAS =-NU1(I)*XG2(I)+CAS
        BXZ4(I)=-XAS
        BXZ6(I)=XAS
        CAS =-NU(I)*(-D4Y(I)+YG2(I)+YG4(I))
        YAS =-NU1(I)*YG3(I)+CAS
        BYX4(I)=-YAS
        BYX6(I)=YAS
        YAS =-NU1(I)*YG1(I)+CAS
        BYZ4(I)=-YAS
        BYZ6(I)=YAS
        CAS =-NU(I)*(-D4Z(I)+ZG3(I)+ZG4(I))
        ZAS =-NU1(I)*ZG2(I)+CAS
        BZX4(I)=-ZAS
        BZX6(I)=ZAS
        ZAS =-NU1(I)*ZG1(I)+CAS
        BZY4(I)=-ZAS
        BZY6(I)=ZAS
       ENDDO
C
       DO I=1,NEL
        CS =-NU(I)*D1X(I)
        XS =-NU1(I)*D3X(I)
        BXY1(I)=BXY1(I)+CS+XS
        BXY2(I)=BXY2(I)+CS-XS
        BXY3(I)=BXY3(I)-CS+XS
        BXY4(I)=BXY4(I)-CS-XS
        BXY5(I)=BXY5(I)-CS+XS
        BXY6(I)=BXY6(I)-CS-XS
        BXY7(I)=BXY7(I)+CS+XS
        BXY8(I)=BXY8(I)+CS-XS
        XS =-NU1(I)*D2X(I)
        BXZ1(I)=BXZ1(I)+CS+XS
        BXZ2(I)=BXZ2(I)+CS-XS
        BXZ3(I)=BXZ3(I)-CS-XS
        BXZ4(I)=BXZ4(I)-CS+XS
        BXZ5(I)=BXZ5(I)-CS-XS
        BXZ6(I)=BXZ6(I)-CS+XS
        BXZ7(I)=BXZ7(I)+CS+XS
        BXZ8(I)=BXZ8(I)+CS-XS
       ENDDO
       DO I=1,NEL
        CS =-NU(I)*D2Y(I)
        YS =-NU1(I)*D3Y(I)
        BYX1(I)=BYX1(I)+CS+YS
        BYX2(I)=BYX2(I)-CS-YS
        BYX3(I)=BYX3(I)-CS+YS
        BYX4(I)=BYX4(I)+CS-YS
        BYX5(I)=BYX5(I)-CS+YS
        BYX6(I)=BYX6(I)+CS-YS
        BYX7(I)=BYX7(I)+CS+YS
        BYX8(I)=BYX8(I)-CS-YS
        YS =-NU1(I)*D1Y(I)
        BYZ1(I)=BYZ1(I)+CS+YS
        BYZ2(I)=BYZ2(I)-CS+YS
        BYZ3(I)=BYZ3(I)-CS-YS
        BYZ4(I)=BYZ4(I)+CS-YS
        BYZ5(I)=BYZ5(I)-CS-YS
        BYZ6(I)=BYZ6(I)+CS-YS
        BYZ7(I)=BYZ7(I)+CS+YS
        BYZ8(I)=BYZ8(I)-CS+YS
       ENDDO
       DO I=1,NEL
        CS =-NU(I)*D3Z(I)
        ZS =-NU1(I)*D2Z(I)
        BZX1(I)=BZX1(I)+CS+ZS
        BZX2(I)=BZX2(I)-CS-ZS
        BZX3(I)=BZX3(I)+CS-ZS
        BZX4(I)=BZX4(I)-CS+ZS
        BZX5(I)=BZX5(I)+CS-ZS
        BZX6(I)=BZX6(I)-CS+ZS
        BZX7(I)=BZX7(I)+CS+ZS
        BZX8(I)=BZX8(I)-CS-ZS
        ZS =-NU1(I)*D1Z(I)
        BZY1(I)=BZY1(I)+CS+ZS
        BZY2(I)=BZY2(I)-CS+ZS
        BZY3(I)=BZY3(I)+CS-ZS
        BZY4(I)=BZY4(I)-CS-ZS
        BZY5(I)=BZY5(I)+CS-ZS
        BZY6(I)=BZY6(I)-CS-ZS
        BZY7(I)=BZY7(I)+CS+ZS
        BZY8(I)=BZY8(I)-CS+ZS
       ENDDO
      ENDIF
      IF (IDTS6==0) THEN
       DO I=1,NEL
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
       ENDDO
      ELSE
      DO I=1,NEL
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      END IF
C
      IF ((IMPL_S >0.AND.JHBE /= 14.AND.JHBE < 20).OR.
     .    ((ISMSTR==10.OR.ISMSTR==12).AND.
     .    (ICP==1.OR.ICP==11.OR.MTN==99.OR.MTN==1).AND.JHBE==14)) THEN
       DO I=1,NEL
        PXY1(I)=PY1(I)
        PXY2(I)=PY2(I)
        PXY3(I)=PY3(I)
        PXY4(I)=PY4(I)
        PXY5(I)=PY5(I)
        PXY6(I)=PY6(I)
        PXY7(I)=PY7(I)
        PXY8(I)=PY8(I)
C
        PYX1(I)=PX1(I)
        PYX2(I)=PX2(I)
        PYX3(I)=PX3(I)
        PYX4(I)=PX4(I)
        PYX5(I)=PX5(I)
        PYX6(I)=PX6(I)
        PYX7(I)=PX7(I)
        PYX8(I)=PX8(I)
       ENDDO
       DO I=1,NEL
        PYZ1(I)=PZ1(I)
        PYZ2(I)=PZ2(I)
        PYZ3(I)=PZ3(I)
        PYZ4(I)=PZ4(I)
        PYZ5(I)=PZ5(I)
        PYZ6(I)=PZ6(I)
        PYZ7(I)=PZ7(I)
        PYZ8(I)=PZ8(I)
C
        PZY1(I)=PY1(I)
        PZY2(I)=PY2(I)
        PZY3(I)=PY3(I)
        PZY4(I)=PY4(I)
        PZY5(I)=PY5(I)
        PZY6(I)=PY6(I)
        PZY7(I)=PY7(I)
        PZY8(I)=PY8(I)
       ENDDO
       DO I=1,NEL
        PXZ1(I)=PZ1(I)
        PXZ2(I)=PZ2(I)
        PXZ3(I)=PZ3(I)
        PXZ4(I)=PZ4(I)
        PXZ5(I)=PZ5(I)
        PXZ6(I)=PZ6(I)
        PXZ7(I)=PZ7(I)
        PXZ8(I)=PZ8(I)
C
        PZX1(I)=PX1(I)
        PZX2(I)=PX2(I)
        PZX3(I)=PX3(I)
        PZX4(I)=PX4(I)
        PZX5(I)=PX5(I)
        PZX6(I)=PX6(I)
        PZX7(I)=PX7(I)
        PZX8(I)=PX8(I)
       ENDDO
      ENDIF 
      IF ((ISMSTR==10.OR.ISMSTR==12).AND.MTN==1.AND.JHBE==14.AND.
     .    (ICP/=1.AND.ICP/=11)) THEN
       DO I=1,NEL
        BXY1(I)=ZERO
        BXZ1(I)=ZERO
        BYX1(I)=ZERO
        BYZ1(I)=ZERO
        BZX1(I)=ZERO
        BZY1(I)=ZERO
        BXY2(I)=ZERO
        BXZ2(I)=ZERO
        BYX2(I)=ZERO
        BYZ2(I)=ZERO
        BZX2(I)=ZERO
        BZY2(I)=ZERO
        BXY3(I)=ZERO
        BXZ3(I)=ZERO
        BYX3(I)=ZERO
        BYZ3(I)=ZERO
        BZX3(I)=ZERO
        BZY3(I)=ZERO
        BXY4(I)=ZERO
        BXZ4(I)=ZERO
        BYX4(I)=ZERO
        BYZ4(I)=ZERO
        BZX4(I)=ZERO
        BZY4(I)=ZERO
        BXY5(I)=ZERO
        BXZ5(I)=ZERO
        BYX5(I)=ZERO
        BYZ5(I)=ZERO
        BZX5(I)=ZERO
        BZY5(I)=ZERO
        BXY6(I)=ZERO
        BXZ6(I)=ZERO
        BYX6(I)=ZERO
        BYZ6(I)=ZERO
        BZX6(I)=ZERO
        BZY6(I)=ZERO
        BXY7(I)=ZERO
        BXZ7(I)=ZERO
        BYX7(I)=ZERO
        BYZ7(I)=ZERO
        BZX7(I)=ZERO
        BZY7(I)=ZERO
        BXY8(I)=ZERO
        BXZ8(I)=ZERO
        BYX8(I)=ZERO
        BYZ8(I)=ZERO
        BZX8(I)=ZERO
        BZY8(I)=ZERO
       ENDDO
      ENDIF 
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       DO I=1,NEL
        BXHI=THIRD*(PX1(I)-PXC1(I))
        BYHI=THIRD*(PY1(I)-PYC1(I))
        BZHI=THIRD*(PZ1(I)-PZC1(I))
        PX1(I)=PXC1(I)+TWO*BXHI
        PY1(I)=PYC1(I)+TWO*BYHI
        PZ1(I)=PZC1(I)+TWO*BZHI
        BXY1(I)=-BXHI
        BXZ1(I)=-BXHI
        BYX1(I)=-BYHI
        BYZ1(I)=-BYHI
        BZX1(I)=-BZHI
        BZY1(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX2(I)-PXC2(I))
        BYHI=THIRD*(PY2(I)-PYC2(I))
        BZHI=THIRD*(PZ2(I)-PZC2(I))
        PX2(I)=PXC2(I)+TWO*BXHI
        PY2(I)=PYC2(I)+TWO*BYHI
        PZ2(I)=PZC2(I)+TWO*BZHI
        BXY2(I)=-BXHI
        BXZ2(I)=-BXHI
        BYX2(I)=-BYHI
        BYZ2(I)=-BYHI
        BZX2(I)=-BZHI
        BZY2(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX3(I)-PXC3(I))
        BYHI=THIRD*(PY3(I)-PYC3(I))
        BZHI=THIRD*(PZ3(I)-PZC3(I))
        PX3(I)=PXC3(I)+TWO*BXHI
        PY3(I)=PYC3(I)+TWO*BYHI
        PZ3(I)=PZC3(I)+TWO*BZHI
        BXY3(I)=-BXHI
        BXZ3(I)=-BXHI
        BYX3(I)=-BYHI
        BYZ3(I)=-BYHI
        BZX3(I)=-BZHI
        BZY3(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX4(I)-PXC4(I))
        BYHI=THIRD*(PY4(I)-PYC4(I))
        BZHI=THIRD*(PZ4(I)-PZC4(I))
        PX4(I)=PXC4(I)+TWO*BXHI
        PY4(I)=PYC4(I)+TWO*BYHI
        PZ4(I)=PZC4(I)+TWO*BZHI
        BXY4(I)=-BXHI
        BXZ4(I)=-BXHI
        BYX4(I)=-BYHI
        BYZ4(I)=-BYHI
        BZX4(I)=-BZHI
        BZY4(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX5(I)+PXC3(I))
        BYHI=THIRD*(PY5(I)+PYC3(I))
        BZHI=THIRD*(PZ5(I)+PZC3(I))
        PX5(I)=-PXC3(I)+TWO*BXHI
        PY5(I)=-PYC3(I)+TWO*BYHI
        PZ5(I)=-PZC3(I)+TWO*BZHI
        BXY5(I)=-BXHI
        BXZ5(I)=-BXHI
        BYX5(I)=-BYHI
        BYZ5(I)=-BYHI
        BZX5(I)=-BZHI
        BZY5(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX6(I)+PXC4(I))
        BYHI=THIRD*(PY6(I)+PYC4(I))
        BZHI=THIRD*(PZ6(I)+PZC4(I))
        PX6(I)=-PXC4(I)+TWO*BXHI
        PY6(I)=-PYC4(I)+TWO*BYHI
        PZ6(I)=-PZC4(I)+TWO*BZHI
        BXY6(I)=-BXHI
        BXZ6(I)=-BXHI
        BYX6(I)=-BYHI
        BYZ6(I)=-BYHI
        BZX6(I)=-BZHI
        BZY6(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX7(I)+PXC1(I))
        BYHI=THIRD*(PY7(I)+PYC1(I))
        BZHI=THIRD*(PZ7(I)+PZC1(I))
        PX7(I)=-PXC1(I)+TWO*BXHI
        PY7(I)=-PYC1(I)+TWO*BYHI
        PZ7(I)=-PZC1(I)+TWO*BZHI
        BXY7(I)=-BXHI
        BXZ7(I)=-BXHI
        BYX7(I)=-BYHI
        BYZ7(I)=-BYHI
        BZX7(I)=-BZHI
        BZY7(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXHI=THIRD*(PX8(I)+PXC2(I))
        BYHI=THIRD*(PY8(I)+PYC2(I))
        BZHI=THIRD*(PZ8(I)+PZC2(I))
        PX8(I)=-PXC2(I)+TWO*BXHI
        PY8(I)=-PYC2(I)+TWO*BYHI
        PZ8(I)=-PZC2(I)+TWO*BZHI
        BXY8(I)=-BXHI
        BXZ8(I)=-BXHI
        BYX8(I)=-BYHI
        BYZ8(I)=-BYHI
        BZX8(I)=-BZHI
        BZY8(I)=-BZHI
       ENDDO
      ENDIF
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
      RETURN
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
