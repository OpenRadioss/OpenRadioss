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
Chd|  S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERI3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    HX,    HY,    HZ,   
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
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC4,JAC5,JAC6,
     .    JAC7,JAC8,JAC9,
     .    SMAX,DELTAX,NU,ICP,IDEG,VOLDP)
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
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ),DETDP
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
C------ICP= 0 : Bindeman&Belytschko assumed strain
C------ICP= 1 : fast assumed volumic strain for the laws(elastoplastic)P=f(rho,rho0)
C------ICP= 11 : EAD nominal assumed strain for the incmpressible laws(hyperelastic)P/=f(rho,rho0)
C
C Jacobian matrix
      DO I=LFT,LLT
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
       DETDP=ONE_OVER_512*(JAC1*JAC_59_68(I)+JAC2*JAC_67_49(I)+JAC3*JAC_48_57(I))
       DET(I) = DETDP
       VOLDP(I) = WI * DETDP
       VOL(I) = VOLDP(I)
      ENDDO
C
      ICOR = 0
      DO I=LFT,LLT
        OFF(I)=OFFG(I)
        IF(OFF(I).EQ.ZERO)THEN
         DET(I)=ONE
         IF (VOL(I).LE.ZERO) VOL(I)=ONE
        ELSEIF (VOL(I).LE.ZERO ) THEN
         VOL(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR.GT.0.AND.IMPL_S.GT.0) THEN
        DO I=LFT,LLT
          IF(VOL(I).LE.ZERO.AND.OFF(I).NE.ZERO)THEN
            VOL(I)= EM20
            OFF(I) =ZERO 
           IF (IMP_CHK.GT.0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV.EQ.1) THEN
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
      DO I=LFT,LLT
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
       DO I=LFT,LLT
       CAS =-NU(I)*(D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY1(I)=-XAS
       BXY7(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ1(I)=-XAS
       BXZ7(I)=XAS
       CAS =-NU(I)*(D4Y(I)+YG2(I)+XG4(I))
C--back to old one due to bench safety SID-IIs_SBLD_v3_1_2_s3_dummy1
C---now s8zderi3 is used only for s8cforc3 and gdt------       
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
      DO I=LFT,LLT
       CAS =-NU(I)*(-D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY2(I)=-XAS
       BXY8(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ2(I)=-XAS
       BXZ8(I)=XAS
       CAS =-NU(I)*(-D4Y(I)+YG2(I)+XG4(I))
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
      DO I=LFT,LLT
       CAS =-NU(I)*(D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY3(I)=-XAS
       BXY5(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ3(I)=-XAS
       BXZ5(I)=XAS
       CAS =-NU(I)*(D4Y(I)+YG2(I)+XG4(I))
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
       DO I=LFT,LLT
        CAS =-NU(I)*(-D4X(I)+XG1(I)+XG4(I))
        XAS =-NU1(I)*XG3(I)+CAS
        BXY4(I)=-XAS
        BXY6(I)=XAS
        XAS =-NU1(I)*XG2(I)+CAS
        BXZ4(I)=-XAS
        BXZ6(I)=XAS
        CAS =-NU(I)*(-D4Y(I)+YG2(I)+XG4(I))
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
       ENDDO
      ELSE
      DO I=LFT,LLT
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      END IF
C
      IF ((IMPL_S >0.AND.JHBE /= 14.AND.JHBE < 20).OR.
     .    ((ISMSTR==10.OR.ISMSTR==12).AND.
     .    (ICP==1.OR.ICP==11.OR.MTN==99.OR.MTN==1).AND.JHBE==14)) THEN
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
C---- debug Isolid=14+Ismstr=10+law1      
      IF ((ISMSTR==10.OR.ISMSTR==12).AND.MTN==1.AND.JHBE==14.AND.
     .    (ICP/=1.AND.ICP/=11)) THEN
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
Chd|====================================================================
Chd|  S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SCHKJABT3                     source/elements/solid/solide4/s4deri3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERIC3(OFF,DET,NGL,
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
     .    HX,    HY,    HZ,   
     .   JAC1,JAC2,JAC3,
     .   JAC4,JAC5,JAC6,
     .   JAC7,JAC8,JAC9,SMAX,SAV,OFFG,NEL)
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
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),SMAX(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,NNEGA,INDEX(MVSIZ)
C     REAL
C                                                                     12
      my_real
     .   DETT ,   
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35
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
C     JACOBIAN
C
      DO I=LFT,LLT
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      END DO
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
        END IF !(ISMSTR==10)
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
C	    
            JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
            JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
            JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C
           DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
	   OFFG(I) = TWO
         ENDDO
      END IF
C      
      DO I=LFT,LLT
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      END DO
C
C Jacobian matrix inverse
C
      DO I=LFT,LLT
        DETT=ONE_OVER_64/DET(I)
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
        PX3(I)= JACI12-JACI3
        PY3(I)= JACI45-JACI6
        PZ3(I)= JACI78-JACI9
      ENDDO
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
         IF(SMAX(I).LE.ZERO)THEN
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
Chd|  S8ZDERICT3                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERICT3(OFF,DET,NGL,
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
     .    HX,    HY,    HZ,   
     .   JAC1,JAC2,JAC3,
     .   JAC4,JAC5,JAC6,
     .   JAC7,JAC8,JAC9,SMAX)
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
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),SMAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT ,   
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35
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
C Jacobian matrix
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
      DO I=LFT,LLT
      JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
      JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=LFT,LLT
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      ENDDO
C
      CALL SCHKJAB3(OFF,DET,NGL)
C
C Jacobian matrix inverse
C
      DO I=LFT,LLT
        DETT=ONE_OVER_64/DET(I)
        JACI1=DETT*JAC_59_68(I)
        JACI4=DETT*JAC_67_49(I)
        JACI7=DETT*JAC_48_57(I)
        JACI2=DETT*JAC_38_29(I)
        JACI5=DETT*JAC_19_37(I)
        JACI8=DETT*JAC_27_18(I)
        JACI3=DETT*JAC_26_35(I)
        JACI6=DETT*JAC_34_16(I)
        JACI9=DETT*JAC_15_24(I)
!
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
!
        PX2(I)= JACI12-JACI3
        PY2(I)= JACI45-JACI6
        PZ2(I)= JACI78-JACI9
        PX4(I)=-JACI12-JACI3
        PY4(I)=-JACI45-JACI6
        PZ4(I)=-JACI78-JACI9
!
        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8
!
        PX1(I)=-JACI12-JACI3
        PY1(I)=-JACI45-JACI6
        PZ1(I)=-JACI78-JACI9
        PX3(I)=JACI12-JACI3
        PY3(I)=JACI45-JACI6
        PZ3(I)=JACI78-JACI9
      ENDDO
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
         IF(SMAX(I).LE.ZERO)THEN
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
Chd|  S8ZDERIPR3                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERIPR3(OFFG,VOLDP,NGL,
     .    KSI,ETA,ZETA,WI,
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    HX, HY,  HZ,   
     .    JAC1,JAC2,JAC3,
     .    JAC4,JAC5,JAC6,
     .    JACI1,JACI2,JACI3,
     .    JACI4,JACI5,JACI6,
     .    JACI7,JACI8,JACI9,NNEGA,INDEX,IPT)
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
     .   OFFG(*),KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JACI1(*),JACI2(*),JACI3(*),
     .   JACI4(*),JACI5(*),JACI6(*),
     .   JACI7(*),JACI8(*),JACI9(*)
      DOUBLE PRECISION 
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT ,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JAC7,JAC8,JAC9,VOL(MVSIZ)
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
      DO I=LFT,LLT
       JAC1(I)=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       JAC7=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       JAC8=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       JAC9=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
C
C     JACOBIAN
C
       JAC_59_68(I)=JAC5(I)*JAC9-JAC6(I)*JAC8
       JAC_67_49(I)=JAC6(I)*JAC7-JAC4(I)*JAC9
       JAC_38_29(I)=(-JAC2(I)*JAC9+JAC3(I)*JAC8)
       JAC_19_37(I)=( JAC1(I)*JAC9-JAC3(I)*JAC7)
       JAC_27_18(I)=(-JAC1(I)*JAC8+JAC2(I)*JAC7)
       JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
       JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
       JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
       JAC_48_57(I)=JAC4(I)*JAC8-JAC5(I)*JAC7
      ENDDO
C
      DO I=LFT,LLT
      DETDP=ONE_OVER_512*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DET(I) = DETDP
      VOLDP(I) = WI * DETDP
      VOL(I) = VOLDP(I)
      ENDDO
C
      ICOR = 0
      DO I=LFT,LLT
        IF(OFFG(I).EQ.ZERO)THEN
         DET(I)=ONE
         IF (VOL(I).LE.ZERO) VOL(I)=ONE
         IF (VOLDP(I).LE.ZERO) VOLDP(I)=ONE
        ELSEIF (VOL(I).LE.ZERO ) THEN
          ICOR=1
        ENDIF
      ENDDO
      IF (ICOR>0.AND.INCONV==1) THEN
       DO I=LFT,LLT
       IF (OFFG(I) /= TWO.AND.OFFG(I) /= ZERO ) THEN
         NNEGA=NNEGA+1
        INDEX(NNEGA)=I
        OFFG(I) = TWO
       END IF
       ENDDO
      END IF
C
      IF (ICOR.GT.0.AND.IMPL_S.GT.0) THEN
        DO I=LFT,LLT
          IF(VOL(I).LE.ZERO)THEN
            VOL(I)= EM20
            VOLDP(I) = EM20
            DET(I)= EM20
           IF (IMP_CHK.GT.0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
      DO I=LFT,LLT
       DETT=ONE_OVER_512/DET(I)
       JACI1(I)=DETT*JAC_59_68(I)
       JACI4(I)=DETT*JAC_67_49(I)
       JACI7(I)=DETT*JAC_48_57(I)
       JACI2(I)=DETT*JAC_38_29(I)
       JACI5(I)=DETT*JAC_19_37(I)
       JACI8(I)=DETT*JAC_27_18(I)
       JACI3(I)=DETT*JAC_26_35(I)
       JACI6(I)=DETT*JAC_34_16(I)
       JACI9(I)=DETT*JAC_15_24(I)
      ENDDO
C
      RETURN
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
Chd|====================================================================
Chd|  S8ZDERIM3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERIM3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
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
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    HX,    HY,    HZ,   
     .    JAC1,JAC2,JAC3,
     .    JAC4,JAC5,JAC6,
     .    JACI1,  JACI2,  JACI3,
     .    JACI4,  JACI5,  JACI6,
     .    JACI7,  JACI8,  JACI9,
     .    SMAX,DELTAX,NU,ICP,IDEG,VOLDP)
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
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
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
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ),
     .   JACI4(MVSIZ), JACI5(MVSIZ), JACI6(MVSIZ),
     .   JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ),
     .   SMAX(*),DELTAX(*),NU(*)
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   NU1(MVSIZ),  
     .   D1X(MVSIZ) , D2X(MVSIZ) , D3X(MVSIZ) , D4X(MVSIZ) ,
     .   D1Y(MVSIZ) , D2Y(MVSIZ) , D3Y(MVSIZ) , D4Y(MVSIZ) ,
     .   D1Z(MVSIZ) , D2Z(MVSIZ) , D3Z(MVSIZ) , D4Z(MVSIZ) ,
     .   XG1(MVSIZ), XG2(MVSIZ), XG3(MVSIZ), XG4(MVSIZ),  
     .   YG1(MVSIZ), YG2(MVSIZ), YG3(MVSIZ), YG4(MVSIZ),   
     .   ZG1(MVSIZ), ZG2(MVSIZ), ZG3(MVSIZ), ZG4(MVSIZ),   
     .   F1,F2,F3,XS,XAS,YS,YAS,ZS,ZAS,CS,CAS ,BXHI,BYHI,BZHI
C-----------------------------------------------
C------repeating but no memory
      DO I=LFT,LLT
       JAC1(I)=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
      ENDDO
C------ICP= 0 : Bindeman&Belytschko assumed strain
C------ICP= 1 : fast assumed volumic strain for the laws(elastoplastic)P=f(rho,rho0)
C------ICP= 11 : EAD nominal assumed strain for the incmpressible laws(hyperelastic)P/=f(rho,rho0)
C
      DO I=LFT,LLT
       NU1(I) = NU(I)/(ONE - NU(I))
       OFF(I) = OFFG(I)
      ENDDO
       F1 = ETA*ZETA
       F2 = KSI*ZETA
       F3 = KSI*ETA
      DO I=LFT,LLT
       D1X(I)=JACI3(I)*ETA +JACI2(I)*ZETA
       D2X(I)=JACI1(I)*ZETA +JACI3(I)*KSI
       D3X(I)=JACI2(I)*KSI +JACI1(I)*ETA
       D4X(I)=JACI1(I)*F1+JACI2(I)*F2+JACI3(I)*F3
C
       D1Y(I)=JACI6(I)*ETA +JACI5(I)*ZETA
       D2Y(I)=JACI4(I)*ZETA +JACI6(I)*KSI
       D3Y(I)=JACI5(I)*KSI +JACI4(I)*ETA
       D4Y(I)=JACI4(I)*F1+JACI5(I)*F2+JACI6(I)*F3
C
       D1Z(I)=JACI9(I)*ETA +JACI8(I)*ZETA
       D2Z(I)=JACI7(I)*ZETA +JACI9(I)*KSI
       D3Z(I)=JACI8(I)*KSI +JACI7(I)*ETA
       D4Z(I)=JACI7(I)*F1+JACI8(I)*F2+JACI9(I)*F3
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
       DO I=LFT,LLT
       CAS =-NU(I)*(D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY1(I)=-XAS
       BXY7(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ1(I)=-XAS
       BXZ7(I)=XAS
       CAS =-NU(I)*(D4Y(I)+YG2(I)+YG4(I))
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
      DO I=LFT,LLT
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
      DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
      DO I=LFT,LLT
        OFF(I) = OFFG(I)
        VOL(I) = VOLDP(I)
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
C
      IF ((IMPL_S >0.AND.JHBE /= 14.AND.JHBE < 20).OR.
     .    ((ISMSTR==10.OR.ISMSTR==12).AND.
     .    (ICP==1.OR.ICP==11.OR.MTN==99.OR.MTN==1).AND.JHBE==14)) THEN
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
C      
      RETURN
      END
Chd|====================================================================
Chd|  S8ZDERICM3                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERICM3(DET,NGL,
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
     .    HX,    HY,    HZ,   
     .   JAC1,JAC2,JAC3,
     .   JAC4,JAC5,JAC6,
     .   JAC7,JAC8,JAC9,SMAX,SAV,OFFG,NNEGA,INDEX,NEL)
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
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "com01_c.inc"
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
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),SMAX(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT ,   
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35
C-----------------------------------------------
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
        ENDDO
C
#include "vectorize.inc"
        DO J=1,NNEGA
          I = INDEX(J)
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
C	    
            JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
            JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
            JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C
          DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
        ENDDO
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
          JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
          JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
          JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
          JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
          JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
          JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
          DETT=ONE_OVER_64/DET(I)
          JACI1=DETT*JAC_59_68(I)
          JACI4=DETT*JAC_67_49(I)
          JACI7=DETT*JAC_48_57(I)
          JACI2=DETT*JAC_38_29(I)
          JACI5=DETT*JAC_19_37(I)
          JACI8=DETT*JAC_27_18(I)
          JACI3=DETT*JAC_26_35(I)
          JACI6=DETT*JAC_34_16(I)
          JACI9=DETT*JAC_15_24(I)
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
         PX1H1(I)=PX1(I)*HX(I,1)+ PY1(I)*HY(I,1)+PZ1(I)*HZ(I,1)
         PX2H1(I)=PX2(I)*HX(I,1)+ PY2(I)*HY(I,1)+PZ2(I)*HZ(I,1)
         PX3H1(I)=PX3(I)*HX(I,1)+ PY3(I)*HY(I,1)+PZ3(I)*HZ(I,1)
         PX4H1(I)=PX4(I)*HX(I,1)+ PY4(I)*HY(I,1)+PZ4(I)*HZ(I,1)
C	 
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H2(I)=PX1(I)*HX(I,2)+ PY1(I)*HY(I,2)+PZ1(I)*HZ(I,2)
         PX2H2(I)=PX2(I)*HX(I,2)+ PY2(I)*HY(I,2)+PZ2(I)*HZ(I,2)
         PX3H2(I)=PX3(I)*HX(I,2)+ PY3(I)*HY(I,2)+PZ3(I)*HZ(I,2)
         PX4H2(I)=PX4(I)*HX(I,2)+ PY4(I)*HY(I,2)+PZ4(I)*HZ(I,2)
C	 
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX(I,3)+ PY1(I)*HY(I,3)+PZ1(I)*HZ(I,3)
         PX2H3(I)=PX2(I)*HX(I,3)+ PY2(I)*HY(I,3)+PZ2(I)*HZ(I,3)
         PX3H3(I)=PX3(I)*HX(I,3)+ PY3(I)*HY(I,3)+PZ3(I)*HZ(I,3)
         PX4H3(I)=PX4(I)*HX(I,3)+ PY4(I)*HY(I,3)+PZ4(I)*HZ(I,3)
C	 
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H4(I)=PX1(I)*HX(I,4)+ PY1(I)*HY(I,4)+PZ1(I)*HZ(I,4)
         PX2H4(I)=PX2(I)*HX(I,4)+ PY2(I)*HY(I,4)+PZ2(I)*HZ(I,4)
         PX3H4(I)=PX3(I)*HX(I,4)+ PY3(I)*HY(I,4)+PZ3(I)*HZ(I,4)
         PX4H4(I)=PX4(I)*HX(I,4)+ PY4(I)*HY(I,4)+PZ4(I)*HZ(I,4)
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
      RETURN
C
 3000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : 3D-ELEMENT NB:',I10/,
     +        ' ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/) 
      END
Chd|====================================================================
Chd|  S8ZDERIMS3                    source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERIMS3(VOLDP,
     .    KSI,ETA,ZETA,WI,
     .    HX,    HY,    HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC1,JAC2,JAC3,
     .    JAC4,JAC5,JAC6,
     .    JAC7,JAC8,JAC9,
     .    JACI1,JACI2,JACI3,
     .    JACI4,JACI5,JACI6,
     .    JACI7,JACI8,JACI9,NNEGA,INDEX)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNEGA,INDEX(MVSIZ)
C     REAL
      my_real
     .   KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),
     .   JACI1(*),JACI2(*),JACI3(*),
     .   JACI4(*),JACI5(*),JACI6(*),
     .   JACI7(*),JACI8(*),JACI9(*)
      DOUBLE PRECISION 
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   DET ,DETT ,  
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   JAC_38_29, JAC_19_37, JAC_27_18,
     .   JAC_26_35, JAC_34_16, JAC_15_24
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
#include "vectorize.inc"
      DO J=1,NNEGA
       I = INDEX(J)
       JAC1(I)=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       JAC7(I)=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       JAC8(I)=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       JAC9(I)=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
C       
C     JACOBIAN
C
       JAC_59_68=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
       JAC_67_49=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
       JAC_38_29=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
       JAC_19_37=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
       JAC_27_18=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
       JAC_26_35=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
       JAC_34_16=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
       JAC_15_24=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
       JAC_48_57=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
       DETDP=ONE_OVER_512*(JAC1(I)*JAC_59_68+JAC2(I)*JAC_67_49+JAC3(I)*JAC_48_57)
       DET = DETDP
       VOLDP(I)= WI*DETDP
       DETT=ONE_OVER_512/DET
       JACI1(I)=DETT*JAC_59_68
       JACI4(I)=DETT*JAC_67_49
       JACI7(I)=DETT*JAC_48_57
       JACI2(I)=DETT*JAC_38_29
       JACI5(I)=DETT*JAC_19_37
       JACI8(I)=DETT*JAC_27_18
       JACI3(I)=DETT*JAC_26_35
       JACI6(I)=DETT*JAC_34_16
       JACI9(I)=DETT*JAC_15_24
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ZDERIT3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERIT3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    HX,    HY,    HZ,   
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
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC4,JAC5,JAC6,
     .    JAC7,JAC8,JAC9,
     .    SMAX,DELTAX,NU,ICP,IDEG,JAC_I)
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
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),
     .   SMAX(*),DELTAX(*),NU(*),JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT , NU1(MVSIZ),  
     .   JAC1,JAC2,JAC3,
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ),
     .   JACI4(MVSIZ), JACI5(MVSIZ), JACI6(MVSIZ),
     .   JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ),
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   JAC_38_29, JAC_19_37, JAC_27_18,
     .   JAC_26_35, JAC_34_16, JAC_15_24,
     .   JACI12(MVSIZ), JACI45(MVSIZ), JACI78(MVSIZ),
     .   D1X(MVSIZ) , D2X(MVSIZ) , D3X(MVSIZ) , D4X(MVSIZ) ,
     .   D1Y(MVSIZ) , D2Y(MVSIZ) , D3Y(MVSIZ) , D4Y(MVSIZ) ,
     .   D1Z(MVSIZ) , D2Z(MVSIZ) , D3Z(MVSIZ) , D4Z(MVSIZ) ,
     .   XG1(MVSIZ), XG2(MVSIZ), XG3(MVSIZ), XG4(MVSIZ),  
     .   YG1(MVSIZ), YG2(MVSIZ), YG3(MVSIZ), YG4(MVSIZ),   
     .   ZG1(MVSIZ), ZG2(MVSIZ), ZG3(MVSIZ), ZG4(MVSIZ),   
     .   F1,F2,F3,XS,XAS,YS,YAS,ZS,ZAS,CS,CAS ,BXHI,BYHI,BZHI,x,y,z
C-----------------------------------------------
C------ICP= 0 : Bindeman&Belytschko assumed strain
C------ICP= 1 : fast assumed volumic strain for the laws(elastoplastic)P=f(rho,rho0)
C------ICP= 11 : EAD nominal assumed strain for the incmpressible laws(hyperelastic)P/=f(rho,rho0)
      IF (ISMSTR==11) THEN
      DO I=LFT,LLT
       JAC1=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       JAC7(I)=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       JAC8(I)=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       JAC9(I)=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
C
C     JACOBIAN
C
       JAC_59_68=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
       JAC_67_49=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
       JAC_38_29=(-JAC2*JAC9(I)+JAC3*JAC8(I))
       JAC_19_37=( JAC1*JAC9(I)-JAC3*JAC7(I))
       JAC_27_18=(-JAC1*JAC8(I)+JAC2*JAC7(I))
       JAC_26_35=( JAC2*JAC6(I)-JAC3*JAC5(I))
       JAC_34_16=(-JAC1*JAC6(I)+JAC3*JAC4(I))
       JAC_15_24=( JAC1*JAC5(I)-JAC2*JAC4(I))
       JAC_48_57=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C
       DET(I)=ONE_OVER_512*(JAC1*JAC_59_68+JAC2*JAC_67_49+JAC3*JAC_48_57)
       VOL(I)= WI*DET(I)
       DETT=ONE_OVER_512/DET(I)
       JACI1(I)=DETT*JAC_59_68
       JACI4(I)=DETT*JAC_67_49
       JACI7(I)=DETT*JAC_48_57
       JACI2(I)=DETT*JAC_38_29
       JACI5(I)=DETT*JAC_19_37
       JACI8(I)=DETT*JAC_27_18
       JACI3(I)=DETT*JAC_26_35
       JACI6(I)=DETT*JAC_34_16
       JACI9(I)=DETT*JAC_15_24
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
C------ISMSTR = 10      
      ELSE
      DO I=LFT,LLT
       JACI1(I)=JAC_I(1,I) 
       JACI4(I)=JAC_I(4,I) 
       JACI7(I)=JAC_I(7,I) 
       JACI2(I)=JAC_I(2,I) 
       JACI5(I)=JAC_I(5,I) 
       JACI8(I)=JAC_I(8,I) 
       JACI3(I)=JAC_I(3,I) 
       JACI6(I)=JAC_I(6,I) 
       JACI9(I)=JAC_I(9,I) 
       VOL(I) =JAC_I(10,I)
      ENDDO    
      END IF !(ISMSTR==11) THEN
C
      DO I=LFT,LLT
       NU1(I) = NU(I)/(ONE - NU(I))
      ENDDO
       F1 = ETA*ZETA
       F2 = KSI*ZETA
       F3 = KSI*ETA
      DO I=LFT,LLT
       D1X(I)=JACI3(I)*ETA +JACI2(I)*ZETA
       D2X(I)=JACI1(I)*ZETA +JACI3(I)*KSI
       D3X(I)=JACI2(I)*KSI +JACI1(I)*ETA
       D4X(I)=JACI1(I)*F1+JACI2(I)*F2+JACI3(I)*F3
C
       D1Y(I)=JACI6(I)*ETA +JACI5(I)*ZETA
       D2Y(I)=JACI4(I)*ZETA +JACI6(I)*KSI
       D3Y(I)=JACI5(I)*KSI +JACI4(I)*ETA
       D4Y(I)=JACI4(I)*F1+JACI5(I)*F2+JACI6(I)*F3
C
       D1Z(I)=JACI9(I)*ETA +JACI8(I)*ZETA
       D2Z(I)=JACI7(I)*ZETA +JACI9(I)*KSI
       D3Z(I)=JACI8(I)*KSI +JACI7(I)*ETA
       D4Z(I)=JACI7(I)*F1+JACI8(I)*F2+JACI9(I)*F3
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
       DO I=LFT,LLT
       CAS =-NU(I)*(D4X(I)+XG1(I)+XG4(I))
       XAS =-NU1(I)*XG3(I)+CAS
       BXY1(I)=-XAS
       BXY7(I)=XAS
       XAS =-NU1(I)*XG2(I)+CAS
       BXZ1(I)=-XAS
       BXZ7(I)=XAS
       CAS =-NU(I)*(D4Y(I)+YG2(I)+YG4(I))
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
      DO I=LFT,LLT
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
      DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
C
      IF ((IMPL_S >0.AND.JHBE /= 14.AND.JHBE < 20).OR.
     .    ((ISMSTR==10.OR.ISMSTR==12).AND.
     .    (ICP==1.OR.ICP==11.OR.MTN==99.OR.MTN==1).AND.JHBE==14)) THEN
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
      END
Chd|====================================================================
Chd|  S8ZDERICTO3                   source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERICTO3(OFF,DET,NGL,
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
     .   HX,    HY,    HZ,   
     .   JAC1,JAC2,JAC3,
     .   JAC4,JAC5,JAC6,
     .   JAC7,JAC8,JAC9,SMAX,JAC_I)
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
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),SMAX(*),JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT(MVSIZ) ,   
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   JAC_38_29, JAC_19_37, JAC_27_18,
     .   JAC_26_35, JAC_34_16, JAC_15_24,
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35
C-----------------------------------------------
C  
      DO I=LFT,LLT
        JACI1=JAC_I(1,I)
        JACI4=JAC_I(4,I)
        JACI7=JAC_I(7,I)
        JACI2=JAC_I(2,I)
        JACI5=JAC_I(5,I)
        JACI8=JAC_I(8,I)
        JACI3=JAC_I(3,I)
        JACI6=JAC_I(6,I)
        JACI9=JAC_I(9,I)
        DET(I) =JAC_I(10,I)
C
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
!
        PX2(I)= JACI12-JACI3
        PY2(I)= JACI45-JACI6
        PZ2(I)= JACI78-JACI9
        PX4(I)=-JACI12-JACI3
        PY4(I)=-JACI45-JACI6
        PZ4(I)=-JACI78-JACI9
!
        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8
!
        PX1(I)=-JACI12-JACI3
        PY1(I)=-JACI45-JACI6
        PZ1(I)=-JACI78-JACI9
        PX3(I)=JACI12-JACI3
        PY3(I)=JACI45-JACI6
        PZ3(I)=JACI78-JACI9
      ENDDO
C       
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
C       
       IF (ISMSTR/=11) RETURN
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
        JAC4(I)=X17+X28-X35-X46
        JAC5(I)=Y17+Y28-Y35-Y46
        JAC6(I)=Z17+Z28-Z35-Z46
        X_17_46=X17+X46
        X_28_35=X28+X35
        Y_17_46=Y17+Y46
        Y_28_35=Y28+Y35
        Z_17_46=Z17+Z46
        Z_28_35=Z28+Z35
!
        JAC7(I)=X_17_46+X_28_35
        JAC8(I)=Y_17_46+Y_28_35
        JAC9(I)=Z_17_46+Z_28_35
        JAC1(I)=X_17_46-X_28_35
        JAC2(I)=Y_17_46-Y_28_35
        JAC3(I)=Z_17_46-Z_28_35
C
C     JACOBIAN
C
        JAC_59_68=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_38_29=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JAC_19_37=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JAC_27_18=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JAC_26_35=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JAC_34_16=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JAC_15_24=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
        JAC_48_57=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
C----surface max mediane-- *16
        SMAX(I)= JAC_59_68*JAC_59_68+JAC_67_49*JAC_67_49
     .            +JAC_48_57*JAC_48_57
        SMAX(I)= MAX(SMAX(I),JAC_38_29*JAC_38_29+JAC_19_37*JAC_19_37
     .            +JAC_27_18*JAC_27_18)
        SMAX(I)= MAX(SMAX(I),JAC_26_35*JAC_26_35+JAC_34_16*JAC_34_16
     .            +JAC_15_24*JAC_15_24)
       ENDDO
       DO I=LFT,LLT
         IF(SMAX(I).LE.ZERO)THEN
           CALL ANCMSG(MSGID=173,ANMODE=ANINFO,
     .                 I1=NGL(I))
           CALL ARRET(2)
         ENDIF
         SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
      END 
Chd|====================================================================
Chd|  S8ZDERII3                     source/elements/solid/solide8z/s8zderi3.F
Chd|-- called by -----------
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZDERII3(OFFG,OFF,VOL,NGL,
     .    KSI,ETA,ZETA,WI,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    HX,    HY,    HZ,   
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
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC4,JAC5,JAC6,
     .    JAC7,JAC8,JAC9,
     .    SMAX,DELTAX,NU,ICP,IDEG)
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
      DO I=LFT,LLT
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
      DO I=LFT,LLT
        OFF(I)=OFFG(I)
        IF(OFF(I).EQ.ZERO)THEN
         DET(I)=ONE
         IF (VOL(I).LE.ZERO) VOL(I)=ONE
        ELSEIF (VOL(I).LE.ZERO ) THEN
         VOL(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR.GT.0.AND.IMPL_S.GT.0) THEN
        DO I=LFT,LLT
          IF(VOL(I).LE.ZERO.AND.OFF(I).NE.ZERO)THEN
            VOL(I)= EM20
            OFF(I) =ZERO 
           IF (IMP_CHK.GT.0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV.EQ.1) THEN
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
      DO I=LFT,LLT
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
       DO I=LFT,LLT
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
      DO I=LFT,LLT
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
      DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
       ENDDO
      ELSE
      DO I=LFT,LLT
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      END IF
C
      IF ((IMPL_S >0.AND.JHBE /= 14.AND.JHBE < 20).OR.
     .    ((ISMSTR==10.OR.ISMSTR==12).AND.
     .    (ICP==1.OR.ICP==11.OR.MTN==99.OR.MTN==1).AND.JHBE==14)) THEN
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
       DO I=LFT,LLT
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
