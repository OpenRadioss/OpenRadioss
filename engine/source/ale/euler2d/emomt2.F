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
Chd|  EMOMT2                        source/ale/euler2d/emomt2.F   
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        UPWIND                        source/elements/solid/solide/upwind.F
Chd|====================================================================
      SUBROUTINE EMOMT2(PM,RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  T11, T12, T13, T14, T21, T22, T23 , T24,
     .                  PY1, PY2, PZ1, PZ2, AIRE,
     .                  DYY, DZZ, DYZ, DZY,VDY,VDZ,
     .                  DELTAX,VIS,VD2,
     .                  NC1,NC2,NC3,NC4,MAT,
     .                  QMV,BUFMAT)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scrupw_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real PM(NPROPM,NUMMAT), RHO(*),
     .        Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .        VY1(*), VY2(*), VY3(*), VY4(*), VZ1(*), VZ2(*), VZ3(*), VZ4(*),
     .        T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*), T24(*),
     .        PY1(*), PY2(*), PZ1(*), PZ2(*), AIRE(*),
     .        DYY(*), DZZ(*), DYZ(*), DZY(*),VDY(*),VDZ(*),
     .        DELTAX(*),VIS(*),VD2(*),QMV(8,*),BUFMAT(*)
      INTEGER MAT(*), NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   Y12(MVSIZ),Z12(MVSIZ),Y23(MVSIZ),Z23(MVSIZ),
     .   Y34(MVSIZ),Z34(MVSIZ),Y41(MVSIZ),Z41(MVSIZ),
     .   Y24(MVSIZ),Z24(MVSIZ),Y13(MVSIZ),Z13(MVSIZ),
     .   DYYP(MVSIZ), DZZP(MVSIZ), DYZP(MVSIZ), DZYP(MVSIZ),
     .   F1(MVSIZ), F2(MVSIZ),A1(MVSIZ),A2(MVSIZ),A3(MVSIZ),GAM(MVSIZ),
     .   F1P(MVSIZ),F2P(MVSIZ),
     .   S(3,MVSIZ),T(3,MVSIZ),
     .   TR(MVSIZ),FAC,DT1H,
     .   PD,PS,PP,PC
      INTEGER I
C-----------------------------------------------
      my_real AAA,DM,DMY,DMZ
      INTEGER IADBUF,IFLG
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
C-----------------------------------------------
C     Reynolds transportation theorem
C-----------------------------------------------
C   DMi = 0.5*DT1*QMV(i,I)  incoming mass from face i
      IF(MTN == 51 .AND. UPWM /= 3)THEN
        IADBUF = NINT(PM(27,MAT(1)))
        IFLG = NINT(BUFMAT(31+IADBUF-1))
C
        IF(IFLG > 1)RETURN
C
        IF(UPWM == 0.)THEN
         DO I=LFT,LLT
          GAM(I)= PM(15,MAT(I))
         ENDDO
        ELSE
         DO I=LFT,LLT
          GAM(I)= CUPWM
         ENDDO
        ENDIF
C
        DO I=LFT,LLT
          AAA = QMV(1,I)+QMV(2,I)+QMV(3,I)+QMV(4,I)
          AAA = FOURTH * AAA
          QMV(1,I) = FOURTH * (QMV(1,I) - AAA)
          QMV(2,I) = FOURTH * (QMV(2,I) - AAA)
          QMV(3,I) = FOURTH * (QMV(3,I) - AAA)
          QMV(4,I) = FOURTH * (QMV(4,I) - AAA)
          DMY = ZERO
          DMZ = ZERO
          DM = QMV(4,I)+QMV(1,I)
          DMY = DMY + VY1(I)*DM
          DMZ = DMZ + VZ1(I)*DM
          DM = QMV(1,I)+QMV(2,I)
          DMY = DMY + VY2(I)*DM
          DMZ = DMZ + VZ2(I)*DM
          DM = QMV(2,I)+QMV(3,I)
          DMY = DMY + VY3(I)*DM
          DMZ = DMZ + VZ3(I)*DM
          DM = QMV(3,I)+QMV(4,I)
          DMY = DMY + VY4(I)*DM
          DMZ = DMZ + VZ4(I)*DM
C
          DMY = -FOURTH * DMY
          DMZ = -FOURTH * DMZ
C
          A1(I) = PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
          A2(I) = PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
          A1(I) = SIGN(GAM(I),A1(I))
          A2(I) = SIGN(GAM(I),A2(I))
C
          T11(I) =  T11(I)+(ONE+A1(I))*DMY
          T12(I) =  T12(I)+(ONE+A2(I))*DMY
          T13(I) =  T13(I)+(ONE-A1(I))*DMY
          T14(I) =  T14(I)+(ONE-A2(I))*DMY
C
          T21(I) =  T21(I)+(ONE+A1(I))*DMZ
          T22(I) =  T22(I)+(ONE+A2(I))*DMZ
          T23(I) =  T23(I)+(ONE-A1(I))*DMZ
          T24(I) =  T24(I)+(ONE-A2(I))*DMZ
        ENDDO
        RETURN
      ENDIF
      IF(UPWM == 0.AND.MTN == 11)RETURN
      IF(UPWM > 1)THEN
       PC=FOURTH
       PD=ONE_OVER_16
       PS=ONE_OVER_16
       PP=ONE_OVER_16
       DT1H=HALF*DT1*CUPWM
       DO I=LFT,LLT
        Y13(I)=Y3(I)-Y1(I)
        Z13(I)=Z3(I)-Z1(I)
        Y24(I)=Y4(I)-Y2(I)
        Z24(I)=Z4(I)-Z2(I)
        Y12(I)=Y2(I)-Y1(I)
        Z12(I)=Z2(I)-Z1(I)
        Y23(I)=Y3(I)-Y2(I)
        Z23(I)=Z3(I)-Z2(I)
        Y34(I)=Y4(I)-Y3(I)
        Z34(I)=Z4(I)-Z3(I)
        Y41(I)=Y1(I)-Y4(I)
        Z41(I)=Z1(I)-Z4(I)
       ENDDO
        DO I=LFT,LLT
         S(2,I)=HALF*(Y12(I)-Y34(I))
         S(3,I)=HALF*(Z12(I)-Z34(I))
         T(2,I)=HALF*(-Y41(I)+Y23(I))
         T(3,I)=HALF*(-Z41(I)+Z23(I))	
       ENDDO
      ENDIF
C
C FORCE AT CENTROID
C
      IF(UPWM < 2.OR.MTN == 11)THEN
       IF(MTN == 11.AND.UPWM > 1)THEN
        CALL UPWIND(RHO,VIS,VDY,VDY,VDZ,S,S,T,DELTAX,GAM)
        DO I=LFT,LLT
         FAC=4.*GAM(I)/AIRE(I)
         A1(I) = FAC*(PY1(I)*VDY(I)+PZ1(I)*VDZ(I))
         A2(I) = FAC*(PY2(I)*VDY(I)+PZ2(I)*VDZ(I))
        ENDDO
       ELSE
        DO I=LFT,LLT
         GAM(I)= PM(15,MAT(I))
         A1(I) = PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
         A2(I) = PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
         A1(I) = SIGN(GAM(I),A1(I))
         A2(I) = SIGN(GAM(I),A2(I))
        ENDDO
       ENDIF
       DO I=LFT,LLT
        FAC = FOURTH*RHO(I)*AIRE(I)
        F1(I) = (VDY(I)*DYY(I)+VDZ(I)*DYZ(I))*FAC
        F2(I) = (VDY(I)*DZY(I)+VDZ(I)*DZZ(I))*FAC
       ENDDO
       DO I=LFT,LLT
        T11(I) =  T11(I)+(ONE+A1(I))*F1(I)
        T12(I) =  T12(I)+(ONE+A2(I))*F1(I)
        T13(I) =  T13(I)+(ONE-A1(I))*F1(I)
        T14(I) =  T14(I)+(ONE-A2(I))*F1(I)
        T21(I) =  T21(I)+(ONE+A1(I))*F2(I)
        T22(I) =  T22(I)+(ONE+A2(I))*F2(I)
        T23(I) =  T23(I)+(ONE-A1(I))*F2(I)
        T24(I) =  T24(I)+(ONE-A2(I))*F2(I)
       ENDDO
C
      ELSE
C
C TRANSPORTATION FORCE WHICH MINIMIZE HOURGLASS ISSUES
C EVALUATE <PHI,UJ*DUI/DXJ> DIRECTLY TO NODES
C DERI PHI1   
C P1Y1=-Z24/TR1 P1Z1= Y24/TR1 P1Y2=-Z23/TR2 P1Z2= Y23/TR2
C                             P1Y4=-Z34/TR4 P1Z4= Y34/TR4
C DERI PHI2   
C P2Y1=-Z41/TR1 P2Z1= Y41/TR1 P2Y2= Z13/TR2 P2Z2=-Y13/TR2 
C P2Y3=-Z34/TR3 P2Z3= Y34/TR3
C DERI PHI3
C                             P3Y2=-Z12/TR2 P3Z2= Y12/TR2
C P3Y3= Z24/TR3 P3Z3=-Y24/TR3 P3Y4=-Z41/TR4 P3Z4= Y41/TR4
C DERIVATE DE PHI4   
C P4Y1=-Z12/TR1 P4Z1= Y12/TR1 
C P4Y3=-Z23/TR3 P4Z3= Y23/TR3 P4Y4=-Z13/TR4 P4Z4= Y13/TR4
C
C DERIV POINT 1
C P1Y1=-Z24/TR1 P1Z1= Y24/TR1 P2Y1=-Z41/TR1 P2Z1= Y41/TR1
C                             P4Y1=-Z12/TR1 P4Z1= Y12/TR1
C DERIV POINT 2 
C P1Y2=-Z23/TR2 P1Z2= Y23/TR2 P2Y2= Z13/TR2 P2Z2=-Y13/TR2
C P3Y2=-Z12/TR2 P3Z2= Y12/TR2
C DERIV POINT 3 
C                             P2Y3=-Z34/TR3 P2Z3= Y34/TR3
C P3Y3= Z24/TR3 P3Z3=-Y24/TR3 P4Y3=-Z23/TR3 P4Z3= Y23/TR3
C DERIV POINT 4 
C P1Y4=-Z34/TR4 P1Z4= Y34/TR4
C P3Y4=-Z41/TR4 P3Z4= Y41/TR4 P4Y4=-Z13/TR4 P4Z4= Y13/TR4
C
C NODE 1      
       CALL UPWIND(RHO,VIS,VY1,VY1,VZ1,S,S,T,DELTAX,GAM)
       DO I=LFT,LLT
        TR(I)=(Y41(I)*Z12(I)-Y12(I)*Z41(I))
        DYYP(I)=(-Z24(I)*VY1(I)-Z41(I)*VY2(I)-Z12(I)*VY4(I))
        DYZP(I)=( Y24(I)*VY1(I)+Y41(I)*VY2(I)+Y12(I)*VY4(I))
        DZZP(I)=( Y24(I)*VZ1(I)+Y41(I)*VZ2(I)+Y12(I)*VZ4(I))
        DZYP(I)=(-Z24(I)*VZ1(I)-Z41(I)*VZ2(I)-Z12(I)*VZ4(I))
        F1P(I)=RHO(I)*(VY1(I)*DYYP(I)+VZ1(I)*DYZP(I))
        F2P(I)=RHO(I)*(VY1(I)*DZYP(I)+VZ1(I)*DZZP(I))
        FAC=PC*GAM(I)/TR(I)
        A1(I)=FAC*(-Z24(I)*VY1(I)+Y24(I)*VZ1(I))
        A2(I)=FAC*(-Z41(I)*VY1(I)+Y41(I)*VZ1(I))
        A3(I)=FAC*(-Z12(I)*VY1(I)+Y12(I)*VZ1(I))
C
        T11(I) =  T11(I)+(PP+A1(I))*F1P(I)
        T12(I) =  T12(I)+(PS+A2(I))*F1P(I)
        T13(I) =  T13(I)+PD*F1P(I)
        T14(I) =  T14(I)+(PS+A3(I))*F1P(I)
        T21(I) =  T21(I)+(PP+A1(I))*F2P(I)
        T22(I) =  T22(I)+(PS+A2(I))*F2P(I)
        T23(I) =  T23(I)+PD*F2P(I)
        T24(I) =  T24(I)+(PS+A3(I))*F2P(I)
       ENDDO
C
       IF(UPWM == 2)THEN
        DO I=LFT,LLT
         FAC=-DT1H/TR(I)
         F1(I)=FAC*(-F1P(I)*DZZP(I)+F2P(I)*DYZP(I))
         F2(I)=FAC*( F1P(I)*DZYP(I)-F2P(I)*DYYP(I))
         T11(I) =  T11(I)+PP*F1(I)
         T12(I) =  T12(I)+PS*F1(I)
         T13(I) =  T13(I)+PD*F1(I)
         T14(I) =  T14(I)+PS*F1(I)
         T21(I) =  T21(I)+PP*F2(I)
         T22(I) =  T22(I)+PS*F2(I)
         T23(I) =  T23(I)+PD*F2(I)
         T24(I) =  T24(I)+PS*F2(I)
        ENDDO
       ENDIF
C NODE 2      
       CALL UPWIND(RHO,VIS,VY2,VY2,VZ2,S,S,T,DELTAX,GAM)
       DO I=LFT,LLT
        TR(I)=(Y12(I)*Z23(I)-Y23(I)*Z12(I))
        DYYP(I)=(-Z23(I)*VY1(I)+Z13(I)*VY2(I)-Z12(I)*VY3(I))
        DYZP(I)=( Y23(I)*VY1(I)-Y13(I)*VY2(I)+Y12(I)*VY3(I))
        DZZP(I)=( Y23(I)*VZ1(I)-Y13(I)*VZ2(I)+Y12(I)*VZ3(I))
        DZYP(I)=(-Z23(I)*VZ1(I)+Z13(I)*VZ2(I)-Z12(I)*VZ3(I))
        F1P(I)=RHO(I)*(VY2(I)*DYYP(I)+VZ2(I)*DYZP(I))
        F2P(I)=RHO(I)*(VY2(I)*DZYP(I)+VZ2(I)*DZZP(I))
        FAC=PC*GAM(I)/TR(I)
        A1(I)=FAC*(-Z23(I)*VY2(I)+Y23(I)*VZ2(I))
        A2(I)=FAC*( Z13(I)*VY2(I)-Y13(I)*VZ2(I))
        A3(I)=FAC*(-Z12(I)*VY2(I)+Y12(I)*VZ2(I))
        T11(I) =  T11(I)+(PS+A1(I))*F1P(I)
        T12(I) =  T12(I)+(PP+A2(I))*F1P(I)
        T13(I) =  T13(I)+(PS+A3(I))*F1P(I)
        T14(I) =  T14(I)+PD*F1P(I)
        T21(I) =  T21(I)+(PS+A1(I))*F2P(I)
        T22(I) =  T22(I)+(PP+A2(I))*F2P(I)
        T23(I) =  T23(I)+(PS+A3(I))*F2P(I)
        T24(I) =  T24(I)+PD*F2P(I)
       ENDDO
       IF(UPWM == 2)THEN
        DO I=LFT,LLT
         FAC=-DT1H/TR(I)
         F1(I)=FAC*(-F1P(I)*DZZP(I)+F2P(I)*DYZP(I))
         F2(I)=FAC*( F1P(I)*DZYP(I)-F2P(I)*DYYP(I))
         T11(I) =  T11(I)+PS*F1(I)
         T12(I) =  T12(I)+PP*F1(I)
         T13(I) =  T13(I)+PS*F1(I)
         T14(I) =  T14(I)+PD*F1(I)
         T21(I) =  T21(I)+PS*F2(I)
         T22(I) =  T22(I)+PP*F2(I)
         T23(I) =  T23(I)+PS*F2(I)
         T24(I) =  T24(I)+PD*F2(I)
        ENDDO
       ENDIF
C NODE 3      
       CALL UPWIND(RHO,VIS,VY3,VY3,VZ3,S,S,T,DELTAX,GAM)
       DO I=LFT,LLT
        TR(I)=(Y23(I)*Z34(I)-Y34(I)*Z23(I))
        DYYP(I)=(-Z34(I)*VY2(I)+Z24(I)*VY3(I)-Z23(I)*VY4(I))
        DYZP(I)=( Y34(I)*VY2(I)-Y24(I)*VY3(I)+Y23(I)*VY4(I))
        DZZP(I)=( Y34(I)*VZ2(I)-Y24(I)*VZ3(I)+Y23(I)*VZ4(I))
        DZYP(I)=(-Z34(I)*VZ2(I)+Z24(I)*VZ3(I)-Z23(I)*VZ4(I))
        F1P(I)=RHO(I)*(VY3(I)*DYYP(I)+VZ3(I)*DYZP(I))
        F2P(I)=RHO(I)*(VY3(I)*DZYP(I)+VZ3(I)*DZZP(I))
        FAC=PC*GAM(I)/TR(I)
        A1(I)=FAC*(-Z34(I)*VY3(I)+Y34(I)*VZ3(I))
        A2(I)=FAC*( Z24(I)*VY3(I)-Y24(I)*VZ3(I))
        A3(I)=FAC*(-Z23(I)*VY3(I)+Y23(I)*VZ3(I))
        T11(I) =  T11(I)+PD*F1P(I)
        T12(I) =  T12(I)+(PS+A1(I))*F1P(I)
        T13(I) =  T13(I)+(PP+A2(I))*F1P(I)
        T14(I) =  T14(I)+(PS+A3(I))*F1P(I)
        T21(I) =  T21(I)+PD*F2P(I)
        T22(I) =  T22(I)+(PS+A1(I))*F2P(I)
        T23(I) =  T23(I)+(PP+A2(I))*F2P(I)
        T24(I) =  T24(I)+(PS+A3(I))*F2P(I)
       ENDDO
       IF(UPWM == 2)THEN
        DO I=LFT,LLT
         FAC=-DT1H/TR(I)
         F1(I)=FAC*(-F1P(I)*DZZP(I)+F2P(I)*DYZP(I))
         F2(I)=FAC*( F1P(I)*DZYP(I)-F2P(I)*DYYP(I))
         T11(I) =  T11(I)+PD*F1(I)
         T12(I) =  T12(I)+PS*F1(I)
         T13(I) =  T13(I)+PP*F1(I)
         T14(I) =  T14(I)+PS*F1(I)
         T21(I) =  T21(I)+PD*F2(I)
         T22(I) =  T22(I)+PS*F2(I)
         T23(I) =  T23(I)+PP*F2(I)
         T24(I) =  T24(I)+PS*F2(I)
        ENDDO
       ENDIF
C NODE  4      
       CALL UPWIND(RHO,VIS,VY4,VY4,VZ4,S,S,T,DELTAX,GAM)
       DO I=LFT,LLT
        TR(I)=(Y34(I)*Z41(I)-Y41(I)*Z34(I))
        DYYP(I)=(-Z34(I)*VY1(I)-Z41(I)*VY3(I)-Z13(I)*VY4(I))
        DYZP(I)=( Y34(I)*VY1(I)+Y41(I)*VY3(I)+Y13(I)*VY4(I))
        DZZP(I)=( Y34(I)*VZ1(I)+Y41(I)*VZ3(I)+Y13(I)*VZ4(I))
        DZYP(I)=(-Z34(I)*VZ1(I)-Z41(I)*VZ3(I)-Z13(I)*VZ4(I))
        F1P(I)=RHO(I)*(VY4(I)*DYYP(I)+VZ4(I)*DYZP(I))
        F2P(I)=RHO(I)*(VY4(I)*DZYP(I)+VZ4(I)*DZZP(I))
        FAC=PC*GAM(I)/TR(I)
        A1(I)=FAC*(-Z34(I)*VY4(I)+Y34(I)*VZ4(I))
        A2(I)=FAC*(-Z41(I)*VY4(I)+Y41(I)*VZ4(I))
        A3(I)=FAC*(-Z13(I)*VY4(I)+Y13(I)*VZ4(I))
        T11(I) =  T11(I)+(PS+A1(I))*F1P(I)
        T12(I) =  T12(I)+PD*F1P(I)
        T13(I) =  T13(I)+(PS+A2(I))*F1P(I)
        T14(I) =  T14(I)+(PP+A3(I))*F1P(I)
        T21(I) =  T21(I)+(PS+A1(I))*F2P(I)
        T22(I) =  T22(I)+PD*F2P(I)
        T23(I) =  T23(I)+(PS+A2(I))*F2P(I)
        T24(I) =  T24(I)+(PP+A3(I))*F2P(I)
       ENDDO
       IF(UPWM == 2)THEN
        DO I=LFT,LLT
         FAC=-DT1H/TR(I)
         F1(I)=FAC*(-F1P(I)*DZZP(I)+F2P(I)*DYZP(I))
         F2(I)=FAC*( F1P(I)*DZYP(I)-F2P(I)*DYYP(I))
         T11(I) =  T11(I)+PS*F1(I)
         T12(I) =  T12(I)+PD*F1(I)
         T13(I) =  T13(I)+PS*F1(I)
         T14(I) =  T14(I)+PP*F1(I)
         T21(I) =  T21(I)+PS*F2(I)
         T22(I) =  T22(I)+PD*F2(I)
         T23(I) =  T23(I)+PS*F2(I)
         T24(I) =  T24(I)+PP*F2(I)
        ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------
      RETURN
      END
