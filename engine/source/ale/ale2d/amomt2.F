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
Chd|  AMOMT2                        source/ale/ale2d/amomt2.F     
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        UPWIND                        source/elements/solid/solide/upwind.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE AMOMT2(
     .   PM , V     , W  ,  RHO,
     .   Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4,
     .   T11, T12   , T13,  T14, T21  , T22, T23 , T24, 
     .   PY1, PY2   , PZ1,  PZ2, AIREM,
     .   VY1, VY2   , VY3,  VY4, VZ1  , VZ2, VZ3 , VZ4,
     .   DYY, DZZ   , DYZ,  DZY,
     .   NC1, NC2   , NC3,  NC4, MAT  , OFF,
     .   QMV, BUFMAT, DELTAX, VIS)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines computes convective term
C from momentum continuity equation in 2D.
C it results into a forces F1,F2 computed at cell 
C centroid.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
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
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PM(NPROPM,NUMMAT), V(3,NUMNOD), W(3,NUMNOD), RHO(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Z1(*), Z2(*), Z3(*), Z4(*),
     .   T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*),T24(*), 
     .   PY1(*), PY2(*), PZ1(*), PZ2(*), AIREM(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), 
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*),
     .   DYY(*), DZZ(*), DYZ(*), DZY(*), OFF(*),QMV(8,*),BUFMAT(*), DELTAX(*), VIS(*)
      INTEGER MAT(*), NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IADBUF,IFLG
      my_real :: F1(MVSIZ), F2(MVSIZ),A1(MVSIZ), A2(MVSIZ), XMS(MVSIZ), GAMMA(MVSIZ),VDY(MVSIZ), VDZ(MVSIZ)
      my_real :: FAC,AAA,DM,DMY,DMZ
      my_real :: FVY1(MVSIZ), FVZ1(MVSIZ), FVY2(MVSIZ), FVZ2(MVSIZ), FVY3(MVSIZ), FVZ3(MVSIZ), FVY4(MVSIZ), FVZ4(MVSIZ)
      my_real :: PC, PD, PS, PP
      my_real
     .   Y12(MVSIZ), Z12(MVSIZ), Y23(MVSIZ), Z23(MVSIZ),
     .   Y34(MVSIZ), Z34(MVSIZ), Y41(MVSIZ), Z41(MVSIZ),
     .   Y24(MVSIZ), Z24(MVSIZ), Y13(MVSIZ), Z13(MVSIZ),
     .   DYYP(MVSIZ), DZZP(MVSIZ), DYZP(MVSIZ), DZYP(MVSIZ),
     .   F1P(MVSIZ), F2P(MVSIZ), A3(MVSIZ),
     .   S(3,MVSIZ), T(3,MVSIZ), TR(MVSIZ)

C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
      !-----------------------------------------------
      !   RELATIVE VELOCITY
      !-----------------------------------------------
      DO I=LFT,LLT
        ! Save fluid velocity
        FVY1(I) = VY1(I)
        VY1(I)  = VY1(I) - W(2,NC1(I))
        ! Save fluid velocity
        FVZ1(I) = VZ1(I)
        VZ1(I)  = VZ1(I) - W(3,NC1(I))
        ! Save fluid velocity
        FVY2(I) = VY2(I)
        VY2(I)  = VY2(I) - W(2,NC2(I))
        ! Save fluid velocity
        FVZ2(I) = VZ2(I)
        VZ2(I)  = VZ2(I) - W(3,NC2(I))
        ! Save fluid velocity
        FVY3(I) = VY3(I)
        VY3(I)  = VY3(I) - W(2,NC3(I))
        ! Save fluid velocity
        FVZ3(I) = VZ3(I)
        VZ3(I)  = VZ3(I) - W(3,NC3(I))
        ! Save fluid velocity
        FVY4(I) = VY4(I)
        VY4(I)  = VY4(I) - W(2,NC4(I))
        ! Save fluid velocity
        FVZ4(I) = VZ4(I)
        VZ4(I)  = VZ4(I) - W(3,NC4(I))
      ENDDO

      !-----------------------------------------------
      !   CENTROID VALUE (MEAN)
      !-----------------------------------------------
      DO I=LFT,LLT
        XMS(I)  = FOURTH*RHO(I)*AIREM(I)
        GAMMA(I)= PM(15,MAT(I))
      ENDDO
      DO I=LFT,LLT
        VDY(I)  = FOURTH*(VY1(I)+VY2(I)+VY3(I)+VY4(I))
        VDZ(I)  = FOURTH*(VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I))
      ENDDO

      !-----------------------------------------------
      !   LAW51 : convective terme computed like in FVM
      !-----------------------------------------------
      !   DMi = 0.5*DT1*QMV(i,I)  masse entrant par la face i
      IF(MTN == 51 .AND. ALE%UPWIND%UPWM /= 3) THEN
        IADBUF = NINT(PM(27,MAT(1)))
        IFLG = NINT(BUFMAT(31+IADBUF-1))
        IF(IFLG > 1)RETURN
        IF(ALE%UPWIND%UPWM == 0.)THEN
         DO I=LFT,LLT
          GAMMA(I)= PM(15,MAT(I))
         ENDDO
        ELSE
         DO I=LFT,LLT
          GAMMA(I)= ALE%UPWIND%CUPWM
         ENDDO
        ENDIF
        DO I=LFT,LLT
          AAA      = QMV(1,I)+QMV(2,I)+QMV(3,I)+QMV(4,I)
          AAA      = FOURTH * AAA
          QMV(1,I) = FOURTH * (QMV(1,I) - AAA)   
          QMV(2,I) = FOURTH * (QMV(2,I) - AAA)   
          QMV(3,I) = FOURTH * (QMV(3,I) - AAA)   
          QMV(4,I) = FOURTH * (QMV(4,I) - AAA)   
          DMY      = ZERO
          DMZ      = ZERO
          DM       = QMV(4,I)+QMV(1,I)
          DMY      = DMY + VY1(I)*DM
          DMZ      = DMZ + VZ1(I)*DM
          DM       = QMV(1,I)+QMV(2,I)
          DMY      = DMY + VY2(I)*DM
          DMZ      = DMZ + VZ2(I)*DM
          DM       = QMV(2,I)+QMV(3,I)
          DMY      = DMY + VY3(I)*DM
          DMZ      = DMZ + VZ3(I)*DM
          DM       = QMV(3,I)+QMV(4,I)
          DMY      = DMY + VY4(I)*DM
          DMZ      = DMZ + VZ4(I)*DM
          DMY      = -FOURTH * DMY
          DMZ      = -FOURTH * DMZ
          A1(I)    = PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
          A2(I)    = PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
          A1(I)    = SIGN(GAMMA(I),A1(I))
          A2(I)    = SIGN(GAMMA(I),A2(I))
          T11(I)   = T11(I)+(ONE+A1(I))*DMY
          T12(I)   = T12(I)+(ONE+A2(I))*DMY
          T13(I)   = T13(I)+(ONE-A1(I))*DMY
          T14(I)   = T14(I)+(ONE-A2(I))*DMY
          T21(I)   = T21(I)+(ONE+A1(I))*DMZ
          T22(I)   = T22(I)+(ONE+A2(I))*DMZ
          T23(I)   = T23(I)+(ONE-A1(I))*DMZ
          T24(I)   = T24(I)+(ONE-A2(I))*DMZ
        ENDDO
      ELSEIF (MTN /= 51 .AND. ALE%UPWIND%UPWM /= 3) THEN
      !-----------------------------------------------
      !   other material laws
      !-----------------------------------------------
        DO I=LFT,LLT
          F1(I)   = (VDY(I)*DYY(I)+VDZ(I)*DYZ(I))*XMS(I)*OFF(I)
          F2(I)   = (VDY(I)*DZY(I)+VDZ(I)*DZZ(I))*XMS(I)*OFF(I)
        ENDDO
        DO I=LFT,LLT
          A1(I)   = PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
          A2(I)   = PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
        ENDDO
        DO I=LFT,LLT
          A1(I)   = SIGN(GAMMA(I),A1(I))
          A2(I)   = SIGN(GAMMA(I),A2(I))
        ENDDO
        DO I=LFT,LLT
          T11(I)   =  T11(I)+(ONE+A1(I))*F1(I)
          T12(I)   =  T12(I)+(ONE+A2(I))*F1(I)
          T13(I)   =  T13(I)+(ONE-A1(I))*F1(I)
          T14(I)   =  T14(I)+(ONE-A2(I))*F1(I)

          T21(I)   =  T21(I)+(ONE+A1(I))*F2(I)
          T22(I)   =  T22(I)+(ONE+A2(I))*F2(I)
          T23(I)   =  T23(I)+(ONE-A1(I))*F2(I)
          T24(I)   =  T24(I)+(ONE-A2(I))*F2(I)
        ENDDO

      ELSEIF (ALE%UPWIND%UPWM == 3) THEN
         PC=FOURTH
         PD=ONE_OVER_16
         PS=ONE_OVER_16
         PP=ONE_OVER_16
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
         !-----------------------------------------------
         ! SUPG implementation for 2D ALE 
         !       NODE 1
         !-----------------------------------------------
         CALL UPWIND(RHO,VIS,VY1,VY1,VZ1,S,S,T,DELTAX,GAMMA)
         DO I=LFT,LLT
            TR(I)=(Y41(I)*Z12(I)-Y12(I)*Z41(I))
            DYYP(I)=(-Z24(I)*FVY1(I)-Z41(I)*FVY2(I)-Z12(I)*FVY4(I))
            DYZP(I)=( Y24(I)*FVY1(I)+Y41(I)*FVY2(I)+Y12(I)*FVY4(I))
            DZZP(I)=( Y24(I)*FVZ1(I)+Y41(I)*FVZ2(I)+Y12(I)*FVZ4(I))
            DZYP(I)=(-Z24(I)*FVZ1(I)-Z41(I)*FVZ2(I)-Z12(I)*FVZ4(I))
            F1P(I)=RHO(I)*(VY1(I)*DYYP(I)+VZ1(I)*DYZP(I))
            F2P(I)=RHO(I)*(VY1(I)*DZYP(I)+VZ1(I)*DZZP(I))
            FAC=ZERO
            IF(TR(I)/=ZERO)FAC=PC*GAMMA(I)/TR(I)
            A1(I)=FAC*(-Z24(I)*VY1(I)+Y24(I)*VZ1(I))
            A2(I)=FAC*(-Z41(I)*VY1(I)+Y41(I)*VZ1(I))
            A3(I)=FAC*(-Z12(I)*VY1(I)+Y12(I)*VZ1(I))
     
            T11(I) =  T11(I)+(PP+A1(I))*F1P(I)
            T12(I) =  T12(I)+(PS+A2(I))*F1P(I)
            T13(I) =  T13(I)+PD*F1P(I)
            T14(I) =  T14(I)+(PS+A3(I))*F1P(I)
            T21(I) =  T21(I)+(PP+A1(I))*F2P(I)
            T22(I) =  T22(I)+(PS+A2(I))*F2P(I)
            T23(I) =  T23(I)+PD*F2P(I)
            T24(I) =  T24(I)+(PS+A3(I))*F2P(I)
         ENDDO
         !-----------------------------------------------
         ! SUPG implementation for 2D ALE 
         !       NODE 2
         !-----------------------------------------------
         CALL UPWIND(RHO,VIS,VY2,VY2,VZ2,S,S,T,DELTAX,GAMMA)
         DO I=LFT,LLT
            TR(I)=(Y12(I)*Z23(I)-Y23(I)*Z12(I))
            DYYP(I)=(-Z23(I)*FVY1(I)+Z13(I)*FVY2(I)-Z12(I)*FVY3(I))
            DYZP(I)=( Y23(I)*FVY1(I)-Y13(I)*FVY2(I)+Y12(I)*FVY3(I))
            DZZP(I)=( Y23(I)*FVZ1(I)-Y13(I)*FVZ2(I)+Y12(I)*FVZ3(I))
            DZYP(I)=(-Z23(I)*FVZ1(I)+Z13(I)*FVZ2(I)-Z12(I)*FVZ3(I))
            F1P(I)=RHO(I)*(VY2(I)*DYYP(I)+VZ2(I)*DYZP(I))
            F2P(I)=RHO(I)*(VY2(I)*DZYP(I)+VZ2(I)*DZZP(I))
            FAC=ZERO
            IF(TR(I)/=ZERO)FAC=PC*GAMMA(I)/TR(I)
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
         !-----------------------------------------------
         ! SUPG implementation for 2D ALE 
         !       NODE 3
         !-----------------------------------------------
         CALL UPWIND(RHO,VIS,VY3,VY3,VZ3,S,S,T,DELTAX,GAMMA)
         DO I=LFT,LLT
            TR(I)=(Y23(I)*Z34(I)-Y34(I)*Z23(I))
            DYYP(I)=(-Z34(I)*FVY2(I)+Z24(I)*FVY3(I)-Z23(I)*FVY4(I))
            DYZP(I)=( Y34(I)*FVY2(I)-Y24(I)*FVY3(I)+Y23(I)*FVY4(I))
            DZZP(I)=( Y34(I)*FVZ2(I)-Y24(I)*FVZ3(I)+Y23(I)*FVZ4(I))
            DZYP(I)=(-Z34(I)*FVZ2(I)+Z24(I)*FVZ3(I)-Z23(I)*FVZ4(I))
            F1P(I)=RHO(I)*(VY3(I)*DYYP(I)+VZ3(I)*DYZP(I))
            F2P(I)=RHO(I)*(VY3(I)*DZYP(I)+VZ3(I)*DZZP(I))
            FAC=ZERO
            IF(TR(I)/=ZERO)FAC=PC*GAMMA(I)/TR(I)
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
         !-----------------------------------------------
         ! SUPG implementation for 2D ALE 
         !       NODE 4
         !-----------------------------------------------
         CALL UPWIND(RHO,VIS,VY4,VY4,VZ4,S,S,T,DELTAX,GAMMA)
         DO I=LFT,LLT
            TR(I)=(Y34(I)*Z41(I)-Y41(I)*Z34(I))
            DYYP(I)=(-Z34(I)*FVY1(I)-Z41(I)*FVY3(I)-Z13(I)*FVY4(I))
            DYZP(I)=( Y34(I)*FVY1(I)+Y41(I)*FVY3(I)+Y13(I)*FVY4(I))
            DZZP(I)=( Y34(I)*FVZ1(I)+Y41(I)*FVZ3(I)+Y13(I)*FVZ4(I))
            DZYP(I)=(-Z34(I)*FVZ1(I)-Z41(I)*FVZ3(I)-Z13(I)*FVZ4(I))
            F1P(I)=RHO(I)*(VY4(I)*DYYP(I)+VZ4(I)*DYZP(I))
            F2P(I)=RHO(I)*(VY4(I)*DZYP(I)+VZ4(I)*DZZP(I))
            FAC=ZERO
            IF(TR(I)/=ZERO)FAC=PC*GAMMA(I)/TR(I)
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
      ENDIF
C-----------------------------------------------
      RETURN
      END
