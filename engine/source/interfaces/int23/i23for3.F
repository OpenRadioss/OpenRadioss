Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i23for3    ../engine/source/interfaces/int23/i23for3.F
!||--- called by ------------------------------------------------------
!||    i23mainf   ../engine/source/interfaces/int23/i23mainf.F
!||--- calls      -----------------------------------------------------
!||    i7ass0     ../engine/source/interfaces/int07/i7ass3.F
!||    i7ass05    ../engine/source/interfaces/int07/i7ass3.F
!||    i7ass2     ../engine/source/interfaces/int07/i7ass3.F
!||    i7ass25    ../engine/source/interfaces/int07/i7ass3.F
!||    i7ass3     ../engine/source/interfaces/int07/i7ass3.F
!||    i7ass35    ../engine/source/interfaces/int07/i7ass3.F
!||    i7sms2     ../engine/source/interfaces/int07/i7sms2.F
!||    ibcoff     ../engine/source/interfaces/interf/ibcoff.F
!||--- uses       -----------------------------------------------------
!||    anim_mod   ../common_source/modules/output/anim_mod.F
!||    h3d_mod    ../engine/share/modules/h3d_mod.F
!||    tri7box    ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I23FOR3(JLT    ,NIN    ,NOINT  ,IBC    ,ICODT  ,
     2                  FSAV    ,GAP    ,STIGLO ,FRIC   ,VISC   ,
     3                  INACTI  ,MFROT  ,IFQ    ,IBAG   ,
     4                  ICURV   ,STIF    ,GAPV  ,ITAB   ,A      ,    
     5                  CAND_P  ,FROT_P ,ALPHA0 ,V     ,ICONTACT,
     6                  NISKYFI,NSVG    ,X1     ,Y1     ,Z1     ,
     7                  X2     ,Y2      ,Z2     ,X3     ,Y3     ,
     8                  Z3     ,X4      ,Y4     ,Z4     ,XI     ,
     9                  YI     ,ZI     ,VXI    ,VYI    ,VZI     ,
     A                  MSI    ,VXM    ,VYM    ,VZM    ,NX      ,
     B                  NY     ,NZ     ,PENE   ,H1     ,H2      ,
     C                  H3     ,H4     ,INDEX  ,CAND_N_N, WEIGHT,
     F                  FXT    ,FYT    ,FZT    ,DT2T   ,
     G                  FCONT   ,FNCONT ,FTCONT ,STIFN  ,VISCN  ,
     H                  NEWFRONT ,ISECIN ,NSTRF  ,SECFCUM,FSKYI ,
     I                  ISKY     ,INTTH  ,MS     ,IX1    ,IX2   ,
     J                  IX3      ,IX4    ,CAND_FX,CAND_FY,CAND_FZ ,
     K                  KMIN     ,KMAX   ,CN_LOC ,CE_LOC  ,MSKYI_SMS ,
     L                  ISKYI_SMS,NSMS   ,JTASK,ISENSINT  ,  FSAVPARIT, 
     M                  NISUB    ,NFT    ,H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE H3D_MOD
      USE ANIM_MOD
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IBC, INACTI, IBAG, NIN, NOINT, INTTH,JTASK,
     .        MFROT, IFQ, ICURV(3),
     .        ICODT(*), ITAB(*) ,ICONTACT(*),
     .        NISKYFI, ISECIN, NSTRF(*),NEWFRONT, ISKY(*), ISKYI_SMS(*)
      INTEGER NSVG(MVSIZ),CAND_N_N(MVSIZ), WEIGHT(*),
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        CN_LOC(MVSIZ), CE_LOC(MVSIZ), INDEX(*), NSMS(MVSIZ), 
     .        ISENSINT(*),NISUB,NFT
      my_real
     .   STIGLO, CAND_P(*), FROT_P(*), FSAV(*), FSKYI(LSKYI,4),
     .   ALPHA0, GAP, FRIC, VISC, KMIN, KMAX, DT2T, MSKYI_SMS(*)
      my_real
     .   STIF(MVSIZ), GAPV(MVSIZ),
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .   X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .   X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .   X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .   X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .   XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),PENE(MVSIZ),
     .   VXM(MVSIZ), VYM(MVSIZ), VZM(MVSIZ), 
     .   FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ)
      my_real
     .   A(3,*), V(3,*), MS(*),
     .   FCONT(3,*), FNCONT(3,*),FTCONT(3,*), STIFN(*), VISCN(*),
     .   SECFCUM(7,NUMNOD,NSECT),
     .   CAND_FX(*), CAND_FY(*), CAND_FZ(*),FSAVPARIT(NISUB+1,11,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,IBID,
     .        IRB(0:511),IRB2(0:511),NA1,NA2,IBCM,IBCS
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ),FNI(MVSIZ),
     .   FXN(MVSIZ), FYN(MVSIZ), FZN(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   XMU(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   VNX, VNY, VNZ, AA, S2, DIST, RDIST, DTI,
     .   V2, FF, ALPHA, BETA,
     .   FX, FY, FZ, FT, FN, FMAX, FTN,
     .   ECONTT, ECONVT,ECONTDT,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8, 
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15,
     .   VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,AREA,P,VV1,VV2,DMU,
     .   DT1INV, VIS, RBID,
     .   IMPX,IMPY,IMPZ
      my_real
     .   PREC
      my_real
     .   STIF0(MVSIZ),
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,DTMINI
C-----------------------------------------------
      RBID = ZERO
      IBID = 0
      IF (IRESP==1) THEN
           PREC = FIVEEM4
      ELSE
           PREC = EM10
      ENDIF
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
C---------------------
C     PENE INITIALE
C---------------------
      IF(INACTI==6)THEN
        DO I=1,JLT
C REDUCTION OF THE INITIAL PENALTY
          CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .      ( (ONE-FIVEEM2)*CAND_P(INDEX(I))
     .        +FIVEEM2*(PENE(I)+FIVEEM2*MAX(GAPV(I)-PENE(I),ZERO)))  )
C SUBTRACTION OF THE INITIAL PENALTY FROM THE PENALTY AND GAP
          PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
          IF( PENE(I)==ZERO )  STIF(I) = ZERO
        ENDDO
       ELSE
        DO I=1,JLT
C REDUCTION OF THE INITIAL PENALTY
          CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .           ((ONE-FIVEEM2)*CAND_P(INDEX(I))+FIVEEM2*PENE(I))  )
C SUBTRACTION OF THE INITIAL PENALTY FROM THE PENALTY AND GAP
          PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
          IF( PENE(I)==ZERO ) STIF(I) = ZERO
        ENDDO
      END IF
C-------------------------------------------
C     FNI + STIF
C---------------------------------
      ECONTT = ZERO
      ECONVT = ZERO
      ECONTDT = ZERO
      DO I=1,JLT
       IF(STIGLO<=ZERO)THEN
         STIF(I) = -STIGLO*STIF(I)
       ELSEIF(STIF(I)/=ZERO)THEN
         IF(STIF(I)/=ZERO) STIF(I) = STIGLO
       ENDIF
       IF(STIF(I)/=ZERO)STIF(I)=MIN(KMAX,MAX(KMIN,STIF(I)))
       ECONTT = ECONTT + STIF(I)*PENE(I)**2
       FNI(I) = - STIF(I) * PENE(I) 
      END DO
C
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO
C---------------------------------
C     DAMPING
C---------------------------------
      DO I=1,JLT
        VX(I) = VXI(I)-VXM(I)
        VY(I) = VYI(I)-VYM(I)
        VZ(I) = VZI(I)-VZM(I)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
      ENDDO
C
      IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
        DO I=1,JLT
          VIS = VISC * SQRT(TWO * STIF(I) * MSI(I))
          FNI(I)  = FNI(I) + VIS * VN(I)
          ECONTDT = ECONTDT + VIS * VN(I) * VN(I) * DT1
C stability only
C inutile         STIF(I) = TWO * (STIF(I) + VIS *DT1INV)
          STIF(I) = STIF(I) + VIS *DT1INV
        ENDDO
      ELSE
        DO I=1,JLT
          C(I) = VISC * SQRT(TWO * STIF(I) * MSI(I))
          FNI(I)  = FNI(I) + C(I) * VN(I)
          ECONTDT= ECONTDT + C(I) * VN(I) * VN(I) * DT1
C stability only
C inutile         C(I) = TWO*C(I)
C inutile          KT(I)= TWO*STIF(I)
          C(I) = C(I)
          KT(I)= STIF(I)
          CF(I)= ZERO
          STIF(I) = KT(I) + C(I) *DT1INV
        ENDDO
      END IF
C---------------------------------
C     CALCULATION OF THE NORMAL FORCE
C---------------------------------
      DO I=1,JLT
       FXN(I)=FNI(I)*NX(I)
       FYN(I)=FNI(I)*NY(I)
       FZN(I)=FNI(I)*NZ(I)
      END DO
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION NORMALE
C---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV8 = ZERO
      FSAV9 = ZERO
      FSAV10= ZERO
      FSAV11= ZERO
      DO I=1,JLT
       IMPX=FXN(I)*DT12
       IMPY=FYN(I)*DT12
       IMPZ=FZN(I)*DT12
       FSAV1 =FSAV1 +IMPX
       FSAV2 =FSAV2 +IMPY
       FSAV3 =FSAV3 +IMPZ
       FSAV8 =FSAV8 +ABS(IMPX)
       FSAV9 =FSAV9 +ABS(IMPY)
       FSAV10=FSAV10+ABS(IMPZ)
       FSAV11=FSAV11+FNI(I)*DT12
      ENDDO
#include "lockon.inc"
       FSAV(1)=FSAV(1)+FSAV1
       FSAV(2)=FSAV(2)+FSAV2
       FSAV(3)=FSAV(3)+FSAV3
       FSAV(8)=FSAV(8)+FSAV8
       FSAV(9)=FSAV(9)+FSAV9
       FSAV(10)=FSAV(10)+FSAV10
       FSAV(11)=FSAV(11)+FSAV11
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,1,I+NFT) =  FXN(I)
          FSAVPARIT(1,2,I+NFT) =  FYN(I)
          FSAVPARIT(1,3,I+NFT) =  FZN(I)
        ENDDO
      ENDIF
C---------------------------------
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
         IF (INCONV==1) THEN
#include "lockon.inc"
           DO I=1,JLT
            FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FXN(I)*H1(I)
            FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FYN(I)*H1(I)
            FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZN(I)*H1(I)
            FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FXN(I)*H2(I)
            FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FYN(I)*H2(I)
            FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZN(I)*H2(I)
            FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FXN(I)*H3(I)
            FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FYN(I)*H3(I)
            FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZN(I)*H3(I)
            FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FXN(I)*H4(I)
            FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FYN(I)*H4(I)
            FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZN(I)*H4(I)
            JG = NSVG(I)
            IF(JG>0) THEN
C In SPMD: Treatment to be redone after reception node Remote if JG <0
              FNCONT(1,JG)=FNCONT(1,JG)- FXN(I)
              FNCONT(2,JG)=FNCONT(2,JG)- FYN(I)
              FNCONT(3,JG)=FNCONT(3,JG)- FZN(I)
            ELSE ! cas noeud remote en SPMD
              JG = -JG
              FNCONTI(NIN)%P(1,JG)=FNCONTI(NIN)%P(1,JG)-FXN(I)
              FNCONTI(NIN)%P(2,JG)=FNCONTI(NIN)%P(2,JG)-FYN(I)
              FNCONTI(NIN)%P(3,JG)=FNCONTI(NIN)%P(3,JG)-FZN(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV==1) THEN
      ENDIF
C---------------------------------
C     NEW FRICTION MODELS
C---------------------------------
      IF (MFROT==0) THEN
C---    Coulomb friction
        DO I=1,JLT
          XMU(I) = FRIC
        ENDDO
      ELSEIF (MFROT==1) THEN
C---    Viscous friction
        DO I=1,JLT
C attention : normal <> elt normal
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
          VV  = SQRT(MAX(EM30,V2))
          AX1 = X3(I) - X1(I)
          AY1 = Y3(I) - Y1(I)
          AZ1 = X3(I) - Z1(I)
          AX2 = X4(I) - X2(I)
          AY2 = Y4(I) - Y2(I)
          AZ2 = X4(I) - Z2(I)
          AX  = AY1*AZ2 - AZ1*AY2
          AY  = AZ1*AX2 - AX1*AZ2
          AZ  = AX1*AY2 - AY1*AX2
          AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
          P =  FNI(I)/AREA
          XMU(I) = FRIC + (FROT_P(1) + FROT_P(4)*P ) * P 
     .           +(FROT_P(2) + FROT_P(3)*P) * VV + FROT_P(5)*V2
          XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ELSEIF(MFROT==2)THEN
C---    Loi Darmstad
        DO I=1,JLT
C attention : normal <> elt normal 
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
          VV  = SQRT(MAX(EM30,V2))
          AX1 = X3(I) - X1(I)
          AY1 = Y3(I) - Y1(I)
          AZ1 = X3(I) - Z1(I)
          AX2 = X4(I) - X2(I)
          AY2 = Y4(I) - Y2(I)
          AZ2 = X4(I) - Z2(I)
          AX  = AY1*AZ2 - AZ1*AY2
          AY  = AZ1*AX2 - AX1*AZ2
          AZ  = AX1*AY2 - AY1*AX2
          AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
          P =  FNI(I)/AREA
          XMU(I) = FRIC
     .           + FROT_P(1)*EXP(FROT_P(2)*VV)*P*P
     .           + FROT_P(3)*EXP(FROT_P(4)*VV)*P
     .           + FROT_P(5)*EXP(FROT_P(6)*VV)
          XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ELSEIF (MFROT==3) THEN
C---    Renard 
        DO I=1,JLT
C attention : normal <> element normal  
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
          VV = SQRT(MAX(EM30,V2))
          IF(VV>=0.AND.VV<=FROT_P(5)) THEN
            DMU = FROT_P(3)-FROT_P(1)
            VV1 = VV / FROT_P(5)
            XMU(I) = FROT_P(1)+ DMU*VV1*(TWO-VV1)
          ELSEIF(VV>FROT_P(5).AND.VV<FROT_P(6)) THEN
            DMU = FROT_P(4)-FROT_P(3) 
            VV1 = (VV - FROT_P(5))/(FROT_P(6)-FROT_P(5))
            XMU(I) = FROT_P(3)+ DMU * (THREE-TWO*VV1)*VV1**2
          ELSE
            DMU = FROT_P(2)-FROT_P(4)
            VV2 = (VV - FROT_P(6))**2
            XMU(I) = FROT_P(2) - DMU / (ONE + DMU*VV2)
          ENDIF
          XMU(I) = MAX(XMU(I),EM30)
        ENDDO
      ELSEIF(MFROT==4)THEN
C---    Exponential decay model
        DO I=1,JLT
          AA = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
          V2 = (VX(I) - NX(I)*AA)**2 
     .       + (VY(I) - NY(I)*AA)**2 
     .       + (VZ(I) - NZ(I)*AA)**2
           VV = SQRT(MAX(EM30,V2))
           XMU(I) = FROT_P(1)
     .        + (FRIC-FROT_P(1))*EXP(-FROT_P(2)*VV)
           XMU(I) = MAX(XMU(I),EM30)
         ENDDO
      ENDIF
C------------------
C    TANGENT FORCE CALCULATION
C------------------
      FSAV4 = ZERO
      FSAV5 = ZERO
      FSAV6 = ZERO
      FSAV12= ZERO
      FSAV13= ZERO
      FSAV14= ZERO
      FSAV15= ZERO
C---------------------------------
C     INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
      IF (IFQ==13) THEN
        ALPHA = MAX(ONE,ALPHA0*DT12)
      ELSE
        ALPHA = ALPHA0
      ENDIF
      DO I=1,JLT
        FX = STIF0(I)*VX(I)*DT12
        FY = STIF0(I)*VY(I)*DT12
        FZ = STIF0(I)*VZ(I)*DT12
        FX = FXT(I) + ALPHA*FX
        FY = FYT(I) + ALPHA*FY
        FZ = FZT(I) + ALPHA*FZ
        FTN = FX*NX(I) + FY*NY(I) + FZ*NZ(I)
        FX = FX - FTN*NX(I)
        FY = FY - FTN*NY(I)
        FZ = FZ - FTN*NZ(I)
        FT = FX*FX + FY*FY + FZ*FZ
        FT = MAX(FT,EM30)
        FN = FNI(I)*FNI(I)
        BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
        FXT(I) = FX * BETA
        FYT(I) = FY * BETA
        FZT(I) = FZ * BETA
        CAND_FX(INDEX(I)) = FXT(I)
        CAND_FY(INDEX(I)) = FYT(I)
        CAND_FZ(INDEX(I)) = FZT(I)
C-------    total force
        FXI(I)=FXN(I) + FXT(I)
        FYI(I)=FYN(I) + FYT(I)
        FZI(I)=FZN(I) + FZT(I)

        ECONVT = ECONVT 
     .         + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I)) 
      ENDDO
C---------------------------------
C
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
         IF (INCONV==1) THEN
#include "lockon.inc"
           DO I=1,JLT
            FTCONT(1,IX1(I)) =FTCONT(1,IX1(I)) + FXT(I)*H1(I)
            FTCONT(2,IX1(I)) =FTCONT(2,IX1(I)) + FYT(I)*H1(I)
            FTCONT(3,IX1(I)) =FTCONT(3,IX1(I)) + FZT(I)*H1(I)
            FTCONT(1,IX2(I)) =FTCONT(1,IX2(I)) + FXT(I)*H2(I)
            FTCONT(2,IX2(I)) =FTCONT(2,IX2(I)) + FYT(I)*H2(I)
            FTCONT(3,IX2(I)) =FTCONT(3,IX2(I)) + FZT(I)*H2(I)
            FTCONT(1,IX3(I)) =FTCONT(1,IX3(I)) + FXT(I)*H3(I)
            FTCONT(2,IX3(I)) =FTCONT(2,IX3(I)) + FYT(I)*H3(I)
            FTCONT(3,IX3(I)) =FTCONT(3,IX3(I)) + FZT(I)*H3(I)
            FTCONT(1,IX4(I)) =FTCONT(1,IX4(I)) + FXT(I)*H4(I)
            FTCONT(2,IX4(I)) =FTCONT(2,IX4(I)) + FYT(I)*H4(I)
            FTCONT(3,IX4(I)) =FTCONT(3,IX4(I)) + FZT(I)*H4(I)
            JG = NSVG(I)
            IF(JG>0) THEN
C In SPMD: Treatment to be redoneafter reception node Remote if JG <0
              FTCONT(1,JG)=FTCONT(1,JG)- FXT(I)
              FTCONT(2,JG)=FTCONT(2,JG)- FYT(I)
              FTCONT(3,JG)=FTCONT(3,JG)- FZT(I)
            ELSE ! cas noeud remote en SPMD
              JG = -JG
              FTCONTI(NIN)%P(1,JG)=FTCONTI(NIN)%P(1,JG)-FXT(I)
              FTCONTI(NIN)%P(2,JG)=FTCONTI(NIN)%P(2,JG)-FYT(I)
              FTCONTI(NIN)%P(3,JG)=FTCONTI(NIN)%P(3,JG)-FZT(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV==1) THEN
      ENDIF
C
      DO I=1,JLT
       IMPX=FXT(I)*DT12
       IMPY=FYT(I)*DT12
       IMPZ=FZT(I)*DT12
       FSAV4 =FSAV4 +IMPX
       FSAV5 =FSAV5 +IMPY
       FSAV6 =FSAV6 +IMPZ
       IMPX=FXI(I)*DT12
       IMPY=FYI(I)*DT12
       IMPZ=FZI(I)*DT12
       FSAV12=FSAV12+ABS(IMPX)
       FSAV13=FSAV13+ABS(IMPY)
       FSAV14=FSAV14+ABS(IMPZ)
       FSAV15=FSAV15+SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
      ENDDO
#include "lockon.inc"
      FSAV(4) = FSAV(4) + FSAV4
      FSAV(5) = FSAV(5) + FSAV5
      FSAV(6) = FSAV(6) + FSAV6
      FSAV(12) = FSAV(12) + FSAV12
      FSAV(13) = FSAV(13) + FSAV13
      FSAV(14) = FSAV(14) + FSAV14
      FSAV(15) = FSAV(15) + FSAV15
      FSAV(26) = FSAV(26) + ECONTT
      FSAV(27) = FSAV(27) + ECONVT
      FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,4,I+NFT) =  FXT(I)
          FSAVPARIT(1,5,I+NFT) =  FYT(I)
          FSAVPARIT(1,6,I+NFT) =  FZT(I)
        ENDDO
      ENDIF
C---------------------------------
#include "lockon.inc"
      IF (INCONV==1) THEN
        ECONTV = ECONTV + ECONVT  ! Frictional Energy
        ECONT  = ECONT + ECONTT   ! Elastic Energy
        ECONTD = ECONTD + ECONTDT ! Damping Energy
      END IF !(INCONV==1) THEN
#include "lockoff.inc"
C---------------------------------
      IF(KDTINT==1)THEN
       IF(VISC/=ZERO)THEN
        DO I=1,JLT
C        C (i) = 2.*C (i)
C
         IF(MSI(I)==ZERO)THEN
          KS(I) =ZERO
          CS(I) =ZERO
          STV(I)=ZERO
         ELSE
          CX  = FOUR*C(I)*C(I)
          CY  = EIGHT*MSI(I)*KT(I)
          AUX   = SQRT(CX+CY)+TWO*C(I)
          STV(I)= KT(I)*AUX*AUX/MAX(CY,EM30)
          KS(I)= KT(I)
          CS(I) =C(I)
         ENDIF      
C
         J1=IX1(I)
         IF(MS(J1)==ZERO)THEN
          K1(I) =ZERO
          C1(I) =ZERO
          ST1(I)=ZERO
         ELSE
          K1(I)=KT(I)*ABS(H1(I))
          C1(I)=C(I)*ABS(H1(I))
          CX   =FOUR*C1(I)*C1(I)
          CY   =EIGHT*MS(J1)*K1(I)
          AUX   = SQRT(CX+CY)+TWO*C1(I)
          ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
         ENDIF      
C
         J1=IX2(I)
         IF(MS(J1)==ZERO)THEN
          K2(I) =ZERO
          C2(I) =ZERO
          ST2(I)=ZERO
         ELSE
          K2(I)=KT(I)*ABS(H2(I))
          C2(I)=C(I)*ABS(H2(I))
          CX   =FOUR*C2(I)*C2(I)
          CY   =EIGHT*MS(J1)*K2(I)
          AUX   = SQRT(CX+CY)+TWO*C2(I)
          ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
         ENDIF      
C
         J1=IX3(I)
         IF(MS(J1)==ZERO)THEN
          K3(I) =ZERO
          C3(I) =ZERO
          ST3(I)=ZERO
         ELSE
          K3(I)=KT(I)*ABS(H3(I))
          C3(I)=C(I)*ABS(H3(I))
          CX   =FOUR*C3(I)*C3(I)
          CY   =EIGHT*MS(J1)*K3(I)
          AUX   = SQRT(CX+CY)+TWO*C3(I)
          ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
         ENDIF      
C
         J1=IX4(I)
         IF(MS(J1)==ZERO)THEN
          K4(I) =ZERO
          C4(I) =ZERO
          ST4(I)=ZERO
         ELSE
          K4(I)=KT(I)*ABS(H4(I))
          C4(I)=C(I)*ABS(H4(I))
          CX   =FOUR*C4(I)*C4(I)
          CY   =EIGHT*MS(J1)*K4(I)
          AUX   = SQRT(CX+CY)+TWO*C4(I)
          ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
         ENDIF      
        ENDDO
C
       ELSE
        DO I=1,JLT
         KS(I) =STIF(I)
         CS(I) =ZERO
         STV(I)=KS(I)
         K1(I) =STIF(I)*ABS(H1(I))
         C1(I) =ZERO
         ST1(I)=K1(I)
         K2(I) =STIF(I)*ABS(H2(I))
         C2(I) =ZERO
         ST2(I)=K2(I)
         K3(I) =STIF(I)*ABS(H3(I))
         C3(I) =ZERO
         ST3(I)=K3(I)
         K4(I) =STIF(I)*ABS(H4(I))
         C4(I) =ZERO
         ST4(I)=K4(I)
        ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------------
      IF(INTTH==0)THEN
        DO I=1,JLT            
         FX1(I)=FXI(I)*H1(I)  
         FY1(I)=FYI(I)*H1(I)  
         FZ1(I)=FZI(I)*H1(I)  
C
         FX2(I)=FXI(I)*H2(I)  
         FY2(I)=FYI(I)*H2(I)  
         FZ2(I)=FZI(I)*H2(I)  
C
         FX3(I)=FXI(I)*H3(I)  
         FY3(I)=FYI(I)*H3(I)  
         FZ3(I)=FZI(I)*H3(I)  
C
         FX4(I)=FXI(I)*H4(I)  
         FY4(I)=FYI(I)*H4(I)  
         FZ4(I)=FZI(I)*H4(I)
C          
        ENDDO                
      END IF
C SPMD: Identification of interf nodes.useful to send
      IF (NSPMD>1) THEN
Ctmp+1 mic only
#include "mic_lockon.inc"
      DO I = 1,JLT
          NN = NSVG(I)
          IF(NN<0)THEN
C temporary tag of nsvfi a -
            NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
          ENDIF
        ENDDO
ctmp+1 mic only
#include "mic_lockoff.inc"
        ENDIF
C-----------------------------------------------------
      IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
        DTMINI=ZERO
        DTI=DT2T
        CALL I7SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3              NIN   ,NOINT ,MSKYI_SMS, ISKYI_SMS,NSMS  ,
     4              KT    ,C     ,CF   ,DTMINI,DTI )
      ENDIF
C
      IF(IDTMINS_INT/=0)THEN
        STIF(1:JLT)=ZERO
      END IF
C-----------------------------------------------------
      IF(IPARIT==3)THEN
       IF(KDTINT==0)THEN
        CALL I7ASS3(JLT  ,IX1  ,IX2  ,IX3  ,IX4  ,
     2             NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3             FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4             FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5             FXI  ,FYI  ,FZI  ,A    ,STIFN)
       ELSE
        CALL I7ASS35(JLT  ,IX1  ,IX2  ,IX3  ,IX4  ,
     2                  NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3                  FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4                  FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5                  FXI  ,FYI  ,FZI  ,A    ,STIFN,VISCN,
     6                  KS   ,K1   ,K2   ,K3   ,K4   ,CS   ,
     7                  C1   ,C2   ,C3   ,C4   )
       ENDIF
      ELSEIF(IPARIT==0)THEN
       IF(KDTINT==0)THEN
         CALL I7ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2               NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3               FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4               FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5               FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6               INTTH ,RBID ,RBID ,RBID ,RBID   ,RBID ,
     7               RBID  ,RBID ,RBID ,JTASK,IBID   ,IBID )

       ELSE
C
         CALL I7ASS05(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                FXI   ,FYI  ,FZI  ,A    ,STIFN  ,VISCN ,
     6                KS    ,K1   ,K2   ,K3   ,K4     ,CS    ,
     7                C1    ,C2   ,C3   ,C4   ,NIN    ,INTTH ,
     8                RBID  ,RBID ,RBID ,RBID ,RBID   ,RBID  ,
     9                JTASK ,RBID ,RBID ,IBID ,IBID   )
       ENDIF
C
      ELSE
        IF(KDTINT==0)THEN
          CALL I7ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2                NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3                FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4                FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5                FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6                NIN   ,NOINT ,INTTH,RBID  ,RBID  ,RBID  ,
     7                RBID  ,RBID  ,RBID  ,RBID ,RBID  ,
     A                IBID  ,IBID  )
        ELSE
          CALL I7ASS25(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                 NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                 FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                 FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                 FXI   ,FYI  ,FZI  ,FSKYI,NISKYFI,NIN  ,
     6                 KS    ,K1   ,K2   ,K3   ,K4     ,CS   ,
     7                 C1    ,C2   ,C3   ,C4   ,ISKY  ,NOINT ,
     8                 INTTH ,RBID ,RBID ,RBID ,RBID  ,RBID  ,
     9                 RBID  ,RBID ,RBID ,IBID ,IBID )
        ENDIF
      ENDIF
C
C-----------------------------------------------------
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0.AND.
     .          ((TT>=TANIM .AND. TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .              (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D /= 0))THEN
         IF (INCONV==1) THEN
#include "lockon.inc"
           DO I=1,JLT
            FCONT(1,IX1(I)) =FCONT(1,IX1(I)) + FX1(I)
            FCONT(2,IX1(I)) =FCONT(2,IX1(I)) + FY1(I)
            FCONT(3,IX1(I)) =FCONT(3,IX1(I)) + FZ1(I)
            FCONT(1,IX2(I)) =FCONT(1,IX2(I)) + FX2(I)
            FCONT(2,IX2(I)) =FCONT(2,IX2(I)) + FY2(I)
            FCONT(3,IX2(I)) =FCONT(3,IX2(I)) + FZ2(I)
            FCONT(1,IX3(I)) =FCONT(1,IX3(I)) + FX3(I)
            FCONT(2,IX3(I)) =FCONT(2,IX3(I)) + FY3(I)
            FCONT(3,IX3(I)) =FCONT(3,IX3(I)) + FZ3(I)
            FCONT(1,IX4(I)) =FCONT(1,IX4(I)) + FX4(I)
            FCONT(2,IX4(I)) =FCONT(2,IX4(I)) + FY4(I)
            FCONT(3,IX4(I)) =FCONT(3,IX4(I)) + FZ4(I)
            JG = NSVG(I)
            IF(JG>0) THEN
C In SPMD: Treatment to be redoen after reception node Remote if JG <0
              FCONT(1,JG)=FCONT(1,JG)- FXI(I)
              FCONT(2,JG)=FCONT(2,JG)- FYI(I)
              FCONT(3,JG)=FCONT(3,JG)- FZI(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV==1) THEN
      ENDIF
C-----------------------------------------------------
      IF(ISECIN>0.AND.INCONV==1)THEN
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2)/=0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S)==NOINT)THEN
              IF(ISECUT/=0)THEN
#include "lockon.inc"
                DO K=1,JLT
C attention to the signs for the accumulation of  forces
C To make it conform with CFORC3
                 IF(SECFCUM(4,IX1(K),I)==1.)THEN
                  SECFCUM(1,IX1(K),I)=SECFCUM(1,IX1(K),I)-FX1(K)
                  SECFCUM(2,IX1(K),I)=SECFCUM(2,IX1(K),I)-FY1(K)
                  SECFCUM(3,IX1(K),I)=SECFCUM(3,IX1(K),I)-FZ1(K)
                ENDIF
                IF(SECFCUM(4,IX2(K),I)==1.)THEN
                  SECFCUM(1,IX2(K),I)=SECFCUM(1,IX2(K),I)-FX2(K)
                  SECFCUM(2,IX2(K),I)=SECFCUM(2,IX2(K),I)-FY2(K)
                  SECFCUM(3,IX2(K),I)=SECFCUM(3,IX2(K),I)-FZ2(K)
                ENDIF
                IF(SECFCUM(4,IX3(K),I)==1.)THEN
                  SECFCUM(1,IX3(K),I)=SECFCUM(1,IX3(K),I)-FX3(K)
                  SECFCUM(2,IX3(K),I)=SECFCUM(2,IX3(K),I)-FY3(K)
                  SECFCUM(3,IX3(K),I)=SECFCUM(3,IX3(K),I)-FZ3(K)
                ENDIF
                IF(SECFCUM(4,IX4(K),I)==1.)THEN
                  SECFCUM(1,IX4(K),I)=SECFCUM(1,IX4(K),I)-FX4(K)
                  SECFCUM(2,IX4(K),I)=SECFCUM(2,IX4(K),I)-FY4(K)
                  SECFCUM(3,IX4(K),I)=SECFCUM(3,IX4(K),I)-FZ4(K)
                ENDIF
                JG = NSVG(K)
                IF(JG>0) THEN
C In SPMD: Treatment to be redone after reception node Remote if JG <0
                  IF(SECFCUM(4,JG,I)==1.)THEN
                    SECFCUM(1,JG,I)=SECFCUM(1,JG,I)+FXI(K)
                    SECFCUM(2,JG,I)=SECFCUM(2,JG,I)+FYI(K)
                    SECFCUM(3,JG,I)=SECFCUM(3,JG,I)+FZI(K)
                  ENDIF
                ENDIF
              ENDDO
#include "lockoff.inc"
             ENDIF
C +fsav(section)
           ENDIF
           K1S=K1S+1
        ENDDO
        K0=NSTRF(K0+24)
       ENDDO
       ENDIF
      ENDIF
C-----------------------------------------------------
      IF((IBAG/=0).OR.(IDAMP_RDOF/=0)) THEN
       DO I=1,JLT
C       IF(PENE(I)/=ZERO)THEN
C modified test for coherence with SPMD communication (spmd_i7tools)
        IF(FXI(I)/=ZERO.OR.FYI(I)/=ZERO.OR.FZI(I)/=ZERO)THEN
          JG = NSVG(I)
          IF(JG>0) THEN
C In SPMD: Treatment to be redone after reception node Remote if JG <0
            ICONTACT(JG)=1
          ENDIF
          ICONTACT(IX1(I))=1
          ICONTACT(IX2(I))=1
          ICONTACT(IX3(I))=1
          ICONTACT(IX4(I))=1
        ENDIF
       ENDDO
      ENDIF
C
      IF(IBC==0) RETURN
C
      DO 400 I=1,JLT
        IF(PENE(I)==ZERO)GOTO 400
        IBCM = IBC / 8
        IBCS = IBC - 8 * IBCM
        IF(IBCS>0) THEN
          IG=NSVG(I)
          IF(IG>0) THEN
C In SPMD: Treatment to be redone after reception node Remote if JG <0
            CALL IBCOFF(IBCS,ICODT(IG))
          ENDIF
        ENDIF
        IF(IBCM>0) THEN
         IG=IX1(I)
         CALL IBCOFF(IBCM,ICODT(IG))
         IG=IX2(I)
         CALL IBCOFF(IBCM,ICODT(IG))
         IG=IX3(I)
         CALL IBCOFF(IBCM,ICODT(IG))
         IG=IX4(I)
         CALL IBCOFF(IBCM,ICODT(IG))
        ENDIF
 400  CONTINUE
C
      RETURN
      END
