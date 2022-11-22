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
Chd|  I10FOR3                       source/interfaces/int10/i10for3.F
Chd|-- called by -----------
Chd|        I10MAINF                      source/interfaces/int10/i10mainf.F
Chd|-- calls ---------------
Chd|        I10SMS2                       source/interfaces/int10/i10sms2.F
Chd|        I7ASS0                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS05                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS25                       source/interfaces/int07/i7ass3.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I10FOR3(
     1    JLT    ,A      ,MS     ,V     ,FSAV  ,
     2    CAND_F ,STIFN  ,STIF   ,FSKYI ,ISKY  ,
     3    ITIED  ,VISC   ,X1     ,X2    ,X3    ,
     4    X4     ,Y1     ,Y2     ,Y3    ,Y4    ,
     5    Z1     ,Z2     ,Z3     ,Z4    ,NSVG  ,
     6    NX1    ,NX2    ,NX3    ,NX4   ,NY1   ,
     7    NY2    ,NY3    ,NY4    ,NZ1   ,NZ2   ,
     8    NZ3    ,NZ4    ,LB1    ,LB2   ,LB3   ,
     9    LB4    ,LC1    ,LC2    ,LC3   ,LC4   ,
     A    P1     ,P2     ,P3     ,P4    ,FCONT ,
     B    IX1    ,IX2    ,IX3    ,IX4   ,GAPV  ,
     C    INDEX  ,NISKYFI,ISECIN,NSTRF ,SECFCUM,
     D    NOINT  ,VISCN  ,VXI    ,VYI   ,VZI   ,
     E    MSI    ,NIN    ,NISUB  ,LISUB ,ADDSUBS,
     F    ADDSUBM,LISUBS ,LISUBM ,CN_LOC,CE_LOC,
     G    FSAVSUB,FNCONT ,FTCONT ,MSKYI_SMS ,ISKYI_SMS ,
     H    NSMS   ,XI     ,YI     ,ZI    ,ICONTACT,
     I    DT2T   ,NELTST ,ITYPTST,JTASK ,ISENSINT,
     J    FSAVPARIT,NFT  ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE H3D_MOD
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
#include      "com08_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,ITIED, NISKYFI,NIN,NELTST,ITYPTST,JTASK
      INTEGER ISKY(*),ISECIN, NSTRF(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), NSMS(MVSIZ), INDEX(*),
     .   NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*),
     .   LISUBM(*),CN_LOC(*),CE_LOC(*),ISKYI_SMS(*),ICONTACT(*),
     .   ISENSINT(*),NFT
      my_real
     .   A(3,*), MS(*), FSAV(*),X1(*),X2(*),X3(*),X4(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   VISC,STIFN(*),CAND_F(6,*), V(3,*),FSKYI(LSKYI,NFSKYI),
     .   FCONT(3,*),
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .   P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .   GAPV(MVSIZ),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*),FSAVSUB(NTHVKI,*),
     .   FNCONT(3,*), FTCONT(3,*), MSKYI_SMS(*),
     .     XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),DT2T,
     .   FSAVPARIT(NISUB+1,11,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, II , K0,NBINTER,K1S,K,J,NN,JG
      INTEGER NISKYL,NISKYL1,NOINT
      INTEGER JSUB,KSUB,JJ,KK,IN,IE,NSUB,IBID
      my_real
     .   FSAVSUB1(24,NISUB),IMPX,IMPY,IMPZ
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   FT1(MVSIZ), FT2(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ), 
     .   VT1(MVSIZ), VT2(MVSIZ), 
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   T1X(MVSIZ),T1Y(MVSIZ),T1Z(MVSIZ),
     .   T2X(MVSIZ),T2Y(MVSIZ),T2Z(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ),
     .   S2,D1,D2,D3,D4,A1,A2,A3,A4,LA1,LA2,LA3,LA4,H0,
     .   ECONTT, DT05, NORMINV, VIS, DT1INV,ECONTDT,
     .   FSAV1, FSAV2, FSAV3,FSAV4 , FSAV5, FSAV6, FSAV7, FSAV8,
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15,
     .   FSAV22, FSAV23, FSAV24
      my_real
     .   C(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   FXN(MVSIZ), FYN(MVSIZ), FZN(MVSIZ), 
     .   FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ),BID ,DTI
C-----------------------------------------------
#include "sync_dir.inc"
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONTDT = ZERO
C
      DT05 = HALF * DT1
      IBID = 0
      BID = ZERO
C-----------------------------------------------
C     CALCUL DE LA PENE REELLE
C-----------------------------------------------
      DO I=1,JLT
C
        D1 = SQRT(P1(I))
        P1(I) = MAX(ZERO, GAPV(I) - D1)
C
        D2 = SQRT(P2(I))
        P2(I) = MAX(ZERO, GAPV(I) - D2)
C
        D3 = SQRT(P3(I))
        P3(I) = MAX(ZERO, GAPV(I) - D3)
C
        D4 = SQRT(P4(I))
        P4(I) = MAX(ZERO, GAPV(I) - D4)
C
        A1 = P1(I)/MAX(EM20,D1)
        A2 = P2(I)/MAX(EM20,D2)
        A3 = P3(I)/MAX(EM20,D3)
        A4 = P4(I)/MAX(EM20,D4)
        N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I) 
        N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I) 
        N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I) 
      ENDDO
C
      DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
         LA1 = ONE - LB1(I) - LC1(I)
         LA2 = ONE - LB2(I) - LC2(I)
         LA3 = ONE - LB3(I) - LC3(I)
         LA4 = ONE - LB4(I) - LC4(I) 
C
          H0    = FOURTH *
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
C
         H0    = ONE/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H0
         H2(I) = H2(I) * H0
         H3(I) = H3(I) * H0
         H4(I) = H4(I) * H0
C
        ELSE
         PENE(I) = P1(I)
         N1(I) = NX1(I)
         N2(I) = NY1(I)
         N3(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = ONE - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
      ENDDO
C
      DO I=1,JLT
         S2 = ONE/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
         N1(I) = N1(I)*S2
         N2(I) = N2(I)*S2
         N3(I) = N3(I)*S2
      ENDDO
C
CC    VMAX = 0.
      DO I=1,JLT
C       correction hourglass
        IF(IX3(I)/=IX4(I))THEN
          H0 = -FOURTH*(H1(I) - H2(I) + H3(I) - H4(I))
          H0 = MIN(H0,H2(I),H4(I))
          H0 = MAX(H0,-H1(I),-H3(I))
          H1(I) = H1(I) + H0
          H2(I) = H2(I) - H0
          H3(I) = H3(I) + H0
          H4(I) = H4(I) - H0
        ENDIF
      ENDDO
C-----------------------------------------------
      DO 5 I=1,JLT
       II = INDEX(I)
       IF(CAND_F(1,II)==ZERO)THEN
C------------------------------------
C       1ER IMPACT ou PAS d'IMPACT
C------------------------------------
        CAND_F(4,II) = H1(I)
        CAND_F(5,II) = H2(I)
        CAND_F(6,II) = H3(I)
       ELSE
C------------------------------------
C       IMPACTS SUIVANTS 
C------------------------------------
        H1(I) = CAND_F(4,II)
        H2(I) = CAND_F(5,II)
        H3(I) = CAND_F(6,II)
        H4(I) = ONE - H1(I) - H2(I) - H3(I)
       ENDIF
 5    CONTINUE
C
      DO 10 I=1,JLT
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                 - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                 - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                 - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
 10   CONTINUE
C
C
      DO 20 I=1,JLT
        T1X(I) = X3(I) - X1(I)
        T1Y(I) = Y3(I) - Y1(I)
        T1Z(I) = Z3(I) - Z1(I)
        NORMINV = ONE/SQRT(T1X(I)**2+T1Y(I)**2+T1Z(I)**2)
        T1X(I) = T1X(I)*NORMINV
        T1Y(I) = T1Y(I)*NORMINV
        T1Z(I) = T1Z(I)*NORMINV
C
        T2X(I) = X4(I) - X2(I)
        T2Y(I) = Y4(I) - Y2(I)
        T2Z(I) = Z4(I) - Z2(I)
C
        NX(I) = T1Y(I)*T2Z(I) - T1Z(I)*T2Y(I)
        NY(I) = T1Z(I)*T2X(I) - T1X(I)*T2Z(I)
        NZ(I) = T1X(I)*T2Y(I) - T1Y(I)*T2X(I)
        NORMINV = ONE/SQRT(NX(I)**2+NY(I)**2+NZ(I)**2)
        NX(I) = NX(I)*NORMINV
        NY(I) = NY(I)*NORMINV
        NZ(I) = NZ(I)*NORMINV
C
        T2X(I) = NY(I)*T1Z(I) - NZ(I)*T1Y(I)
        T2Y(I) = NZ(I)*T1X(I) - NX(I)*T1Z(I)
        T2Z(I) = NX(I)*T1Y(I) - NY(I)*T1X(I)
C
        VN(I)  = VX(I)*NX(I)  + VY(I)*NY(I)  + VZ(I)*NZ(I)  
        VT1(I) = VX(I)*T1X(I) + VY(I)*T1Y(I) + VZ(I)*T1Z(I)  
        VT2(I) = VX(I)*T2X(I) + VY(I)*T2Y(I) + VZ(I)*T2Z(I)  
 20   CONTINUE
C
      DO 25 I=1,JLT
      IF(PENE(I)==ZERO.AND.CAND_F(1,INDEX(I))==ZERO)THEN
C------------------------------------
C       PAS ENCORE D'IMPACT OU REBOND
C------------------------------------
        VN(I)  = ZERO
        VT1(I) = ZERO
        VT2(I) = ZERO
      ENDIF
 25   CONTINUE
C
      DO 40 I=1,JLT
        II = INDEX(I)
        ECONTT = ECONTT + CAND_F(1,II) * VN(I)  * DT05
        ECONTT = ECONTT + CAND_F(2,II) * VT1(I) * DT05
        ECONTT = ECONTT + CAND_F(3,II) * VT2(I) * DT05
        FNI(I) = CAND_F(1,II) + VN(I)  * DT1 * STIF(I)
        FT1(I) = CAND_F(2,II) + VT1(I) * DT1 * STIF(I)
        FT2(I) = CAND_F(3,II) + VT2(I) * DT1 * STIF(I)
 40   CONTINUE
C
      DO 100 I=1,JLT
      IF(ITIED==0)THEN
          IF(CAND_F(1,INDEX(I))*FNI(I)<ZERO)THEN
C------------------------------------
C           REBOND
C------------------------------------
            IF(PENE(I)==ZERO)THEN
Cel on ne devrait plus passer par ici (teste dans i10dst3)
              CAND_F(1,INDEX(I)) =ZERO
            ELSE
              CAND_F(1,INDEX(I)) = SIGN(EM30,CAND_F(1,INDEX(I)))
            ENDIF
            FNI(I) = ZERO
            FT1(I) = ZERO
            FT2(I) = ZERO
            VN(I)  = ZERO
            VT1(I) = ZERO
            VT2(I) = ZERO    
          ELSE
            IF (INCONV==1) CAND_F(1,INDEX(I)) = FNI(I)
          ENDIF
      ELSEIF(FNI(I)==ZERO.AND.PENE(I)/=ZERO)THEN
            CAND_F(1,INDEX(I)) = EM30
      ELSE
            IF (INCONV==1) CAND_F(1,INDEX(I)) = FNI(I)
      ENDIF
      IF (INCONV==1) THEN
      CAND_F(2,INDEX(I)) = FT1(I)
      CAND_F(3,INDEX(I)) = FT2(I)
      ENDIF 
C    
 100  CONTINUE
C
      IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
       DO 120 I=1,JLT
        VIS = VISC * SQRT(TWO * STIF(I) * MSI(I)) 
        FNI(I) = FNI(I) + VN(I)  * VIS
        FT1(I) = FT1(I) + VT1(I) * VIS
        FT2(I) = FT2(I) + VT2(I) * VIS
        STIF(I) = STIF(I) + TWO * VIS * DT1INV
C       no second order correction :
        ECONTDT = ECONTDT 
     .          + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
 120   CONTINUE
      ELSE
       DO I=1,JLT
        VIS = VISC * SQRT(TWO * STIF(I) * MSI(I)) 
        FNI(I) = FNI(I) + VN(I)  * VIS
        FT1(I) = FT1(I) + VT1(I) * VIS
        FT2(I) = FT2(I) + VT2(I) * VIS
C   2*C dans modsti
        C(I) = VIS
C       no second order correction :
        ECONTDT = ECONTDT 
     .          + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
       ENDDO
      ENDIF
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION TOTALE
C---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV4 = ZERO
      FSAV5 = ZERO
      FSAV6 = ZERO    
      FSAV8 = ZERO
      FSAV9 = ZERO
      FSAV10= ZERO
      FSAV11= ZERO
      FSAV12= ZERO
      FSAV13= ZERO
      FSAV14= ZERO
      FSAV15= ZERO
      FSAV22= ZERO
      FSAV23= ZERO
      FSAV24= ZERO
      DO I=1,JLT
        II = INDEX(I)
        ECONTT = ECONTT + CAND_F(1,II) * VN(I)  * DT05
        ECONTT = ECONTT + CAND_F(2,II) * VT1(I) * DT05
        ECONTT = ECONTT + CAND_F(3,II) * VT2(I) * DT05
        FXN(I)= NX(I)*FNI(I)
        FYN(I)= NY(I)*FNI(I)
        FZN(I)= NZ(I)*FNI(I)
        FXT(I)= T1X(I)*FT1(I) + T2X(I)*FT2(I)
        FYT(I)= T1Y(I)*FT1(I) + T2Y(I)*FT2(I)
        FZT(I)= T1Z(I)*FT1(I) + T2Z(I)*FT2(I)
        IMPX=FXN(I)*DT12
        IMPY=FYN(I)*DT12
        IMPZ=FZN(I)*DT12
        FSAV1=FSAV1+IMPX
        FSAV2=FSAV2+IMPY
        FSAV3=FSAV3+IMPZ
        FSAV8 =FSAV8 +ABS(IMPX)
        FSAV9 =FSAV9 +ABS(IMPY)
        FSAV10=FSAV10+ABS(IMPZ)
        FSAV11=FSAV11+FNI(I)*DT12
        IMPX=FXT(I)*DT12
        IMPY=FYT(I)*DT12
        IMPZ=FZT(I)*DT12
        FSAV4=FSAV4+IMPX
        FSAV5=FSAV5+IMPY
        FSAV6=FSAV6+IMPZ
        FSAV12=FSAV12+ABS(IMPX)
        FSAV13=FSAV13+ABS(IMPY)
        FSAV14=FSAV14+ABS(IMPZ)
        FSAV15=FSAV15+SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
        FXI(I) = FXN(I) + FXT(I)
        FYI(I) = FYN(I) + FYT(I)
        FZI(I) = FZN(I) + FZT(I) 
        IMPX=FXI(I)*DT12
        IMPY=FYI(I)*DT12
        IMPZ=FZI(I)*DT12
        XP(I)=XI(I)+PENE(I)*N1(I)
        YP(I)=YI(I)+PENE(I)*N2(I)
        ZP(I)=ZI(I)+PENE(I)*N3(I)
        FSAV22=FSAV22+YP(I)*IMPZ-ZP(I)*IMPY
        FSAV23=FSAV23+ZP(I)*IMPX-XP(I)*IMPZ
        FSAV24=FSAV24+XP(I)*IMPY-YP(I)*IMPX
      ENDDO
      IF(ISENSINT(1)/=0) THEN
	DO I=1,JLT
          FSAVPARIT(1,1,I+NFT) =  FXN(I)
          FSAVPARIT(1,2,I+NFT) =  FYN(I)
          FSAVPARIT(1,3,I+NFT) =  FZN(I)
          FSAVPARIT(1,4,I+NFT) =  FXT(I)
          FSAVPARIT(1,5,I+NFT) =  FYT(I)
          FSAVPARIT(1,6,I+NFT) =  FZT(I)
	ENDDO
      ENDIF
C---------------------------------
       IF (INCONV==1) THEN
#include "lockon.inc"
        FSAV(1)=FSAV(1)+FSAV1
        FSAV(2)=FSAV(2)+FSAV2
        FSAV(3)=FSAV(3)+FSAV3
        FSAV(4)=FSAV(4)+FSAV4
        FSAV(5)=FSAV(5)+FSAV5
        FSAV(6)=FSAV(6)+FSAV6
        FSAV(8)  = FSAV(8)  +FSAV8
        FSAV(9)  = FSAV(9)  +FSAV9
        FSAV(10) = FSAV(10) +FSAV10
        FSAV(11) = FSAV(11) +FSAV11
        FSAV(12) = FSAV(12) + FSAV12
        FSAV(13) = FSAV(13) + FSAV13
        FSAV(14) = FSAV(14) + FSAV14
        FSAV(15) = FSAV(15) + FSAV15
        FSAV(22) = FSAV(22) + FSAV22
        FSAV(23) = FSAV(23) + FSAV23
        FSAV(24) = FSAV(24) + FSAV24
        ECONT_CUMU  = ECONT_CUMU  + ECONTT ! Elastic energy for tied contact 10 : it is cumulated energy
        ECONTD = ECONTD + ECONTDT ! Damping contact energy
        FSAV(26) = FSAV(26) + ECONTT
        FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
       ENDIF 
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB/=0)THEN
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN>0)THEN
           IN=CN_LOC(I)
           IE=CE_LOC(I)
           JJ  =ADDSUBS(IN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ<ADDSUBS(IN+1))
             JSUB=LISUBS(JJ)
             DO WHILE(KK<ADDSUBM(IE+1))
               KSUB=LISUBM(KK)   
               IF(KSUB==JSUB)THEN
                 IMPX=FXN(I)*DT12
                 IMPY=FYN(I)*DT12
                 IMPZ=FZN(I)*DT12
C                main side :
                 FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                 FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                 FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                 IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) =  FXN(I)
                   FSAVPARIT(JSUB+1,2,I+NFT) =  FYN(I)
                   FSAVPARIT(JSUB+1,3,I+NFT) =  FZN(I)
                 ENDIF
C
                 FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                 FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                 FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
                 FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
C
                 IMPX=FXT(I)*DT12
                 IMPY=FYT(I)*DT12
                 IMPZ=FZT(I)*DT12
C                main side :
                 FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                 FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                 FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
                 IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                   FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                   FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                 ENDIF
C
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
                 FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                 FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                 FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
                 FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                           +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
        	 FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                           +YP(I)*IMPZ-ZP(I)*IMPY
        	 FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
                 FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX
                 KK=KK+1
                 GO TO 200             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 GO TO 200
               END IF
             END DO
 200       CONTINUE
           JJ=JJ+1
           END DO
          END IF
         END DO
        IF(NSPMD>1) THEN
C boucle separee sinon bug PGI en SMP
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN<0)THEN
           NN = -NN
           IE=CE_LOC(I)
           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ<ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             DO WHILE(KK<ADDSUBM(IE+1))
               KSUB=LISUBM(KK)   
               IF(KSUB==JSUB)THEN
                 IMPX=FXN(I)*DT12
                 IMPY=FYN(I)*DT12
                 IMPZ=FZN(I)*DT12
C                main side :
                 FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                 FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                 FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                 FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                 FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                 FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
                 FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
C
                 IMPX=FXT(I)*DT12
                 IMPY=FYT(I)*DT12
                 IMPZ=FZT(I)*DT12
C                main side :
                 FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                 FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                 FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
                 FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                 FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                 FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
                 FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                           +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
        	 FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                           +YP(I)*IMPZ-ZP(I)*IMPY
        	 FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
                 FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX
                 KK=KK+1
                 GO TO 250             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 GO TO 250
               END IF
             END DO
 250       CONTINUE
           JJ=JJ+1
           END DO
          END IF
         END DO 
        END IF 
#include "lockon.inc"
         DO JSUB=1,NISUB
           NSUB=LISUB(JSUB)
           DO J=1,24
             FSAVSUB(J,NSUB)=FSAVSUB(J,NSUB)+FSAVSUB1(J,JSUB)
           END DO
         END DO
#include "lockoff.inc"
      END IF
C---------------------------------
      DO 160 I=1,JLT
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
 160  CONTINUE
C
Cel spmd : identification des noeuds interf. utiles a envoyer (exchange_a)
      IF (NSPMD>1) THEN
Ctmp+1 mic only to avoid compiler bug
#include "mic_lockon.inc"
        DO I = 1,JLT
          NN = NSVG(I)
          IF(NN<0)THEN
C tag temporaire de NSVFI a -
            NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
          ENDIF
        ENDDO
ctmp+1 mic only
#include "mic_lockoff.inc"
      ENDIF
C
      IF(KDTINT/=0)THEN
        DO I=1,JLT
          K1(I) =STIF(I)*ABS(H1(I))
          C1(I) =C(I)*ABS(H1(I))
          K2(I) =STIF(I)*ABS(H2(I))
          C2(I) =C(I)*ABS(H2(I))
          K3(I) =STIF(I)*ABS(H3(I))
          C3(I) =C(I)*ABS(H3(I))
          K4(I) =STIF(I)*ABS(H4(I))
          C4(I) =C(I)*ABS(H4(I))
        ENDDO
      END IF
C
      IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
        DTI=DT2T
        CALL I10SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3              NIN   ,NOINT ,MSKYI_SMS, ISKYI_SMS,NSMS  ,
     4              STIF  ,C     ,DTI  )
        IF(DTI<DT2T)THEN
          DT2T    = DTI
          NELTST  = NOINT
          ITYPTST = 10
        ENDIF
      ENDIF
C
      IF(IDTMINS_INT/=0)THEN
        STIF(1:JLT)=ZERO
      END IF
C
      IF(IPARIT==0)THEN
        IF(KDTINT==0)THEN
          CALL I7ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6                IBID  ,BID  ,BID  ,BID  ,BID    ,BID  ,
     7                BID   ,BID  ,BID  ,JTASK,IBID ) 
        ELSE
          CALL I7ASS05(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                 NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                 FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                 FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                 FXI   ,FYI  ,FZI  ,A    ,STIFN  ,VISCN,
     6                 STIF  ,K1   ,K2   ,K3   ,K4     ,C    ,
     7                 C1    ,C2   ,C3   ,C4   ,NIN    ,IBID ,
     8                 BID   ,BID  ,BID  ,BID  ,BID    ,BID,
     9                 JTASK ,BID  ,BID  ,IBID )
        END IF
      ELSE
        IF(KDTINT==0)THEN
          CALL I7ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2                NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3                FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4                FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5                FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6                NIN   ,NOINT ,IBID ,BID  ,BID  ,BID    ,
     7                BID   ,BID   ,BID  ,BID  ,BID  ,
     A                IBID)
        ELSE
          CALL I7ASS25(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                 NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                 FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                 FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                 FXI   ,FYI  ,FZI  ,FSKYI,NISKYFI,NIN  ,
     6                 STIF  ,K1   ,K2   ,K3     ,K4   ,C    ,
     7                 C1    ,C2   ,C3   ,C4     ,ISKY ,NOINT,
     8                 IBID  ,BID  ,BID  ,BID    ,BID  ,BID  ,
     9                 BID   ,BID  ,BID  ,IBID )
        END IF
      END IF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0)THEN
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
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              FCONT(1,JG)=FCONT(1,JG)- FXI(I)
              FCONT(2,JG)=FCONT(2,JG)- FYI(I)
              FCONT(3,JG)=FCONT(3,JG)- FZI(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV==1) THEN
      ENDIF
C---------------------------------
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .              (MANIM>=4.AND.MANIM<=15).OR. H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
         IF (INCONV==1) THEN
#include "lockon.inc"
           DO I=1,JLT
            FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FX1(I)
            FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FY1(I)
            FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZ1(I)
            FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FX2(I)
            FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FY2(I)
            FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZ2(I)
            FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FX3(I)
            FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FY3(I)
            FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZ3(I)
            FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FX4(I)
            FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FY4(I)
            FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZ4(I)
            JG = NSVG(I)
            IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              FNCONT(1,JG)=FNCONT(1,JG)- FXI(I)
              FNCONT(2,JG)=FNCONT(2,JG)- FYI(I)
              FNCONT(3,JG)=FNCONT(3,JG)- FZI(I)
            ELSE ! cas noeud remote en SPMD
              JG = -JG
              FNCONTI(NIN)%P(1,JG)=FNCONTI(NIN)%P(1,JG)-FXI(I)
              FNCONTI(NIN)%P(2,JG)=FNCONTI(NIN)%P(2,JG)-FYI(I)
              FNCONTI(NIN)%P(3,JG)=FNCONTI(NIN)%P(3,JG)-FZI(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV==1) THEN
      ENDIF
C
C---------------------------------
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
C   attention aux signes pour le cumul des efforts
C   a rendre conforme avec CFORC3
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
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
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
      IF (IDAMP_RDOF/=0) THEN
       DO I=1,JLT
C       IF(PENE(I)/=ZERO)THEN
C test modifie pour coherence avec communication SPMD (spmd_i7tools)
        IF(FXI(I)/=ZERO.OR.FYI(I)/=ZERO.OR.FZI(I)/=ZERO)THEN
          JG = NSVG(I)
          IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
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
      RETURN
      END
