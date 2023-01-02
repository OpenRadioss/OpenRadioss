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
Chd|  I20FOR3                       source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        I7ASS0                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS05                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS2                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS25                       source/interfaces/int07/i7ass3.F
Chd|        I7ASS3                        source/interfaces/int07/i7ass3.F
Chd|        I7ASS35                       source/interfaces/int07/i7ass3.F
Chd|        I7CURV                        source/interfaces/int07/i7curv.F
Chd|        I7SMS2                        source/interfaces/int07/i7sms2.F
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20FOR3(JLT   ,A      ,VA     ,IBCC  ,ICODT  ,
     2                  FSAV   ,GAP    ,FRIC   ,MS    ,VISC   ,
     3                  VISCF  ,NOINT  ,STFA   ,ITAB  ,CN_LOC ,
     4                  STIGLO ,STIFN  ,STIF   ,FSKYI ,ISKY   ,
     6                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     7                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     8                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     9                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     A                  P1     ,P2     ,P3     ,P4    ,FCONT  ,
     B                  IX1L   ,IX2L   ,IX3L   ,IX4L  ,NSVG   ,
     C                  IVIS2  ,NELTST ,ITYPTST,DT2T  ,
     D                  GAPV   ,INACTI ,INDEX  ,NISKYFI,
     E                  KINET  ,NEWFRONT,ISECIN,NSTRF ,SECFCUM,
     F                  X      ,XA     ,CE_LOC ,MFROT ,IFQ    ,
     G                  FROT_P ,CAND_FX,CAND_FY,CAND_FZ,ALPHA0,
     H                  IFPEN  ,GAPR   ,DXANC  ,NLN   ,NLG    ,
     I                  IBAG  ,ICONTACT,NSV    ,PENIS ,PENIM  ,
     J                  VISCN  ,VXI    ,VYI    ,VZI   ,MSI    ,
     K                  KINI   ,NIN    ,NISUB  ,LISUB ,ADDSUBS,
     L                  ADDSUBM,LISUBS ,LISUBM ,FSAVSUB,CAND_N ,
     M                  ILAGM  ,ICURV  ,NOD_NORMAL ,FNCONT ,FTCONT ,
     N                  X1     ,X2     ,X3     ,X4    ,Y1    ,
     O                  Y2     ,Y3     ,Y4     ,Z1    ,Z2    ,
     P                  Z3     ,Z4     ,XI     ,YI    ,ZI    ,
     Q                  IADM   ,RCURVI ,RCONTACT,ACONTACT,PCONTACT,
     R                  ANGLMI ,PADM   ,INTTH   ,PHI   , FTHE     ,
     S                  FTHESKYI,DAANC6,TEMP   ,TEMPI  ,RSTIF  ,
     T                  IFORM  ,GAP_S  ,IGAP   ,ALPHAK ,MSKYI_SMS,
     U                  ISKYI_SMS,NSMS ,CMAJ   ,JTASK,ISENSINT,
     V                  FSAVPARIT      ,NFT    ,H3D_DATA)
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,IBCC,IBCM,IBCS,IVIS2,INACTI,IBAG,NIN,
     .        NTY ,NLN,NLG(NLN),NSV(*),
     .        ICODT(*), ITAB(*), ISKY(*), KINET(*),
     .        MFROT, IFQ, NOINT,NEWFRONT,ISECIN, NSTRF(*),
     .        IFPEN(*) ,ICONTACT(*), CAND_N(*),
     .        KINI(*),
     .        ISET, NISKYFI,IADM,INTTH,IFORM, IGAP,JTASK
      INTEGER IX1L(MVSIZ), IX2L(MVSIZ), IX3L(MVSIZ), IX4L(MVSIZ),
     .        CN_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ),
     .        NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*),
     .        LISUBM(*),ILAGM,ICURV(3), 
     .        ISKYI_SMS(*), NSMS(*), ISENSINT(*),NFT
      my_real
     .   STIGLO,FROT_P(*), X(3,*), XA(3,*),DXANC(3,*),
     .   A(3,*), MS(*), VA(3,*), FSAV(*),FCONT(3,*), 
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),ALPHA0,
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T,STFA(*),STIFN(*),
     .   FSKYI(LSKYI,NFSKYI),FSAVSUB(NTHVKI,*), FNCONT(3,*),FTCONT(3,*),
     .   MSKYI_SMS(*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),GAPR(MVSIZ),SECFCUM(7,NUMNOD,NSECT),
     .     TMP(MVSIZ),STIFSAV(MVSIZ), VISCN(*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .     X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .     X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .     X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .     XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),PENIS(2,*),PENIM(2,*),
     .     PHI(MVSIZ), FTHE(*),FTHESKYI(LSKYI),TEMP(*), TEMPI(MVSIZ),
     .     RSTIF,FSAVPARIT(NISUB+1,11,*)
      my_real
     .     NOD_NORMAL(3,*), RCURVI(*), RCONTACT(*), ACONTACT(*),
     .     PCONTACT(*),PADM, ANGLMI(*),GAP_S(*),ALPHAK(3,*),CMAJ(MVSIZ)
      DOUBLE PRECISION
     .     DAANC6(3,6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J1,IG,J,JG,IM,IS,K0,NBINTER,K1S,K,IL,IE,NN,NI,NA1,NA2,
     .        JSUB,KSUB,JJ,KK,IN,NSUB,ISIGN,IPROJ,IBID
      INTEGER IX1G(MVSIZ), IX2G(MVSIZ), IX3G(MVSIZ), IX4G(MVSIZ)
      my_real
     .   FXR(MVSIZ), FYR(MVSIZ), FZR(MVSIZ),
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),
     .   FSAVSUB1(15,NISUB),MASM(MVSIZ)
      my_real
     .   VNX, VNY, VNZ, AA, CRIT,S2,DIST,RDIST,
     .   V2, FM2, DT1INV, VISCA, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,E10, H0D, S2D, SUM,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8, 
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15, FFO,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA,STFR,VISR,   
     .   PREC,PS,XSA,PIS,PPLUS,CX,CY,CFI,AUX,TM,TS,IMPX,IMPY,IMPZ,BB,
     .   NN1,NN2,NN3,NN4,XN1,YN1,ZN1,XN2,YN2,ZN2,XN3,YN3,ZN3,XN4,YN4,
     .   ZN4,DTMINI,BID
C
      DOUBLE PRECISION FX6(6,MVSIZ), FY6(6,MVSIZ), FZ6(6,MVSIZ)
C
C-----------------------------------------------
#include "sync_dir.inc"
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
      ECONTT = ZERO
      ECONVT = ZERO
      DO I=1,JLT
        STIF0(I) = STIF(I)
        IX1G(I) = NLG(IX1L(I))
        IX2G(I) = NLG(IX2L(I))
        IX3G(I) = NLG(IX3L(I))
        IX4G(I) = NLG(IX4L(I))
      ENDDO
C--------------------------------------------------------
C  UNIQUEMENT POUR PAQUET DE QUADRANGLE
C--------------------------------------------------------
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
       IF(ICURV(1) == 3) THEN
         DO I=1,JLT
C
          BB = GAPV(I)+CMAJ(I)
C
          D1 = SQRT(P1(I))
          P1(I) = MAX(ZERO, BB - D1)
C
          D2 = SQRT(P2(I))
          P2(I) = MAX(ZERO, BB - D2)
C
          D3 = SQRT(P3(I))
          P3(I) = MAX(ZERO, BB - D3)
C
          D4 = SQRT(P4(I))
          P4(I) = MAX(ZERO, BB - D4)
C
          A1 = P1(I)/MAX(EM20,D1)
          A2 = P2(I)/MAX(EM20,D2)
          A3 = P3(I)/MAX(EM20,D3)
          A4 = P4(I)/MAX(EM20,D4)
          N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I) 
          N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I) 
          N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I) 
         ENDDO
       ELSE
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
       ENDIF
C
       DO I=1,JLT
        IF(IX3G(I)/=IX4G(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))

         LA1 = ONE - LB1(I) - LC1(I)
         LA2 = ONE - LB2(I) - LC2(I)
         LA3 = ONE - LB3(I) - LC3(I)
         LA4 = ONE - LB4(I) - LC4(I) 

         H0    = FOURTH * 
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)

         H00    = ONE/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H00
         H2(I) = H2(I) * H00
         H3(I) = H3(I) * H00
         H4(I) = H4(I) * H00
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
C---------------------
C     COURBURE FIXE
C---------------------
      IF(ICURV(1)==1)THEN
C       spherique (que concave pour le moment)
        NA1 = ICURV(2)
        DO I=1,JLT
          RR = 1.E30
          A0X = XA(1,NA1)
          A0Y = XA(2,NA1)
          A0Z = XA(3,NA1)
C
          RX = X1(I)-A0X
          RY = Y1(I)-A0Y
          RZ = Z1(I)-A0Z
          RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
          RX = X2(I)-A0X
          RY = Y2(I)-A0Y
          RZ = Z2(I)-A0Z
          RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
          RX = X3(I)-A0X
          RY = Y3(I)-A0Y
          RZ = Z3(I)-A0Z
          RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
          IF(IX3G(I)/=IX4G(I))THEN
            RX = X4(I)-A0X
            RY = Y4(I)-A0Y
            RZ = Z4(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
          ENDIF
          RX = XI(I)-A0X
          RY = YI(I)-A0Y
          RZ = ZI(I)-A0Z
          RS = SQRT(RX*RX + RY*RY + RZ*RZ)
          RR = SQRT(RR)
          IF(RS-RR+GAPV(I)<0.)THEN
            STIF(I) = 0.
            PENE(I) = 0.
          ELSEIF(RS-RR+GAPV(I)<PENE(I))THEN
            PENE(I) = RS-RR+GAPV(I)
          ENDIF
          N1(I) = -RX 
          N2(I) = -RY 
          N3(I) = -RZ 
        ENDDO
      ELSEIF(ICURV(1)==2)THEN
C       cylindrique (que concave pour le moment)
        NA1 = ICURV(2)
        NA2 = ICURV(3)
        DO I=1,JLT
          RR = 1.E30
          A0X = XA(1,NA1)
          A0Y = XA(2,NA1)
          A0Z = XA(3,NA1)
          ANX = XA(1,NA2)-A0X
          ANY = XA(2,NA2)-A0Y
          ANZ = XA(3,NA2)-A0Z
          AAN = 1. / (ANX*ANX + ANY*ANY + ANZ*ANZ) 

          AAX = X1(I)-A0X
          AAY = Y1(I)-A0Y
          AAZ = Z1(I)-A0Z
          AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
          RX = AAX - AAA * ANX 
          RY = AAY - AAA * ANY 
          RZ = AAZ - AAA * ANZ 
          RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)

          AAX = X2(I)-A0X
          AAY = Y2(I)-A0Y
          AAZ = Z2(I)-A0Z
          AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
          RX = AAX - AAA * ANX 
          RY = AAY - AAA * ANY 
          RZ = AAZ - AAA * ANZ 
          RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)

          AAX = X3(I)-A0X
          AAY = Y3(I)-A0Y
          AAZ = Z3(I)-A0Z
          AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
          RX = AAX - AAA * ANX 
          RY = AAY - AAA * ANY 
          RZ = AAZ - AAA * ANZ 
          RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
          IF(IX3G(I)/=IX4G(I))THEN

            AAX = X4(I)-A0X
            AAY = Y4(I)-A0Y
            AAZ = Z4(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX 
            RY = AAY - AAA * ANY 
            RZ = AAZ - AAA * ANZ 
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
          ENDIF
          AAX = XI(I)-A0X
          AAY = YI(I)-A0Y
          AAZ = ZI(I)-A0Z

          AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
          RX = AAX - AAA * ANX 
          RY = AAY - AAA * ANY 
          RZ = AAZ - AAA * ANZ 
          RS = SQRT(RX*RX + RY*RY + RZ*RZ)
          RR = SQRT(RR)
          IF(RS-RR+GAPV(I)<0.)THEN
            STIF(I) = 0.
            PENE(I) = 0.
          ELSEIF(RS-RR+GAPV(I)<PENE(I))THEN
            PENE(I) = RS-RR+GAPV(I)
            N1(I) = -RX 
            N2(I) = -RY 
            N3(I) = -RZ
          ELSEIF(RS-RR-GAPV(I)>0.)THEN
            STIF(I) = 0.
            PENE(I) = 0.
          ELSEIF(RS-RR-GAPV(I) < PENE(I))THEN
            XN1 = X1(I) - XI(I)
            YN1 = Y1(I) - YI(I)
            ZN1 = Z1(I) - ZI(I)
            XN2 = X2(I) - XI(I)
            YN2 = Y2(I) - YI(I)
            ZN2 = Z2(I) - ZI(I)
            XN3 = X3(I) - XI(I)
            YN3 = Y3(I) - YI(I)
            ZN3 = Z3(I) - ZI(I)
C --  
            NN1 = (YN1*ZN2-YN2*ZN1) * RX +
     .           (ZN1*XN2-ZN2*XN1) * RY +
     .           (XN1*YN2-XN2*YN1) * RZ 
            NN2 = (YN2*ZN3-YN3*ZN2) * RX +
     .           (ZN2*XN3-ZN3*XN2) * RY +
     .           (XN2*YN3-XN3*YN2) * RZ 
            NN3 = (YN3*ZN4-YN4*ZN3) * RX +
     .           (ZN3*XN4-ZN4*XN3) * RY +
     .           (XN3*YN4-XN4*YN3) * RZ 
            IF(IX3L(I)/=IX4L(I))THEN
              XN4 = X4(I) - XI(I)
              YN4 = Y4(I) - YI(I)
              ZN4 = Z4(I) - ZI(I)
              NN4 = (YN4*ZN1-YN1*ZN4) * RX +
     .           (ZN4*XN1-ZN1*XN4) * RY +
     .           (XN4*YN1-XN1*YN4) * RZ 
            ELSE
              XN4 = ZERO
              YN4 = ZERO
              ZN4 = ZERO
              NN4=ZERO
            ENDIF
            IF( NN1>=ZERO .AND. NN2>=ZERO
     .                  .AND. NN3>=ZERO .AND. NN4>=ZERO) THEN
              IPROJ = 1
            ELSEIF( NN1<=ZERO .AND. NN2<=ZERO
     .                  .AND. NN3<=ZERO .AND. NN4<=ZERO) THEN
              IPROJ = 1
            ELSE
              IPROJ = 0
            ENDIF
C --        
            IF(IPROJ == 1)THEN
              PENE(I) = -RS+RR+GAPV(I)
              N1(I) = RX 
              N2(I) = RY 
              N3(I) = RZ 
            ENDIF
          ENDIF
        ENDDO

      ELSEIF(ICURV(1) == 3)THEN
        CALL I7CURV(JLT    ,PENE   ,N1     ,N2        ,
     1              N3     ,GAPV   ,XA     ,NOD_NORMAL,
     2              IX1L   ,IX2L   ,IX3L   ,IX4L      ,
     3              H1     ,H2     ,H3     ,H4        ,
     4              X1     ,X2     ,X3     ,X4    ,Y1	 ,
     5              Y2     ,Y3     ,Y4     ,Z1    ,Z2	 ,
     6              Z3     ,Z4     ,XI     ,YI    ,ZI	 )

        DO I=1,JLT
          IF(PENE(I)<ZERO)THEN
            STIF(I) =ZERO
            PENE(I) =ZERO
          END IF
        END DO
      ENDIF

      DO I=1,JLT
         S2 = ONE/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
         N1(I) = N1(I)*S2
         N2(I) = N2(I)*S2
         N3(I) = N3(I)*S2
      ENDDO
C
      DO I=1,JLT
          VX(I) = VXI(I) - H1(I)*VA(1,IX1L(I)) - H2(I)*VA(1,IX2L(I))
     .                   - H3(I)*VA(1,IX3L(I)) - H4(I)*VA(1,IX4L(I))
          VY(I) = VYI(I) - H1(I)*VA(2,IX1L(I)) - H2(I)*VA(2,IX2L(I))
     .                   - H3(I)*VA(2,IX3L(I)) - H4(I)*VA(2,IX4L(I))
          VZ(I) = VZI(I) - H1(I)*VA(3,IX1L(I)) - H2(I)*VA(3,IX2L(I))
     .                   - H3(I)*VA(3,IX3L(I)) - H4(I)*VA(3,IX4L(I))
          VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
      ENDDO

      DO I=1,JLT
C       correction hourglass
        H0 = -.25*(H1(I) - H2(I) + H3(I) - H4(I))
        H0 = MIN(H0,H2(I),H4(I))
        H0 = MAX(H0,-H1(I),-H3(I))
        IF(IX3G(I)==IX4G(I))H0 = ZERO
        H1(I) = H1(I) + H0
        H2(I) = H2(I) - H0
        H3(I) = H3(I) + H0
        H4(I) = H4(I) - H0
      ENDDO
C---------------------
C      PENE INITIALE
C---------------------
      IF(INACTI==5.or.INACTI==6)THEN
c         DO I=1,JLT
cC REDUCTION DE LA PENE INITIALE
cC           CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),PENE(I))
c           CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
c     .        ( (ONE-FIVEEM2)*CAND_P(INDEX(I))
c     .          +FIVEEM2*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I))))  )
cC SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
c           PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
c           IF( PENE(I)==ZERO )  STIF(I) = ZERO
c           GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
c         ENDDO
#include "lockon.inc"
C---
        IF(IGAP > 0)THEN
          DO I=1,JLT
            IS = CN_LOC(I)
            IM = CE_LOC(I)
            NN = NSVG(I)
            PPLUS = PENE(I) + ZEP05*(GAPV(I)-PENE(I))
            IF(NN > 0) THEN
              IF (PPLUS < GAP_S(IS)) THEN
                PENIS(2,IS) = MAX(PENIS(2,IS),PPLUS)
              ELSE
                PENIS(2,IS) = MAX(PENIS(2,IS),GAP_S(IS))
                PENIM(2,IM) = MAX(PENIM(2,IM),PPLUS-GAP_S(IS))
              END IF
            ELSE
              IF (PPLUS < GAPFI(NIN)%P(-NN)) THEN
                PENFI(NIN)%P(2,-NN) = MAX(PENFI(NIN)%P(2,-NN),PPLUS)
              ELSE
                PENFI(NIN)%P(2,-NN) = MAX(PENFI(NIN)%P(2,-NN),
     +                                    GAPFI(NIN)%P(-NN))
                PENIM(2,IM) = MAX(PENIM(2,IM),PPLUS-GAPFI(NIN)%P(-NN))
              END IF
            ENDIF
          ENDDO
        ELSE
          DO I=1,JLT
            IM = CE_LOC(I)
            PPLUS = PENE(I) + ZEP05*(GAPV(I)-PENE(I))
            PENIM(2,IM) = MAX(PENIM(2,IM),PPLUS)
          ENDDO
        END IF
C---
c        DO I=1,JLT
c          AAA = GAP_S(IS)/GAPV(I)                      
c          PPLUS=(PENE(I)+ZEP05*(GAPV(I)-PENE(I)))      
c          NN = NSVG(I)
c          IF(NN > 0) THEN
c            PENIS(2,CN_LOC(I)) = MAX(PENIS(2,CN_LOC(I)),AAA*PPLUS)
c          ELSE
c            PENFI(NIN)%P(2,-NN) = MAX(PENFI(NIN)%P(2,-NN),AAA*PPLUS)
c          END IF
c          PENIM(2,CE_LOC(I)) = MAX(PENIM(2,CE_LOC(I)),(ONE-AAA)*PPLUS)
c        ENDDO
C---
#include "lockoff.inc"
        DO I=1,JLT
          IS = CN_LOC(I)    
          IM = CE_LOC(I)    
         NN = NSVG(I)
         IF(NN > 0) THEN
            PIS = PENIS(1,IS)
         ELSE
           PIS = PENFI(NIN)%P(1,-NN)
         END IF
          PENE(I) = PENE(I) - PIS - PENIM(1,IM)
         PENE(I) = MAX(PENE(I),ZERO)
          IF (PENE(I) == ZERO )STIF(I)=ZERO
          GAPV(I) = GAPV(I) - PIS - PENIM(1,IM)
        END DO
      ENDIF
C---------------------
C
      DTI = 1.E20
C
      DO 600 I=1,JLT
          DIST=GAPV(I)-PENE(I)
          RDIST  = HALF*DIST / MAX(EM30,-VN(I))
          DTI = MIN(RDIST,DTI)
 600  CONTINUE
C intermediate variable coming from starter input deck interface cards
C not read for this type of interface
      DTMINI=EP20
C
      IF(DTI<=DTMIN1(10))THEN
        DTI = 1.E20
        DO I=1,JLT
            DIST=GAPV(I)-PENE(I)
            DTI2   = HALF*DIST / MAX(EM30,-VN(I))
            IF(DTI2<=DTMIN1(10))THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10)')
     .            ' **WARNING MINIMUM TIME STEP ',DTI2,
     .            ' IN INTERFACE ',NOINT
             NN = NSVG(I)
             IF(NN>0)THEN
               NI = ITAB(NN)
             ELSE
               NI = ITAFI(NIN)%P(-NN)
             ENDIF
#include "lockoff.inc"
             IF(IDTMIN(10)==1)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',NI
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .     ITAB(IX1G(I)),ITAB(IX2G(I)),ITAB(IX3G(I)),ITAB(IX4G(I))  
#include "lockoff.inc"
               TSTOP = TT
             ELSEIF(IDTMIN(10)==2)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,I10,A,I10)')'   REMOVE SECONDARY NODE ',
     .               NI,' FROM INTERFACE ',NOINT                 
               IF(NN>0) THEN
                 STFA(NSV(CN_LOC(I))) = -ABS(STFA(NSV(CN_LOC(I))))
               ELSE
                 STIFI(NIN)%P(-NN) = -ABS(STIFI(NIN)%P(-NN))
               ENDIF
#include "lockoff.inc"
               STIF(I) = ZERO
               NEWFRONT = -1
               DTI = DTMIN1(10)
             ELSEIF(IDTMIN(10)==5)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',NI                 
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .    ITAB(IX1G(I)),ITAB(IX2G(I)),ITAB(IX3G(I)),ITAB(IX4G(I))  
#include "lockoff.inc"
               MSTOP = 2
             ELSEIF(IDTMIN(10)==6.AND.ILAGM==2)THEN
              IG=NSVG(I)
              IF(KINET(IG)+KINET(IX1G(I))+KINET(IX2G(I))
     .          +KINET(IX3G(I))+KINET(IX4G(I))==0)THEN
               CAND_N(INDEX(I)) = -IABS(CAND_N(INDEX(I)))
               STIF(I) = ZERO
               DTI2 = 1.E20
#include "lockon.inc"
               WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',ITAB(NSVG(I))                 
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .    ITAB(IX1G(I)),ITAB(IX2G(I)),ITAB(IX3G(I)),ITAB(IX4G(I))  
#include "lockoff.inc"
              ENDIF                
              DTI = MIN(DTI2,DTI)
             ENDIF                
            ENDIF               
        ENDDO
      ENDIF
      IF(DTI<DT2T)THEN
        DT2T    = DTI
        NELTST  = NOINT
        ITYPTST = 10
      ENDIF
C-------------------------------------------
      IF(IMPL_S>0)THEN
       IF(IMP_INT7==2)THEN
        DO I=1,JLT
         IF(STIGLO<=ZERO)THEN
          STIF(I) = HALF*STIF(I) 
         ELSEIF(STIF(I)/=ZERO)THEN
          STIF(I) = STIGLO 
         ENDIF
         FNI(I)= -STIF(I) * PENE(I) 
        ENDDO
       ELSE
        DO I=1,JLT
         FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
         FACM1 = 1./FAC
         IF( (GAPV(I)-PENE(I))/GAPV(I) <PREC .AND. 
     .     STIF(I)>0. ) THEN
          STIF(I) = 0.
          NEWFRONT = -1
#include "lockon.inc"
          NN = NSVG(I)
          IF(NN>0)THEN
           NI = ITAB(NN)
           STFA(NSV(CN_LOC(I))) = -ABS(STFA(NSV(CN_LOC(I))))
          ELSE
           NI = ITAFI(NIN)%P(-NN)
           STIFI(NIN)%P(-NN) = -ABS(STIFI(NIN)%P(-NN))
          ENDIF
          WRITE(ISTDO,'(A,I10)')' WARNING INTERFACE ',NOINT
          WRITE(ISTDO,'(A,I10,A)')' NODE ',NI,
     .                   ' DE-ACTIVATED FROM INTERFACE'                 
          WRITE(IOUT ,'(A,I10)')' WARNING INTERFACE ',NOINT
          WRITE(IOUT ,'(A,I10,A)')' NODE ',NI,
     .                   ' DE-ACTIVATED FROM INTERFACE' 
#include "lockoff.inc"
         ENDIF
         IF(STIGLO<=ZERO)THEN
          ECONTT = ECONTT + HALF*STIF(I)*GAPV(I)**2 *( FACM1 -  
     .        ONE -LOG(FACM1) )
          STIF(I) = HALF*STIF(I) * FAC
         ELSEIF(STIF(I)/=ZERO)THEN
          ECONTT = ECONTT + STIGLO*GAPV(I)**2 *( FACM1 - ONE - 
     .        LOG(FACM1) )
          STIF(I) = STIGLO * FAC
         ENDIF
         FNI(I)= -STIF(I) * PENE(I) 
        ENDDO
       ENDIF
      ELSE              ! fin impl_s>0
      DO 100 I=1,JLT
      FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
      FACM1 = 1./FAC
      IF( (GAPV(I)-PENE(I))/GAPV(I) <PREC .AND. 
     .     STIF(I)>0. ) THEN
         STIF(I) = 0.
         NEWFRONT = -1
#include "lockon.inc"
         NN = NSVG(I)
         IF(NN>0)THEN
           NI = ITAB(NN)
           STFA(NSV(CN_LOC(I))) = -ABS(STFA(NSV(CN_LOC(I))))
         ELSE
           NI = ITAFI(NIN)%P(-NN)
           STIFI(NIN)%P(-NN) = -ABS(STIFI(NIN)%P(-NN))
         ENDIF
         WRITE(ISTDO,'(A,I10)')' WARNING INTERFACE ',NOINT
         WRITE(ISTDO,'(A,I10,A)')' NODE ',NI,
     .                   ' DE-ACTIVATED FROM INTERFACE'                 
         WRITE(IOUT ,'(A,I10)')' WARNING INTERFACE ',NOINT
         WRITE(IOUT ,'(A,I10,A)')' NODE ',NI,
     .                   ' DE-ACTIVATED FROM INTERFACE' 
#include "lockoff.inc"
      ENDIF
      IF(STIGLO<=ZERO)THEN
        ECONTT = ECONTT + HALF*STIF(I)*GAPV(I)**2 *( FACM1 - ONE - 
     .        LOG(FACM1) )
        STIF(I) = HALF*STIF(I) * FAC
      ELSEIF(STIF(I)/=ZERO)THEN
          ECONTT = ECONTT + STIGLO*GAPV(I)**2 *(FACM1 - ONE - LOG(FACM1))
        STIF(I) = STIGLO * FAC
      ENDIF
      FNI(I)= -STIF(I) * PENE(I) 
 100  CONTINUE
      ENDIF
C---------------------------------
C     DAMPING + FRIC
C---------------------------------
      IF(VISC/=ZERO.OR.VISCF/=ZERO)THEN
        IF(IVIS2==0)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT
            VIS2(I) = TWO * STIF(I) * MSI(I)
            IF(VN(I)<ZERO) VIS2(I) = VIS2(I) / 
     .         ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
          ENDDO
C---------------------------------
          VISCA = ZEP4
          IF(KDTINT==0.AND.IDTMINS/=2)THEN
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO* MSI(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
            STIF(I) = STIF(I) + FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
            FFO = FF
            FF = FF * VN(I)  
            FNI(I)  = FNI(I) + FF
           ENDDO
          ELSE
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            C(I)= FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MSI(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10)
            KT(I)= STIF(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF = C(I) * VN(I)
            FNI(I)  = FNI(I) + FF
            CF(I)   = FAC*SQRT(VISCF)*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDDO
          ENDIF
        ELSEIF(IVIS2==1)THEN
C---------------------------------
C         TEST
C---------------------------------
          DO I=1,JLT
            MASM(I) = MS(IX1G(I))*H1(I)
     .              + MS(IX2G(I))*H2(I)
     .              + MS(IX3G(I))*H3(I)
     .              + MS(IX4G(I))*H4(I)
            MASM(I) = MSI(I) * MASM(I) / MAX(EM30,MSI(I)+MASM(I))
            VIS2(I) = TWO * STIF(I) * MASM(I)
            IF(VN(I)<ZERO) VIS2(I) = VIS2(I) / 
     .         ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
          ENDDO
C---------------------------------
          VISCA = ZEP4
          IF(KDTINT==0.AND.IDTMINS/=2)THEN
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO* MASM(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
            STIF(I) = STIF(I) + FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
            FFO = FF
            FF = FF * VN(I)  
            FNI(I)  = FNI(I) + FF
           ENDDO
          ELSE
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            C(I)= FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MASM(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10)
            KT(I)= STIF(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF = C(I) * VN(I)
            FNI(I)  = FNI(I) + FF
            CF(I)   = FAC*SQRT(VISCF)*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDDO
          ENDIF
        ELSEIF(IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
          DO I=1,JLT
            VIS2(I) = TWO* STIF(I) * MSI(I)
            VIS2(I) = VIS2(I) / 
     .           ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
          ENDDO
          VISCA = HALF
          DO I=1,JLT
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MSI(I) * ABS(VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = STIF(I) + TWO * FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
           FF = FF * VN(I)  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT
            VIS2(I) = TWO * STIF(I) * MSI(I)
          ENDDO
C---------------------------------
          DO I=1,JLT
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * ( VISC * VIS ) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)
           STIF(I) = STIF(I) * GAPV(I) / 
     .               MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = STIF(I) + TWO* FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
           FF = FF * VN(I)  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
          DO I=1,JLT
           VIS2(I) = TWO* STIF(I) * MSI(I)
           VIS = SQRT(VIS2(I)) 
           STIF(I) = STIF(I) * GAPV(I) / 
     .          MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
          ENDDO
        ELSEIF(IVIS2==5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT
           MAS2  = MS(IX1G(I))*H1(I)
     .           + MS(IX2G(I))*H2(I)
     .           + MS(IX3G(I))*H3(I)
     .           + MS(IX4G(I))*H4(I)
           VIS2(I) = TWO* STIF(I) * MSI(I)
           VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
           STIF(I) = STIF(I) * GAPV(I) / 
     .          MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF*VIS2(I))*DT1INV)
           FF = VIS * VN(I)  
           ECONVT = ECONVT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
           FNI(I)  = MIN(FNI(I),FF)
          ENDDO
        ELSE
        ENDIF
      ELSE
         DO I=1,JLT
           VIS2(I) = ZERO
           STIF(I) = STIF(I) * GAPV(I) / 
     .            MAX((GAPV(I) - PENE(I)),EM10) 
         ENDDO
      ENDIF
C---------------------------------
C     REDUCTION RIGIDITE ANCRAGE
C---------------------------------
#include "lockon.inc"
      DO I=1,JLT
        ISIGN=1
        AAA = ONE-PENE(I)/GAPV(I)
        IL = IX1L(I)
        IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
        ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        IL = IX2L(I)
        IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
        ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        IL = IX3L(I)
        IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
        ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        IL = IX4L(I)
        IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
        ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        IF(NSVG(I)>0) THEN
          IL = NSV(CN_LOC(I))
          IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
          ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        ELSE
C         SPMD remote SECONDARYs
          IL = - NSVG(I)
          IF(PENE(I)>ZERO.OR.ALPHAKFI(NIN)%P(IL)<ZERO)ISIGN=-1
          ALPHAKFI(NIN)%P(IL)=ISIGN*MIN(ABS(ALPHAKFI(NIN)%P(IL)),AAA)
        ENDIF
      ENDDO
#include "lockoff.inc"
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
       FXI(I)=N1(I)*FNI(I)
       FYI(I)=N2(I)*FNI(I)
       FZI(I)=N3(I)*FNI(I)
       IMPX=FXI(I)*DT12
       IMPY=FYI(I)*DT12
       IMPZ=FZI(I)*DT12
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
          FSAVPARIT(1,1,I+NFT) =  FXI(I)
          FSAVPARIT(1,2,I+NFT) =  FYI(I)
          FSAVPARIT(1,3,I+NFT) =  FZI(I)
	ENDDO
      ENDIF
c
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0.AND.
     .          (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D .OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
#include "lockon.inc"
           DO I=1,JLT
            FNCONT(1,IX1G(I)) =FNCONT(1,IX1G(I)) + FXI(I)*H1(I)
            FNCONT(2,IX1G(I)) =FNCONT(2,IX1G(I)) + FYI(I)*H1(I)
            FNCONT(3,IX1G(I)) =FNCONT(3,IX1G(I)) + FZI(I)*H1(I)
            FNCONT(1,IX2G(I)) =FNCONT(1,IX2G(I)) + FXI(I)*H2(I)
            FNCONT(2,IX2G(I)) =FNCONT(2,IX2G(I)) + FYI(I)*H2(I)
            FNCONT(3,IX2G(I)) =FNCONT(3,IX2G(I)) + FZI(I)*H2(I)
            FNCONT(1,IX3G(I)) =FNCONT(1,IX3G(I)) + FXI(I)*H3(I)
            FNCONT(2,IX3G(I)) =FNCONT(2,IX3G(I)) + FYI(I)*H3(I)
            FNCONT(3,IX3G(I)) =FNCONT(3,IX3G(I)) + FZI(I)*H3(I)
            FNCONT(1,IX4G(I)) =FNCONT(1,IX4G(I)) + FXI(I)*H4(I)
            FNCONT(2,IX4G(I)) =FNCONT(2,IX4G(I)) + FYI(I)*H4(I)
            FNCONT(3,IX4G(I)) =FNCONT(3,IX4G(I)) + FZI(I)*H4(I)
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
      ENDIF
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB/=0)THEN
         DO JSUB=1,NISUB
           DO J=1,15
               FSAVSUB1(J,JSUB)=ZERO
           END DO
         ENDDO
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
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
C                MAIN side :
                 FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                 FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                 FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                 FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                 FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                 FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                 FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
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
                 IMPX=FXI(I)*DT12
                 IMPY=FYI(I)*DT12
                 IMPZ=FZI(I)*DT12
C                MAIN side :
                 FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                 FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                 FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                 FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                 FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                 FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                 FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                 KK=KK+1
                 GO TO 150             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 GO TO 150
               END IF
             END DO
 150       CONTINUE
           JJ=JJ+1
           END DO
          END IF

         END DO

        END IF 
      END IF

C---------------------------------
C       NEW FRICTION MODELS
C---------------------------------
        IF (MFROT==0) THEN
C---      Coulomb friction
          DO I=1,JLT
            XMU(I) = FRIC
          ENDDO
        ELSEIF (MFROT==1) THEN
C---      Viscous friction
          DO I=1,JLT
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2 
     .         + (VY(I) - N2(I)*AA)**2 
     .         + (VZ(I) - N3(I)*AA)**2
            VV  = SQRT(MAX(EM30,V2))
            AX1 = X3(I) - X1(I)
            AY1 = Y3(I) - Y1(I)
            AZ1 = Z3(I) - Z1(I)
            AX2 = X4(I) - X2(I)
            AY2 = Y4(I) - Y2(I)
            AZ2 = Z4(I) - Z2(I)
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRIC + (FROT_P(1) + FROT_P(4)*P ) * P 
     .             +(FROT_P(2) + FROT_P(3)*P) * VV + FROT_P(5)*V2
          ENDDO
        ELSEIF(MFROT==2)THEN
C---        Loi Darmstad
          DO I=1,JLT
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2 
     .         + (VY(I) - N2(I)*AA)**2 
     .         + (VZ(I) - N3(I)*AA)**2
            VV = SQRT(MAX(EM30,V2))
            AX1 = X3(I) - X1(I)
            AY1 = Y3(I) - Y1(I)
            AZ1 = Z3(I) - Z1(I)
            AX2 = X4(I) - X2(I)
            AY2 = Y4(I) - Y2(I)
            AZ2 = Z4(I) - Z2(I)
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
                AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRIC
     .             + FROT_P(1)*EXP(FROT_P(2)*VV)*P*P
     .             + FROT_P(3)*EXP(FROT_P(4)*VV)*P
     .             + FROT_P(5)*EXP(FROT_P(6)*VV)
          ENDDO
        ELSEIF (MFROT==3) THEN
C---        Loi Renard
          DO I=1,JLT
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2 
     .         + (VY(I) - N2(I)*AA)**2 
     .         + (VZ(I) - N3(I)*AA)**2
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

        IF (IFQ>=10) THEN
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
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

            FX = CAND_FX(INDEX(I)) + ALPHA*FX
            FY = CAND_FY(INDEX(I)) + ALPHA*FY
            FZ = CAND_FZ(INDEX(I)) + ALPHA*FZ

            FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
            FX = FX - FTN*N1(I)
            FY = FY - FTN*N2(I)
            FZ = FZ - FTN*N3(I)
            FT = FX*FX + FY*FY + FZ*FZ
            FT = MAX(FT,EM30)

            FN = FXI(I)**2+FYI(I)**2+FZI(I)**2

            BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))

            FXT(I) = FX * BETA
            FYT(I) = FY * BETA
            FZT(I) = FZ * BETA

            CAND_FX(INDEX(I)) = FXT(I)
            CAND_FY(INDEX(I)) = FYT(I)
            CAND_FZ(INDEX(I)) = FZT(I)
            IFPEN(INDEX(I)) = 1

C-------      total force
            FXI(I)=FXI(I) + FXT(I)
            FYI(I)=FYI(I) + FYT(I)
            FZI(I)=FZI(I) + FZT(I)
            ECONVT = ECONVT 
     .             + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
          ENDDO
C---------------------------------
C         TOTAL (VISCOUS) FORMULATION + FRICTION FILTERING 
C---------------------------------
        ELSEIF (IFQ>0) THEN

          IF (IFQ==3) THEN
            ALPHA = MAX(ONE,ALPHA0*DT12)
          ELSE
            ALPHA = ALPHA0
          ENDIF
          ALPHI = ONE - ALPHA
          DO I=1,JLT
            VNX = N1(I)*VN(I)
            VNY = N2(I)*VN(I)
            VNZ = N3(I)*VN(I)
            VX(I) = VX(I) - VNX
            VY(I) = VY(I) - VNY
            VZ(I) = VZ(I) - VNZ
            V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
            VIS2(I) = VISCF * VIS2(I)
            FM2  = (XMU(I)*FNI(I))**2
            F2   = VIS2(I) * V2
            A2 = MIN(F2,FM2) / MAX(EM30,F2)
            AA = SQRT(A2 * VIS2(I))
            FX = AA * VX(I)
            FY = AA * VY(I)
            FZ = AA * VZ(I)

            FXT(I) = ALPHA*FX + ALPHI*CAND_FX(INDEX(I))
            FYT(I) = ALPHA*FY + ALPHI*CAND_FY(INDEX(I))
            FZT(I) = ALPHA*FZ + ALPHI*CAND_FZ(INDEX(I))
            CAND_FX(INDEX(I)) = FXT(I)
            CAND_FY(INDEX(I)) = FYT(I)
            CAND_FZ(INDEX(I)) = FZT(I)
            IFPEN(INDEX(I)) = 1    
C-------      total force
            FXI(I) = FXI(I) + FXT(I)
            FYI(I) = FYI(I) + FYT(I)
            FZI(I) = FZI(I) + FZT(I)
            ECONVT = ECONVT 
     .             + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
          ENDDO
        ELSE
C---------------------------------
C       TOTAL (VISCOUS) FORMULATION / NO FRICTION FILTERING 
C---------------------------------
          DO I=1,JLT
            VNX = N1(I)*VN(I)
            VNY = N2(I)*VN(I)
            VNZ = N3(I)*VN(I)
            VX(I) = VX(I) - VNX
            VY(I) = VY(I) - VNY
            VZ(I) = VZ(I) - VNZ
            V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
            VIS2(I) = VISCF * VIS2(I)
            FM2  = (XMU(I)*FNI(I))**2
            F2   = VIS2(I) * V2
            A2 = MIN(F2,FM2) / MAX(EM30,F2)
            AA = SQRT(A2 * VIS2(I))
            FXT(I) = AA * VX(I)
            FYT(I) = AA * VY(I)
            FZT(I) = AA * VZ(I)
C-------      total force
            FXI(I)=FXI(I) + FXT(I)
            FYI(I)=FYI(I) + FYT(I)
            FZI(I)=FZI(I) + FZT(I)
            ECONVT = ECONVT + AA * V2 * DT1
         ENDDO
        ENDIF
C---------------------------------
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          (TT>=TANIM.OR.TT>=TOUTP.OR.TT>=H3D_DATA%TH3D.OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
#include "lockon.inc"
           DO I=1,JLT
            FTCONT(1,IX1G(I)) =FTCONT(1,IX1G(I)) + FXT(I)*H1(I)
            FTCONT(2,IX1G(I)) =FTCONT(2,IX1G(I)) + FYT(I)*H1(I)
            FTCONT(3,IX1G(I)) =FTCONT(3,IX1G(I)) + FZT(I)*H1(I)
            FTCONT(1,IX2G(I)) =FTCONT(1,IX2G(I)) + FXT(I)*H2(I)
            FTCONT(2,IX2G(I)) =FTCONT(2,IX2G(I)) + FYT(I)*H2(I)
            FTCONT(3,IX2G(I)) =FTCONT(3,IX2G(I)) + FZT(I)*H2(I)
            FTCONT(1,IX3G(I)) =FTCONT(1,IX3G(I)) + FXT(I)*H3(I)
            FTCONT(2,IX3G(I)) =FTCONT(2,IX3G(I)) + FYT(I)*H3(I)
            FTCONT(3,IX3G(I)) =FTCONT(3,IX3G(I)) + FZT(I)*H3(I)
            FTCONT(1,IX4G(I)) =FTCONT(1,IX4G(I)) + FXT(I)*H4(I)
            FTCONT(2,IX4G(I)) =FTCONT(2,IX4G(I)) + FYT(I)*H4(I)
            FTCONT(3,IX4G(I)) =FTCONT(3,IX4G(I)) + FZT(I)*H4(I)
            JG = NSVG(I)
            IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
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
      ENDIF

C---------------------------------
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
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
	DO I=1,JLT
          FSAVPARIT(1,4,I+NFT) =  FXT(I)
          FSAVPARIT(1,5,I+NFT) =  FYT(I)
          FSAVPARIT(1,6,I+NFT) =  FZT(I)
	ENDDO
      ENDIF
C
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
                 IMPX=FXT(I)*DT12
                 IMPY=FYT(I)*DT12
                 IMPZ=FZT(I)*DT12
C                MAIN side :
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
C
                 FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                           +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
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
                 IMPX=FXT(I)*DT12
                 IMPY=FYT(I)*DT12
                 IMPZ=FZT(I)*DT12
C                MAIN side :
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
C
                 FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                           +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                 KK=KK+1
                 GO TO 300             
               ELSE IF(KSUB<JSUB)THEN
                 KK=KK+1
               ELSE
                 GO TO 300
               END IF
             END DO
 300       CONTINUE
           JJ=JJ+1
           END DO
          END IF

         END DO 

        END IF 
#include "lockon.inc"
         DO JSUB=1,NISUB
           NSUB=LISUB(JSUB)
           DO J=1,15
             FSAVSUB(J,NSUB)=FSAVSUB(J,NSUB)+FSAVSUB1(J,JSUB)
           END DO
         END DO
#include "lockoff.inc"
      END IF
C---------------------------------
#include "lockon.inc"
        ECONTV = ECONTV + ECONVT
        ECONT  = ECONT + ECONTT
#include "lockoff.inc"
C---------------------------------
      IF(KDTINT==1)THEN
       IF(    (VISC/=ZERO.OR.VISCF/=ZERO)
     .   .AND.(IVIS2==0.OR.IVIS2==1))THEN
        DO I=1,JLT
C        C(I)=2.*C(I)
         IF(MSI(I)==ZERO)THEN
          KS(I) =ZERO
          CS(I) =ZERO
          STV(I)=ZERO
         ELSE
          CX  = FOUR*C(I)*C(I)
          CY  = EIGHT*MSI(I)*KT(I)
          AUX   = SQRT(CX+CY)+TWO*C(I)
          STV(I)= KT(I)*AUX*AUX/MAX(CY,EM30)
          AUX   = TWO*CF(I)*CF(I)/MAX(MSI(I),EM20)
          IF(AUX>STV(I))THEN
           KS(I) =ZERO
           CS(I) =CF(I)
           STV(I)=AUX
          ELSE
           KS(I)= KT(I)
           CS(I) =C(I)
          ENDIF
         ENDIF      
C
         J1=IX1G(I)
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
          CFI   = CF(I)*ABS(H1(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX>ST1(I))THEN
           K1(I) =ZERO
           C1(I) =CFI
           ST1(I)=AUX
          ENDIF         
         ENDIF      
C
         J1=IX2G(I)
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
          CFI   = CF(I)*ABS(H2(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX>ST2(I))THEN
           K2(I) =ZERO
           C2(I) =CFI
           ST2(I)=AUX
          ENDIF         
         ENDIF      
C
         J1=IX3G(I)
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
          CFI   = CF(I)*ABS(H3(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX>ST3(I))THEN
           K3(I) =ZERO
           C3(I) =CFI
           ST3(I)=AUX
          ENDIF         
         ENDIF      
C
         J1=IX4G(I)
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
          CFI   = CF(I)*ABS(H4(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX>ST4(I))THEN
           K4(I) =ZERO
           C4(I) =CFI
           ST4(I)=AUX
          ENDIF         
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

      IF(IDTMIN(10)==1.OR.IDTMIN(10)==2.OR.
     .   IDTMIN(10)==5.OR.IDTMIN(10)==6)THEN

       DTMI0 = EP20
       IF(KDTINT==0)THEN
        DO I=1,JLT
         DTMI(I) = EP20
         MAS2  = TWO * MSI(I)
         IF(MAS2>ZERO.AND.STIF(I)>ZERO.AND.
     .     IRB(KINI(I))==0.AND.IRB2(KINI(I))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STIF(I)))
         ENDIF
         MAS2  = TWO* MS(IX1G(I))
         IF(MAS2>ZERO.AND.H1(I)*STIF(I)>ZERO.AND.
     .     IRB(KINET(IX1G(I)))==0.AND.IRB2(KINET(IX1G(I)))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H1(I)*STIF(I))))
         ENDIF
         MAS2  = TWO * MS(IX2G(I))
         IF(MAS2>ZERO.AND.H2(I)*STIF(I)>ZERO.AND.
     .     IRB(KINET(IX2G(I)))==0.AND.IRB2(KINET(IX2G(I)))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H2(I)*STIF(I))))
         ENDIF
         MAS2  = TWO* MS(IX3G(I))
         IF(MAS2>ZERO.AND.H3(I)*STIF(I)>ZERO.AND.
     .     IRB(KINET(IX3G(I)))==0.AND.IRB2(KINET(IX3G(I)))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H3(I)*STIF(I))))
         ENDIF
         MAS2  = TWO * MS(IX4G(I))
         IF(MAS2>ZERO.AND.H4(I)*STIF(I)>ZERO.AND.
     .     IRB(KINET(IX4G(I)))==0.AND.IRB2(KINET(IX4G(I)))==0)THEN
           DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H4(I)*STIF(I))))
         ENDIF
         DTMI0 = MIN(DTMI0,DTMI(I))
        ENDDO

       ELSE
        DO I=1,JLT
         DTMI(I) = EP20
         MAS2  = TWO * MSI(I)
         MAS2  = TWO * MSI(I)
         IF(MAS2>ZERO.AND.STV(I)>ZERO.AND.
     .     IRB(KINI(I))==0.AND.IRB2(KINI(I))==0)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STV(I)))
         ENDIF
         MAS2  = TWO * MS(IX1G(I))
         IF(MAS2>ZERO.AND.ST1(I)>ZERO.AND.
     .     IRB(KINET(IX1G(I)))==0.AND.IRB2(KINET(IX1G(I)))==0)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST1(I))))
         ENDIF
         MAS2  = TWO * MS(IX2G(I))
         IF(MAS2>ZERO.AND.ST2(I)>ZERO.AND.
     .     IRB(KINET(IX2G(I)))==0.AND.IRB2(KINET(IX2G(I)))==0)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST2(I))))
         ENDIF
         MAS2  = TWO * MS(IX3G(I))
         IF(MAS2>ZERO.AND.ST3(I)>ZERO.AND.
     .     IRB(KINET(IX3G(I)))==0.AND.IRB2(KINET(IX3G(I)))==0)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST3(I))))
         ENDIF
         MAS2  = TWO * MS(IX4G(I))
         IF(MAS2>ZERO.AND.ST4(I)>ZERO.AND.
     .     IRB(KINET(IX4G(I)))==0.AND.IRB2(KINET(IX4G(I)))==0)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST4(I))))
         ENDIF
         DTMI0 = MIN(DTMI0,DTMI(I))
        ENDDO
       ENDIF
       IF(DTMI0<=DTMIN1(10))THEN
        DO I=1,JLT
         IF(DTMI(I)<=DTMIN1(10))THEN
           JG = NSVG(I)
           IF(JG>0)THEN
             NI = ITAB(JG)
           ELSE
             NI = ITAFI(NIN)%P(-JG)
           ENDIF
           IF(IDTMIN(10)==1)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE ',NOINT
             WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                
             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .         ITAB(IX1G(I)),ITAB(IX2G(I)),ITAB(IX3G(I)),ITAB(IX4G(I)) 
#include "lockoff.inc"
             TSTOP = TT
           ELSEIF(IDTMIN(10)==2)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE ',NOINT
             WRITE(IOUT,'(A,I10,A,I10)')'   DELETE SECONDARY NODE ',
     .         NI,' FROM INTERFACE ',NOINT                 
             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .    ITAB(IX1G(I)),ITAB(IX2G(I)),ITAB(IX3G(I)),ITAB(IX4G(I)) 
             IF(JG>0) THEN
               STFA(NSV(CN_LOC(I))) = -ABS(STFA(NSV(CN_LOC(I))))
             ELSE
               STIFI(NIN)%P(-JG) = -ABS(STIFI(NIN)%P(-JG))
             ENDIF
#include "lockoff.inc"
             NEWFRONT = -1
           ELSEIF(IDTMIN(10)==5)THEN
#include "lockon.inc"
             WRITE(IOUT,'(A,E12.4,A,I10)')
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE ',NOINT
             WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                 
             WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .    ITAB(IX1G(I)),ITAB(IX2G(I)),ITAB(IX3G(I)),ITAB(IX4G(I)) 
#include "lockoff.inc"
             MSTOP = 2
           ELSEIF(IDTMIN(10)==6.AND.ILAGM==2)THEN
             IF(KINET(JG)+KINET(IX1G(I))+KINET(IX2G(I))
     .         +KINET(IX3G(I))+KINET(IX4G(I))==0 )THEN
               CAND_N(INDEX(I)) = -IABS(CAND_N(INDEX(I)))
               STIF(I) = 0.
               FXI(I)  = 0.
               FYI(I)  = 0.
               FZI(I)  = 0.
             ENDIF                
           ENDIF                
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C=======================================================================
C     FORCES sur noeuds mains
C=======================================================================
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
      ENDDO                

C=======================================================================
C     FORCES PARITH ON sur noeud d'ancrage SECONDARY
C=======================================================================
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FXI, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FYI, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZI, FZ6)
#include "lockon.inc"
c     noeuds second
      DO I = 1,JLT
        IG = NSVG(I)
        IF(IG > 0)THEN
          IL = NSV(CN_LOC(I))
          DO K = 1,6
            DAANC6(1,K,IL) = DAANC6(1,K,IL) - FX6(K,I)
            DAANC6(2,K,IL) = DAANC6(2,K,IL) - FY6(K,I)
            DAANC6(3,K,IL) = DAANC6(3,K,IL) - FZ6(K,I)
          ENDDO
        ELSE
C
C      SPMD remote SECONDARYs
C
          IL = - IG
          DO K = 1,6
            DAANC6FI(NIN)%P(1,K,IL) = DAANC6FI(NIN)%P(1,K,IL) - FX6(K,I)
            DAANC6FI(NIN)%P(2,K,IL) = DAANC6FI(NIN)%P(2,K,IL) - FY6(K,I)
            DAANC6FI(NIN)%P(3,K,IL) = DAANC6FI(NIN)%P(3,K,IL) - FZ6(K,I)
          ENDDO
        ENDIF
      ENDDO

c     noeuds matre

      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX1, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY1, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ1, FZ6)
      DO I = 1,JLT
        IL = IX1L(I)
        DO K = 1,6
          DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
          DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
          DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
        ENDDO
      ENDDO

      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX2, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY2, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ2, FZ6)
      DO I = 1,JLT
        IL = IX2L(I)
        DO K = 1,6
          DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
          DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
          DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
        ENDDO
      ENDDO

      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX3, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY3, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ3, FZ6)
      DO I = 1,JLT
        IL = IX3L(I)
        DO K = 1,6
          DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
          DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
          DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
        ENDDO
      ENDDO

      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX4, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY4, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ4, FZ6)
      DO I = 1,JLT
        IL = IX4L(I)
        DO K = 1,6
          DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
          DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
          DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
        ENDDO
      ENDDO
#include "lockoff.inc"
C=======================================================================
C=======================================================================
C     mise a ZERO des FORCES sur noeuds mains et second
C     si PENE (sur noeud second) < GAPR (gap reel)
C=======================================================================
      DO I = 1,JLT
        IF(GAPV(I) > GAPR(I))THEN
          IG = NSVG(I)
          IF(IG > 0)THEN
            IL = NSV(CN_LOC(I))
            XSA = N1(I)*(DXANC(1,IL)-H1(I)*DXANC(1,IX1L(I))
     .                            -H2(I)*DXANC(1,IX2L(I))
     .                            -H3(I)*DXANC(1,IX3L(I))
     .                            -H4(I)*DXANC(1,IX4L(I)))
     .          + N2(I)*(DXANC(2,IL)-H1(I)*DXANC(2,IX1L(I))
     .                            -H2(I)*DXANC(2,IX2L(I))
     .                            -H3(I)*DXANC(2,IX3L(I))
     .                            -H4(I)*DXANC(2,IX4L(I)))
     .          + N3(I)*(DXANC(3,IL)-H1(I)*DXANC(3,IX1L(I))
     .                            -H2(I)*DXANC(3,IX2L(I))
     .                            -H3(I)*DXANC(3,IX3L(I))
     .                            -H4(I)*DXANC(3,IX4L(I)))
          ELSE
C
C      SPMD remote SECONDARYs
C
C      ********  ATTENTION DXANCFI a communiquer dans TRI20BOX ************
C
            IL = - IG
            XSA = N1(I)*(DXANCFI(NIN)%P(1,IL)-H1(I)*DXANC(1,IX1L(I))
     .                            -H2(I)*DXANC(1,IX2L(I))
     .                            -H3(I)*DXANC(1,IX3L(I))
     .                            -H4(I)*DXANC(1,IX4L(I)))
     .          + N2(I)*(DXANCFI(NIN)%P(2,IL)-H1(I)*DXANC(2,IX1L(I))
     .                            -H2(I)*DXANC(2,IX2L(I))
     .                            -H3(I)*DXANC(2,IX3L(I))
     .                            -H4(I)*DXANC(2,IX4L(I)))
     .          + N3(I)*(DXANCFI(NIN)%P(3,IL)-H1(I)*DXANC(3,IX1L(I))
     .                            -H2(I)*DXANC(3,IX2L(I))
     .                            -H3(I)*DXANC(3,IX3L(I))
     .                            -H4(I)*DXANC(3,IX4L(I)))
          END IF
          PS = PENE(I) - XSA - GAPV(I) + GAPR(I)
          IF(PS <= ZERO)THEN
            STIF(I) = ZERO
            FXI(I)  = ZERO
            FYI(I)  = ZERO
            FZI(I)  = ZERO
            FX1(I)  = ZERO
            FY1(I)  = ZERO
            FZ1(I)  = ZERO
            FX2(I)  = ZERO
            FY2(I)  = ZERO
            FZ2(I)  = ZERO
            FX3(I)  = ZERO
            FY3(I)  = ZERO
            FZ3(I)  = ZERO
            FX4(I)  = ZERO
            FY4(I)  = ZERO
            FZ4(I)  = ZERO
            IF (IFQ>0) THEN
              CAND_FX(INDEX(I)) = ZERO
              CAND_FY(INDEX(I)) = ZERO
              CAND_FZ(INDEX(I)) = ZERO
C             IFPEN(INDEX(I)) = 0    
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C=======================================================================
C     FORCES sur noeuds maites et second
C=======================================================================
C---------------------------------
      IF(INTTH == 0 .OR. IFORM == 0) THEN
         DO I=1,JLT            
          PHI1(I) = ZERO
          PHI2(I) = ZERO
          PHI3(I) = ZERO
          PHI4(I) = ZERO 
C
         ENDDO                
      ELSEIF(IFORM > 0) THEN
         DO I=1,JLT
          TM = H1(I)*TEMP(IX1G(I)) + H2(I)*TEMP(IX2G(I)) 
     .       + H3(I)*TEMP(IX3G(I)) + H4(I)*TEMP(IX4G(I))
           
          TS = TEMPI(I)
C          
          AX1 = XA(1,IX3L(I)) - XA(1,IX1L(I))
          AY1 = XA(2,IX3L(I)) - XA(2,IX1L(I))
          AZ1 = XA(3,IX3L(I)) - XA(3,IX1L(I))
          AX2 = XA(1,IX4L(I)) - XA(1,IX2L(I))
          AY2 = XA(2,IX4L(I)) - XA(2,IX2L(I))
          AZ2 = XA(3,IX4L(I)) - XA(3,IX2L(I))
C          
          AX  = AY1*AZ2 - AZ1*AY2
          AY  = AZ1*AX2 - AX1*AZ2
          AZ  = AX1*AY2 - AY1*AX2
C          
          AREA = ONE_OVER_8*SQRT(AX*AX+AY*AY+AZ*AZ)          
          PHI(I)  =  AREA* (TM - TS)*DT1 / RSTIF                       
          PHI1(I) = -PHI(I) *H1(I)
          PHI2(I) = -PHI(I) *H2(I)
          PHI3(I) = -PHI(I) *H3(I)
          PHI4(I) = -PHI(I) *H4(I)
         ENDDO        
      ENDIF 
C spmd : identification des noeuds interf. utiles a envoyer
      IF (NSPMD>1) THEN
Ctmp+1 mic only
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
      IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
        DTI=DT2T
        CALL I7SMS2(JLT   ,IX1G  ,IX2G ,IX3G ,IX4G ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3              NIN   ,NOINT ,MSKYI_SMS, ISKYI_SMS,NSMS  ,
     4              KT    ,C     ,CF   ,DTMINI,DTI )
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
      BID = ZERO
      IBID =0 
      IF(IPARIT==3)THEN
       IF(KDTINT==0)THEN
        CALL I7ASS3(JLT  ,IX1G  ,IX2G  ,IX3G  ,IX4G  ,
     2             NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3             FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4             FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5             FXI  ,FYI  ,FZI  ,A    ,STIFN)
       ELSE
        CALL I7ASS35(JLT  ,IX1G  ,IX2G  ,IX3G  ,IX4G  ,
     2                  NSVG ,H1   ,H2   ,H3   ,H4   ,STIF ,
     3                  FX1  ,FY1  ,FZ1  ,FX2  ,FY2  ,FZ2  ,
     4                  FX3  ,FY3  ,FZ3  ,FX4  ,FY4  ,FZ4  ,
     5                  FXI  ,FYI  ,FZI  ,A    ,STIFN,VISCN,
     6                  KS   ,K1   ,K2   ,K3   ,K4   ,CS   ,
     7                  C1   ,C2   ,C3   ,C4   )
       ENDIF
      ELSEIF(IPARIT==0)THEN
       IF(KDTINT==0)THEN
         CALL I7ASS0(JLT   ,IX1G  ,IX2G  ,IX3G  ,IX4G    ,
     2               NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3               FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4               FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5               FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6               INTTH ,PHI  ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7               PHI4  ,BID  ,BID  ,JTASK,IBID   )

       ELSE
C
         CALL I7ASS05(JLT   ,IX1G  ,IX2G  ,IX3G  ,IX4G    ,
     2                NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                FXI   ,FYI  ,FZI  ,A    ,STIFN  ,VISCN ,
     6                KS    ,K1   ,K2   ,K3   ,K4     ,CS    ,
     7                C1    ,C2   ,C3   ,C4   ,NIN    ,INTTH ,
     8                PHI   ,FTHE ,PHI1 , PHI2  ,PHI3 , PHI4,JTASK,
     9                BID   ,BID  ,IBID )
       ENDIF
C
      ELSE
        IF(KDTINT==0)THEN
          CALL I7ASS2(JLT   ,IX1G   ,IX2G  ,IX3G  ,IX4G  ,
     2                NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3                FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4                FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5                FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6                NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI,PHI1,
     7                PHI2  ,PHI3  , PHI4,BID  ,BID    ,
     A                IBID)
        ELSE
          CALL I7ASS25(JLT   ,IX1G  ,IX2G  ,IX3G  ,IX4G    ,
     2                 NSVG  ,H1   ,H2   ,H3   ,H4     ,
     3                 FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                 FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                 FXI   ,FYI  ,FZI  ,FSKYI,NISKYFI,NIN  ,
     6                 KS    ,K1   ,K2   ,K3   ,K4     ,CS   ,
     7                 C1    ,C2   ,C3   ,C4   ,ISKY   ,NOINT ,
     8                 INTTH ,PHI  ,FTHESKYI,PHI1,PHI2 ,PHI3, 
     9                 PHI4  ,BID  ,BID    ,IBID )
        ENDIF
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0)THEN
#include "lockon.inc"
c         goto 1234
           DO I=1,JLT
            FCONT(1,IX1G(I)) =FCONT(1,IX1G(I)) + FX1(I)
            FCONT(2,IX1G(I)) =FCONT(2,IX1G(I)) + FY1(I)
            FCONT(3,IX1G(I)) =FCONT(3,IX1G(I)) + FZ1(I)
            FCONT(1,IX2G(I)) =FCONT(1,IX2G(I)) + FX2(I)
            FCONT(2,IX2G(I)) =FCONT(2,IX2G(I)) + FY2(I)
            FCONT(3,IX2G(I)) =FCONT(3,IX2G(I)) + FZ2(I)
            FCONT(1,IX3G(I)) =FCONT(1,IX3G(I)) + FX3(I)
            FCONT(2,IX3G(I)) =FCONT(2,IX3G(I)) + FY3(I)
            FCONT(3,IX3G(I)) =FCONT(3,IX3G(I)) + FZ3(I)
            FCONT(1,IX4G(I)) =FCONT(1,IX4G(I)) + FX4(I)
            FCONT(2,IX4G(I)) =FCONT(2,IX4G(I)) + FY4(I)
            FCONT(3,IX4G(I)) =FCONT(3,IX4G(I)) + FZ4(I)
            JG = NSVG(I)
            IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              FCONT(1,JG)=FCONT(1,JG)- FXI(I)
              FCONT(2,JG)=FCONT(2,JG)- FYI(I)
              FCONT(3,JG)=FCONT(3,JG)- FZI(I)
            ENDIF
           ENDDO
c 1234    continue
#include "lockoff.inc"
      ENDIF
C-----------------------------------------------------
      IF(ISECIN>0)THEN
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
C attention aux signes pour le cumul des efforts
C a rendre conforme avec CFORC3
                 IF(SECFCUM(4,IX1G(K),I)==1.)THEN
                  SECFCUM(1,IX1G(K),I)=SECFCUM(1,IX1G(K),I)-FX1(K)
                  SECFCUM(2,IX1G(K),I)=SECFCUM(2,IX1G(K),I)-FY1(K)
                  SECFCUM(3,IX1G(K),I)=SECFCUM(3,IX1G(K),I)-FZ1(K)
                ENDIF
                IF(SECFCUM(4,IX2G(K),I)==1.)THEN
                  SECFCUM(1,IX2G(K),I)=SECFCUM(1,IX2G(K),I)-FX2(K)
                  SECFCUM(2,IX2G(K),I)=SECFCUM(2,IX2G(K),I)-FY2(K)
                  SECFCUM(3,IX2G(K),I)=SECFCUM(3,IX2G(K),I)-FZ2(K)
                ENDIF
                IF(SECFCUM(4,IX3G(K),I)==1.)THEN
                  SECFCUM(1,IX3G(K),I)=SECFCUM(1,IX3G(K),I)-FX3(K)
                  SECFCUM(2,IX3G(K),I)=SECFCUM(2,IX3G(K),I)-FY3(K)
                  SECFCUM(3,IX3G(K),I)=SECFCUM(3,IX3G(K),I)-FZ3(K)
                ENDIF
                IF(SECFCUM(4,IX4G(K),I)==1.)THEN
                  SECFCUM(1,IX4G(K),I)=SECFCUM(1,IX4G(K),I)-FX4(K)
                  SECFCUM(2,IX4G(K),I)=SECFCUM(2,IX4G(K),I)-FY4(K)
                  SECFCUM(3,IX4G(K),I)=SECFCUM(3,IX4G(K),I)-FZ4(K)
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

      IF(IBAG/=0.OR.IADM/=0)THEN
       DO I=1,JLT

C       IF(PENE(I)/=ZERO)THEN
C test modifie pour coherence avec communication SPMD (spmd_i7tools)
        IF(FXI(I)/=ZERO.OR.FYI(I)/=ZERO.OR.FZI(I)/=ZERO)THEN

          JG = NSVG(I)
          IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
            ICONTACT(JG)=1
          ENDIF

          ICONTACT(IX1G(I))=1
          ICONTACT(IX2G(I))=1
          ICONTACT(IX3G(I))=1
          ICONTACT(IX4G(I))=1
        ENDIF
       ENDDO
      ENDIF

      IF(IADM/=0)THEN
       DO I=1,JLT
         JG = NSVG(I)
#include "lockon.inc"
         IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
           RCONTACT(JG)=MIN(RCONTACT(JG),RCURVI(I))
         END IF
         RCONTACT(IX1G(I))=MIN(RCONTACT(IX1G(I)),RCURVI(I))
         RCONTACT(IX2G(I))=MIN(RCONTACT(IX2G(I)),RCURVI(I))
         RCONTACT(IX3G(I))=MIN(RCONTACT(IX3G(I)),RCURVI(I))
         RCONTACT(IX4G(I))=MIN(RCONTACT(IX4G(I)),RCURVI(I))
#include "lockoff.inc"
       END DO
      END IF
      IF(IADM>=2)THEN
       DO I=1,JLT
         JG = NSVG(I)
#include "lockon.inc"
         IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
           PCONTACT(JG)=MAX(PCONTACT(JG),PENE(I)/(PADM*GAPV(I)))
           ACONTACT(JG)=MIN(ACONTACT(JG),ANGLMI(I))
         END IF
#include "lockoff.inc"
       END DO
      END IF

      IF(IBCC==0) RETURN
C
      DO 400 I=1,JLT

        IF(PENE(I)==ZERO)GOTO 400
        IBCM = IBCC / 8
        IBCS = IBCC - 8 * IBCM
        IF(IBCS>0) THEN
          IG=NSVG(I)
          IF(IG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
            CALL IBCOFF(IBCS,ICODT(IG))
          ENDIF
        ENDIF
        IF(IBCM>0) THEN
         IG=IX1G(I)
         CALL IBCOFF(IBCM,ICODT(IG))
         IG=IX2G(I)
         CALL IBCOFF(IBCM,ICODT(IG))
         IG=IX3G(I)
         CALL IBCOFF(IBCM,ICODT(IG))
         IG=IX4G(I)
         CALL IBCOFF(IBCM,ICODT(IG))
        ENDIF
 400  CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  I20FOR3C                      source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20FOR3C(NLN   ,NLG  ,MS     ,DXANC ,
     2                    DVANC ,STFA ,WEIGHT ,INACTI,
     3                    DAANC6,STFAC,PENIA  ,ALPHAK,
     4                    DAANC ,KMIN  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE ICONTACT_MOD
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
#include      "scr11_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*),NLN,INACTI,NLG(*)
      my_real
     .       DVANC(3,*),DXANC(3,*),DAANC(3,*),STFA(*),PENIA(5,*),
     .       STFAC,MS(*),ALPHAK(3,*),KMIN
      DOUBLE PRECISION
     .       DAANC6(3,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J

      my_real
     .   DDX,STFR,VISR,UNSDT2,FX,FY,FZ,ECONTT, ECONVT,
     .   FXR(NLN), FYR(NLN), FZR(NLN),DX(NLN),DY(NLN),DZ(NLN)
      DOUBLE PRECISION
     .   FX6(6,NLN), FY6(6,NLN), FZ6(6,NLN)

C-----------------------------------------------

      UNSDT2 = DT2/MAX(DT2*DT2,EM30)

C----------------------------------------------------------------------
C     penetration initiale
C
C     PENIA(1:3,I) vecteur directeur normee
C     PENIA(4,I) module de la penetration initiale
C     PENIA(4,I) module corrigee pour le cycle suivant
C----------------------------------------------------------------------
      IF(INACTI >= 5)THEN
        DO I = 1,NLN
          DX(I) = DXANC(1,I) - PENIA(1,I)*PENIA(4,I)
          DY(I) = DXANC(2,I) - PENIA(2,I)*PENIA(4,I)
          DZ(I) = DXANC(3,I) - PENIA(3,I)*PENIA(4,I)
          DDX = DX(I)*PENIA(1,I) + DY(I)*PENIA(2,I) + DZ(I)*PENIA(3,I)
          DDX = HALF*MIN(DDX,ZERO)
          PENIA(5,I) = MAX(ZERO,PENIA(5,I),PENIA(4,I)+DDX)
c a faire ici ou dans I20BUCE_CRIT IF(PENIA(5,I) /= ZERO)NACTI=NACTI+1
        ENDDO
      ELSE
        DO I = 1,NLN
          DX(I) = DXANC(1,I)
          DY(I) = DXANC(2,I)
          DZ(I) = DXANC(3,I)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
C     NOEUDS MAIN SECONDARY edge
C----------------------------------------------------------------------

      ECONTT = ZERO
      ECONVT = ZERO
      ECONTV = ZERO
      DO I = 1,NLN
        J = NLG(I)
          IF(STFAC > ZERO)THEN
c            STFR = HALF * STFAC * ABS(STFA(I)) 
            STFR = HALF * MAX(KMIN,STFAC*ABS(STFA(I))) * ALPHAK(1,I)
          ELSE
c            STFR = HALF * ABS(STFAC) 
            STFR = HALF * ABS(STFAC) * ALPHAK(1,I)
          ENDIF 
c essai viscosite critique 
c          VISR = 0.1 * TWO * SQRT(STFR * MS(J))
          VISR = TWO * SQRT(STFR * MS(J))

          FX = STFR * DX(I) 
          FY = STFR * DY(I) 
          FZ = STFR * DZ(I) 
          FXR(I) = FX + VISR * DVANC(1,I)
          FYR(I) = FY + VISR * DVANC(2,I)
          FZR(I) = FZ + VISR * DVANC(3,I)
          IF(FX /= ZERO)ALPHAK(3,I)=MIN(ALPHAK(3,I),FXR(I)/FX)
          IF(FY /= ZERO)ALPHAK(3,I)=MIN(ALPHAK(3,I),FYR(I)/FY)
          IF(FZ /= ZERO)ALPHAK(3,I)=MIN(ALPHAK(3,I),FZR(I)/FZ)
          DAANC(1,I) = - FXR(I)
          DAANC(2,I) = - FYR(I)
          DAANC(3,I) = - FZR(I)

        IF(WEIGHT(J) == 1)THEN
          ECONTT = ECONTT + HALF*STFR*(DX(I)**2+DY(I)**2+DZ(I)**2)
          ECONVT = ECONVT
     .           + VISR*(DVANC(1,I)**2+DVANC(2,I)**2+DVANC(3,I)**2)
        ENDIF
      ENDDO

#include "lockon.inc"
      ECONTV = ECONTV + ECONVT*DT1
      ECONT  = ECONT + ECONTT
#include "lockoff.inc"

      RETURN
      END
Chd|====================================================================
Chd|  I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        I20ASS0                       source/interfaces/int20/i20for3.F
Chd|        I20ASS05                      source/interfaces/int20/i20for3.F
Chd|        I20ASS2                       source/interfaces/int20/i20for3.F
Chd|        I20ASS25                      source/interfaces/int20/i20for3.F
Chd|        I20SMS2E                      source/interfaces/int20/i20sms2.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20FOR3E(
     1                  JLT    ,A      ,V      ,IBC     ,ICODT  ,
     2                  FSAV   ,GAP    ,FRIC   ,MS      ,VISC   ,
     3                  VISCF  ,NOINT  ,ITAB   ,CS_LOC  ,CM_LOC ,
     4                  STIGLO ,STIFN  ,STIF   ,FSKYI   ,ISKY   ,
     5                  FCONT  ,STFS   ,STFM   ,DT2T    ,HS1    ,
     6                  HS2    ,HM1    ,HM2    ,N1      ,N2     , 
     7                  M1     ,M2     ,IVIS2  ,NELTST  ,ITYPTST,
     8                  NX     ,NY     ,NZ     ,GAPV    ,PENISE  , 
     9                  PENIME ,INACTI,NISKYFIE,NEWFRONT,ISECIN ,
     A                  NSTRF  ,SECFCUM,VISCN  ,NLINSA  ,MS1    ,
     B                  MS2    ,MM1    ,MM2    ,VXS1    ,VYS1   ,
     C                  VZS1   ,VXS2   ,VYS2   ,VZS2    ,VXM1   ,
     D                  VYM1   ,VZM1   ,VXM2   ,VYM2    ,VZM2   ,
     E                  NIN    ,N1L    ,N2L    ,M1L     ,M2L    ,
     F                  DAANC6 ,ALPHAK ,MSKYI_SMS,ISKYI_SMS,NSMS,
     G                  JTASK  ,ISENSINT,  FSAVPARIT ,NISUB ,NFT,
     H                  H3D_DATA)
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
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,IBC,IVIS2,INACTI,NLINSA,NISKYFIE,NIN
      INTEGER ICODT(*), ITAB(*), ISKY(*),
     .        NOINT,NEWFRONT,ISECIN, NSTRF(*), ISKYI_SMS(*)
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ),
     .        N1L(MVSIZ),N2L(MVSIZ),M1L(MVSIZ),M2L(MVSIZ),
     .        CS_LOC(MVSIZ), CM_LOC(MVSIZ), NSMS(MVSIZ),JTASK, 
     .        ISENSINT(*),NISUB,NFT
      my_real
     .   STIGLO, 
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   STFS(*),STFM(*),STIFN(*),FSKYI(LSKYI,NFSKYI),GAPV(*),
     .   PENISE(2,*), PENIME(2,*),ALPHAK(3,*), MSKYI_SMS(*),
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*),
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ),
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),FSAVPARIT(NISUB+1,11,*)
      DOUBLE PRECISION DAANC6(3,6,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J , K0,NBINTER,K1S,K, NI, IL, IG
      INTEGER NISKYL,NISKYL1,ISIGN
      my_real
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   PENE(MVSIZ),MASMIN(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ),
     .   VNX, VNY, VNZ, AA, VMAX,S2,DIST,RDIST,
     .   V2, FM2, DT1INV, VISCA,  FAC,  FF,
     .   FX, FY, FZ, F2, MAS2, DTMI0,DTI,
     .   FACM1, ECONTT, ECONVT, A2,MASM,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6,
     .   DTI2, PPLUS
      my_real PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,AAA
      DOUBLE PRECISION 
     .   FX6(6,MVSIZ), FY6(6,MVSIZ), FZ6(6,MVSIZ)
C-----------------------------------------------
#include "sync_dir.inc"
      IF (IRESP == 1) THEN
         PREC = FIVEEM4
      ELSE 
         PREC = EM10
      ENDIF
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO     
C
      DO I=1,JLT
         S2 = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)
         PENE(I) = GAPV(I) - S2
         S2 = ONE/MAX(EM30,S2)
         NX(I) = NX(I)*S2
         NY(I) = NY(I)*S2
         NZ(I) = NZ(I)*S2
      ENDDO
C
      IF(INACTI==5.or.INACTI==6)THEN
#include "lockon.inc"
        DO I=1,JLT
         PPLUS=HALF*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I)))
         IF(CS_LOC(I)<=NLINSA) THEN
           PENISE(2,CS_LOC(I)) = MAX(PENISE(2,CS_LOC(I)),PPLUS)
         ELSE
           NI = CS_LOC(I)-NLINSA
           PENFIE(NIN)%P(2,NI) = MAX(PENFIE(NIN)%P(2,NI),PPLUS)
         END IF
         PENIME(2,CM_LOC(I)) = MAX(PENIME(2,CM_LOC(I)),PPLUS)
        ENDDO
#include "lockoff.inc"
        DO I=1,JLT
         IF(CS_LOC(I)<=NLINSA) THEN
           PENE(I) = PENE(I) - PENISE(1,CS_LOC(I)) - PENIME(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           IF(PENE(I)==ZERO)STIF(I)=ZERO
           GAPV(I) = GAPV(I) - PENISE(1,CS_LOC(I)) - PENIME(1,CM_LOC(I))
         ELSE
           NI = CS_LOC(I)-NLINSA
           PENE(I) = PENE(I) - PENFIE(NIN)%P(1,NI) - PENIME(1,CM_LOC(I))
           PENE(I) = MAX(PENE(I),ZERO)
           IF(PENE(I)==ZERO)STIF(I)=ZERO
           GAPV(I) = GAPV(I) - PENFIE(NIN)%P(1,NI) - PENIME(1,CM_LOC(I))
         END IF
        END DO
      ENDIF

      VMAX = ZERO
      DO I=1,JLT
        GAPV(I) = ZEP9*GAPV(I)
        VX(I) = HS1(I)*VXS1(I) + HS2(I)*VXS2(I)
     .        - HM1(I)*VXM1(I) - HM2(I)*VXM2(I)
        VY(I) = HS1(I)*VYS1(I) + HS2(I)*VYS2(I)
     .        - HM1(I)*VYM1(I) - HM2(I)*VYM2(I)
        VZ(I) = HS1(I)*VZS1(I) + HS2(I)*VZS2(I)
     .        - HM1(I)*VZM1(I) - HM2(I)*VZM2(I)
        VN(I) = NX(I)*VX(I) + NY(I)*VY(I) + NZ(I)*VZ(I)
      ENDDO
C-------------------------------------------
      DO I=1,JLT
       FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
       FACM1 = ONE/FAC
       IF(( (GAPV(I)-PENE(I))/GAPV(I) )<PREC .AND. 
     .                                STIF(I)>ZERO ) THEN
         STIF(I) = ZERO
        IF (IMPL_S==0) THEN 
         NEWFRONT = -1
#include "lockon.inc"
         IF(CS_LOC(I)<=NLINSA)THEN
           STFS(CS_LOC(I)) = -ABS(STFS(CS_LOC(I)))
           WRITE(ISTDO,*)'WARNING INTERFACE NB',NOINT
           WRITE(ISTDO,*)'LINE CONNECTING NODES ',ITAB(N1(I)),
     .               ITAB(N2(I)),'DE-ACTIVATED FROM INTERFACE'                 
           WRITE(ISTDO,*)'IMPACTED ON ',ITAB(M1(I)),ITAB(M2(I))                 
           WRITE(IOUT,*)'WARNING INTERFACE NB',NOINT
           WRITE(IOUT,*)'GAP=',GAPV(I),'PENE=',PENE(I)
           WRITE(IOUT,*)'LINE CONNECTING NODES ',ITAB(N1(I)),
     .               ITAB(N2(I)),'DE-ACTIVATED FROM INTERFACE'
           WRITE(IOUT,*)'IMPACTED ON ',ITAB(M1(I)),ITAB(M2(I))                 
         ELSE
           NI = CS_LOC(I)-NLINSA
           STIFIE(NIN)%P(NI) = -ABS(STIFIE(NIN)%P(NI))
           WRITE(ISTDO,*)'WARNING INTERFACE NB',NOINT
           WRITE(ISTDO,*)'LINE CONNECTING NODES ',ITAFIE(NIN)%P(N1(I)),
     .           ITAFIE(NIN)%P(N2(I)),'DE-ACTIVATED FROM INTERFACE'                 
           WRITE(IOUT,*)'WARNING INTERFACE NB',NOINT
           WRITE(IOUT,*)'GAP=',GAPV(I),'PENE=',PENE(I)
           WRITE(IOUT,*)'LINE CONNECTING NODES ',ITAFIE(NIN)%P(N1(I)),
     .           ITAFIE(NIN)%P(N2(I)),'DE-ACTIVATED FROM INTERFACE'
         END IF
#include "lockoff.inc"
        ENDIF  
         PENE(I)= ZERO
       ENDIF
       ECONTT = ECONTT + HALF*STIF(I)*GAPV(I)**2 *( FACM1 - ONE - 
     .        LOG(FACM1) )
       STIF(I) = HALF*STIF(I) * FAC
       FNI(I)= -STIF(I) * PENE(I) 
      ENDDO

      DTI = EP20
C
      DO I=1,JLT
          DIST=GAPV(I)-PENE(I)
          RDIST  = HALF*DIST / MAX(EM30,-VN(I))
          DTI = MIN(RDIST,DTI)
      ENDDO
C
      IF(DTI<=DTMIN1(10))THEN
        DO I=1,JLT
            DIST=GAPV(I)-PENE(I)
            DTI2   = HALF*DIST / MAX(EM30,-VN(I))
            IF(DTI2<=DTMIN1(10))THEN
#include "lockon.inc"
             IF(CS_LOC(I)<=NLINSA)THEN
               WRITE(IOUT,*)
     .            ' **WARNING MINIMUM TIME STEP ',DTI2,
     .            'IN INTERFACE NB',NOINT
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I)) 
             ELSE
               WRITE(IOUT,*)
     .            ' **WARNING MINIMUM TIME STEP ',DTI2,
     .            'IN INTERFACE NB',NOINT
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAFIE(NIN)%P(N1(I)),
     .             ITAFIE(NIN)%P(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
             END IF
#include "lockoff.inc"
             IF(IDTMIN(10)==1)THEN
               TSTOP = TT
             ELSEIF(IDTMIN(10)==2)THEN
#include "lockon.inc"
               WRITE(IOUT,*)'REMOVE SECONDARY LINE FROM INTERFACE'                 
               IF(CS_LOC(I)<=NLINSA)THEN
                 STFS(CS_LOC(I)) = -ABS(STFS(CS_LOC(I)))
               ELSE
                 NI = CS_LOC(I)-NLINSA
                 STIFIE(NIN)%P(NI) = -ABS(STIFIE(NIN)%P(NI))
               END IF
#include "lockoff.inc"
               NEWFRONT = -1
               STIF(I) = ZERO
               DTI = DTMIN1(10)
             ELSEIF(IDTMIN(10)==5)THEN
               MSTOP = 2
             ENDIF                
            ENDIF               
        ENDDO
      ENDIF
C
      IF(DTI<DT2T)THEN
        DT2T    = DTI
        NELTST  = NOINT
        ITYPTST = 10
      ENDIF
C---------------------------------
C     DAMPING + FRIC
C---------------------------------
      IF(VISC/=ZERO.OR.VISCF/=ZERO)THEN
          DO I=1,JLT
           MAS2  = MS1(I)*HS1(I)
     .           + MS2(I)*HS2(I)
           MASM  = MM1(I)*HM1(I)
     .           + MM2(I)*HM2(I)
           MASMIN(I) = MIN(MAS2,MASM)
           VIS2(I) = TWO * STIF(I) * MIN(MAS2,MASM)
          ENDDO
      ENDIF
C---------------------------------
      IF(VISC/=ZERO)THEN
        IF(IVIS2==0.OR.IVIS2==1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT
            IF(VN(I)<ZERO)
     .      VIS2(I) = VIS2(I)/(MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)))
          ENDDO
C---------------------------------
          VISCA = ZEP4
          IF(KDTINT==0.AND.IDTMINS/=2)THEN
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF  = FAC * (
     .        VISC * VIS +
     .        VISCA**2 * TWO * MASMIN(I) * MAX(ZERO,-VN(I)) / 
     .                MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I)/MAX((GAPV(I)-PENE(I)),EM10) 
            STIF(I) = STIF(I) + FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
            FF = MIN(FF * VN(I),-FNI(I))  
c           FF = MIN(FF * VN(I),ZERO)  
            FNI(I)  = FNI(I) + FF
cc          ECONVT = ECONVT + FF * VN(I) * DT1
           ENDDO

          ELSE
           DO I=1,JLT
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            C(I)= FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MASMIN(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10)
            KT(I)   = STIF(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF = MIN(C(I) * VN(I),-FNI(I))  
c           FF = MIN(FF * VN(I),ZERO)  
            FNI(I)  = FNI(I) + FF
            CF(I)   = FAC*SQRT(VISCF)*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
cc          ECONVT = ECONVT + C(I) * VN(I) * DT1
           ENDDO
          ENDIF

        ELSEIF(IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
          DO I=1,JLT
            VIS2(I) = VIS2(I)/( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)))
          ENDDO
C---------------------------------
          VISCA = HALF
          DO I=1,JLT
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * TWO * MASMIN(I) * ABS(VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I)-PENE(I)),EM10) 
           STIF(I) = STIF(I) + TWO * FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
           FF = MIN(FF * VN(I),-FNI(I))  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * ( VISC * VIS ) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I)-PENE(I)),EM10) 
           STIF(I) = STIF(I) + TWO * FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
           FF = MIN(FF * VN(I),-FNI(I))  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
          DO I=1,JLT
           VIS = SQRT(VIS2(I)) 
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I)-PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF)*VIS*DT1INV)
          ENDDO
        ELSEIF(IVIS2==5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT
           MAS2  = MS1(I)*HS1(I)
     .           + MS2(I)*HS2(I)
           MASM  = MM1(I)*HM1(I)
     .           + MM2(I)*HM2(I)
           VIS = 2. * VISC * DT1INV * MASM * MAS2 / 
     .          MAX(EM30,MASM+MAS2)
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) -PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCF*VIS2(I))*DT1INV)
           FF = VIS * VN(I)  
           ECONVT = ECONVT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
           FNI(I)  = MIN(FNI(I),FF)
          ENDDO
        ELSE
        ENDIF
      ELSE
         DO I=1,JLT
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
         ENDDO
      ENDIF
C---------------------------------
C     REDUCTION RIGIDITE ANCRAGE
C---------------------------------
#include "lockon.inc"
      DO I=1,JLT
        ISIGN=1
        IF(PENE(I)>ZERO)ISIGN=-1
        AAA = ONE-PENE(I)/GAPV(I)
        IL = M1L(I)
        IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
        ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        IL = M2L(I)
        IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
        ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        IF(CS_LOC(I) <= NLINSA)THEN
          IL = N1L(I)
          IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
          ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
          IL = N2L(I)
          IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
          ALPHAK(2,IL)=ISIGN*MIN(ABS(ALPHAK(2,IL)),AAA)
        ELSE
C         SPMD remote SECONDARYs
          IL = N1(I)
          IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
          ALPHAKFIE(NIN)%P(IL)=ISIGN*MIN(ABS(ALPHAKFIE(NIN)%P(IL)),AAA)
          IL = N2(I)
          IF(PENE(I)>ZERO.OR.ALPHAK(2,IL)<ZERO)ISIGN=-1
          ALPHAKFIE(NIN)%P(IL)=ISIGN*MIN(ABS(ALPHAKFIE(NIN)%P(IL)),AAA)
        ENDIF
      ENDDO
#include "lockoff.inc"
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION NORMALE
C---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      DO I=1,JLT
       FXI(I)=NX(I)*FNI(I)
       FYI(I)=NY(I)*FNI(I)
       FZI(I)=NZ(I)*FNI(I)
       FSAV1=FSAV1+FXI(I)*DT12
       FSAV2=FSAV2+FYI(I)*DT12
       FSAV3=FSAV3+FZI(I)*DT12
      ENDDO
       IF (IMCONV==1) THEN 
#include "lockon.inc"
        FSAV(1)=FSAV(1)+FSAV1
        FSAV(2)=FSAV(2)+FSAV2
        FSAV(3)=FSAV(3)+FSAV3
#include "lockoff.inc"
       ENDIF 
      IF(ISENSINT(1)/=0) THEN
	DO I=1,JLT
          FSAVPARIT(1,1,I+NFT) =  FXI(I)
          FSAVPARIT(1,2,I+NFT) =  FYI(I)
          FSAVPARIT(1,3,I+NFT) =  FZI(I)
	ENDDO
      ENDIF
C---------------------------------
C     FRICTION
C---------------------------------
      IF(FRIC*VISCF/=0.)THEN
       FSAV4 = ZERO
       FSAV5 = ZERO
       FSAV6 = ZERO     
       DO I=1,JLT
        VNX = NX(I)*VN(I)
        VNY = NY(I)*VN(I)
        VNZ = NZ(I)*VN(I)
        VX(I) = VX(I) - VNX
        VY(I) = VY(I) - VNY
        VZ(I) = VZ(I) - VNZ
        V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
        VIS2(I) = VISCF * VIS2(I)
        FM2  = (FRIC*FNI(I))**2
        F2   = VIS2(I) * V2
        A2 = MIN(F2,FM2) / MAX(EM30,F2)
        AA = SQRT(A2 * VIS2(I))
        FX = AA * VX(I)
        FY = AA * VY(I)
        FZ = AA * VZ(I)
        FSAV4 = FSAV4 + FX*DT12
        FSAV5 = FSAV5 + FY*DT12
        FSAV6 = FSAV6 + FZ*DT12
        FXI(I)=FXI(I) + FX
        FYI(I)=FYI(I) + FY
        FZI(I)=FZI(I) + FZ
        ECONVT = ECONVT + AA * V2 * DT1
       ENDDO
       IF (IMCONV==1) THEN 
#include "lockon.inc"
         FSAV(4) = FSAV(4) + FSAV4
         FSAV(5) = FSAV(5) + FSAV5
         FSAV(6) = FSAV(6) + FSAV6
#include "lockoff.inc"
       ENDIF 
       IF(ISENSINT(1)/=0) THEN
	 DO I=1,JLT
           FM2  = (FRIC*FNI(I))**2
           F2   = VIS2(I) * V2
           A2 = MIN(F2,FM2) / MAX(EM30,F2)
           AA = SQRT(A2 * VIS2(I))
           FSAVPARIT(1,4,I+NFT) =  AA * VX(I)
           FSAVPARIT(1,5,I+NFT) =  AA * VY(I)
           FSAVPARIT(1,6,I+NFT) =  AA * VZ(I)
	 ENDDO
       ENDIF
      ENDIF
C
       IF (IMCONV==1) THEN 
#include "lockon.inc"
        ECONTV = ECONTV + ECONVT
        ECONT  = ECONT + ECONTT
#include "lockoff.inc"
       ENDIF 
C---------------------------------
      DO I=1,JLT
       FX1(I)=-FXI(I)*HS1(I)
       FY1(I)=-FYI(I)*HS1(I)
       FZ1(I)=-FZI(I)*HS1(I)
C
       FX2(I)=-FXI(I)*HS2(I)
       FY2(I)=-FYI(I)*HS2(I)
       FZ2(I)=-FZI(I)*HS2(I)
C
       FX3(I)=FXI(I)*HM1(I)
       FY3(I)=FYI(I)*HM1(I)
       FZ3(I)=FZI(I)*HM1(I)
C
       FX4(I)=FXI(I)*HM2(I)
       FY4(I)=FYI(I)*HM2(I)
       FZ4(I)=FZI(I)*HM2(I)
C
      ENDDO
C
      IF (NSPMD>1) THEN
Ctmp+1 mic only
#include "mic_lockon.inc"
        DO I = 1,JLT
          IF(CS_LOC(I)>NLINSA)THEN
            NI = CS_LOC(I)-NLINSA
C tag temporaire de NSVFI a -
            NSVFIE(NIN)%P(NI) = -ABS(NSVFIE(NIN)%P(NI))
          ENDIF
        ENDDO
ctmp+1 mic only
#include "mic_lockoff.inc"
      ENDIF
C
      DO I=1,JLT
           STIF(I) = TWO*STIF(I)
      ENDDO
C
C---------------------------------
      IF(KDTINT==1.OR.IDTMINS==2)THEN
       IF(     (VISC/=ZERO)
     .   .AND.(IVIS2==0.OR.IVIS2==1))THEN
        DO I=1,JLT
         CX= C(I)*C(I)
C
         IF(MS1(I)==ZERO)THEN
          K1(I) =ZERO
          C1(I) =ZERO
         ELSE
          K1(I)=KT(I)*ABS(HS1(I))
          C1(I)=C(I)*ABS(HS1(I))
          CX   =FOUR*C1(I)*C1(I)
          CY   =EIGHT*MS1(I)*K1(I)
          AUX   = SQRT(CX+CY)+TWO*C1(I)
          ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(HS1(I))
          AUX   = TWO*CFI*CFI/MAX(MS1(I),EM20)
          IF(AUX>ST1(I))THEN
           K1(I) =ZERO
           C1(I) =CFI
          ENDIF         
         ENDIF         
C
         IF(MS2(I)==ZERO)THEN
          K2(I) =ZERO
          C2(I) =ZERO
         ELSE
          K2(I)=KT(I)*ABS(HS2(I))
          C2(I)=C(I)*ABS(HS2(I))
          CX   =FOUR*C2(I)*C2(I)
          CY   =EIGHT*MS2(I)*K2(I)
          AUX   = SQRT(CX+CY)+TWO*C2(I)
          ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(HS2(I))
          AUX   = TWO*CFI*CFI/MAX(MS2(I),EM20)
          IF(AUX>ST2(I))THEN
           K2(I) =ZERO
           C2(I) =CFI
          ENDIF         
         ENDIF         
C
       IF(MM1(I)==ZERO)THEN
          K3(I) =ZERO
          C3(I) =ZERO
         ELSE
          K3(I)=KT(I)*ABS(HM1(I))
          C3(I)=C(I)*ABS(HM1(I))
          CX   =FOUR*C3(I)*C3(I)
          CY   =EIGHT*MM1(I)*K3(I)
          AUX   = SQRT(CX+CY)+TWO*C3(I)
          ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(HM1(I))
          AUX   = TWO*CFI*CFI/MAX(MM1(I),EM20)
          IF(AUX>ST3(I))THEN
           K3(I) =ZERO
           C3(I) =CFI
          ENDIF         
         ENDIF         
C
         IF(MM2(I)==ZERO)THEN
          K4(I) =ZERO
          C4(I) =ZERO
         ELSE
          K4(I)=KT(I)*ABS(HM2(I))
          C4(I)=C(I)*ABS(HM2(I))
          CX   =FOUR*C4(I)*C4(I)
          CY   =EIGHT*MM2(I)*K4(I)
          AUX   = SQRT(CX+CY)+TWO*C4(I)
          ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(HM2(I))
          AUX   = TWO*CFI*CFI/MAX(MM2(I),EM20)
          IF(AUX>ST4(I))THEN
           K4(I) =ZERO
           C4(I) =CFI
          ENDIF         
         ENDIF         
        ENDDO
       ELSE
        DO I=1,JLT
          K1(I) =STIF(I)*ABS(HS1(I))
          C1(I) =ZERO
          K2(I) =STIF(I)*ABS(HS2(I))
          C2(I) =ZERO
          K3(I) =STIF(I)*ABS(HM1(I))
          C3(I) =ZERO
          K4(I) =STIF(I)*ABS(HM2(I))
          C4(I) =ZERO
        ENDDO
       ENDIF
      ENDIF
C=======================================================================
C     FORCES PARITH ON sur noeud d'ancrage SECOND
C=======================================================================
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX1, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY1, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ1, FZ6)
#include "lockon.inc"
      DO I = 1,JLT
        IF(CS_LOC(I)<=NLINSA)THEN
          IL = N1L(I)
C        IG = N1(I)
C        IF(IG > 0)THEN
          DO K = 1,6
            DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
            DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
            DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
          ENDDO
        ELSE
C         SPMD remote SECONDARYs
C          IL = - IG
          IL = N1(I)
          DO K = 1,6
            DAANC6FIE(NIN)%P(1,K,IL) = DAANC6FIE(NIN)%P(1,K,IL)
     .                               + FX6(K,I)
            DAANC6FIE(NIN)%P(2,K,IL) = DAANC6FIE(NIN)%P(2,K,IL)
     .                               + FY6(K,I)
            DAANC6FIE(NIN)%P(3,K,IL) = DAANC6FIE(NIN)%P(3,K,IL)
     .                               + FZ6(K,I)
          ENDDO
        ENDIF
      ENDDO
#include "lockoff.inc"
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX2, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY2, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ2, FZ6)
#include "lockon.inc"
      DO I = 1,JLT
        IF(CS_LOC(I)<=NLINSA)THEN
          IL = N2L(I)
C        IG = N2(I)
C        IF(IG > 0)THEN
          DO K = 1,6
            DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
            DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
            DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
          ENDDO
        ELSE
C         SPMD remote SECONDARYs
          IL = N2(I)
C          IL = - IG
          DO K = 1,6
            DAANC6FIE(NIN)%P(1,K,IL) = DAANC6FIE(NIN)%P(1,K,IL)
     .                               + FX6(K,I)
            DAANC6FIE(NIN)%P(2,K,IL) = DAANC6FIE(NIN)%P(2,K,IL)
     .                               + FY6(K,I)
            DAANC6FIE(NIN)%P(3,K,IL) = DAANC6FIE(NIN)%P(3,K,IL)
     .                               + FZ6(K,I)
          ENDDO
        ENDIF
      ENDDO
#include "lockoff.inc"
C=======================================================================
C     FORCES PARITH ON sur noeud d'ancrage MAIN
C=======================================================================
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX3, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY3, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ3, FZ6)
#include "lockon.inc"
      DO I = 1,JLT
        IL = M1L(I)
          DO K = 1,6
            DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
            DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
            DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
          ENDDO
      ENDDO
#include "lockoff.inc"
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FX4, FX6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FY4, FY6)
      CALL FOAT_TO_6_FLOAT(1  ,JLT  ,FZ4, FZ6)
#include "lockon.inc"
      DO I = 1,JLT
        IL = M2L(I)
          DO K = 1,6
            DAANC6(1,K,IL) = DAANC6(1,K,IL) + FX6(K,I)
            DAANC6(2,K,IL) = DAANC6(2,K,IL) + FY6(K,I)
            DAANC6(3,K,IL) = DAANC6(3,K,IL) + FZ6(K,I)
          ENDDO
      ENDDO
#include "lockoff.inc"
C=======================================================================
C     mise a ZERO des FORCES sur noeuds maites et second
C     si PENE (su noeud second) < GAPR (gap reel)
C=======================================================================
C=======================================================================
C     FORCES sur noeuds maites et second
C=======================================================================
C---------------------------------
      IF(IPARIT==0)THEN
       IF(KDTINT==0)THEN
         CALL I20ASS0(JLT  ,CS_LOC,N1   ,N2   ,M1   ,
     2                M2   ,HS1   ,HS2  ,HM1  ,HM2  ,
     3                FX1  ,FY1   ,FZ1  ,FX2  ,FY2  ,
     4                FZ2  ,FX3   ,FY3  ,FZ3  ,FX4  ,
     5                FY4  ,FZ4   ,A    ,STIFN,STIF ,
     6                NLINSA,NIN  ,JTASK)             
       ELSE
         CALL I20ASS05(JLT   ,CS_LOC,N1    ,N2   ,M1   ,
     2                 M2    ,HS1   ,HS2   ,HM1  ,HM2  ,
     3                 FX1   ,FY1   ,FZ1   ,FX2  ,FY2  ,
     4                 FZ2   ,FX3   ,FY3   ,FZ3  ,FX4  ,
     5                 FY4   ,FZ4   ,A     ,STIFN,NLINSA,
     6                 K1    ,K2    ,K3    ,K4   ,C1   ,
     7                 C2    ,C3    ,C4    ,VISCN,NIN ,JTASK )
       END IF
      ELSE
       IF(KDTINT==0)THEN
         CALL I20ASS2(JLT   ,CS_LOC ,N1    ,N2    ,M1      ,
     2                M2    ,HS1    ,HS2   ,HM1   ,HM2     ,
     3                FX1   ,FY1    ,FZ1   ,FX2   ,FY2     ,
     4                FZ2   ,FX3    ,FY3   ,FZ3   ,FX4     ,
     5                FY4   ,FZ4    ,FSKYI ,ISKY  ,NISKYFIE,
     6                STIF  ,NLINSA  ,NIN  ,NOINT )
       ELSE
         CALL I20ASS25(JLT  ,CS_LOC ,N1    ,N2      ,M1    ,
     2                 M2   ,HS1    ,HS2   ,HM1     ,HM2   ,
     3                 FX1  ,FY1    ,FZ1   ,FX2     ,FY2   ,
     4                 FZ2  ,FX3    ,FY3   ,FZ3     ,FX4   ,
     5                 FY4  ,FZ4    ,ISKY  ,NISKYFIE,NLINSA ,
     6                 K1   ,K2     ,K3    ,K4      ,C1    ,
     7                 C2   ,C3     ,C4    ,NIN     , NOINT)
       END IF
      END IF            
C
      IF(IDTMINS==2)
     .  CALL I20SMS2E(JLT   ,CS_LOC ,N1    ,N2     ,M1     ,
     2                M2    ,HS1    ,HS2   ,HM1    ,HM2    ,
     3                STIF  ,NIN    ,NOINT ,MSKYI_SMS ,ISKYI_SMS,
     4                NSMS  ,K1     ,K2    ,K3     ,K4     ,
     5                C1    ,C2     ,C3    ,C4     ,NLINSA )
C
      IF(IDTMIN(10)==1.OR.IDTMIN(10)==2)THEN
       DTMI0 = EP20
       DO I=1,JLT
        DTMI(I) = EP20
        MAS2  = TWO * MASMIN(I)
        IF(MAS2>ZERO.AND.STIF(I)>ZERO)THEN
          DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STIF(I)))
        ENDIF
        DTMI0 = MIN(DTMI0,DTMI(I))
       ENDDO
       IF(DTMI0<=DTMIN1(10))THEN
        DO I=1,JLT
         IF(DTMI(I)<=DTMIN1(10))THEN
           IF(IDTMIN(10)==1)THEN
#include "lockon.inc"
            IF(CS_LOC(I)<=NLINSA) THEN
             WRITE(IOUT,*)
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))
            ELSE
             WRITE(IOUT,*)
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT
               WRITE(IOUT,*)'SECONDARY NODES NB',ITAFIE(NIN)%P(N1(I)),
     .             ITAFIE(NIN)%P(N2(I))               
               WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))
            END IF
#include "lockoff.inc"
             TSTOP = TT
           ELSEIF(IDTMIN(10)==2)THEN
#include "lockon.inc"
            IF(CS_LOC(I)<=NLINSA) THEN
             WRITE(IOUT,*)
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
             WRITE(IOUT,*)'DELETE SECONDARY LINE FROM INTERFACE'                 
             STFS(CS_LOC(I)) = -ABS(STFS(CS_LOC(I)))
            ELSE
             NI = CS_LOC(I)-NLINSA
             WRITE(IOUT,*)
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAFIE(NIN)%P(N1(I)),
     .             ITAFIE(NIN)%P(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
             WRITE(IOUT,*)'DELETE SECONDARY LINE FROM INTERFACE'                 
             STIFIE(NIN)%P(NI) = -ABS(STIFIE(NIN)%P(NI))
            END IF
#include "lockoff.inc"
             NEWFRONT = -1
           ELSEIF(IDTMIN(10)==5)THEN
#include "lockon.inc"
            IF(CS_LOC(I)<=NLINSA) THEN
             WRITE(IOUT,*)
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAB(N1(I)),
     .             ITAB(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
            ELSE
             WRITE(IOUT,*)
     .       ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .       ' IN INTERFACE NB',NOINT
             WRITE(IOUT,*)'SECONDARY NODES NB',ITAFIE(NIN)%P(N1(I)),
     .             ITAFIE(NIN)%P(N2(I))               
             WRITE(IOUT,*)'MAIN NODES NB',ITAB(M1(I)),
     .             ITAB(M2(I))  
            END IF
#include "lockoff.inc"
             MSTOP = 2
           ENDIF                
         ENDIF
        ENDDO
       ENDIF
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0)THEN
#include "lockon.inc"
c         goto 1234
           DO I=1,JLT
            IF(CS_LOC(I)<=NLINSA) THEN
              FCONT(1,N1(I)) =FCONT(1,N1(I)) + FX1(I)
              FCONT(2,N1(I)) =FCONT(2,N1(I)) + FY1(I)
              FCONT(3,N1(I)) =FCONT(3,N1(I)) + FZ1(I)
              FCONT(1,N2(I)) =FCONT(1,N2(I)) + FX2(I)
              FCONT(2,N2(I)) =FCONT(2,N2(I)) + FY2(I)
              FCONT(3,N2(I)) =FCONT(3,N2(I)) + FZ2(I)
            END IF
            FCONT(1,M1(I)) =FCONT(1,M1(I)) + FX3(I)
            FCONT(2,M1(I)) =FCONT(2,M1(I)) + FY3(I)
            FCONT(3,M1(I)) =FCONT(3,M1(I)) + FZ3(I)
            FCONT(1,M2(I)) =FCONT(1,M2(I)) + FX4(I)
            FCONT(2,M2(I)) =FCONT(2,M2(I)) + FY4(I)
            FCONT(3,M2(I)) =FCONT(3,M2(I)) + FZ4(I)
           ENDDO
c 1234    continue
#include "lockoff.inc"
      ENDIF
C
      IF(ISECIN>0)THEN
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
              IF(CS_LOC(I)<=NLINSA) THEN
               IF(SECFCUM(4,N1(K),I)==1.)THEN
                SECFCUM(1,N1(K),I)=SECFCUM(1,N1(K),I)-FX1(K)
                SECFCUM(2,N1(K),I)=SECFCUM(2,N1(K),I)-FY1(K)
                SECFCUM(3,N1(K),I)=SECFCUM(3,N1(K),I)-FZ1(K)
               ENDIF
               IF(SECFCUM(4,N2(K),I)==1.)THEN
                SECFCUM(1,N2(K),I)=SECFCUM(1,N2(K),I)-FX2(K)
                SECFCUM(2,N2(K),I)=SECFCUM(2,N2(K),I)-FY2(K)
                SECFCUM(3,N2(K),I)=SECFCUM(3,N2(K),I)-FZ2(K)
               ENDIF
              END IF
              IF(SECFCUM(4,M1(K),I)==1.)THEN
                SECFCUM(1,M1(K),I)=SECFCUM(1,M1(K),I)-FX3(K)
                SECFCUM(2,M1(K),I)=SECFCUM(2,M1(K),I)-FY3(K)
                SECFCUM(3,M1(K),I)=SECFCUM(3,M1(K),I)-FZ3(K)
              ENDIF
              IF(SECFCUM(4,M2(K),I)==1.)THEN
                SECFCUM(1,M2(K),I)=SECFCUM(1,M2(K),I)-FX4(K)
                SECFCUM(2,M2(K),I)=SECFCUM(2,M2(K),I)-FY4(K)
                SECFCUM(3,M2(K),I)=SECFCUM(3,M2(K),I)-FZ4(K)
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
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I20ASS0                       source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20ASS0(JLT  ,CS_LOC,N1   ,N2   ,M1   ,
     2                   M2   ,HS1   ,HS2  ,HM1  ,HM2  ,
     3                   FX1  ,FY1   ,FZ1  ,FX2  ,FY2  ,
     4                   FZ2  ,FX3   ,FY3  ,FZ3  ,FX4  ,
     5                   FY4  ,FZ4   ,A    ,STIFN,STIF ,
     6                   NRTS ,NIN,JTASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS, NIN,
     +        CS_LOC(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ),JTASK
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        A(3,*), STIFN(*), STIF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1,NODFI,ISHIFT
C-----------------------------------------------
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
          J1=N1(I)
          A(1,J1)=A(1,J1)+FX1(I)
          A(2,J1)=A(2,J1)+FY1(I)
          A(3,J1)=A(3,J1)+FZ1(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS1(I))
C
          J1=N2(I)
          A(1,J1)=A(1,J1)+FX2(I)
          A(2,J1)=A(2,J1)+FY2(I)
          A(3,J1)=A(3,J1)+FZ2(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HS2(I))
        ELSE
          J1=N1(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX1(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY1(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ1(I)
          STNFIE(NIN)%P(J1+ISHIFT) = STNFIE(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS1(I))
C
          J1=N2(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX2(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY2(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ2(I)
          STNFIE(NIN)%P(J1+ISHIFT) = STNFIE(NIN)%P(J1+ISHIFT) + STIF(I)*ABS(HS2(I))
        END IF
      END DO
C
      DO I=1,JLT
       J1=M1(I)
       A(1,J1)=A(1,J1)+FX3(I)
       A(2,J1)=A(2,J1)+FY3(I)
       A(3,J1)=A(3,J1)+FZ3(I)
       STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM1(I))
C
       J1=M2(I)
       A(1,J1)=A(1,J1)+FX4(I)
       A(2,J1)=A(2,J1)+FY4(I)
       A(3,J1)=A(3,J1)+FZ4(I)
       STIFN(J1) = STIFN(J1) + STIF(I)*ABS(HM2(I))
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I20ASS05                      source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20ASS05(JLT  ,CS_LOC,N1    ,N2   ,M1   ,
     2                    M2   ,HS1   ,HS2   ,HM1  ,HM2  ,
     3                    FX1  ,FY1   ,FZ1   ,FX2  ,FY2  ,
     4                    FZ2  ,FX3   ,FY3   ,FZ3  ,FX4  ,
     5                    FY4  ,FZ4   ,A     ,STIFN,NRTS ,
     6                    K1   ,K2    ,K3    ,K4   ,C1   ,
     7                    C2   ,C3    ,C4    ,VISCN,NIN ,JTASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS, NIN,
     +        CS_LOC(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ),JTASK
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .        A(3,*), STIFN(*), VISCN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1,NODFI,ISHIFT
C-----------------------------------------------
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
C
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
          J1=N1(I)
          A(1,J1)=A(1,J1)+FX1(I)
          A(2,J1)=A(2,J1)+FY1(I)
          A(3,J1)=A(3,J1)+FZ1(I)
          STIFN(J1)=STIFN(J1)+K1(I)
          VISCN(J1)=VISCN(J1)+C1(I)
C
          J1=N2(I)
          A(1,J1)=A(1,J1)+FX2(I)
          A(2,J1)=A(2,J1)+FY2(I)
          A(3,J1)=A(3,J1)+FZ2(I)
          STIFN(J1)=STIFN(J1)+K2(I)
          VISCN(J1)=VISCN(J1)+C2(I)
        ELSE
          J1=N1(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX1(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY1(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ1(I)
          STNFIE(NIN)%P(J1+ISHIFT)=STNFIE(NIN)%P(J1+ISHIFT)+K1(I)
          VSCFIE(NIN)%P(J1+ISHIFT)=VSCFIE(NIN)%P(J1+ISHIFT)+C1(I)
C
          J1=N2(I)
          AFIE(NIN)%P(1,J1+ISHIFT)=AFIE(NIN)%P(1,J1+ISHIFT)+FX2(I)
          AFIE(NIN)%P(2,J1+ISHIFT)=AFIE(NIN)%P(2,J1+ISHIFT)+FY2(I)
          AFIE(NIN)%P(3,J1+ISHIFT)=AFIE(NIN)%P(3,J1+ISHIFT)+FZ2(I)
          STNFIE(NIN)%P(J1+ISHIFT)=STNFIE(NIN)%P(J1+ISHIFT)+K2(I)
          VSCFIE(NIN)%P(J1+ISHIFT)=VSCFIE(NIN)%P(J1+ISHIFT)+C2(I)
        END IF
      END DO
C
      DO I=1,JLT
       J1=M1(I)
       A(1,J1)=A(1,J1)+FX3(I)
       A(2,J1)=A(2,J1)+FY3(I)
       A(3,J1)=A(3,J1)+FZ3(I)
       STIFN(J1)=STIFN(J1)+K3(I)
       VISCN(J1)=VISCN(J1)+C3(I)
C
       J1=M2(I)
       A(1,J1)=A(1,J1)+FX4(I)
       A(2,J1)=A(2,J1)+FY4(I)
       A(3,J1)=A(3,J1)+FZ4(I)
       STIFN(J1)=STIFN(J1)+K4(I)
       VISCN(J1)=VISCN(J1)+C4(I)
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I20ASS2                       source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20ASS2(JLT   ,CS_LOC ,N1    ,N2    ,M1      ,
     2                   M2    ,HS1    ,HS2   ,HM1   ,HM2     ,
     3                   FX1   ,FY1    ,FZ1   ,FX2   ,FY2     ,
     4                   FZ2   ,FX3    ,FY3   ,FZ3   ,FX4     ,
     5                   FY4   ,FZ4    ,FSKYI ,ISKY  ,NISKYFIE,
     6                   STIF  ,NRTS   ,NIN   ,NOINT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS,NISKYFIE,NIN,NOINT,
     +        CS_LOC(*),ISKY(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        FSKYI(LSKYI,NFSKYI), STIF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NISKYL1, NISKYL,IGP,IGM, NISKYFIEL
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF (HM1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (HM2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO

      IGP = 0
      IGM = 0
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
           IGP = IGP+2
        ELSE
           IGM = IGM+1
        ENDIF
      ENDDO 
       
#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIEL = NISKYFIE
      NISKYFIE = NISKYFIE + IGM
#include "lockoff.inc"

      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIEL+IGM > NLSKYFIE(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HS1(I))
          ISKY(NISKYL) = N1(I)
C
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=STIF(I)*ABS(HS2(I))
          ISKY(NISKYL) = N2(I)
        ELSE
          NISKYFIEL = NISKYFIEL + 1
          FSKYFIE(NIN)%P(1,NISKYFIEL)=FX1(I)
          FSKYFIE(NIN)%P(2,NISKYFIEL)=FY1(I)
          FSKYFIE(NIN)%P(3,NISKYFIEL)=FZ1(I)
          FSKYFIE(NIN)%P(4,NISKYFIEL)=STIF(I)*ABS(HS1(I))
          FSKYFIE(NIN)%P(5,NISKYFIEL)=FX2(I)
          FSKYFIE(NIN)%P(6,NISKYFIEL)=FY2(I)
          FSKYFIE(NIN)%P(7,NISKYFIEL)=FZ2(I)
          FSKYFIE(NIN)%P(8,NISKYFIEL)=STIF(I)*ABS(HS2(I))
          ISKYFIE(NIN)%P(NISKYFIEL) = CS_LOC(I)-NRTS
        END IF
      END DO
C
      DO I=1,JLT
       IF (HM1(I)/=ZERO) THEN
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FX3(I)
        FSKYI(NISKYL,2)=FY3(I)
        FSKYI(NISKYL,3)=FZ3(I)
        FSKYI(NISKYL,4)=STIF(I)*ABS(HM1(I))
        ISKY(NISKYL) = M1(I)
       ENDIF
      ENDDO
      DO I=1,JLT
       IF (HM2(I)/=ZERO) THEN
        NISKYL = NISKYL + 1
        FSKYI(NISKYL,1)=FX4(I)
        FSKYI(NISKYL,2)=FY4(I)
        FSKYI(NISKYL,3)=FZ4(I)
        FSKYI(NISKYL,4)=STIF(I)*ABS(HM2(I))
        ISKY(NISKYL) = M2(I)
       ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I20ASS25                      source/interfaces/int20/i20for3.F
Chd|-- called by -----------
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20ASS25(JLT   ,CS_LOC ,N1    ,N2      ,M1    ,
     2                    M2    ,HS1    ,HS2   ,HM1     ,HM2   ,
     3                    FX1   ,FY1    ,FZ1   ,FX2     ,FY2   ,
     4                    FZ2   ,FX3    ,FY3   ,FZ3     ,FX4   ,
     5                    FY4   ,FZ4    ,ISKY  ,NISKYFIE,NRTS  ,
     6                    K1    ,K2     ,K3    ,K4      ,C1    ,
     7                    C2    ,C3     ,C4    ,NIN     ,NOINT )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NRTS,NISKYFIE,NIN,NOINT,
     +        CS_LOC(*),ISKY(*),
     +        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      my_real
     .        HS1(MVSIZ),HS2(MVSIZ),HM1(MVSIZ),HM2(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .        FSKYI(LSKYI,NFSKYI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, NISKYL1, NISKYL,IGP,IGM, NISKYFIEL
C
        NISKYL1 = 0
        DO I = 1, JLT
          IF (HM1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
        ENDDO
        DO I = 1, JLT
          IF (HM2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
        ENDDO

        IGP = 0
        IGM = 0
        DO I=1,JLT
          IF(CS_LOC(I)<=NRTS) THEN
             IGP = IGP+2
          ELSE
             IGM = IGM+1
          ENDIF
        ENDDO        

#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIEL = NISKYFIE
      NISKYFIE = NISKYFIE + IGM
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      IF (NISKYFIEL+IGM > NLSKYFIE(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      DO I=1,JLT
        IF(CS_LOC(I)<=NRTS) THEN
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX1(I)
          FSKYI(NISKYL,2)=FY1(I)
          FSKYI(NISKYL,3)=FZ1(I)
          FSKYI(NISKYL,4)=K1(I)
          FSKYI(NISKYL,5)=C1(I)
          ISKY(NISKYL) = N1(I)
C
          NISKYL = NISKYL + 1
          FSKYI(NISKYL,1)=FX2(I)
          FSKYI(NISKYL,2)=FY2(I)
          FSKYI(NISKYL,3)=FZ2(I)
          FSKYI(NISKYL,4)=K2(I)
          FSKYI(NISKYL,5)=C2(I)
          ISKY(NISKYL) = N2(I)
        ELSE
          NISKYFIEL = NISKYFIEL + 1
          FSKYFIE(NIN)%P(1,NISKYFIEL)=FX1(I)
          FSKYFIE(NIN)%P(2,NISKYFIEL)=FY1(I)
          FSKYFIE(NIN)%P(3,NISKYFIEL)=FZ1(I)
          FSKYFIE(NIN)%P(4,NISKYFIEL)=K1(I)
          FSKYFIE(NIN)%P(5,NISKYFIEL)=C1(I)
          FSKYFIE(NIN)%P(6,NISKYFIEL)=FX2(I)
          FSKYFIE(NIN)%P(7,NISKYFIEL)=FY2(I)
          FSKYFIE(NIN)%P(8,NISKYFIEL)=FZ2(I)
          FSKYFIE(NIN)%P(9,NISKYFIEL)=K2(I)
          FSKYFIE(NIN)%P(10,NISKYFIEL)=C2(I)
          ISKYFIE(NIN)%P(NISKYFIEL) = CS_LOC(I)-NRTS
        END IF
      END DO
C
      DO I=1,JLT
        IF (HM1(I)/=ZERO) THEN
         NISKYL = NISKYL + 1
         FSKYI(NISKYL,1)=FX3(I)
         FSKYI(NISKYL,2)=FY3(I)
         FSKYI(NISKYL,3)=FZ3(I)
         FSKYI(NISKYL,4)=K3(I)
         FSKYI(NISKYL,5)=C3(I)
         ISKY(NISKYL) = M1(I)
        ENDIF
      ENDDO
      DO I=1,JLT
        IF (HM2(I)/=ZERO) THEN
         NISKYL = NISKYL + 1
         FSKYI(NISKYL,1)=FX4(I)
         FSKYI(NISKYL,2)=FY4(I)
         FSKYI(NISKYL,3)=FZ4(I)
         FSKYI(NISKYL,4)=K4(I)
         FSKYI(NISKYL,5)=C4(I)
         ISKY(NISKYL) = M2(I)
        ENDIF
      ENDDO
C
      RETURN
      END
