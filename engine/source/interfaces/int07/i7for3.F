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
Chd|  I7FOR3                        source/interfaces/int07/i7for3.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        I7CURV                        source/interfaces/int07/i7curv.F
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7FOR3(JLT       ,A         ,V          ,IBCC       ,ICODT  ,
     2                  FSAV      ,GAP       ,FRIC       ,MS         ,VISC   ,
     3                  VISCF     ,NOINT     ,STFN       ,ITAB       ,CN_LOC ,
     4                  STIGLO    ,STIFN     ,STIF       ,FSKYI      ,ISKY   ,
     5                  NX1       ,NX2       ,NX3        ,NX4        ,NY1    ,
     6                  NY2       ,NY3       ,NY4        ,NZ1        ,NZ2    ,
     7                  NZ3       ,NZ4       ,LB1        ,LB2        ,LB3    ,
     8                  LB4       ,LC1       ,LC2        ,LC3        ,LC4    ,
     9                  P1        ,P2        ,P3         ,P4         ,FCONT  ,
     A                  IX1       ,IX2       ,IX3        ,IX4        ,NSVG   ,
     B                  IVIS2     ,NELTST    ,ITYPTST    ,DT2T       ,GAPV   ,
     C                  INACTI    ,CAND_P     ,INDEX     ,KINET      ,NEWFRONT,
     D                  ISECIN    ,NSTRF      ,X         ,IRECT      ,CE_LOC  ,
     E                  MFROT     ,IFQ        ,CAND_FX    ,CAND_FY   ,
     F                  CAND_FZ   ,ALPHA0     ,IFPEN     ,IBAG       ,
     H                  ICONTACT  ,VISCN      ,VXI       ,VYI        ,VZI     ,
     I                  MSI       ,KINI       ,NIN       ,NISUB      ,LISUB   ,
     J                  ADDSUBS   ,ADDSUBM    ,LISUBS    ,LISUBM     ,FSAVSUB ,
     K                  CAND_N    ,ILAGM      ,ICURV     ,NOD_NORMAL ,FNCONT  ,
     L                  FTCONT    ,X1         ,X2        ,X3         ,X4      ,
     M                  Y1        ,Y2         ,Y3        ,Y4         ,Z1      ,
     N                  Z2        , Z3        ,Z4        ,XI         ,YI      ,
     O                  ZI        , IADM      ,RCURVI    ,RCONTACT   ,ACONTACT,
     P                  PCONTACT  ,ANGLMI     ,PADM      ,INTTH      ,TEMP    ,
     Q                  TEMPI     ,IFORM      ,NPC       ,TF         ,CMAJ    ,
     R                  DTMINI    ,DRAD       ,FHEATS    ,FHEATM     ,EFRICT  ,
     S                  QFRIC      ,FNI       ,IFRIC     ,
     T                  JTASK      ,H1      ,
     U                  H2         ,H3        ,H4        ,KS         ,KT      ,
     V                  K1         ,K2        ,K3        ,K4         ,C1      ,
     W                  C2         ,C3        ,C4        ,CS         ,C       ,
     X                  CF         ,TINT      ,XFRIC     ,FXI        ,FYI     , 
     Y                  FZI        ,FX1       ,FY1       ,FZ1        ,FX2     ,
     Z                  FY2        ,FZ2       ,FX3       ,FY3        ,FZ3     ,
     1                  FX4        ,FY4       ,FZ4       ,ISENSINT  ,FSAVPARIT,
     5                  NFT        ,SYM_FLAG_TYPE19,H3D_DATA,FRICC  ,VISCFFRIC,
     6                  FRIC_COEFS ,ITIED     ,JLT_TIED  ,CAND_F    ,IORTHFRIC,
     7                  FRIC_COEFS2,FRICC2    ,VISCFFRIC2,NFORTH    ,NFISOT   ,
     8                  INDEXORTH  ,INDEXISOT ,DIR1      ,DIR2      ,TAGNCONT ,
     9                  KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,TYPSUB,INFLG_SUBS,
     A                  INFLG_SUBM)
                          
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
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,IBCC,IBCM,IBCS,IVIS2,INACTI,IBAG,NIN,
     .        IFRIC,IFORM,INTFRIC,ITIED,JLT_TIED,IORTHFRIC,NFORTH ,NFISOT ,
     .        ICODT(*), ITAB(*), ISKY(*), KINET(*),
     .        MFROT, IFQ, NOINT,NEWFRONT,ISECIN, NSTRF(*),
     .        IRECT(4,*),IFPEN(*) ,ICONTACT(*), CAND_N(*),
     .        KINI(*),NPC(*),JTASK,TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        KLOADPINTER(NINTER+1),LOADPINTER((NINTER+1)*NLOADP_HYD),
     .        LOADP_HYD_INTER(NLOADP_HYD),TYPSUB(*),
     .        ISET, IADM,INTTH,NFT
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        CN_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ),
     .        NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*),
     .        LISUBM(*),ILAGM,ICURV(3), ISENSINT(*),SYM_FLAG_TYPE19,
     .        INDEXORTH(MVSIZ)  ,INDEXISOT(MVSIZ),INFLG_SUBS(*), INFLG_SUBM(*)
      my_real
     .   STIGLO,CAND_P(*), X(3,*),
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),ALPHA0,
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T,STFN(*),STIFN(*),
     .   FSKYI(LSKYI,NFSKYI),FSAVSUB(NTHVKI,*), FNCONT(3,*),FTCONT(3,*),
     .   DTMINI,FHEATS,FHEATM,
     .   FSAVPARIT(NISUB+1,11,*), CAND_F(8,*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     TMP(MVSIZ),
     .     STIFSAV(MVSIZ), VISCN(*),TF(*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .     X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .     X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .     X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .     XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),
     .    TEMP(*), TEMPI(MVSIZ),
     .     FNI(MVSIZ), FNS(MVSIZ)
      my_real
     .     NOD_NORMAL(3,*), RCURVI(*), RCONTACT(*), ACONTACT(*),
     .     PCONTACT(*),PADM, ANGLMI(*),CMAJ(MVSIZ), EFRICT(MVSIZ),
     .     QFRIC,DRAD,TINT,XFRIC,FRIC_COEFS(MVSIZ,10), FRICC(MVSIZ),
     .     VISCFFRIC(MVSIZ),FRIC_COEFS2(MVSIZ,10),FRICC2(MVSIZ),
     .     VISCFFRIC2(MVSIZ),DIR1(MVSIZ,3)      ,DIR2(MVSIZ,3) 
      my_real
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), 
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI, II, 
     .        NA1,NA2,IPROJ,IDTM,PP,PPL,ITYPSUB,ISS1,ISS2,IMS1,IMS2
      my_real
     .   FXR(MVSIZ), FYR(MVSIZ), FZR(MVSIZ)
      my_real
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,DIST,RDIST,
     .   V2, FM2, DT1INV, VISCA, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,ECONTDT,ECONTTIED,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8, 
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15,
     .   FSAV22, FSAV23, FSAV24, FFO,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA,STFR,VISR ,TM,TS  
      my_real
     .   PREC,NN1,NN2,NN3,NN4,XN1,YN1,ZN1,XN2,YN2,ZN2,XN3,YN3,ZN3,
     .   XN4,YN4,ZN4
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   CX,CY,CFI,AUX, FX6(6,MVSIZ), FY6(6,MVSIZ), FZ6(6,MVSIZ),
     .   DTM, PHIM,QFRICT,DYDX,TH,THI(MVSIZ),FRICT(MVSIZ),FORC_SIGN,
     .   VT1(MVSIZ), VT2(MVSIZ), 
     .   FT1(MVSIZ),FT2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   T1X(MVSIZ),T1Y(MVSIZ),T1Z(MVSIZ),
     .   T2X(MVSIZ),T2Y(MVSIZ),T2Z(MVSIZ), DT05, NORMINV,
     .   XH, YH, ZH, SIDE(MVSIZ),XMU2(MVSIZ),CSA ,SNA ,ALPHAF ,FTT1 ,FTT2,
     .   AN(NFORTH) ,BN(NFORTH) ,NEP1 ,NEP2 ,NEP ,C11 ,C22 ,ANS , EP ,SIGNC
      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB
      my_real
     .   FSAVSUB1(24,NISUB),IMPX,IMPY,IMPZ,BB,GAPR
      my_real
     .   TOTAL 
      INTEGER BITGET
      EXTERNAL BITGET
C
C-----------------------------------------------
C      1 .. JLT : all impacts
C          1 .. JLT-JLT_TIED     : Standard TYPE7 formulation
C          JLT-JLT_TIED+1 .. JLT : Modified formulation (tied nodes with possible rebound)
C-----------------------------------------------
#include "sync_dir.inc"
      IF (IRESP.EQ.1) THEN
           PREC = CINQEM4
      ELSE
           PREC = EM10
      ENDIF
      IF(DT1.GT.ZERO)THEN
        DT1INV = UN/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO
      QFRICT = ZERO
      ECONTDT = ZERO
      ECONTTIED = ZERO
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO 
C
C--------------------------------------------------------
C  For sym type7 of type19 interface normal and tangential forces must have opposite sign
C--------------------------------------------------------
      FORC_SIGN = UN
      IF (SYM_FLAG_TYPE19 > 0) FORC_SIGN = -UN
C
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
      IF(INTTH == 0.OR.DRAD == ZERO) THEN ! NO HEAT EXCHANGE
       IF(ICURV(1) == 3) THEN
         DO I=1,JLT-JLT_TIED
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
        DO I=1,JLT-JLT_TIED
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
       DO I=1,JLT-JLT_TIED
        IF(IX3(I).NE.IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
         LA1 = UN - LB1(I) - LC1(I)
         LA2 = UN - LB2(I) - LC2(I)
         LA3 = UN - LB3(I) - LC3(I)
         LA4 = UN - LB4(I) - LC4(I) 
C
         H0    = QUART * 
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
         H00    = UN/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
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
         H3(I) = UN - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
       ENDDO
      ELSE  ! HEAT EXCHANGE
       IF(ICURV(1) == 3) THEN
         DO I=1,JLT-JLT_TIED
          BB = GAPV(I)+CMAJ(I)
          GAPR = MAX(BB,DRAD)
C
          D1 = SQRT(P1(I))
          P1(I) = MAX(ZERO, GAPR - D1)
C
          D2 = SQRT(P2(I))
          P2(I) = MAX(ZERO, GAPR - D2)
C
          D3 = SQRT(P3(I))
          P3(I) = MAX(ZERO, GAPR - D3)
C
          D4 = SQRT(P4(I))
          P4(I) = MAX(ZERO, GAPR - D4)
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
        DO I=1,JLT-JLT_TIED
         GAPR = MAX(GAPV(I),DRAD)
C
         D1 = SQRT(P1(I))
         P1(I) = MAX(ZERO, GAPR - D1)
C
         D2 = SQRT(P2(I))
         P2(I) = MAX(ZERO, GAPR - D2)
C
         D3 = SQRT(P3(I))
         P3(I) = MAX(ZERO, GAPR - D3)
C
         D4 = SQRT(P4(I))
         P4(I) = MAX(ZERO, GAPR - D4)
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
       DO I=1,JLT-JLT_TIED
        IF(IX3(I).NE.IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
         PENE(I) = MAX(ZERO,PENE(I)+GAPV(I)-MAX(GAPV(I),DRAD))
C
         LA1 = UN - LB1(I) - LC1(I)
         LA2 = UN - LB2(I) - LC2(I)
         LA3 = UN - LB3(I) - LC3(I)
         LA4 = UN - LB4(I) - LC4(I) 
C
         H0    = QUART * 
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 
     .         + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
         H00    = UN/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
C
         H1(I) = H1(I) * H00
         H2(I) = H2(I) * H00
         H3(I) = H3(I) * H00
         H4(I) = H4(I) * H00
C
        ELSE
         PENE(I) = P1(I)
         PENE(I) = MAX(ZERO,PENE(I)+GAPV(I)-MAX(GAPV(I),DRAD))
         N1(I) = NX1(I)
         N2(I) = NY1(I)
         N3(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = UN - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
       ENDDO
      ENDIF ! IF(INTTH == 0)
C---------------------
C     COURBURE FIXE
C---------------------
      IF(ICURV(1).EQ.1)THEN
C       spherique (que concave pour le moment)
        NA1 = ICURV(2)
        DO I=1,JLT-JLT_TIED
          RR = 1.E30
          A0X = X(1,NA1)
          A0Y = X(2,NA1)
          A0Z = X(3,NA1)
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
          IF(IX3(I).NE.IX4(I))THEN
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
          IF(RS-RR+GAPV(I).LT.0.)THEN
            STIF(I) = 0.
            PENE(I) = 0.
          ELSEIF(RS-RR+GAPV(I).LT.PENE(I))THEN
            PENE(I) = RS-RR+GAPV(I)
          ENDIF
          N1(I) = -RX 
          N2(I) = -RY 
          N3(I) = -RZ 
        ENDDO
      ELSEIF(ICURV(1).EQ.2)THEN
        NA1 = ICURV(2)
        NA2 = ICURV(3)
        DO I=1,JLT-JLT_TIED
          RR = 1.E30
          A0X = X(1,NA1)
          A0Y = X(2,NA1)
          A0Z = X(3,NA1)
          ANX = X(1,NA2)-A0X
          ANY = X(2,NA2)-A0Y
          ANZ = X(3,NA2)-A0Z
          AAN = 1. / (ANX*ANX + ANY*ANY + ANZ*ANZ) 
C
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
          IF(IX3(I).NE.IX4(I))THEN
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
C --  
          IF(RS-RR+GAPV(I).LT.0.)THEN
            STIF(I) = 0.
            PENE(I) = 0.
          ELSEIF(RS-RR+GAPV(I).LT.PENE(I))THEN
            PENE(I) = RS-RR+GAPV(I)
            N1(I) = -RX 
            N2(I) = -RY 
            N3(I) = -RZ 
          ELSEIF(RS-RR-GAPV(I).GT.0.)THEN
            STIF(I) = 0.
            PENE(I) = 0.
          ELSEIF(RS-RR-GAPV(I) .LT. PENE(I))THEN
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
            IF(IX3(I).NE.IX4(I))THEN
              XN4 = X4(I) - XI(I)
              YN4 = Y4(I) - YI(I)
              ZN4 = Z4(I) - ZI(I)
              NN3 = (YN3*ZN4-YN4*ZN3) * RX +
     .           (ZN3*XN4-ZN4*XN3) * RY +
     .           (XN3*YN4-XN4*YN3) * RZ 
              NN4 = (YN4*ZN1-YN1*ZN4) * RX +
     .           (ZN4*XN1-ZN1*XN4) * RY +
     .           (XN4*YN1-XN1*YN4) * RZ 
            ELSE
              NN3 = (YN3*ZN1-YN1*ZN3) * RX +
     .           (ZN3*XN1-ZN1*XN3) * RY +
     .           (XN3*YN1-XN1*YN3) * RZ 
              NN4 = NN3
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
     1              N3     ,GAPV   ,X      ,NOD_NORMAL,
     2              IX1    ,IX2    ,IX3    ,IX4       ,
     3              H1     ,H2     ,H3     ,H4        ,
     4              X1     ,X2     ,X3     ,X4    ,Y1    ,
     5              Y2     ,Y3     ,Y4     ,Z1    ,Z2    ,
     6              Z3     ,Z4     ,XI     ,YI    ,ZI    )
        DO I=1,JLT-JLT_TIED
          IF(PENE(I).LT.ZERO)THEN
            STIF(I) =ZERO
            PENE(I) =ZERO
          END IF
        END DO
      ENDIF
C
      DO I=1,JLT-JLT_TIED
        S2 = UN/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
        N1(I) = N1(I)*S2
        N2(I) = N2(I)*S2
        N3(I) = N3(I)*S2
      ENDDO
C
      DO I=1,JLT-JLT_TIED
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                 - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                 - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                 - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
        VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
C       correction hourglass
        H0 = -.25*(H1(I) - H2(I) + H3(I) - H4(I))
        H0 = MIN(H0,H2(I),H4(I))
        H0 = MAX(H0,-H1(I),-H3(I))
        IF(IX3(I).EQ.IX4(I))H0 = ZERO
        H1(I) = H1(I) + H0
        H2(I) = H2(I) - H0
        H3(I) = H3(I) + H0
        H4(I) = H4(I) - H0
      END DO
C-------------------------------------------
C     IMPACTS SUIVANTS (Tied)
C------------------------------------
      DO I=JLT-JLT_TIED+1,JLT
        II=INDEX(I)
        H1(I) = CAND_F(4,II)
        H2(I) = CAND_F(5,II)
        H3(I) = CAND_F(6,II)
        H4(I) = UN - H1(I) - H2(I) - H3(I)
      END DO
C
      DO I=JLT-JLT_TIED+1,JLT
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
        IF(IX3(I).NE.IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
        ELSE
         PENE(I) = P1(I)
        ENDIF
C
      END DO
C------------------------------------
      IF(ITIED /= 0)THEN
C
C       Computed for every impact !
        DO I=1,JLT
C
          T1X(I) = X3(I) - X1(I)
          T1Y(I) = Y3(I) - Y1(I)
          T1Z(I) = Z3(I) - Z1(I)
          NORMINV = UN/SQRT(T1X(I)**2+T1Y(I)**2+T1Z(I)**2)
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
          NORMINV = UN/SQRT(NX(I)**2+NY(I)**2+NZ(I)**2)
          NX(I) = NX(I)*NORMINV
          NY(I) = NY(I)*NORMINV
          NZ(I) = NZ(I)*NORMINV
C
          T2X(I) = NY(I)*T1Z(I) - NZ(I)*T1Y(I)
          T2Y(I) = NZ(I)*T1X(I) - NX(I)*T1Z(I)
          T2Z(I) = NX(I)*T1Y(I) - NY(I)*T1X(I)
C
C         SIDE(I)=NX(I)*N1(I)+NY(I)*N2(I)+NZ(I)*N3(I)
          XH=H1(I)*X1(I)+H2(I)*X2(I)+H3(I)*X3(I)+H4(I)*X4(I)
          YH=H1(I)*Y1(I)+H2(I)*Y2(I)+H3(I)*Y3(I)+H4(I)*Y4(I)
          ZH=H1(I)*Z1(I)+H2(I)*Z2(I)+H3(I)*Z3(I)+H4(I)*Z4(I)
          SIDE(I)=(XI(I)-XH)*NX(I)+(YI(I)-YH)*NY(I)+(ZI(I)-ZH)*NZ(I)
C
        END DO
      END IF
C
      DO I=JLT-JLT_TIED+1,JLT
        N1(I) = NX(I)
        N2(I) = NY(I)
        N3(I) = NZ(I)
      END DO
C------------------------------------
      DO I=JLT-JLT_TIED+1,JLT
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                 - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                 - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                 - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
        VN(I)  = VX(I)*NX(I)  + VY(I)*NY(I)  + VZ(I)*NZ(I)  
        VT1(I) = VX(I)*T1X(I) + VY(I)*T1Y(I) + VZ(I)*T1Z(I)  
        VT2(I) = VX(I)*T2X(I) + VY(I)*T2Y(I) + VZ(I)*T2Z(I)  
      END DO
C---------------------
C      PENE INITIALE
C---------------------
      IF(INACTI.EQ.5)THEN
         DO I=1,JLT-JLT_TIED
C REDUCTION DE LA PENE INITIALE
C          CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),PENE(I))
           CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .            ((UN-CINQEM2)*CAND_P(INDEX(I))+CINQEM2*PENE(I))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
           PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
           IF( PENE(I).EQ.ZERO )  STIF(I) = ZERO
           GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
         ENDDO

         DO I=JLT-JLT_TIED+1,JLT
C
C          CAND_P does not vary during tying (IF ITIED=1, rebound occurs when PENE < CAND_P)
           PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
           GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
         ENDDO

      ELSE IF(INACTI.EQ.6)THEN
         DO I=1,JLT-JLT_TIED
C REDUCTION DE LA PENE INITIALE
C           CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),PENE(I))
           CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .        ( (UN-CINQEM2)*CAND_P(INDEX(I))
     .          +CINQEM2*(PENE(I)+CINQEM2*(GAPV(I)-PENE(I))))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
           PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
           IF( PENE(I).EQ.ZERO )  STIF(I) = ZERO
           GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
         ENDDO

         DO I=JLT-JLT_TIED+1,JLT
C
C          CAND_P does not vary during tying (IF ITIED=1, rebound occurs when PENE < CAND_P)
           PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
           GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
         ENDDO

      ENDIF
C---------------------
      IF(ITIED == 0)THEN
C
        DTI = 1.E20
C
        DO 600 I=1,JLT
            DIST=GAPV(I)-PENE(I)
            RDIST  = UNDEMI*DIST / MAX(EM30,-VN(I))
            DTI = MIN(RDIST,DTI)
 600    CONTINUE
C
C Force to DEL
        IF (DTMINI.GT.ZERO) THEN 
          DTM=DTMINI
          IDTM=2
        ELSE 
          DTM=DTMIN1(10)
          IDTM=IDTMIN(10)
        END IF

        IF(DTI.LE.DTM)THEN
          DTI = 1.E20
          DO I=1,JLT
              DIST =GAPV(I)-PENE(I)
              DTI2   = UNDEMI*DIST / MAX(EM30,-VN(I))
              IF(DTI2.LE.DTM)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .              ' **WARNING MINIMUM TIME STEP ',DTI2,
     .              ' IN INTERFACE ',NOINT,'(DTMIN=',DTM,')'
               IF ( ISTAMPING == 1) THEN
                 WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(ISTDO,'(A)')'You may need to check if there is enou 
     .gh clearance between the tools,'
                 WRITE(ISTDO,'(A)')'and that they do not penetrate each ot
     .her during their travel'
                 WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(IOUT, '(A)')'You may need to check if there is enou
     .gh clearance between the tools,'
                 WRITE(IOUT, '(A)')'and that they do not penetrate each ot
     .her during their travel'
               ENDIF
               NN = NSVG(I)
               IF(NN.GT.0)THEN
                 NI = ITAB(NN)
               ELSE
                 NI = ITAFI(NIN)%P(-NN)
               ENDIF
#include "lockoff.inc"
               IF(IDTM.EQ.1)THEN
#include "lockon.inc"
                 WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',NI
                 WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .              ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))  
#include "lockoff.inc"
                 TSTOP = TT
               ELSEIF(IDTM.EQ.2)THEN
#include "lockon.inc"
                 WRITE(IOUT,'(A,I10,A,I10)')'   REMOVE SECONDARY NODE ',
     .                 NI,' FROM INTERFACE ',NOINT                 
                 IF(NN.GT.0) THEN
                   STFN(CN_LOC(I)) = -ABS(STFN(CN_LOC(I)))
                 ELSE
                   STIFI(NIN)%P(-NN) = -ABS(STIFI(NIN)%P(-NN))
                 ENDIF
#include "lockoff.inc"
                 STIF(I) = ZERO
                 NEWFRONT = -1
                 DTI = DTM

               ELSEIF(IDTM.EQ.5)THEN
#include "lockon.inc"
                 WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',NI                 
                 WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .              ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))  
#include "lockoff.inc"
                 MSTOP = 2
               ELSEIF(IDTM.EQ.6.AND.ILAGM.EQ.2)THEN
                IG=NSVG(I)
                IF(KINET(IG)+KINET(IX1(I))+KINET(IX2(I))
     .            +KINET(IX3(I))+KINET(IX4(I)).EQ.0)THEN
                 CAND_N(INDEX(I)) = -IABS(CAND_N(INDEX(I)))
                 STIF(I) = ZERO
                 DTI2 = 1.E20
#include "lockon.inc"
                 WRITE(IOUT,'(A,I10)') '   SECONDARY NODE :   ',ITAB(NSVG(I))                 
                 WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .              ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))  
#include "lockoff.inc"
                ENDIF                
                DTI = MIN(DTI2,DTI)
               ENDIF                
              ENDIF               
          ENDDO
        ENDIF
        IF(DTI.LT.DT2T)THEN
          DT2T    = DTI
          NELTST  = NOINT
          ITYPTST = 10
        ENDIF

      ENDIF ! IF(ITIED == 0)THEN
C-------------------------------------------
C
Cqz 2:const,1:linear,0:nonlinear
      IF(IMPL_S.GT.0)THEN
       IF(IMP_INT7.EQ.2)THEN
        DO I=1,JLT
         IF(STIGLO.LE.ZERO)THEN
          STIF(I) = UNDEMI*STIF(I) 
         ELSEIF(STIF(I).NE.ZERO)THEN
          STIF(I) = STIGLO 
         ENDIF
         FNI(I)= -STIF(I) * PENE(I) 
        ENDDO
       ELSE
        DO I=1,JLT
         FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
         FACM1 = 1./FAC
         IF( (GAPV(I)-PENE(I))/GAPV(I) .LT.PREC .AND. 
     .     STIF(I).GT.0. ) THEN
          STIF(I) = 0.
          NEWFRONT = -1
#include "lockon.inc"
          NN = NSVG(I)
          IF(NN.GT.0)THEN
           NI = ITAB(NN)
           STFN(CN_LOC(I)) = -ABS(STFN(CN_LOC(I)))
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
          IF ( ISTAMPING == 1) THEN
            WRITE(ISTDO,'(A)')'The run encountered a problem in an inter
     .face Type 7.'
            WRITE(ISTDO,'(A)')'You may need to check if there is enough 
     .clearance between the tools,'
            WRITE(ISTDO,'(A)')'and that they do not penetrate each other
     . during their travel'
            WRITE(IOUT, '(A)')'The run encountered a problem in an inter
     .face Type 7.'
            WRITE(IOUT, '(A)')'You may need to check if there is enough 
     .clearance between the tools,'
            WRITE(IOUT, '(A)')'and that they do not penetrate each other
     . during their travel'
          ENDIF 
#include "lockoff.inc"
         ENDIF
         IF(STIGLO.LE.ZERO)THEN
          IF(INCONV.EQ.1) ECONTT = ECONTT + UNDEMI*STIF(I)*GAPV(I)**2 *  
     .        ( FACM1 -UN -LOG(FACM1) )
          STIF(I) = UNDEMI*STIF(I) * FAC
         ELSEIF(STIF(I).NE.ZERO)THEN
          IF(INCONV.EQ.1)ECONTT = ECONTT + STIGLO*GAPV(I)**2 * 
     .        ( FACM1 - UN -LOG(FACM1) )
          STIF(I) = STIGLO * FAC
         ENDIF
         FNI(I)= -STIF(I) * PENE(I) 
        ENDDO
       ENDIF
       IF(STIGLO.LE.ZERO)THEN
        DO I=1,JLT
          STIF0(I) = UNDEMI*STIF0(I)
        ENDDO
       ENDIF
      ELSE              ! fin impl_s>0
      DO I=1,JLT-JLT_TIED
      FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
      FACM1 = UN/FAC
      IF( (GAPV(I)-PENE(I))/GAPV(I) .LT.PREC .AND. 
     .     STIF(I).GT.ZERO ) THEN
         STIF(I) = ZERO
         NEWFRONT = -1
#include "lockon.inc"
         NN = NSVG(I)
         IF(NN.GT.0)THEN
           NI = ITAB(NN)
           STFN(CN_LOC(I)) = -ABS(STFN(CN_LOC(I)))
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
          IF ( ISTAMPING == 1) THEN
            WRITE(ISTDO,'(A)')'The run encountered a problem in an inter
     .face Type 7.'
            WRITE(ISTDO,'(A)')'You may need to check if there is enough 
     .clearance between the tools,'
            WRITE(ISTDO,'(A)')'and that they do not penetrate each other
     . during their travel'
            WRITE(IOUT, '(A)')'The run encountered a problem in an inter
     .face Type 7.'
            WRITE(IOUT, '(A)')'You may need to check if there is enough 
     .clearance between the tools,'
            WRITE(IOUT, '(A)')'and that they do not penetrate each other
     . during their travel'
          ENDIF
#include "lockoff.inc"
      ENDIF
      IF(STIGLO.LE.ZERO)THEN
        ECONTT = ECONTT + UNDEMI*STIF(I)*GAPV(I)**2 *( FACM1 - UN - 
     .        LOG(FACM1) )
        STIF(I) = UNDEMI*STIF(I) * FAC
      ELSEIF(STIF(I).NE.ZERO)THEN
        ECONTT = ECONTT + STIGLO*GAPV(I)**2 *( FACM1 - UN - 
     .        LOG(FACM1) )
        STIF(I) = STIGLO * FAC
      ENDIF
      FNI(I)= -STIF(I) * PENE(I) 
      END DO
      ENDIF
C---------------------------------
      IF(ITIED/=0)THEN
        DO I=1,JLT-JLT_TIED
          FNS(I)= FNI(I)
        END DO
      END IF
C---------------------------------
C     Force vs tied impacts (cf TYPE10)
C---------------------------------
      IF(ITIED == 1)THEN
        DO I=JLT-JLT_TIED+1,JLT

          II=INDEX(I)
          IF(PENE(I).EQ.ZERO.AND.SIDE(I)*CAND_F(8,II) > ZERO)THEN
C--------------------------------------
C           REBOUND
C--------------------------------------
            VN(I)  = ZERO
            VT1(I) = ZERO
            VT2(I) = ZERO    
          END IF
        END DO
      END IF

      DT05=UNDEMI*DT1
      DO I=JLT-JLT_TIED+1,JLT
        II=INDEX(I)

        FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,II)) ) )
        STIF(I) = UNDEMI*STIF0(I) * FAC**2 ! Tangent Stiffness

        ECONTTIED = ECONTTIED + CAND_F(1,II) * VN(I)  * DT05
        ECONTTIED = ECONTTIED + CAND_F(2,II) * VT1(I) * DT05
        ECONTTIED = ECONTTIED + CAND_F(3,II) * VT2(I) * DT05

        FNI(I) = CAND_F(1,II) + VN(I)  * DT1 * STIF(I)
        FT1(I) = CAND_F(2,II) + VT1(I) * DT1 * STIF(I)
        FT2(I) = CAND_F(3,II) + VT2(I) * DT1 * STIF(I)
      END DO

      IF(ITIED == 1)THEN
        DO I=JLT-JLT_TIED+1,JLT

          II=INDEX(I)
          IF(PENE(I).EQ.ZERO.AND.SIDE(I)*CAND_F(8,II) > ZERO)THEN
C--------------------------------------
C           REBOUND
C--------------------------------------
            CAND_F(1,II) =ZERO
            FNI(I) = ZERO
            FT1(I) = ZERO
            FT2(I) = ZERO
            VN(I)  = ZERO
            VT1(I) = ZERO
            VT2(I) = ZERO    
          ELSEIF(FNI(I).EQ.ZERO)THEN
            CAND_F(1,II) = SIGN(EM30,CAND_F(1,II))
          ELSE
            IF (INCONV.EQ.1) CAND_F(1,II) =FNI(I) ! CAND_F(1,II) negative vs NX, NY, NZ
          ENDIF
          IF (INCONV.EQ.1) THEN
            CAND_F(2,II) = FT1(I)
            CAND_F(3,II) = FT2(I)
          ENDIF 
        END DO
      ELSEIF(ITIED == 2)THEN
        DO I=JLT-JLT_TIED+1,JLT

          II=INDEX(I)
          IF(FNI(I).EQ.ZERO.AND.PENE(I).NE.ZERO)THEN
            CAND_F(1,II) = EM30
          ELSE
            IF (INCONV.EQ.1) CAND_F(1,II) =FNI(I) ! CAND_F(1,II) negative vs NX, NY, NZ
          ENDIF
          IF (INCONV.EQ.1) THEN
            CAND_F(2,II) = FT1(I)
            CAND_F(3,II) = FT2(I)
          ENDIF 
        END DO
      END IF
C
      DO I=JLT-JLT_TIED+1,JLT
        II = INDEX(I)
        ECONTTIED = ECONTTIED + CAND_F(1,II) * VN(I)  * DT05
        ECONTTIED = ECONTTIED + CAND_F(2,II) * VT1(I) * DT05
        ECONTTIED = ECONTTIED + CAND_F(3,II) * VT2(I) * DT05
      END DO
C---------------------------------
C     DAMPING + FRIC
C---------------------------------
C     goto 999

      IF(VISC.NE.ZERO)THEN
        IF(IVIS2.EQ.0.OR.IVIS2.EQ.1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT-JLT_TIED
            VIS2(I) = DEUX * STIF(I) * MSI(I)
            IF(VN(I).LT.ZERO) VIS2(I) = VIS2(I) / 
     .         ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
          ENDDO
C---------------------------------
          VISCA = ZEP4
          IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
           DO I=1,JLT-JLT_TIED
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * DEUX* MSI(I) * MAX(ZERO,-VN(I)) / 
     .             MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
            STIF(I) = STIF(I) + FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FFO = FF
            FF = FF * VN(I)  
            FNI(I)  = FNI(I) + FF
           ENDDO
          ELSE
           DO I=1,JLT-JLT_TIED
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            C(I)= FAC * (
     .       VISC * VIS +
     .       VISCA**2 * DEUX * MSI(I) * MAX(ZERO,-VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
            STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10)
            KT(I)= STIF(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF = C(I) * VN(I)
            FNI(I)  = FNI(I) + FF
            CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDDO
          ENDIF
        ELSEIF(IVIS2.EQ.2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
          DO I=1,JLT-JLT_TIED
            VIS2(I) = DEUX* STIF(I) * MSI(I)
            VIS2(I) = VIS2(I) / 
     .           ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
          ENDDO
C---------------------------------
          VISCA = UNDEMI
          DO I=1,JLT-JLT_TIED
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * (
     .       VISC * VIS +
     .       VISCA**2 * DEUX * MSI(I) * ABS(VN(I)) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)    )
           STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = STIF(I) + DEUX * FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
           FF = FF * VN(I)  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2.EQ.3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT-JLT_TIED
            VIS2(I) = DEUX * STIF(I) * MSI(I)
          ENDDO
C---------------------------------
          DO I=1,JLT-JLT_TIED
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF  = FAC * ( VISC * VIS ) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)
           STIF(I) = STIF(I) * GAPV(I) / 
     .               MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = STIF(I) + DEUX* FF * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
           FF = FF * VN(I)  
           FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2.EQ.4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
          DO I=1,JLT-JLT_TIED
           VIS2(I) = DEUX* STIF(I) * MSI(I)
           VIS = SQRT(VIS2(I)) 
           STIF(I) = STIF(I) * GAPV(I) / 
     .          MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
          ENDDO
        ELSEIF(IVIS2.EQ.5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT-JLT_TIED
           MAS2  = MS(IX1(I))*H1(I)
     .           + MS(IX2(I))*H2(I)
     .           + MS(IX3(I))*H3(I)
     .           + MS(IX4(I))*H4(I)
           VIS2(I) = DEUX* STIF(I) * MSI(I)
           VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
           STIF(I) = STIF(I) * GAPV(I) / 
     .          MAX((GAPV(I) - PENE(I)),EM10) 
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
           FF = VIS * VN(I)  
           ECONTDT = ECONTDT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
           FNI(I)  = MIN(FNI(I),FF)
          ENDDO
        ELSE
        ENDIF
      ELSE
         DO I=1,JLT-JLT_TIED
           IF(VISCFFRIC(I).NE.ZERO) THEN
             IF(IVIS2.EQ.0.OR.IVIS2.EQ.1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
               VIS2(I) = DEUX * STIF(I) * MSI(I)
               IF(VN(I).LT.ZERO) VIS2(I) = VIS2(I) / 
     .           ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
C---------------------------------
C---------------------------------
               VISCA = ZEP4
               IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
                 FAC = STIF(I) / MAX(EM30,STIF(I))
                 VIS = SQRT(VIS2(I)) 
                 FF  = FAC * (
     .           VISC * VIS +
     .           VISCA**2 * DEUX* MSI(I) * MAX(ZERO,-VN(I)) / 
     .                 MAX((GAPV(I) - PENE(I)),EM10)    )
                 STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
                 STIF(I) = STIF(I) + FF * DT1INV
!
                 STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
                 FFO = FF
                 FF = FF * VN(I)  
                 FNI(I)  = FNI(I) + FF
              ELSE
                 FAC = STIF(I) / MAX(EM30,STIF(I))
                 VIS = SQRT(VIS2(I))
                 C(I)= FAC * (
     .                 VISC * VIS +
     .                 VISCA**2 * DEUX * MSI(I) * MAX(ZERO,-VN(I)) / 
     .                 MAX((GAPV(I) - PENE(I)),EM10)    )
                 STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10)
                 KT(I)= STIF(I)
                 STIF(I) = STIF(I) + C(I) * DT1INV
                 FF = C(I) * VN(I)
                 FNI(I)  = FNI(I) + FF
                 CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
                 STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
               ENDIF
             ELSEIF(IVIS2.EQ.2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
               VIS2(I) = DEUX* STIF(I) * MSI(I)
               VIS2(I) = VIS2(I) / 
     .                ( MAX(EM10,(GAPV(I)-PENE(I))/GAPV(I)) )
C---------------------------------
               VISCA = UNDEMI
               FAC = STIF(I) / MAX(EM30,STIF(I))
               VIS = SQRT(VIS2(I)) 
               FF  = FAC * (
     .              VISC * VIS +
     .              VISCA**2 * DEUX * MSI(I) * ABS(VN(I)) / 
     .                  MAX((GAPV(I) - PENE(I)),EM10)    )
               STIF(I) = STIF(I) * GAPV(I) / MAX((GAPV(I) - PENE(I)),EM10) 
               STIF(I) = STIF(I) + DEUX * FF * DT1INV
               STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
               FF = FF * VN(I)  
               FNI(I)  = FNI(I) + FF
            ELSEIF(IVIS2.EQ.3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
               VIS2(I) = DEUX * STIF(I) * MSI(I)
C---------------------------------
               FAC = STIF(I) / MAX(EM30,STIF(I))
               VIS = SQRT(VIS2(I)) 
               FF  = FAC * ( VISC * VIS ) / 
     .               MAX((GAPV(I) - PENE(I)),EM10)
               STIF(I) = STIF(I) * GAPV(I) / 
     .               MAX((GAPV(I) - PENE(I)),EM10) 
               STIF(I) = STIF(I) + DEUX* FF * DT1INV
               STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
               FF = FF * VN(I)  
               FNI(I)  = FNI(I) + FF
            ELSEIF(IVIS2.EQ.4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
              VIS2(I) = DEUX* STIF(I) * MSI(I)
              VIS = SQRT(VIS2(I)) 
              STIF(I) = STIF(I) * GAPV(I) / 
     .          MAX((GAPV(I) - PENE(I)),EM10) 
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            ELSEIF(IVIS2.EQ.5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
               MAS2  = MS(IX1(I))*H1(I)
     .                + MS(IX2(I))*H2(I)
     .                + MS(IX3(I))*H3(I)
     .                + MS(IX4(I))*H4(I)
               VIS2(I) = DEUX* STIF(I) * MSI(I)
               VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 / 
     .               MAX(EM30,MSI(I)+MAS2)
               STIF(I) = STIF(I) * GAPV(I) / 
     .          MAX((GAPV(I) - PENE(I)),EM10) 
               STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
               FF = VIS * VN(I)  
               ECONTDT = ECONTDT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
               FNI(I)  = MIN(FNI(I),FF)
             ELSE
             ENDIF
           ELSE
cbb  initialisation du tableau VIS2 pour eviter des problemes
           VIS2(I) = ZERO
           STIF(I) = STIF(I) * GAPV(I) / 
     .            MAX((GAPV(I) - PENE(I)),EM10) 
           ENDIF
         ENDDO
      ENDIF
C---------------------------------
C     Damping vs tied impacts
C---------------------------------
      IF(ITIED.NE.0)THEN
       IF(VISC.NE.ZERO)THEN
        IF(IVIS2.EQ.0.OR.IVIS2.EQ.1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
           DO I=JLT-JLT_TIED+1,JLT
             FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
             STIF(I) = UNDEMI*STIF0(I) * FAC

             VIS = VISC * SQRT(DEUX * STIF(I) * MSI(I)) 
             FNI(I) = FNI(I) + VN(I)  * VIS
             FT1(I) = FT1(I) + VT1(I) * VIS
             FT2(I) = FT2(I) + VT2(I) * VIS

             STIF(I) = STIF(I)*FAC
             STIF(I) = STIF(I) + DEUX * VIS * DT1INV

             ECONTDT = ECONTDT 
     .               + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
           END DO

          ELSE
           DO I=JLT-JLT_TIED+1,JLT
             FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
             STIF(I) = UNDEMI*STIF0(I) * FAC

             VIS = VISC * SQRT(DEUX * STIF(I) * MSI(I)) 
             FNI(I) = FNI(I) + VN(I)  * VIS
             FT1(I) = FT1(I) + VT1(I) * VIS
             FT2(I) = FT2(I) + VT2(I) * VIS
             C(I)   = VIS

             STIF(I) = STIF(I)*FAC
             KT(I)   = STIF(I)
             STIF(I) = STIF(I) + DEUX * VIS * DT1INV

             ECONTDT = ECONTDT 
     .               + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
           END DO
          ENDIF
        ELSEIF(IVIS2.EQ.2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
          DO I=JLT-JLT_TIED+1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
            STIF(I) = UNDEMI*STIF0(I) * FAC

            VIS = SQRT(DEUX* STIF(I) * MSI(I))  
            FNI(I) = FNI(I) + VN(I)  * VIS
            FT1(I) = FT1(I) + VT1(I) * VIS
            FT2(I) = FT2(I) + VT2(I) * VIS

            STIF(I) = STIF(I)*FAC
            STIF(I) = STIF(I) + DEUX * VIS * DT1INV

            ECONTDT = ECONTDT 
     .              + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
          END DO
        ELSEIF(IVIS2.EQ.3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=JLT-JLT_TIED+1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
            STIF(I) = UNDEMI*STIF0(I) * FAC

            VIS = SQRT(DEUX* STIF(I) * MSI(I))
            FNI(I) = FNI(I) + VN(I)  * VIS
            FT1(I) = FT1(I) + VT1(I) * VIS
            FT2(I) = FT2(I) + VT2(I) * VIS

            STIF(I) = STIF(I)*FAC
            STIF(I) = STIF(I) + DEUX * VIS * DT1INV

            ECONTDT = ECONTDT 
     .              + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
          END DO
        ELSEIF(IVIS2.EQ.4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
        ELSEIF(IVIS2.EQ.5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=JLT-JLT_TIED+1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
            STIF(I) = UNDEMI*STIF0(I) * FAC

            MAS2  = MS(IX1(I))*H1(I)
     .            + MS(IX2(I))*H2(I)
     .            + MS(IX3(I))*H3(I)
     .            + MS(IX4(I))*H4(I)
            VIS = SQRT(DEUX* STIF(I) * MSI(I)) 
            FNI(I) = FNI(I) + VN(I)  * VIS
            FT1(I) = FT1(I) + VT1(I) * VIS
            FT2(I) = FT2(I) + VT2(I) * VIS

            STIF(I) = STIF(I)*FAC
            STIF(I) = STIF(I) + DEUX * VIS * DT1INV

            ECONTDT = ECONTDT 
     .              + VIS * (VX(I)*VX(I)+VY(I)*VY(I)+VZ(I)*VZ(I)) * DT1
          END DO
        ELSE
          DO I=JLT-JLT_TIED+1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
            STIF(I) = UNDEMI*STIF0(I) * FAC
            STIF(I) = STIF(I)*FAC
          END DO
        ENDIF
       ELSE
        DO I=JLT-JLT_TIED+1,JLT
          FAC = GAPV(I)/MAX( EM10,( GAPV(I)-MIN(PENE(I),CAND_F(7,INDEX(I))) ) )
          STIF(I) = UNDEMI*STIF0(I) * FAC
          STIF(I) = STIF(I)*FAC
        END DO
       END IF
      END IF
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
C
      DO I=1,JLT
       FXI(I)=N1(I)*FNI(I)
       FYI(I)=N2(I)*FNI(I)
       FZI(I)=N3(I)*FNI(I)
C      if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
       IMPX=FORC_SIGN*FXI(I)*DT12
       IMPY=FORC_SIGN*FYI(I)*DT12
       IMPZ=FORC_SIGN*FZI(I)*DT12
       FSAV1 =FSAV1 +IMPX
       FSAV2 =FSAV2 +IMPY
       FSAV3 =FSAV3 +IMPZ
       FSAV8 =FSAV8 +ABS(IMPX)
       FSAV9 =FSAV9 +ABS(IMPY)
       FSAV10=FSAV10+ABS(IMPZ)
       FSAV11=FSAV11+ABS(FNI(I))*DT12
      ENDDO
#include "lockon.inc"
      FSAV(1)=FSAV(1)+FSAV1
      FSAV(2)=FSAV(2)+FSAV2
      FSAV(3)=FSAV(3)+FSAV3
      FSAV(8)=FSAV(8)+FSAV8
      FSAV(9)=FSAV(9)+FSAV9
      FSAV(10)=FSAV(10)+FSAV10
      FSAV(11)=FSAV(11)+FSAV11
C
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,1,I+NFT) = FORC_SIGN*FXI(I)
          FSAVPARIT(1,2,I+NFT) = FORC_SIGN*FYI(I)
          FSAVPARIT(1,3,I+NFT) = FORC_SIGN*FZI(I)
        ENDDO
      ENDIF
C---------------------------------
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR.H3D_DATA%MH3D.NE.0))THEN
         IF (INCONV.EQ.1) THEN
#include "lockon.inc"
           DO I=1,JLT
            FNCONT(1,IX1(I)) =FNCONT(1,IX1(I)) + FXI(I)*H1(I)
            FNCONT(2,IX1(I)) =FNCONT(2,IX1(I)) + FYI(I)*H1(I)
            FNCONT(3,IX1(I)) =FNCONT(3,IX1(I)) + FZI(I)*H1(I)
            FNCONT(1,IX2(I)) =FNCONT(1,IX2(I)) + FXI(I)*H2(I)
            FNCONT(2,IX2(I)) =FNCONT(2,IX2(I)) + FYI(I)*H2(I)
            FNCONT(3,IX2(I)) =FNCONT(3,IX2(I)) + FZI(I)*H2(I)
            FNCONT(1,IX3(I)) =FNCONT(1,IX3(I)) + FXI(I)*H3(I)
            FNCONT(2,IX3(I)) =FNCONT(2,IX3(I)) + FYI(I)*H3(I)
            FNCONT(3,IX3(I)) =FNCONT(3,IX3(I)) + FZI(I)*H3(I)
            FNCONT(1,IX4(I)) =FNCONT(1,IX4(I)) + FXI(I)*H4(I)
            FNCONT(2,IX4(I)) =FNCONT(2,IX4(I)) + FYI(I)*H4(I)
            FNCONT(3,IX4(I)) =FNCONT(3,IX4(I)) + FZI(I)*H4(I)
            JG = NSVG(I)
            IF(JG.GT.0) THEN
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
         END IF !(INCONV.EQ.1) THEN
      ENDIF
C
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------

      IF(NISUB.NE.0)THEN
         DO JSUB=1,NISUB
           DO J=1,24
               FSAVSUB1(J,JSUB)=ZERO
           END DO
         ENDDO
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN.GT.0)THEN
           IN=CN_LOC(I)
           IE=CE_LOC(I)
           JJ  =ADDSUBS(IN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBS(IN+1))
             JSUB=LISUBS(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter   

               KSUB=LISUBM(KK)   


               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1))) 
 
                 IF(KSUB.EQ.JSUB)THEN
C                if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
                    IMPX=FORC_SIGN*FXI(I)*DT12
                    IMPY=FORC_SIGN*FYI(I)*DT12
                    IMPZ=FORC_SIGN*FZI(I)*DT12
Clm              MAIN side :
                    FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                    FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                    FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      FSAVPARIT(JSUB+1,1,I+NFT) =  FORC_SIGN*FXI(I)
                      FSAVPARIT(JSUB+1,2,I+NFT) =  FORC_SIGN*FYI(I)
                      FSAVPARIT(JSUB+1,3,I+NFT) =  FORC_SIGN*FZI(I)
                    ENDIF
C
                    FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                    FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                    FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                    FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+ABS(FNI(I))*DT12
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK)

               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : secondary side

                IMPX=FORC_SIGN*FXI(I)*DT12
                IMPY=FORC_SIGN*FYI(I)*DT12
                IMPZ=FORC_SIGN*FZI(I)*DT12

                FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ

                FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+ABS(FNI(I))*DT12

	            IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) =  FORC_SIGN*FXI(I)
                   FSAVPARIT(JSUB+1,2,I+NFT) =  FORC_SIGN*FYI(I)
                   FSAVPARIT(JSUB+1,3,I+NFT) =  FORC_SIGN*FZI(I)
		        ENDIF

                JJ=JJ+1

             ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfs

                ISS2 = BITGET(INFLG_SUBS(JJ),0)
                ISS1 = BITGET(INFLG_SUBS(JJ),1)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1))) 
                   IMS2 = BITGET(INFLG_SUBM(KK),0)
                   IMS1 = BITGET(INFLG_SUBM(KK),1)
                   IF(KSUB.EQ.JSUB)THEN
                     IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                         (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK) 
                        CYCLE             
                     END IF

                     IMPX=FORC_SIGN*FXI(I)*DT12
                     IMPY=FORC_SIGN*FYI(I)*DT12
                     IMPZ=FORC_SIGN*FZI(I)*DT12


                     IF(IMS2 > 0)THEN
                        FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)-IMPX
                        FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)-IMPY
                        FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)-IMPZ
                     ELSE
                        FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                        FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                        FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                     END IF

                     FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                     FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                     FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                     FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+ABS(FNI(I))*DT12


                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0)THEN
                          FSAVPARIT(JSUB+1,1,I+NFT) =  -FORC_SIGN*FXI(I)
                          FSAVPARIT(JSUB+1,2,I+NFT) =  -FORC_SIGN*FYI(I)
                          FSAVPARIT(JSUB+1,3,I+NFT) =  -FORC_SIGN*FZI(I)
                       ELSE
                          FSAVPARIT(JSUB+1,1,I+NFT) =  FORC_SIGN*FXI(I)
                          FSAVPARIT(JSUB+1,2,I+NFT) =  FORC_SIGN*FYI(I)
                          FSAVPARIT(JSUB+1,3,I+NFT) =  FORC_SIGN*FZI(I)
                       END IF
                     ENDIF
                   ENDIF
                   KK=KK+1
                   KSUB=LISUBM(KK) 
                ENDDO
                JJ=JJ+1
             ENDIF
           END DO

          END IF

          IE=CE_LOC(I)

          KK  =ADDSUBM(IE)
          DO WHILE(KK.LT.ADDSUBM(IE+1))
             KSUB=LISUBM(KK)

             ITYPSUB = TYPSUB(KSUB)
             IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

                IMPX=-FORC_SIGN*FXI(I)*DT12
                IMPY=-FORC_SIGN*FYI(I)*DT12
                IMPZ=-FORC_SIGN*FZI(I)*DT12

                FSAVSUB1(1,KSUB)=FSAVSUB1(1,KSUB)+IMPX
                FSAVSUB1(2,KSUB)=FSAVSUB1(2,KSUB)+IMPY
                FSAVSUB1(3,KSUB)=FSAVSUB1(3,KSUB)+IMPZ

                FSAVSUB1(8,KSUB) =FSAVSUB1(8,KSUB) +ABS(IMPX)
                FSAVSUB1(9,KSUB) =FSAVSUB1(9,KSUB) +ABS(IMPY)
                FSAVSUB1(10,KSUB)=FSAVSUB1(10,KSUB)+ABS(IMPZ)

                FSAVSUB1(11,KSUB)=FSAVSUB1(11,KSUB)+ABS(FNI(I))*DT12

		        IF(ISENSINT(KSUB+1)/=0) THEN
                   FSAVPARIT(KSUB+1,1,I+NFT) =  FXI(I)
                   FSAVPARIT(KSUB+1,2,I+NFT) =  FYI(I)
                   FSAVPARIT(KSUB+1,3,I+NFT) =  FZI(I)
		        ENDIF


              ENDIF
              KK=KK+1
           ENDDO
         END DO

        IF(NSPMD.GT.1) THEN
C boucle separee sinon bug PGI en SMP
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN.LT.0)THEN
           NN = -NN
           IE=CE_LOC(I)
           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter   

               KSUB=LISUBM(KK)   
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))    
                 IF(KSUB.EQ.JSUB)THEN
C                if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
                   IMPX=FORC_SIGN*FXI(I)*DT12
                   IMPY=FORC_SIGN*FYI(I)*DT12
                   IMPZ=FORC_SIGN*FZI(I)*DT12
Clm              MAIN side :
                   FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                   FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                   FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     FSAVPARIT(JSUB+1,1,I+NFT) =  FORC_SIGN*FXI(I)
                     FSAVPARIT(JSUB+1,2,I+NFT) =  FORC_SIGN*FYI(I)
                     FSAVPARIT(JSUB+1,3,I+NFT) =  FORC_SIGN*FZI(I)
                   ENDIF
C
                   FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                   FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                   FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                   FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+ABS(FNI(I))*DT12
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

                IMPX=FORC_SIGN*FXI(I)*DT12
                IMPY=FORC_SIGN*FYI(I)*DT12
                IMPZ=FORC_SIGN*FZI(I)*DT12

                FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ

                FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12

	            IF(ISENSINT(JSUB+1)/=0) THEN
                   FSAVPARIT(JSUB+1,1,I+NFT) =  FORC_SIGN*FXI(I)
                   FSAVPARIT(JSUB+1,2,I+NFT) =  FORC_SIGN*FYI(I)
                   FSAVPARIT(JSUB+1,3,I+NFT) =  FORC_SIGN*FZI(I)
		        ENDIF

                JJ=JJ+1

             ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces

                ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
                ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))  
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB.EQ.JSUB)THEN
                     IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                        (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK) 
                        CYCLE          
                     END IF

                     IMPX=FORC_SIGN*FXI(I)*DT12
                     IMPY=FORC_SIGN*FYI(I)*DT12
                     IMPZ=FORC_SIGN*FZI(I)*DT12

                     IF(IMS2 > 0)THEN
                       FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)-IMPX
                       FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)-IMPY
                       FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)-IMPZ
                       FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)-FNI(I)*DT12
                     ELSE
                       FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                       FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                       FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
                       FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
                     END IF
C
                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0)THEN
                         FSAVPARIT(JSUB+1,1,I+NFT) =  -FORC_SIGN*FXI(I)
                         FSAVPARIT(JSUB+1,2,I+NFT) =  -FORC_SIGN*FYI(I)
                         FSAVPARIT(JSUB+1,3,I+NFT) =  -FORC_SIGN*FZI(I)
                       ELSE
                         FSAVPARIT(JSUB+1,1,I+NFT) =  FORC_SIGN*FXI(I)
                         FSAVPARIT(JSUB+1,2,I+NFT) =  FORC_SIGN*FYI(I)
                         FSAVPARIT(JSUB+1,3,I+NFT) =  FORC_SIGN*FZI(I)
                       END IF
                     ENDIF

                     FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                     FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                     FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                   ENDIF
                   KK=KK+1
                   KSUB=LISUBM(KK) 
                ENDDO
                JJ=JJ+1

             ENDIF
           END DO
          END IF
         END DO
        END IF 
      END IF

C------------For /LOAD/PRESSURE tag nodes in contact-------------
      IF(NINTLOADP > 0) THEN
         DO K = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1) 
            PP = LOADPINTER(K)
            PPL = LOADP_HYD_INTER(PP)
            DO I=1,JLT
               TAGNCONT(PPL,IX1(I)) = 1
               TAGNCONT(PPL,IX2(I)) = 1
               TAGNCONT(PPL,IX3(I)) = 1
               TAGNCONT(PPL,IX4(I)) = 1
               JG = NSVG(I)
               IF(JG.GT.0) THEN
C  SPMD : do same after reception of forces for remote nodes 
                  TAGNCONT(PPL,JG) = 1
               ENDIF

            ENDDO
         ENDDO
      ENDIF
C++++++++++++++++++++++++++++++++++++++++
C   Friction coefficient computation
C++++++++++++++++++++++++++++++++++++++++

       IF(IORTHFRIC == 0) THEN 
C++ Isotropic Friction

         IF (MFROT.EQ.0) THEN
C---      Coulomb friction
            DO I=1,JLT-JLT_TIED
               XMU(I) = FRICC(I)
            ENDDO
          ELSEIF (MFROT.EQ.1) THEN
C---      Viscous friction
            DO I=1,JLT-JLT_TIED
              IE=CE_LOC(I)
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV  = SQRT(MAX(EM30,V2))
              AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
              AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
              AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
              AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
              AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
              AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
              AX  = AY1*AZ2 - AZ1*AY2
              AY  = AZ1*AX2 - AX1*AZ2
              AZ  = AX1*AY2 - AY1*AX2
              AREA = UNDEMI*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P 
     .               +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*V2
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO
          ELSEIF(MFROT.EQ.2)THEN
C---        Loi Darmstad
            DO I=1,JLT-JLT_TIED
              IE=CE_LOC(I)
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV = SQRT(MAX(EM30,V2))
              AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
              AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
              AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
              AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
              AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
              AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
              AX  = AY1*AZ2 - AZ1*AY2
              AY  = AZ1*AX2 - AX1*AZ2
              AZ  = AX1*AY2 - AY1*AX2
              AREA = UNDEMI*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) 
     .               + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .               + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .               + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO
          ELSEIF (MFROT.EQ.3) THEN
C---        Loi Renard
            DO I=1,JLT-JLT_TIED
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV = SQRT(MAX(EM30,V2))
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS(I,5)) THEN
                DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
                VV1 = VV / FRIC_COEFS(I,5)
                XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(DEUX-VV1)
              ELSEIF(VV.GT.FRIC_COEFS(I,5).AND.VV.LT.FRIC_COEFS(I,6)) THEN
                DMU = FRIC_COEFS(I,4)-FRIC_COEFS(I,3) 
                VV1 = (VV - FRIC_COEFS(I,5))/(FRIC_COEFS(I,6)-FRIC_COEFS(I,5))
                XMU(I) = FRIC_COEFS(I,3)+ DMU * (TROIS-DEUX*VV1)*VV1**2
              ELSE
                DMU = FRIC_COEFS(I,2)-FRIC_COEFS(I,4)
                VV2 = (VV - FRIC_COEFS(I,6))**2
                XMU(I) = FRIC_COEFS(I,2) - DMU / (UN + DMU*VV2)
              ENDIF
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO
          ENDIF

       ELSE
C++ Orthotropic Friction

          IF (MFROT.EQ.0) THEN
C---      Coulomb friction
#include   "vectorize.inc"
            DO K=1,NFISOT
               I = INDEXISOT(K)
               XMU(I) = FRICC(I)
            ENDDO
#include   "vectorize.inc"
            DO K=1,NFORTH
               I = INDEXORTH(K)
               XMU(I) = FRICC(I)
               XMU2(I) = FRICC2(I)
               IF(XMU(I).LE.EM30) THEN
                 XMU(I) = EM30
                 DIR1(I,1:3) = ZERO
                 AN(K) = ZERO
               ELSE
                 AN(K)=XMU(I)**2  
                 AN(K)=UN/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=UN/BN(K)   
               ENDIF
            ENDDO


          ELSEIF (MFROT.EQ.1) THEN
C---      Viscous friction
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IE=CE_LOC(I)
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV  = SQRT(MAX(EM30,V2))

              AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
              AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
              AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
              AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
              AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
              AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
              AX  = AY1*AZ2 - AZ1*AY2
              AY  = AZ1*AX2 - AX1*AZ2
              AZ  = AX1*AY2 - AY1*AX2
              AREA = UNDEMI*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P 
     .               +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*V2
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples
              I = INDEXORTH(K)
              IE=CE_LOC(I)
c
              AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
              AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
              AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
              AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
              AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
              AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
              AX  = AY1*AZ2 - AZ1*AY2
              AY  = AZ1*AX2 - AX1*AZ2
              AZ  = AX1*AY2 - AY1*AX2
              AREA = UNDEMI*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
c
              V2 = VX(I)*DIR1(I,1) +VY(I)*DIR1(I,2)+VZ(I)*DIR1(I,3)
              VV  = MAX(EM30,V2)
              XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P 
     .               +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*VV*VV

              V2 = VX(I)*DIR2(I,1) +VY(I)*DIR2(I,2)+VZ(I)*DIR2(I,3) 
              VV  = MAX(EM30,V2)
              XMU2(I) = FRICC2(I) + (FRIC_COEFS2(I,1) + FRIC_COEFS2(I,4)*P ) * P 
     .             +(FRIC_COEFS2(I,2) + FRIC_COEFS2(I,3)*P) * VV + FRIC_COEFS2(I,5)*VV*VV

              XMU(I) = MAX(XMU(I),EM30)
              XMU2(I) = MAX(XMU2(I),EM30)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples : treat differently cases where mu1=0 AND/OR mu2=0
               I = INDEXORTH(K)
               IF(XMU(I).LE.EM30) THEN
                 XMU(I) = EM30
                 DIR1(I,1:3) = ZERO
                 AN(K) = ZERO
               ELSE
                 AN(K)=XMU(I)**2  
                 AN(K)=UN/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=UN/BN(K)   
               ENDIF
             ENDDO

          ELSEIF(MFROT.EQ.2)THEN
C---        Loi Darmstad
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IE=CE_LOC(I)
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV = SQRT(MAX(EM30,V2))
              AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
              AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
              AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
              AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
              AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
              AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
              AX  = AY1*AZ2 - AZ1*AY2
              AY  = AZ1*AX2 - AX1*AZ2
              AZ  = AX1*AY2 - AY1*AX2
              AREA = UNDEMI*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) 
     .               + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .               + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .               + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO
c
#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples
              I = INDEXORTH(K)
              IE=CE_LOC(I)
c
              AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
              AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
              AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
              AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
              AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
              AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
              AX  = AY1*AZ2 - AZ1*AY2
              AY  = AZ1*AX2 - AX1*AZ2
              AZ  = AX1*AY2 - AY1*AX2
              AREA = UNDEMI*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
c
              V2 = VX(I)*DIR1(I,1) +VY(I)*DIR1(I,2)+VZ(I)*DIR1(I,3)
              VV  = MAX(EM30,V2)
              XMU(I) = FRICC(I) 
     .               + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .               + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .               + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
c
              V2 = VX(I)*DIR2(I,1) +VY(I)*DIR2(I,2)+VZ(I)*DIR2(I,3)
              VV  = MAX(EM30,V2)
              XMU2(I) = FRICC2(I) 
     .               + FRIC_COEFS2(I,1)*EXP(FRIC_COEFS2(I,2)*VV)*P*P
     .               + FRIC_COEFS2(I,3)*EXP(FRIC_COEFS2(I,4)*VV)*P
     .               + FRIC_COEFS2(I,5)*EXP(FRIC_COEFS2(I,6)*VV)
              XMU(I) = MAX(XMU(I),EM30)
              XMU2(I) = MAX(XMU2(I),EM30)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples : treat differently cases where mu1=0 AND/OR mu2=0
               I = INDEXORTH(K)
               IF(XMU(I).LE.EM30) THEN
                 XMU(I) = EM30
                 DIR1(I,1:3) = ZERO
                 AN(K) = ZERO
               ELSE
                 AN(K)=XMU(I)**2  
                 AN(K)=UN/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=UN/BN(K)   
               ENDIF
             ENDDO

          ELSEIF (MFROT.EQ.3) THEN
C---        Loi Renard
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV = SQRT(MAX(EM30,V2))
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS(I,5)) THEN
                DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
                VV1 = VV / FRIC_COEFS(I,5)
                XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(DEUX-VV1)
              ELSEIF(VV.GT.FRIC_COEFS(I,5).AND.VV.LT.FRIC_COEFS(I,6)) THEN
                DMU = FRIC_COEFS(I,4)-FRIC_COEFS(I,3) 
                VV1 = (VV - FRIC_COEFS(I,5))/(FRIC_COEFS(I,6)-FRIC_COEFS(I,5))
                XMU(I) = FRIC_COEFS(I,3)+ DMU * (TROIS-DEUX*VV1)*VV1**2
              ELSE
                DMU = FRIC_COEFS(I,2)-FRIC_COEFS(I,4)
                VV2 = (VV - FRIC_COEFS(I,6))**2
                XMU(I) = FRIC_COEFS(I,2) - DMU / (UN + DMU*VV2)
              ENDIF
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples
              I = INDEXORTH(K)
c
              V2 = VX(I)*DIR1(I,1) +VY(I)*DIR1(I,2)+VZ(I)*DIR1(I,3)
              VV  = MAX(EM30,V2)
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS(I,5)) THEN
                DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
                VV1 = VV / FRIC_COEFS(I,5)
                XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(DEUX-VV1)
              ELSEIF(VV.GT.FRIC_COEFS(I,5).AND.VV.LT.FRIC_COEFS(I,6)) THEN
                DMU = FRIC_COEFS(I,4)-FRIC_COEFS(I,3) 
                VV1 = (VV - FRIC_COEFS(I,5))/(FRIC_COEFS(I,6)-FRIC_COEFS(I,5))
                XMU(I) = FRIC_COEFS(I,3)+ DMU * (TROIS-DEUX*VV1)*VV1**2
              ELSE
                DMU = FRIC_COEFS(I,2)-FRIC_COEFS(I,4)
                VV2 = (VV - FRIC_COEFS(I,6))**2
                XMU(I) = FRIC_COEFS(I,2) - DMU / (UN + DMU*VV2)
              ENDIF

              V2 = VX(I)*DIR2(I,1) +VY(I)*DIR2(I,2)+VZ(I)*DIR2(I,3)
              VV  = MAX(EM30,V2)
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS2(I,5)) THEN
                DMU = FRIC_COEFS2(I,3)-FRIC_COEFS2(I,1)
                VV1 = VV / FRIC_COEFS2(I,5)
                XMU2(I) = FRIC_COEFS2(I,1)+ DMU*VV1*(DEUX-VV1)
              ELSEIF(VV.GT.FRIC_COEFS2(I,5).AND.VV.LT.FRIC_COEFS2(I,6)) THEN
                DMU = FRIC_COEFS2(I,4)-FRIC_COEFS2(I,3) 
                VV1 = (VV - FRIC_COEFS2(I,5))/(FRIC_COEFS2(I,6)-FRIC_COEFS2(I,5))
                XMU2(I) = FRIC_COEFS2(I,3)+ DMU * (TROIS-DEUX*VV1)*VV1**2
              ELSE
                DMU = FRIC_COEFS2(I,2)-FRIC_COEFS2(I,4)
                VV2 = (VV - FRIC_COEFS2(I,6))**2
                XMU2(I) = FRIC_COEFS2(I,2) - DMU / (UN + DMU*VV2)
              ENDIF
              XMU(I) = MAX(XMU(I),EM30)
              XMU2(I) = MAX(XMU2(I),EM30)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples : treat differently cases where mu1=0 AND/OR mu2=0
               I = INDEXORTH(K)
               IF(XMU(I).LE.EM30) THEN
                 XMU(I) = EM30
                 DIR1(I,1:3) = ZERO
                 AN(K) = ZERO
               ELSE
                 AN(K)=XMU(I)**2  
                 AN(K)=UN/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=UN/BN(K)   
               ENDIF
             ENDDO

          ENDIF

       ENDIF ! IORTHFRIC

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

        IF(IORTHFRIC ==0 ) THEN
C++ Isotropic Friction
C---------------------------------
          IF (IFQ.GE.10) THEN
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
            IF (IFQ.EQ.13) THEN
              ALPHA = MAX(UN,ALPHA0*DT12)
            ELSE
              ALPHA = ALPHA0
            ENDIF
            IF (INCONV.EQ.1) THEN
              DO I=1,JLT-JLT_TIED
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
               BETA = MIN(UN,XMU(I)*SQRT(FN/FT))
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
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
               EFRICT(I) = ZERO
               IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                 EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                       (FZ-FZT(I))*FZT(I) 
                 EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                 QFRICT     =  QFRICT + EFRICT(I)             
               ENDIF
               ECONVT = ECONVT 
     .                + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
             ENDDO


C--------implicit non converge---         
            ELSE
              DO I=1,JLT-JLT_TIED
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
                BETA = MIN(UN,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
                IFPEN(INDEX(I)) = 1
              ENDDO
            ENDIF
C---------------------------------
C         TOTAL (VISCOUS) FORMULATION + FRICTION FILTERING 
C---------------------------------
            ELSEIF (IFQ.GT.0) THEN
            IF (IFQ.EQ.3) THEN
               ALPHA = MAX(UN,ALPHA0*DT12)
            ELSE
               ALPHA = ALPHA0
            ENDIF
            ALPHI = UN - ALPHA
            DO I=1,JLT-JLT_TIED
              VNX = N1(I)*VN(I)
              VNY = N2(I)*VN(I)
              VNZ = N3(I)*VN(I)
              VX(I) = VX(I) - VNX
              VY(I) = VY(I) - VNY
              VZ(I) = VZ(I) - VNZ
              V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
              VIS2(I) = VISCFFRIC(I) * VIS2(I)
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
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
              EFRICT(I) = ZERO
              IF( INTTH > 0) THEN
               EFRICT(I) = DT1*(FXT(I)*VX(I) + FYT(I)*VY(I) + FZT(I)*VZ(I)) ! FRICTIONAL ENERGY 
               ECONVT = ECONVT + EFRICT(I)
               QFRICT  =  QFRICT + EFRICT(I)
              ELSE
               ECONVT = ECONVT 
     .                + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
              ENDIF

            ENDDO
          ELSE
C---------------------------------
C       TOTAL (VISCOUS) FORMULATION / NO FRICTION FILTERING 
C---------------------------------
            DO I=1,JLT-JLT_TIED
              VNX = N1(I)*VN(I)
              VNY = N2(I)*VN(I)
              VNZ = N3(I)*VN(I)
              VX(I) = VX(I) - VNX
              VY(I) = VY(I) - VNY
              VZ(I) = VZ(I) - VNZ
              V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
              VIS2(I) = VISCFFRIC(I) * VIS2(I)
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
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
              EFRICT(I) = ZERO
              IF( INTTH > 0) THEN
                EFRICT(I) = AA * V2 * DT1  ! FRICTIONAL ENERGY 
                ECONVT = ECONVT + EFRICT(I)
                QFRICT  =  QFRICT + EFRICT(I)
              ELSE
                ECONVT = ECONVT + AA * V2 * DT1

              ENDIF

            ENDDO
          ENDIF


        ELSE
C++ Orthotropic Friction

C---------------------------------
          IF (IFQ.GE.10) THEN
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
            IF (IFQ.EQ.13) THEN
               ALPHA = MAX(UN,ALPHA0*DT12)
            ELSE
               ALPHA = ALPHA0
            ENDIF
            IF (INCONV.EQ.1) THEN
#include   "vectorize.inc"
               DO K=1,NFISOT  ! isotropic friction couples
                 I = INDEXISOT(K)
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

                 BETA = MIN(UN,XMU(I)*SQRT(FN/FT))

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

C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
                 EFRICT(I) = ZERO
                 IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                      (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                 ENDIF
                 ECONVT = ECONVT 
     .                  + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
               ENDDO

#include   "vectorize.inc"
               DO K=1,NFORTH ! Orthotropic friction couples
                 I = INDEXORTH(K)
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

                 FTT1= FX*DIR1(I,1) + FY*DIR1(I,2) + FZ*DIR1(I,3)
                 FTT2= FX*DIR2(I,1) + FY*DIR2(I,2) + FZ*DIR2(I,3)

                 FT = FTT1*FTT1*AN(K) + FTT2*FTT2*BN(K)    
                 FT = MAX(FT,EM30)
                 FN = FXI(I)**2+FYI(I)**2+FZI(I)**2

                 BETA = FN/FT

                 IF(BETA == 0 ) THEN
                    FXT(I) = ZERO 
                    FYT(I) = ZERO 
                    FZT(I) = ZERO 
                 ELSEIF(BETA > 1) THEN ! Inside the ellipse
                   FXT(I) = FX 
                   FYT(I) = FY 
                   FZT(I) = FZ 
                 ELSE            ! outside the ellipse
 
!  Projection on local tangent of ellipse (outside of ellipse)
!       ANS = (Fadh-Fproj).n
                   NEP1 =FTT1*AN(K)/FN
                   NEP2 =FTT2*BN(K)/FN
                   NEP  =NEP1*NEP1+NEP2*NEP2
                   NEP  =SQRT(NEP)

                   EP=NEP1*FTT1+NEP2*FTT2

                   ANS=(EP-SQRT(EP))/MAX(EM20,NEP)
                   NEP1 =NEP1/MAX(EM20,NEP)
                   NEP2 =NEP2/MAX(EM20,NEP)

!  Projection on ellipse 
                   C11 =FTT1-ANS*NEP1
                   C22 =FTT2-ANS*NEP2

                   ALPHAF = ATAN(C22/C11)

                   SIGNC = FTT1/MAX(EM20,ABS(FTT1))
                   CSA = SIGNC*ABS(COS(ALPHAF))
                   SIGNC = FTT2/MAX(EM20,ABS(FTT2))
                   SNA = SIGNC*ABS(SIN(ALPHAF))
! Ft computation
                   FT = SQRT(FN / (CSA*CSA*AN(K) + SNA*SNA*BN(K)))
                   FTT1 = FT * CSA
                   FTT2 = FT * SNA

                   FXT(I) = FTT1 * DIR1(I,1) + FTT2 * DIR2(I,1)
                   FYT(I) = FTT1 * DIR1(I,2) + FTT2 * DIR2(I,2) 
                   FZT(I) = FTT1 * DIR1(I,3) + FTT2 * DIR2(I,3) 

                 ENDIF
                 CAND_FX(INDEX(I)) = FXT(I)
                 CAND_FY(INDEX(I)) = FYT(I)
                 CAND_FZ(INDEX(I)) = FZT(I)
                 IFPEN(INDEX(I)) = 1
C-------      total force
                 FXI(I)=FXI(I) + FXT(I)
                 FYI(I)=FYI(I) + FYT(I)
                 FZI(I)=FZI(I) + FZT(I)

C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
                 EFRICT(I) = ZERO
                 IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                         (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                 ENDIF
                 ECONVT = ECONVT 
     .                + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
               ENDDO

C--------implicit non converge---         
            ELSE
#include   "vectorize.inc"
               DO K=1,NFISOT  ! isotropic friction couples
                 I = INDEXISOT(K)
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
                 BETA = MIN(UN,XMU(I)*SQRT(FN/FT))
                 FXT(I) = FX * BETA
                 FYT(I) = FY * BETA
                 FZT(I) = FZ * BETA
                 FXI(I)=FXI(I) + FXT(I)
                 FYI(I)=FYI(I) + FYT(I)
                 FZI(I)=FZI(I) + FZT(I)
                 IFPEN(INDEX(I)) = 1
               ENDDO

#include   "vectorize.inc"
               DO K=1,NFORTH ! Orthotropic friction couples
                 I = INDEXORTH(K)
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

                 FTT1= FX*DIR1(I,1) + FY*DIR1(I,2) + FZ*DIR1(I,3)
                 FTT2= FX*DIR2(I,1) + FY*DIR2(I,2) + FZ*DIR2(I,3)
                 FT = FTT1*FTT1*AN(K) + FTT2*FTT2*BN(K)
                 FT = MAX(FT,EM30)
                 FN = FXI(I)**2+FYI(I)**2+FZI(I)**2

                 BETA = FN/FT
            
                 IF(BETA == 0 ) THEN
                    FXT(I) = ZERO 
                    FYT(I) = ZERO 
                    FZT(I) = ZERO 
                 ELSEIF(BETA > 1) THEN ! Inside the ellipse
                   FXT(I) = FX 
                   FYT(I) = FY 
                   FZT(I) = FZ 
                 ELSE            ! outside the ellipse
 
!  Projection on local tangent of ellipse (outside of ellipse)
!       ANS = (Fadh-Fproj).n
                   NEP1 =FTT1*AN(K)/FN
                   NEP2 =FTT2*BN(K)/FN
                   NEP  =NEP1*NEP1+NEP2*NEP2
                   NEP  =SQRT(NEP)

                   EP=NEP1*FTT1+NEP2*FTT2

                   ANS=(EP-SQRT(EP))/MAX(EM20,NEP)
                   NEP1 =NEP1/MAX(EM20,NEP)
                   NEP2 =NEP2/MAX(EM20,NEP)

!  Projection on ellipse 
                   C11 =FTT1-ANS*NEP1
                   C22 =FTT2-ANS*NEP2

                   ALPHAF = ATAN(C22/C11)

                   SIGNC = FTT1/MAX(EM20,ABS(FTT1))
                   CSA = SIGNC*ABS(COS(ALPHAF))
                   SIGNC = FTT2/MAX(EM20,ABS(FTT2))
                   SNA = SIGNC*ABS(SIN(ALPHAF))
! Ft computation
                   FT = SQRT(FN / (CSA*CSA*AN(K) + SNA*SNA*BN(K)))
                   FTT1 = FT * CSA
                   FTT2 = FT * SNA

                   FXT(I) = FTT1 * DIR1(I,1) + FTT2 * DIR2(I,1)
                   FYT(I) = FTT1 * DIR1(I,2) + FTT2 * DIR2(I,2) 
                   FZT(I) = FTT1 * DIR1(I,3) + FTT2 * DIR2(I,3) 

                 ENDIF

                 FXI(I)=FXI(I) + FXT(I)
                 FYI(I)=FYI(I) + FYT(I)
                 FZI(I)=FZI(I) + FZT(I)
                 IFPEN(INDEX(I)) = 1
               ENDDO


            ENDIF
C---------------------------------
C         TOTAL (VISCOUS) FORMULATION + FRICTION FILTERING 
C---------------------------------
          ELSEIF (IFQ.GT.0) THEN
            IF (IFQ.EQ.3) THEN
               ALPHA = MAX(UN,ALPHA0*DT12)
            ELSE
               ALPHA = ALPHA0
            ENDIF
            ALPHI = UN - ALPHA
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
               I = INDEXISOT(K)
               VNX = N1(I)*VN(I)
               VNY = N2(I)*VN(I)
               VNZ = N3(I)*VN(I)
               VX(I) = VX(I) - VNX
               VY(I) = VY(I) - VNY
               VZ(I) = VZ(I) - VNZ
               V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
               VIS2(I) = VISCFFRIC(I) * VIS2(I)
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
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
              EFRICT(I) = ZERO
              IF( INTTH > 0) THEN
                EFRICT(I) = DT1*(FXT(I)*VX(I) + FYT(I)*VY(I) + FZT(I)*VZ(I)) ! FRICTIONAL ENERGY 
                ECONVT = ECONVT + EFRICT(I)
                QFRICT  =  QFRICT + EFRICT(I)
              ELSE
                ECONVT = ECONVT 
     .                + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
              ENDIF

            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples
              I = INDEXORTH(K)
              VNX = N1(I)*VN(I)
              VNY = N2(I)*VN(I)
              VNZ = N3(I)*VN(I)
              VX(I) = VX(I) - VNX
              VY(I) = VY(I) - VNY
              VZ(I) = VZ(I) - VNZ

              VIS2(I) = VISCFFRIC(I) * VIS2(I)  
            
              VIS = SQRT(VIS2(I))
              FX = VIS*VX(I)
              FY = VIS*VY(I)
              FZ = VIS*VZ(I)

              FTT1= FX*DIR1(I,1) + FY*DIR1(I,2) + FZ*DIR1(I,3)
              FTT2= FX*DIR2(I,1) + FY*DIR2(I,2) + FZ*DIR2(I,3)
              FT = FTT1*FTT1*AN(K) + FTT2*FTT2*BN(K)
              FT = MAX(FT,EM30)
              FN = FNI(I)*FNI(I)

              BETA = FN/FT            

              IF(BETA == 0 ) THEN
                 FXT(I) = ZERO 
                 FYT(I) = ZERO 
                 FZT(I) = ZERO 
              ELSEIF(BETA > 1) THEN ! Inside the ellipse
                 FXT(I) = FX 
                 FYT(I) = FY 
                 FZT(I) = FZ 
              ELSE            ! outside the ellipse
 
!  Projection on local tangent of ellipse (outside of ellipse)
!       ANS = (Fadh-Fproj).n
                 NEP1 =FTT1*AN(K)/FN
                 NEP2 =FTT2*BN(K)/FN
                 NEP  =NEP1*NEP1+NEP2*NEP2
                 NEP  =SQRT(NEP)

                 EP=NEP1*FTT1+NEP2*FTT2

                 ANS=(EP-SQRT(EP))/MAX(EM20,NEP)
                 NEP1 =NEP1/MAX(EM20,NEP)
                 NEP2 =NEP2/MAX(EM20,NEP)

!  Projection on ellipse 
                 C11 =FTT1-ANS*NEP1
                 C22 =FTT2-ANS*NEP2

                 ALPHAF = ATAN(C22/C11)

                 SIGNC = FTT1/MAX(EM20,ABS(FTT1))
                 CSA = SIGNC*ABS(COS(ALPHAF))
                 SIGNC = FTT2/MAX(EM20,ABS(FTT2))
                 SNA = SIGNC*ABS(SIN(ALPHAF))
! Ft computation
                 FT = FNI(I) /SQRT( (CSA*CSA*AN(K) + SNA*SNA*BN(K)))
                 FTT1 = FT * CSA
                 FTT2 = FT * SNA

                 FXT(I) = FTT1 * DIR1(I,1) + FTT2 * DIR2(I,1)
                 FYT(I) = FTT1 * DIR1(I,2) + FTT2 * DIR2(I,2) 
                 FZT(I) = FTT1 * DIR1(I,3) + FTT2 * DIR2(I,3) 

               ENDIF

               FXT(I) = ALPHA*FXT(I) + ALPHI*CAND_FX(INDEX(I))
               FYT(I) = ALPHA*FYT(I) + ALPHI*CAND_FY(INDEX(I))
               FZT(I) = ALPHA*FZT(I) + ALPHI*CAND_FZ(INDEX(I))
               CAND_FX(INDEX(I)) = FXT(I)
               CAND_FY(INDEX(I)) = FYT(I)
               CAND_FZ(INDEX(I)) = FZT(I)
               IFPEN(INDEX(I)) = 1    
C-------      total force
               FXI(I) = FXI(I) + FXT(I)
               FYI(I) = FYI(I) + FYT(I)
               FZI(I) = FZI(I) + FZT(I)
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
               EFRICT(I) = ZERO
               IF( INTTH > 0) THEN
                EFRICT(I) = DT1*(FXT(I)*VX(I) + FYT(I)*VY(I) + FZT(I)*VZ(I)) ! FRICTIONAL ENERGY 
                ECONVT = ECONVT + EFRICT(I)
                QFRICT  =  QFRICT + EFRICT(I)
               ELSE
                ECONVT = ECONVT 
     .                + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
               ENDIF

            ENDDO



          ELSE
C---------------------------------
C       TOTAL (VISCOUS) FORMULATION / NO FRICTION FILTERING 
C---------------------------------
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              VNX = N1(I)*VN(I)
              VNY = N2(I)*VN(I)
              VNZ = N3(I)*VN(I)
              VX(I) = VX(I) - VNX
              VY(I) = VY(I) - VNY
              VZ(I) = VZ(I) - VNZ
              V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
              VIS2(I) = VISCFFRIC(I) * VIS2(I)
              FM2  = (XMU(I)*FNI(I))**2
              F2   = VIS2(I) * V2
              A2 = MIN(F2,FM2) / MAX(EM30,F2)
              AA = SQRT(A2 * VIS2(I))
              FXT(I) = AA * VX(I)
              FYT(I) = AA * VY(I)
              FZT(I) = AA * VZ(I)
C-------      total force
              FXI(I) = FXI(I) + FXT(I)
              FYI(I) = FYI(I) + FYT(I)
              FZI(I) = FZI(I) + FZT(I)
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
              EFRICT(I) = ZERO
              IF( INTTH > 0) THEN
                EFRICT(I) = AA * V2 * DT1  ! FRICTIONAL ENERGY 
                ECONVT = ECONVT + EFRICT(I)
                QFRICT  =  QFRICT + EFRICT(I)
              ELSE
                ECONVT = ECONVT + AA * V2 * DT1

              ENDIF

            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples
              I = INDEXORTH(K)
              VNX = N1(I)*VN(I)
              VNY = N2(I)*VN(I)
              VNZ = N3(I)*VN(I)
              VX(I) = VX(I) - VNX
              VY(I) = VY(I) - VNY
              VZ(I) = VZ(I) - VNZ
              VIS2(I) = VISCFFRIC(I) * VIS2(I)

              VIS = SQRT(VIS2(I))
              FX = VIS*VX(I)
              FY = VIS*VY(I)
              FZ = VIS*VZ(I)

              FTT1= FX*DIR1(I,1) + FY*DIR1(I,2) + FZ*DIR1(I,3)
              FTT2= FX*DIR2(I,1) + FY*DIR2(I,2) + FZ*DIR2(I,3)
              FT = FTT1*FTT1*AN(K) + FTT2*FTT2*BN(K)
              FT = MAX(FT,EM30)
              FN = FNI(I)*FNI(I)

              BETA = FN/FT
             
              IF(BETA == 0 ) THEN
                    FXT(I) = ZERO 
                    FYT(I) = ZERO 
                    FZT(I) = ZERO 
              ELSEIF(BETA > 1) THEN ! Inside the ellipse
                 FXT(I) = FX 
                 FYT(I) = FY 
                 FZT(I) = FZ 
              ELSE            ! outside the ellipse
 
!  Projection on local tangent of ellipse (outside of ellipse)
!       ANS = (Fadh-Fproj).n
                 NEP1 =FTT1*AN(K)/FN
                 NEP2 =FTT2*BN(K)/FN
                 NEP  =NEP1*NEP1+NEP2*NEP2
                 NEP  =SQRT(NEP)

                 EP=NEP1*FTT1+NEP2*FTT2

                 ANS=(EP-SQRT(EP))/MAX(EM20,NEP)
                 NEP1 =NEP1/MAX(EM20,NEP)
                 NEP2 =NEP2/MAX(EM20,NEP)

!  Projection on ellipse 
                 C11 =FTT1-ANS*NEP1
                 C22 =FTT2-ANS*NEP2

                 ALPHAF = ATAN(C22/C11)

                 SIGNC = FTT1/MAX(EM20,ABS(FTT1))
                 CSA = SIGNC*ABS(COS(ALPHAF))
                 SIGNC = FTT2/MAX(EM20,ABS(FTT2))
                 SNA = SIGNC*ABS(SIN(ALPHAF))
! Ft computation
                 FT = FNI(I) /SQRT( (CSA*CSA*AN(K) + SNA*SNA*BN(K)))
                 FTT1 = FT * CSA
                 FTT2 = FT * SNA

                 FXT(I) = FTT1 * DIR1(I,1) + FTT2 * DIR2(I,1)
                 FYT(I) = FTT1 * DIR1(I,2) + FTT2 * DIR2(I,2) 
                 FZT(I) = FTT1 * DIR1(I,3) + FTT2 * DIR2(I,3) 

               ENDIF


C-------      total force
              FXI(I)=FXI(I) + FXT(I)
              FYI(I)=FYI(I) + FYT(I)
              FZI(I)=FZI(I) + FZT(I)
C---------------------------------
C       CONTACT ENERGY CALCULATION
C---------------------------------
              EFRICT(I) = ZERO
              IF( INTTH > 0) THEN
                EFRICT(I) = AA * V2 * DT1  ! FRICTIONAL ENERGY 
                ECONVT = ECONVT + EFRICT(I)
                QFRICT  =  QFRICT + EFRICT(I)
              ELSE
                ECONVT = ECONVT + AA * V2 * DT1
              ENDIF

            ENDDO


          ENDIF

        ENDIF
C---------------------------------
        IF(ITIED /= 0)THEN
         DO I=JLT-JLT_TIED+1,JLT
           FXT(I)= T1X(I)*FT1(I) + T2X(I)*FT2(I)
           FYT(I)= T1Y(I)*FT1(I) + T2Y(I)*FT2(I)
           FZT(I)= T1Z(I)*FT1(I) + T2Z(I)*FT2(I)
C--------      total force
           FXI(I)=FXI(I) + FXT(I)
           FYI(I)=FYI(I) + FYT(I)
           FZI(I)=FZI(I) + FZT(I)
         END DO
        END IF
C---------------------------------
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR.H3D_DATA%MH3D.NE.0))THEN
         IF (INCONV.EQ.1) THEN
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
            IF(JG.GT.0) THEN
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
         END IF !(INCONV.EQ.1) THEN
      ENDIF
C
C---------------------------------
      FSAV22= ZERO
      FSAV23= ZERO
      FSAV24= ZERO
      DO I=1,JLT
C      if type7 sym of type19 opposite sign for tangential force -> forc_sign = -1
       IMPX=FORC_SIGN*FXT(I)*DT12
       IMPY=FORC_SIGN*FYT(I)*DT12
       IMPZ=FORC_SIGN*FZT(I)*DT12
       FSAV4 =FSAV4 +IMPX
       FSAV5 =FSAV5 +IMPY
       FSAV6 =FSAV6 +IMPZ
C      if type7 sym of type19 opposite sign for tangential force -> forc_sign = -1
       IMPX=FORC_SIGN*FXI(I)*DT12
       IMPY=FORC_SIGN*FYI(I)*DT12
       IMPZ=FORC_SIGN*FZI(I)*DT12
       FSAV12=FSAV12+ABS(IMPX)
       FSAV13=FSAV13+ABS(IMPY)
       FSAV14=FSAV14+ABS(IMPZ)
       FSAV15=FSAV15+SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
       XP(I)=XI(I)+PENE(I)*N1(I)
       YP(I)=YI(I)+PENE(I)*N2(I)
       ZP(I)=ZI(I)+PENE(I)*N3(I)
       FSAV22=FSAV22+YP(I)*IMPZ-ZP(I)*IMPY
       FSAV23=FSAV23+ZP(I)*IMPX-XP(I)*IMPZ
       FSAV24=FSAV24+XP(I)*IMPY-YP(I)*IMPX
      ENDDO
#include "lockon.inc"
        FSAV(4) = FSAV(4) + FSAV4
        FSAV(5) = FSAV(5) + FSAV5
        FSAV(6) = FSAV(6) + FSAV6
        FSAV(12) = FSAV(12) + FSAV12
        FSAV(13) = FSAV(13) + FSAV13
        FSAV(14) = FSAV(14) + FSAV14
        FSAV(15) = FSAV(15) + FSAV15
        FSAV(22) = FSAV(22) + FSAV22
        FSAV(23) = FSAV(23) + FSAV23
        FSAV(24) = FSAV(24) + FSAV24
        FSAV(25) = FSAV(25) + (FHEATS+FHEATM)*QFRICT
        FSAV(26) = FSAV(26) + ECONTT  ! ECONTTIED part is not added in CE_ELAST for the moment
        FSAV(27) = FSAV(27) + ECONVT- (FHEATS+FHEATM)*QFRICT
        FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,4,I+NFT) =  FORC_SIGN*FXT(I)
          FSAVPARIT(1,5,I+NFT) =  FORC_SIGN*FYT(I)
          FSAVPARIT(1,6,I+NFT) =  FORC_SIGN*FZT(I)
        ENDDO
      ENDIF
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB.NE.0)THEN
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN.GT.0)THEN
           IN=CN_LOC(I)
           IE=CE_LOC(I)
           JJ  =ADDSUBS(IN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBS(IN+1))
             JSUB=LISUBS(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter   

               KSUB=LISUBM(KK)   
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))    
                 IF(KSUB.EQ.JSUB)THEN
C                if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
                   IMPX=FORC_SIGN*FXT(I)*DT12
                   IMPY=FORC_SIGN*FYT(I)*DT12
                   IMPZ=FORC_SIGN*FZT(I)*DT12
Clm              MAIN side :
                   FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                   FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                   FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
C                if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
                   IMPX=FORC_SIGN*FXI(I)*DT12
                   IMPY=FORC_SIGN*FYI(I)*DT12
                   IMPZ=FORC_SIGN*FZI(I)*DT12
                   FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                   FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                   FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                     FSAVPARIT(JSUB+1,4,I+NFT) =  FORC_SIGN*FXT(I)
                     FSAVPARIT(JSUB+1,5,I+NFT) =  FORC_SIGN*FYT(I)
                     FSAVPARIT(JSUB+1,6,I+NFT) =  FORC_SIGN*FZT(I)
                   ENDIF
C
                   FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                             +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                   FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                             +YP(I)*IMPZ-ZP(I)*IMPY
                   FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                             +ZP(I)*IMPX-XP(I)*IMPZ
                   FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                             +XP(I)*IMPY-YP(I)*IMPX
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

C            if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
               IMPX=FORC_SIGN*FXT(I)*DT12
               IMPY=FORC_SIGN*FYT(I)*DT12
               IMPZ=FORC_SIGN*FZT(I)*DT12
Cl           MAIN side :
               FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
               FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
               FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
C            if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
               IMPX=FORC_SIGN*FXI(I)*DT12
               IMPY=FORC_SIGN*FYI(I)*DT12
               IMPZ=FORC_SIGN*FZI(I)*DT12
               FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
               FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
               FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
               IF(ISENSINT(JSUB+1)/=0) THEN
                  FSAVPARIT(JSUB+1,4,I+NFT) =  FORC_SIGN*FXT(I)
                  FSAVPARIT(JSUB+1,5,I+NFT) =  FORC_SIGN*FYT(I)
                  FSAVPARIT(JSUB+1,6,I+NFT) =  FORC_SIGN*FZT(I)
               ENDIF
C
               FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                          +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
               FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                          +YP(I)*IMPZ-ZP(I)*IMPY
               FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                          +ZP(I)*IMPX-XP(I)*IMPZ
               FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                          +XP(I)*IMPY-YP(I)*IMPX

               JJ=JJ+1

             ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces
C         
C            Find if node is on Surface S1 -> S2  or S2 ->S1
                ISS2 = BITGET(INFLG_SUBS(JJ),0)
                ISS1 = BITGET(INFLG_SUBS(JJ),1)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))   
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB.EQ.JSUB)THEN
!                S and M candidates on the same sub_interface
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK) 
                        CYCLE            
                    END IF

                     IMPX=FORC_SIGN*FXT(I)*DT12
                     IMPY=FORC_SIGN*FYT(I)*DT12
                     IMPZ=FORC_SIGN*FZT(I)*DT12
                     IF(IMS2 > 0) THEN
Clm              main side :
                        FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)-IMPX
                        FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)-IMPY
                        FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)-IMPZ
                     ELSE
                        FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                        FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                        FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
                     ENDIF
C
                     IMPX=FORC_SIGN*FXI(I)*DT12
                     IMPY=FORC_SIGN*FYI(I)*DT12
                     IMPZ=FORC_SIGN*FZI(I)*DT12
                     FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                     FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                     FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0) THEN
                         FSAVPARIT(JSUB+1,4,I+NFT) =  -FORC_SIGN*FXT(I)
                         FSAVPARIT(JSUB+1,5,I+NFT) =  -FORC_SIGN*FYT(I)
                         FSAVPARIT(JSUB+1,6,I+NFT) =  -FORC_SIGN*FZT(I)
                       ELSE
                         FSAVPARIT(JSUB+1,4,I+NFT) =  FORC_SIGN*FXT(I)
                         FSAVPARIT(JSUB+1,5,I+NFT) =  FORC_SIGN*FYT(I)
                         FSAVPARIT(JSUB+1,6,I+NFT) =  FORC_SIGN*FZT(I)
                       ENDIF
                     ENDIF
C 
                     FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                               +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                     FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                           +YP(I)*IMPZ-ZP(I)*IMPY
                     FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
                     FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX
                   ENDIF
                   KK=KK+1
                   KSUB=LISUBM(KK) 
                ENDDO
                JJ=JJ+1

             ENDIF

           END DO
          END IF

          IE=CE_LOC(I)

          KK  =ADDSUBM(IE) ! Addresse du M
          DO WHILE(KK.LT.ADDSUBM(IE+1))
!             all sub interfaces of S           
             KSUB=LISUBM(KK)
!            KSUB Croissant
             ITYPSUB = TYPSUB(KSUB)
             IF(ITYPSUB == 2 ) THEN ! Inter =0 : collecting forces from all inter with only 1 surface
                IMPX=-FORC_SIGN*FXT(I)*DT12
                IMPY=-FORC_SIGN*FYT(I)*DT12
                IMPZ=-FORC_SIGN*FZT(I)*DT12
Clm              main side :
                FSAVSUB1(4,KSUB)=FSAVSUB1(4,KSUB)+IMPX
                FSAVSUB1(5,KSUB)=FSAVSUB1(5,KSUB)+IMPY
                FSAVSUB1(6,KSUB)=FSAVSUB1(6,KSUB)+IMPZ
C
                IMPX=FXI(I)*DT12
                IMPY=FYI(I)*DT12
                IMPZ=FZI(I)*DT12
                FSAVSUB1(12,KSUB)=FSAVSUB1(12,KSUB)+ABS(IMPX)
                FSAVSUB1(13,KSUB)=FSAVSUB1(13,KSUB)+ABS(IMPY)
                FSAVSUB1(14,KSUB)=FSAVSUB1(14,KSUB)+ABS(IMPZ)
C
                IF(ISENSINT(KSUB+1)/=0) THEN
                  FSAVPARIT(KSUB+1,4,I+NFT) =  -FORC_SIGN*FXT(I)
                  FSAVPARIT(KSUB+1,5,I+NFT) =  -FORC_SIGN*FYT(I)
                  FSAVPARIT(KSUB+1,6,I+NFT) =  -FORC_SIGN*FZT(I)
                ENDIF
C
               FSAVSUB1(15,KSUB)= FSAVSUB1(15,KSUB)
     .                          +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
               FSAVSUB1(22,KSUB)=FSAVSUB1(22,KSUB)
     .                          +YP(I)*IMPZ-ZP(I)*IMPY
               FSAVSUB1(23,KSUB)=FSAVSUB1(23,KSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
               FSAVSUB1(24,KSUB)=FSAVSUB1(24,KSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX

             ENDIF
             KK=KK+1
           ENDDO
         END DO
        IF(NSPMD.GT.1) THEN
C boucle separee sinon bug PGI en SMP
         DO I=1,JLT
          NN = NSVG(I)
          IF(NN.LT.0)THEN
           NN = -NN
           IE=CE_LOC(I)
           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             ITYPSUB = TYPSUB(JSUB)

             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter  

               KSUB=LISUBM(KK)   
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))   
                 IF(KSUB.EQ.JSUB)THEN
C                if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
                    IMPX=FORC_SIGN*FXT(I)*DT12
                    IMPY=FORC_SIGN*FYT(I)*DT12
                    IMPZ=FORC_SIGN*FZT(I)*DT12
Clm              MAIN side :
                    FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                    FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                    FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
C                if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
                    IMPX=FORC_SIGN*FXI(I)*DT12
                    IMPY=FORC_SIGN*FYI(I)*DT12
                    IMPZ=FORC_SIGN*FZI(I)*DT12
                    FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                    FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                    FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C 
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      FSAVPARIT(JSUB+1,4,I+NFT) =  FORC_SIGN*FXT(I)
                      FSAVPARIT(JSUB+1,5,I+NFT) =  FORC_SIGN*FYT(I)
                      FSAVPARIT(JSUB+1,6,I+NFT) =  FORC_SIGN*FZT(I)
                    ENDIF
C
                    FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                           +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                    FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                           +YP(I)*IMPZ-ZP(I)*IMPY
                    FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
                    FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX
C
                 END IF

                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

C            if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
               IMPX=FORC_SIGN*FXT(I)*DT12
               IMPY=FORC_SIGN*FYT(I)*DT12
               IMPZ=FORC_SIGN*FZT(I)*DT12
Cl           MAIN side :
               FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
               FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
               FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
C
C            if type7 sym of type19 opposite sign for normal force -> forc_sign = -1
               IMPX=FORC_SIGN*FXI(I)*DT12
               IMPY=FORC_SIGN*FYI(I)*DT12
               IMPZ=FORC_SIGN*FZI(I)*DT12
               FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
               FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
               FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
               IF(ISENSINT(JSUB+1)/=0) THEN
                  FSAVPARIT(JSUB+1,4,I+NFT) =  FORC_SIGN*FXT(I)
                  FSAVPARIT(JSUB+1,5,I+NFT) =  FORC_SIGN*FYT(I)
                  FSAVPARIT(JSUB+1,6,I+NFT) =  FORC_SIGN*FZT(I)
               ENDIF
C
               FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                          +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
               FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                          +YP(I)*IMPZ-ZP(I)*IMPY
               FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                          +ZP(I)*IMPX-XP(I)*IMPZ
               FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                          +XP(I)*IMPY-YP(I)*IMPX

               JJ=JJ+1
             ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces

               ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
               ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
               KSUB=LISUBM(KK)  
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1))) 
                 IMS2 = BITGET(INFLG_SUBM(KK),0)
                 IMS1 = BITGET(INFLG_SUBM(KK),1)
                 IF(KSUB.EQ.JSUB)THEN
                   IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                      (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBM(KK) 
                      CYCLE            
                   END IF

                   IMPX=FORC_SIGN*FXT(I)*DT12
                   IMPY=FORC_SIGN*FYT(I)*DT12
                   IMPZ=FORC_SIGN*FZT(I)*DT12
                   IF(IMS2 > 0) THEN
Clm              main side :
                      FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)-IMPX
                      FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)-IMPY
                      FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)-IMPZ
                   ELSE
                      FSAVSUB1(4,JSUB)=FSAVSUB1(4,JSUB)+IMPX
                      FSAVSUB1(5,JSUB)=FSAVSUB1(5,JSUB)+IMPY
                      FSAVSUB1(6,JSUB)=FSAVSUB1(6,JSUB)+IMPZ
                   ENDIF
C
                   IMPX=FXI(I)*DT12
                   IMPY=FYI(I)*DT12
                   IMPZ=FZI(I)*DT12
                   FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                   FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                   FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                   IF(ISENSINT(JSUB+1)/=0) THEN
                      IF(IMS2 > 0) THEN
                         FSAVPARIT(JSUB+1,4,I) =  FORC_SIGN*FXT(I)
                         FSAVPARIT(JSUB+1,5,I) =  FORC_SIGN*FYT(I)
                         FSAVPARIT(JSUB+1,6,I) =  FORC_SIGN*FZT(I)
                      ELSE
                         FSAVPARIT(JSUB+1,4,I) =  -FORC_SIGN*FXT(I)
                         FSAVPARIT(JSUB+1,5,I) =  -FORC_SIGN*FYT(I)
                         FSAVPARIT(JSUB+1,6,I) =  -FORC_SIGN*FZT(I)
                      ENDIF
                   ENDIF
C 
                   FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                              +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                   FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                           +YP(I)*IMPZ-ZP(I)*IMPY
                   FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
                   FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX
                 ENDIF
                 KK=KK+1
                 KSUB=LISUBM(KK) 
               ENDDO
               JJ=JJ+1

             ENDIF
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
           FSAVSUB(22,NSUB)=FSAVSUB(22,NSUB)+FSAVSUB1(22,JSUB)
           FSAVSUB(23,NSUB)=FSAVSUB(23,NSUB)+FSAVSUB1(23,JSUB)
           FSAVSUB(24,NSUB)=FSAVSUB(24,NSUB)+FSAVSUB1(24,JSUB)
         END DO
#include "lockoff.inc"
      END IF
C---------------------------------
#include "lockon.inc"
        IF (INCONV.EQ.1) THEN
        ECONTV = ECONTV + ECONVT ! Frictional Energy
        ECONT  = ECONT + ECONTT  ! Elatic Energy
        ECONTD = ECONTD + ECONTDT ! Damping Energy
        ECONT_CUMU = ECONT_CUMU + ECONTTIED ! Elastic energy for tied contact : it is cumulated energy
        END IF !(INCONV.EQ.1) THEN
        IF (INTTH.NE.0) THEN
           QFRIC = QFRIC + (FHEATS+FHEATM)*QFRICT    ! FRICTIONAL HEAT ADDED TO INTERNAL ENERGY
           ECONTV = ECONTV - (FHEATS+FHEATM)*QFRICT  ! FRICTIONAL HEAT REMOVED FROM CONTACT ENERGY
        ENDIF
#include "lockoff.inc"
C---------------------------------
      IF(KDTINT.EQ.1)THEN
       IF(    (VISC.NE.ZERO)
     .   .AND.(IVIS2.EQ.0.OR.IVIS2.EQ.1))THEN
        DO I=1,JLT
C        C(I)=2.*C(I)
C
         IF(MSI(I).EQ.ZERO)THEN
          KS(I) =ZERO
          CS(I) =ZERO
          STV(I)=ZERO
         ELSE
          CX  = QUATRE*C(I)*C(I)
          CY  = HUIT*MSI(I)*KT(I)
          AUX   = SQRT(CX+CY)+DEUX*C(I)
          STV(I)= KT(I)*AUX*AUX/MAX(CY,EM30)
          AUX   = DEUX*CF(I)*CF(I)/MAX(MSI(I),EM20)
          IF(AUX.GT.STV(I))THEN
           KS(I) =ZERO
           CS(I) =CF(I)
           STV(I)=AUX
          ELSE
           KS(I)= KT(I)
           CS(I) =C(I)
          ENDIF
         ENDIF      
C
         J1=IX1(I)
         IF(MS(J1).EQ.ZERO)THEN
          K1(I) =ZERO
          C1(I) =ZERO
          ST1(I)=ZERO
         ELSE
          K1(I)=KT(I)*ABS(H1(I))
          C1(I)=C(I)*ABS(H1(I))
          CX   =QUATRE*C1(I)*C1(I)
          CY   =HUIT*MS(J1)*K1(I)
          AUX   = SQRT(CX+CY)+DEUX*C1(I)
          ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H1(I))
          AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX.GT.ST1(I))THEN
           K1(I) =ZERO
           C1(I) =CFI
           ST1(I)=AUX
          ENDIF         
         ENDIF      
C
         J1=IX2(I)
         IF(MS(J1).EQ.ZERO)THEN
          K2(I) =ZERO
          C2(I) =ZERO
          ST2(I)=ZERO
         ELSE
          K2(I)=KT(I)*ABS(H2(I))
          C2(I)=C(I)*ABS(H2(I))
          CX   =QUATRE*C2(I)*C2(I)
          CY   =HUIT*MS(J1)*K2(I)
          AUX   = SQRT(CX+CY)+DEUX*C2(I)
          ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H2(I))
          AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX.GT.ST2(I))THEN
           K2(I) =ZERO
           C2(I) =CFI
           ST2(I)=AUX
          ENDIF         
         ENDIF      
C
         J1=IX3(I)
         IF(MS(J1).EQ.ZERO)THEN
          K3(I) =ZERO
          C3(I) =ZERO
          ST3(I)=ZERO
         ELSE
          K3(I)=KT(I)*ABS(H3(I))
          C3(I)=C(I)*ABS(H3(I))
          CX   =QUATRE*C3(I)*C3(I)
          CY   =HUIT*MS(J1)*K3(I)
          AUX   = SQRT(CX+CY)+DEUX*C3(I)
          ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H3(I))
          AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX.GT.ST3(I))THEN
           K3(I) =ZERO
           C3(I) =CFI
           ST3(I)=AUX
          ENDIF         
         ENDIF      
C
         J1=IX4(I)
         IF(MS(J1).EQ.ZERO)THEN
          K4(I) =ZERO
          C4(I) =ZERO
          ST4(I)=ZERO
         ELSE
          K4(I)=KT(I)*ABS(H4(I))
          C4(I)=C(I)*ABS(H4(I))
          CX   =QUATRE*C4(I)*C4(I)
          CY   =HUIT*MS(J1)*K4(I)
          AUX   = SQRT(CX+CY)+DEUX*C4(I)
          ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H4(I))
          AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
          IF(AUX.GT.ST4(I))THEN
           K4(I) =ZERO
           C4(I) =CFI
           ST4(I)=AUX
          ENDIF         
         ENDIF      
        ENDDO
C
       ELSE
         DO I=1,JLT
           IF(    (VISCFFRIC(I).NE.ZERO)
     .       .AND.(IVIS2.EQ.0.OR.IVIS2.EQ.1))THEN
             IF(MSI(I).EQ.ZERO)THEN
              KS(I) =ZERO
              CS(I) =ZERO
              STV(I)=ZERO
             ELSE
              CX  = QUATRE*C(I)*C(I)
              CY  = HUIT*MSI(I)*KT(I)
              AUX   = SQRT(CX+CY)+DEUX*C(I)
              STV(I)= KT(I)*AUX*AUX/MAX(CY,EM30)
              AUX   = DEUX*CF(I)*CF(I)/MAX(MSI(I),EM20)
              IF(AUX.GT.STV(I))THEN
                KS(I) =ZERO
                CS(I) =CF(I)
                STV(I)=AUX
              ELSE
                KS(I)= KT(I)
                CS(I) =C(I)
              ENDIF
             ENDIF      
C
             J1=IX1(I)
             IF(MS(J1).EQ.ZERO)THEN
               K1(I) =ZERO
               C1(I) =ZERO
               ST1(I)=ZERO
             ELSE
               K1(I)=KT(I)*ABS(H1(I))
               C1(I)=C(I)*ABS(H1(I))
               CX   =QUATRE*C1(I)*C1(I)
               CY   =HUIT*MS(J1)*K1(I)
               AUX   = SQRT(CX+CY)+DEUX*C1(I)
               ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
               CFI   = CF(I)*ABS(H1(I))
              AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
              IF(AUX.GT.ST1(I))THEN
                K1(I) =ZERO
                C1(I) =CFI
                ST1(I)=AUX
              ENDIF         
             ENDIF      
C
             J1=IX2(I)
             IF(MS(J1).EQ.ZERO)THEN
               K2(I) =ZERO
               C2(I) =ZERO
               ST2(I)=ZERO
             ELSE
               K2(I)=KT(I)*ABS(H2(I))
               C2(I)=C(I)*ABS(H2(I))
               CX   =QUATRE*C2(I)*C2(I)
               CY   =HUIT*MS(J1)*K2(I)
               AUX   = SQRT(CX+CY)+DEUX*C2(I)
               ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
               CFI   = CF(I)*ABS(H2(I))
               AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
               IF(AUX.GT.ST2(I))THEN
                 K2(I) =ZERO
                 C2(I) =CFI
                 ST2(I)=AUX
               ENDIF         
              ENDIF      
C
             J1=IX3(I)
             IF(MS(J1).EQ.ZERO)THEN
               K3(I) =ZERO
               C3(I) =ZERO
               ST3(I)=ZERO
             ELSE
               K3(I)=KT(I)*ABS(H3(I))
               C3(I)=C(I)*ABS(H3(I))
               CX   =QUATRE*C3(I)*C3(I)
               CY   =HUIT*MS(J1)*K3(I)
               AUX   = SQRT(CX+CY)+DEUX*C3(I)
               ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
               CFI   = CF(I)*ABS(H3(I))
               AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
               IF(AUX.GT.ST3(I))THEN
                 K3(I) =ZERO
                 C3(I) =CFI
                 ST3(I)=AUX
               ENDIF         
             ENDIF      
C
             J1=IX4(I)
             IF(MS(J1).EQ.ZERO)THEN
                K4(I) =ZERO
                C4(I) =ZERO
                ST4(I)=ZERO
             ELSE
                K4(I)=KT(I)*ABS(H4(I))
                C4(I)=C(I)*ABS(H4(I))
                CX   =QUATRE*C4(I)*C4(I)
                CY   =HUIT*MS(J1)*K4(I)
                AUX   = SQRT(CX+CY)+DEUX*C4(I)
                ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
                CFI   = CF(I)*ABS(H4(I))
                AUX   = DEUX*CFI*CFI/MAX(MS(J1),EM20)
                IF(AUX.GT.ST4(I))THEN
                  K4(I) =ZERO
                  C4(I) =CFI
                  ST4(I)=AUX
                ENDIF         
              ENDIF   
           ELSE
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
           ENDIF
         ENDDO
       ENDIF
      ENDIF
C
C---------------------
      IF(ITIED /= 0)THEN
C
        DTI = 1.E20
C
        DO I=1,JLT-JLT_TIED
          DIST  = GAPV(I)-PENE(I)
          RDIST = UNDEMI*DIST / MAX(EM30,-VN(I))
          DTI   = MIN(RDIST,DTI)
        END DO
C
        IF (DTMINI.GT.ZERO) THEN 
          DTM =DTMINI
        ELSE 
          DTM =EM30
        END IF

        IF(DTI.LE.DTM)THEN
          DTI = 1.E20
          DO I=1,JLT-JLT_TIED
              DIST =GAPV(I)-PENE(I)
              DTI2   = UNDEMI*DIST / MAX(EM30,-VN(I))
              IF(DTI2.LE.DTM.AND.CAND_F(1,INDEX(I)).EQ.ZERO)THEN
               NN = NSVG(I)
               IF(NN.GT.0)THEN
                 NI = ITAB(NN)
               ELSE
                 NI = ITAFI(NIN)%P(-NN)
               ENDIF
#include "lockon.inc"
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .              ' **WARNING MINIMUM TIME STEP ',DTI2,
     .              ' IN INTERFACE ',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,'(A,I10,A,I10)')'   TYING SECONDARY NODE VS MAIN SEGMENT',
     .             NI,' IN INTERFACE ',NOINT           
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .           ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
               CAND_F(1,INDEX(I))= FNS(I)*SIGN(UN,SIDE(I)) ! negative vs NX NY NZ
               CAND_F(2,INDEX(I))= FXT(I)*T1X(I)+FYT(I)*T1Y(I)+FZT(I)*T1Z(I)
               CAND_F(3,INDEX(I))= FXT(I)*T2X(I)+FYT(I)*T2Y(I)+FZT(I)*T2Z(I)
               CAND_F(4,INDEX(I))= H1(I)
               CAND_F(5,INDEX(I))= H2(I)
               CAND_F(6,INDEX(I))= H3(I)
               CAND_F(7,INDEX(I))= PENE(I)
               CAND_F(8,INDEX(I))= SIDE(I)

              ENDIF                 
          ENDDO
        ENDIF

      ENDIF ! IF(ITIED /= 0)THEN

C--------------------------------------------
      IF(ITIED == 0)THEN

       IF(IDTM.EQ.1.OR.IDTM.EQ.2.OR.
     .    IDTM.EQ.5.OR.IDTM.EQ.6)THEN
         DTMI0 = EP20
         IF(KDTINT.EQ.0)THEN
          DO I=1,JLT
           DTMI(I) = EP20
           MAS2  = DEUX * MSI(I)
           IF(MAS2.GT.ZERO.AND.STIF(I).GT.ZERO.AND.
     .       IRB(KINI(I)).EQ.0.AND.IRB2(KINI(I)).EQ.0)THEN
             DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STIF(I)))
           ENDIF
           MAS2  = DEUX* MS(IX1(I))
           IF(MAS2.GT.ZERO.AND.H1(I)*STIF(I).GT.ZERO.AND.
     .       IRB(KINET(IX1(I))).EQ.0.AND.IRB2(KINET(IX1(I))).EQ.0)THEN
             DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H1(I)*STIF(I))))
           ENDIF
           MAS2  = DEUX * MS(IX2(I))
           IF(MAS2.GT.ZERO.AND.H2(I)*STIF(I).GT.ZERO.AND.
     .       IRB(KINET(IX2(I))).EQ.0.AND.IRB2(KINET(IX2(I))).EQ.0)THEN
             DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H2(I)*STIF(I))))
           ENDIF
           MAS2  = DEUX* MS(IX3(I))
           IF(MAS2.GT.ZERO.AND.H3(I)*STIF(I).GT.ZERO.AND.
     .       IRB(KINET(IX3(I))).EQ.0.AND.IRB2(KINET(IX3(I))).EQ.0)THEN
             DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H3(I)*STIF(I))))
           ENDIF
           MAS2  = DEUX * MS(IX4(I))
           IF(MAS2.GT.ZERO.AND.H4(I)*STIF(I).GT.ZERO.AND.
     .       IRB(KINET(IX4(I))).EQ.0.AND.IRB2(KINET(IX4(I))).EQ.0)THEN
             DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(H4(I)*STIF(I))))
           ENDIF
           DTMI0 = MIN(DTMI0,DTMI(I))
          ENDDO
         ELSE
          DO I=1,JLT
           DTMI(I) = EP20
           MAS2  = DEUX * MSI(I)
           IF(MAS2.GT.ZERO.AND.STV(I).GT.ZERO.AND.
     .       IRB(KINI(I)).EQ.0.AND.IRB2(KINI(I)).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/STV(I)))
           ENDIF
           MAS2  = DEUX * MS(IX1(I))
           IF(MAS2.GT.ZERO.AND.ST1(I).GT.ZERO.AND.
     .       IRB(KINET(IX1(I))).EQ.0.AND.IRB2(KINET(IX1(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST1(I))))
           ENDIF
           MAS2  = DEUX * MS(IX2(I))
           IF(MAS2.GT.ZERO.AND.ST2(I).GT.ZERO.AND.
     .       IRB(KINET(IX2(I))).EQ.0.AND.IRB2(KINET(IX2(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST2(I))))
           ENDIF
           MAS2  = DEUX * MS(IX3(I))
           IF(MAS2.GT.ZERO.AND.ST3(I).GT.ZERO.AND.
     .       IRB(KINET(IX3(I))).EQ.0.AND.IRB2(KINET(IX3(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST3(I))))
           ENDIF
           MAS2  = DEUX * MS(IX4(I))
           IF(MAS2.GT.ZERO.AND.ST4(I).GT.ZERO.AND.
     .       IRB(KINET(IX4(I))).EQ.0.AND.IRB2(KINET(IX4(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),DTFAC1(10)*SQRT(MAS2/(ST4(I))))
           ENDIF
           DTMI0 = MIN(DTMI0,DTMI(I))
          ENDDO
         ENDIF
         IF(DTMI0.LE.DTM)THEN
          DO I=1,JLT
           IF(DTMI(I).LE.DTM)THEN
             JG = NSVG(I)
             IF(JG.GT.0)THEN
               NI = ITAB(JG)
             ELSE
               NI = ITAFI(NIN)%P(-JG)
             ENDIF
             IF(IDTM.EQ.1)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .         ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .         ' IN INTERFACE ',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                    
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .           ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
               TSTOP = TT
               IF ( ISTAMPING == 1) THEN
                 WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(ISTDO,'(A)')'You may need to check if there is enou 
     .gh clearance between the tools,'
                 WRITE(ISTDO,'(A)')'and that they do not penetrate each ot
     .her during their travel'
                 WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(IOUT, '(A)')'You may need to check if there is enou
     .gh clearance between the tools,'
                 WRITE(IOUT, '(A)')'and that they do not penetrate each ot
     .her during their travel'
               ENDIF
             ELSEIF(IDTM.EQ.2)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .         ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .         ' IN INTERFACE ',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,'(A,I10,A,I10)')'   DELETE SECONDARY NODE ',
     .           NI,' FROM INTERFACE ',NOINT                 
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .           ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
               IF(JG.GT.0) THEN
                 STFN(CN_LOC(I)) = -ABS(STFN(CN_LOC(I)))
               ELSE
                 STIFI(NIN)%P(-JG) = -ABS(STIFI(NIN)%P(-JG))
               ENDIF
               IF ( ISTAMPING == 1) THEN
                 WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(ISTDO,'(A)')'You may need to check if there is enou 
     .gh clearance between the tools,'
                 WRITE(ISTDO,'(A)')'and that they do not penetrate each ot
     .her during their travel'
                 WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(IOUT, '(A)')'You may need to check if there is enou
     .gh clearance between the tools,'
                 WRITE(IOUT, '(A)')'and that they do not penetrate each ot
     .her during their travel'
               ENDIF
#include "lockoff.inc"
               NEWFRONT = -1
             ELSEIF(IDTM.EQ.5)THEN
#include "lockon.inc"
               WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .         ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .         ' IN INTERFACE ',NOINT,'(DTMIN=',DTM,')'
               WRITE(IOUT,'(A,I10)') '   SECONDARY NODE   : ',NI                     
               WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .           ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
               MSTOP = 2
               IF ( ISTAMPING == 1) THEN
                 WRITE(ISTDO,'(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(ISTDO,'(A)')'You may need to check if there is enou 
     .gh clearance between the tools,'
                 WRITE(ISTDO,'(A)')'and that they do not penetrate each ot
     .her during their travel'
                 WRITE(IOUT, '(A)')'The run encountered a problem in an in
     .terface Type 7.'
                 WRITE(IOUT, '(A)')'You may need to check if there is enou
     .gh clearance between the tools,'
                 WRITE(IOUT, '(A)')'and that they do not penetrate each ot
     .her during their travel'
               ENDIF
             ELSEIF(IDTM.EQ.6.AND.ILAGM.EQ.2)THEN
               IF(KINET(JG)+KINET(IX1(I))+KINET(IX2(I))
     .           +KINET(IX3(I))+KINET(IX4(I)).EQ.0 )THEN
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

      ELSE ! IF(ITIED == 0)THEN

        IF (DTMINI.GT.ZERO) THEN 
          DTM=DTMINI
        ELSE 
          DTM=EM30
        END IF

        DTMI0 = EP20
        IF(KDTINT.EQ.0)THEN
         DO I=1,JLT-JLT_TIED
          DTMI(I) = EP20
          MAS2  = DEUX * MSI(I)
          IF(MAS2.GT.ZERO.AND.STIF(I).GT.ZERO.AND.
     .      IRB(KINI(I)).EQ.0.AND.IRB2(KINI(I)).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),SQRT(MAS2/STIF(I)))
          ENDIF
          MAS2  = DEUX* MS(IX1(I))
          IF(MAS2.GT.ZERO.AND.H1(I)*STIF(I).GT.ZERO.AND.
     .      IRB(KINET(IX1(I))).EQ.0.AND.IRB2(KINET(IX1(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(H1(I)*STIF(I))))
          ENDIF
          MAS2  = DEUX * MS(IX2(I))
          IF(MAS2.GT.ZERO.AND.H2(I)*STIF(I).GT.ZERO.AND.
     .      IRB(KINET(IX2(I))).EQ.0.AND.IRB2(KINET(IX2(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(H2(I)*STIF(I))))
          ENDIF
          MAS2  = DEUX* MS(IX3(I))
          IF(MAS2.GT.ZERO.AND.H3(I)*STIF(I).GT.ZERO.AND.
     .      IRB(KINET(IX3(I))).EQ.0.AND.IRB2(KINET(IX3(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(H3(I)*STIF(I))))
          ENDIF
          MAS2  = DEUX * MS(IX4(I))
          IF(MAS2.GT.ZERO.AND.H4(I)*STIF(I).GT.ZERO.AND.
     .      IRB(KINET(IX4(I))).EQ.0.AND.IRB2(KINET(IX4(I))).EQ.0)THEN
            DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(H4(I)*STIF(I))))
          ENDIF
          DTMI0 = MIN(DTMI0,DTMI(I))
         ENDDO
        ELSE
         DO I=1,JLT-JLT_TIED
          DTMI(I) = EP20
          MAS2  = DEUX * MSI(I)
          IF(MAS2.GT.ZERO.AND.STV(I).GT.ZERO.AND.
     .      IRB(KINI(I)).EQ.0.AND.IRB2(KINI(I)).EQ.0)THEN
           DTMI(I) = MIN(DTMI(I),SQRT(MAS2/STV(I)))
          ENDIF
          MAS2  = DEUX * MS(IX1(I))
          IF(MAS2.GT.ZERO.AND.ST1(I).GT.ZERO.AND.
     .      IRB(KINET(IX1(I))).EQ.0.AND.IRB2(KINET(IX1(I))).EQ.0)THEN
           DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(ST1(I))))
          ENDIF
          MAS2  = DEUX * MS(IX2(I))
          IF(MAS2.GT.ZERO.AND.ST2(I).GT.ZERO.AND.
     .      IRB(KINET(IX2(I))).EQ.0.AND.IRB2(KINET(IX2(I))).EQ.0)THEN
           DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(ST2(I))))
          ENDIF
          MAS2  = DEUX * MS(IX3(I))
          IF(MAS2.GT.ZERO.AND.ST3(I).GT.ZERO.AND.
     .      IRB(KINET(IX3(I))).EQ.0.AND.IRB2(KINET(IX3(I))).EQ.0)THEN
           DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(ST3(I))))
          ENDIF
          MAS2  = DEUX * MS(IX4(I))
          IF(MAS2.GT.ZERO.AND.ST4(I).GT.ZERO.AND.
     .      IRB(KINET(IX4(I))).EQ.0.AND.IRB2(KINET(IX4(I))).EQ.0)THEN
           DTMI(I) = MIN(DTMI(I),SQRT(MAS2/(ST4(I))))
          ENDIF
          DTMI0 = MIN(DTMI0,DTMI(I))
         ENDDO
        ENDIF
        IF(DTMI0.LE.DTM)THEN
         DO I=1,JLT-JLT_TIED
          IF(DTMI(I).LE.DTM.AND.CAND_F(1,INDEX(I)).EQ.ZERO)THEN
            JG = NSVG(I)
            IF(JG.GT.0)THEN
              NI = ITAB(JG)
            ELSE
              NI = ITAFI(NIN)%P(-JG)
             ENDIF
#include "lockon.inc"
            WRITE(IOUT,'(A,E12.4,A,I10,A,E12.4,A)')
     .      ' **WARNING MINIMUM TIME STEP ',DTMI(I),
     .      ' IN INTERFACE ',NOINT,'(DTMIN=',DTM,')'
            WRITE(IOUT,'(A,I10,A,I10)')'   FREEZE SECONDARY NODE VS MAIN SEGMENT',
     .        NI,' IN INTERFACE ',NOINT               
            WRITE(IOUT,'(A,4I10)')'   MAIN NODES : ',
     .        ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I)) 
#include "lockoff.inc"
            CAND_F(1,INDEX(I))= FNS(I)*SIGN(UN,SIDE(I)) ! negative vs NX NY NZ
            CAND_F(2,INDEX(I))= FXT(I)*T1X(I)+FYT(I)*T1Y(I)+FZT(I)*T1Z(I)
            CAND_F(3,INDEX(I))= FXT(I)*T2X(I)+FYT(I)*T2Y(I)+FZT(I)*T2Z(I)
            CAND_F(4,INDEX(I))= H1(I)
            CAND_F(5,INDEX(I))= H2(I)
            CAND_F(6,INDEX(I))= H3(I)
            CAND_F(7,INDEX(I))= PENE(I)
            CAND_F(8,INDEX(I))= SIDE(I)
          ENDIF
         ENDDO
        ENDIF

      ENDIF ! IF(ITIED == 0)THEN, ELSE
C===================================================================
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
C
C spmd : identification des noeuds interf. utiles a envoyer
      IF (NSPMD.GT.1) THEN
Ctmp+1 mic only to avoid compiler bug
#include "mic_lockon.inc"
        DO I = 1,JLT
          NN = NSVG(I)
          IF(NN.LT.0)THEN
C tag temporaire de NSVFI a -
            NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
          ENDIF
        ENDDO
ctmp+1 mic only
#include "mic_lockoff.inc"
      ENDIF
C
C-----------------------------------------------------      
      IF((IBAG.NE.0).OR.(IADM.NE.0).OR.(IDAMP_RDOF.NE.0)) THEN
       DO I=1,JLT
C       IF(PENE(I).NE.ZERO)THEN
C test modifie pour coherence avec communication SPMD (spmd_i7tools)
        IF(FXI(I).NE.ZERO.OR.FYI(I).NE.ZERO.OR.FZI(I).NE.ZERO)THEN
          JG = NSVG(I)
          IF(JG.GT.0) THEN
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
      IF(IADM.NE.0)THEN
       DO I=1,JLT
         JG = NSVG(I)
#include "lockon.inc"
         IF(JG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
           RCONTACT(JG)=MIN(RCONTACT(JG),RCURVI(I))
         END IF
         RCONTACT(IX1(I))=MIN(RCONTACT(IX1(I)),RCURVI(I))
         RCONTACT(IX2(I))=MIN(RCONTACT(IX2(I)),RCURVI(I))
         RCONTACT(IX3(I))=MIN(RCONTACT(IX3(I)),RCURVI(I))
         RCONTACT(IX4(I))=MIN(RCONTACT(IX4(I)),RCURVI(I))
#include "lockoff.inc"
       END DO
      END IF
      IF(IADM.GE.2)THEN
       DO I=1,JLT
         JG = NSVG(I)
#include "lockon.inc"
         IF(JG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
           PCONTACT(JG)=MAX(PCONTACT(JG),PENE(I)/(PADM*GAPV(I)))
           ACONTACT(JG)=MIN(ACONTACT(JG),ANGLMI(I))
         END IF
#include "lockoff.inc"
       END DO
      END IF
C
      IF(IBCC.EQ.0) RETURN
C
      DO 400 I=1,JLT
        IF(PENE(I).EQ.ZERO)GOTO 400
        IBCM = IBCC / 8
        IBCS = IBCC - 8 * IBCM
        IF(IBCS.GT.0) THEN
          IG=NSVG(I)
          IF(IG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
            CALL IBCOFF(IBCS,ICODT(IG))
          ENDIF
        ENDIF
        IF(IBCM.GT.0) THEN
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

