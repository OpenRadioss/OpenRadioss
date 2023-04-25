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
Chd|  I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        I24ASS0                       source/interfaces/int24/i24for3.F
Chd|        I24ASS2                       source/interfaces/int24/i24for3.F
Chd|        I24FOR1_FIC                   source/interfaces/int24/i24for3e.F
Chd|        I24FOR1_FICR                  source/interfaces/int24/i24for3e.F
Chd|        I24SMS2                       source/interfaces/int24/i24for3.F
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24FOR3(JLT ,A  ,V       ,IBCC        ,ICODT     ,
     2    FSAV        ,GAP       ,FRIC    ,MS          ,VISC      ,
     3    VISCF       ,NOINT     ,STFN    ,ITAB        ,CN_LOC    ,
     4    STIGLO      ,STIFN     ,STIF    ,FSKYI       ,ISKY      ,
     5    N1          ,N2        ,N3      ,H1          ,H2        ,
     6    H3          ,H4        ,FCONT   ,PENE        ,
     7    IX1         ,IX2       ,IX3     ,IX4         ,NSVG      ,
     8    IVIS2       ,NELTST    ,ITYPTST ,DT2T        ,SUBTRIA   ,
     9    GAPV        ,INACTI    ,INDEX   ,NISKYFI     ,
     A    KINET       ,NEWFRONT  ,ISECIN  ,NSTRF       ,SECFCUM   ,
     B    X           ,IRECT     ,CE_LOC  ,MFROT       ,IFQ       ,
     C    FROT_P      ,SECND_FR   ,ALPHA0  ,
     D    IBAG        ,ICONTACT  ,IRTLM   ,
     E    VISCN       ,VXI       ,VYI     ,VZI         ,MSI       ,
     F    KINI        ,NIN       ,NISUB   ,LISUB       ,ADDSUBS   ,
     G    ADDSUBM     ,LISUBS    ,LISUBM  ,FSAVSUB     ,CAND_N    ,
     H    ILAGM       ,ICURV     ,FNCONT  ,FTCONT      ,NSN       ,
     I    X1          ,X2        ,X3      ,X4          ,Y1        ,
     J    Y2          ,Y3        ,Y4      ,Z1          ,Z2        ,
     K    Z3          ,Z4        ,XI      ,YI          ,ZI        ,
     L    IADM        ,RCURVI    ,RCONTACT ,ACONTACT   ,PCONTACT  ,
     M    ANGLMI      ,PADM      ,INTTH    ,PHI        , FTHE     ,
     N    FTHESKYI    ,TEMP      ,TEMPI   ,RSTIF       ,IFORM     ,
     O    MSKYI_SMS   ,ISKYI_SMS ,NSMS    ,CAND_N_N    ,PENE_OLD  ,
     P    STIF_OLD    ,MBINFLG   ,ILEV    ,IGSTI       ,KMIN      ,
     P    INTPLY      ,IPLY      ,INOD_PXFEM,NM1       ,NM2       ,
     R    NM3         ,NREBOU    ,IRTSE   ,NSNE        ,IS2SE     ,
     S    IS2PT       ,MSEGTYP   ,JTASK   ,ISENSINT  ,
     U    FSAVPARIT   ,NFT       ,H3D_DATA,FRICC       ,VISCFFRIC ,
     V    FRIC_COEFS  ,T2MAIN_SMS,INTNITSCHE,FORNEQSI,IORTHFRIC ,
     W    FRIC_COEFS2 ,FRICC2    ,VISCFFRIC2,NFORTH    ,NFISOT    ,
     X    INDEXORTH   , INDEXISOT,DIR1      ,DIR2      ,T2FAC_SMS ,
     Y    F_PFIT      ,TAGNCONT  ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,
     Z    TYPSUB      ,INFLG_SUBS,INFLG_SUBM ,NINLOADP  ,DGAPLOADINT,
     1    S_LOADPINTER,DIST      ,IXX        ,INTEREFRIC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PLYXFEM_MOD
      USE H3D_MOD
      USE ANIM_MOD
      USE OUTPUTS_MOD
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
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,IBCC,IBCM,IBCS,IVIS2,INACTI,IBAG,NIN,
     .        INTNITSCHE,IORTHFRIC,NFORTH ,NFISOT,
     .        NSN,ICODT(*), ITAB(*), ISKY(*), KINET(*),IRTLM(2,NSN),
     .        MFROT, IFQ, NOINT,NEWFRONT,ISECIN, NSTRF(*),
     .        IRECT(4,*),ICONTACT(*), CAND_N(*),
     .        KINI(*),SUBTRIA(MVSIZ),MBINFLG(*),ILEV,
     .        ISET, NISKYFI,IADM,INTTH,IFORM,CAND_N_N(*),INTPLY,
     .        MSEGTYP(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        TYPSUB(*),JTASK
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        CN_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ),
     .        NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*),
     .        LISUBM(*),ILAGM,ICURV(3), NREBOU,
     .        ISKYI_SMS(*), NSMS(*),IGSTI,IPLY(4,*),INOD_PXFEM(*),
     .        IRTSE(5,*),NSNE ,IS2SE(2,*),IS2PT(*), ISENSINT(*),NFT,T2MAIN_SMS(6,*),
     .        INDEXORTH(MVSIZ)  ,INDEXISOT(MVSIZ),INFLG_SUBS(*), INFLG_SUBM(*)
      INTEGER  , INTENT(IN) :: NINLOADP,S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .        LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER  , INTENT(IN) :: IXX(MVSIZ,13)
      INTEGER  , INTENT(IN) :: INTEREFRIC
      my_real
     .   STIGLO,FROT_P(*), X(3,*),
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   SECND_FR(6,*),ALPHA0,
     .   GAP, FRIC,VISC,VISCF,VIS,DT2T,STFN(*),STIFN(*),
     .   FSKYI(LSKYI,NFSKYI),FSAVSUB(NTHVKI,*), FNCONT(3,*),FTCONT(3,*),
     .   MSKYI_SMS(*),KMIN
      my_real
     .     LA(MVSIZ), LB(MVSIZ), LC(MVSIZ),STIF(MVSIZ),
     .     GAPV(MVSIZ), PENE(MVSIZ),
     .     SECFCUM(7,NUMNOD,NSECT),
     .     TMP(MVSIZ),
     .     STIFSAV(MVSIZ), VISCN(*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .     X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .     X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .     X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .     XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ),
     .     PHI(MVSIZ), FTHE(*),FTHESKYI(LSKYI),TEMP(*), TEMPI(MVSIZ),
     .     FSAVPARIT(NISUB+1,11,*),
     .     FRICC(MVSIZ),VISCFFRIC(MVSIZ),FRIC_COEFS(MVSIZ,10),FORNEQSI(MVSIZ,3),
     .     FRIC_COEFS2(MVSIZ,10),FRICC2(MVSIZ),VISCFFRIC2(MVSIZ),
     .     DIR1(MVSIZ,3),DIR2(MVSIZ,3),T2FAC_SMS(*)


      my_real
     .     RCURVI(*), RCONTACT(*), ACONTACT(*),
     .     PCONTACT(*),PADM, ANGLMI(*),RSTIF,
     .     PENE_OLD(5,NSN),STIF_OLD(2,NSN),F_PFIT
      my_real  , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      my_real  , INTENT(INOUT) :: DIST(MVSIZ)

      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2,N,IMS2,ITAG,NN1,NN2,NN3,
     .        NN4,ii,IFIT,PP,PPL,ITYPSUB,ISS1,ISS2,IMS1,IX
      my_real
     .   FXR(MVSIZ), FYR(MVSIZ), FZR(MVSIZ)
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ),EFRIC_L(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,
     .   V2, FM2, DT1INV, VISCA, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, ECONTDT,
     .   D1,D2,D3,D4,A1,A2,A3,A4,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8,
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15,
     .   FSAV22, FSAV23, FSAV24, FFO,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA,BBB,STFR,VISR ,TM,TS,
     .   VN1(3),VN2(3),VN3(3),VN4(4),
     .   CSA ,SNA ,ALPHAF ,FTT1 ,FTT2 ,
     .   AN(NFORTH) ,BN(NFORTH) ,NEP1 ,NEP2 ,NEP ,C11 ,C22 ,ANS ,EP ,
     .   SIGNC,DGAPLOAD,GAPP,EFRIC_LS,EFRIC_LM
      my_real
     .   PREC,FACV
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),DPENE(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX, FX6(6,MVSIZ), FY6(6,MVSIZ), FZ6(6,MVSIZ),
     .   PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ), PHI4(MVSIZ),
     .   DTMINI,STIF0_IMP(MVSIZ),PENN,TINY,
     .   FNNIT1,FNNIT2,FNNIT3,FNNITSCHE,T3,FNITXT(MVSIZ),FNITYT(MVSIZ),FNITZT(MVSIZ),
     .   XMU2(MVSIZ)
      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB,ibug,ip,NCFIT
      INTEGER  ISEGTYP(MVSIZ),IXSS(4),NOD1,NOD2,NS
      my_real
     .   FSAVSUB1(24,NISUB),IMPX,IMPY,IMPZ,VNM(MVSIZ),SFAC,STMIN,
     .   FXS(4),FYS(4),FZS(4),HH,FINC,FA,FB,DDP,PREC1

      DOUBLE PRECISION RP1,RP2,STIF_N
      INTEGER TAGIP(MVSIZ),ISPDO
C
      INTEGER BITGET
      EXTERNAL BITGET
C-----------------------------------------------
      DTMINI=ZERO
      IF (IRESP==1) THEN
        PREC = FIVEEM4
        TINY = EM15
        PREC1= EM05
      ELSE
        PREC = EM10
        TINY = EM30
        PREC1= ZERO
      ENDIF
      IF(DT1>ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO
      ECONTDT = ZERO
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO
      EFRIC_L(1:JLT) = ZERO
C---------------------
C     COURBURE FIXE
C---------------------
      IF(ICURV(1)==1)THEN
      ELSEIF(ICURV(1)==2)THEN
      ELSEIF(ICURV(1) == 3)THEN
      ENDIF
      NCFIT = ICURV(2)
      IF (INACTI==-1.AND.IMPL_S==0.AND.NCFIT>0) THEN
        IFIT = 1
      ELSE
        IFIT = 0
      END IF
C-------------------------------------------
C     PENE Offset removed to i24dst3.F
C-------------------------------------------
C
      IF(INTPLY == 0) THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)THEN
            H1(I) = ZERO
            H2(I) = ZERO
            H3(I) = ZERO
            H4(I) = ZERO
            FNI(I)= ZERO
C
            FXI(I)=ZERO
            FYI(I)=ZERO
            FZI(I)=ZERO
C
            FX1(I)=ZERO
            FY1(I)=ZERO
            FZ1(I)=ZERO
C
            FX2(I)=ZERO
            FY2(I)=ZERO
            FZ2(I)=ZERO
C
            FX3(I)=ZERO
            FY3(I)=ZERO
            FZ3(I)=ZERO
C
            FX4(I)=ZERO
            FY4(I)=ZERO
            FZ4(I)=ZERO
          ELSE
            VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                     - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
            VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                     - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
            VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                     - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
            VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            VNM(I) = NM1(I)*VX(I) + NM2(I)*VY(I) + NM3(I)*VZ(I)
          ENDIF
        ENDDO
      ELSE
        DO I=1,JLT
          IF(PENE(I) == ZERO)THEN
            H1(I) = ZERO
            H2(I) = ZERO
            H3(I) = ZERO
            H4(I) = ZERO
            FNI(I)= ZERO
C
            FXI(I)=ZERO
            FYI(I)=ZERO
            FZI(I)=ZERO
C
            FX1(I)=ZERO
            FY1(I)=ZERO
            FZ1(I)=ZERO
C
            FX2(I)=ZERO
            FY2(I)=ZERO
            FZ2(I)=ZERO
C
            FX3(I)=ZERO
            FY3(I)=ZERO
            FZ3(I)=ZERO
C
            FX4(I)=ZERO
            FY4(I)=ZERO
            FZ4(I)=ZERO
          ELSE
            JJ = IPLY(1,I)
            NN1 =INOD_PXFEM(IX1(I))
            NN2 =INOD_PXFEM(IX2(I))
            NN3 =INOD_PXFEM(IX3(I))
            NN4 =INOD_PXFEM(IX4(I))
C
            VN1(1) = V(1,IX1(I))
            VN1(2) = V(2,IX1(I))
            VN1(3) = V(3,IX1(I))
C
            VN2(1) = V(1,IX2(I))
            VN2(2) = V(2,IX2(I))
            VN2(3) = V(3,IX2(I))
C
            VN3(1) = V(1,IX3(I))
            VN3(2) = V(2,IX3(I))
            VN3(3) = V(3,IX3(I))
C
            VN4(1) = V(1,IX4(I))
            VN4(2) = V(2,IX4(I))
            VN4(3) = V(3,IX4(I))
C
            IF(IPLYXFEM /= 2)THEN
C
C           possible energy creation after full delamination
C           - seen with IPLYXFEM=1 -
              JJ = IPLY(1,I)
              IF(NN1 > 0 .AND. JJ > 0) THEN
                VN1(1) = VN1(1) + PLY(JJ)%V(1,NN1)
                VN1(2) = VN1(2) + PLY(JJ)%V(2,NN1)
                VN1(3) = VN1(3) + PLY(JJ)%V(3,NN1)
              ENDIF
              JJ = IPLY(2,I)
              IF(NN2 > 0 .AND. JJ > 0) THEN
                VN2(1) = VN2(1) + PLY(JJ)%V(1,NN2)
                VN2(2) = VN2(2) + PLY(JJ)%V(2,NN2)
                VN2(3) = VN2(3) + PLY(JJ)%V(3,NN2)
              ENDIF
              JJ = IPLY(3,I)
              IF(NN3 > 0 .AND. JJ > 0) THEN
                VN3(1) = VN3(1) + PLY(JJ)%V(1,NN3)
                VN3(2) = VN3(2) + PLY(JJ)%V(2,NN3)
                VN3(3) = VN3(3) + PLY(JJ)%V(3,NN3)
              ENDIF
              JJ = IPLY(4,I)
              IF(NN4 > 0 .AND. JJ > 0) THEN
                VN4(1) = VN4(1) + PLY(JJ)%V(1,NN4)
                VN4(2) = VN4(2) + PLY(JJ)%V(2,NN4)
                VN4(3) = VN4(3) + PLY(JJ)%V(3,NN4)
              ENDIF
            END IF

            VX(I) = VXI(I) - H1(I)*VN1(1) - H2(I)*VN2(1)
     .                     - H3(I)*VN3(1) - H4(I)*VN4(1)
            VY(I) = VYI(I) - H1(I)*VN1(2) - H2(I)*VN2(2)
     .                     - H3(I)*VN3(2) - H4(I)*VN4(2)
            VZ(I) = VZI(I) - H1(I)*VN1(3) - H2(I)*VN2(3)
     .                     - H3(I)*VN3(3) - H4(I)*VN4(3)
            VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            VNM(I) = NM1(I)*VX(I) + NM2(I)*VY(I) + NM3(I)*VZ(I)
          ENDIF
        ENDDO

      ENDIF
C-------------------------------------------
C     stifness reduction to avoid positive
C     force jump and energy generation
C-------------------------------------------
      IF (IFIT >0) THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          STIF(I) = STIF0(I)
c          STIF(I) = FINC*STIF0(I)
          IF(STIGLO<=ZERO) STIF(I) = HALF*STIF(I)
          FNI(I)= -STIF(I) * PENE(I)
        ENDDO
      ELSEIF(IMPL_S>0.AND.IGSTI==6)THEN
C-------0.001*STIF initially (first contact), special case(rebound) 0.0001*STIF but >=KMIN------
C---   NREBOU<0 rebound in this cycle, : -1 -> at first contact; -2 -> after first contact
        IF (NREBOU == -1) THEN
          SFAC = EM04
        ELSE
          SFAC = EM03
        END IF
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          JG = NSVG(I)
          N  = CAND_N_N(I)
          IF(JG > 0)THEN
            IF (STIF_OLD(1,N)==ZERO.OR.NREBOU==-1) THEN
              STIF_OLD(1,N) = SFAC*STIF0(I)
            ELSEIF (NREBOU <-1) THEN
              STMIN = EM04*STIF0(I)
              STIF_OLD(1,N) = MAX(STMIN,EM01*STIF_OLD(1,N))
            END IF
            STIF_OLD(1,N) = MAX(KMIN,STIF_OLD(1,N))
            STIF(I) = STIF_OLD(1,N)
          ELSE
            JG = -JG
            IF (STIF_OLDFI(NIN)%P(1,JG)==ZERO.OR.NREBOU==-1) THEN
              STIF_OLDFI(NIN)%P(1,JG)= SFAC*STIF0(I)
            ELSEIF (NREBOU <-1) THEN
              STMIN = EM04*STIF0(I)
              STIF_OLDFI(NIN)%P(1,JG)=
     +            MAX(STMIN,EM01*STIF_OLDFI(NIN)%P(1,JG))
            END IF
            STIF_OLDFI(NIN)%P(1,JG)= MAX(KMIN,STIF_OLDFI(NIN)%P(1,JG))
            STIF(I) = STIF_OLDFI(NIN)%P(1,JG)
          END IF
        END DO
        IF(INCONV >=0)THEN
C---------if multi-contact in the same groupe---
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            JG = NSVG(I)
            N  = CAND_N_N(I)
            IF(JG > 0)THEN
              STIF0_IMP(I) = STIF_OLD(1,N)
            ELSE
              STIF0_IMP(I) = STIF_OLDFI(NIN)%P(1,-JG)
            END IF
          END DO
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            JG = NSVG(I)
            N  = CAND_N_N(I)
            IF(JG > 0)THEN
C---------if multi-contact in the same groupe---
              IF(PENE_OLD(2,N)/= ZERO.OR.PENE_OLD(5,N)/= ZERO)THEN
                IF(PENE(I) > PENE_OLD(2,N) )THEN
                  FACM1 = PENE(I)/MAX(EM20,PENE_OLD(2,N))
                  IF (STIF0_IMP(I)/STIF0(I) <EM02) THEN
                    FACM1 = ONEP2*FACM1
                    PENN = THREE_HALF
                  ELSE
                    PENN = ONEP05
                  END IF
                  FACM1=MIN(PENN,FACM1)
                  STIF(I) = FACM1*STIF0_IMP(I)
C-----------never higher than STIF0----------------
                  STIF(I) = MIN(STIF(I),STIF0(I))
                ENDIF
C------------quand  PENE_OLD(2,N)=PENE_OLD(1,N)
                IF(INCONV ==1 )THEN
                  PENE_OLD(1,N) = PENE(I)
                  STIF_OLD(1,N) = STIF(I)
                ENDIF
              ELSEIF(INCONV ==1 )THEN
c new impact, possible multi impact
#include "lockon.inc"
                PENE_OLD(1,N) = MAX(PENE_OLD(1,N),PENE(I))
                STIF_OLD(1,N) = MIN(STIF_OLD(1,N),STIF(I))
#include "lockoff.inc"
              ENDIF
            ELSE
              JG = -JG
              IF(PENE_OLDFI(NIN)%P(2,JG)/= ZERO.OR.
     .                           PENE_OLDFI(NIN)%P(5,JG)/= ZERO)THEN
                IF(PENE(I) > PENE_OLDFI(NIN)%P(2,JG) )THEN
                  FACM1 = PENE(I)/MAX(EM20,PENE_OLDFI(NIN)%P(2,JG))
                  IF (STIF0_IMP(I)/STIF0(I) <EM02) THEN
                    FACM1 = ONEP2*FACM1
                    PENN = THREE_HALF
                  ELSE
                    PENN = ONEP05
                  END IF
                  FACM1=MIN(PENN,FACM1)
                  STIF(I) = FACM1*STIF0_IMP(I)
                  STIF(I) = MIN(STIF(I),STIF0(I))
                ENDIF
                IF(INCONV ==1 )THEN
                  PENE_OLDFI(NIN)%P(1,JG) = PENE(I)
                  STIF_OLDFI(NIN)%P(1,JG) = STIF(I)
                END IF !(INCONV ==1 )THEN
              ELSEIF(INCONV ==1 )THEN
#include "lockon.inc"
                PENE_OLDFI(NIN)%P(1,JG) =
     *                                MAX(PENE_OLDFI(NIN)%P(1,JG),PENE(I))
                STIF_OLDFI(NIN)%P(1,JG) =
     *                                MIN(STIF_OLDFI(NIN)%P(1,JG),STIF(I))
#include "lockoff.inc"
              ENDIF
            ENDIF
          ENDDO
C-------to pass for implicit
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            JG = NSVG(I)
            N  = CAND_N_N(I)
            IF(JG > 0)THEN
              STIF_OLD(2,N) = STIF(I)
            ELSE
              STIF_OLDFI(NIN)%P(2,-JG) = STIF(I)
            END IF
          END DO
        END IF !(INCONV>=0)THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          IF(STIGLO<=ZERO)THEN
            STIF(I) = HALF*STIF(I)
ccc              ECONTT = ECONTT + HALF*STIF(I)*PENE(I)**2
          ELSEIF(STIF(I)/=ZERO)THEN
            STIF(I) = STIGLO
ccc              ECONTT = ECONTT + HALF*STIGLO*PENE(I)**2
          ENDIF
          FNI(I)= -STIF(I) * PENE(I)
        ENDDO

C------explicit or implicit+Istif/=6
      ELSE
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          DPENE(I) = MAX(ZERO,-VNM(I)*DT1)
          JG = NSVG(I)
          N  = CAND_N_N(I)
          ISPDO = 1
          IF(JG > 0)THEN
            IF(TT /= ZERO.AND.(PENE_OLD(2,N)/= ZERO.OR.PENE_OLD(5,N)/= ZERO))THEN
              DDP = PENE(I)-PENE_OLD(2,N)-DPENE(I)
              IF(PENE(I) > PENE_OLD(2,N) .AND.DDP>ZERO)THEN
                RP1 = PENE_OLD(2,N)/PENE(I)
                RP2 = DPENE(I)/PENE(I)
                IF (IRESP==1.AND. RP2<EM01) ISPDO = 0
                IF (ISPDO==1) STIF(I) = STIF_OLD(2,N)*RP1+ STIF(I)*RP2
              ELSEIF(.NOT.(PENE_OLD(2,N)== ZERO.AND.STIF_OLD(2,N)==ZERO))THEN
C-----------first contact STIF_OLD(2,N) should not be zero
                STIF(I) = STIF_OLD(2,N)
              END IF
              IF(INCONV ==1 )THEN
                PENE_OLD(1,N) = PENE(I)
                STIF_OLD(1,N) = STIF(I)
              ENDIF
            ELSEIF(INCONV ==1 )THEN
c new impact, possible multi impact
#include "lockon.inc"
              PENE_OLD(1,N) = MAX(PENE_OLD(1,N),PENE(I))
              STIF_OLD(1,N) = MAX(STIF_OLD(1,N),STIF(I))
#include "lockoff.inc"
            ENDIF
          ELSE
            JG = -JG
            IF(TT /= ZERO.AND.(PENE_OLDFI(NIN)%P(2,JG)/= ZERO.OR.
     .                         PENE_OLDFI(NIN)%P(5,JG)/= ZERO))THEN
              DDP = PENE(I)-PENE_OLDFI(NIN)%P(2,JG)-DPENE(I)
              IF(PENE(I) > PENE_OLDFI(NIN)%P(2,JG) .AND.DDP>ZERO)THEN
                RP1 = PENE_OLDFI(NIN)%P(2,JG)/PENE(I)
                RP2 = DPENE(I)/PENE(I)
                IF (IRESP==1.AND. RP2<EM01) ISPDO = 0
                IF (ISPDO==1)STIF(I) = STIF_OLDFI(NIN)%P(2,JG)*RP1+ STIF(I)*RP2
              ELSEIF(.NOT.(PENE_OLDFI(NIN)%P(2,JG)== ZERO.AND.
     .                            STIF_OLDFI(NIN)%P(2,JG)==ZERO))THEN
                STIF(I) = STIF_OLDFI(NIN)%P(2,JG)
              END IF
              IF(INCONV ==1 )THEN
                PENE_OLDFI(NIN)%P(1,JG) = PENE(I)
                STIF_OLDFI(NIN)%P(1,JG) = STIF(I)
              ENDIF
            ELSEIF(INCONV ==1 )THEN
#include "lockon.inc"
              PENE_OLDFI(NIN)%P(1,JG) =
     *                              MAX(PENE_OLDFI(NIN)%P(1,JG),PENE(I))
              STIF_OLDFI(NIN)%P(1,JG) =
     *                              MAX(STIF_OLDFI(NIN)%P(1,JG),STIF(I))
#include "lockoff.inc"
            ENDIF
          ENDIF
        ENDDO
C-------------------------------------------
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          IF(STIGLO<=ZERO)THEN
            STIF(I) = HALF*STIF(I)
          ELSEIF(STIF(I)/=ZERO)THEN
            STIF(I) = STIGLO
          ENDIF
          FNI(I)= -STIF(I) * PENE(I)

        ENDDO
C-------------------------------------------
      ENDIF !IF(IMPL_S>0.AND.IGSTI==6)
C-------------------------------------------
C     Contact energy (elastic)
C-------------------------------------------
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
        ECONTT = ECONTT+HALF*STIF(I)*PENE(I)**2
      ENDDO
C
      IF(INTNITSCHE > 0 ) THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          FNNIT1 = FORNEQSI(I,1)*N1(I)
          FNNIT2 = FORNEQSI(I,2)*N2(I)
          FNNIT3 = FORNEQSI(I,3)*N3(I)
          FNNITSCHE = FNNIT1 + FNNIT2 + FNNIT3
C
          FNI(I) = MIN(ZERO,FNI(I) - HALF*FNNITSCHE)
C
        ENDDO
      ENDIF
C---------------------------------
C     DAMPING + FRIC
C---------------------------------
C     goto 999
      IF(VISC/=ZERO)THEN
        IF(IVIS2==0)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT
            VIS2(I) = TWO * STIF(I) * MSI(I)
          ENDDO
C---------------------------------
C---------------------------------
          IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
            DO I=1,JLT
              IF(PENE(I) == ZERO)CYCLE
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS = SQRT(VIS2(I))
              FF  = FAC * VISC * VIS
              STIF(I) = STIF0(I) + FF * DT1INV
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
              FFO = FF
              FF = FF * VN(I)
C           ECONVT = ECONVT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
            ENDDO
          ELSE
            DO I=1,JLT
              IF(PENE(I) == ZERO)CYCLE
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS = SQRT(VIS2(I))
              C(I)= FAC * VISC * VIS
              KT(I)= STIF0(I)
              STIF(I) = STIF0(I) + C(I) * DT1INV
              FF = C(I) * VN(I)
C           ECONVT = ECONVT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
              CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
              STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
            ENDDO
          ENDIF
        ELSEIF(IVIS2==1.OR.IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          IF(IVIS2==1)THEN
            FACV = TWO
          ELSE
            FACV = FOUR
          END IF
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            MAS2  = MS(IX1(I))*H1(I)
     .            + MS(IX2(I))*H2(I)
     .            + MS(IX3(I))*H3(I)
     .            + MS(IX4(I))*H4(I)
            VIS2(I) = FACV* STIF(I) * MSI(I) * MAS2 /
     .           MAX(EM30,MSI(I)+MAS2)
          ENDDO
C---------------------------------
C---------------------------------
          IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
            DO I=1,JLT
              IF(PENE(I) == ZERO)CYCLE
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS = SQRT(VIS2(I))
              FF  = FAC * VISC * VIS
              STIF(I) = STIF0(I) + FF * DT1INV
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
              FFO = FF
              FF = FF * VN(I)
C           ECONTDT = ECONTDT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
            ENDDO
          ELSE
            DO I=1,JLT
              IF(PENE(I) == ZERO)CYCLE
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS = SQRT(VIS2(I))
              C(I)= FAC * VISC * VIS
              KT(I)= STIF0(I)
              STIF(I) = STIF(I) + C(I) * DT1INV
              FF = C(I) * VN(I)
C           ECONTDT = ECONTDT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
              CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
              STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
            ENDDO
          ENDIF
        ELSEIF(IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE
C---------------------------------
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            VIS2(I) = TWO* STIF(I) * MSI(I)
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            FF  = FAC * VISC * VIS
            STIF(I) = STIF0(I) + TWO * FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF = FF * VN(I)
C          ECONTDT = ECONTDT + FF * VN(I) * DT1
            FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            VIS2(I) = TWO * STIF(I) * MSI(I)
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            FF  = FAC *  VISC * VIS
            STIF(I) = STIF0(I) + TWO* FF * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF = FF * VN(I)
C          ECONTDT = ECONTDT+ FF * VN(I) * DT1
            FNI(I)  = FNI(I) + FF
          ENDDO
        ELSEIF(IVIS2==4)THEN
C---------------------------------
C         VISC = 0
C---------------------------------
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS2(I) = TWO* STIF(I) * MSI(I)
            VIS = SQRT(VIS2(I))
            STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
          ENDDO
        ELSEIF(IVIS2==5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            MAS2  = MS(IX1(I))*H1(I)
     .            + MS(IX2(I))*H2(I)
     .            + MS(IX3(I))*H3(I)
     .            + MS(IX4(I))*H4(I)
            VIS2(I) = TWO* STIF(I) * MSI(I)
            VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 /
     .           MAX(EM30,MSI(I)+MAS2)
            STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
            FF = VIS * VN(I)
            ECONTDT = ECONTDT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
            FNI(I)  = MIN(FNI(I),FF)
          ENDDO
        ELSE
        ENDIF
      ELSE
        DO I=1,JLT
          IF(VISCFFRIC(I)/=ZERO)THEN

            IF(IVIS2==0)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
              VIS2(I) = TWO * STIF(I) * MSI(I)
C---------------------------------
C---------------------------------
              IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
                IF(PENE(I) == ZERO)CYCLE
                FAC = STIF(I) / MAX(EM30,STIF(I))
                VIS = SQRT(VIS2(I))
                FF  = FAC * VISC * VIS
                STIF(I) = STIF0(I) + FF * DT1INV
                STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
                FFO = FF
                FF = FF * VN(I)
C                ECONTDT = ECONTDT + FF * VN(I) * DT1
                FNI(I)  = FNI(I) + FF
              ELSE
                IF(PENE(I) == ZERO)CYCLE
                FAC = STIF(I) / MAX(EM30,STIF(I))
                VIS = SQRT(VIS2(I))
                C(I)= FAC * VISC * VIS
                KT(I)= STIF0(I)
                STIF(I) = STIF0(I) + C(I) * DT1INV
                FF = C(I) * VN(I)
C               ECONTDT = ECONTDT + FF * VN(I) * DT1
                FNI(I)  = FNI(I) + FF
                CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
                STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
              ENDIF
            ELSEIF(IVIS2==1.OR.IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
              IF(IVIS2==1)THEN
                FACV = TWO
              ELSE
                FACV = FOUR
              END IF
              IF(PENE(I) == ZERO)CYCLE
              MAS2  = MS(IX1(I))*H1(I)
     .              + MS(IX2(I))*H2(I)
     .              + MS(IX3(I))*H3(I)
     .              + MS(IX4(I))*H4(I)
              VIS2(I) = FACV* STIF(I) * MSI(I) * MAS2 /
     .           MAX(EM30,MSI(I)+MAS2)
C---------------------------------
C---------------------------------
              IF(KDTINT==0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
                IF(PENE(I) == ZERO)CYCLE
                FAC = STIF(I) / MAX(EM30,STIF(I))
                VIS = SQRT(VIS2(I))
                FF  = FAC * VISC * VIS
                STIF(I) = STIF0(I) + FF * DT1INV
                STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
                FFO = FF
                FF = FF * VN(I)
C              ECONTDT = ECONTDT + FF * VN(I) * DT1
                FNI(I)  = FNI(I) + FF
              ELSE
                IF(PENE(I) == ZERO)CYCLE
                FAC = STIF(I) / MAX(EM30,STIF(I))
                VIS = SQRT(VIS2(I))
                C(I)= FAC * VISC * VIS
                KT(I)= STIF0(I)
                STIF(I) = STIF(I) + C(I) * DT1INV
                FF = C(I) * VN(I)
C              ECONTDT = ECONTDT + FF * VN(I) * DT1
                FNI(I)  = FNI(I) + FF
                CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
                STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
              ENDIF
            ELSEIF(IVIS2==2)THEN
C---------------------------------
C         VISC QUAD TYPE
C---------------------------------
              IF(PENE(I) == ZERO)CYCLE
              VIS2(I) = TWO* STIF(I) * MSI(I)
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS = SQRT(VIS2(I))
              FF  = FAC * VISC * VIS
              STIF(I) = STIF0(I) + TWO * FF * DT1INV
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
              FF = FF * VN(I)
C           ECONTDT = ECONTDT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
            ELSEIF(IVIS2==3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
              IF(PENE(I) == ZERO)CYCLE
              VIS2(I) = TWO * STIF(I) * MSI(I)
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS = SQRT(VIS2(I))
              FF  = FAC *  VISC * VIS
              STIF(I) = STIF0(I) + TWO* FF * DT1INV
              STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
              FF = FF * VN(I)
C           ECONTDT = ECONTDT + FF * VN(I) * DT1
              FNI(I)  = FNI(I) + FF
            ELSEIF(IVIS2==4)THEN
C---------------------------------
C         VISC = 0
C---------------------------------
              IF(PENE(I) == ZERO)CYCLE
              FAC = STIF(I) / MAX(EM30,STIF(I))
              VIS2(I) = TWO* STIF(I) * MSI(I)
              VIS = SQRT(VIS2(I))
              STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            ELSEIF(IVIS2==5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
              IF(PENE(I) == ZERO)CYCLE
              MAS2  = MS(IX1(I))*H1(I)
     .             + MS(IX2(I))*H2(I)
     .             + MS(IX3(I))*H3(I)
     .             + MS(IX4(I))*H4(I)
              VIS2(I) = TWO* STIF(I) * MSI(I)
              VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 /
     .            MAX(EM30,MSI(I)+MAS2)
              STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
              FF = VIS * VN(I)
              ECONTDT = ECONTDT + MIN(ZERO,FF-FNI(I)) * VN(I) * DT1
              FNI(I)  = MIN(FNI(I),FF)
            ELSE
            ENDIF


          ELSE
cbb  initialisation du tableau VIS2 pour eviter des problemes
            VIS2(I) = ZERO
          ENDIF
        ENDDO
      ENDIF

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
      IF (ILEV==2) THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          IE=CE_LOC(I)
          IMS2 = BITGET(MBINFLG(IE),1)
          FXI(I)=N1(I)*FNI(I)
          FYI(I)=N2(I)*FNI(I)
          FZI(I)=N3(I)*FNI(I)
          IMPX=FXI(I)*DT12
          IMPY=FYI(I)*DT12
          IMPZ=FZI(I)*DT12
          FSAV8 =FSAV8 +ABS(IMPX)
          FSAV9 =FSAV9 +ABS(IMPY)
          FSAV10=FSAV10+ABS(IMPZ)
          IF (IMS2 >0 ) THEN
            FSAV1 =FSAV1 -IMPX
            FSAV2 =FSAV2 -IMPY
            FSAV3 =FSAV3 -IMPZ
            FSAV11=FSAV11-FNI(I)*DT12
          ELSE
            FSAV1 =FSAV1 +IMPX
            FSAV2 =FSAV2 +IMPY
            FSAV3 =FSAV3 +IMPZ
            FSAV11=FSAV11+FNI(I)*DT12
          END IF
        ENDDO
      ELSE
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
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
      END IF !(ILEV==2) THEN
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
          IF(PENE(I) == ZERO)CYCLE
          FSAVPARIT(1,1,I+NFT) =  FXI(I)
          FSAVPARIT(1,2,I+NFT) =  FYI(I)
          FSAVPARIT(1,3,I+NFT) =  FZI(I)
        ENDDO
      ENDIF
C---------------------------------
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          ((TT>=TANIM.AND.TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
        IF (INCONV==1) THEN
#include "lockon.inc"
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
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
            IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              IF (JG >NUMNOD) THEN
                IG = JG - NUMNOD
                CALL I24FOR1_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                         IG      ,FXI(I)  ,FYI(I)  ,FZI(I)  ,FNCONT  ,
     +                         -1      )
              ELSE
                FNCONT(1,JG)=FNCONT(1,JG)- FXI(I)
                FNCONT(2,JG)=FNCONT(2,JG)- FYI(I)
                FNCONT(3,JG)=FNCONT(3,JG)- FZI(I)
              END IF
            ELSE ! cas noeud remote en SPMD
              JG = -JG
              IF(ISEDGE_FI(NIN)%P(JG)==1)THEN
                CALL I24FOR1_FICR(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                      JG      ,FXI(I)  ,FYI(I)  ,FZI(I)  ,FNCONTI(NIN)%P(1,1)  ,
     +                      -1       ,NIN )
              ELSE
                FNCONTI(NIN)%P(1,JG)=FNCONTI(NIN)%P(1,JG)-FXI(I)
                FNCONTI(NIN)%P(2,JG)=FNCONTI(NIN)%P(2,JG)-FYI(I)
                FNCONTI(NIN)%P(3,JG)=FNCONTI(NIN)%P(3,JG)-FZI(I)
              ENDIF
            ENDIF
          ENDDO
#include "lockoff.inc"
        END IF !(INCONV==1) THEN
      ENDIF
C
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB/=0)THEN
        DO JSUB=1,NISUB
          DO J=1,24
            FSAVSUB1(J,JSUB)=ZERO
          END DO
        ENDDO
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          NN = NSVG(I)
          IF(NN>0)THEN
            IN=CN_LOC(I)
            IF (MSEGTYP(CE_LOC(I)) < 0) THEN
              IE= - MSEGTYP(CE_LOC(I))
            ELSE
              IE = CE_LOC(I)
            ENDIF
            JJ  =ADDSUBS(IN)
            KK  =ADDSUBM(IE)
            DO WHILE(JJ<ADDSUBS(IN+1))

              JSUB=LISUBS(JJ)
              ITYPSUB = TYPSUB(JSUB)

              IF(ITYPSUB == 1 ) THEN  ! Defining specific inter
                KSUB=LISUBM(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBM(IE+1)))

                  IF(KSUB==JSUB)THEN
                    IMPX=FXI(I)*DT12
                    IMPY=FYI(I)*DT12
                    IMPZ=FZI(I)*DT12
C                 main side :
                    FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                    FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                    FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                      FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                      FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                    ENDIF
C
                    FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                    FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                    FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                    FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
C
                  END IF

                  KK=KK+1
                  KSUB=LISUBM(KK)
                ENDDO
                JJ=JJ+1

              ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surf : secondary side

                IMPX=FXI(I)*DT12
                IMPY=FYI(I)*DT12
                IMPZ=FZI(I)*DT12

                FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ

                FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12

                IF(ISENSINT(JSUB+1)/=0) THEN
                  FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                  FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                  FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                ENDIF

                JJ=JJ+1

              ELSEIF(ITYPSUB == 3) THEN  !Inter =0 : collecting forces from all inter with 2 surfs

                ISS2 = BITGET(INFLG_SUBS(JJ),0)
                ISS1 = BITGET(INFLG_SUBS(JJ),1)
                KSUB=LISUBM(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBM(IE+1)))
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB==JSUB)THEN
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                        (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBM(KK)
                      CYCLE
                    END IF

                    IMPX=FXI(I)*DT12
                    IMPY=FYI(I)*DT12
                    IMPZ=FZI(I)*DT12

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
                        FSAVPARIT(JSUB+1,1,I+NFT) =  -FXI(I)
                        FSAVPARIT(JSUB+1,2,I+NFT) =  -FYI(I)
                        FSAVPARIT(JSUB+1,3,I+NFT) =  -FZI(I)
                      ELSE
                        FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                        FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                        FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                      END IF
                    ENDIF
C
                    FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                    FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                    FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                  END IF

                  KK=KK+1
                  KSUB=LISUBM(KK)
                ENDDO
                JJ=JJ+1
              ENDIF

            END DO
          END IF


          IF (MSEGTYP(CE_LOC(I)) < 0) THEN
            IE= - MSEGTYP(CE_LOC(I))
          ELSE
            IE = CE_LOC(I)
          ENDIF

          KK  =ADDSUBM(IE)
          DO WHILE(KK<ADDSUBM(IE+1))
            KSUB=LISUBM(KK)

            ITYPSUB = TYPSUB(KSUB)
            IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : main side

              IMPX=-FXI(I)*DT12
              IMPY=-FYI(I)*DT12
              IMPZ=-FZI(I)*DT12

              FSAVSUB1(1,KSUB)=FSAVSUB1(1,KSUB)+IMPX
              FSAVSUB1(2,KSUB)=FSAVSUB1(2,KSUB)+IMPY
              FSAVSUB1(3,KSUB)=FSAVSUB1(3,KSUB)+IMPZ

              FSAVSUB1(8,KSUB) =FSAVSUB1(8,KSUB) +ABS(IMPX)
              FSAVSUB1(9,KSUB) =FSAVSUB1(9,KSUB) +ABS(IMPY)
              FSAVSUB1(10,KSUB)=FSAVSUB1(10,KSUB)+ABS(IMPZ)

              FSAVSUB1(11,KSUB)=FSAVSUB1(11,KSUB)-FNI(I)*DT12

              IF(ISENSINT(KSUB+1)/=0) THEN
                FSAVPARIT(KSUB+1,1,I+NFT) =  -FXI(I)
                FSAVPARIT(KSUB+1,2,I+NFT) =  -FYI(I)
                FSAVPARIT(KSUB+1,3,I+NFT) =  -FZI(I)
              ENDIF


            ENDIF
            KK=KK+1
          ENDDO

        END DO


        IF(NSPMD>1) THEN
C loop split because of a PGI bug
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            NN = NSVG(I)
            IF(NN<0)THEN
              NN = -NN
              IF (MSEGTYP(CE_LOC(I)) < 0) THEN
                IE= - MSEGTYP(CE_LOC(I))
              ELSE
                IE = CE_LOC(I)
              ENDIF
              JJ  =ADDSUBSFI(NIN)%P(NN)
              KK  =ADDSUBM(IE)
              DO WHILE(JJ<ADDSUBSFI(NIN)%P(NN+1))
                JSUB=LISUBSFI(NIN)%P(JJ)
                ITYPSUB = TYPSUB(JSUB)

                IF(ITYPSUB == 1 ) THEN  ! Defining specific inter

                  KSUB=LISUBM(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBM(IE+1)))
                    IF(KSUB==JSUB)THEN
                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12
C                main side :
                      FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                      FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                      FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ
C
                      IF(ISENSINT(JSUB+1)/=0) THEN
                        FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                        FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                        FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                      ENDIF
C
                      FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                      FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                      FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                      FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12
C
                    END IF

                    KK=KK+1
                    KSUB=LISUBM(KK)
                  ENDDO
                  JJ=JJ+1

                ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surf : secondary side

                  IMPX=FXI(I)*DT12
                  IMPY=FYI(I)*DT12
                  IMPZ=FZI(I)*DT12

                  FSAVSUB1(1,JSUB)=FSAVSUB1(1,JSUB)+IMPX
                  FSAVSUB1(2,JSUB)=FSAVSUB1(2,JSUB)+IMPY
                  FSAVSUB1(3,JSUB)=FSAVSUB1(3,JSUB)+IMPZ

                  FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                  FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                  FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)

                  FSAVSUB1(11,JSUB)=FSAVSUB1(11,JSUB)+FNI(I)*DT12

                  IF(ISENSINT(JSUB+1)/=0) THEN
                    FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                    FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                    FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
                  ENDIF

                  JJ=JJ+1

                ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces

                  ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
                  ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
                  KSUB=LISUBM(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBM(IE+1)))
                    IMS2 = BITGET(INFLG_SUBM(KK),0)
                    IMS1 = BITGET(INFLG_SUBM(KK),1)
                    IF(KSUB==JSUB)THEN
                      IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                         (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK)
                        CYCLE
                      END IF

                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12

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
                          FSAVPARIT(JSUB+1,1,I+NFT) =  -FXI(I)
                          FSAVPARIT(JSUB+1,2,I+NFT) =  -FYI(I)
                          FSAVPARIT(JSUB+1,3,I+NFT) =  -FZI(I)
                        ELSE
                          FSAVPARIT(JSUB+1,1,I+NFT) =  FXI(I)
                          FSAVPARIT(JSUB+1,2,I+NFT) =  FYI(I)
                          FSAVPARIT(JSUB+1,3,I+NFT) =  FZI(I)
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

C---------------------------------
C       NEW FRICTION MODELS
C---------------------------------

C   Friction coefficient computation
C++++++++++++++++++++++++++++++++++++++++

      IF(IORTHFRIC == 0) THEN
C++ Isotropic Friction

        IF (MFROT==0) THEN
C---      Coulomb friction
          DO I=1,JLT
            IF(PENE(I) == 0) THEN
              XMU(I) = ZERO
              CYCLE
            ENDIF
            XMU(I) = FRICC(I)
          ENDDO
        ELSEIF (MFROT==1) THEN
C---      Viscous friction
          DO I=1,JLT
            IF(PENE(I) == 0) THEN
              XMU(I) = ZERO
              CYCLE
            ENDIF
            IE=CE_LOC(I)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
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
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P
     .             +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*V2
            XMU(I) = MAX(XMU(I),EM30)
          ENDDO
        ELSEIF(MFROT==2)THEN
C---        Loi Darmstad
          DO I=1,JLT
            IF(PENE(I) == 0) THEN
              XMU(I) = ZERO
              CYCLE
            ENDIF
            IE=CE_LOC(I)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
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
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRICC(I)
     .             + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .             + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .             + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
            XMU(I) = MAX(XMU(I),EM30)
          ENDDO
        ELSEIF (MFROT==3) THEN
C---        Loi Renard
          DO I=1,JLT
            IF(PENE(I) == 0) THEN
              XMU(I) = ZERO
              CYCLE
            ENDIF
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
            VV = SQRT(MAX(EM30,V2))
            IF(VV>=0.AND.VV<=FRIC_COEFS(I,5)) THEN
              DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
              VV1 = VV / FRIC_COEFS(I,5)
              XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(TWO-VV1)
            ELSEIF(VV>FRIC_COEFS(I,5).AND.VV<FRIC_COEFS(I,6)) THEN
              DMU = FRIC_COEFS(I,4)-FRIC_COEFS(I,3)
              VV1 = (VV - FRIC_COEFS(I,5))/(FRIC_COEFS(I,6)-FRIC_COEFS(I,5))
              XMU(I) = FRIC_COEFS(I,3)+ DMU * (THREE-TWO*VV1)*VV1**2
            ELSE
              DMU = FRIC_COEFS(I,2)-FRIC_COEFS(I,4)
              VV2 = (VV - FRIC_COEFS(I,6))**2
              XMU(I) = FRIC_COEFS(I,2) - DMU / (ONE + DMU*VV2)
            ENDIF
            XMU(I) = MAX(XMU(I),EM30)
          ENDDO
        ENDIF

      ELSE
C++ Orthotropic Friction

        IF (MFROT==0) THEN
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
            IF(XMU(I)<=EM30) THEN
              XMU(I) = EM30
              DIR1(I,1:3) = ZERO
              AN(K) = ZERO
            ELSE
              AN(K)=XMU(I)**2
              AN(K)=ONE/AN(K)
            ENDIF
            IF(XMU2(I)<=EM30) THEN
              XMU2(I) = EM30
              DIR2(I,1:3) = ZERO
              BN(K) = ZERO
            ELSE
              BN(K)=XMU2(I)**2
              BN(K)=ONE/BN(K)
            ENDIF
          ENDDO
        ELSEIF (MFROT==1) THEN
C---      Viscous friction
#include   "vectorize.inc"
          DO K=1,NFISOT  ! isotropic friction couples
            I = INDEXISOT(K)
            IE=CE_LOC(I)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
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
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P
     .             +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*V2
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
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
c
            V2 = VX(I)*DIR1(K,1) +VY(I)*DIR1(K,2)+VZ(I)*DIR1(K,3)
            VV  = MAX(EM30,V2)
            XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P
     .             +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*VV*VV

            V2 = VX(I)*DIR2(K,1) +VY(I)*DIR2(K,2)+VZ(I)*DIR2(K,3)
            VV  = MAX(EM30,V2)
            XMU2(I) = FRICC2(I) + (FRIC_COEFS2(I,1) + FRIC_COEFS2(I,4)*P ) * P
     .           +(FRIC_COEFS2(I,2) + FRIC_COEFS2(I,3)*P) * VV + FRIC_COEFS2(I,5)*VV*VV

            XMU(I) = MAX(XMU(I),EM30)
            XMU2(I) = MAX(XMU2(I),EM30)

          ENDDO

#include   "vectorize.inc"
          DO K=1,NFORTH ! Orthotropic friction couples : treat differently cases where mu1=0 AND/OR mu2=0
            I = INDEXORTH(K)
            IF(XMU(I)<=EM30) THEN
              XMU(I) = EM30
              DIR1(I,1:3) = ZERO
              AN(K) = ZERO
            ELSE
              AN(K)=XMU(I)**2
              AN(K)=ONE/AN(K)
            ENDIF
            IF(XMU2(I)<=EM30) THEN
              XMU2(I) = EM30
              DIR2(I,1:3) = ZERO
              BN(K) = ZERO
            ELSE
              BN(K)=XMU2(I)**2
              BN(K)=ONE/BN(K)
            ENDIF
          ENDDO


        ELSEIF(MFROT==2)THEN
C---        Loi Darmstad
#include   "vectorize.inc"
          DO K=1,NFISOT  ! isotropic friction couples
            I = INDEXISOT(K)
            IE=CE_LOC(I)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
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
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRICC(I)
     .             + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .             + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .             + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
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
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
c
            V2 = VX(I)*DIR1(K,1) +VY(I)*DIR1(K,2)+VZ(I)*DIR1(K,3)
            VV  = MAX(EM30,V2)
            XMU(I) = FRICC(I)
     .             + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .             + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .             + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
c
            V2 = VX(I)*DIR2(K,1) +VY(I)*DIR2(K,2)+VZ(I)*DIR2(K,3)
            VV  = MAX(EM30,V2)
            XMU2(I) = FRICC2(I)
     .             + FRIC_COEFS2(I,1)*EXP(FRIC_COEFS2(I,2)*VV)*P*P
     .             + FRIC_COEFS2(I,3)*EXP(FRIC_COEFS2(I,4)*VV)*P
     .             + FRIC_COEFS2(I,5)*EXP(FRIC_COEFS2(I,6)*VV)

            XMU(I) = MAX(XMU(I),EM30)
            XMU2(I) = MAX(XMU2(I),EM30)

          ENDDO

#include   "vectorize.inc"
          DO K=1,NFORTH ! Orthotropic friction couples : treat differently cases where mu1=0 AND/OR mu2=0
            I = INDEXORTH(K)
            IF(XMU(I)<=EM30) THEN
              XMU(I) = EM30
              DIR1(I,1:3) = ZERO
              AN(K) = ZERO
            ELSE
              AN(K)=XMU(I)**2
              AN(K)=ONE/AN(K)
            ENDIF
            IF(XMU2(I)<=EM30) THEN
              XMU2(I) = EM30
              DIR2(I,1:3) = ZERO
              BN(K) = ZERO
            ELSE
              BN(K)=XMU2(I)**2
              BN(K)=ONE/BN(K)
            ENDIF
          ENDDO

        ELSEIF (MFROT==3) THEN
C---        Loi Renard
#include   "vectorize.inc"
          DO K=1,NFISOT  ! isotropic friction couples
            I = INDEXISOT(K)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
            VV = SQRT(MAX(EM30,V2))
            IF(VV>=0.AND.VV<=FRIC_COEFS(I,5)) THEN
              DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
              VV1 = VV / FRIC_COEFS(I,5)
              XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(TWO-VV1)
            ELSEIF(VV>FRIC_COEFS(I,5).AND.VV<FRIC_COEFS(I,6)) THEN
              DMU = FRIC_COEFS(I,4)-FRIC_COEFS(I,3)
              VV1 = (VV - FRIC_COEFS(I,5))/(FRIC_COEFS(I,6)-FRIC_COEFS(I,5))
              XMU(I) = FRIC_COEFS(I,3)+ DMU * (THREE-TWO*VV1)*VV1**2
            ELSE
              DMU = FRIC_COEFS(I,2)-FRIC_COEFS(I,4)
              VV2 = (VV - FRIC_COEFS(I,6))**2
              XMU(I) = FRIC_COEFS(I,2) - DMU / (ONE + DMU*VV2)
            ENDIF
            XMU(I) = MAX(XMU(I),EM30)
          ENDDO

#include   "vectorize.inc"
          DO K=1,NFORTH ! Orthotropic friction couples
            I = INDEXORTH(K)
c
            V2 = VX(I)*DIR1(K,1) +VY(I)*DIR1(K,2)+VZ(I)*DIR1(K,3)
            VV  = MAX(EM30,V2)
            IF(VV>=0.AND.VV<=FRIC_COEFS(I,5)) THEN
              DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
              VV1 = VV / FRIC_COEFS(I,5)
              XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(TWO-VV1)
            ELSEIF(VV>FRIC_COEFS(I,5).AND.VV<FRIC_COEFS(I,6)) THEN
              DMU = FRIC_COEFS(I,4)-FRIC_COEFS(I,3)
              VV1 = (VV - FRIC_COEFS(I,5))/(FRIC_COEFS(I,6)-FRIC_COEFS(I,5))
              XMU(I) = FRIC_COEFS(I,3)+ DMU * (THREE-TWO*VV1)*VV1**2
            ELSE
              DMU = FRIC_COEFS(I,2)-FRIC_COEFS(I,4)
              VV2 = (VV - FRIC_COEFS(I,6))**2
              XMU(I) = FRIC_COEFS(I,2) - DMU / (ONE + DMU*VV2)
            ENDIF

            V2 = VX(I)*DIR2(K,1) +VY(I)*DIR2(K,2)+VZ(I)*DIR2(K,3)
            VV  = MAX(EM30,V2)
            IF(VV>=0.AND.VV<=FRIC_COEFS2(I,5)) THEN
              DMU = FRIC_COEFS2(I,3)-FRIC_COEFS2(I,1)
              VV1 = VV / FRIC_COEFS2(I,5)
              XMU2(I) = FRIC_COEFS2(I,1)+ DMU*VV1*(TWO-VV1)
            ELSEIF(VV>FRIC_COEFS2(I,5).AND.VV<FRIC_COEFS2(I,6)) THEN
              DMU = FRIC_COEFS2(I,4)-FRIC_COEFS2(I,3)
              VV1 = (VV - FRIC_COEFS2(I,5))/(FRIC_COEFS2(I,6)-FRIC_COEFS2(I,5))
              XMU2(I) = FRIC_COEFS2(I,3)+ DMU * (THREE-TWO*VV1)*VV1**2
            ELSE
              DMU = FRIC_COEFS2(I,2)-FRIC_COEFS2(I,4)
              VV2 = (VV - FRIC_COEFS2(I,6))**2
              XMU2(I) = FRIC_COEFS2(I,2) - DMU / (ONE + DMU*VV2)
            ENDIF
            XMU(I) = MAX(XMU(I),EM30)
            XMU2(I) = MAX(XMU2(I),EM30)
          ENDDO

#include   "vectorize.inc"
          DO K=1,NFORTH ! Orthotropic friction couples : treat differently cases where mu1=0 AND/OR mu2=0
            I = INDEXORTH(K)
            IF(XMU(I)<=EM30) THEN
              XMU(I) = EM30
              DIR1(I,1:3) = ZERO
              AN(K) = ZERO
            ELSE
              AN(K)=XMU(I)**2
              AN(K)=ONE/AN(K)
            ENDIF
            IF(XMU2(I)<=EM30) THEN
              XMU2(I) = EM30
              DIR2(I,1:3) = ZERO
              BN(K) = ZERO
            ELSE
              BN(K)=XMU2(I)**2
              BN(K)=ONE/BN(K)
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
      IF (IFQ /= 0) THEN
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
        IF (IFQ==13) THEN
          ALPHA = MAX(ONE,ALPHA0*DT12)
        ELSE
          ALPHA = ALPHA0
        ENDIF

        IF(INTNITSCHE > 0 ) THEN

          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FTN = FORNEQSI(I,1)*N1(I) + FORNEQSI(I,2)*N2(I) + FORNEQSI(I,3)*N3(I)
            FNITXT(I) = HALF*(FORNEQSI(I,1) - FTN*N1(I))
            FNITYT(I) = HALF*(FORNEQSI(I,2) - FTN*N2(I))
            FNITZT(I) = HALF*(FORNEQSI(I,3) - FTN*N3(I))
          ENDDO
        ELSE
          DO I=1,JLT
            FNITXT(I) = ZERO
            FNITYT(I) = ZERO
            FNITZT(I) = ZERO
          ENDDO
        ENDIF

        IF(IORTHFRIC ==0 ) THEN
C++ Isotropic Friction

          IF (INCONV==1) THEN
            DO I=1,JLT
              IF(PENE(I) == ZERO)CYCLE
              FX = STIF0(I)*VX(I)*DT12
              FY = STIF0(I)*VY(I)*DT12
              FZ = STIF0(I)*VZ(I)*DT12
              JG = NSVG(I)
              IF(JG>0) THEN
                N = CAND_N_N(I)
c             SECND_FR(4:6,N) is the old friction force
                FX = SECND_FR(4,N) + ALPHA*FX
                FY = SECND_FR(5,N) + ALPHA*FY
                FZ = SECND_FR(6,N) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA

#include "lockon.inc"
                IF (ABS(FXT(I)- FNITXT(I))>ABS(SECND_FR(1,N))) SECND_FR(1,N) = FXT(I)- FNITXT(I)
                IF (ABS(FYT(I)- FNITYT(I))>ABS(SECND_FR(2,N))) SECND_FR(2,N) = FYT(I)- FNITYT(I)
                IF (ABS(FZT(I)- FNITZT(I))>ABS(SECND_FR(3,N))) SECND_FR(3,N) = FZT(I)- FNITZT(I)
C case equal abs but opposite sign
                IF ((FXT(I)- FNITXT(I))==-SECND_FR(1,N)) SECND_FR(1,N) = ABS(FXT(I)- FNITXT(I))
                IF ((FYT(I)- FNITYT(I))==-SECND_FR(2,N)) SECND_FR(2,N) = ABS(FYT(I)- FNITYT(I))
                IF ((FZT(I)- FNITZT(I))==-SECND_FR(3,N)) SECND_FR(3,N) = ABS(FZT(I)- FNITZT(I))

#include "lockoff.inc"
              ELSE ! cas noeud remote en SPMD
                JG = -JG
c             SECND_FRFI(NIN)%P(4:6,JG) is the old friction force
                FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
#include "lockon.inc"
                IF ( ABS(FXT(I)- FNITXT(I)) > ABS(SECND_FRFI(NIN)%P(1,JG)) )
     *                                  SECND_FRFI(NIN)%P(1,JG) = FXT(I)- FNITXT(I)
                IF ( ABS(FYT(I)- FNITYT(I)) > ABS(SECND_FRFI(NIN)%P(2,JG)) )
     *                                  SECND_FRFI(NIN)%P(2,JG) = FYT(I)- FNITYT(I)
                IF ( ABS(FZT(I)- FNITZT(I)) > ABS(SECND_FRFI(NIN)%P(3,JG)) )
     *                                  SECND_FRFI(NIN)%P(3,JG) = FZT(I)- FNITZT(I)
C
                IF ((FXT(I)- FNITXT(I))== - SECND_FRFI(NIN)%P(1,JG) )
     *                           SECND_FRFI(NIN)%P(1,JG) = ABS(FXT(I)- FNITXT(I))
                IF ((FYT(I)- FNITYT(I))== - SECND_FRFI(NIN)%P(2,JG) )
     *                           SECND_FRFI(NIN)%P(2,JG) = ABS(FYT(I)- FNITYT(I))
                IF ((FZT(I)- FNITZT(I))== - SECND_FRFI(NIN)%P(3,JG) )
     *                           SECND_FRFI(NIN)%P(3,JG) = ABS(FZT(I)- FNITZT(I))
#include "lockoff.inc"
              ENDIF
C-------      total force
              FXI(I)=FXI(I) + FXT(I)
              FYI(I)=FYI(I) + FYT(I)
              FZI(I)=FZI(I) + FZT(I)
              EFRIC_L(I) = DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
              ECONVT = ECONVT + EFRIC_L(I)
            ENDDO
C--------implicit non converge---
          ELSE
            DO I=1,JLT
              IF(PENE(I) == ZERO)CYCLE
              FX = STIF0(I)*VX(I)*DT12
              FY = STIF0(I)*VY(I)*DT12
              FZ = STIF0(I)*VZ(I)*DT12
              JG = NSVG(I)
              N = CAND_N_N(I)
              IF(JG>0) THEN
c             SECND_FR(4:6,N) is the old friction force
                FX = SECND_FR(4,N) + ALPHA*FX
                FY = SECND_FR(5,N) + ALPHA*FY
                FZ = SECND_FR(6,N) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
              ELSE ! cas noeud remote en SPMD
                JG = -JG
                FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
              ENDIF
C            IFPEN(INDEX(I)) = 1
            ENDDO
          ENDIF

        ELSE
C++ Orthotropic Friction

          IF (INCONV==1) THEN
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IF(PENE(I) == ZERO)CYCLE
              FX = STIF0(I)*VX(I)*DT12
              FY = STIF0(I)*VY(I)*DT12
              FZ = STIF0(I)*VZ(I)*DT12
              JG = NSVG(I)
              IF(JG>0) THEN
                N = CAND_N_N(I)
c             SECND_FR(4:6,N) is the old friction force
                FX = SECND_FR(4,N) + ALPHA*FX
                FY = SECND_FR(5,N) + ALPHA*FY
                FZ = SECND_FR(6,N) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
#include "lockon.inc"
                IF (ABS(FXT(I)- FNITXT(I))>ABS(SECND_FR(1,N))) SECND_FR(1,N) = FXT(I)- FNITXT(I)
                IF (ABS(FYT(I)- FNITYT(I))>ABS(SECND_FR(2,N))) SECND_FR(2,N) = FYT(I)- FNITYT(I)
                IF (ABS(FZT(I)- FNITZT(I))>ABS(SECND_FR(3,N))) SECND_FR(3,N) = FZT(I)- FNITZT(I)
C case equal abs but opposite sign
                IF ((FXT(I)- FNITXT(I))==-SECND_FR(1,N)) SECND_FR(1,N) = ABS(FXT(I)- FNITXT(I))
                IF ((FYT(I)- FNITYT(I))==-SECND_FR(2,N)) SECND_FR(2,N) = ABS(FYT(I)- FNITYT(I))
                IF ((FZT(I)- FNITZT(I))==-SECND_FR(3,N)) SECND_FR(3,N) = ABS(FZT(I)- FNITZT(I))
#include "lockoff.inc"
              ELSE ! cas noeud remote en SPMD
                JG = -JG
c             SECND_FRFI(NIN)%P(4:6,JG) is the old friction force
                FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
#include "lockon.inc"
                IF ( ABS(FXT(I)- FNITXT(I)) > ABS(SECND_FRFI(NIN)%P(1,JG)) )
     *                                  SECND_FRFI(NIN)%P(1,JG) = FXT(I)- FNITXT(I)
                IF ( ABS(FYT(I)- FNITYT(I)) > ABS(SECND_FRFI(NIN)%P(2,JG)) )
     *                                  SECND_FRFI(NIN)%P(2,JG) = FYT(I)- FNITYT(I)
                IF ( ABS(FZT(I)- FNITZT(I)) > ABS(SECND_FRFI(NIN)%P(3,JG)) )
     *                                  SECND_FRFI(NIN)%P(3,JG) = FZT(I)- FNITZT(I)
C
                IF ((FXT(I)- FNITXT(I))== - SECND_FRFI(NIN)%P(1,JG) )
     *                           SECND_FRFI(NIN)%P(1,JG) = ABS(FXT(I)- FNITXT(I))
                IF ((FYT(I)- FNITYT(I))== - SECND_FRFI(NIN)%P(2,JG) )
     *                           SECND_FRFI(NIN)%P(2,JG) = ABS(FYT(I)- FNITYT(I))
                IF ((FZT(I)- FNITZT(I))== - SECND_FRFI(NIN)%P(3,JG) )
     *                           SECND_FRFI(NIN)%P(3,JG) = ABS(FZT(I)- FNITZT(I))
#include "lockoff.inc"
              ENDIF
C-------      total force
              FXI(I)=FXI(I) + FXT(I)
              FYI(I)=FYI(I) + FYT(I)
              FZI(I)=FZI(I) + FZT(I)
              EFRIC_L(I) = DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
              ECONVT = ECONVT + EFRIC_L(I)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH  ! Orthotropic friction couples
              I = INDEXORTH(K)
              IF(PENE(I) == ZERO)CYCLE
              FX = STIF0(I)*VX(I)*DT12
              FY = STIF0(I)*VY(I)*DT12
              FZ = STIF0(I)*VZ(I)*DT12
              JG = NSVG(I)
              IF(JG>0) THEN
                N = CAND_N_N(I)
c             SECND_FR(4:6,N) is the old friction force
                FX = SECND_FR(4,N) + ALPHA*FX
                FY = SECND_FR(5,N) + ALPHA*FY
                FZ = SECND_FR(6,N) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)

                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)

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

#include "lockon.inc"
                IF (ABS(FXT(I))>ABS(SECND_FR(1,N))) SECND_FR(1,N) = FXT(I)
                IF (ABS(FYT(I))>ABS(SECND_FR(2,N))) SECND_FR(2,N) = FYT(I)
                IF (ABS(FZT(I))>ABS(SECND_FR(3,N))) SECND_FR(3,N) = FZT(I)
C case equal abs but opposite sign
                IF (FXT(I)==-SECND_FR(1,N)) SECND_FR(1,N) = ABS(FXT(I))
                IF (FYT(I)==-SECND_FR(2,N)) SECND_FR(2,N) = ABS(FYT(I))
                IF (FZT(I)==-SECND_FR(3,N)) SECND_FR(3,N) = ABS(FZT(I))
#include "lockoff.inc"
              ELSE ! cas noeud remote en SPMD
                JG = -JG
c             SECND_FRFI(NIN)%P(4:6,JG) is the old friction force
                FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
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
#include "lockon.inc"
                IF ( ABS(FXT(I)) > ABS(SECND_FRFI(NIN)%P(1,JG)) )
     *                                  SECND_FRFI(NIN)%P(1,JG) = FXT(I)
                IF ( ABS(FYT(I)) > ABS(SECND_FRFI(NIN)%P(2,JG)) )
     *                                  SECND_FRFI(NIN)%P(2,JG) = FYT(I)
                IF ( ABS(FZT(I)) > ABS(SECND_FRFI(NIN)%P(3,JG)) )
     *                                  SECND_FRFI(NIN)%P(3,JG) = FZT(I)
C
                IF (FXT(I)== - SECND_FRFI(NIN)%P(1,JG) )
     *                           SECND_FRFI(NIN)%P(1,JG) = ABS(FXT(I))
                IF (FYT(I)== - SECND_FRFI(NIN)%P(2,JG) )
     *                           SECND_FRFI(NIN)%P(2,JG) = ABS(FYT(I))
                IF (FZT(I)== - SECND_FRFI(NIN)%P(3,JG) )
     *                           SECND_FRFI(NIN)%P(3,JG) = ABS(FZT(I))
#include "lockoff.inc"
              ENDIF
C-------      total force
              FXI(I)=FXI(I) + FXT(I)
              FYI(I)=FYI(I) + FYT(I)
              FZI(I)=FZI(I) + FZT(I)
              EFRIC_L(I) = DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
              ECONVT = ECONVT + EFRIC_L(I)
            ENDDO


C--------implicit non converge---
          ELSE
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IF(PENE(I) == ZERO)CYCLE
              IF(PENE(I) == ZERO)CYCLE
              FX = STIF0(I)*VX(I)*DT12
              FY = STIF0(I)*VY(I)*DT12
              FZ = STIF0(I)*VZ(I)*DT12
              JG = NSVG(I)
              N = CAND_N_N(I)
              IF(JG>0) THEN
c             SECND_FR(4:6,N) is the old friction force
                FX = SECND_FR(4,N) + ALPHA*FX
                FY = SECND_FR(5,N) + ALPHA*FY
                FZ = SECND_FR(6,N) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
              ELSE ! cas noeud remote en SPMD
                JG = -JG
                FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)
                FT = FX*FX + FY*FY + FZ*FZ
                FT = MAX(FT,TINY)
                FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                FXT(I) = FX * BETA
                FYT(I) = FY * BETA
                FZT(I) = FZ * BETA
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
              ENDIF
C            IFPEN(INDEX(I)) = 1
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH  ! Orthotropic friction couples
              I = INDEXORTH(K)
              IF(PENE(I) == ZERO)CYCLE
              IF(PENE(I) == ZERO)CYCLE
              FX = STIF0(I)*VX(I)*DT12
              FY = STIF0(I)*VY(I)*DT12
              FZ = STIF0(I)*VZ(I)*DT12
              JG = NSVG(I)
              N = CAND_N_N(I)
              IF(JG>0) THEN
c             SECND_FR(4:6,N) is the old friction force
                FX = SECND_FR(4,N) + ALPHA*FX
                FY = SECND_FR(5,N) + ALPHA*FY
                FZ = SECND_FR(6,N) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)

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
              ELSE ! cas noeud remote en SPMD
                JG = -JG
                FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                FX = FX - FTN*N1(I)
                FY = FY - FTN*N2(I)
                FZ = FZ - FTN*N3(I)
                FX = FX + FNITXT(I)
                FY = FY + FNITYT(I)
                FZ = FZ + FNITZT(I)

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
              ENDIF
C            IFPEN(INDEX(I)) = 1
            ENDDO

          ENDIF !imp

        ENDIF ! iorth



      ENDIF    !ifq
C
C---------------------------------
      IF((ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.AND.
     .          ((TT>=TANIM.AND.TT<=TANIM_STOP).OR.TT>=TOUTP.OR.(TT>=H3D_DATA%TH3D.AND.TT<=H3D_DATA%TH3D_STOP).OR.
     .              (MANIM>=4.AND.MANIM<=15).OR.H3D_DATA%MH3D/=0))
     .   .OR.H3D_DATA%N_VECT_PCONT_MAX>0)THEN
        IF (INCONV==1) THEN
#include "lockon.inc"
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
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
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              IF (JG >NUMNOD) THEN
                IG = JG - NUMNOD
                CALL I24FOR1_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                          IG     ,FXT(I)  ,FYT(I)  ,FZT(I)  ,FTCONT  ,
     +                          -1     )
              ELSE
                FTCONT(1,JG)=FTCONT(1,JG)- FXT(I)
                FTCONT(2,JG)=FTCONT(2,JG)- FYT(I)
                FTCONT(3,JG)=FTCONT(3,JG)- FZT(I)
              ENDIF
            ELSE
              JG=-JG
              IF(ISEDGE_FI(NIN)%P(JG)==1)THEN
                CALL I24FOR1_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                       JG      ,FXT(I)  ,FYT(I)  ,FZT(I)  ,FTCONTI(NIN)%P(1,1) ,
     +                       -1      )
              ELSE ! cas noeud remote en SPMD non edge
                FTCONTI(NIN)%P(1,JG)=FTCONTI(NIN)%P(1,JG)-FXT(I)
                FTCONTI(NIN)%P(2,JG)=FTCONTI(NIN)%P(2,JG)-FYT(I)
                FTCONTI(NIN)%P(3,JG)=FTCONTI(NIN)%P(3,JG)-FZT(I)
              ENDIF
            END IF
          ENDDO
#include "lockoff.inc"
        END IF !(INCONV==1) THEN
      ENDIF
C
C---------------------------------
      FSAV22= ZERO
      FSAV23= ZERO
      FSAV24= ZERO
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
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
      FSAV(26) = FSAV(26) + ECONTT
      FSAV(27) = FSAV(27) + ECONVT
      FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          FSAVPARIT(1,4,I+NFT) =  FXI(I)
          FSAVPARIT(1,5,I+NFT) =  FYI(I)
          FSAVPARIT(1,6,I+NFT) =  FZI(I)
        ENDDO
      ENDIF
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB/=0)THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          NN = NSVG(I)
          IF(NN>0)THEN
            IN=CN_LOC(I)
            IF (MSEGTYP(CE_LOC(I)) < 0) THEN
              IE= - MSEGTYP(CE_LOC(I))
            ELSE
              IE = CE_LOC(I)
            ENDIF
            JJ  =ADDSUBS(IN)
            KK  =ADDSUBM(IE)
            DO WHILE(JJ<ADDSUBS(IN+1))
              JSUB=LISUBS(JJ)
              ITYPSUB = TYPSUB(JSUB)

              IF(ITYPSUB == 1 ) THEN
                KSUB=LISUBM(KK)
                DO WHILE(KK<ADDSUBM(IE+1))

                  IF(KSUB==JSUB)THEN
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
C
                    IF(ISENSINT(JSUB+1)/=0) THEN
                      FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                      FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                      FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                    ENDIF
C
                    FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                            +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                    FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                                       +YP(I)*IMPZ-ZP(I)*IMPY
                    FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                                       +ZP(I)*IMPX-XP(I)*IMPZ
                    FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                            +XP(I)*IMPY-YP(I)*IMPX
C
                  END IF

                  KK=KK+1
                  KSUB=LISUBM(KK)
                ENDDO
                JJ=JJ+1

              ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface : second side

                IMPX=FXT(I)*DT12
                IMPY=FYT(I)*DT12
                IMPZ=FZT(I)*DT12
C             main side :
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
                IF(ISENSINT(JSUB+1)/=0) THEN
                  FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                  FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                  FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
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

                JJ=JJ+1


              ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces
C
C            Find if node is on Surface S1 -> S2  or S2 ->S1
                ISS2 = BITGET(INFLG_SUBS(JJ),0)
                ISS1 = BITGET(INFLG_SUBS(JJ),1)
                KSUB=LISUBM(KK)
                DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBM(IE+1)))
                  IMS2 = BITGET(INFLG_SUBM(KK),0)
                  IMS1 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB==JSUB)THEN
!                S and M candidates on the same sub_interface
                    IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                       (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBM(KK)
                      CYCLE
                    END IF

                    IMPX=FXT(I)*DT12
                    IMPY=FYT(I)*DT12
                    IMPZ=FZT(I)*DT12
                    IF(IMS2 > 0) THEN
C                main side :
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
                        FSAVPARIT(JSUB+1,4,I+NFT) =  -FXT(I)
                        FSAVPARIT(JSUB+1,5,I+NFT) =  -FYT(I)
                        FSAVPARIT(JSUB+1,6,I+NFT) =  -FZT(I)
                      ELSE
                        FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                        FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                        FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                      ENDIF
                    ENDIF
C
                    FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                              +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                    FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                          +YP(I)*IMPZ-ZP(I)*IMPY
                    FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                          +ZP(I)*IMPX-XP(I)*IMPZ
                    FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                          +XP(I)*IMPY-YP(I)*IMPX
                  ENDIF
                  KK=KK+1
                  KSUB=LISUBM(KK)
                ENDDO
                JJ=JJ+1


              ENDIF

            END DO
          END IF

          IF (MSEGTYP(CE_LOC(I)) < 0) THEN
            IE= - MSEGTYP(CE_LOC(I))
          ELSE
            IE = CE_LOC(I)
          ENDIF

          KK  =ADDSUBM(IE) ! Addresse du M
          DO WHILE(KK<ADDSUBM(IE+1))
!             all sub interfaces of S
            KSUB=LISUBM(KK)
!            KSUB Croissant
            ITYPSUB = TYPSUB(KSUB)
            IF(ITYPSUB == 2 ) THEN ! Inter =0 : collecting forces from all inter with only 1 surface
              IMPX=-FXT(I)*DT12
              IMPY=-FYT(I)*DT12
              IMPZ=-FZT(I)*DT12
C                main side :
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
                FSAVPARIT(KSUB+1,4,I+NFT) =  -FXT(I)
                FSAVPARIT(KSUB+1,5,I+NFT) =  -FYT(I)
                FSAVPARIT(KSUB+1,6,I+NFT) =  -FZT(I)
              ENDIF
C
              FSAVSUB1(15,KSUB)= FSAVSUB1(15,KSUB)
     .                         +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
              FSAVSUB1(22,KSUB)=FSAVSUB1(22,KSUB)
     .                         +YP(I)*IMPZ-ZP(I)*IMPY
              FSAVSUB1(23,KSUB)=FSAVSUB1(23,KSUB)
     .                         +ZP(I)*IMPX-XP(I)*IMPZ
              FSAVSUB1(24,KSUB)=FSAVSUB1(24,KSUB)
     .                         +XP(I)*IMPY-YP(I)*IMPX

            ENDIF
            KK=KK+1
          ENDDO

        END DO


        IF(NSPMD>1) THEN
C loop split because of a PGI bug
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            NN = NSVG(I)
            IF(NN<0)THEN
              NN = -NN
              IF (MSEGTYP(CE_LOC(I)) < 0) THEN
                IE= - MSEGTYP(CE_LOC(I))
              ELSE
                IE = CE_LOC(I)
              ENDIF
              JJ  =ADDSUBSFI(NIN)%P(NN)
              KK  =ADDSUBM(IE)
              DO WHILE(JJ<ADDSUBSFI(NIN)%P(NN+1))
                JSUB=LISUBSFI(NIN)%P(JJ)
                ITYPSUB = TYPSUB(JSUB)

                IF(ITYPSUB == 1 ) THEN
                  KSUB=LISUBM(KK)
                  DO WHILE(KK<ADDSUBM(IE+1))

                    IF(KSUB==JSUB)THEN
                      IMPX=FXT(I)*DT12
                      IMPY=FYT(I)*DT12
                      IMPZ=FZT(I)*DT12
C                  main side :
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
                      IF(ISENSINT(JSUB+1)/=0) THEN
                        FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                        FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                        FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                      ENDIF
C
                      FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                                 +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
                      FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                                             +YP(I)*IMPZ-ZP(I)*IMPY
                      FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                                             +ZP(I)*IMPX-XP(I)*IMPZ
                      FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                               +XP(I)*IMPY-YP(I)*IMPX
C
                    END IF

                    KK=KK+1
                    KSUB=LISUBM(KK)
                  ENDDO
                  JJ=JJ+1

                ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

                  IMPX=FXT(I)*DT12
                  IMPY=FYT(I)*DT12
                  IMPZ=FZT(I)*DT12
C             main side :
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
                  IF(ISENSINT(JSUB+1)/=0) THEN
                    FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                    FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                    FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
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

                  JJ=JJ+1

                ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces

                  ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
                  ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
                  KSUB=LISUBM(KK)
                  DO WHILE((KSUB<=JSUB).AND.(KK<ADDSUBM(IE+1)))
                    IMS2 = BITGET(INFLG_SUBM(KK),0)
                    IMS1 = BITGET(INFLG_SUBM(KK),1)
                    IF(KSUB==JSUB)THEN
                      IF(.NOT.((IMS1 == 1 .AND. ISS2 == 1).OR.
     .                         (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK)
                        CYCLE
                      END IF

                      IMPX=FXI(I)*DT12
                      IMPY=FYI(I)*DT12
                      IMPZ=FZI(I)*DT12

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
                        IF(IMS2 > 0 ) THEN
                          FSAVPARIT(JSUB+1,4,I+NFT) =  -FXT(I)
                          FSAVPARIT(JSUB+1,5,I+NFT) =  -FYT(I)
                          FSAVPARIT(JSUB+1,6,I+NFT) =  -FZT(I)
                        ELSE
                          FSAVPARIT(JSUB+1,4,I+NFT) =  FXT(I)
                          FSAVPARIT(JSUB+1,5,I+NFT) =  FYT(I)
                          FSAVPARIT(JSUB+1,6,I+NFT) =  FZT(I)
                        ENDIF
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
      IF (INCONV==1) THEN
        ECONTV = ECONTV + ECONVT ! Frictional Energy
        ECONT  = ECONT + ECONTT  ! Elatic Energy
        ECONTD = ECONTD + ECONTDT ! Damping Energy
      END IF !(INCONV==1) THEN
#include "lockoff.inc"
C---------------------------------
C---------------------------------
      IF(INTEREFRIC >0)THEN
        IF (INCONV==1) THEN
#include "lockon.inc"
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            EFRIC_LM = HALF*EFRIC_L(I)
            EFRIC(INTEREFRIC,IX1(I)) = EFRIC(INTEREFRIC,IX1(I)) + H1(I)*EFRIC_LM
            EFRIC(INTEREFRIC,IX2(I)) = EFRIC(INTEREFRIC,IX2(I)) + H2(I)*EFRIC_LM
            EFRIC(INTEREFRIC,IX3(I)) = EFRIC(INTEREFRIC,IX3(I)) + H3(I)*EFRIC_LM
            EFRIC(INTEREFRIC,IX4(I)) = EFRIC(INTEREFRIC,IX4(I)) + H4(I)*EFRIC_LM
            JG = NSVG(I)
            EFRIC_LS = HALF*EFRIC_L(I)
            IF(JG>0) THEN
              EFRIC(INTEREFRIC,JG) = EFRIC(INTEREFRIC,JG) + EFRIC_LS
            ELSE ! node remote
              JG = -JG
              EFRICFI(NIN)%P(JG)=EFRICFI(NIN)%P(JG)+ EFRIC_LS
            ENDIF
          ENDDO
#include "lockoff.inc"
        END IF !(INCONV==1) THEN
      ENDIF
C---------------------------------
      IF(H3D_DATA%N_SCAL_CSE_FRIC >0)THEN
        IF (INCONV==1) THEN
#include "lockon.inc"
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            EFRIC_LM = HALF*EFRIC_L(I)
            EFRICG(IX1(I)) = EFRICG(IX1(I)) + H1(I)*EFRIC_LM
            EFRICG(IX2(I)) = EFRICG(IX2(I)) + H2(I)*EFRIC_LM
            EFRICG(IX3(I)) = EFRICG(IX3(I)) + H3(I)*EFRIC_LM
            EFRICG(IX4(I)) = EFRICG(IX4(I)) + H4(I)*EFRIC_LM
            JG = NSVG(I)
            EFRIC_LS = HALF*EFRIC_L(I)
            IF(JG>0) THEN
              EFRICG(JG) = EFRICG(JG) + EFRIC_LS
            ELSE ! node remote
              JG = -JG
              EFRICGFI(NIN)%P(JG)=EFRICGFI(NIN)%P(JG)+ EFRIC_LS
            ENDIF
          ENDDO
#include "lockoff.inc"
        END IF !(INCONV==1) THEN
      ENDIF
C---------------------------------
      IF(KDTINT==1)THEN
        IF(    (VISC/=ZERO)
     .    .AND.(IVIS2==0.OR.IVIS2==1))THEN
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
C        C(I)=2.*C(I)
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
              CFI   = CF(I)*ABS(H1(I))
              AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
              IF(AUX>ST1(I))THEN
                K1(I) =ZERO
                C1(I) =CFI
                ST1(I)=AUX
              ENDIF
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
              CFI   = CF(I)*ABS(H2(I))
              AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
              IF(AUX>ST2(I))THEN
                K2(I) =ZERO
                C2(I) =CFI
                ST2(I)=AUX
              ENDIF
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
              CFI   = CF(I)*ABS(H3(I))
              AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
              IF(AUX>ST3(I))THEN
                K3(I) =ZERO
                C3(I) =CFI
                ST3(I)=AUX
              ENDIF
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
            IF(VISCFFRIC(I)/=ZERO
     .        .AND.(IVIS2==0.OR.IVIS2==1))THEN
              IF(PENE(I) == ZERO)CYCLE
C        C(I)=2.*C(I)
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
                CFI   = CF(I)*ABS(H1(I))
                AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
                IF(AUX>ST1(I))THEN
                  K1(I) =ZERO
                  C1(I) =CFI
                  ST1(I)=AUX
                ENDIF
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
                CFI   = CF(I)*ABS(H2(I))
                AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
                IF(AUX>ST2(I))THEN
                  K2(I) =ZERO
                  C2(I) =CFI
                  ST2(I)=AUX
                ENDIF
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
                CFI   = CF(I)*ABS(H3(I))
                AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
                IF(AUX>ST3(I))THEN
                  K3(I) =ZERO
                  C3(I) =CFI
                  ST3(I)=AUX
                ENDIF
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
                CFI   = CF(I)*ABS(H4(I))
                AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
                IF(AUX>ST4(I))THEN
                  K4(I) =ZERO
                  C4(I) =CFI
                  ST4(I)=AUX
                ENDIF
              ENDIF
            ELSE
              IF(PENE(I) == ZERO)CYCLE
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
C=======================================================================
C---------------------------------
      ITAG = 0
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
!!
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
        PHI1(I) = ZERO
        PHI2(I) = ZERO
        PHI3(I) = ZERO
        PHI4(I) = ZERO
      ENDDO


      IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
        DTI=DT2T
        CALL I24SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3              NIN   ,NOINT ,MSKYI_SMS, ISKYI_SMS,NSMS  ,
     4              KT    ,C     ,CF   ,DTMINI,DTI  ,
     5              IRTSE ,NSNE  ,IS2SE,IS2PT ,T2MAIN_SMS  ,
     6              T2FAC_SMS)
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
C------------For /LOAD/PRESSURE tag nodes in contact-------------
      TAGIP(1:JLT) = 0
      IF(NINLOADP > 0) THEN
        DO K = KLOADPINTER(NIN)+1, KLOADPINTER(NIN+1)
          PP = LOADPINTER(K)
          PPL = LOADP_HYD_INTER(PP)
          DGAPLOAD = DGAPLOADINT(K)
          DO I=1,JLT
            GAPP= GAPV(I) + DGAPLOAD
            IF((PENE(I)/=ZERO).OR.(DIST(I) <= GAPP.AND.DIST(I) >= ZERO)) THEN
              TAGIP(I) = 1
              IF(PENE(I)==ZERO) THEN
                IX1(I) = IXX(I,1)
                IX2(I) = IXX(I,2)
                IX3(I) = IXX(I,3)
                IX4(I) = IXX(I,4)
              ENDIF
              TAGNCONT(PPL,IXX(I,1)) = 1
              TAGNCONT(PPL,IXX(I,2)) = 1
              TAGNCONT(PPL,IXX(I,3)) = 1
              TAGNCONT(PPL,IXX(I,4)) = 1
              JG = NSVG(I)
              IF(JG>0) THEN
C  SPMD : do same after reception of forces for remote nodes
                IF (JG <= NUMNOD) THEN
                  TAGNCONT(PPL,JG) = 1
                ELSE
                  IG = JG - NUMNOD
                  IF (IG > 0) THEN
                    IF (IS2SE(1,IG) > 0) THEN
                      IE = IS2SE(1,IG)
                    ELSEIF(IS2SE(2,IG) > 0) THEN
                      IE = IS2SE(2,IG)
                    END IF
                    DO J =1,4
                      TAGNCONT(PPL,IRTSE(J,IE)) = 1
                    END DO
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO

      ENDIF
C
C
C spmd : identification des noeuds interf. utiles a envoyer
      IF (NSPMD>1) THEN
ctmp+1 mic only
#include "mic_lockon.inc"
        DO I = 1,JLT
          NN = NSVG(I)
          IF(NN<0)THEN
C tag temporaire de NSVFI a -
            HH = H1(I)+H2(I)+H3(I)+H4(I)
            IF(HH /= ZERO.OR.TAGIP(I)==1)THEN
              IF(ISEDGE_FI(NIN)%P(-NN)==0)THEN
                NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
              ELSE
                J1=IRTSE_FI(NIN)%P(1,-NN)
                NSVFI(NIN)%P(J1) = -ABS(NSVFI(NIN)%P(J1))
                J1=IRTSE_FI(NIN)%P(2,-NN)
                NSVFI(NIN)%P(J1) = -ABS(NSVFI(NIN)%P(J1))
                J1=IRTSE_FI(NIN)%P(3,-NN)
                NSVFI(NIN)%P(J1) = -ABS(NSVFI(NIN)%P(J1))
                J1=IRTSE_FI(NIN)%P(4,-NN)
                NSVFI(NIN)%P(J1) = -ABS(NSVFI(NIN)%P(J1))
              ENDIF
            ENDIF
          ENDIF
        ENDDO
ctmp+1 mic only
#include "mic_lockoff.inc"
      ENDIF

C
      IF(IPARIT==3)THEN
        stop 789
      ELSEIF(IPARIT==0)THEN
        CALL I24ASS0(JLT  ,IX1    ,IX2  ,IX3  ,IX4    ,
     2              NSVG  ,H1     ,H2   ,H3   ,H4     ,STIF ,
     3              FX1   ,FY1    ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4              FX3   ,FY3    ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5              FXI   ,FYI    ,FZI  ,A    ,STIFN  ,NIN  ,
     6              INTTH ,PHI    ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7              PHI4  ,INTPLY ,IPLY ,INOD_PXFEM   ,
     8              IRTSE ,NSNE   ,IS2SE  ,IS2PT,JTASK )

      ELSE
        CALL I24ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF  ,
     3              FX1   ,FY1   ,FZ1  ,FX2  ,FY2  ,FZ2    ,
     4              FX3   ,FY3   ,FZ3  ,FX4  ,FY4  ,FZ4    ,
     5              FXI   ,FYI   ,FZI  ,FSKYI,ISKY ,NISKYFI,
     6              NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI ,PHI1,
     7              PHI2  ,PHI3 , PHI4 ,ITAB ,INTPLY,IPLY   ,
     8              INOD_PXFEM,IRTSE,NSNE,IS2SE,IS2PT,TAGIP )
      ENDIF
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT>0)THEN
        IF (INCONV==1) THEN
#include "lockon.inc"
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
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
              IF (JG >NUMNOD) THEN
                IG = JG - NUMNOD
                CALL I24FOR1_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                         IG      ,FXI(I)  ,FYI(I)  ,FZI(I)  ,FCONT   ,
     +                         -1      )
              ELSE
                FCONT(1,JG)=FCONT(1,JG)- FXI(I)
                FCONT(2,JG)=FCONT(2,JG)- FYI(I)
                FCONT(3,JG)=FCONT(3,JG)- FZI(I)
              ENDIF

            ELSE
!           On QA test ../miniqa/INTERF/INT_24/implicit_Int24_Ipen0/data/CONVI7TU_0001.rad 2 x 2 JG = -1
C           IF(ISEDGE_FI(NIN)%P(JG)==1)THEN
C            ELSE
C            ENDIF

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
                    IF(PENE(K) == ZERO)CYCLE
C attention aux signes pour le cumul des efforts
C a rendre conforme avec CFORC3
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
                      IF (JG >NUMNOD) THEN
                        IG = JG - NUMNOD
                        IF(SECFCUM(4,JG,I)==1.)THEN
                          CALL I24FOR1_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                                  IG      ,FXI(I)  ,FYI(I)  ,FZI(I)  ,SECFCUM(1,1,I),
     +                                   1      )
                        ENDIF
                      ELSE
                        IF(SECFCUM(4,JG,I)==1.)THEN
                          SECFCUM(1,JG,I)=SECFCUM(1,JG,I)+FXI(K)
                          SECFCUM(2,JG,I)=SECFCUM(2,JG,I)+FYI(K)
                          SECFCUM(3,JG,I)=SECFCUM(3,JG,I)+FZI(K)
                        ENDIF
                      ENDIF
                    ELSE
C Noeud Remote
                      JG=-JG
                      IF(ISEDGE_FI(NIN)%P(JG)==1)THEN
                        IF(SECFCUM(4,JG,I)==1.)THEN
                          CALL I24FOR1_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                         JG      ,FXI(I)  ,FYI(I)  ,FZI(I)  ,SECFCUM(1,1,I),
     +                          1      )
                        ENDIF
                      ELSE
                        IF(SECFCUM(4,JG,I)==1.)THEN
                          SECFCUM(1,JG,I)=SECFCUM(1,JG,I)+FXI(K)
                          SECFCUM(2,JG,I)=SECFCUM(2,JG,I)+FYI(K)
                          SECFCUM(3,JG,I)=SECFCUM(3,JG,I)+FZI(K)
                        ENDIF
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
          IF(PENE(I) == ZERO)CYCLE
C test modifie pour coherence avec communication SPMD (spmd_i7tools)
c        IF(FXI(I)/=ZERO.OR.FYI(I)/=ZERO.OR.FZI(I)/=ZERO)THEN
          JG = NSVG(I)
          IF(JG>0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
            ICONTACT(JG)=1
          ENDIF
          ICONTACT(IX1(I))=1
          ICONTACT(IX2(I))=1
          ICONTACT(IX3(I))=1
          ICONTACT(IX4(I))=1
        ENDDO
      ENDIF
C
      IF(IADM/=0)THEN
      END IF
      IF(IADM>=2)THEN
      END IF
C
      IF(IBCC==0) RETURN
C
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
        IBCM = IBCC / 8
        IBCS = IBCC - 8 * IBCM
        IF(IBCS>0) THEN
          IG=NSVG(I)
C---------obsolate option, no need to update
          IF(IG>0.AND.IG<=NUMNOD) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
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
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I24ASS2                       source/interfaces/int24/i24for3.F
Chd|-- called by -----------
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I24FORC_FIC                   source/interfaces/int24/i24for3e.F
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24ASS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  FX1   ,FY1   ,FZ1  ,FX2  ,FY2   ,FZ2    ,
     4                  FX3   ,FY3   ,FZ3  ,FX4  ,FY4   ,FZ4    ,
     5                  FXI   ,FYI   ,FZI  ,FSKYI,ISKY  ,NISKYFI,
     6                  NIN   ,NOINT ,INTTH,PHI  ,FTHESKYI,PHI1,
     7                  PHI2  ,PHI3 , PHI4 ,ITAB,INTPLY ,IPLY  ,
     8                  INOD  ,IRTSE ,NSNE  ,IS2SE  ,IS2PT,TAGIP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE PLYXFEM_MOD
      USE DEBUG_MOD
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
#include      "parit_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NISKYFI,NIN,NOINT,INTTH,
     .        ISKY(*),ITAB(*),
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .        INTPLY,IPLY(4,*),INOD(*),
     .        IRTSE(5,*),NSNE ,IS2SE(2,*),IS2PT(*)
      INTEGER , INTENT(INOUT) :: TAGIP(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    FSKYI(LSKYI,NFSKYI),FTHESKYI(LSKYI),PHI(MVSIZ),
     .    PHI1(*),PHI2(*)  ,PHI3(*) ,PHI4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    HH(MVSIZ),FICI(5),FICS(4,4),FICSTH(4,5)
      INTEGER I, J1, IG, NISKYL1, NISKYL,IGP,IGM,IDR,NISKYFIL,J
      INTEGER ITG,NN,ILY,JG,IXSS(4),NFIC
      INTEGER se
C
      NISKYL1 = 0
      TAGIP(1:JLT) = 0
      DO I = 1, JLT
        HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
        IF(HH(I)==ZERO.AND.TAGIP(I)==0)CYCLE
        IF (H1(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
        IF (H2(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
        IF (H3(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
        IF (H4(I)/=ZERO.OR.TAGIP(I)==1) NISKYL1 = NISKYL1 + 1
      ENDDO
C
C Precalcul impact locaux / remote
C
      IGP = 0
      IGM = 0
      DO I=1,JLT
        IF(HH(I)==ZERO.AND.TAGIP(I)==0)CYCLE
        IG =NSVG(I)
        IF(IG>0) THEN
          IGP = IGP+1
          IF (IG > NUMNOD) IGP = IGP+3
        ELSE
          IGM = IGM+1
          IF(ISEDGE_FI(NIN)%P(-IG)==1)IGM = IGM+3
        ENDIF
      ENDDO
C
#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIL = NISKYFI
      NISKYFI = NISKYFI + IGM
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM > NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      IF(INTTH == 0 ) THEN
        IF(INTPLY == 0 )THEN
          DO I=1,JLT
            IF (H1(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX1(I)
              FSKYI(NISKYL,2)=FY1(I)
              FSKYI(NISKYL,3)=FZ1(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
              ISKY(NISKYL) = IX1(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H2(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX2(I)
              FSKYI(NISKYL,2)=FY2(I)
              FSKYI(NISKYL,3)=FZ2(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
              ISKY(NISKYL) = IX2(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H3(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX3(I)
              FSKYI(NISKYL,2)=FY3(I)
              FSKYI(NISKYL,3)=FZ3(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
              ISKY(NISKYL) = IX3(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H4(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX4(I)
              FSKYI(NISKYL,2)=FY4(I)
              FSKYI(NISKYL,3)=FZ4(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
              ISKY(NISKYL) = IX4(I)
            ENDIF
          ENDDO
C
          DO I=1,JLT
            IF(HH(I)==ZERO.AND.TAGIP(I)==0)CYCLE
            IG =NSVG(I)
            IF(IG>0) THEN
              IF (IG >NUMNOD) THEN
                JG = IG - NUMNOD
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                CALL I24FORC_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                         JG      ,NFIC    ,FICI    ,FICS    ,IXSS    )
                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=-FICS(J,1)
                  FSKYI(NISKYL,2)=-FICS(J,2)
                  FSKYI(NISKYL,3)=-FICS(J,3)
                  FSKYI(NISKYL,4)= FICS(J,4)
                  ISKY(NISKYL) = J1
                END DO
              ELSE
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=-FXI(I)
                FSKYI(NISKYL,2)=-FYI(I)
                FSKYI(NISKYL,3)=-FZI(I)
                FSKYI(NISKYL,4)= STIF(I)
                ISKY(NISKYL) = IG
              END IF !(IG >NUMNOD) THEN
            ELSE
              IG = -IG
              IF(ISEDGE_FI(NIN)%P(IG)==1)THEN
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                CALL I24FORC_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   , IS2PT_FI(NIN)%P(1)  ,
     +                         IG      ,NFIC    ,FICI    ,FICS    ,IXSS    )

                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYFIL = NISKYFIL + 1
                  FSKYFI(NIN)%P(1,NISKYFIL)=-FICS(J,1)
                  FSKYFI(NIN)%P(2,NISKYFIL)=-FICS(J,2)
                  FSKYFI(NIN)%P(3,NISKYFIL)=-FICS(J,3)
                  FSKYFI(NIN)%P(4,NISKYFIL)= FICS(J,4)
                  ISKYFI(NIN)%P(NISKYFIL) = J1
                END DO

              ELSE
                NISKYFIL = NISKYFIL + 1
                FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
                FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
                FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
                FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
                ISKYFI(NIN)%P(NISKYFIL) = IG
              ENDIF
            ENDIF
          ENDDO
        ELSE
C with plyxfem formulation  s
          DO I=1,JLT
            IF (H1(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX1(I)
              FSKYI(NISKYL,2)=FY1(I)
              FSKYI(NISKYL,3)=FZ1(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
              ISKY(NISKYL) = IX1(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX1(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY1(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ1(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(1,I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H2(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX2(I)
              FSKYI(NISKYL,2)=FY2(I)
              FSKYI(NISKYL,3)=FZ2(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
              ISKY(NISKYL) = IX2(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX2(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY2(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ2(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(2,I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H3(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX3(I)
              FSKYI(NISKYL,2)=FY3(I)
              FSKYI(NISKYL,3)=FZ3(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
              ISKY(NISKYL) = IX3(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX3(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY3(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ3(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(3,I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H4(I)/=ZERO.OR.TAGIP(I)==1) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX4(I)
              FSKYI(NISKYL,2)=FY4(I)
              FSKYI(NISKYL,3)=FZ4(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
              ISKY(NISKYL) = IX4(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX4(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY4(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ4(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(4,I)
            ENDIF
          ENDDO
C
          DO I=1,JLT
            IF(HH(I)==ZERO.AND.TAGIP(I)==0)CYCLE
            IG =NSVG(I)
            IF(IG>0) THEN
              IF (IG >NUMNOD) THEN
                JG = IG - NUMNOD
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                CALL I24FORC_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                         JG      ,NFIC    ,FICI    ,FICS    ,IXSS    )
                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=-FICS(J,1)
                  FSKYI(NISKYL,2)=-FICS(J,2)
                  FSKYI(NISKYL,3)=-FICS(J,3)
                  FSKYI(NISKYL,4)= FICS(J,4)
                  ISKY(NISKYL) = J1
                END DO
              ELSE
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=-FXI(I)
                FSKYI(NISKYL,2)=-FYI(I)
                FSKYI(NISKYL,3)=-FZI(I)
                FSKYI(NISKYL,4)= STIF(I)
                ISKY(NISKYL) = IG
              END IF !(IG >NUMNOD) THEN
C
            ELSE
              IG = -IG
              IF(ISEDGE_FI(NIN)%P(IG)==1)THEN
                JG = IG
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                CALL I24FORC_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)    ,IS2PT_FI(NIN)%P(1),
     +                       JG      ,NFIC    ,FICI    ,FICS    ,IXSS    )
                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYFIL = NISKYFIL + 1
                  FSKYFI(NIN)%P(1,NISKYFIL)=-FICS(J,1)
                  FSKYFI(NIN)%P(2,NISKYFIL)=-FICS(J,2)
                  FSKYFI(NIN)%P(3,NISKYFIL)=-FICS(J,3)
                  FSKYFI(NIN)%P(4,NISKYFIL)= FICS(J,4)
                  ISKYFI(NIN)%P(NISKYFIL) = J1
                END DO
              ELSE
                NISKYFIL = NISKYFIL + 1
                FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
                FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
                FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
                FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
                ISKYFI(NIN)%P(NISKYFIL) = IG
              ENDIF
C  must be be done for second/plyxfem node if it must be considered  -----
            ENDIF
          ENDDO
        ENDIF
C Thermique
      ELSE
        IF(INTPLY == 0) THEN
          DO I=1,JLT
            IF (H1(I)/=0.) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX1(I)
              FSKYI(NISKYL,2)=FY1(I)
              FSKYI(NISKYL,3)=FZ1(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
              ISKY(NISKYL) = IX1(I)
              FTHESKYI(NISKYL) = PHI1(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H2(I)/=ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX2(I)
              FSKYI(NISKYL,2)=FY2(I)
              FSKYI(NISKYL,3)=FZ2(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
              ISKY(NISKYL) = IX2(I)
              FTHESKYI(NISKYL) = PHI2(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H3(I)/=ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX3(I)
              FSKYI(NISKYL,2)=FY3(I)
              FSKYI(NISKYL,3)=FZ3(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
              ISKY(NISKYL) = IX3(I)
              FTHESKYI(NISKYL) = PHI3(I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H4(I)/=ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX4(I)
              FSKYI(NISKYL,2)=FY4(I)
              FSKYI(NISKYL,3)=FZ4(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
              ISKY(NISKYL) = IX4(I)
              FTHESKYI(NISKYL) = PHI4(I)
            ENDIF
          ENDDO
C
          DO I=1,JLT
            IF(HH(I)==ZERO)CYCLE
            IG =NSVG(I)
            IF(IG>0) THEN
              IF (IG >NUMNOD) THEN
                JG = IG - NUMNOD
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                NFIC = NFIC + 1
                FICI(NFIC)=PHI(I)
                CALL I24FORC_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                         JG      ,NFIC    ,FICI    ,FICSTH  ,IXSS    )
                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=-FICSTH(J,1)
                  FSKYI(NISKYL,2)=-FICSTH(J,2)
                  FSKYI(NISKYL,3)=-FICSTH(J,3)
                  FSKYI(NISKYL,4)= FICSTH(J,4)
                  FTHESKYI(NISKYL)=FICSTH(J,5)
                  ISKY(NISKYL) = J1
                END DO
              ELSE
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=-FXI(I)
                FSKYI(NISKYL,2)=-FYI(I)
                FSKYI(NISKYL,3)=-FZI(I)
                FSKYI(NISKYL,4)= STIF(I)
                ISKY(NISKYL) = IG
                FTHESKYI(NISKYL)=PHI(I)
              END IF !(IG >NUMNOD) THEN
            ELSE
              IG = -IG
              IF(ISEDGE_FI(NIN)%P(IG)==1)THEN
                JG = IG
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                NFIC = NFIC + 1
                FICI(NFIC)=PHI(I)
                CALL I24FORC_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                         JG      ,NFIC    ,FICI    ,FICSTH  ,IXSS    )

                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYFIL = NISKYFIL + 1
                  FSKYFI(NIN)%P(1,NISKYFIL)=-FICSTH(J,1)
                  FSKYFI(NIN)%P(2,NISKYFIL)=-FICSTH(J,2)
                  FSKYFI(NIN)%P(3,NISKYFIL)=-FICSTH(J,3)
                  FSKYFI(NIN)%P(4,NISKYFIL)= FICSTH(J,4)
                  FTHESKYFI(NIN)%P(NISKYFIL)=FICSTH(J,5)
                  ISKY(NISKYL) = J1
                END DO
              ELSE
                NISKYFIL = NISKYFIL + 1
                FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
                FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
                FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
                FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
                ISKYFI(NIN)%P(NISKYFIL) = IG
                FTHESKYFI(NIN)%P(NISKYFIL)=PHI(I)
              ENDIF
            ENDIF
          ENDDO
C with plyxfem formulation
        ELSE
          DO I=1,JLT
            IF (H1(I)/=0.) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX1(I)
              FSKYI(NISKYL,2)=FY1(I)
              FSKYI(NISKYL,3)=FZ1(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
              ISKY(NISKYL) = IX1(I)
              FTHESKYI(NISKYL) = PHI1(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX1(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY1(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ1(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(1,I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H2(I)/=ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX2(I)
              FSKYI(NISKYL,2)=FY2(I)
              FSKYI(NISKYL,3)=FZ2(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
              ISKY(NISKYL) = IX2(I)
              FTHESKYI(NISKYL) = PHI2(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX2(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY2(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ2(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(2,I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H3(I)/=ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX3(I)
              FSKYI(NISKYL,2)=FY3(I)
              FSKYI(NISKYL,3)=FZ3(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
              ISKY(NISKYL) = IX3(I)
              FTHESKYI(NISKYL) = PHI3(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX3(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY3(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ3(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(3,I)
            ENDIF
          ENDDO
          DO I=1,JLT
            IF (H4(I)/=ZERO) THEN
              NISKYL = NISKYL + 1
              FSKYI(NISKYL,1)=FX4(I)
              FSKYI(NISKYL,2)=FY4(I)
              FSKYI(NISKYL,3)=FZ4(I)
              FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
              ISKY(NISKYL) = IX4(I)
              FTHESKYI(NISKYL) = PHI4(I)
C
              PLYSKYI%FSKYI(NISKYL,1)=FX4(I)
              PLYSKYI%FSKYI(NISKYL,2)=FY4(I)
              PLYSKYI%FSKYI(NISKYL,3)=FZ4(I)
              PLYSKYI%FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
              PLYSKYI%FSKYI(NISKYL,5)=IPLY(4,I)
            ENDIF
          ENDDO
C
          DO I=1,JLT
            IF(HH(I)==ZERO)CYCLE
            IG =NSVG(I)
            IF(IG>0) THEN
              IF (IG >NUMNOD) THEN
                JG = IG - NUMNOD
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                NFIC = NFIC + 1
                FICI(NFIC)=PHI(I)
                CALL I24FORC_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                         JG      ,NFIC    ,FICI    ,FICSTH  ,IXSS    )
                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYL = NISKYL + 1
                  FSKYI(NISKYL,1)=-FICSTH(J,1)
                  FSKYI(NISKYL,2)=-FICSTH(J,2)
                  FSKYI(NISKYL,3)=-FICSTH(J,3)
                  FSKYI(NISKYL,4)= FICSTH(J,4)
                  FTHESKYI(NISKYL)=FICSTH(J,5)
                  ISKY(NISKYL) = J1
                END DO
              ELSE
                NISKYL = NISKYL + 1
                FSKYI(NISKYL,1)=-FXI(I)
                FSKYI(NISKYL,2)=-FYI(I)
                FSKYI(NISKYL,3)=-FZI(I)
                FSKYI(NISKYL,4)= STIF(I)
                ISKY(NISKYL) = IG
                FTHESKYI(NISKYL)=PHI(I)
              END IF !(IG >NUMNOD)
CC the second node is not  plyxfem
cc            PLYSKYI(1)%FSKYI(NISKYL,1)=-FXI(I)
cc            PLYSKYI(1)%FSKYI(NISKYL,2)=-FYI(I)
cc            PLYSKYI(1)%FSKYI(NISKYL,3)=-FZI(I)
cc            PLYSKYI(I)%FSKYI(NISKYL,4)=STIF(I)
            ELSE
              IG = -IG
              IF(ISEDGE_FI(NIN)%P(IG)==1)THEN
                JG = IG
                NFIC=1
                FICI(NFIC)=FXI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FYI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=FZI(I)
                NFIC = NFIC + 1
                FICI(NFIC)=STIF(I)
                NFIC = NFIC + 1
                FICI(NFIC)=PHI(I)
                CALL I24FORC_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                         JG      ,NFIC    ,FICI    ,FICSTH  ,IXSS    )
                DO J = 1,4
                  J1 = IXSS(J)
                  NISKYFIL = NISKYFIL + 1
                  FSKYFI(NIN)%P(1,NISKYFIL)=-FICSTH(J,1)
                  FSKYFI(NIN)%P(2,NISKYFIL)=-FICSTH(J,2)
                  FSKYFI(NIN)%P(3,NISKYFIL)=-FICSTH(J,3)
                  FSKYFI(NIN)%P(4,NISKYFIL)= FICSTH(J,4)
                  FTHESKYFI(NIN)%P(NISKYFIL)=FICSTH(J,5)
                  ISKYFI(NIN)%P(NISKYFIL)  = J1
                END DO

              ELSE
                NISKYFIL = NISKYFIL + 1
                FSKYFI(NIN)%P(1,NISKYFIL)=-FXI(I)
                FSKYFI(NIN)%P(2,NISKYFIL)=-FYI(I)
                FSKYFI(NIN)%P(3,NISKYFIL)=-FZI(I)
                FSKYFI(NIN)%P(4,NISKYFIL)= STIF(I)
                ISKYFI(NIN)%P(NISKYFIL) = IG
                FTHESKYFI(NIN)%P(NISKYFIL)=PHI(I)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I24ASS0                       source/interfaces/int24/i24for3.F
Chd|-- called by -----------
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|        I24FORC_FIC                   source/interfaces/int24/i24for3e.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                  NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                  FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                  FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                  FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6                  INTTH ,PHI  ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7                  PHI4  ,INTPLY, IPLY,INOD,IRTSE ,NSNE  ,
     8                  IS2SE  ,IS2PT ,JTASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PLYXFEM_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN,INTTH,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .        INTPLY,IPLY(4,*),INOD(*),IRTSE(5,*),NSNE,IS2SE(2,*),
     .        IS2PT(*) ,JTASK
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    A(3,*),  STIFN(*),PHI(*), FTHE(*),
     .    PHI1(*), PHI2(*), PHI3(*), PHI4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    HH(MVSIZ),FICI(5),FICS(4,4),FICSTH(4,5)
      INTEGER I, J1, IG,ILY,NN,JG,IXSS(4),NFIC,J,ISHIFT,NODFI

C
      IF(INTTH == 0) THEN
        DO I=1,JLT
          HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
          IF(HH(I)==ZERO)CYCLE
          J1=IX1(I)
          A(1,J1)=A(1,J1)+FX1(I)
          A(2,J1)=A(2,J1)+FY1(I)
          A(3,J1)=A(3,J1)+FZ1(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
C
          J1=IX2(I)
          A(1,J1)=A(1,J1)+FX2(I)
          A(2,J1)=A(2,J1)+FY2(I)
          A(3,J1)=A(3,J1)+FZ2(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
C
          J1=IX3(I)
          A(1,J1)=A(1,J1)+FX3(I)
          A(2,J1)=A(2,J1)+FY3(I)
          A(3,J1)=A(3,J1)+FZ3(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
C
          J1=IX4(I)
          A(1,J1)=A(1,J1)+FX4(I)
          A(2,J1)=A(2,J1)+FY4(I)
          A(3,J1)=A(3,J1)+FZ4(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
        ENDDO
      ELSE
        DO I=1,JLT
          HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
          IF(HH(I)==ZERO)CYCLE
          J1=IX1(I)
          A(1,J1)=A(1,J1)+FX1(I)
          A(2,J1)=A(2,J1)+FY1(I)
          A(3,J1)=A(3,J1)+FZ1(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
          FTHE(J1) = FTHE(J1) + PHI1(I)
C
          J1=IX2(I)
          A(1,J1)=A(1,J1)+FX2(I)
          A(2,J1)=A(2,J1)+FY2(I)
          A(3,J1)=A(3,J1)+FZ2(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
          FTHE(J1) = FTHE(J1) + PHI2(I)
C
          J1=IX3(I)
          A(1,J1)=A(1,J1)+FX3(I)
          A(2,J1)=A(2,J1)+FY3(I)
          A(3,J1)=A(3,J1)+FZ3(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
          FTHE(J1) = FTHE(J1) + PHI3(I)
C
          J1=IX4(I)
          A(1,J1)=A(1,J1)+FX4(I)
          A(2,J1)=A(2,J1)+FY4(I)
          A(3,J1)=A(3,J1)+FZ4(I)
          STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
          FTHE(J1) = FTHE(J1) + PHI4(I)
        ENDDO
      ENDIF
      IF(INTPLY > 0) THEN
        DO I=1,JLT
          HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
          IF(HH(I)==ZERO)CYCLE
          J1=IX1(I)
          NN = INOD(J1)
          ILY = IPLY(1,I)
          IF(INOD(J1) > 0 .AND. ILY > 0 ) THEN
            PLY(ILY)%A(1,NN)=PLY(ILY)%A(1,NN) + FX1(I)
            PLY(ILY)%A(2,NN)=PLY(ILY)%A(2,NN) + FY1(I)
            PLY(ILY)%A(3,NN)=PLY(ILY)%A(3,NN) + FZ1(I)
            PLY(ILY)%A(4,NN)=PLY(ILY)%A(4,NN) + STIF(I)*ABS(H1(I))
          ENDIF
C
          J1=IX2(I)
          NN = INOD(J1)
          ILY = IPLY(2,I)
          IF(INOD(J1) > 0 .AND. ILY > 0) THEN
            PLY(ILY)%A(1,NN)=PLY(ILY)%A(1,NN) +FX2(I)
            PLY(ILY)%A(2,NN)=PLY(ILY)%A(2,NN) +FY2(I)
            PLY(ILY)%A(3,NN)=PLY(ILY)%A(3,NN) +FZ2(I)
            PLY(ILY)%A(4,NN)=PLY(ILY)%A(4,NN) + STIF(I)*ABS(H2(I))
          ENDIF
C
          J1=IX3(I)
          NN = INOD(J1)
          ILY = IPLY(3,I)
          IF(INOD(J1) > 0 .AND. ILY > 0) THEN
            PLY(ILY)%A(1,NN)=PLY(ILY)%A(1,NN) +FX3(I)
            PLY(ILY)%A(2,NN)=PLY(ILY)%A(2,NN) +FY3(I)
            PLY(ILY)%A(3,NN)=PLY(ILY)%A(3,NN) +FZ3(I)
            PLY(ILY)%A(4,NN)=PLY(ILY)%A(4,NN)  + STIF(I)*ABS(H3(I))
          ENDIF
C
          J1=IX4(I)
          NN = INOD(J1)
          ILY = IPLY(4,I)
          IF(INOD(J1) > 0 .AND. ILY > 0) THEN
            PLY(ILY)%A(1,NN)=PLY(ILY)%A(1,NN) +FX4(I)
            PLY(ILY)%A(2,NN)=PLY(ILY)%A(2,NN) +FY4(I)
            PLY(ILY)%A(3,NN)=PLY(ILY)%A(3,NN) +FZ4(I)
            PLY(ILY)%A(4,NN)=PLY(ILY)%A(4,NN) + STIF(I)*ABS(H4(I))
          ENDIF
        ENDDO
      ENDIF
C
      NODFI = NLSKYFI(NIN)
      ISHIFT = NODFI*(JTASK-1)
      IF(INTTH == 0 ) THEN
        DO I=1,JLT
          IF(HH(I)==ZERO)CYCLE
          IG=NSVG(I)
          IF(IG>0)THEN
            IF (IG >NUMNOD) THEN
              JG = IG - NUMNOD
              NFIC=1
              FICI(NFIC)=FXI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FYI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FZI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=STIF(I)
              CALL I24FORC_FIC(     3,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                       JG      ,NFIC    ,FICI    ,FICS    ,IXSS    )
              DO J = 1,4
                J1 = IXSS(J)
                A(1,J1)=A(1,J1)-FICS(J,1)
                A(2,J1)=A(2,J1)-FICS(J,2)
                A(3,J1)=A(3,J1)-FICS(J,3)
                STIFN(J1) = STIFN(J1) + FICS(J,4)
              END DO
            ELSE
              A(1,IG)=A(1,IG)-FXI(I)
              A(2,IG)=A(2,IG)-FYI(I)
              A(3,IG)=A(3,IG)-FZI(I)
              STIFN(IG) = STIFN(IG) + STIF(I)
            END IF !(IG >NUMNOD) THEN
          ELSE
            IG = -IG
            IF(ISEDGE_FI(NIN)%P(IG)==1)THEN
              JG = IG
              NFIC=1
              FICI(NFIC)=FXI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FYI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FZI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=STIF(I)
              CALL I24FORC_FIC(     3,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                      JG      ,NFIC    ,FICI    ,FICS    ,IXSS    )
              DO J = 1,4
                J1 = IXSS(J)
                AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)-FICS(J,1)
                AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)-FICS(J,2)
                AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)-FICS(J,3)
                STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT)  + FICS(J,4)
              END DO

            ELSE
              AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
              AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
              AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
              STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT) + STIF(I)
            ENDIF
          ENDIF
        ENDDO
C
      ELSE
        DO I=1,JLT
          IF(HH(I)==ZERO)CYCLE
          IG=NSVG(I)
          IF(IG>0)THEN
            IF (IG >NUMNOD) THEN
              JG = IG - NUMNOD
              NFIC=1
              FICI(NFIC)=FXI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FYI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FZI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=STIF(I)
              NFIC = NFIC + 1
              FICI(NFIC)=PHI(I)
              CALL I24FORC_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                       JG      ,NFIC    ,FICI    ,FICSTH  ,IXSS    )
              DO J = 1,4
                J1 = IXSS(J)
                A(1,J1)=A(1,J1)-FICSTH(J,1)
                A(2,J1)=A(2,J1)-FICSTH(J,2)
                A(3,J1)=A(3,J1)-FICSTH(J,3)
                STIFN(J1) = STIFN(J1) + FICSTH(J,4)
                FTHE(J1)=FTHE(J1) + FICSTH(J,5)
              END DO
            ELSE
              A(1,IG)=A(1,IG)-FXI(I)
              A(2,IG)=A(2,IG)-FYI(I)
              A(3,IG)=A(3,IG)-FZI(I)
              STIFN(IG) = STIFN(IG) + STIF(I)
              FTHE(IG)=FTHE(IG) + PHI(I)
            END IF !(IG >NUMNOD) THEN
          ELSE
            IG = -IG
            IF(ISEDGE_FI(NIN)%P(IG)==1)THEN
              JG = IG
              NFIC=1
              FICI(NFIC)=FXI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FYI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=FZI(I)
              NFIC = NFIC + 1
              FICI(NFIC)=STIF(I)
              NFIC = NFIC + 1
              FICI(NFIC)=PHI(I)
              CALL I24FORC_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   ,IS2PT_FI(NIN)%P(1)   ,
     +                       JG      ,NFIC    ,FICI    ,FICSTH  ,IXSS    )
              DO J = 1,4
                J1 = IXSS(J)
                AFI(NIN)%P(1,J1+ISHIFT)=AFI(NIN)%P(1,J1+ISHIFT)-FICSTH(J,1)
                AFI(NIN)%P(2,J1+ISHIFT)=AFI(NIN)%P(2,J1+ISHIFT)-FICSTH(J,2)
                AFI(NIN)%P(3,J1+ISHIFT)=AFI(NIN)%P(3,J1+ISHIFT)-FICSTH(J,3)

                STNFI(NIN)%P(J1+ISHIFT)=STNFI(NIN)%P(J1+ISHIFT) +  FICSTH(J,4)
                FTHEFI(NIN)%P(J1+ISHIFT)= FTHEFI(NIN)%P(J1+ISHIFT) + FICSTH(J,5)
              END DO

            ELSE
              AFI(NIN)%P(1,IG+ISHIFT)=AFI(NIN)%P(1,IG+ISHIFT)-FXI(I)
              AFI(NIN)%P(2,IG+ISHIFT)=AFI(NIN)%P(2,IG+ISHIFT)-FYI(I)
              AFI(NIN)%P(3,IG+ISHIFT)=AFI(NIN)%P(3,IG+ISHIFT)-FZI(I)
              STNFI(NIN)%P(IG+ISHIFT)=STNFI(NIN)%P(IG+ISHIFT) + STIF(I)
              FTHEFI(NIN)%P(IG+ISHIFT)= FTHEFI(NIN)%P(IG+ISHIFT) + PHI(I)
            ENDIF
          ENDIF
        ENDDO
      ENDIF

cc      IF(INTPLY > 0) THEN
cc        DO I=1,JLT
cc           IF(HH(I)==ZERO)CYCLE
cc           IG=NSVG(I)
cc           NN = INOD(IG)
cc           IF(IG>0 .AND. NN > 0)THEN
cc             ILY = IPLY(5,I)
cc             PLY(ILY)%A(1,NN) = PLY(ILY)%A(1,NN) - FXI(I)
cc             PLY(ILY)%A(2,NN) = PLY(ILY)%A(2,NN) - FYI(I)
cc             PLY(ILY)%A(3,NN) = PLY(ILY)%A(3,NN) - FZI(I)
cc             PLY(ILY)%A(4,NN) = PLY(ILY)%A(4,NN) + STIF(I)
cc           ELSE
C avoir l'aspect spmd
cc             IG = -IG
cc             AFI(NIN)%P(1,IG)=AFI(NIN)%P(1,IG)-FXI(I)
cc             AFI(NIN)%P(2,IG)=AFI(NIN)%P(2,IG)-FYI(I)
cc             AFI(NIN)%P(3,IG)=AFI(NIN)%P(3,IG)-FZI(I)
cc           ENDIF
cc         ENDDO
cc      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I24SMS2                       source/interfaces/int24/i24for3.F
Chd|-- called by -----------
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I24FORC_FIC                   source/interfaces/int24/i24for3e.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  NIN   ,NOINT ,MSKYI_SMS ,ISKYI_SMS,NSMS ,
     4                  KT    ,C     ,CF   ,DTMINI,DTI    ,
     5                  IRTSE ,NSNE  ,IS2SE,IS2PT ,T2MAIN_SMS,
     6                  T2FAC_SMS)
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
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NIN,NOINT,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .        NSMS(*), ISKYI_SMS(LSKYI_SMS,*),IRTSE(5,*),NSNE,IS2SE(2,*),
     .        IS2PT(*),T2MAIN_SMS(6,*)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    MSKYI_SMS(*), KT(MVSIZ), C(MVSIZ), CF(MVSIZ), DTMINI, DTI,T2FAC_SMS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, NISKYL1, NISKYL, NN,JG,IXSS(4),NFIC,J,NS,NL, K,KK,IE
      my_real
     .        HH(MVSIZ),MAS, DTS,DTM_INT,FICI,FICS(4),FAC

C-----------------------------------------------
C     IF contact on nodes of TYPE2 additional connections must be created (T2MAIN_SMS(1) > 1)
C-----------------------------------------------
C
      NISKYL1 = 0
C
      DO I=1,JLT

        HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
        IF(NSMS(I)==0.OR.STIF(I)==ZERO.OR.HH(I)==ZERO) CYCLE
C
        IG =NSVG(I)
        IF(IG > 0) THEN
          IF (IG > NUMNOD) THEN
            NL = 0
            NS = IG - NUMNOD
            FICI = ONE
            CALL I24FORC_FIC(3,IRTSE,NSNE,IS2SE,IS2PT,NS,1,FICI,FICS,IXSS)
            DO J=1,4
              NL = NL + T2MAIN_SMS(1,IXSS(J))*CEILING(FICS(J))
            ENDDO
          ELSE
            NL = T2MAIN_SMS(1,IG)
          ENDIF
        ELSE
          IF(ISEDGE_FI(NIN)%P(-IG)==1) THEN
            NL = 0
            FICI = ONE
            CALL I24FORC_FIC(3,IRTSE_FI(NIN)%P(1,1),NSNE,IS2SE_FI(NIN)%P(1,1), IS2PT_FI(NIN)%P(1),-IG,1,FICI,FICS,IXSS)
            DO J=1,4
              NL = NL + T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))*CEILING(FICS(J))
            ENDDO
          ELSE
            NL = T2MAIN_SMS_FI(NIN)%P(1,-IG)
          ENDIF
        ENDIF
C
        IF (H1(I)/=ZERO) NISKYL1 = NISKYL1 + NL*T2MAIN_SMS(1,IX1(I))
        IF (H2(I)/=ZERO) NISKYL1 = NISKYL1 + NL*T2MAIN_SMS(1,IX2(I))
        IF (H3(I)/=ZERO) NISKYL1 = NISKYL1 + NL*T2MAIN_SMS(1,IX3(I))
        IF (H4(I)/=ZERO) NISKYL1 = NISKYL1 + NL*T2MAIN_SMS(1,IX4(I))
C
      ENDDO
C
#include "lockon.inc"
      NISKYL     = NISKY_SMS
      NISKY_SMS  = NISKY_SMS + NISKYL1
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1 > LSKYI_SMS) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      IF (DTMINI>ZERO) THEN
        DTM_INT=DTMINI
      ELSE
        DTM_INT=DTMINS_INT
      ENDIF

      DO I=1,JLT
        IF(NSMS(I)==0.OR.STIF(I)==ZERO.OR.HH(I)==ZERO) CYCLE
C
        IF(NSMS(I)>0)THEN
          DTS = DTMINS/DTFACS
          DTI=MIN(DTI,DTMINS)
        ELSE
          DTS = DTM_INT/DTFACS_INT
          DTI=MIN(DTI,DTM_INT)
        END IF

        MAS= DTS * ( DTS * KT(I) + C(I) )
        MAS = HALF * MAX( MAS, DTS * CF(I) )
C
C-----------------------------------------------
C      FAC = 1.0 for all cases
C      FAC = 0.5 for type2 crossed connections - T2MAIN(i)*T2MAIN(j)=16
C-----------------------------------------------
C
        IG =NSVG(I)
        IF(IG >  0)THEN
          IF (IG > NUMNOD) THEN
C-----------------------------------------------
C-- Edge 2 Edge contact
C-----------------------------------------------
            NS = IG - NUMNOD
            FICI = MAS
            CALL I24FORC_FIC(3     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                       NS     ,1     ,FICI    ,FICS    ,IXSS    )
C
            DO J = 1, 4
              IF (FICS(J) > ZERO) THEN
                IF(H1(I)/=ZERO)THEN
                  FAC = T2FAC_SMS(IXSS(J))
                  IF (T2MAIN_SMS(1,IXSS(J))*T2MAIN_SMS(1,IX1(I))==16) FAC = HALF*MAX(T2FAC_SMS(IXSS(J)),T2FAC_SMS(IX1(I)))
                  DO K=1,T2MAIN_SMS(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX1(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H1(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX1(I))
                      ISKYI_SMS(NISKYL,3)=ISPMD+1
                    ENDDO
                  ENDDO
                ENDIF
                IF(H2(I)/=ZERO)THEN
                  FAC = T2FAC_SMS(IXSS(J))
                  IF (T2MAIN_SMS(1,IXSS(J))*T2MAIN_SMS(1,IX2(I))==16) FAC = HALF*MAX(T2FAC_SMS(IXSS(J)),T2FAC_SMS(IX2(I)))
                  DO K=1,T2MAIN_SMS(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX2(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H2(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX2(I))
                      ISKYI_SMS(NISKYL,3)=ISPMD+1
                    ENDDO
                  ENDDO
                ENDIF
                IF(H3(I)/=ZERO)THEN
                  FAC = T2FAC_SMS(IXSS(J))
                  IF (T2MAIN_SMS(1,IXSS(J))*T2MAIN_SMS(1,IX3(I))==16) FAC = HALF*MAX(T2FAC_SMS(IXSS(J)),T2FAC_SMS(IX3(I)))
                  DO K=1,T2MAIN_SMS(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX3(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H3(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX3(I))
                      ISKYI_SMS(NISKYL,3)=ISPMD+1
                    ENDDO
                  ENDDO
                ENDIF
                IF(H4(I)/=ZERO)THEN
                  FAC = T2FAC_SMS(IXSS(J))
                  IF (T2MAIN_SMS(1,IXSS(J))*T2MAIN_SMS(1,IX4(I))==16) FAC = HALF*MAX(T2FAC_SMS(IXSS(J)),T2FAC_SMS(IX4(I)))
                  DO K=1,T2MAIN_SMS(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX4(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H4(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX4(I))
                      ISKYI_SMS(NISKYL,3)=ISPMD+1
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
            END DO
          ELSE
C-----------------------------------------------
C-- Main / node contact
C-----------------------------------------------
            IF(H1(I)/=ZERO)THEN
              FAC = T2FAC_SMS(IG)
              IF (T2MAIN_SMS(1,IG)*T2MAIN_SMS(1,IX1(I))==16) FAC = HALF*MAX(T2FAC_SMS(IG),T2FAC_SMS(IX1(I)))
              DO K=1,T2MAIN_SMS(1,IG)
                DO KK=1,T2MAIN_SMS(1,IX1(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H1(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IG)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX1(I))
                  ISKYI_SMS(NISKYL,3)=ISPMD+1
                ENDDO
              ENDDO
            END IF
            IF(H2(I)/=ZERO)THEN
              FAC = T2FAC_SMS(IG)
              IF (T2MAIN_SMS(1,IG)*T2MAIN_SMS(1,IX2(I))==16) FAC = HALF*MAX(T2FAC_SMS(IG),T2FAC_SMS(IX2(I)))
              DO K=1,T2MAIN_SMS(1,IG)
                DO KK=1,T2MAIN_SMS(1,IX2(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H2(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IG)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX2(I))
                  ISKYI_SMS(NISKYL,3)=ISPMD+1
                ENDDO
              ENDDO
            END IF
            IF(H3(I)/=ZERO)THEN
              FAC = T2FAC_SMS(IG)
              IF (T2MAIN_SMS(1,IG)*T2MAIN_SMS(1,IX3(I))==16) FAC = HALF*MAX(T2FAC_SMS(IG),T2FAC_SMS(IX3(I)))
              DO K=1,T2MAIN_SMS(1,IG)
                DO KK=1,T2MAIN_SMS(1,IX3(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H3(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IG)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX3(I))
                  ISKYI_SMS(NISKYL,3)=ISPMD+1
                ENDDO
              ENDDO
            END IF
            IF(H4(I)/=ZERO)THEN
              FAC = T2FAC_SMS(IG)
              IF (T2MAIN_SMS(1,IG)*T2MAIN_SMS(1,IX4(I))==16) FAC = HALF*MAX(T2FAC_SMS(IG),T2FAC_SMS(IX4(I)))
              DO K=1,T2MAIN_SMS(1,IG)
                DO KK=1,T2MAIN_SMS(1,IX4(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H4(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS(K+1,IG)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX4(I))
                  ISKYI_SMS(NISKYL,3)=ISPMD+1
                ENDDO
              ENDDO
            END IF
          END IF !(IG > NUMNOD) THNE
C
C-----------------------------------------------
        ELSE
C-----------------------------------------------
C
          NN = -IG
          IF (ISEDGE_FI(NIN)%P(NN)==1)THEN
C-----------------------------------------------
C-- E2E remote contact
C-----------------------------------------------
            FICI = MAS
            CALL I24FORC_FIC(3     ,IRTSE_FI(NIN)%P(1,1)   ,NSNE    ,IS2SE_FI(NIN)%P(1,1)   , IS2PT_FI(NIN)%P(1)  ,
     +                       NN      ,1    ,FICI    ,FICS    ,IXSS    )
C
            DO J = 1, 4
              IF (FICS(J) > ZERO) THEN
                IF(H1(I)/=ZERO)THEN
                  FAC = T2FAC_SMS_FI(NIN)%P(IXSS(J))
                  IF (T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))*T2MAIN_SMS(1,IX1(I))==16)
     .               FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(IXSS(J)),T2FAC_SMS(IX1(I)))
                  DO K=1,T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX1(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H1(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX1(I))
                      ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(IXSS(J))
                    ENDDO
                  ENDDO
                END IF
                IF(H2(I)/=ZERO)THEN
                  FAC = T2FAC_SMS_FI(NIN)%P(IXSS(J))
                  IF (T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))*T2MAIN_SMS(1,IX2(I))==16)
     .              FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(IXSS(J)),T2FAC_SMS(IX2(I)))
                  DO K=1,T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX2(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H2(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX2(I))
                      ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(IXSS(J))
                    ENDDO
                  ENDDO
                END IF
                IF(H3(I)/=ZERO)THEN
                  FAC = T2FAC_SMS_FI(NIN)%P(IXSS(J))
                  IF (T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))*T2MAIN_SMS(1,IX3(I))==16)
     .               FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(IXSS(J)),T2FAC_SMS(IX3(I)))
                  DO K=1,T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX3(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H3(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX3(I))
                      ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(IXSS(J))
                    ENDDO
                  ENDDO
                END IF
                IF(H4(I)/=ZERO)THEN
                  FAC = T2FAC_SMS_FI(NIN)%P(IXSS(J))
                  IF (T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))*T2MAIN_SMS(1,IX4(I))==16)
     .               FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(IXSS(J)),T2FAC_SMS(IX4(I)))
                  DO K=1,T2MAIN_SMS_FI(NIN)%P(1,IXSS(J))
                    DO KK=1,T2MAIN_SMS(1,IX4(I))
                      NISKYL=NISKYL+1
                      MSKYI_SMS(NISKYL)=ABS(H4(I))*FICS(J)*FAC
                      ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,IXSS(J))
                      ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX4(I))
                      ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(IXSS(J))
                    ENDDO
                  ENDDO
                END IF
              ENDIF
            ENDDO
          ELSE
C-----------------------------------------------
C-- Main / node remote contact
C-----------------------------------------------
            IF(H1(I)/=ZERO)THEN
              FAC = T2FAC_SMS_FI(NIN)%P(NN)
              IF (T2MAIN_SMS_FI(NIN)%P(1,NN)*T2MAIN_SMS(1,IX1(I))==16)
     .           FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(NN),T2FAC_SMS(IX1(I)))
              DO K=1,T2MAIN_SMS_FI(NIN)%P(1,NN)
                DO KK=1,T2MAIN_SMS(1,IX1(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H1(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,NN)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX1(I))
                  ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
                ENDDO
              ENDDO
            END IF
            IF(H2(I)/=ZERO)THEN
              FAC = T2FAC_SMS_FI(NIN)%P(NN)
              IF (T2MAIN_SMS_FI(NIN)%P(1,NN)*T2MAIN_SMS(1,IX2(I))==16)
     .           FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(NN),T2FAC_SMS(IX2(I)))
              DO K=1,T2MAIN_SMS_FI(NIN)%P(1,NN)
                DO KK=1,T2MAIN_SMS(1,IX2(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H2(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,NN)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX2(I))
                  ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
                ENDDO
              ENDDO
            END IF
            IF(H3(I)/=ZERO)THEN
              FAC = T2FAC_SMS_FI(NIN)%P(NN)
              IF (T2MAIN_SMS_FI(NIN)%P(1,NN)*T2MAIN_SMS(1,IX3(I))==16)
     .           FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(NN),T2FAC_SMS(IX3(I)))
              DO K=1,T2MAIN_SMS_FI(NIN)%P(1,NN)
                DO KK=1,T2MAIN_SMS(1,IX3(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H3(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,NN)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX3(I))
                  ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
                ENDDO
              ENDDO
            END IF
            IF(H4(I)/=ZERO)THEN
              FAC = T2FAC_SMS_FI(NIN)%P(NN)
              IF (T2MAIN_SMS_FI(NIN)%P(1,NN)*T2MAIN_SMS(1,IX4(I))==16)
     .           FAC = HALF*MAX(T2FAC_SMS_FI(NIN)%P(NN),T2FAC_SMS(IX4(I)))
              DO K=1,T2MAIN_SMS_FI(NIN)%P(1,NN)
                DO KK=1,T2MAIN_SMS(1,IX4(I))
                  NISKYL=NISKYL+1
                  MSKYI_SMS(NISKYL)=ABS(H4(I))*MAS*FAC
                  ISKYI_SMS(NISKYL,1)=T2MAIN_SMS_FI(NIN)%P(K+1,NN)
                  ISKYI_SMS(NISKYL,2)=T2MAIN_SMS(KK+1,IX4(I))
                  ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
                ENDDO
              ENDDO
            END IF
          ENDIF
        END IF
      ENDDO
C
      RETURN
      END
C

