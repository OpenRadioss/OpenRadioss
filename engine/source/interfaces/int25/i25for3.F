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
Chd|  I25FOR3                       source/interfaces/int25/i25for3.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|        I25SMS2                       source/interfaces/int25/i25for3.F
Chd|        IBCOFF                        source/interfaces/interf/ibcoff.F
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25FOR3(JLT ,A  ,V       ,IBCC        ,ICODT     ,
     2    FSAV        ,MS        ,VISC    ,
     3    VISCF       ,NOINT     ,STFN    ,ITAB        ,CN_LOC    ,
     4    STIGLO      ,STIFN     ,STIF    ,INACTI      ,INDEX     ,
     5    N1          ,N2        ,N3      ,H1          ,H2        ,
     6    H3          ,H4        ,FCONT   ,PENE        ,NRTM      ,
     7    IX1         ,IX2       ,IX3     ,IX4         ,NSVG      ,
     8    IVIS2       ,NELTST    ,ITYPTST ,DT2T        ,
     A    KINET       ,NEWFRONT  ,ISECIN  ,NSTRF       ,SECFCUM   ,
     B    X           ,IRECT     ,CE_LOC  ,MFROT       ,IFQ       ,
     C    SECND_FR     ,ALPHA0    ,IBAG    ,ICONTACT    ,IRTLM     ,
     E    VISCN       ,VXI       ,VYI     ,VZI         ,MSI       ,
     F    KINI        ,NIN       ,NISUB   ,LISUB       ,ADDSUBS   ,
     G    ADDSUBM     ,LISUBS    ,LISUBM  ,INFLG_SUBS  ,INFLG_SUBM,
     H    FSAVSUB     ,ILAGM     ,ICURV   ,FNCONT      ,FTCONT    ,
     I    NSN         ,XX        ,YY      ,ZZ          ,
     J    XI          ,YI        ,ZI      ,ANGLMI      ,PADM      ,  
     K    IADM        ,RCURVI    ,RCONTACT ,ACONTACT   ,PCONTACT  ,
     N    MSKYI_SMS   ,ISKYI_SMS ,NSMS    ,CAND_N_N    ,PENE_OLD  ,
     O    STIF_OLD    ,MBINFLG   ,ILEV    ,IGSTI       ,KMIN      , 
     P    INTPLY      ,NM1       ,NM2     ,NM3         ,
     P    MSEGTYP     ,JTASK     ,ISENSINT  ,
     T    FSAVPARIT   ,H3D_DATA  ,FRICC  ,VISCFFRIC    ,FRIC_COEFS, 
     U    GAPV        ,VISCFLUID ,SIGMAXADH,VISCADHFACT, IF_ADH   , 
     V    AREAS       , BASE_ADH ,IORTHFRIC,FRIC_COEFS2,FRICC2    ,
     W    VISCFFRIC2  ,NFORTH   ,NFISOT    ,INDEXORTH  , INDEXISOT,
     B    DIR1        ,DIR2     ,APINCH    ,STIFPINCH  ,FNI       ,
     C    FX1         ,FY1      ,FZ1       ,FX2        ,FY2       ,
     D    FZ2         ,FX3      ,FY3       ,FZ3        ,FX4       ,
     E    FY4         ,FZ4      ,FXI       ,FYI        ,FZI       ,
     C    INTTH       ,DRAD     ,FHEATS    ,FHEATM     ,QFRIC     ,
     D    EFRICT      ,TAGNCONT ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,
     E    TYPSUB      ,NCFIT    )

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE PLYXFEM_MOD
      USE H3D_MOD
      USE PINCHTYPE_MOD
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
#include      "task_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,JLT,IBCC,IBCM,IBCS,IVIS2,INACTI,IBAG,NIN,
     .        NSN,IORTHFRIC,NFORTH ,NFISOT,
     .        ICODT(*), ITAB(*), KINET(*),IRTLM(4,NSN),
     .        MFROT, IFQ, NOINT,NEWFRONT,ISECIN, NSTRF(*),
     .        IRECT(4,*),ICONTACT(*),
     .        KINI(*),MBINFLG(*),ILEV,JTASK,
     .        ISET, IADM,INTTH,IFORM,NRTM,
     .        CAND_N_N(*),INTPLY,MSEGTYP(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        KLOADPINTER(NINTER+1),LOADPINTER((NINTER+1)*NLOADP_HYD),
     .        LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        CN_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ),
     .        NISUB, LISUB(*), ADDSUBS(*), ADDSUBM(*), LISUBS(*), LISUBM(*),
     .        INFLG_SUBS(*), INFLG_SUBM(*), ILAGM, ICURV(3), 
     .        ISKYI_SMS(*), NSMS(*),IGSTI,
     .        ISENSINT(*), IF_ADH(MVSIZ),
     .        INDEXORTH(MVSIZ)  ,INDEXISOT(MVSIZ),TYPSUB(*),NCFIT
      my_real
     .   STIGLO, X(3,*),
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   SECND_FR(6,*),ALPHA0,
     .   VISC,VISCF,VIS,DT2T,STFN(*),STIFN(*),
     .   FSAVSUB(NTHVKI,*), FNCONT(3,*),FTCONT(3,*),
     .   MSKYI_SMS(*),KMIN, VISCFLUID, SIGMAXADH, VISCADHFACT,
     .   APINCH(3,*),STIFPINCH(*)
      my_real
     .     FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .     FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .     FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .     FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .     LA(MVSIZ), LB(MVSIZ), LC(MVSIZ),STIF(MVSIZ),
     .     PENE(MVSIZ),
     .     SECFCUM(7,NUMNOD,NSECT),
     .     TMP(MVSIZ), 
     .     STIFSAV(MVSIZ), VISCN(*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     XX(MVSIZ,5),YY(MVSIZ,5),ZZ(MVSIZ,5),
     .     XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ),
     .     FSAVPARIT(NISUB+1,11,*),FRIC_COEFS(MVSIZ,10), FRICC(MVSIZ),
     .     VISCFFRIC(MVSIZ), GAPV(MVSIZ), AREAS(MVSIZ), BASE_ADH(MVSIZ),
     .     FRIC_COEFS2(MVSIZ,10),FRICC2(MVSIZ),VISCFFRIC2(MVSIZ), DIR1(MVSIZ,3),
     .     DIR2(MVSIZ,3),
     .     DRAD, FHEATS, FHEATM, QFRIC, EFRICT(MVSIZ)   
      my_real
     .     RCURVI(*), RCONTACT(*), ACONTACT(*),
     .     PCONTACT(*),PADM, ANGLMI(*),RSTIF,
     .     PENE_OLD(5,NSN),STIF_OLD(2,NSN)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2,N,ITAG,NN1,NN2,NN3,NN4,
     .        IGRN,ISS1,ISS2,IMS1,IMS2,PP,PPL,ITYPSUB
      my_real
     .   FXR(MVSIZ), FYR(MVSIZ), FZR(MVSIZ)
      my_real
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ), FF(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,DIST,RDIST,
     .   V2, FM2, DT1INV, VISCA, FAC,ALPHI,ALPHA,BETA,
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
     .   CSA ,SNA ,ALPHAF ,FTT1 ,FTT2 ,QFRICT,
     .   AN(NFORTH) ,BN(NFORTH) ,NEP1 ,NEP2 ,NEP ,C11 ,C22 ,ANS ,EP,SIGNC
      my_real
     .   PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),DPENE(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX, FX6(6,MVSIZ), FY6(6,MVSIZ), FZ6(6,MVSIZ),
     .   DTMINI,STIF0_IMP(MVSIZ),PENN,TINY,XMU2(MVSIZ)
      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB,ibug,ip
      INTEGER  ISEGTYP(MVSIZ),IXSS(4),NOD1,NOD2,NS
      my_real
     .   FSAVSUB1(24,NISUB),IMPX,IMPY,IMPZ,VNM(MVSIZ),SFAC,STMIN,
     .   FXS(4),FYS(4),FZS(4)
      my_real
     .    STIFADH, FACTOR  
C
      INTEGER BITGET
      EXTERNAL BITGET
C-----------------------------------------------
#include "sync_dir.inc"
      DTMINI=ZERO
      IF (IRESP.EQ.1) THEN
           PREC = FIVEEM4
           TINY = EM15
      ELSE
           PREC = EM10
           TINY = EM30
      ENDIF
      IF(DT1.GT.ZERO)THEN
        DT1INV = ONE/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO
      ECONTDT = ZERO
      QFRICT = ZERO
      DO I=1,JLT
        STIF0(I) = STIF(I)
      ENDDO
C-------------------------------------------
C     PENE Offset removed to i24dst3.F
C-------------------------------------------
C           
      DO I=1,JLT
        IF(PENE(I) == ZERO.AND.INTTH == 0)THEN
           H1(I) = ZERO
           H2(I) = ZERO
           H3(I) = ZERO
           H4(I) = ZERO
        ELSEIF(PENE(I) == ZERO)THEN
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
     .                   - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
          VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                   - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
          VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                   - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
          VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
        ENDIF
      ENDDO
C-------------------------------------------
C     stifness reduction to avoid positive 
C     force jump and energy generation
C-------------------------------------------
      DO I=1,JLT
C
        IF(PENE(I) == ZERO) CYCLE
C
        DPENE(I) = MAX(ZERO,-VN(I)*DT1)
        JG = NSVG(I)
        N  = CAND_N_N(I)
        IF(JG > 0)THEN
          IF(TT /= ZERO)THEN
            IF(PENE(I) > PENE_OLD(2,N) .AND. 
     .         DPENE(I) < PENE(I)-PENE_OLD(2,N))THEN
              STIF(I) = (STIF_OLD(2,N)*PENE_OLD(2,N)+ STIF(I)*DPENE(I))
     .                                /PENE(I)
            ELSE
              STIF(I) = STIF_OLD(2,N) 
            ENDIF
            PENE_OLD(1,N) = PENE(I)
            STIF_OLD(1,N) = STIF(I) 
          ELSE
           PENE_OLD(1,N) = MAX(PENE_OLD(1,N),PENE(I))
           STIF_OLD(1,N) = MAX(STIF_OLD(1,N),STIF(I)) 
          ENDIF
c            if(itab(nsvg(i))==7444)
c     .          print *,'i25for3 stif',itab(jg),pene(i),pene_old(2,n),pene(i)-pene_old(2,n),dpene(i),stif_old(2,n),stif(i)
        ELSE
          JG = -JG
          IF(TT /= ZERO)THEN
            IF(PENE(I) > PENE_OLDFI(NIN)%P(2,JG) .and. 
     .         DPENE(I) < PENE(I)-PENE_OLDFI(NIN)%P(2,JG))THEN
              STIF(I) = (STIF_OLDFI(NIN)%P(2,JG)*PENE_OLDFI(NIN)%P(2,JG)
     .                  + STIF(I)*DPENE(I)) / PENE(I)
            ELSE
              STIF(I) = STIF_OLDFI(NIN)%P(2,JG) 
            ENDIF
            PENE_OLDFI(NIN)%P(1,JG) = PENE(I)
            STIF_OLDFI(NIN)%P(1,JG) = STIF(I) 
          ELSE
            PENE_OLDFI(NIN)%P(1,JG) = 
     *                           MAX(PENE_OLDFI(NIN)%P(1,JG),PENE(I))
            STIF_OLDFI(NIN)%P(1,JG) = 
     *                           MAX(STIF_OLDFI(NIN)%P(1,JG),STIF(I)) 
          ENDIF
        ENDIF
c            if(nsvg(i)>0.and.3827132.or.
c     .       itab(ix1(i))==3827132.or.itab(ix2(i))==3827132.or.itab(ix3(i))==3827132.or.itab(ix4(i))==3827132)
c     .          print *,'i25for3 stif',STIF_OLD(2,N),pene_old(2,n),stif_old(1,n),pene_old(1,n),pene(i),pene_old(5,n),
c     .                  itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i))
      ENDDO 
C-------------------------------------------
        IF (NCFIT >0) THEN
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            STIF(I) = STIF0(I)
            IF(STIGLO.LE.ZERO) STIF(I) = HALF*STIF(I) 
            FNI(I)= -STIF(I) * PENE(I) 
          ENDDO
        ELSEIF(IVIS2.NE.-1)THEN ! NO ADHESION CASE
          DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            IF(STIGLO.LE.ZERO)THEN
              STIF(I) = HALF*STIF(I) 
            ELSEIF(STIF(I).NE.ZERO)THEN
              STIF(I) = STIGLO 
            ENDIF
            FNI(I)= -STIF(I) * PENE(I)
          ENDDO
        ELSE ! ADHESION CASE 
C BASE_ADH = base where the adhesion spring is connected
C IF_ADH   = 1 if adhesion spring exists else 0
C PENE     = measured from the outer boundary of adhesion zone (i.e. 2*(real_gap))
          DO I=1,JLT
            IF(PENE(I) == ZERO) CYCLE
            IF(STIGLO.LE.ZERO)THEN
              STIF(I) = HALF*STIF(I) 
            ELSEIF(STIF(I).NE.ZERO)THEN
              STIF(I) = STIGLO 
            ENDIF
            STIFADH = SIGMAXADH*AREAS(I)/MAX(EM30,BASE_ADH(I))
            IF(PENE(I) < BASE_ADH(I))THEN                      ! INSIDE ADHESION ZONE
              FNI(I) = STIFADH*IF_ADH(I)*(BASE_ADH(I)-PENE(I))
            ELSE                                               ! INSIDE PENETRATION ZONE
              FNI(I) = -STIF(I)*(PENE(I)-BASE_ADH(I))
            ENDIF
          ENDDO       
        ENDIF
C---------------------------------
C     Contact Energy (elastic)
C---------------------------------
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
        ECONTT = ECONTT+HALF*STIF(I)*PENE(I)**2
      END DO
C---------------------------------
C     DAMPING + FRIC
C---------------------------------         
      FF(1:JLT)=ZERO
C
      IF(VISC.NE.ZERO)THEN
        IF(IVIS2.EQ.6)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT
            VIS2(I) = TWO * STIF(I) * MSI(I)
          ENDDO
C---------------------------------
          IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
           DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF(I)  = FAC * VISC * VIS 
            STIF(I) = STIF0(I) + FF(I) * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF(I)   = FF(I) * VN(I)  
            FNI(I)  = FNI(I) + FF(I)
           ENDDO
          ELSE
           DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            C(I)= FAC * VISC * VIS 
            KT(I)= STIF0(I)
            STIF(I) = STIF0(I) + C(I) * DT1INV
            FF(I)   = C(I) * VN(I)
            FNI(I)  = FNI(I) + FF(I)
            CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDDO
          ENDIF
        ELSEIF(IVIS2.EQ.1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
          DO I=1,JLT
           IF(PENE(I) == ZERO)CYCLE
           MAS2  = MS(IX1(I))*H1(I)
     .           + MS(IX2(I))*H2(I)
     .           + MS(IX3(I))*H3(I)
     .           + MS(IX4(I))*H4(I)
           VIS2(I) = TWO* STIF(I) * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
          ENDDO
C---------------------------------
          IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
           DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF(I)  = FAC * VISC * VIS 
            STIF(I) = STIF0(I) + FF(I) * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF(I)   = FF(I) * VN(I)  
            FNI(I)  = FNI(I) + FF(I)
           ENDDO
          ELSE
           DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            C(I)= FAC * VISC * VIS 
            KT(I)= STIF0(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF(I) = C(I) * VN(I)
            FNI(I)  = FNI(I) + FF(I)
            CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDDO
          ENDIF
        ELSEIF(IVIS2.EQ.2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
          DO I=1,JLT
           IF(PENE(I) == ZERO)CYCLE
           VIS2(I) = TWO* STIF(I) * MSI(I)
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF(I)  = FAC * VISC * VIS 
           STIF(I) = STIF0(I) + TWO * FF(I) * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
           FF(I)   = FF(I) * VN(I)  
           FNI(I)  = FNI(I) + FF(I)
          ENDDO
        ELSEIF(IVIS2.EQ.3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
          DO I=1,JLT
           IF(PENE(I) == ZERO)CYCLE
           VIS2(I) = TWO * STIF(I) * MSI(I)
           FAC = STIF(I) / MAX(EM30,STIF(I))
           VIS = SQRT(VIS2(I)) 
           FF(I)  = FAC *  VISC * VIS 
           STIF(I) = STIF0(I) + TWO* FF(I) * DT1INV
           STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
           FF(I)   = FF(I) * VN(I)  
           FNI(I)  = FNI(I) + FF(I)
          ENDDO
        ELSEIF(IVIS2.EQ.4)THEN
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
        ELSEIF(IVIS2.EQ.5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
          DO I=1,JLT
           IF(PENE(I) == ZERO)CYCLE
           MAS2  = MS(IX1(I))*H1(I)
     .           + MS(IX2(I))*H2(I)
     .           + MS(IX3(I))*H3(I)
     .           + MS(IX4(I))*H4(I)
           VIS2(I) = TWO* STIF(I) * MSI(I)
           VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
           STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
           FF(I)   = MIN(ZERO,FF(I)-FNI(I))
           FNI(I)  = FNI(I)+FF(I)
          ENDDO
        ELSEIF(IVIS2.EQ.-1)THEN
C---------------------------------
C         ADHESION MODEL, NORMAL DAMPING WITH VISC + ADHESION SPRING
C---------------------------------
          DO I=1,JLT
           IF(PENE(I) == ZERO)CYCLE
           MAS2  = MS(IX1(I))*H1(I)
     .           + MS(IX2(I))*H2(I)
     .           + MS(IX3(I))*H3(I)
     .           + MS(IX4(I))*H4(I)
           VIS2(I) = TWO* STIF(I) * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
          ENDDO
C---------------------------------
          IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
           DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            STIFADH = SIGMAXADH*AREAS(I)/MAX(EM30,BASE_ADH(I))
            FF(I)  = FAC * VISC * VIS
            STIF(I) = STIF0(I) + FF(I) * DT1INV
            STIF(I) = MAX(STIF(I), STIFADH + FF(I) * DT1INV)                                 ! NORMAL DIRECTION
            STIF(I) = MAX(STIF(I),FAC*VISCADHFACT*VISCFLUID*TWO/GAPV(I)*AREAS(I)*DT1INV) ! TANGENTIAL DIRECTION
            FF(I)   = FF(I) * VN(I)  
            FNI(I)  = FNI(I) + FF(I)          
           ENDDO
          ELSE
           DO I=1,JLT
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I))
            STIFADH = SIGMAXADH*AREAS(I)/MAX(EM30,BASE_ADH(I))
            C(I)  = FAC * VISC * VIS
            KT(I) = STIF0(I)
            STIF(I) = STIF(I) + C(I) * DT1INV
            FF(I) = C(I) * VN(I)
            FNI(I)  = FNI(I) + FF(I)
            CF(I)   = FAC*VISCADHFACT*VISCFLUID*TWO/GAPV(I)*AREAS(I)                     ! TANGENTIAL DIRECTION
            STIF(I) = MAX(STIF(I), STIFADH + C(I) * DT1INV)                               ! NORMAL DIRECTION
            STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)                                          ! TANGENTIAL DIRECTION              
           ENDDO
          ENDIF
        ELSE
        ENDIF
      ELSE
        DO I=1,JLT

         IF(VISCFFRIC(I).NE.ZERO  ) THEN

          IF(IVIS2.EQ.6)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
            VIS2(I) = TWO * STIF(I) * MSI(I)
C---------------------------------
C---------------------------------
           IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
             IF(PENE(I) == ZERO)CYCLE
             FAC = STIF(I) / MAX(EM30,STIF(I))
             VIS = SQRT(VIS2(I)) 
             FF(I)  = FAC * VISC * VIS 
             STIF(I) = STIF0(I) + FF(I) * DT1INV
             STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
             FF(I)   = FF(I) * VN(I)  
             FNI(I)  = FNI(I) + FF(I)
           ELSE
             IF(PENE(I) == ZERO)CYCLE
             FAC = STIF(I) / MAX(EM30,STIF(I))
             VIS = SQRT(VIS2(I))
             C(I)= FAC * VISC * VIS 
             KT(I)= STIF0(I)
             STIF(I) = STIF0(I) + C(I) * DT1INV
             FF(I)   = C(I) * VN(I)
             FNI(I)  = FNI(I) + FF(I)
             CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
             STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDIF
         ELSEIF(IVIS2.EQ.1)THEN
C---------------------------------
C         VISC QUAD TYPE V227
C---------------------------------
            IF(PENE(I) == ZERO)CYCLE
            MAS2  = MS(IX1(I))*H1(I)
     .           + MS(IX2(I))*H2(I)
     .           + MS(IX3(I))*H3(I)
     .           + MS(IX4(I))*H4(I)
            VIS2(I) = TWO* STIF(I) * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
C---------------------------------
           IF(KDTINT.EQ.0.AND.(IDTMINS/=2.AND.IDTMINS_INT==0))THEN
             IF(PENE(I) == ZERO)CYCLE
             FAC = STIF(I) / MAX(EM30,STIF(I))
             VIS = SQRT(VIS2(I)) 
             FF(I)  = FAC * VISC * VIS 
             STIF(I) = STIF0(I) + FF(I) * DT1INV
             STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
             FF(I)   = FF(I) * VN(I)  
             FNI(I)  = FNI(I) + FF(I)
           ELSE
             IF(PENE(I) == ZERO)CYCLE
             FAC = STIF(I) / MAX(EM30,STIF(I))
             VIS = SQRT(VIS2(I))
             C(I)= FAC * VISC * VIS 
             KT(I)= STIF0(I)
             STIF(I) = STIF(I) + C(I) * DT1INV
             FF(I)   = C(I) * VN(I)
             FNI(I)  = FNI(I) + FF(I)
             CF(I)   = FAC*SQRT(VISCFFRIC(I))*VIS
             STIF(I) = MAX(STIF(I) ,CF(I)*DT1INV)
           ENDIF
         ELSEIF(IVIS2.EQ.2)THEN
C---------------------------------
C         VISC QUAD TYPE V23f(bug aero amerrissage)
C---------------------------------
            IF(PENE(I) == ZERO)CYCLE
            VIS2(I) = TWO* STIF(I) * MSI(I)
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF(I)  = FAC * VISC * VIS 
            STIF(I) = STIF0(I) + TWO * FF(I) * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF(I)   = FF(I) * VN(I)  
            FNI(I)  = FNI(I) + FF(I)
         ELSEIF(IVIS2.EQ.3)THEN
C---------------------------------
C         VISC QUAD = 0
C---------------------------------
            IF(PENE(I) == ZERO)CYCLE
            VIS2(I) = TWO * STIF(I) * MSI(I)
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS = SQRT(VIS2(I)) 
            FF(I)  = FAC *  VISC * VIS 
            STIF(I) = STIF0(I) + TWO* FF(I) * DT1INV
            STIF(I) = MAX(STIF(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
            FF(I)   = FF(I) * VN(I)  
            FNI(I)  = FNI(I) + FF(I)
         ELSEIF(IVIS2.EQ.4)THEN
C---------------------------------
C         VISC = 0 
C---------------------------------
            IF(PENE(I) == ZERO)CYCLE
            FAC = STIF(I) / MAX(EM30,STIF(I))
            VIS2(I) = TWO* STIF(I) * MSI(I)
            VIS = SQRT(VIS2(I)) 
            STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I))*VIS*DT1INV)
         ELSEIF(IVIS2.EQ.5)THEN
C---------------------------------
C         VISC = 2M/dt    => pour visc < 1 , stable : dt < 2M/visc = dt
C         M=M1*M2/M1+M2      pour visc = 1 , choc elastique
C                            pour visc = 0.5 , choc mou
C---------------------------------
            IF(PENE(I) == ZERO)CYCLE
            MAS2  = MS(IX1(I))*H1(I)
     .           + MS(IX2(I))*H2(I)
     .           + MS(IX3(I))*H3(I)
     .           + MS(IX4(I))*H4(I)
            VIS2(I) = TWO* STIF(I) * MSI(I)
            VIS = 2. * VISC * DT1INV * MSI(I) * MAS2 / 
     .          MAX(EM30,MSI(I)+MAS2)
            STIF(I) = MAX(STIF0(I) ,FAC*SQRT(VISCFFRIC(I)*VIS2(I))*DT1INV)
            FF(I)   = VIS * VN(I)  
            FF(I)   = MIN(ZERO,FF(I)-FNI(I))
            FNI(I)  = FNI(I)+FF(I)
         ELSE
         ENDIF

       
         ELSE 
cbb  initialisation du tableau VIS2 pour eviter des problemes
           VIS2(I) = ZERO
         ENDIF
        ENDDO
      ENDIF
C---------------------------------
C     Energy absorbed (damping)
C---------------------------------
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
        JG = NSVG(I)
        IF(JG > 0)THEN
          N  = CAND_N_N(I)
          ECONTDT        = ECONTDT+PENE_OLD(3,N)*VN(I)*DT1
          PENE_OLD(3,N) = HALF*FF(I)
          ECONTDT        = ECONTDT+PENE_OLD(3,N)*VN(I)*DT1
        ELSE
          JG = -JG
          ECONTDT                  = ECONTDT+PENE_OLDFI(NIN)%P(3,JG)*VN(I)*DT1
          PENE_OLDFI(NIN)%P(3,JG) = HALF*FF(I)
          ECONTDT                  = ECONTDT+PENE_OLDFI(NIN)%P(3,JG)*VN(I)*DT1
        END IF         
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
         IF (IMS2 > 0 ) THEN
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
         IF(ISENSINT(1)/=0) THEN
           IF (IMS2 >0 ) THEN
             FSAVPARIT(1,1,I) =  -FXI(I)
             FSAVPARIT(1,2,I) =  -FYI(I)
             FSAVPARIT(1,3,I) =  -FZI(I)
           ELSE
             FSAVPARIT(1,1,I) =  FXI(I)
             FSAVPARIT(1,2,I) =  FYI(I)
             FSAVPARIT(1,3,I) =  FZI(I)
           END IF
         ENDIF
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
       IF(ISENSINT(1)/=0) THEN
         FSAVPARIT(1,1,I) =  FXI(I)
         FSAVPARIT(1,2,I) =  FYI(I)
         FSAVPARIT(1,3,I) =  FZI(I)
       ENDIF
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
C---------------------------------
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR.H3D_DATA%MH3D.NE.0))THEN
         IF (INCONV.EQ.1) THEN
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
          IF(PENE(I) == ZERO)CYCLE
          NN = NSVG(I)
          IF(NN.GT.0)THEN
           IN=CN_LOC(I)

           IF (MSEGTYP(CE_LOC(I)) < 0) THEN
             IE= - MSEGTYP(CE_LOC(I))
           ELSE
             IE = CE_LOC(I)
           ENDIF
           IF(IE > NRTM) IE=IE-NRTM

           JJ  =ADDSUBS(IN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBS(IN+1))
             JSUB=LISUBS(JJ)

             ITYPSUB = TYPSUB(JSUB)
             IF(ITYPSUB == 1 ) THEN  ! Defining specific inter  

                ISS1 = BITGET(INFLG_SUBS(JJ),0)
                ISS2 = BITGET(INFLG_SUBS(JJ),1)
                IGRN = BITGET(INFLG_SUBS(JJ),2)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1))) 
                   IMS1 = BITGET(INFLG_SUBM(KK),0)
                   IMS2 = BITGET(INFLG_SUBM(KK),1)
                   IF(KSUB.EQ.JSUB)THEN
                     IF(.NOT.(((IMS1 == 1 .OR.  IMS2 == 1) .AND. IGRN == 1).OR.
     .                         (IMS1 == 1 .AND. ISS2 == 1).OR.
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
                          FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                       ELSE
                          FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  FZI(I)
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
        
             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

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
                   FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                   FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                   FSAVPARIT(JSUB+1,3,I) =  FZI(I)
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

                     FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                     FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                     FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)


                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0)THEN
                          FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                       ELSE
                          FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                          FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                          FSAVPARIT(JSUB+1,3,I) =  FZI(I)
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

          IF (MSEGTYP(CE_LOC(I)) < 0) THEN
             IE= - MSEGTYP(CE_LOC(I))
          ELSE
             IE = CE_LOC(I)
          ENDIF
          IF(IE > NRTM) IE=IE-NRTM

          KK  =ADDSUBM(IE)
          DO WHILE(KK.LT.ADDSUBM(IE+1))
             KSUB=LISUBM(KK)

             ITYPSUB = TYPSUB(KSUB)
             IF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

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
                   FSAVPARIT(KSUB+1,1,I) =  -FXI(I)
                   FSAVPARIT(KSUB+1,2,I) =  -FYI(I)
                   FSAVPARIT(KSUB+1,3,I) =  -FZI(I)
		        ENDIF


              ENDIF
              KK=KK+1
             ENDDO

         END DO
        IF(NSPMD.GT.1) THEN
C boucle separee sinon bug PGI en SMP
         DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          NN = NSVG(I)
          IF(NN.LT.0)THEN
           NN = -NN

           IF (MSEGTYP(CE_LOC(I)) < 0) THEN
             IE= - MSEGTYP(CE_LOC(I))
           ELSE
             IE = CE_LOC(I)
           ENDIF
           IF(IE > NRTM) IE=IE-NRTM

           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             ITYPSUB = TYPSUB(JSUB)
             IF(ITYPSUB == 1 ) THEN

                ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
                ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
                IGRN = BITGET(INFLG_SUBSFI(NIN)%P(JJ),2)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))  
                  IMS1 = BITGET(INFLG_SUBM(KK),0)
                  IMS2 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB.EQ.JSUB)THEN
                     IF(.NOT.(((IMS1 == 1 .OR.  IMS2 == 1) .AND. IGRN == 1).OR.
     .                        (IMS1 == 1 .AND. ISS2 == 1).OR.
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
                         FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                         FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                         FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                       ELSE
                         FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                         FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                         FSAVPARIT(JSUB+1,3,I) =  FZI(I)
                       END IF
                     ENDIF
C
                     FSAVSUB1(8,JSUB) =FSAVSUB1(8,JSUB) +ABS(IMPX)
                     FSAVSUB1(9,JSUB) =FSAVSUB1(9,JSUB) +ABS(IMPY)
                     FSAVSUB1(10,JSUB)=FSAVSUB1(10,JSUB)+ABS(IMPZ)
C
                   ENDIF
                   KK=KK+1
                   KSUB=LISUBM(KK) 
                ENDDO
                JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

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
                   FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                   FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                   FSAVPARIT(JSUB+1,3,I) =  FZI(I)
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
                         FSAVPARIT(JSUB+1,1,I) =  -FXI(I)
                         FSAVPARIT(JSUB+1,2,I) =  -FYI(I)
                         FSAVPARIT(JSUB+1,3,I) =  -FZI(I)
                       ELSE
                         FSAVPARIT(JSUB+1,1,I) =  FXI(I)
                         FSAVPARIT(JSUB+1,2,I) =  FYI(I)
                         FSAVPARIT(JSUB+1,3,I) =  FZI(I)
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
               IF(PENE(I) == ZERO)CYCLE
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

C---------------------------------
C       NEW FRICTION MODELS
C---------------------------------

C   Friction coefficient computation
C++++++++++++++++++++++++++++++++++++++++

      IF(IVIS2.NE.-1) THEN ! friction calculation not needed for adhesion case

       IF(IORTHFRIC == 0) THEN 
C++ Isotropic Friction

         IF (MFROT.EQ.0) THEN
C---      Coulomb friction
            DO I=1,JLT
               XMU(I) = FRICC(I)
            ENDDO
          ELSEIF (MFROT.EQ.1) THEN
C---      Viscous friction
            DO I=1,JLT
              IE=CE_LOC(I)
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                CYCLE 
              ENDIF
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
              AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P 
     .               +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*V2
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO
          ELSEIF(MFROT.EQ.2)THEN
C---        Loi Darmstad
            DO I=1,JLT
              IE=CE_LOC(I)
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                CYCLE 
              ENDIF
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
              AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) 
     .               + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .               + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .               + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
              XMU(I) = MAX(XMU(I),EM30)
            ENDDO
          ELSEIF (MFROT.EQ.3) THEN
C---        Loi Renard
            DO I=1,JLT
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                CYCLE 
              ENDIF
              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV = SQRT(MAX(EM30,V2))
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS(I,5)) THEN
                DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
                VV1 = VV / FRIC_COEFS(I,5)
                XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(TWO-VV1)
              ELSEIF(VV.GT.FRIC_COEFS(I,5).AND.VV.LT.FRIC_COEFS(I,6)) THEN
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
                 AN(K)=ONE/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=ONE/BN(K)   
               ENDIF
            ENDDO
          ELSEIF (MFROT.EQ.1) THEN
C---      Viscous friction
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                CYCLE 
              ENDIF
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
              AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
              P = -FNI(I)/AREA
              XMU(I) = FRICC(I) + (FRIC_COEFS(I,1) + FRIC_COEFS(I,4)*P ) * P 
     .               +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*V2

              XMU(I) = MAX(XMU(I),EM30)
            ENDDO

#include   "vectorize.inc"
            DO K=1,NFORTH ! Orthotropic friction couples
              I = INDEXORTH(K)
              IE=CE_LOC(I)
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                XMU2(I) = ZERO
                CYCLE 
              ENDIF


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
     .               +(FRIC_COEFS(I,2) + FRIC_COEFS(I,3)*P) * VV + FRIC_COEFS(I,5)*VV*VV

              V2 = VX(I)*DIR2(K,1) +VY(I)*DIR2(K,2)+VZ(I)*DIR2(K,3) 
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
                 AN(K)=ONE/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=ONE/BN(K)   
               ENDIF
             ENDDO

          ELSEIF(MFROT.EQ.2)THEN
C---        Loi Darmstad
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                CYCLE 
              ENDIF

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
              AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
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
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                XMU2(I) = ZERO
                CYCLE 
              ENDIF

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
     .               + FRIC_COEFS(I,1)*EXP(FRIC_COEFS(I,2)*VV)*P*P
     .               + FRIC_COEFS(I,3)*EXP(FRIC_COEFS(I,4)*VV)*P
     .               + FRIC_COEFS(I,5)*EXP(FRIC_COEFS(I,6)*VV)
c
              V2 = VX(I)*DIR2(K,1) +VY(I)*DIR2(K,2)+VZ(I)*DIR2(K,3)
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
                 AN(K)=ONE/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
                  XMU2(I) = EM30
                  DIR2(I,1:3) = ZERO
                  BN(K) = ZERO
               ELSE
                  BN(K)=XMU2(I)**2   
                  BN(K)=ONE/BN(K)   
               ENDIF
             ENDDO

          ELSEIF (MFROT.EQ.3) THEN
C---        Loi Renard
#include   "vectorize.inc"
            DO K=1,NFISOT  ! isotropic friction couples
              I = INDEXISOT(K)
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                CYCLE 
              ENDIF

              AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
              V2 = (VX(I) - N1(I)*AA)**2 
     .           + (VY(I) - N2(I)*AA)**2 
     .           + (VZ(I) - N3(I)*AA)**2
              VV = SQRT(MAX(EM30,V2))
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS(I,5)) THEN
                DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
                VV1 = VV / FRIC_COEFS(I,5)
                XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(TWO-VV1)
              ELSEIF(VV.GT.FRIC_COEFS(I,5).AND.VV.LT.FRIC_COEFS(I,6)) THEN
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
              IF(PENE(I) == 0) THEN
                XMU(I) = ZERO
                XMU2(I) = ZERO
                CYCLE 
              ENDIF

c
              V2 = VX(I)*DIR1(K,1) +VY(I)*DIR1(K,2)+VZ(I)*DIR1(K,3)
              VV  = MAX(EM30,V2)
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS(I,5)) THEN
                DMU = FRIC_COEFS(I,3)-FRIC_COEFS(I,1)
                VV1 = VV / FRIC_COEFS(I,5)
                XMU(I) = FRIC_COEFS(I,1)+ DMU*VV1*(TWO-VV1)
              ELSEIF(VV.GT.FRIC_COEFS(I,5).AND.VV.LT.FRIC_COEFS(I,6)) THEN
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
              IF(VV.GE.0.AND.VV.LE.FRIC_COEFS2(I,5)) THEN
                DMU = FRIC_COEFS2(I,3)-FRIC_COEFS2(I,1)
                VV1 = VV / FRIC_COEFS2(I,5)
                XMU2(I) = FRIC_COEFS2(I,1)+ DMU*VV1*(TWO-VV1)
              ELSEIF(VV.GT.FRIC_COEFS2(I,5).AND.VV.LT.FRIC_COEFS2(I,6)) THEN
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
               IF(XMU(I).LE.EM30) THEN
                 XMU(I) = EM30
                 DIR1(I,1:3) = ZERO
                 AN(K) = ZERO
               ELSE
                 AN(K)=XMU(I)**2  
                 AN(K)=ONE/AN(K)  
               ENDIF
               IF(XMU2(I).LE.EM30) THEN
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
      ENDIF
C------------------
C    TANGENT FORCE CALCULATION
C------------------
      FXT(1:JLT)=ZERO
      FYT(1:JLT)=ZERO
      FZT(1:JLT)=ZERO
C
      IF(IVIS2.EQ.-1)THEN ! ADHESION CASE
        DO I=1,JLT
          IF(PENE(I)==ZERO)CYCLE
          VNX = N1(I)*VN(I)
          VNY = N2(I)*VN(I)
          VNZ = N3(I)*VN(I)
          VX(I) = VX(I) - VNX
          VY(I) = VY(I) - VNY
          VZ(I) = VZ(I) - VNZ
C         SHEAR STRESS = VISCOSITY * DU/DY - NEWTONIAN SHEAR LAW 
          FACTOR = VISCADHFACT*VISCFLUID*TWO/GAPV(I)*AREAS(I)    
          FXT(I) = FACTOR*VX(I)
          FYT(I) = FACTOR*VY(I)
          FZT(I) = FACTOR*VZ(I)
C-------  total force and energy
          FXI(I) = FXI(I) + FXT(I)
          FYI(I) = FYI(I) + FYT(I)
          FZI(I) = FZI(I) + FZT(I)
          ECONTDT = ECONTDT + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I)) ! Tangential contact energy
        ENDDO
      ELSE  ! NO ADHESION CASE
        IF (IFQ /= 0) THEN
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
          IF (IFQ.EQ.13) THEN
              ALPHA = MAX(ONE,ALPHA0*DT12)
          ELSE
              ALPHA = ALPHA0
          ENDIF

          IF(IORTHFRIC ==0 ) THEN
C++ Isotropic Friction

            IF (INCONV.EQ.1) THEN
              DO I=1,JLT
                IF(PENE(I) == ZERO)CYCLE
                FX = STIF0(I)*VX(I)*DT12
                FY = STIF0(I)*VY(I)*DT12
                FZ = STIF0(I)*VZ(I)*DT12
                JG = NSVG(I)
                IF(JG.GT.0) THEN
                  N = CAND_N_N(I)
c             SECND_FR(4:6,N) is the old friction force
c              if(itab(jg)==31774.or.itab(jg)==6992.or.itab(jg)==7106)
c     .    print *,'slav_fr nat',itab(jg),SECND_FR(4:6,N),PENE_OLD(1:5,N),STIF_OLD(1:2,N)
                  FX = SECND_FR(4,N) + ALPHA*FX
                  FY = SECND_FR(5,N) + ALPHA*FY
                  FZ = SECND_FR(6,N) + ALPHA*FZ
                  FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                  FX = FX - FTN*N1(I)
                  FY = FY - FTN*N2(I)
                  FZ = FZ - FTN*N3(I)
                  FT = FX*FX + FY*FY + FZ*FZ
                  FT = MAX(FT,TINY)
                  FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                  BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))

                  FXT(I) = FX * BETA
                  FYT(I) = FY * BETA
                  FZT(I) = FZ * BETA
C
                  SECND_FR(1,N) = FXT(I)
                  SECND_FR(2,N) = FYT(I)
                  SECND_FR(3,N) = FZT(I)

C 
c            if(itab(jg)==7444)
c     .          print *,'i25for3 natif',itab(jg),pene(i),fxt(i),fyt(i),fzt(i),fxi(i),fyi(i),fzi(i)
                ELSE ! cas noeud remote en SPMD 
                  JG = -JG
c             SECND_FRFI(NIN)%P(4:6,JG) is the old friction force
c              if(itafi(nin)%p(jg)==31774.or.itafi(nin)%p(jg)==6992.or.itafi(nin)%p(jg)==7106)
c     .    print *,'slav_fr rem',itafi(nin)%p(jg),SECND_FRFI(NIN)%P(4:6,JG),PENE_OLDFI(nin)%p(1:5,JG),STIF_OLDFI(nin)%p(1:2,JG)
                  FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                  FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                  FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                  FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                  FX = FX - FTN*N1(I)
                  FY = FY - FTN*N2(I)
                  FZ = FZ - FTN*N3(I)
                  FT = FX*FX + FY*FY + FZ*FZ
                  FT = MAX(FT,TINY)
                  FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                  BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                  FXT(I) = FX * BETA
                  FYT(I) = FY * BETA
                  FZT(I) = FZ * BETA
C
                  SECND_FRFI(NIN)%P(1,JG) = FXT(I)
                  SECND_FRFI(NIN)%P(2,JG) = FYT(I)
                  SECND_FRFI(NIN)%P(3,JG) = FZT(I)
C 
c            if(itafi(nin)%p(jg)==27715)
c     .          print *,'i25for3 remote',itafi(nin)%p(jg),pene(i),fxt(i),fyt(i),fzt(i),fxi(i),fyi(i),fzi(i)
                ENDIF
C-------      total force
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
                ECONVT = ECONVT 
     .                 + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))

                IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                       (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                ENDIF

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
                IF(JG.GT.0) THEN
c             SECND_FR(1:3,N) is the old friction force
                  FX = SECND_FR(4,N) + ALPHA*FX
                  FY = SECND_FR(5,N) + ALPHA*FY
                  FZ = SECND_FR(6,N) + ALPHA*FZ
                  FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                  FX = FX - FTN*N1(I)
                  FY = FY - FTN*N2(I)
                  FZ = FZ - FTN*N3(I)
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

            IF (INCONV.EQ.1) THEN
#include   "vectorize.inc"
              DO K=1,NFISOT  ! isotropic friction couples
                I = INDEXISOT(K)
                IF(PENE(I) == ZERO)CYCLE
                FX = STIF0(I)*VX(I)*DT12
                FY = STIF0(I)*VY(I)*DT12
                FZ = STIF0(I)*VZ(I)*DT12
                JG = NSVG(I)
                IF(JG.GT.0) THEN
                  N = CAND_N_N(I)
c             SECND_FR(4:6,N) is the old friction force
c              if(itab(jg)==31774.or.itab(jg)==6992.or.itab(jg)==7106)
c     .    print *,'slav_fr nat',itab(jg),SECND_FR(4:6,N),PENE_OLD(1:5,N),STIF_OLD(1:2,N)
                  FX = SECND_FR(4,N) + ALPHA*FX
                  FY = SECND_FR(5,N) + ALPHA*FY
                  FZ = SECND_FR(6,N) + ALPHA*FZ
                  FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                  FX = FX - FTN*N1(I)
                  FY = FY - FTN*N2(I)
                  FZ = FZ - FTN*N3(I)
                  FT = FX*FX + FY*FY + FZ*FZ
                  FT = MAX(FT,TINY)
                  FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                  BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                  FXT(I) = FX * BETA
                  FYT(I) = FY * BETA
                  FZT(I) = FZ * BETA
C
                  SECND_FR(1,N) = FXT(I)
                  SECND_FR(2,N) = FYT(I)
                  SECND_FR(3,N) = FZT(I)
C 
c            if(itab(jg)==7444)
c     .          print *,'i25for3 natif',itab(jg),pene(i),fxt(i),fyt(i),fzt(i),fxi(i),fyi(i),fzi(i)
                ELSE ! cas noeud remote en SPMD 
                  JG = -JG
c             SECND_FRFI(NIN)%P(4:6,JG) is the old friction force
c              if(itafi(nin)%p(jg)==31774.or.itafi(nin)%p(jg)==6992.or.itafi(nin)%p(jg)==7106)
c     .    print *,'slav_fr rem',itafi(nin)%p(jg),SECND_FRFI(NIN)%P(4:6,JG),PENE_OLDFI(nin)%p(1:5,JG),STIF_OLDFI(nin)%p(1:2,JG)
                  FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                  FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                  FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
                  FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                  FX = FX - FTN*N1(I)
                  FY = FY - FTN*N2(I)
                  FZ = FZ - FTN*N3(I)
                  FT = FX*FX + FY*FY + FZ*FZ
                  FT = MAX(FT,TINY)
                  FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
                  BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
                  FXT(I) = FX * BETA
                  FYT(I) = FY * BETA
                  FZT(I) = FZ * BETA
C
                  SECND_FRFI(NIN)%P(1,JG) = FXT(I)
                  SECND_FRFI(NIN)%P(2,JG) = FYT(I)
                  SECND_FRFI(NIN)%P(3,JG) = FZT(I)
C 
c            if(itafi(nin)%p(jg)==27715)
c     .          print *,'i25for3 remote',itafi(nin)%p(jg),pene(i),fxt(i),fyt(i),fzt(i),fxi(i),fyi(i),fzi(i)
                ENDIF
C-------      total force
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
                ECONVT = ECONVT 
     .                 + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))

                IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                       (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                ENDIF

              ENDDO

#include   "vectorize.inc"
              DO K=1,NFORTH  ! Orthotropic friction couples
                I = INDEXORTH(K)
                IF(PENE(I) == ZERO)CYCLE
                FX = STIF0(I)*VX(I)*DT12
                FY = STIF0(I)*VY(I)*DT12
                FZ = STIF0(I)*VZ(I)*DT12
                JG = NSVG(I)
                IF(JG.GT.0) THEN
                  N = CAND_N_N(I)
c             SECND_FR(4:6,N) is the old friction force
c              if(itab(jg)==31774.or.itab(jg)==6992.or.itab(jg)==7106)
c     .    print *,'slav_fr nat',itab(jg),SECND_FR(4:6,N),PENE_OLD(1:5,N),STIF_OLD(1:2,N)
                  FX = SECND_FR(4,N) + ALPHA*FX
                  FY = SECND_FR(5,N) + ALPHA*FY
                  FZ = SECND_FR(6,N) + ALPHA*FZ
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
C
                  SECND_FR(1,N) = FXT(I)
                  SECND_FR(2,N) = FYT(I)
                  SECND_FR(3,N) = FZT(I)
C 
c            if(itab(jg)==7444)
c     .          print *,'i25for3 natif',itab(jg),pene(i),fxt(i),fyt(i),fzt(i),fxi(i),fyi(i),fzi(i)
                ELSE ! cas noeud remote en SPMD 
                  JG = -JG
c             SECND_FRFI(NIN)%P(4:6,JG) is the old friction force
c              if(itafi(nin)%p(jg)==31774.or.itafi(nin)%p(jg)==6992.or.itafi(nin)%p(jg)==7106)
c     .    print *,'slav_fr rem',itafi(nin)%p(jg),SECND_FRFI(NIN)%P(4:6,JG),PENE_OLDFI(nin)%p(1:5,JG),STIF_OLDFI(nin)%p(1:2,JG)
                  FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                  FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                  FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
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
C
                  SECND_FRFI(NIN)%P(1,JG) = FXT(I)
                  SECND_FRFI(NIN)%P(2,JG) = FYT(I)
                  SECND_FRFI(NIN)%P(3,JG) = FZT(I)
C 
c            if(itafi(nin)%p(jg)==27715)
c     .          print *,'i25for3 remote',itafi(nin)%p(jg),pene(i),fxt(i),fyt(i),fzt(i),fxi(i),fyi(i),fzi(i)
                ENDIF
C-------      total force
                FXI(I)=FXI(I) + FXT(I)
                FYI(I)=FYI(I) + FYT(I)
                FZI(I)=FZI(I) + FZT(I)
                ECONVT = ECONVT 
     .                 + DT1*(VX(I)*FXT(I)+VY(I)*FYT(I)+VZ(I)*FZT(I))
                IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                       (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                ENDIF

              ENDDO
C--------implicit non converge---         
            ELSE
#include   "vectorize.inc"
              DO K=1,NFISOT  ! isotropic friction couples
                I = INDEXISOT(K)
                IF(PENE(I) == ZERO)CYCLE
                FX = STIF0(I)*VX(I)*DT12
                FY = STIF0(I)*VY(I)*DT12
                FZ = STIF0(I)*VZ(I)*DT12
                JG = NSVG(I)
                N = CAND_N_N(I)
                IF(JG.GT.0) THEN
c             SECND_FR(1:3,N) is the old friction force
                  FX = SECND_FR(4,N) + ALPHA*FX
                  FY = SECND_FR(5,N) + ALPHA*FY
                  FZ = SECND_FR(6,N) + ALPHA*FZ
                  FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
                  FX = FX - FTN*N1(I)
                  FY = FY - FTN*N2(I)
                  FZ = FZ - FTN*N3(I)
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

                IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                       (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                ENDIF
              ENDDO

#include   "vectorize.inc"
              DO K=1,NFORTH  ! Orthotropic friction couples
                I = INDEXORTH(K)
                IF(PENE(I) == ZERO)CYCLE
                FX = STIF0(I)*VX(I)*DT12
                FY = STIF0(I)*VY(I)*DT12
                FZ = STIF0(I)*VZ(I)*DT12
                JG = NSVG(I)
                N = CAND_N_N(I)
                IF(JG.GT.0) THEN
c             SECND_FR(1:3,N) is the old friction force
                  FX = SECND_FR(4,N) + ALPHA*FX
                  FY = SECND_FR(5,N) + ALPHA*FY
                  FZ = SECND_FR(6,N) + ALPHA*FZ
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
                ELSE ! cas noeud remote en SPMD 
                  JG = -JG
                  FX = SECND_FRFI(NIN)%P(4,JG) + ALPHA*FX
                  FY = SECND_FRFI(NIN)%P(5,JG) + ALPHA*FY
                  FZ = SECND_FRFI(NIN)%P(6,JG) + ALPHA*FZ
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
                ENDIF
C            IFPEN(INDEX(I)) = 1
                IF( INTTH > 0 .AND.BETA.NE.ZERO) THEN
                   EFRICT(I) = (FX-FXT(I))*FXT(I) + (FY-FYT(I))*FYT(I) + 
     .                       (FZ-FZT(I))*FZT(I) 
                   EFRICT(I) = EFRICT(I)/STIF0(I) ! FRICTIONAL ENERGY 
                   QFRICT     =  QFRICT + EFRICT(I)             
                ENDIF
              ENDDO
            ENDIF



          ENDIF

        ENDIF
      ENDIF
C
C---------------------------------
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT.GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR.H3D_DATA%MH3D.NE.0))THEN
         IF (INCONV.EQ.1) THEN
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
      FSAV4 = ZERO
      FSAV5 = ZERO
      FSAV6 = ZERO
      FSAV12= ZERO
      FSAV13= ZERO
      FSAV14= ZERO
      FSAV15= ZERO
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
      FSAV(25) = FSAV(25) + (FHEATS+FHEATM)*QFRICT
      FSAV(26) = FSAV(26) + ECONTT
      FSAV(27) = FSAV(27) + ECONVT - (FHEATS+FHEATM)*QFRICT
      FSAV(28) = FSAV(28) + ECONTDT
#include "lockoff.inc"
C
      IF(ISENSINT(1)/=0) THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          FSAVPARIT(1,4,I) =  FXT(I)
          FSAVPARIT(1,5,I) =  FYT(I)
          FSAVPARIT(1,6,I) =  FZT(I)
        ENDDO
      ENDIF
C
C---------------------------------
C     SORTIES TH PAR SOUS INTERFACE
C---------------------------------
      IF(NISUB.NE.0)THEN
         DO I=1,JLT
          IF(PENE(I) == ZERO)CYCLE
          NN = NSVG(I)
          IF(NN.GT.0)THEN
           IN=CN_LOC(I)

           IF (MSEGTYP(CE_LOC(I)) < 0) THEN
             IE= - MSEGTYP(CE_LOC(I))
           ELSE
             IE = CE_LOC(I)
           ENDIF
           IF(IE > NRTM) IE=IE-NRTM

           JJ  =ADDSUBS(IN) ! Addresse du S
           KK  =ADDSUBM(IE) ! Addresse du M

           DO WHILE(JJ.LT.ADDSUBS(IN+1))
!             all sub interfaces of S           
             JSUB=LISUBS(JJ)
!            JSUB Croissant
             ITYPSUB = TYPSUB(JSUB)
             IF(ITYPSUB == 1 ) THEN
C         
C            Find if node is on Surface S1 S2 ou GRNOD
                ISS1 = BITGET(INFLG_SUBS(JJ),0)
                ISS2 = BITGET(INFLG_SUBS(JJ),1)
                IGRN = BITGET(INFLG_SUBS(JJ),2)
                KSUB=LISUBM(KK)  
                DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))   
                  IMS1 = BITGET(INFLG_SUBM(KK),0)
                  IMS2 = BITGET(INFLG_SUBM(KK),1)
                  IF(KSUB.EQ.JSUB)THEN
!                S and M candidates on the same sub_interface
                    IF(.NOT.(((IMS1 == 1 .OR.  IMS2 == 1) .AND. IGRN == 1).OR.
     .                      (IMS1 == 1 .AND. ISS2 == 1).OR.
     .                        (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                        KK=KK+1
                        KSUB=LISUBM(KK) 
                        CYCLE            
                    END IF
                    IMPX=FXT(I)*DT12
                    IMPY=FYT(I)*DT12
                    IMPZ=FZT(I)*DT12
Clm              main side :
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
                      FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                      FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                      FSAVPARIT(JSUB+1,6,I) =  FZT(I)
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

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

                IMPX=FXT(I)*DT12
                IMPY=FYT(I)*DT12
                IMPZ=FZT(I)*DT12
Clm              main side :
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
                  FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                  FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                  FSAVPARIT(JSUB+1,6,I) =  FZT(I)
                ENDIF
C
               FSAVSUB1(15,JSUB)= FSAVSUB1(15,JSUB)
     .                          +SQRT(IMPX*IMPX+IMPY*IMPY+IMPZ*IMPZ)
               FSAVSUB1(22,JSUB)=FSAVSUB1(22,JSUB)
     .                          +YP(I)*IMPZ-ZP(I)*IMPY
                FSAVSUB1(23,JSUB)=FSAVSUB1(23,JSUB)
     .                           +ZP(I)*IMPX-XP(I)*IMPZ
                FSAVSUB1(24,JSUB)=FSAVSUB1(24,JSUB)
     .                           +XP(I)*IMPY-YP(I)*IMPX

                JJ=JJ+1
  
             ELSEIF(ITYPSUB == 3 ) THEN   ! Inter =0 : collecting forces from all inter with 2 surfaces

C         
C            Find if node is on Surface S1 S2 ou GRNOD
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

                     IMPX=FXT(I)*DT12
                     IMPY=FYT(I)*DT12
                     IMPZ=FZT(I)*DT12

                     IF(IMS2 > 0)THEN
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
                     FSAVSUB1(12,JSUB)=FSAVSUB1(12,JSUB)+ABS(IMPX)
                     FSAVSUB1(13,JSUB)=FSAVSUB1(13,JSUB)+ABS(IMPY)
                     FSAVSUB1(14,JSUB)=FSAVSUB1(14,JSUB)+ABS(IMPZ)
C
                     IF(ISENSINT(JSUB+1)/=0) THEN
                       IF(IMS2 > 0)THEN
                         FSAVPARIT(JSUB+1,4,I) =  -FXT(I)
                         FSAVPARIT(JSUB+1,5,I) =  -FYT(I)
                         FSAVPARIT(JSUB+1,6,I) =  -FZT(I)
                       ELSE
                         FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                         FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                         FSAVPARIT(JSUB+1,6,I) =  FZT(I)
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


          IF (MSEGTYP(CE_LOC(I)) < 0) THEN
             IE= - MSEGTYP(CE_LOC(I))
          ELSE
             IE = CE_LOC(I)
          ENDIF
          IF(IE > NRTM) IE=IE-NRTM

          KK  =ADDSUBM(IE) ! Addresse du M
          DO WHILE(KK.LT.ADDSUBM(IE+1))
!             all sub interfaces of S           
             KSUB=LISUBM(KK)
!            KSUB Croissant
             ITYPSUB = TYPSUB(KSUB)
             IF(ITYPSUB == 2 ) THEN ! Inter =0 : collecting forces from all inter with only 1 surface
                IMPX=-FXT(I)*DT12
                IMPY=-FYT(I)*DT12
                IMPZ=-FZT(I)*DT12
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
                  FSAVPARIT(KSUB+1,4,I) =  -FXT(I)
                  FSAVPARIT(KSUB+1,5,I) =  -FYT(I)
                  FSAVPARIT(KSUB+1,6,I) =  -FZT(I)
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
          IF(PENE(I) == ZERO)CYCLE
          NN = NSVG(I)
          IF(NN.LT.0)THEN
           NN = -NN

           IF (MSEGTYP(CE_LOC(I)) < 0) THEN
             IE= - MSEGTYP(CE_LOC(I))
           ELSE
             IE = CE_LOC(I)
           ENDIF
           IF(IE > NRTM) IE=IE-NRTM

           JJ  =ADDSUBSFI(NIN)%P(NN)
           KK  =ADDSUBM(IE)
           DO WHILE(JJ.LT.ADDSUBSFI(NIN)%P(NN+1))
             JSUB=LISUBSFI(NIN)%P(JJ)
             ITYPSUB = TYPSUB(JSUB)
             IF(ITYPSUB == 1 ) THEN
               ISS1 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),0)
               ISS2 = BITGET(INFLG_SUBSFI(NIN)%P(JJ),1)
               IGRN = BITGET(INFLG_SUBSFI(NIN)%P(JJ),2)
               KSUB=LISUBM(KK)  
               DO WHILE((KSUB.LE.JSUB).AND.(KK.LT.ADDSUBM(IE+1)))     
                 IMS1 = BITGET(INFLG_SUBM(KK),0)
                 IMS2 = BITGET(INFLG_SUBM(KK),1)
                 IF(KSUB.EQ.JSUB)THEN
                   IF(.NOT.(((IMS1 == 1 .OR.  IMS2 == 1) .AND. IGRN == 1).OR.
     .                      (IMS1 == 1 .AND. ISS2 == 1).OR.
     .                      (IMS2 == 1 .AND. ISS1 == 1)))  THEN
                      KK=KK+1
                      KSUB=LISUBM(KK) 
                      CYCLE            
                   END IF
                   IMPX=FXT(I)*DT12
                   IMPY=FYT(I)*DT12
                   IMPZ=FZT(I)*DT12
Clm              main side :
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
                     FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                     FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                     FSAVPARIT(JSUB+1,6,I) =  FZT(I)
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
                   ENDIF
                   KK=KK+1
                   KSUB=LISUBM(KK) 
                ENDDO
                JJ=JJ+1

             ELSEIF(ITYPSUB == 2 ) THEN   ! Inter =0 : collecting forces from all inter with only 1 surface

                IMPX=FXT(I)*DT12
                IMPY=FYT(I)*DT12
                IMPZ=FZT(I)*DT12
Clm              main side :
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
                  FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                  FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                  FSAVPARIT(JSUB+1,6,I) =  FZT(I)
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
               IGRN = BITGET(INFLG_SUBSFI(NIN)%P(JJ),2)
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

                   IMPX=FXT(I)*DT12
                   IMPY=FYT(I)*DT12
                   IMPZ=FZT(I)*DT12
                   IF(IMS2 > 0)THEN
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
                    IF(IMS2 > 0)THEN
                         FSAVPARIT(JSUB+1,4,I) =  -FXT(I)
                         FSAVPARIT(JSUB+1,5,I) =  -FYT(I)
                         FSAVPARIT(JSUB+1,6,I) =  -FZT(I)
                     ELSE
                         FSAVPARIT(JSUB+1,4,I) =  FXT(I)
                         FSAVPARIT(JSUB+1,5,I) =  FYT(I)
                         FSAVPARIT(JSUB+1,6,I) =  FZT(I)
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
          ECONTV = ECONTV + ECONVT  ! Frictional Energy
          ECONT  = ECONT + ECONTT   ! Elatic Energy
          ECONTD = ECONTD + ECONTDT ! Damping Energy
          IF (INTTH > 0) THEN
            QFRIC = QFRIC + (FHEATS+FHEATM)*QFRICT    ! FRICTIONAL HEAT ADDED TO INTERNAL ENERGY
            ECONTV = ECONTV - (FHEATS+FHEATM)*QFRICT  ! FRICTIONAL HEAT REMOVED FROM CONTACT ENERGY
          ENDIF
        END IF !(INCONV.EQ.1) THEN
#include "lockoff.inc"
C---------------------------------
      IF(KDTINT.EQ.1)THEN
       IF(    (VISC.NE.ZERO)
     .   .AND.(IVIS2.EQ.6.OR.IVIS2.EQ.1))THEN
        DO I=1,JLT
         IF(PENE(I) == ZERO)CYCLE
C        C(I)=2.*C(I)
C
         IF(MSI(I).EQ.ZERO)THEN
          KS(I) =ZERO
          CS(I) =ZERO
          STV(I)=ZERO
         ELSE
          CX  = FOUR*C(I)*C(I)
          CY  = EIGHT*MSI(I)*KT(I)
          AUX   = SQRT(CX+CY)+TWO*C(I)
          STV(I)= KT(I)*AUX*AUX/MAX(CY,EM30)
          AUX   = TWO*CF(I)*CF(I)/MAX(MSI(I),EM20)
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
          CX   =FOUR*C1(I)*C1(I)
          CY   =EIGHT*MS(J1)*K1(I)
          AUX   = SQRT(CX+CY)+TWO*C1(I)
          ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H1(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
          CX   =FOUR*C2(I)*C2(I)
          CY   =EIGHT*MS(J1)*K2(I)
          AUX   = SQRT(CX+CY)+TWO*C2(I)
          ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H2(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
          CX   =FOUR*C3(I)*C3(I)
          CY   =EIGHT*MS(J1)*K3(I)
          AUX   = SQRT(CX+CY)+TWO*C3(I)
          ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H3(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
          CX   =FOUR*C4(I)*C4(I)
          CY   =EIGHT*MS(J1)*K4(I)
          AUX   = SQRT(CX+CY)+TWO*C4(I)
          ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
          CFI   = CF(I)*ABS(H4(I))
          AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
         IF(VISCFFRIC(I).NE.ZERO) THEN
          IF(PENE(I) == ZERO)CYCLE
C        C(I)=2.*C(I)
C
          IF(MSI(I).EQ.ZERO)THEN
           KS(I) =ZERO
           CS(I) =ZERO
           STV(I)=ZERO
          ELSE
           CX  = FOUR*C(I)*C(I)
           CY  = EIGHT*MSI(I)*KT(I)
           AUX   = SQRT(CX+CY)+TWO*C(I)
           STV(I)= KT(I)*AUX*AUX/MAX(CY,EM30)
           AUX   = TWO*CF(I)*CF(I)/MAX(MSI(I),EM20)
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
           CX   =FOUR*C1(I)*C1(I)
           CY   =EIGHT*MS(J1)*K1(I)
           AUX   = SQRT(CX+CY)+TWO*C1(I)
           ST1(I)= K1(I)*AUX*AUX/MAX(CY,EM30)
           CFI   = CF(I)*ABS(H1(I))
           AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
           CX   =FOUR*C2(I)*C2(I)
           CY   =EIGHT*MS(J1)*K2(I)
           AUX   = SQRT(CX+CY)+TWO*C2(I)
           ST2(I)= K2(I)*AUX*AUX/MAX(CY,EM30)
           CFI   = CF(I)*ABS(H2(I))
           AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
           CX   =FOUR*C3(I)*C3(I)
           CY   =EIGHT*MS(J1)*K3(I)
           AUX   = SQRT(CX+CY)+TWO*C3(I)
           ST3(I)= K3(I)*AUX*AUX/MAX(CY,EM30)
           CFI   = CF(I)*ABS(H3(I))
           AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
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
           CX   =FOUR*C4(I)*C4(I)
           CY   =EIGHT*MS(J1)*K4(I)
           AUX   = SQRT(CX+CY)+TWO*C4(I)
           ST4(I)= K4(I)*AUX*AUX/MAX(CY,EM30)
           CFI   = CF(I)*ABS(H4(I))
           AUX   = TWO*CFI*CFI/MAX(MS(J1),EM20)
           IF(AUX.GT.ST4(I))THEN
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
      ENDDO
C
C
C spmd : identification des noeuds interf. utiles a envoyer
      IF (NSPMD.GT.1) THEN
ctmp+1 mic only
#include "mic_lockon.inc"
        DO I = 1,JLT
          NN = NSVG(I)
          IF(NN.LT.0 .AND. H1(I)+H2(I)+H3(I)+H4(I).NE.0)THEN
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
        CALL I25SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4  ,
     2              NSVG  ,H1    ,H2   ,H3   ,H4   ,STIF   ,
     3              NIN   ,NOINT ,MSKYI_SMS, ISKYI_SMS,NSMS  ,
     4              KT    ,C     ,CF   ,DTMINI,DTI  )
        IF(DTI.LT.DT2T)THEN
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



C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0)THEN
         IF (INCONV.EQ.1) THEN
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
            IF(JG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
              FCONT(1,JG)=FCONT(1,JG)- FXI(I)
              FCONT(2,JG)=FCONT(2,JG)- FYI(I)
              FCONT(3,JG)=FCONT(3,JG)- FZI(I)
            ENDIF
           ENDDO
#include "lockoff.inc"
         END IF !(INCONV.EQ.1) THEN
      ENDIF
C-----------------------------------------------------
      IF(ISECIN.GT.0.AND.INCONV.EQ.1)THEN
        K0=NSTRF(25)
        IF(NSTRF(1)+NSTRF(2).NE.0)THEN
          DO I=1,NSECT
           NBINTER=NSTRF(K0+14)
           K1S=K0+30
           DO J=1,NBINTER
            IF(NSTRF(K1S).EQ.NOINT)THEN
              IF(ISECUT.NE.0)THEN
#include "lockon.inc"
                DO K=1,JLT
                IF(PENE(K) == ZERO)CYCLE
C attention aux signes pour le cumul des efforts
C a rendre conforme avec CFORC3
                 IF(SECFCUM(4,IX1(K),I).EQ.1.)THEN
                  SECFCUM(1,IX1(K),I)=SECFCUM(1,IX1(K),I)-FX1(K)
                  SECFCUM(2,IX1(K),I)=SECFCUM(2,IX1(K),I)-FY1(K)
                  SECFCUM(3,IX1(K),I)=SECFCUM(3,IX1(K),I)-FZ1(K)
                ENDIF
                IF(SECFCUM(4,IX2(K),I).EQ.1.)THEN
                  SECFCUM(1,IX2(K),I)=SECFCUM(1,IX2(K),I)-FX2(K)
                  SECFCUM(2,IX2(K),I)=SECFCUM(2,IX2(K),I)-FY2(K)
                  SECFCUM(3,IX2(K),I)=SECFCUM(3,IX2(K),I)-FZ2(K)
                ENDIF
                IF(SECFCUM(4,IX3(K),I).EQ.1.)THEN
                  SECFCUM(1,IX3(K),I)=SECFCUM(1,IX3(K),I)-FX3(K)
                  SECFCUM(2,IX3(K),I)=SECFCUM(2,IX3(K),I)-FY3(K)
                  SECFCUM(3,IX3(K),I)=SECFCUM(3,IX3(K),I)-FZ3(K)
                ENDIF
                IF(SECFCUM(4,IX4(K),I).EQ.1.)THEN
                  SECFCUM(1,IX4(K),I)=SECFCUM(1,IX4(K),I)-FX4(K)
                  SECFCUM(2,IX4(K),I)=SECFCUM(2,IX4(K),I)-FY4(K)
                  SECFCUM(3,IX4(K),I)=SECFCUM(3,IX4(K),I)-FZ4(K)
                ENDIF
                JG = NSVG(K)
                IF(JG.GT.0) THEN
C en SPMD : traitement a refaire apres reception noeud remote si JG < 0
                  IF(SECFCUM(4,JG,I).EQ.1.)THEN
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
      IF(IBAG.NE.0.OR.IADM.NE.0)THEN
       DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
C test modifie pour coherence avec communication SPMD (spmd_i7tools)
c        IF(FXI(I).NE.ZERO.OR.FYI(I).NE.ZERO.OR.FZI(I).NE.ZERO)THEN
          JG = NSVG(I)
          IF(JG.GT.0) THEN
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
      IF(IADM.NE.0)THEN
      END IF
      IF(IADM.GE.2)THEN
      END IF
C
      IF(IBCC.EQ.0) RETURN
C
      DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
        IBCM = IBCC / 8
        IBCS = IBCC - 8 * IBCM
        IF(IBCS.GT.0) THEN
          IG=NSVG(I)
C---------obsolate option, no need to update          
          IF(IG.GT.0.AND.IG<=NUMNOD) THEN
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
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I25SMS2                       source/interfaces/int25/i25for3.F
Chd|-- called by -----------
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  NIN   ,NOINT ,MSKYI_SMS ,ISKYI_SMS,NSMS ,
     4                  KT    ,C     ,CF   ,DTMINI,DTI    )
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
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NIN,NOINT,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .        NSMS(*), ISKYI_SMS(LSKYI_SMS,*)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    MSKYI_SMS(*), KT(MVSIZ), C(MVSIZ), CF(MVSIZ), DTMINI, DTI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, NISKYL1, NISKYL, NN,JG,IXSS(4),NFIC,J,NS
      my_real
     .        HH(MVSIZ),MAS, DTS,DTM_INT,FICI,FICS(4)
C
C
      NISKYL1 = 0
      DO I=1,JLT
       HH(I)=H1(I)+H2(I)+H3(I)+H4(I)
       IF(NSMS(I)==0.OR.STIF(I)==ZERO.OR.HH(I)==ZERO) CYCLE
       IF (H1(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
       IF (H2(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
       IF (H3(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
       IF (H4(I).NE.ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
#include "lockon.inc"
      NISKYL     = NISKY_SMS
      NISKY_SMS  = NISKY_SMS + NISKYL1
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1 .GT. LSKYI_SMS) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
C
      IF (DTMINI.GT.ZERO) THEN 
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
       IG =NSVG(I)
       IF(IG >  0)THEN
         IF(H1(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H1(I))*MAS
          ISKYI_SMS(NISKYL,1)=IG
          ISKYI_SMS(NISKYL,2)=IX1(I)
          ISKYI_SMS(NISKYL,3)=ISPMD+1
         END IF
         IF(H2(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H2(I))*MAS
          ISKYI_SMS(NISKYL,1)=IG
          ISKYI_SMS(NISKYL,2)=IX2(I)
          ISKYI_SMS(NISKYL,3)=ISPMD+1
         END IF
         IF(H3(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H3(I))*MAS
          ISKYI_SMS(NISKYL,1)=IG
          ISKYI_SMS(NISKYL,2)=IX3(I)
          ISKYI_SMS(NISKYL,3)=ISPMD+1
         END IF
         IF(H4(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H4(I))*MAS
          ISKYI_SMS(NISKYL,1)=IG
          ISKYI_SMS(NISKYL,2)=IX4(I)
          ISKYI_SMS(NISKYL,3)=ISPMD+1
         END IF
       ELSE
         NN = -IG
         IF(H1(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H1(I))*MAS
          ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
          ISKYI_SMS(NISKYL,2)=IX1(I)
          ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
         END IF
         IF(H2(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H2(I))*MAS
          ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
          ISKYI_SMS(NISKYL,2)=IX2(I)
          ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
         END IF
         IF(H3(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H3(I))*MAS
          ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
          ISKYI_SMS(NISKYL,2)=IX3(I)
          ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
         END IF
         IF(H4(I).NE.ZERO)THEN
          NISKYL=NISKYL+1
          MSKYI_SMS(NISKYL)=ABS(H4(I))*MAS
          ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
          ISKYI_SMS(NISKYL,2)=IX4(I)
          ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
         END IF
       END IF
      ENDDO                
C      
      RETURN
      END
C
