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
Chd|  I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|-- called by -----------
Chd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|-- calls ---------------
Chd|        I25CDCOR3                     source/interfaces/int25/i25mainf.F
Chd|        I25COR3_21                    source/interfaces/int25/i25cor3.F
Chd|        I25COR3_22                    source/interfaces/int25/i25cor3.F
Chd|        I25DST3_21                    source/interfaces/int25/i25dst3_21.F
Chd|        I25DST3_22                    source/interfaces/int25/i25dst3_22.F
Chd|        I25GLOB                       source/interfaces/int25/i25dst3_1.F
Chd|        I25GLOB_22                    source/interfaces/int25/i25dst3_22.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COMP_2(
     1                   IPARI   ,INTBUF_TAB ,X      ,ITAB   ,NIN        ,
     2                   LINDMAX ,KINET      ,JTASK  ,NB_DST2,V          ,
     3                   ICODT   ,ISKEW      )
C=======================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE TRI7BOX
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN
      INTEGER IPARI(NPARI,NINTER),
     .        ITAB(*), KINET(*), ICODT(*), ISKEW(*)
      INTEGER NB_DST2,JTASK,LINDMAX
C     REAL
      my_real
     .   X(3,*), V(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JD(50),KD(50), JFI, KFI,IEDGE,
     .        I, J, L, H, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI,
     .        NB_LOC, I_STOK_LOC,DEBUT,
     .        ILAGM, LENR, INTTH,IFORM,INTPLY,
     .        NADMSR, I_STOK_GLO, MGLOB, MG, N, NSNR, NN, IERROR
      INTEGER  LENT, MAXCC
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), ISHEL(MVSIZ),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),SUBTRIA(MVSIZ),
     .        FAR(MVSIZ,4), KSLIDE(MVSIZ,4), MVOISN(MVSIZ,4),
     .        MVOISA(MVSIZ,4), MVOISB(MVSIZ,4), IBOUND(4,MVSIZ),
     .        IBOUNDA(4,MVSIZ), IBOUNDB(4,MVSIZ), ETYP(MVSIZ)
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX2
C     REAL
      my_real
     .   FRIC, GAP,
     .   VISC,VISCF,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,RSTIF,FHEAT,TINT,EPS
C-----------------------------------------------
C     REAL
      my_real
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .   NAX(MVSIZ,5), NAY(MVSIZ,5), NAZ(MVSIZ,5),
     .   NBX(MVSIZ,5), NBY(MVSIZ,5), NBZ(MVSIZ,5),
     .   XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ),
     .   GAPV(MVSIZ),MSI(MVSIZ),
     .   NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ),
     .   TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ), GAPS(MVSIZ), GAPM(MVSIZ),
     .   PENT(MVSIZ,4), LB(MVSIZ,4), LC(MVSIZ,4),
     .   LBH(MVSIZ,4), LCH(MVSIZ,4), GAP_NM(4,MVSIZ),
     .   DIST(MVSIZ), GAPMXL(MVSIZ)
      my_real
     .     RCURVI(MVSIZ), ANGLMI(MVSIZ), ANGLT, PADM,PENMIN,MARGE,DRAD,DGAPLOAD
      INTEGER NRTMFT, NRTMLT, NMNFT, NMNLT, NRADM
      INTEGER ICURV,ILEV
C-----------------------------------------------
C
      NRTM   =IPARI(4,NIN)
      NSN   =IPARI(5,NIN)
      NSNR  =IPARI(24,NIN)
      NTY   =IPARI(7,NIN)
      IBC   =IPARI(11,NIN)
      IVIS2 =IPARI(14,NIN)
      IF(IPARI(33,NIN)==1) RETURN
      NOINT =IPARI(15,NIN)
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      ISECIN=IPARI(28,NIN)
      MFROT =IPARI(30,NIN)
      IFQ =IPARI(31,NIN)
      IBAG =IPARI(32,NIN)
      IGSTI=IPARI(34,NIN)
      ICURV =IPARI(39,NIN)
      IEDGE =IPARI(58,NIN)
      NADMSR=IPARI(67,NIN)

C adaptive meshing
      IADM =IPARI(44,NIN)
      NRADM=IPARI(49,NIN)
      PADM =INTBUF_TAB%VARIABLES(24)
      ANGLT=INTBUF_TAB%VARIABLES(25)
      MARGE=INTBUF_TAB%VARIABLES(25)
C heat interface
      INTTH = IPARI(47,NIN)
      IFORM = IPARI(48,NIN)
      INTPLY = IPARI(66,NIN)
C      
      STIGLO=-INTBUF_TAB%STFAC(1)
C  
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      VISC  =INTBUF_TAB%VARIABLES(14)
      VISCF =INTBUF_TAB%VARIABLES(15)
C
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
      RSTIF   = INTBUF_TAB%VARIABLES(20)
      FHEAT   = INTBUF_TAB%VARIABLES(21)
      TINT    = INTBUF_TAB%VARIABLES(22)
      PENMIN  = INTBUF_TAB%VARIABLES(38)
      EPS     = INTBUF_TAB%VARIABLES(39)
      DRAD = ZERO
      IF(INTTH > 0) DRAD    = INTBUF_TAB%VARIABLES(32)
      DGAPLOAD    = INTBUF_TAB%VARIABLES(46)

      ILEV  = IPARI(20,NIN)
      ALLOCATE(INDEX2(LINDMAX))

c----------------------------------------------------
c   Rayon de courbure : calcul des normales nodales (normees)
C   IADM!=0 + Icurv!=0 non available (starter error).
c----------------------------------------------------
      IF(IADM/=0)THEN
      END IF!(IADM/=0)
C-----------------------------------------------------------------------
C
C     decoupage statique
C
      I_STOK_GLO = INTBUF_TAB%I_STOK(2)
C
      NB_LOC = I_STOK_GLO / NTHREAD
      IF (JTASK==NTHREAD) THEN
        I_STOK_LOC = I_STOK_GLO-NB_LOC*(NTHREAD-1)
      ELSE
        I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC

      I_STOK = 0
C
      DO I = JTASK, I_STOK_GLO, NTHREAD
        N = INTBUF_TAB%CAND_OPT_N(I)
        L = INTBUF_TAB%CAND_OPT_E(I)
        MG= INTBUF_TAB%MSEGLO(L)
        IF(N <= NSN)THEN
C       
C         candidates for sliding (IRTLM /= 0 and IRTLM /= MG) & non previously impacted (IRTLM == 0)
          IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0.AND.INTBUF_TAB%IRTLM(4*(N-1)+1)/=MG)THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
          ENDIF
        ELSE
          IF(IRTLM_FI(NIN)%P(1,N-NSN) > 0.AND.IRTLM_FI(NIN)%P(1,N-NSN)/=MG)THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
          ENDIF
        END IF
      ENDDO
C
      CALL MY_BARRIER
C
      IF (DEBUG(3)>=1) THEN
          NB_DST2 = NB_DST2 + I_STOK
      ENDIF

      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
	  CALL I25CDCOR3(
     1     JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_OPT_E,INTBUF_TAB%CAND_OPT_N,
     2     CAND_E_N,CAND_N_N )
C cand_n et cand_e remplace par cand_n_n et cand_e_n
          CALL I25COR3_21(
     1  JLT       ,X          ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV  ,CAND_E_N  ,
     2  CAND_N_N  ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS,STIF      ,IGAP       ,
     3  XI         ,YI          ,ZI         ,IX1       ,IX2                 ,
     4  IX3        ,IX4         ,NSVG       ,NSN       ,INTBUF_TAB%MSEGTYP24,
     5  ETYP       ,NIN        ,INTBUF_TAB%GAP_S,GAPS  ,INTBUF_TAB%ADMSR    ,
     6  INTBUF_TAB%EDGE_BISECTOR,XX ,YY         ,ZZ    ,NNX                 ,
     7  NNY        ,NNZ       ,INTBUF_TAB%GAP_M ,GAPM  ,INTBUF_TAB%GAP_NM   ,
     8  GAP_NM     ,INTBUF_TAB%ISLIDE,KSLIDE    ,INTBUF_TAB%MVOISIN,MVOISN  ,
     9  INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,GAPMXL,INTBUF_TAB%LBOUND,IBOUND )
C
          CALL I25DST3_21(
     1 JLT         ,CAND_N_N    ,CAND_E_N    ,NRTM        ,XX          ,
     2 YY          ,ZZ          ,XI          ,YI          ,ZI          ,
     3 NIN         ,NSN         ,IX1         ,IX2         ,IX3         ,
     4 IX4         ,NSVG        ,STIF        ,INACTI      ,INTBUF_TAB%MSEGLO,
     5 GAPS        ,GAPM        ,INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S,
     6 GAP_NM      ,ITAB        ,NNX         ,NNY         ,NNZ        ,
     7 FAR         ,PENT        ,DIST        ,LB          ,LC         ,
     8 LBH         ,LCH         ,KSLIDE      ,MVOISN      ,GAPMXL     ,
     9 IBOUND      ,INTBUF_TAB%VTX_BISECTOR,ETYP ,ICODT  ,ISKEW      ,
     A DRAD        ,DGAPLOAD  )
C
          CALL I25GLOB(
     1 JLT    ,CAND_N_N ,CAND_E_N ,
     2 NIN    ,NSN      ,IX1      ,IX2    ,IX3    ,
     3 IX4    ,NSVG     ,STIF     ,INACTI ,INTBUF_TAB%MSEGLO ,
     4 INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S ,ITAB   ,
     5 FAR    ,PENT     ,LBH    ,LCH    ,
C    5 FAR    ,PENT     ,LB     ,LC     ,
     6 INDEX2(NFT+1) ,INTBUF_TAB%FARM ,INTBUF_TAB%PENM ,
     .                                                  INTBUF_TAB%LBM   ,
     7 INTBUF_TAB%LCM )
C
      ENDDO
C-----------------------------------------------------------------------

      I_STOK = 0
C
      DO I = JTASK, I_STOK_GLO, NTHREAD
        N = INTBUF_TAB%CAND_OPT_N(I)
        L = INTBUF_TAB%CAND_OPT_E(I)
        MG= INTBUF_TAB%MSEGLO(L)
        IF(N <= NSN)THEN
C       
C         candidates for sliding (IRTLM /= 0 and IRTLM /= MG) & non previously impacted (IRTLM == 0)
          IF(INTBUF_TAB%IRTLM(4*(N-1)+1)<=0)THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
          ENDIF
        ELSE
          IF(IRTLM_FI(NIN)%P(1,N-NSN)<=0)THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
          ENDIF
        END IF
      ENDDO
C
C     CALL MY_BARRIER
C
      IF (DEBUG(3)>=1) THEN
          NB_DST2 = NB_DST2 + I_STOK
      ENDIF

      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I25CDCOR3(
     1     JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_OPT_E,INTBUF_TAB%CAND_OPT_N,
     2     CAND_E_N,CAND_N_N )
C cand_n et cand_e remplace par cand_n_n et cand_e_n
          CALL I25COR3_22(
     1  JLT       ,X          ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV  ,CAND_E_N  ,
     2  CAND_N_N  ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS,STIF      ,IGAP,
     3  XI         ,YI        ,ZI           ,VXI          ,VYI       ,
     4  VZI        ,IX1       ,IX2          ,IX3          ,IX4       ,
     5  NSVG       ,NSN       ,V            ,
     6  NIN        ,INTBUF_TAB%GAP_S,GAPS   ,INTBUF_TAB%ADMSR ,
     .                                              INTBUF_TAB%EDGE_BISECTOR,
     7  XX        ,YY         ,ZZ        ,
     C  VX1       ,VX2        ,VX3       ,VX4          ,
     D  VY1       ,VY2        ,VY3       ,VY4          ,
     E  VZ1       ,VZ2        ,VZ3       ,VZ4          ,
     E  NAX       ,NAY        ,NAZ       ,
     E  NBX       ,NBY        ,NBZ       ,
     J  INTBUF_TAB%GAP_M,GAPM   ,INTBUF_TAB%GAP_NM,GAP_NM ,
     L  INTBUF_TAB%MVOISIN,NRTM ,INTBUF_TAB%MSEGTYP24,ISHEL   ,
     P  MVOISA  ,MVOISB  ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,GAPMXL,
     Q  INTBUF_TAB%LBOUND,IBOUNDA,IBOUNDB )

C
          CALL I25DST3_22(
     1 JLT         ,CAND_N_N    ,CAND_E_N    ,ISHEL       ,
     2 XX          ,YY          ,ZZ          ,
     3 XI          ,YI          ,ZI          ,
     4 VX1         ,VX2         ,VX3         ,VX4         ,VXI         ,
     5 VY1         ,VY2         ,VY3         ,VY4         ,VYI         ,
     6 VZ1         ,VZ2         ,VZ3         ,VZ4         ,VZI         ,
     7 NIN         ,NSN         ,IX1         ,
     9 IX2         ,IX3         ,IX4         ,NSVG        ,STIF        ,
     A INACTI      ,INTBUF_TAB%MSEGLO,GAPS   ,GAPM        ,
     B INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S,GAP_NM ,
     C INTBUF_TAB%PENE_OLD,INTBUF_TAB%STIF_OLD,ITAB ,
     D PENMIN     ,EPS       ,INTBUF_TAB%ICONT_I,MARGE   ,
     E NAX     ,NAY   ,NAZ     ,
     E NBX     ,NBY   ,NBZ     ,
     J FAR     ,PENT  ,
     L SUBTRIA ,LB    ,LC      ,LBH   ,LCH    ,
     P MVOISA  ,MVOISB,GAPMXL  ,IBOUNDA,IBOUNDB,
     Q INTBUF_TAB%VTX_BISECTOR ,DRAD   ,DGAPLOAD)
C
          CALL I25GLOB_22(
     1 JLT    ,CAND_N_N ,CAND_E_N ,INTBUF_TAB%CAND_OPT_E,
     2 NIN    ,NSN      ,IX1      ,IX2    ,IX3    ,
     3 IX4    ,NSVG     ,STIF     ,INACTI ,INTBUF_TAB%MSEGLO ,
     4 INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S ,ITAB   ,SUBTRIA  ,
     5 FAR     ,PENT    ,LB      ,LC       ,
C    5 FAR     ,PENT    ,LBH     ,LCH      ,
     6 INDEX2(NFT+1) ,INTBUF_TAB%FARM ,INTBUF_TAB%PENM ,
     .                                                  INTBUF_TAB%LBM   ,
     7 INTBUF_TAB%LCM )
C
      ENDDO
C-----------------------------------------------------------------------
C
      CALL MY_BARRIER
C
C pmax_gap..
#include "lockon.inc"
      INTBUF_TAB%VARIABLES(23) = ZERO
#include "lockoff.inc"
C-----------------------------------------------------------------------
      DEALLOCATE(INDEX2)
      RETURN
      END

