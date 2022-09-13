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
Chd|  I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        I25COR3_1                     source/interfaces/int25/i25cor3.F
Chd|        I25DST3_1                     source/interfaces/int25/i25dst3_1.F
Chd|        I25GLOB_1                     source/interfaces/int25/i25dst3_1.F
Chd|        I25PREP_SLID_1                source/interfaces/int25/i25slid.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COMP_1(
     1                   IPARI   ,INTBUF_TAB ,X      ,ITAB   ,NIN        ,
     2                   KINET      ,JTASK  ,NB_DST1,V       ,SENSOR     )
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
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NB_DST1, JTASK
      INTEGER IPARI(NPARI,NINTER), ITAB(*), KINET(*)
C     REAL
      my_real 
     .   X(3,*), V(3,*), SENSOR(NSENSR,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I, J, L, H, I_STOK_RTLM, I_STOK, JLT , NFT,
     .        INACTI, NADMSR, NB_LOC, DEBUT, IGAP,
     .        MG, N, NSN, NSNR, IVIS2, ISENS
      INTEGER  LENT, MAXCC
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), FAR(MVSIZ,4), SUBTRIA(MVSIZ), 
     .        MVOISN(MVSIZ,4), IBOUND(4,MVSIZ)
C     REAL
C-----------------------------------------------
C     REAL
      my_real
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .   XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5), 
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   GAPS(MVSIZ), GAPM(MVSIZ),
     .   PENT(MVSIZ,4), DIST(MVSIZ), LB(MVSIZ,4), LC(MVSIZ,4), 
     .   LBH(MVSIZ,4), LCH(MVSIZ,4), GAP_NM(4,MVSIZ), GAPMXL(MVSIZ),
     .   TS, STARTT, STOPT, DRAD, DGAPLOAD
C-----------------------------------------------
        NSN   =IPARI(5,NIN)
        IVIS2 =IPARI(14,NIN)
        NSNR  =IPARI(24,NIN)
        INACTI=IPARI(22,NIN)
        NADMSR=IPARI(67,NIN)
        IGAP  =IPARI(21,NIN)
        DRAD = ZERO
        IF(IPARI(47,NIN) > 0) DRAD    = INTBUF_TAB%VARIABLES(32)
        DGAPLOAD    = INTBUF_TAB%VARIABLES(46)
C
        STARTT=INTBUF_TAB%VARIABLES(3)
        STOPT =INTBUF_TAB%VARIABLES(11)
        IF(STARTT>TT) RETURN ! dont look for sliding
        IF(TT>STOPT)  RETURN
C
C       Look if interface is activated 
        ISENS = IPARI(64,NIN)  
        IF(ISENS/=0)  THEN         ! Interface activated by sensor
           TS = SENSOR(2,ISENS)
        ELSE
           TS = TT
        ENDIF
        IF(TT<TS) RETURN
C
        I_STOK_RTLM=INTBUF_TAB%I_STOK(3) ! IRTLM/=0 was stored at the beginning of CAND_OPT
        NB_LOC = I_STOK_RTLM / NTHREAD
        IF (JTASK==NTHREAD) THEN
          I_STOK = I_STOK_RTLM-NB_LOC*(NTHREAD-1)
        ELSE
          I_STOK = NB_LOC
        ENDIF
C-----------------------------------------------------------------------
        IF (DEBUG(3)>=1) THEN
          NB_DST1  = NB_DST1  + I_STOK
        ENDIF
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          DEBUT = (JTASK-1)*NB_LOC + NFT
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I25COR3_1(
     1      JLT       ,X          ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV  ,INTBUF_TAB%CAND_OPT_E(DEBUT+1),
     2      INTBUF_TAB%CAND_OPT_N(DEBUT+1),INTBUF_TAB%IRTLM ,INTBUF_TAB%STFM ,INTBUF_TAB%STFNS,STIF ,
     3      IGAP       ,XI        ,YI           ,ZI       ,
     4      IX1        ,IX2       ,IX3          ,IX4       ,
     5      NSVG       ,NSN       ,
     6      NIN        ,INTBUF_TAB%GAP_S,GAPS   ,INTBUF_TAB%ADMSR ,
     .                                                  INTBUF_TAB%EDGE_BISECTOR,
     7      XX        ,YY         ,ZZ        ,
     8      NNX       ,NNY        ,NNZ       ,
     9      INTBUF_TAB%GAP_M,GAPM ,INTBUF_TAB%GAP_NM,GAP_NM ,SUBTRIA,
     A      INTBUF_TAB%MVOISIN,MVOISN    ,
     B      INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML, GAPMXL,INTBUF_TAB%LBOUND,IBOUND)
C
          CALL I25DST3_1(
     1      JLT         ,INTBUF_TAB%CAND_OPT_N(DEBUT+1),INTBUF_TAB%CAND_OPT_E(DEBUT+1),
     2      XX          ,YY          ,ZZ          ,
     3      XI          ,YI          ,ZI          ,
     5      NIN         ,NSN         ,IX1         ,
     6      IX2         ,IX3         ,IX4         ,NSVG        ,STIF        ,
     7      INACTI      ,INTBUF_TAB%MSEGLO,GAPS   ,GAPM        ,GAPMXL      ,
     8      INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S,GAP_NM ,ITAB ,
     9      INTBUF_TAB%ICONT_I,NNX   ,NNY         ,NNZ         ,
     A      FAR               ,PENT  ,DIST        ,LB          ,LC          ,
     B      LBH               ,LCH   ,SUBTRIA     ,MVOISN      ,IBOUND      ,
     C      INTBUF_TAB%VTX_BISECTOR  ,DRAD        ,DGAPLOAD    )
C
          CALL I25GLOB_1(
     1      JLT    ,INTBUF_TAB%CAND_OPT_N(DEBUT+1),INTBUF_TAB%CAND_OPT_E(DEBUT+1),
     2      NIN    ,NSN      ,IX1      ,IX2    ,IX3    ,
     3      IX4    ,NSVG     ,STIF     ,INACTI ,INTBUF_TAB%MSEGLO ,
     4      INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S ,ITAB   ,
     5      FAR    ,PENT     ,LBH    ,LCH    ,
C    5      FAR    ,PENT     ,LB     ,LC     ,
     6      INTBUF_TAB%FARM(4*DEBUT+1) ,INTBUF_TAB%PENM(4*DEBUT+1) ,
     .                                 INTBUF_TAB%LBM(4*DEBUT+1) ,INTBUF_TAB%LCM(4*DEBUT+1) )
C
          CALL I25PREP_SLID_1(
     1 JLT      ,INTBUF_TAB%CAND_OPT_N(DEBUT+1),INTBUF_TAB%CAND_OPT_E(DEBUT+1),NIN     ,
     2 NSN      ,NSNR   ,INACTI ,INTBUF_TAB%MSEGLO ,
     3 INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S ,ITAB     ,INTBUF_TAB%FARM(4*DEBUT+1),
     .                                                         INTBUF_TAB%PENM(4*DEBUT+1) ,
     4 INTBUF_TAB%IRECTM,NADMSR   ,INTBUF_TAB%ADMSR,INTBUF_TAB%LBM(4*DEBUT+1)   ,
     .                                                        INTBUF_TAB%LCM(4*DEBUT+1)   ,
     5 INTBUF_TAB%ISLIDE ,INTBUF_TAB%NSV)

      ENDDO
C-----------------------------------------------------------------------
      RETURN
      END

