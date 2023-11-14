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
Chd|  I11KE3                        source/interfaces/int11/i11ke3.F
Chd|-- called by -----------
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        ASSEM_INT11                   source/implicit/assem_int.F   
Chd|        ASS_SPMD11                    source/implicit/assem_int.F   
Chd|        I11COR3                       source/interfaces/int11/i11cor3.F
Chd|        I11DSTK3                      source/interfaces/int11/i11dstk3.F
Chd|        I11KEG3                       source/interfaces/int11/i11keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I11KE3(
     1      A         ,V        ,MS        ,IPARI   ,INTBUF_TAB,
     2      X        ,NIN       ,NUM_IMP ,CAND_N    ,
     3      CAND_E    ,IDDL     ,K_DIAG    ,K_LT    ,IADK      ,
     4      JDIK     ,GAP_IMP   ,LREM      ,ITAB    ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl2_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),NIN,IDDL(*),
     .        IADK(*) ,JDIK(*),LREM,ITAB(*)
C     REAL
      my_real DT2T,
     .   X(*), A(3,*), V(3,*),MS(*),
     .   K_DIAG(*),K_LT(*),GAP_IMP

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NOINT,I, I_STOK, NSEG,JLT , NFT, JLT_NEW, IGAP, 
     .        I_STOK_LOC,DEBUT,IDESAC, IGSTI, IBID
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO, GAPMIN, KMIN, KMAX
C-----------------------------------------------
C     REAL
      my_real
     .     NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .     HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .     STIF(MVSIZ),GAPV(MVSIZ),
     .     XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .     XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .     XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .     VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .     VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .     VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .     MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ) ,
     .     K1I11(9,MVSIZ),K1J11(9,MVSIZ),K2I11(9,MVSIZ),
     .     K2J11(9,MVSIZ),K1I12(9,MVSIZ),K1J12(9,MVSIZ),
     .     K2I12(9,MVSIZ),K2J12(9,MVSIZ),OFF(MVSIZ),BID(MVSIZ)
      INTEGER :: NRTS
C-----------------------------------------------
      IF(NUM_IMP==0) RETURN
      IBID=0
C
      NRTS  =IPARI(3)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      IGSTI =IPARI(34)
C
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      FRIC =INTBUF_TAB%VARIABLES(1)
      GAP  =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      KMIN =INTBUF_TAB%VARIABLES(17)
      KMAX =INTBUF_TAB%VARIABLES(18)
C------pour evider la desactivation des noeuds secnds------
      GAP_IMP=MIN(GAP_IMP,ZEP9*GAPMIN)
      IDESAC = 0
C
        I_STOK = NUM_IMP
C  90c1        LREM=0
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I11COR3(
     1  JLT          ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM,X    ,V      ,
     2  CAND_N(NFT+1),CAND_E(NFT+1),INTBUF_TAB%STFS,INTBUF_TAB%STFM,GAPMIN ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP   ,GAPV         ,MS     ,
     4  STIF         ,XXS1         ,XXS2         ,XYS1         ,XYS2   ,
     5  XZS1         ,XZS2         ,XXM1         ,XXM2         ,XYM1   ,
     6  XYM2         ,XZM1         ,XZM2         ,VXS1         ,VXS2   ,
     7  VYS1         ,VYS2         ,VZS1         ,VZS2         ,VXM1   ,
     8  VXM2         ,VYM1         ,VYM2         ,VZM1         ,VZM2   ,
     9  MS1          ,MS2          ,MM1          ,MM2          ,N1     ,
     A  N2           ,M1           ,M2           ,NRTS         ,NIN    ,
     B  IGSTI        ,KMIN         ,KMAX         ,IBID         ,IBID   ,
     C  INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,IBID ,BID          ,BID    ,
     D  BID          ,BID          ,BID      ,INTBUF_TAB%AREAS ,BID    ,
     E  BID          ,IBID         ,IBID   ,INTBUF_TAB%IELEC,INTBUF_TAB%IELES  ,
     F  IBID         ,ITAB         ,IBID   ,IBID               ,IBID   ,
     G  IBID         ,IBID         )
          CALL I11DSTK3(
     1        JLT    ,CAND_N(NFT+1),CAND_E(NFT+1),HS1   ,HS2   ,
     2        HM1    , HM2  ,NX    ,NY    ,NZ    ,
     3        STIF   ,N1    ,N2    ,M1    ,M2    ,
     4        XXS1   ,XXS2  ,XYS1  ,XYS2  ,
     5        XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     6        XYM2   ,XZM1  ,XZM2  ,VXS1  ,VXS2  ,
     7        VYS1   ,VYS2  ,VZS1  ,VZS2  ,VXM1  ,
     8        VXM2   ,VYM1  ,VYM2  ,VZM1  ,VZM2  ,
     9        MS1    ,MS2   ,MM1   ,MM2   ,GAPV  )
          CALL I11KEG3(
     1  JLT          ,A      ,V         ,GAP     ,FRIC              ,
     2  MS           ,CAND_N(NFT+1),CAND_E(NFT+1),STIGLO  ,STIF     ,
     3  HS1          ,HS2          ,HM1          ,HM2     ,NX       ,
     4  NY           ,NZ           ,GAPV,INTBUF_TAB%PENIS,INTBUF_TAB%PENIM,
     5  IPARI(22)    ,NRTS         ,MS1      ,MS2     ,MM1          ,
     6  MM2          ,VXS1         ,VYS1     ,VZS1    ,VXS2         ,
     7  VYS2         ,VZS2         ,VXM1     ,VYM1    ,VZM1         ,
     8  VXM2         ,VYM2         ,VZM2     ,K1I11   ,K1I12        ,
     9  K1J11        ,K1J12        ,K2I11    ,K2I12   ,K2J11        ,
     A  K2J12        ,N1           ,N2       ,NIN     ,LREM         ,
     B  OFF          ,SK_INT       ,IDESAC   )
C
           IF (IMACH==3.AND.NSPMD>1) THEN
            LREM = LREM + JLT
            CALL ASS_SPMD11(3      ,N1       ,N2        ,M1       ,         
     1         M2       ,JLT       ,IDDL     ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,K1I11    ,K1I12     ,K1J11    ,
     9         K1J12    ,K2I11     ,K2I12    ,K2J11     ,K2J12    ,
     A         OFF      ,NIN       ,CAND_N(NFT+1),NRTS  )
            LREM = LREM - JLT
           ENDIF 
            CALL ASSEM_INT11(3     ,N1        ,N2        ,M1       ,
     1         M2       ,JLT       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,K1I11    ,K1I12     ,K1J11    ,
     9         K1J12    ,K2I11     ,K2I12    ,K2J11     ,K2J12    ,
     A         OFF      )
        ENDDO
C
        IF(IDESAC>0)  GAP_IMP=-NOINT
C
      RETURN
      END
Chd|====================================================================
Chd|  I11FORCF3                     source/interfaces/int11/i11ke3.F
Chd|-- called by -----------
Chd|        I7MAINFR                      source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        I11CORP3                      source/interfaces/int11/i11corp3.F
Chd|        I11FRF3                       source/interfaces/int11/i11keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I11FORCF3(  A     ,V      ,MS   ,X       ,D       ,
     1                    IPARI   ,INTBUF_TAB,NIN     ,
     2                    NUM_IMP ,CAND_N ,CAND_E)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),NIN
C     REAL
      my_real 
     .  A(3,*), MS(*), V(3,*),D(3,*),X(*) 

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, 
     .        INACTI, NRTS,NTY,L
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, STOPT
      my_real
     .   HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), STIF(MVSIZ),
     .   MS1(MVSIZ),MS2(MVSIZ),MM1(MVSIZ),MM2(MVSIZ),
     .   VXS1(MVSIZ),VYS1(MVSIZ),VZS1(MVSIZ),VXS2(MVSIZ),VYS2(MVSIZ),
     .   VZS2(MVSIZ),VXM1(MVSIZ),VYM1(MVSIZ),VZM1(MVSIZ),VXM2(MVSIZ),
     .   VYM2(MVSIZ),VZM2(MVSIZ),DXS1(MVSIZ),DYS1(MVSIZ),DZS1(MVSIZ),
     .   DXS2(MVSIZ),DYS2(MVSIZ),DZS2(MVSIZ),DXM1(MVSIZ),DYM1(MVSIZ),
     .   DZM1(MVSIZ),DXM2(MVSIZ),DYM2(MVSIZ),DZM2(MVSIZ)
C-----------------------------------------------
C     REAL
C----------------------------------------------
      IF(NUM_IMP==0) RETURN
C
      NRTS  =IPARI(3)
      NTY   =IPARI(7)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
      FRIC  =INTBUF_TAB%VARIABLES(1)
C
        I_STOK = NUM_IMP
        L = 0
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
            JLT_NEW=0
           CALL I11CORP3(JLT   ,X     ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM,
     1                   CAND_E(NFT+1),CAND_N(NFT+1),STIF ,HS1 ,     
     2                   HS2   ,HM1   ,HM2   ,NX      ,NY      ,  
     3                   NZ    ,N1    ,N2    ,M1      ,M2      ,
     4                   VXS1  ,VYS1  ,VZS1  ,VXS2    ,VYS2    ,
     5                   VZS2  ,VXM1  ,VYM1  ,VZM1    ,VXM2    ,
     6                   VYM2  ,VZM2  ,DXS1  ,DYS1    ,DZS1    ,  
     7                   DXS2  ,DYS2  ,DZS2  ,DXM1    ,DYM1    ,  
     8                   DZM1  ,DXM2  ,DYM2  ,DZM2    ,MS1     ,
     9                   MS2   ,MM1   ,MM2   ,MS      ,D       , 
     A                   V     ,NRTS  ,NIN   ,JLT_NEW ,L       )
            L = L +JLT_NEW
            JLT=JLT_NEW
            CALL I11FRF3(JLT    ,A      ,V      ,FRIC   ,HS1    ,
     1                  HS2     ,HM1    ,HM2    ,NX     ,NY     ,
     2                  NZ      ,MS1    ,MS2    ,MM1    ,MM2    ,
     3                  VXS1    ,VYS1   ,VZS1   ,VXS2   ,VYS2   ,
     4                  VZS2    ,VXM1   ,VYM1   ,VZM1   ,VXM2   ,
     5                  VYM2    ,VZM2   ,N1     ,N2     ,M1     ,
     6                   M2     ,DXS1   ,DYS1   ,DZS1   ,DXS2   ,  
     7                   DYS2   ,DZS2   ,DXM1   ,DYM1   ,DZM1   ,  
     8                   DXM2   ,DYM2   ,DZM2   ,STIF   , NIN   ,
     9                   SK_INT )
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_I11MAINF                  source/interfaces/int11/i11ke3.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I11CDCOR3                     source/interfaces/int11/i11cdcor3.F
Chd|        I11COR3                       source/interfaces/int11/i11cor3.F
Chd|        I11DSTK3                      source/interfaces/int11/i11dstk3.F
Chd|        I11DSTR3                      source/interfaces/int11/i11dstk3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_I11MAINF(
     1       IPARI  ,INTBUF_TAB,X         ,
     2       V      ,MS        ,NIN       ,LINDMAX  ,
     5       JTASK  ,NUM_IMP   ,NS_IMP    ,NE_IMP   ,
     6       ITAB   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), NIN,JTASK,LINDMAX
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),ITAB(*)
C     REAL
      my_real 
     .  MS(*), V(3,*),X(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NOINT,I, I_STOK, NSEG,JLT , NFT, JLT_NEW, IGAP, 
     .        I_STOK_LOC,DEBUT,LREM,IDESAC,NB_LOC, IGSTI, IBID
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ),
     .        CS_LOC(MVSIZ),CM_LOC(MVSIZ),INDEX2(LINDMAX)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO, GAPMIN, KMIN, KMAX
C-----------------------------------------------
C     REAL
      my_real
     .     NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .     HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .     STIF(MVSIZ),GAPV(MVSIZ),
     .     XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .     XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .     XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .     VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .     VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .     VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .     MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ),
     .     BID(MVSIZ)
      INTEGER :: NRTS
C-----------------------------------------------
      IF(NUM_IMP==0) RETURN
C
      IBID=0
C
      NRTS  =IPARI(3)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      IGSTI =IPARI(34)
C
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      FRIC =INTBUF_TAB%VARIABLES(1)
      GAP  =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      KMIN =INTBUF_TAB%VARIABLES(17)
      KMAX =INTBUF_TAB%VARIABLES(18)
C
        I_STOK = INTBUF_TAB%I_STOK(1)
C decoupage statique
        NB_LOC = I_STOK / NTHREAD
        IF (JTASK==NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
        ELSE
          I_STOK_LOC = NB_LOC
        ENDIF
        DEBUT = (JTASK-1)*NB_LOC
        I_STOK = 0
C
        NUM_IMP = 0
C recalcul du istok
        DO I = DEBUT+1, DEBUT+I_STOK_LOC
          IF(INTBUF_TAB%CAND_N(I)<0) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
C inbuf == cand_S
            INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
          ENDIF
        ENDDO
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I11CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,CM_LOC,
     2         CS_LOC)
          CALL I11COR3(
     1  JLT          ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM,X    ,V      ,
     2  CS_LOC       ,CM_LOC       ,INTBUF_TAB%STFS,INTBUF_TAB%STFM,GAPMIN ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP   ,GAPV         ,MS     ,
     4  STIF         ,XXS1         ,XXS2         ,XYS1         ,XYS2   ,
     5  XZS1         ,XZS2         ,XXM1         ,XXM2         ,XYM1   ,
     6  XYM2         ,XZM1         ,XZM2         ,VXS1         ,VXS2   ,
     7  VYS1         ,VYS2         ,VZS1         ,VZS2         ,VXM1   ,
     8  VXM2         ,VYM1         ,VYM2         ,VZM1         ,VZM2   ,
     9  MS1          ,MS2          ,MM1          ,MM2          ,N1     ,
     A  N2           ,M1           ,M2           ,NRTS         ,NIN    ,
     B  IGSTI        ,KMIN         ,KMAX         ,IBID         ,IBID   ,
     C  INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML ,IBID,BID          ,BID    ,
     D  BID          ,BID          ,BID      ,INTBUF_TAB%AREAS ,BID    ,
     E  BID          ,IBID         ,IBID   ,INTBUF_TAB%IELEC,INTBUF_TAB%IELES  ,
     F  IBID         ,ITAB         ,IBID   ,IBID               ,IBID   ,
     G  IBID         ,IBID         )
          CALL I11DSTK3(
     1        JLT    ,CS_LOC,CM_LOC,HS1   ,HS2   ,
     2        HM1    , HM2  ,NX    ,NY    ,NZ    ,
     3        STIF   ,N1    ,N2    ,M1    ,M2    ,
     4        XXS1   ,XXS2  ,XYS1  ,XYS2  ,
     5        XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     6        XYM2   ,XZM1  ,XZM2  ,VXS1  ,VXS2  ,
     7        VYS1   ,VYS2  ,VZS1  ,VZS2  ,VXM1  ,
     8        VXM2   ,VYM1  ,VYM2  ,VZM1  ,VZM2  ,
     9        MS1    ,MS2   ,MM1   ,MM2   ,GAPV  )
         CALL I11DSTR3(
     1                 JLT   ,CS_LOC,CM_LOC,STIF   ,GAPV   ,
     2                 NX    ,NY    ,NZ    ,JLT_NEW)
C
            DO I = 1 ,JLT_NEW
             NS_IMP(I+NUM_IMP)=CS_LOC(I)
             NE_IMP(I+NUM_IMP)=CM_LOC(I)
            ENDDO
            NUM_IMP=NUM_IMP+JLT_NEW
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I11FKU3                       source/interfaces/int11/i11ke3.F
Chd|-- called by -----------
Chd|        INT_FKU3                      source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I11COR3                       source/interfaces/int11/i11cor3.F
Chd|        I11CORKP3                     source/interfaces/int11/i11corp3.F
Chd|        I11DSTK3                      source/interfaces/int11/i11dstk3.F
Chd|        I11KFOR3                      source/interfaces/int11/i11keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I11FKU3(
     1      A         ,V         ,MS        ,D        ,
     2      IPARI     ,INTBUF_TAB,X        ,NIN     ,
     3      NUM_IMP   ,CAND_N    ,CAND_E    ,IUPD   )  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),NIN,IUPD
C     REAL
      my_real DT2T,
     .   X(*), A(3,*),V(3,*),D(3,*),MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, NSEG,JLT , NFT, JLT_NEW, IGAP, 
     .        I_STOK_LOC,DEBUT,LREM,IDESAC, IGSTI, IBID
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO, GAPMIN, KMIN, KMAX
C-----------------------------------------------
C     REAL
      my_real
     .     NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .     HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .     STIF(MVSIZ),GAPV(MVSIZ),
     .     XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .     XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .     XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .     VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .     VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .     VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .     MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ) ,
     .     DXS1(MVSIZ),DYS1(MVSIZ),DZS1(MVSIZ),
     .     DXS2(MVSIZ),DYS2(MVSIZ),DZS2(MVSIZ),BID(MVSIZ)
      INTEGER :: NRTS
C-----------------------------------------------
      IF(NUM_IMP==0) RETURN
C
      IBID=0
C
      NRTS  =IPARI(3)
      IGAP  =IPARI(21)
      IGSTI =IPARI(34)
C
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      FRIC =INTBUF_TAB%VARIABLES(1)
      GAP  =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      KMIN =INTBUF_TAB%VARIABLES(17)
      KMAX =INTBUF_TAB%VARIABLES(18)
      IDESAC = 0
C
        I_STOK = NUM_IMP
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I11COR3(
     1  JLT          ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM,X      ,V     ,
     2  CAND_N(NFT+1),CAND_E(NFT+1),INTBUF_TAB%STFS,INTBUF_TAB%STFM,GAPMIN ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP         ,GAPV         ,MS     ,
     4  STIF         ,XXS1         ,XXS2         ,XYS1         ,XYS2   ,
     5  XZS1         ,XZS2         ,XXM1         ,XXM2         ,XYM1   ,
     6  XYM2         ,XZM1         ,XZM2         ,VXS1         ,VXS2   ,
     7  VYS1         ,VYS2         ,VZS1         ,VZS2         ,VXM1   ,
     8  VXM2         ,VYM1         ,VYM2         ,VZM1         ,VZM2   ,
     9  MS1          ,MS2          ,MM1          ,MM2          ,N1     ,
     A  N2           ,M1           ,M2           ,NRTS         ,NIN    ,
     B  IGSTI        ,KMIN         ,KMAX         ,IBID         ,IBID   ,
     C  INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML ,IBID,BID          ,BID    ,
     D  BID          ,BID          ,BID      ,INTBUF_TAB%AREAS ,BID    ,
     E  BID          ,IBID         ,IBID   ,INTBUF_TAB%IELEC,INTBUF_TAB%IELES  ,
     F  IBID         ,IBID         ,IBID   ,IBID               ,IBID   ,
     G  IBID         ,IBID         )
         CALL I11CORKP3(JLT   ,
     1                    XXS1   ,XXS2    ,XYS1    ,XYS2   ,XZS1   ,
     2                    XZS2   ,DXS1    ,DYS1    ,DZS1   ,DXS2   ,  
     3                    DYS2   ,DZS2    ,D       ,CAND_N(NFT+1),NRTS ,
     4                    N1     ,N2      ,NIN     ,IUPD   )
          CALL I11DSTK3(
     1        JLT    ,CAND_N(NFT+1),CAND_E(NFT+1),HS1   ,HS2   ,
     2        HM1    , HM2  ,NX    ,NY    ,NZ    ,
     3        STIF   ,N1    ,N2    ,M1    ,M2    ,
     4        XXS1   ,XXS2  ,XYS1  ,XYS2  ,
     5        XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     6        XYM2   ,XZM1  ,XZM2  ,VXS1  ,VXS2  ,
     7        VYS1   ,VYS2  ,VZS1  ,VZS2  ,VXM1  ,
     8        VXM2   ,VYM1  ,VYM2  ,VZM1  ,VZM2  ,
     9        MS1    ,MS2   ,MM1   ,MM2   ,GAPV  )
         CALL I11KFOR3(
     1        JLT    ,A       ,V       ,GAP    ,MS     ,
     2        CAND_N(NFT+1),CAND_E(NFT+1),STIF    ,FRIC   ,
     3        HS1    ,HS2     ,HM1    ,HM2    ,NX     ,
     4        NY     ,NZ     ,GAPV    ,INTBUF_TAB%PENIS,INTBUF_TAB%PENIM, 
     5        IPARI(22)    ,NRTS   ,MS1     ,MS2    ,MM1    ,
     6        MM2    ,VXS1   ,VYS1    ,VZS1   ,VXS2   ,
     7        VYS2   ,VZS2   ,VXM1    ,VYM1   ,VZM1   ,
     8        VXM2   ,VYM2   ,VZM2    ,N1      ,N2    ,
     9        M1     ,M2     ,NIN     ,DXS1    ,DYS1  ,
     A        DZS1   ,DXS2   ,DYS2    ,DZS2    ,D     ,
     B        SK_INT)
        END DO 
C
      RETURN
      END
