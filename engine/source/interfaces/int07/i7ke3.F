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
Chd|  I7KE3                         source/interfaces/int07/i7ke3.F
Chd|-- called by -----------
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        ASSEM_INT                     source/implicit/assem_int.F   
Chd|        ASS_SPMD                      source/implicit/assem_int.F   
Chd|        FFIZERO                       source/interfaces/int07/i7ke3.F
Chd|        I7CORK3                       source/interfaces/int07/i7cork3.F
Chd|        I7DSTK3                       source/interfaces/int07/i7dstk3.F
Chd|        I7KEG3                        source/interfaces/int07/i7keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I7KE3(  A     ,V      ,MS   ,
     1                   IPARI ,INTBUF_TAB,X    ,NIN   ,
     2                   NUM_IMP,CAND_N,CAND_E     ,INDEX2,
     3                   IDDL  ,K_DIAG ,K_LT ,IADK ,JDIK  ,
     4                   GAP_IMP,LREM  )
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl2_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), INDEX2(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),NIN,IDDL(*),
     .         IADK(*)     ,JDIK(*),LREM
C     REAL
      my_real 
     .  A(3,*), MS(*), V(3,*),X(*),K_DIAG(*),K_LT(*) 
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JD(50),KD(50), JFI, KFI,
     .        I, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), I3N 
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,GAP_IMP
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), OFF(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),
     .    MSI(MVSIZ),KI11(9,MVSIZ),KJ11(9,MVSIZ),
     .    KK11(9,MVSIZ),KL11(9,MVSIZ),KI12(9,MVSIZ),
     .    KJ12(9,MVSIZ),KK12(9,MVSIZ),KL12(9,MVSIZ)
C
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
      IF(IPARI(33,NIN).EQ.1) RETURN
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      STIGLO=-INTBUF_TAB%STFAC(1)
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAP_IMP=MIN(GAP_IMP,GAPMIN)
      IGSTI=IPARI(34,NIN) 
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
      IF(NTY.EQ.3)THEN
      ELSEIF(NTY.EQ.4)THEN
      ELSEIF(NTY.EQ.5)THEN
      ELSEIF(NTY.EQ.6)THEN
C
      ELSEIF(NTY.EQ.7)THEN
C
        CALL FFIZERO(NUM_IMP    ,NIN   ,NSN     ,CAND_N     )
        I_STOK = NUM_IMP
C  90c1        LREM=0
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I7CORK3(JLT ,X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     1                CAND_E(NFT+1)  ,CAND_N(NFT+1) ,INTBUF_TAB%STFM,
     2                INTBUF_TAB%STFNS,X1 ,X2  ,
     2                X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     3                Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     4                XI      ,YI    ,ZI    ,STIF  ,IX1    ,
     5                IX2     ,IX3   ,IX4   ,NSVG  ,IGAP   ,
     6                GAPMIN  ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,GAPV,
     7                MS      ,VXI   ,VYI   ,VZI   ,MSI   ,
     8                V      ,IGSTI ,KMIN   ,KMAX    ,GAPMAX,
     9                GAPMIN ,NIN    ,NTY   ,NSN    )
          CALL I7DSTK3(
     1                  JLT    ,CAND_N(NFT+1) ,CAND_E(NFT+1) ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     5                  NX1    ,NX2    ,NX3    ,NX4    ,NY1    ,
     6                  NY2    ,NY3    ,NY4    ,NZ1    ,NZ2    ,
     7                  NZ3    ,NZ4    ,LB1    ,LB2    ,LB3    ,
     8                  LB4    ,LC1    ,LC2    ,LC3    ,LC4    ,
     9                  P1     ,P2     ,P3     ,P4     ,IX1    ,
     A                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     B                  I3N    ,GAPV   ,INACTI ,INTBUF_TAB%CAND_P  ,
     C                  INDEX2(NFT+1))
            CALL I7KEG3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     2                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     3                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     4                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     5                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     6                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     7                  GAPV,INACTI,INTBUF_TAB%CAND_P,INDEX2(NFT+1),
     8                  STIGLO ,STIF   ,VXI    ,VYI    ,VZI   ,
     9                  MSI    ,KI11   ,KI12   ,KJ11   ,KJ12  ,
     A                  KK11   ,KK12   ,KL11   ,KL12   ,OFF   ,
     B                  SK_INT ,LREM   ,IPARI(39,NIN)  ,X     ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     )
           IF (IMACH.EQ.3.AND.NSPMD.GT.1) THEN
            LREM = LREM + JLT
            CALL ASS_SPMD(3        ,NSVG     ,IX1    ,IX2    ,IX3    ,       
     1         IX4      ,JLT       ,IDDL     ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      ,NIN       )
            LREM = LREM - JLT
           ENDIF 
            CALL ASSEM_INT(3       ,NSVG     ,IX1    ,IX2    ,IX3    ,
     1         IX4      ,JLT       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      )
        ENDDO
C
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I7MAINFR                      source/interfaces/int07/i7ke3.F
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        I10FORCF3                     source/interfaces/int10/i10ke3.F
Chd|        I11FORCF3                     source/interfaces/int11/i11ke3.F
Chd|        I7FORCF3                      source/interfaces/int07/i7ke3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I7MAINFR(  A     ,V     ,D      ,X     ,MS    ,
     1                     IPARI  ,INTBUF_TAB    ,NUM_IMP,NS_IMP,
     2                     NE_IMP)
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NUM_IMP(*),NS_IMP(*),NE_IMP(*)
      my_real
     .  A(3,*),V(3,*),D(3,*),X(3,*) ,MS(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N, IAD,NTY
C
      IAD=1
      DO N = 1, NINTER
        NTY   =IPARI(7,N)
        IF(NTY.EQ.7.OR.NTY.EQ.24) THEN
          CALL I7FORCF3( A    ,V     ,MS     ,X       ,D       ,
     1       IPARI     ,INTBUF_TAB(N) ,N       ,
     2       NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   )
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.10) THEN
          CALL I10FORCF3( A    ,V     ,MS     ,X       ,D       ,
     1       IPARI(1,N),INTBUF_TAB(N) ,N       ,
     2       NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   )
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.11) THEN
          CALL I11FORCF3( A    ,V     ,MS     ,X       ,D       ,
     1        IPARI(1,N),INTBUF_TAB(N) ,N       ,
     2        NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   )
          IAD=IAD+NUM_IMP(N)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I7FORCF3                      source/interfaces/int07/i7ke3.F
Chd|-- called by -----------
Chd|        I7MAINFR                      source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        I24CORP3                      source/interfaces/int24/i24cork3.F
Chd|        I7CORP3                       source/interfaces/int07/i7cork3.F
Chd|        I7FRF3                        source/interfaces/int07/i7keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I7FORCF3(  A     ,V      ,MS   ,X    ,D ,
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
      INTEGER IPARI(NPARI,*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),NIN
C     REAL
      my_real 
     .  A(3,*), MS(*), V(3,*),D(3,*),X(*) 

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, 
     .        INACTI, NSN,NTY,L
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), 
     .        IX4(MVSIZ),NSVG(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, STOPT
C-----------------------------------------------
C     REAL
      my_real
     .    VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),
     .    DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ),
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),
     .    MSI(MVSIZ),STIF(MVSIZ),
     .    N1(MVSIZ),N2(MVSIZ),N3(MVSIZ)
C----------------------------------------------
C
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
      IF(IPARI(33,NIN).EQ.1) RETURN
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      FRIC  =INTBUF_TAB%VARIABLES(1)
C
      IF(NTY.EQ.3)THEN
      ELSEIF(NTY.EQ.4)THEN
      ELSEIF(NTY.EQ.5)THEN
      ELSEIF(NTY.EQ.6)THEN
C
      ELSEIF(NTY.EQ.7)THEN
C
        I_STOK = NUM_IMP
        L = 0
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
            JLT_NEW=0
            CALL I7CORP3(JLT   ,X     ,INTBUF_TAB%IRECTM,CAND_E(NFT+1),
     1                   CAND_N(NFT+1),STIF  ,H1      ,H2      ,
     2                   H3    ,H4    ,N1    ,N2      ,N3      ,
     3                   IX1   ,IX2   ,IX3   ,IX4     ,NSVG    ,
     4                   VXI   ,VYI   ,VZI   ,MSI     ,DXI     ,
     5                   DYI   ,DZI   ,NSN   ,NIN     ,JLT_NEW ,
     6                   L     )
            L = L +JLT_NEW
            JLT=JLT_NEW
            CALL I7FRF3(JLT    ,A      ,V      ,MS     ,FRIC   ,
     1                  N1     ,N2     ,N3     ,H1     ,H2     ,
     2                  H3     ,H4     ,IX1    ,IX2    ,IX3    ,
     3                  IX4   ,NSVG    ,VXI    ,VYI    ,VZI    ,
     4                  MSI   ,DXI     ,DYI    ,DZI    ,STIF   ,
     5                  NIN   ,D       ,SK_INT )
       ENDDO
      ELSEIF(NTY == 24)THEN
C
        I_STOK = NUM_IMP
        L = 0
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
            JLT_NEW=0
            CALL I24CORP3(JLT   ,X     ,INTBUF_TAB%IRECTM,CAND_E(NFT+1),
     1                   CAND_N(NFT+1),STIF  ,H1      ,H2      ,
     2                   H3    ,H4    ,N1    ,N2      ,N3      ,
     3                   IX1   ,IX2   ,IX3   ,IX4     ,NSVG    ,
     4                   VXI   ,VYI   ,VZI   ,MSI     ,DXI     ,
     5                   DYI   ,DZI   ,NSN   ,NIN     ,JLT_NEW ,
     6                   L     )
            L = L +JLT_NEW
            JLT=JLT_NEW
            CALL I7FRF3(JLT    ,A      ,V      ,MS     ,FRIC   ,
     1                  N1     ,N2     ,N3     ,H1     ,H2     ,
     2                  H3     ,H4     ,IX1    ,IX2    ,IX3    ,
     3                  IX4   ,NSVG    ,VXI    ,VYI    ,VZI    ,
     4                  MSI   ,DXI     ,DYI    ,DZI    ,STIF   ,
     5                  NIN   ,D       ,SK_INT )
       ENDDO
C
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_I7MAINF                   source/interfaces/int07/i7ke3.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        I7CORK3                       source/interfaces/int07/i7cork3.F
Chd|        I7DSTK3                       source/interfaces/int07/i7dstk3.F
Chd|        I7DSTR3                       source/interfaces/int07/i7dstk3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_I7MAINF(
     1       IPARI  ,INTBUF_TAB,X        ,
     2       V      ,MS        ,NIN       ,LINDMAX  ,
     5       JTASK  ,NUM_IMP   ,NS_IMP    ,NE_IMP   ,IND_IMP)
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl2_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NIN,JTASK,LINDMAX
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*)
C     REAL
      my_real 
     .  MS(*), V(3,*),X(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI,
     .        NB_LOC,I_STOK_LOC,DEBUT
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),INDEX2(LINDMAX),
     .        I3N ,LREM
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), OFF(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),
     .    MSI(MVSIZ)
C
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
      IF(IPARI(33,NIN).EQ.1) RETURN
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      STIGLO=-INTBUF_TAB%STFAC(1)
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      IGSTI=IPARI(34,NIN) 
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
      IF(NTY.EQ.3)THEN
      ELSEIF(NTY.EQ.4)THEN
      ELSEIF(NTY.EQ.5)THEN
      ELSEIF(NTY.EQ.6)THEN
C
      ELSEIF(NTY.EQ.7)THEN
C
        I_STOK = INTBUF_TAB%I_STOK(1)
C decoupage statique
        IF(INACTI.NE.7.OR.IMACH.EQ.3)THEN
          NB_LOC = I_STOK / NTHREAD
          IF (JTASK.EQ.NTHREAD) THEN
            I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
          ELSE
            I_STOK_LOC = NB_LOC
          ENDIF
          DEBUT = (JTASK-1)*NB_LOC
        ELSE ! INACTI = 7 et IMACH NE 3
          IF(JTASK.EQ.1)THEN
            I_STOK_LOC = I_STOK
            DEBUT = 0
          ELSE
            I_STOK_LOC = 0
            DEBUT = 0
          END IF
        END IF
        I_STOK = 0
C
        NUM_IMP = 0
C
C recalcul du istok
C
        IF (INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
          DO I = DEBUT+1, DEBUT+I_STOK_LOC
            IF(INTBUF_TAB%CAND_N(I).LT.0) THEN
              I_STOK = I_STOK + 1
              INDEX2(I_STOK) = I
C inbuf == cand_n
              INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
            ELSE
C remise a 0 de cand_p
              INTBUF_TAB%CAND_P(I) = ZERO
            ENDIF
          ENDDO
        ELSE
          DO I = DEBUT+1, DEBUT+I_STOK_LOC
            IF(INTBUF_TAB%CAND_N(I).LT.0) THEN
              I_STOK = I_STOK + 1
              INDEX2(I_STOK) = I
C inbuf == cand_n
              INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
            ENDIF
          ENDDO
        ENDIF
C
c------------------------------------------------
c        I_STOK = NUM_IMP
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     2         CAND_E_N,CAND_N_N)
          CALL I7CORK3(JLT ,X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     1                CAND_E_N ,CAND_N_N ,INTBUF_TAB%STFM,
     2                INTBUF_TAB%STFNS,X1 ,X2  ,
     2                X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     3                Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     4                XI      ,YI    ,ZI    ,STIF  ,IX1    ,
     5                IX2     ,IX3   ,IX4   ,NSVG  ,IGAP   ,
     6                GAPMIN  ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,GAPV,
     7                MS      ,VXI   ,VYI   ,VZI   ,MSI   ,
     8                V      ,IGSTI ,KMIN   ,KMAX    ,GAPMAX,
     9                GAPMIN ,NIN    ,NTY   ,NSN    )
          CALL I7DSTK3(
     1                  JLT    ,CAND_N_N,CAND_E_N ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     5                  NX1    ,NX2    ,NX3    ,NX4    ,NY1    ,
     6                  NY2    ,NY3    ,NY4    ,NZ1    ,NZ2    ,
     7                  NZ3    ,NZ4    ,LB1    ,LB2    ,LB3    ,
     8                  LB4    ,LC1    ,LC2    ,LC3    ,LC4    ,
     9                  P1     ,P2     ,P3     ,P4     ,IX1    ,
     A                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     B                  I3N    ,GAPV   ,INACTI ,INTBUF_TAB%CAND_P  ,
     C                  INDEX2(NFT+1))
           CALL I7DSTR3(
     1                  JLT    ,CAND_N_N,CAND_E_N,CN_LOC ,CE_LOC ,
     2                  P1     ,P2     ,P3     ,P4     ,STIF   ,
     3                  GAPV   ,INACTI ,INTBUF_TAB%CAND_P,INDEX2(NFT+1),
     4                  JLT_NEW)
C
            DO I = 1 ,JLT_NEW
             NS_IMP(I+NUM_IMP)=CN_LOC(I)
             NE_IMP(I+NUM_IMP)=CE_LOC(I)
             IND_IMP(I+NUM_IMP)=INDEX2(I+NFT)
            ENDDO
            NUM_IMP=NUM_IMP+JLT_NEW
        ENDDO
C
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I7FKU3                        source/interfaces/int07/i7ke3.F
Chd|-- called by -----------
Chd|        INT_FKU3                      source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I7CORK3                       source/interfaces/int07/i7cork3.F
Chd|        I7CORKP3                      source/interfaces/int07/i7cork3.F
Chd|        I7DSTK3                       source/interfaces/int07/i7dstk3.F
Chd|        I7KFOR3                       source/interfaces/int07/i7keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I7FKU3(A      ,V     ,MS     ,D     ,
     1                  IPARI ,INTBUF_TAB,X    ,NIN   ,
     2                  NUM_IMP,CAND_N,CAND_E ,INDEX2,IUPD  )
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "scr05_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), INDEX2(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),NIN
C     REAL
      my_real 
     .  MS(*),V(3,*),X(*),A(3,*),D(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI,IUPD
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), I3N 
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,GAP_IMP
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), OFF(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),
     .    DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ),
     .    MSI(MVSIZ)
C   ---- A(3,I) input:D(3,I); output:Fint(3,I)----- 
      IF(NUM_IMP.EQ.0) RETURN
C
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
      IF(NTY.NE.7)RETURN
      IF(IPARI(33,NIN).EQ.1) RETURN
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      STIGLO=-INTBUF_TAB%STFAC(1)
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      IGSTI=IPARI(34,NIN) 
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
      MFROT =IPARI(30,NIN)
      IFQ =IPARI(31,NIN) 
C
      IF(NTY.EQ.3)THEN
      ELSEIF(NTY.EQ.4)THEN
      ELSEIF(NTY.EQ.5)THEN
      ELSEIF(NTY.EQ.6)THEN
C
      ELSEIF(NTY.EQ.7)THEN
C
        I_STOK = NUM_IMP
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I7CORK3(JLT ,X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     1                CAND_E(NFT+1)  ,CAND_N(NFT+1) ,INTBUF_TAB%STFM,
     2                INTBUF_TAB%STFNS,X1 ,X2  ,
     2                X3      ,X4    ,Y1    ,Y2    ,Y3     ,
     3                Y4      ,Z1    ,Z2    ,Z3    ,Z4     ,
     4                XI      ,YI    ,ZI    ,STIF  ,IX1    ,
     5                IX2     ,IX3   ,IX4   ,NSVG  ,IGAP   ,
     6                GAPMIN  ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,GAPV,
     7                MS      ,VXI   ,VYI   ,VZI   ,MSI   ,
     8                V      ,IGSTI ,KMIN   ,KMAX    ,GAPMAX,
     9                GAPMIN ,NIN    ,NTY   ,NSN    )
         CALL I7CORKP3(
     1                    JLT   ,XI      ,YI     ,ZI     ,D      ,   
     2                    DXI   ,DYI     ,DZI    ,NSVG   ,NIN    ,
     3                    IUPD    )
          CALL I7DSTK3(
     1                  JLT    ,CAND_N(NFT+1) ,CAND_E(NFT+1) ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     5                  NX1    ,NX2    ,NX3    ,NX4    ,NY1    ,
     6                  NY2    ,NY3    ,NY4    ,NZ1    ,NZ2    ,
     7                  NZ3    ,NZ4    ,LB1    ,LB2    ,LB3    ,
     8                  LB4    ,LC1    ,LC2    ,LC3    ,LC4    ,
     9                  P1     ,P2     ,P3     ,P4     ,IX1    ,
     A                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     B                  I3N    ,GAPV   ,INACTI ,INTBUF_TAB%CAND_P  ,
     C                  INDEX2(NFT+1))
           CALL I7KFOR3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     2                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     3                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     4                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     5                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     6                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     7                  GAPV   ,INACTI ,INTBUF_TAB%CAND_P,INDEX2(NFT+1),
     8                  STIGLO ,STIF   ,VXI    ,VYI    ,VZI   ,MSI    ,
     9                  X      ,INTBUF_TAB%IRECTM,CAND_E(NFT+1),MFROT ,
     A                  IFQ    ,INTBUF_TAB%FRIC_P,INTBUF_TAB%FTSAVX,
     B                  INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,
     .                  INTBUF_TAB%XFILTR(1),
     C                  DXI    ,DYI    ,DZI    ,D     ,SK_INT ,
     D                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     E                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     F                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     G                  IPARI(39,NIN)  )
        ENDDO
C
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  FFIZERO                       source/interfaces/int07/i7ke3.F
Chd|-- called by -----------
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|        I7KE3                         source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE FFIZERO(JLT    ,NIN    ,NSN     ,CAND_N   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
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
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, CAND_N(*),NSN ,NIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NS,NI,NN
C-----------------------------------------------
C--------------------------------------------------------
C  Due to multi-contacts, zero initialization should be done globaly
C--------------------------------------------------------
       IF (NSPMD.GT.1) THEN
        DO I=1,JLT
         NI = CAND_N(I)
         IF(NI > NSN)THEN
          NN = NI - NSN
          NS=IND_INT(NIN)%P(NN)
C---------pour diag_ss---
          FFI(1,NS)=ZERO
          FFI(2,NS)=ZERO
          FFI(3,NS)=ZERO
          DFI(1,NS)=ZERO
          DFI(2,NS)=ZERO
          DFI(3,NS)=ZERO
         ENDIF
        ENDDO
       END IF
C
       RETURN
      END
