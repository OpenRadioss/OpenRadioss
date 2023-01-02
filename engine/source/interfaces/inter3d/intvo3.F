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
Chd|  INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- called by -----------
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|-- calls ---------------
Chd|        I3COR3                        source/interfaces/inter3d/i3cor3.F
Chd|        I3CST3                        source/interfaces/inter3d/i3cst3.F
Chd|        I3DIS3                        source/interfaces/inter3d/i3dis3.F
Chd|        I3FOR3                        source/interfaces/inter3d/i3for3.F
Chd|        I3FRI3                        source/interfaces/inter3d/i3fri3.F
Chd|        I3GAP3                        source/interfaces/inter3d/i3gap3.F
Chd|        I3LOC3                        source/interfaces/inter3d/i3loc3.F
Chd|        I3MSR3                        source/interfaces/inter3d/i3msr3.F
Chd|        I5FOR3                        source/interfaces/inter3d/i5for3.F
Chd|        I5FRI3                        source/interfaces/inter3d/i3fri3.F
Chd|        I5IMPK3                       source/interfaces/inter3d/i5cork3.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTVO3(IPARI,X ,A  ,
     2                  ICODT,FSAV ,MWA  ,V ,MS ,
     3                  FSKYI,ISKY ,FCONT,FNCONT,FTCONT,
     4                  ICONTACT,RCONTACT,NUM_IMP,NS_IMP  ,
     5                  NE_IMP  ,STIFN,ITAB,INTBUF_TAB,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
      USE H3D_MOD 
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
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "bucki_c.inc"
#include      "warn_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ICODT(*), MWA(*), ISKY(*),
     .        ICONTACT(*)
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),ITAB(*)
C     REAL
      my_real
     .   X(*), A(3,*), FSAV(*), V(3,*), MS(*),
     .   FSKYI(LSKYI,NFSKYI),FCONT(3,*), FNCONT(3,*), FTCONT(3,*),
     .   RCONTACT(*),STIFN(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),I
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   DIST(MVSIZ)
      INTEGER IBC, NCIMP, NRTM4, NOINT, NGROUS, NGROUM, NG,IGIMP, 
     .        NCI, I_STOK, NSEG, LLT_NEW, IDT, JFI, KFI
      INTEGER IFQ,MFROT,JD(50),KD(50),IBAG,IADM,IFORM,IFT0

      INTEGER, DIMENSION(MVSIZ) :: IX11,IX12,IX13,IX14
      my_real, DIMENSION(MVSIZ) :: X11,X12,X13,X14,XI
      my_real, DIMENSION(MVSIZ) :: Y11,Y12,Y13,Y14,YI
      my_real, DIMENSION(MVSIZ) :: Z11,Z12,Z13,Z14,ZI
      my_real, DIMENSION(MVSIZ) :: XFACE,N1,N2,N3
      my_real, DIMENSION(MVSIZ) :: SSC,TTC,AREA,THK,ALP
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,ANS,STIF
      my_real, DIMENSION(MVSIZ) :: XX1,XX2,XX3,XX4
      my_real, DIMENSION(MVSIZ) :: YY1,YY2,YY3,YY4
      my_real, DIMENSION(MVSIZ) :: ZZ1,ZZ2,ZZ3,ZZ4
      my_real, DIMENSION(MVSIZ) :: XI1,XI2,XI3,XI4
      my_real, DIMENSION(MVSIZ) :: YI1,YI2,YI3,YI4
      my_real, DIMENSION(MVSIZ) :: ZI1,ZI2,ZI3,ZI4
      my_real, DIMENSION(MVSIZ) :: XN1,XN2,XN3,XN4
      my_real, DIMENSION(MVSIZ) :: YN1,YN2,YN3,YN4
      my_real, DIMENSION(MVSIZ) :: ZN1,ZN2,ZN3,ZN4
      my_real, DIMENSION(MVSIZ) :: XP,YP,ZP
      my_real, DIMENSION(MVSIZ) :: H1,H2,H3,H4
C     REAL
      my_real
     .   STARTT, FRIC, GAP, MAXBOX, MINBOX, STOPT, FB_CAM, FB_IMM,
     .   VISC,VISCF,FNOR,DEPTH
C-----------------------------------------------
C----     
      NRTS   = IPARI(3)
      NRTM   = IPARI(4)
      NSN    = IPARI(5)
      NMN    = IPARI(6)
      IBC   =IPARI(11)
      IBUC  =IPARI(12)
      NCIMP =IPARI(13)
      NRTM4 =IPARI(14)
      NOINT =IPARI(15)
      MFROT =IPARI(30)
      IBAG =IPARI(32)
      IADM =IPARI(44)
      IFORM=IPARI(48)
      IFT0 =IPARI(50)
      NGROUS=1+(NSN-1)/NVSIZ
      NGROUM=1+(NMN-1)/NVSIZ
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      FRIC =INTBUF_TAB%VARIABLES(1)
      GAP  =INTBUF_TAB%VARIABLES(2)
      VISC =INTBUF_TAB%VARIABLES(14)
      VISCF=INTBUF_TAB%VARIABLES(15)
      FNOR =INTBUF_TAB%VARIABLES(4)
      DEPTH=INTBUF_TAB%VARIABLES(5)
C
      DIST = ZERO
C
      IF(NTY==3)THEN
       DO 100 NG=1,NGROUS
       NFT=(NG-1)*NVSIZ
       LFT=1
       LLT=MIN0(NVSIZ,NSN-NFT)
       CALL I3LOC3(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1             INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%NSEGM,XI,YI,ZI,
     2                  XFACE)
       CALL I3MSR3(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1          INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,XFACE)
       CALL I3COR3(
     1    X  ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     1                  IX11   ,IX12   ,IX13   ,IX14  ,X11   ,
     2                  X12    ,X13    ,X14    ,Y11   ,Y12   ,
     3                  Y13    ,Y14    ,Z11   ,Z12    ,Z13   ,
     4                  Z14    )
       CALL I3CST3(X11   ,X12     ,X13   ,X14   ,XI ,
     1             Y11   ,Y12     ,Y13   ,Y14   ,YI ,
     2             Z11   ,Z12     ,Z13   ,Z14   ,ZI ,
     3             XFACE ,N1      ,N2    ,N3   ,SSC,
     4             TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5             XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6             YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7             ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8             YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9             ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1             XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2             YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4,
     3             AREA)
       CALL I3GAP3(GAP,AREA,THK,ALP)
       CALL I3DIS3(IGIMP,NTY,DIST ,X11   ,X12,
     1             X13   ,X14     ,XI    ,Y11   ,Y12 ,
     2             Y13   ,Y14     ,YI    ,Z11   ,Z12 ,
     3             Z13   ,Z14     ,ZI    ,XFACE ,N1 ,
     4             N2    ,N3      ,SSC   ,TTC   ,ALP,
     5             ANS   ,XP      ,YP    ,ZP    ,H1 ,
     6             H2    ,H3      ,H4)
       IF(IGIMP==0)GOTO 100
       CALL I3FOR3(LFT   ,LLT   ,NFT   ,
     2      A ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%STFM,
     3      INTBUF_TAB%STFNS,IBC ,ICODT  ,1   ,FSAV ,
     4      IGIMP     ,FSKYI     ,ISKY      ,FCONT  ,H3D_DATA,
     5      N1    ,N2    ,N3    ,IX11  ,IX12     ,
     6      IX13  ,IX14  ,ANS   ,
     7      THK   ,H1    ,H2    ,H3    ,H4       ,
     8      XFACE ,STIF  ,XX3   ,YY3   ,ZZ3      ,
     7      XX4   ,YY4   ,YI1   ,YI2   ,YI3      ,
     8      ZZ4   ,ZI1   ,ZI2   ,ZI3   ,XI1      ,
     9      XI2   ,XI3   ,XI4)
       IF(FRIC==0.)GOTO 100
       IF(IGIMP==0)GOTO 100
       CALL I3FRI3(LFT   ,LLT   ,NFT   ,X ,A  ,
     2      INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     3      NTY,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     4      INTBUF_TAB%VARIABLES(1),1,FSAV,FSKYI,ISKY,
     5      FCONT,H3D_DATA,N1   ,N2    ,N3    ,
     6      IX11  ,IX12  ,IX13  ,IX14  ,H1    ,
     7      H2    ,H3    ,H4    ,SSC   ,TTC   ,
     8      XFACE ,STIF  ,XP    ,YP    ,ZP    ,
     9      XX3   )!,YY3   ,ZZ3   ,XX4   ,YY4   ,
!     1      YI1   ,YI2   ,YI3   ,ZZ3   ,ZI1   ,
!     2      ZI2   ,ZI3   ,XI1   ,XI2   ,XI3   ,
!     3      XI4)
 100   CONTINUE
       DO 130 NG=1,NGROUM
       NFT=(NG-1)*NVSIZ
       LFT=1
       LLT=MIN0(NVSIZ,NMN-NFT)
       CALL I3LOC3(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     1          INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,INTBUF_TAB%NSEGS,XI,YI,ZI,
     2          XFACE)
       CALL I3MSR3(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     1      INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,XFACE)
       CALL I3COR3(X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,
     1             IX11   ,IX12   ,IX13   ,IX14  ,X11   ,
     2             X12    ,X13    ,X14    ,Y11   ,Y12   ,
     3             Y13    ,Y14    ,Z11   ,Z12    ,Z13   ,
     4             Z14    )
       CALL I3CST3(X11   ,X12     ,X13   ,X14   ,XI ,
     1             Y11   ,Y12     ,Y13   ,Y14   ,YI ,
     2             Z11   ,Z12     ,Z13   ,Z14   ,ZI ,
     3             XFACE ,N1      ,N2    ,N3   ,SSC,
     4             TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5             XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6             YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7             ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8             YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9             ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1             XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2             YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4,
     3             AREA)
       CALL I3GAP3(GAP,AREA,THK,ALP)
       CALL I3DIS3(IGIMP,NTY,DIST,X11   ,X12,
     1             X13   ,X14     ,XI    ,Y11   ,Y12 ,
     2             Y13   ,Y14     ,YI    ,Z11   ,Z12 ,
     3             Z13   ,Z14     ,ZI    ,XFACE ,N1 ,
     4             N2    ,N3      ,SSC   ,TTC   ,ALP,
     5             ANS   ,XP      ,YP    ,ZP    ,H1 ,
     6             H2    ,H3      ,H4)
       IF(IGIMP==0)GOTO 130
       CALL I3FOR3(LFT   ,LLT   ,NFT   , A  ,
     2      INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,INTBUF_TAB%STFS,
     3      INTBUF_TAB%STFNM,IBC  ,ICODT        ,-1        ,FSAV      ,
     4      IGIMP  ,FSKYI     ,ISKY      ,FCONT     ,H3D_DATA, 
     5      N1    ,N2    ,N3    ,IX11  ,IX12     ,
     6      IX13  ,IX14  ,ANS   ,
     7      THK   ,H1    ,H2    ,H3    ,H4       ,
     8      XFACE ,STIF  ,XX3   ,YY3   ,ZZ3      ,
     7      XX4   ,YY4   ,YI1   ,YI2   ,YI3      ,
     8      ZZ4   ,ZI1   ,ZI2   ,ZI3   ,XI1      ,
     9      XI2   ,XI3   ,XI4)
       IF(FRIC==0.)GOTO 130
       IF(IGIMP==0)GOTO 130
       IPARI(29) = 1
       CALL I3FRI3(LFT   ,LLT   ,NFT   ,X      ,A    ,
     2      INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,
     3      NTY,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%FRICOM,
     4      INTBUF_TAB%VARIABLES(1),-1  ,FSAV  ,FSKYI  ,ISKY  ,FCONT,H3D_DATA  ,N1   ,N2    ,N3    ,
     6      IX11  ,IX12  ,IX13  ,IX14  ,H1    ,
     7      H2    ,H3    ,H4    ,SSC   ,TTC   ,
     8      XFACE ,STIF  ,XP    ,YP    ,ZP    ,
     9      XX3   )!,YY3   ,ZZ3   ,XX4   ,YY4   ,
!     1      YI1   ,YI2   ,YI3   ,ZZ4   ,ZI1   ,
!     2      ZI2   ,ZI3   ,XI1   ,XI2   ,XI3   ,
!     3      XI4)

 130   CONTINUE
C
      ELSEIF(NTY==4)THEN
C
      ELSEIF(NTY==5)THEN
        IF (IMPL_S==1) THEN
         NUM_IMP = 0
         VISC  =ZERO
         VISCF =ZERO
        ENDIF
        DO 150 NG=1,NGROUS
        NFT=(NG-1)*NVSIZ
        LFT=1
        LLT=MIN0(NVSIZ,NSN-NFT)
        CALL I3LOC3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .          INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%NSEGM,XI,YI,ZI,
     2                  XFACE)
        CALL I3MSR3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .          INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,XFACE)
        CALL I3COR3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     .              INTBUF_TAB%IRTLM,
     1              IX11   ,IX12   ,IX13   ,IX14  ,X11   ,
     2              X12    ,X13    ,X14    ,Y11   ,Y12   ,
     3              Y13    ,Y14    ,Z11   ,Z12    ,Z13   ,
     4              Z14    )
        CALL I3CST3(X11   ,X12     ,X13   ,X14   ,XI ,
     1             Y11   ,Y12     ,Y13   ,Y14   ,YI ,
     2             Z11   ,Z12     ,Z13   ,Z14   ,ZI ,
     3             XFACE ,N1      ,N2    ,N3   ,SSC,
     4             TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5             XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6             YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7             ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8             YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9             ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1             XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2             YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4,
     3             AREA)
        CALL I3GAP3(GAP,AREA,THK,ALP)
        CALL I3DIS3(IGIMP,NTY,DIST,X11   ,X12,
     1             X13   ,X14     ,XI    ,Y11   ,Y12 ,
     2             Y13   ,Y14     ,YI    ,Z11   ,Z12 ,
     3             Z13   ,Z14     ,ZI    ,XFACE ,N1 ,
     4             N2    ,N3      ,SSC   ,TTC   ,ALP,
     5             ANS   ,XP      ,YP    ,ZP    ,H1 ,
     6             H2    ,H3      ,H4)
        IF(IGIMP==0)GOTO 150
        CALL I5FOR3(LFT   ,LLT   ,NFT   , A ,
     2          INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%STFM,
     3          INTBUF_TAB%STFNS,IBC   ,ICODT     ,FSAV      ,IGIMP     ,
     4          FSKYI     ,ISKY     ,FCONT     ,FNCONT    ,ICONTACT  ,
     5          IBAG      ,IADM    ,H3D_DATA,
     6          IX11  ,IX12  ,IX13    ,IX14   ,N1    ,
     7          N2    ,N3    ,XFACE   ,H1     ,H2    ,
     8          H3    ,H4    ,THK     ,ANS    ,STIF  ,
     9          XX3 )
        IF (IMPL_S==1) THEN
         CALL I5IMPK3(INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     .                INTBUF_TAB%IRTLM,NUM_IMP,NS_IMP,NE_IMP,ANS)
        ENDIF
        IF(FRIC==0.AND.MFROT==0)GOTO 150
        IF(IGIMP==0)GOTO 150
        IPARI(29) = 1
        CALL I5FRI3(LFT    ,LLT    ,NFT    ,IPARI  ,X      ,A      ,
     2          INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     3          NTY          ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     4          FRIC         ,1            ,FSAV         ,FSKYI        ,
     5          ISKY         ,FCONT        ,V          ,INTBUF_TAB%VARIABLES(4),
     6          INTBUF_TAB%FRIC_P,INTBUF_TAB%XFILTR,INTBUF_TAB%FTSAV,FTCONT,H3D_DATA ,
     7          N1    ,N2    ,
     7          N3    ,IX11  ,IX12    ,IX13  ,IX14  ,
     8          XP    ,YP    ,ZP      ,SSC   ,TTC   ,
     9          XFACE ,STIF  ,H1      ,H2    ,H3    ,
     1          H4    ,AREA  ,XX3)
 150    CONTINUE
C
      ELSEIF(NTY==6)THEN
C
      ELSEIF(NTY==7.OR.NTY==22)THEN
C
      ELSEIF(NTY==8)THEN

 180    CONTINUE

C
      ENDIF

      RETURN
      END
