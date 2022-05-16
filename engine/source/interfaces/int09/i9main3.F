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
Chd|  I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|-- called by -----------
Chd|        INTAL1                        source/ale/intal1.F           
Chd|-- calls ---------------
Chd|        I3CST3                        source/interfaces/inter3d/i3cst3.F
Chd|        I3DIS3                        source/interfaces/inter3d/i3dis3.F
Chd|        I3GAP3                        source/interfaces/inter3d/i3gap3.F
Chd|        I3LOC3                        source/interfaces/inter3d/i3loc3.F
Chd|        I3MSR3                        source/interfaces/inter3d/i3msr3.F
Chd|        I9AVEL                        source/interfaces/int09/i9avel.F
Chd|        I9COR3                        source/interfaces/int09/i9cor3.F
Chd|        I9FRMS                        source/interfaces/int09/i9frms.F
Chd|        I9IMP3                        source/interfaces/int09/i9imp3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I9MAIN3(IPARI,INTBUF_TAB        ,X         ,V         ,
     1           A            ,ISKEW   ,SKEW     ,LCOD      ,FN        ,
     2           FT           ,MS      ,MSMN     ,MSMT      ,ITAB      ,
     3           FSAV        )
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
#include      "vect07_c.inc"
#include      "scr04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ISKEW(*), LCOD(*), ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(*), V(*), A(*), SKEW(*), FN(*), MSMN(*),
     .   FT(*), MSMT(*), MS(*) ,FSAV(NTHVKI,*)

       TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, IGIMP, NGROUS
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ) :: XFACE,N1,N2,N3
      my_real, DIMENSION(MVSIZ) :: SSC,TTC,AREA,THK,ALP
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,ANS
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
      my_real
     .   STARTT, FRIC, GAP, STOPT,DIST
C-----------------------------------------------
C
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      MST   =IPARI(9)
C
      IF(NMN.EQ.0)RETURN
C
      FRIC =INTBUF_TAB%VARIABLES(1)
      GAP  =INTBUF_TAB%VARIABLES(2)
      NGROUS=1+(NSN-1)/NVSIZ
C-----------------------------------------------------
C     RECHERCHE DES POINTS IMPACTES
C-----------------------------------------------------
        DO 150 NG=1,NGROUS
        NFT=(NG-1)*NVSIZ
        LFT=1
        LLT=MIN(NVSIZ,NSN-NFT)
        CALL I3LOC3(X     ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .          INTBUF_TAB%NSV,
     2          INTBUF_TAB%ILOCS,INTBUF_TAB%NSEGM,XI,YI,ZI,
     3          XFACE)
        CALL I3MSR3(X     ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     2          INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,XFACE)
        CALL I9COR3(X     ,V         ,         A,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,
     2          INTBUF_TAB%NSV,INTBUF_TAB%IRTLM ,IX1              ,IX2   ,IX3   ,
     3          IX4           ,X1               ,X2               ,X3    ,X4    ,
     4          XI            ,Y1               ,Y2               ,Y3    ,Y4    ,
     5          YI            ,Z1               ,Z2               ,Z3    ,Z4    ,
     6          ZI)
        CALL I3CST3(    X1    ,X2      ,X3    ,X4    ,XI ,
     1                  Y1    ,Y2      ,Y3    ,Y4    ,YI ,
     2                  Z1    ,Z2      ,Z3    ,Z4    ,ZI ,
     3                  XFACE ,N1      ,N2    ,N3    ,SSC,
     4                  TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5                  XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6                  YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7                  ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8                  YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9                  ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1                  XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2                  YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4, 
     3                  AREA)
        CALL I3GAP3(GAP,AREA,THK,ALP)
        CALL I3DIS3(IGIMP ,NTY     ,DIST  ,X1    ,X2 ,
     1              X3    ,X4      ,XI    ,Y1    ,Y2 ,
     2              Y3    ,Y4      ,YI    ,Z1    ,Z2 ,
     3              Z3    ,Z4      ,ZI    ,XFACE ,N1 ,
     4              N2    ,N3      ,SSC   ,TTC   ,ALP,
     5              ANS   ,XP      ,YP    ,ZP    ,H1 ,
     6              H2    ,H3      ,H4)
        CALL I9IMP3(INTBUF_TAB%ILOCS,INTBUF_TAB%CSTS,INTBUF_TAB%N,XFACE,N1  ,
     1              N2              ,N3             ,SSC         ,TTC  ,THK ,
     2              ANS)
 150    CONTINUE
C-----------------------------------------------------
C     SOMME DES FORCES ET MASSES
C-----------------------------------------------------
      CALL I9FRMS
     1   (X         ,SKEW      ,A         ,FN        ,FT        ,
     2    INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,
     .    INTBUF_TAB%NSV,
     3    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,MS        ,INTBUF_TAB%N,LCOD  ,
     4    ISKEW     ,FRIC      ,MSMN      ,MSMT      )
C-----------------------------------------------------
C     CALCUL DES ACCEL. MAINS ET VITESSES SECONDS
C-----------------------------------------------------
      CALL I9AVEL
     1   (X         ,SKEW      ,A         ,FN        ,FT        ,
     2    INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,
     .    INTBUF_TAB%NSV,
     3    INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,LCOD         ,V         ,ISKEW  ,
     4    INTBUF_TAB%N,MS        ,INTBUF_TAB%VARIABLES(1),MSMN      ,MSMT   ,
     5    INTBUF_TAB%STFNS,ITAB      ,FSAV      )
C
      RETURN
      END
