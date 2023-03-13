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
Chd|  I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|-- called by -----------
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|-- calls ---------------
Chd|        I3COR3                        source/interfaces/inter3d/i3cor3.F
Chd|        I3CST3                        source/interfaces/inter3d/i3cst3.F
Chd|        I3DIS3                        source/interfaces/inter3d/i3dis3.F
Chd|        I3GAP3                        source/interfaces/inter3d/i3gap3.F
Chd|        I3LOC3                        source/interfaces/inter3d/i3loc3.F
Chd|        I3MSR3                        source/interfaces/inter3d/i3msr3.F
Chd|        I6ASS3                        source/interfaces/inter3d/i6ass3.F
Chd|        I6DAMP                        source/interfaces/inter3d/i6damp.F
Chd|        I6FOR3                        source/interfaces/inter3d/i6for3.F
Chd|        I6FRI3                        source/interfaces/inter3d/i6fri3.F
Chd|        I6INI3                        source/interfaces/inter3d/i6ini3.F
Chd|        I6IPMACT                      source/interfaces/inter3d/i6impact.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I6MAIN(IPARI,X     ,A   ,
     2                  ICODT,NPC   ,TF     ,V     ,FSAV,
     3                  DT2T ,NELTST,ITYPTST,FSKYI ,ISKY,
     4                  FCONT,MS,INTBUF_TAB,H3D_DATA)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect07_c.inc"
#include      "scr04_c.inc"
#include      "parit_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST
      INTEGER IPARI(*), ICODT(*), NPC(*), ISKY(*)
C     REAL
      my_real DT2T,
     .   X(*),A(*),TF(*),V(*),FSAV(*),FSKYI(LSKYI,NFSKYI),
     .   FCONT(3,*),MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KD(50), JD(50), KFI, JFI, 
     .   INACTI,NOINT, NGROUS, NGROUM, NG, IGIMP,INTY,IFRICF,IFRICV,
     .   IDAMPV,IDAMPF
      INTEGER LOLD(MVSIZ)
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
     .   VNI(MVSIZ),VNT(MVSIZ)
      my_real
     .   ANSMX,ASCALV,ASCALF,FSCALV,FMX,FMY,FMZ,STARTT,FRIC,SFRIC,VISC, 
     .   STIFF, GAP, STOPT,DIST
C=======================================================================
      NSN   = IPARI(5)
      NMN   = IPARI(6)
      INTY  = IPARI(7)
      NOINT = IPARI(15)
      INACTI= IPARI(22)
      IFRICF = IPARI(51)
      IDAMPV = IPARI(52)
      IDAMPF = IPARI(53)
      IFRICV = IPARI(54)
C
      ANSMX =EP15
      FMX=ZERO
      FMY=ZERO
      FMZ=ZERO
C
      NGROUS=1+(NSN-1)/NVSIZ
      NGROUM=1+(NMN-1)/NVSIZ
C----
      STARTT=INTBUF_TAB%VARIABLES(3)
      IF(STARTT > TT) RETURN
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(TT > STOPT)  RETURN
c
      FRIC  = INTBUF_TAB%VARIABLES(1)
      GAP   = INTBUF_TAB%VARIABLES(2)
      SFRIC = INTBUF_TAB%VARIABLES(5)
      VISC  = INTBUF_TAB%VARIABLES(14)
      STIFF = INTBUF_TAB%VARIABLES(21)
      ASCALF= INTBUF_TAB%VARIABLES(22)
      ASCALV= INTBUF_TAB%VARIABLES(23)
      FSCALV= INTBUF_TAB%VARIABLES(24)
c
      CALL I6INI3(INTBUF_TAB%FS,INTBUF_TAB%FM,NSN,NMN)
c
c---  Loop over secnd group
c
      DO NG=1,NGROUS
        NFT=(NG-1)*NVSIZ
        LFT=1
        LLT=MIN0(NVSIZ,NSN-NFT)
        CALL I3LOC3(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .        INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%NSEGM,XI   ,YI     ,ZI,
     2                  XFACE)
        CALL I3MSR3(X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .        INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,XFACE)
        CALL I3COR3(X	   ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,
     1                  IX1   ,IX2   ,IX3   ,IX4   ,X1   ,
     2                  X2    ,X3    ,X4    ,Y1    ,Y2   ,
     3                  Y3    ,Y4    ,Z1    ,Z2    ,Z3   ,
     4                  Z4    )
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
        CALL I3DIS3(IGIMP,INTY,DIST,X1    ,X2 ,
     1              X3   ,X4  ,XI  ,Y1    ,Y2 ,
     2              Y3   ,Y4  ,YI  ,Z1    ,Z2 ,
     3              Z3   ,Z4  ,ZI  ,XFACE ,N1 ,
     4              N2   ,N3  ,SSC ,TTC   ,ALP,
     5              ANS  ,XP  ,YP  ,ZP    ,H1 ,
     6              H2   ,H3  ,H4)
        IF(IGIMP == 0)CYCLE

        CALL I6FOR3(
     .      INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM         ,  
     .      INTBUF_TAB%STFM,INTBUF_TAB%STFNS,IGIMP                 ,INTBUF_TAB%FS    , 
     .      INTBUF_TAB%FM,ANSMX        , FMX     ,FMY   , FMZ      ,INTBUF_TAB%FCONT ,
     4      N1  ,N2    ,N3    ,XFACE ,ANS   ,
     5      H1    ,H2    ,H3    ,H4    ,THK,
     6      XX3   ,YY3   ,ZZ3   ,XX4   ,YY4   ,
     7      YI1   ,YI2   ,YI3   ,ZZ4   ,ZI1   ,
     8      ZI2   ,ZI3   ,XI1   ,XI2   ,XI3   ,
     9      XI4)
c
        IF (VISC > ZERO .or. IDAMPV > 0 .or. FRIC > ZERO) 
     .    CALL I6IPMACT(
     1         LOLD  ,INTBUF_TAB%IRTLM,INTBUF_TAB%IRTLOM,INTBUF_TAB%CSTS,
     2         INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,V      ,
     3         VNI            ,VNT          ,N1      ,N2    ,
     4         N3     ,SSC    ,TTC     ,H1    ,
     5         H2     ,H3     ,H4      ,XFACE)

        IF (VISC > ZERO .or. IDAMPV > 0) 
     .    CALL I6DAMP(
     .         V            ,NPC          ,TF           ,INTBUF_TAB%IRECTM,
     1         INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%IRTLOM,
     2         INTBUF_TAB%CSTS,INTBUF_TAB%FS,INTBUF_TAB%FM,VISC       ,
     3         IDAMPV       ,IDAMPF       ,LOLD           ,MS           ,
     4         VNI          ,ASCALF       ,ASCALV         ,FSCALV       ,
     5         H1           ,H2           ,H3             ,H4           ,
     6         XX3          ,YY3          ,ZZ3            ,XX4          ,
     7         YY4          ,YI1          ,YI2            ,YI3          ,
     8         ZZ4          ,ZI1          ,ZI2            ,ZI3          ,
     9         XI1          ,XI2          ,XI3            ,XI4)
c
        IF (FRIC > ZERO)
     .  CALL I6FRI3(X           ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,
     .       INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,
     .       INTBUF_TAB%FRICOS,FRIC   ,INTBUF_TAB%FS,INTBUF_TAB%FM,
     .       SFRIC        ,IFRICF     ,NPC          ,TF           ,LOLD    ,
     .       IFRICV       ,VNT        ,ASCALF       ,ASCALV       ,STIFF   ,
     5       N1     ,N2    ,N3    ,SSC   ,TTC ,
     6       XFACE  ,XP    ,YP    ,ZP    ,H1  ,
     7       H2     ,H3    ,H4    ,XX3   ,YY3 ,
     7       ZZ3    ,XX4   ,YY4   ,YI1   ,YI2 ,
     8       YI3    ,ZZ4   ,ZI1   ,ZI2   ,ZI3 ,
     9       XI1    ,XI2   ,XI3   ,XI4)
c

      ENDDO
c------------------
C      INVERSION DES FM (PASSAGE SECND/MAIN)
c
       FMX=-FMX
       FMY=-FMY
       FMZ=-FMZ
c      
c---  Loop over main group 
c       
       DO NG=1,NGROUM
         NFT=(NG-1)*NVSIZ
         LFT=1
         LLT=MIN0(NVSIZ,NMN-NFT)
         CALL I3LOC3(X	   ,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     .        INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,INTBUF_TAB%NSEGS,XI   ,YI     ,ZI,
     2                  XFACE)
         CALL I3MSR3(X	   ,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     .        INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,XFACE)
         CALL I3COR3(X	   ,INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,
     1               IX1   ,IX2              ,IX3           ,IX4           ,X1              ,
     2               X2    ,X3               ,X4            ,Y1            ,Y2              ,
     3               Y3    ,Y4               ,Z1            ,Z2            ,Z3              ,
     4               Z4    )

         CALL I3CST3(X1    ,X2      ,X3    ,X4    ,XI ,
     1               Y1    ,Y2      ,Y3    ,Y4    ,YI ,
     2               Z1    ,Z2      ,Z3    ,Z4    ,ZI ,
     3               XFACE ,N1      ,N2    ,N3    ,SSC,
     4               TTC   ,X0      ,Y0    ,Z0    ,XX1,
     5               XX2   ,XX3     ,XX4   ,YY1   ,YY2,
     6               YY3   ,YY4     ,ZZ1   ,ZZ2   ,ZZ3,
     7               ZZ4   ,XI1     ,XI2   ,XI3   ,XI4,
     8               YI1   ,YI2     ,YI3   ,YI4   ,ZI1,
     9               ZI2   ,ZI3     ,ZI4   ,XN1   ,XN2,
     1               XN3   ,XN4     ,YN1   ,YN2   ,YN3,
     2               YN4   ,ZN1     ,ZN2   ,ZN3   ,ZN4,
     3               AREA)
         CALL I3GAP3(GAP,AREA,THK,ALP)
         CALL I3DIS3(IGIMP,INTY,DIST,X1    ,X2 ,
     1               X3   ,X4  ,XI  ,Y1    ,Y2 ,
     2               Y3   ,Y4  ,YI  ,Z1    ,Z2 ,
     3               Z3   ,Z4  ,ZI  ,XFACE ,N1 ,
     4               N2   ,N3  ,SSC ,TTC   ,ALP,
     5               ANS  ,XP  ,YP  ,ZP    ,H1 ,
     6               H2   ,H3  ,H4)

         IF (IGIMP == 0) CYCLE
c
         CALL I6FOR3(
     .  	INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,
     .  	INTBUF_TAB%STFS,INTBUF_TAB%STFNM,IGIMP	 ,INTBUF_TAB%FM,
     .  	INTBUF_TAB%FS,ANSMX	   ,FMX 	 ,FMY	  ,
     .  	FMZ	     ,INTBUF_TAB%FCONT,
     4      N1  ,N2    ,N3    ,XFACE ,ANS   ,
     5      H1    ,H2    ,H3    ,H4    ,THK,
     6      XX3   ,YY3   ,ZZ3   ,XX4   ,YY4   ,
     7      YI1   ,YI2   ,YI3   ,ZZ4   ,ZI1   ,
     8      ZI2   ,ZI3   ,XI1   ,XI2   ,XI3   ,
     9      XI4)
c
        IF (VISC > ZERO .or. IDAMPV > 0 .or. FRIC > ZERO) 
     .    CALL I6IPMACT(
     .         LOLD  ,INTBUF_TAB%IRTLS,INTBUF_TAB%IRTLOS,INTBUF_TAB%CSTM,
     .         INTBUF_TAB%IRECTS  ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,V      ,
     .         VNI    ,VNT    ,N1      ,N2    ,
     4         N3     ,SSC    ,TTC     ,H1    ,
     5         H2     ,H3     ,H4      ,XFACE)

        IF (VISC > ZERO .or. IDAMPV > 0) 
     .    CALL I6DAMP(
     .         V            ,NPC          ,TF           ,INTBUF_TAB%IRECTS,
     .         INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,INTBUF_TAB%IRTLOS,
     .         INTBUF_TAB%CSTM,INTBUF_TAB%FM,INTBUF_TAB%FS,VISC         ,
     .         IDAMPV       ,IDAMPF       ,LOLD           ,MS           ,
     .         VNI          ,ASCALF       ,ASCALV         ,FSCALV       ,
     5         H1           ,H2           ,H3             ,H4           ,
     6         XX3          ,YY3          ,ZZ3            ,XX4          ,
     7         YY4          ,YI1          ,YI2            ,YI3          ,
     8         ZZ4          ,ZI1          ,ZI2            ,ZI3          ,
     9         XI1          ,XI2          ,XI3            ,XI4)
c
         IF (FRIC > ZERO)
     .     CALL I6FRI3(X        ,INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,
     .      INTBUF_TAB%MSR,INTBUF_TAB%IRTLS,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,
     .      INTBUF_TAB%FRICOM,FRIC     ,INTBUF_TAB%FM,INTBUF_TAB%FS,
     .      SFRIC        ,IFRICF    ,NPC          ,TF        ,LOLD    ,
     .      IFRICV       ,VNT       ,ASCALF       ,ASCALV    ,STIFF   ,
     5      N1           ,N2        ,N3           ,SSC       ,TTC ,
     6      XFACE  ,XP    ,YP    ,ZP    ,H1  ,
     7      H2     ,H3    ,H4    ,XX3   ,YY3 ,
     7      ZZ3    ,XX4   ,YY4   ,YI1   ,YI2 ,
     8      YI3    ,ZZ4   ,ZI1   ,ZI2   ,ZI3 ,
     9      XI1    ,XI2   ,XI3   ,XI4)
      ENDDO
C-----------
      CALL I6ASS3(
     1     A		 ,INTBUF_TAB%MSR ,INTBUF_TAB%NSV ,INTBUF_TAB%FS  ,	    
     2  INTBUF_TAB%FM  ,NPC	       ,TF	       ,ANSMX	       ,     
     3  FMX	       ,FMY	       ,FMZ	       ,INTBUF_TAB%VARIABLES(4),      
     4  IPARI(11)      ,V	       ,NOINT	       ,NSN	       ,      
     5  NMN	       ,FSAV	       ,DT2T	       ,NELTST         ,      
     6  ITYPTST        ,INTBUF_TAB%STFAC(1)  ,FSKYI	       ,ISKY	       ,     
     7  FCONT,INTBUF_TAB%VARIABLES(19),INTBUF_TAB%VARIABLES(20),STIFF    ,	
     8  IPARI(47)      ,IPARI(49)      ,IPARI(58)      ,INTBUF_TAB%FCONT  ,
     9  INTBUF_TAB%ANSMX0(1)  ,INTBUF_TAB%ANSMX0(2),H3D_DATA )   
C-----------
      RETURN
      END
