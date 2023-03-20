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
Chd|  INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- called by -----------
Chd|        INTFOP8                       source/interfaces/interf/intfop8.F
Chd|-- calls ---------------
Chd|        I8COR3                        source/interfaces/inter3d/i8cor3.F
Chd|        I8CST3                        source/interfaces/inter3d/i8cst3.F
Chd|        I8DIS3                        source/interfaces/inter3d/i8dis3.F
Chd|        I8FOR3                        source/interfaces/inter3d/i8for3.F
Chd|        I8GAP3                        source/interfaces/inter3d/i8gap3.F
Chd|        I8LOC3                        source/interfaces/inter3d/i8loc3.F
Chd|        I8MSR3                        source/interfaces/inter3d/i8msr3.F
Chd|        SPMD_I8_COMMSLV               source/mpi/interfaces/spmd_i8tool.F
Chd|        SPMD_I8_ILOC                  source/mpi/interfaces/spmd_i8tool.F
Chd|        SPMD_I8_INDEX                 source/mpi/interfaces/spmd_i8tool.F
Chd|        SPMD_I8_IRTL                  source/mpi/interfaces/spmd_i8tool.F
Chd|        SPMD_I8_UPDBUF                source/mpi/interfaces/spmd_i8tool.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INT8_MOD                      ../common_source/modules/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTVO8(IPARI,X ,A  ,
     2                  ICODT,FSAV ,V ,MS ,
     3                  FSKYI,ISKY ,FCONT,FNCONT,FTCONT,
     4                  ICONTACT,RCONTACT,
     5                  STIFN,ITAB,INTBUF_TAB, T8, H3D_DATA ,
     6                  NIN  , PSKIDS,TAGNCONT,KLOADPINTER,LOADPINTER,
     7                  LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD 
      USE INT8_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr04_c.inc"
#include      "bucki_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*), ICODT(*), ISKY(*),
     .        ICONTACT(*) ,NIN
      INTEGER ITAB(*),
     .   TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .   KLOADPINTER(NINTER+1),LOADPINTER(NINTER*NLOADP_HYD),
     .   LOADP_HYD_INTER(NLOADP_HYD)
C     REAL
      my_real
     .   X(*), A(3,*), FSAV(*), V(3,*), MS(*),
     .   FSKYI(LSKYI,NFSKYI),FCONT(3,*), FNCONT(3,*), FTCONT(3,*),
     .   RCONTACT(*),STIFN(*),PSKIDS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(INT8_STRUCT_) T8
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,SIZ
      INTEGER, DIMENSION(:,:), ALLOCATABLE  ::
     .  TAB_RMAX_UID
      INTEGER, DIMENSION(:), ALLOCATABLE  ::
     . HAS_MOVED,
     .   INDEX_IN_COMM,
     .   IX1, IX2, IX3,IX4,
     .   NSV_A, !NSV reduced only to active secnd on the proc
     .   NSV2,  !Numbering from NSV_A to NSV
     .   IRTL_A !IRTL reduced only to active secnd on the proc
C     REAL
      my_real, DIMENSION(:), ALLOCATABLE :: TAB_RMAX
      my_real, DIMENSION(:), ALLOCATABLE :: DISTANCE,DIST
      my_real, DIMENSION(:), ALLOCATABLE :: X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3
      my_real, DIMENSION(:), ALLOCATABLE :: X4,Y4,Z4
      my_real, DIMENSION(:), ALLOCATABLE :: XI,XI_A,YI,YI_A,ZI,ZI_A
      my_real, DIMENSION(:), ALLOCATABLE :: XP,YP,ZP,N1,N2,N3
      my_real, DIMENSION(:), ALLOCATABLE :: ANS,SSC,TTC
      my_real, DIMENSION(:), ALLOCATABLE :: H1,H2,H3,H4
      my_real, DIMENSION(:), ALLOCATABLE :: FACE,FACE_A,STIF
      my_real, DIMENSION(:), ALLOCATABLE :: X0,XX1,XX2,XX3,XX4
      my_real, DIMENSION(:), ALLOCATABLE :: Y0,YY1,YY2,YY3,YY4
      my_real, DIMENSION(:), ALLOCATABLE :: Z0,ZZ1,ZZ2,ZZ3,ZZ4
      my_real, DIMENSION(:), ALLOCATABLE :: XI1,XI2,XI3,XI4
      my_real, DIMENSION(:), ALLOCATABLE :: YI1,YI2,YI3,YI4
      my_real, DIMENSION(:), ALLOCATABLE :: ZI1,ZI2,ZI3,ZI4
      my_real, DIMENSION(:), ALLOCATABLE :: XN1,XN2,XN3,XN4
      my_real, DIMENSION(:), ALLOCATABLE :: YN1,YN2,YN3,YN4
      my_real, DIMENSION(:), ALLOCATABLE :: ZN1,ZN2,ZN3,ZN4
      my_real, DIMENSION(:), ALLOCATABLE :: THK,AREA,ALP
      my_real, DIMENSION(:), ALLOCATABLE :: DISTLIN
      INTEGER IBC, NCIMP, NRTM4, NOINT,IGIMP,NINSKID
      INTEGER MFROT,IBAG,IADM,IFORM,IFT0,IFLINEAR
C     REAL
      my_real
     .   STARTT, FRIC, GAP,  STOPT,  
     .   VISC,VISCF,FNOR,DEPTH,FRIC_LAST,FNOR_LAST

C     INTEGER aa,bb,cc,dd 
C     INTEGER FLAG
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
      IFLINEAR = IPARI(49)
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
      FRIC_LAST =INTBUF_TAB%VARIABLES(6)
      FNOR_LAST =INTBUF_TAB%VARIABLES(7)
C
C
      IF(NTY==3)THEN
C
      ELSEIF(NTY==8)THEN
        ALLOCATE(
     .  DISTANCE(NSN),
     .  DIST(NSN),
     .  IX1(NSN),
     .  IX2(NSN),
     .  IX3(NSN),
     .  IX4(NSN),
     .  X1(NSN), X2(NSN), X3(NSN), X4(NSN),
     .  Y1(NSN), Y2(NSN), Y3(NSN), Y4(NSN),
     .  Z1(NSN), Z2(NSN), Z3(NSN), Z4(NSN),
     .  XI(NSN), XI_A(NSN),
     .  YI(NSN), YI_A(NSN),
     .  ZI(NSN), ZI_A(NSN),
     .  XP(NSN), N1(NSN),
     .  YP(NSN), N2(NSN),
     .  ZP(NSN), N3(NSN), 
     .  ANS(NSN),SSC(NSN),TTC(NSN),
     .  H1(NSN),H2(NSN),H3(NSN),H4(NSN),
     .  FACE(NSN),FACE_A(NSN),
     .  STIF(NSN), 
     .  X0(NSN), XX1(NSN), XX2(NSN),
     .  Y0(NSN), YY1(NSN), YY2(NSN),
     .  Z0(NSN), ZZ1(NSN), ZZ2(NSN),
     .  XX3(NSN), XX4(NSN), XI1(NSN),
     .  YY3(NSN), YY4(NSN), YI1(NSN),
     .  ZZ3(NSN), ZZ4(NSN), ZI1(NSN),
     .  XI2(NSN), XI3(NSN), XI4(NSN),
     .  YI2(NSN), YI3(NSN), YI4(NSN),
     .  ZI2(NSN), ZI3(NSN), ZI4(NSN),
     .  XN1(NSN), XN2(NSN), XN3(NSN), XN4(NSN),
     .  YN1(NSN), YN2(NSN), YN3(NSN), YN4(NSN),
     .  ZN1(NSN), ZN2(NSN), ZN3(NSN), ZN4(NSN),
     .  THK(NSN), AREA(NSN),ALP(NSN),
     .  HAS_MOVED(NSN),
     .  TAB_RMAX(NSN),TAB_RMAX_UID(4,NSN),
     .  IRTL_A(NSN),NSV_A(NSN),NSV2(NSN),
     .  INDEX_IN_COMM(NMN),DISTLIN(NSN))

        DIST = 0
        DISTLIN(1:NSN) = ZERO
!       DO 180 NG=1,NGROUS
        NFT=0
        LFT=1
        LLT=NSN                   
        IF(NSPMD > 1) THEN
        ! WRITE(6,*) "================================="
        ! WRITE(6,*) __FILE__,__LINE__,T8%S_COMM,NMN,NSN
        ! WRITE(6,*) "================================="

          CALL SPMD_I8_INDEX(NMN,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM,T8%S_COMM)

          IF(T8%IS_ACTIVATED == 0) THEN
            CALL SPMD_I8_COMMSLV(NSN,
     .              INTBUF_TAB%ILOCS,INTBUF_TAB%NSV,
     .              ITAB,T8%BUFFER,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM)

            CALL SPMD_I8_UPDBUF(NSN, INTBUF_TAB%ILOCS,INTBUF_TAB%NSV,
     .            ITAB,T8%BUFFER,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM)
            T8%IS_ACTIVATED = 1
          ENDIF

        ENDIF

        CALL I8LOC3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .          INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,INTBUF_TAB%NSEGM,
     .          XI,YI,ZI,FACE, ITAB,DISTANCE,IFLINEAR,DISTLIN)
        IF(NSPMD > 1) THEN

          CALL SPMD_I8_ILOC(INTBUF_TAB%ILOCS,    INTBUF_TAB%MSR,ITAB,T8%BUFFER,
     .                      DISTANCE)

          CALL SPMD_I8_COMMSLV(NSN,
     .              INTBUF_TAB%ILOCS,INTBUF_TAB%NSV,
     .              ITAB,T8%BUFFER,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM)
          
          CALL SPMD_I8_UPDBUF(NSN,INTBUF_TAB%ILOCS,INTBUF_TAB%NSV,
     .              ITAB,T8%BUFFER,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM)
        ENDIF
        CALL I8MSR3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     .              INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     .              INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,FACE,
     .              NSN,ITAB, HAS_MOVED,TAB_RMAX,TAB_RMAX_UID)

        IF(NSPMD > 1) THEN
         CALL SPMD_I8_IRTL(INTBUF_TAB%IRTLM,HAS_MOVED,
     .                    TAB_RMAX,TAB_RMAX_UID,
     .                    ITAB,T8%BUFFER)

        ENDIF

        NFT=0
        LFT=1
        LLT=0                   
        NSV_A(1:NSN) = 0
        IRTL_A(1:NSN) = 0
        NSV2(1:NSN) = 0
        XI_A(1:NSN) = 0
        YI_A(1:NSN) = 0
        ZI_A(1:NSN) = 0
        FACE_A(1:NSN) = 0

        ! Compact NSV,IRTL,XI,ZI and FACE
        ! by keeping only secnd nodes that are 
        ! active on this SPMD domain
        DO I = 1,NSN
           IF(INTBUF_TAB%ILOCS(I) > -1 .AND.
     .        INTBUF_TAB%IRTLM(I) > 0) THEN
             LLT = LLT +1 
             NSV2(LLT) = I
             NSV_A(LLT) = INTBUF_TAB%NSV(I)
             IRTL_A(LLT) = INTBUF_TAB%IRTLM(I)
             XI_A(LLT) = XI(I)
             YI_A(LLT) = YI(I)
             ZI_A(LLT) = ZI(I)
             FACE_A(LLT) = FACE(I)
             
           ELSEIF (IFORM==2) THEN
            !WRITE(6,*) "S_FTSAVX=",INTBUF_TAB%S_FTSAVX,NSN 
            INTBUF_TAB%FTSAVX(I) = 0
            INTBUF_TAB%FTSAVY(I) = 0
            INTBUF_TAB%FTSAVZ(I) = 0
           ENDIF 
        ENDDO 
  
        NINSKID = 0
        IF(H3D_DATA%N_SCAL_SKID > 0) THEN
           NINSKID = H3D_DATA%N_SKID_INTER(NIN)
        ENDIF

        CALL I8COR3(X ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,NSV_A,
     .  	IRTL_A, 
     . IX1 ,IX2 ,IX3, 
     . IX4, X1 ,Y1,
     . Z1, X2 ,Y2 ,Z2,
     . X3 ,Y3 ,Z3,
     . X4 ,Y4 ,Z4)
        CALL I8CST3(X1 ,Y1 ,Z1,
     .              X2 ,Y2 ,Z2,
     .              X3 ,Y3 ,Z3,
     .              X4,Y4,Z4,
     .              XI_A,YI_A,ZI_A,
     .              N1,N2,N3,
     .               ANS, SSC, TTC,
     .              FACE_A,
     .                X0,  Y0,  Z0,
     .               XX1, YY1, ZZ1,
     .               XX2, YY2, ZZ2,
     .               XX3, YY3, ZZ3,
     .               XX4, YY4, ZZ4,
     .               XI1, YI1, ZI1,
     .               XI2, YI2, ZI2,
     .               XI3, YI3, ZI3,
     .               XI4, YI4, ZI4,
     .               XN1, YN1, ZN1,
     .               XN2, YN2, ZN2,
     .               XN3, YN3, ZN3,
     .               XN4, YN4, ZN4,
     .              AREA)

        CALL I8GAP3(GAP, THK,AREA, ALP)
        CALL I8DIS3(IGIMP,NTY,DIST,
     .              X1 ,Y1 ,Z1,
     .              X2 ,Y2 ,Z2,
     .              X3 ,Y3 ,Z3,
     .              X4,Y4,Z4,
     .              XI_A,YI_A,ZI_A,
     .              XP,YP,ZP,
     .              N1,N2,N3,
     .               ANS, SSC, TTC,
     .                H1,  H2,  H3,
     .                H4,FACE_A, ALP )

        CALL I8FOR3(LFT  ,LLT	,NFT   ,A   ,
     2          INTBUF_TAB%MSR,NSV_A,IRTL_A,INTBUF_TAB%STFM,
     .          INTBUF_TAB%NSV,NSV2, INTBUF_TAB%ILOCS, 
     3          INTBUF_TAB%STFNS,IBC     ,ICODT	    ,FSAV      ,IGIMP  , 
     4          X	     ,V       ,MS	    ,FRIC      ,NSN    ,
     5          FSKYI	     ,ISKY    ,FCONT	    ,RCONTACT  ,IFORM  ,
     6          INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,VISC ,FNOR   ,
     7          DEPTH	     ,DIST	   ,INTBUF_TAB%GAPN,INTBUF_TAB%STF8,
     8          STIFN	     ,FNCONT	   ,FTCONT	 ,ITAB,   IFT0,
     9          IX1     ,IX2       ,IX3      ,IX4, 
     A          XI_A    ,YI_A      ,ZI_A,
     B          N1    ,N2      ,N3,
     C           ANS    , SSC      , TTC,
     D            H1    ,  H2      ,  H3     ,  H4, 
     E          FACE_A    ,STIF      , XX3,
     G           YY3    , ZZ3      , XX4,
     H           YY4    , ZZ4      , XI1,
     I           YI1    , ZI1      , XI2,
     J           YI2    , ZI2      , XI3,
     K           YI3    , ZI3      , XI4,
     L           THK    ,H3D_DATA  , NINSKID,
     M           H3D_DATA%N_SCAL_SKID, PSKIDS,INTBUF_TAB%IRECTM,NIN,
     N           TAGNCONT ,KLOADPINTER,LOADPINTER ,LOADP_HYD_INTER,
     O           IFLINEAR ,FRIC_LAST,FNOR_LAST,DISTLIN)

      
      
        DEALLOCATE(
     .  DISTANCE,
     .  DIST,
     .  IX1,
     .  IX2,
     .  IX3,
     .  IX4,
     .  X1, X2, X3, X4,
     .  Y1, Y2, Y3, Y4,
     .  Z1, Z2, Z3, Z4,
     .  XI, XI_A,
     .  YI, YI_A,
     .  ZI, ZI_A,
     .  XP, N1,
     .  YP, N2,
     .  ZP, N3, 
     .  ANS,SSC,TTC,
     .  H1,H2,H3,H4,
     .  FACE,FACE_A,
     .  STIF, 
     .  X0, XX1, XX2,
     .  Y0, YY1, YY2,
     .  Z0, ZZ1, ZZ2,
     .  XX3, XX4, XI1,
     .  YY3, YY4, YI1,
     .  ZZ3, ZZ4, ZI1,
     .  XI2, XI3, XI4,
     .  YI2, YI3, YI4,
     .  ZI2, ZI3, ZI4,
     .  XN1, XN2, XN3, XN4,
     .  YN1, YN2, YN3, YN4,
     .  ZN1, ZN2, ZN3, ZN4,
     .  THK, AREA,ALP,
     .  HAS_MOVED,
     .  TAB_RMAX,TAB_RMAX_UID,
     .  IRTL_A,NSV_A,NSV2,
     .  INDEX_IN_COMM,
     .  DISTLIN)

      ENDIF ! TYPE 8

      RETURN
      END
