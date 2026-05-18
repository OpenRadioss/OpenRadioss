Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    intvo8            ../engine/source/interfaces/inter3d/intvo8.F
!||--- called by ------------------------------------------------------
!||    intfop8           ../engine/source/interfaces/interf/intfop8.F
!||--- calls      -----------------------------------------------------
!||    i8cor3            ../engine/source/interfaces/inter3d/i8cor3.F
!||    i8cst3            ../engine/source/interfaces/inter3d/i8cst3.F
!||    i8dis3            ../engine/source/interfaces/inter3d/i8dis3.F
!||    i8for3            ../engine/source/interfaces/inter3d/i8for3.F
!||    i8gap3            ../engine/source/interfaces/inter3d/i8gap3.F
!||    i8loc3            ../engine/source/interfaces/inter3d/i8loc3.F
!||    i8msr3            ../engine/source/interfaces/inter3d/i8msr3.F
!||    spmd_i8_commslv   ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_iloc      ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_index     ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_irtl      ../engine/source/mpi/interfaces/spmd_i8tool.F
!||    spmd_i8_updbuf    ../engine/source/mpi/interfaces/spmd_i8tool.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod           ../engine/share/modules/h3d_mod.F
!||    int8_mod          ../common_source/modules/interfaces/int8_mod.F90
!||    intbufdef_mod     ../common_source/modules/interfaces/intbufdef_mod.F90
!||    output_mod        ../common_source/modules/output/output_mod.F90
!||====================================================================
      SUBROUTINE INTVO8(OUTPUT,IPARI,X ,A  ,
     2                  ICODT,FSAV ,V ,MS ,
     3                  FSKYI,ISKY ,FCONT,FNCONT,FTCONT,
     4                  ICONTACT,RCONTACT,
     5                  STIFN,ITAB,INTBUF_TAB, T8, H3D_DATA ,
     6                  NIN  , PSKIDS,TAGNCONT,KLOADPINTER,LOADPINTER,
     7                  LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE OUTPUT_MOD
      USE INTBUFDEF_MOD 
      USE INT8_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type(output_), intent(inout) :: output
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
      INTEGER I
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
      INTEGER :: NMN, NTY, NSN
      INTEGER :: LFT, LLT, NFT
C-----------------------------------------------

      NSN    = IPARI(5)
      NMN    = IPARI(6)
      NTY   = IPARI(7)
      IBC   =IPARI(11)
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
        call my_alloc(DISTANCE, NSN, "DISTANCE")
        call my_alloc(DIST, NSN, "DIST")
        call my_alloc(IX1, NSN, "IX1")
        call my_alloc(IX2, NSN, "IX2")
        call my_alloc(IX3, NSN, "IX3")
        call my_alloc(IX4, NSN, "IX4")
        call my_alloc(X1, NSN, "X1")
        call my_alloc(X2, NSN, "X2")
        call my_alloc(X3, NSN, "X3")
        call my_alloc(X4, NSN, "X4")
        call my_alloc(Y1, NSN, "Y1")
        call my_alloc(Y2, NSN, "Y2")
        call my_alloc(Y3, NSN, "Y3")
        call my_alloc(Y4, NSN, "Y4")
        call my_alloc(Z1, NSN, "Z1")
        call my_alloc(Z2, NSN, "Z2")
        call my_alloc(Z3, NSN, "Z3")
        call my_alloc(Z4, NSN, "Z4")
        call my_alloc(XI, NSN, "XI")
        call my_alloc(XI_A, NSN, "XI_A")
        call my_alloc(YI, NSN, "YI")
        call my_alloc(YI_A, NSN, "YI_A")
        call my_alloc(ZI, NSN, "ZI")
        call my_alloc(ZI_A, NSN, "ZI_A")
        call my_alloc(XP, NSN, "XP")
        call my_alloc(N1, NSN, "N1")
        call my_alloc(YP, NSN, "YP")
        call my_alloc(N2, NSN, "N2")
        call my_alloc(ZP, NSN, "ZP")
        call my_alloc(N3, NSN, "N3")
        call my_alloc(ANS, NSN, "ANS")
        call my_alloc(SSC, NSN, "SSC")
        call my_alloc(TTC, NSN, "TTC")
        call my_alloc(H1, NSN, "H1")
        call my_alloc(H2, NSN, "H2")
        call my_alloc(H3, NSN, "H3")
        call my_alloc(H4, NSN, "H4")
        call my_alloc(FACE, NSN, "FACE")
        call my_alloc(FACE_A, NSN, "FACE_A")
        call my_alloc(STIF, NSN, "STIF")
        call my_alloc(X0, NSN, "X0")
        call my_alloc(XX1, NSN, "XX1")
        call my_alloc(XX2, NSN, "XX2")
        call my_alloc(Y0, NSN, "Y0")
        call my_alloc(YY1, NSN, "YY1")
        call my_alloc(YY2, NSN, "YY2")
        call my_alloc(Z0, NSN, "Z0")
        call my_alloc(ZZ1, NSN, "ZZ1")
        call my_alloc(ZZ2, NSN, "ZZ2")
        call my_alloc(XX3, NSN, "XX3")
        call my_alloc(XX4, NSN, "XX4")
        call my_alloc(XI1, NSN, "XI1")
        call my_alloc(YY3, NSN, "YY3")
        call my_alloc(YY4, NSN, "YY4")
        call my_alloc(YI1, NSN, "YI1")
        call my_alloc(ZZ3, NSN, "ZZ3")
        call my_alloc(ZZ4, NSN, "ZZ4")
        call my_alloc(ZI1, NSN, "ZI1")
        call my_alloc(XI2, NSN, "XI2")
        call my_alloc(XI3, NSN, "XI3")
        call my_alloc(XI4, NSN, "XI4")
        call my_alloc(YI2, NSN, "YI2")
        call my_alloc(YI3, NSN, "YI3")
        call my_alloc(YI4, NSN, "YI4")
        call my_alloc(ZI2, NSN, "ZI2")
        call my_alloc(ZI3, NSN, "ZI3")
        call my_alloc(ZI4, NSN, "ZI4")
        call my_alloc(XN1, NSN, "XN1")
        call my_alloc(XN2, NSN, "XN2")
        call my_alloc(XN3, NSN, "XN3")
        call my_alloc(XN4, NSN, "XN4")
        call my_alloc(YN1, NSN, "YN1")
        call my_alloc(YN2, NSN, "YN2")
        call my_alloc(YN3, NSN, "YN3")
        call my_alloc(YN4, NSN, "YN4")
        call my_alloc(ZN1, NSN, "ZN1")
        call my_alloc(ZN2, NSN, "ZN2")
        call my_alloc(ZN3, NSN, "ZN3")
        call my_alloc(ZN4, NSN, "ZN4")
        call my_alloc(THK, NSN, "THK")
        call my_alloc(AREA, NSN, "AREA")
        call my_alloc(ALP, NSN, "ALP")
        call my_alloc(HAS_MOVED, NSN, "HAS_MOVED")
        call my_alloc(TAB_RMAX, NSN, "TAB_RMAX")
        call my_alloc(TAB_RMAX_UID, 4, NSN, "TAB_RMAX_UID")
        call my_alloc(IRTL_A, NSN, "IRTL_A")
        call my_alloc(NSV_A, NSN, "NSV_A")
        call my_alloc(NSV2, NSN, "NSV2")
        call my_alloc(INDEX_IN_COMM, NMN, "INDEX_IN_COMM")
        call my_alloc(DISTLIN, NSN, "DISTLIN")

        DIST = 0
        DISTLIN(1:NSN) = ZERO
!       DO 180 NG=1,NGROUS
        NFT=0
        LFT=1
        LLT=NSN                   
        IF(NSPMD > 1) THEN

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

        CALL I8LOC3(
     1   X,                INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,  INTBUF_TAB%MSR,
     2   INTBUF_TAB%NSV,   INTBUF_TAB%ILOCS, INTBUF_TAB%NSEGM, XI,
     3   YI,               ZI,               FACE,             ITAB,
     4   DISTANCE,         IFLINEAR,         DISTLIN,          NSN,
     5   LFT,              LLT,              NFT)
        IF(NSPMD > 1) THEN

          CALL SPMD_I8_ILOC(INTBUF_TAB%ILOCS,    INTBUF_TAB%MSR,ITAB,T8%BUFFER,
     .                      DISTANCE)

          CALL SPMD_I8_COMMSLV(NSN,
     .              INTBUF_TAB%ILOCS,INTBUF_TAB%NSV,
     .              ITAB,T8%BUFFER,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM)
          
          CALL SPMD_I8_UPDBUF(NSN,INTBUF_TAB%ILOCS,INTBUF_TAB%NSV,
     .              ITAB,T8%BUFFER,T8%SPMD_COMM_PATTERN,INDEX_IN_COMM)
        ENDIF
        CALL I8MSR3(
     1   X,                INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,  INTBUF_TAB%MSR,
     2   INTBUF_TAB%NSV,   INTBUF_TAB%ILOCS, INTBUF_TAB%IRTLM, INTBUF_TAB%NSEGM,
     3   FACE,             NSN,              ITAB,             HAS_MOVED,
     4   TAB_RMAX,         TAB_RMAX_UID,     LFT,              LLT,
     5   NFT)

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
        ! by keeping only second nodes that are 
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

        CALL I8COR3(
     1   X,                INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,   NSV_A,
     2   IRTL_A,           IX1,              IX2,              IX3,
     3   IX4,              X1,               Y1,               Z1,
     4   X2,               Y2,               Z2,               X3,
     5   Y3,               Z3,               X4,               Y4,
     6   Z4,               LFT,              LLT,              NFT)
        CALL I8CST3(
     1   X1,      Y1,      Z1,      X2,
     2   Y2,      Z2,      X3,      Y3,
     3   Z3,      X4,      Y4,      Z4,
     4   XI_A,    YI_A,    ZI_A,    N1,
     5   N2,      N3,      ANS,     SSC,
     6   TTC,     FACE_A,  X0,      Y0,
     7   Z0,      XX1,     YY1,     ZZ1,
     8   XX2,     YY2,     ZZ2,     XX3,
     9   YY3,     ZZ3,     XX4,     YY4,
     A   ZZ4,     XI1,     YI1,     ZI1,
     B   XI2,     YI2,     ZI2,     XI3,
     C   YI3,     ZI3,     XI4,     YI4,
     D   ZI4,     XN1,     YN1,     ZN1,
     E   XN2,     YN2,     ZN2,     XN3,
     F   YN3,     ZN3,     XN4,     YN4,
     G   ZN4,     AREA,    LFT,     LLT)

        CALL I8GAP3(
     1   GAP,     THK,     AREA,    ALP,
     2   LFT,     LLT)
        CALL I8DIS3(
     1   IGIMP,   NTY,     DIST,    X1,
     2   Y1,      Z1,      X2,      Y2,
     3   Z2,      X3,      Y3,      Z3,
     4   X4,      Y4,      Z4,      XI_A,
     5   YI_A,    ZI_A,    XP,      YP,
     6   ZP,      N1,      N2,      N3,
     7   ANS,     SSC,     TTC,     H1,
     8   H2,      H3,      H4,      FACE_A,
     9   ALP,     LFT,     LLT)

        CALL I8FOR3(output, LFT  ,LLT  ,NFT   ,A   ,
     2          INTBUF_TAB%MSR,NSV_A,IRTL_A,INTBUF_TAB%STFM,
     .          INTBUF_TAB%NSV,NSV2, INTBUF_TAB%ILOCS, 
     3          INTBUF_TAB%STFNS,IBC     ,ICODT      ,FSAV      ,IGIMP  , 
     4          X       ,V       ,MS      ,FRIC      ,NSN    ,
     5          FSKYI       ,ISKY    ,FCONT      ,RCONTACT  ,IFORM  ,
     6          INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,VISC ,FNOR   ,
     7          DEPTH       ,DIST     ,INTBUF_TAB%GAPN,INTBUF_TAB%STF8,
     8          STIFN       ,FNCONT     ,FTCONT   ,ITAB,   IFT0,
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

      
      
        call my_dealloc(DISTANCE)
        call my_dealloc(DIST)
        call my_dealloc(IX1)
        call my_dealloc(IX2)
        call my_dealloc(IX3)
        call my_dealloc(IX4)
        call my_dealloc(X1)
        call my_dealloc(X2)
        call my_dealloc(X3)
        call my_dealloc(X4)
        call my_dealloc(Y1)
        call my_dealloc(Y2)
        call my_dealloc(Y3)
        call my_dealloc(Y4)
        call my_dealloc(Z1)
        call my_dealloc(Z2)
        call my_dealloc(Z3)
        call my_dealloc(Z4)
        call my_dealloc(XI)
        call my_dealloc(XI_A)
        call my_dealloc(YI)
        call my_dealloc(YI_A)
        call my_dealloc(ZI)
        call my_dealloc(ZI_A)
        call my_dealloc(XP)
        call my_dealloc(N1)
        call my_dealloc(YP)
        call my_dealloc(N2)
        call my_dealloc(ZP)
        call my_dealloc(N3)
        call my_dealloc(ANS)
        call my_dealloc(SSC)
        call my_dealloc(TTC)
        call my_dealloc(H1)
        call my_dealloc(H2)
        call my_dealloc(H3)
        call my_dealloc(H4)
        call my_dealloc(FACE)
        call my_dealloc(FACE_A)
        call my_dealloc(STIF)
        call my_dealloc(X0)
        call my_dealloc(XX1)
        call my_dealloc(XX2)
        call my_dealloc(Y0)
        call my_dealloc(YY1)
        call my_dealloc(YY2)
        call my_dealloc(Z0)
        call my_dealloc(ZZ1)
        call my_dealloc(ZZ2)
        call my_dealloc(XX3)
        call my_dealloc(XX4)
        call my_dealloc(XI1)
        call my_dealloc(YY3)
        call my_dealloc(YY4)
        call my_dealloc(YI1)
        call my_dealloc(ZZ3)
        call my_dealloc(ZZ4)
        call my_dealloc(ZI1)
        call my_dealloc(XI2)
        call my_dealloc(XI3)
        call my_dealloc(XI4)
        call my_dealloc(YI2)
        call my_dealloc(YI3)
        call my_dealloc(YI4)
        call my_dealloc(ZI2)
        call my_dealloc(ZI3)
        call my_dealloc(ZI4)
        call my_dealloc(XN1)
        call my_dealloc(XN2)
        call my_dealloc(XN3)
        call my_dealloc(XN4)
        call my_dealloc(YN1)
        call my_dealloc(YN2)
        call my_dealloc(YN3)
        call my_dealloc(YN4)
        call my_dealloc(ZN1)
        call my_dealloc(ZN2)
        call my_dealloc(ZN3)
        call my_dealloc(ZN4)
        call my_dealloc(THK)
        call my_dealloc(AREA)
        call my_dealloc(ALP)
        call my_dealloc(HAS_MOVED)
        call my_dealloc(TAB_RMAX)
        call my_dealloc(TAB_RMAX_UID)
        call my_dealloc(IRTL_A)
        call my_dealloc(NSV_A)
        call my_dealloc(NSV2)
        call my_dealloc(INDEX_IN_COMM)
        call my_dealloc(DISTLIN)

      ENDIF ! TYPE 8

      RETURN
      END
