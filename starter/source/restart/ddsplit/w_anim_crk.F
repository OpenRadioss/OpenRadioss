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
C
Chd|====================================================================
Chd|  W_ANIM_CRK                    source/restart/ddsplit/w_anim_crk.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE W_ANIM_CRK(
     .               IXC     ,IXTG   ,NUMELC_L,NUMELTG_L,NODLOCAL,
     .               NUMNOD_L ,INOD_L     ,CEL     ,CEP_XFE,PROC,
     .               IEDGECRK_L,IBORDEDGE_L,NUMEDGES_L,INDEX_CRKXFEM,
     .               INOD_CRKXFEM,LCNECRKXFEM_L,EDGEGLOBAL,CEP,CRKLVSET,
     .               NCRKPART, INDX_CRK, CRKSHELL,CRKSKY  ,CRKAVX  ,
     .               CRKEDGE ,XFEM_PHANTOM,NUMNODCRK_L)
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),NUMELC_L, NODLOCAL(*),INDX_CRK(*),
     .        NUMNOD_L,CEL(*),CEP_XFE(*),PROC,NCRKPART,
     .        INOD_L(*),IXTG(NIXTG,*),NUMELTG_L,NUMNODCRK_L,
     .        IEDGECRK_L(*),IBORDEDGE_L(*),NUMEDGES_L,
     . INDEX_CRKXFEM(*),INOD_CRKXFEM(*),LCNECRKXFEM_L,
     . EDGEGLOBAL(*),CEP(*)
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)  :: CRKSHELL
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX)  :: CRKLVSET
      TYPE (XFEM_SKY_)    , DIMENSION(NLEVMAX)  :: CRKSKY
      TYPE (XFEM_AVX_)    , DIMENSION(NLEVMAX)  :: CRKAVX
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX) :: CRKEDGE      
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX) :: XFEM_PHANTOM 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ELEM,ND,NDSZ_L,ELSZ_L,ELPL,NCOUNT,EMPL,ILAY,
     . ELTYP,IX(4),OFFC,OFFTG,OFF,IDIM,NEXT,ELSZC_L,ELSZTG_L,
     . SH4N,SH3N,SH4N_L,SH3N_L,CRKSHELLID_L,
     . NELXFE_L,IED,IED_GL,NLAY,LEN,LENLAY,NCOUNTALL,ELEM_GL,ELEM_L,
     . NENR,XFENUMNODS,IEL_L
      my_real
     . REDGE(NUMEDGES_L)
c
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODTAG,ELEMTAG,CRKSIZN_L
      INTEGER, DIMENSION(:), ALLOCATABLE   :: ELEMXFEMID,
     .    ELCUT,ITRI,TAGXP
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: IFI,ENRICH0,IEDGEX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOD2ELC,EDGEIFI,EDGEENR
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: XFECRKNODID,ELTYPE,NOD2IAD
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: ICUTEDGE,EDGEICRK,LAYCUT
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: EDGETIP
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NOD_XFENODES
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SH_XFENODES
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: ELEMLOC_C,ELEMLOC_TG
      my_real, DIMENSION(:)  , ALLOCATABLE :: RATIOEDGE,AVX,AVXX,FSKY,AREA
C=======================================================================
!     1d array
      ALLOCATE( NODTAG(NUMNOD_L),ELEMTAG(NUMELC_L+NUMELTG_L) )
      ALLOCATE( CRKSIZN_L(NLEVMAX) )
! --------------------------------------
      OFFC = NUMELS + NUMELQ
      OFFTG = OFFC + NUMELC + NUMELT + NUMELP + NUMELR
C
      CALL WRITE_I_C(NCRKPART,1)
      CALL WRITE_I_C(NCRKXFE, 1)   ! total Number of Xfem elements
      CALL WRITE_I_C(INDX_CRK,NCRKPART)
C
      CRKSHELLID_L = 0
      NCOUNTALL = 0
C
      ALLOCATE(ELEMLOC_C(NUMELC))
      ALLOCATE(ELEMLOC_TG(NUMELTG))
      ELEMLOC_C  = 0
      ELEMLOC_TG = 0
c
      J = 0
      DO I=1,NUMELC
        IF(CEP(I+OFFC) == PROC)THEN
          J = J + 1
          ELEMLOC_C(I) = J
        ENDIF
      ENDDO
      J = 0
      DO I=1,NUMELTG
        IF(CEP(I+OFFTG) == PROC)THEN
          J = J + 1
          ELEMLOC_TG(I) = J
        ENDIF
      ENDDO
c===================================================================
c     Stockage par ILEV 
c===================================================================
      ALLOCATE (ELCUT(NUMELC_L+NUMELTG_L))
c
      DO K=1,NLEVMAX
        NODTAG  = 0
        ELEMTAG = 0
        CRKSIZN_L(K) = 0
        SH4N_L   = 0
        SH3N_L   = 0
        NELXFE_L = 0
c
        DO I=1,CRKSHELL(K)%CRKNUMSHELL
          ELTYP = CRKSHELL(K)%ELTYPE(I)
          ELEM  = CRKSHELL(K)%PHANTOML(I)
          IF (CEP_XFE(I) == PROC) THEN
            NELXFE_L = NELXFE_L + 1
            ELEMTAG(NELXFE_L)=I
            CRKSIZN_L(K)=CRKSIZN_L(K)+ELTYP  ! somme noeuds phantomes par ply par proc
            IF (ELTYP == 4) THEN
              SH4N_L = SH4N_L + 1
              DO J=1,ELTYP
                ND = IXC(J+1,ELEM)
                NODTAG(NODLOCAL(ND))=ND  ! noeud global = f(node local)
              ENDDO
            ELSEIF (ELTYP == 3) THEN
              SH3N_L = SH3N_L + 1
              DO J=1,ELTYP
                ND = IXTG(J+1,ELEM)
                NODTAG(NODLOCAL(ND))=ND  ! noeud global = f(node local)
              ENDDO
            END IF
          END IF
        END DO ! I=1,CRKSHELL(K)%CRKNUMSHELL
C------------------------------
        ELSZC_L  = SH4N_L                      ! nb elements par ply
        ELSZTG_L = SH3N_L
        ELSZ_L   = ELSZC_L + ELSZTG_L
C
        NDSZ_L=0
        DO I=1,NUMNOD_L
          IF (NODTAG(I) > 0) NDSZ_L=NDSZ_L+1    ! nb noeuds par ply  (image std)
        ENDDO
C
        IDIM = CRKSIZN_L(K)  ! nb noeuds phantomes par ply par proc
C
        ALLOCATE(ELEMXFEMID(ELSZ_L))
        ALLOCATE(ELTYPE(ELSZ_L))
        ALLOCATE(AREA(ELSZ_L))

        ELCUT = 0
        ELEMXFEMID = 0
        ELTYPE  = 0
        AREA    = 0
        ALLOCATE(KNOD2ELC(4,ELSZ_L),XFECRKNODID(4*ELSZ_L),
     .           SH_XFENODES(4,ELSZ_L),ENRICH0(LCNECRKXFEM_L),
     .           NOD_XFENODES(4*ELSZ_L))
        IF (K==1) THEN
          ALLOCATE(LAYCUT(ELSZ_L))
          ALLOCATE(IEDGEX(IDIM))
          IEDGEX = 0
       ENDIF
C
        ENRICH0 = 0
        KNOD2ELC = 0
        XFECRKNODID = 0
        NOD_XFENODES = 0
        SH_XFENODES = 0
C---
c       Local element tables
C---
        NCOUNT = 0
        ELPL = 0
        NEXT = 0
        DO I=1,NELXFE_L    ! elements loc par ply par proc
          IF (ELEMTAG(I) > 0) THEN
             ELPL = ELPL+1
             ND   = ELEMTAG(I)   ! N element global par ply
             ELTYP   = CRKSHELL(K)%ELTYPE(ND)
             ELEM_GL = CRKSHELL(K)%PHANTOML(ND)
             IF (ELTYP == 4) THEN
               ELEM_L = ELEMLOC_C(ELEM_GL)
               ELTYPE(ELPL) = 0
             ELSEIF (ELTYP == 3) THEN
               ELEM_L = ELEMLOC_TG(ELEM_GL)
               ELTYPE(ELPL) = 1
             ENDIF
C
             ELEMXFEMID(ELPL) = CRKSHELL(K)%PHANTOMG(ND)   ! N global = f(N local)
             ILAY = (K-1)/NXEL + 1
             ELCUT(ELPL) = XFEM_PHANTOM(ILAY)%ELCUT(ND)
C
             IF (K==1) THEN
               LENLAY = ELSZ_L
               LAYCUT(ELPL) = CRKEDGE(K)%LAYCUT(ND)
              ENDIF
C
             IF (K==1) THEN
               DO J=1,ELTYP
                 IF(ELTYP==4)THEN
                   IEDGEX(NEXT+J) = CRKLVSET(K)%EDGE(J,ND)
                 ELSEIF(ELTYP==3)THEN
                  IEDGEX(NEXT+J) = CRKLVSET(K)%EDGETG(J,ND-ECRKXFEC)
                 ENDIF
               ENDDO
               NEXT = NEXT + ELTYP
             ENDIF
C
             DO J=1,4
              KNOD2ELC(J,ELPL) = CRKSHELL(K)%XNODEG(J,ND)
C
              NCOUNT = NCOUNT + 1
              XFECRKNODID(NCOUNT) = CRKSHELL(K)%XNODEG(J,ND)
              NCOUNTALL = NCOUNTALL + 1
              NOD_XFENODES(NCOUNT) = NCOUNTALL
              SH_XFENODES(J,ELPL) = NCOUNTALL
            END DO
          ENDIF
        ENDDO  !  1,NELXFE_L
C
        XFENUMNODS = 4*ELSZ_L
        ALLOCATE(AVX(3*LCNECRKXFEM_L))
        ALLOCATE(AVXX(3*XFENUMNODS))
        ALLOCATE(FSKY(8*LCNECRKXFEM_L))
        ALLOCATE(NOD2IAD(XFENUMNODS))
        AVX   = ZERO
        AVXX  = ZERO
        FSKY  = ZERO
        NOD2IAD = 0
c
c------ CRKSHELL
        CALL WRITE_I_C(ELSZC_L    , 1)
        CALL WRITE_I_C(ELSZTG_L   , 1)
        CALL WRITE_I_C(ELSZ_L     , 1)
        CALL WRITE_I_C(ELEMXFEMID , ELSZ_L)  ! CRKSHELL(ILEV)%CRKSHELLID
        CALL WRITE_I_C(ELTYPE     , ELSZ_L)  ! local xfemelement type = 0/1
c------ CRKNOD
        CALL WRITE_I_C(XFECRKNODID    , XFENUMNODS)
        CALL WRITE_I_C(NOD_XFENODES   , XFENUMNODS)
c------ CRKSHELL
c        CALL WRITE_I_C(KNOD2ELC       , XFENUMNODS)
        CALL WRITE_I_C(SH_XFENODES    , XFENUMNODS)
c------ CRKLVSET
        CALL WRITE_I_C(ENRICH0    , LCNECRKXFEM_L) ! CRKLVSET(ILEV)%ENR0(1,IADC1)
        CALL WRITE_I_C(ENRICH0    , LCNECRKXFEM_L) ! CRKLVSET(ILEV)%ENR0(2,IADC1)
        CALL WRITE_DB (AREA       , ELSZ_L)        ! CRKLVSET(ILEV)%AREA(ELCRK)
c------ CRKAVX
        CALL WRITE_DB(AVX,3*LCNECRKXFEM_L)  ! CRKAVX(I)%A
        CALL WRITE_DB(AVX,3*LCNECRKXFEM_L)  ! CRKAVX(I)%AR
        CALL WRITE_DB(AVX,3*LCNECRKXFEM_L)  ! CRKAVX(I)%V
        CALL WRITE_DB(AVX,3*LCNECRKXFEM_L)  ! CRKAVX(I)%VR
        CALL WRITE_DB(AVX,3*LCNECRKXFEM_L)  ! CRKAVX(I)%X
        CALL WRITE_DB(AVX,3*LCNECRKXFEM_L)  ! CRKAVX(I)%U
        CALL WRITE_DB(AVXX,3*XFENUMNODS)      ! CRKAVX(I)%XX
c------ CRKSKY
        CALL WRITE_DB(FSKY,8*LCNECRKXFEM_L) ! CRKSKY(I)%FSKY
c------ CRKNOD
        CALL WRITE_I_C(NOD2IAD,XFENUMNODS)    ! CRKNOD(I)%NOD2IAD
        CALL WRITE_I_C(XFENUMNODS,1)        ! CRKNOD(I)%XFENUMNODS
c-------
        DEALLOCATE(ELEMXFEMID)
        DEALLOCATE(ELTYPE)
        DEALLOCATE(AREA)
        DEALLOCATE(ENRICH0)
        DEALLOCATE(KNOD2ELC)
        DEALLOCATE(XFECRKNODID)
        DEALLOCATE(NOD_XFENODES)
        DEALLOCATE(SH_XFENODES)
        DEALLOCATE(AVX,AVXX,FSKY)
        DEALLOCATE(NOD2IAD)
      ENDDO    ! K=1,NLEVMAX
c
      DEALLOCATE(ELEMLOC_C)
      DEALLOCATE(ELEMLOC_TG)
c===================================================================
c     Stockage par layer
c===================================================================
      NLAY = INT(NLEVMAX/NXEL)
      NENR = INT(IENRNOD/NLEVMAX)
c
c---  write XFEM_PHANTOM  ----------------------------
      ALLOCATE(IFI  (LCNECRKXFEM_L)  )
      ALLOCATE(TAGXP(NUMNODCRK_L*IENRNOD*5))  ! IENRNOD -> NENR
      ALLOCATE(ITRI (ELSZ_L*2))        
      TAGXP = 0
      IFI   = 0    
      ITRI  = 0    
c
      DO ILAY=1,NLAY
        CALL WRITE_I_C(ELCUT ,ELSZ_L)  
        CALL WRITE_I_C(IFI   ,LCNECRKXFEM_L)  
        CALL WRITE_I_C(TAGXP ,NUMNODCRK_L*IENRNOD*5)  
        CALL WRITE_I_C(ITRI  ,ELSZ_L*2)  
      ENDDO
c
      DEALLOCATE(ELCUT)
      DEALLOCATE(ITRI )
      DEALLOCATE(TAGXP )
      DEALLOCATE(IFI  )
C
c---  write CRKEDGE  ----------------------------
      IDIM = CRKSIZN_L(1)
c      ALLOCATE(IEDGEX(IDIM))
      ALLOCATE(EDGEICRK(NUMEDGES_L))   
      ALLOCATE(EDGEIFI(2,NUMEDGES_L))  
      ALLOCATE(EDGEENR(2,NUMEDGES_L))  
      ALLOCATE(EDGETIP(2,NUMEDGES_L))  
      ALLOCATE(ICUTEDGE (NUMEDGES_L))   
      ALLOCATE(RATIOEDGE(NUMEDGES_L))
      EDGEICRK  = 0
      EDGEIFI   = 0
      EDGEENR   = 0
      EDGETIP   = 0
c
      DO ILAY=1,NLAY
        K = (ILAY-1)*NXEL + 1
        DO IED=1,NUMEDGES_L
          IED_GL = EDGEGLOBAL(IED)
          EDGEICRK(IED)  = CRKEDGE(ILAY)%EDGEICRK(IED_GL)    ! Id fissure sur un edge
          EDGEIFI(1,IED) = CRKEDGE(ILAY)%EDGEIFI(1,IED_GL)   ! signe lvset sur un edge (+/- ICRK)
          EDGEIFI(2,IED) = CRKEDGE(ILAY)%EDGEIFI(2,IED_GL)  
          EDGEENR(1,IED) = CRKEDGE(ILAY)%EDGEENR(1,IED_GL)   ! enrich lvset sur un edge
          EDGEENR(2,IED) = CRKEDGE(ILAY)%EDGEENR(2,IED_GL)   
          EDGETIP(1,IED) = CRKEDGE(ILAY)%EDGETIP(1,IED_GL)   ! flag d'edge interne/ext
          EDGETIP(2,IED) = CRKEDGE(ILAY)%EDGETIP(2,IED_GL)
          ICUTEDGE(IED)  = CRKLVSET(K)%ICUTEDGE(IED_GL)
          RATIOEDGE(IED) = CRKLVSET(K)%RATIOEDGE(IED_GL)
        ENDDO
        CALL WRITE_I_C(LAYCUT      ,LENLAY)
        CALL WRITE_I_C(IEDGEX      ,IDIM)
        CALL WRITE_I_C(EDGEICRK    ,NUMEDGES_L)
        CALL WRITE_I_C(EDGEIFI     ,NUMEDGES_L*2)
        CALL WRITE_I_C(EDGEENR     ,NUMEDGES_L*2)
        CALL WRITE_I_C(EDGETIP     ,NUMEDGES_L*2)
        CALL WRITE_I_C(IBORDEDGE_L ,NUMEDGES_L)  ! CRKEDGE(IL)%IBORDEDGE(NUMEDGES) 
        CALL WRITE_I_C(ICUTEDGE    ,NUMEDGES_L)  ! CRKEDGE(IL)%ICUTEDGE(NUMEDGES)  
        CALL WRITE_DB (RATIOEDGE   ,NUMEDGES_L)  ! CRKEDGE(IL)%RATIO(NUMEDGES)     
      END DO    !   ILAY=1,NLAY
c      
      DEALLOCATE(RATIOEDGE)
      DEALLOCATE(ICUTEDGE)
      DEALLOCATE(EDGETIP)  
      DEALLOCATE(EDGEENR)   
      DEALLOCATE(EDGEIFI)   
      DEALLOCATE(EDGEICRK)  
      DEALLOCATE(IEDGEX)   
      IF (ALLOCATED(LAYCUT)) DEALLOCATE(LAYCUT)
c----------------------------------
c     Stockage global
c----------------------------------
        LEN = 4*ELSZC_L + 3*ELSZTG_L
        CALL WRITE_I_C(IEDGECRK_L  ,LEN)      ! XEDGE4N(4*ELSZC) + XEDGE3N(3*ELSZTG)
C-----------
! --------------------------------------
!     1d array
      DEALLOCATE(NODTAG)
      DEALLOCATE(ELEMTAG)
      DEALLOCATE(CRKSIZN_L)
! --------------------------------------
      RETURN
      END
