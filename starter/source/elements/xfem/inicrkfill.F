Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INICRKFILL                    source/elements/xfem/inicrkfill.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE INICRKFILL(ELBUF_TAB,XFEM_TAB,
     .                      IXC      ,IXTG   ,IPARG    ,INICRACK,
     .                      X        ,IEL_CRK,INOD_CRK ,XREFC  ,XREFTG  ,
     .                      IEDGESH4 ,IEDGESH3,NODEDGE,CRKLVSET,
     .                      CRKSHELL ,CRKEDGE ,XFEM_PHANTOM,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
      USE ELBUFDEF_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,*),INOD_CRK(*),
     .   IEL_CRK(*),IEDGESH4(4,*),IEDGESH3(3,*),NODEDGE(2,*),ITAB(*)
      my_real
     .   X(3,*),XREFC(4,3,*),XREFTG(3,3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX)     :: CRKLVSET
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)     :: CRKSHELL
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX)    :: CRKEDGE       
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX)    :: XFEM_PHANTOM  
      TYPE (INICRACK_)    , DIMENSION(NINICRACK)   :: INICRACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,KR,N,ID,ICRK,NXSEG,NXNOD
      INTEGER, DIMENSION(:,:) ,ALLOCATABLE :: NODLS
      INTEGER, DIMENSION(:,:) ,ALLOCATABLE :: TAGSKYC,TAGSKYTG
      INTEGER, DIMENSION(:)   ,ALLOCATABLE :: NTAG
      INTEGER, DIMENSION(:)   ,ALLOCATABLE :: KNOD2ELC,TAGEDGE
      my_real ,DIMENSION(:)   ,ALLOCATABLE :: RATIOLS
      CHARACTER TITR*nchartitle
C=======================================================================
      ALLOCATE(KNOD2ELC(NUMNOD+1))
      ALLOCATE(TAGSKYC(4,NUMELC))
      ALLOCATE(TAGSKYTG(3,NUMELTG))
      ALLOCATE(TAGEDGE(NUMEDGES))
      KNOD2ELC = 0
      TAGSKYC  = 0
      TAGSKYTG = 0
      TAGEDGE  = 0
c-----------------
      DO K=1,4
        DO I=1,NUMELC
          N = IXC(K+1,I)
          KNOD2ELC(N) = KNOD2ELC(N) + 1
          TAGSKYC(K,I) = KNOD2ELC(N)
        END DO
      END DO
C
      DO K=1,3
        DO I=1,NUMELTG
          N = IXTG(K+1,I)
          KNOD2ELC(N) = KNOD2ELC(N) + 1
          TAGSKYTG(K,I) = KNOD2ELC(N)
        END DO
      END DO
c-----------------
      DO ICRK=1,NINICRACK
        ID      = INICRACK(ICRK)%ID
        NXNOD   = INICRACK(ICRK)%NSEG
        TITR    = INICRACK(ICRK)%TITLE
        NXSEG   = NXNOD - 1
C---
        IF (NXSEG > 0) THEN
          ALLOCATE(NODLS(2,NXNOD))
          ALLOCATE(NTAG(NUMNOD))
          ALLOCATE(RATIOLS(NXNOD))
          NODLS = 0
          NTAG = 0
          RATIOLS = ZERO
C---
          DO N=1,NXNOD
            NODLS(1,N) = INICRACK(ICRK)%SEG(N)%NODES(1)
            NODLS(2,N) = INICRACK(ICRK)%SEG(N)%NODES(2)
            RATIOLS(N) = INICRACK(ICRK)%SEG(N)%RATIO
          ENDDO
C---
          CALL LSLOCAL(ELBUF_TAB,XFEM_TAB,
     .          IPARG   ,IXC     ,IXTG    ,XREFC            ,XREFTG  ,
     .          X       ,ICRK    ,INOD_CRK,NXSEG            ,NODLS   ,
     .          RATIOLS ,NTAG    ,IEL_CRK ,IEL_CRK(1+NUMELC),IEDGESH4,
     .          IEDGESH3,NODEDGE ,TAGSKYC ,TAGSKYTG         ,KNOD2ELC,
     .          TAGEDGE ,CRKLVSET,CRKSHELL,CRKEDGE          ,XFEM_PHANTOM,
     .          ITAB    ,ID      ,TITR    )
C---
          DEALLOCATE(NODLS,NTAG,RATIOLS)
        END IF
C---
      ENDDO ! DO ICRK=1,NINICRACK
C---
      DEALLOCATE(TAGSKYC,TAGSKYTG,KNOD2ELC,TAGEDGE)
C-----------
      RETURN
      END
