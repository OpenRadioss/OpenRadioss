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
Chd|  ALLOCXFEM                     source/elements/xfem/allocxfem.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE ALLOCXFEM(
     .           IXC    ,IXTG  ,IPARG  ,LCNE_CRKXFEM,CRKLVSET,
     .           CRKSKY ,CRKAVX,CRKEDGE,XFEM_PHANTOM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),IPARG(NPARG,*),LCNE_CRKXFEM
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX) :: CRKLVSET
      TYPE (XFEM_SKY_)    , DIMENSION(NLEVMAX) :: CRKSKY
      TYPE (XFEM_AVX_)    , DIMENSION(NLEVMAX) :: CRKAVX
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX):: CRKEDGE
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX):: XFEM_PHANTOM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER KNOD2ELC(NUMNOD+1)
      INTEGER I,J,N,LFT,LLT,NFT,NEL,ITY,IXFEM,NG
C=======================================================================
      KNOD2ELC = 0
      DO NG=1,NGROUP
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        ITY=IPARG(5,NG)
        IXFEM=IPARG(54,NG)
        LFT=1
        LLT=MIN(NVSIZ,NEL)
        IF (IXFEM == 0 ) CYCLE
        IF (ITY==3) THEN
          DO I=LFT,LLT
            DO J=2,5
              N = IXC(J,I+NFT)
              KNOD2ELC(N) = KNOD2ELC(N) + 1
            END DO
          END DO
        ELSE IF(ITY==7) THEN
          DO I=LFT,LLT
            DO J=2,4
              N = IXTG(J,I+NFT)
              KNOD2ELC(N) = KNOD2ELC(N) + 1
            END DO
          END DO
        END IF
      ENDDO ! NG=1,NGROUP
C
      IENRNOD = 0
      DO I=1,NUMNOD
        IENRNOD = MAX(IENRNOD,KNOD2ELC(I))
      END DO
C
      IENRNOD = MAX(IENRNOD,4)
      IENRNOD = IENRNOD * NLEVMAX
C--------------------
C     INITIALISATION
C--------------------
C  crack xfem for layered shell
C--------------------
c
        DO I=1,NLEVMAX       
          ALLOCATE(CRKLVSET(I)%ELCUT(ECRKXFE))
          ALLOCATE(CRKLVSET(I)%HFI0(4,ECRKXFE))
          ALLOCATE(CRKLVSET(I)%ENRICH0(4,ECRKXFE))
          ALLOCATE(CRKLVSET(I)%EDGE0(4,ECRKXFE))
C
          ALLOCATE(CRKLVSET(I)%EDGE(4,ECRKXFEC))
          ALLOCATE(CRKLVSET(I)%EDGETG(3,ECRKXFETG))
          ALLOCATE(CRKLVSET(I)%ICUTEDGE(NUMEDGES))
          ALLOCATE(CRKLVSET(I)%RATIOEDGE(NUMEDGES))
C
          ALLOCATE(CRKAVX(I)%A(3,LCNE_CRKXFEM))
          ALLOCATE(CRKAVX(I)%AR(3,LCNE_CRKXFEM))
          ALLOCATE(CRKAVX(I)%V(3,LCNE_CRKXFEM))
          ALLOCATE(CRKAVX(I)%VR(3,LCNE_CRKXFEM))
          ALLOCATE(CRKAVX(I)%X(3,LCNE_CRKXFEM))
          ALLOCATE(CRKAVX(I)%U(3,LCNE_CRKXFEM))
          ALLOCATE(CRKAVX(I)%XX(3,4*ECRKXFE))
c
          ALLOCATE(CRKSKY(I)%FSKY(8,LCNE_CRKXFEM))
C
          CRKLVSET(I)%ELCUT= 0
          CRKLVSET(I)%HFI0 = 0
          CRKLVSET(I)%ENRICH0 = 0
          CRKLVSET(I)%EDGE0 = 0
C
          CRKLVSET(I)%EDGE= 0
          CRKLVSET(I)%EDGETG = 0
          CRKLVSET(I)%ICUTEDGE = 0
          CRKLVSET(I)%RATIOEDGE = 0
C
          CRKAVX(I)%A  = 0
          CRKAVX(I)%AR = 0
          CRKAVX(I)%V  = 0
          CRKAVX(I)%VR = 0
          CRKAVX(I)%X  = 0
          CRKAVX(I)%U  = 0
          CRKAVX(I)%XX = 0
          CRKSKY(I)%FSKY = 0
        ENDDO
C
        DO I=1,NXLAYMAX
          ALLOCATE(CRKEDGE(I)%LAYCUT(ECRKXFE))
          ALLOCATE(CRKEDGE(I)%EDGEICRK(NUMEDGES))
          ALLOCATE(CRKEDGE(I)%EDGEIFI(2,NUMEDGES))
          ALLOCATE(CRKEDGE(I)%EDGEENR(2,NUMEDGES))
          ALLOCATE(CRKEDGE(I)%EDGETIP(2,NUMEDGES))
          CRKEDGE(I)%LAYCUT   = 0
          CRKEDGE(I)%EDGEICRK = 0
          CRKEDGE(I)%EDGEIFI  = 0
          CRKEDGE(I)%EDGEENR  = 0
          CRKEDGE(I)%EDGETIP  = 0
        ENDDO
c
        DO I=1,NXLAYMAX
          ALLOCATE(XFEM_PHANTOM(I)%ELCUT(ECRKXFE))
          ALLOCATE(XFEM_PHANTOM(I)%IFI(LCNECRKXFEM))
          ALLOCATE(XFEM_PHANTOM(I)%ITRI(2,ECRKXFE))
          ALLOCATE(XFEM_PHANTOM(I)%TAGXP(1,1,1))     ! not used in starter
          XFEM_PHANTOM(I)%ELCUT = 0
          XFEM_PHANTOM(I)%TAGXP = 0
          XFEM_PHANTOM(I)%IFI   = 0
          XFEM_PHANTOM(I)%ITRI  = 0
        ENDDO
C---
      RETURN
      END
