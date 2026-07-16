Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    allocxfem      ../starter/source/elements/xfem/allocxfem.F
!||--- called by ------------------------------------------------------
!||    lectur         ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE ALLOCXFEM(
     .           IXC    ,IXTG  ,IPARG  ,LCNE_CRKXFEM,CRKLVSET,
     .           CRKSKY ,CRKAVX,CRKEDGE,XFEM_PHANTOM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
      use element_mod , only : nixc,nixtg
      use my_alloc_mod
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
C     INITIALIZATION
C--------------------
C  crack xfem for layered shell
C--------------------
c
        DO I=1,NLEVMAX       
          call my_alloc(CRKLVSET(I)%ELCUT, ECRKXFE, "CRKLVSET(I)%ELCUT")
          call my_alloc(CRKLVSET(I)%HFI0, 4, ECRKXFE, "CRKLVSET(I)%HFI0")
          call my_alloc(CRKLVSET(I)%ENRICH0, 4, ECRKXFE, "CRKLVSET(I)%ENRICH0")
          call my_alloc(CRKLVSET(I)%EDGE0, 4, ECRKXFE, "CRKLVSET(I)%EDGE0")
C
          call my_alloc(CRKLVSET(I)%EDGE, 4, ECRKXFEC, "CRKLVSET(I)%EDGE")
          call my_alloc(CRKLVSET(I)%EDGETG, 3, ECRKXFETG, "CRKLVSET(I)%EDGETG")
          call my_alloc(CRKLVSET(I)%ICUTEDGE, NUMEDGES, "CRKLVSET(I)%ICUTEDGE")
          call my_alloc(CRKLVSET(I)%RATIOEDGE, NUMEDGES, "CRKLVSET(I)%RATIOEDGE")
C
          call my_alloc(CRKAVX(I)%A, 3, LCNE_CRKXFEM, "CRKAVX(I)%A")
          call my_alloc(CRKAVX(I)%AR, 3, LCNE_CRKXFEM, "CRKAVX(I)%AR")
          call my_alloc(CRKAVX(I)%V, 3, LCNE_CRKXFEM, "CRKAVX(I)%V")
          call my_alloc(CRKAVX(I)%VR, 3, LCNE_CRKXFEM, "CRKAVX(I)%VR")
          call my_alloc(CRKAVX(I)%X, 3, LCNE_CRKXFEM, "CRKAVX(I)%X")
          call my_alloc(CRKAVX(I)%U, 3, LCNE_CRKXFEM, "CRKAVX(I)%U")
          call my_alloc(CRKAVX(I)%XX, 3, 4*ECRKXFE, "CRKAVX(I)%XX")
c
          call my_alloc(CRKSKY(I)%FSKY, 8, LCNE_CRKXFEM, "CRKSKY(I)%FSKY")
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
          call my_alloc(CRKEDGE(I)%LAYCUT, ECRKXFE, "CRKEDGE(I)%LAYCUT")
          call my_alloc(CRKEDGE(I)%EDGEICRK, NUMEDGES, "CRKEDGE(I)%EDGEICRK")
          call my_alloc(CRKEDGE(I)%EDGEIFI, 2, NUMEDGES, "CRKEDGE(I)%EDGEIFI")
          call my_alloc(CRKEDGE(I)%EDGEENR, 2, NUMEDGES, "CRKEDGE(I)%EDGEENR")
          call my_alloc(CRKEDGE(I)%EDGETIP, 2, NUMEDGES, "CRKEDGE(I)%EDGETIP")
          CRKEDGE(I)%LAYCUT   = 0
          CRKEDGE(I)%EDGEICRK = 0
          CRKEDGE(I)%EDGEIFI  = 0
          CRKEDGE(I)%EDGEENR  = 0
          CRKEDGE(I)%EDGETIP  = 0
        ENDDO
c
        DO I=1,NXLAYMAX
          call my_alloc(XFEM_PHANTOM(I)%ELCUT, ECRKXFE, "XFEM_PHANTOM(I)%ELCUT")
          call my_alloc(XFEM_PHANTOM(I)%IFI, LCNECRKXFEM, "XFEM_PHANTOM(I)%IFI")
          call my_alloc(XFEM_PHANTOM(I)%ITRI, 2, ECRKXFE, "XFEM_PHANTOM(I)%ITRI")
          call my_alloc(XFEM_PHANTOM(I)%TAGXP, 1, 1, 1, "XFEM_PHANTOM(I)%TAGXP") ! not used in starter
          XFEM_PHANTOM(I)%ELCUT = 0
          XFEM_PHANTOM(I)%TAGXP = 0
          XFEM_PHANTOM(I)%IFI   = 0
          XFEM_PHANTOM(I)%ITRI  = 0
        ENDDO
C---
      RETURN
      END
