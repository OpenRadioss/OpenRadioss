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
Chd|  UPENRIC3_N4                   source/elements/xfem/upenric3_nx.F
Chd|-- called by -----------
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPENRIC3_N4(XFEM_TAB,
     .                   IPARG  ,IXC     ,NFT    ,JFT     ,JLT  ,
     .                   ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,IXFEM,
     .                   CRKEDGE,XEDGE4N )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),NFT,JFT,JLT,IXFEM,
     .        ELCUTC(2,*),IADC_CRK(4,*),IEL_CRK(*),XEDGE4N(4,*), 
     .        INOD_CRK(*)
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ELCRK,IADC1,IADC2,IADC3,IADC4,LAYCUT,
     .  IR,IS,IT,ILEV,ILAY,IXEL,NXLAY,NCUT,NELCRK,ELCUT,IECUT,EDGE
      INTEGER JCT(MVSIZ)
      my_real, DIMENSION(:) ,POINTER  ::  XOFF
C=======================================================================
      NELCRK = 0
      DO I=JFT,JLT
        JCT(I) = 0
        IF (ELCUTC(1,I+NFT) /= 0) THEN
          NELCRK = NELCRK + 1
          JCT(NELCRK) = I
        ENDIF
      ENDDO
      IF (NELCRK == 0) RETURN
C-----
      IR = 1
      IS = 1
      IT = 1
C
      DO IXEL=1,NXEL
        NXLAY = XFEM_TAB(IXEL)%NLAY
        DO ILAY=1,NXLAY
          ILEV = NXEL*(ILAY-1) + IXEL
          IF (NXLAY> 1) THEN
            XOFF => XFEM_TAB(IXEL)%BUFLY(ILAY)%LBUF(IR,IS,IT)%OFF
          ELSEIF (NXLAY== 1) THEN
            XOFF => XFEM_TAB(IXEL)%GBUF%OFF
          ENDIF
C---
          DO NCUT=1,NELCRK
            I = JCT(NCUT)
            ELCRK = IEL_CRK(I+NFT)
            ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
            IF (ELCUT /= 0) THEN
c---          set nodal enrichment to zero for desactivated phantom elements
              IF (XOFF(I) == ZERO) THEN
                IADC1 = IADC_CRK(1,ELCRK)
                IADC2 = IADC_CRK(2,ELCRK)
                IADC3 = IADC_CRK(3,ELCRK)
                IADC4 = IADC_CRK(4,ELCRK)
                CRKLVSET(ILEV)%ENR0(1,IADC1) = 0
                CRKLVSET(ILEV)%ENR0(1,IADC2) = 0
                CRKLVSET(ILEV)%ENR0(1,IADC3) = 0
                CRKLVSET(ILEV)%ENR0(1,IADC4) = 0
              END IF
c             update ICUTEDGE (ICUTEDGE=1) : tip edge becomes internal
              DO K=1,4
                EDGE  = XEDGE4N(K,ELCRK)
                IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
                IF (IECUT /= 0)  CRKEDGE(ILAY)%ICUTEDGE(EDGE) = 1
              ENDDO
C
              LAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
              IF (ABS(LAYCUT) == 1) CRKEDGE(ILAY)%LAYCUT(ELCRK) = 2
            ENDIF ! IF(ELCUT /= 0)
          ENDDO ! DO NCUT=1,NELCRK
        ENDDO !  DO ILAY=1,NXLAY
      ENDDO  !  DO IXEL=1,NXEL
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  UPENRIC3_N3                   source/elements/xfem/upenric3_nx.F
Chd|-- called by -----------
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPENRIC3_N3(XFEM_TAB,
     .                    IPARG  ,IXTG     ,NFT    ,JFT     ,JLT  ,
     .                    ELCUTC ,IAD_CRKTG,IEL_CRKTG,INOD_CRK,IXFEM,
     .                    CRKEDGE,XEDGE3N  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXTG(NIXTG,*),NFT,JFT,JLT,IXFEM,
     .  ELCUTC(2,*),IAD_CRKTG(3,*),XEDGE3N(3,*),IEL_CRKTG(*),
     .  INOD_CRK(*)
C
      TYPE(ELBUF_STRUCT_), TARGET ,DIMENSION(NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ELCRK,ELCRKTG,ELCUT,IADC(3),
     .        JCT(MVSIZ),NELCRK,EDGE,LAYCUT,IECUT,NCUT,
     .        IR,IS,IT,ILAY,NXLAY,IXEL,ILEV
      my_real, DIMENSION(:) ,POINTER  ::  XOFF
C=======================================================================
      NELCRK = 0
      DO I=JFT,JLT
        JCT(I) = 0
        IF (ELCUTC(1,I+NFT) /= 0) THEN
          NELCRK = NELCRK + 1
          JCT(NELCRK) = I
        ENDIF
      ENDDO
      IF (NELCRK == 0) RETURN
C-----------------------------------------------
      IR = 1
      IS = 1
      IT = 1
C
      DO IXEL=1,NXEL
        NXLAY = XFEM_TAB(IXEL)%NLAY
        DO ILAY=1,NXLAY
          IF (NXLAY> 1) THEN
            XOFF => XFEM_TAB(IXEL)%BUFLY(ILAY)%LBUF(IR,IS,IT)%OFF
          ELSEIF (NXLAY== 1) THEN
            XOFF => XFEM_TAB(IXEL)%GBUF%OFF
          ENDIF
C---
          ILEV = NXEL*(ILAY-1) + IXEL
C---
          DO NCUT=1,NELCRK
            I = JCT(NCUT)
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK  = ELCRKTG + ECRKXFEC
            ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
            IF (ELCUT /= 0) THEN
C
              IADC(1) = IAD_CRKTG(1,ELCRKTG)
              IADC(2) = IAD_CRKTG(2,ELCRKTG)
              IADC(3) = IAD_CRKTG(3,ELCRKTG)
C---
              IF (XOFF(I) == ZERO) THEN
                CRKLVSET(ILEV)%ENR0(1,IADC(1)) = 0
                CRKLVSET(ILEV)%ENR0(1,IADC(2)) = 0
                CRKLVSET(ILEV)%ENR0(1,IADC(3)) = 0
              ENDIF ! IF(OFF == ZERO)
C
              DO K=1,3
                EDGE  = XEDGE3N(K,ELCRKTG)
                IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
                IF (IECUT /= 0)  CRKEDGE(ILAY)%ICUTEDGE(EDGE) = 1
              ENDDO
C
              LAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
              IF (ABS(LAYCUT) == 1) CRKEDGE(ILAY)%LAYCUT(ELCRK) = 2
            ENDIF ! IF(ELCUT /= 0)
          ENDDO ! DO NCUT=1,NELCRK
        ENDDO  !  DO ILAY=1,NXLAY
      ENDDO  !  DO IXEL=1,NXEL
C-----------------------------------------------
      RETURN
      END
