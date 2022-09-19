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
Chd|  PRECRKLAY                     source/elements/xfem/precrklay.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE PRECRKLAY(JFT     ,JLT    ,NFT     ,NLAY   ,ELCRKINI,
     .                     IEL_CRK,INOD_CRK,NODENR  ,CRKEDGE,XEDGE4N )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NLAY
      INTEGER ELCRKINI(NLAY,*),IEL_CRK(*),INOD_CRK(*),
     .   NODENR(*),XEDGE4N(4,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ILAY,ELCRK,ELCUT,IEDGE,ICUT
C=======================================================================
C     check for advancing within an uncut element layer
C-----------------------
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)  ! N  systeme elem xfem
        DO ILAY=1,NLAY
          ELCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ELCUT == 0)THEN   ! not cut yet
c           tag uncut layer for advancing
            ICUT = 0
            DO K=1,4   ! edges
              IEDGE = XEDGE4N(K,ELCRK)    ! N  local de l'arrete (elem sys xfem)
              ICUT  = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)    ! flag arrete coupe/non
              IF (ICUT == 1) THEN        ! tag elements avec un crack sur le bord
                ELCRKINI(ILAY,I) = 2    ! avancement de fissure dans l'element possible
                EXIT
              ENDIF
            ENDDO
          ENDIF ! IF(ELCUT == 0)THEN
        ENDDO ! DO ILAY=1,NLAY
      ENDDO ! DO I=JFT,JLT
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  PRECRKLAYTG                   source/elements/xfem/precrklay.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE PRECRKLAYTG(JFT      ,JLT      ,NFT     ,NLAY    ,ELCRKINI,
     .                       IEL_CRKTG,INOD_CRK ,NODENR  ,CRKEDGE ,XEDGE3N )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NLAY
      INTEGER ELCRKINI(NLAY,*),IEL_CRKTG(*),INOD_CRK(*),
     .   NODENR(*),XEDGE3N(3,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ILAY,ELCRK,ELCRKTG,ELCUT,IEDGE,ICUT
C=======================================================================
C     check for advancing within an uncut element layer
C-----------------------
      DO I=JFT,JLT
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK = ELCRKTG + ECRKXFEC
        DO ILAY=1,NLAY
          ELCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ELCUT == 0) THEN
c           tag uncut layer for advancing
            ICUT = 0
            DO K=1,3
              IEDGE = XEDGE3N(K,ELCRKTG)
              ICUT  = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
              IF (ICUT == 1) THEN
                ELCRKINI(ILAY,I) = 2
                EXIT
              ENDIF
            ENDDO
          ENDIF ! IF(ELCUT == 0)THEN
        ENDDO   ! DO ILAY=1,NLAY
      ENDDO     ! DO I=JFT,JLT
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  CRKOFFC                       source/elements/xfem/precrklay.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CRKOFFC(ELBUF_STR,XFEM_STR  ,
     .                   JFT      ,JLT       ,NFT    ,IR      ,IS        ,
     .                   NXLAY    ,IEL_CRK   ,CRKEDGE,XEDGE4N )
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
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NXLAY,IR,IS
      INTEGER IEL_CRK(*),XEDGE4N(4,*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL), TARGET :: XFEM_STR  ! take xfem_str
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ILAY,ELCRK,IFAC,ILAYCRK,IEDGE,IXEL
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------
c     tag bord libre d'un element std efface dans une loi
c     on delete les phantomes
C=======================================================================
      GBUF => ELBUF_STR%GBUF
C---
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)   ! N  element sys xfem
        IF (ELCRK == 0) CYCLE
        IFAC = 0
        DO ILAY=1,NXLAY
          ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ILAYCRK /= 0) CYCLE
          IF (GBUF%OFF(I) == ZERO) IFAC = IFAC + 1
        ENDDO
C----
        IF (IFAC == NXLAY) THEN
          DO ILAY=1,NXLAY
            DO K=1,4
              IEDGE = XEDGE4N(K,ELCRK)
              CRKEDGE(ILAY)%IBORDEDGE(IEDGE) = 2   ! devient bord libre
            ENDDO
C---
            DO IXEL=1,NXEL
              IF (NXLAY == 1) THEN
                XFEM_STR(IXEL)%GBUF%OFF(I) = ZERO
              ELSEIF (NXLAY > 1) THEN
                XFEM_STR(IXEL)%BUFLY(ILAY)%LBUF(IR,IS,1)%OFF(I) = ZERO
              ENDIF
            ENDDO
C---
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  CRKOFFTG                      source/elements/xfem/precrklay.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CRKOFFTG(ELBUF_STR,XFEM_STR  ,
     .                    JFT      ,JLT       ,NFT    ,IR      ,IS        ,
     .                    NXLAY    ,IEL_CRKTG ,CRKEDGE,XEDGE3N )
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
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NXLAY,IR,IS
      INTEGER IEL_CRKTG(*),XEDGE3N(3,*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL), TARGET :: XFEM_STR  ! take xfem_str
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ILAY,ELCRKTG,ELCRK,IFAC,ILAYCRK,IEDGE,IXEL
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C=======================================================================
      GBUF => ELBUF_STR%GBUF
C---
      DO I=JFT,JLT
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK = ELCRKTG + ECRKXFEC
        IF (ELCRK == 0) CYCLE
        IFAC = 0
        DO ILAY=1,NXLAY
          ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ILAYCRK /= 0) CYCLE
          IF (GBUF%OFF(I) == ZERO) IFAC = IFAC + 1
        ENDDO
C----
        IF (IFAC == NXLAY) THEN
          DO ILAY=1,NXLAY
            DO K=1,3
              IEDGE = XEDGE3N(K,ELCRKTG)
              CRKEDGE(ILAY)%IBORDEDGE(IEDGE) = 2
            ENDDO
C---
            DO IXEL=1,NXEL
              IF (NXLAY == 1) THEN
                XFEM_STR(IXEL)%GBUF%OFF(I) = ZERO
              ELSEIF (NXLAY > 1) THEN
                XFEM_STR(IXEL)%BUFLY(ILAY)%LBUF(IR,IS,1)%OFF(I) = ZERO
              ENDIF
            ENDDO
C---
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
