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
Chd|  PREINICRK3N                   source/elements/xfem/preinicrk3N.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        LSINTX                        source/elements/xfem/preinicrk4N.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE PREINICRK3N(ELBUF_STR,XFEM_STR,
     .                    X1L     ,Y1L     ,X2L     ,Y2L     ,X3L     ,
     .                    Y3L     ,LFT     ,LLT     ,NFT     ,NXLAY   ,
     .                    IELCRKTG,EDGETG  ,BETA0   ,IEDGESH3,ELCUT   ,
     .                    XNOD    ,IXTG    ,NODEDGE ,TAGSKYTG,KNOD2ELC,
     .                    TAGEDGE ,CRKLVSET,CRKSHELL,CRKEDGE ,XFEM_PHANTOM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE XFEM2DEF_MOD
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
C   C o m m o n   B l o c K s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NFT,NXLAY
      INTEGER IELCRKTG(*),EDGETG(3,*),IEDGESH3(3,*),ELCUT(*),
     .  XNOD(2,2),IXTG(NIXTG,*),NODEDGE(2,*),TAGSKYTG(3,*),KNOD2ELC(*),
     .  TAGEDGE(*)
      my_real
     . X1L(*),Y1L(*),X2L(*),Y2L(*),X3L(*),Y3L(*),BETA0(2)
C
      TYPE (ELBUF_STRUCT_), TARGET  :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL) , TARGET :: XFEM_STR
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX)       :: CRKLVSET
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)       :: CRKSHELL
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX)      :: CRKEDGE
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX)      :: XFEM_PHANTOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,II,IL,ILAY,ELCRK,IED,ICUT,IEDGE,IC1,IC2,ICRK,p1,p2,
     .  NOD1,NOD2,JCRK,IXEL
      INTEGER dd(3),d1(3),d2(3),IFI(2),ILEV(NXEL),N(3),ISIGN0(3),
     .  IENR0(3),IENR(3),NTAG(3)
      my_real
     .   FIT(3,MVSIZ),XN(3),YN(3),XMI(2),YMI(2),BETA(2,MVSIZ),
     .   OFF_PHANTOM
      my_real  LSINTX
      EXTERNAL LSINTX
      DATA dd/2,3,1/
      DATA d1/2,3,4/
      DATA d2/3,4,2/
C
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
C=======================================================================
      DO I=LFT,LLT
        XN(1)=X1L(I)
        YN(1)=Y1L(I)
        XN(2)=X2L(I)
        YN(2)=Y2L(I)
        XN(3)=X3L(I)
        YN(3)=Y3L(I)
        IF (ELCUT(I+NFT) > 0) THEN
          DO K=1,3
            p1 = K
            p2 = dd(K)
            IED = EDGETG(K,I+NFT)
            IF (IED > 0) THEN
              XMI(IED) = UNDEMI*(XN(p1) + XN(p2))
              YMI(IED) = UNDEMI*(YN(p1) + YN(p2))
            ENDIF
          ENDDO
C
          DO K=1,3
            FIT(K,I) = LSINTX(XMI(1),YMI(1),XMI(2),YMI(2),XN(K),YN(K))
          ENDDO
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        ELCRK = IELCRKTG(I+NFT)
        BETA(1,I) = ZERO
        BETA(2,I) = ZERO
        IF(ELCUT(I+NFT) > 0)THEN
C
          JCRK = ELCRK - ECRKXFEC
          DO K=1,3
            IEDGE = IEDGESH3(K,JCRK)
            IED = EDGETG(K,I+NFT)
            IF (IED > 0) THEN
              NOD1 = NODEDGE(1,IEDGE)
              NOD2 = NODEDGE(2,IEDGE)
              IF (NOD1 == XNOD(IED,1) .and. NOD2 == XNOD(IED,2)) THEN
                BETA(IED,I) = BETA0(IED)
              ELSE IF (NOD2 == XNOD(IED,1) .and. NOD1 == XNOD(IED,2)) THEN
                BETA(IED,I) = UN - BETA0(IED)
              END IF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)
        DO K=1,NXEL
          ILEV(K) = II + K
        ENDDO
        DO I=LFT,LLT
          ELCRK = IELCRKTG(I+NFT)
          JCRK = ELCRK - ECRKXFEC
          IF (ELCUT(I+NFT) > 0) THEN
            ICRK = CRKSHELL(ILEV(1))%PHANTOMG(ELCRK)
            CRKLVSET(ILEV(1))%ELCUT(ELCRK) =  ICRK
            CRKLVSET(ILEV(2))%ELCUT(ELCRK) = -ICRK
c
            XFEM_PHANTOM(ILAY)%ELCUT(ELCRK) = ICRK
            CRKEDGE(ILAY)%LAYCUT(ELCRK) = 2
C
            N(1) = IXTG(2,I+NFT)
            N(2) = IXTG(3,I+NFT)
            N(3) = IXTG(4,I+NFT)
C
            ISIGN0(1) = INT(SIGN(UN,FIT(1,I))) * ICRK
            ISIGN0(2) = INT(SIGN(UN,FIT(2,I))) * ICRK
            ISIGN0(3) = INT(SIGN(UN,FIT(3,I))) * ICRK
C
            NTAG(1:3) = 0
C
            DO K=1,3
              IENR0(K) = 0
              IENR(K)  = 0
              IED = EDGETG(K,I+NFT)
              IF (IED > 0) THEN
                NTAG(K)     = NTAG(K) + 1
                NTAG(dd(K)) = NTAG(dd(K)) + 1
              ENDIF
            ENDDO
C
            DO K=1,3
              IED = EDGETG(K,I+NFT)
              IEDGE = IEDGESH3(K,JCRK)
              IF(IED > 0)THEN
                NOD1 = NODEDGE(1,IEDGE)
                NOD2 = NODEDGE(2,IEDGE)
                IF(NOD1 == N(K) .and. NOD2 == N(dd(K)))THEN
                  p1 = K
                  p2 = dd(K)
                ELSE IF(NOD2 == N(K) .and. NOD1 == N(dd(K)))THEN
                  p1 = dd(K)
                  p2 = K
                END IF
                IF(NTAG(p1) > 0.AND.CRKEDGE(ILAY)%EDGEENR(1,IEDGE) > 0)
     .            IENR0(p1) = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)
                IF(NTAG(p2) > 0.AND.CRKEDGE(ILAY)%EDGEENR(2,IEDGE) > 0)
     .            IENR0(p2) = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)
              ENDIF
            ENDDO
C
            DO K=1,3
              IF(IENR0(K) /= 0)THEN
                IENR(K) = IENR0(K)
              ELSE
                IENR(K) = TAGSKYTG(K,I+NFT)+KNOD2ELC(N(K))*(ILAY-1)
              ENDIF
            ENDDO
C
            DO K=1,3
              IED = EDGETG(K,I+NFT)
              IEDGE = IEDGESH3(K,JCRK)
              IF(IED > 0)THEN
                DO IL=1,NXEL
                  CRKLVSET(ILEV(IL))%EDGETG(K,JCRK) = IED ! (=1,2)
                  CRKLVSET(ILEV(IL))%ICUTEDGE(IEDGE) = 1
                  CRKLVSET(ILEV(IL))%RATIOEDGE(IEDGE) = BETA(IED,I)
                ENDDO
C
                CRKEDGE(ILAY)%EDGETIP(1,IEDGE) = MAX(IED,
     .                                     CRKEDGE(ILAY)%EDGETIP(1,IEDGE))
                CRKEDGE(ILAY)%EDGETIP(2,IEDGE) = 
     .                        CRKEDGE(ILAY)%EDGETIP(2,IEDGE) + 1
C
c               add check if BETA (0:1)
C
                IF(CRKEDGE(ILAY)%EDGEICRK(IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEICRK(IEDGE)  = ICRK
C
                NOD1 = NODEDGE(1,IEDGE)
                NOD2 = NODEDGE(2,IEDGE)
                IF(NOD1 == N(K) .and. NOD2 == N(dd(K)))THEN
                  IFI(1) = ISIGN0(K)
                  IFI(2) = ISIGN0(dd(K))
                  p1 = K
                  p2 = dd(K)
                ELSE IF(NOD2 == N(K) .and. NOD1 == N(dd(K)))THEN
                  IFI(1) = ISIGN0(dd(K))
                  IFI(2) = ISIGN0(K)
                  p1 = dd(K)
                  p2 = K
                END IF
                IF(CRKEDGE(ILAY)%EDGEIFI(1,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEIFI(1,IEDGE) = IFI(1)
                IF(CRKEDGE(ILAY)%EDGEIFI(2,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEIFI(2,IEDGE) = IFI(2)
cc                CRKEDGE(ILAY)%EDGEENR(1,IEDGE) = IENR(p1)
                IF(CRKEDGE(ILAY)%EDGEENR(1,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEENR(1,IEDGE) = IENR(p1)
cc                CRKEDGE(ILAY)%EDGEENR(2,IEDGE) = IENR(p2)
                IF(CRKEDGE(ILAY)%EDGEENR(2,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEENR(2,IEDGE) = IENR(p2)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
c     activation of cracked elements (OFFG = 1)
C
      IF (NXLAY > 1)THEN  ! multilayer
c        DO IXEL=1,NXEL   ! ATTENTION: Third phantom not activated yet
        DO IXEL=1,2     
          DO ILAY=1,NXLAY
            LBUF => XFEM_STR(IXEL)%BUFLY(ILAY)%LBUF(1,1,1)
            DO I=LFT,LLT
              IF(ELCUT(I+NFT) > 0)THEN
                OFF_PHANTOM = LBUF%OFF(I)
                LBUF%OFF(I) = - OFF_PHANTOM
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ELSE  ! monolayer
c        DO IXEL=1,NXEL   ! ATTENTION: Third phantom not activated yet
        DO IXEL=1,2
          GBUF => XFEM_STR(IXEL)%GBUF
          DO I=LFT,LLT
            IF(ELCUT(I+NFT) > 0)THEN
              OFF_PHANTOM = GBUF%OFF(I)
              GBUF%OFF(I) = - OFF_PHANTOM
            ENDIF
          ENDDO
        ENDDO
      ENDIF  !  IF(NXLAY > 1)THEN
C
c     remove already cracked elements (OFFG = 0)
C
      DO I=LFT,LLT
        IF(ELCUT(I+NFT) > 0)THEN
          ELBUF_STR%GBUF%OFF(I) = ZERO
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        ELCRK = IELCRKTG(I+NFT) - ECRKXFEC
        IF (ELCUT(I+NFT) > 0) THEN
          DO K=1,3
            IED   = EDGETG(K,I+NFT)
            IEDGE = IEDGESH3(K,ELCRK)
            IF (IED > 0 .and. IEDGE > 0) THEN
              TAGEDGE(IEDGE) = TAGEDGE(IEDGE) + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
