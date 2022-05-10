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
Chd|  PREINICRK4N                   source/elements/xfem/preinicrk4N.F
Chd|-- called by -----------
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        LSINTX                        source/elements/xfem/preinicrk4N.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE PREINICRK4N(ELBUF_STR,XFEM_STR,
     .               X1L     ,Y1L     ,X2L     ,Y2L      ,X3L     ,
     .               Y3L     ,X4L     ,Y4L     ,LFT      ,LLT     ,
     .               NFT     ,NXLAY   ,IELCRKC ,EDGEC    ,BETA0   ,
     .               IEDGESH4,ELCUT   ,XNOD    ,IXC      ,NODEDGE ,
     .               TAGSKYC ,KNOD2ELC,TAGEDGE ,CRKLVSET ,CRKSHELL,
     .               CRKEDGE ,XFEM_PHANTOM)
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
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,NFT,NXLAY
      INTEGER IELCRKC(*),EDGEC(4,*),IEDGESH4(4,*),ELCUT(*),XNOD(2,2),
     . IXC(NIXC,*),NODEDGE(2,*),TAGSKYC(4,*),KNOD2ELC(*),TAGEDGE(*)
      my_real
     . X1L(*),Y1L(*),X2L(*),Y2L(*),X3L(*),Y3L(*),X4L(*),Y4L(*),
     . BETA0(2)
C
      TYPE (ELBUF_STRUCT_), TARGET   :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL) , TARGET :: XFEM_STR
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX)       :: CRKLVSET
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)       :: CRKSHELL
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX)      :: CRKEDGE
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX)      :: XFEM_PHANTOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,II,R,ELCRK,IED,p1,p2,dd(4),d1(4),d2(4),IFI(2),
     . ICUT,IEDGE,IC1,IC2,ICRK,ILEV(NXEL),IL,ILAY,N(4),ISIGN0(4),
     . NOD1,NOD2,IXEL,IENR0(4),IENR(4),NTAG(4)
      my_real
     .   FIT(4,MVSIZ),XN(4),YN(4),XMI(2),YMI(2),BETA(2,MVSIZ),
     .   OFF_PHANTOM
      EXTERNAL  LSINTX
      my_real  LSINTX
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
C
      DATA dd/2,3,4,1/
      DATA d1/2,3,4,5/
      DATA d2/3,4,5,2/
C=======================================================================
      DO I=LFT,LLT
        XN(1)=X1L(I)
        YN(1)=Y1L(I)
        XN(2)=X2L(I)
        YN(2)=Y2L(I)
        XN(3)=X3L(I)
        YN(3)=Y3L(I)
        XN(4)=X4L(I)
        YN(4)=Y4L(I)
        IF (ELCUT(I+NFT) > 0) THEN
          DO r=1,4       ! edges
            p1 = r
            p2 = dd(r)
            IED = EDGEC(r,I+NFT)
            IF (IED > 0) THEN
              XMI(IED) = UNDEMI*(XN(p1)+XN(p2))
              YMI(IED) = UNDEMI*(YN(p1)+YN(p2))
            ENDIF
          ENDDO
C
          DO r=1,4     ! nodes
            FIT(r,I) = LSINTX(XMI(1),YMI(1),XMI(2),YMI(2),XN(r),YN(r))
          ENDDO
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        ELCRK = IELCRKC(I+NFT)
        BETA(1,I) = ZERO
        BETA(2,I) = ZERO
        IF (ELCUT(I+NFT) > 0) THEN
C
          DO r=1,4   ! edges
            IEDGE = IEDGESH4(r,ELCRK)
            IED = EDGEC(r,I+NFT)
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
      ENDDO   !   I=LFT,LLT
c------------------------------------------------------------
c
      DO ILAY=1,NXLAY
        II = NXEL*(ILAY-1)
        DO K=1,NXEL
          ILEV(K) = II + K
        ENDDO
        DO I=LFT,LLT
          ELCRK = IELCRKC(I+NFT)
          IF (ELCUT(I+NFT) > 0) THEN
            ICRK = CRKSHELL(ILEV(1))%PHANTOMG(ELCRK)  ! global xfem element N
            CRKLVSET(ILEV(1))%ELCUT(ELCRK) =  ICRK ! virer
            CRKLVSET(ILEV(2))%ELCUT(ELCRK) = -ICRK ! virer
c
            XFEM_PHANTOM(ILAY)%ELCUT(ELCRK) = ICRK
            CRKEDGE(ILAY)%LAYCUT(ELCRK) = 2
C
            N(1) = IXC(2,I+NFT)
            N(2) = IXC(3,I+NFT)
            N(3) = IXC(4,I+NFT)
            N(4) = IXC(5,I+NFT)
C
            ISIGN0(1) = INT(SIGN(UN,FIT(1,I))) * ICRK
            ISIGN0(2) = INT(SIGN(UN,FIT(2,I))) * ICRK
            ISIGN0(3) = INT(SIGN(UN,FIT(3,I))) * ICRK
            ISIGN0(4) = INT(SIGN(UN,FIT(4,I))) * ICRK
C
            NTAG(1:4) = 0
C
            DO r=1,4
              IENR0(r) = 0
              IENR(r)=0
              IED = EDGEC(r,I+NFT)
              IF(IED > 0)THEN
                NTAG(r) = NTAG(r) + 1
                NTAG(dd(r)) = NTAG(dd(r)) + 1
              ENDIF
            ENDDO
C
            DO r=1,4
              IED = EDGEC(r,I+NFT)
              IEDGE = IEDGESH4(r,ELCRK)
              IF(IED > 0)THEN
                NOD1 = NODEDGE(1,IEDGE)
                NOD2 = NODEDGE(2,IEDGE)
                IF(NOD1 == N(r) .and. NOD2 == N(dd(r)))THEN
                  p1 = r
                  p2 = dd(r)
                ELSE IF(NOD2 == N(r) .and. NOD1 == N(dd(r)))THEN
                  p1 = dd(r)
                  p2 = r
                END IF
                IF(NTAG(p1) > 0.AND.CRKEDGE(ILAY)%EDGEENR(1,IEDGE) > 0)
     .            IENR0(p1) = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)
                IF(NTAG(p2) > 0.AND.CRKEDGE(ILAY)%EDGEENR(2,IEDGE) > 0)
     .            IENR0(p2) = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)
              ENDIF
            ENDDO
C
            DO r=1,4
              IF(IENR0(r) /= 0)THEN
                IENR(r) = IENR0(r)
              ELSE
                IENR(r) = TAGSKYC(r,I+NFT)+KNOD2ELC(N(r))*(ILAY-1)
              ENDIF
            ENDDO
C
            DO r=1,4
              IED = EDGEC(r,I+NFT)
              IEDGE = IEDGESH4(r,ELCRK)
              IF (IED > 0) THEN
                DO IL=1,NXEL
                  CRKLVSET(ILEV(IL))%EDGE(r,ELCRK) = IED ! (=1,2)
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
                IF(NOD1 == N(r) .and. NOD2 == N(dd(r)))THEN
                  IFI(1) = ISIGN0(r)
                  IFI(2) = ISIGN0(dd(r))
                  p1 = r
                  p2 = dd(r)
                ELSE IF(NOD2 == N(r) .and. NOD1 == N(dd(r)))THEN
                  IFI(1) = ISIGN0(dd(r))
                  IFI(2) = ISIGN0(r)
                  p1 = dd(r)
                  p2 = r
                END IF
                IF(CRKEDGE(ILAY)%EDGEIFI(1,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEIFI(1,IEDGE) = IFI(1)
                IF(CRKEDGE(ILAY)%EDGEIFI(2,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEIFI(2,IEDGE) = IFI(2)
                IF(CRKEDGE(ILAY)%EDGEENR(1,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEENR(1,IEDGE) = IENR(p1)
                IF(CRKEDGE(ILAY)%EDGEENR(2,IEDGE) == 0)
     .             CRKEDGE(ILAY)%EDGEENR(2,IEDGE) = IENR(p2)
              ENDIF
            ENDDO  ! r=1,4
          ENDIF
        ENDDO      ! I=LFT,LLT
      ENDDO        ! ILAY=1,NXLAY
C------------------------------------------------------
c     activation of cracked elements (OFFG = 1)
C------------------------------------------------------
      IF (NXLAY > 1) THEN  ! multilayer
        DO IXEL=1,NXEL
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
        DO IXEL=1,NXEL
          GBUF => XFEM_STR(IXEL)%GBUF
          DO I=LFT,LLT
           IF(ELCUT(I+NFT) > 0)THEN
              OFF_PHANTOM = GBUF%OFF(I)
              GBUF%OFF(I) = - OFF_PHANTOM
            ENDIF
          ENDDO
        ENDDO
      ENDIF  !  IF(NXLAY > 1)THEN
C------------------------------------------------------
c     remove (replace) already cracked elements (OFFG = 0)
C------------------------------------------------------
      DO I=LFT,LLT
        IF(ELCUT(I+NFT) > 0)THEN
          ELBUF_STR%GBUF%OFF(I) = ZERO
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        ELCRK = IELCRKC(I+NFT)
        IF(ELCUT(I+NFT) > 0)THEN
          DO r=1,4
            IED = EDGEC(r,I+NFT)
            IEDGE = IEDGESH4(r,ELCRK)
            IF(IED > 0)THEN
              TAGEDGE(IEDGE) = TAGEDGE(IEDGE) + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  LSINTX                        source/elements/xfem/preinicrk4N.F
Chd|-- called by -----------
Chd|        PREINICRK3N                   source/elements/xfem/preinicrk3N.F
Chd|        PREINICRK4N                   source/elements/xfem/preinicrk4N.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION LSINTX(Y1, Z1, Y2, Z2, Y, Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      MY_REAL
     .   Y1,Z1,Y2,Z2,Y,Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      MY_REAL ARIA,AB
C===================================================
      ARIA = ((Y2*Z-Y*Z2)-(Y1*Z-Y*Z1)+(Y1*Z2-Z1*Y2))
      AB   = (Y2-Y1)**2 + (Z2-Z1)**2
      IF (AB > ZERO) THEN
        LSINTX = ARIA/SQRT(AB)
      ELSE
        LSINTX = ZERO
      ENDIF
C-----------
      RETURN
      END
