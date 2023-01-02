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
Chd|  UPOFFC                        source/elements/xfem/upoffc.F 
Chd|-- called by -----------
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPOFFC(XFEM_TAB ,NG      ,
     .                  NFT      ,JFT     ,JLT      ,IXFEM    ,IEL_CRK ,
     .                  ELCUTC   ,INOD_CRK,IADC_CRK ,IXC      ,NXLAY   ,
     .                  CRKEDGE  ,XEDGE4N )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NG,NFT,JFT,JLT,IXFEM,NXLAY,
     .  IEL_CRK(*),ELCUTC(2,*),INOD_CRK(*),IADC_CRK(4,*),XEDGE4N(4,*),
     .  IXC(NIXC,*)
C
      TYPE(ELBUF_STRUCT_),  DIMENSION(NGROUP,NXEL) :: XFEM_TAB
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IEL,NEL,ELCRK,ICUT,ILAYCUT,ILAY,ILEV,ITRI,P1,P2,
     .  IEDGE,IBOUNDEDGE,ELCUT,IXEL,IR,IS,IT,IEDGE0,FAC,ICUT0
      INTEGER D(4),NN(4),OPEN_EDGE(4,NXLAY),ENR_NOD(4),IADC(4),IOFF(NXEL,JLT)
      my_real, DIMENSION(:) ,POINTER  ::  XOFF
      DATA d/2,3,4,1/
C=======================================================================
      IOFF = 0
      IR = 1
      IS = 1
      IT = 1
      NEL = JLT - JFT + 1
C
      DO I=JFT,JLT
        IEL  = I+NFT
        ICUT = ELCUTC(1,IEL)
        IF (ICUT == 0) CYCLE
        ELCRK = IEL_CRK(IEL)
C
        IADC(1) = IADC_CRK(1,ELCRK)
        IADC(2) = IADC_CRK(2,ELCRK)
        IADC(3) = IADC_CRK(3,ELCRK)
        IADC(4) = IADC_CRK(4,ELCRK)
C
        NN(1) = INOD_CRK(IXC(2,IEL))   ! noeud sys xfem
        NN(2) = INOD_CRK(IXC(3,IEL))
        NN(3) = INOD_CRK(IXC(4,IEL))
        NN(4) = INOD_CRK(IXC(5,IEL))
C
        DO ILAY=1,NXLAY
          ILAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          DO K=1,4
            OPEN_EDGE(K,ILAY) = 0
          ENDDO
c
          DO IXEL=1,NXEL
            IF (NXLAY> 1) THEN
              XOFF => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(IR,IS,IT)%OFF(1:NEL)
            ELSEIF (NXLAY== 1) THEN
              XOFF => XFEM_TAB(NG,IXEL)%GBUF%OFF(1:NEL)
            ENDIF
C---
            IF (ABS(ILAYCUT) == 1) THEN  !  new crack initialized ou avance (+1,-1)
c             activate new cracked layer:
              XOFF(I) = -XOFF(I)
            ELSE IF (ILAYCUT == 2) THEN  ! old cut layer
c             delete already cracked phantom element if new crack is touching it
c
              ILEV = NXEL*(ILAY-1) + IXEL
              ENR_NOD(1) = CRKLVSET(ILEV)%ENR0(1,IADC(1))
              ENR_NOD(2) = CRKLVSET(ILEV)%ENR0(1,IADC(2))
              ENR_NOD(3) = CRKLVSET(ILEV)%ENR0(1,IADC(3))
              ENR_NOD(4) = CRKLVSET(ILEV)%ENR0(1,IADC(4))
              DO K=1,4
                p1 = K
                p2 = d(K)
                IEDGE = XEDGE4N(K,ELCRK)
                IBOUNDEDGE = 0
                ICUT0      = 0
                IEDGE0 = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                IF (IEDGE > 0) THEN
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(IEDGE)
                  ICUT0      = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
                ENDIF
                IF (IBOUNDEDGE == 0 .AND. IEDGE0 == 0 .AND. ICUT0 > 0) THEN
                  IF (ENR_NOD(p1) == 0 .AND. ENR_NOD(p2) == 0) THEN
                    XOFF(I) = ZERO
                    IOFF(IXEL,I) = 1
                    OPEN_EDGE(K,ILAY) = 1     ! bord libre
                  ENDIF
                ENDIF
              ENDDO
C---
            ENDIF  !  IF (ABS(ILAYCUT) == 1)
          ENDDO  !  DO IXEL=1,NXEL
c
          FAC = 0                                      
          DO K=1,4                                     
            IF (OPEN_EDGE(K,ILAY) == 1) FAC = FAC + 1  
          ENDDO                                        
          IF (FAC > 0) THEN                            
            DO K=1,4                                   
              IEDGE = XEDGE4N(K,ELCRK)                 
              IF (OPEN_EDGE(K,ILAY) == 1) THEN         
                CRKEDGE(ILAY)%IBORDEDGE(IEDGE) = 2     
              ENDIF                                    
            ENDDO                                      
          ENDIF                                        
C--------------------------
c         delete both phantoms on the same side for ITRI /= 0
C--------------------------
          ITRI  = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)
          IF (ITRI < 0) THEN
            IF (NXLAY > 1) THEN
              IF (XFEM_TAB(NG,2)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,3)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) =  ZERO
              ELSEIF (XFEM_TAB(NG,3)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,2)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) =  ZERO
              ENDIF
            ELSE
              IF (XFEM_TAB(NG,2)%GBUF%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,3)%GBUF%OFF(I) =  ZERO
              ELSEIF (XFEM_TAB(NG,3)%GBUF%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,2)%GBUF%OFF(I) =  ZERO
              ENDIF
            ENDIF              
          ELSEIF (ITRI > 0) THEN
            IF (NXLAY > 1) THEN
              IF (XFEM_TAB(NG,1)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,3)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) =  ZERO
              ELSEIF (XFEM_TAB(NG,3)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,1)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) =  ZERO
              ENDIF
            ELSE
              IF (XFEM_TAB(NG,1)%GBUF%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,3)%GBUF%OFF(I) =  ZERO
              ELSEIF (XFEM_TAB(NG,3)%GBUF%OFF(I) == ZERO) THEN
                  XFEM_TAB(NG,1)%GBUF%OFF(I) =  ZERO
              ENDIF
            ENDIF              
          ENDIF
C---
        ENDDO  !  DO ILAY=1,NXLAY
      ENDDO  !  DO I=JFT,JLT
C-----------------------------------------------
      RETURN
      END
