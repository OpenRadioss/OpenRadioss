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
Chd|  UPENRIC2_N4                   source/elements/xfem/upenric2_n4.F
Chd|-- called by -----------
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENRIC2_N4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,
     .                       ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,
     .                       NXLAY  ,CRKEDGE ,XEDGE4N)
C-----------------------------------------------
      USE CRACKXFEM_MOD
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
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),ELCUTC(2,*),INOD_CRK(*),
     .  IADC_CRK(4,*),IEL_CRK(*),ENRTAG(NUMNOD,*),XEDGE4N(4,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,IAD,NELCRK,ELCRK,ILEV,ILAY,IXEL,ELCUT,LAYCUT,
     .   IECUT,ENR,IBOUNDEDGE,IED,EDGE,FAC,COUNT,ITIP,ITRI,NUMXEL,NSX
      INTEGER JCT(MVSIZ),NTAG(4),D(4),NS(4),IADC(4)
      DATA D/2,3,4,1/
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
c--------------------
      DO ILAY=1,NXLAY
        DO IR=1,NELCRK
          I = JCT(IR)
          ELCRK = IEL_CRK(I+NFT)                       
          ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)     
          IF (ELCUT /= 0) THEN
            ITRI    = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)    
            LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)         
            NS(1)   = IXC(2,I+NFT)       
            NS(2)   = IXC(3,I+NFT)       
            NS(3)   = IXC(4,I+NFT)       
            NS(4)   = IXC(5,I+NFT)       
            IADC(1) = IADC_CRK(1,ELCRK)  
            IADC(2) = IADC_CRK(2,ELCRK)  
            IADC(3) = IADC_CRK(3,ELCRK)  
            IADC(4) = IADC_CRK(4,ELCRK)
            IF (ITRI == 0) THEN
              NUMXEL = 2
            ELSE
              NUMXEL = NXEL
            ENDIF 
c
            DO IXEL=1,NUMXEL
              ILEV = NXEL*(ILAY-1) + IXEL
              FAC      = 0
              NTAG(1:4)= 0
c---------------------------------------
              IF (ABS(LAYCUT) == 1) THEN
c               new advancing (or init) crack => LAYCUT = +/- 1
c---------------------------------------
                DO K=1,4
                  IED   = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)   ! = 1,2 => sequence des arretes coupes
                  EDGE  = XEDGE4N(K,ELCRK) ! global egdge N 
                  IECUT = CRKEDGE(ILAY)%ICUTEDGE(EDGE)
c                 Tip edges => NTAG = 2               
                  IF (IED > 0 .and. IECUT == 2) THEN    ! (cas IECUT=3 traite avant)
                    NTAG(K)    = 2
                    NTAG(d(K)) = 2
                    FAC = FAC + 1
                  ENDIF
                ENDDO
c               crack touches boundary or goes through a hole due to erased element
c               touching boundary    => (IBOUNDEDGE == 1)
c               going through a hole => (IBOUNDEDGE == 2)
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
c                 Tag boundary edges : NTAG=1               
                  IF ( IED > 0 .and. IBOUNDEDGE > 0) THEN
                    NTAG(K)    = 1
                    NTAG(d(K)) = 1
                    FAC = FAC + 1
                  ENDIF
                ENDDO
c
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  ITIP = CRKEDGE(ILAY)%EDGETIP(2,EDGE)
                  IF (IED > 0 .and. ITIP == 1) THEN
                    IF (ITRI /= 0 .and. IXEL == 3) THEN  
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                      FAC = FAC + 1
                    ENDIF                           
                  ENDIF                           
                ENDDO
c               NTAG = 1 for all except internal tip nodes
                IF (FAC > 0) THEN
                  DO K=1,4
                    IF (NTAG(K) /= 2) NTAG(K) = 1
                  ENDDO
                ENDIF
c---------------------------------------
              ELSE IF (LAYCUT == 2) THEN
c               old crack, element already cut previously
c---------------------------------------
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                  IF (IED > 0 .and. IBOUNDEDGE == 2) THEN
                    NTAG(K)    = 1
                    NTAG(d(K)) = 1
                    FAC = FAC + 1
                  ENDIF
                END DO
c
c               check if no tip edge in phantom element (count=0 => no tip)
c
                COUNT = 0
                DO K=1,4
                  IED  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
                  EDGE = XEDGE4N(K,ELCRK)
                  ITIP = CRKEDGE(ILAY)%EDGETIP(2,EDGE)
                  IF (IED > 0 .and. ITIP == 1) COUNT = COUNT + 1
                END DO
c                
                IF (COUNT == 0) THEN  ! already cracked phantom, no tip edge => no negative enrichments
                  DO K=1,4
                    IAD  = IADC(K)
                    IF (ITRI < 0 .and. IXEL == 2 .and. CRKLVSET(ILEV)%ENR0(1,IAD) < 0) THEN
c                     enrichment of IXEL = 2 < 0 => copy from IXEL = 3 and tag edge
                      CRKLVSET(ILEV)%ENR0(1,IAD) = CRKLVSET(ILEV+1)%ENR0(1,IAD)
                      FAC = FAC + 1
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                    ELSEIF (ITRI > 0 .and. IXEL == 1 .and. CRKLVSET(ILEV)%ENR0(1,IAD) < 0) THEN
c                     enrichment of IXEL = 1 < 0 => copy from IXEL = 3 and tag edge
                      CRKLVSET(ILEV)%ENR0(1,IAD) = CRKLVSET(ILEV+2)%ENR0(1,IAD)
                      FAC = FAC + 1
                      NTAG(K)    = 1
                      NTAG(d(K)) = 1
                    ENDIF
                  END DO
                ENDIF  !  COUNT == 0
c                
              ENDIF    !  LAYCUT
C--------------------------------------------------------------------------
c             set ENRTAG for nodal enrichment update (ENR > 0 only for boundary nodes)
C--------------------------------------------------------------------------
              IF (FAC > 0) THEN
                DO K=1,4
                  ENR = ABS(CRKLVSET(ILEV)%ENR0(1,IADC(K)))
                  IF (ENR > 0 .and. NTAG(K) == 1) THEN
                    IF (ENRTAG(NS(K),ENR) == 0) ENRTAG(NS(K),ENR) = ENR
                  ENDIF
                ENDDO
              ENDIF
C--------------------------------------------------------------------------
            ENDDO  !  DO IXEL=1,NXEL
          ENDIF    !  IF(ELCUT /= 0)THEN
        ENDDO      !  DO IR=1,NELCRK
      ENDDO        !  DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
