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
c
Chd|====================================================================
Chd|  CRK_TAGXP3                    source/elements/xfem/upxfem_tagxp.F
Chd|-- called by -----------
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_TAGXP3(IPARG  ,IXTG     ,NFT      ,JFT     ,JLT   ,
     .                      ELCUTC ,IAD_CRKTG,IEL_CRKTG,INOD_CRK,ENRTAG,
     .                      NXLAY  ,CRKEDGE  ,XEDGE3N  ,ITAB    )
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IPARG(NPARG,*),IXTG(NIXTG,*),ELCUTC(2,*),INOD_CRK(*),
     .  IAD_CRKTG(3,*),XEDGE3N(3,*),IEL_CRKTG(*),ENRTAG(NUMNOD,*),ITAB(NUMNOD)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,ELCRK,ELCRKTG,ILEV,ILAY,IXEL,ELCUT,IECUT,NELCRK,NSX,
     .  IED,EDGE,FAC,IBOUNDEDGE,LAYCUT,ENR0,ENR,ITRI,ITIP,IAD,COUNT
      INTEGER JCT(MVSIZ),IADC(3),NS(3)
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
C
      DO ILAY=1,NXLAY
        DO IXEL=1,NXEL
          ILEV = NXEL*(ILAY-1) + IXEL
          DO IR=1,NELCRK
            I = JCT(IR)
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK  = ELCRKTG + ECRKXFEC
            ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
            IF (ELCUT /= 0) THEN
              NS(1) = IXTG(2,I+NFT)
              NS(2) = IXTG(3,I+NFT)
              NS(3) = IXTG(4,I+NFT)
              IADC(1) = IAD_CRKTG(1,ELCRKTG)
              IADC(2) = IAD_CRKTG(2,ELCRKTG)
              IADC(3) = IAD_CRKTG(3,ELCRKTG)
              ITRI    = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)
              LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)
c---------------------------------------
              IF (ABS(LAYCUT) == 1) THEN
c               new advancing crack
c               Tag phantom nodes with positive enrichment to copy the velocities
c---------------------------------------
                IF (IXEL == 3) THEN         ! sender
                  DO K=1,3                                                       
                    IAD  = IADC(K)                                                 
                    ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)
                    ENR  = CRKLVSET(ILEV)%ENR0(1,IAD)
                    EDGE = XEDGE3N(K,ELCRKTG) ! global egdge N       
                    IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                    NSX = INOD_CRK(NS(K))
                    IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN
                      XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,ENR) = IAD                                       
                      XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,ENR) = ILEV 
                      XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2   ! counter
                    ENDIF                                                             
                  ENDDO                                                               
                ELSE                        ! receiver                                             
                  DO K=1,3                                                        
                    IAD  = IADC(K)                                                 
                    ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)
                    ENR  = ABS(CRKLVSET(ILEV)%ENR0(1,IAD))
                    EDGE = XEDGE3N(K,ELCRKTG) ! global egdge N       
                    IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)
                    NSX = INOD_CRK(NS(K))
                    IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN
                      XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,ENR) = IAD                                  
                      XFEM_PHANTOM(ILAY)%TAGXP(5,NSX,ENR) = ILEV
                      XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2      ! counter
                    ENDIF                                                             
                  ENDDO
                ENDIF                                                             
              ENDIF   ! LAYCUT = 1                                                            
C--------------------------------------------------------------------------
            ENDIF  ! IF(ELCUT /= 0)THEN
          ENDDO  !  DO IR=1,NELCRK
        ENDDO  !  DO IXEL=1,NXEL
      ENDDO  !  DO ILAY=1,NXLAY
C-----------------------------------------------
      RETURN
      END
