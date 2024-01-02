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
c
Chd|====================================================================
Chd|  CRK_TAGXP4                    source/elements/xfem/crk_tagxp4.F
Chd|-- called by -----------
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_TAGXP4(IPARG  ,IXC     ,NFT    ,JFT     ,JLT   ,
     .                       ELCUTC ,IADC_CRK,IEL_CRK,INOD_CRK,ENRTAG,
     .                       NXLAY  ,CRKEDGE ,XEDGE4N,ITAB    )
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
     .  IADC_CRK(4,*),IEL_CRK(*),ENRTAG(NUMNOD,*),XEDGE4N(4,*),ITAB(*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IR,IAD,NELCRK,ELCRK,ILEV,ILAY,IXEL,ELCUT,LAYCUT,
     .   IECUT,ENR0,ENR,IBOUNDEDGE,IED,EDGE,COUNT,ITIP,ITRI,NUMXEL,NSX,
     .   ISEND_NSX,ISEND_IAD
      INTEGER JCT(MVSIZ),NS(4),IADC(4)
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
          ELCRK  = IEL_CRK(I+NFT)                       
          ELCUT  = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
          LAYCUT = ABS(CRKEDGE(ILAY)%LAYCUT(ELCRK))        
          IF (ELCUT /= 0 .and. LAYCUT == 1) THEN  !  new advancing crack
            ITRI    = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK)    
            NS(1)   = IXC(2,I+NFT)       
            NS(2)   = IXC(3,I+NFT)       
            NS(3)   = IXC(4,I+NFT)       
            NS(4)   = IXC(5,I+NFT)       
            IADC(1) = IADC_CRK(1,ELCRK)  
            IADC(2) = IADC_CRK(2,ELCRK)  
            IADC(3) = IADC_CRK(3,ELCRK)  
            IADC(4) = IADC_CRK(4,ELCRK)
            ISEND_NSX = 0
            ISEND_IAD = 0
c           Tag phantom nodes with new positive enrichment to copy the velocities
            IF (ITRI /= 0) THEN
              IXEL = 3 
              ILEV = NXEL*(ILAY-1) + IXEL 
              ! sender = third phantom
              DO K=1,4                                                            
                IAD  = IADC(K)                                                     
                ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)                         
                ENR  = CRKLVSET(ILEV)%ENR0(1,IAD)                         
                EDGE = XEDGE4N(K,ELCRK) ! global egdge N                  
                IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)                
                NSX = INOD_CRK(NS(K))                                     
                IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN                   
                  XFEM_PHANTOM(ILAY)%TAGXP(1,NSX,ENR) = IAD                                           
                  XFEM_PHANTOM(ILAY)%TAGXP(2,NSX,ENR) = ILEV              
                  XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2   ! counter
                  ISEND_NSX = NSX   
                  ISEND_IAD = IAD   
                ENDIF                                                                
              ENDDO         
            ENDIF
c
            NUMXEL = 2    ! receiver : first or second phantom
            DO IXEL=1,NUMXEL
              ILEV = NXEL*(ILAY-1) + IXEL
              DO K=1,4                                                          
                IAD  = IADC(K)                                                   
                ENR0 = CRKLVSET(ILEV)%ENR0(2,IAD)                         
                ENR  = ABS(CRKLVSET(ILEV)%ENR0(1,IAD))                    
                EDGE = XEDGE4N(K,ELCRK) ! global egdge N                  
                IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(EDGE)                
                NSX = INOD_CRK(NS(K)) 
                IF (ISEND_NSX /= NSX  .and. ISEND_IAD /= IAD) THEN                                  
                  IF (ENR > 0 .and. IBOUNDEDGE /= 2) THEN                   
                    XFEM_PHANTOM(ILAY)%TAGXP(4,NSX,ENR) = IAD                                  
                    XFEM_PHANTOM(ILAY)%TAGXP(5,NSX,ENR) = ILEV                    
                    XFEM_PHANTOM(ILAY)%TAGXP(3,NSX,ENR) = 2      ! counter        
                  ENDIF                                                             
                ENDIF                                                               
              ENDDO                                                               
            ENDDO  !  DO IXEL=1,NUMXEL
C--------------------------------------------------------------------------
          ENDIF    !  IF (ELCUT /= 0 .and. LAYCUT == 1)
        ENDDO      !  DO IR=1,NELCRK
      ENDDO        !  DO ILAY=1,NXLAY
c-----------------------------------------------
      RETURN
      END
