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
Chd|  UPENRIC_LAST                  source/elements/xfem/upenric_last.F
Chd|-- called by -----------
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE UPENRIC_LAST(
     .      IXC     ,NFT    ,JFT    ,JLT    ,IADC_CRK, 
     .      IEL_CRK ,ELCUTC ,NXLAY  ,CRKEDGE)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IXC(NIXC,*),IADC_CRK(4,*),IEL_CRK(*),ELCUTC(2,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,NS,ILEV,ILAY,IXEL,ICUT,LAYCUT,ELCRK,IADC1,IADC2,IADC3,IADC4
C=======================================================================
        DO I=JFT,JLT
        NS   = I+NFT                                                     
        ICUT = ELCUTC(1,NS)                                              
          IF (ICUT == 0) CYCLE
        ELCUTC(2,NS) = ICUT                                              
        ELCRK = IEL_CRK(NS)                                              
        IADC1 = IADC_CRK(1,ELCRK)                                        
        IADC2 = IADC_CRK(2,ELCRK)                                        
        IADC3 = IADC_CRK(3,ELCRK)                                        
        IADC4 = IADC_CRK(4,ELCRK)                                        
          DO ILAY = 1,NXLAY
          LAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)                           
          IF (LAYCUT == 0) CYCLE                                         
            II = NXEL*(ILAY-1)    
          DO IXEL=1,NXEL                                                 
            ILEV = II + IXEL                                             
            CRKLVSET(ILEV)%ENR0(2,IADC1) = CRKLVSET(ILEV)%ENR0(1,IADC1)  
            CRKLVSET(ILEV)%ENR0(2,IADC2) = CRKLVSET(ILEV)%ENR0(1,IADC2)  
            CRKLVSET(ILEV)%ENR0(2,IADC3) = CRKLVSET(ILEV)%ENR0(1,IADC3)  
            CRKLVSET(ILEV)%ENR0(2,IADC4) = CRKLVSET(ILEV)%ENR0(1,IADC4)  
            ENDDO
          ENDDO
        ENDDO
C-------------
      RETURN
      END
