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
Chd|  INIT_MLAW_TAG                 source/elements/elbuf_init/init_mlaw_tag.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE INIT_MLAW_TAG(MLAW_TAG,MY_SIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------        
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tablen_c.inc"
#include      "inter22.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: MY_SIZE
        TYPE(MLAW_TAG_),DIMENSION(MY_SIZE), INTENT(inout) :: MLAW_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
!   -----------------------------------------------
        DO I=1,MY_SIZE
            MLAW_TAG(I)%G_GAMA = 0 
            MLAW_TAG(I)%G_EINT = 0
            MLAW_TAG(I)%G_EINS = 0
            MLAW_TAG(I)%G_RHO = 0
            MLAW_TAG(I)%G_DELTAX = 0
            MLAW_TAG(I)%G_TEMP = 0
            MLAW_TAG(I)%G_PLA = 0
            MLAW_TAG(I)%G_EPSD = 0
            MLAW_TAG(I)%G_TB  = 0
            MLAW_TAG(I)%G_RK = 0
            MLAW_TAG(I)%G_RE = 0
            MLAW_TAG(I)%G_BFRAC = 0
            MLAW_TAG(I)%G_SIGI = 0
            MLAW_TAG(I)%G_EPSQ = 0
            MLAW_TAG(I)%G_IERR = 0
            MLAW_TAG(I)%G_DAMDL = 0
            MLAW_TAG(I)%G_SEQ = 0
            MLAW_TAG(I)%G_ABURN = 0
            MLAW_TAG(I)%G_MU = 0
            MLAW_TAG(I)%G_PLANL = 0
            MLAW_TAG(I)%G_EPSDNL = 0
            MLAW_TAG(I)%G_MAXFRAC = 0
            MLAW_TAG(I)%G_MAXEPS = 0
            MLAW_TAG(I)%G_DMG = 0
            MLAW_TAG(I)%G_TOTDEPL = 0
            MLAW_TAG(I)%G_TOTROT = 0
            MLAW_TAG(I)%G_DEP_IN_TENS = 0
            MLAW_TAG(I)%G_DEP_IN_COMP = 0
            MLAW_TAG(I)%G_ROT_IN_TENS = 0
            MLAW_TAG(I)%G_ROT_IN_COMP = 0
            MLAW_TAG(I)%G_POSX = 0
            MLAW_TAG(I)%G_POSY = 0
            MLAW_TAG(I)%G_POSZ = 0
            MLAW_TAG(I)%G_POSXX = 0
            MLAW_TAG(I)%G_POSYY = 0
            MLAW_TAG(I)%G_POSZZ = 0
            MLAW_TAG(I)%G_YIELD = 0
            MLAW_TAG(I)%G_RUPTCRIT = 0
            MLAW_TAG(I)%G_NUVAR = 0
            MLAW_TAG(I)%G_MASS = 0
            MLAW_TAG(I)%G_SKEW_ID = 0
c           layer variables 
c   
            MLAW_TAG(I)%LY_DIRA = 0
c
c           local variables (per integration point)
c   
            MLAW_TAG(I)%L_GAMA = 0
            MLAW_TAG(I)%L_EINT = 0  
            MLAW_TAG(I)%L_EINS = 0  
            MLAW_TAG(I)%L_RHO = 0   
            MLAW_TAG(I)%L_DP_DRHO = 0            
            MLAW_TAG(I)%L_STRA = 0     
            MLAW_TAG(I)%L_DELTAX = 0
            MLAW_TAG(I)%L_PLA = 0   
            MLAW_TAG(I)%L_TB = 0    
            MLAW_TAG(I)%L_RK = 0    
            MLAW_TAG(I)%L_TEMP = 0  
            MLAW_TAG(I)%L_EPSA = 0  
            MLAW_TAG(I)%L_EPSD = 0  
            MLAW_TAG(I)%L_EPSQ = 0  
            MLAW_TAG(I)%L_EPSF = 0  
            MLAW_TAG(I)%L_VK = 0    
            MLAW_TAG(I)%L_SF = 0       
            MLAW_TAG(I)%L_DAM = 0      
            MLAW_TAG(I)%L_SIGL = 0     
            MLAW_TAG(I)%L_SIGV = 0     
            MLAW_TAG(I)%L_SIGA = 0     
            MLAW_TAG(I)%L_SIGB = 0     
            MLAW_TAG(I)%L_SIGC = 0     
            MLAW_TAG(I)%L_SIGD = 0     
            MLAW_TAG(I)%L_SIGF = 0     
            MLAW_TAG(I)%L_RE = 0    
            MLAW_TAG(I)%L_DSUM = 0     
            MLAW_TAG(I)%L_ROB = 0   
            MLAW_TAG(I)%L_DGLO = 0     
            MLAW_TAG(I)%L_CRAK = 0    
            MLAW_TAG(I)%L_ANG  = 0     
            MLAW_TAG(I)%L_EPE  = 0     
            MLAW_TAG(I)%L_EPC = 0      
            MLAW_TAG(I)%L_XST = 0      
            MLAW_TAG(I)%L_SSP = 0      
            MLAW_TAG(I)%L_Z = 0
            MLAW_TAG(I)%L_FRAC  = 0       !    pourcentage de phase (multimat) 
            MLAW_TAG(I)%L_BFRAC = 0
            MLAW_TAG(I)%L_DMG = 0
            MLAW_TAG(I)%L_ABURN = 0
            MLAW_TAG(I)%L_MU = 0
            MLAW_TAG(I)%L_PLANL  = 0
            MLAW_TAG(I)%L_EPSDNL = 0
            MLAW_TAG(I)%L_DMGSCL = 0
c---    uvar            
            MLAW_TAG(I)%NUVAR = 0           
            MLAW_TAG(I)%NVARTMP = 0      
            MLAW_TAG(I)%L_SEQ = 0      ! equivalent stress (/= VON MISES)
            MLAW_TAG(I)%L_FAC_YLD = 0    
        ENDDO
!   -----------------------------------------------
        RETURN
        END SUBROUTINE INIT_MLAW_TAG
