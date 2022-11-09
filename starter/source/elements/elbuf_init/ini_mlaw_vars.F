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
Chd|  INI_MLAW_VARS                 source/elements/elbuf_init/ini_mlaw_vars.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE INI_MLAW_VARS(MLAW_TAG ,FAIL_TAG ,VISC_TAG ,NUMMAT)
C-----------------------------------------------
C     M o d u l e s
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT
      TYPE(MLAW_TAG_)    ,TARGET ,DIMENSION(NUMMAT  )   :: MLAW_TAG
      TYPE(FAIL_TAG_)    ,TARGET ,DIMENSION(0:MAXFAIL ) :: FAIL_TAG
      TYPE(VISC_TAG_)    ,TARGET ,DIMENSION(0:MAXVISC ) :: VISC_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT,ILAW
      TYPE(MLAW_TAG_)    , POINTER :: MTAG
      TYPE(FAIL_TAG_)    , POINTER :: FTAG
      TYPE(VISC_TAG_)    , POINTER :: VTAG
C=======================================================================
C     Set default variables used in all matarial laws
C=======================================================================
c      attention, apres la lecture de mats, remtettre     zero tout pour la loi 0 (void)

      DO IMAT = 1,NUMMAT
        MTAG => MLAW_TAG(IMAT)
c       global variables - scalars
        MTAG%G_EINT   = 1 
        MTAG%G_EINS   = 0 
        MTAG%G_RHO    = 0   ! G_RHO > 0 only for solids   
        MTAG%G_DELTAX = 0 
        MTAG%G_EPSD   = 0 
        MTAG%G_EPSQ   = 0	
        MTAG%G_PLA    = 0 
        MTAG%G_TEMP   = 0 
        MTAG%G_TB     = 0 
        MTAG%G_RE     = 0 
        MTAG%G_RK     = 0 
        MTAG%G_GAMA   = 0             
        MTAG%G_BFRAC  = 0             
        MTAG%G_SIGI   = 0             
        MTAG%G_IERR   = 0             
        MTAG%G_DAMDL  = 0
        MTAG%G_SEQ    = 0
        MTAG%G_ABURN  = 0  
        MTAG%G_MU     = 0 
        MTAG%G_PLANL  = 0 
        MTAG%G_EPSDNL = 0 
        MTAG%G_TSAIWU = 0
        MTAG%G_MAXFRAC= 0   
        MTAG%G_MAXEPS = 0
        MTAG%G_DMG    = 0  
C
        MTAG%G_TOTDEPL = 0                
        MTAG%G_TOTROT  = 0              
        MTAG%G_DEP_IN_TENS = 0            
        MTAG%G_DEP_IN_COMP = 0        
        MTAG%G_ROT_IN_TENS = 0               
        MTAG%G_ROT_IN_COMP = 0           
        MTAG%G_POSX     = 0                                                               
        MTAG%G_POSY     = 0                                                               
        MTAG%G_POSZ     = 0                                                               
        MTAG%G_POSXX    = 0                                                               
        MTAG%G_POSYY    = 0                                                               
        MTAG%G_POSZZ    = 0                                                               
        MTAG%G_YIELD    = 0                                                               
        MTAG%G_RUPTCRIT = 0                                                               
        MTAG%G_NUVAR    = 0                                                                      
        MTAG%G_MASS     = 0
        MTAG%G_SKEW_ID  = 0
c---
c       for seatbelt elements
c---
        MTAG%G_SLIPRING_ID  = 0
        MTAG%G_SLIPRING_FRAM_ID  = 0
        MTAG%G_SLIPRING_STRAND  = 0
        MTAG%G_RETRACTOR_ID  = 0
        MTAG%G_RINGSLIP  = 0
        MTAG%G_ADD_NODE  = 0
        MTAG%G_UPDATE  = 0
        MTAG%G_FRAM_FACTOR  = 0
        MTAG%G_INTVAR  = 0
                        
c---    local variables (per layer) 

        MTAG%LY_DIRA  = 0 
c---    local variables (per integration point)

        MTAG%L_EINT   = 1           
        MTAG%L_EINS   = 0                   
        MTAG%L_RHO    = 0        ! L_RHO > 0 only for solids                         
        MTAG%L_DP_DRHO= 0        
        IF (IRESP == 1) MTAG%L_DP_DRHO= 1                   
        MTAG%L_DELTAX = 0                   
        MTAG%L_PLA    = 0           
        MTAG%L_TB     = 0           
        MTAG%L_RK     = 0           
        MTAG%L_STRA   = 0           
        MTAG%L_EPSA   = 0           
        MTAG%L_EPSD   = 0           
        MTAG%L_EPSQ   = 0           
        MTAG%L_EPSF   = 0           
        MTAG%L_SIGL   = 0           
        MTAG%L_SIGV   = 0           
        MTAG%L_SIGA   = 0           
        MTAG%L_SIGB   = 0           
        MTAG%L_SIGC   = 0           
        MTAG%L_SIGD   = 0           
        MTAG%L_SIGF   = 0           
        MTAG%L_TEMP   = 0           
        MTAG%L_RE     = 0           
        MTAG%L_VK     = 0           
        MTAG%L_SF     = 0
        MTAG%L_ROB    = 0
        MTAG%L_DAM    = 0
        MTAG%L_DSUM   = 0
        MTAG%L_DGLO   = 0
        MTAG%L_CRAK   = 0
        MTAG%L_ANG    = 0
        MTAG%L_EPE    = 0
        MTAG%L_EPC    = 0
        MTAG%L_XST    = 0
        MTAG%L_SSP    = 0
        MTAG%L_Z      = 0
        MTAG%L_GAMA   = 0  
        MTAG%L_FRAC   = 0  
        MTAG%L_BFRAC  = 0  	
        MTAG%NUVAR    = 0  
        MTAG%NVARTMP  = 0    
        MTAG%L_DMG    = 0  	
        MTAG%L_SEQ    = 0
        MTAG%L_FAC_YLD= 0  
        MTAG%L_ABURN  = 0  
        MTAG%L_MU     = 0  
        MTAG%L_PLANL  = 0  
        MTAG%L_EPSDNL = 0 
        MTAG%L_DMGSCL = 0  
        MTAG%L_TSAIWU = 0	 
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE INI_MLAW_VARS
