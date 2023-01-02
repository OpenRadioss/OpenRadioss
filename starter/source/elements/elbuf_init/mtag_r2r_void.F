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
Chd|  MTAG_R2R_VOID                 source/elements/elbuf_init/mtag_r2r_void.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE MTAG_R2R_VOID(MLAW_TAG, MTAG_INI, NUMMAT0  ,NUMMAT  )
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMMAT0,NUMMAT
      TYPE(MLAW_TAG_) ,DIMENSION(NUMMAT0) :: MTAG_INI
      TYPE(MLAW_TAG_) ,DIMENSION(NUMMAT)  :: MLAW_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
c     Copy variable tags from standard materials
c-----------------------------------------------
      DO I = 1,NUMMAT0
        MLAW_TAG(I)%G_EINT   = MTAG_INI(I)%G_EINT  
        MLAW_TAG(I)%G_EINS   = MTAG_INI(I)%G_EINS  
        MLAW_TAG(I)%G_RHO    = MTAG_INI(I)%G_RHO   
        MLAW_TAG(I)%G_DELTAX = MTAG_INI(I)%G_DELTAX
        MLAW_TAG(I)%G_EPSD   = MTAG_INI(I)%G_EPSD  
        MLAW_TAG(I)%G_EPSQ   = MTAG_INI(I)%G_EPSQ  
        MLAW_TAG(I)%G_PLA    = MTAG_INI(I)%G_PLA   
        MLAW_TAG(I)%G_TEMP   = MTAG_INI(I)%G_TEMP  
        MLAW_TAG(I)%G_TB     = MTAG_INI(I)%G_TB    
        MLAW_TAG(I)%G_RE     = MTAG_INI(I)%G_RE    
        MLAW_TAG(I)%G_RK     = MTAG_INI(I)%G_RK    
        MLAW_TAG(I)%G_GAMA   = MTAG_INI(I)%G_GAMA        
        MLAW_TAG(I)%G_BFRAC  = MTAG_INI(I)%G_BFRAC       
        MLAW_TAG(I)%G_SIGI   = MTAG_INI(I)%G_SIGI        
        MLAW_TAG(I)%G_IERR   = MTAG_INI(I)%G_IERR        
        MLAW_TAG(I)%G_DAMDL  = MTAG_INI(I)%G_DAMDL 
        MLAW_TAG(I)%G_SEQ    = MTAG_INI(I)%G_SEQ   
        MLAW_TAG(I)%G_ABURN  = MTAG_INI(I)%G_ABURN 
        MLAW_TAG(I)%G_MU     = MTAG_INI(I)%G_MU    
        MLAW_TAG(I)%G_DMG    = MTAG_INI(I)%G_DMG   
        MLAW_TAG(I)%G_MAXFRAC= MTAG_INI(I)%G_MAXFRAC
        MLAW_TAG(I)%G_MAXEPS = MTAG_INI(I)%G_MAXEPS 
C
        MLAW_TAG(I)%G_TOTDEPL = MTAG_INI(I)%G_TOTDEPL               
        MLAW_TAG(I)%G_TOTROT  = MTAG_INI(I)%G_TOTROT              
        MLAW_TAG(I)%G_DEP_IN_TENS = MTAG_INI(I)%G_DEP_IN_TENS           
        MLAW_TAG(I)%G_DEP_IN_COMP = MTAG_INI(I)%G_DEP_IN_COMP       
        MLAW_TAG(I)%G_ROT_IN_TENS = MTAG_INI(I)%G_ROT_IN_TENS              
        MLAW_TAG(I)%G_ROT_IN_COMP = MTAG_INI(I)%G_ROT_IN_COMP          
        MLAW_TAG(I)%G_POSX     = MTAG_INI(I)%G_POSX                                                                 
        MLAW_TAG(I)%G_POSY     = MTAG_INI(I)%G_POSY                                                                 
        MLAW_TAG(I)%G_POSZ     = MTAG_INI(I)%G_POSZ                                                                 
        MLAW_TAG(I)%G_POSXX    = MTAG_INI(I)%G_POSXX                                                                
        MLAW_TAG(I)%G_POSYY    = MTAG_INI(I)%G_POSYY                                                                
        MLAW_TAG(I)%G_POSZZ    = MTAG_INI(I)%G_POSZZ                                                                
        MLAW_TAG(I)%G_YIELD    = MTAG_INI(I)%G_YIELD                                                                
        MLAW_TAG(I)%G_RUPTCRIT = MTAG_INI(I)%G_RUPTCRIT                                                             
        MLAW_TAG(I)%G_NUVAR    = MTAG_INI(I)%G_NUVAR            
        MLAW_TAG(I)%G_MASS     = MTAG_INI(I)%G_MASS                            
c---    local variables (per layer) 

        MLAW_TAG(I)%LY_DIRA  = MTAG_INI(I)%LY_DIRA
c---    local variables (per integration point)

        MLAW_TAG(I)%L_EINT   = MTAG_INI(I)%L_EINT            
        MLAW_TAG(I)%L_EINS   = MTAG_INI(I)%L_EINS                    
        MLAW_TAG(I)%L_RHO    = MTAG_INI(I)%L_RHO                    
        MLAW_TAG(I)%L_DP_DRHO= MTAG_INI(I)%L_DP_DRHO      
        MLAW_TAG(I)%L_DP_DRHO= MTAG_INI(I)%L_DP_DRHO                 
        MLAW_TAG(I)%L_DELTAX = MTAG_INI(I)%L_DELTAX                  
        MLAW_TAG(I)%L_PLA    = MTAG_INI(I)%L_PLA             
        MLAW_TAG(I)%L_TB     = MTAG_INI(I)%L_TB              
        MLAW_TAG(I)%L_RK     = MTAG_INI(I)%L_RK              
        MLAW_TAG(I)%L_STRA   = MTAG_INI(I)%L_STRA            
        MLAW_TAG(I)%L_EPSA   = MTAG_INI(I)%L_EPSA            
        MLAW_TAG(I)%L_EPSD   = MTAG_INI(I)%L_EPSD            
        MLAW_TAG(I)%L_EPSQ   = MTAG_INI(I)%L_EPSQ            
        MLAW_TAG(I)%L_EPSF   = MTAG_INI(I)%L_EPSF            
        MLAW_TAG(I)%L_SIGL   = MTAG_INI(I)%L_SIGL            
        MLAW_TAG(I)%L_SIGV   = MTAG_INI(I)%L_SIGV            
        MLAW_TAG(I)%L_SIGA   = MTAG_INI(I)%L_SIGA            
        MLAW_TAG(I)%L_SIGB   = MTAG_INI(I)%L_SIGB            
        MLAW_TAG(I)%L_SIGC   = MTAG_INI(I)%L_SIGC            
        MLAW_TAG(I)%L_SIGD   = MTAG_INI(I)%L_SIGD            
        MLAW_TAG(I)%L_SIGF   = MTAG_INI(I)%L_SIGF            
        MLAW_TAG(I)%L_TEMP   = MTAG_INI(I)%L_TEMP            
        MLAW_TAG(I)%L_RE     = MTAG_INI(I)%L_RE              
        MLAW_TAG(I)%L_VK     = MTAG_INI(I)%L_VK              
        MLAW_TAG(I)%L_SF     = MTAG_INI(I)%L_SF     
        MLAW_TAG(I)%L_ROB    = MTAG_INI(I)%L_ROB    
        MLAW_TAG(I)%L_DAM    = MTAG_INI(I)%L_DAM    
        MLAW_TAG(I)%L_DSUM   = MTAG_INI(I)%L_DSUM   
        MLAW_TAG(I)%L_DGLO   = MTAG_INI(I)%L_DGLO   
        MLAW_TAG(I)%L_CRAK   = MTAG_INI(I)%L_CRAK   
        MLAW_TAG(I)%L_ANG    = MTAG_INI(I)%L_ANG    
        MLAW_TAG(I)%L_EPE    = MTAG_INI(I)%L_EPE    
        MLAW_TAG(I)%L_EPC    = MTAG_INI(I)%L_EPC    
        MLAW_TAG(I)%L_XST    = MTAG_INI(I)%L_XST    
        MLAW_TAG(I)%L_SSP    = MTAG_INI(I)%L_SSP    
        MLAW_TAG(I)%L_Z      = MTAG_INI(I)%L_Z      
        MLAW_TAG(I)%L_GAMA   = MTAG_INI(I)%L_GAMA   
        MLAW_TAG(I)%L_FRAC   = MTAG_INI(I)%L_FRAC   
        MLAW_TAG(I)%L_BFRAC  = MTAG_INI(I)%L_BFRAC  	
        MLAW_TAG(I)%NUVAR    = MTAG_INI(I)%NUVAR    
        MLAW_TAG(I)%NVARTMP  = MTAG_INI(I)%NVARTMP    
        MLAW_TAG(I)%L_DMG    = MTAG_INI(I)%L_DMG    	
        MLAW_TAG(I)%L_SEQ    = MTAG_INI(I)%L_SEQ    
        MLAW_TAG(I)%L_FAC_YLD= MTAG_INI(I)%L_FAC_YLD
        MLAW_TAG(I)%L_ABURN  = MTAG_INI(I)%L_ABURN  
        MLAW_TAG(I)%L_MU     = MTAG_INI(I)%L_MU     	 
      ENDDO
c
c-----------------------------------------------
c     Fill variable tags for additional rad2rad void materials
c-----------------------------------------------
      DO I = NUMMAT0+1,NUMMAT
c       global variables - scalars
        MLAW_TAG(I)%G_EINT   = 0 
        MLAW_TAG(I)%G_EINS   = 0 
        MLAW_TAG(I)%G_RHO    = 0 
        MLAW_TAG(I)%G_DELTAX = 0 
        MLAW_TAG(I)%G_EPSD   = 0 
        MLAW_TAG(I)%G_EPSQ   = 0	
        MLAW_TAG(I)%G_PLA    = 0 
        MLAW_TAG(I)%G_TEMP   = 0 
        MLAW_TAG(I)%G_TB     = 0 
        MLAW_TAG(I)%G_RE     = 0 
        MLAW_TAG(I)%G_RK     = 0 
        MLAW_TAG(I)%G_GAMA   = 0             
        MLAW_TAG(I)%G_BFRAC  = 0             
        MLAW_TAG(I)%G_SIGI   = 0             
        MLAW_TAG(I)%G_IERR   = 0             
        MLAW_TAG(I)%G_DAMDL  = 0
        MLAW_TAG(I)%G_SEQ    = 0
        MLAW_TAG(I)%G_ABURN  = 0  
        MLAW_TAG(I)%G_MU     = 0   
        MLAW_TAG(I)%G_DMG    = 0  
C
        MLAW_TAG(I)%G_TOTDEPL = 0                
        MLAW_TAG(I)%G_TOTROT  = 0              
        MLAW_TAG(I)%G_DEP_IN_TENS = 0            
        MLAW_TAG(I)%G_DEP_IN_COMP = 0        
        MLAW_TAG(I)%G_ROT_IN_TENS = 0               
        MLAW_TAG(I)%G_ROT_IN_COMP = 0           
        MLAW_TAG(I)%G_POSX     = 0                                                               
        MLAW_TAG(I)%G_POSY     = 0                                                               
        MLAW_TAG(I)%G_POSZ     = 0                                                               
        MLAW_TAG(I)%G_POSXX    = 0                                                               
        MLAW_TAG(I)%G_POSYY    = 0                                                               
        MLAW_TAG(I)%G_POSZZ    = 0                                                               
        MLAW_TAG(I)%G_YIELD    = 0                                                               
        MLAW_TAG(I)%G_RUPTCRIT = 0                                                               
        MLAW_TAG(I)%G_NUVAR    = 0                                                                      
        MLAW_TAG(I)%G_MASS     = 0
        MLAW_TAG(I)%G_SKEW_ID  = 0      
        MLAW_TAG(I)%G_MAXFRAC  = 0 
        MLAW_TAG(I)%G_MAXEPS   = 0
                      
c---    local variables (per layer) 

        MLAW_TAG(I)%LY_DIRA  = 0 
c---    local variables (per integration point)

        MLAW_TAG(I)%L_EINT   = 0           
        MLAW_TAG(I)%L_EINS   = 0                   
        MLAW_TAG(I)%L_RHO    = 0        ! L_RHO > 0 only for solids                         
        MLAW_TAG(I)%L_DP_DRHO= 0        
        MLAW_TAG(I)%L_DP_DRHO= 0                   
        MLAW_TAG(I)%L_DELTAX = 0                   
        MLAW_TAG(I)%L_PLA    = 0           
        MLAW_TAG(I)%L_TB     = 0           
        MLAW_TAG(I)%L_RK     = 0           
        MLAW_TAG(I)%L_STRA   = 0           
        MLAW_TAG(I)%L_EPSA   = 0           
        MLAW_TAG(I)%L_EPSD   = 0           
        MLAW_TAG(I)%L_EPSQ   = 0           
        MLAW_TAG(I)%L_EPSF   = 0           
        MLAW_TAG(I)%L_SIGL   = 0           
        MLAW_TAG(I)%L_SIGV   = 0           
        MLAW_TAG(I)%L_SIGA   = 0           
        MLAW_TAG(I)%L_SIGB   = 0           
        MLAW_TAG(I)%L_SIGC   = 0           
        MLAW_TAG(I)%L_SIGD   = 0           
        MLAW_TAG(I)%L_SIGF   = 0           
        MLAW_TAG(I)%L_TEMP   = 0           
        MLAW_TAG(I)%L_RE     = 0           
        MLAW_TAG(I)%L_VK     = 0           
        MLAW_TAG(I)%L_SF     = 0
        MLAW_TAG(I)%L_ROB    = 0
        MLAW_TAG(I)%L_DAM    = 0
        MLAW_TAG(I)%L_DSUM   = 0
        MLAW_TAG(I)%L_DGLO   = 0
        MLAW_TAG(I)%L_CRAK   = 0
        MLAW_TAG(I)%L_ANG    = 0
        MLAW_TAG(I)%L_EPE    = 0
        MLAW_TAG(I)%L_EPC    = 0
        MLAW_TAG(I)%L_XST    = 0
        MLAW_TAG(I)%L_SSP    = 0
        MLAW_TAG(I)%L_Z      = 0
        MLAW_TAG(I)%L_GAMA   = 0  
        MLAW_TAG(I)%L_FRAC   = 0  
        MLAW_TAG(I)%L_BFRAC  = 0  	
        MLAW_TAG(I)%NUVAR    = 0  
        MLAW_TAG(I)%NVARTMP  = 0    
        MLAW_TAG(I)%L_DMG    = 0  	
        MLAW_TAG(I)%L_SEQ    = 0
        MLAW_TAG(I)%L_FAC_YLD= 0  
        MLAW_TAG(I)%L_ABURN  = 0  
        MLAW_TAG(I)%L_MU     = 0  	 
      ENDDO
c-----------------------------------------------
      RETURN
      END
