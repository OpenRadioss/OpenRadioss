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
Chd|  ZEROVARS_AUTO                 source/elements/elbuf_init/zerovars_auto.F
Chd|-- called by -----------
Chd|        ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE ZEROVARS_AUTO(ELBUF_STR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_),TARGET  :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILAY,NLAY,IXFEM,IAD
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(G_BUFEL_)     , POINTER :: GBUF
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      IAD = 0
c        
      GBUF%G_NOFF  = 0  
      IAD = IAD + 1
      GBUF%G_IERR  = 0  
      IAD = IAD + 1
      GBUF%G_OFF   = 0  
      IAD = IAD + 1
      GBUF%G_GAMA  = 0   
      IAD = IAD + 1
      GBUF%G_SMSTR = 0  
      IAD = IAD + 1
      GBUF%G_HOURG = 0   
      IAD = IAD + 1
      GBUF%G_BFRAC = 0   
      IAD = IAD + 1
      GBUF%G_EINT  = 0  
      IAD = IAD + 1
      GBUF%G_EINS  = 0  
      IAD = IAD + 1
      GBUF%G_RHO   = 0   
      IAD = IAD + 1
      GBUF%G_QVIS  = 0   
      IAD = IAD + 1
      GBUF%G_DELTAX= 0   
      IAD = IAD + 1
      GBUF%G_VOL   = 0   
      IAD = IAD + 1
      GBUF%G_EPSD  = 0   
      IAD = IAD + 1
      GBUF%G_EPSQ  = 0   
      IAD = IAD + 1
      GBUF%G_PLA   = 0   
      IAD = IAD + 1
      GBUF%G_TEMP  = 0   
      IAD = IAD + 1
      GBUF%G_TB    = 0   
      IAD = IAD + 1
      GBUF%G_RK    = 0  
      IAD = IAD + 1
      GBUF%G_RE    = 0   
      IAD = IAD + 1
      GBUF%G_SIG   = 0   
      IAD = IAD + 1
      GBUF%G_FOR   = 0   
      IAD = IAD + 1
      GBUF%G_MOM   = 0   
      IAD = IAD + 1
      GBUF%G_THK   = 0   
      IAD = IAD + 1
      GBUF%G_TAG22  = 0   
      IAD = IAD + 1
      GBUF%G_STRA  = 0   
      IAD = IAD + 1
      GBUF%G_SIGI  = 0   
      IAD = IAD + 1
      GBUF%G_DMG   = 0   
      IAD = IAD + 1
      GBUF%G_FORPG = 0   
      IAD = IAD + 1
      GBUF%G_MOMPG = 0   
      IAD = IAD + 1
C
      GBUF%G_FORPGPINCH = 0   
      IAD = IAD + 1
      GBUF%G_MOMPGPINCH = 0   
      IAD = IAD + 1
      GBUF%G_EPGPINCHXZ = 0   
      IAD = IAD + 1
      GBUF%G_EPGPINCHYZ = 0   
      IAD = IAD + 1
      GBUF%G_EPGPINCHZZ = 0   
      IAD = IAD + 1
C
      GBUF%G_STRPG = 0
      IAD = IAD + 1
      GBUF%G_UELR  = 0
      IAD = IAD + 1
      GBUF%G_UELR1 = 0
      IAD = IAD + 1
      GBUF%G_DAMDL = 0
      IAD = IAD + 1
      GBUF%G_FORTH = 0   
      IAD = IAD + 1
      GBUF%G_EINTTH = 0   
      IAD = IAD + 1
      GBUF%G_FILL  = 0   
      IAD = IAD + 1
      GBUF%G_SEQ   = 0
      IAD = IAD + 1
      GBUF%G_STRW = 0  
      IAD = IAD + 1
      GBUF%G_STRWPG = 0  
      IAD = IAD + 1
      GBUF%G_THK_I  = 0   
      IAD = IAD + 1
      GBUF%G_JAC_I  = 0
      IAD = IAD + 1
      GBUF%G_DT     = 0
      IAD = IAD + 1
      GBUF%G_ISMS   = 0
      IAD = IAD + 1
      GBUF%G_BPRELD = 0   ! Bolt preloading
      IAD = IAD + 1
      GBUF%G_ABURN = 0   
      IAD = IAD + 1
      GBUF%G_MU = 0  
      IAD = IAD + 1
      GBUF%G_TEMPG = 0 
      IAD = IAD + 1
      GBUF%G_COR_NF = 0   
      IAD = IAD + 1
      GBUF%G_COR_FR = 0   
      IAD = IAD + 1
      GBUF%G_COR_XR = 0 
      IAD = IAD + 1
      GBUF%G_MAXFRAC = 0   
      IAD = IAD + 1
      GBUF%G_MAXEPS = 0   
      IAD = IAD + 1
      GBUF%G_BETAORTH   = 0     
      IAD = IAD + 1
      GBUF%G_AMU = 0   
      
C 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
      IAD = IAD + 1
      GBUF%G_AREA = 0
      IAD = IAD + 1
      GBUF%G_SKEW = 0
      IAD = IAD + 1
      GBUF%G_LENGTH = 0
      IAD = IAD + 1
      GBUF%G_TOTDEPL = 0
      IAD = IAD + 1
      GBUF%G_TOTROT = 0
      IAD = IAD + 1
      GBUF%G_FOREP = 0
      IAD = IAD + 1
      GBUF%G_MOMEP = 0
      IAD = IAD + 1
      GBUF%G_DEP_IN_TENS = 0
      IAD = IAD + 1
      GBUF%G_DEP_IN_COMP = 0
      IAD = IAD + 1
      GBUF%G_ROT_IN_TENS = 0
      IAD = IAD + 1
      GBUF%G_ROT_IN_COMP = 0
      IAD = IAD + 1
      GBUF%G_POSX = 0
      IAD = IAD + 1
      GBUF%G_POSY = 0
      IAD = IAD + 1
      GBUF%G_POSZ = 0
      IAD = IAD + 1
      GBUF%G_POSXX = 0
      IAD = IAD + 1
      GBUF%G_POSYY = 0
      IAD = IAD + 1
      GBUF%G_POSZZ = 0
      IAD = IAD + 1
      GBUF%G_YIELD = 0
      IAD = IAD + 1
      GBUF%G_LENGTH_ERR = 0
      IAD = IAD + 1
      GBUF%G_DV = 0
      IAD = IAD + 1
      GBUF%G_DFS = 0
      IAD = IAD + 1
      GBUF%G_SKEW_ERR = 0
      IAD = IAD + 1
      GBUF%G_E6 = 0
      IAD = IAD + 1
      GBUF%G_RUPTCRIT = 0
      IAD = IAD + 1
      GBUF%G_MASS = 0
      IAD = IAD + 1
      GBUF%G_V_REPCVT = 0
      IAD = IAD + 1
      GBUF%G_VR_REPCVT = 0
      IAD = IAD + 1
      GBUF%G_NUVAR = 0
      IAD = IAD + 1
      GBUF%G_NUVARN = 0
      IAD = IAD + 1
      GBUF%G_DEFINI = 0
      IAD = IAD + 1
      GBUF%G_FORINI = 0
      IAD = IAD + 1
      GBUF%G_INIFRIC = 0
      IAD = IAD + 1
      GBUF%G_STRHG = 0
      IAD = IAD + 1
      GBUF%G_ETOTSH = 0
      IAD = IAD + 1
      GBUF%G_DT_PITER = 0
      IAD = IAD + 1
      GBUF%G_SKEW_ID = 0
      IAD = IAD + 1
      GBUF%G_GAMA_R  = 0
C
C seatbelts elements
      IAD = IAD + 1
      GBUF%G_SLIPRING_ID = 0
      IAD = IAD + 1
      GBUF%G_SLIPRING_FRAM_ID = 0
      IAD = IAD + 1
      GBUF%G_SLIPRING_STRAND = 0
      IAD = IAD + 1
      GBUF%G_RETRACTOR_ID = 0
      IAD = IAD + 1
      GBUF%G_RINGSLIP = 0
      IAD = IAD + 1
      GBUF%G_ADD_NODE = 0
      IAD = IAD + 1
      GBUF%G_UPDATE = 0  
      IAD = IAD + 1
      GBUF%G_FRAM_FACTOR = 0 
      IAD = IAD + 1
      GBUF%G_INTVAR = 0  
      IAD = IAD + 1
      GBUF%G_IDT_TSH = 0  
c---
      IAD = IAD + 1
      GBUF%NVAR_GLOB = IAD
C
c---------------------
      DO ILAY = 1, ELBUF_STR%NLAY
        BUFLY => ELBUF_STR%BUFLY(ILAY)
C
        IAD = 0
c        
        BUFLY%L_OFF    = 0
        IAD = IAD + 1
        BUFLY%L_GAMA   = 0
        IAD = IAD + 1
        BUFLY%L_STRA   = 0
        IAD = IAD + 1
        BUFLY%L_FRAC   = 0
        IAD = IAD + 1
        BUFLY%L_BFRAC = 0
        IAD = IAD + 1
        BUFLY%L_EINT   = 0
        IAD = IAD + 1
        BUFLY%L_EINS   = 0
        IAD = IAD + 1
        BUFLY%L_RHO    = 0
        IAD = IAD + 1
        BUFLY%L_DP_DRHO = 0
        IAD = IAD + 1
        BUFLY%L_QVIS   = 0
        IAD = IAD + 1
        BUFLY%L_DELTAX = 0
        IAD = IAD + 1
        BUFLY%L_VOL    = 0
        IAD = IAD + 1
        BUFLY%L_EPSA   = 0
        IAD = IAD + 1
        BUFLY%L_EPSD   = 0
        IAD = IAD + 1
        BUFLY%L_EPSQ   = 0
        IAD = IAD + 1
        BUFLY%L_EPSF   = 0
        IAD = IAD + 1
        BUFLY%L_PLA    = 0
        IAD = IAD + 1
        BUFLY%L_TEMP   = 0
        IAD = IAD + 1
        BUFLY%L_TB     = 0
        IAD = IAD + 1
        BUFLY%L_RK     = 0
        IAD = IAD + 1
        BUFLY%L_RE     = 0
        IAD = IAD + 1
        BUFLY%L_VK     = 0
        IAD = IAD + 1
        BUFLY%L_SF     = 0
        IAD = IAD + 1
        BUFLY%L_ROB    = 0
        IAD = IAD + 1
        BUFLY%L_DAM    = 0
        IAD = IAD + 1
        BUFLY%L_DSUM   = 0
        IAD = IAD + 1
        BUFLY%L_DGLO   = 0
        IAD = IAD + 1
        BUFLY%L_CRAK   = 0
        IAD = IAD + 1
        BUFLY%L_ANG    = 0
        IAD = IAD + 1
        BUFLY%L_EPE    = 0
        IAD = IAD + 1
        BUFLY%L_EPC    = 0
        IAD = IAD + 1
        BUFLY%L_XST    = 0
        IAD = IAD + 1
        BUFLY%L_SSP    = 0
        IAD = IAD + 1
        BUFLY%L_Z      = 0
        IAD = IAD + 1
        BUFLY%L_VISC   = 0
        IAD = IAD + 1
        BUFLY%L_SIGL   = 0
        IAD = IAD + 1
        BUFLY%L_SIGV   = 0
        IAD = IAD + 1
        BUFLY%L_SIGA   = 0
        IAD = IAD + 1
        BUFLY%L_SIGB   = 0
        IAD = IAD + 1
        BUFLY%L_SIGC   = 0
        IAD = IAD + 1
        BUFLY%L_SIGD   = 0
        IAD = IAD + 1
        BUFLY%L_SIGF   = 0
        IAD = IAD + 1
        BUFLY%L_SIG    = 0
        IAD = IAD + 1
        BUFLY%L_SIGPLY = 0
        IAD = IAD + 1
        BUFLY%L_FOR    = 0
        IAD = IAD + 1
        BUFLY%L_MOM    = 0
        IAD = IAD + 1
        BUFLY%L_THK    = 0
        IAD = IAD + 1
        BUFLY%L_SMSTR  = 0
        IAD = IAD + 1
        BUFLY%L_DMG    = 0
        IAD = IAD + 1
        BUFLY%L_FORTH  = 0
        IAD = IAD + 1
        BUFLY%L_EINTTH = 0
        IAD = IAD + 1
        BUFLY%L_SEQ    = 0
        IAD = IAD + 1
        BUFLY%L_JAC_I  = 0
        IAD = IAD + 1
        BUFLY%L_FAC_YLD= 0
        IAD = IAD + 1
        BUFLY%L_ABURN = 0
        IAD = IAD + 1
        BUFLY%L_MU = 0
        IAD = IAD + 1
        BUFLY%L_PIJ = 0
        IAD = IAD + 1
        BUFLY%L_VOL0DP = 0
        IAD = IAD + 1
c        
c---
C  nb of local variable
        BUFLY%NVAR_LOC = IAD
c---
        IAD = 0
        BUFLY%LY_DMG   = 0
        IAD = IAD + 1
        BUFLY%LY_GAMA  = 0
        IAD = IAD + 1
        BUFLY%LY_DIRA  = 0
        IAD = IAD + 1
        BUFLY%LY_DIRB  = 0
        IAD = IAD + 1
        BUFLY%LY_CRKDIR= 0
        IAD = IAD + 1
        BUFLY%LY_PLAPT = 0
        IAD = IAD + 1
        BUFLY%LY_SIGPT = 0
        IAD = IAD + 1
        BUFLY%LY_HOURG = 0
        IAD = IAD + 1
        BUFLY%LY_UELR  = 0
        IAD = IAD + 1
        BUFLY%LY_UELR1 = 0
        IAD = IAD + 1
        BUFLY%LY_OFFPG = 0
        IAD = IAD + 1
        BUFLY%LY_OFF   = 0
        IAD = IAD + 1
c---
C  nb of layer variable
        BUFLY%NVAR_LAY = IAD
c---
      ENDDO 
c-----------------------------------------------
c
      DO ILAY = 1, ELBUF_STR%NINTLAY
        IAD = 0
        ELBUF_STR%INTLAY(ILAY)%NVAR_MAT = 0
        IAD = IAD + 1
        ELBUF_STR%INTLAY(ILAY)%NVARTMP  = 0
        IAD = IAD + 1
        ELBUF_STR%INTLAY(ILAY)%NFAIL    = 0
      ENDDO
c-----------------------------------------------
      RETURN
      END SUBROUTINE ZEROVARS_AUTO
