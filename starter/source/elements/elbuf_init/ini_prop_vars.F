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
Chd|  INI_PROP_VARS                 source/elements/elbuf_init/ini_prop_vars.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE INI_PROP_VARS(PROP_TAG)
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
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PROP_TAG_)    ,TARGET ,DIMENSION(0:MAXPROP)  :: PROP_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPROP
      TYPE(PROP_TAG_)    , POINTER :: PTAG
C=======================================================================
C     Set default variables used in all property types
C=======================================================================
c-----  Void property 
      PTAG => PROP_TAG(0)
c-----
      PTAG%G_OFF    = 1             
      PTAG%G_NOFF   = 0             
      PTAG%G_GAMA   = 0             
      PTAG%G_HOURG  = 0             
      PTAG%G_SIG    = 0   
      PTAG%G_STRA   = 0   
      PTAG%G_FOR    = 0             
      PTAG%G_MOM    = 0             
      PTAG%G_EINT   = 0             
      PTAG%G_EINS   = 0             
      PTAG%G_THK    = 0             
      PTAG%G_VOL    = 0             
      PTAG%G_QVIS   = 0             
      PTAG%G_FORPG  = 0           
      PTAG%G_MOMPG  = 0
C
      PTAG%G_FORPGPINCH = 0
      PTAG%G_MOMPGPINCH = 0
      PTAG%G_EPGPINCHXZ = 0
      PTAG%G_EPGPINCHYZ = 0  
      PTAG%G_EPGPINCHZZ = 0
C            
      PTAG%G_STRPG  = 0
      PTAG%G_FORTH  = 0             
      PTAG%G_EINTTH = 0             
      PTAG%G_FILL   = 0             
c     layer variables
      PTAG%LY_DMG   = 0             
      PTAG%LY_GAMA  = 0             
      PTAG%LY_DIRA  = 0             
      PTAG%LY_DIRB  = 0             
      PTAG%LY_PLAPT = 0             
      PTAG%LY_SIGPT = 0
      PTAG%LY_HOURG = 0
c
      PTAG%L_OFF    = 1                     
      PTAG%L_GAMA   = 0  
      PTAG%L_STRA   = 0             
      PTAG%L_SIG    = 0                     
      PTAG%L_SIGPLY = 0                     
      PTAG%L_SIGL   = 0                     
      PTAG%L_SIGD   = 0                     
      PTAG%L_SIGV   = 0                     
      PTAG%L_FOR    = 0             
      PTAG%L_MOM    = 0
      PTAG%L_EINT   = 0      
      PTAG%L_EINS   = 0                     
      PTAG%L_THK    = 0             
      PTAG%L_VOL    = 0             
      PTAG%L_QVIS   = 0                       
      PTAG%NUVAR    = 0
      PTAG%L_FORTH  = 0
      PTAG%L_EINTTH = 0     
C
      PTAG%G_AREA   = 0
      PTAG%G_SKEW   = 0
      PTAG%G_LENGTH = 0
      PTAG%G_TOTDEPL= 0
      PTAG%G_TOTROT = 0
      PTAG%G_FOREP  = 0
      PTAG%G_MOMEP  = 0
      PTAG%G_DEP_IN_TENS = 0
      PTAG%G_DEP_IN_COMP = 0
      PTAG%G_ROT_IN_TENS = 0
      PTAG%G_ROT_IN_COMP = 0
      PTAG%G_POSX   = 0
      PTAG%G_POSY   = 0
      PTAG%G_POSZ   = 0
      PTAG%G_POSXX  = 0
      PTAG%G_POSYY  = 0
      PTAG%G_POSZZ  = 0
      PTAG%G_YIELD  = 0
      PTAG%G_LENGTH_ERR = 0
      PTAG%G_DV     = 0
      PTAG%G_DFS    = 0
      PTAG%G_SKEW_ERR = 0
      PTAG%G_E6     = 0
      PTAG%G_RUPTCRIT = 0
      PTAG%G_MASS = 0
      PTAG%G_V_REPCVT = 0
      PTAG%G_VR_REPCVT = 0
      PTAG%G_NUVAR = 0
      PTAG%G_NUVARN = 0
      PTAG%G_DEFINI = 0
      PTAG%G_FORINI = 0
      PTAG%G_INIFRIC = 0
      PTAG%G_SKEW_ID = 0
c
      DO IPROP = 1,MAXPROP
        PTAG => PROP_TAG(IPROP)
c       global variables - scalars
        PTAG%G_OFF    = 1           
        PTAG%G_NOFF   = 0           
        PTAG%G_GAMA   = 0           
        PTAG%G_HOURG  = 0           
        PTAG%G_SMSTR  = 0           
        PTAG%G_SIG    = 0           
        PTAG%G_FOR    = 0           
        PTAG%G_MOM    = 0           
        PTAG%G_EINT   = 1           
        PTAG%G_EINS   = 0           
        PTAG%G_THK    = 0           
        PTAG%G_VOL    = 0           
        PTAG%G_QVIS   = 0           
        PTAG%G_FORPG  = 0           
        PTAG%G_MOMPG  = 0
        PTAG%G_STRA   = 0
C
        PTAG%G_FORPGPINCH = 0
        PTAG%G_MOMPGPINCH = 0
        PTAG%G_EPGPINCHXZ = 0       
        PTAG%G_EPGPINCHYZ = 0  
        PTAG%G_EPGPINCHZZ = 0  
C            
        PTAG%G_STRPG  = 0
        PTAG%G_FORTH  = 0           
        PTAG%G_EINTTH = 0           
        PTAG%G_COR_NF = 0           
        PTAG%G_COR_FR = 0           
        PTAG%G_COR_XR = 0           
C    - 1D - elem (TRUSS, BEAM, SPRING, NSTRAND)
        PTAG%G_AREA   = 0
        PTAG%G_SKEW   = 0
        PTAG%G_LENGTH = 0
        PTAG%G_TOTDEPL= 0
        PTAG%G_TOTROT = 0
        PTAG%G_FOREP  = 0
        PTAG%G_MOMEP  = 0
        PTAG%G_DEP_IN_TENS = 0
        PTAG%G_DEP_IN_COMP = 0
        PTAG%G_ROT_IN_TENS = 0
        PTAG%G_ROT_IN_COMP = 0
        PTAG%G_POSX   = 0
        PTAG%G_POSY   = 0
        PTAG%G_POSZ   = 0
        PTAG%G_POSXX  = 0
        PTAG%G_POSYY  = 0
        PTAG%G_POSZZ  = 0
        PTAG%G_YIELD  = 0
        PTAG%G_LENGTH_ERR = 0
        PTAG%G_DV     = 0
        PTAG%G_DFS    = 0
        PTAG%G_SKEW_ERR = 0
        PTAG%G_E6     = 0
        PTAG%G_RUPTCRIT = 0
        PTAG%G_MASS = 0
        PTAG%G_V_REPCVT = 0
        PTAG%G_VR_REPCVT = 0
        PTAG%G_NUVAR = 0
        PTAG%G_NUVARN = 0
        PTAG%G_DEFINI = 0
        PTAG%G_FORINI = 0
        PTAG%G_INIFRIC = 0
        PTAG%G_SKEW_ID = 0
c
C---
c       layer variables
        PTAG%LY_DMG   = 0             
        PTAG%LY_GAMA  = 0             
        PTAG%LY_DIRA  = 0             
        PTAG%LY_DIRB  = 0             
        PTAG%LY_PLAPT = 0             
        PTAG%LY_SIGPT = 0
        PTAG%LY_HOURG = 0
c
c---    local variables (per integration point)
c
        PTAG%L_OFF    = 1                   
        PTAG%L_GAMA   = 0  
        PTAG%L_STRA   = 0           
        PTAG%L_SIG    = 0                   
        PTAG%L_SIGPLY = 0                   
        PTAG%L_SIGL   = 0                   
        PTAG%L_SIGV   = 0                   
        PTAG%L_SIGV   = 0                   
        PTAG%L_FOR    = 0           
        PTAG%L_MOM    = 0
        PTAG%L_EINT   = 1    !      
        PTAG%L_EINS   = 0                   
        PTAG%L_THK    = 0           
        PTAG%L_VOL    = 0           
        PTAG%L_QVIS   = 0                     
        PTAG%NUVAR    = 0 
        PTAG%L_FORTH  = 0           
        PTAG%L_EINTTH = 0              
        PTAG%G_FILL   = 0             
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE INI_PROP_VARS
