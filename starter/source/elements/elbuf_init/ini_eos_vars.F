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
Chd|  INI_EOS_VARS                  source/elements/elbuf_init/ini_eos_vars.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|====================================================================
      SUBROUTINE INI_EOS_VARS(EOS_TAG,MAXEOS)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                                          :: MAXEOS
      TYPE(EOS_TAG_)    ,TARGET ,DIMENSION(0:MAXEOS)   :: EOS_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                 :: ILAW,IEOS
      TYPE(EOS_TAG_), POINTER :: ETAG
C=======================================================================
C     Set default variables used in all equation of state (eos)
C=======================================================================

      DO IEOS = 0,MAXEOS
        ETAG => EOS_TAG(IEOS)

        ETAG%G_MU     = 0
        ETAG%G_TB     = 0
        ETAG%G_ABURN  = 0
        ETAG%G_BFRAC  = 0
 	
        ETAG%L_MU     = 0
        ETAG%L_TB     = 0
        ETAG%L_ABURN  = 0
        ETAG%L_BFRAC  = 0
      ENDDO

C-----------------------------------------------
      RETURN
      END SUBROUTINE INI_EOS_VARS
