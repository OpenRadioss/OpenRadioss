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
Chd|  GR_ENTITY_INI                 source/model/group/gr_entity_ini.F
Chd|-- called by -----------
Chd|        GROUP_INI                     source/model/group/group_ini.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE GR_ENTITY_INI(IGR,NGR,LENGROUP)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE GROUPDEF_MOD
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
      INTEGER NGR,LENGROUP
!
      TYPE(GROUP_)   ,DIMENSION(NGR) :: IGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,NENTITY,GRTYPE,TRI,GRPGRP,LEVEL,R2R_ALL,
     .        R2R_SHARE,L_GROUP
      CHARACTER TITR*nchartitle
C-----------------------------------------------
!---
!  Size of SUBSET_ structure
!---
!
      L_GROUP = 0
      DO IGU=1,NGR
! variables
        ID        = IGR(IGU)%ID
            L_GROUP = L_GROUP + 1
        NENTITY   = IGR(IGU)%NENTITY
            L_GROUP = L_GROUP + 1
        GRTYPE    = IGR(IGU)%GRTYPE
            L_GROUP = L_GROUP + 1
        TRI       = IGR(IGU)%SORTED
            L_GROUP = L_GROUP + 1
        GRPGRP    = IGR(IGU)%GRPGRP
            L_GROUP = L_GROUP + 1
        LEVEL     = IGR(IGU)%LEVEL
            L_GROUP = L_GROUP+1
!        TITR      = IGR(IGU)%TITLE
!            L_GROUP = L_GROUP+1
        R2R_ALL   = IGR(IGU)%R2R_ALL
            L_GROUP = L_GROUP+1
        R2R_SHARE = IGR(IGU)%R2R_SHARE
            L_GROUP = L_GROUP+1
! arrays
            L_GROUP = L_GROUP + NENTITY   !  IGR(IGR)%ENTITY(NENTITY)
      ENDDO ! DO IGU=1,NGR
!
!---------
      LENGROUP = L_GROUP
!---------
      RETURN
      END
