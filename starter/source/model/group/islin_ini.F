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
Chd|  ISLIN_INI                     source/model/group/islin_ini.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ISLIN_INI(IGRSLIN)
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
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SURF_)   ,DIMENSION(NSLIN) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L_SLIN,ISU,ID,NSEG,TYPE,PROC,
     .        LEVEL,NSEG_R2R_ALL,NSEG_R2R_SHARE
C-----------------------------------------------
!---
!  Size of ISLIN structure
!---
!
      L_SLIN = 0
      DO ISU=1,NSLIN
! variables
        ID        = IGRSLIN(ISU)%ID
            L_SLIN = L_SLIN + 1
        NSEG      = IGRSLIN(ISU)%NSEG
            L_SLIN = L_SLIN + 1
        TYPE      = IGRSLIN(ISU)%TYPE
            L_SLIN = L_SLIN + 1
        LEVEL     = IGRSLIN(ISU)%LEVEL
            L_SLIN = L_SLIN + 1
        NSEG_R2R_ALL = IGRSLIN(ISU)%NSEG_R2R_ALL
            L_SLIN = L_SLIN + 1
        NSEG_R2R_SHARE = IGRSLIN(ISU)%NSEG_R2R_SHARE
            L_SLIN = L_SLIN + 1
!
          L_SLIN = L_SLIN + 2*NSEG  ! nodes of segment
          L_SLIN = L_SLIN + NSEG    ! PROC  of segment
          L_SLIN = L_SLIN + NSEG    ! ELTYP of segment
          L_SLIN = L_SLIN + NSEG    ! ELEM  of segment
      ENDDO ! DO ISU=1,NSLIN
!
!---------
      LENISLIN = L_SLIN
!---------
      RETURN
      END
