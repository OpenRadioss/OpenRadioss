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
Chd|  SUBSET_INI                    source/model/group/subset_ini.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SUBSET_INI(SUBSET)
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
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_IA
!
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,ISU,NCHILD,NBPART,NBTPART,ICHILD,IPART,IPARTTOT,
     .        PARENT,LEVEL,TH_PART,L_SUB,THIAD
      CHARACTER TITR*nchartitle
C-----------------------------------------------
!---
!  Size of SUBSET_ structure
!---
!
      L_SUB = 0
      DO ISU=1,NSUBS
! variables
        ID       = SUBSET(ISU)%ID
          L_SUB = L_SUB + 1
        NCHILD   = SUBSET(ISU)%NCHILD
          L_SUB = L_SUB + 1
        NBPART   = SUBSET(ISU)%NPART
          L_SUB = L_SUB + 1
        NBTPART  = SUBSET(ISU)%NTPART
          L_SUB = L_SUB + 1
        PARENT   = SUBSET(ISU)%PARENT
          L_SUB = L_SUB + 1
        LEVEL   = SUBSET(ISU)%LEVEL
          L_SUB = L_SUB+1
        THIAD   = SUBSET(ISU)%THIAD
          L_SUB = L_SUB+1
!!        TH_FLAG = SUBSET(ISU)%TH_FLAG
!!          L_SUB = L_SUB+1
!!        TITR    = SUBSET(ISU)%TITLE
!!          L_SUB = L_SUB+1
!
! arrays
          L_SUB = L_SUB + NCHILD
          L_SUB = L_SUB + NBPART
          L_SUB = L_SUB + NBTPART
          L_SUB = L_SUB + 10  !  /iTH (9) + /TH (1) -> SUBSET(I)%NVARTH(ITHFLAG)
      ENDDO ! DO ISU=1,NSUBS
!
!---------
      LENSUBS = L_SUB
!---------
      RETURN
      END
