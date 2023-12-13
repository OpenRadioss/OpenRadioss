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
Chd|  W_SUBSET_STR                  source/output/restart/w_subset_str.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/input/freform.F        
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_SUBSET_STR(SUBSET)
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,ISU,NCHILD,NBPART,NBTPART,ICHILD,IPART,IPARTTOT,
     .        PARENT,LEVEL,TH_PART,L_SUB,ERR,ITITLE(LTITR),THIAD,IVARTH
      CHARACTER(LEN=nchartitle) :: TITR
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  ISUB
C-----------------------------------------------
      DO ISU=1,NSUBS
        TITR    = SUBSET(ISU)%TITLE
        CALL FRETITL(TITR,ITITLE,LTITR)
        CALL WRITE_I_C(ITITLE,LTITR)
      ENDDO ! DO ISU=1,NSUBS
!
      ERR = 0
      ALLOCATE (ISUB(LENSUBS), STAT=ERR)
      ISUB(1:LENSUBS) = 0  
      L_SUB = 0
!
      DO ISU=1,NSUBS
        ID       = SUBSET(ISU)%ID
        NCHILD   = SUBSET(ISU)%NCHILD
        NBPART   = SUBSET(ISU)%NPART
        NBTPART  = SUBSET(ISU)%NTPART
        PARENT   = SUBSET(ISU)%PARENT
        LEVEL    = SUBSET(ISU)%LEVEL
        THIAD    = SUBSET(ISU)%THIAD
!!        TH_FLAG = SUBSET(ISU)%TH_FLAG
!!        TITR    = SUBSET(ISU)%TITLE
!
        ISUB(L_SUB+1) = ID
          L_SUB = L_SUB+1
        ISUB(L_SUB+1) = NCHILD
          L_SUB = L_SUB+1
        ISUB(L_SUB+1) = NBPART
          L_SUB = L_SUB+1
        ISUB(L_SUB+1) = NBTPART
          L_SUB = L_SUB+1
        ISUB(L_SUB+1) = PARENT
          L_SUB = L_SUB+1
        ISUB(L_SUB+1) = LEVEL
          L_SUB = L_SUB+1
        ISUB(L_SUB+1) = THIAD
          L_SUB = L_SUB+1
!!        ISUB(L_SUB+1) = TH_FLAG
!!          L_SUB = L_SUB+1
! NOMBRE DE SUBSETS FILS
        DO J=1,NCHILD
          ICHILD = SUBSET(ISU)%CHILD(J)
          ISUB(L_SUB+1) = ICHILD
          L_SUB = L_SUB+1
        ENDDO
! NOMBRE DE PARTS FILLES
        DO J=1,NBPART
          IPART = SUBSET(ISU)%PART(J)
          ISUB(L_SUB+1) = IPART
          L_SUB = L_SUB+1
        ENDDO
! NOMBRE TOTAL DE PARTS (DESCENDANCE)
        DO J=1,NBTPART
          IPARTTOT = SUBSET(ISU)%TPART(J)
          ISUB(L_SUB+1) = IPARTTOT
          L_SUB = L_SUB+1
        ENDDO
!---
!   - TIME HISTORY - /iTH (9) + /TH (1)
!---
        DO J=1,10
          IVARTH = SUBSET(ISU)%NVARTH(J)
          ISUB(L_SUB+1) = IVARTH
          L_SUB = L_SUB+1
        ENDDO
      ENDDO ! DO ISU=1,NSUBS
!
!---------
      CALL WRITE_I_C(ISUB,L_SUB)
!---------
      DEALLOCATE (ISUB)
!---------
      RETURN
      END
