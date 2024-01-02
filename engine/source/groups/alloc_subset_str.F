Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ALLOC_SUBSET_STR              source/groups/alloc_subset_str.F
Chd|-- called by -----------
Chd|        SUBSET_INI                    source/groups/subset_ini.F    
Chd|-- calls ---------------
Chd|        FRETITL2                      source/input/freform.F        
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALLOC_SUBSET_STR(SUBSET)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
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
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,ISU,NCHILD,NBPART,NBTPART,ICHILD,IPART,IPARTTOT,
     .        PARENT,LEVEL,TH_PART,L_SUB,ERR,ITITLE(LTITR),THIAD,IVARTH,
     .        LEN
      CHARACTER(LEN=LTITR) ::  TITR,NEW_TITLE(NSUBS)
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  ISUB
!=======================================================================
      DO ISU=1,NSUBS
        CALL READ_I_C(ITITLE,LTITR)
        CALL FRETITL2(TITR,ITITLE,LTITR)
        LEN = 0
        DO J=1,LTITR
          IF (TITR(J:J) /= ' ') LEN = J
        ENDDO
        NEW_TITLE(ISU) = TITR(1:LEN)
      ENDDO ! DO ISU=1,NSUBS
!!------  Allocate and read subset data from restart
      ALLOCATE (ISUB(LENSUBS) ,STAT=ERR)
      CALL READ_I_C(ISUB, LENSUBS)
!=======================================================================
!
      L_SUB = 0
!
      DO ISU=1,NSUBS
        ID       = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
        NCHILD   = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
        NBPART   = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
        NBTPART  = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
        PARENT   = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
        LEVEL   = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
        THIAD   = ISUB(L_SUB+1)
          L_SUB = L_SUB+1
!!        TH_FLAG = ISUB(L_SUB+1)
!!          L_SUB = L_SUB+1
!!        ICHAR(TITR)    = ISUB(L_SUB+1)
!!          L_SUB = L_SUB+1
!---
       SUBSET(ISU)%ID     = ID
       SUBSET(ISU)%NCHILD = NCHILD
       SUBSET(ISU)%NPART  = NBPART
       SUBSET(ISU)%NTPART = NBTPART
       SUBSET(ISU)%PARENT = PARENT
       SUBSET(ISU)%LEVEL  = LEVEL
       SUBSET(ISU)%THIAD  = THIAD
!!       SUBSET(ISU)%TH_FLAG = TH_FLAG
       SUBSET(ISU)%TITLE = NEW_TITLE(ISU)
       ALLOCATE(SUBSET(ISU)%CHILD(NCHILD))
       ALLOCATE(SUBSET(ISU)%PART(NBPART))
       ALLOCATE(SUBSET(ISU)%TPART(NBTPART))
       ALLOCATE(SUBSET(ISU)%NVARTH(10))  !  /iTH (9) + /TH (1) -> SUBSET(I)%NVARTH(ITHFLAG)
!---
! NOMBRE DE SUBSETS FILS
        DO J=1,NCHILD
          ICHILD = ISUB(L_SUB+1)
            L_SUB = L_SUB+1
          SUBSET(ISU)%CHILD(J) = ICHILD
        ENDDO
! NOMBRE DE PARTS FILLES
        DO J=1,NBPART
          IPART = ISUB(L_SUB+1)
            L_SUB = L_SUB+1
          SUBSET(ISU)%PART(J) = IPART
        ENDDO
! NOMBRE TOTAL DE PARTS (DESCENDANCE)
        DO J=1,NBTPART
          IPARTTOT = ISUB(L_SUB+1)
            L_SUB = L_SUB+1
          SUBSET(ISU)%TPART(J) = IPARTTOT
        ENDDO
!---
!   - TIME HISTORY - /iTH (9) + /TH (1)
!---
        DO J=1,10
          IVARTH =  ISUB(L_SUB+1)
            L_SUB = L_SUB+1
          SUBSET(ISU)%NVARTH(J) = IVARTH
        ENDDO
      ENDDO ! DO ISU=1,NSUBS
!---------
      DEALLOCATE (ISUB)
!---------
      RETURN
      END
