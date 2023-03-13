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
Chd|  ALLOC_GROUP_STR               source/groups/alloc_group_str.F
Chd|-- called by -----------
Chd|        GROUP_INI                     source/groups/group_ini.F     
Chd|-- calls ---------------
Chd|        FRETITL2                      source/input/freform.F        
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALLOC_GROUP_STR(IGR,NGR,LENIGR)
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
#include      "scr17_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGR,LENIGR
!
      TYPE(GROUP_)   ,DIMENSION(NGR) :: IGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,GRTYPE,SORTED,GRPGRP,
     .        LEVEL,R2R_ALL,R2R_SHARE,ENTITY,NENTITY,L_GROUP,
     .        ITITLE(LTITR),ERR,LEN
      CHARACTER(LEN=LTITR) ::  TITR,NEW_TITLE(NGR)
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGROUP
C-----------------------------------------------
      DO IGU=1,NGR
        CALL READ_I_C(ITITLE,LTITR)
        CALL FRETITL2(TITR,ITITLE,LTITR)
        LEN = 0
        DO J=1,LTITR
          IF (TITR(J:J) /= ' ') LEN = J
        ENDDO
        NEW_TITLE(IGU) = TITR(1:LEN)
      ENDDO ! DO IGU=1,NGR
!------  Allocate and read subset data from restart
      ALLOCATE (IGROUP(LENIGR) ,STAT=ERR)
      CALL READ_I_C(IGROUP, LENIGR)
!=======================================================================
!
      L_GROUP = 0
!
      DO IGU=1,NGR
        ID        = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
        NENTITY   = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
        GRTYPE    = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
        SORTED    = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
        GRPGRP    = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
        LEVEL     = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
!        TITR      = NEW_TITLE(IGU)
!          L_GROUP = L_GROUP+1
        R2R_ALL   = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
        R2R_SHARE = IGROUP(L_GROUP+1)
          L_GROUP = L_GROUP+1
!
!        ICHAR(TITR)    = IGROUP(L_GROUP+1)
!          L_GROUP = L_GROUP+1
!---
       IGR(IGU)%ID        = ID
       IGR(IGU)%NENTITY   = NENTITY
       IGR(IGU)%GRTYPE    = GRTYPE
       IGR(IGU)%SORTED    = SORTED
       IGR(IGU)%GRPGRP    = GRPGRP
       IGR(IGU)%LEVEL     = LEVEL
       IGR(IGU)%TITLE     = NEW_TITLE(IGU)
       IGR(IGU)%R2R_ALL   = R2R_ALL
       IGR(IGU)%R2R_SHARE = R2R_SHARE
!---
! GROUP ENTITIES
!
       MY_ALLOCATE(IGR(IGU)%ENTITY,NENTITY)
!
        DO J=1,NENTITY
          ENTITY = IGROUP(L_GROUP+1)
            L_GROUP = L_GROUP+1
          IGR(IGU)%ENTITY(J) = ENTITY
        ENDDO
      ENDDO ! DO IGU=1,NGR
!---------
      DEALLOCATE (IGROUP)
!---------
      RETURN
      END
