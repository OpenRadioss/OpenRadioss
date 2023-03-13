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
Chd|  W_GR_ENTITY                   source/output/restart/w_gr_entity.F
Chd|-- called by -----------
Chd|        W_GROUP_STR                   source/output/restart/w_group_str.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/input/freform.F        
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_GR_ENTITY(IGR,NGR,LENIGR)
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
#include      "nchar_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGR,LENIGR
!
      TYPE (GROUP_) , DIMENSION(NGR) :: IGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,NENTITY,GRTYPE,SORTED,GRPGRP,LEVEL,
     .        ERR,R2R_ALL,R2R_SHARE,ENTITY,ITITLE(LTITR),L_GROUP
      CHARACTER(LEN=nchartitle) :: TITR
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  IGROUP
C-----------------------------------------------
!       LTITR = 40
C-----------------------------------------------
      DO IGU=1,NGR
        TITR    = IGR(IGU)%TITLE
        CALL FRETITL(TITR,ITITLE,LTITR)
        CALL WRITE_I_C(ITITLE,LTITR)
      ENDDO ! DO IGU=1,NGR
!
      ERR = 0
      ALLOCATE (IGROUP(LENIGR), STAT=ERR)
      IGROUP(1:LENIGR) = 0  
      L_GROUP = 0
!
      DO IGU=1,NGR
        ID        = IGR(IGU)%ID
        NENTITY   = IGR(IGU)%NENTITY
        GRTYPE    = IGR(IGU)%GRTYPE
        SORTED    = IGR(IGU)%SORTED
        GRPGRP    = IGR(IGU)%GRPGRP
        LEVEL     = IGR(IGU)%LEVEL
        TITR      = IGR(IGU)%TITLE  !!!- already written above loop
        R2R_ALL   = IGR(IGU)%R2R_ALL
        R2R_SHARE = IGR(IGU)%R2R_SHARE
!
        IGROUP(L_GROUP+1) = ID
          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = NENTITY
          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = GRTYPE
          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = SORTED
          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = GRPGRP
          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = LEVEL
          L_GROUP = L_GROUP+1
!        IGROUP(L_GROUP+1) = TITR  -!!!- already written above loop
!          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = R2R_ALL
          L_GROUP = L_GROUP+1
        IGROUP(L_GROUP+1) = R2R_SHARE
          L_GROUP = L_GROUP+1
! GROUP ENTITIES
        DO J=1,NENTITY
          ENTITY = IGR(IGU)%ENTITY(J)
          IGROUP(L_GROUP+1) = ENTITY
          L_GROUP = L_GROUP+1
        ENDDO
      ENDDO ! DO IGU=1,NGR
!---------
      CALL WRITE_I_C(IGROUP,L_GROUP)
!---------
      DEALLOCATE (IGROUP)
!---------
      RETURN
      END
