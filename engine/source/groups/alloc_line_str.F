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
Chd|  ALLOC_LINE_STR                source/groups/alloc_line_str.F
Chd|-- called by -----------
Chd|        LINE_INI                      source/groups/line_ini.F      
Chd|-- calls ---------------
Chd|        FRETITL2                      source/input/freform.F        
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALLOC_LINE_STR(IGRSLIN)
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
#include      "my_allocate.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SURF_)   ,DIMENSION(NSLIN) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,I,J,K,L_SLIN,ID,NSEG,TYPE,PROC,
     .        LEVEL,NSEG_R2R_ALL,NSEG_R2R_SHARE,NODE,
     .        ELTYP,ELEM,ERR,ITITLE(LTITR),LEN
      CHARACTER(LEN=LTITR) :: TITR,NEW_TITLE(NSLIN)
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  ISLINI
!=======================================================================
      DO ISU=1,NSLIN
        CALL READ_I_C(ITITLE,LTITR)
        CALL FRETITL2(TITR,ITITLE,LTITR)
        LEN = 0
        DO J=1,LTITR
          IF (TITR(J:J) /= ' ') LEN = J
        ENDDO
        NEW_TITLE(ISU) = TITR(1:LEN)
      ENDDO ! DO ISU=1,NSLIN
!!------  Allocate and read subset data from restart
      ALLOCATE (ISLINI(LENISLIN) ,STAT=ERR)
      CALL READ_I_C(ISLINI, LENISLIN)
!=======================================================================
      L_SLIN = 0
!
      DO ISU=1,NSLIN
        ID             = ISLINI(L_SLIN+1)
          L_SLIN = L_SLIN+1
        NSEG           = ISLINI(L_SLIN+1)
          L_SLIN = L_SLIN+1
        TYPE           = ISLINI(L_SLIN+1)
          L_SLIN = L_SLIN+1
        LEVEL          = ISLINI(L_SLIN+1)
          L_SLIN = L_SLIN+1
        NSEG_R2R_ALL   = ISLINI(L_SLIN+1)
          L_SLIN = L_SLIN+1
        NSEG_R2R_SHARE = ISLINI(L_SLIN+1)
          L_SLIN = L_SLIN+1
!
!        ICHAR(TITR)    = ISLINI(L_SLIN+1)
!          L_SLIN = L_SLIN+1
!---
       IGRSLIN(ISU)%ID             = ID
       IGRSLIN(ISU)%NSEG           = NSEG
       IGRSLIN(ISU)%TYPE           = TYPE
       IGRSLIN(ISU)%LEVEL          = LEVEL
       IGRSLIN(ISU)%NSEG_R2R_ALL   = NSEG_R2R_ALL
       IGRSLIN(ISU)%NSEG_R2R_SHARE = NSEG_R2R_SHARE
!
! LINE ENTITIES (NODES, PROC, ELTYP, ELEM)
!
       ALLOCATE(IGRSLIN(ISU)%NODES(NSEG,2))
       ALLOCATE(IGRSLIN(ISU)%ELTYP(NSEG))
       ALLOCATE(IGRSLIN(ISU)%ELEM(NSEG))
       ALLOCATE(IGRSLIN(ISU)%PROC(NSEG))
!
        DO J=1,NSEG
          DO K=1,2
            NODE = ISLINI(L_SLIN+1)
              L_SLIN = L_SLIN+1
            IGRSLIN(ISU)%NODES(J,K) = NODE
          ENDDO
          PROC = ISLINI(L_SLIN+1)
            L_SLIN = L_SLIN+1
          IGRSLIN(ISU)%PROC(J) = PROC
          ELTYP = ISLINI(L_SLIN+1)
            L_SLIN = L_SLIN+1
          IGRSLIN(ISU)%ELTYP(J) = ELTYP
          ELEM = ISLINI(L_SLIN+1)
            L_SLIN = L_SLIN+1
          IGRSLIN(ISU)%ELEM(J) = ELEM
        ENDDO
      ENDDO ! DO ISU=1,NSLIN
!---------
      DEALLOCATE (ISLINI)
!---------
      RETURN
      END
