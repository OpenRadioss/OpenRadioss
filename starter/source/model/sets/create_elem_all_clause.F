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
Chd|  CREATE_ELEM_ALL_CLAUSE        source/model/sets/create_elem_all_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_ELEM_ALL_CLAUSE( CLAUSE )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create Element list from PART list
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,IE,EL,IP,EL_FIRST,EL_LAST,SIZE,IND,SZELMAX,I
C=======================================================================
!
      !  SOLID
      IF (NUMELS > 0) THEN
        CLAUSE%NB_SOLID = NUMELS
        ALLOCATE( CLAUSE%SOLID(NUMELS) )
        DO I=1,NUMELS
         CLAUSE%SOLID(I) = I
        ENDDO
      ENDIF ! IF (NUMELS > 0)

!
      !  QUAD
      IF (NUMELQ > 0) THEN
        CLAUSE%NB_QUAD = NUMELQ
        ALLOCATE( CLAUSE%QUAD(NUMELQ) )
        DO I=1,NUMELQ
         CLAUSE%QUAD(I) = I
        ENDDO
      ENDIF ! IF (NUMELQ > 0)
!
      !  SHELL (4N)
      IF (NUMELC > 0) THEN
        CLAUSE%NB_SH4N = NUMELC
        ALLOCATE( CLAUSE%SH4N(NUMELC) )
        DO I=1,NUMELC
         CLAUSE%SH4N(I) = I
        ENDDO
      ENDIF ! IF (NUMELC > 0)

!
      !  SHELL (3N)
      IF (NUMELTG > 0) THEN
        CLAUSE%NB_SH3N = NUMELTG
        ALLOCATE( CLAUSE%SH3N(NUMELTG) )
        DO I=1,NUMELTG
          CLAUSE%SH3N(I) = I
        ENDDO
      ENDIF ! IF (NUMELTG > 0)

!
      !  TRIA
      IF (NUMELTRIA > 0) THEN
        CLAUSE%NB_TRIA = NUMELTRIA
        ALLOCATE( CLAUSE%TRIA(NUMELTRIA) )
        DO I=1,NUMELTRIA
          CLAUSE%TRIA(I) = I
        ENDDO
      ENDIF ! IF (NUMELTRIA > 0)
!
      !  TRUSS
      IF (NUMELT > 0) THEN
        CLAUSE%NB_TRUSS = NUMELT
        ALLOCATE( CLAUSE%TRUSS(NUMELT) )
        DO I=1,NUMELT
          CLAUSE%TRUSS(I) = I
        ENDDO
      ENDIF ! IF (NUMELT > 0)

!
      !  BEAM
      IF (NUMELP > 0) THEN
        CLAUSE%NB_BEAM = NUMELP
        ALLOCATE( CLAUSE%BEAM(NUMELP) )
        DO I=1,NUMELP
          CLAUSE%BEAM(I) = I
        ENDDO
      ENDIF ! IF (NUMELP > 0)
!
      !  SPRING
      IF (NUMELR > 0) THEN
        CLAUSE%NB_SPRING = NUMELR
        ALLOCATE( CLAUSE%SPRING(NUMELR) )
        DO I=1,NUMELR
          CLAUSE%SPRING(I) = I
        ENDDO
      ENDIF ! IF (NUMELR > 0)
!
C-------------------------
      RETURN
      END
