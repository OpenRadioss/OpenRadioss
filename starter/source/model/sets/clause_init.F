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
Chd|  CLAUSE_INIT                   source/model/sets/clause_init.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CLAUSE_INIT(CLAUSE)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CLAUSE%NB_NODE= 0             ! CLAUSE nb of nodes
      CLAUSE%NB_PART= 0             ! CLAUSE nb of parts
      CLAUSE%NB_SOLID= 0            ! CLAUSE nb of solids
      CLAUSE%NB_QUAD= 0             ! CLAUSE nb of quads
      CLAUSE%NB_SH4N= 0             ! CLAUSE nb of sh4n
      CLAUSE%NB_SH3N= 0             ! CLAUSE nb of sh3n
      CLAUSE%NB_TRIA= 0             ! CLAUSE nb of trias
      CLAUSE%NB_BEAM= 0             ! CLAUSE nb of beams
      CLAUSE%NB_TRUSS= 0            ! CLAUSE nb of truss
      CLAUSE%NB_SPRING= 0           ! CLAUSE nb of spring
      CLAUSE%NB_SURF_SEG= 0         ! CLAUSE nb of surfacce's segments
      CLAUSE%NB_LINE_SEG= 0         ! CLAUSE nb of line/edge's segments
      CLAUSE%NB_RBODY= 0            ! CLAUSE nb of Rbodys
!---
      IF(ALLOCATED( CLAUSE%NODE ))          DEALLOCATE( CLAUSE%NODE )
      IF(ALLOCATED( CLAUSE%PART ))          DEALLOCATE( CLAUSE%PART )
      IF(ALLOCATED( CLAUSE%SOLID ))         DEALLOCATE( CLAUSE%SOLID )
      IF(ALLOCATED( CLAUSE%QUAD ))          DEALLOCATE( CLAUSE%QUAD )
      IF(ALLOCATED( CLAUSE%SH4N ))          DEALLOCATE( CLAUSE%SH4N )
      IF(ALLOCATED( CLAUSE%SH3N ))          DEALLOCATE( CLAUSE%SH3N )
      IF(ALLOCATED( CLAUSE%TRIA ))          DEALLOCATE( CLAUSE%TRIA )
      IF(ALLOCATED( CLAUSE%BEAM ))          DEALLOCATE( CLAUSE%BEAM )
      IF(ALLOCATED( CLAUSE%TRUSS ))         DEALLOCATE( CLAUSE%TRUSS )
      IF(ALLOCATED( CLAUSE%SPRING ))        DEALLOCATE( CLAUSE%SPRING )
      IF(ALLOCATED( CLAUSE%SURF_NODES ))    DEALLOCATE( CLAUSE%SURF_NODES )
      IF(ALLOCATED( CLAUSE%SURF_ELTYP ))    DEALLOCATE( CLAUSE%SURF_ELTYP )
      IF(ALLOCATED( CLAUSE%SURF_ELEM ))     DEALLOCATE( CLAUSE%SURF_ELEM )
      IF(ALLOCATED( CLAUSE%LINE_NODES ))    DEALLOCATE( CLAUSE%LINE_NODES )
      IF(ALLOCATED( CLAUSE%LINE_ELTYP ))    DEALLOCATE( CLAUSE%LINE_ELTYP )
      IF(ALLOCATED( CLAUSE%LINE_ELEM ))     DEALLOCATE( CLAUSE%LINE_ELEM )
      IF(ALLOCATED( CLAUSE%RBODY ))         DEALLOCATE( CLAUSE%RBODY )
C-----
      RETURN
      END
