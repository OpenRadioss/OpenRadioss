Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    group_ini         ../engine/source/groups/group_ini.F
!||--- called by ------------------------------------------------------
!||    rdresb            ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    alloc_group_str   ../engine/source/groups/alloc_group_str.F
!||--- uses       -----------------------------------------------------
!||    group_mod         ../engine/share/modules/group_mod.F
!||    groupdef_mod      ../common_source/modules/groupdef_mod.F
!||    message_mod       ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE GROUP_INI
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE GROUP_MOD
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
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
!!     IGRNOD,IGRBRICK,IGRQUAD,...
!
!  --- READ IGRNOD ---
      IF (NGRNOD > 0)THEN 
        ALLOCATE(IGRNOD(NGRNOD))
        CALL ALLOC_GROUP_STR(IGRNOD,NGRNOD,LENIGRNOD)
      ELSE           
        ALLOCATE(IGRNOD(0))
      ENDIF
!  --- READ IGRBRIC ---
      IF (NGRBRIC > 0)THEN 
        ALLOCATE(IGRBRIC(NGRBRIC))
        CALL ALLOC_GROUP_STR(IGRBRIC,NGRBRIC,LENIGRBRIC)
      ELSE           
        ALLOCATE(IGRBRIC(0))
      ENDIF
!  --- READ IGRQUAD ---
      IF (NGRQUAD > 0)THEN 
        ALLOCATE(IGRQUAD(NGRQUAD))
        CALL ALLOC_GROUP_STR(IGRQUAD,NGRQUAD,LENIGRQUAD)
      ELSE           
        ALLOCATE(IGRQUAD(0))
      ENDIF
!  --- READ IGRSH4N ---
      IF (NGRSHEL > 0)THEN 
        ALLOCATE(IGRSH4N(NGRSHEL))
        CALL ALLOC_GROUP_STR(IGRSH4N,NGRSHEL,LENIGRSH4N)
      ELSE           
        ALLOCATE(IGRSH4N(0))
      ENDIF
!  --- READ IGRTRUSS ---
      IF (NGRTRUS > 0)THEN 
        ALLOCATE(IGRTRUSS(NGRTRUS))
        CALL ALLOC_GROUP_STR(IGRTRUSS,NGRTRUS,LENIGRTRUS)
      ELSE           
        ALLOCATE(IGRTRUSS(0))
      ENDIF
!  --- READ IGRBEAM ---
      IF (NGRBEAM > 0)THEN 
        ALLOCATE(IGRBEAM(NGRBEAM))
        CALL ALLOC_GROUP_STR(IGRBEAM,NGRBEAM,LENIGRBEAM)
      ELSE           
        ALLOCATE(IGRBEAM(0))
      ENDIF
!  --- READ IGRSPRING ---
      IF (NGRSPRI > 0)THEN 
        ALLOCATE(IGRSPRING(NGRSPRI))
        CALL ALLOC_GROUP_STR(IGRSPRING,NGRSPRI,LENIGRSPRI)
      ELSE           
        ALLOCATE(IGRSPRING(0))
      ENDIF
!  --- READ IGRSH3N ---
      IF (NGRSH3N > 0)THEN 
        ALLOCATE(IGRSH3N(NGRSH3N))
        CALL ALLOC_GROUP_STR(IGRSH3N,NGRSH3N,LENIGRSH3N)
      ELSE           
        ALLOCATE(IGRSH3N(0))
      ENDIF
!  --- READ IGRPART ---
      IF (NGRPART > 0)THEN 
        ALLOCATE(IGRPART(NGRPART))
        CALL ALLOC_GROUP_STR(IGRPART,NGRPART,LENIGRPART)
      ELSE           
        ALLOCATE(IGRPART(0))
      ENDIF
!---------
      RETURN
      END
