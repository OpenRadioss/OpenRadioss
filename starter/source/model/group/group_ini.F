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
Chd|  GROUP_INI                     source/model/group/group_ini.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GR_ENTITY_INI                 source/model/group/gr_entity_ini.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE GROUP_INI(IGRNOD   ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N,
     .                     IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRPART )
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
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LENGROUP
C-----------------------------------------------
!  --- IGRNOD ---
      LENGROUP = 0
      IF (NGRNOD > 0)  CALL GR_ENTITY_INI(IGRNOD,NGRNOD,LENGROUP)
      LENIGRNOD = LENGROUP
!  --- IGRBRIC ---
      LENGROUP = 0
      IF (NGRBRIC > 0) CALL GR_ENTITY_INI(IGRBRIC,NGRBRIC,LENGROUP)
      LENIGRBRIC = LENGROUP
!  --- IGRQUAD ---
      LENGROUP = 0
      IF (NGRQUAD > 0) CALL GR_ENTITY_INI(IGRQUAD,NGRQUAD,LENGROUP)
      LENIGRQUAD = LENGROUP
!  --- IGRSH4N ---
      LENGROUP = 0
      IF (NGRSHEL > 0) CALL GR_ENTITY_INI(IGRSH4N,NGRSHEL,LENGROUP)
      LENIGRSH4N = LENGROUP
!  --- IGRTRUSS ---
      LENGROUP = 0
      IF (NGRTRUS > 0) CALL GR_ENTITY_INI(IGRTRUSS,NGRTRUS,LENGROUP)
      LENIGRTRUS = LENGROUP
!  --- IGRBEAM ---
      LENGROUP = 0
      IF (NGRBEAM > 0) CALL GR_ENTITY_INI(IGRBEAM,NGRBEAM,LENGROUP)
      LENIGRBEAM = LENGROUP
!  --- IGRSPRING ---
      LENGROUP = 0
      IF (NGRSPRI > 0) CALL GR_ENTITY_INI(IGRSPRING,NGRSPRI,LENGROUP)
      LENIGRSPRI = LENGROUP
!  --- IGRSH3N ---
      LENGROUP = 0
      IF (NGRSH3N > 0) CALL GR_ENTITY_INI(IGRSH3N,NGRSH3N,LENGROUP)
      LENIGRSH3N = LENGROUP
!  --- IGRPART ---
      LENGROUP = 0
      IF (NGRPART > 0) CALL GR_ENTITY_INI(IGRPART,NGRPART,LENGROUP)
      LENIGRPART = LENGROUP
!---------
      RETURN
      END
