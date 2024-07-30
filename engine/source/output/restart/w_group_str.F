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
      !||====================================================================
      !||    w_group_str    ../engine/source/output/restart/w_group_str.F
      !||--- called by ------------------------------------------------------
      !||    wrrestp        ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    w_gr_entity    ../engine/source/output/restart/w_gr_entity.F
      !||--- uses       -----------------------------------------------------
      !||    group_mod      ../engine/share/modules/group_mod.F
      !||    groupdef_mod   ../common_source/modules/groupdef_mod.F
      !||====================================================================
      SUBROUTINE W_GROUP_STR
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
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
!  --- WRITE IGRNOD ---
      CALL W_GR_ENTITY(IGRNOD,NGRNOD,LENIGRNOD)
!  --- WRITE IGRBRIC ---
      CALL W_GR_ENTITY(IGRBRIC,NGRBRIC,LENIGRBRIC)
!  --- WRITE IGRQUAD ---
      CALL W_GR_ENTITY(IGRQUAD,NGRQUAD,LENIGRQUAD)
!  --- WRITE IGRSH4N ---
      CALL W_GR_ENTITY(IGRSH4N,NGRSHEL,LENIGRSH4N)
!  --- WRITE IGRTRUSS ---
      CALL W_GR_ENTITY(IGRTRUSS,NGRTRUS,LENIGRTRUS)
!  --- WRITE IGRBEAM ---
      CALL W_GR_ENTITY(IGRBEAM,NGRBEAM,LENIGRBEAM)
!  --- WRITE IGRSPRING ---
      CALL W_GR_ENTITY(IGRSPRING,NGRSPRI,LENIGRSPRI)
!  --- WRITE IGRSH3N ---
      CALL W_GR_ENTITY(IGRSH3N,NGRSH3N,LENIGRSH3N)
!  --- WRITE IGRPART ---
      CALL W_GR_ENTITY(IGRPART,NGRPART,LENIGRPART)
!---------
      RETURN
      END
