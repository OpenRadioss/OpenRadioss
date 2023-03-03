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
Chd|  LINE_BUFFER                   source/model/sets/line_buffer.F
Chd|-- called by -----------
Chd|        CREATE_LINE_FROM_ELEMENT      source/model/sets/create_line_from_element.F
Chd|-- calls ---------------
Chd|        ELEM_1D_LINE_BUFFER           source/model/sets/elem_1D_line_buffer.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE LINE_BUFFER(
     .                   IXT      ,IXP    ,IXR  ,BUFTMPLINE ,  NSEG,
     .                   IAD_LINE ,CLAUSE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),BUFTMPLINE(*)
      INTEGER NSEG,IAD_LINE
!
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C=======================================================================
!------------------
!             counting / buffering CLAUSE line segments from 1D elems
!---
!
!     ---  truss ---
      IF ( CLAUSE%NB_TRUSS > 0 ) CALL ELEM_1D_LINE_BUFFER(
     .                             IXT     ,NIXT      ,2         ,3      ,4,
     .                             NSEG    ,IAD_LINE  ,BUFTMPLINE,CLAUSE)

!     ---  beam ---
      IF ( CLAUSE%NB_BEAM > 0 ) CALL ELEM_1D_LINE_BUFFER(
     .                             IXP     ,NIXP      ,2         ,3     ,5,
     .                             NSEG    ,IAD_LINE  ,BUFTMPLINE,CLAUSE)

!     ---  spring ---
      IF ( CLAUSE%NB_SPRING > 0 ) CALL ELEM_1D_LINE_BUFFER(
     .                             IXR     ,NIXR      ,2         ,3      ,6,
     .                             NSEG    ,IAD_LINE  ,BUFTMPLINE,CLAUSE)
!---
      RETURN
      END
