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
!||    st_utable_dum   ../starter/source/user_interface/utable.F
!||--- called by ------------------------------------------------------
!||    starter0        ../starter/source/starter/starter0.F
!||====================================================================
      SUBROUTINE ST_UTABLE_DUM(IERR)
#include      "implicit_f.inc"
      INTEGER IERR
      IERR=0
      END      
!||====================================================================
!||    get_u_numtable   ../starter/source/user_interface/utable.F
!||--- uses       -----------------------------------------------------
!||    table_glob_mod   ../starter/share/modules1/table_glob_mod.F
!||    table_mod        ../starter/share/modules1/table_mod.F
!||====================================================================
      INTEGER FUNCTION GET_U_NUMTABLE(TID) 
      USE TABLE_GLOB_MOD
      USE TABLE_MOD
#include      "implicit_f.inc"
#include      "com04_c.inc"         
      TYPE(TTABLE) CUR_TABLE
      INTEGER I,TID
C need NTABLE
C
      GET_U_NUMTABLE = 0
      DO I=1,NTABLE
        IF(TABLE(I)%NOTABLE==TID)THEN
          GET_U_NUMTABLE = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
!||====================================================================
!||    get_u_table      ../starter/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    table_interp     ../starter/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    table_glob_mod   ../starter/share/modules1/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_U_TABLE(ITABLE,XX,YY) 
      USE TABLE_GLOB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      INTEGER ITABLE
      my_real, 
     .       DIMENSION(:) :: XX
      my_real
     .       YY
C
      CALL TABLE_INTERP(TABLE(ITABLE),XX,YY)
      RETURN
      END
!||====================================================================
!||    get_u_vtable     ../starter/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    table_vinterp    ../starter/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    table_glob_mod   ../starter/share/modules1/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_U_VTABLE(ITABLE,NEL0,IPOS,XX,YY,DYDX1) 
      USE TABLE_GLOB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABLE,NEL0
      INTEGER, DIMENSION(:,:) :: IPOS
      my_real, 
     .       DIMENSION(:,:) :: XX
      my_real
     .       YY(*), DYDX1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      CALL TABLE_VINTERP(TABLE(ITABLE),NEL0,NEL0,IPOS,XX,YY,DYDX1)
      RETURN
      END
