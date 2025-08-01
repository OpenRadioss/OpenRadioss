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
!||    utable_dum   ../engine/source/user_interface/utable.F
!||--- called by ------------------------------------------------------
!||    radioss2     ../engine/source/engine/radioss2.F
!||====================================================================
      SUBROUTINE UTABLE_DUM(IERR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
       INTEGER IERR
       IERR=0
      END
!||====================================================================
!||    get_u_numtable   ../engine/source/user_interface/utable.F
!||--- uses       -----------------------------------------------------
!||    table_glob_mod   ../engine/share/modules/table_glob_mod.F
!||    table_mod        ../engine/share/modules/table_mod.F
!||====================================================================
      INTEGER FUNCTION GET_U_NUMTABLE(TID) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_GLOB_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------      
#include      "implicit_f.inc"
C-----------------------------------------------
#include      "com04_c.inc"     
C----------------------------------------------- 
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
!||    get_u_table           ../engine/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    table_interp          ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_glob_mod        ../engine/share/modules/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_U_TABLE(ITABLE,XX,YY) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
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
!||    get_table_value       ../engine/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    table_interp          ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_glob_mod        ../engine/share/modules/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_TABLE_VALUE(ITABLE,XX,XXDIM,YY) 
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
       INTEGER ITABLE,XXDIM
       my_real XX(XXDIM)

       my_real
     .       YY
       my_real, DIMENSION(:),ALLOCATABLE :: XX2

      ALLOCATE (XX2(XXDIM))
      XX2(1:XXDIM)=XX(1:XXDIM)
C
      CALL TABLE_INTERP(TABLE(ITABLE),XX2,YY)
      
      DEALLOCATE(XX2)
      RETURN
      END
!||====================================================================
!||    get_table_value_dydx   ../engine/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    my_flush               ../engine/source/system/machine.F
!||    table_interp_dydx      ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod    ../engine/share/modules/table_mod.F
!||    table_glob_mod         ../engine/share/modules/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_TABLE_VALUE_DYDX(ITABLE,XX,XXDIM,YY,DYDX) 
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
       INTEGER ITABLE,XXDIM
       my_real XX(XXDIM)

       my_real
     .       YY,DYDX
       my_real, DIMENSION(:),ALLOCATABLE :: XX2

      ALLOCATE (XX2(XXDIM))
      XX2(1:XXDIM)=XX(1:XXDIM)
C
      call my_flush(6)
      CALL TABLE_INTERP_DYDX(TABLE(ITABLE),XX2,XXDIM,YY,DYDX)
      
      DEALLOCATE(XX2)
      RETURN
      END
!||====================================================================
!||    get_u_vtable          ../engine/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_glob_mod        ../engine/share/modules/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_U_VTABLE(ITABLE,NEL0,IPOS,XX,YY,DYDX1) 
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
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

!||====================================================================
!||    get_vtable_value      ../engine/source/user_interface/utable.F
!||--- calls      -----------------------------------------------------
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    table_glob_mod        ../engine/share/modules/table_glob_mod.F
!||====================================================================
      SUBROUTINE GET_VTABLE_VALUE(ITABLE,NEL0,IPOS,XX,XXDIM,YY,DYDX1) 
      USE TABLE_GLOB_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABLE,NEL0,XXDIM
      INTEGER IPOS(NEL0,XXDIM)
      my_real  XX(NEL0,XXDIM)
      my_real  YY(*), DYDX1(*)
     
      my_real, DIMENSION(:,:), ALLOCATABLE :: XX2 
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IPOS2

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      ALLOCATE (XX2(NEL0,XXDIM))
      ALLOCATE (IPOS2(NEL0,XXDIM))

      IPOS2(1:NEL0,1:XXDIM) = IPOS(1:NEL0,1:XXDIM)
      XX2(1:NEL0,1:XXDIM) = XX(1:NEL0,1:XXDIM)

      CALL TABLE_VINTERP(TABLE(ITABLE),NEL0,NEL0,IPOS2,XX2,YY,DYDX1)

      DEALLOCATE(XX2)
      DEALLOCATE(IPOS2)

      RETURN
      END
