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
Chd|  INTERFACE_UTABLE_MOD          source/user_interface/interface_utable.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTERFACE_UTABLE_MOD
C-----------------------------------------------
#include      "my_real.inc"
      INTERFACE
C
        INTEGER FUNCTION GET_U_NUMTABLE(I)
        INTEGER I
        END FUNCTION GET_U_NUMTABLE
C
        SUBROUTINE GET_U_TABLE(I,XX,YY)
        INTEGER I
        my_real, 
     .         DIMENSION(:) :: XX
        my_real
     .         YY
        END SUBROUTINE GET_U_TABLE
C
        SUBROUTINE GET_U_VTABLE(I,NEL,IPOS,XX,YY,DYDX)
        INTEGER I,NEL
        INTEGER, DIMENSION(:,:) :: IPOS 
        my_real, 
     .         DIMENSION(:,:) :: XX
        my_real
     .         YY(*), DYDX(*)
        END SUBROUTINE GET_U_VTABLE
C
      END INTERFACE
      END MODULE INTERFACE_UTABLE_MOD
