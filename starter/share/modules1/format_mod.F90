!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module format_mod

        ! number of character per field/column
        INTEGER, PARAMETER :: LFIELD = 10

        ! input format
        CHARACTER(LEN=50), PARAMETER :: FMT_I      = "(I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_2I     = "(2I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_3I     = "(3I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_5I     = "(5I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_6I     = "(6I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_8I     = "(8I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_10I    = "(10I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_I_2F   = "(I10,2F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_I_3F   = "(I10,3F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_F      = "(F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_2F     = "(2F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_3F     = "(3F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_5F     = "(5F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_A      = "(A10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_A_I    = "(A10,I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_A_I_F  = "(A10,I10,F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_THGR   = "(2I10,A80)"

        ! output format
        CHARACTER(LEN=50), PARAMETER :: FMW_I      = "(I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_2I     = "(2I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_4I     = "(4I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_5I     = "(5I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_6I     = "(6I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_7I     = "(7I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_10I    = "(10I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_I_A    = "(I10,A)"
        CHARACTER(LEN=50), PARAMETER :: FMW_A_I    = "(A,I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_A_I_A  = "(A,I10,A)"
        CHARACTER(LEN=50), PARAMETER :: FMW_I_3F   = "(I10,1P3G20.13)"
        CHARACTER(LEN=50), PARAMETER :: FMW_5I_F   = "(5I10,1PG20.13)"
        CHARACTER(LEN=50), PARAMETER :: FMW_7I_2F  = "(7I10,1P2G20.13)"
        CHARACTER(LEN=50), PARAMETER :: FMW_2I_X_F = "(2I10,10X,1PG20.13)"

      contains

      end module format_mod
