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
Chd|  CHECKSUM_MOD                  source/mpi/output/node_checksum.F
Chd|-- called by -----------
Chd|        SPMD_FLUSH_ACCEL              source/mpi/output/spmd_flush_accel.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE CHECKSUM_MOD
        INTEGER, PARAMETER :: ROOT = 65521
        INTEGER, PARAMETER :: TWO_POWER_16 = 65536

        interface
        subroutine adler32(data, len, checksum) bind(C, name="cpp_adler32")
          use iso_c_binding
          type(c_ptr), value :: data
          integer(c_size_t), value :: len
          integer(c_int) :: checksum
        end subroutine adler32
        end interface
      CONTAINS
!! \brief Returns Adler32 checksum of A(1:SIZ2,1:SIZ1)
        FUNCTION  DOUBLE_ARRAY_CHECKSUM(A,SIZ1,SIZ2) RESULT(CHECKSUM)
           USE ISO_C_BINDING
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
          IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          INTEGER, INTENT(IN) :: SIZ1,SIZ2 !< sizes
          DOUBLE PRECISION, TARGET, INTENT(IN) :: A(SIZ2,SIZ1) !< 2D array of real values
          INTEGER :: CHECKSUM !< return value, Adler 32 checksum of A
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
          integer(c_size_t) :: len
C-----------------------------------------------
          len = SIZ2*SIZ1
          len = len * sizeof(A(1,1))
          call adler32(c_loc(A), len, checksum)
        END
      END MODULE CHECKSUM_MOD

