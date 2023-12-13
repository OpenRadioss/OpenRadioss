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
Chd|  WRITE_MOD                     comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      MODULE WRITE_MOD
#include "my_real.inc"
      CONTAINS
!! \brief Wrapper routine to WRITE_I_C, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_INTEGER                 comm/write_mod.F              
Chd|-- called by -----------
Chd|        WRITE_SENSORS                 starter/source/tools/sensor/write_sensors.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     tools/input_output/write_routtines.c
Chd|====================================================================
        SUBROUTINE WRITE_INTEGER(A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: A !< value to be written in a file
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ONE
C-----------------------------------------------
        ONE = 1
        CALL WRITE_I_C(A,ONE)
        END SUBROUTINE
Chd|====================================================================
Chd|  WRITE_INTEGER_1D              comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_I_C                     tools/input_output/write_routtines.c
Chd|====================================================================
        SUBROUTINE WRITE_INTEGER_1D(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: A(N) !< value to be written in a file
        INTEGER, INTENT(IN) :: N !< Dimension1
        CALL WRITE_I_C(A,N)
        END SUBROUTINE
Chd|====================================================================
Chd|  WRITE_INTEGER_2D              comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_I_C                     tools/input_output/write_routtines.c
Chd|====================================================================
        SUBROUTINE WRITE_INTEGER_2D(A,N,M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: A(N,M) !< value to be written in a file
        INTEGER, INTENT(IN) :: N,M !< Dimensions
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = M * N
        CALL WRITE_I_C(A,D)
        END SUBROUTINE
Chd|====================================================================
Chd|  WRITE_INTEGER_3D              comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_I_C                     tools/input_output/write_routtines.c
Chd|====================================================================
        SUBROUTINE WRITE_INTEGER_3D(A,L,N,M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: A(L,N,M) !< value to be written in a file
        INTEGER, INTENT(IN) :: L,N,M !< Dimensions
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = L * M * N
        CALL WRITE_I_C(A,D)
        END SUBROUTINE

!! \brief Wrapper routine to WRITE_DB, to be used in order to avoid dummy-argument mismatch at error at compile tile

!! \brief Wrapper routine to WRITE_DB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_REAL                    comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_REAL(A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real , INTENT(IN) :: A  !< value to be written
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ONE
C-----------------------------------------------
        ONE = 1
        CALL WRITE_DB(A,ONE)
        END SUBROUTINE
!! \brief Wrapper routine to WRITE_DB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_REAL_1D                 comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_REAL_1D(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real , INTENT(IN) :: A(N)  !< value to be written
        INTEGER , INTENT(IN) :: N  !<  size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = N
        CALL WRITE_DB(A,D)
        END SUBROUTINE
!! \brief Wrapper routine to WRITE_DB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_REAL_2D                 comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_REAL_2D(A,M,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real , INTENT(IN) :: A(M,N)  !< value to be written
        INTEGER, INTENT(IN) :: N,M  !<  size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = M * N
        CALL WRITE_DB(A,D)
        END SUBROUTINE
!! \brief Wrapper routine to WRITE_DB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_REAL_3D                 comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_REAL_3D(A,L,M,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real , INTENT(IN) :: A(L,M,N)  !< value to be written
        INTEGER , INTENT(IN) :: L,N,M  !<  size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = L * M * N
        CALL WRITE_DB(A,D)
        END SUBROUTINE
!! \brief Wrapper routine to WRITE_DPDB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_DOUBLE                  comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DPDB                    starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DPDB                    engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_DOUBLE(A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        DOUBLE PRECISION , INTENT(IN) :: A  !< value to be written
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: ONE
C-----------------------------------------------
        ONE = 1
        CALL WRITE_DPDB(A,ONE)
        END SUBROUTINE

!! \brief Wrapper routine to WRITE_DPDB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_DOUBLE_1D               comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DPDB                    starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DPDB                    engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_DOUBLE_1D(A,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        DOUBLE PRECISION , INTENT(IN) :: A(N)  !< value to be written
        INTEGER, INTENT(IN) :: N  !<  size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = N
        CALL WRITE_DPDB(A,D)
        END SUBROUTINE
!! \brief Wrapper routine to WRITE_DPDB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_DOUBLE_2D               comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DPDB                    starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DPDB                    engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_DOUBLE_2D(A,M,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        DOUBLE PRECISION , INTENT(IN) :: A(M,N)  !< value to be written
        INTEGER, INTENT(IN) :: N,M  !<  size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = M * N
        CALL WRITE_DPDB(A,D)
        END SUBROUTINE
!! \brief Wrapper routine to WRITE_DPDB, to be used in order to avoid dummy-argument mismatch at error at compile tile
Chd|====================================================================
Chd|  WRITE_DOUBLE_3D               comm/write_mod.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_DPDB                    starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DPDB                    engine/source/output/tools/write_db.F
Chd|====================================================================
        SUBROUTINE WRITE_DOUBLE_3D(A,L,M,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        DOUBLE PRECISION , INTENT(IN) :: A(L,M,N)  !< value to be written
        INTEGER , INTENT(IN) :: L,N,M  !<  size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: D
C-----------------------------------------------
        D = L * M * N
        CALL WRITE_DPDB(A,D)
        END SUBROUTINE
      END MODULE
