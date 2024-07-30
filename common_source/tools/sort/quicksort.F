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
      !||    quicksort                 ../common_source/tools/sort/quicksort.F
      !||--- called by ------------------------------------------------------
      !||    fvmesh1                   ../engine/source/airbag/fvmesh.F
      !||    stat_inimap1d_file_spmd   ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap1d_spmd        ../engine/source/output/sta/stat_inimap1d_spmd.F
      !||    stat_inimap2d_file_spmd   ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd        ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      RECURSIVE SUBROUTINE QUICKSORT(A, IDX, FIRST, LAST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: FIRST, LAST
      INTEGER, INTENT(INOUT) :: IDX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X, T
      INTEGER :: I, J, I1, I2
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(FIRST>LAST)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      X = A( (FIRST + LAST) / 2 )
      I = FIRST
      J = LAST
      DO
         DO WHILE (A(I) < X)
            I = I + 1
         ENDDO
         DO WHILE(X < A(J))
            J = J - 1
         ENDDO
         IF (I >= J) EXIT
         T = A(I)
         A(I) = A(J)
         A(J) = T
         I1 = IDX(i) 
         IDX(I) = IDX(J) 
         IDX(J) = I1
         I = I + 1
         J = J - 1
      ENDDO
      IF (FIRST < I - 1) CALL QUICKSORT(A, IDX, FIRST, I - 1)
      IF (J + 1 < LAST)  CALL QUICKSORT(A, IDX, J + 1, LAST)
      END SUBROUTINE 
C________________________________________________________________________________________________________

      !||====================================================================
      !||    quicksort_i                      ../common_source/tools/sort/quicksort.F
      !||--- called by ------------------------------------------------------
      !||    ale_compute_connectivity         ../common_source/modules/ale/ale_connectivity_mod.F
      !||    ini_inimap1d                     ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    monvol_check_delete_duplicated   ../starter/source/airbag/monvol_check_delete_duplicated.F
      !||    sensor_init                      ../engine/source/tools/sensor/sensor_init.F
      !||    st_qaprint_model_tools           ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      RECURSIVE SUBROUTINE QUICKSORT_I(A, FIRST, LAST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: FIRST, LAST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .     X, T
      INTEGER :: I, J, I1, I2
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(FIRST>LAST)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      X = A( (FIRST + LAST) / 2 )
      I = FIRST
      J = LAST
      DO
         DO WHILE (A(I) < X)
            I = I + 1
         ENDDO
         DO WHILE(X < A(J))
            J = J - 1
         ENDDO
         IF (I >= J) EXIT
         T = A(I)
         A(I) = A(J)
         A(J) = T
         I = I + 1
         J = J - 1
      ENDDO
      IF (FIRST < I - 1) CALL QUICKSORT_I(A, FIRST, I - 1)
      IF (J + 1 < LAST)  CALL QUICKSORT_I(A, J + 1, LAST)
      END SUBROUTINE 
C________________________________________________________________________________________________________

      !||====================================================================
      !||    quicksort_i2                    ../common_source/tools/sort/quicksort.F
      !||--- called by ------------------------------------------------------
      !||    ale_compute_connectivity        ../common_source/modules/ale/ale_connectivity_mod.F
      !||    nloc_dmg_init                   ../starter/source/materials/fail/nloc_dmg_init.F
      !||    spmd_ne_connect                 ../starter/source/ale/spmd_ne_connect.F
      !||    st_qaprint_constraints          ../starter/source/output/qaprint/st_qaprint_constraints.F
      !||    st_qaprint_general_controls     ../starter/source/output/qaprint/st_qaprint_general_controls.F
      !||    st_qaprint_initial_conditions   ../starter/source/output/qaprint/st_qaprint_initial_conditions.F
      !||    st_qaprint_loads                ../starter/source/output/qaprint/st_qaprint_loads.F
      !||    st_qaprint_model_tools          ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||    st_qaprint_monvol               ../starter/source/output/qaprint/st_qaprint_monvol.F
      !||    st_qaprint_thgrou               ../starter/source/output/qaprint/st_qaprint_time_histories.F
      !||    stat_inimap2d_file_spmd         ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd              ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      RECURSIVE SUBROUTINE QUICKSORT_I2(A, IDX, FIRST, LAST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: FIRST, LAST
      INTEGER, INTENT(INOUT) :: IDX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: X, T
      INTEGER :: I, J, I1, I2
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(FIRST>LAST)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      X = A( (FIRST + LAST) / 2 )
      I = FIRST
      J = LAST
      DO
         DO WHILE (A(I) < X)
            I = I + 1
         ENDDO
         DO WHILE(X < A(J))
            J = J - 1
         ENDDO
         IF (I >= J) EXIT
         T = A(I)
         A(I) = A(J)
         A(J) = T
         I1 = IDX(i) 
         IDX(I) = IDX(J) 
         IDX(J) = I1
         I = I + 1
         J = J - 1
      ENDDO
      IF (FIRST < I - 1) CALL QUICKSORT_I2(A, IDX, FIRST, I - 1)
      IF (J + 1 < LAST)  CALL QUICKSORT_I2(A, IDX, J + 1, LAST)
      END SUBROUTINE 
C________________________________________________________________________________________________________
      
      !||====================================================================
      !||    quicksort_integer_2arrays   ../common_source/tools/sort/quicksort.F
      !||--- called by ------------------------------------------------------
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      RECURSIVE SUBROUTINE QUICKSORT_INTEGER_2ARRAYS(A, B, FIRST, LAST)
      ! SORT columns A,B  (key=A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: A(*),B(*)
      INTEGER, INTENT(IN) :: FIRST, LAST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .     X, T
      INTEGER :: I, J, I1, I2
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(FIRST>LAST)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      X = A( (FIRST + LAST) / 2 )
      I = FIRST
      J = LAST
      DO
         DO WHILE (A(I) < X)
            I = I + 1
         ENDDO
         DO WHILE(X < A(J))
            J = J - 1
         ENDDO
         IF (I >= J) EXIT
         T = A(I)
         A(I) = A(J)
         A(J) = T
         !
         T = B(I)
         B(I) = B(J)
         B(J) = T   
         !      
         I = I + 1
         J = J - 1
      ENDDO
      IF (FIRST < I - 1) CALL QUICKSORT_INTEGER_2ARRAYS(A,B, FIRST, I - 1)
      IF (J + 1 < LAST)  CALL QUICKSORT_INTEGER_2ARRAYS(A,B, J + 1, LAST)
      END SUBROUTINE        
C________________________________________________________________________________________________________
      
      !||====================================================================
      !||    quicksort_integer_3arrays   ../common_source/tools/sort/quicksort.F
      !||--- called by ------------------------------------------------------
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      RECURSIVE SUBROUTINE QUICKSORT_INTEGER_3ARRAYS(A, B, C, FIRST, LAST)
      ! SORT columns A,B,C  (key=A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: A(*),B(*),C(*)
      INTEGER, INTENT(IN) :: FIRST, LAST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .     X, T
      INTEGER :: I, J, I1, I2
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(FIRST>LAST)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      X = A( (FIRST + LAST) / 2 )
      I = FIRST
      J = LAST
      DO
         DO WHILE (A(I) < X)
            I = I + 1
         ENDDO
         DO WHILE(X < A(J))
            J = J - 1
         ENDDO
         IF (I >= J) EXIT
         T = A(I)
         A(I) = A(J)
         A(J) = T
         !
         T = B(I)
         B(I) = B(J)
         B(J) = T  
         !
         T = C(I)
         C(I) = C(J)
         C(J) = T            
         !      
         I = I + 1
         J = J - 1
      ENDDO
      IF (FIRST < I - 1) CALL QUICKSORT_INTEGER_3ARRAYS(A,B,C, FIRST, I - 1)
      IF (J + 1 < LAST)  CALL QUICKSORT_INTEGER_3ARRAYS(A,B,C, J + 1, LAST)
      END SUBROUTINE         
C________________________________________________________________________________________________________
