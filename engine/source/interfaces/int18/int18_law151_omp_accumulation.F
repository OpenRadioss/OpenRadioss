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
!||    int18_law151_omp_accumulation   ../engine/source/interfaces/int18/int18_law151_omp_accumulation.F
!||--- called by ------------------------------------------------------
!||    resol                           ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    message_mod                     ../engine/share/message_module/message_mod.F
!||    multi_fvm_mod                   ../common_source/modules/ale/multi_fvm_mod.F90
!||    tri7box                         ../engine/share/modules/tri7box.F
!||====================================================================
        SUBROUTINE INT18_LAW151_OMP_ACCUMULATION( MULTI_FVM )
!$COMMENT
!       INT18_LAW151_OMP_ACCUMULATION description
!       accumulation of force for remote nodes on the 
!       main OpenMP thread
!       only useful when OMP_NUM_THREADS > 1
!       
!       INT18_LAW151_OMP_ACCUMULATION organization :
!           loop over the NTHREADS OpenMP task + NODFI phantom nodes 
!           & accumulation in the 1rst memory cell
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: J,N,NN
        INTEGER :: NODFI,SHIFT_FORCE_INT
!   -----------------------------------
        IF(NTHREAD > 1) THEN
            DO NN=1,MULTI_FVM%NUMBER_INT18
                N = MULTI_FVM%INT18_LIST(NN)
                NODFI = MULTI_FVM%R_AFI(N)%NODFI
                DO J=2,NTHREAD
                    SHIFT_FORCE_INT = (J-1)*NODFI
                    MULTI_FVM%R_AFI(N)%R_FORCE_INT(1:3,1:6,1:NODFI) = 
     .               MULTI_FVM%R_AFI(N)%R_FORCE_INT(1:3,1:6,1:NODFI) 
     .               + MULTI_FVM%R_AFI(N)%R_FORCE_INT(1:3,1:6,1+SHIFT_FORCE_INT:NODFI+SHIFT_FORCE_INT)
                    MULTI_FVM%R_AFI(N)%R_FORCE_INT(1:3,1:6,1+SHIFT_FORCE_INT:NODFI+SHIFT_FORCE_INT) = 0.d+00                        
                ENDDO
            ENDDO
        ENDIF 

        RETURN
        END SUBROUTINE INT18_LAW151_OMP_ACCUMULATION
