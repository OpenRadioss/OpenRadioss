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
!||    spmd_get_inacti_global     ../engine/source/mpi/interfaces/spmd_get_inacti_global.F
!||--- called by ------------------------------------------------------
!||    inttri                     ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    spmd_iallreduce_int_comm   ../engine/source/mpi/generic/spmd_iallreduce_int_comm.F
!||--- uses       -----------------------------------------------------
!||    inter_sorting_mod          ../engine/share/modules/inter_sorting_mod.F
!||    inter_struct_mod           ../engine/share/modules/inter_struct_mod.F
!||====================================================================
        SUBROUTINE SPMD_GET_INACTI_GLOBAL(IPARI,NB_INTER_SORTED,LIST_INTER_SORTED,INTER_STRUCT)

        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER,DIMENSION(NPARI,NINTER), INTENT(inout) :: IPARI
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
        INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI 
        INTEGER :: I,N,N_BUFFER
        INTEGER, DIMENSION(NB_INTER_7_INACTI) :: S_BUFFER
        INTEGER, DIMENSION(NB_INTER_7_INACTI) :: R_BUFFER
        CHARACTER(len=4) :: MY_OPERATION
        INTEGER :: CODE
        INTEGER :: STATUT(MPI_STATUS_SIZE)
C-----------------------------------------------
        IF(NB_INTER_7_INACTI>0) THEN
            S_BUFFER(1:NB_INTER_7_INACTI) = 1
            R_BUFFER(1:NB_INTER_7_INACTI) = 0
        ENDIF
        DO I=1,NB_INTER_SORTED
            N = LIST_INTER_SORTED(I)
            N_BUFFER = LIST_INTER_7_INACTI(N)
            IF(N_BUFFER>0) THEN
                IF(INTER_STRUCT(N)%INACTI<0) S_BUFFER( N_BUFFER ) = 0
            ENDIF
        ENDDO
        
        MY_OPERATION(1:4) =''
        MY_OPERATION ="SUM"

        CALL SPMD_IALLREDUCE_INT_COMM(S_BUFFER,R_BUFFER,NB_INTER_7_INACTI,
     .                                MY_OPERATION,COMM_INACTI,REQUEST_INACTI)

        CALL MPI_WAIT(REQUEST_INACTI,STATUT,CODE)

        IF(NB_INTER_SORTED>0) THEN
            DO I=1,NB_INTER_SORTED
                N = LIST_INTER_SORTED(I)
                N_BUFFER = LIST_INTER_7_INACTI(N)
                IF(N_BUFFER>0) THEN
                    IF (R_BUFFER(N_BUFFER)/=0) THEN 
                        INTER_STRUCT(N)%INACTI=IPARI(22,N)
                    ELSE
                        INTER_STRUCT(N)%INACTI=-ABS(IPARI(22,N))
                    END IF
                    IPARI(22,N) = INTER_STRUCT(N)%INACTI
                ENDIF
            ENDDO
        ENDIF
C
#endif
        RETURN
        END SUBROUTINE SPMD_GET_INACTI_GLOBAL
