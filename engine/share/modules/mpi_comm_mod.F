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
C generic module intended to pack information needed for asynchronous communications
C where send and received are not done in the same routine
Chd|====================================================================
Chd|  MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|-- called by -----------
Chd|        ALLOCATE_COMM_STRUCT          share/modules/mpi_comm_mod.F  
Chd|        DEALLOCATE_COMM_STRUCT        share/modules/mpi_comm_mod.F  
Chd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25NORMP                      source/interfaces/int25/i25norm.F
Chd|        SPMD_I25FRONT_NOR             source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I25_SLIDE_EXCH           source/mpi/interfaces/spmd_i25slide.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MPI_COMMOD
      TYPE MPI_COMM_STRUCT
         INTEGER,DIMENSION(:,:), POINTER :: SEND_RQ
         INTEGER,DIMENSION(:,:), POINTER :: RECV_RQ
         INTEGER,DIMENSION(:,:), POINTER :: TAG
         INTEGER,DIMENSION(:,:), POINTER :: SIZ
      END TYPE MPI_COMM_STRUCT
      TYPE MPI_COMM_NOR_STRUCT
         INTEGER, DIMENSION(:), POINTER :: SEND_RQ
         INTEGER, DIMENSION(:), POINTER :: RECV_RQ
         INTEGER, DIMENSION(:), POINTER :: ISINDEX,IRINDEX
         INTEGER, DIMENSION(:), POINTER :: IAD_RECV,IAD_SEND
         INTEGER :: NBIRECV,NBISEND
         REAL(kind=4) , DIMENSION(:) , POINTER :: SEND_BUF, RECV_BUF
      END TYPE MPI_COMM_NOR_STRUCT

      END MODULE MPI_COMMOD 

Chd|====================================================================
Chd|  ALLOCATE_COMM_STRUCT          share/modules/mpi_comm_mod.F  
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|====================================================================
      SUBROUTINE ALLOCATE_COMM_STRUCT(OBJECT,S1,S2)
        USE MPI_COMMOD

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
        TYPE(MPI_COMM_STRUCT) :: OBJECT
        INTEGER, INTENT(IN) :: S1,S2
        ALLOCATE(OBJECT%SEND_RQ(S1,S2)) 
        ALLOCATE(OBJECT%RECV_RQ(S1,S2)) 
        ALLOCATE(OBJECT%TAG    (S1,S2))
        ALLOCATE(OBJECT%SIZ    (S1,S2))     
      END SUBROUTINE
Chd|====================================================================
Chd|  DEALLOCATE_COMM_STRUCT        share/modules/mpi_comm_mod.F  
Chd|-- called by -----------
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|-- calls ---------------
Chd|        MPI_COMMOD                    share/modules/mpi_comm_mod.F  
Chd|====================================================================
      SUBROUTINE DEALLOCATE_COMM_STRUCT(OBJECT,S1,S2)
       USE MPI_COMMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
        TYPE(MPI_COMM_STRUCT) :: OBJECT
        INTEGER, INTENT(IN) :: S1,S2
        DEALLOCATE(OBJECT%SEND_RQ) 
        DEALLOCATE(OBJECT%RECV_RQ) 
        DEALLOCATE(OBJECT%TAG    )
        DEALLOCATE(OBJECT%SIZ    )     
      END SUBROUTINE


