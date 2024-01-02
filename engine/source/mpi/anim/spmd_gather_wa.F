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
Chd|  SPMD_GATHER_WA                source/mpi/anim/spmd_gather_wa.F
Chd|-- called by -----------
Chd|        DPARRWS                       source/output/anim/generate/dparrws.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_GATHER_WA(MODE,SIZE_BUFFER_S,SIZE_BUFFER_R,SINDEX,RINDEX_PROC,
     1                           BUFFER_S,BUFFER_R,SHIFT_R,NB_ELEM_R)
C-----------------------------------------------    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
!       MODE = 0 --> collect the index array for each proc   
!       MODE = 1 --> collect all the buffer arraies
!       SIZE_BUFFER_S is the size of the local sended array
        INTEGER, INTENT(IN) :: MODE,SIZE_BUFFER_S
!       SIZE_BUFFER_R is the size of the global received array, proc 0 
        INTEGER, INTENT(INOUT) :: SIZE_BUFFER_R
!       SINDEX : index buffer array, local on each proc                      
        INTEGER, DIMENSION(NSECT+1,3), INTENT(IN) :: SINDEX
!       RINDEX_PROC : global index buffer array, only on proc 0
        INTEGER, DIMENSION(NSECT+1,3,NSPMD), INTENT(INOUT) :: RINDEX_PROC
!       BUFFER_S : sended buffer
        INTEGER, DIMENSION(SIZE_BUFFER_S), INTENT(IN) :: BUFFER_S
!       BUFFER_R : received buffer, size = SIZE_BUFFER_R on proc 0
        INTEGER, DIMENSION(*), INTENT(INOUT) :: BUFFER_R
        INTEGER, DIMENSION(NSPMD), INTENT(INOUT) :: SHIFT_R,NB_ELEM_R
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,STEP

C-----------------------------------------------
      DATA MSGOFF/7069/
C-----------------------------------------------
        
        IF(MODE==0) THEN
        !       collect the local index arraies on the main processor
                CALL MPI_GATHER(SINDEX,3*(NSECT+1),MPI_INTEGER,
     .                   RINDEX_PROC,3*(NSECT+1),MPI_INTEGER,0,MPI_COMM_WORLD,ierror)  
        !       sum all the size of local sended arraies in order to get the size 
        !       of the global array on proc 0                  
                IF(ISPMD==0) THEN
                        NB_ELEM_R(1:NSPMD) = RINDEX_PROC(NSECT+1,1,1:NSPMD)
                        SIZE_BUFFER_R = 0
                        DO I=1,NSPMD
                                SIZE_BUFFER_R = SIZE_BUFFER_R + NB_ELEM_R(I)
                        ENDDO
        !       shift for the _gatherv comm
                        SHIFT_R(1) = 0
                        DO I=2,NSPMD
                                SHIFT_R(I) = SHIFT_R(I-1)+NB_ELEM_R(I-1)
                        ENDDO
                ELSE
                        SIZE_BUFFER_R = 0
                ENDIF
        ELSEIF(MODE==1) THEN

                CALL MPI_GATHERV(BUFFER_S,SIZE_BUFFER_S,MPI_INTEGER,
     .                           BUFFER_R,NB_ELEM_R,SHIFT_R,MPI_INTEGER,
     .                           0,MPI_COMM_WORLD,ierror)   
        ENDIF
#endif
       RETURN
       END
