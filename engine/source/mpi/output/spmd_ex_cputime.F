Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SPMD_EX_CPUTIME               source/mpi/output/spmd_ex_cputime.F
Chd|-- called by -----------
Chd|        CUMULTIME_MP                  source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EX_CPUTIME(DIM_TAB3,TAB3,TAB4)
C     exchange and sum of cputime/mpitask 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIM_TAB3
      my_real, DIMENSION(DIM_TAB3) :: TAB3
      my_real, DIMENSION(NSPMD+1,DIM_TAB3) ::TAB4
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,DIM,SENDER,RECIP,MSGTYP
      INTEGER IERROR,MSGOFF
      my_real, DIMENSION(DIM_TAB3) :: RBUF
      INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS
      DATA MSGOFF/239/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DIM=DIM_TAB3
      IF(ISPMD.EQ.0) THEN
        DO J=1,DIM_TAB3
         TAB4(NSPMD+1,J) = ZERO
         TAB4(1,J) = TAB3(J)
        ENDDO

        DO I = 2, NSPMD
          MSGTYP=MSGOFF
          SENDER = I-1        
          CALL MPI_RECV(RBUF,DIM,REAL,SENDER,MSGTYP,
     .                   MPI_COMM_WORLD,STATUS,IERROR) 
          DO J=1,DIM_TAB3
           TAB4(I,J) = RBUF(J)
          ENDDO
        END DO
      ELSE
        RECIP = 0
        MSGTYP=MSGOFF
        DO J=1,DIM_TAB3
         RBUF(J) = TAB3(J)
        ENDDO
        CALL MPI_SEND(RBUF,DIM,REAL,RECIP,MSGTYP,
     .                   MPI_COMM_WORLD,IERROR)
      ENDIF     
C
#endif
      RETURN
      END
