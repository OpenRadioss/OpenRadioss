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
C
Chd|====================================================================
Chd|  SPMD_PRINTCPUINFO             source/mpi/output/spmd_printcpuinfo.F
Chd|-- called by -----------
Chd|        PRINTCPU                      source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_PRINTCPUINFO(HOSTNAME,LENH,CPUNAM,LENC,FREQUENCE,
     *                             MEM,SWAP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*256 HOSTNAME,CPUNAM
      INTEGER LENH, LENC, FREQUENCE,MEM,SWAP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGTYP,
     .    MSGOFF,MSGOFF2,MSGOFF3,I
      DATA MSGOFF/190/
      DATA MSGOFF2/191/
      DATA MSGOFF3/192/
      CHARACTER*256 CBUF1,CBUF2
      INTEGER LENR(5)

      IF (ISPMD.EQ.0) THEN
        DO I=2,NSPMD
          MSGTYP = MSGOFF
          CALL MPI_RECV(
     .      LENR,5,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,STATUS,IERROR)

          MSGTYP = MSGOFF2
          CALL MPI_RECV(
     .      CBUF1,LENR(1),MPI_CHARACTER,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,STATUS,IERROR)

          MSGTYP = MSGOFF3
          CALL MPI_RECV(
     .      CBUF2,LENR(2),MPI_CHARACTER,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,STATUS,IERROR)

          IF (MEM.GT.-1)THEN
           WRITE(IOUT,'(I4,A,A,A,A,A,I4,A,I6,A,I6,A)') I,'   ',
     .     CBUF1(1:LENR(1)),'   ',CBUF2(1:LENR(2)),', ',LENR(3),
     .     ' MHz, ',LENR(4),' MB RAM, ',LENR(5),' MB swap'
          ELSE
           WRITE(IOUT,'(I4,A,A,A,A,A,I4,A)') I,'   ',
     .     CBUF1(1:LENR(1)),'   ',CBUF2(1:LENR(2)),', ',LENR(3),' MHz '
          ENDIF

        ENDDO
      ELSE
        LENR(1) = LENH
        LENR(2) = LENC
        LENR(3) = FREQUENCE
        LENR(4) = MEM
        LENR(5) = SWAP

        MSGTYP = MSGOFF
        CALL MPI_SEND(LENR,5,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .              MPI_COMM_WORLD,ierror)

        MSGTYP = MSGOFF2
        CALL MPI_SEND(HOSTNAME,LENH,MPI_CHARACTER,IT_SPMD(1),MSGTYP,
     .              MPI_COMM_WORLD,ierror)

        MSGTYP = MSGOFF3
        CALL MPI_SEND(CPUNAM,LENC,MPI_CHARACTER,IT_SPMD(1),MSGTYP,
     .              MPI_COMM_WORLD,ierror)
      ENDIF

#endif
      RETURN
      END
