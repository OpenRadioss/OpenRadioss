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
Chd|  SPMD_I7CURVCOM                source/mpi/interfaces/spmd_i7curvcom.F
Chd|-- called by -----------
Chd|        I20NORMNP                     source/interfaces/int20/i20rcurv.F
Chd|        I20NORMP                      source/interfaces/int20/i20curv.F
Chd|        I20NORMSP                     source/interfaces/int20/i20curv.F
Chd|        I7NORMNP                      source/interfaces/int07/i7rcurv.F
Chd|        I7NORMP                       source/interfaces/int07/i7curv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_I7CURVCOM(IAD_ELEM,FR_ELEM,ADSKYT,FSKYT,
     .                          ISDSIZ  ,IRCSIZ ,ITAG  ,LENR    ,LENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENR,LENS,ITAG(*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ISDSIZ(*), IRCSIZ(*),
     .        ADSKYT(0:*)
       my_real
     .        FSKYT(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I ,J ,N1, N2, N3, N4,IERROR, IAD, IAD1, IAD2, SIZ, NB,
     .        MSGTYP, LOC_PROC, CC, MSGOFF,
     .        STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD)
C     REAL
       my_real
     .        BUFR(LENR), BUFS(LENS)
      DATA MSGOFF/195/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
C
      IAD = 1
      DO I=1,NSPMD
        SIZ = IRCSIZ(I)
        IF(SIZ.GT.0)THEN
          SIZ = SIZ*3+IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      BUFR(IAD),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
            IAD = IAD + SIZ
        ENDIF
      END DO

      DO I=1,NSPMD
        IF(ISDSIZ(I).GT.0)THEN
          IAD = 0
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N1 = FR_ELEM(J)
            IAD1 = ITAG(N1)
            IAD2 = ADSKYT(N1)-1
            NB = IAD2-IAD1+1
            IAD = IAD + 1
            BUFS(IAD) = NB
            DO CC = IAD1, IAD2
              IAD = IAD + 1
              BUFS(IAD) = FSKYT(1,CC)
              IAD = IAD + 1
              BUFS(IAD) = FSKYT(2,CC)
              IAD = IAD + 1
              BUFS(IAD) = FSKYT(3,CC)
            END DO
          END DO

          SIZ = 3*ISDSIZ(I)+IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
          MSGTYP = MSGOFF
          CALL MPI_SEND(
     S      BUFS,SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,IERROR)
        END IF
      END DO
C
      IAD = 0
      DO I = 1, NSPMD
        IF(IRCSIZ(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N1 = FR_ELEM(J)
            IAD = IAD + 1
            NB = NINT(BUFR(IAD))
            DO CC = 1, NB
              IAD1 = ADSKYT(N1)
              ADSKYT(N1) = ADSKYT(N1)+1
              IAD = IAD + 1
              FSKYT(1,IAD1) = BUFR(IAD)
              IAD = IAD + 1
              FSKYT(2,IAD1) = BUFR(IAD)
              IAD = IAD + 1
              FSKYT(3,IAD1) = BUFR(IAD)
            END DO
          END DO
        END IF
      END DO
C
#endif
      RETURN
      END
