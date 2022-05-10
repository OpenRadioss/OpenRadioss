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
Chd|  SPMD_SD_GAU                   source/mpi/output/spmd_sd_gau.F
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        AGAUGE0                       source/ale/agauge.F           
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SD_GAU(GAUGE,IGAUP,NGAUP)
C envoi vers p0 du tableau GAUGE calcule sur chaque proc
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGAUP(*), NGAUP(*)
      my_real
     .        GAUGE(LLGAUGE,*)
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,MSGOFF,IERROR,LOC_PROC,NN,L,I,K,N,II,J,
     .        IDEB,SIZ,A_AR,NBIRECV,INDEX,
     .        IRINDEX(NSPMD),REQ_R(NSPMD),IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      my_real
     .        BUF(LLGAUGE+1,NBGAUGE)
      DATA MSGOFF/202/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IF (LOC_PROC.EQ.1) THEN
        IDEB = 1
        NBIRECV = 0
        DO I = 2, NSPMD
          IAD_RECV(I) = IDEB
          IF(NGAUP(I).GT.0)THEN
            NBIRECV = NBIRECV + 1
            IRINDEX(NBIRECV) = I
            NN = NGAUP(I)
            SIZ = (LLGAUGE+1)*NN
            MSGTYP = MSGOFF
            CALL MPI_IRECV(
     S        BUF(1,IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
           IDEB = IDEB + NN
          END IF
        END DO
C
        DO II = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
          I = IRINDEX(INDEX)
          L = IAD_RECV(I)
          NN = NGAUP(I)
          DO N = L, L+NN-1
            K = NINT(BUF(1,N))
            DO J = 1, LLGAUGE
              GAUGE(J,K) = BUF(J+1,N)
            END DO
          END DO
        END DO
C
      ELSE
        K = 0
        DO N = 1, NBGAUGE
         IF(IGAUP(N).EQ.LOC_PROC)THEN
           K = K + 1
           BUF(1,K) = N
           DO J = 1, LLGAUGE
             BUF(J+1,K) = GAUGE(J,N)
           END DO
         END IF
        END DO
        IF(K.GT.0)THEN
          SIZ = (LLGAUGE+1)*K
          MSGTYP=MSGOFF
          CALL MPI_SEND(
     S      BUF,SIZ,REAL,IT_SPMD(1),MSGTYP,
     G      MPI_COMM_WORLD,IERROR)
        END IF
      END IF
C
#endif
      RETURN
      END
