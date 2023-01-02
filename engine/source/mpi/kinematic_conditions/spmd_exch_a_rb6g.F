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
Chd|  SPMD_EXCH_A_RB6G              source/mpi/kinematic_conditions/spmd_exch_a_rb6g.F
Chd|-- called by -----------
Chd|        RBYFOR                        source/constraints/general/rbody/rbyfor.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_RB6G(NPBY,RBF6)
C realise le cumul des acc et stiffness des noeuds main de rigid bodies
C dans le cas ou tous les procs sont concernes gather/scatter proc0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER NPBY(NNPBY,*)
      DOUBLE PRECISION
     .        RBF6(8,6,NRBYKIN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,A_AR,I,J,M,P,IRB,NN,
     .        MSGOFF,SIZ,IDEB,LEN
      INTEGER STATUS(MPI_STATUS_SIZE), REQ(NSPMD), IERROR, PMAIN
      DATA MSGOFF/166/
      PARAMETER (A_AR = 48)           ! 6*8
      DOUBLE PRECISION
     .        BUF(NRBYKIN*(A_AR+1)*(NSPMD-1))
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      PMAIN=1
      LOC_PROC = ISPMD + 1
      LEN=NRBYKIN*(A_AR+1)
      IDEB=1
      IF(LOC_PROC==PMAIN) THEN
        DO P = 2, NSPMD
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     .      BUF(IDEB),LEN,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     .      MSGTYP,MPI_COMM_WORLD,REQ(P-1),IERROR    )
          IDEB = IDEB+LEN
        END DO
        DO IRB = 1, NRBYKIN
          M = NPBY(1,IRB)
          IF(M <= 0) THEN
            RBF6(1,1,IRB)= ZERO
            RBF6(1,2,IRB)= ZERO
            RBF6(1,3,IRB)= ZERO
            RBF6(1,4,IRB)= ZERO
            RBF6(1,5,IRB)= ZERO
            RBF6(1,6,IRB)= ZERO
            RBF6(2,1,IRB)= ZERO
            RBF6(2,2,IRB)= ZERO
            RBF6(2,3,IRB)= ZERO
            RBF6(2,4,IRB)= ZERO
            RBF6(2,5,IRB)= ZERO
            RBF6(2,6,IRB)= ZERO
            RBF6(3,1,IRB)= ZERO
            RBF6(3,2,IRB)= ZERO
            RBF6(3,3,IRB)= ZERO
            RBF6(3,4,IRB)= ZERO
            RBF6(3,5,IRB)= ZERO
            RBF6(3,6,IRB)= ZERO
            RBF6(4,1,IRB)= ZERO
            RBF6(4,2,IRB)= ZERO
            RBF6(4,3,IRB)= ZERO
            RBF6(4,4,IRB)= ZERO
            RBF6(4,5,IRB)= ZERO
            RBF6(4,6,IRB)= ZERO
            RBF6(5,1,IRB)= ZERO
            RBF6(5,2,IRB)= ZERO
            RBF6(5,3,IRB)= ZERO
            RBF6(5,4,IRB)= ZERO
            RBF6(5,5,IRB)= ZERO
            RBF6(5,6,IRB)= ZERO
            RBF6(6,1,IRB)= ZERO
            RBF6(6,2,IRB)= ZERO
            RBF6(6,3,IRB)= ZERO
            RBF6(6,4,IRB)= ZERO
            RBF6(6,5,IRB)= ZERO
            RBF6(6,6,IRB)= ZERO
            RBF6(7,1,IRB)= ZERO
            RBF6(7,2,IRB)= ZERO
            RBF6(7,3,IRB)= ZERO
            RBF6(7,4,IRB)= ZERO
            RBF6(7,5,IRB)= ZERO
            RBF6(7,6,IRB)= ZERO
            RBF6(8,1,IRB)= ZERO
            RBF6(8,2,IRB)= ZERO
            RBF6(8,3,IRB)= ZERO
            RBF6(8,4,IRB)= ZERO
            RBF6(8,5,IRB)= ZERO
            RBF6(8,6,IRB)= ZERO
          END IF
        END DO
        DO I=1,NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ,P,STATUS,IERROR)
          CALL MPI_GET_COUNT(STATUS,MPI_DOUBLE_PRECISION,SIZ,ierror)
          NN = SIZ/(A_AR+1)
          IDEB = 1+(P-1)*(A_AR+1)*NRBYKIN
          DO J = 1, NN
            IRB = NINT(BUF(IDEB))
            IDEB=IDEB+1
            RBF6(1,1,IRB)= RBF6(1,1,IRB) + BUF(IDEB)
            RBF6(1,2,IRB)= RBF6(1,2,IRB) + BUF(IDEB+1)
            RBF6(1,3,IRB)= RBF6(1,3,IRB) + BUF(IDEB+2)
            RBF6(1,4,IRB)= RBF6(1,4,IRB) + BUF(IDEB+3)
            RBF6(1,5,IRB)= RBF6(1,5,IRB) + BUF(IDEB+4)
            RBF6(1,6,IRB)= RBF6(1,6,IRB) + BUF(IDEB+5)
            RBF6(2,1,IRB)= RBF6(2,1,IRB) + BUF(IDEB+6)
            RBF6(2,2,IRB)= RBF6(2,2,IRB) + BUF(IDEB+7)
            RBF6(2,3,IRB)= RBF6(2,3,IRB) + BUF(IDEB+8)
            RBF6(2,4,IRB)= RBF6(2,4,IRB) + BUF(IDEB+9)
            RBF6(2,5,IRB)= RBF6(2,5,IRB) + BUF(IDEB+10)
            RBF6(2,6,IRB)= RBF6(2,6,IRB) + BUF(IDEB+11)
            RBF6(3,1,IRB)= RBF6(3,1,IRB) + BUF(IDEB+12)
            RBF6(3,2,IRB)= RBF6(3,2,IRB) + BUF(IDEB+13)
            RBF6(3,3,IRB)= RBF6(3,3,IRB) + BUF(IDEB+14)
            RBF6(3,4,IRB)= RBF6(3,4,IRB) + BUF(IDEB+15)
            RBF6(3,5,IRB)= RBF6(3,5,IRB) + BUF(IDEB+16)
            RBF6(3,6,IRB)= RBF6(3,6,IRB) + BUF(IDEB+17)
            RBF6(4,1,IRB)= RBF6(4,1,IRB) + BUF(IDEB+18)
            RBF6(4,2,IRB)= RBF6(4,2,IRB) + BUF(IDEB+19)
            RBF6(4,3,IRB)= RBF6(4,3,IRB) + BUF(IDEB+20)
            RBF6(4,4,IRB)= RBF6(4,4,IRB) + BUF(IDEB+21)
            RBF6(4,5,IRB)= RBF6(4,5,IRB) + BUF(IDEB+22)
            RBF6(4,6,IRB)= RBF6(4,6,IRB) + BUF(IDEB+23)
            RBF6(5,1,IRB)= RBF6(5,1,IRB) + BUF(IDEB+24)
            RBF6(5,2,IRB)= RBF6(5,2,IRB) + BUF(IDEB+25)
            RBF6(5,3,IRB)= RBF6(5,3,IRB) + BUF(IDEB+26)
            RBF6(5,4,IRB)= RBF6(5,4,IRB) + BUF(IDEB+27)
            RBF6(5,5,IRB)= RBF6(5,5,IRB) + BUF(IDEB+28)
            RBF6(5,6,IRB)= RBF6(5,6,IRB) + BUF(IDEB+29)
            RBF6(6,1,IRB)= RBF6(6,1,IRB) + BUF(IDEB+30)
            RBF6(6,2,IRB)= RBF6(6,2,IRB) + BUF(IDEB+31)
            RBF6(6,3,IRB)= RBF6(6,3,IRB) + BUF(IDEB+32)
            RBF6(6,4,IRB)= RBF6(6,4,IRB) + BUF(IDEB+33)
            RBF6(6,5,IRB)= RBF6(6,5,IRB) + BUF(IDEB+34)
            RBF6(6,6,IRB)= RBF6(6,6,IRB) + BUF(IDEB+35)
            RBF6(7,1,IRB)= RBF6(7,1,IRB) + BUF(IDEB+36)
            RBF6(7,2,IRB)= RBF6(7,2,IRB) + BUF(IDEB+37)
            RBF6(7,3,IRB)= RBF6(7,3,IRB) + BUF(IDEB+38)
            RBF6(7,4,IRB)= RBF6(7,4,IRB) + BUF(IDEB+39)
            RBF6(7,5,IRB)= RBF6(7,5,IRB) + BUF(IDEB+40)
            RBF6(7,6,IRB)= RBF6(7,6,IRB) + BUF(IDEB+41)
            RBF6(8,1,IRB)= RBF6(8,1,IRB) + BUF(IDEB+42)
            RBF6(8,2,IRB)= RBF6(8,2,IRB) + BUF(IDEB+43)
            RBF6(8,3,IRB)= RBF6(8,3,IRB) + BUF(IDEB+44)
            RBF6(8,4,IRB)= RBF6(8,4,IRB) + BUF(IDEB+45)
            RBF6(8,5,IRB)= RBF6(8,5,IRB) + BUF(IDEB+46)
            RBF6(8,6,IRB)= RBF6(8,6,IRB) + BUF(IDEB+47)
            IDEB = IDEB+A_AR
          ENDDO
        ENDDO
        LEN=NRBYKIN*A_AR
        CALL MPI_BCAST(RBF6,LEN,MPI_DOUBLE_PRECISION,IT_SPMD(PMAIN),
     .                 MPI_COMM_WORLD,IERROR)
C
      ELSE
        IDEB = 1
        DO IRB = 1, NRBYKIN
         M = NPBY(1,IRB)
         IF(M > 0) THEN
          BUF(IDEB) = IRB
          IDEB = IDEB + 1
          BUF(IDEB)   = RBF6(1,1,IRB)
          BUF(IDEB+1) = RBF6(1,2,IRB)
          BUF(IDEB+2) = RBF6(1,3,IRB)
          BUF(IDEB+3) = RBF6(1,4,IRB)
          BUF(IDEB+4) = RBF6(1,5,IRB)
          BUF(IDEB+5) = RBF6(1,6,IRB)
          BUF(IDEB+6) = RBF6(2,1,IRB)
          BUF(IDEB+7) = RBF6(2,2,IRB)
          BUF(IDEB+8) = RBF6(2,3,IRB)
          BUF(IDEB+9) = RBF6(2,4,IRB)
          BUF(IDEB+10)= RBF6(2,5,IRB)
          BUF(IDEB+11)= RBF6(2,6,IRB)
          BUF(IDEB+12)= RBF6(3,1,IRB)
          BUF(IDEB+13)= RBF6(3,2,IRB)
          BUF(IDEB+14)= RBF6(3,3,IRB)
          BUF(IDEB+15)= RBF6(3,4,IRB)
          BUF(IDEB+16)= RBF6(3,5,IRB)
          BUF(IDEB+17)= RBF6(3,6,IRB)
          BUF(IDEB+18)= RBF6(4,1,IRB)
          BUF(IDEB+19)= RBF6(4,2,IRB)
          BUF(IDEB+20)= RBF6(4,3,IRB)
          BUF(IDEB+21)= RBF6(4,4,IRB)
          BUF(IDEB+22)= RBF6(4,5,IRB)
          BUF(IDEB+23)= RBF6(4,6,IRB)
          BUF(IDEB+24)= RBF6(5,1,IRB)
          BUF(IDEB+25)= RBF6(5,2,IRB)
          BUF(IDEB+26)= RBF6(5,3,IRB)
          BUF(IDEB+27)= RBF6(5,4,IRB)
          BUF(IDEB+28)= RBF6(5,5,IRB)
          BUF(IDEB+29)= RBF6(5,6,IRB)
          BUF(IDEB+30)= RBF6(6,1,IRB)
          BUF(IDEB+31)= RBF6(6,2,IRB)
          BUF(IDEB+32)= RBF6(6,3,IRB)
          BUF(IDEB+33)= RBF6(6,4,IRB)
          BUF(IDEB+34)= RBF6(6,5,IRB)
          BUF(IDEB+35)= RBF6(6,6,IRB)
          BUF(IDEB+36)= RBF6(7,1,IRB)
          BUF(IDEB+37)= RBF6(7,2,IRB)
          BUF(IDEB+38)= RBF6(7,3,IRB)
          BUF(IDEB+39)= RBF6(7,4,IRB)
          BUF(IDEB+40)= RBF6(7,5,IRB)
          BUF(IDEB+41)= RBF6(7,6,IRB)
          BUF(IDEB+42)= RBF6(8,1,IRB)
          BUF(IDEB+43)= RBF6(8,2,IRB)
          BUF(IDEB+44)= RBF6(8,3,IRB)
          BUF(IDEB+45)= RBF6(8,4,IRB)
          BUF(IDEB+46)= RBF6(8,5,IRB)
          BUF(IDEB+47)= RBF6(8,6,IRB)
          IDEB = IDEB + A_AR
         ENDIF
        ENDDO
        MSGTYP = MSGOFF 
        LEN=IDEB-1
        CALL MPI_SEND(BUF,LEN,MPI_DOUBLE_PRECISION,IT_SPMD(PMAIN),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        LEN=NRBYKIN*A_AR
        CALL MPI_BCAST(RBF6,LEN,MPI_DOUBLE_PRECISION,IT_SPMD(PMAIN),
     .                 MPI_COMM_WORLD,IERROR)
C
      END IF
C
#endif
      RETURN
      END
