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
C
Chd|====================================================================
Chd|  SPMD_SYNC_MMXG2               source/mpi/interfaces/spmd_sync_mmxg2.F
Chd|-- called by -----------
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SYNC_MMXG2(
     1    ISENDTO,IRCVFROM,NEWFRONT,XSLV_L,XMSR_L,
     2    VSLV_L ,VMSR_L  ,INTLIST ,NINTC ,TZINF ,
     3    SIZE_T ,IPARI   , DELTA_PMAX_GAP,MAXDGAP       )
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISENDTO(NINTER+1,*) ,IRCVFROM(NINTER+1,*),
     .        NEWFRONT(*), NINTC, INTLIST(*), IPARI(NPARI,*)
      my_real
     .        XSLV_L(18,*), XMSR_L(12,*), VSLV_L(6,*),
     .        VMSR_L(6,*), TZINF(*), SIZE_T(*),DELTA_PMAX_GAP(*),
     .        MAXDGAP(NINTER)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,IERROR,I_LEN,myop,
     .        KK, L, J, NIN, REQ,
     .        STATUS(MPI_STATUS_SIZE),type_reduc
      PARAMETER (I_LEN = 46)
      my_real
     .        SBUF(NINTC*I_LEN), RBUF(NINTC*I_LEN)
      INTEGER :: MESSAGE_LEN
      my_real SBUF3(NINTC), RBUF3(NINTC) 
      INTEGER SBUF2(NINTC), RBUF2(NINTC) 

      INTEGER :: I17 !Number of TYPE17 interface 

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      SBUF3(1:NINTC) = ZERO
      SBUF2(1:NINTC) = 0


      I17 = 0
      LOC_PROC = ISPMD+1
      IF(IRCVFROM(NINTER+1,LOC_PROC)>0.OR.
     +   ISENDTO(NINTER+1,LOC_PROC)>0) THEN

C Pack original data      
       L = 0
       DO KK=1,NINTC
        NIN = INTLIST(KK)
C X MIN/MAX
        DO J=1,18
          IF(MOD((J-1)/3,2) == 0) THEN
            SBUF(L+J) = XSLV_L(J,NIN)
          ELSE
            SBUF(L+J) =-XSLV_L(J,NIN)
          ENDIF
        END DO
        L = L + 18
C
        DO J=1,12
          IF(MOD((J-1)/3,2) == 0) THEN
            SBUF(L+J) = XMSR_L(J,NIN)
          ELSE
            SBUF(L+J) =-XMSR_L(J,NIN)
          ENDIF
        END DO
        L = L + 12
C V MIN/MAX
        DO J=1,6
          IF(MOD((J-1)/3,2) == 0) THEN
            SBUF(L+J) = VSLV_L(J,NIN)
          ELSE
            SBUF(L+J) =-VSLV_L(J,NIN)
          ENDIF
        END DO
        L = L + 6
C
        DO J=1,6
          IF(MOD((J-1)/3,2) == 0) THEN
            SBUF(L+J) = VMSR_L(J,NIN)
          ELSE
            SBUF(L+J) = -VMSR_L(J,NIN)
          ENDIF
        END DO
        L = L + 6

        IF(IPARI(7,NIN)/=17)THEN
C NEWFRONT
          SBUF(L+1) = -NEWFRONT(NIN)
          SBUF2(KK) = NEWFRONT(NIN)
          SBUF3(KK) = 0 
        ELSE     ! interface 17 avec courbure

          SBUF(L+1) = ZERO
C SIZE
          SBUF3(KK) = SIZE_T(NIN)
          I17 = I17+1
        END IF
        L = L + 1
C TZINF
        SBUF(L+1) = -TZINF(KK)
        L = L + 1
C DELTA_PMAX_GAP
        SBUF(L+1) = DELTA_PMAX_GAP(NIN)
        L = L + 1
C T25 main gap changes with thickness change
        SBUF(L+1) = MAXDGAP(NIN)
        L = L + 1
       END DO
       MESSAGE_LEN = NINTC * I_LEN
       call MPI_AllReduce(SBUF, RBUF, MESSAGE_LEN, REAL , MPI_MAX,
     &                   COMM_CONT, ierror)
C       MESSAGE_LEN = NINTC
C        call MPI_AllReduce(SBUF2, RBUF2, MESSAGE_LEN, MPI_INTEGER , MPI_SUM,
C    &                   COMM_CONT, ierror)

       IF(I17 > 0) THEN
         MESSAGE_LEN = NINTC
         call MPI_AllReduce(SBUF3, RBUF3, MESSAGE_LEN, REAL , MPI_SUM,
     &                   COMM_CONT, ierror)
       ENDIF

       L = 0
       DO KK=1,NINTC
         NIN = INTLIST(KK)
         IF(IRCVFROM(NIN,LOC_PROC)/=0.OR.
     +        ISENDTO(NIN,LOC_PROC)/=0)THEN
           DO J=1,18
             IF(MOD((J-1)/3,2) == 0) THEN
              XSLV_L(J,NIN) = RBUF(L+J)
             ELSE
              XSLV_L(J,NIN) =-RBUF(L+J)
             ENDIF
           END DO
           L = L + 18
           DO J=1,12
             IF(MOD((J-1)/3,2) == 0) THEN
               XMSR_L(J,NIN) = RBUF(L+J)
             ELSE
               XMSR_L(J,NIN) =-RBUF(L+J)
             ENDIF
           END DO
           L = L + 12
           DO J=1,6
             IF(MOD((J-1)/3,2) == 0) THEN
               VSLV_L(J,NIN) = RBUF(L+J)
             ELSE
               VSLV_L(J,NIN) =-RBUF(L+J)
             ENDIF
           END DO
           L = L + 6
           DO J=1,6
             IF(MOD((J-1)/3,2) == 0) THEN
              VMSR_L(J,NIN) = RBUF(L+J)
             ELSE
              VMSR_L(J,NIN) = -RBUF(L+J)
             ENDIF
           END DO
           L = L + 6
           IF(IPARI(7,NIN)/=17)THEN
             NEWFRONT(NIN) = - NINT(RBUF(L+1))
           ELSE     ! interface 17 avec courbure
             SIZE_T(NIN) = RBUF3(KK)
           END IF
           L = L + 1
           TZINF(KK) = -RBUF(L+1)
           L = L + 1
           DELTA_PMAX_GAP(NIN) = RBUF(L+1)
           L = L + 1
C T25 main gap changes with thickness change
           MAXDGAP(NIN) = RBUF(L+1)
           L = L + 1
         ELSE
           L = L + I_LEN
         END IF
       END DO

      END IF


#endif
      RETURN
      END
