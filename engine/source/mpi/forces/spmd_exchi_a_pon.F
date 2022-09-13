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
Chd|  SPMD_EXCHI_A_PON              source/mpi/forces/spmd_exchi_a_pon.F
Chd|-- called by -----------
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_EXCHI_A_PON(
     1   IAD_ELEM,FR_ELEM ,SIZI   ,ISKY   ,FSKYI ,
     2   FSKYIF  ,ITAGX   ,ADSKYI ,NBIRCT ,NBISDT,
     3   NBIRCP  ,NBISDP  ,FTHESKYI, FTHESKYIF,CONDNSKYI,
     4   CONDNSKYIF,FSKYIF_PXFEM)
C
       USE PLYXFEM_MOD       
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
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), NBIRCP(*), NBISDP(*),
     .        ITAGX(*), ADSKYI(0:*), ISKY(*),
     .        SIZI, NBIRCT, NBISDT
      my_real
     .        FSKYI(LSKYI,NFSKYI),FSKYIF(NFSKYI,*),FTHESKYI(*),
     .        FTHESKYIF(*),CONDNSKYI(*),CONDNSKYIF(*),
     .        FSKYIF_PXFEM(5,LSKYI)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,J,NOD,LOC_PROC,IERROR,INDEX,IDEB,
     .        SIZ,L,NB_NOD,CC,NBIRECV, II, JJ, L0,MSGOFF,
     .        IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),IRINDEX(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),POINT,LSAV
      my_real, DIMENSION(:), ALLOCATABLE :: RBUF,SBUF
      DATA MSGOFF/170/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      ALLOCATE(RBUF((SIZI)*NBIRCT))
      ALLOCATE(SBUF((SIZI)*NBISDT))
      NBIRECV = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF(NBIRCP(I)>0) THEN
          SIZ = (SIZI)*NBIRCP(I)
          MSGTYP = MSGOFF 
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      ENDDO
C
C Envoi interfaces
C
      L = 1
      DO I=1,NSPMD
       IF(NBISDP(I)>0) THEN
         L0 = L
         IDEB = IAD_ELEM(1,I)-1
C
         LSAV=L
         IF(INTPLYXFEM == 0) THEN
C           
           IF(INTHEAT == 0 )THEN
            IF(NFSKYI==4)THEN
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   L = L + SIZI
                 END DO
               END IF
             END DO
            ELSE
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FSKYIF(5,CC)
                   L = L + SIZI
                 END DO
               END IF
             END DO
            END IF
C + la th  ermique
           ELSE
            IF(NFSKYI==4)THEN
             IF(NODADT_THERM == 1) THEN
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FTHESKYIF(CC)
                   SBUF(L+6) = CONDNSKYIF(CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ELSE
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FTHESKYIF(CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ENDIF
            ELSE
             IF(NODADT_THERM == 1) THEN
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FSKYIF(5,CC)
                   SBUF(L+6) = FTHESKYIF(CC)
                   SBUF(L+7) = CONDNSKYIF(CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ELSE
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FSKYIF(5,CC)
                   SBUF(L+6) = FTHESKYIF(CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ENDIF
            END IF
           ENDIF ! INTHEAT
         ELSE
C for type24 + pxfem          
           IF(INTHEAT == 0 )THEN
            IF(NFSKYI==4)THEN
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
C                   
                   SBUF(L+5) = FSKYIF_PXFEM(1,CC)
                   SBUF(L+6) = FSKYIF_PXFEM(2,CC)
                   SBUF(L+7) = FSKYIF_PXFEM(3,CC)
                   SBUF(L+8) = FSKYIF_PXFEM(4,CC)
                   SBUF(L+9) = FSKYIF_PXFEM(5,CC)
                   L = L + SIZI
                 END DO
               END IF
             END DO
            ELSE
             DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FSKYIF(5,CC)
C                   
                   SBUF(L+6) = FSKYIF_PXFEM(1,CC)
                   SBUF(L+7) = FSKYIF_PXFEM(2,CC)
                   SBUF(L+8) = FSKYIF_PXFEM(3,CC)
                   SBUF(L+9) = FSKYIF_PXFEM(4,CC)
                   SBUF(L+10) = FSKYIF_PXFEM(5,CC)
                   L = L + SIZI
                 END DO
               END IF
             END DO
            END IF
C + la th  ermique
           ELSE
            IF(NFSKYI==4)THEN
             IF(NODADT_THERM == 1) THEN
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FTHESKYIF(CC)
                   SBUF(L+6) = CONDNSKYIF(CC)
C                   
                   SBUF(L+7) = FSKYIF_PXFEM(1,CC)
                   SBUF(L+8) = FSKYIF_PXFEM(2,CC)
                   SBUF(L+9) = FSKYIF_PXFEM(3,CC)
                   SBUF(L+10)= FSKYIF_PXFEM(4,CC)
                   SBUF(L+11)= FSKYIF_PXFEM(5,CC)                   
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ELSE
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FTHESKYIF(CC)
C                   
                   SBUF(L+6) = FSKYIF_PXFEM(1,CC)
                   SBUF(L+7) = FSKYIF_PXFEM(2,CC)
                   SBUF(L+8) = FSKYIF_PXFEM(3,CC)
                   SBUF(L+9) = FSKYIF_PXFEM(4,CC)
                   SBUF(L+10)= FSKYIF_PXFEM(5,CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ENDIF
            ELSE
             IF(NODADT_THERM == 1) THEN
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FSKYIF(5,CC)
                   SBUF(L+6) = FTHESKYIF(CC)
                   SBUF(L+7) = CONDNSKYIF(CC)
C                   
                   SBUF(L+8) = FSKYIF_PXFEM(1,CC)
                   SBUF(L+9) = FSKYIF_PXFEM(2,CC)
                   SBUF(L+10)= FSKYIF_PXFEM(3,CC)
                   SBUF(L+11)= FSKYIF_PXFEM(4,CC)
                   SBUF(L+12)= FSKYIF_PXFEM(5,CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ELSE
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               IF(ITAGX(NOD)==1)THEN
#include "scalar.inc"
                 DO CC = ADSKYI(NOD-1),ADSKYI(NOD)-1
                   SBUF(L)   = J-IDEB
                   SBUF(L+1) = FSKYIF(1,CC)
                   SBUF(L+2) = FSKYIF(2,CC)
                   SBUF(L+3) = FSKYIF(3,CC)
                   SBUF(L+4) = FSKYIF(4,CC)
                   SBUF(L+5) = FSKYIF(5,CC)
                   SBUF(L+6) = FTHESKYIF(CC)
C                   
                   SBUF(L+7) = FSKYIF_PXFEM(1,CC)
                   SBUF(L+8) = FSKYIF_PXFEM(2,CC)
                   SBUF(L+9) = FSKYIF_PXFEM(3,CC)
                   SBUF(L+10)= FSKYIF_PXFEM(4,CC)
                   SBUF(L+11)= FSKYIF_PXFEM(5,CC)
                   L = L + SIZI
                 END DO
               END IF
              END DO
             ENDIF
            END IF
           ENDIF ! INTHEAT 
         ENDIF ! INTPLYXFEM
C
         SIZ = NBISDP(I)*(SIZI)
         MSGTYP = MSGOFF
         CALL MPI_ISEND(
     S      SBUF(L0),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       END IF
      END DO
C
C decompactage partie interface
C
      NBIRCT = 0
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        L = IAD_RECV(I)
        NB_NOD = NBIRCP(I)
        IDEB = IAD_ELEM(1,I)-1
C
        IF(INTPLYXFEM == 0) THEN
          IF(INTHEAT == 0 ) THEN
           IF(NFSKYI==4)THEN
#include "vectorize.inc"
            DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              L = L + SIZI
            ENDDO
           ELSE
#include "vectorize.inc"
            DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FSKYI(NISKY+J,5) = RBUF(L+5)
              L = L + SIZI
            ENDDO
           ENDIF
C + la thermique
          ELSE
           IF(NFSKYI==4)THEN
            IF(NODADT_THERM == 1) THEN
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FTHESKYI(NISKY + J) = RBUF(L+5)
              CONDNSKYI(NISKY + J)= RBUF(L+6)
              L = L + SIZI
             ENDDO
            ELSE
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FTHESKYI(NISKY + J) = RBUF(L+5)
              L = L + SIZI
             ENDDO
            ENDIF
           ELSE
            IF(NODADT_THERM == 1) THEN
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FSKYI(NISKY+J,5) = RBUF(L+5)
              FTHESKYI(NISKY + J) = RBUF(L+6)
              CONDNSKYI(NISKY + J)= RBUF(L+7)
              L = L + SIZI
             ENDDO
            ELSE
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FSKYI(NISKY+J,5) = RBUF(L+5)
              FTHESKYI(NISKY + J) = RBUF(L+6)
              L = L + SIZI
             ENDDO
            ENDIF
           ENDIF

          ENDIF ! INTHEAT
      ELSE ! INTPLYXFEM > 0
          IF(INTHEAT == 0 ) THEN
           IF(NFSKYI==4)THEN
#include "vectorize.inc"
            DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
C              
              PLYSKYI%FSKYI(NISKY+J,1) = RBUF(L+5)
              PLYSKYI%FSKYI(NISKY+J,2) = RBUF(L+6)
              PLYSKYI%FSKYI(NISKY+J,3) = RBUF(L+7)
              PLYSKYI%FSKYI(NISKY+J,4) = RBUF(L+8)
              PLYSKYI%FSKYI(NISKY+J,5) = RBUF(L+9)
              L = L + SIZI
            ENDDO
           ELSE
#include "vectorize.inc"
            DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FSKYI(NISKY+J,5) = RBUF(L+5)
C              
              PLYSKYI%FSKYI(NISKY+J,1) = RBUF(L+6)
              PLYSKYI%FSKYI(NISKY+J,2) = RBUF(L+7)
              PLYSKYI%FSKYI(NISKY+J,3) = RBUF(L+8)
              PLYSKYI%FSKYI(NISKY+J,4) = RBUF(L+9)
              PLYSKYI%FSKYI(NISKY+J,5) = RBUF(L+10)
              L = L + SIZI
            ENDDO
           ENDIF
C + la thermique
          ELSE
           IF(NFSKYI==4)THEN
            IF(NODADT_THERM == 1) THEN
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FTHESKYI(NISKY + J) = RBUF(L+5)
              CONDNSKYI(NISKY + J)= RBUF(L+6)
C              
              PLYSKYI%FSKYI(NISKY+J,1) = RBUF(L+7)
              PLYSKYI%FSKYI(NISKY+J,2) = RBUF(L+8)
              PLYSKYI%FSKYI(NISKY+J,3) = RBUF(L+9)
              PLYSKYI%FSKYI(NISKY+J,4) = RBUF(L+10)
              PLYSKYI%FSKYI(NISKY+J,5) = RBUF(L+11)
              L = L + SIZI
             ENDDO
            ELSE
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FTHESKYI(NISKY + J) = RBUF(L+5)
C              
              PLYSKYI%FSKYI(NISKY+J,1) = RBUF(L+6)
              PLYSKYI%FSKYI(NISKY+J,2) = RBUF(L+7)
              PLYSKYI%FSKYI(NISKY+J,3) = RBUF(L+8)
              PLYSKYI%FSKYI(NISKY+J,4) = RBUF(L+9)
              PLYSKYI%FSKYI(NISKY+J,5) = RBUF(L+10)
              L = L + SIZI
             ENDDO
            ENDIF
           ELSE
            IF(NODADT_THERM == 1) THEN
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FSKYI(NISKY+J,5) = RBUF(L+5)
              FTHESKYI(NISKY + J) = RBUF(L+6)
              CONDNSKYI(NISKY + J)= RBUF(L+7)
C              
              PLYSKYI%FSKYI(NISKY+J,1) = RBUF(L+8)
              PLYSKYI%FSKYI(NISKY+J,2) = RBUF(L+9)
              PLYSKYI%FSKYI(NISKY+J,3) = RBUF(L+10)
              PLYSKYI%FSKYI(NISKY+J,4) = RBUF(L+11)
              PLYSKYI%FSKYI(NISKY+J,5) = RBUF(L+12)
              L = L + SIZI
             ENDDO
            ELSE
#include "vectorize.inc"
             DO J = 1, NB_NOD
              JJ = NINT(RBUF(L))+IDEB
              NOD = FR_ELEM(JJ)
              ISKY(NISKY+J) = NOD
              FSKYI(NISKY+J,1) = RBUF(L+1)
              FSKYI(NISKY+J,2) = RBUF(L+2)
              FSKYI(NISKY+J,3) = RBUF(L+3)
              FSKYI(NISKY+J,4) = RBUF(L+4)
              FSKYI(NISKY+J,5) = RBUF(L+5)
              FTHESKYI(NISKY + J) = RBUF(L+6)
C              
              PLYSKYI%FSKYI(NISKY+J,1) = RBUF(L+7)
              PLYSKYI%FSKYI(NISKY+J,2) = RBUF(L+8)
              PLYSKYI%FSKYI(NISKY+J,3) = RBUF(L+9)
              PLYSKYI%FSKYI(NISKY+J,4) = RBUF(L+10)
              PLYSKYI%FSKYI(NISKY+J,5) = RBUF(L+11)
              L = L + SIZI
             ENDDO
            ENDIF
           ENDIF

          ENDIF ! INTHEAT      
      
      ENDIF ! INTPLYXFEM 
        NISKY = NISKY + NB_NOD
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(NBISDP(I)>0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
      DEALLOCATE(RBUF)
      DEALLOCATE(SBUF)
C
#endif
      RETURN
      END
