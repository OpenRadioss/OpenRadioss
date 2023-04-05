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
#include "macro.inc"
Chd|====================================================================
Chd|  SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALETHE                        source/ale/alethe.F           
Chd|        ALEW1                         source/ale/grid/alew1.F       
Chd|        ALEW5                         source/ale/grid/alew5.F       
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_XVOIS(
     1    X     ,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,
     2    LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBRCVOIS(*), NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .        LENCOM
      my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN      
      DATA MSGOFF/3000/
      my_real
     .        WA(LENCOM*3)
C-----------------------------------------------
C
C Updating X on adjacent domains
C 

      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)
      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NBRCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = 3*NBRCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1),LEN,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),ierror)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NBSDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LNSDVOIS(IDEB+N)
            WA(IDEB2+3*(N-1)+1) = X(1,NN)
            WA(IDEB2+3*(N-1)+2) = X(2,NN)
            WA(IDEB2+3*(N-1)+3) = X(3,NN)
          ENDDO
          CALL MPI_ISEND(
     S      WA(IDEB2+1),LEN*3,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),ierror)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + 3*LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        IDEB2 = IAD_RECV(I)
        IDEB = (IDEB2-1)/3
        DO N = 1, NBRCVOIS(I)
          NN = LNRCVOIS(IDEB+N)
          X(1,NN) = WA(IDEB2+3*(N-1))
          X(2,NN) = WA(IDEB2+3*(N-1)+1)
          X(3,NN) = WA(IDEB2+3*(N-1)+2)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NBSDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_WVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEW                          source/ale/grid/alew.F        
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_WVOIS(X       ,D       ,W ,NBRCVOIS,NBSDVOIS,
     .                      LNRCVOIS,LNSDVOIS,LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBRCVOIS(*), NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .        LENCOM
      my_real X(3,*), D(3,*), W(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN      
      DATA MSGOFF/3001/
      my_real WA(LENCOM*9)
C-----------------------------------------------
C
C Updating X on adjacent domains
C 
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NBRCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = 9*NBRCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1),LEN,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NBSDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LNSDVOIS(IDEB+N)
            WA(IDEB2+9*(N-1)+1) = X(1,NN)
            WA(IDEB2+9*(N-1)+2) = X(2,NN)
            WA(IDEB2+9*(N-1)+3) = X(3,NN)
            WA(IDEB2+9*(N-1)+4) = D(1,NN)
            WA(IDEB2+9*(N-1)+5) = D(2,NN)
            WA(IDEB2+9*(N-1)+6) = D(3,NN)
            WA(IDEB2+9*(N-1)+7) = W(1,NN)
            WA(IDEB2+9*(N-1)+8) = W(2,NN)
            WA(IDEB2+9*(N-1)+9) = W(3,NN)
          ENDDO
          CALL MPI_ISEND(
     S      WA(IDEB2+1),LEN*9,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + 9*LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB2 = IAD_RECV(I)
        IDEB = (IDEB2-1)/9
        DO N = 1, NBRCVOIS(I)
          NN = LNRCVOIS(IDEB+N)
          X(1,NN) = WA(IDEB2+9*(N-1))
          X(2,NN) = WA(IDEB2+9*(N-1)+1)
          X(3,NN) = WA(IDEB2+9*(N-1)+2)
          D(1,NN) = WA(IDEB2+9*(N-1)+3)
          D(2,NN) = WA(IDEB2+9*(N-1)+4)
          D(3,NN) = WA(IDEB2+9*(N-1)+5)
          W(1,NN) = WA(IDEB2+9*(N-1)+6)
          W(2,NN) = WA(IDEB2+9*(N-1)+7)
          W(3,NN) = WA(IDEB2+9*(N-1)+8)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NBSDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_EVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        AETURB                        source/ale/turbulence/aeturb.F
Chd|        AKTURB                        source/ale/turbulence/akturb.F
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_EVOIS(
     1     T       ,VAL2  ,NERCVOIS,NESDVOIS,LERCVOIS,
     2     LESDVOIS,LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),
     .        LENCOM
      my_real T(*), VAL2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN      
      DATA MSGOFF/3002/
      my_real
     .        WA(LENCOM*2)
C-----------------------------------------------
C
C Updating X on adjacent domains
C 
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = 2*NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1),LEN,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(IDEB2+2*(N-1)+1) = T(NN)
            WA(IDEB2+2*(N-1)+2) = VAL2(NN)
          ENDDO
          CALL MPI_ISEND(
     S      WA(IDEB2+1),LEN*2,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + 2*LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        IDEB2 = IAD_RECV(I)
        IDEB = (IDEB2-1)/2
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          T(NN) = WA(IDEB2+2*(N-1))
          VAL2(NN) = WA(IDEB2+2*(N-1)+1)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        ALEW6                         source/ale/grid/alew6.F       
Chd|        AREZON                        source/ale/arezon.F           
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_E1VOIS(
     1                        PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                        LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),LENCOM
      my_real PHI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3003/
      my_real WA(LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C 
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1),LEN,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(IDEB2+N) = PHI(NN)
          ENDDO
          CALL MPI_ISEND(
     S      WA(IDEB2+1),LEN,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN) = WA(IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_E6VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_E6VOIS(
     1                       PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),LENCOM
      my_real PHI(NUMELS+NSVOIS,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3004/
      my_real
     .        WA(6,LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*6,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(1,IDEB2+N) = PHI(NN,1)
            WA(2,IDEB2+N) = PHI(NN,2)
            WA(3,IDEB2+N) = PHI(NN,3)
            WA(4,IDEB2+N) = PHI(NN,4)
            WA(5,IDEB2+N) = PHI(NN,5)
            WA(6,IDEB2+N) = PHI(NN,6)
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*6,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN,1) = WA(1,IDEB+N)
          PHI(NN,2) = WA(2,IDEB+N)
          PHI(NN,3) = WA(3,IDEB+N)
          PHI(NN,4) = WA(4,IDEB+N)
          PHI(NN,5) = WA(5,IDEB+N)
          PHI(NN,6) = WA(6,IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_E4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_E4VOIS(
     1                       PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*), LENCOM
      my_real PHI(NUMELQ+NQVOIS,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3004/
      my_real
     .        WA(4,LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*4,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(1,IDEB2+N) = PHI(NN,1)
            WA(2,IDEB2+N) = PHI(NN,2)
            WA(3,IDEB2+N) = PHI(NN,3)
            WA(4,IDEB2+N) = PHI(NN,4)
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*4,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN,1) = WA(1,IDEB+N)
          PHI(NN,2) = WA(2,IDEB+N)
          PHI(NN,3) = WA(3,IDEB+N)
          PHI(NN,4) = WA(4,IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_E3VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_E3VOIS(
     1                       PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*), LENCOM
      my_real PHI(NUMELTG+NTGVOIS,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3004/
      my_real WA(3,LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C
      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*3,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(1,IDEB2+N) = PHI(NN,1)
            WA(2,IDEB2+N) = PHI(NN,2)
            WA(3,IDEB2+N) = PHI(NN,3)
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*3,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN,1) = WA(1,IDEB+N)
          PHI(NN,2) = WA(2,IDEB+N)
          PHI(NN,3) = WA(3,IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALEW6                         source/ale/grid/alew6.F       
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_ENVOIS(DIM,
     1                       PHI     ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                       LESDVOIS,LENCOM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: DIM
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*), LENCOM
      my_real PHI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, NDIM, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
      DATA MSGOFF/3004/
      my_real
     .     WA(DIM, LENCOM)
C-----------------------------------------------
C
C Updating Phi on adjacent elements
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*DIM,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            DO NDIM = 1, DIM
               WA(NDIM,IDEB2+N) = PHI(DIM * (NN - 1) + NDIM)
            ENDDO
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*DIM,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          DO NDIM = 1, DIM
             PHI(DIM * (NN - 1) + NDIM) = WA(NDIM,IDEB+N)
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_I1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_I1VOIS(
     1                       PHI   ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     2                       LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),PHI(*), LENCOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN, WA(LENCOM)      
      DATA MSGOFF/3005/
C-----------------------------------------------
C
C Updating X on adjacent domains
C 
      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1),LEN,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),ierror)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(IDEB2+N) = PHI(NN)
          ENDDO
          CALL MPI_ISEND(
     S      WA(IDEB2+1),LEN,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),ierror)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN) = WA(IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,ierror)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_I8VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_I8VOIS(
     1                       PHI   ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     2                       LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),
     .        PHI(NUMELS+NSVOIS,8), LENCOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN, WA(8,LENCOM)
      DATA MSGOFF/3006/
C-----------------------------------------------
C
C Updating X on adjacent domains
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*8,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),ierror)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(1,IDEB2+N) = PHI(NN,1)
            WA(2,IDEB2+N) = PHI(NN,2)
            WA(3,IDEB2+N) = PHI(NN,3)
            WA(4,IDEB2+N) = PHI(NN,4)
            WA(5,IDEB2+N) = PHI(NN,5)
            WA(6,IDEB2+N) = PHI(NN,6)
            WA(7,IDEB2+N) = PHI(NN,7)
            WA(8,IDEB2+N) = PHI(NN,8)

          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*8,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),ierror)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN,1) = WA(1,IDEB+N)
          PHI(NN,2) = WA(2,IDEB+N)
          PHI(NN,3) = WA(3,IDEB+N)
          PHI(NN,4) = WA(4,IDEB+N)
          PHI(NN,5) = WA(5,IDEB+N)
          PHI(NN,6) = WA(6,IDEB+N)
          PHI(NN,7) = WA(7,IDEB+N)
          PHI(NN,8) = WA(8,IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,ierror)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_I4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_I4VOIS(
     1                       PHI   ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     2                       LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),
     .        PHI(NUMELQ+NQVOIS,4), LENCOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN, WA(4,LENCOM)
      DATA MSGOFF/3006/
C-----------------------------------------------
C
C Updating X on adjacent domains
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*4,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),ierror)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(1,IDEB2+N) = PHI(NN,1)
            WA(2,IDEB2+N) = PHI(NN,2)
            WA(3,IDEB2+N) = PHI(NN,3)
            WA(4,IDEB2+N) = PHI(NN,4)
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*4,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),ierror)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN,1) = WA(1,IDEB+N)
          PHI(NN,2) = WA(2,IDEB+N)
          PHI(NN,3) = WA(3,IDEB+N)
          PHI(NN,4) = WA(4,IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,ierror)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_I3VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_I3VOIS(
     1                       PHI   ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     2                       LENCOM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),
     .        PHI(NUMELTG+NTGVOIS,3), LENCOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN, WA(3,LENCOM)
      DATA MSGOFF/3006/
C-----------------------------------------------
C
C Updating X on adjacent domains
C
      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(1,IDEB2+1),LEN*3,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),ierror)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          DO N = 1, LEN
            NN = LESDVOIS(IDEB+N)
            WA(1,IDEB2+N) = PHI(NN,1)
            WA(2,IDEB2+N) = PHI(NN,2)
            WA(3,IDEB2+N) = PHI(NN,3)
          ENDDO
          CALL MPI_ISEND(
     S      WA(1,IDEB2+1),LEN*3,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),ierror)
          IDEB = IDEB + LEN
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
C
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)-1
        DO N = 1, NERCVOIS(I)
          NN = LERCVOIS(IDEB+N)
          PHI(NN,1) = WA(1,IDEB+N)
          PHI(NN,2) = WA(2,IDEB+N)
          PHI(NN,3) = WA(3,IDEB+N)
        ENDDO
      ENDDO
C
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,ierror)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_L11VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_L11VOIS(
     1                        LBVOIS  ,IPARG   ,ELBUF_TAB,PM      ,IXS,
     2                        IXQ     ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     3                        LENCOM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
      INTEGER IPARG(NPARG,*),IXS(NIXS,*), IXQ(NIXQ,*),
     .        NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),
     .        LENCOM
      my_real LBVOIS(6,*), PM(NPROPM,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,II,JJ,IDEB,IDEB2,MSGOFF,IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), INDEX,
     .        LEN, ML, NI, KTY, KLT, MFT, IS,
     .        KB1, KB2, KB3 ,KB4 ,KB10, KB11, KB12, KKB2,
     .        G_PLA,G_RK,L_RE,L_TEMP,KK(6),K
      DATA MSGOFF/3007/
      my_real WA(6*LENCOM)
      my_real ELBUF(10000)  ! contents not used
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
C-----------------------------------------------
C
C Updating X on adjacent domains
C 
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IDEB = 0
      IDEB2 = 0
      NBIRECV = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF 
        IAD_RECV(I) = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN = 6*NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1)   , LEN,REAL      , IT_SPMD(I) , MSGTYP,
     G        MPI_COMM_WORLD, REQ_R(NBIRECV), ierror)
          IDEB2 = IDEB2 + LEN
        ENDIF
      ENDDO
      IDEB = 0
      DO I = 1, NSPMD
        MSGTYP = MSGOFF
        LEN = NESDVOIS(I)
        IF(LEN>0) THEN
          KTY = -1
          KLT = -1
          MFT = -1
          DO N = 1, LEN
            II = IDEB2+6*(N-1)
            NN = LESDVOIS(IDEB+N)
            ! Searching in element buffer : sending if material law /= 11
            IF (N2D == 0) THEN
              ML=NINT(PM(19,IXS(1,NN)))
            ELSE
              ML=NINT(PM(19,IXQ(1,NN)))
            ENDIF

            IF (ML /= 11) THEN
              DO NI=1,NGROUP                                     
                GBUF => ELBUF_TAB(NI)%GBUF                       
                LBUF => ELBUF_TAB(NI)%BUFLY(1)%LBUF(1,1,1)       
                KTY = IPARG(5,NI)                                
                KLT = IPARG(2,NI)                                
                MFT = IPARG(3,NI)                                
                IF( (KTY == 1.OR.KTY == 2).AND.(NN<=KLT+MFT) .AND. NN>MFT) THEN  
                  G_PLA = ELBUF_TAB(NI)%GBUF%G_PLA                
                  G_RK  = ELBUF_TAB(NI)%GBUF%G_RK                 
                  L_RE  = ELBUF_TAB(NI)%BUFLY(1)%L_RE             
                  L_TEMP= ELBUF_TAB(NI)%BUFLY(1)%L_TEMP           
                  IS = NN-MFT
!
                  DO K=1,6
                    KK(K) = KLT*(K-1)
                  ENDDO
!
                  WA(II+1) =-(GBUF%SIG(KK(1)+IS)+GBUF%SIG(KK(2)+IS)+ GBUF%SIG(KK(3)+IS))*THIRD               
                  WA(II+2) = GBUF%EINT(IS)                        
                  WA(II+3) = GBUF%RHO(IS)                         
                  IF (G_PLA > 0) THEN                             
                    WA(II+4) = GBUF%PLA(IS)                       
                  ELSEIF (G_RK > 0) THEN                          
                    WA(II+4) = GBUF%RK(IS)                        
                  ELSE                                            
                    WA(II+4) = ZERO                               
                  ENDIF                                           
                  IF (L_TEMP > 0)THEN                             
                    WA(II+5) = LBUF%TEMP(IS)                      
                  ELSE                                            
                    WA(II+5) = ZERO                               
                  ENDIF                                           
                  IF (L_RE > 0) THEN                              
                    WA(II+6) = LBUF%RE(IS)                        
                  ELSE                                            
                    WA(II+6) = ZERO                               
                  ENDIF                                           
                EXIT                                         
                ENDIF                                            
              ENDDO!next NI                                              
            ELSE  ! ML == 11
              WA(II+1) = ZERO
              WA(II+2) = ZERO
              WA(II+3) = ZERO
              WA(II+4) = ZERO
              WA(II+5) = ZERO
              WA(II+6) = ZERO
            ENDIF
          ENDDO!next N
          CALL MPI_ISEND(
     S      WA(IDEB2+1)    ,LEN*6    ,REAL   ,IT_SPMD(I) , MSGTYP,
     G      MPI_COMM_WORLD ,REQ_S(I) ,IERROR)
          IDEB  = IDEB + LEN
          IDEB2 = IDEB2 + 6*LEN
        ENDIF
      ENDDO

      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I = IRINDEX(INDEX)
        IDEB2 = IAD_RECV(I)
        IDEB = (IDEB2-1)/6
        DO N = 1, NERCVOIS(I)
          JJ = IDEB2+6*(N-1)
          NN = LERCVOIS(IDEB+N)-NUMELS-NUMELQ
          LBVOIS(1,NN) = WA(JJ)
          LBVOIS(2,NN) = WA(JJ+1)
          LBVOIS(3,NN) = WA(JJ+2)
          LBVOIS(4,NN) = WA(JJ+3)
          LBVOIS(5,NN) = WA(JJ+4)
          LBVOIS(6,NN) = WA(JJ+5)
        ENDDO
      ENDDO

      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,ierror)
        ENDIF
      ENDDO
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  NISGE                         source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION NISGE(N,IA,IT)
C
      IMPLICIT NONE
      INTEGER N,IT
      INTEGER IA
      DIMENSION IA(N)
C
      INTEGER ISUM,INC,I,J,IDUMMY,ILEN,VLEN
C The above value is required for allowing streaming
      PARAMETER(VLEN=512)
      DIMENSION IDUMMY(VLEN)
C
      IF (N == 0) GOTO 10
      INC = 1
C
      DO I=1,N,VLEN
        ISUM = 0
        ILEN = MIN(VLEN,N-I+1)
#include "vectorize.inc"
        DO J=1,ILEN
          IDUMMY(J) = 0
        ENDDO
C
        DO J=1,ILEN
          IDUMMY(J) = INC
          IF (IA(I+J-1)>=IT) INC = 0
        ENDDO
C
#include "vectorize.inc"
        DO J=1,ILEN
          ISUM = ISUM + IDUMMY(J)
        ENDDO
C
        IF (INC/=1) THEn
          NISGE = I + ISUM - 1
          RETURN
        ENDIF
      ENDDO
C
10    NISGE = N + 1
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_EXALEW                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEW2                         source/ale/grid/alew2.F       
Chd|        ALEW4                         source/ale/grid/alew4.F       
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_EXALEW(WA,WB,IAD_ELEM,FR_ELEM,NALE,SIZE,LENR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------   
C Sum of grid velocities WA,WB at boundary nodes. Parith/off
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
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), NALE(*),
     .        SIZE, LENR
      my_real 
     .        WA(3,*),WB(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,SIZ,J, L
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      INTEGER IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),REQ_R(NSPMD),REQ_S(NSPMD), MSGOFF
      my_real RBUF(SIZE*LENR), SBUF(SIZE*LENR)
      DATA MSGOFF/3008/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
C SIZE = 6
      DO I=1,NSPMD
        SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
            L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L               
      END DO
C
      L = 1
      IAD_SEND(1) = L
      DO I=1,NSPMD
#include "vectorize.inc"
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(IABS(NALE(NOD)) == 1) THEN
            SBUF(L  ) = WA(1,NOD)
            SBUF(L+1) = WA(2,NOD)
            SBUF(L+2) = WA(3,NOD)
            SBUF(L+3) = WB(1,NOD)
            SBUF(L+4) = WB(2,NOD)
            SBUF(L+5) = WB(3,NOD)
            L = L + SIZE
          ENDIF
        ENDDO
        IAD_SEND(I+1) = L
      ENDDO
C
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF
      ENDDO        
C
C decompaction
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            IF(IABS(NALE(NOD)) == 1) THEN
              WA(1,NOD) = WA(1,NOD) + RBUF(L  )
              WA(2,NOD) = WA(2,NOD) + RBUF(L+1)
              WA(3,NOD) = WA(3,NOD) + RBUF(L+2)
              WB(1,NOD) = WB(1,NOD) + RBUF(L+3)
              WB(2,NOD) = WB(2,NOD) + RBUF(L+4)
              WB(3,NOD) = WB(3,NOD) + RBUF(L+5)
              L = L + SIZE
            END IF
          ENDDO         
        ENDIF
      END DO
C
C Wait terminaison isend
C
        DO I = 1, NSPMD
          IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)
     .     CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDDO
C
        CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_EXALEW_PON               source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEW2                         source/ale/grid/alew2.F       
Chd|        ALEW4                         source/ale/grid/alew4.F       
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_EXALEW_PON(
     1   FSKY  ,FSKYV ,IAD_ELEM,FR_ELEM,NALE,
     2   ADDCNE,PROCNE,FR_NBCC ,SIZE   ,LENR,
     3   LENS  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------     
C Sum of grid velocities WA,WB at boundary nodes. Parith/on
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),FR_NBCC(2,*),NALE(*),ADDCNE(*), PROCNE(*),SIZE, LENR ,LENS
      my_real FSKY(8,LSKY), FSKYV(LSKY,8)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,SIZ,J,L,CC,IERROR,NBIRECV,
     .        II, INDEX,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),IRINDEX(NSPMD),MSGOFF
      my_real RBUF(SIZE*LENR+1), SBUF(SIZE*LENS)
      DATA MSGOFF/3009/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD + 1
C SIZE = 6
C
      NBIRECV = 0
      L = 1     
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
          MSGTYP = MSGOFF
          SIZ = SIZE*FR_NBCC(2,I)
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_IRECV(
     S        RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
C
      L = 1
      IAD_SEND(1) = L
      DO I=1,NSPMD
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(IABS(NALE(NOD)) == 1) THEN
            IF(IVECTOR == 1) THEN
              DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                IF(PROCNE(CC) == LOC_PROC) THEN
                  SBUF(L)   = FSKYV(CC,1)
                  SBUF(L+1) = FSKYV(CC,2)
                  SBUF(L+2) = FSKYV(CC,3)
                  SBUF(L+3) = FSKYV(CC,4)
                  SBUF(L+4) = FSKYV(CC,5)
                  SBUF(L+5) = FSKYV(CC,6)
                  L = L + SIZE
                ENDIF
              ENDDO
            ELSE
              DO CC = ADDCNE(NOD),ADDCNE(NOD+1)-1
                IF(PROCNE(CC) == LOC_PROC) THEN
                  SBUF(L)   = FSKY(1,CC)    
                  SBUF(L+1) = FSKY(2,CC)
                  SBUF(L+2) = FSKY(3,CC)
                  SBUF(L+3) = FSKY(4,CC)
                  SBUF(L+4) = FSKY(5,CC)
                  SBUF(L+5) = FSKY(6,CC)
                  L = L + SIZE
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
        IAD_SEND(I+1) = L
      ENDDO
C
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     S        SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF
      ENDDO
C
C decompaction
C
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        L = IAD_RECV(I)
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(IABS(NALE(NOD)) == 1) THEN
            IF(IVECTOR == 1) THEN
              DO CC = ADDCNE(NOD), ADDCNE(NOD+1)-1
                IF(PROCNE(CC) == I) THEN
                  FSKYV(CC,1) = RBUF(L)
                  FSKYV(CC,2) = RBUF(L+1)
                  FSKYV(CC,3) = RBUF(L+2)
                  FSKYV(CC,4) = RBUF(L+3)
                  FSKYV(CC,5) = RBUF(L+4)
                  FSKYV(CC,6) = RBUF(L+5)
                  L = L + SIZE
                ENDIF
              ENDDO
            ELSE
              DO CC = ADDCNE(NOD), ADDCNE(NOD+1)-1
                IF(PROCNE(CC) == I) THEN
                  FSKY(1,CC) = RBUF(L)
                  FSKY(2,CC) = RBUF(L+1)
                  FSKY(3,CC) = RBUF(L+2)
                  FSKY(4,CC) = RBUF(L+3)
                  FSKY(5,CC) = RBUF(L+4)
                  FSKY(6,CC) = RBUF(L+5)
                  L = L + SIZE
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO

C
C Wait terminaison isend
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0) THEN
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GLOB_DMIN9               source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_DMIN9(V,LEN)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------  
C minimum of array V (length=LEN) of type my_real
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,K,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      my_real VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      IF (LEN > 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  REAL,MPI_MIN,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD == 0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_EXWALX                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXWALX(NSW,NIMP,X,WEIGHT,WA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIMP, NSW(*), WEIGHT(*)
      my_real
     .        X(3,*), WA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, J, L, N, LOC_PROC, N_LEN, N_LEN2, NOD, SIZ,
     .        MSGOFF, IERROR, MSGTYP, LEN, M1, NB_NOD,
     .        STATUS(MPI_STATUS_SIZE)       
      DATA MSGOFF/3010/
C-----------------------------------------------
C
C Exchange X on previous line of RWALE TYPE 2
C
      LOC_PROC = ISPMD+1
      N_LEN  = 4
      N_LEN2 = 3
      IF(ISPMD/=0) THEN
C
C X local sending
C
        L = 1
        DO J=1,NIMP
          M1=ABS(NSW(J))                      
          IF(WEIGHT(M1) == 1)THEN
            WA(L) = M1
            WA(L+1) = X(1,M1)
            WA(L+2) = X(2,M1)
            WA(L+3) = X(3,M1)
            L = L + N_LEN
          ENDIF
        ENDDO
        LEN = L - 1
C  
        MSGTYP = MSGOFF
        CALL MPI_SEND(
     S      WA,LEN,REAL,IT_SPMD(1),MSGTYP,
     G      MPI_COMM_WORLD,ierror)
C
C X global receiving
C  
        MSGTYP = MSGOFF
        LEN = N_LEN*NIMP
        CALL MPI_RECV(
     S      WA,LEN,REAL,IT_SPMD(1),MSGTYP,
     G      MPI_COMM_WORLD,status     ,ierror)
C
        L = 1
        DO J = 1, NIMP
          NOD = ABS(NSW(J))
          X(1,NOD) = WA(L+1)
          X(2,NOD) = WA(L+2)
          X(3,NOD) = WA(L+3)
          L = L + N_LEN2
        ENDDO
      ELSE
C
C Getting back X
C
        DO I = 2, NSPMD
          MSGTYP = MSGOFF 
          LEN = NIMP*N_LEN
          CALL MPI_RECV(
     S      WA,LEN,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,status     ,ierror)
          CALL MPI_GET_COUNT(STATUS,REAL,SIZ,ierror)
          NB_NOD = SIZ/N_LEN
C
          L = 1
          DO J = 1, NB_NOD
            NOD = NINT(WA(L))
            X(1,NOD) = WA(L+1)
            X(2,NOD) = WA(L+2)
            X(3,NOD) = WA(L+3)
            L = L + N_LEN
          ENDDO
        ENDDO
C
C Sending updated X
C
        L = 1
        DO J=1,NIMP
          M1=ABS(NSW(J))
          WA(L+1) = X(1,M1)
          WA(L+2) = X(2,M1)
          WA(L+3) = X(3,M1)
          L = L + N_LEN2
        ENDDO
        LEN = L - 1
C
        DO I = 2, NSPMD
          MSGTYP = MSGOFF 
          CALL MPI_SEND(
     S      WA,LEN,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,ierror)
        ENDDO

      ENDIF
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_EXTAG                    source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        RGWAT2                        source/interfaces/int09/rgwat2.F
Chd|        RGWAT3                        source/interfaces/int09/rgwat3.F
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE SPMD_EXTAG(NTAG,IAD_ELEM,FR_ELEM,LENR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------  
C Sum of tags
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
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), SIZE, LENR, NTAG(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,NB_NOD,
     .        SIZ,J, L,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        RBUF(LENR), SBUF(LENR) ,MSGOFF
       DATA MSGOFF/3011/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD + 1
C
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L               
      END DO
C
      L = 1
      IAD_SEND(1) = L
      DO I=1,NSPMD
#include "vectorize.inc"
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(NTAG(NOD)>0) THEN
C           removing initial tag already treated locally
            SBUF(L) =  NTAG(NOD)-1
          ELSE
            SBUF(L) =  0
          END IF
          L=L+1
        END DO
        IAD_SEND(I+1) = L
      ENDDO
C
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF
      ENDDO        
C
C decompaction
C
      DO I = 1, NSPMD
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            NTAG(NOD) = NTAG(NOD)+RBUF(L)
            L = L + 1
          ENDDO         
        ENDIF
      END DO
C Wait terminaison isend
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)
     .     CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SEGCOM                   source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE SPMD_SEGCOM(SEGVAR,NPSEGCOM,LSEGCOM,SIZE,FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SEGVAR_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPSEGCOM(*), LSEGCOM(*), FLAG, SIZE
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD), IRINDEX(NSPMD),
     .        LOC_PROC, N, KK, NN, NBIRECV, II, INDEX, LEN      
      DATA MSGOFF/3012/
      my_real WA(SIZE*ALE%GLOBAL%NVCONV)
C-----------------------------------------------
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD+1
      IF(FLAG == 0) THEN  
C
C Sending segvar from pi to p0
C
        IF(LOC_PROC/=1)THEN
          MSGTYP = MSGOFF 
          LEN = NPSEGCOM(1)
          IF(LEN>0) THEN
            DO N = 1, LEN
              KK = LSEGCOM(N)
              
              NN=1
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rho)THEN
               WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%RHO(KK)
              ENDIF
              
              NN=2              
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_eint)THEN
                WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%EINT(KK)
              ENDIF
              
              NN=3
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rk)THEN
                WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%RK(KK)
              ENDIF
              
              NN=4
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_re)THEN
                WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%RE(KK)
              ENDIF
              
              NN=5
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_uvar)THEN
                WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%UVAR(KK)                                                        
              ENDIF
             
            ENDDO
            CALL MPI_SEND(
     S        WA            ,LEN*ALE%GLOBAL%NVCONV,REAL,IT_SPMD(1),MSGTYP,
     G        MPI_COMM_WORLD,IERROR    )
          ENDIF
C
        ELSE
          IDEB = 0
          NBIRECV = 0
          DO I = 2, NSPMD
            MSGTYP = MSGOFF
            IAD_RECV(I) = IDEB+1
            IF(NPSEGCOM(I)>0) THEN
              NBIRECV = NBIRECV + 1
              IRINDEX(NBIRECV) = I
              LEN = ALE%GLOBAL%NVCONV*NPSEGCOM(I)
              CALL MPI_IRECV(
     S          WA(IDEB+1),LEN,REAL,IT_SPMD(I),MSGTYP,
     G          MPI_COMM_WORLD,REQ_R(NBIRECV),ierror)
              IDEB = IDEB + LEN
            ENDIF
          ENDDO
C
          IDEB = 0
          DO II = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
            I = IRINDEX(INDEX)
            IDEB2 = IAD_RECV(I)-1
            IDEB = IDEB2 / ALE%GLOBAL%NVCONV
            DO N = 1, NPSEGCOM(I)
              KK = LSEGCOM(IDEB+N)
              
              NN=1
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rho)THEN
               SEGVAR%RHO(KK) = WA(IDEB2+ALE%GLOBAL%NVCONV*(N-1)+NN) 
              ENDIF
              
              NN=2              
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_eint)THEN
                SEGVAR%EINT(KK) = WA(IDEB2+ALE%GLOBAL%NVCONV*(N-1)+NN)
              ENDIF
              
              NN=3
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rk)THEN
                SEGVAR%RK(KK) = WA(IDEB2+ALE%GLOBAL%NVCONV*(N-1)+NN)
              ENDIF
              
              NN=4
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_re)THEN
                SEGVAR%RE(KK) = WA(IDEB2+ALE%GLOBAL%NVCONV*(N-1)+NN)
              ENDIF
              
              NN=5
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_uvar)THEN
                SEGVAR%UVAR(KK) = WA(IDEB2+ALE%GLOBAL%NVCONV*(N-1)+NN)                                                       
              ENDIF
                            
            ENDDO
          ENDDO
        ENDIF
C
      ELSE
C
C Sending segvar from p0 to pi
C
        IF(LOC_PROC/=1)THEN
          MSGTYP = MSGOFF 
          LEN = NPSEGCOM(1)
          IF(LEN>0) THEN
            CALL MPI_RECV(
     S        WA,LEN*ALE%GLOBAL%NVCONV,REAL,IT_SPMD(1),MSGTYP,
     G        MPI_COMM_WORLD,status,ierror)
            DO N = 1, LEN
              KK = LSEGCOM(N)
                            
              NN=1
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rho)THEN
               SEGVAR%RHO(KK) = WA(ALE%GLOBAL%NVCONV*(N-1)+NN) 
              ENDIF
              
              NN=2              
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_eint)THEN
                SEGVAR%EINT(KK) = WA(ALE%GLOBAL%NVCONV*(N-1)+NN)
              ENDIF
              
              NN=3
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rk)THEN
                SEGVAR%RK(KK) = WA(ALE%GLOBAL%NVCONV*(N-1)+NN)
              ENDIF
              
              NN=4
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_re)THEN
                SEGVAR%RE(KK) = WA(ALE%GLOBAL%NVCONV*(N-1)+NN)
              ENDIF
              
              NN=5
              IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_uvar)THEN
                SEGVAR%UVAR(KK) = WA(ALE%GLOBAL%NVCONV*(N-1)+NN)                                                       
              ENDIF              
                            
            ENDDO
          ENDIF
C
        ELSE
          IDEB = 0
          DO I = 2, NSPMD
            LEN = NPSEGCOM(I)
            IF(LEN>0) THEN
              MSGTYP = MSGOFF
              DO N = 1, LEN
                KK = LSEGCOM(IDEB+N)
                
                NN=1
                IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rho)THEN
                  WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%RHO(KK)
                ENDIF
                
                NN=2              
                IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_eint)THEN
                   WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%EINT(KK)
                ENDIF
                
                NN=3
                IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_rk)THEN
                   WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%RK(KK)
                ENDIF
                
                NN=4
                IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_re)THEN
                   WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%RE(KK)
                ENDIF
                
                NN=5
                IF(NN <= ALE%GLOBAL%NVCONV .AND. SEGVAR%has_uvar)THEN
                   WA(ALE%GLOBAL%NVCONV*(N-1)+NN) = SEGVAR%UVAR(KK)                                                       
                ENDIF
              
                              
              ENDDO
              CALL MPI_SEND(
     S        WA            ,LEN*ALE%GLOBAL%NVCONV,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,IERROR    )
              IDEB = IDEB + LEN
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_INIT_EBCS                source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_INIT_EBCS(V,ISIZXV,IAD_ELEM,FR_ELEM,EBCS_TAB)
      USE EBCS_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), ISIZXV  
      my_real V(3,*)
      TYPE(t_ebcs_tab), INTENT(IN) :: EBCS_TAB
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I, NOD,LOC_PROC, IERROR, MSGOFF,
     .        SIZ, L, E_LEN, ICOMV, KK, TYP, J,
     .        STATUS(MPI_STATUS_SIZE)
      my_real WA(3*ISIZXV)
      DATA MSGOFF/3013/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      LOC_PROC = ISPMD + 1
      E_LEN=3
      IF(ISPMD == 0) THEN
        ICOMV = 0
        DO I=1,NEBCS
          KK=(I-1)*NPEBC
          TYP =EBCS_TAB%tab(I)%poly%type
          IF(TYP == 4.OR.TYP == 5) ICOMV = 1
        ENDDO
        CALL SPMD_IBCAST(ICOMV,ICOMV,1,1,0,2)
        IF(ICOMV == 0) RETURN
C
        L = 0
C
        DO I=1,NSPMD
#include      "vectorize.inc"
          DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            WA(L+1) =  V(1,NOD)
            WA(L+2) =  V(2,NOD)
            WA(L+3) =  V(3,NOD)
            L = L + E_LEN
          END DO
        ENDDO
C
C   exchanging messages
C
        L = 1
        DO I=1,NSPMD
C----------------------------------------------------------------------------
          IF(IAD_ELEM(1,I+1)-IAD_ELEM(2,I)>0)THEN
            MSGTYP = MSGOFF
            SIZ = E_LEN*(IAD_ELEM(1,I+1)-IAD_ELEM(2,I))
            CALL MPI_SEND(
     S        WA(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,IERROR)
            L = L + SIZ
          ENDIF        
        ENDDO
C
C decompaction
C
      ELSE
        CALL SPMD_IBCAST(ICOMV,ICOMV,1,1,0,2)
        IF(ICOMV == 0) RETURN
C
        SIZ = E_LEN*(IAD_ELEM(1,2)-IAD_ELEM(2,1))
        IF(SIZ/=0)THEN
          L = 0
          MSGTYP = MSGOFF
          CALL MPI_RECV(
     S      WA,SIZ,REAL,IT_SPMD(1),MSGTYP,
     G      MPI_COMM_WORLD,STATUS,IERROR)
#include      "vectorize.inc"
          DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
            NOD = FR_ELEM(J)
            V(1,NOD)  =  WA(L+1)
            V(2,NOD)  =  WA(L+2)
            V(3,NOD)  =  WA(L+3)
            L = L + E_LEN
          END DO
        ENDIF
      ENDIF
C
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END
      
Chd|====================================================================
Chd|  SPMD_L51VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_L51VOIS(
     1     LBVOIS  ,IPARG    ,ELBUF_TAB ,PM       ,IXS     ,
     2     IXQ     ,NERCVOIS ,NESDVOIS  ,LERCVOIS ,LESDVOIS,
     3     LENCOM  ,IPM      ,BUFMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      
      INTEGER                  :: IPARG(NPARG,*),IXS(NIXS,*), IXQ(NIXQ,*),
     .                            NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*),
     .                            LENCOM, IPM(NPROPMI,*)
      my_real                  :: LBVOIS(M51_IFLG6_SIZE,*), PM(NPROPM,*), BUFMAT(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER                  :: I,II,JJ,IDEB,IDEB2,MSGOFF,IERROR,MSGTYP,IAD_RECV(NSPMD),
     .                            STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .                            LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), INDEX,
     .                            LEN, ML, NI, KTY, KLT, MFT, IS,
     .                            KB1, KB2, KB3 ,KB4 ,KB10, KB11, KB12, KKB2,
     .                            G_PLA,G_RK,L_RE,L_TEMP,IMAT,IADBUF,IFLG,NELG,ITRIMAT,KK,KJ(6),K

      DATA MSGOFF/3014/
      
      my_real                  :: WA(M51_IFLG6_SIZE*LENCOM), ELBUF(10000)   !necessary size : M51_IFLG6_SIZE

      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_MAT_)  ,POINTER :: MBUF               
C-----------------------------------------------
       CALL STARTIME(MACRO_TIMER_SPMDCFD,1)

      ! X on remote nodes
      LOC_PROC             = ISPMD+1
      IDEB                 = 0
      IDEB2                = 0
      NBIRECV              = 0
      DO I = 1, NSPMD
        MSGTYP             = MSGOFF 
        IAD_RECV(I)        = IDEB2+1
        IF(NERCVOIS(I)>0) THEN
          NBIRECV          = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          LEN              = M51_IFLG6_SIZE*NERCVOIS(I)
          CALL MPI_IRECV(
     S        WA(IDEB2+1)    , LEN            ,REAL  , IT_SPMD(I) , MSGTYP,
     G        MPI_COMM_WORLD , REQ_R(NBIRECV) ,ierror
     .                  )
          IDEB2            = IDEB2 + LEN
        ENDIF
      ENDDO
      IDEB      = 0
      DO I = 1, NSPMD
        MSGTYP  = MSGOFF
        LEN     = NESDVOIS(I)
        IF(LEN>0) THEN
          KTY   = -1
          KLT   = -1
          MFT   = -1
          DO N  = 1, LEN
            II  = IDEB2+M51_IFLG6_SIZE*(N-1)
            NN  = LESDVOIS(IDEB+N)
            IF (N2D == 0) THEN
              ML   = NINT(PM(19,IXS(1,NN)))
              IMAT = IXS(1,NN)
            ELSE
              ML   = NINT(PM(19,IXQ(1,NN)))
              IMAT = IXQ(1,NN)              
            ENDIF
            IADBUF = IPM(7,IMAT)
            IFLG   = 6
            IF(ML==51)IFLG   = NINT(BUFMAT(IADBUF-1+31)) 
            ! SEARCH FOR REMOTE ELEM DATA TO SEND
            IF (IFLG /= 6) THEN
                DO NI=1,NGROUP
                  GBUF        => ELBUF_TAB(NI)%GBUF
                  LBUF        => ELBUF_TAB(NI)%BUFLY(1)%LBUF(1,1,1)
                  MBUF        => ELBUF_TAB(NI)%BUFLY(1)%MAT(1,1,1)
                  LBUF        => ELBUF_TAB(NI)%BUFLY(1)%LBUF(1,1,1)
                  KTY         = IPARG(5,NI)
                  KLT         = IPARG(2,NI)
                  MFT         = IPARG(3,NI)
                  IF( (KTY    == 1.OR.KTY == 2).AND.(NN<=KLT+MFT) .AND. NN>MFT) THEN
                   L_TEMP     = ELBUF_TAB(NI)%BUFLY(1)%L_TEMP
                   IS         = NN-MFT
                   DO K=1,6
                     KJ(K) = KLT*(K-1)
                   ENDDO
                   !-------------GLOBAL DATA------------------------------------------!
                   WA(II+1)   = -(GBUF%SIG(KJ(1)+IS)+GBUF%SIG(KJ(2)+IS)+GBUF%SIG(KJ(3)+IS))*THIRD           
                   WA(II+2)   = GBUF%EINT(IS)                    
                   WA(II+3)   = GBUF%RHO(IS)                                   
                   IF (L_TEMP > 0)THEN                         
                     WA(II+4) = LBUF%TEMP(IS)                  
                   ELSE                                        
                     WA(II+4) = ZERO                           
                   ENDIF
                   WA(II+5)   = LBUF%SSP(IS)
                   WA(II+6)   = ZERO 
                   IF(ELBUF_TAB(NI)%BUFLY(1)%L_PLA > 0)WA(II+6)   = LBUF%PLA(IS) 
                   !-------------SUBMATERIAL STATE------------------------------------!                
                   !Submaterial Data
                   NELG       = KLT
                   ITRIMAT    = 1
                   KK         = N0PHAS + (ITRIMAT-1)*NVPHAS 
                   IADBUF=18 ; WA(II+07) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !PRES
                   IADBUF=1  ; WA(II+08) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !VFRAC
                   IADBUF=8  ; WA(II+09) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !ENER
                   IADBUF=9  ; WA(II+10) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !DENS
                   IADBUF=16 ; WA(II+11) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !TEMP
                   IADBUF=14 ; WA(II+12) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !SSP
                   IADBUF=15 ; WA(II+13) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !EPSP
                   ITRIMAT    = 2
                   KK         = N0PHAS + (ITRIMAT-1)*NVPHAS 
                   IADBUF=18 ; WA(II+14) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !PRES
                   IADBUF=1  ; WA(II+15) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !VFRAC
                   IADBUF=8  ; WA(II+16) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !ENER
                   IADBUF=9  ; WA(II+17) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !DENS
                   IADBUF=16 ; WA(II+18) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !TEMP
                   IADBUF=14 ; WA(II+19) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !SSP
                   IADBUF=15 ; WA(II+20) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !EPSP
                   ITRIMAT    = 3
                   KK         = N0PHAS + (ITRIMAT-1)*NVPHAS 
                   IADBUF=18 ; WA(II+21) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !PRES
                   IADBUF=1  ; WA(II+22) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !VFRAC
                   IADBUF=8  ; WA(II+23) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !ENER
                   IADBUF=9  ; WA(II+24) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !DENS
                   IADBUF=16 ; WA(II+25) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !TEMP 
                   IADBUF=14 ; WA(II+26) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !SSP
                   IADBUF=15 ; WA(II+27) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !EPSP
                   ITRIMAT    = 4
                   KK         = N0PHAS + (ITRIMAT-1)*NVPHAS 
                   IADBUF=18 ; WA(II+28) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !PRES
                   IADBUF=1  ; WA(II+29) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !VFRAC
                   IADBUF=8  ; WA(II+30) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !ENER
                   IADBUF=9  ; WA(II+31) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !DENS
                   IADBUF=16 ; WA(II+32) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !TEMP
                   IADBUF=14 ; WA(II+33) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !SSP 
                   IADBUF=15 ; WA(II+34) = MBUF%VAR(NELG*(IADBUF+KK-1)+IS) !EPSP 
                   !
                   WA(II+35) = MBUF%VAR(NELG*3+IS) !UVAR(4,I)
                   WA(II+36) = IFLG    
                   IADBUF = IPM(7,IMAT)  
                   WA(II+37) = 51 +    100*NINT(BUFMAT(IADBUF-1+276+4))                     
     .                            +   1000*NINT(BUFMAT(IADBUF-1+276+3)) 
     .                            +  10000*NINT(BUFMAT(IADBUF-1+276+2)) 
     .                            + 100000*NINT(BUFMAT(IADBUF-1+276+1))           
                   !-------------REMAINING INDEXES------------------------------------!                     
                   !WA(II+36:II+36)       = ZERO                    
                   EXIT
                 ENDIF
                ENDDO
            ELSE  ! iflg = 6
              WA(II+1:II+M51_IFLG6_SIZE) = ZERO
              WA(II+36)                  = 6
              WA(II+37)                  = ML
            ENDIF
          ENDDO
          CALL MPI_ISEND(
     S      WA(IDEB2+1),LEN*M51_IFLG6_SIZE,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          IDEB  = IDEB + LEN
          IDEB2 = IDEB2 + M51_IFLG6_SIZE*LEN
        ENDIF
      ENDDO
      IDEB = 0
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
        I                 = IRINDEX(INDEX)
        IDEB2             = IAD_RECV(I)
        IDEB              = (IDEB2-1)/M51_IFLG6_SIZE
        DO N = 1, NERCVOIS(I)
          JJ              = IDEB2+M51_IFLG6_SIZE*(N-1)
          NN              = LERCVOIS(IDEB+N)-NUMELS-NUMELQ
          LBVOIS(1:M51_IFLG6_SIZE,NN) = WA(JJ+0:JJ+M51_IFLG6_SIZE-1)
        ENDDO
      ENDDO
      DO I = 1, NSPMD
        IF(NESDVOIS(I)>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,ierror)
        ENDIF
      ENDDO
      CALL STOPTIME(MACRO_TIMER_SPMDCFD,1)

#endif
      RETURN
      END

