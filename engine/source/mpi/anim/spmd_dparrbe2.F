Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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

!||====================================================================
!||    spmd_dparrbe2         ../engine/source/mpi/anim/spmd_dparrbe2.F
!||--- called by ------------------------------------------------------
!||    genani                ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_glob_isum9       ../engine/source/mpi/interfaces/spmd_th.F
!||    write_i_c             ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_DPARRBE2(LRBE2, IRBE2,NODGLOB,WEIGHT,NERBE2Y,
     *                         NERBE2T )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),NODGLOB(*),WEIGHT(*),
     * NERBE2Y,NERBE2T(NRBE2G)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,N,P
      INTEGER SNRBE2,SIZRBE2,SBUFSIZ,PSNRBE2
      INTEGER NSN,IADG,IAD,SN,MN,NGRBE

      INTEGER,  DIMENSION(:),ALLOCATABLE :: SZLOCRBE2,PGLOBRBE2,MAINNODS
      INTEGER,  DIMENSION(:),ALLOCATABLE :: SENDBUF,RECBUF,
     *                                      P0RBE2BUF,IADRBE2
      INTEGER,  DIMENSION(:,:),ALLOCATABLE :: P0RECRBE2, IIN

C MPI variables 
      INTEGER MSGOFF,MSGOFF2,MSGTYP
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      DATA MSGOFF/7020/
      DATA MSGOFF2/7021/
C-----------------------------------------------
      ALLOCATE(SZLOCRBE2(NRBE2G))
      ALLOCATE(PGLOBRBE2(NRBE2G))
      ALLOCATE(MAINNODS(NRBE2G))
C-----------------------------------------------
C     1st step - send to pro 0 a table with number
C                  Local send nodes by Rbe2 to send
C                  and preparation of the send buffer
C                  (taille)
      NERBE2T = 0
      SNRBE2 = 0
      SBUFSIZ = 0
      SZLOCRBE2=0
      PGLOBRBE2 = 0

      DO I=1,NRBE2
          NGRBE = IRBE2(10,I)
          SZLOCRBE2(NGRBE) = 0
          NSN = IRBE2(5,I)
          DO N=1,NSN
           IF (WEIGHT(LRBE2(IRBE2(1,I)+N))==1)
     .       SZLOCRBE2(NGRBE) = SZLOCRBE2(NGRBE)  + 1
          ENDDO
          SBUFSIZ = SBUFSIZ + SZLOCRBE2(NGRBE)

      ENDDO

C Sending to proc 0 of the size table

      IF (ISPMD == 0) THEN
C Procement reception of sizes
        ALLOCATE(P0RECRBE2(NRBE2G,NSPMD))
        DO I=1,NRBE2G
           P0RECRBE2(I,1) = SZLOCRBE2(I)
        ENDDO

        DO P=2,NSPMD
           MSGTYP = MSGOFF
           CALL MPI_RECV(P0RECRBE2(1,P),NRBE2G,MPI_INTEGER,IT_SPMD(P),
     *                   MSGTYP,SPMD_COMM_WORLD,STATUS,IERROR)
        ENDDO

      ELSE
C Procs Other shipments
        MSGTYP = MSGOFF
        CALL MPI_SEND(SZLOCRBE2,NRBE2G,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,SPMD_COMM_WORLD,IERROR)

      ENDIF

C --------------------------------------------------------------
C Sending to proc 0 of the nodes of RBE2 & writing to disk
C --------------------------------------------------------------
      IF (ISPMD /= 0) THEN
C ------------------------
C Procs other than pro 0
C ------------------------
         ALLOCATE(SENDBUF(SBUFSIZ))
         SNRBE2 = 0
         DO I=1,NRBE2
           NSN = IRBE2(5,I)
           IAD = IRBE2(1,I)
           DO N=1,NSN
              SN = LRBE2(IAD+N)
              IF (WEIGHT(SN) == 1 )THEN
                SNRBE2 = SNRBE2+1
                SENDBUF(SNRBE2)=NODGLOB(SN)
              ENDIF
           ENDDO
         ENDDO
         IF (SNRBE2 > 0)THEN
           MSGTYP = MSGOFF2
           CALL  MPI_SEND(SENDBUF,SNRBE2,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     *                  SPMD_COMM_WORLD,IERROR)
         ENDIF
         DEALLOCATE(SENDBUF)

C Sending of secondary nodes
        MAINNODS = 0
        DO I=1,NRBE2
          MN = IRBE2(3,I)
            IF(MN/=0)THEN
            IF (WEIGHT(MN)==1)THEN
              NGRBE = IRBE2(10,I)
              MAINNODS(NGRBE)=NODGLOB(MN)
            ENDIF
            ENDIF
        ENDDO
        CALL SPMD_GLOB_ISUM9(MAINNODS,NRBE2G)
        

      ELSE
C --------------------------------------------------------------------
C PROC 0
C --------------------------------------------------------------------
C P0RBE2BUF reception table (reception table = LRBE2 Global)
C IADRBE2 Pointers to P0RBE2BUF Global
         ALLOCATE(IADRBE2(NRBE2G+1))
         ALLOCATE(P0RBE2BUF(NERBE2Y))

C preparation IADRBE2
         IADRBE2(1)=0
         DO I=1,NRBE2G
           SNRBE2 = P0RECRBE2(I,1)
           DO N=2,NSPMD
             SNRBE2 = SNRBE2 + P0RECRBE2(I,N)
           ENDDO
           IADRBE2(I+1)=IADRBE2(I)+SNRBE2
         ENDDO

C preparation P0RECRBE2 for proc0
         DO I=1,NRBE2G
              PGLOBRBE2(I)=IADRBE2(I)
         ENDDO
           
         DO I=1,NRBE2
           NSN = IRBE2(5,I)
           IAD = IRBE2(1,I)
           NGRBE = IRBE2(10,I)
           IADG = IADRBE2(NGRBE)
           SNRBE2 = 0
           DO N=1,NSN
             SN = LRBE2( IAD+N )
             IF (WEIGHT(SN) == 1 )THEN
               SNRBE2 = SNRBE2+1
               P0RBE2BUF(IADG + SNRBE2) = NODGLOB(SN)
             ENDIF
           ENDDO
           PGLOBRBE2(NGRBE)=PGLOBRBE2(NGRBE) + SNRBE2
         ENDDO

C Reception of RBE2 from other procs
         DO P=2,NSPMD
C Size of the reception buffer
           SIZRBE2 = 0
           DO I=1,NRBE2G
             SIZRBE2 = SIZRBE2 + P0RECRBE2(I,P)
           ENDDO

           IF (SIZRBE2 > 0) THEN
             ALLOCATE(RECBUF(SIZRBE2))
             MSGTYP = MSGOFF2 
             CALL MPI_RECV(RECBUF,SIZRBE2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     *                   SPMD_COMM_WORLD,STATUS,IERROR)

             PSNRBE2=0
             DO I=1,NRBE2G
               IADG = PGLOBRBE2(I)
               DO N=1,P0RECRBE2(I,P)
                 PSNRBE2 = PSNRBE2 + 1
                 P0RBE2BUF(IADG + N) = RECBUF(PSNRBE2)
               ENDDO
               PGLOBRBE2(I) = PGLOBRBE2(I) + P0RECRBE2(I,P)
             ENDDO
             DEALLOCATE(RECBUF)
           ENDIF
         ENDDO
C  Reception of main Nodes
         MAINNODS=0
         DO I=1,NRBE2
           MN = IRBE2(3,I)
           IF (WEIGHT(MN)==1) THEN
             NGRBE = IRBE2(10,I)
             MAINNODS(NGRBE)=NODGLOB(MN)
           ENDIF        
         ENDDO
         CALL SPMD_GLOB_ISUM9(MAINNODS,NRBE2G)

C Writing to disk
         DO I=1,NRBE2G
           NSN = IADRBE2(I+1) - IADRBE2(I)
           IADG =IADRBE2(I)
           MN = MAINNODS(I)
           ALLOCATE(IIN(2,NSN))
           NERBE2T(I)=NSN
           DO N=1,NSN
             IIN(1,N)=MN-1
             IIN(2,N)=P0RBE2BUF(IADG + N)-1
           ENDDO
           CALL WRITE_I_C(IIN,2*NSN)
           DEALLOCATE(IIN)
         ENDDO
         DEALLOCATE(IADRBE2)
         DEALLOCATE(P0RBE2BUF)
         DEALLOCATE(P0RECRBE2)
      ENDIF


      DEALLOCATE(SZLOCRBE2)
      DEALLOCATE(PGLOBRBE2)
      DEALLOCATE(MAINNODS)

#endif
      RETURN
      END
