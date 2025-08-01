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
!||    h3d_create_rbodies_impi     ../engine/source/output/h3d/h3d_build_fortran/h3d_create_rbodies_impi.F
!||--- called by ------------------------------------------------------
!||    genh3d                      ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- calls      -----------------------------------------------------
!||    c_h3d_create_rbodies_impi   ../engine/source/output/h3d/h3d_build_cpp/c_h3d_create_rbodies.cpp
!||    spmd_glob_isum9             ../engine/source/mpi/interfaces/spmd_th.F
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod         ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE H3D_CREATE_RBODIES_IMPI(NPBY,LPBY,FR_RBY2,IAD_RBY2,
     .                        SBUFSPM,SBUFRECVM,
     .                        SBUFSPO,SPORBY,
     .                        NODGLOB,WEIGHT,ITAB,COMPID_RBODIES)


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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*),FR_RBY2(3,*),IAD_RBY2(4,*)
      INTEGER SBUFSPM,SBUFRECVM,SBUFSPO,NODGLOB(*),SPORBY,WEIGHT(*),
     .        ITAB(*),COMPID_RBODIES
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER PMAIN,JENVOIE,I,J,K,L,S,B,M,P,N,
     .        RECOISDE(NSPMD),
     .        PORBY(SPORBY),II(2),PTRPO(NSPMD+1),PTRPOO(NSPMD+1)
C
      INTEGER BUFSPM(SBUFSPM),BUFRECVM(SBUFRECVM+NSPMD+1),
     .        BUFSEND(NSPMD+1),BUFRECP(NSPMD+1),

     .        BUFSPO(SBUFSPO),NBNOD,SIZ,LPO,NSN,PTR,NOD,NN,NR,
     .        SRBY
      INTEGER MAINND(NRBYKIN)
      INTEGER ID_RBY(NRBYKIN)

C   MPI variables
      INTEGER LOC_PROC
      INTEGER MSGOFF,MSGOFF2,MSGTYP,INFO,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,ISD(NSPMD)

      DATA MSGOFF/7018/
      DATA MSGOFF2/7019/

      LOC_PROC = ISPMD + 1

C SEND main NODES TO PROC 0
      DO J=1,NRBYKIN
        PMAIN = FR_RBY2(3,J)
        IF (LOC_PROC==ABS(PMAIN))THEN
          MAINND(J)=ITAB( NPBY(1,J) )-1
        ELSE
          MAINND(J)= 0
        ENDIF
        ID_RBY(J)= NPBY(6,J)
      ENDDO

      CALL SPMD_GLOB_ISUM9(MAINND,NRBYKIN)
      DO I=1,SBUFRECVM
        BUFRECVM(I)=0
      ENDDO
C   Fill send buffer: second -> main

      L = 1

      DO I=1,NSPMD

        BUFSEND(I)=L
        S = 1
C
        DO J=1,NRBYKIN

          PMAIN = FR_RBY2(3,J)
          NBNOD =  FR_RBY2(1,J)

          IF ( NBNOD/=0  .AND.
     .        ABS(PMAIN)==I .AND. LOC_PROC/=I) THEN

            BUFSPM(L) = J
            BUFSPM(L+1) = NBNOD
            L = L + 2
            NR = 1
            DO K=1,NPBY(2,J)
              IF (WEIGHT(LPBY(K+S-1))==1) THEN
                BUFSPM(L+NR-1) = ITAB(LPBY(K+S-1))-1
                NR = NR +1
              ENDIF
            ENDDO
            L = L+NBNOD
          ENDIF
          S = S + NPBY(2,J)
        ENDDO
      ENDDO
      BUFSEND(NSPMD+1)=L

C   Send buffer
      DO I=1,NSPMD

        IF (IAD_RBY2(1,I)>0) THEN

          MSGTYP = MSGOFF
          B = BUFSEND(I)
          SIZ = BUFSEND(I+1)-BUFSEND(I)
          CALL MPI_ISEND(BUFSPM(B),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .       SPMD_COMM_WORLD,ISD(I),ierror)

        ENDIF
      ENDDO

C   Processor that owns main node: receive secondary nodes
      L=1
      DO I = 1, NSPMD

        BUFRECP(I)=L
        IF (IAD_RBY2(2,I)>0) THEN

          MSGTYP = MSGOFF
          CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)
          CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

          CALL MPI_RECV(BUFRECVM(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                SPMD_COMM_WORLD,STATUS,ierror)

          L = L + SIZ
          BUFRECVM(L)=0
          L=L+1
        ENDIF
      ENDDO
      DO I=1,NSPMD

        IF (IAD_RBY2(1,I)>0) THEN
          CALL MPI_WAIT(ISD(I),STATUS,IERROR)
        ENDIF
      ENDDO
      BUFRECP(NSPMD+1)=L
C   Packing
      L = 0
      K = 1

      DO I=1,NRBYKIN
        PMAIN = FR_RBY2(3,I)

        IF (ABS(PMAIN)==LOC_PROC) THEN

          NBNOD = FR_RBY2(1,I)
          NN = L+1
          L = L+2
          NR = 1

          DO J = 1,NPBY(2,I)
            IF (PMAIN<=0) THEN
              BUFSPO(L+NR)=ITAB(LPBY(K+J-1))-1
              NR = NR+1
            ELSE
              IF (WEIGHT(LPBY(K+J-1)) ==1) THEN
                BUFSPO(L+NR)=ITAB(LPBY(K+J-1))-1
                NR = NR+1
              ENDIF
            ENDIF
          ENDDO

          L=L+NR-1

          SRBY = NR-1

          IF (PMAIN>0) THEN
            DO P=1,NSPMD

              IF (IAD_RBY2(2,P)>0) THEN
                M = BUFRECP(P)
                IF (BUFRECVM(M)==I) THEN

                  NBNOD=BUFRECVM(M+1)
                  BUFRECP(P)=BUFRECP(P)+2
                  NR = 1
                  DO J=BUFRECP(P),BUFRECP(P)+NBNOD-1
                    BUFSPO(L+NR)=BUFRECVM(J)
                    NR=NR+1
                  ENDDO
                  L = L+NR-1
                  SRBY = SRBY + NR-1
                  BUFRECP(P)=BUFRECP(P)+NBNOD
                ENDIF
              ENDIF
            ENDDO
          ENDIF
          BUFSPO(NN)=I
          BUFSPO(NN+1)=SRBY
        ENDIF
        K =K+NPBY(2,I)

      ENDDO
      IF (ISPMD/=0 .AND .L>0) THEN

        MSGTYP = MSGOFF2
        CALL MPI_SEND(BUFSPO,L,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .       SPMD_COMM_WORLD,ierror)
      ENDIF


C   Receive from processors that have one main node of RB
      IF (ISPMD==0) THEN
        DO I=1,NSPMD
          RECOISDE(I)=0
        ENDDO
        DO I=1,NRBYKIN
          RECOISDE(ABS(FR_RBY2(3,I)))=1
        ENDDO

        LPO=1
        PTRPO(1)=LPO
        DO I=1,L
          PORBY(I)=BUFSPO(I)
        ENDDO
        LPO = LPO+L

        DO I=2,NSPMD

          IF (RECOISDE(I)==1) THEN
            MSGTYP = MSGOFF2
            PTRPO(I) = LPO
            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                   SPMD_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(PORBY(LPO),SIZ,MPI_INTEGER,IT_SPMD(I),
     .                MSGTYP, SPMD_COMM_WORLD,STATUS,ierror)
            LPO=LPO+SIZ
          ELSE
            PTRPO(I) = LPO
          ENDIF
        ENDDO
        PTRPO(NSPMD+1)=LPO

        PTRPOO=PTRPO
        CALL C_H3D_CREATE_RBODIES_IMPI(ITAB,NRBYKIN,MAINND,ID_RBY,PTRPO,PTRPOO,PORBY,NSPMD,
     .                                 COMPID_RBODIES)




      ENDIF
#endif
      RETURN
      END
