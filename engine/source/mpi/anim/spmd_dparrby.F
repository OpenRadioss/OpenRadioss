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
Chd|  SPMD_DPARRBY                  source/mpi/anim/spmd_dparrby.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE SPMD_DPARRBY(NPBY,LPBY,FR_RBY2,IAD_RBY2,
     .                        SBUFSPM,SBUFRECVM,
     .                        SBUFSPO,SPORBY,
     .                        NODGLOB,WEIGHT,ITAB)


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
      INTEGER NPBY(NNPBY,*),LPBY(*),FR_RBY2(3,*),IAD_RBY2(4,*)
      INTEGER SBUFSPM,SBUFRECVM,SBUFSPO,NODGLOB(*),SPORBY,WEIGHT(*),
     .        ITAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER PMAIN,JENVOIE,I,J,K,L,S,B,M,P,N,
     .          RECOISDE(NSPMD),
     .          II(2),PTRPO(NSPMD+1),PTRPOO(NSPMD+1)
Cow
        INTEGER BUFSEND(NSPMD+1),BUFRECP(NSPMD+1),
     .          NBNOD,SIZ,LPO,NSN,PTR,NOD,NN,NR,
     .          SRBY
        INTEGER, DIMENSION(:), ALLOCATABLE :: BUFSPM,BUFRECVM,PORBY,BUFSPO

        INTEGER MAINND(NRBYKIN)

Cow MPI variables 
      INTEGER LOC_PROC
      INTEGER MSGOFF,MSGOFF2,MSGTYP,INFO,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,ISD(NSPMD)
C-----------------------------------------------
      DATA MSGOFF/7018/
      DATA MSGOFF2/7019/
C-----------------------------------------------
      ALLOCATE(BUFSPM(SBUFSPM))
      ALLOCATE(BUFRECVM(SBUFRECVM+NSPMD+1))
      ALLOCATE(PORBY(SPORBY))
      ALLOCATE(BUFSPO(SBUFSPO))

      LOC_PROC = ISPMD + 1 

C SEND main NODES TO PROC 0
      DO J=1,NRBYKIN
        PMAIN = FR_RBY2(3,J)
        IF (LOC_PROC==ABS(PMAIN))THEN
            MAINND(J)=NODGLOB( NPBY(1,J) )-1
        ELSE
            MAINND(J)= 0 
        ENDIF
      ENDDO   

      CALL SPMD_GLOB_ISUM9(MAINND,NRBYKIN)
Cow init
       DO I=1,SBUFRECVM
           BUFRECVM(I)=0
       ENDDO
Cow Remplissage du buffer d envoi proc ayant des noeuds secnds
Cow vers proc ayant Noeud Main

        L = 1

        DO I=1,NSPMD

          BUFSEND(I)=L
          S = 1
C
          DO J=1,NRBYKIN

            PMAIN = FR_RBY2(3,J)
            NBNOD =  FR_RBY2(1,J)

            IF ( NBNOD.NE.0  .AND.
     .          ABS(PMAIN).EQ.I .AND. LOC_PROC.NE.I) THEN

               BUFSPM(L) = J
               BUFSPM(L+1) = NBNOD
               L = L + 2
               NR = 1
               DO K=1,NPBY(2,J)
                 IF (WEIGHT(LPBY(K+S-1)).EQ.1) THEN
                   BUFSPM(L+NR-1) = NODGLOB(LPBY(K+S-1))-1
                   NR = NR +1
                 ENDIF
               ENDDO
               L = L+NBNOD
            ENDIF
            S = S + NPBY(2,J)
          ENDDO
        ENDDO
        BUFSEND(NSPMD+1)=L

Cow Envoi du buffer BUFSPM vers les differents procs
        DO I=1,NSPMD
  
          IF (IAD_RBY2(1,I).GT.0) THEN

            MSGTYP = MSGOFF
            B = BUFSEND(I)
            SIZ = BUFSEND(I+1)-BUFSEND(I)
            CALL MPI_ISEND(BUFSPM(B),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .         MPI_COMM_WORLD,ISD(I),ierror)

          ENDIF
        ENDDO

Cow Procs qui contient des Noeuds Main... Reception des noeuds secnds des
Cow differents processeurs

        L=1
        DO I = 1, NSPMD

          BUFRECP(I)=L
          IF (IAD_RBY2(2,I).GT.0) THEN

            MSGTYP = MSGOFF
            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
	    CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(BUFRECVM(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

            L = L + SIZ
            BUFRECVM(L)=0
            L=L+1
           ENDIF
        ENDDO
        DO I=1,NSPMD
  
          IF (IAD_RBY2(1,I).GT.0) THEN
            CALL MPI_WAIT(ISD(I),STATUS,IERROR)
          ENDIF
        ENDDO
        BUFRECP(NSPMD+1)=L
Cow Maintenant que les procs mains ont tout
Cow Il faut tout compacter dans le tableau d'envoi vers proc0

        L = 0
        K = 1

        DO I=1,NRBYKIN
          PMAIN = FR_RBY2(3,I)

          IF (ABS(PMAIN).EQ.LOC_PROC) THEN

Cow on recopie du proc main les noeuds secnds
               NBNOD = FR_RBY2(1,I)
Cow sauvegarde du ptr pour mettre la taille du RB
               NN = L+1
               L = L+2
               NR = 1

               DO J = 1,NPBY(2,I)
Cow des RBY qui on un PMAIN n  gatif n'ont pas forcement tous leur noeuds
Cow avec weight(I)=1
                 IF (PMAIN.LE.0) THEN
                    BUFSPO(L+NR)=NODGLOB(LPBY(K+J-1))-1
                    NR = NR+1
                 ELSE
                   IF (WEIGHT(LPBY(K+J-1)) .EQ.1) THEN
                      BUFSPO(L+NR)=NODGLOB(LPBY(K+J-1))-1
                      NR = NR+1
                   ENDIF
                 ENDIF
               ENDDO

               L=L+NR-1

               SRBY = NR-1

Cow le rb n'est pas local, on parcours les procs pour savoir s'il n a
Cow pas des bouts de RB.
             IF (PMAIN.GT.0) THEN
               DO P=1,NSPMD

                IF (IAD_RBY2(2,P).GT.0) THEN
Cow pointeur courrant de la pile du proc 
                  M = BUFRECP(P)
Cow numero du RB
                  IF (BUFRECVM(M).EQ.I) THEN

                    NBNOD=BUFRECVM(M+1)
                    BUFRECP(P)=BUFRECP(P)+2
Cow recopie
                    NR = 1
                    DO J=BUFRECP(P),BUFRECP(P)+NBNOD-1
                      BUFSPO(L+NR)=BUFRECVM(J)
                      NR=NR+1
                    ENDDO
                    L = L+NR-1
                    SRBY = SRBY + NR-1
Cow mise a jour du pointeur pour le RB suivant
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
Cow envoi tableau vers proc 0
        IF (ISPMD.NE.0 .AND .L.GT.0) THEN

          MSGTYP = MSGOFF2
          CALL MPI_SEND(BUFSPO,L,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .         MPI_COMM_WORLD,ierror)
        ENDIF


Cow je recois des messages des procs qui ont un noeud main de RB
        IF (ISPMD.EQ.0) THEN
          DO I=1,NSPMD
            RECOISDE(I)=0
          ENDDO
          DO I=1,NRBYKIN
            RECOISDE(ABS(FR_RBY2(3,I)))=1
          ENDDO

          LPO=1
          PTRPO(1)=LPO
Cow Je remplis ma file pour PO
          DO I=1,L
            PORBY(I)=BUFSPO(I)
          ENDDO
          LPO = LPO+L
          
          DO I=2,NSPMD

            IF (RECOISDE(I).EQ.1) THEN
ccc             
              MSGTYP = MSGOFF2
              PTRPO(I) = LPO
              CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                     MPI_COMM_WORLD,STATUS,ierror)
              CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

              CALL MPI_RECV(PORBY(LPO),SIZ,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP, MPI_COMM_WORLD,STATUS,ierror)
              LPO=LPO+SIZ
            ELSE
              PTRPO(I) = LPO
            ENDIF
          ENDDO                
          PTRPO(NSPMD+1)=LPO
Cow ecriture sur disque
           PTRPOO=PTRPO

           DO I=1,NRBYKIN
             II(1)=MAINND(I)

             DO P=1,NSPMD
               PTR = PTRPO(P)
               IF(PTR<PTRPOO(P+1))THEN
                IF(PORBY(PTR)==I)THEN
                 NSN = PORBY(PTR+1)
                 PTR = PTR+2
                 DO N=1,NSN
                  II(2)=PORBY(PTR+N-1)
                  CALL WRITE_I_C(II,2)
                 ENDDO
                 PTRPO(P)=PTRPO(P) + NSN +2
                ENDIF
               ENDIF
             ENDDO
           ENDDO
        ENDIF

      DEALLOCATE(BUFSPM)
      DEALLOCATE(BUFRECVM)
      DEALLOCATE(PORBY)
      DEALLOCATE(BUFSPO)
#endif
        RETURN
      END
