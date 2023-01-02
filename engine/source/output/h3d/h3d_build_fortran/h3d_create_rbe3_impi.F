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
Chd|  H3D_CREATE_RBE3_IMPI          source/output/h3d/h3d_build_fortran/h3d_create_rbe3_impi.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|====================================================================
      SUBROUTINE H3D_CREATE_RBE3_IMPI(LRBE3, IRBE3,NODGLOB,WEIGHT,NERBE3Y,
     *                         NERBE3T ,ITAB,COMPID_RBE3S)
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
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NODGLOB(*),WEIGHT(*),
     * NERBE3Y,NERBE3T(NRBE3G),ITAB(*),COMPID_RBE3S
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,N,P, SZLOCRBE3(NRBE3G),PGLOBRBE3(NRBE3G),ID
      INTEGER SNRBE3,SIZRBE3,SBUFSIZ,PSNRBE3
      INTEGER NSN,IADG,IAD,SN,MN,NGRBE
      INTEGER SECNDNODS(NRBE3G),ID_RBE3(NRBE3G)
      INTEGER,  DIMENSION(:),ALLOCATABLE :: SENDBUF,RECBUF,
     *                                      P0RBE3BUF,IADRBE3
      INTEGER,  DIMENSION(:,:),ALLOCATABLE :: P0RECRBE3, IIN

C MPI variables 
      INTEGER LOC_PROC
      INTEGER MSGOFF,MSGOFF2,MSGTYP,INFO,ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,ISD(NSPMD)

      DATA MSGOFF/7020/
      DATA MSGOFF2/7021/
C-----------------------------------------------
C     1ere etape - envoyer au proc 0 un tableau avec nombre
C                  noeuds secnds locaux par RBE3 a envoyer
C                  et preparation du buffer d envoi
C                  (taille)
      NERBE3T = 0
      SNRBE3 = 0
      SBUFSIZ = 0
      SZLOCRBE3=0
      PGLOBRBE3 = 0

      DO I=1,NRBE3
          NGRBE = IRBE3(10,I)
          SZLOCRBE3(NGRBE) = 0
          NSN = IRBE3(5,I)
          DO N=1,NSN
           IF (WEIGHT(LRBE3(IRBE3(1,I)+N))==1)
     .       SZLOCRBE3(NGRBE) = SZLOCRBE3(NGRBE)  + 1
          ENDDO
          SBUFSIZ = SBUFSIZ + SZLOCRBE3(NGRBE)

      ENDDO

C Envoi vers le proc 0 du tableau des tailles

      IF (ISPMD == 0) THEN
C Proc zero reception des tailles
        ALLOCATE(P0RECRBE3(NRBE3G,NSPMD))
        DO I=1,NRBE3G
           P0RECRBE3(I,1) = SZLOCRBE3(I)
        ENDDO

        DO P=2,NSPMD
           MSGTYP = MSGOFF
           CALL MPI_RECV(P0RECRBE3(1,P),NRBE3G,MPI_INTEGER,IT_SPMD(P),
     *                   MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
        ENDDO

      ELSE
C Procs autres envoi
        MSGTYP = MSGOFF
        CALL MPI_SEND(SZLOCRBE3,NRBE3G,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

      ENDIF

C --------------------------------------------------------------
C Envoi vers le proc 0 des noeuds des RBE3 & ecriture sur disque
C --------------------------------------------------------------
      IF (ISPMD /= 0) THEN
C ------------------------
C Procs autres que proc 0
C ------------------------
         ALLOCATE(SENDBUF(SBUFSIZ))
         SNRBE3 = 0
         DO I=1,NRBE3
           NSN = IRBE3(5,I)
           IAD = IRBE3(1,I)
           DO N=1,NSN
              SN = LRBE3(IAD+N)
              IF (WEIGHT(SN) == 1 )THEN
                SNRBE3 = SNRBE3+1
                SENDBUF(SNRBE3)=ITAB(SN)
              ENDIF
           ENDDO
         ENDDO
         IF (SNRBE3 > 0)THEN
           MSGTYP = MSGOFF2
           CALL  MPI_SEND(SENDBUF,SNRBE3,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     *                  MPI_COMM_WORLD,IERROR)
         ENDIF
         DEALLOCATE(SENDBUF)

C Envoi des noeuds secnds
        SECNDNODS = 0
        DO I=1,NRBE3
          MN = IRBE3(3,I)
	  IF(MN/=0)THEN
            IF (WEIGHT(MN)==1)THEN
              NGRBE = IRBE3(10,I)
              SECNDNODS(NGRBE)=ITAB(MN)
            ENDIF
	  ENDIF
        ENDDO
        CALL SPMD_GLOB_ISUM9(SECNDNODS,NRBE3G)

C Envoi des Ids
        ID_RBE3 = 0
        DO I=1,NRBE3
          ID = IRBE3(2,I)
	  IF(IRBE3(3,I)/=0)THEN
            IF (WEIGHT(IRBE3(3,I))==1)THEN
              NGRBE = IRBE3(10,I)
              ID_RBE3(NGRBE)=ID
            ENDIF
	  ENDIF
        ENDDO
        CALL SPMD_GLOB_ISUM9(ID_RBE3,NRBE3G)
        

      ELSE
C --------------------------------------------------------------------
C PROC 0
C --------------------------------------------------------------------
C P0RBE3BUF tableau de reception (tableau de reception = LRBE3 Global)
C IADRBE3 pointeurs vers P0RBE3BUF global
         ALLOCATE(IADRBE3(NRBE3G+1))
         ALLOCATE(P0RBE3BUF(NERBE3Y))

C preparation IADRBE3
         IADRBE3(1)=0
         DO I=1,NRBE3G
           SNRBE3 = P0RECRBE3(I,1)
           DO N=2,NSPMD
             SNRBE3 = SNRBE3 + P0RECRBE3(I,N)
           ENDDO
           IADRBE3(I+1)=IADRBE3(I)+SNRBE3
         ENDDO

C preparation P0RECRBE3 pour le proc0
         DO I=1,NRBE3G
	    PGLOBRBE3(I)=IADRBE3(I)
         ENDDO
	 
         DO I=1,NRBE3
           NSN = IRBE3(5,I)
           IAD = IRBE3(1,I)
           NGRBE = IRBE3(10,I)
           IADG = IADRBE3(NGRBE)
           SNRBE3 = 0
           DO N=1,NSN
             SN = LRBE3( IAD+N )
             IF (WEIGHT(SN) == 1 )THEN
               SNRBE3 = SNRBE3+1
               P0RBE3BUF(IADG + SNRBE3) = ITAB(SN)
             ENDIF
           ENDDO
           PGLOBRBE3(NGRBE)=PGLOBRBE3(NGRBE) + SNRBE3
         ENDDO


C Reception des RBE3 des autres procs
         DO P=2,NSPMD
C Taille du buffer de reception
           SIZRBE3 = 0
           DO I=1,NRBE3G
             SIZRBE3 = SIZRBE3 + P0RECRBE3(I,P)
           ENDDO

           IF (SIZRBE3 > 0) THEN
             ALLOCATE(RECBUF(SIZRBE3))
             MSGTYP = MSGOFF2 
             CALL MPI_RECV(RECBUF,SIZRBE3,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     *                   MPI_COMM_WORLD,STATUS,IERROR)

             PSNRBE3=0
             DO I=1,NRBE3G
               IADG = PGLOBRBE3(I)
               DO N=1,P0RECRBE3(I,P)
                 PSNRBE3 = PSNRBE3 + 1
                 P0RBE3BUF(IADG + N) = RECBUF(PSNRBE3)
               ENDDO
               PGLOBRBE3(I) = PGLOBRBE3(I) + P0RECRBE3(I,P)
             ENDDO
             DEALLOCATE(RECBUF)
           ENDIF
         ENDDO
C  Reception des Noeuds secnds
         SECNDNODS=0
         DO I=1,NRBE3
           MN = IRBE3(3,I)
           IF (WEIGHT(MN)==1) THEN
             NGRBE = IRBE3(10,I)
             SECNDNODS(NGRBE)=ITAB(MN)
           ENDIF        
         ENDDO
         CALL SPMD_GLOB_ISUM9(SECNDNODS,NRBE3G)

C Reception des Ids
        ID_RBE3 = 0
        DO I=1,NRBE3
          ID = IRBE3(2,I)
	  IF(IRBE3(3,I)/=0)THEN
            IF (WEIGHT(IRBE3(3,I))==1)THEN
              NGRBE = IRBE3(10,I)
              ID_RBE3(NGRBE)=ID
            ENDIF
	  ENDIF
        ENDDO
        CALL SPMD_GLOB_ISUM9(ID_RBE3,NRBE3G)


         CALL C_H3D_CREATE_RBE3_IMPI(ITAB,NRBE3G,IADRBE3,SECNDNODS,P0RBE3BUF,ID_RBE3,
     .                               COMPID_RBE3S)

        ENDIF
#endif
        RETURN
      END 
