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
Chd|====================================================================
Chd|  SPMD_VGATH_ERR                source/mpi/anim/spmd_vgath_err.F
Chd|-- called by -----------
Chd|        SORTIE_ERROR                  source/output/sortie_error.F  
Chd|-- calls ---------------
Chd|        SPMD_OUTPITAB                 source/mpi/interfaces/spmd_outp.F
Chd|====================================================================
       SUBROUTINE SPMD_VGATH_ERR(X,MS,MSINI,NODGLOB,WEIGHT,NUM,IFLAG,
     .                           ITAB,LENG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   X(3,*),MS(*),MSINI(*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,IFLAG,ITAB(*),LENG
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      REAL, DIMENSION(:) , ALLOCATABLE :: MAXV0, MAXV1, MAXV2

      my_real
     .       , DIMENSION(:), ALLOCATABLE :: V, MAXV, MAXV_1, MAXV_2, MAXV_RES0,
     .                                      MAXV_RES1, MAXV_RES2

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF,FLAG,ITABG(LENG)
      INTEGER SIZ,MSGTYP,I,J,K,NG,NREC,MSGOFF2,
     .        L,ISMASSCHANGE,MSGOFF3,MSGOFF4
      INTEGER, DIMENSION(:) , ALLOCATABLE :: IDNOD0, IDNOD, IDNOD_RES

      DATA MSGOFF/7007/
      DATA MSGOFF2/7008/
      DATA MSGOFF3/7009/
      DATA MSGOFF4/7010/
C=======================================================================
      IF (NSPMD > 1)
     .    CALL SPMD_OUTPITAB(ITAB,WEIGHT,NODGLOB,ITABG)
c
       ALLOCATE(MAXV0(NUM),MAXV1(NUM),MAXV2(NUM))

       ALLOCATE(V(NUMNOD),MAXV(NUM),MAXV_1(NUM),MAXV_2(NUM),MAXV_RES0(NUM),
     .          MAXV_RES1(NUM),MAXV_RES2(NUM))

       ALLOCATE(IDNOD0(NUM),IDNOD(NUM),IDNOD_RES(NUM))

       IDNOD(1:NUM) = 0
       IDNOD0(1:NUM) = 0
       MAXV(1:NUM) = ZERO
       MAXV_1(1:NUM) = ZERO
       MAXV_2(1:NUM) = ZERO
       MAXV0(1:NUM) = ZERO
       SIZ = NUM
       IF(IFLAG==1)THEN
         DO I=1,NUMNOD
           V(I)=SQRT(X(1,I)**2+X(2,I)**2+X(3,I)**2)
         ENDDO
       ELSEIF(IFLAG==2)THEN
         DO I=1,NUMNOD
           V(I)=HALF*MS(I)*(X(1,I)**2+X(2,I)**2+X(3,I)**2)
         ENDDO
       ELSEIF(IFLAG==3)THEN
         DO I=1,NUMNOD
           V(I)=MS(I)
         ENDDO
       ELSEIF(IFLAG==4)THEN
         DO I=1,NUMNOD
           V(I)=(MS(I)-MSINI(I))/MAX(EM20,MSINI(I))
         ENDDO
       ENDIF
       IF (ISPMD/=0) THEN

         DO I=1,NUMNOD
           IF (WEIGHT(I)==1) THEN
             FLAG = 0
             DO J=1,NUM
               IF(V(I) > MAXV0(J) .AND. FLAG == 0) THEN
                 DO K=1,NUM-J
                   IDNOD0(NUM-K+1) = IDNOD0(NUM-K)
                   MAXV0(NUM-K+1) = MAXV0(NUM-K)
                   MAXV1(NUM-K+1) = MAXV1(NUM-K)
                   MAXV2(NUM-K+1) = MAXV2(NUM-K)
                 ENDDO
                 IDNOD0(J) = NODGLOB(I)
                 MAXV0(J) = V(I)
                 MAXV1(J) = MS(I)
                 MAXV2(J) = MSINI(I)
                 FLAG = 1
               ENDIF
             ENDDO
           END IF
         END DO

Csv a cause de la version simple precision, on ne peux pas metre l'entier
Csv dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
Csv de noeuds au max

         MSGTYP = MSGOFF
         CALL MPI_SEND(IDNOD0,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF2
         CALL MPI_SEND(MAXV0,SIZ,MPI_REAL,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF3
         CALL MPI_SEND(MAXV1,SIZ,MPI_REAL,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF4
         CALL MPI_SEND(MAXV2,SIZ,MPI_REAL,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE ! ISPMD == 0
          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN
              FLAG = 0
              DO J=1,NUM
                IF(V(I) > MAXV(J) .AND. FLAG == 0) THEN
                  DO K=1,NUM-J
                    IDNOD(NUM-K+1) = IDNOD(NUM-K)
                    MAXV(NUM-K+1) = MAXV(NUM-K)
                    MAXV_1(NUM-K+1) = MAXV_1(NUM-K)
                    MAXV_2(NUM-K+1) = MAXV_2(NUM-K)
                  ENDDO
                  IDNOD(J) = NODGLOB(I)
                  MAXV(J) = V(I)
                  MAXV_1(J) = MS(I)
                  MAXV_2(J) = MSINI(I)
                  FLAG = 1
                ENDIF
              ENDDO

            ENDIF
          ENDDO

          DO I=2,NSPMD

Csv Reception 
            MSGTYP = MSGOFF

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
	    CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IDNOD0,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

Csv Reception

            MSGTYP = MSGOFF2
            CALL MPI_RECV(MAXV0,SIZ,MPI_REAL,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

            MSGTYP = MSGOFF3
            CALL MPI_RECV(MAXV1,SIZ,MPI_REAL,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

            MSGTYP = MSGOFF4
            CALL MPI_RECV(MAXV2,SIZ,MPI_REAL,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)


            J = 1
            K = 1
            L = 1
            DO WHILE( J <= SIZ .AND. K <= SIZ .AND. L <= SIZ)
                IF(MAXV(J) > MAXV0(K))THEN
                  MAXV_RES0(L) = MAXV(J)
                  IDNOD_RES(L) = IDNOD(J)
                  MAXV_RES1(L) = MAXV_1(J)
                  MAXV_RES2(L) = MAXV_2(J)
                  J = J + 1
                  L = L + 1
                ELSEIF(MAXV0(K) > MAXV(J))THEN
                  MAXV_RES0(L) = MAXV0(K)
                  IDNOD_RES(L) = IDNOD0(K)
                  MAXV_RES1(L) = MAXV1(K)
                  MAXV_RES2(L) = MAXV2(K)
                  K = K + 1
                  L = L + 1
                ELSE
                  MAXV_RES0(L) = MAXV0(K)
                  IDNOD_RES(L) = IDNOD0(K)
                  MAXV_RES1(L) = MAXV1(K)
                  MAXV_RES2(L) = MAXV2(K)
                  J = J + 1
                  K = K + 1
                  L = L + 1
                ENDIF
            ENDDO
            DO J=1,SIZ
              MAXV(J) =  MAXV_RES0(J)
              IDNOD(J) = IDNOD_RES(J)
              MAXV_1(J) =  MAXV_RES1(J)
              MAXV_2(J) =  MAXV_RES2(J)
            ENDDO

          ENDDO
          IF(IFLAG == 1)THEN
            WRITE(IOUT,2000)
            WRITE(IOUT,1000)
            WRITE(IOUT,2000)
            WRITE(IOUT,1001)
            DO I=1,NUM
              IF(MAXV_RES0(I) /= ZERO)THEN
                WRITE(IOUT,1100) ITABG(IDNOD_RES(I)),MAXV_RES0(I),
     .               HALF*(MAXV_RES1(I))*MAXV_RES0(I)**2,
     .               MAXV_RES1(I),MAXV_RES2(I),
     .               (MAXV_RES1(I)-MAXV_RES2(I))/MAX(EM20,MAXV_RES2(I))
              ENDIF
            ENDDO
          ELSEIF(IFLAG == 2)THEN
            WRITE(IOUT,2000)
            WRITE(IOUT,1200)
            WRITE(IOUT,2000)
            WRITE(IOUT,1201)
            DO I=1,NUM
              WRITE(IOUT,1300) ITABG(IDNOD_RES(I)),MAXV_RES0(I)
            ENDDO
          ELSEIF(IFLAG == 3)THEN
            WRITE(IOUT,2000)
            WRITE(IOUT,1400)
            WRITE(IOUT,2000)
            WRITE(IOUT,1401)
            DO I=1,NUM
              WRITE(IOUT,1500) ITABG(IDNOD_RES(I)),MAXV_RES0(I)
            ENDDO
          ELSEIF(IFLAG == 4 .AND. (IDTMIN(11) == 3 .OR.IDTMIN(11)==8))THEN
            ISMASSCHANGE = 0
            DO I=1,NUM
              IF(MAXV_RES0(I) /=ZERO) ISMASSCHANGE = 1
            ENDDO
            IF(ISMASSCHANGE /= 0)THEN
              WRITE(IOUT,2000)
              WRITE(IOUT,1600)
              WRITE(IOUT,2000)
              WRITE(IOUT,1601)
              DO I=1,NUM
               IF(MAXV_RES0(I) /= ZERO)
     .         WRITE(IOUT,1700) ITABG(IDNOD_RES(I)),MAXV_RES1(I),
     .                          MAXV_RES2(I),MAXV_RES0(I)
              ENDDO
            ENDIF
          ENDIF
       ENDIF
C
 1000 FORMAT('***  NODES WITH HIGHEST VELOCITY')
 1001 FORMAT('      NODE   VELOCITY     K-ENER       MASS      MASS0
     .   DM/MASS0')
 1100 FORMAT(I10,5G11.4)
c
 1200 FORMAT('***  NODES WITH HIGHEST KINEMATIC ENERGY')
 1201 FORMAT('  NODE   K-ENER    MASS  MASS0   MASS/MASS0')
 1300 FORMAT(I10,G11.4)
c
 1400 FORMAT('***  NODES WITH HIGHEST MASS')
 1401 FORMAT('  NODE     MASS  MASS0   MASS/MASS0')
 1500 FORMAT(I10,G11.4)
c
 1600 FORMAT('***  NODES WITH HIGHEST MASS CHANGE')
 1601 FORMAT('      NODE       MASS      MASS0   DM/MASS0')
 1700 FORMAT(I10,3G11.4)
c
 2000 FORMAT('      ')
 2100 FORMAT('                          ** STATISTICS   **')
c
       DEALLOCATE(MAXV0,MAXV1,MAXV2)
       DEALLOCATE(V,MAXV,MAXV_RES0,MAXV_RES1,MAXV_RES2)
       DEALLOCATE(IDNOD0,IDNOD,IDNOD_RES)

#endif
       RETURN
       END
