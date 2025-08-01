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
!||    spmd_stat_pgather     ../engine/source/mpi/output/spmd_stat.F
!||--- called by ------------------------------------------------------
!||    dynain_c_strag        ../engine/source/output/dynain/dynain_c_strag.F
!||    dynain_c_strsg        ../engine/source/output/dynain/dynain_c_strsg.F
!||    stat_c_auxf           ../engine/source/output/sta/stat_c_auxf.F
!||    stat_c_epspf          ../engine/source/output/sta/stat_c_epspf.F
!||    stat_c_fail           ../engine/source/output/sta/stat_c_fail.F
!||    stat_c_orth_loc       ../engine/source/output/sta/stat_c_orth_loc.F
!||    stat_c_straf          ../engine/source/output/sta/stat_c_straf.F
!||    stat_c_strafg         ../engine/source/output/sta/stat_c_strafg.F
!||    stat_c_strsf          ../engine/source/output/sta/stat_c_strsf.F
!||    stat_c_strsfg         ../engine/source/output/sta/stat_c_strsfg.F
!||    stat_p_aux            ../engine/source/output/sta/stat_p_aux.F
!||    stat_p_full           ../engine/source/output/sta/stat_p_full.F
!||    stat_r_full           ../engine/source/output/sta/stat_r_full.F
!||    stat_s_auxf           ../engine/source/output/sta/stat_s_auxf.F
!||    stat_s_eref           ../engine/source/output/sta/stat_s_eref.F
!||    stat_s_fail           ../engine/source/output/sta/stat_s_fail.F
!||    stat_s_ortho          ../engine/source/output/sta/stat_s_ortho.F
!||    stat_s_straf          ../engine/source/output/sta/stat_s_straf.F
!||    stat_s_strsf          ../engine/source/output/sta/stat_s_strsf.F
!||    stat_sphcel_full      ../engine/source/output/sta/stat_sphcel_full.F90
!||    stat_t_full           ../engine/source/output/sta/stat_t_full.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
       SUBROUTINE SPMD_STAT_PGATHER(PTV,PTLEN,PTV_P0,PTLEN_P0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER PTLEN,PTLEN_P0,PTV(PTLEN),PTV_P0(0:MAX(1,PTLEN_P0))
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,
     .        MSGTYP,I,J,IAD,IDEB, POLD,
     .        LENP(NSPMD),DISP(NSPMD)



         
        CALL MPI_GATHER(
     S    PTLEN         ,1     ,MPI_INTEGER,
     R    LENP          ,1     ,MPI_INTEGER,IT_SPMD(1),
     G    SPMD_COMM_WORLD,IERROR)
C
        IAD=0
        IF(ISPMD==0)THEN
          DO I=1,NSPMD
            DISP(I) = IAD
            IAD = IAD+LENP(I)
          END DO
        END IF
C
        CALL MPI_GATHERV(
     S    PTV           ,PTLEN ,MPI_INTEGER,
     R    PTV_P0(1)     ,LENP  ,DISP,MPI_INTEGER  ,IT_SPMD(1),
     G    SPMD_COMM_WORLD,IERROR)
C
        IF(ISPMD==0)THEN
C         construit les pointeurs globaux de fin de zone
          PTV_P0(0)=0
          DO I=2,NSPMD
            IDEB = DISP(I)
            POLD = PTV_P0(IDEB)
            DO J=1,LENP(I)
              PTV_P0(IDEB+J)=PTV_P0(IDEB+J)+POLD
            END DO
          END DO
        END IF

#endif
       RETURN
       END
!||====================================================================
!||    spmd_iget_partn_sta   ../engine/source/mpi/output/spmd_stat.F
!||--- called by ------------------------------------------------------
!||    dynain_shel_spmd      ../engine/source/output/dynain/dynain_shel_spmd.F
!||    stat_beam_spmd        ../engine/source/output/sta/stat_beam_spmd.F
!||    stat_brick_spmd       ../engine/source/output/sta/stat_brick_spmd.F
!||    stat_shel_spmd        ../engine/source/output/sta/stat_shel_spmd.F
!||    stat_sphcel_spmd      ../engine/source/output/sta/stat_sphcel_spmd.F90
!||    stat_spring_spmd      ../engine/source/output/sta/stat_spring_spmd.F
!||    stat_truss_spmd       ../engine/source/output/sta/stat_truss_spmd.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_IGET_PARTN_STA(
     .             SIZE,STAT_NUMEL,STAT_LENELG,LENG,NP,
     .             IADG,NPGLOB,STAT_INDX)
C gather sur p0 du tableau wa en fonction des parts (IADG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------

#include "spmd.inc"

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZE, STAT_NUMEL, STAT_LENELG, LENG, NP(*),
     .        IADG(NSPMD,*),NPGLOB(*),STAT_INDX(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,IDEB,K,N,NB_TMP,LEN,
     .        NBF_L,NPT(SIZE*STAT_NUMEL)
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DATA MSGOFF/10001/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NBF_L = SIZE*STAT_NUMEL
      IF (ISPMD/=0) THEN
        MSGTYP=MSGOFF


        CALL MPI_SEND(NP,NBF_L,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                SPMD_COMM_WORLD,ierror)


        STAT_LENELG=0

      ELSE
        DO K=1,NBF_L
           NPGLOB(K) = NP(K)
        ENDDO
        IDEB = NBF_L +  1
C
        DO K=2,NSPMD
          MSGTYP=MSGOFF

          CALL MPI_PROBE(IT_SPMD(K),MSGTYP,
     .           SPMD_COMM_WORLD,STATUS,ierror)
          CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,NB_TMP,ierror)
C                                                                     12
          CALL MPI_RECV(NPGLOB(IDEB),NB_TMP,MPI_INTEGER,IT_SPMD(K),
     .          MSGTYP,SPMD_COMM_WORLD,STATUS,ierror)

          IDEB = IDEB + NB_TMP
        END DO

        STAT_LENELG=IDEB/SIZE
        
      END IF
C
#endif
      RETURN
      END

!||====================================================================
!||    spmd_dstat_vgath      ../engine/source/mpi/output/spmd_stat.F
!||--- called by ------------------------------------------------------
!||    dynain_node           ../engine/source/output/dynain/dynain_node.F
!||    stat_n_vel            ../engine/source/output/sta/state_n_vel.F
!||    stat_node             ../engine/source/output/sta/stat_node.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
       SUBROUTINE SPMD_DSTAT_VGATH(V,NODGLOB,WEIGHT,VGATH,NODTAG,
     .                             NODTAGLOB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
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
       my_real
     .   V(3,*),VGATH(3,*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF,NODTAG(*),
     .         NODTAGLOB(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/10002/
      DATA MSGOFF2/10002/
      my_real 
     .       BUFSR(3,NUMNODM)
      INTEGER IBUF(NUMNODM)
C   Tableau utilise par proc 0
         
       IF (ISPMD/=0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (NODTAG(I)/=0) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(1,SIZ) = V(1,I)
             BUFSR(2,SIZ) = V(2,I) 
             BUFSR(3,SIZ) = V(3,I) 
           END IF
         END DO

C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max

         MSGTYP = MSGOFF2 
         CALL MPI_SEND(IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

         MSGTYP = MSGOFF 
         CALL MPI_SEND(BUFSR,3*SIZ,REAL,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

          
       ELSE

          NODTAGLOB(1:NUMNODG)=0
          DO I=1,NUMNOD
            IF (NODTAG(I)/=0) THEN
              NG = NODGLOB(I)
              NODTAGLOB(NG)=1
              VGATH(1,NG) = V(1,I)
              VGATH(2,NG) = V(2,I)
              VGATH(3,NG) = V(3,I)
            ENDIF
          ENDDO


          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF2 

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)

C   Reception du buffer flottant double des adresses NODGLOB

            MSGTYP = MSGOFF 
            CALL MPI_RECV(BUFSR,3*SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)
         
           NREC = SIZ
           DO K = 1, NREC
             NG = IBUF(K)
             NODTAGLOB(NG)=1
             VGATH(1,NG) = BUFSR(1,K)
             VGATH(2,NG) = BUFSR(2,K)
             VGATH(3,NG) = BUFSR(3,K)
           ENDDO
          ENDDO


       ENDIF

#endif
       RETURN
       END
!||====================================================================
!||    spmd_dstat_gath       ../engine/source/mpi/output/spmd_stat.F
!||--- called by ------------------------------------------------------
!||    stat_n_temp           ../engine/source/output/sta/stat_n_temp.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
       SUBROUTINE SPMD_DSTAT_GATH(V,NODGLOB,WEIGHT,VGATH,NODTAG,
     .                             NODTAGLOB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
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
       my_real
     .   V(*),VGATH(*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF,NODTAG(*),
     .         NODTAGLOB(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/10003/
      DATA MSGOFF2/10003/
      my_real 
     .       BUFSR(NUMNODM)
      INTEGER IBUF(NUMNODM)
C   Tableau utilise par proc 0
         
       IF (ISPMD/=0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (NODTAG(I)/=0) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(SIZ) = V(I)
           END IF
         END DO

C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max

         MSGTYP = MSGOFF2
         CALL MPI_SEND(IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

         MSGTYP = MSGOFF 
         CALL MPI_SEND(BUFSR,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

          
       ELSE

          NODTAGLOB(1:NUMNODG)=0
          DO I=1,NUMNOD
            IF (NODTAG(I)/=0) THEN
              NG = NODGLOB(I)
              NODTAGLOB(NG)=1
              VGATH(NG) = V(I)
            ENDIF
          ENDDO


          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF2

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)

C   Reception du buffer flottant double des adresses NODGLOB

            MSGTYP = MSGOFF
            CALL MPI_RECV(BUFSR,SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)
         
           NREC = SIZ
           DO K = 1, NREC
             NG = IBUF(K)
             NODTAGLOB(NG)=1
             VGATH(NG) = BUFSR(K)
           ENDDO
          ENDDO


       ENDIF

#endif
       RETURN
       END


!||====================================================================
!||    spmd_istat_gath       ../engine/source/mpi/output/spmd_stat.F
!||--- called by ------------------------------------------------------
!||    stat_n_bcs            ../engine/source/output/sta/stat_n_bcs.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
       SUBROUTINE SPMD_ISTAT_GATH(VI,NODGLOB,VIGATH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
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
       INTEGER  VI(*),VIGATH(*),NODGLOB(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF,MSGOFF2
      INTEGER SIZ,MSGTYP,I,K,NG,NREC

      DATA MSGOFF/10003/
      DATA MSGOFF2/10003/
      INTEGER IBUFN(NUMNODM),IBUFM(NUMNODM)
C   Tableau utilise par proc 0
         
       IF (ISPMD/=0) THEN

         SIZ = 0
         DO I=1,NUMNOD
            SIZ = SIZ + 1
            IBUFN(SIZ) = NODGLOB(I)
            IBUFM(SIZ) = VI(I)
         END DO

C

         MSGTYP = MSGOFF
         CALL MPI_SEND(IBUFN,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

         MSGTYP = MSGOFF2
         CALL MPI_SEND(IBUFM,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

        
       ELSE
          DO I=1,NUMNOD
             NG = NODGLOB(I)
             VIGATH(NG) = VI(I)
          ENDDO


          DO I=2,NSPMD

C Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUFN,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)

C Reception Integer Buffer of ICODE
            MSGTYP = MSGOFF2
            CALL MPI_RECV(IBUFM,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)
         
           NREC = SIZ
           DO K = 1, NREC
             NG = IBUFN(K)
             VIGATH(NG) = IBUFM(K)
           ENDDO
          ENDDO


       ENDIF

#endif
       RETURN
       END



