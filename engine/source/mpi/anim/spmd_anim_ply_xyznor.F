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
Chd|  SPMD_ANIM_PLY_XYZNOR          source/mpi/anim/spmd_anim_ply_xyznor.F
Chd|-- called by -----------
Chd|        XYZNOR_PLY                    source/output/anim/generate/xyznor_ply.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_ANIM_PLY_XYZNOR( NODGLOB,IPLY,XNORM,EMPSIZPL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*)
      INTEGER IPLY,IDPLY,EMPSIZPL
      my_real
     .   XNORM(3,*)
      REAL R4
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,STAT(MPI_STATUS_SIZE,NSPMD-1), IERR
      INTEGER I,N,ND,EMPL,P,ITAG,INORM(3),K,IXNN

      INTEGER PLYSIZ
      my_real
     .   S3000,S
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: WRTBUF
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FSENDBUF,FRECBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISENDBUF,IRECBUF
C-----------------------------------------------
      DATA MSGOFF/7058/
      DATA MSGOFF2/7059/
C-----------------------------------------------
      S3000 = THREE1000
      IXNN = S3000
      IF(FMT_ANI==4)IXNN=0
C
      IF (ISPMD ==0) THEN
        PLYSIZ  = PLYNOD (IPLY)%PLYNUMNODS
        ALLOCATE(WRTBUF(3,PLYSIZG(IPLY)))

C Les plynods du proc 0
        DO ND=1,PLYSIZ
             I = PLYNOD(IPLY)%NODES(ND)
             EMPL =  PLYNOD(IPLY)%PLYNODID(ND)-EMPSIZPL

             S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
             IF(S/=ZERO)THEN
                S = S3000 / S
                INORM(1) = XNORM(1,I) * S
                INORM(2) = XNORM(2,I) * S
                INORM(3) = XNORM(3,I) * S
             ELSE
                INORM(1) = 0
                INORM(2) = 0
                INORM(3) = IXNN
             END IF

             WRTBUF(1,EMPL)= INORM(1)
             WRTBUF(2,EMPL)= INORM(2)
             WRTBUF(3,EMPL)= INORM(3)
        END DO
C Les plynods des autres procs
           DO P=2,NSPMD
              ALLOCATE(IRECBUF(PLYIADNOD(IPLY,P)))
              ALLOCATE(FRECBUF(3,PLYIADNOD(IPLY,P)))

              ITAG=MSGOFF

              CALL MPI_RECV(IRECBUF,PLYIADNOD(IPLY,P), MPI_INTEGER,
     .                      IT_SPMD(P),ITAG, MPI_COMM_WORLD, STAT, IERR)

              ITAG=MSGOFF2
              CALL MPI_RECV(FRECBUF,PLYIADNOD(IPLY,P)*3, MPI_INTEGER,
     .                      IT_SPMD(P),ITAG, MPI_COMM_WORLD, STAT, IERR)

              DO I=1,PLYIADNOD(IPLY,P)
                 EMPL = IRECBUF(I)-EMPSIZPL
                 WRTBUF(1,EMPL)=FRECBUF(1,I)
                 WRTBUF(2,EMPL)=FRECBUF(2,I)
                 WRTBUF(3,EMPL)=FRECBUF(3,I)
              ENDDO
              DEALLOCATE(IRECBUF,FRECBUF)
           ENDDO

           CALL WRITE_S_C(WRTBUF,3*PLYSIZG(IPLY))
           DEALLOCATE(WRTBUF)
           EMPSIZPL=EMPSIZPL+PLYSIZG(IPLY)

      ELSE
        PLYSIZ = PLYNOD(IPLY)%PLYNUMNODS
        ALLOCATE (FSENDBUF(3,PLYSIZ))
        ALLOCATE (ISENDBUF(PLYSIZ))

        DO ND=1,PLYSIZ
             I = PLYNOD(IPLY)%NODES(ND)

             S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
             IF(S/=ZERO)THEN
                S = S3000 / S
                INORM(1) = XNORM(1,I) * S
                INORM(2) = XNORM(2,I) * S
                INORM(3) = XNORM(3,I) * S
             ELSE
                INORM(1) = 0
                INORM(2) = 0
                INORM(3) = IXNN
             END IF

           FSENDBUF(1,ND) = INORM(1)
           FSENDBUF(2,ND) = INORM(2)
           FSENDBUF(3,ND) = INORM(3)

           ISENDBUF(ND) =  PLYNOD(IPLY)%PLYNODID(ND)
        END DO

        ITAG=MSGOFF
        CALL MPI_SEND(ISENDBUF,PLYSIZ,MPI_INTEGER,IT_SPMD(1),
     .                   ITAG,MPI_COMM_WORLD,IERR)

        ITAG=MSGOFF2
        CALL MPI_SEND(FSENDBUF,PLYSIZ*3,MPI_INTEGER,IT_SPMD(1),
     .                   ITAG,MPI_COMM_WORLD,IERR)
C
        DEALLOCATE(ISENDBUF,FSENDBUF)
      ENDIF

#endif
      RETURN
      END
