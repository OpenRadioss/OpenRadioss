Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C-----------------------------------------------
Chd|====================================================================
Chd|  SPMD_ANIM_PLY_VELVEC          source/mpi/anim/spmd_anim_ply_velvec.F
Chd|-- called by -----------
Chd|        VELVEC                        source/output/anim/generate/velvec.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_ANIM_PLY_VELVEC( NODGLOB,IPLY,
     *                                 NOD_PXFEM, IFUNC,EMPSIZPL)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),NOD_PXFEM(*)
      INTEGER IPLY,IFUNC,EMPSIZPL
      REAL R4
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,STAT(MPI_STATUS_SIZE,NSPMD-1), IERR
      INTEGER I,N,ND,EMPL,P,ITAG,EMP
      INTEGER PLYSIZ
      my_real
     .   VN,VAL
C
      REAL , DIMENSION(:,:), ALLOCATABLE :: WRTBUF
      my_real
     * , DIMENSION(:,:), ALLOCATABLE :: FSENDBUF,FRECBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISENDBUF,IRECBUF
C-----------------------------------------------
      DATA MSGOFF/7060/
      DATA MSGOFF2/7061/
C-----------------------------------------------

C
      IF (ISPMD ==0) THEN
        PLYSIZ  = PLYNOD (IPLY)%PLYNUMNODS
        ALLOCATE(WRTBUF(3,PLYSIZG(IPLY)))

C Les plynods du proc 0
        DO ND=1,PLYSIZ
             I = PLYNOD(IPLY)%NODES(ND)
             N = NOD_PXFEM(I)
             EMPL = PLYNOD(IPLY)%PLYNODID(ND)-EMPSIZPL

             IF (IFUNC==1)THEN
                WRTBUF(1,EMPL)= PLY(IPLY)%V(1,N)
                WRTBUF(2,EMPL)= PLY(IPLY)%V(2,N)
                WRTBUF(3,EMPL)= PLY(IPLY)%V(3,N)

             ELSEIF (IFUNC==2)THEN
                WRTBUF(1,EMPL)= PLY(IPLY)%U(1,N)
                WRTBUF(2,EMPL)= PLY(IPLY)%U(2,N)
                WRTBUF(3,EMPL)= PLY(IPLY)%U(3,N)

             ELSEIF (IFUNC==3)THEN
                WRTBUF(1,EMPL)= PLY(IPLY)%A(1,N)
                WRTBUF(2,EMPL)= PLY(IPLY)%A(2,N)
                WRTBUF(3,EMPL)= PLY(IPLY)%A(3,N)
             ELSE
                WRTBUF(1,EMPL)= ZERO
                WRTBUF(2,EMPL)= ZERO
                WRTBUF(3,EMPL)= ZERO
             ENDIF
        END DO
C Les plynods des autres procs
           DO P=2,NSPMD
             IF (PLYIADNOD(IPLY,P)>0)THEN
              ALLOCATE(IRECBUF(PLYIADNOD(IPLY,P)))
              ALLOCATE(FRECBUF(3,PLYIADNOD(IPLY,P)))
              ITAG=MSGOFF
              CALL MPI_RECV(IRECBUF,PLYIADNOD(IPLY,P), MPI_INTEGER,
     .                      IT_SPMD(P),ITAG, MPI_COMM_WORLD, STAT, IERR)
              ITAG=MSGOFF2
              CALL MPI_RECV(FRECBUF,PLYIADNOD(IPLY,P)*3, REAL,
     .                      IT_SPMD(P),ITAG, MPI_COMM_WORLD, STAT, IERR)

              DO I=1,PLYIADNOD(IPLY,P)
                 EMPL = IRECBUF(I)-EMPSIZPL
                 WRTBUF(1,EMPL)=FRECBUF(1,I)
                 WRTBUF(2,EMPL)=FRECBUF(2,I)
                 WRTBUF(3,EMPL)=FRECBUF(3,I)
              ENDDO
              DEALLOCATE(IRECBUF,FRECBUF)
            ENDIF
           ENDDO
           CALL WRITE_R_C(WRTBUF,3*PLYSIZG(IPLY))
           DEALLOCATE(WRTBUF)
           EMPSIZPL = EMPSIZPL + PLYSIZG(IPLY)
      ELSE
        PLYSIZ = PLYNOD(IPLY)%PLYNUMNODS
        ALLOCATE (FSENDBUF(3,PLYSIZ))
        ALLOCATE (ISENDBUF(PLYSIZ))
        IF (PLYSIZ > 0) THEN
         DO ND=1,PLYSIZ
           I = PLYNOD(IPLY)%NODES(ND)
           N = NOD_PXFEM(I)

           IF (IFUNC==1)THEN
              FSENDBUF(1,ND) = PLY(IPLY)%V(1,N)
              FSENDBUF(2,ND) = PLY(IPLY)%V(2,N)
              FSENDBUF(3,ND) = PLY(IPLY)%V(3,N)

           ELSEIF (IFUNC==2)THEN
              FSENDBUF(1,ND) = PLY(IPLY)%U(1,N)
              FSENDBUF(2,ND) = PLY(IPLY)%U(2,N)
              FSENDBUF(3,ND) = PLY(IPLY)%U(3,N)

           ELSEIF (IFUNC==3)THEN
              FSENDBUF(1,ND) = PLY(IPLY)%A(1,N)
              FSENDBUF(2,ND) = PLY(IPLY)%A(2,N)
              FSENDBUF(3,ND) = PLY(IPLY)%A(3,N)
           ELSE
              FSENDBUF(1,ND) = ZERO
              FSENDBUF(2,ND) = ZERO
              FSENDBUF(3,ND) = ZERO
           ENDIF

             ISENDBUF(ND) =  PLYNOD(IPLY)%PLYNODID(ND)
         END DO

         ITAG=MSGOFF
         CALL MPI_SEND(ISENDBUF,PLYSIZ,MPI_INTEGER,IT_SPMD(1),
     .                   ITAG,MPI_COMM_WORLD,IERR)

         ITAG=MSGOFF2
         CALL MPI_SEND(FSENDBUF,PLYSIZ*3,REAL,IT_SPMD(1),
     .                   ITAG,MPI_COMM_WORLD,IERR)
C
         DEALLOCATE(ISENDBUF,FSENDBUF)
        ENDIF
      ENDIF
C-----------------------------------------------
#endif
      RETURN
      END
