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
Chd|  SPMD_ANIM_PLY_XYZNOD          source/mpi/anim/spmd_anim_ply_xyznod.F
Chd|-- called by -----------
Chd|        XYZNOD_PLY                    source/output/anim/generate/xyznod_ply.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_ANIM_PLY_XYZNOD( NODGLOB,IPLY, IDPLY,
     *                                 NOD_PXFEM, X, ZI_PLY,EMPSIZPL )
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
      INTEGER NODGLOB(*),NOD_PXFEM(*),EMPSIZPL
      INTEGER IPLY,IDPLY
      my_real
     .   X(3,*),ZI_PLY(NPLYXFE,*)
      REAL R4
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,STAT(MPI_STATUS_SIZE,NSPMD-1), IERR
C-----------------------------------------------
      DATA MSGOFF/7055/
      DATA MSGOFF2/7056/
C-----------------------------------------------
      INTEGER I,N,II,ND,EMPL,P,ITAG
      INTEGER PLYSIZ
      my_real
     .   VN,VAL,NORM
C
      REAL , DIMENSION(:,:), ALLOCATABLE :: WRTBUF
      my_real
     * , DIMENSION(:,:), ALLOCATABLE :: FSENDBUF,FRECBUF
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISENDBUF,IRECBUF
C-----------------------------------------------

       II = IDPLY
C

      IF (ISPMD ==0) THEN
        PLYSIZ  = PLYNOD (IPLY)%PLYNUMNODS
        ALLOCATE(WRTBUF(3,PLYSIZG(IPLY)))

C Les plynods du proc 0
        DO ND=1,PLYSIZ
             I = PLYNOD(IPLY)%NODES(ND)
             N = NOD_PXFEM(I)
             NORM = SQRT(VN_NOD(1,N)**2+VN_NOD(2,N)**2+VN_NOD(3,N)**2)
             NORM = UN/MAX(EM20,NORM)
             EMPL = PLYNOD(IPLY)%PLYNODID(ND)-EMPSIZPL
             
             VN = VN_NOD(1,N)*NORM
             VAL = X(1,I)+ZI_PLY(N,IPLY)*VN + PLY(IPLY)%U(1,N)
             WRTBUF(1,EMPL)= VAL

             VN = VN_NOD(2,N)*NORM
             VAL = X(2,I)+ZI_PLY(N,IPLY)*VN + PLY(IPLY)%U(2,N)
             WRTBUF(2,EMPL)= VAL

             VN = VN_NOD(3,N)*NORM
             VAL = X(3,I)+ZI_PLY(N,IPLY)*VN + PLY(IPLY)%U(3,N)
             WRTBUF(3,EMPL)= VAL
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
           EMPSIZPL=EMPSIZPL+PLYSIZG(IPLY)
      ELSE
        PLYSIZ = PLYNOD(IPLY)%PLYNUMNODS
        ALLOCATE (FSENDBUF(3,PLYSIZ))
        ALLOCATE (ISENDBUF(PLYSIZ))
        IF (PLYSIZ > 0) THEN
         DO ND=1,PLYSIZ
           I = PLYNOD(IPLY)%NODES(ND)
           N = NOD_PXFEM(I)
           NORM = SQRT(VN_NOD(1,N)**2+VN_NOD(2,N)**2+VN_NOD(3,N)**2)
           NORM = UN/MAX(EM20,NORM)
           VN = VN_NOD(1,N)*NORM
           FSENDBUF(1,ND) = X(1,I)+ZI_PLY(N,IPLY)*VN + PLY(IPLY)%U(1,N)
           VN = VN_NOD(2,N)*NORM
           FSENDBUF(2,ND) = X(2,I)+ZI_PLY(N,IPLY)*VN + PLY(IPLY)%U(2,N)
           VN = VN_NOD(3,N)*NORM
           FSENDBUF(3,ND) = X(3,I)+ZI_PLY(N,IPLY)*VN + PLY(IPLY)%U(3,N)
           ISENDBUF(ND) = PLYNOD(IPLY)%PLYNODID(ND)
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
