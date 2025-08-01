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
!||    spmd_exch_userwi       ../engine/source/mpi/user_interface/spmd_exch_userwi.F
!||--- called by ------------------------------------------------------
!||    user_windows_routine   ../engine/source/user_interface/user_windows.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod    ../engine/source/mpi/spmd_comm_world.F90
!||    user_windows_mod       ../common_source/modules/user_windows_mod.F
!||====================================================================
      SUBROUTINE SPMD_EXCH_USERWI(A,AR,USER_WINDOWS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE USER_WINDOWS_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(USER_WINDOWS_),INTENT(INOUT) :: USER_WINDOWS
      my_real
     *  A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,P,I,ND,IAD,NUM,SIZ,
     .        STATUS(MPI_STATUS_SIZE),IERROR,LOC_PROC

      my_real
     * , DIMENSION(:,:), ALLOCATABLE ::  RBUF,SBUF

      DATA MSGOFF/227/
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IF (ISPMD==0)THEN
        SIZ = USER_WINDOWS%IAD_USERW(NSPMD+1)-1
        ALLOCATE( SBUF(6,SIZ))
        DO P=2,NSPMD
          NUM = USER_WINDOWS%IAD_USERW(P+1)-USER_WINDOWS%IAD_USERW(P)

          IF(NUM > 0)THEN
            IAD = USER_WINDOWS%IAD_USERW(P)
            DO I=1,NUM
              ND = USER_WINDOWS%FR_USERW(IAD+I-1)
              SBUF(1,IAD+I-1)=A(1,ND)
              SBUF(2,IAD+I-1)=A(2,ND)
              SBUF(3,IAD+I-1)=A(3,ND)
              SBUF(4,IAD+I-1)=AR(1,ND)
              SBUF(5,IAD+I-1)=AR(2,ND)
              SBUF(6,IAD+I-1)=AR(3,ND)
            ENDDO
          ENDIF
        ENDDO

        DO P=2,NSPMD
           NUM = USER_WINDOWS%IAD_USERW(P+1)-USER_WINDOWS%IAD_USERW(P)
           IAD = USER_WINDOWS%IAD_USERW(P)
           IF(NUM > 0)THEN
             MSGTYP=MSGOFF
             CALL MPI_SEND(SBUF(1,IAD),6*NUM,REAL,IT_SPMD(P),MSGTYP,
     .                SPMD_COMM_WORLD,ierror)
           ENDIF
        ENDDO
        DEALLOCATE(SBUF)
      ELSE
        NUM = USER_WINDOWS%S_FR_USERW
        ALLOCATE(RBUF(6,NUM))
        IF(NUM > 0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(RBUF,6*NUM,REAL,IT_SPMD(1),MSGTYP,
     .                SPMD_COMM_WORLD,STATUS,IERROR)
          DO I=1,NUM
            ND=USER_WINDOWS%FR_USERW(I)
            A(1,ND)=A(1,ND)+RBUF(1,I)
            A(2,ND)=A(2,ND)+RBUF(2,I)
            A(3,ND)=A(3,ND)+RBUF(3,I)
            AR(1,ND)=A(1,ND)+RBUF(4,I)
            AR(2,ND)=A(2,ND)+RBUF(5,I)
            AR(3,ND)=A(3,ND)+RBUF(6,I)
          ENDDO
        ENDIF
        DEALLOCATE(RBUF)
      ENDIF

#endif
      RETURN
      END
