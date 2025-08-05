!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module spmd_gather_nodal_scalar_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief generic subroutine gather all domain array to domain 0 in a global array v_glob(num)
!! \details
       subroutine spmd_gather_nodal_scalar(v_glob,num,v,numnod,weight,nodglob)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use spmd_comm_world_mod, only : spmd_comm_world
        use precision_mod , only : WP
        use spmd_send_mod, only: spmd_send
        use spmd_recv_mod, only: spmd_recv
        use constant_mod , only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
#include "com01_c.inc"
#include "spmd_c.inc"
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
       real(kind=WP) :: V(*)
       INTEGER :: WEIGHT(*),NODGLOB(*),NUM
       integer,intent(in) :: numnod
       real(kind=WP) :: V_GLOB(NUM)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/7014/
      DATA MSGOFF2/7015/
      real(kind=WP), DIMENSION(:) , ALLOCATABLE :: BUFSR
      INTEGER, DIMENSION(:) , ALLOCATABLE :: IBUF
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
       ALLOCATE(BUFSR(NUMNODM), IBUF(NUMNODM))
       bufsr = zero
       IF (ISPMD /= 0) THEN
         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I)==1) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(SIZ) = V(I) 
           END IF
         END DO 

         MSGTYP = MSGOFF2
         CALL spmd_send(IBUF,SIZ,IT_SPMD(1),MSGTYP)
         MSGTYP = MSGOFF 
         CALL spmd_SEND(BUFSR,SIZ,IT_SPMD(1),MSGTYP)

       ELSE

          siz = 0
          DO I=1,NUMNOD
            IF (WEIGHT(I)==1) THEN 
              siz = siz +1
              NG = NODGLOB(I)
              V_GLOB(NG) = V(I)
            ENDIF
          ENDDO

          DO I=2,NSPMD
            !--- Receiving the full buffer of NODGLOB addresses
            MSGTYP = MSGOFF2
            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,SPMD_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)
            CALL spmd_RECV(IBUF,SIZ,IT_SPMD(I),MSGTYP)
            !--- Receiving the floating buffer of NODGLOB addresses
            MSGTYP = MSGOFF 
            CALL spmd_RECV(BUFSR,SIZ,IT_SPMD(I),MSGTYP)
            NREC = SIZ
            DO K = 1, NREC
              NG = IBUF(K)
              V_GLOB(NG) = BUFSR(K)
            ENDDO
          ENDDO

       ENDIF
       DEALLOCATE(BUFSR,IBUF)
#endif

       end subroutine spmd_gather_nodal_scalar
       end module spmd_gather_nodal_scalar_mod
