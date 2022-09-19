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
Chd|  SPMD_COLLECT_MULTI_FVM        source/mpi/output/spmd_collect_multi_fvm.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_COLLECT_MULTI_FVM(IXS,MULTI_FVM)
!$COMMENT
!       SPMD_COLLECT_MULTI_FVM description
!       creation of a file with hexa values of MULTI_FVM%VEL
!
!       SPMD_COLLECT_MULTI_FVM organization :
!        * gather of MULTI_FVM values on the main processor
!        * sorting of the value according the global user ID of the
!          element
!        * creation of the *.adb file
!
!$ENDCOMMENT
      USE INOUTFILE_MOD
      USE DEBUG_MOD
      USE MULTI_FVM_MOD
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
#include      "spmd_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NIXS,*), INTENT(in) :: IXS
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGOFF0,MSGTYP,INFO,I,K,NG,N,
     .        EMPL,FILEN
      CHARACTER FILNAM*100,CYCLENUM*7
      INTEGER :: LEN_TMP_NAME,SHIFT
      CHARACTER(len=2148) :: TMP_NAME
      INTEGER, DIMENSION(NSPMD) :: SIZE_NUMELS
      INTEGER, DIMENSION(:), ALLOCATABLE :: SDNODG
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: AGLOB,RECGLOB
      ! array for sorting
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITRI,WORK,INDEX_TRI

#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      DATA MSGOFF0/176/
      DATA MSGOFF/177/
#endif
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(ISPMD==0) THEN
        ALLOCATE( SDNODG(NUMELSG) )
        ALLOCATE( RECGLOB(4,NUMELSG) )
      ELSE
        ALLOCATE( SDNODG(NUMELS) )
        ALLOCATE( RECGLOB(0,0) )
        ALLOCATE( AGLOB(4,NUMELS) )
      ENDIF

      SIZE_NUMELS(1:NSPMD) = 0
#ifdef MPI
      IF (ISPMD/=0) THEN

      DO I = 1,NUMELS
!        SDNODG(I) = IXS(NIXS,I)
        AGLOB(1,I) = IXS(NIXS,I)
        AGLOB(2,I) = MULTI_FVM%VEL(1,I)
        AGLOB(3,I) = MULTI_FVM%VEL(2,I)
        AGLOB(4,I) = MULTI_FVM%VEL(3,I)
      ENDDO

        MSGTYP=MSGOFF0
        CALL MPI_SEND(NUMELS,1,MPI_INTEGER,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)

        MSGTYP=MSGOFF
        CALL MPI_SEND(AGLOB,4*NUMELS,MPI_DOUBLE_PRECISION,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
      ELSE
        SIZE_NUMELS(1) = NUMELS
        DO K=2,NSPMD
            MSGTYP=MSGOFF0
            CALL MPI_RECV(SIZE_NUMELS(K),1,MPI_INTEGER,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)

            ALLOCATE( AGLOB(4,SIZE_NUMELS(K)) )

            MSGTYP=MSGOFF
            CALL MPI_RECV(AGLOB,4*SIZE_NUMELS(K),MPI_DOUBLE_PRECISION,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)

            SHIFT = 0
            DO I=1,K-1
                SHIFT = SHIFT + SIZE_NUMELS(I)
            ENDDO
            DO I=1,SIZE_NUMELS(K)
                RECGLOB(1,I+SHIFT) = AGLOB(1,I)
                RECGLOB(2,I+SHIFT) = AGLOB(2,I)
                RECGLOB(3,I+SHIFT) = AGLOB(3,I)
                RECGLOB(4,I+SHIFT) = AGLOB(4,I)
            ENDDO
            DEALLOCATE( AGLOB )
        END DO
      ENDIF
#endif
C

        IF(ISPMD==0) THEN
            WRITE(CYCLENUM,'(I7.7)')NCYCLE
            FILNAM=ROOTNAM(1:ROOTLEN)//'_FVM_'//CHRUN//'_'//CYCLENUM//'.adb'

            LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

            OPEN(UNIT=IDBG5,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

            FILEN = ROOTLEN+17

            DO I=1,NUMELS
                N =  IXS(NIXS,I)
                RECGLOB(1,I) = IXS(NIXS,I)
                RECGLOB(2,I) = MULTI_FVM%VEL(1,I)
                RECGLOB(3,I) = MULTI_FVM%VEL(2,I)
                RECGLOB(4,I) = MULTI_FVM%VEL(3,I)
            ENDDO

            ALLOCATE( ITRI(NUMELSG) )
            ALLOCATE( INDEX_TRI(2*NUMELSG) )
            ALLOCATE( WORK(70000) )
            DO I=1,NUMELSG
                INDEX_TRI(I) = I
                ITRI(I) = nint(RECGLOB(1,I))
            ENDDO

            CALL MY_ORDERS(0,WORK,ITRI,INDEX_TRI,NUMELSG,1)


            DO I = 1, NUMELSG
                N = INDEX_TRI(I)
                WRITE(IDBG5,'(A,I10,I10,Z20,Z20,Z20)' ) '>',NCYCLE,NINT(RECGLOB(1,N)),RECGLOB(2,N),RECGLOB(3,N),RECGLOB(4,N)
            END DO
            WRITE (IOUT,1300)  FILNAM(1:FILEN)
            WRITE (ISTDO,1300) FILNAM(1:FILEN)
            CLOSE(UNIT=IDBG5)

            DEALLOCATE( ITRI )
            DEALLOCATE( INDEX_TRI )
            DEALLOCATE( WORK )

      END IF

        IF(ALLOCATED(SDNODG)) DEALLOCATE( SDNODG )
        IF(ALLOCATED(RECGLOB)) DEALLOCATE( RECGLOB )
        IF(ALLOCATED(AGLOB)) DEALLOCATE( AGLOB )
C
 1300   FORMAT (4X,' DEBUG ANALYSIS FVM FILE :',1X,A,' WRITTEN')
      RETURN
      END SUBROUTINE SPMD_COLLECT_MULTI_FVM
