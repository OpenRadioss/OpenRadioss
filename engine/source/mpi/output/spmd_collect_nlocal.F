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
Chd|  SPMD_COLLECT_NLOCAL           source/mpi/output/spmd_collect_nlocal.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_COLLECT_NLOCAL(A      ,SIZEA   ,NUMNOD_LOCAL,
     .                               POSI   ,NLOC_DMG,SIZP0       ,
     .                               NODGLOB,ITAB    )

      USE INOUTFILE_MOD
      USE DEBUG_MOD
      USE NLOCAL_REG_MOD
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
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),NUMNOD_LOCAL,NODGLOB(*)
      INTEGER NPOS,SIZP0
      INTEGER MSGOFF,MSGOFF0,MSGTYP,INFO,I,K,NG,N,
     .        EMPL,SDNODG(NUMNODM),FILEN 
      DOUBLE PRECISION
     .        AGLOB(2,NUMNODM),RECGLOB(2,SIZP0) 
C     REAL

C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER POSI(*)
      INTEGER SIZEA
      my_real 
     .   A(SIZEA)
      TYPE(NLOCAL_STR_), TARGET                  :: NLOC_DMG 
      CHARACTER FILNAM*100,CYCLENUM*7

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME

#ifdef MPI 
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      DATA MSGOFF0/176/
      DATA MSGOFF/177/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      RECGLOB(1:2,1:SIZP0) = -1
      IF (ISPMD/=0) THEN
      N=0
      DO I = 1,NUMNOD_LOCAL
        NPOS = POSI(I)                              ! Position of its first d.o.f in A
        
        N=N+1
        SDNODG(N) = NODGLOB(NLOC_DMG%INDX(I))
        AGLOB(1,N) = ITAB( NLOC_DMG%INDX(I) )
        AGLOB(2,N) = A(NPOS)
      ENDDO


        MSGTYP=MSGOFF0
        CALL MPI_SEND(SDNODG,N,MPI_INTEGER,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        MSGTYP=MSGOFF
        CALL MPI_SEND(AGLOB,2*N,MPI_DOUBLE_PRECISION,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
      ELSE

        DO K=2,NSPMD
            MSGTYP=MSGOFF0
            CALL MPI_RECV(SDNODG,NUMNODM,MPI_INTEGER,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)

            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,N,ierror)

            MSGTYP=MSGOFF
            CALL MPI_RECV(AGLOB,2*N,MPI_DOUBLE_PRECISION,
     .                    IT_SPMD(K),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,IERROR)


            DO I=1,N
                EMPL = SDNODG(I)
                RECGLOB(1,EMPL) = AGLOB(1,I)
                RECGLOB(2,EMPL) = AGLOB(2,I)
            ENDDO
        END DO
      ENDIF
#endif
C

        IF(ISPMD==0) THEN
            WRITE(CYCLENUM,'(I7.7)')NCYCLE
            FILNAM=ROOTNAM(1:ROOTLEN)//'_NLOCAL_'//CHRUN//'_'//CYCLENUM//'.adb'

            LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM)) 
       
            OPEN(UNIT=IDBG5,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

            FILEN = ROOTLEN+17

            DO I=1,NUMNOD_LOCAL
                NPOS = POSI(I)                              ! Position of its first d.o.f in A
                N =  NODGLOB(NLOC_DMG%INDX(I))
                RECGLOB(1,N) = ITAB( NLOC_DMG%INDX(I) )
                RECGLOB(2,N) = A(NPOS)
            ENDDO

        
            DO I = 1, NUMNODG
                IF(NINT(RECGLOB(1,I))/=-1) THEN
                    WRITE(IDBG5,'(A,I10,I10,Z20)' ) '>',NCYCLE,NINT(RECGLOB(1,I)),RECGLOB(2,I)
                ENDIF
            END DO
            WRITE (IOUT,1300)  FILNAM(1:FILEN)
            WRITE (ISTDO,1300) FILNAM(1:FILEN)
            CLOSE(UNIT=IDBG5)

      END IF
C
 1300   FORMAT (4X,' DEBUG ANALYSIS NLOCAL FILE :',1X,A,' WRITTEN')
      RETURN
      END SUBROUTINE SPMD_COLLECT_NLOCAL
