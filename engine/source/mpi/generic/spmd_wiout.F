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
C
Chd|====================================================================
Chd|  SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|-- called by -----------
Chd|        FVSTATS                       source/airbag/fvstats.F       
Chd|        FXBYPID                       source/constraints/fxbody/fxbypid.F
Chd|        LECTUR                        source/input/lectur.F         
Chd|        RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SZ_PRINT                      source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        IO_MOD                        share/modules/io_mod.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_WIOUT(IOUT,IWIOUT)
C ecrit le buffer L01 sur p0
      USE MESSAGE_MOD 
      USE IO_MOD
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
      INTEGER IOUT, IWIOUT
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,LOC_PROC,
     .        BUFSIZ,MSGOFF,SIZ,I,P,LEN,N,II,
     .        IOS,STATUS(MPI_STATUS_SIZE),IERROR,IBLANC,LNEW
      INTEGER, DIMENSION(:),ALLOCATABLE :: IWA


      INTEGER :: NINDX_PROC,SIZE_MESSAGE
	  INTEGER, DIMENSION(NSPMD) :: IWIOUT_SPMD
	  INTEGER, DIMENSION(NSPMD) :: PROC_RCV,DISPLACEMENT
      INTEGER :: INDX,REQ_S
      INTEGER, DIMENSION(NSPMD) :: REQ_R

      DATA MSGOFF/105/
      CHARACTER(LEN=NCHAROUT) :: LINE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C IWIOUT est utilise pour connaitre le nb max de characteres a echanger
      ! MPI comm : every proc sends its IWIOUT_SAVE to the main proc
      IWIOUT_SPMD(1:NSPMD) = 0
      CALL MPI_GATHER( IWIOUT_SAVE,1,MPI_INTEGER,
     .                 IWIOUT_SPMD,1,MPI_INTEGER,
     .                 0,MPI_COMM_WORLD,ierror)


      LOC_PROC = ISPMD + 1
      LEN = NCHAROUT
      IF(ISPMD/=0) ALLOCATE(IWA(IWIOUT+1))
!   ---------------------------------
!   Main proc : receives the message from other proc and writes the message in the *.out file
      IF(ISPMD==0) THEN
        NINDX_PROC = 0
        SIZE_MESSAGE = 0
        !   ------------------
        DISPLACEMENT(1:NSPMD) = 0
        DO P = 2, NSPMD
            IF(IWIOUT_SPMD(P)/=0) THEN
                NINDX_PROC = NINDX_PROC + 1 
                PROC_RCV(NINDX_PROC) = P
                DISPLACEMENT(NINDX_PROC) = SIZE_MESSAGE
                SIZE_MESSAGE = SIZE_MESSAGE + IWIOUT_SPMD(P) + 1
            ENDIF
        ENDDO
        ALLOCATE(IWA(SIZE_MESSAGE))
        !   ------------------
        DO I=1,NINDX_PROC
          P = PROC_RCV(I)
          MSGTYP = MSGOFF
          CALL MPI_IRECV(IWA(1+DISPLACEMENT(I)),IWIOUT_SPMD(P)+1,MPI_INTEGER,IT_SPMD(P),
     .                  MSGTYP,MPI_COMM_WORLD,REQ_R(I),ierror)


        ENDDO
        !   ------------------
        DO II=1,NINDX_PROC
            CALL MPI_WAITANY(NINDX_PROC,REQ_R,INDX,STATUS,ierror)
            P = PROC_RCV(INDX)
            SIZ = IWIOUT_SPMD(P)
            DO N = 1, SIZ, LEN
Cel traitement special pour eliminer les blancs de fin de chaine
                I = LEN
                IBLANC = ICHAR(' ')
                DO WHILE (IWA(DISPLACEMENT(INDX)+N+I)==IBLANC.AND.I>1)
                    I = I-1
                ENDDO
                LNEW = I
C
                DO I = 1, LNEW
                    LINE(I:I) = CHAR(IWA(DISPLACEMENT(INDX)+N+I))
                ENDDO
                WRITE(IOUT,FMT='(A)')LINE(1:LNEW)
            ENDDO
        ENDDO
        !   ------------------
!   ---------------------------------
      ELSE
!   Secondary proc : sends the message to the main proc
C rewind fait ds check        REWIND(UNIT=IOUT)
        IF(IWIOUT_SAVE>0) THEN
            IOS = 0
            SIZ = 1
            !   ------------------
            DO WHILE(IOS==0)
              READ(UNIT=IOUT,IOSTAT=IOS,FMT='(A)') LINE
              IF(IOS==0) THEN
                DO I = 1, LEN
                  IWA(SIZ+I) = ICHAR(LINE(I:I))
                ENDDO
                SIZ = SIZ + LEN
              ENDIF
            ENDDO
            !   ------------------
            IWA(1) = SIZ-1
            REWIND(UNIT=IOUT)
            WRITE(UNIT=IOUT,IOSTAT=IOS,FMT='(A)')
            MSGTYP = MSGOFF
            CALL MPI_ISEND(IWA   ,SIZ           ,MPI_INTEGER,IT_SPMD(1),
     .                    MSGTYP,MPI_COMM_WORLD,REQ_S,ierror     )

            CALL MPI_WAIT(REQ_S,STATUS,IERROR)
            !   ------------------
        ENDIF
      ENDIF
!   ---------------------------------
      DEALLOCATE(IWA)
C

#endif
      RETURN
      END SUBROUTINE SPMD_WIOUT
