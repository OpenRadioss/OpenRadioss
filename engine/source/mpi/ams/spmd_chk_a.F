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
Chd|  SPMD_CHK_A                    source/mpi/ams/spmd_chk_a.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_CHK_A(A        ,ITAB ,IAD_ELEM ,FR_ELEM,LENR  )
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
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "spmd_c.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), LENR,ITAB(*)
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR, MSGOFF,
     .        SIZ,J,K,L,NB_NOD,SIZE,N,INDEX,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),IFIL,FILEN
      DATA MSGOFF/1221/

      DOUBLE PRECISION
     .        RBUF(3*LENR,NSPMD-1), SBUF(3*LENR ),AX,AY,AZ
      CHARACTER FILNAM*100,CYCLENUM*7
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      SIZE = 3
      LOC_PROC = ISPMD + 1
      IF(ISPMD==0) THEN
        DO I = 2, NSPMD
	  SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
          MSGTYP=MSGOFF
          CALL MPI_IRECV(RBUF(1,I-1),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
	IFIL = IDBG5+2
        WRITE(CYCLENUM,'(I7.7)')NCYCLE
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.dadb'

       OPEN(UNIT=IFIL,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+18
C
        NB_NOD = 0
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
	  L=1
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            AX = RBUF(L,INDEX)  -A(1,NOD)
            AY = RBUF(L+1,INDEX)-A(2,NOD)
            AZ = RBUF(L+2,INDEX)-A(3,NOD)
           IF (AX/=ZERO.OR.AY/=ZERO.OR.AZ/=ZERO) THEN
            WRITE(IFIL,'(A,I10,I10,1P5E20.13)' ) '>',I,ITAB(NOD),AX,AY,AZ,RBUF(L,INDEX),A(1,NOD)
             NB_NOD = NB_NOD + 1
           ENDIF
            L = L + SIZE
          END DO
        END DO
        WRITE (ISTDO,1300) FILNAM(1:FILEN),NB_NOD
        WRITE (IOUT,1300) FILNAM(1:FILEN),NB_NOD
        CLOSE(UNIT=IFIL)
C
      ELSE
C preparation envoi  partie fixe (elem) a proc I
          I = 1
	  SIZ = 0
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            SBUF(SIZ+1  ) =  A(1,NOD)
            SBUF(SIZ+2) =  A(2,NOD)
            SBUF(SIZ+3) =  A(3,NOD)
            SIZ = SIZ + SIZE
          END DO
        MSGTYP = MSGOFF
        CALL MPI_SEND(SBUF,SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

      END IF
C
#endif
      RETURN
 1300   FORMAT (4X,' DEBUG FILE:',1X,A,' WRITTEN; ','W/ Num of Diff:',I8)
      END
C

