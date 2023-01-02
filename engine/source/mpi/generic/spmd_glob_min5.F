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
C==========================================================================
Chd|====================================================================
Chd|  SPMD_GLOB_MIN5                source/mpi/generic/spmd_glob_min5.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_MIN5(DT2  ,ITYPTS,NELTS,ICODT,IMSCH,
     .                         TSTOP,IWIOUT,MSTOP,ISMSCH,
     .                         INT24USE,NBINTC,INTLIST,IPARI,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITYPTS, NELTS, ICODT ,IMSCH, IWIOUT,MSTOP,
     .        ISMSCH
C Interface type 24 communication addon
      INTEGER INT24USE,NBINTC,INTLIST(*),IPARI(NPARI,*)
      my_real
     .        DT2, TSTOP
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IERROR, LOC_PROC, MSTOP1, MSTOP2, LEN, myop,
     .        STATUS(MPI_STATUS_SIZE)
      my_real,
     . DIMENSION(:),ALLOCATABLE :: RBUF, SBUF 
C interface type 24 comm Addon
      INTEGER NIN,NI,NTY
      INTEGER ::  MY_TYPE
      INTEGER GLOB_MIN
      EXTERNAL GLOB_MIN
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(INT24USE==1)THEN
        LEN = 10 + NBINTC
      ELSE
        LEN = 10
      ENDIF
      ALLOCATE(RBUF(LEN))
      ALLOCATE(SBUF(LEN))
      RBUF(1:LEN)=ZERO
      
      MSTOP1=0
      MSTOP2=0
      IF(MSTOP==1)MSTOP1=1
      IF(MSTOP==2)MSTOP2=1
C
      LOC_PROC=ISPMD+1
C
      SBUF(1) = DT2
      SBUF(2) = ITYPTS
      SBUF(3) = NELTS 
      SBUF(4) = IEXICODT
      SBUF(5) = IMSCH
      SBUF(6) = TSTOP
      SBUF(7) = IWIOUT
      SBUF(8) = MSTOP1
      SBUF(9) = MSTOP2
      SBUF(10)= ISMSCH
      
      IF (INT24USE==1)THEN
         DO NI=1,NBINTC
            NIN = INTLIST(NI)
            NTY   = IPARI( 7,NIN)
            IF(NTY==24.OR.NTY==25) THEN
              SBUF(10+NI) = INTBUF_TAB(NIN)%VARIABLES(23)
            ELSE
              SBUF(10+NI) = ZERO
            ENDIF
         ENDDO
      ENDIF

      CALL MPI_TYPE_CONTIGUOUS(10,REAL,MY_TYPE,IERROR)
      CALL MPI_TYPE_COMMIT(MY_TYPE,IERROR)
C
      CALL MPI_OP_CREATE(GLOB_MIN, .TRUE., MYOP, IERROR)
      
      CALL MPI_ALLREDUCE(SBUF, RBUF, 1, MY_TYPE, MYOP,
     &                   MPI_COMM_WORLD, IERROR)
     
      IF (INT24USE==1)THEN
        CALL MPI_ALLREDUCE(SBUF(11), RBUF(11), NBINTC, REAL , MPI_MAX,
     &                   MPI_COMM_WORLD, IERROR)
      ENDIF

      CALL MPI_OP_FREE(MYOP, IERROR)
      CALL MPI_TYPE_FREE(MY_TYPE,IERROR)

C
      DT2      = RBUF(1)
      ITYPTS   = NINT(RBUF(2))
      NELTS    = NINT(RBUF(3))
      IEXICODT = NINT(RBUF(4))
      IMSCH    = NINT(RBUF(5))
      TSTOP    = RBUF(6)
      IWIOUT   = NINT(RBUF(7))
      MSTOP1   = NINT(RBUF(8))
      MSTOP2   = NINT(RBUF(9))
      IF(MSTOP1/=0)MSTOP=1
      IF(MSTOP2/=0)MSTOP=2
      ISMSCH   = RBUF(10)
C
      IF (INT24USE==1)THEN
         DO NI=1,NBINTC
            NIN = INTLIST(NI)
            NTY   = IPARI( 7,NIN)
            IF(NTY==24.OR.NTY==25) THEN
              INTBUF_TAB(NIN)%VARIABLES(23) = RBUF(10+NI)
            ENDIF
         ENDDO
      ENDIF
C
      DEALLOCATE(RBUF)
      DEALLOCATE(SBUF)
C
#endif
      RETURN
      END  
