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
Chd|  SPMD_FVB_ASUB1                source/mpi/anim/spmd_fvb_asub1.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_ASUB1(II, FVPBUF)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER II, FVPBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, PMAIN, J, ITAG, MSGOFF, STAT(MPI_STATUS_SIZE),
     .        IERR, NPOLH_ANIM, IADP
C-----------------------------------------------
      DATA MSGOFF /7051/
C-----------------------------------------------
C
      IADP=0
      DO I=1,NFVBAG
         II=II+1
         PMAIN=FVSPMD(I)%PMAIN
         IF (ISPMD==0) THEN
            IF (ISPMD==PMAIN-1) THEN
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  FVPBUF(IADP+J)=II-1
               ENDDO
               IADP=IADP+FVDATA(I)%NPOLH_ANIM
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NPOLH_ANIM, 1, MPI_INTEGER,
     .                       IT_SPMD(PMAIN), ITAG, MPI_COMM_WORLD,
     .                       STAT, IERR)
               DO J=1,NPOLH_ANIM
                  FVPBUF(IADP+J)=II-1
               ENDDO
               IADP=IADP+NPOLH_ANIM
            ENDIF
         ELSE
            IF (ISPMD==PMAIN-1) THEN
               ITAG=MSGOFF
               CALL MPI_SEND(FVDATA(I)%NPOLH_ANIM, 1, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, MPI_COMM_WORLD,
     .                       IERR)
            ENDIF
         ENDIF
      ENDDO
C
#endif
      RETURN
      END
