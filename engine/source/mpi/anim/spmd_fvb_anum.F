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
Chd|  SPMD_FVB_ANUM                 source/mpi/anim/spmd_fvb_anum.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_ANUM(FVOFF, IDMAX, NFVNOD)
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
      INTEGER FVOFF(2,*), IDMAX, NFVNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, PMASTER, J, JJ, ITAG, MSGOFF, STAT(MPI_STATUS_SIZE),
     .        IERR, NNS_ANIM
C-----------------------------------------------
      DATA MSGOFF/7050/
C-----------------------------------------------
C
      DO I=1,NFVBAG
         PMASTER=FVSPMD(I)%PMASTER
         IF (ISPMD.EQ.0) THEN
            IF (ISPMD.EQ.PMASTER-1) THEN
               DO J=1,FVDATA(I)%NNS_ANIM
                  JJ=FVOFF(2,I)+J
                  CALL WRITE_I_C(JJ,1)
               ENDDO
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NNS_ANIM, 1, MPI_INTEGER, IT_SPMD(PMASTER),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               DO J=1,NNS_ANIM
                  JJ=FVOFF(2,I)+J
                  CALL WRITE_I_C(JJ,1)
               ENDDO
            ENDIF
         ELSE
            IF (ISPMD.EQ.PMASTER-1) THEN
               ITAG=MSGOFF
               CALL MPI_SEND(FVDATA(I)%NNS_ANIM, 1, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, MPI_COMM_WORLD,
     .                       IERR)
            ENDIF
         ENDIF
      ENDDO
C
      IF (ISPMD.EQ.0) THEN
         CALL WRITE_I_C(IDMAX+NFVNOD+1,1)
         CALL WRITE_I_C(IDMAX+NFVNOD+2,1)
         CALL WRITE_I_C(IDMAX+NFVNOD+3,1)
      ENDIF     
C
#endif
      RETURN
      END      
