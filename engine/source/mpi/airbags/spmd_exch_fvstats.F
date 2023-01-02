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
Chd|  SPMD_EXCH_FVSTATS             source/mpi/airbags/spmd_exch_fvstats.F
Chd|-- called by -----------
Chd|        ECRIT                         source/output/ecrit.F         
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_FVSTATS(MONVOL)                           
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  MONVOL(*)
#ifdef MPI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, N, ITYP, ID, IFV, LEN
      INTEGER BUFS(6,NVOLU), BUFR(6,NVOLU)
      INTEGER IERROR

C
      K1 = 1
      IFV = 0
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP==6 .OR. ITYP==8) THEN
           IFV = IFV + 1
           ID=MONVOL(K1-1+1)
           IF((FVSPMD(IFV)%PMAIN-1 == ISPMD) .OR. (NSPMD == 1)) THEN
             BUFS(1,IFV)=ID                       
             BUFS(2,IFV)=MONVOL(K1-1+49)
             BUFS(3,IFV)=MONVOL(K1-1+70)
             BUFS(4,IFV)=MONVOL(K1-1+71)
             BUFS(5,IFV)=MONVOL(K1-1+72)
             BUFS(6,IFV)=MONVOL(K1-1+73)
           ELSE
      !Each processor fills the buffer with 0 if not PMAIN of the
      !FVMBAG                    
             BUFS(1,IFV) = 0
             BUFS(2,IFV) = 0
             BUFS(3,IFV) = 0
             BUFS(4,IFV) = 0
             BUFS(5,IFV) = 0
             BUFS(6,IFV) = 0
           ENDIF
         ENDIF
         K1=K1+NIMV
      ENDDO


      !Exchange
      ! ALLREDUCE is OK there (soon after another global
      ! synchronization)
      IF(IFV > 0 ) THEN
        LEN = IFV*6
        CALL MPI_ALLREDUCE(BUFS,BUFR,LEN,
     .                  MPI_INTEGER,MPI_SUM,
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF




      ! Proc 0 updates its monvol
      IF ( ISPMD == 0 ) THEN 
        K1 = 1
        IFV = 0
        DO N=1,NVOLU
           ITYP=MONVOL(K1-1+2)
           IF (ITYP==6 .OR. ITYP==8) THEN
             IFV = IFV + 1
             MONVOL(K1-1+1)  = BUFR(1,IFV)
             MONVOL(K1-1+49) = BUFR(2,IFV)
             MONVOL(K1-1+70) = BUFR(3,IFV)
             MONVOL(K1-1+71) = BUFR(4,IFV)
             MONVOL(K1-1+72) = BUFR(5,IFV)
             MONVOL(K1-1+73) = BUFR(6,IFV)
           ENDIF
           K1=K1+NIMV
        ENDDO
      ENDIF


 
#endif
      RETURN
      END

