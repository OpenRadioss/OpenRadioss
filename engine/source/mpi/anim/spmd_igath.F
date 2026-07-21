Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    spmd_igath            ../engine/source/mpi/anim/spmd_igath.F
!||--- called by ------------------------------------------------------
!||    aniskew               ../engine/source/output/anim/generate/aniskew.F
!||    aniskewf              ../engine/source/output/anim/generate/aniskewf.F
!||    dsphnor               ../engine/source/output/anim/generate/dsphnor.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_IGATH(SRBUF,LEN,LRECV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_MOD
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
      INTEGER SRBUF(*), LEN, LRECV
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,MSGTYP,I,RLEN,S1
      DATA MSGOFF/7013/
      DATA MSGOFF2/7068/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      S1 = 1
      IF (ISPMD/=0) THEN
        MSGTYP=MSGOFF2

        CALL spmd_send(LEN,S1,IT_SPMD(1),MSGTYP)

        MSGTYP=MSGOFF
        IF(LEN > 0 ) THEN
          CALL spmd_send(SRBUF(1:LEN),LEN,IT_SPMD(1),MSGTYP)
        ENDIF

      ELSE

        LRECV = LEN

        DO I=2,NSPMD
            MSGTYP=MSGOFF2
            CALL spmd_recv(RLEN,S1,IT_SPMD(I),MSGTYP)
            MSGTYP=MSGOFF

            IF(RLEN > 0 ) THEN
              ! Even though Receive of length = 0 can cost nothing
              ! SRBUF(LRECV+1) must exist
              CALL spmd_recv(SRBUF(LRECV+1:LRECV+RLEN),RLEN,
     .                       IT_SPMD(I),MSGTYP)

            ENDIF
            LRECV = LRECV + RLEN 

        END DO

      ENDIF
C
#endif
      RETURN
      END
