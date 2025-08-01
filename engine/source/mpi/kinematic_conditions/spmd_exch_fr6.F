Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spmd_exch_fr6         ../engine/source/mpi/kinematic_conditions/spmd_exch_fr6.F
!||--- called by ------------------------------------------------------
!||    airbagb               ../engine/source/airbag/airbag2.F
!||    airbagb1              ../engine/source/airbag/airbagb1.F
!||    damping_vref          ../engine/source/assembly/damping_vref.F
!||    get_volume_area       ../engine/source/airbag/get_volume_area.F90
!||    poro                  ../engine/source/ale/porous/poro.F
!||    rbyact                ../engine/source/constraints/general/rbody/rbyact.F
!||    rbypid                ../engine/source/constraints/general/rbody/rbypid.F
!||    rgwal0                ../engine/source/constraints/general/rwall/rgwal0.F
!||    rgwal0_imp            ../engine/source/constraints/general/rwall/rgwal0.F
!||    rgwath                ../engine/source/interfaces/int09/rgwath.F
!||    rlink0                ../engine/source/constraints/general/rlink/rlink0.F
!||    rlink10               ../engine/source/constraints/general/rlink/rlink10.F
!||    rlink11               ../engine/source/constraints/general/rlink/rlink10.F
!||    sms_rgwal_0           ../engine/source/ams/sms_rgwal0.F
!||    sms_rlink10           ../engine/source/ams/sms_rlink.F
!||    sms_rlink11           ../engine/source/ams/sms_rlink.F
!||    spgauge               ../engine/source/elements/sph/spgauge.F
!||    volpvgb               ../engine/source/airbag/volpvg.F
!||    volum0                ../engine/source/airbag/volum0.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_EXCH_FR6(FR,FS6,LEN)
C cumul force sur frontiere en parith/on
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
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
      INTEGER LEN, FR(*)
      DOUBLE PRECISION FS6(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,P,PMAIN, LOC_PROC, J, L,
     .        IDEB, NBINDEX, INDEX, REQ(NSPMD-1)
      DATA MSGOFF/162/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DOUBLE PRECISION FTMP(LEN*(NSPMD-1))
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      PMAIN = FR(NSPMD+2)
      LOC_PROC = ISPMD+1
      IF(LOC_PROC==PMAIN) THEN
        IDEB = 1
        L = 0
        DO P = 1, NSPMD
          IF(LOC_PROC/=P.AND.FR(P)/=0)THEN
            L=L+1
C           INDEXI(L)=P
            MSGTYP = MSGOFF
            CALL MPI_IRECV(FTMP(IDEB)  ,LEN ,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     .                    MSGTYP,SPMD_COMM_WORLD,REQ(L),IERROR    )
            IDEB = IDEB + LEN
          END IF
        END DO
        NBINDEX = L
C
        DO L = 1, NBINDEX
          CALL MPI_WAITANY(NBINDEX,REQ,INDEX,STATUS,IERROR)
C            P = INDEXI(INDEX)
          IDEB = LEN*(INDEX-1)
          DO J = 1, LEN
            FS6(J) = FS6(J) + FTMP(IDEB+J)
          END DO
        ENDDO
C
        DO P = 1, NSPMD
          IF(LOC_PROC/=P.AND.FR(P)/=0)THEN
            MSGTYP = MSGOFF 
            CALL MPI_SEND(FS6   ,LEN ,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     .                    MSGTYP,SPMD_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C
      ELSE
        IF(FR(LOC_PROC)/=0) THEN
          MSGTYP = MSGOFF
          CALL MPI_SEND(FS6   ,LEN ,MPI_DOUBLE_PRECISION,IT_SPMD(PMAIN),
     .                  MSGTYP,SPMD_COMM_WORLD,IERROR)
          CALL MPI_RECV(FS6   ,LEN ,MPI_DOUBLE_PRECISION,IT_SPMD(PMAIN),
     .                  MSGTYP,SPMD_COMM_WORLD,STATUS,IERROR    )
        ENDIF
      ENDIF
C
#endif
      RETURN
      END
