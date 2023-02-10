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
CCgw|============================================================
Chd|====================================================================
Chd|  W_IBCL                        source/restart/ddsplit/w_ibcl.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_IBCL(IBCL,CEP,PROC,NODLOCAL,NCONLD_L,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_IA, PROC, NCONLD_L, IBCL(NIBCLD,*), CEP(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IBTMP(NIBCLD,NCONLD_L), N1, N2, N3, N4, OFF, NB_L
C
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR
     +    + NUMELX
      NB_L = 0
C
      DO I = 1, NCONLD
        IF(CEP(I+OFF)==PROC) THEN
          NB_L = NB_L + 1
          N1 = IBCL(1,I)
          N2 = IBCL(2,I)
          N3 = IBCL(3,I)
          N4 = IBCL(4,I)
          IBTMP(1,NB_L) = NODLOCAL(N1)
          DO J = 2, NIBCLD
            IBTMP(J,NB_L) = IBCL(J,I)
          END DO
          IF(N4/=-1) THEN
            IBTMP(2,NB_L) = NODLOCAL(N2)
            IF(N3/=0)IBTMP(3,NB_L) = NODLOCAL(N3)
            IF(N4/=0)IBTMP(4,NB_L) = NODLOCAL(N4)
          END IF
        END IF
      END DO
      CALL WRITE_I_C(IBTMP,NIBCLD*NCONLD_L)
      LEN_IA = LEN_IA + NIBCLD*NCONLD_L
C
      RETURN
      END
