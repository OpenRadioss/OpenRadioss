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
!||    w_fac      ../starter/source/restart/ddsplit/w_fac.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_FAC(FAC,CEP,PROC,NCONLD_L,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_AM, PROC, NCONLD_L, CEP(*)
      my_real
     .        FAC(LFACCLD,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, OFF, NB_L
      my_real
     .        FACTMP(LFACCLD,NCONLD_L)
C
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR
     +    + NUMELX
      NB_L = 0
C
      DO I = 1, NCONLD
        IF(CEP(I+OFF)==PROC) THEN
           NB_L = NB_L + 1
          DO J = 1, LFACCLD
            FACTMP(J,NB_L) = FAC(J,I)
          END DO
        END IF
      END DO
      CALL WRITE_DB(FACTMP,NCONLD_L*LFACCLD)
      LEN_AM = LEN_AM + NCONLD_L*LFACCLD
C
      RETURN
      END
