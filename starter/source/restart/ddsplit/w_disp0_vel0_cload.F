Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  W_DISP0_VEL0_CLOAD            source/restart/ddsplit/w_disp0_vel0_cload.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|====================================================================
      SUBROUTINE W_DISP0_VEL0_CLOAD(DISP0,CEP    ,PROC  ,NCLOAD_L,LEN_AM,
     .                             OFF  ,LEN_CEP ,NCLOAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: OFF,NCLOAD_L,PROC,LEN_CEP,NCLOAD
      INTEGER, INTENT(INOUT) :: LEN_AM
      INTEGER, INTENT(IN) :: CEP(LEN_CEP)
      my_real, INTENT(IN) :: DISP0(6,NCLOAD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NB_L
      my_real
     .        DISP0TMP(6,NCLOAD_L)
C-----------------------------------------------
      NB_L = 0
C
      DO I = 1, NCLOAD
        IF(CEP(I+OFF)==PROC) THEN
          NB_L = NB_L + 1
          DO J = 1, 6
            DISP0TMP(J,NB_L) = DISP0(J,I)
          END DO
        END IF
      END DO
      CALL WRITE_DB(DISP0TMP,NCLOAD_L*6)
      LEN_AM = LEN_AM + NCLOAD_L*6
C
      RETURN
      END
    