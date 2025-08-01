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
!||    c_poro    ../starter/source/restart/ddsplit/c_poro.F
!||--- called by ------------------------------------------------------
!||    ddsplit   ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    nlocal    ../starter/source/spmd/node/ddtools.F
!||====================================================================
      SUBROUTINE C_PORO(PORNOD,PROC,NUMPOR_L,GEO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NUMPOR_L,
     .          PORNOD(*)
       my_real
     .          GEO(NPROPG,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN, NAD, IG, J
C
      NAD=0
      DO IG = 1, NUMGEO
        IF(INT(GEO(12,IG))==15)THEN
          N=INT(GEO(31,IG))
          IF(N>0)THEN
            DO J = NAD+1, NAD+N
              NN = PORNOD(J)
              IF(NLOCAL(NN,PROC+1)==1)THEN
                NUMPOR_L = NUMPOR_L + 1
              END IF
            END DO
            NAD = NAD + N
          END IF
        END IF
      END DO
C
      RETURN
      END
