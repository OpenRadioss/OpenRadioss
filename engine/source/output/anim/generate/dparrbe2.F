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
!||====================================================================
!||    dparrbe2    ../engine/source/output/anim/generate/dparrbe2.F
!||--- called by ------------------------------------------------------
!||    genani      ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_i_c   ../common_source/tools/input_output/write_routtines.c
!||====================================================================
      SUBROUTINE DPARRBE2(LRBE2 ,IRBE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,
     .   NSN, II(2)
C-----------------------------------------------
C seul le proc 0 effectue le traitement
      IF (NSPMD > 1) GOTO 100
C
      DO I=1,NRBE2
        NSN = IRBE2(5,I)
        II(1) = IRBE2(3,I) - 1
        DO N=1,NSN
           II(2) = LRBE2(IRBE2(1,I)+N)-1
           CALL WRITE_I_C(II,2)
        ENDDO
      ENDDO
C
 100  CONTINUE
      RETURN
      END
