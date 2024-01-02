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
Chd|  W_ELIG3D                      source/restart/ddsplit/w_elig3d.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE W_ELIG3D(KXIG3D,NUMELIG3D_L,ISUMNIG3D_L,IXIG3D,
     +                  PROC,NODLOCAL,LEN_IA)
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
      INTEGER NUMELIG3D_L, PROC, ISUMNIG3D_L,LEN_IA,
     .        KXIG3D(NIXIG3D,*), NODLOCAL(*), IXIG3D(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NN, IAD, IAD_L, J, NB_L, NNOD, IE_L,
     .        KXIG3D_L(NIXIG3D,NUMELIG3D_L), IXIG3D_L(ISUMNIG3D_L)
C
      IE_L = 0
      NB_L = 0
C
      DO I = 1, NUMELIG3D
          NNOD  = KXIG3D(3,I)
          IAD   = KXIG3D(4,I)
C
          IE_L = IE_L + 1
          DO J = 1, NIXIG3D
            KXIG3D_L(J,IE_L) = KXIG3D(J,I)
          ENDDO
          IAD_L = NB_L+1
          KXIG3D_L(4,IE_L) = IAD_L
          NB_L = NB_L + NNOD
          DO J = 1, NNOD
            IXIG3D_L(IAD_L+J-1) = NODLOCAL(IXIG3D(IAD+J-1))
          ENDDO
      ENDDO
C
      CALL WRITE_I_C(KXIG3D_L,NUMELIG3D_L*NIXIG3D)
      CALL WRITE_I_C(IXIG3D_L,ISUMNIG3D_L)
      LEN_IA = LEN_IA + NUMELIG3D_L*NIXIG3D + ISUMNIG3D_L
C
      RETURN
      END
