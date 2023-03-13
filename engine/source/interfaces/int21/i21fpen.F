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
Chd|  I21FPEN                       source/interfaces/int21/i21fpen.F
Chd|-- called by -----------
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21FPEN(
     1      NSN    ,I_STOK  ,CAND_N ,CAND_E,PENI ,
     2      FTXSAV,FTYSAV ,FTZSAV,CAND_TAG,IFPEN ,
     3      INACTI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   Remise a 0 de IFPEN si le nd n'est plus candidat
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,NSN,INACTI
      INTEGER CAND_N(*), CAND_E(*), IFPEN(*), CAND_TAG(*)
C     REAL
      my_real
     .   FTXSAV(*), FTYSAV(*), FTZSAV(*), PENI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C=======================================================================
C
      DO N=1,NSN
        CAND_TAG(N) = 0
      END DO
C
      DO I=1,I_STOK
        N = CAND_N(I)
        CAND_TAG(N)=1
      END DO
C
       DO N=1,NSN
         IF(CAND_TAG(N)==0.AND.IFPEN(N)/=0)THEN
           FTXSAV(N)=ZERO
           FTYSAV(N)=ZERO
           FTZSAV(N)=ZERO
           PENI(N)  =ZERO
           IFPEN(N) =0
         END IF
       END DO
C
      RETURN
      END
