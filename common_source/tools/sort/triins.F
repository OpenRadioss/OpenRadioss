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
Chd|  TRIINS                        src/init1/triins.F            
Chd|-- called by -----------
Chd|        TRIRAP                        src/init1/trirap.F            
Chd|-- calls ---------------
Chd|        ECHANG                        src/init1/echang.F            
Chd|        ICOMPC                        src/init1/icompc.F            
Chd|====================================================================
      SUBROUTINE TRIINS(N,TAB,N2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, N2
C     REAL
      my_real
     .   TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER ICOMPC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO 10 K=2,N
          L=K-1
 150      IF(L>=1) THEN
               IF(ICOMPC(L,L+1,TAB)>0) THEN
               CALL ECHANG(L,L+1,TAB)
               CALL ECHANG(N2+L,N2+L+1,TAB)
               L=L-1
               GO TO 150
               ENDIF
          ENDIF
   10     CONTINUE
      RETURN
      END
