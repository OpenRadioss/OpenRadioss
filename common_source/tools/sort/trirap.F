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
Chd|  TRIRAP                        src/init1/trirap.F            
Chd|-- called by -----------
Chd|        ADD_MASS_STAT                 src/lectur1/add_mass_stat.F   
Chd|        FIND_DT_ENGINE                src/rad2r/r2r_speedup.F       
Chd|        OUTRI                         src/materdt/outri.F           
Chd|        OUTRIN                        src/materdt/outri.F           
Chd|-- calls ---------------
Chd|        TRIINS                        src/init1/triins.F            
Chd|        IPARTI                        src/init1/iparti.F            
Chd|====================================================================
      SUBROUTINE TRIRAP(N,TAB,N2)
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
      INTEGER SEUIL, SEPAR, I, J, ISUIV, JSUIV, NIVPIL, PILE(40)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IPARTI
      DATA SEUIL/15/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      NIVPIL=0
      IF(N-1>SEUIL) THEN
          I=1
          J=N
 100      CONTINUE
             SEPAR=IPARTI(I,J,TAB,N2)
             IF(SEPAR-I<=J-SEPAR) THEN
                ISUIV=SEPAR+1
                JSUIV=J
                J=SEPAR-1
              ELSE
                ISUIV=I
                JSUIV=SEPAR-1
                I=SEPAR+1
              ENDIF
              IF(JSUIV-ISUIV>SEUIL) THEN
                NIVPIL=NIVPIL+2
                PILE(NIVPIL)=ISUIV
                PILE(NIVPIL-1)=JSUIV
             ENDIF
             IF(J-I>SEUIL) GO TO 100
             IF(NIVPIL/=0) THEN
                I=PILE(NIVPIL)
                J=PILE(NIVPIL-1)
                NIVPIL=NIVPIL-2
                GO TO 100
             ENDIF
      ENDIF
      CALL TRIINS(N,TAB,N2)
      RETURN
      END
