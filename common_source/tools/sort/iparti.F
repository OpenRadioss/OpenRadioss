Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  IPARTI                        src/init1/iparti.F            
Chd|-- called by -----------
Chd|        TRIRAP                        src/init1/trirap.F            
Chd|-- calls ---------------
Chd|        ECHANG                        src/init1/echang.F            
Chd|        ICOMPC                        src/init1/icompc.F            
Chd|====================================================================
      INTEGER FUNCTION IPARTI(I,J,TAB,N2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I, J, N2
C     REAL
      my_real
     .   TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MILIEU, U, V
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER ICOMPC
      IPARTI=I
      IF(J.GT.I) THEN
          MILIEU=(I+J)/2
          U=I
          V=J+1
C
          IF(ICOMPC(J,MILIEU,TAB).LT.0) THEN
             CALL ECHANG(J,MILIEU,TAB)
             CALL ECHANG(N2+J,N2+MILIEU,TAB)
          ENDIF
          IF(ICOMPC(J,I,TAB).LT.0) THEN
             CALL ECHANG(J,I,TAB)
             CALL ECHANG(N2+J,N2+I,TAB)
          ENDIF
          IF(ICOMPC(I,MILIEU,TAB).LT.0) THEN
             CALL ECHANG(I,MILIEU,TAB)
             CALL ECHANG(N2+I,N2+MILIEU,TAB)
          ENDIF
C
 1000     IF(U.LT.V) THEN
C
 1100       CONTINUE
                U=U+1
                IF(ICOMPC(U,I,TAB).LT.0) GO TO 1100
C
 1200       CONTINUE
                V=V-1
                IF(ICOMPC(V,I,TAB).GT.0) GO TO 1200
C
             CALL ECHANG(U,V,TAB)
             CALL ECHANG(N2+U,N2+V,TAB)
             GO TO 1000
          ENDIF
          IF(U.GT.V) CALL ECHANG(U,V,TAB)
          IF(U.GT.V) CALL ECHANG(N2+U,N2+V,TAB)
          U=MIN0(U,V)
          CALL ECHANG(I,U,TAB)
          CALL ECHANG(N2+I,N2+U,TAB)
          IPARTI=U
       ENDIF
       RETURN
       END
