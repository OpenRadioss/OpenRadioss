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
Chd|  DEGES4V                       source/elements/solid/solide/deges4v.F
Chd|-- called by -----------
Chd|        SDLEN_DEGE                    source/elements/solid/solide/sdlen_dege.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/sdlen_sms.F
Chd|        SLDEGE                        source/elements/solid/solide/sldege.F
Chd|-- calls ---------------
Chd|        NODEDEGE                      source/elements/solid/solide/nodedege.F
Chd|        TETRA4V                       source/elements/solid/solide/tetra4v.F
Chd|====================================================================
      SUBROUTINE DEGES4V(DET,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   DET,X1, X2, X3, X4,X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,NNOD
C                                                                     12
      my_real
     .   XYZ(3,4),XIJ,YIJ,ZIJ
C-----------------------------------------------
C
             NNOD=1
             XYZ(1,NNOD)=X1
             XYZ(2,NNOD)=Y1
             XYZ(3,NNOD)=Z1
       CALL NODEDEGE(X2,Y2,Z2,XYZ,NNOD)
       CALL NODEDEGE(X3,Y3,Z3,XYZ,NNOD)
       CALL NODEDEGE(X4,Y4,Z4,XYZ,NNOD)
       IF (NNOD==4) GOTO 100     
       CALL NODEDEGE(X5,Y5,Z5,XYZ,NNOD)     
       IF (NNOD==4) GOTO 100     
       CALL NODEDEGE(X6,Y6,Z6,XYZ,NNOD)     
       IF (NNOD==4) GOTO 100     
       CALL NODEDEGE(X7,Y7,Z7,XYZ,NNOD)     
       IF (NNOD==4) GOTO 100     
       CALL NODEDEGE(X8,Y8,Z8,XYZ,NNOD)     
 100   CONTINUE
       CALL TETRA4V(DET,
     .   XYZ(1,1), XYZ(1,2), XYZ(1,3), XYZ(1,4), 
     .   XYZ(2,1), XYZ(2,2), XYZ(2,3), XYZ(2,4), 
     .   XYZ(3,1), XYZ(3,2), XYZ(3,3), XYZ(3,4))
C      
      RETURN
      END
