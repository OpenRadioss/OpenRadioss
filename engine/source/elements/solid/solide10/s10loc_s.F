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
Chd|  S10LOC_S                      source/elements/solid/solide10/s10coor3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|====================================================================
      SUBROUTINE S10LOC_S(
     1   NC,      X,       R11,     R12,
     2   R13,     R21,     R22,     R23,
     3   R31,     R32,     R33,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      INTEGER NC(MVSIZ,10)
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),X(3,*)
      DOUBLE PRECISION 
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NN
      
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
C=======================================================================
      DO N=1,4
          DO I=1,NEL
            NN = NC(I,N)           
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)			 
          ENDDO
      END DO
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=1,NEL
        RX(I) = XX(I,1) - XX(I,4)
        RY(I) = YY(I,1) - YY(I,4)
        RZ(I) = ZZ(I,1) - ZZ(I,4)
        SX(I) = XX(I,2) - XX(I,4)
        SY(I) = YY(I,2) - YY(I,4)
        SZ(I) = ZZ(I,2) - ZZ(I,4)
        TX(I) = XX(I,3) - XX(I,4)
        TY(I) = YY(I,3) - YY(I,4)
        TZ(I) = ZZ(I,3) - ZZ(I,4)
      END DO 
      CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     NEL)
C-----------
      RETURN
      END
