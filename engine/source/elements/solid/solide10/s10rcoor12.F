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
Chd|====================================================================
Chd|  S10RCOOR12                    source/elements/solid/solide10/s10rcoor12.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|        SMORTHO3                      source/elements/solid/solide4/smortho3.F
Chd|====================================================================
      SUBROUTINE S10RCOOR12(
     1   OFF,     X,       XDP,     NC,
     2   R11,     R12,     R13,     R21,
     3   R22,     R23,     R31,     R32,
     4   R33,     NEL)
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC(MVSIZ,10)
     
      DOUBLE PRECISION
     .  XDP(3,*)
C     REAL
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),OFF(*),
     .   X(3,*)
      DOUBLE PRECISION 
     .   XX(MVSIZ,4), YY(MVSIZ,4), ZZ(MVSIZ,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N ,NN   
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
C=======================================================================
         DO N=1,4
          DO I=1,NEL
            NN = NC(I,N)           
            IF(IRESP==1)THEN
              XX(I,N)=XDP(1,NN)
              YY(I,N)=XDP(2,NN)
              ZZ(I,N)=XDP(3,NN)        
              ELSE
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)       
            ENDIF      
          END DO 
         END DO 
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=1,NEL
        IF (OFF(I) <= ONE ) CYCLE 
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
      CALL SMORTHO3(
     1   OFF,     RX,      RY,      RZ,
     2   SX,      SY,      SZ,      TX,
     3   TY,      TZ,      R11,     R12,
     4   R13,     R21,     R22,     R23,
     5   R31,     R32,     R33,     NEL)
C-----------
      RETURN
      END
