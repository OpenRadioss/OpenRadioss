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
Chd|  S4RCOOR12                     source/elements/solid/solide4/s4rcoor12.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        SMORTHO3                      source/elements/solid/solide4/smortho3.F
Chd|====================================================================
      SUBROUTINE S4RCOOR12(
     1   OFF,     NC1,     NC2,     NC3,
     2   NC4,     X,       XDP,     D,
     3   R11,     R12,     R13,     R21,
     4   R22,     R23,     R31,     R32,
     5   R33,     NEL,     JCVT)
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
      INTEGER, INTENT(IN) :: JCVT
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C     REAL
      my_real
     .   OFF(*),X(3,*),D(3,*),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      DOUBLE PRECISION 
     .   XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      
      DOUBLE PRECISION
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
    
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
C=======================================================================
C---for case of Ismstr=10-> Ismstr11 :
C----1) config is fixed at t= t_dtmin but will not be updated like Ismstr11
C----2) [H] = [B](t=0)*{u}, [B](t=0)<-JAC_I, {u}={d}; 
C----3) in case of JCVT=0, [H] and [D] are transformed to local system and
C-      return to global system for stress
C ---------local system compute----------     
      IF(IRESP==1)THEN 
        DO I=1,NEL
         IF (OFF(I) <= ONE ) CYCLE 
          X1(I)=XDP(1,NC1(I))  
          Y1(I)=XDP(2,NC1(I))  
          Z1(I)=XDP(3,NC1(I))  
          X2(I)=XDP(1,NC2(I))  
          Y2(I)=XDP(2,NC2(I))  
          Z2(I)=XDP(3,NC2(I))  
          X3(I)=XDP(1,NC3(I))  
          Y3(I)=XDP(2,NC3(I))  
          Z3(I)=XDP(3,NC3(I))  
          X4(I)=XDP(1,NC4(I))  
          Y4(I)=XDP(2,NC4(I))  
          Z4(I)=XDP(3,NC4(I))  
        ENDDO
      ELSE  
        DO I=1,NEL
         IF (OFF(I) <= ONE ) CYCLE 
          X1(I)=X(1,NC1(I))  
          Y1(I)=X(2,NC1(I))  
          Z1(I)=X(3,NC1(I))  
          X2(I)=X(1,NC2(I))  
          Y2(I)=X(2,NC2(I))  
          Z2(I)=X(3,NC2(I))  
          X3(I)=X(1,NC3(I))  
          Y3(I)=X(2,NC3(I))  
          Z3(I)=X(3,NC3(I))  
          X4(I)=X(1,NC4(I))  
          Y4(I)=X(2,NC4(I))  
          Z4(I)=X(3,NC4(I))    
        ENDDO      
      ENDIF     
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      DO I=1,NEL
       IF (OFF(I) <= ONE ) CYCLE 
       X43 = X4(I) - X3(I)
       Y43 = Y4(I) - Y3(I)
       Z43 = Z4(I) - Z3(I)
       X41 = X4(I) - X1(I)
       Y41 = Y4(I) - Y1(I)
       Z41 = Z4(I) - Z1(I)
       X42 = X4(I) - X2(I)
       Y42 = Y4(I) - Y2(I)
       Z42 = Z4(I) - Z2(I)
C
       RX(I) =  -X41
       RY(I) =  -Y41
       RZ(I) =  -Z41
       SX(I) =  -X42
       SY(I) =  -Y42
       SZ(I) =  -Z42
C
       TX(I) =  -X43
       TY(I) =  -Y43
       TZ(I) =  -Z43
      END DO 
      CALL SMORTHO3(
     1   OFF,     RX,      RY,      RZ,
     2   SX,      SY,      SZ,      TX,
     3   TY,      TZ,      R11,     R12,
     4   R13,     R21,     R22,     R23,
     5   R31,     R32,     R33,     NEL)
     
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
C       CALL S4RROTA3(OFF,
C     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
C     .   X0(I,1), X0(I,2), X0(I,3), X0(I,4), 
C     .   Y0(I,1), Y0(I,2), Y0(I,3), Y0(I,4), 
C     .   Z0(I,1), Z0(I,2), Z0(I,3), Z0(I,4))
C       CALL S4RROTA3(OFF,
C     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
C     .   VX0(I,1), VX0(I,2), VX0(I,3), VX0(I,4), 
C     .   VY0(I,1), VY0(I,2), VY0(I,3), VY0(I,4), 
C     .   VZ0(I,1), VZ0(I,2), VZ0(I,3), VZ0(I,4))
C    
C-----------
      RETURN
      END
