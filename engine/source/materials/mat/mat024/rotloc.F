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
Chd|  ROTLOC                        source/materials/mat/mat024/rotloc.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTLOC(NEL,GAMA,
     .                  RX,RY,RZ,SX,SY,SZ,
     .                  R11,R12,R13,R21,R22,
     .                  R23,R31,R32,R33)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real, DIMENSION(NEL), INTENT(INOUT)  :: RX,RY,RZ,SX,SY,SZ
      my_real, DIMENSION(NEL), INTENT(OUT)    :: R11,R12,R13,R21,R22,
     .                                           R23,R31,R32,R33
      my_real, DIMENSION(MVSIZ,6) :: GAMA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real RR
      my_real, DIMENSION(NEL) :: S11,S12,S13,S21,S22,S23,S31,S32,S33,
     .   TX,TY,TZ
c=======================================================================
      IF (JSPH.EQ.0) THEN
        IF (JCVT.EQ.0) THEN
          IF(N2D.EQ.0)THEN
            DO I = 1,NEL
            RR   =SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)
            RX(I)=RX(I)/RR
            RY(I)=RY(I)/RR
            RZ(I)=RZ(I)/RR
            ENDDO
            DO I = 1,NEL
            TX(I)=RY(I)*SZ(I)-RZ(I)*SY(I)
            TY(I)=RZ(I)*SX(I)-RX(I)*SZ(I)
            TZ(I)=RX(I)*SY(I)-RY(I)*SX(I)
            RR   =SQRT(TX(I)**2+TY(I)**2+TZ(I)**2)
            TX(I)=TX(I)/RR
            TY(I)=TY(I)/RR
            TZ(I)=TZ(I)/RR
            ENDDO
            DO I = 1,NEL
            SX(I)=TY(I)*RZ(I)-TZ(I)*RY(I)
            SY(I)=TZ(I)*RX(I)-TX(I)*RZ(I)
            SZ(I)=TX(I)*RY(I)-TY(I)*RX(I)
            RR   =SQRT(SX(I)**2+SY(I)**2+SZ(I)**2)
            SX(I)=SX(I)/RR
            SY(I)=SY(I)/RR
            SZ(I)=SZ(I)/RR
            ENDDO
          ELSE
C---------------------------------------------
C            EN 2D LES CONTRAINTES SONT :
C            1=YY 2=ZZ 3=TT 4=YZ 5=0 6=0
C            EN CONTRADICTION AVEC X=T
C            ATTENTION DGAMAER!
C---------------------------------------------
            DO I = 1,NEL
            RR   = SQRT(SY(I)**2+SZ(I)**2)
            RX(I)= SY(I)/RR
            RY(I)= SZ(I)/RR
            RZ(I)= ZERO
            SX(I)=-RY(I)
            SY(I)= RX(I)
            SZ(I)= ZERO
            TX(I)= ZERO
            TY(I)= ZERO
            TZ(I)= ONE    
            ENDDO
          ENDIF
C
          DO I = 1,NEL
          S11(I)=GAMA(I,1)
          S21(I)=GAMA(I,2)
          S31(I)=GAMA(I,3)
          S12(I)=GAMA(I,4)
          S22(I)=GAMA(I,5)
          S32(I)=GAMA(I,6)
          S13(I)=S21(I)*S32(I)-S31(I)*S22(I)
          S23(I)=S31(I)*S12(I)-S11(I)*S32(I)
          S33(I)=S11(I)*S22(I)-S21(I)*S12(I)
          ENDDO
C
          DO I = 1,NEL
          R11(I) = S11(I)*RX(I)+S21(I)*SX(I)+S31(I)*TX(I)
          R21(I) = S11(I)*RY(I)+S21(I)*SY(I)+S31(I)*TY(I)
          R31(I) = S11(I)*RZ(I)+S21(I)*SZ(I)+S31(I)*TZ(I)
C
          R12(I) = S12(I)*RX(I)+S22(I)*SX(I)+S32(I)*TX(I)
          R22(I) = S12(I)*RY(I)+S22(I)*SY(I)+S32(I)*TY(I)
          R32(I) = S12(I)*RZ(I)+S22(I)*SZ(I)+S32(I)*TZ(I)
C
          R13(I) = S13(I)*RX(I)+S23(I)*SX(I)+S33(I)*TX(I)
          R23(I) = S13(I)*RY(I)+S23(I)*SY(I)+S33(I)*TY(I)
          R33(I) = S13(I)*RZ(I)+S23(I)*SZ(I)+S33(I)*TZ(I)
          ENDDO
c-------------------------------------------------------
        ELSE
c         CO-ROTATIONAL FORMULATION IN ORTHOTROPIC CO-ROTATIONAL SYSTEM.
          DO I = 1,NEL
           R11(I) = ONE
           R21(I) = ZERO
           R31(I) = ZERO
C
           R12(I) = ZERO
           R22(I) = ONE
           R32(I) = ZERO
C
           R13(I) = ZERO
           R23(I) = ZERO
           R33(I) = ONE
          ENDDO
        ENDIF
C-------------------------------------------------------
      ELSE
        DO I = 1,NEL
         R11(I)=GAMA(I,1)
         R21(I)=GAMA(I,2)
         R31(I)=GAMA(I,3)
         R12(I)=GAMA(I,4)
         R22(I)=GAMA(I,5)
         R32(I)=GAMA(I,6)
         R13(I)=R21(I)*R32(I)-R31(I)*R22(I)
         R23(I)=R31(I)*R12(I)-R11(I)*R32(I)
         R33(I)=R11(I)*R22(I)-R21(I)*R12(I)
        ENDDO
      ENDIF
c-----------
      RETURN
      END
