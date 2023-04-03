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
Chd|  MREPLOC                       source/materials/mat_share/mreploc.F
Chd|-- called by -----------
Chd|        M25LAW                        source/materials/mat/mat025/m25law.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MREPLOC(
     1   ANG,     R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,
     6   JSPH)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JSPH
      my_real  ANG(MVSIZ,6),
     .R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   RX(MVSIZ)    ,RY(MVSIZ)    ,RZ(MVSIZ)    ,
     .   SX(MVSIZ)    ,SY(MVSIZ)    ,SZ(MVSIZ)    ,
     .   TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   S11, S12, S13, S21, 
     .   S22, S23, S31,
     .   S32, S33, RR,  RRD
C

      IF(JSPH==0)THEN
        IF(N2D==0)THEN
          DO 100 I=1,NEL
             RR=MAX(SQRT(RX(I)**2+RY(I)**2+RZ(I)**2),EM20)
             RX(I)=RX(I)/RR
             RY(I)=RY(I)/RR
             RZ(I)=RZ(I)/RR
 100      CONTINUE
C
          DO 105 I=1,NEL
            TX(I)=RY(I)*SZ(I)-RZ(I)*SY(I)
            TY(I)=RZ(I)*SX(I)-RX(I)*SZ(I)
            TZ(I)=RX(I)*SY(I)-RY(I)*SX(I)
            RR=MAX(SQRT(TX(I)**2+TY(I)**2+TZ(I)**2),EM20)
            TX(I)=TX(I)/RR
            TY(I)=TY(I)/RR
            TZ(I)=TZ(I)/RR
 105      CONTINUE
C
          DO 110 I=1,NEL
             SX(I)=TY(I)*RZ(I)-TZ(I)*RY(I)
             SY(I)=TZ(I)*RX(I)-TX(I)*RZ(I)
             SZ(I)=TX(I)*RY(I)-TY(I)*RX(I)
             RR=MAX(SQRT(SX(I)**2+SY(I)**2+SZ(I)**2),EM20)
             SX(I)=SX(I)/RR
             SY(I)=SY(I)/RR
             SZ(I)=SZ(I)/RR
 110      CONTINUE
        ELSE
C---------------------------------------------
C     EN 2D LES CONTRAINTES SONT :
C     1=YY 2=ZZ 3=TT 4=YZ 5=0 6=0
C     EN CONTRADICTION AVEC X=T
C     ATTENTION DANGER!
C---------------------------------------------
          DO 120 I=1,NEL
              RR= MAX(SQRT(SY(I)**2+SZ(I)**2),EM20)
              RX(I)= SY(I)/RR
              RY(I)= SZ(I)/RR
              RZ(I)= ZERO
              SX(I)=-RY(I)
              SY(I)= RX(I)
              SZ(I)= ZERO
              TX(I)= ZERO
              TY(I)= ZERO
              TZ(I)= ONE     
 120      CONTINUE
        ENDIF
C
        DO I=1,NEL
          S11=ANG(I,1)
          S21=ANG(I,2)
          S31=ANG(I,3)
          S12=ANG(I,4)
          S22=ANG(I,5)
          S32=ANG(I,6)
          S13=S21*S32-S31*S22
          S23=S31*S12-S11*S32
          S33=S11*S22-S21*S12
C
          R11(I) = S11*RX(I)+S21*SX(I)+S31*TX(I)
          R21(I) = S11*RY(I)+S21*SY(I)+S31*TY(I)
          R31(I) = S11*RZ(I)+S21*SZ(I)+S31*TZ(I)
C           
          R12(I) = S12*RX(I)+S22*SX(I)+S32*TX(I)
          R22(I) = S12*RY(I)+S22*SY(I)+S32*TY(I)
          R32(I) = S12*RZ(I)+S22*SZ(I)+S32*TZ(I)
C
          R13(I) = S13*RX(I)+S23*SX(I)+S33*TX(I)
          R23(I) = S13*RY(I)+S23*SY(I)+S33*TY(I)
          R33(I) = S13*RZ(I)+S23*SZ(I)+S33*TZ(I)
        ENDDO
C
      ELSE
C
       DO I=1,NEL
         R11(I)=ANG(I,1)
         R21(I)=ANG(I,2)
         R31(I)=ANG(I,3)
         R12(I)=ANG(I,4)
         R22(I)=ANG(I,5)
         R32(I)=ANG(I,6)
         R13(I)=R21(I)*R32(I)-R31(I)*R22(I)
         R23(I)=R31(I)*R12(I)-R11(I)*R32(I)
         R33(I)=R11(I)*R22(I)-R21(I)*R12(I)
       END DO
C
      END IF
C
      RETURN
      END
