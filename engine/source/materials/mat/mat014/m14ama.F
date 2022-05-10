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
Chd|  M14AMA                        source/materials/mat/mat014/m14ama.F
Chd|-- called by -----------
Chd|        M12LAW                        source/materials/mat/mat012/m12law.F
Chd|        M14LAW                        source/materials/mat/mat014/m14law.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M14AMA(PM, A,  RX, RY, RZ, SX, 
     2                  SY, SZ, AX, AY, AZ, BX,
     3                  BY, BZ, CX, CY, CZ)
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
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real PM(NPROPM,*), A(MVSIZ,6)
      my_real
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),
     .   AX(*), AY(*), AZ(*), BX(*), BY(*), BZ(*), CX(*), CY(*),CZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .   TX(MVSIZ), TY(MVSIZ),TZ(MVSIZ), RR
C-----------------------------------------------
C
      IF (JSPH.EQ.1) THEN
        DO I=LFT,LLT
          AX(I)=A(I,1)
          AY(I)=A(I,2)
          AZ(I)=A(I,3)
          BX(I)=A(I,4)
          BY(I)=A(I,5)
          BZ(I)=A(I,6)
          CX(I)=AY(I)*BZ(I)-AZ(I)*BY(I)
          CY(I)=AZ(I)*BX(I)-AX(I)*BZ(I)
          CZ(I)=AX(I)*BY(I)-AY(I)*BX(I)
        END DO
C
      ELSEIF(JCVT.EQ.0)THEN
       IF (N2D.EQ.0) THEN
        DO I=LFT,LLT
          RR=UN/SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)
          RX(I)=RX(I)*RR
          RY(I)=RY(I)*RR
          RZ(I)=RZ(I)*RR
C
          TX(I)=RY(I)*SZ(I)-RZ(I)*SY(I)
          TY(I)=RZ(I)*SX(I)-RX(I)*SZ(I)
          TZ(I)=RX(I)*SY(I)-RY(I)*SX(I)
          RR=UN/SQRT(TX(I)**2+TY(I)**2+TZ(I)**2)
          TX(I)=TX(I)*RR
          TY(I)=TY(I)*RR
          TZ(I)=TZ(I)*RR
C
          SX(I)=TY(I)*RZ(I)-TZ(I)*RY(I)
          SY(I)=TZ(I)*RX(I)-TX(I)*RZ(I)
          SZ(I)=TX(I)*RY(I)-TY(I)*RX(I)
          RR=1./SQRT(SX(I)**2+SY(I)**2+SZ(I)**2)
          SX(I)=SX(I)*RR
          SY(I)=SY(I)*RR
          SZ(I)=SZ(I)*RR
        ENDDO
       ELSE
        DO I=LFT,LLT
          RR=UN/SQRT(SY(I)**2+SZ(I)**2)
          SY(I)=SY(I)*RR
          SZ(I)=SZ(I)*RR
C
          TY(I)=-RX(I)*SZ(I)
          TZ(I)=RX(I)*SY(I)
          RR=UN/SQRT(TY(I)**2+TZ(I)**2)
          TY(I)=TY(I)*RR
          TZ(I)=TZ(I)*RR
        ENDDO
       ENDIF
C
       DO 200 I=LFT,LLT
       AX(I)= A(I,1)*RX(I)+A(I,2)*SX(I)+A(I,3)*TX(I)
       AY(I)= A(I,1)*RY(I)+A(I,2)*SY(I)+A(I,3)*TY(I)
       AZ(I)= A(I,1)*RZ(I)+A(I,2)*SZ(I)+A(I,3)*TZ(I)
C
       BX(I)= A(I,4)*RX(I)+A(I,5)*SX(I)+A(I,6)*TX(I)
       BY(I)= A(I,4)*RY(I)+A(I,5)*SY(I)+A(I,6)*TY(I)
       BZ(I)= A(I,4)*RZ(I)+A(I,5)*SZ(I)+A(I,6)*TZ(I)
C
Clm 22/06/98
C       CX(I)= A(7,I)*RX(I)+A(8,I)*SX(I)+A(9,I)*TX(I)
C       CY(I)= A(7,I)*RY(I)+A(8,I)*SY(I)+A(9,I)*TY(I)
C       CZ(I)= A(7,I)*RZ(I)+A(8,I)*SZ(I)+A(9,I)*TZ(I)
C
        CX(I)=AY(I)*BZ(I)-AZ(I)*BY(I)
        CY(I)=AZ(I)*BX(I)-AX(I)*BZ(I)
        CZ(I)=AX(I)*BY(I)-AY(I)*BX(I)
  200  CONTINUE
C-------------------------------
C     CO-ROTATIONAL FORMULATION IN  ORTHOTROPIC CO-ROTATIONAL SYSTEM.
      ELSE
        DO 500 I=LFT,LLT
         AX(I)=UN
         BX(I)=ZERO
         CX(I)=ZERO
         AY(I)=ZERO
         BY(I)=UN
         CY(I)=ZERO
         AZ(I)=ZERO
         BZ(I)=ZERO
         CZ(I)=UN 
  500   CONTINUE
      ENDIF
C
      RETURN
      END
