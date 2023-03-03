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
Chd|  POINTS_TO_FRAME               source/model/submodel/3points_to_frame.F
Chd|-- called by -----------
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE POINTS_TO_FRAME(X1, X2, X3 ,PP, IERROR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .  X1(3),X2(3),X3(3),PP(3,3)
      INTEGER IERROR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  U(3),V(3),W(3),
     .  NN,PNOR1,PNOR2,PNORM1,DET,DET1,DET2,DET3
C-----------------------------------------------
      IERROR = 0
C-----
      U(1) = X2(1) - X1(1)
      U(2) = X2(2) - X1(2)
      U(3) = X2(3) - X1(3)
      V(1) = X3(1) - X1(1)
      V(2) = X3(2) - X1(2)
      V(3) = X3(3) - X1(3)
      W(1) = U(2)*V(3)-U(3)*V(2)   ! W=UxV
      W(2) = U(3)*V(1)-U(1)*V(3)
      W(3) = U(1)*V(2)-U(2)*V(1)
      V(1) = W(2)*U(3)-W(3)*U(2)   ! V=WxU
      V(2) = W(3)*U(1)-W(1)*U(3)
      V(3) = W(1)*U(2)-W(2)*U(1)
C------------
C     TESTS DE CONSISTANCE
C------------
      PNOR1=SQRT(U(1)*U(1)+U(2)*U(2)+U(3)*U(3))
      IF (PNOR1 < EM20) THEN
        IERROR=1
C       CALL ANCMSG(MSGID=1866,
C     .             MSGTYPE=MSGERROR,
C     .             ANMODE=ANINFO_BLIND_1,
C     .             I1=ID,C1=TITR,
C     .             I2=N1,
C     .             I3=N2)
        RETURN
      END IF
C     CALCUL DE COLINEARITE DES VECTEURS N1N2 ET N1N3
      PNOR2=SQRT(V(1)*V(1)+V(2)*V(2)+V(3)*V(3))
      IF (PNOR2 > EM20) THEN
        PNORM1=ONE/(PNOR1*PNOR2)
        DET1=ABS((U(2)*V(3)-U(3)*V(2))*PNORM1)
        DET2=ABS((U(3)*V(1)-U(1)*V(3))*PNORM1)
        DET3=ABS((U(1)*V(2)-U(2)*V(1))*PNORM1)
        DET= MAX(DET1,DET2,DET3)
      ELSE
        DET=ZERO
      ENDIF
      IF (DET < EM5) THEN
        IERROR=2
C       CALL ANCMSG(MSGID=1867,
C    .              MSGTYPE=MSGWARNING,
C    .              ANMODE=ANINFO_BLIND_1,
C    .              I1=ID,C1=TITR)
        IF(ABS(U(2)) > EM5) THEN
          V(1)=ABS(U(1))+TEN
        ELSE
          V(2)=TEN
        ENDIF
      ENDIF
C------------
      W(1) = U(2)*V(3)-U(3)*V(2)   ! W=UxV
      W(2) = U(3)*V(1)-U(1)*V(3)
      W(3) = U(1)*V(2)-U(2)*V(1)
C------------
      NN = ONE/MAX(EM20,SQRT(U(1)*U(1)+U(2)*U(2)+U(3)*U(3)))
      U(1) = U(1)*NN
      U(2) = U(2)*NN
      U(3) = U(3)*NN
      NN = ONE/MAX(EM20,SQRT(V(1)*V(1)+V(2)*V(2)+V(3)*V(3)))
      V(1) = V(1)*NN
      V(2) = V(2)*NN
      V(3) = V(3)*NN
      NN = ONE/MAX(EM20,SQRT(W(1)*W(1)+W(2)*W(2)+W(3)*W(3)))
      W(1) = W(1)*NN
      W(2) = W(2)*NN
      W(3) = W(3)*NN
C------------
      PP(1,1)=U(1)
      PP(2,1)=U(2)
      PP(3,1)=U(3)
      PP(1,2)=V(1)
      PP(2,2)=V(2)
      PP(3,2)=V(3)
      PP(1,3)=W(1)
      PP(2,3)=W(2)
      PP(3,3)=W(3)
C------------
      RETURN 
      END 
