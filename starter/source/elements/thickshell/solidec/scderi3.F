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
Chd|  SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|-- called by -----------
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCDERI3(VOL    ,VEUL   ,GEO    ,
     .         VZL  ,VZQ  ,NGL  ,NGEO ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,  
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,  
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8, DET)  

      USE MESSAGE_MOD
C------------------------------------------------------
C
C------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*),NGEO(*)
      my_real
     .   VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*),
     .   VZL(*),VZQ(*), DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   DETT(MVSIZ), 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   AJ1(MVSIZ), AJ2(MVSIZ), AJ3(MVSIZ), AJ4(MVSIZ),
     .   AJ5(MVSIZ), AJ6(MVSIZ), AJ7(MVSIZ), AJ8(MVSIZ),
     .   AJ9(MVSIZ),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ), AJI4(MVSIZ), 
     .   AJI5(MVSIZ), AJI6(MVSIZ), AJI7(MVSIZ), AJI8(MVSIZ), 
     .   AJI9(MVSIZ), 
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ), 
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ),
     .   AJ5968(MVSIZ), AJ6749(MVSIZ), AJ4857(MVSIZ), 
     .   AJ12(MVSIZ),   AJ45(MVSIZ),   AJ78(MVSIZ),
     .   A17(MVSIZ),    A28(MVSIZ), 
     .   B17(MVSIZ),    B28(MVSIZ),    C17(MVSIZ),   C28(MVSIZ)
      my_real
     .   H1X,H2X,H1Y,H2Y,H1Z,H2Z
C=======================================================================
      DO I=LFT,LLT
        X17(I)=X7(I)-X1(I)
        X28(I)=X8(I)-X2(I)
        X35(I)=X5(I)-X3(I)
        X46(I)=X6(I)-X4(I)
        Y17(I)=Y7(I)-Y1(I)
        Y28(I)=Y8(I)-Y2(I)
        Y35(I)=Y5(I)-Y3(I)
        Y46(I)=Y6(I)-Y4(I)
        Z17(I)=Z7(I)-Z1(I)
        Z28(I)=Z8(I)-Z2(I)
        Z35(I)=Z5(I)-Z3(I)
        Z46(I)=Z6(I)-Z4(I)
      ENDDO
C
      DO I=LFT,LLT
Cqz-------ri.xi-----------
        AJ3(I)=X17(I)+X28(I)-X35(I)-X46(I)
        AJ1(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        AJ2(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17(I)=X17(I)+X46(I)
        A28(I)=X28(I)+X35(I)
        B17(I)=Y17(I)+Y46(I)
        B28(I)=Y28(I)+Y35(I)
        C17(I)=Z17(I)+Z46(I)
        C28(I)=Z28(I)+Z35(I)
      ENDDO
C
      DO I=LFT,LLT
Cqz-------si.xi-----------
        AJ6(I)=A17(I)+A28(I)
        AJ4(I)=B17(I)+B28(I)
        AJ5(I)=C17(I)+C28(I)
Cqz-------ti.xi-----------
        AJ9(I)=A17(I)-A28(I)
        AJ7(I)=B17(I)-B28(I)
        AJ8(I)=C17(I)-C28(I)
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
        AJ5968(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        AJ6749(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        AJ4857(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
        DET(I)=O64TH*(AJ1(I)*AJ5968(I)+AJ2(I)*AJ6749(I)+AJ3(I)*AJ4857(I))
        VOL(I)=DET(I)
      ENDDO
C
      DO I=LFT,LLT
        IF(DET(I).GT.ZERO) CYCLE
        CALL ANCMSG(MSGID=245,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=NGL(I))
      ENDDO
C
       DO I=LFT,LLT
        H1X = X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I)
        H1Y = Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I)
        H1Z = Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I)
        H2X = X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I)
        H2Y = Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I)
        H2Z = Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I)
        VZL(I) =  O64TH*AJ5(I)*(AJ9(I)*H1Y+AJ1(I)*H2X-AJ3(I)*H2Y-AJ7(I)*H1X)
     .          + O64TH*AJ4(I)*(AJ3(I)*H2Z+AJ8(I)*H1X-AJ9(I)*H1Z-AJ2(I)*H2X)
     .          + O64TH*AJ6(I)*(AJ7(I)*H1Z+AJ2(I)*H2Y-AJ1(I)*H2Z-AJ8(I)*H1Y)
        VZQ(I) = O64TH*AJ5(I)*(H1Y*H2X-H2Y*H1X)
       ENDDO
c--------------------------
      IF (JEUL == 0) RETURN
C
      DO I=LFT,LLT
        DETT(I)=O64TH/DET(I)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
        AJI1(I)=DETT(I)*AJ5968(I)
        AJI4(I)=DETT(I)*AJ6749(I)
        AJI7(I)=DETT(I)*AJ4857(I)
        AJI2(I)=DETT(I)*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5(I)=DETT(I)*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8(I)=DETT(I)*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3(I)=DETT(I)*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6(I)=DETT(I)*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9(I)=DETT(I)*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      ENDDO
C
      DO I=LFT,LLT
        AJ12(I)=AJI1(I)-AJI2(I)
        AJ45(I)=AJI4(I)-AJI5(I)
        AJ78(I)=AJI7(I)-AJI8(I)
      ENDDO
C
      DO I=LFT,LLT
        VEUL(3,I) = AJ12(I)+AJI3(I)
        VEUL(4,I) = AJ12(I)-AJI3(I)
        VEUL(7,I) = AJ45(I)+AJI6(I)
        VEUL(8,I) = AJ45(I)-AJI6(I)
        VEUL(11,I)= AJ78(I)+AJI9(I)
        VEUL(12,I)= AJ78(I)-AJI9(I)
      ENDDO
C
      DO I=LFT,LLT
        AJ12(I)=AJI1(I)+AJI2(I)
        AJ45(I)=AJI4(I)+AJI5(I)
        AJ78(I)=AJI7(I)+AJI8(I)
      ENDDO
C
      DO I=LFT,LLT
        VEUL(1,I) =-AJ12(I)-AJI3(I)
        VEUL(2,I) =-AJ12(I)+AJI3(I)
        VEUL(5,I) =-AJ45(I)-AJI6(I)
        VEUL(6,I) =-AJ45(I)+AJI6(I)
        VEUL(9,I) =-AJ78(I)-AJI9(I)
        VEUL(10,I)=-AJ78(I)+AJI9(I)
      ENDDO
C
      IF (GEO(12,NGEO(I)) == 15) THEN
          DO I=LFT,LLT
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          ENDDO
      ENDIF
c-----------
      RETURN
      END
