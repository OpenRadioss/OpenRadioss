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
Chd|  SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|-- called by -----------
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SZDERI3(
     .              VOL  ,VEUL ,GEO  ,IGEO , 
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5   ,XD6  ,XD7   ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5   ,YD6  ,YD7   ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5   ,ZD6  ,ZD7   ,ZD8   ,
     .              PX1  ,PX2  ,PX3  ,PX4  ,
     .              PY1  ,PY2  ,PY3  ,PY4  ,
     .              PZ1  ,PZ2  ,PZ3  ,PZ4  , 
     .              AJ1  ,AJ2  ,AJ3  ,AJ4  ,AJ5  ,AJ6  ,AJ9  ,
     .              NGL  ,NGEO ,DET  ,VOLDP)
      USE MESSAGE_MOD
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
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),NGL(*),NGEO(*)
      my_real
     .   VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*),
     .   AJ1(*), AJ2(*), AJ3(*), AJ4(*), AJ5(*), AJ6(*), AJ9(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      
           
      my_real
     .   DETT(MVSIZ),  AJ7(MVSIZ), AJ8(MVSIZ) , 
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ), AJI4(MVSIZ), 
     .   AJI5(MVSIZ),AJI6(MVSIZ),AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ), 
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ), AJ12(MVSIZ), 
     .   AJ45(MVSIZ),AJ78(MVSIZ),A17(MVSIZ),A28(MVSIZ),B17(MVSIZ),
     .   B28(MVSIZ),C17(MVSIZ),C28(MVSIZ) 
      DOUBLE PRECISION
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ)
C=======================================================================
      
      DO I=LFT,LLT
        X17(I)=XD7(I)-XD1(I)
        X28(I)=XD8(I)-XD2(I)
        X35(I)=XD5(I)-XD3(I)
        X46(I)=XD6(I)-XD4(I)
        Y17(I)=YD7(I)-YD1(I)
        Y28(I)=YD8(I)-YD2(I)
        Y35(I)=YD5(I)-YD3(I)
        Y46(I)=YD6(I)-YD4(I)
        Z17(I)=ZD7(I)-ZD1(I)
        Z28(I)=ZD8(I)-ZD2(I)
        Z35(I)=ZD5(I)-ZD3(I)
        Z46(I)=ZD6(I)-ZD4(I)
      ENDDO
C
      DO I=LFT,LLT
        AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
        AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17(I)=X17(I)+X46(I)
        A28(I)=X28(I)+X35(I)
        B17(I)=Y17(I)+Y46(I)
        B28(I)=Y28(I)+Y35(I)
        C17(I)=Z17(I)+Z46(I)
        C28(I)=Z28(I)+Z35(I)
      ENDDO
C
      DO I=LFT,LLT
        AJ7(I)=A17(I)+A28(I)
        AJ8(I)=B17(I)+B28(I)
        AJ9(I)=C17(I)+C28(I)
        AJ1(I)=A17(I)-A28(I)
        AJ2(I)=B17(I)-B28(I)
        AJ3(I)=C17(I)-C28(I)
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
        JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
C
      DO I=LFT,LLT
       VOLDP(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
       DET(I)=VOLDP(I)
       VOL(I)=DET(I)
      ENDDO
C
      DO I=LFT,LLT
        IF(DET(I) <=ZERO) THEN
          CALL ANCMSG(MSGID=245,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NGL(I))
        ENDIF
      ENDDO
C
      IF (JEUL == 0 .AND. NXREF == 0) RETURN
C
      DO I=LFT,LLT
        DETT(I)=ONE_OVER_64/DET(I)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
        AJI1(I)=DETT(I)*JAC_59_68(I)
        AJI4(I)=DETT(I)*JAC_67_49(I)
        AJI7(I)=DETT(I)*JAC_48_57(I)
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
        PX2(I)= AJ12(I)-AJI3(I)
        PY2(I)= AJ45(I)-AJI6(I)
        PZ2(I)= AJ78(I)-AJI9(I)
        PX4(I)=-AJ12(I)-AJI3(I)
        PY4(I)=-AJ45(I)-AJI6(I)
        PZ4(I)=-AJ78(I)-AJI9(I)
      ENDDO
C
      DO I=LFT,LLT
        AJ12(I)=AJI1(I)+AJI2(I)
        AJ45(I)=AJI4(I)+AJI5(I)
        AJ78(I)=AJI7(I)+AJI8(I)
      ENDDO
C
      DO I=LFT,LLT
        PX1(I)=-AJ12(I)-AJI3(I)
        PY1(I)=-AJ45(I)-AJI6(I)
        PZ1(I)=-AJ78(I)-AJI9(I)
        PX3(I)=AJ12(I)-AJI3(I)
        PY3(I)=AJ45(I)-AJI6(I)
        PZ3(I)=AJ78(I)-AJI9(I)
      ENDDO
C
      IF(JEUL /= 0)THEN
        DO I=LFT,LLT
          VEUL(3,I) = PX3(I)
          VEUL(4,I) = PY3(I)
          VEUL(7,I) = PZ3(I)
          VEUL(8,I) = PX4(I)
          VEUL(11,I)= PY4(I)
          VEUL(12,I)= PZ4(I)
          VEUL(1,I) = PX1(I)
          VEUL(2,I) = PY1(I)
          VEUL(5,I) = PZ1(I)
          VEUL(6,I) = PX2(I)
          VEUL(9,I) = PY2(I)
          VEUL(10,I)= PZ2(I)
        END DO
        IF (IGEO(11,NGEO(LFT)) == 15) THEN
          DO I=LFT,LLT
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          ENDDO              
        ENDIF
      ENDIF
C-----------
      RETURN
      END
