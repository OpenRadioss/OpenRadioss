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
Chd|  SDERI3                        source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SDERI3(
     .              VOL  ,VEUL ,GEO  ,IGEO , 
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5   ,XD6  ,XD7   ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5   ,YD6  ,YD7   ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5   ,ZD6  ,ZD7   ,ZD8   ,
     .              AJ1  ,AJ2  ,AJ3  ,AJ4  ,AJ5  ,AJ6  ,NGL  ,NGEO ,
     .              PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .              PZ1  ,PZ2  ,PZ3  ,PZ4, DET   ,VOLDP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C----------------------------------------------------------
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
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*), NGL(*), NGEO(*)
C     REAL
      my_real
     .   VOL(*), VEUL(LVEUL,*) , GEO(NPROPG,*),
     .   AJ1(*), AJ2(*), AJ3(*), AJ4(*), AJ5(*), AJ6(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), DET(*)
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
      
C     REAL
      my_real
     .   AJ7(MVSIZ), AJ8(MVSIZ) , AJ9(MVSIZ),
     .   A17, A28,
     .   B17, B28,
     .   C17, C28
      my_real
     .   DETT(MVSIZ), 
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ), 
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ),
     .   AJ12(MVSIZ),   AJ45(MVSIZ),   AJ78(MVSIZ)
      DOUBLE PRECISION
     .   X17, X28, X35, X46,
     .   Y17, Y28, Y35, Y46,
     .   Z17, Z28, Z35, Z46
C=======================================================================
      DO I=LFT,LLT
        X17=XD7(I)-XD1(I)
        X28=XD8(I)-XD2(I)
        X35=XD5(I)-XD3(I)
        X46=XD6(I)-XD4(I)
        Y17=YD7(I)-YD1(I)
        Y28=YD8(I)-YD2(I)
        Y35=YD5(I)-YD3(I)
        Y46=YD6(I)-YD4(I)
        Z17=ZD7(I)-ZD1(I)
        Z28=ZD8(I)-ZD2(I)
        Z35=ZD5(I)-ZD3(I)
        Z46=ZD6(I)-ZD4(I)
        AJ1(I)=X17+X28-X35-X46
        AJ2(I)=Y17+Y28-Y35-Y46
        AJ3(I)=Z17+Z28-Z35-Z46
        A17=X17+X46
        A28=X28+X35
        B17=Y17+Y46
        B28=Y28+Y35
        C17=Z17+Z46
        C28=Z28+Z35
        AJ4(I)=A17+A28
        AJ5(I)=B17+B28
        AJ6(I)=C17+C28
        AJ7(I)=A17-A28
        AJ8(I)=B17-B28
        AJ9(I)=C17-C28
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
        JAC1(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC2(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC3(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
       VOLDP(I)=ONE_OVER_64*(AJ1(I)*JAC1(I)+AJ2(I)*JAC2(I)+AJ3(I)*JAC3(I))
       DET(I)=VOLDP(I)
       VOL(I)=DET(I)
      ENDDO     
C
      IF(JEUL * (1 - IMULTI_FVM) /= 0)THEN
        DO I=LFT,LLT
          VEUL(32,I) = VOL(I)
        ENDDO
      ENDIF             
C
      DO I=LFT,LLT
        IF (DET(I) > ZERO) CYCLE
        IF (IGEO(11,NGEO(I)).NE.0 .AND. IGEO(11,NGEO(I)).NE.43) THEN
          CALL ANCMSG(MSGID=245,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=NGL(I))
        ELSE
          CALL ANCMSG(MSGID=635,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=NGL(I))
        ENDIF
      ENDDO
C
      IF( JEUL==0 .AND. NXREF==0) RETURN
C
      DO I=LFT,LLT
        DETT(I)=ONE_OVER_64/MAX(DET(I),EM20)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
        AJI1(I)=DETT(I)*JAC1(I)
        AJI4(I)=DETT(I)*JAC2(I)
        AJI7(I)=DETT(I)*JAC3(I)
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
        PX3(I)= AJ12(I)+AJI3(I)
        PY3(I)= AJ45(I)+AJI6(I)
        PZ3(I)= AJ78(I)+AJI9(I)
        PX4(I)= AJ12(I)-AJI3(I)
        PY4(I)= AJ45(I)-AJI6(I)
        PZ4(I)= AJ78(I)-AJI9(I)
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
        PX2(I)=-AJ12(I)+AJI3(I)
        PY2(I)=-AJ45(I)+AJI6(I)
        PZ2(I)=-AJ78(I)+AJI9(I)
      ENDDO
C---
      IF(JEUL * (1 - IMULTI_FVM) /= 0)THEN
        DO I=LFT,LLT
          VEUL(1,I) = PX1(I)
          VEUL(2,I) = PX2(I)
          VEUL(3,I) = PX3(I)
          VEUL(4,I) = PX4(I)
          VEUL(5,I) = PY1(I)
          VEUL(6,I) = PY2(I)
          VEUL(7,I) = PY3(I)
          VEUL(8,I) = PY4(I)
          VEUL(9,I) = PZ1(I)
          VEUL(10,I)= PZ2(I)
          VEUL(11,I)= PZ3(I)
          VEUL(12,I)= PZ4(I)
        END DO
        IF (IGEO(11,NGEO(LFT)) == 15) THEN
          DO I=LFT,LLT
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          END DO              
        END IF
      END IF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SJAC_I                        source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SJAC_I(
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              JAC_I)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C------------------------------------------------------
C------------------------------------------------------
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   X7(*), X8(*), Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*),
     .   Y8(*), Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C     REAL
      my_real
     .   AJ1(MVSIZ), AJ2(MVSIZ), AJ3(MVSIZ), 
     .   AJ4(MVSIZ), AJ5(MVSIZ), AJ6(MVSIZ),
     .   AJ7(MVSIZ), AJ8(MVSIZ) , AJ9(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) 
      my_real
     .   DET(MVSIZ), DETT(MVSIZ), 
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ), 
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ),
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ),
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ),
     .   AJ12(MVSIZ),   AJ45(MVSIZ),   AJ78(MVSIZ)
C=======================================================================
      DO I=LFT,LLT
        XD1(I)=X1(I)
        XD2(I)=X2(I)
        XD3(I)=X3(I)
        XD4(I)=X4(I)
        XD5(I)=X5(I)
        XD6(I)=X6(I)
        XD7(I)=X7(I)
        XD8(I)=X8(I)
        YD1(I)=Y1(I)
        YD2(I)=Y2(I)
        YD3(I)=Y3(I)
        YD4(I)=Y4(I)
        YD5(I)=Y5(I)
        YD6(I)=Y6(I)
        YD7(I)=Y7(I)
        YD8(I)=Y8(I)    
        ZD1(I)=Z1(I)
        ZD2(I)=Z2(I)
        ZD3(I)=Z3(I)
        ZD4(I)=Z4(I)
        ZD5(I)=Z5(I)
        ZD6(I)=Z6(I)
        ZD7(I)=Z7(I)
        ZD8(I)=Z8(I)
      ENDDO     

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
        AJ1(I)=X17(I)+X28(I)-X35(I)-X46(I)
        AJ2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        AJ3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17(I)=X17(I)+X46(I)
        A28(I)=X28(I)+X35(I)
        B17(I)=Y17(I)+Y46(I)
        B28(I)=Y28(I)+Y35(I)
        C17(I)=Z17(I)+Z46(I)
        C28(I)=Z28(I)+Z35(I)
      ENDDO
         
      DO I=LFT,LLT
        AJ4(I)=A17(I)+A28(I)
        AJ5(I)=B17(I)+B28(I)
        AJ6(I)=C17(I)+C28(I)
        AJ7(I)=A17(I)-A28(I)
        AJ8(I)=B17(I)-B28(I)
        AJ9(I)=C17(I)-C28(I)
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
        JAC1(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC2(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC3(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=LFT,LLT
       DET(I) =ONE_OVER_64*(AJ1(I)*JAC1(I)+AJ2(I)*JAC2(I)+AJ3(I)*JAC3(I))
       DETT(I)=ONE_OVER_64/MAX(DET(I),EM20)
      ENDDO     
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
        AJI1(I)=DETT(I)*JAC1(I)
        AJI4(I)=DETT(I)*JAC2(I)
        AJI7(I)=DETT(I)*JAC3(I)
        AJI2(I)=DETT(I)*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5(I)=DETT(I)*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8(I)=DETT(I)*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3(I)=DETT(I)*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6(I)=DETT(I)*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9(I)=DETT(I)*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      ENDDO
C
       DO I=LFT,LLT
        JAC_I(1,I)  = AJI1(I)
        JAC_I(4,I)  = AJI4(I)
        JAC_I(7,I)  = AJI7(I)
        JAC_I(2,I)  = AJI2(I)
        JAC_I(5,I)  = AJI5(I)
        JAC_I(8,I)  = AJI8(I)
        JAC_I(3,I)  = AJI3(I)
        JAC_I(6,I)  = AJI6(I)
        JAC_I(9,I)  = AJI9(I)
        JAC_I(10,I) = DET(I)
       ENDDO
C-----------
      RETURN
      END
