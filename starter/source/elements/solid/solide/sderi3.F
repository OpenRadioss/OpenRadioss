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
     .              JAC1 ,JAC2 ,JAC3 ,JAC4 ,JAC5  ,JAC6 ,NGL   ,NGEO  ,
     .              PX1  ,PX2  ,PX3  ,PX4  ,PY1   ,PY2  ,PY3   ,PY4   ,
     .              PZ1  ,PZ2  ,PZ3  ,PZ4  ,DET   ,VOLDP,NEL   ,JEUL  ,
     .              NXREF,IMULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C----------------------------------------------------------
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
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IGEO(NPROPGI,NUMGEO), NGL(NEL), NGEO(NEL)
      INTEGER :: NEL, JEUL, NXREF, IMULTI_FVM
C
      my_real
     .   VOL(NEL), VEUL(LVEUL,*) , GEO(NPROPG,NUMGEO),
     .   JAC1(NEL), JAC2(NEL), JAC3(NEL), JAC4(NEL), JAC5(NEL), JAC6(NEL),
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL), DET(NEL)
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   VOLDP(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C
      my_real :: JAC7(MVSIZ), JAC8(MVSIZ) , JAC9(MVSIZ)
      my_real
     .   X_17_46, X_28_35,
     .   Y_17_46, Y_28_35,
     .   Z_17_46, Z_28_35
      my_real
     .   DETT(MVSIZ), 
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ), 
     .   JACI4(MVSIZ), JACI5(MVSIZ), JACI6(MVSIZ),
     .   JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ)
      DOUBLE PRECISION
     .   X17, X28, X35, X46,
     .   Y17, Y28, Y35, Y46,
     .   Z17, Z28, Z35, Z46
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
C Jacobian matrix
      DO I=1,NEL
        X17=XD7(I)-XD1(I)
        X28=XD8(I)-XD2(I)
        X35=XD5(I)-XD3(I)
        X46=XD6(I)-XD4(I)
C
        Y17=YD7(I)-YD1(I)
        Y28=YD8(I)-YD2(I)
        Y35=YD5(I)-YD3(I)
        Y46=YD6(I)-YD4(I)
C
        Z17=ZD7(I)-ZD1(I)
        Z28=ZD8(I)-ZD2(I)
        Z35=ZD5(I)-ZD3(I)
        Z46=ZD6(I)-ZD4(I)
C
        JAC1(I)=X17+X28-X35-X46
        JAC2(I)=Y17+Y28-Y35-Y46
        JAC3(I)=Z17+Z28-Z35-Z46
C
        X_17_46=X17+X46
        X_28_35=X28+X35
        Y_17_46=Y17+Y46
        Y_28_35=Y28+Y35
        Z_17_46=Z17+Z46
        Z_28_35=Z28+Z35
C
        JAC4(I)=X_17_46+X_28_35
        JAC5(I)=Y_17_46+Y_28_35
        JAC6(I)=Z_17_46+Z_28_35
C
        JAC7(I)=X_17_46-X_28_35
        JAC8(I)=Y_17_46-Y_28_35
        JAC9(I)=Z_17_46-Z_28_35
      ENDDO
C
      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
       VOLDP(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
       DET(I)=VOLDP(I)
       VOL(I)=DET(I)
      ENDDO     
C
      IF(JEUL * (1 - IMULTI_FVM) /= 0)THEN
        DO I=1,NEL
          VEUL(32,I) = VOL(I)
        ENDDO
      ENDIF             
C
      DO I=1,NEL
        IF (DET(I) > ZERO) CYCLE
        IF (IGEO(11,NGEO(I)) /= 0 .AND. IGEO(11,NGEO(I)) /= 43) THEN
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
      DO I=1,NEL
        DETT(I)=ONE_OVER_64/MAX(DET(I),EM20)
      ENDDO
C
C Jacobian matrix inverse
      DO I=1,NEL
        JACI1(I)=DETT(I)*JAC_59_68(I)
        JACI4(I)=DETT(I)*JAC_67_49(I)
        JACI7(I)=DETT(I)*JAC_48_57(I)
        JACI2(I)=DETT(I)*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JACI5(I)=DETT(I)*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JACI8(I)=DETT(I)*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JACI3(I)=DETT(I)*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JACI6(I)=DETT(I)*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JACI9(I)=DETT(I)*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      ENDDO
C
C Shape functions partial derivatives
      DO I=1,NEL
        PX1(I)=-JACI1(I)-JACI2(I)-JACI3(I)
        PY1(I)=-JACI4(I)-JACI5(I)-JACI6(I)
        PZ1(I)=-JACI7(I)-JACI8(I)-JACI9(I)
C
        PX2(I)=-JACI1(I)-JACI2(I)+JACI3(I)
        PY2(I)=-JACI4(I)-JACI5(I)+JACI6(I)
        PZ2(I)=-JACI7(I)-JACI8(I)+JACI9(I)
C
        PX3(I)= JACI1(I)-JACI2(I)+JACI3(I)
        PY3(I)= JACI4(I)-JACI5(I)+JACI6(I)
        PZ3(I)= JACI7(I)-JACI8(I)+JACI9(I)
C
        PX4(I)= JACI1(I)-JACI2(I)-JACI3(I)
        PY4(I)= JACI4(I)-JACI5(I)-JACI6(I)
        PZ4(I)= JACI7(I)-JACI8(I)-JACI9(I)
      ENDDO
C
      IF(JEUL * (1 - IMULTI_FVM) /= 0)THEN
        DO I=1,NEL
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
C
        IF (IGEO(11,NGEO(1)) == 15) THEN
          DO I=1,NEL
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          END DO              
        END IF
      END IF
C
      RETURN
      END SUBROUTINE SDERI3
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
     .              JAC_I,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
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
      INTEGER :: NEL
C
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   X7(*), X8(*), Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*),
     .   Y8(*), Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C      
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C
      my_real
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ), 
     .   JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     .   JAC7(MVSIZ), JAC8(MVSIZ), JAC9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ)
C
      my_real
     .   DET(MVSIZ), DETT(MVSIZ), 
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ),
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ)
C=======================================================================
      DO I=1,NEL
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

      DO I=1,NEL
        X17(I)=XD7(I)-XD1(I)
        X28(I)=XD8(I)-XD2(I)
        X35(I)=XD5(I)-XD3(I)
        X46(I)=XD6(I)-XD4(I)
C
        Y17(I)=YD7(I)-YD1(I)
        Y28(I)=YD8(I)-YD2(I)
        Y35(I)=YD5(I)-YD3(I)
        Y46(I)=YD6(I)-YD4(I)
C
        Z17(I)=ZD7(I)-ZD1(I)
        Z28(I)=ZD8(I)-ZD2(I)
        Z35(I)=ZD5(I)-ZD3(I)
        Z46(I)=ZD6(I)-ZD4(I)
      ENDDO
C
C Jacobian Matrix
      DO I=1,NEL               
        JAC1(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
C
        JAC4(I)=X17(I)+X46(I)+X28(I)+X35(I)
        JAC5(I)=Y17(I)+Y46(I)+Y28(I)+Y35(I)
        JAC6(I)=Z17(I)+Z46(I)+Z28(I)+Z35(I)
C
        JAC7(I)=X17(I)+X46(I)-X28(I)-X35(I)
        JAC8(I)=Y17(I)+Y46(I)-Y28(I)-Y35(I)
        JAC9(I)=Z17(I)+Z46(I)-Z28(I)-Z35(I)
      ENDDO
C
      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
       DET(I) =ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
       DETT(I)=ONE_OVER_64/MAX(DET(I),EM20)
      ENDDO     
C
C Jacobian matrix inverse
      DO I=1,NEL
        JAC_I(1,I)=DETT(I)*JAC_59_68(I)
        JAC_I(4,I)=DETT(I)*JAC_67_49(I)
        JAC_I(7,I)=DETT(I)*JAC_48_57(I)
        JAC_I(2,I)=DETT(I)*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JAC_I(5,I)=DETT(I)*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JAC_I(8,I)=DETT(I)*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JAC_I(3,I)=DETT(I)*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JAC_I(6,I)=DETT(I)*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JAC_I(9,I)=DETT(I)*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      ENDDO
C
      DO I=1,NEL
        JAC_I(10,I) = DET(I)
      ENDDO
C
      RETURN
      END SUBROUTINE SJAC_I
