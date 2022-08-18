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
     .              JAC1 ,JAC2 ,JAC3 ,JAC4 ,JAC5 ,JAC6 ,JAC9 ,
     .              NGL  ,NGEO ,DET  ,VOLDP,NEL  ,JEUL ,NXREF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IGEO(NPROPGI,*),NGL(*),NGEO(*),NEL,JEUL,NXREF
      my_real
     .   VOL(*), VEUL(LVEUL,*), GEO(NPROPG,*),
     .   JAC1(*), JAC2(*), JAC3(*), JAC4(*), JAC5(*), JAC6(*), JAC9(*),
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
      INTEGER I
      my_real
     .   DETT(MVSIZ),  JAC7(MVSIZ), JAC8(MVSIZ) , 
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ), JACI4(MVSIZ), 
     .   JACI5(MVSIZ), JACI6(MVSIZ), JACI7(MVSIZ), JACI8(MVSIZ), JACI9(MVSIZ), 
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35
      DOUBLE PRECISION
     .   X17, X28, X35, X46,
     .   Y17, Y28, Y35, Y46,
     .   Z17, Z28, Z35, Z46
C=======================================================================
C
C Jacobian matrix
      DO I=1,NEL
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
C
        JAC4(I)=X17+X28-X35-X46
        JAC5(I)=Y17+Y28-Y35-Y46
        JAC6(I)=Z17+Z28-Z35-Z46
        X_17_46=X17+X46
        X_28_35=X28+X35
        Y_17_46=Y17+Y46
        Y_28_35=Y28+Y35
        Z_17_46=Z17+Z46
        Z_28_35=Z28+Z35
        JAC7(I)=X_17_46+X_28_35
        JAC8(I)=Y_17_46+Y_28_35
        JAC9(I)=Z_17_46+Z_28_35
        JAC1(I)=X_17_46-X_28_35
        JAC2(I)=Y_17_46-Y_28_35
        JAC3(I)=Z_17_46-Z_28_35
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
      DO I=1,NEL
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
      DO I=1,NEL
        DETT(I)=ONE_OVER_64/DET(I)
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
C Shape functions derivatives
      DO I=1,NEL
        PX2(I)= JACI1(I)-JACI2(I)-JACI3(I)
        PY2(I)= JACI4(I)-JACI5(I)-JACI6(I)
        PZ2(I)= JACI7(I)-JACI8(I)-JACI9(I)
C
        PX4(I)=-JACI1(I)+JACI2(I)-JACI3(I)
        PY4(I)=-JACI4(I)+JACI5(I)-JACI6(I)
        PZ4(I)=-JACI7(I)+JACI8(I)-JACI9(I)
C
        PX1(I)=-JACI1(I)-JACI2(I)-JACI3(I)
        PY1(I)=-JACI4(I)-JACI5(I)-JACI6(I)
        PZ1(I)=-JACI7(I)-JACI8(I)-JACI9(I)
C
        PX3(I)= JACI1(I)+JACI2(I)-JACI3(I)
        PY3(I)= JACI4(I)+JACI5(I)-JACI6(I)
        PZ3(I)= JACI7(I)+JACI8(I)-JACI9(I)
      ENDDO
C
      IF(JEUL /= 0)THEN
        DO I=1,NEL
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
        IF (IGEO(11,NGEO(1)) == 15) THEN
          DO I=1,NEL
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          ENDDO              
        ENDIF
      ENDIF
C-----------
      RETURN
      END SUBROUTINE SZDERI3
