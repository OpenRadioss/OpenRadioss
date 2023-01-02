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
Chd|  SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|-- called by -----------
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCDERI3(NEL,VOL,JEUL ,VEUL ,GEO  ,
     .         VZL  ,VZQ  ,NGL  ,NGEO ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,  
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,  
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8, DET)  

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL,JEUL,NGL(*),NGEO(*)
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
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ), JAC4(MVSIZ),
     .   JAC5(MVSIZ), JAC6(MVSIZ), JAC7(MVSIZ), JAC8(MVSIZ),
     .   JAC9(MVSIZ),
     .   JACI1(MVSIZ), JACI2(MVSIZ), JACI3(MVSIZ), JACI4(MVSIZ), 
     .   JACI5(MVSIZ), JACI6(MVSIZ), JACI7(MVSIZ), JACI8(MVSIZ), 
     .   JACI9(MVSIZ), 
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ), 
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   X_17_46(MVSIZ),Y_17_46(MVSIZ),Z_17_46(MVSIZ),
     .   X_28_35(MVSIZ),Y_28_35(MVSIZ),Z_28_35(MVSIZ)
      my_real
     .   H1X,H2X,H1Y,H2Y,H1Z,H2Z
C=======================================================================
      DO I=1,NEL
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
C Jacobian matrix
      DO I=1,NEL
C  -------ri.xi-----------
        JAC3(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC1(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC2(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        X_17_46(I)=X17(I)+X46(I)
        Y_17_46(I)=Y17(I)+Y46(I)
        Z_17_46(I)=Z17(I)+Z46(I)
        X_28_35(I)=X28(I)+X35(I)
        Y_28_35(I)=Y28(I)+Y35(I)
        Z_28_35(I)=Z28(I)+Z35(I)
      ENDDO
C
      DO I=1,NEL
C  -------si.xi-----------
        JAC6(I)=X_17_46(I)+X_28_35(I)
        JAC4(I)=Y_17_46(I)+Y_28_35(I)
        JAC5(I)=Z_17_46(I)+Z_28_35(I)
C  -------ti.xi-----------
        JAC9(I)=X_17_46(I)-X_28_35(I)
        JAC7(I)=Y_17_46(I)-Y_28_35(I)
        JAC8(I)=Z_17_46(I)-Z_28_35(I)
      ENDDO
C
      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
        DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
        VOL(I)=DET(I)
      ENDDO
C
      DO I=1,NEL
        IF(DET(I)>ZERO) CYCLE
        CALL ANCMSG(MSGID=245,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=NGL(I))
      ENDDO
C
      DO I=1,NEL
        H1X = X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I)
        H1Y = Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I)
        H1Z = Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I)
        H2X = X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I)
        H2Y = Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I)
        H2Z = Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I)
        VZL(I) =  ONE_OVER_64*JAC5(I)*(JAC9(I)*H1Y+JAC1(I)*H2X-JAC3(I)*H2Y-JAC7(I)*H1X)
     .          + ONE_OVER_64*JAC4(I)*(JAC3(I)*H2Z+JAC8(I)*H1X-JAC9(I)*H1Z-JAC2(I)*H2X)
     .          + ONE_OVER_64*JAC6(I)*(JAC7(I)*H1Z+JAC2(I)*H2Y-JAC1(I)*H2Z-JAC8(I)*H1Y)
        VZQ(I) =  ONE_OVER_64*JAC5(I)*(H1Y*H2X-H2Y*H1X)
      ENDDO
C--------------------------
      IF (JEUL == 0) RETURN
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
      DO I=1,NEL
        VEUL(3,I) = JACI1(I)-JACI2(I)+JACI3(I)
        VEUL(4,I) = JACI1(I)-JACI2(I)-JACI3(I)
        VEUL(7,I) = JACI4(I)-JACI5(I)+JACI6(I)
        VEUL(8,I) = JACI4(I)-JACI5(I)-JACI6(I)
        VEUL(11,I)= JACI7(I)-JACI8(I)+JACI9(I)
        VEUL(12,I)= JACI7(I)-JACI8(I)-JACI9(I)
      ENDDO
C
      DO I=1,NEL
        VEUL(1,I) =-JACI1(I)-JACI2(I)-JACI3(I)
        VEUL(2,I) =-JACI1(I)-JACI2(I)+JACI3(I)
        VEUL(5,I) =-JACI4(I)-JACI5(I)-JACI6(I)
        VEUL(6,I) =-JACI4(I)-JACI5(I)+JACI6(I)
        VEUL(9,I) =-JACI7(I)-JACI8(I)-JACI9(I)
        VEUL(10,I)=-JACI7(I)-JACI8(I)+JACI9(I)
      ENDDO
C
      IF (GEO(12,NGEO(I)) == 15) THEN
          DO I=1,NEL
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          ENDDO
      ENDIF
c-----------
      RETURN
      END SUBROUTINE SCDERI3
