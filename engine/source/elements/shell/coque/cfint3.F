Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CFINT3                        source/elements/shell/coque/cfint3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CFINT3(JFT,JLT  ,FOR    ,MOM    ,THK0   ,THK02  ,
     2            PX1    ,PX2    ,PY1    ,PY2    ,AREA   ,Z2     ,
     3            F11    ,F12    ,F13    ,F14    ,F21    ,F22    ,
     4            F23    ,F24    ,F31    ,F32    ,F33    ,F34    ,
     5            H11    ,H12    ,H13    ,H21    ,H22    ,H23    ,
     6            H31    ,H32    ,H33    ,B11    ,B12    ,B13    ,
     7            B14    ,B21    ,B22    ,B23    ,B24    ,NEL    ,
     8            M11    ,M12    ,M13    ,M14    ,M21    ,M22    , 
     9            M23    ,M24    ,M31    ,M32    ,M33    ,M34    ,
     A            E1X    ,E1Y    ,E1Z    ,E2X    ,E2Y    ,E2Z    ,
     B            E3X    ,E3Y    ,E3Z    ,IHBE   ,NPT    ,FZERO  )
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
      INTEGER JFT,JLT,NPT,IHBE,NEL
C     REAL
      my_real
     .   FOR(NEL,5), MOM(NEL,3), THK0(*), THK02(*),
     .   AREA(*),   PX1(*), PX2(*), PY1(*), PY2(*), Z2(*),
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .     B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ),
     .     B21(MVSIZ), B22(MVSIZ), B23(MVSIZ), B24(MVSIZ),
     .     H11(MVSIZ), H12(MVSIZ), H13(MVSIZ), H14(MVSIZ),
     .     H21(MVSIZ), H22(MVSIZ), H23(MVSIZ), H24(MVSIZ),
     .     H31(MVSIZ), H32(MVSIZ), H33(MVSIZ), H34(MVSIZ)
      my_real
     .     M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .     M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .     M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ)
      my_real
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),FZERO(3,4,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .     M4(MVSIZ), M5(MVSIZ), F1(MVSIZ), F2(MVSIZ), F3(MVSIZ), 
     .     F4(MVSIZ), F5(MVSIZ), M1(MVSIZ), M2(MVSIZ), M3(MVSIZ),
     .     G11T(MVSIZ), G21T(MVSIZ), G12T(MVSIZ), G22T(MVSIZ), 
     .     G13T(MVSIZ), G23T(MVSIZ), G14T(MVSIZ), G24T(MVSIZ),
     .     G31T(MVSIZ), G32T(MVSIZ), G33T(MVSIZ), G34T(MVSIZ),
     .     F1A,F2A,F3A,F4A,F5A, G11TA, G21TA, G12TA, G22TA, 
     .     G13TA, G23TA, G14TA, G24TA, G31TA, G32TA, G33TA, G34TA,
     .     M1A,M2A,M3A,TEMP1,TEMP2,TEMP3
C=======================================================================
C               POUR ISIGI = 5 OU -5
C     CHANGEMENT DE REPERE DE FZERO POUR TT=DT1
C          REPERE GLOBAL ---> REPERE LOCAL
C-----------------------------------------------
      IF(IABS(ISIGI)==5.AND.NCYCLE==1.AND.IRUN==1) THEN
        DO I=JFT,JLT
          DO J=1,4
            TEMP1=FZERO(1,J,I)
            TEMP2=FZERO(2,J,I)
            TEMP3=FZERO(3,J,I)
            FZERO(1,J,I)=TEMP1*E1X(I)+TEMP2*E1Y(I)+TEMP3*E1Z(I)
            FZERO(2,J,I)=TEMP1*E2X(I)+TEMP2*E2Y(I)+TEMP3*E2Z(I)
            FZERO(3,J,I)=TEMP1*E3X(I)+TEMP2*E3Y(I)+TEMP3*E3Z(I)
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------
C               POUR ISIGI = 5 OU -5
C     PRISE EN COMPTE DES FORCES D EQUILIBRAGE
C      DANS L EVALUATION DES EFFORTS INTERNES
C-----------------------------------------------
      IF((IABS(ISIGI)==5.AND.NCYCLE>=1.AND.IRUN==1).OR.
     .     (IABS(ISIGI)==5.AND.IRUN>1)) THEN
          DO I=JFT,JLT
            F1A= FOR(I,1)*THK0(I)
            F2A= FOR(I,2)*THK0(I)
            F3A= FOR(I,3)*THK0(I)
            F4A= FOR(I,4)*THK0(I)
            F5A= FOR(I,5)*THK0(I)
            M4(I)= F4A*AREA(I)
            M5(I)= F5A*AREA(I)
C
            F12(I)= F1A*PX2(I)+F3A*PY2(I)
            F22(I)= F2A*PY2(I)+F3A*PX2(I)
            F32(I)= F5A*PX2(I)+F4A*PY2(I)
            F11(I)= F1A*PX1(I)+F3A*PY1(I)
            F21(I)= F2A*PY1(I)+F3A*PX1(I)
            F31(I)= F5A*PX1(I)+F4A*PY1(I)
C
            G11TA=F11(I)+H11(I)
            G13TA=H13(I)-F11(I)
            G21TA=F21(I)+H21(I)
            G23TA=H23(I)-F21(I)
            G31TA=F31(I)+H31(I)
            G33TA=H33(I)-F31(I)
            G12TA=F12(I)+H12(I)
            G22TA=F22(I)+H22(I)
            G32TA=F32(I)+H32(I)
            G14TA=-G11TA-G12TA-G13TA
            G24TA=-G21TA-G22TA-G23TA
            G34TA=-G31TA-G32TA-G33TA
C
            G11TA=G11TA+FZERO(1,1,I)
            G13TA=G13TA+FZERO(1,3,I)
            G21TA=G21TA+FZERO(2,1,I)
            G23TA=G23TA+FZERO(2,3,I)
            G31TA=G31TA+FZERO(3,1,I)
            G33TA=G33TA+FZERO(3,3,I)
            G12TA=G12TA+FZERO(1,2,I)
            G22TA=G22TA+FZERO(2,2,I)
            G32TA=G32TA+FZERO(3,2,I)
            G14TA=G14TA+FZERO(1,4,I)
            G24TA=G24TA+FZERO(2,4,I)
            G34TA=G34TA+FZERO(3,4,I)
C
            F11(I)=E1X(I)*G11TA+E2X(I)*G21TA+E3X(I)*G31TA
            F12(I)=E1X(I)*G12TA+E2X(I)*G22TA+E3X(I)*G32TA
            F13(I)=E1X(I)*G13TA+E2X(I)*G23TA+E3X(I)*G33TA
            F14(I)=E1X(I)*G14TA+E2X(I)*G24TA+E3X(I)*G34TA
C
            F21(I)=E1Y(I)*G11TA+E2Y(I)*G21TA+E3Y(I)*G31TA
            F22(I)=E1Y(I)*G12TA+E2Y(I)*G22TA+E3Y(I)*G32TA
            F23(I)=E1Y(I)*G13TA+E2Y(I)*G23TA+E3Y(I)*G33TA
            F24(I)=E1Y(I)*G14TA+E2Y(I)*G24TA+E3Y(I)*G34TA
C
            F31(I)=E1Z(I)*G11TA+E2Z(I)*G21TA+E3Z(I)*G31TA
            F32(I)=E1Z(I)*G12TA+E2Z(I)*G22TA+E3Z(I)*G32TA
            F33(I)=E1Z(I)*G13TA+E2Z(I)*G23TA+E3Z(I)*G33TA
            F34(I)=E1Z(I)*G14TA+E2Z(I)*G24TA+E3Z(I)*G34TA
          ENDDO
      ENDIF
      IF(IABS(ISIGI)/=5.OR.TT==0) THEN
          DO I=JFT,JLT
            F1A= FOR(I,1)*THK0(I)
            F2A= FOR(I,2)*THK0(I)
            F3A= FOR(I,3)*THK0(I)
            F4A= FOR(I,4)*THK0(I)
            F5A= FOR(I,5)*THK0(I)
            M4(I)= F4A*AREA(I)
            M5(I)= F5A*AREA(I)
C
            F12(I)= F1A*PX2(I)+F3A*PY2(I)
            F22(I)= F2A*PY2(I)+F3A*PX2(I)
            F32(I)= F5A*PX2(I)+F4A*PY2(I)
            F11(I)= F1A*PX1(I)+F3A*PY1(I)
            F21(I)= F2A*PY1(I)+F3A*PX1(I)
            F31(I)= F5A*PX1(I)+F4A*PY1(I)
C
            G11TA=F11(I)+H11(I)
            G13TA=H13(I)-F11(I)
            G21TA=F21(I)+H21(I)
            G23TA=H23(I)-F21(I)
            G31TA=F31(I)+H31(I)
            G33TA=H33(I)-F31(I)
            G12TA=F12(I)+H12(I)
            G22TA=F22(I)+H22(I)
            G32TA=F32(I)+H32(I)
            F11(I)=E1X(I)*G11TA+E2X(I)*G21TA+E3X(I)*G31TA
            F12(I)=E1X(I)*G12TA+E2X(I)*G22TA+E3X(I)*G32TA
            F13(I)=E1X(I)*G13TA+E2X(I)*G23TA+E3X(I)*G33TA
            F21(I)=E1Y(I)*G11TA+E2Y(I)*G21TA+E3Y(I)*G31TA
            F22(I)=E1Y(I)*G12TA+E2Y(I)*G22TA+E3Y(I)*G32TA
            F23(I)=E1Y(I)*G13TA+E2Y(I)*G23TA+E3Y(I)*G33TA
            F31(I)=E1Z(I)*G11TA+E2Z(I)*G21TA+E3Z(I)*G31TA
            F32(I)=E1Z(I)*G12TA+E2Z(I)*G22TA+E3Z(I)*G32TA
            F33(I)=E1Z(I)*G13TA+E2Z(I)*G23TA+E3Z(I)*G33TA
            F14(I)=-F11(I)-F12(I)-F13(I)
            F24(I)=-F21(I)-F22(I)-F23(I)
            F34(I)=-F31(I)-F32(I)-F33(I)
          ENDDO
      ENDIF
C
      IF((IHBE>=2).AND.IABS(NPT)/=1)THEN
        DO I=JFT,JLT
          M4(I)= M4(I)+(H21(I)+H23(I))*Z2(I)
          M5(I)= M5(I)+(H11(I)+H13(I))*Z2(I)
        ENDDO
C
      ENDIF
C
      DO I=JFT,JLT
        M1A= MOM(I,1)*THK02(I)
        M2A= MOM(I,2)*THK02(I)
        M3A= MOM(I,3)*THK02(I)
        M4(I)= M4(I)*FOURTH
        M5(I)= M5(I)*FOURTH
C
        M11(I)=-M2A*PY1(I)-M3A*PX1(I)
        M21(I)= M1A*PX1(I)+M3A*PY1(I)
        M12(I)=-M2A*PY2(I)-M3A*PX2(I)
        M22(I)= M1A*PX2(I)+M3A*PY2(I)
C
        G11TA = M11(I)-M4(I)+B11(I)
        G13TA =-M11(I)-M4(I)+B13(I)
        G12TA = M12(I)-M4(I)+B12(I)
        G14TA =-M12(I)-M4(I)+B14(I)
        G21TA = M21(I)+M5(I)+B21(I)
        G23TA =-M21(I)+M5(I)+B23(I)
        G22TA = M22(I)+M5(I)+B22(I)
        G24TA =-M22(I)+M5(I)+B24(I)
C
        M11(I)=E1X(I)*G11TA+E2X(I)*G21TA
        M12(I)=E1X(I)*G12TA+E2X(I)*G22TA
        M13(I)=E1X(I)*G13TA+E2X(I)*G23TA
        M14(I)=E1X(I)*G14TA+E2X(I)*G24TA
        M21(I)=E1Y(I)*G11TA+E2Y(I)*G21TA
        M22(I)=E1Y(I)*G12TA+E2Y(I)*G22TA
        M23(I)=E1Y(I)*G13TA+E2Y(I)*G23TA
        M24(I)=E1Y(I)*G14TA+E2Y(I)*G24TA
        M31(I)=E1Z(I)*G11TA+E2Z(I)*G21TA
        M32(I)=E1Z(I)*G12TA+E2Z(I)*G22TA
        M33(I)=E1Z(I)*G13TA+E2Z(I)*G23TA
        M34(I)=E1Z(I)*G14TA+E2Z(I)*G24TA
      ENDDO
c-----------
      RETURN
      END
c
