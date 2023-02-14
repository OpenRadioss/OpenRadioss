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
Chd|  S8SCOORK_IMP                  source/elements/solid/solide8s/srcoork_imp.F
Chd|-- called by -----------
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|-- calls ---------------
Chd|        CRTRANS_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|====================================================================
      SUBROUTINE S8SCOORK_IMP(X,IXS,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   OFFG,OFF,SAV, R,
     .   V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,KHBE,GAMA0 ,GAMA,
     .   NEL,TRM,KL)      !
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   OFFG(*), OFF(*), SAV(NEL,21), GAMA0(NEL,6),GAMA(MVSIZ,6),
     .   R(3,3,MVSIZ)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K22(9,*)    ,K23(9,*)   ,
     .   K24(9,*)   ,K25(9,*)   ,K26(9,*)    ,K27(9,*)    ,K28(9,*)   ,
     .   K33(9,*)   ,K34(9,*)   ,K35(9,*)    ,K36(9,*)    ,K37(9,*)   ,
     .   K38(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,K47(9,*)   ,
     .   K48(9,*)   ,K55(9,*)   ,K56(9,*)    ,K57(9,*)    ,K58(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)    ,K77(9,*)    ,K78(9,*)   ,
     .   K88(9,*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),
     .        NC5(*), NC6(*), NC7(*), NC8(*), MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*),KHBE
      DOUBLE PRECISION
     .   TRM(NEL,24,24),INVJ(MVSIZ,3,3),KL(576,NEL),
     .   V1(MVSIZ,9), V2(MVSIZ,9), V3(MVSIZ,9), V4(MVSIZ,9),
     .   V5(MVSIZ,9), V6(MVSIZ,9), V7(MVSIZ,9), V8(MVSIZ,9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MXT_1
C     REAL
      my_real
     .   G11(MVSIZ),G12(MVSIZ),G13(MVSIZ),
     .   G21(MVSIZ),G22(MVSIZ),G23(MVSIZ),
     .   G31(MVSIZ),G32(MVSIZ),G33(MVSIZ),
     .   T11(MVSIZ),T12(MVSIZ),T13(MVSIZ),
     .   T21(MVSIZ),T22(MVSIZ),T23(MVSIZ),
     .   T31(MVSIZ),T32(MVSIZ),T33(MVSIZ)
      my_real
     .   XL,YL,ZL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ)
      DOUBLE PRECISION DX_DR,DX_DS,DX_DT,DY_DR,DY_DS,DY_DT,DZ_DR,DZ_DS,DZ_DT
      DOUBLE PRECISION X0(8),Y0(8),Z0(8),DETM1,DETJ0
C-----------------------------------------------
C
      MXT_1 = IXS(1,LFT)
      DO I=LFT,LLT
        NGEO(I)=IXS(10,I)
        NGL(I)=IXS(11,I)
        MXT(I)=MXT_1
        NC1(I)=IXS(2,I)
        NC2(I)=IXS(3,I)
        NC3(I)=IXS(4,I)
        NC4(I)=IXS(5,I)
        NC5(I)=IXS(6,I)
        NC6(I)=IXS(7,I)
        NC7(I)=IXS(8,I)
        NC8(I)=IXS(9,I)
      ENDDO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO I=LFT,LLT
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        X5(I)=X(1,NC5(I))
        Y5(I)=X(2,NC5(I))
        Z5(I)=X(3,NC5(I))
        X6(I)=X(1,NC6(I))
        Y6(I)=X(2,NC6(I))
        Z6(I)=X(3,NC6(I))
        X7(I)=X(1,NC7(I))
        Y7(I)=X(2,NC7(I))
        Z7(I)=X(3,NC7(I))
        X8(I)=X(1,NC8(I))
        Y8(I)=X(2,NC8(I))
        Z8(I)=X(3,NC8(I))
      ENDDO
C-----------
C     Local system
C-----------
      DO I=LFT,LLT
        X0(1) = ZERO
        Y0(1) = ZERO
        Z0(1) = ZERO
        X0(2) = SAV(I,1)
        Y0(2) = SAV(I,2)
        Z0(2) = SAV(I,3)
        X0(3) = SAV(I,4)
        Y0(3) = SAV(I,5)
        Z0(3) = SAV(I,6)
        X0(4) = SAV(I,7)
        Y0(4) = SAV(I,8)
        Z0(4) = SAV(I,9)
        X0(5) = SAV(I,10)
        Y0(5) = SAV(I,11)
        Z0(5) = SAV(I,12)
        X0(6) = SAV(I,13)
        Y0(6) = SAV(I,14)
        Z0(6) = SAV(I,15)
        X0(7) = SAV(I,16)
        Y0(7) = SAV(I,17)
        Z0(7) = SAV(I,18)
        X0(8) = SAV(I,19)
        Y0(8) = SAV(I,20)
        Z0(8) = SAV(I,21)

        DX_DR = (X0(3)+X0(4)+X0(7)+X0(8))-(X0(1)+X0(2)+X0(5)+X0(6))
        DY_DR = (Y0(3)+Y0(4)+Y0(7)+Y0(8))-(Y0(1)+Y0(2)+Y0(5)+Y0(6))
        DZ_DR = (Z0(3)+Z0(4)+Z0(7)+Z0(8))-(Z0(1)+Z0(2)+Z0(5)+Z0(6))
        DX_DS = (X0(5)+X0(6)+X0(7)+X0(8))-(X0(1)+X0(2)+X0(3)+X0(4))
        DY_DS = (Y0(5)+Y0(6)+Y0(7)+Y0(8))-(Y0(1)+Y0(2)+Y0(3)+Y0(4))
        DZ_DS = (Z0(5)+Z0(6)+Z0(7)+Z0(8))-(Z0(1)+Z0(2)+Z0(3)+Z0(4))
        DX_DT = (X0(2)+X0(3)+X0(6)+X0(7))-(X0(1)+X0(4)+X0(5)+X0(8))
        DY_DT = (Y0(2)+Y0(3)+Y0(6)+Y0(7))-(Y0(1)+Y0(4)+Y0(5)+Y0(8))
        DZ_DT = (Z0(2)+Z0(3)+Z0(6)+Z0(7))-(Z0(1)+Z0(4)+Z0(5)+Z0(8))
        DETJ0 =(DX_DR*(DY_DS*DZ_DT-DZ_DS*DY_DT)
     .         -DX_DS*(DY_DR*DZ_DT-DY_DT*DZ_DR)
     .         +DX_DT*(DY_DR*DZ_DS-DY_DS*DZ_DR))
        DETM1 = ONE/DETJ0
        DETM1 = EIGHT*DETM1
        INVJ(I,1,1) = (DY_DS*DZ_DT-DZ_DS*DY_DT)*DETM1
        INVJ(I,2,1) = (DZ_DR*DY_DT-DY_DR*DZ_DT)*DETM1
        INVJ(I,3,1) = (DY_DR*DZ_DS-DY_DS*DZ_DR)*DETM1
        INVJ(I,1,2) = (DX_DT*DZ_DS-DX_DS*DZ_DT)*DETM1
        INVJ(I,2,2) = (DX_DR*DZ_DT-DX_DT*DZ_DR)*DETM1
        INVJ(I,3,2) = (DX_DS*DZ_DR-DX_DR*DZ_DS)*DETM1
        INVJ(I,1,3) = (DX_DS*DY_DT-DX_DT*DY_DS)*DETM1
        INVJ(I,2,3) = (DX_DT*DY_DR-DX_DR*DY_DT)*DETM1
        INVJ(I,3,3) = (DX_DR*DY_DS-DX_DS*DY_DR)*DETM1
      ENDDO
      CALL CRTRANS_IMP(SAV,INVJ,
     .             X1, X2, X3, X4, X5, X6, X7, X8,
     .             Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .             Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .             V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,
     .             R, TRM, NEL)
C-----------
C
C-----------
      DO I = LFT,LLT
        X1(I) = ZERO
        Y1(I) = ZERO
        Z1(I) = ZERO
        X2(I) = SAV(I,1)
        Y2(I) = SAV(I,2)
        Z2(I) = SAV(I,3)
        X3(I) = SAV(I,4)
        Y3(I) = SAV(I,5)
        Z3(I) = SAV(I,6)
        X4(I) = SAV(I,7)
        Y4(I) = SAV(I,8)
        Z4(I) = SAV(I,9)
        X5(I) = SAV(I,10)
        Y5(I) = SAV(I,11)
        Z5(I) = SAV(I,12)
        X6(I) = SAV(I,13)
        Y6(I) = SAV(I,14)
        Z6(I) = SAV(I,15)
        X7(I) = SAV(I,16)
        Y7(I) = SAV(I,17)
        Z7(I) = SAV(I,18)
        X8(I) = SAV(I,19)
        Y8(I) = SAV(I,20)
        Z8(I) = SAV(I,21)
      ENDDO
      DO I=LFT,LLT
        IF(OFFG(I)>ONE)THEN
          OFF(I) = OFFG(I)-ONE
        ENDIF
      ENDDO
      DO I=LFT,LLT
        OFF(I) = MIN(ONE,OFFG(I))
      ENDDO
C-----------
      DO J=1,9
        DO I=LFT,LLT
          K11(J,I)=ZERO
          K12(J,I)=ZERO
          K13(J,I)=ZERO
          K14(J,I)=ZERO
          K15(J,I)=ZERO
          K16(J,I)=ZERO
          K17(J,I)=ZERO
          K18(J,I)=ZERO
          K22(J,I)=ZERO
          K23(J,I)=ZERO
          K24(J,I)=ZERO
          K25(J,I)=ZERO
          K26(J,I)=ZERO
          K27(J,I)=ZERO
          K28(J,I)=ZERO
          K33(J,I)=ZERO
          K34(J,I)=ZERO
          K35(J,I)=ZERO
          K36(J,I)=ZERO
          K37(J,I)=ZERO
          K38(J,I)=ZERO
          K44(J,I)=ZERO
          K45(J,I)=ZERO
          K46(J,I)=ZERO
          K47(J,I)=ZERO
          K48(J,I)=ZERO
          K55(J,I)=ZERO
          K56(J,I)=ZERO
          K57(J,I)=ZERO
          K58(J,I)=ZERO
          K66(J,I)=ZERO
          K67(J,I)=ZERO
          K68(J,I)=ZERO
          K77(J,I)=ZERO
          K78(J,I)=ZERO
          K88(J,I)=ZERO
        ENDDO
      ENDDO

      DO J=LFT,LLT
        DO I=1,576
          KL(I,J)=ZERO
        ENDDO
      ENDDO
C-----------
      RETURN
      END
