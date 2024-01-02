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
Chd|  SRCOORK                       source/elements/solid/solide8z/srcoork.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        SCORTHO3                      source/elements/thickshell/solidec/scortho3.F
Chd|        SORTHDIR17                    source/elements/solid/solide8z/sorthdir17.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREPISOT3                     source/elements/solid/solide/srepisot3.F
Chd|====================================================================
      SUBROUTINE SRCOORK(
     1   X,       IXS,     X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      OFFG,    OFF,
     8   SAV,     R11,     R12,     R13,
     9   R21,     R22,     R23,     R31,
     A   R32,     R33,     NC1,     NC2,
     B   NC3,     NC4,     NC5,     NC6,
     C   NC7,     NC8,     NGL,     MXT,
     D   NGEO,    K11,     K12,     K13,
     E   K14,     K15,     K16,     K17,
     F   K18,     K22,     K23,     K24,
     G   K25,     K26,     K27,     K28,
     H   K33,     K34,     K35,     K36,
     I   K37,     K38,     K44,     K45,
     J   K46,     K47,     K48,     K55,
     K   K56,     K57,     K58,     K66,
     L   K67,     K68,     K77,     K78,
     M   K88,     KHBE,    GAMA0,   GAMA,
     N   NEL,     ISMSTR,  JHBE,    JCVT,
     O   IREP,    IGTYP,   ISORTH)
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
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: IREP
      INTEGER, INTENT(IN) :: IGTYP
      INTEGER, INTENT(IN) :: ISORTH
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   OFFG(*), OFF(*), SAV(NEL,21), GAMA0(NEL,6),GAMA(MVSIZ,6),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
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
C-----------------------------------------------
C
      MXT_1 = IXS(1,1)
      DO I=1,NEL
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
      DO I=1,NEL
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
C     REPERE CONVECTE (ITERATIONS).
C-----------
      CALL SREPISOT3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      X7,      X8,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Y7,      Y8,
     5   Z1,      Z2,      Z3,      Z4,
     6   Z5,      Z6,      Z7,      Z8,
     7   RX,      RY,      RZ,      SX,
     8   SY,      SZ,      TX,      TY,
     9   TZ,      NEL)
      IF (KHBE==15) THEN
       CALL SCORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     NEL)
      ELSEIF (KHBE==1 .OR. KHBE==2. OR. KHBE == 12 .OR. KHBE==14 .OR.
     .    KHBE==17.OR .KHBE==24) THEN
       CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R12,     R13,     R11,
     4   R22,     R23,     R21,     R32,
     5   R33,     R31,     NEL)
      ELSE
       CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     NEL)
      ENDIF
C-------sauf thick shells --------------     
      IF (IGTYP /= 21 .AND. IGTYP /= 22) THEN
        IF (ISORTH == 0) THEN
          DO I=1,NEL                                          
            GAMA(I,1) = ONE                             
            GAMA(I,2) = ZERO                              
            GAMA(I,3) = ZERO
            GAMA(I,4) = ZERO                              
            GAMA(I,5) = ONE                              
            GAMA(I,6) = ZERO
          ENDDO      
        ELSE    
          CALL SORTHDIR3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     GAMA0,   GAMA,
     6   NEL,     IREP)
         IF (KHBE==17) THEN
          CALL SORTHDIR17(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     GAMA,    NEL,
     6   JCVT)
         END IF 
        ENDIF
      ENDIF
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
C     X=RX' <=> X'=t(R)X chgt de base.
      IF((ISMSTR==1.OR.ISMSTR==3).OR.
     .   (ISMSTR==2.AND.IDTMIN(1)==3))THEN
       IF (JHBE==14.OR.JHBE==24.OR.JHBE==15.OR.JHBE==222) THEN
       DO I=1,NEL
       IF(OFFG(I)>ONE)THEN
        X1(I)=SAV(I,1)
        Y1(I)=SAV(I,2)
        Z1(I)=SAV(I,3)
        X2(I)=SAV(I,4)
        Y2(I)=SAV(I,5)
        Z2(I)=SAV(I,6)
        X3(I)=SAV(I,7)
        Y3(I)=SAV(I,8)
        Z3(I)=SAV(I,9)
        X4(I)=SAV(I,10)
        Y4(I)=SAV(I,11)
        Z4(I)=SAV(I,12)
        X5(I)=SAV(I,13)
        Y5(I)=SAV(I,14)
        Z5(I)=SAV(I,15)
        X6(I)=SAV(I,16)
        Y6(I)=SAV(I,17)
        Z6(I)=SAV(I,18)
        X7(I)=SAV(I,19)
        Y7(I)=SAV(I,20)
        Z7(I)=SAV(I,21)
        X8(I)=ZERO
        Y8(I)=ZERO
        Z8(I)=ZERO
        OFF(I) = OFFG(I)-ONE
       ELSE
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=ZL
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=ZL
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=ZL
        XL=R11(I)*X7(I)+R21(I)*Y7(I)+R31(I)*Z7(I)
        YL=R12(I)*X7(I)+R22(I)*Y7(I)+R32(I)*Z7(I)
        ZL=R13(I)*X7(I)+R23(I)*Y7(I)+R33(I)*Z7(I)
        X7(I)=XL
        Y7(I)=YL
        Z7(I)=ZL
        XL=R11(I)*X8(I)+R21(I)*Y8(I)+R31(I)*Z8(I)
        YL=R12(I)*X8(I)+R22(I)*Y8(I)+R32(I)*Z8(I)
        ZL=R13(I)*X8(I)+R23(I)*Y8(I)+R33(I)*Z8(I)
        X8(I)=XL
        Y8(I)=YL
        Z8(I)=ZL
C       SAV dans rep. local.
C       SAV(I,1)=X1(I)-X8(I)
C       SAV(I,2)=Y1(I)-Y8(I)
C       SAV(I,3)=Z1(I)-Z8(I)
C       SAV(I,4)=X2(I)-X8(I)
C       SAV(I,5)=Y2(I)-Y8(I)
C       SAV(I,6)=Z2(I)-Z8(I)
C       SAV(I,7)=X3(I)-X8(I)
C       SAV(I,8)=Y3(I)-Y8(I)
C       SAV(I,9)=Z3(I)-Z8(I)
C       SAV(I,10)=X4(I)-X8(I)
C       SAV(I,11)=Y4(I)-Y8(I)
C       SAV(I,12)=Z4(I)-Z8(I)
C       SAV(I,13)=X5(I)-X8(I)
C       SAV(I,14)=Y5(I)-Y8(I)
C       SAV(I,15)=Z5(I)-Z8(I)
C       SAV(I,16)=X6(I)-X8(I)
C       SAV(I,17)=Y6(I)-Y8(I)
C       SAV(I,18)=Z6(I)-Z8(I)
C       SAV(I,19)=X7(I)-X8(I)
C       SAV(I,20)=Y7(I)-Y8(I)
C       SAV(I,21)=Z7(I)-Z8(I)
        OFF(I) = OFFG(I)
       ENDIF
       ENDDO
C------due to different initial systems and [K] is calculated using always HA8 system      
       ELSE
        DO I=1,NEL
        IF(OFFG(I)>ONE)THEN
         X1(I)=SAV(I,1)
         Y1(I)=SAV(I,2)
         Z1(I)=SAV(I,3)
         X2(I)=SAV(I,4)
         Y2(I)=SAV(I,5)
         Z2(I)=SAV(I,6)
         X3(I)=SAV(I,7)
         Y3(I)=SAV(I,8)
         Z3(I)=SAV(I,9)
         X4(I)=SAV(I,10)
         Y4(I)=SAV(I,11)
         Z4(I)=SAV(I,12)
         X5(I)=SAV(I,13)
         Y5(I)=SAV(I,14)
         Z5(I)=SAV(I,15)
         X6(I)=SAV(I,16)
         Y6(I)=SAV(I,17)
         Z6(I)=SAV(I,18)
         X7(I)=SAV(I,19)
         Y7(I)=SAV(I,20)
         Z7(I)=SAV(I,21)
         X8(I)=ZERO
         Y8(I)=ZERO
         Z8(I)=ZERO
         OFF(I) = OFFG(I)-ONE
        ENDIF
        ENDDO
        IF (JCVT==0) THEN
        DO I=1,NEL
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=ZL
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=ZL
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=ZL
        XL=R11(I)*X7(I)+R21(I)*Y7(I)+R31(I)*Z7(I)
        YL=R12(I)*X7(I)+R22(I)*Y7(I)+R32(I)*Z7(I)
        ZL=R13(I)*X7(I)+R23(I)*Y7(I)+R33(I)*Z7(I)
        X7(I)=XL
        Y7(I)=YL
        Z7(I)=ZL
        XL=R11(I)*X8(I)+R21(I)*Y8(I)+R31(I)*Z8(I)
        YL=R12(I)*X8(I)+R22(I)*Y8(I)+R32(I)*Z8(I)
        ZL=R13(I)*X8(I)+R23(I)*Y8(I)+R33(I)*Z8(I)
        X8(I)=XL
        Y8(I)=YL
        Z8(I)=ZL
        ENDDO
        ELSE
        DO I=1,NEL
        XL=Z1(I)
        YL=X1(I)
        ZL=Y1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=Z2(I)
        YL=X2(I)
        ZL=Y2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=Z3(I)
        YL=X3(I)
        ZL=Y3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=Z4(I)
        YL=X4(I)
        ZL=Y4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=ZL
        XL=Z5(I)
        YL=X5(I)
        ZL=Y5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=ZL
        XL=Z6(I)
        YL=X6(I)
        ZL=Y6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=ZL
        XL=Z7(I)
        YL=X7(I)
        ZL=Y7(I)
        X7(I)=XL
        Y7(I)=YL
        Z7(I)=ZL
        XL=Z8(I)
        YL=X8(I)
        ZL=Y8(I)
        X8(I)=XL
        Y8(I)=YL
        Z8(I)=ZL
        ENDDO
        ENDIF !IF (JCVT==0)
        DO I=1,NEL
        IF(OFFG(I)<=ONE)THEN
C       SAV dans rep. local.
        SAV(I,1)=X1(I)-X8(I)
        SAV(I,2)=Y1(I)-Y8(I)
        SAV(I,3)=Z1(I)-Z8(I)
        SAV(I,4)=X2(I)-X8(I)
        SAV(I,5)=Y2(I)-Y8(I)
        SAV(I,6)=Z2(I)-Z8(I)
        SAV(I,7)=X3(I)-X8(I)
        SAV(I,8)=Y3(I)-Y8(I)
        SAV(I,9)=Z3(I)-Z8(I)
        SAV(I,10)=X4(I)-X8(I)
        SAV(I,11)=Y4(I)-Y8(I)
        SAV(I,12)=Z4(I)-Z8(I)
        SAV(I,13)=X5(I)-X8(I)
        SAV(I,14)=Y5(I)-Y8(I)
        SAV(I,15)=Z5(I)-Z8(I)
        SAV(I,16)=X6(I)-X8(I)
        SAV(I,17)=Y6(I)-Y8(I)
        SAV(I,18)=Z6(I)-Z8(I)
        SAV(I,19)=X7(I)-X8(I)
        SAV(I,20)=Y7(I)-Y8(I)
        SAV(I,21)=Z7(I)-Z8(I)
        OFF(I) = OFFG(I)
        ENDIF
        ENDDO
       ENDIF !IF (JHBE==14.OR.JHBE==24...)
C
      ELSE
       DO I=1,NEL
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=ZL
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=ZL
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=ZL
        XL=R11(I)*X7(I)+R21(I)*Y7(I)+R31(I)*Z7(I)
        YL=R12(I)*X7(I)+R22(I)*Y7(I)+R32(I)*Z7(I)
        ZL=R13(I)*X7(I)+R23(I)*Y7(I)+R33(I)*Z7(I)
        X7(I)=XL
        Y7(I)=YL
        Z7(I)=ZL
        XL=R11(I)*X8(I)+R21(I)*Y8(I)+R31(I)*Z8(I)
        YL=R12(I)*X8(I)+R22(I)*Y8(I)+R32(I)*Z8(I)
        ZL=R13(I)*X8(I)+R23(I)*Y8(I)+R33(I)*Z8(I)
        X8(I)=XL
        Y8(I)=YL
        Z8(I)=ZL
        OFF(I) = MIN(ONE,OFFG(I))
       ENDDO
C
       ENDIF
C-----------
      DO J=1,9      
      DO I=1,NEL
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
C-----------
      RETURN
      END
