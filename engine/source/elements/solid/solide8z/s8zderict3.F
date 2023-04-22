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
Chd|  S8ZDERICT3                    source/elements/solid/solide8z/s8zderict3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SCHKJAB3                      source/elements/solid/solide/schkjab3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERICT3(
     1   OFF,     DET,     NGL,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      PX1,
     8   PX2,     PX3,     PX4,     PY1,
     9   PY2,     PY3,     PY4,     PZ1,
     A   PZ2,     PZ3,     PZ4,     PX1H1,
     B   PX1H2,   PX1H3,   PX1H4,   PX2H1,
     C   PX2H2,   PX2H3,   PX2H4,   PX3H1,
     D   PX3H2,   PX3H3,   PX3H4,   PX4H1,
     E   PX4H2,   PX4H3,   PX4H4,   HX,
     F   HY,      HZ,      JAC1,    JAC2,
     G   JAC3,    JAC4,    JAC5,    JAC6,
     H   JAC7,    JAC8,    JAC9,    SMAX,
     I   NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
      INTEGER, INTENT(IN) :: NEL
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C     REAL
      my_real
     .   OFF(*),DET(*), 
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JAC7(*),JAC8(*),JAC9(*),SMAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT ,   
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35
C-----------------------------------------------
C
      DO I=1,NEL
        X17=X7(I)-X1(I)
        X28=X8(I)-X2(I)
        X35=X5(I)-X3(I)
        X46=X6(I)-X4(I)
        Y17=Y7(I)-Y1(I)
        Y28=Y8(I)-Y2(I)
        Y35=Y5(I)-Y3(I)
        Y46=Y6(I)-Y4(I)
        Z17=Z7(I)-Z1(I)
        Z28=Z8(I)-Z2(I)
        Z35=Z5(I)-Z3(I)
        Z46=Z6(I)-Z4(I)
C
C Jacobian matrix
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
      JAC_38_29(I)=(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
      JAC_19_37(I)=( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
      JAC_27_18(I)=(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
      JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
      JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
      JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      END DO
C
      DO I=1,NEL
      DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      ENDDO
C
      CALL SCHKJAB3(
     1   OFF,     DET,     NGL,     NEL)
C
C Jacobian matrix inverse
C
      DO I=1,NEL
        DETT=ONE_OVER_64/DET(I)
        JACI1=DETT*JAC_59_68(I)
        JACI4=DETT*JAC_67_49(I)
        JACI7=DETT*JAC_48_57(I)
        JACI2=DETT*JAC_38_29(I)
        JACI5=DETT*JAC_19_37(I)
        JACI8=DETT*JAC_27_18(I)
        JACI3=DETT*JAC_26_35(I)
        JACI6=DETT*JAC_34_16(I)
        JACI9=DETT*JAC_15_24(I)
!
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
!
        PX2(I)= JACI12-JACI3
        PY2(I)= JACI45-JACI6
        PZ2(I)= JACI78-JACI9
        PX4(I)=-JACI12-JACI3
        PY4(I)=-JACI45-JACI6
        PZ4(I)=-JACI78-JACI9
!
        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8
!
        PX1(I)=-JACI12-JACI3
        PY1(I)=-JACI45-JACI6
        PZ1(I)=-JACI78-JACI9
        PX3(I)=JACI12-JACI3
        PY3(I)=JACI45-JACI6
        PZ3(I)=JACI78-JACI9
      ENDDO
C
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H1(I)=PX1(I)*HX(I,1)+ PY1(I)*HY(I,1)+PZ1(I)*HZ(I,1)
         PX2H1(I)=PX2(I)*HX(I,1)+ PY2(I)*HY(I,1)+PZ2(I)*HZ(I,1)
         PX3H1(I)=PX3(I)*HX(I,1)+ PY3(I)*HY(I,1)+PZ3(I)*HZ(I,1)
         PX4H1(I)=PX4(I)*HX(I,1)+ PY4(I)*HY(I,1)+PZ4(I)*HZ(I,1)
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H2(I)=PX1(I)*HX(I,2)+ PY1(I)*HY(I,2)+PZ1(I)*HZ(I,2)
         PX2H2(I)=PX2(I)*HX(I,2)+ PY2(I)*HY(I,2)+PZ2(I)*HZ(I,2)
         PX3H2(I)=PX3(I)*HX(I,2)+ PY3(I)*HY(I,2)+PZ3(I)*HZ(I,2)
         PX4H2(I)=PX4(I)*HX(I,2)+ PY4(I)*HY(I,2)+PZ4(I)*HZ(I,2)
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=1,NEL
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX(I,3)+ PY1(I)*HY(I,3)+PZ1(I)*HZ(I,3)
         PX2H3(I)=PX2(I)*HX(I,3)+ PY2(I)*HY(I,3)+PZ2(I)*HZ(I,3)
         PX3H3(I)=PX3(I)*HX(I,3)+ PY3(I)*HY(I,3)+PZ3(I)*HZ(I,3)
         PX4H3(I)=PX4(I)*HX(I,3)+ PY4(I)*HY(I,3)+PZ4(I)*HZ(I,3)
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H4(I)=PX1(I)*HX(I,4)+ PY1(I)*HY(I,4)+PZ1(I)*HZ(I,4)
         PX2H4(I)=PX2(I)*HX(I,4)+ PY2(I)*HY(I,4)+PZ2(I)*HZ(I,4)
         PX3H4(I)=PX3(I)*HX(I,4)+ PY3(I)*HY(I,4)+PZ3(I)*HZ(I,4)
         PX4H4(I)=PX4(I)*HX(I,4)+ PY4(I)*HY(I,4)+PZ4(I)*HZ(I,4)
       ENDDO
C----surface max mediane-- *16
       DO I=1,NEL
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
       DO I=1,NEL
         IF(SMAX(I)<=ZERO)THEN
           CALL ANCMSG(MSGID=173,ANMODE=ANINFO,
     .                 I1=NGL(I))
           CALL ARRET(2)
         ENDIF
         SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END      
