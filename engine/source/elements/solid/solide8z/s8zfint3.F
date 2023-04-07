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
Chd|  S8ZFINT3                      source/elements/solid/solide8z/s8zfint3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZFINT3(
     1   SIG,     SIGOR,   PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     PX5,     PX6,
     5   PX7,     PX8,     PY5,     PY6,
     6   PY7,     PY8,     PZ5,     PZ6,
     7   PZ7,     PZ8,     PXY1,    PXY2,
     8   PXY3,    PXY4,    PXY5,    PXY6,
     9   PXY7,    PXY8,    PYX1,    PYX2,
     A   PYX3,    PYX4,    PYX5,    PYX6,
     B   PYX7,    PYX8,    PXZ1,    PXZ2,
     C   PXZ3,    PXZ4,    PXZ5,    PXZ6,
     D   PXZ7,    PXZ8,    PZX1,    PZX2,
     E   PZX3,    PZX4,    PZX5,    PZX6,
     F   PZX7,    PZX8,    PYZ1,    PYZ2,
     G   PYZ3,    PYZ4,    PYZ5,    PYZ6,
     H   PYZ7,    PYZ8,    PZY1,    PZY2,
     I   PZY3,    PZY4,    PZY5,    PZY6,
     J   PZY7,    PZY8,    BXY1,    BXY2,
     K   BXY3,    BXY4,    BXY5,    BXY6,
     L   BXY7,    BXY8,    BYX1,    BYX2,
     M   BYX3,    BYX4,    BYX5,    BYX6,
     N   BYX7,    BYX8,    BXZ1,    BXZ2,
     O   BXZ3,    BXZ4,    BXZ5,    BXZ6,
     P   BXZ7,    BXZ8,    BZX1,    BZX2,
     Q   BZX3,    BZX4,    BZX5,    BZX6,
     R   BZX7,    BZX8,    BYZ1,    BYZ2,
     S   BYZ3,    BYZ4,    BYZ5,    BYZ6,
     T   BYZ7,    BYZ8,    BZY1,    BZY2,
     U   BZY3,    BZY4,    BZY5,    BZY6,
     V   BZY7,    BZY8,    F11,     F21,
     W   F31,     F12,     F22,     F32,
     X   F13,     F23,     F33,     F14,
     Y   F24,     F34,     F15,     F25,
     Z   F35,     F16,     F26,     F36,
     1   F17,     F27,     F37,     F18,
     2   F28,     F38,     VOL,     QVIS,
     3   PP,      EINT,    RHO,     Q,
     4   DEFP,    EPSD,    EPSDM,   SIGM,
     5   EINTM,   RHOM,    QM,      DEFPM,
     6   VOLG,    STI,     STIN,    ICP,
     7   OFF,     VOL0,    VOL0G,   G_PLA,
     8   G_EPSD,  JFAC,    EINTTH,  EINTTHM,
     9   IEXPAN,  NEL,     IDEG,    CONDE,
     A   CONDEN,  MTN,     ISMSTR)
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
#include      "nsvis_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER ICP,G_PLA,G_EPSD,IEXPAN,NEL,IDEG(*)
C     REAL
      my_real
     .   SIG(NEL,6),SIGOR(NEL,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX5(*), PX6(*), PX7(*), PX8(*),  
     .   PY5(*), PY6(*), PY7(*), PY8(*),  
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),  
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),QVIS(*),PP(*),
     .   EINT(*),RHO(*),Q(*),DEFPM(*),DEFP(*),
     .   SIGM(NEL,6),EINTM(*),RHOM(*),QM(*),EPSD(*),EPSDM(*),
     .   VOLG(*),STI(*),STIN(*),OFF(*),VOL0(*),VOL0G(*),JFAC(*),
     .   EINTTH(*),EINTTHM(*),CONDE(MVSIZ),CONDEN(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   P(MVSIZ),FAC(MVSIZ),COEF,FVOL,
     .   QVIS_LOC,VOL_LOC
C-----------------------------------------------
      IF (ICP==1.AND.(ISMSTR==10.OR.ISMSTR==12)) THEN
       DO I=1,NEL
        QVIS_LOC = QVIS(I)
        FVOL=JFAC(I)*VOL(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS_LOC)*FVOL
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS_LOC)*FVOL
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS_LOC)*FVOL
        S4(I)=(SIG(I,4)+SVIS(I,4))*FVOL
        S5(I)=(SIG(I,5)+SVIS(I,5))*FVOL
        S6(I)=(SIG(I,6)+SVIS(I,6))*FVOL
       ENDDO
      ELSEIF (ICP==1) THEN
       COEF=THIRD
C------due to degratation of rubber test, will definitively fix w/ real Kt       
       IF (MTN==42.OR.MTN==62.OR.MTN==82)COEF=ZEP3
       DO I=1,NEL
        VOL_LOC = VOL(I)
        P(I) =COEF*(SIG(I,1)+SIG(I,2)+SIG(I,3)
     .            +SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
        IF (IDEG(I)>10) P(I) =QVIS(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-P(I))*VOL_LOC
        S2(I)=(SIG(I,2)+SVIS(I,2)-P(I))*VOL_LOC
        S3(I)=(SIG(I,3)+SVIS(I,3)-P(I))*VOL_LOC
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC
       ENDDO
      ELSE
       DO I=1,NEL
        QVIS_LOC = QVIS(I)
        VOL_LOC = VOL(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS_LOC)*VOL_LOC
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS_LOC)*VOL_LOC
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS_LOC)*VOL_LOC
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC
       ENDDO
      ENDIF
       DO I=1,NEL
        F11(I)=F11(I)-(S1(I)*PX1(I)+S4(I)*PXY1(I)+S6(I)*PXZ1(I))
        F21(I)=F21(I)-(S2(I)*PY1(I)+S4(I)*PYX1(I)+S5(I)*PYZ1(I))
        F31(I)=F31(I)-(S3(I)*PZ1(I)+S6(I)*PZX1(I)+S5(I)*PZY1(I))
        F12(I)=F12(I)-(S1(I)*PX2(I)+S4(I)*PXY2(I)+S6(I)*PXZ2(I))
        F22(I)=F22(I)-(S2(I)*PY2(I)+S4(I)*PYX2(I)+S5(I)*PYZ2(I))
        F32(I)=F32(I)-(S3(I)*PZ2(I)+S6(I)*PZX2(I)+S5(I)*PZY2(I))
        F13(I)=F13(I)-(S1(I)*PX3(I)+S4(I)*PXY3(I)+S6(I)*PXZ3(I))
        F23(I)=F23(I)-(S2(I)*PY3(I)+S4(I)*PYX3(I)+S5(I)*PYZ3(I))
        F33(I)=F33(I)-(S3(I)*PZ3(I)+S6(I)*PZX3(I)+S5(I)*PZY3(I))
        F14(I)=F14(I)-(S1(I)*PX4(I)+S4(I)*PXY4(I)+S6(I)*PXZ4(I))
        F24(I)=F24(I)-(S2(I)*PY4(I)+S4(I)*PYX4(I)+S5(I)*PYZ4(I))
        F34(I)=F34(I)-(S3(I)*PZ4(I)+S6(I)*PZX4(I)+S5(I)*PZY4(I))
        F15(I)=F15(I)-(S1(I)*PX5(I)+S4(I)*PXY5(I)+S6(I)*PXZ5(I))
        F25(I)=F25(I)-(S2(I)*PY5(I)+S4(I)*PYX5(I)+S5(I)*PYZ5(I))
        F35(I)=F35(I)-(S3(I)*PZ5(I)+S6(I)*PZX5(I)+S5(I)*PZY5(I))
        F16(I)=F16(I)-(S1(I)*PX6(I)+S4(I)*PXY6(I)+S6(I)*PXZ6(I))
        F26(I)=F26(I)-(S2(I)*PY6(I)+S4(I)*PYX6(I)+S5(I)*PYZ6(I))
        F36(I)=F36(I)-(S3(I)*PZ6(I)+S6(I)*PZX6(I)+S5(I)*PZY6(I))
        F17(I)=F17(I)-(S1(I)*PX7(I)+S4(I)*PXY7(I)+S6(I)*PXZ7(I))
        F27(I)=F27(I)-(S2(I)*PY7(I)+S4(I)*PYX7(I)+S5(I)*PYZ7(I))
        F37(I)=F37(I)-(S3(I)*PZ7(I)+S6(I)*PZX7(I)+S5(I)*PZY7(I))
        F18(I)=F18(I)-(S1(I)*PX8(I)+S4(I)*PXY8(I)+S6(I)*PXZ8(I))
        F28(I)=F28(I)-(S2(I)*PY8(I)+S4(I)*PYX8(I)+S5(I)*PYZ8(I))
        F38(I)=F38(I)-(S3(I)*PZ8(I)+S6(I)*PZX8(I)+S5(I)*PZY8(I))
       ENDDO
C  
       IF (ICP/=1) THEN
        DO I=1,NEL
         F11(I)=F11(I)-(S2(I)*BXY1(I)+S3(I)*BXZ1(I))
         F21(I)=F21(I)-(S1(I)*BYX1(I)+S3(I)*BYZ1(I))
         F31(I)=F31(I)-(S1(I)*BZX1(I)+S2(I)*BZY1(I))
         F12(I)=F12(I)-(S2(I)*BXY2(I)+S3(I)*BXZ2(I))
         F22(I)=F22(I)-(S1(I)*BYX2(I)+S3(I)*BYZ2(I))
         F32(I)=F32(I)-(S1(I)*BZX2(I)+S2(I)*BZY2(I))
         F13(I)=F13(I)-(S2(I)*BXY3(I)+S3(I)*BXZ3(I))
         F23(I)=F23(I)-(S1(I)*BYX3(I)+S3(I)*BYZ3(I))
         F33(I)=F33(I)-(S1(I)*BZX3(I)+S2(I)*BZY3(I))
         F14(I)=F14(I)-(S2(I)*BXY4(I)+S3(I)*BXZ4(I))
         F24(I)=F24(I)-(S1(I)*BYX4(I)+S3(I)*BYZ4(I))
         F34(I)=F34(I)-(S1(I)*BZX4(I)+S2(I)*BZY4(I))
         F15(I)=F15(I)-(S2(I)*BXY5(I)+S3(I)*BXZ5(I))
         F25(I)=F25(I)-(S1(I)*BYX5(I)+S3(I)*BYZ5(I))
         F35(I)=F35(I)-(S1(I)*BZX5(I)+S2(I)*BZY5(I))
         F16(I)=F16(I)-(S2(I)*BXY6(I)+S3(I)*BXZ6(I))
         F26(I)=F26(I)-(S1(I)*BYX6(I)+S3(I)*BYZ6(I))
         F36(I)=F36(I)-(S1(I)*BZX6(I)+S2(I)*BZY6(I))
         F17(I)=F17(I)-(S2(I)*BXY7(I)+S3(I)*BXZ7(I))
         F27(I)=F27(I)-(S1(I)*BYX7(I)+S3(I)*BYZ7(I))
         F37(I)=F37(I)-(S1(I)*BZX7(I)+S2(I)*BZY7(I))
         F18(I)=F18(I)-(S2(I)*BXY8(I)+S3(I)*BXZ8(I))
         F28(I)=F28(I)-(S1(I)*BYX8(I)+S3(I)*BYZ8(I))
         F38(I)=F38(I)-(S1(I)*BZX8(I)+S2(I)*BZY8(I))
        ENDDO
       ENDIF
C----------------------------------------------    /
C   - post-traitement-valeur moyenne au sens a'=(_/  a dv ) /v
      DO I=1,NEL
         FAC(I) = OFF(I)*VOL(I)/VOLG(I)
         SIGM(I,1) = SIGM(I,1) + FAC(I) * SIGOR(I,1)
         SIGM(I,2) = SIGM(I,2) + FAC(I) * SIGOR(I,2)
         SIGM(I,3) = SIGM(I,3) + FAC(I) * SIGOR(I,3)
         SIGM(I,4) = SIGM(I,4) + FAC(I) * SIGOR(I,4)
         SIGM(I,5) = SIGM(I,5) + FAC(I) * SIGOR(I,5)
         SIGM(I,6) = SIGM(I,6) + FAC(I) * SIGOR(I,6)
         RHOM(I)   = RHOM(I)   + FAC(I) * RHO(I)
         EINTM(I)  = EINTM(I)  + EINT(I)*VOL0(I)/VOL0G(I)
         QM(I)     = QM(I)     + FAC(I) * Q(I)
C
         STIN(I)   = STIN(I)   + STI(I)
      ENDDO
      IF(NODADT_THERM == 1) THEN
        DO I=1,NEL
          CONDEN(I)= CONDEN(I)+ CONDE(I)
        ENDDO
      ENDIF
      IF (IEXPAN > 0) THEN
        DO I=1,NEL
          EINTTHM(I)= EINTTHM(I)+ EINTTH(I)*VOL0(I)/VOL0G(I)
        ENDDO
      ENDIF
      IF (G_PLA > 0) THEN
        DO I=1,NEL
          DEFPM(I)  = DEFPM(I)  + FAC(I) * DEFP(I)
        ENDDO
      ENDIF
      IF (G_EPSD > 0) THEN
        DO I=1,NEL
          EPSDM(I) = EPSDM(I) + FAC(I) * EPSD(I)
        ENDDO
      ENDIF
      IF (ICP == 1) THEN
        DO I=1,NEL
          PP(I) = PP(I) + FAC(I)* (P(I)-QVIS(I))
        ENDDO
      ENDIF
c-----------
      RETURN
      END
