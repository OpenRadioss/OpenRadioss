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
Chd|  S8ZDERIPR3                    source/elements/solid/solide8z/s8zderipr3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZDERIPR3(
     1   OFFG,    VOLDP,   NGL,     KSI,
     2   ETA,     ZETA,    WI,      CJ1,
     3   CJ2,     CJ3,     CJ4,     CJ5,
     4   CJ6,     CJ7,     CJ8,     CJ9,
     5   HX,      HY,      HZ,      JAC1,
     6   JAC2,    JAC3,    JAC4,    JAC5,
     7   JAC6,    JACI1,   JACI2,   JACI3,
     8   JACI4,   JACI5,   JACI6,   JACI7,
     9   JACI8,   JACI9,   NNEGA,   INDEX,
     A   IPT,     NEL)
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NNEGA,INDEX(*),IPT
C     REAL
      my_real
     .   OFFG(*),KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC1(*),JAC2(*),JAC3(*),
     .   JAC4(*),JAC5(*),JAC6(*),
     .   JACI1(*),JACI2(*),JACI3(*),
     .   JACI4(*),JACI5(*),JACI6(*),
     .   JACI7(*),JACI8(*),JACI9(*)
      DOUBLE PRECISION 
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT ,
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   JAC7,JAC8,JAC9,VOL(MVSIZ)
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
      DO I=1,NEL
       JAC1(I)=CJ1(I)+HX(I,3)*ETA+(HX(I,2)+HX(I,4)*ETA)*ZETA
       JAC2(I)=CJ2(I)+HY(I,3)*ETA+(HY(I,2)+HY(I,4)*ETA)*ZETA
       JAC3(I)=CJ3(I)+HZ(I,3)*ETA+(HZ(I,2)+HZ(I,4)*ETA)*ZETA
C
       JAC4(I)=CJ4(I)+HX(I,1)*ZETA+(HX(I,3)+HX(I,4)*ZETA)*KSI
       JAC5(I)=CJ5(I)+HY(I,1)*ZETA+(HY(I,3)+HY(I,4)*ZETA)*KSI
       JAC6(I)=CJ6(I)+HZ(I,1)*ZETA+(HZ(I,3)+HZ(I,4)*ZETA)*KSI
C
       JAC7=CJ7(I)+HX(I,2)*KSI+(HX(I,1)+HX(I,4)*KSI)*ETA
       JAC8=CJ8(I)+HY(I,2)*KSI+(HY(I,1)+HY(I,4)*KSI)*ETA
       JAC9=CJ9(I)+HZ(I,2)*KSI+(HZ(I,1)+HZ(I,4)*KSI)*ETA
C
C     JACOBIAN
C
       JAC_59_68(I)=JAC5(I)*JAC9-JAC6(I)*JAC8
       JAC_67_49(I)=JAC6(I)*JAC7-JAC4(I)*JAC9
       JAC_38_29(I)=(-JAC2(I)*JAC9+JAC3(I)*JAC8)
       JAC_19_37(I)=( JAC1(I)*JAC9-JAC3(I)*JAC7)
       JAC_27_18(I)=(-JAC1(I)*JAC8+JAC2(I)*JAC7)
       JAC_26_35(I)=( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
       JAC_34_16(I)=(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
       JAC_15_24(I)=( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
       JAC_48_57(I)=JAC4(I)*JAC8-JAC5(I)*JAC7
      ENDDO
C
      DO I=1,NEL
      DETDP=ONE_OVER_512*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      DET(I) = DETDP
      VOLDP(I) = WI * DETDP
      VOL(I) = VOLDP(I)
      ENDDO
C
      ICOR = 0
      DO I=1,NEL
        IF(OFFG(I)==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) VOL(I)=ONE
         IF (VOLDP(I)<=ZERO) VOLDP(I)=ONE
        ELSEIF (VOL(I)<=ZERO ) THEN
          ICOR=1
        ENDIF
      ENDDO
      IF (ICOR>0.AND.INCONV==1) THEN
       DO I=1,NEL
       IF (OFFG(I) /= TWO.AND.OFFG(I) /= ZERO ) THEN
         NNEGA=NNEGA+1
        INDEX(NNEGA)=I
        OFFG(I) = TWO
       END IF
       ENDDO
      END IF
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=1,NEL
          IF(VOL(I)<=ZERO)THEN
            VOL(I)= EM20
            VOLDP(I) = EM20
            DET(I)= EM20
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
      DO I=1,NEL
       DETT=ONE_OVER_512/DET(I)
       JACI1(I)=DETT*JAC_59_68(I)
       JACI4(I)=DETT*JAC_67_49(I)
       JACI7(I)=DETT*JAC_48_57(I)
       JACI2(I)=DETT*JAC_38_29(I)
       JACI5(I)=DETT*JAC_19_37(I)
       JACI8(I)=DETT*JAC_27_18(I)
       JACI3(I)=DETT*JAC_26_35(I)
       JACI6(I)=DETT*JAC_34_16(I)
       JACI9(I)=DETT*JAC_15_24(I)
      ENDDO
C
      RETURN
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
