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
Chd|  S8EDERIPR3                    source/elements/solid/solide8e/s8ederipr3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERIPR3(
     1   OFFG,    VOLDP,   NGL,     WI,
     2   AJ1,     AJ2,     AJ3,     AJ4,
     3   AJ5,     AJ6,     AJ7,     AJ8,
     4   AJ9,     AJI1,    AJI2,    AJI3,
     5   AJI4,    AJI5,    AJI6,    AJI7,
     6   AJI8,    AJI9,    NNEGA,   INDEX,
     7   IPT,     NEL)
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
     .   OFFG(*),WI,
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   AJI1(*), AJI2(*), AJI3(*),
     .   AJI4(*), AJI5(*), AJI6(*),
     .   AJI7(*), AJI8(*), AJI9(*)
      DOUBLE PRECISION 
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   VOL(MVSIZ),DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ)
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
C
      DO I=1,NEL
       JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
       JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
       JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
       JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
       JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
       JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
       JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
       JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
       JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=1,NEL
      DETDP=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DET(I)=DETDP
      VOLDP(I)= WI*DETDP
      VOL(I)= VOLDP(I)
      ENDDO
C
      ICOR = 0
      DO I=1,NEL
        IF(OFFG(I)==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) THEN
           VOL(I)=ONE
           VOLDP(I)= ONE
         END IF
        ELSEIF (VOL(I)<=ZERO ) THEN
          ICOR=1
        ENDIF
      ENDDO
      IF (ICOR>0.AND.INCONV==1) THEN
       DO I=1,NEL
        IF (OFFG(I) /= TWO .AND.OFFG(I) /= ZERO ) THEN
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
            VOLDP(I)= EM20
            DET(I)= EM20
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV==1.AND.ABS(OFFG(I))/=TWO) THEN
c#include "lockon.inc"
c            WRITE(ISTDO,2000) NGL(I)
c            WRITE(IOUT ,2000) NGL(I)
c#include "lockoff.inc"
c            IDEL7NOK = 1   
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
C
      DO I=1,NEL
       DETT(I)=ONE_OVER_512/DET(I)
       AJI1(I)=DETT(I)*JAC_59_68(I)
       AJI4(I)=DETT(I)*JAC_67_49(I)
       AJI7(I)=DETT(I)*JAC_48_57(I)
       AJI2(I)=DETT(I)*JAC_38_29(I)
       AJI5(I)=DETT(I)*JAC_19_37(I)
       AJI8(I)=DETT(I)*JAC_27_18(I)
       AJI3(I)=DETT(I)*JAC_26_35(I)
       AJI6(I)=DETT(I)*JAC_34_16(I)
       AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
