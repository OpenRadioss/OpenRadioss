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
Chd|  RBILAN33                      source/elements/joint/rbilan33.F
Chd|-- called by -----------
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|====================================================================
      SUBROUTINE RBILAN33(
     1   JFT,     JLT,     EINT,    PARTSAV,
     2   UMAS,    V,       IPARTR,  GRESAV,
     3   GRTH,    IGRTH,   NC1,     NC2,
     4   IGRE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IGRE
      INTEGER JFT,JLT, IPARTR(*),GRTH(*),IGRTH(*),NC1(*),NC2(*)
      my_real UMAS(*),EINT(*),PARTSAV(NPSAV,*),V(3,*),GRESAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX, FLAG
      my_real
     .   VXA, VYA, VZA, XMAS2, VA2, REINTT, 
     .   EK(MVSIZ), XM(MVSIZ), YM(MVSIZ), ZM(MVSIZ),
     .   RBIDON(1),OFF(MVSIZ)
C-----------------------------------------------
      FLAG = 0
      RBIDON = ZERO
      OFF    = ZERO
      REINTT = ZERO
      DO I=JFT,JLT
        REINTT=REINTT + EINT(I)
      ENDDO
C
#include "atomic.inc"
        REINT = REINT + REINTT
#include "atomend.inc"
C
      DO I=JFT,JLT
        XMAS2=UMAS(I)*HALF
        VXA=V(1,NC1(I))+V(1,NC2(I))
        VYA=V(2,NC1(I))+V(2,NC2(I))
        VZA=V(3,NC1(I))+V(3,NC2(I))
        XM(I)= XMAS2*VXA
        YM(I)= XMAS2*VYA
        ZM(I)= XMAS2*VZA
        VA2   =V(1,NC1(I))*V(1,NC1(I))+V(1,NC2(I))*V(1,NC2(I))
     .        +V(2,NC1(I))*V(2,NC1(I))+V(2,NC2(I))*V(2,NC2(I))
     .        +V(3,NC1(I))*V(3,NC1(I))+V(3,NC2(I))*V(3,NC2(I))
        EK(I)= XMAS2*VA2*HALF
      ENDDO
C
      IF (IGRE /= 0) THEN
        FLAG = 0
        CALL GRELEM_SAV(JFT   ,JLT   ,GRESAV,IGRTH ,GRTH  ,
     2                  OFF   ,EINT  ,EK    ,XM    ,YM    ,
     3                  ZM    ,UMAS  ,RBIDON,RBIDON,RBIDON,
     4                  RBIDON,RBIDON,RBIDON,RBIDON,RBIDON,
     5                  RBIDON,RBIDON,RBIDON,RBIDON,RBIDON,
     6                  RBIDON,FLAG)
      ENDIF
C
      DO I=JFT,JLT
         MX = IPARTR(I)
         PARTSAV(1,MX)= PARTSAV(1,MX) + EINT(I)
         PARTSAV(2,MX)= PARTSAV(2,MX) + EK(I)
         PARTSAV(3,MX)= PARTSAV(3,MX) + XM(I)
         PARTSAV(4,MX)= PARTSAV(4,MX) + YM(I)
         PARTSAV(5,MX)= PARTSAV(5,MX) + ZM(I)
         PARTSAV(6,MX)= PARTSAV(6,MX) + UMAS(I)
      ENDDO
C
      RETURN
      END
