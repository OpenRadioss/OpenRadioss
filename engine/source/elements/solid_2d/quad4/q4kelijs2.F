Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    q4kelijs2   ../engine/source/elements/solid_2d/quad4/q4kelijs2.F
!||--- called by ------------------------------------------------------
!||    q4kel2      ../engine/source/elements/solid_2d/quad4/q4kel2.F
!||====================================================================
      SUBROUTINE Q4KELIJS2(
     1   PYI,     PZI,     PYJ,     PZJ,
     2   PYCI,    PZCI,    PYCJ,    PZCJ,
     3   AY,      R22,     R23,     DD,
     4   GG,      KIJ,     KIJU,    KIJL,
     5   IS,      NEL,     JCVT)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JCVT
      INTEGER IS
      my_real
     .   PYI(*), PZI(*), PYJ(*), PZJ(*),
     .   PYCI(*), PZCI(*), PYCJ(*), PZCJ(*), AY(*), R22(*), R23(*),
     .   DD(3,3,*), GG(*), KIJ(2,2,*), KIJU(2,2,*), KIJL(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,IASY
      my_real
     .    AYS(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO EP=1,NEL
        KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                DD(1,1,EP)*PYI(EP)*PYJ(EP) + 
     .                GG(EP)*PZCI(EP)*PZCJ(EP)
        KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                DD(1,2,EP)*PYI(EP)*PZJ(EP) + 
     .                GG(EP)*PZCI(EP)*PYCJ(EP)
        KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                DD(1,2,EP)*PZI(EP)*PYJ(EP) + 
     .                GG(EP)*PYCI(EP)*PZCJ(EP)
        KIJ(2,2,EP) = KIJ(2,2,EP) + 
     .                DD(2,2,EP)*PZI(EP)*PZJ(EP) + 
     .                GG(EP)*PYCI(EP)*PYCJ(EP)
      ENDDO
C
      IF (N2D==1) THEN
        DO EP=1,NEL
          AYS(EP) = AY(EP)*AY(EP)
        ENDDO
        IF (JCVT==0) THEN
          DO EP=1,NEL
            KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                  DD(1,3,EP)*AY(EP)*(PYI(EP)+PYJ(EP)) + 
     .                  DD(3,3,EP)*AYS(EP)
            KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                  DD(2,3,EP)*AY(EP)*PZJ(EP) 
            KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                  DD(2,3,EP)*PZI(EP)*AY(EP) 
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          DO EP=1,NEL
C            KIJU(1,1,EP) = KIJU(1,1,EP) - 
C     .                  AY(EP)*(DD(1,1,EP)*PYJ(EP)+DD(1,3,EP)*AY(EP))
C            KIJU(1,2,EP) = KIJU(1,2,EP) - AY(EP)*DD(1,2,EP)*PZJ(EP)
C            KIJU(2,1,EP) = KIJU(2,1,EP) - AY(EP)*GG(EP)*PZCJ(EP)
C            KIJU(2,2,EP) = KIJU(2,2,EP) - AY(EP)*GG(EP)*PYCJ(EP)
C            KIJL(1,1,EP) = KIJL(1,1,EP) - 
C     .                  AY(EP)*(DD(1,1,EP)*PYI(EP)+DD(1,3,EP)*AY(EP))
C            KIJL(1,2,EP) = KIJL(1,2,EP) - AY(EP)*GG(EP)*PZCI(EP)
C            KIJL(2,1,EP) = KIJL(2,1,EP) - AY(EP)*DD(1,2,EP)*PZI(EP)
C            KIJL(2,2,EP) = KIJL(2,2,EP) - AY(EP)*GG(EP)*PYCI(EP)
C          ENDDO
C          ENDIF
        ELSE
          DO EP=1,NEL
            KIJ(1,1,EP) = KIJ(1,1,EP) + 
     .                  DD(1,3,EP)*R22(EP)*AY(EP)*(PYI(EP)+PYJ(EP)) +
     .                  DD(3,3,EP)*R22(EP)*R22(EP)*AYS(EP)
            KIJ(1,2,EP) = KIJ(1,2,EP) + 
     .                  DD(1,3,EP)*R23(EP)*AY(EP)*PYI(EP) +
     .                  DD(2,3,EP)*R22(EP)*AY(EP)*PZJ(EP) +
     .                  DD(3,3,EP)*R22(EP)*R23(EP)*AYS(EP)
            KIJ(2,1,EP) = KIJ(2,1,EP) + 
     .                  DD(1,3,EP)*R23(EP)*AY(EP)*PYJ(EP) +
     .                  DD(2,3,EP)*R22(EP)*AY(EP)*PZI(EP) +
     .                  DD(3,3,EP)*R22(EP)*R23(EP)*AYS(EP)
            KIJ(2,2,EP) = KIJ(2,2,EP) + 
     .                  DD(2,3,EP)*R23(EP)*AY(EP)*(PZI(EP)+PZJ(EP)) +
     .                  DD(3,3,EP)*R23(EP)*R23(EP)*AYS(EP)
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
