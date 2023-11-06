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
Chd|  LAW_USERSO                    source/user_interface/law_userso.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE LAW_USERSO
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS
C---------+---------+---+---+--------------------------------------------
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C DPLA    | NEL     | F |R/W| INCREMENTAL PLASTIC STRAIN
C---------+---------+---+---+--------------------------------------------
C
        TYPE ULAWINTBUF
C integer
          INTEGER  NCYCLE
          INTEGER  ID
          INTEGER  IPTR
          INTEGER  IPTS
          INTEGER  IPTT
C floating
          my_real R11(MVSIZ)
          my_real R12(MVSIZ)
          my_real R13(MVSIZ)
C
          my_real R21(MVSIZ)
          my_real R22(MVSIZ)
          my_real R23(MVSIZ)
C
          my_real R31(MVSIZ)
          my_real R32(MVSIZ)
          my_real R33(MVSIZ)
C
C
          my_real EPSPXX(MVSIZ)
          my_real EPSPYY(MVSIZ)
          my_real EPSPZZ(MVSIZ)
          my_real EPSPXY(MVSIZ)
          my_real EPSPYZ(MVSIZ)
          my_real EPSPZX(MVSIZ)
c
          my_real EPSXX(MVSIZ)
          my_real EPSYY(MVSIZ)
          my_real EPSZZ(MVSIZ)
          my_real EPSXY(MVSIZ)
          my_real EPSYZ(MVSIZ)
          my_real EPSZX(MVSIZ)
C--------when Ismstr=10 we input [F] et [U]
          my_real FPSXX(MVSIZ)
          my_real FPSYY(MVSIZ)
          my_real FPSZZ(MVSIZ)
          my_real FPSXY(MVSIZ)
          my_real FPSYZ(MVSIZ)
          my_real FPSZX(MVSIZ)
          my_real FPSYX(MVSIZ)
          my_real FPSZY(MVSIZ)
          my_real FPSXZ(MVSIZ)
C
          my_real UPSXX(MVSIZ)
          my_real UPSYY(MVSIZ)
          my_real UPSZZ(MVSIZ)
          my_real UPSXY(MVSIZ)
          my_real UPSYZ(MVSIZ)
          my_real UPSXZ(MVSIZ)
C
          my_real DEPSXX(MVSIZ)
          my_real DEPSYY(MVSIZ)
          my_real DEPSZZ(MVSIZ)
          my_real DEPSXY(MVSIZ)
          my_real DEPSYZ(MVSIZ)
          my_real DEPSZX(MVSIZ)
C
          my_real SIGOXX(MVSIZ)
          my_real SIGOYY(MVSIZ)
          my_real SIGOZZ(MVSIZ)
          my_real SIGOXY(MVSIZ)
          my_real SIGOYZ(MVSIZ)
          my_real SIGOZX(MVSIZ)

          my_real RHO0(MVSIZ)
          my_real TEMP(MVSIZ)      ! temperature
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
          my_real SIGNXX(MVSIZ)
          my_real SIGNYY(MVSIZ)
          my_real SIGNZZ(MVSIZ)
          my_real SIGNXY(MVSIZ)
          my_real SIGNYZ(MVSIZ)
          my_real SIGNZX(MVSIZ)
C
          my_real SIGVXX(MVSIZ)
          my_real SIGVYY(MVSIZ)
          my_real SIGVZZ(MVSIZ)
          my_real SIGVXY(MVSIZ)
          my_real SIGVYZ(MVSIZ)
          my_real SIGVZX(MVSIZ)

          my_real DPLA(MVSIZ)

        END TYPE ULAWINTBUF
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      END MODULE LAW_USERSO

