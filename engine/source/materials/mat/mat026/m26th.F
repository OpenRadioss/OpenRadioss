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
Chd|  M26TH                         source/materials/mat/mat026/m26th.F
Chd|-- called by -----------
Chd|        ATHERM                        source/ale/atherm.F           
Chd|-- calls ---------------
Chd|        MINTP_RT                      ../common_source/eos/mintp_rt.F
Chd|====================================================================
      SUBROUTINE M26TH(MAT, RHO,T,XK,PM,SESAME,Z)
C----------------------------------------
C     CALCUL DE LA CONDUCTIVITE THERMIQUE
C----------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*)
      my_real
     .   RHO(*), T(*), XK(*), PM(NPROPM,*), SESAME(*), Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MX, NR, NT, IDR, IDT, IDQ
      my_real
     .   ZZ, DELTAT, XLAMB, STEFAN, RL, TL, OPAC, DXDR, ROSSEL, XKR
      REAL*8 NE, NA, ATOM
C-----------------------------------------------
      DATA NA /6.0225E+23/
C-----------------------------------------------
      !----------------------------
      !     CONDUCTION ELECTRONIQUE
      !----------------------------
      DO I=LFT,LLT
        J=I+NFT
        MX = MAT(I)
        ZZ = MAX(EM10,Z(I))
        DELTAT= THREEP44 * ZEP26 * LOG(ZZ) / ZZ
        IF(DELTAT>ZERO)THEN
          DELTAT= ONE / ( ONE +  DELTAT)
        ELSE
          DELTAT= ZERO
        ENDIF
        ATOM  = PM(37,MX)
        NE    = RHO(I)*NA*ZZ/ATOM
        XLAMB = PM(36,MX)*T(I)**THREE_HALF/SQRT(NE)
        XLAMB = MAX(ONE,XLAMB)
        XLAMB = MAX(EM10, LOG(XLAMB))
        XK(J) = XK(J) + ZEP4*DELTAT*PM(35,MX)*T(I)**TWOP5 / (ZZ*XLAMB)
      ENDDO

      !----------------------------
      !     RADIATION
      !----------------------------
      DO I=LFT,LLT
        J=I+NFT
        MX = MAT(I)
        STEFAN = PM(51,MX)
        IF(STEFAN>ZERO.AND.T(I)>EP04)THEN
          NR     = NINT(PM(48,MX))
          NT     = NINT(PM(49,MX))
          IDR    = NINT(PM(50,MX))
          IDT = IDR + NR
          IDQ = IDT + NT
          RL = LOG10(RHO(I))
          TL = LOG10(T(I))
          CALL MINTP_RT(SESAME(IDR),NR,
     +        SESAME(IDT),NT,SESAME(IDQ),RL,TL,OPAC,DXDR)
          OPAC = TEN**OPAC
          ROSSEL = ONE / ( RHO(I) * OPAC)
          XKR = SIXTEEN * STEFAN * T(I)**3 * ROSSEL * THIRD
          XK(J) = XK(J) + XKR
        ENDIF
        XK(J) = MIN(XK(J),PM(52,MX))
      ENDDO
C
      RETURN
      END
