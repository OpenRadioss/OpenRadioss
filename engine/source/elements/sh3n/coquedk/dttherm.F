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
Chd|  DTTHERM                       source/elements/sh3n/coquedk/cndt3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DTTHERM(
     1   JFT,     JLT,     PM,      T,
     2   RE,      RHO,     RK,      VOL,
     3   LC,      MAT,     DT_THERM,OFF,
     4   CONDE,   JTUR)
C============================================================================
C   I m p l i c i t   T y p e s
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JTUR
      INTEGER JFT  ,JLT 
      INTEGER MAT(*)
C
      my_real
     .   PM(NPROPM,*),  T(*), LC(*),DT_THERM,OFF(*),CONDE(*),VOL(*),RHO(*),
     .   RK(*),RE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MT
      my_real
     .  SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU,
     .   ATU, DT, LC2,RPR
C=======================================================================
C
c       DT_THERM = EP20
       MT  = MAT(JFT)
       SPH = PM(69,MT)
       AK1 = PM(75,MT)
       BK1 = PM(76,MT)
       AK2 = PM(77,MT)
       BK2 = PM(78,MT)
       TLI = PM(80,MT)
       DO I=JFT,JLT
          IF(T(I)<TLI)THEN
            AKK=AK1+BK1*T(I)
          ELSE
            AKK=AK2+BK2*T(I)
          ENDIF
          IF(JTUR/=0)THEN
            XMU = RHO(I)*PM(24,MT)
            TMU = PM(81,MT)
            RPR = PM(95,MT)
            ATU=RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
            AKK=AKK*(1.+ATU)
          ENDIF
          AKK=AKK*THEACCFACT
          LC2 = LC(I)*LC(I)
          DT = DTFACTHERM*HALF*LC2*SPH/MAX(AKK,EM20)
          IF(DT<DT_THERM)  DT_THERM = DT
          CONDE(I) = FOUR*VOL(I)*AKK/LC2  
          CONDE(I) = CONDE(I)*OFF(I)
       ENDDO

C----6---------------------------------------------------------------7---------8
      RETURN
      END
