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
C
Chd|====================================================================
Chd|  TITRE2                        source/output/outfile/titre2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TITRE2
C------------------------------------
C     MATERIAUX
C------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "titr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TITRE(14)='     MATERIAL NUMBER. . . . . . . . . . . . .'
      TITRE(15)='     MATERIAL LAW . . . . . . . . . . . . . .'
      TITRE(16)='     INITIAL DENSITY. . . . . . . . . . . . .'
      TITRE(17)='     EOF REFERENCE DENSITY. . . . . . . . . .'
      TITRE(18)='     QUADRATIC BULK VISCOSITY . . . . . . . .'
      TITRE(19)='     LINEAR BULK VISCOSITY. . . . . . . . . .'
      TITRE(20)='     HOURGLASS VISCOSITY. . . . . . . . . . .'
      TITRE(21)='         ARBITRARY EULER LAGRANGE MATERIAL   '
      TITRE(22)='         ---------------------------------   '
      TITRE(23)='     UPWIND FOR MOMENTUM TRANSPORT. . . . . .'
      TITRE(24)='     UPWIND FOR OTHER TRANSPORT . . . . . . .'
      TITRE(25)='     SHELL HOURGLASS MEMBRANE DAMPING . . . .'
      TITRE(26)='     SHELL HOURGLASS FLEXURAL DAMPING . . . .'
      TITRE(27)='     SHELL HOURGLASS ROTATIONAL DAMPING . . .'
      TITRE(28)='     BEAM STRUCTURAL MEMBRANE DAMPING . . . .'
      TITRE(29)='     BEAM STRUCTURAL FLEXURAL DAMPING . . . .'
C------------------------------------
C     MATERIAUX COMMUN
C------------------------------------
      TITRE(30)='     YOUNG''S MODULUS . . . . . . . . . . . .'
      TITRE(31)='     POISSON''S RATIO . . . . . . . . . . . .'
      TITRE(32)='     SHEAR MODULUS  . . . . . . . . . . . . .'
      TITRE(33)='     YIELD COEFFICIENT CA . . . . . . . . . .'
      TITRE(34)='     YIELD COEFFICIENT CB . . . . . . . . . .'
      TITRE(35)='     YIELD COEFFICIENT CN . . . . . . . . . .'
      TITRE(36)='     EPS-MAX. . . . . . . . . . . . . . . . .'
      TITRE(37)='     SIG-MAX. . . . . . . . . . . . . . . . .'
      TITRE(38)='     STRAIN RATE COEFFICIENT CC . . . . . . .'
      TITRE(39)='     REFERENCE STRAIN RATE. . . . . . . . . .'
      TITRE(40)='     PSH: PRESSURE SHIFT. . . . . . . . . . .'
      TITRE(41)='     C0 . . . . . . . . . . . . . . . . . . .'
      TITRE(42)='     C1 . . . . . . . . . . . . . . . . . . .'
      TITRE(43)='     C2 . . . . . . . . . . . . . . . . . . .'
      TITRE(44)='     C3 . . . . . . . . . . . . . . . . . . .'
      TITRE(45)='     C4 . . . . . . . . . . . . . . . . . . .'
      TITRE(46)='     C5 . . . . . . . . . . . . . . . . . . .'
      TITRE(47)='     PRESSURE CUTOFF. . . . . . . . . . . . .'
      TITRE(48)='     INITIAL INTERNAL ENERGY PER UNIT VOLUME.'
      TITRE(49)='     VISCOSITY. . . . . . . . . . . . . . . .'
      TITRE(50)='         THERMAL MATERIAL  '
      TITRE(51)='         ----------------  '
      TITRE(52)='     T0 . . . . . . . . . . . . . . . . . . .'
      TITRE(53)='     SPECIFIC HEAT. . . . . . . . . . . . . .'
      TITRE(54)='     AS . . . . . . . . . . . . . . . . . . .'
      TITRE(55)='     BS . . . . . . . . . . . . . . . . . . .'
      TITRE(56)='     AL . . . . . . . . . . . . . . . . . . .'
      TITRE(57)='     BL . . . . . . . . . . . . . . . . . . .'
      TITRE(58)='     TEMPERATURE LOAD CURVE . . . . . . . . .'
      TITRE(59)='     RK0 INITIAL TURBULENT ENERGY . . . . . .'
      TITRE(60)='     CMU TURBULENT VISCOSITY COEFFICIENT. . .'
      TITRE(61)='     SK  K DIFFUSION COEFFICIENT. . . . . . .'
      TITRE(62)='     SE  EPSILON DIFFUSION COEFFICIENT. . . .'
      TITRE(63)='     PR/PRT LAM./TURB. PRANDT NUMBER RATIO. .'
      TITRE(64)='     EPS0 SHOULD BE > 0. IF CC /= 0.'
      TITRE(65)='     BULK MODULUS . . . . . . . . . . . . . .'
C------------------------------------
C     MATERIAUX 1
C------------------------------------
      TITRE(100)='       ELASTIC LAW                          '
      TITRE(101)='       -----------                          '
      TITRE(102)='       ELASTIC PLASTIC LAW                  '
      TITRE(103)='       -------------------                  '
C
      TITRE(105)='       ELASTIC PLASTIC HYDRODYNAMIC         '
      TITRE(106)='       ----------------------------         '
      TITRE(107)='       JOHNSON COOK LAW                     '
      TITRE(108)='       ----------------                     '
      TITRE(109)='     TEMPERATURE EXPONENT. . . . . . . . . .'
      TITRE(110)='     MELTING TEMPERATURE DEGREE K. . . . . .'
      TITRE(111)='     THETA-MAX . . . . . . . . . . . . . . .'
      TITRE(112)='       J.W.L. EXPLOSIVE                     '
      TITRE(113)='       ----------------                     '
      TITRE(114)='     A . . . . . . . . . . . . . . . . . . .'
      TITRE(115)='     B . . . . . . . . . . . . . . . . . . .'
      TITRE(116)='     R1. . . . . . . . . . . . . . . . . . .'
      TITRE(117)='     R2. . . . . . . . . . . . . . . . . . .'
      TITRE(118)='     W . . . . . . . . . . . . . . . . . . .'
      TITRE(119)='     DETONATION VELOCITY . . . . . . . . . .'
      TITRE(120)='     CHAPMAN JOUGUET PRESSURE. . . . . . . .'
      TITRE(121)='     CHAPMAN JOUGUET VOLUME. . . . . . . . .'
      TITRE(122)='     DETONATION TIMES FOR JWL ELEMENTS      '
      TITRE(123)='     ---------------------------------      '
      TITRE(124)='     ELEMENT  DETONATION TIME               '
C
      TITRE(126)='       VISCOUS FLUID                        '
      TITRE(127)='       --------------                       '
      TITRE(128)='     CT1 EPSILON SOURCE COEFFICIENT C1 . . .'
      TITRE(129)='     CT2 EPSILON SOURCE COEFFICIENT C2 . . .'
      TITRE(130)='     CT3 EPSILON SOURCE COEFFICIENT C3 . . .'
      TITRE(131)='       SOIL & CONCRETE                      '
      TITRE(132)='       ---------------                      '
      TITRE(133)='     YIELD COEFFICIENT A0. . . . . . . . . .'
      TITRE(134)='     YIELD COEFFICIENT A1. . . . . . . . . .'
      TITRE(135)='     YIELD COEFFICIENT A2. . . . . . . . . .'
      TITRE(136)='     A-MAX . . . . . . . . . . . . . . . . .'
      TITRE(137)='     UNLOADING BULK. . . . . . . . . . . . .'
      TITRE(138)='     MAX VOLUMIC COMPRESSION . . . . . . . .'
      TITRE(139)='     PRESSURE CUTOFF . . . . . . . . . . . .'
C
      TITRE(150)='       LAW FOR FLUID BOUNDARY ELEMENTS      '
      TITRE(151)='       -------------------------------      '
      TITRE(152)='     ITYP. . . . . . . . . . . . . . . . . .'
      TITRE(153)='     ITYP = 0 : GAS'
      TITRE(154)='     ITYP = 1 : LIQUID'
      TITRE(155)='     ITYP = 2 : TIME DEPENDENT'
      TITRE(156)='     PSH: PRESSURE SHIFT . . . . . . . . . .'
      TITRE(157)='     GAMMA CONSTANT. . . . . . . . . . . . .'
      TITRE(158)='     STAGNATION PRESSURE . . . . . . . . . .'
      TITRE(159)='     STAGNATION ENERGY . . . . . . . . . . .'
      TITRE(160)='     CRITICAL VELOCITY . . . . . . . . . . .'
      TITRE(161)='     CRITICAL DENSITY. . . . . . . . . . . .'
      TITRE(162)='     CRITICAL PRESSURE . . . . . . . . . . .'
      TITRE(163)='     REFERENCE NODE (VELOCITY) . . . . . . .'
      TITRE(164)='     STAGNATION DENSITY  LOAD CURVE. . . . .'
      TITRE(165)='     STAGNATION PRESSURE LOAD CURVE. . . . .'
      TITRE(166)='     STAGNATION ENERGY   LOAD CURVE. . . . .'
      TITRE(167)='     DENSITY    LOAD CURVE . . . . . . . . .'
      TITRE(168)='     PRESSURE   LOAD CURVE . . . . . . . . .'
      TITRE(169)='     ENERGY     LOAD CURVE . . . . . . . . .'
      TITRE(170)='     CHARACTERISTIC LENGTH . . . . . . . . .'
      TITRE(171)='     FINAL STAGNATION DENSITY. . . . . . . .'
      TITRE(172)='     FINAL STAGNATION PRESSURE(P-PSH). . . .'
      TITRE(173)='     FINAL STAGNATION ENERGY . . . . . . . .'
      TITRE(174)='     CHARACTERISTIC SOUND SPEED. . . . . . .'
      TITRE(175)='     CHARACTERISTIC LENGTH . . . . . . . . .'
      TITRE(176)='     FINAL DENSITY . . . . . . . . . . . . .'
      TITRE(177)='     FINAL PRESSURE(P-PSH) . . . . . . . . .'
      TITRE(178)='     FINAL ENERGY. . . . . . . . . . . . . .'
      TITRE(179)='     INITIAL PRESSURE(P-PSH) . . . . . . . .'
      TITRE(180)='     INITIAL ENERGY. . . . . . . . . . . . .'
      TITRE(181)='     INITIAL EPSILON . . . . . . . . . . . .'
      TITRE(182)='     K ENERGY   LOAD CURVE . . . . . . . . .'
      TITRE(183)='     EPSILON    LOAD CURVE . . . . . . . . .'
      TITRE(184)='     INITIAL THERMAL FLUX. . . . . . . . . .'
      TITRE(185)='     FLUX LOAD CURVE. . .  . . . . . . . . .'
C
      TITRE(200)='       WALL BOUNDARY                        '
      TITRE(201)='       -------------                        '
      TITRE(202)='     K  LOGARITHMIQUE LAW COEFFICIENT. . . .'
      TITRE(203)='     E  LOGARITHMIQUE LAW COEFFICIENT. . . .'
      TITRE(204)='     A  K,EPSILON ECCENTRICITY . . . . . . .'
      TITRE(205)='     B  SOURCE FLAG. . . . . . . . . . . . .'
C
      RETURN
      END
