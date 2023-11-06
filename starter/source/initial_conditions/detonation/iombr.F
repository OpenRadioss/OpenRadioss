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
Chd|  IOMBR                         source/initial_conditions/detonation/iombr.F
Chd|-- called by -----------
Chd|        ECRAN1                        source/initial_conditions/detonation/ecran1.F
Chd|        ECRAN2                        source/initial_conditions/detonation/ecran2.F
Chd|-- calls ---------------
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      INTEGER FUNCTION IOMBR(DETONATORS,X,IECR,DDMX,VDET_ARG)
        USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real DDMX
        INTEGER IECR(*)
        my_real X(3,NUMNOD)
        my_real VDET_ARG
        TYPE(DETONATOR_STRUCT_) :: DETONATORS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr11_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER JOMBR, KOMBR, II, I,NPE
        my_real YLD, ZLD, DD, D2, EPS, DD_UP, DD_DOWN,
     .          Y2, Z2,  Y1, Z1, CROSS_PROD_1,CROSS_PROD_2,
     .          D1, Y12, Z12, A1, A2, A3, Y3, Z3, D3
C-----------------------------------------------

        NPE = DETONATORS%NPE !number of points
        IOMBR=0 !function result
        JOMBR=0
        KOMBR=0
        !YD,ZD : detonation source
        !YL,ZL : point for which lightening time is going to be computed
        YLD=YL-YD
        ZLD=ZL-ZD
        DD=YLD**2+ZLD**2 !distance from source

        IF(DD > DDMX)THEN
          IOMBR=2  !function result
          GOTO 999
        ENDIF

        !--initialization first node
        EPS=EM5*DD
        DD_UP=DD+EPS
        DD_DOWN=DD-EPS
        II=IECR(1)
        Y2=X(2,II)
        Z2=X(3,II)
        CROSS_PROD_2=YLD*(Z2-ZD)-ZLD*(Y2-YD)
        D2 =(Y2-YD)*YLD+(Z2-ZD)*ZLD

        !--loop over nodes
        DO I=2,NPE
          Y1=Y2
          Z1=Z2
          CROSS_PROD_1=CROSS_PROD_2
          D1 =D2
          II=IECR(I)
          Y2=X(2,II)
          Z2=X(3,II)
          CROSS_PROD_2=YLD*(Z2-ZD)-ZLD*(Y2-YD)
          D2=(Y2-YD)*YLD+(Z2-ZD)*ZLD
          IF(CROSS_PROD_1*CROSS_PROD_2 <= EPS)THEN
            !Pi-1 and Pi in different half planes of boundary (DL)
            IF(D2 > DD_UP .AND. D1 > DD_UP)THEN
              !Pi-1 and Pi after L
              IOMBR=0
            ELSEIF(D2 < -EPS.AND.D1 < -EPS)THEN
              !Pi-1 and Pi before D
              IOMBR=0
            ELSE
              !SEARCHING INTERSECTION [Pi-1,Pi] with [DL]
              Y12=Y1-Y2
              Z12=Z1-Z2
              A1=YL*ZLD-ZL*YLD
              A2=Y1*Z12-Z1*Y12
              A3=Y12*ZLD-Z12*YLD
              IF(ABS(A3) > EPS)THEN
                Y3=(Y12*A1-YLD*A2)/A3
                Z3=(Z12*A1-ZLD*A2)/A3
                D3=(Y3-YD)*YLD+(Z3-ZD)*ZLD
                IF(D3 > DD_UP.OR.D3 < -EPS)THEN
                  IOMBR=0
                ELSEIF(D3 > EPS.AND.D3 < DD_DOWN)THEN
                  !intersection inside
                  IOMBR=1
                  GOTO 999
                ELSEIF(ABS(D3) <= EPS)THEN
                  !intersection in extremity D
                  IF(A3 > EPS)JOMBR=1
                ELSEIF(ABS(D3-DD) <= EPS)THEN
                  !intersection in extremity L
                  IF(A3 < -EPS)KOMBR=1
                ELSE
                  IOMBR=0
                ENDIF !(D3 > DD_UP.OR.D3 < -EPS)
              ELSE
                IOMBR=0
              ENDIF !(ABS(A3) > EPS)
            ENDIF !(D2 > DD_UP.AND.D1 > DD_UP)
          ELSE
            !(CROSS_PROD_1*CROSS_PROD_2 > EPS) Pi-1 and Pi are in the same half plane of boundary (DL)
            IOMBR=0
          ENDIF !(CROSS_PROD_1*CROSS_PROD_2<=EPS)
        END DO !I=2,NPE

        IF(JOMBR+KOMBR == 2)THEN
          IOMBR=1
        ELSE
          DTO=DTO0+SQRT(DD)/VDET_ARG
        ENDIF

 999    CONTINUE
        IF(IOMBR == 0)DDMX=DD

        RETURN
      END
