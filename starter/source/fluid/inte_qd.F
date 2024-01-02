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
Chd|  INTHQD                        source/fluid/inte_qd.F        
Chd|-- called by -----------
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTHQD(X1 , Y1,  Z1,  X2,  Y2,  Z2,
     .                  X3,  Y3,  Z3,  X4,  Y4,  Z4,
     .                  XP,  YP,  ZP,  XS,  YS,  ZS,
     .                  NRX, NRY, NRZ, D2, JAC,RVAL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4,
     .        XP, YP, ZP, D2, JAC, NRX, NRY, NRZ, 
     .        XS, YS, ZS, RVAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP
      my_real
     .        PG(28), WPG(14),  R2, W, XG, YG, ZG,
     .        VAL1, VAL2, VAL3, VAL4, VALPHI,
     .        KSIP, ETAP
C
      DATA PG  / .0000000000000, .0000000000000,
     .          -.5773502691896,-.5773502691896,
     .           .5773502691896,-.5773502691896,
     .          -.5773502691896, .5773502691896,
     .           .5773502691896, .5773502691896,
     .          -.7745966692415,-.7745966692415,
     .           .0000000000000,-.7745966692415,
     .           .7745966692415,-.7745966692415,
     .          -.7745966692415, .0000000000000,
     .           .0000000000000, .0000000000000,
     .           .7745966692415, .0000000000000,
     .          -.7745966692415, .7745966692415,
     .           .0000000000000, .7745966692415,
     .           .7745966692415, .7745966692415/
      DATA WPG  / 1.00000000000,
     .           .2500000000000, .2500000000000,
     .           .2500000000000, .2500000000000,
     .           .0771604938272, .1234567901234,
     .           .0771604938272, .1234567901234,
     .           .1975308641975, .1234567901234,
     .           .0771604938272, .1234567901234,
     .           .0771604938272/
C-------------------------------------------------------------
C DISTANCE A LA SOURCE
      R2=(XP-XS)**2+(YP-YS)**2+(ZP-ZS)**2
C NOMBRE DE POINTS DE GAUSS
      IF (R2>HUNDRED*D2) THEN
         NPG=1
         IAD=1
      ELSEIF (R2>TWENTY5*D2) THEN
         NPG=4
         IAD=2
      ELSE
         NPG=9
         IAD=6
      ENDIF
C INTEGRATION
      RVAL=ZERO
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         KSIP=PG(IAD2)
         ETAP=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=FOURTH*(ONE-KSIP)*(ONE-ETAP)
         VAL2=FOURTH*(ONE+KSIP)*(ONE-ETAP)
         VAL3=FOURTH*(ONE+KSIP)*(ONE+ETAP)
         VAL4=FOURTH*(ONE-KSIP)*(ONE+ETAP)
         XG=VAL1*X1+VAL2*X2+VAL3*X3+VAL4*X4
         YG=VAL1*Y1+VAL2*Y2+VAL3*Y3+VAL4*Y4
         ZG=VAL1*Z1+VAL2*Z2+VAL3*Z3+VAL4*Z4
         R2=(XG-XP)**2+(YG-YP)**2+(ZG-ZP)**2
         IF(R2>EM20)THEN
           VALPHI=-(NRX*(XG-XP)+NRY*(YG-YP)+NRZ*(ZG-ZP))/(R2**THREE_HALF)
           RVAL  =RVAL+W*VALPHI*JAC
         ENDIF
      ENDDO
C
      RETURN
      END
C
C
Chd|====================================================================
Chd|  INTGQD                        source/fluid/inte_qd.F        
Chd|-- called by -----------
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTGQD(X1 , Y1,  Z1,  X2,  Y2,  Z2,
     .                  X3,  Y3,  Z3,  X4,  Y4,  Z4,
     .                  XP,  YP,  ZP,  XS,  YS,  ZS,
     .                  D2, JAC,RVAL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4,
     .        XP, YP, ZP, D2, JAC, 
     .        XS, YS, ZS, RVAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP
      my_real
     .        PG(28), WPG(14),  R2, W, XG, YG, ZG,
     .        VAL1, VAL2, VAL3, VAL4, VALPHI,
     .        KSIP, ETAP
C
      DATA PG  / .0000000000000, .0000000000000,
     .          -.5773502691896,-.5773502691896,
     .           .5773502691896,-.5773502691896,
     .          -.5773502691896, .5773502691896,
     .           .5773502691896, .5773502691896,
     .          -.7745966692415,-.7745966692415,
     .           .0000000000000,-.7745966692415,
     .           .7745966692415,-.7745966692415,
     .          -.7745966692415, .0000000000000,
     .           .0000000000000, .0000000000000,
     .           .7745966692415, .0000000000000,
     .          -.7745966692415, .7745966692415,
     .           .0000000000000, .7745966692415,
     .           .7745966692415, .7745966692415/
      DATA WPG  / 1.00000000000,
     .           .2500000000000, .2500000000000,
     .           .2500000000000, .2500000000000,
     .           .0771604938272, .1234567901234,
     .           .0771604938272, .1234567901234,
     .           .1975308641975, .1234567901234,
     .           .0771604938272, .1234567901234,
     .           .0771604938272/
C-------------------------------------------------------------
C DISTANCE A LA SOURCE
      R2=(XP-XS)**2+(YP-YS)**2+(ZP-ZS)**2
C NOMBRE DE POINTS DE GAUSS
      IF (R2>HUNDRED*D2) THEN
         NPG=1
         IAD=1
      ELSEIF (R2>TWENTY5*D2) THEN
         NPG=4
         IAD=2
      ELSE
         NPG=9
         IAD=6
      ENDIF
C INTEGRATION
      RVAL=ZERO
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         KSIP=PG(IAD2)
         ETAP=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=FOURTH*(ONE-KSIP)*(ONE-ETAP)
         VAL2=FOURTH*(ONE+KSIP)*(ONE-ETAP)
         VAL3=FOURTH*(ONE+KSIP)*(ONE+ETAP)
         VAL4=FOURTH*(ONE-KSIP)*(ONE+ETAP)
         XG=VAL1*X1+VAL2*X2+VAL3*X3+VAL4*X4
         YG=VAL1*Y1+VAL2*Y2+VAL3*Y3+VAL4*Y4
         ZG=VAL1*Z1+VAL2*Z2+VAL3*Z3+VAL4*Z4
         R2=(XG-XP)**2+(YG-YP)**2+(ZG-ZP)**2
         IF(R2>EM20)THEN
           VALPHI=ONE/SQRT(R2)
           RVAL  =RVAL+W*VALPHI*JAC
         ENDIF
      ENDDO
C
      RETURN
      END

