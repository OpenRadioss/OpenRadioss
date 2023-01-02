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
Chd|  INTHTG                        source/fluid/inte_tg.F        
Chd|-- called by -----------
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|        MASS_FLUID_TG                 source/fluid/mass-fluid_tg.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTHTG(X1 , Y1,  Z1,  X2,  Y2,  Z2,  
     .                  X3,  Y3,  Z3,  XP,  YP,  ZP,                    
     .                  NRX,NRY, NRZ,  D2, JAC,
     .                  XS,  YS,  ZS, RVAL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        XP, YP, ZP, D2, JAC, NRX, NRY, NRZ, 
     .        XS, YS, ZS, RVAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP, IHG
      my_real
     .        PG(50), WPG(25),  R2, 
     .        VAL1, VAL2, VAL3, W, XG, YG, ZG, VALPHI,
     .        ETA1, ETA2
C
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
C
C DISTANCE A LA SOURCE
      R2=(XP-XS)**2+(YP-YS)**2+(ZP-ZS)**2
C NOMBRE DE POINTS DE GAUSS
      IF (R2>HUNDRED*D2) THEN
         NPG=1
         IAD=1
      ELSEIF (R2>TWENTY5*D2) THEN
         NPG=4
         IAD=2
      ELSEIF (R2>FOUR*D2) THEN
         NPG=7
         IAD=6
      ELSE
         NPG=13
         IAD=13
      ENDIF
C INTEGRATION
      RVAL=ZERO
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         ETA1=PG(IAD2)
         ETA2=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=ONE-ETA1-ETA2
         VAL2=ETA1         
         VAL3=ETA2          
         XG=VAL1*X1+VAL2*X2+VAL3*X3
         YG=VAL1*Y1+VAL2*Y2+VAL3*Y3
         ZG=VAL1*Z1+VAL2*Z2+VAL3*Z3
         R2=(XG-XP)**2+(YG-YP)**2+(ZG-ZP)**2
         IF(R2>EM20)THEN
           VALPHI=-(NRX*(XG-XP)+NRY*(YG-YP)+NRZ*(ZG-ZP))/(R2**THREE_HALF)
           RVAL  = RVAL + W*VALPHI*JAC
         ENDIF
      ENDDO
C
      RETURN
      END
C
C
Chd|====================================================================
Chd|  INTGTG                        source/fluid/inte_tg.F        
Chd|-- called by -----------
Chd|        MASS_FLUID_QD                 source/fluid/mass-fluid_qd.F  
Chd|        MASS_FLUID_TG                 source/fluid/mass-fluid_tg.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTGTG(X1 , Y1,  Z1,  X2,  Y2,  Z2,  
     .                  X3,  Y3,  Z3,  XP,  YP,  ZP,                    
     .                  D2, JAC,
     .                  XS,  YS,  ZS, RVAL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        XP, YP, ZP, D2, JAC, 
     .        XS, YS, ZS, RVAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPG, IAD, IAD2, IP, IHG
      my_real
     .        PG(50), WPG(25),  R2, 
     .        VAL1, VAL2, VAL3, W, XG, YG, ZG, VALPHI,
     .        ETA1, ETA2
C
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
C
C DISTANCE A LA SOURCE
      R2=(XP-XS)**2+(YP-YS)**2+(ZP-ZS)**2
C NOMBRE DE POINTS DE GAUSS
      IF (R2>HUNDRED*D2) THEN
         NPG=1
         IAD=1
      ELSEIF (R2>TWENTY5*D2) THEN
         NPG=4
         IAD=2
      ELSEIF (R2>FOUR*D2) THEN
         NPG=7
         IAD=6
      ELSE
         NPG=13
         IAD=13
      ENDIF
C INTEGRATION
      RVAL=ZERO
      IAD2=2*(IAD-1)+1
      DO IP=1,NPG
         W=WPG(IAD)
         ETA1=PG(IAD2)
         ETA2=PG(IAD2+1)
         IAD=IAD+1
         IAD2=IAD2+2
         VAL1=ONE-ETA1-ETA2
         VAL2=ETA1         
         VAL3=ETA2          
         XG=VAL1*X1+VAL2*X2+VAL3*X3
         YG=VAL1*Y1+VAL2*Y2+VAL3*Y3
         ZG=VAL1*Z1+VAL2*Z2+VAL3*Z3
         R2=(XG-XP)**2+(YG-YP)**2+(ZG-ZP)**2
         IF(R2>EM20)THEN
           VALPHI= ONE/SQRT(R2)
           RVAL  = RVAL + W*VALPHI*JAC
         ENDIF
      ENDDO
C
      RETURN
      END

