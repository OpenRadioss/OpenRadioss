Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SIGEPS01GPINCH                source/materials/mat/mat001/sigeps01gpinch.F
Chd|-- called by -----------
Chd|        MULAWGLCPINCH                 source/elements/shell/coqueba/mulawglcpinch.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS01GPINCH(
     1                     JFT     ,JLT     ,G       ,THK     ,OFF     ,
     2                     GS      ,A1      ,A2      ,NU      ,THK0    ,
     3                     NEL     ,SSP     ,RHO     ,
     4                     DEPSXX  ,DEPSYY  ,DEPSZZ  ,
     5                     DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6                     DEPBXX  ,DEPBYY  ,DEPBXY  ,
     7                     DEPPXZ  ,DEPPYZ  ,
     8                     SIGOXX  ,SIGOYY  ,SIGOZZ  ,
     9                     SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     A                     MOMOXX  ,MOMOYY  ,MOMOXY  ,
     B                     MOMOPXZ ,MOMOPYZ ,
     C                     SIGNXX  ,SIGNYY  ,SIGNZZ  ,
     D                     SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     E                     MOMNXX  ,MOMNYY  ,MOMNXY  ,
     F                     MOMNPXZ ,MOMNPYZ )
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
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NEL,ISMSTR
C     REAL
      my_real
     .  OFF(*),GS(*),THK(*)
      my_real
     .   NU(MVSIZ),G(MVSIZ),E(MVSIZ),A1(MVSIZ),A2(MVSIZ),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   DEPPXZ(NEL),DEPPYZ(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL),
     .   MOMOPXZ(NEL),MOMOPYZ(NEL)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    MOMNPXZ(NEL),MOMNPYZ(NEL),
     .    SSP(NEL),RHO(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J
C     REAL
      my_real
     .   PA1(MVSIZ),PA2(MVSIZ),PA3(MVSIZ),PA4(MVSIZ),PA5(MVSIZ),
     .   B1(MVSIZ),B2(MVSIZ),B3(MVSIZ),B4(MVSIZ),B5(MVSIZ),
     .   THK0(MVSIZ),THK08(MVSIZ)
C-----------------------------------------------
C
      DO I=JFT,JLT
C         [PM] reverse engineer E and ANU from A11 and A12
C         [PM] can't use A11/A12 since they correspond to shell (plane stress)
          E(I)   = A1(I)*(ONE-NU(I)**2)
C
C         PA1 = C1111, C2222, C3333
C         PA2 = C1122, C1133, C2233
C         PA3 = C1212, C1313, C2323
C         PA4 = C1111*, C2222* 
C         PA5 = C1122*
          PA1(I) = E(I)*(ONE-NU(I))/(ONE+NU(I))/(ONE-TWO*NU(I))          
          PA2(I) = E(I)*NU(I)/(ONE+NU(I))/(ONE-TWO*NU(I))         
          PA3(I) = E(I)/TWO/(ONE+NU(I))         
          PA4(I) = (ONE+NU(I))*(ONE-TWO*NU(I))/(ONE-NU(I)**2)/(ONE-NU(I))*PA1(I)         
          PA5(I) = (ONE+NU(I))*(ONE-TWO*NU(I))/(ONE-NU(I)**2)*PA2(I)
C
          THK08(I) = THK0(I)*ONE_OVER_12
          B1(I) = PA1(I)*THK08(I)
          B2(I) = PA2(I)*THK08(I)
          B3(I) = PA3(I)*THK08(I)
          B4(I) = PA4(I)*THK08(I)
          B5(I) = PA5(I)*THK08(I)
      ENDDO
C
      DO I=JFT,JLT
C
        SIGNXX(I)=SIGOXX(I)+PA1(I)*DEPSXX(I)+PA2(I)*DEPSYY(I)+PA2(I)*DEPSZZ(I)
        SIGNYY(I)=SIGOYY(I)+PA2(I)*DEPSXX(I)+PA1(I)*DEPSYY(I)+PA2(I)*DEPSZZ(I)
        SIGNZZ(I)=SIGOZZ(I)+PA2(I)*DEPSXX(I)+PA2(I)*DEPSYY(I)+PA1(I)*DEPSZZ(I)
C
        SIGNXY(I)=SIGOXY(I)+G(I)*DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+GS(I)*(DEPSYZ(I)+ZERO*DEPPYZ(I))
        SIGNZX(I)=SIGOZX(I)+GS(I)*(DEPSZX(I)+ZERO*DEPPXZ(I))
C
        MOMNXX(I)=MOMOXX(I)+B4(I)*DEPBXX(I)+B5(I)*DEPBYY(I)
        MOMNYY(I)=MOMOYY(I)+B5(I)*DEPBXX(I)+B4(I)*DEPBYY(I)
        MOMNXY(I)=MOMOXY(I)+B3(I)*DEPBXY(I)
C
        MOMNPXZ(I)=MOMOPXZ(I)+B3(I)*DEPPXZ(I)
        MOMNPYZ(I)=MOMOPYZ(I)+B3(I)*DEPPYZ(I)
C
C       Sound speed
C       SSP(I) = SQRT(E(I)/RHO(I))                      ! historic
C       SSP(I) = SQRT(E(I)/RHO(I)/(ONE-ANU(I)*ANU(I)))   ! for true shells
        SSP(I) = SQRT(PA1(I)/RHO(I))                    ! like solids
C
      ENDDO
      RETURN
      END
