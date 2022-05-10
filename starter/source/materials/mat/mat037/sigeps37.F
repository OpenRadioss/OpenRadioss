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
Chd|  SIGEPS37                      source/materials/mat/mat037/sigeps37.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS37(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VOLUME , EINT   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX, 
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF                    )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines aim is to compute initial sound speed
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL,     NUPARAM, NUVAR

      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VOLUME(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     . FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER :: I
      my_real :: C1,GAM,PMIN,P0,RHO10,RHO20,A1,P,MU1P1,MU2P1,RHO1,RHO2,A


C----------------------------------------------------------------
      DO I=1,NEL
        C1           =  UPARAM(4)
        GAM          =  UPARAM(5)
        PMIN         =  UPARAM(8)
        P0           =  UPARAM(9)
        RHO10        =  UPARAM(11)
        RHO20        =  UPARAM(12)
        A1           =  UPARAM(10)         
        P            = -P0  
        RHO(I)       =  RHO10 * A1 + (UN-A1)*RHO20     
        IF(GAM*C1.GE.EM30)THEN !if Liquid and gas correctly defined
          MU1P1      = UN  !(P/C1+UN)
          MU2P1      = UN  !( UN+P/P0)**(UN/GAM)
          RHO1       = RHO10*MU1P1
          RHO2       = RHO20*MU2P1
          A          = (RHO(I)-RHO2)/(RHO1-RHO2)
          SOUNDSP(I) = A*SQRT(C1/RHO1) + (UN-A) * SQRT(GAM*P0/RHO2) 
        ENDIF 
         
      ENDDO


      RETURN

      END
