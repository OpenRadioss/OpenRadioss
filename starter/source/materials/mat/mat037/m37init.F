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
Chd|  M37INIT                       source/materials/mat/mat037/m37init.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE M37INIT(
     .               IPM     ,PM        ,
     .               NGL     ,NUVAR     ,UVAR    ,UPARAM    ,X       ,
     .               MAT     ,IPARG     ,IFORM   ,IX        ,NIX     ,
     .               ILOADP    ,FACLOAD ,GBUF      ,NEL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is initializing cell based on law37
C In case of /LOAD/HYDRO is attached to it then
C   Pressure is computed so that P(t=0) = P0 + DP 
C     where DP=rho0.g.h
C     and h is signed distance from water/air free surface
C     which is defined by B and n in input
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IPM(NPROPMI,NUMMAT),NGL(MVSIZ)      ,MAT(MVSIZ), IPARG(NPARG)   ,IFORM,NIX,IX(NIX,*)
      my_real :: PM(NPROPM,NUMMAT),X(3,NUMNOD), UVAR(LLT,NUVAR)
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*),NEL
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(G_BUFEL_), INTENT(INOUT),TARGET :: GBUF  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: I,J,NUVAR, GG1, GG2, GG3, ISFLUID,NL
      INTEGER  :: NPH,IFLG,IE,IADBUF,NUPARAM, M_UID,M_IID,IGRAV
      my_real  :: MFRAC, A, MU1P1, MU1P2, RHO1, RHO2, RHO10, RHO20, C1, GAM, PMIN, P0, MU2P1, RHO,P,A1,PSH,DEPTH,Z(3)
      my_real  :: UPARAM(*),XBAS,YBAS,ZBAS,NX,NY,NZ,GRAV, BZ(3),DOTPROD, DELTA_P, RHO0_LIQ,RHO0_GAS, RHO_LIQ,RHO_GAS
      my_real  :: POLD,HEIGHT
      my_real  :: N(3,8)
C-----------------------------------------------

      !---------------------------------!
      ! CELL INIT.   (UVAR ARRAY)       !
      !---------------------------------!
      !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
      !UVAR(I,2) : density of gas
      !UVAR(I,3) : density of liquid
      !UVAR(I,4) : volumetric fraction of liquid
      !UVAR(I,5) : volumetric fraction of gas
      DO I=LFT,LLT
        IE        =  I+NFT
        C1        =  UPARAM(4)
        GAM       =  UPARAM(5)
        PMIN      =  UPARAM(8)
        P0        =  UPARAM(9)
        RHO10     =  UPARAM(11)
        RHO20     =  UPARAM(12)
        A1        =  UPARAM(10)         
        P         = -P0  
        RHO       =  RHO10 * A1 + (ONE-A1)*RHO20     
        IF(GAM*C1>=EM30)THEN !if Liquid and gas correctly defined
          MU1P1     = ONE       !(P/C1+ONE)
          MU2P1     = ONE       !( ONE+P/P0)**(ONE/GAM)
          RHO1      = RHO10*MU1P1
          RHO2      = RHO20*MU2P1
          A         = (RHO-RHO2)/(RHO1-RHO2)
          UVAR(I,1) = A*RHO1
          UVAR(I,2) = RHO2
          UVAR(I,3) = RHO1
          UVAR(I,4) = A
          IF(UVAR(I,4)<EM20)UVAR(I,4)=ZERO
          UVAR(I,5) = ONE-UVAR(I,4)
          !SOUNDSP(I)  = A*SQRT(C1/RHO1) + (ONE-A) * SQRT(GAM*P0/RHO2)  -> check sigeps37.F (starter)
        ELSE !boundary element
          UVAR(I,3) = RHO
        ENDIF
      ENDDO

C-----------------------------------------------
      RETURN
      END SUBROUTINE M37INIT
