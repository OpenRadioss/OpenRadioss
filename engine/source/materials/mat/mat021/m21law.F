Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    m21law         ../engine/source/materials/mat/mat021/m21law.F
      !||--- called by ------------------------------------------------------
      !||    mmain          ../engine/source/materials/mat_share/mmain.F90
      !||--- calls      -----------------------------------------------------
      !||    finter         ../engine/source/tools/curve/finter.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod   ../common_source/modules/constant_mod.F
      !||====================================================================
      SUBROUTINE M21LAW (
     1           PM      ,OFF     ,SIG     ,EINT   ,RHO     ,
     2           EPSQ    ,EPXE    ,VOL     ,MAT    ,SSP     ,
     3           DVOL    ,VNEW    ,D1      ,D2     ,D3      ,
     4           D4      ,D5      ,D6      ,SOLD1  ,SOLD2   ,
     5           SOLD3   ,SOLD4   ,SOLD5   ,SOLD6  ,TF      ,
     6           NPF     ,SIGY    ,DEFP    ,IPM    ,PNEW    ,
     7           PSH     ,MU      ,SEQ_OUTPUT,NEL  ,NUMMAT  ,
     8           DPLA    ,MU_BAK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CONSTANT_MOD , ONLY : EM14, ZERO, EM20, THREE, THIRD, TWO, HALF, ONE, ONEP333, EP20
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      implicit none
C-----------------------------------------------
C   I n c l u d e d   F i l e s
C-----------------------------------------------
#include      "my_real.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPF(*),MAT(NEL),IPM(NPROPMI,NUMMAT),NEL
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT)
      my_real,INTENT(IN) :: OFF(NEL),EINT(NEL)
      my_real,INTENT(IN) :: RHO(NEL), VOL(NEL), TF(*), SEQ_OUTPUT(NEL)
      my_real,INTENT(IN) :: VNEW(NEL)
      my_real,INTENT(IN) :: D1(NEL), D2(NEL), D3(NEL), D4(NEL), D5(NEL), D6(NEL)
      my_real,INTENT(IN) :: DVOL(NEL), MU(NEL)
      my_real,INTENT(IN) :: SOLD1(NEL), SOLD2(NEL), SOLD3(NEL)
      my_real,INTENT(IN) :: SOLD4(NEL), SOLD5(NEL), SOLD6(NEL)
      my_real,INTENT(INOUT) :: MU_BAK(NEL) !< history for unloading path
      my_real,INTENT(INOUT) :: PSH(NEL) !< pressure shift
      my_real,INTENT(INOUT) :: SSP(NEL) !< sound speed
      my_real,INTENT(INOUT) :: PNEW(NEL) !< current pressure
      my_real,INTENT(INOUT) :: SIG(NEL,6) !< stress tensor
      my_real,INTENT(INOUT) :: EPXE(NEL),DEFP(NEL),EPSQ(NEL),DPLA(NEL)
      my_real,INTENT(INOUT) :: SIGY(NEL) !< yield surface
      INTEGER,INTENT(IN) :: NUMMAT !< number of aterial laws (array size)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, IFUNC, NPOINT
      my_real :: DPDM(NEL),dpdm0 !< total derivative dP/d(mu)
      my_real :: T1(NEL), T2(NEL), T3(NEL), T4(NEL),T5(NEL), T6(NEL) !< deviatoric stress tensor
      my_real :: POLD(NEL), P(NEL), PNE1, PTOT, PMIN !< pressure and derivated values
      my_real :: AJ2(NEL) !< yield criteria
      my_real :: G,GG,G43 !< shear modulus and derivated values
      my_real :: BMIN, BMAX !< minimum and maximum unload modulus (tension : bmin ,  max compaction bmax)
      my_real :: BULK(NEL) !< unload modulus
      my_real :: MUMIN, MUMAX !< volumetric strain for max compaction
      my_real :: G0(NEL),A0(NEL),A1(NEL),A2(NEL),AMAX, YIELD2(NEL) !< yield criterion parameter
      my_real :: PSTAR !< yield criterion root
      my_real :: FACY !< function scale factor
      my_real :: RHO0(NEL) !< initial density
      my_real,EXTERNAL :: FINTER !< get /FUNCT value
      my_real :: alpha
      my_real :: p_
      my_real :: SVRT
      my_real :: RATIO(NEL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine uses Drucker-Prager criteria
C F = J2 - A0 + A1*P + A2*P**2 
C to calculate deviatoric stresses.
C If F > 1 then deviatoric tensor is projected on
C Yield surface using scale factor RATIO(I).
C Pressure is calculated from compaction EoS (using user function)
C Energy integration is made in MEINT subroutine (warning : EoS not depending on internal energy)
C
C VARIABLE DEFINITIONS :
C
C G0    : YIELD ENVELOPE
C AJ2   : 2ND INVARIANT FROM DEVIATORIC TENSOR
C EPX   : OLD MU
C EPSEQ : VOLUMETRIC PLASTIC STRAIN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !----------------------------------------------------------------!
      !  PARAMETER INIT.                                               !
      !----------------------------------------------------------------!  
      MX = MAT(1)
      G = DT1*PM(22,MX)
      G43 = ONEP333*PM(22,MX)
      GG  = TWO*G
      BMIN = PM(32,MX)
      BMAX = PM(35,MX)
      MUMIN = -EP20
      MUMAX = PM(36,MX)
      PMIN = PM(37,MX)
      PSH(1:NEL) = PM(43,MX)
      PSTAR = PM(44,MX)        
      A0(1:NEL) = PM(38,MX)
      A1(1:NEL) = PM(39,MX)
      A2(1:NEL) = PM(40,MX)
      AMAX = PM(41,MX)
      RHO0(1:NEL) = PM(1,MX)
      FACY = PM(42,MX)
      IFUNC = IPM(11,MX)      
      
       !----------------------------------------------------------------!
      !  STATE INIT.                                                   !
      !----------------------------------------------------------------!   
      DO I=1,NEL
        POLD(I)=-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
      ENDDO

      !----------------------------------------------------------------!
      !  DEVIATORIC STRESS TENSOR                                      !
      !----------------------------------------------------------------!  
      DO I=1,NEL
        T1(I)=SIG(I,1)+POLD(I)
        T2(I)=SIG(I,2)+POLD(I)
        T3(I)=SIG(I,3)+POLD(I)
        T4(I)=SIG(I,4)
        T5(I)=SIG(I,5)
        T6(I)=SIG(I,6)
      ENDDO

      !----------------------------------------------------------------!
      !  PRESSURE                                                      !
      !----------------------------------------------------------------!   
      DO I=1,NEL
        P(I) = FACY*FINTER(IFUNC,MU(I),NPF,TF,DPDM(I))
        P_ = finter(IFUNC,mu_bak(i),npf,tf,dpdm0)
        !linear unload modulus
        alpha = one
        if(mumax > zero)then
          alpha=mu_bak(i)/mumax
        endif
        bulk(i) = alpha*bmax+(one-alpha)*bmin
        pne1 = p_-(mu_bak(i)-mu(i))*bulk(i)
        if(mu_bak(i) > mumin) p(i) = min(pne1, p(i))
        p(i) = max(p(i),pmin) *off(i)  
        if(mu(i) > mu_bak(i)) mu_bak(i) = min(mumax,mu(i))              
      ENDDO
      
      !----------------------------------------------------------------!
      !  SOUND SPEED                                                   !
      !----------------------------------------------------------------!
      do i=1,nel
        dpdm(i) = max(bulk(i),dpdm(i))
        DPDM(I)= G43 + MAX(BULK(I),DPDM(I))
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))             
      enddo !next i   
      
      !----------------------------------------------------------------!
      !  OUTPUT                                                        !
      !----------------------------------------------------------------!
      do i=1,nel
        p(i)=max(pmin,p(i))*off(i)
        pnew(i) = p(i)-psh(i)
      enddo !next i

      !----------------------------------------------------------------!
      !  YIELD CRITERIA IF P > PMIN                                    !
      !----------------------------------------------------------------! 
      DO I=1,NEL
        IF(P(I) < PMIN)THEN
          A0(I)=ZERO
          A1(I)=ZERO
          A2(I)=ZERO
        ENDIF
      ENDDO

      !----------------------------------------------------------------!
      !  DEVIATORIC TENSOR - ELASTIC INCREMENT                         !
      !----------------------------------------------------------------! 
      DO I=1,NEL
        SVRT = THIRD*(D1(I)+D2(I)+D3(I))
        T1(I)=T1(I)+GG*(D1(I)-SVRT)
        T2(I)=T2(I)+GG*(D2(I)-SVRT)
        T3(I)=T3(I)+GG*(D3(I)-SVRT)
        T4(I)=T4(I)+G*D4(I)
        T5(I)=T5(I)+G*D5(I)
        T6(I)=T6(I)+G*D6(I)
      ENDDO

      !----------------------------------------------------------------!
      !  YIELD SURFACE                                                 !
      !----------------------------------------------------------------!  
      DO I=1,NEL
        AJ2(I)=HALF*(T1(I)**2+T2(I)**2+T3(I)**2)+T4(I)**2+T5(I)**2+T6(I)**2
        PTOT = P(I) + PSH(I)
        G0(I) =A0(I)+A1(I)*PTOT+A2(I)*PTOT*PTOT
        G0(I)= MIN(AMAX,G0(I))
        G0(I)= MAX(ZERO,G0(I))
        IF(PTOT <= PSTAR)G0(I)=ZERO
        YIELD2(I)=AJ2(I)-G0(I)
      ENDDO

      !----------------------------------------------------------------!
      !  PROJECTION FACTOR ON YIELD SURFACE                            !
      !----------------------------------------------------------------!  
      DO I=1,NEL
        RATIO(I) = ZERO
        IF(YIELD2(I)<=ZERO .AND. G0(I)>ZERO)THEN
          RATIO(I) = ONE
        ELSE
          RATIO(I) = SQRT(G0(I)/(AJ2(I)+ EM14))
        ENDIF
      ENDDO

      !----------------------------------------------------------------!
      !  DEVIATORIC STRESS TENSOR                                      !
      !----------------------------------------------------------------!  
      DO I=1,NEL
        P(I)=P(I)*OFF(I)
        PNEW(I) = P(I)
        SIG(I,1)=RATIO(I)*T1(I)*OFF(I)
        SIG(I,2)=RATIO(I)*T2(I)*OFF(I)
        SIG(I,3)=RATIO(I)*T3(I)*OFF(I)
        SIG(I,4)=RATIO(I)*T4(I)*OFF(I)
        SIG(I,5)=RATIO(I)*T5(I)*OFF(I)
        SIG(I,6)=RATIO(I)*T6(I)*OFF(I)
        DPLA(I)=(ONE-RATIO(I))*SQRT(AJ2(I))*DT1 / MAX(EM20,THREE*G)
      ENDDO
          
      !----------------------------------------------------------------!
      !  OUTPUT / MISC                                                 !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        SIGY(I) = G0(I)     !YIELD SURFACE
        DEFP(I) = DEFP(I) + DPLA(I)
        EPXE(I) = DEFP(I)
        EPSQ(I) = MU_BAK(I) ! volumetric plastic strain
      ENDDO


      RETURN

      END
