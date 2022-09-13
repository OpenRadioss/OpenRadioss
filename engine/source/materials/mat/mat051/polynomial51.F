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
Chd|  POLYUN51                      source/materials/mat/mat051/polynomial51.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLYUN51 (
     .    C0      ,C1     ,C2      ,C3       ,C4     , C5   ,GG,
     .    VOLUME  ,DVOL   ,VOLD    ,V        ,
     .    RHO     ,MASA   ,RHOA0   ,RHOA     ,DD     , MU   ,MUP1,
     .    POLD    ,PEXT   ,P       ,PM       ,PP     , Q    ,QOLD ,QQ ,
     .    RHO0E   ,EINTA  ,SOUNDSP ,VISCMAX  ,XL     , SSP, 
     .    QA      ,QB     ,E_INF   ,IPLA1    ,UPARAM , EPX )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real :: C0,C1,C2,C3,C5,C4,GG,VOLUME,DVOL,VOLD,RHO,MASA ,RHOA0,DD,MU, POLD,Q,QOLD,PEXT,P,PM
      my_real :: RHO0E,EINTA ,SOUNDSP,VISCMAX,XL,RHOA,V,SSP,PP,QQ, QA,QB, E_INF,UPARAM(*),EPX,PNE1
      INTEGER :: IPLA1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ibug
      my_real :: AA,BB,DPDV,DPDMU,V0,QAL,QBL,MUP1,MUMX,G43,BULK, BULK2
C------------------------
      MUMX    = UPARAM(20)
      BULK    = UPARAM(21)
      BULK2   = UPARAM(21) 
C------------------------
      V0      = MASA / RHOA0
      DVOL    = VOLUME - VOLD
      RHO     = MASA/VOLUME
      MUP1    = RHO/RHOA0
      MU      = MUP1 - ONE

      DPDV    = (-(C1 + (C2 + C2 + THREE*C3*MU)*MAX(ZERO,MU) + C5*RHO0E )*MUP1*MUP1 -(C4 + C5*MU)*(POLD+PEXT) )   /   V0
!      IF(DPDV>=ZERO)THEN
!        DPDV  = (-(C1 + (C2 + C2 + THREE*C3*MU)*MAX(ZERO,MU) + C5*RHO0E )*MUP1) / V0
!      ENDIF

      DPDMU   = -DPDV*VOLUME/MUP1
      DPDMU   = ABS(DPDMU)
      SOUNDSP = SQRT((DPDMU + TWO_THIRD*GG)/RHOA0)
      QAL     = QA*XL
      QAL     = QAL*QAL
      QBL     = QB*XL
      VISCMAX = RHO*(QAL*MAX(ZERO,DD) + QBL*SOUNDSP)
      Q       = VISCMAX*MAX(ZERO,DD)
      AA      = (C4 + C5*MU)/V0
      BB      = HALF*(VOLUME-VOLD)
! EINTA   = EINTA - (POLD+PEXT+PEXT)*BB

      P       = ( C0 + C1*MU+ MAX(MU,ZERO)*(C2*MU + C3*MU*MU) + AA*EINTA )!  /  (ONE+AA*BB)
      P       = MAX(P,PM)      
! EINTA   = EINTA - P*BB          
! EINTA   = MAX(EINTA, E_INF*V0)  


      DPDV    = (-(C1 + (C2 + C2 + THREE*C3*MU)*MAX(ZERO,MU) + C5*RHO0E )*MUP1*MUP1 -(C4 + C5*MU)*(P+PEXT) )   /   V0
      DPDMU   = -DPDV*VOLUME/MUP1
      DPDMU   = ABS(DPDMU)
      SOUNDSP = SQRT((DPDMU + TWO_THIRD*GG)/RHOA0)

      RHOA    = RHO
      V       = VOLUME
      SSP     = SOUNDSP
      PP      = P
      QQ      = Q

      RETURN
      END
Chd|====================================================================
Chd|  DPDV_POLY51                   source/materials/mat/mat051/polynomial51.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DPDV_POLY51 (C01,C11,C21,C31,TC31,C41,C51,
     .          V10,V1,V1OLD,MU1,EINT1,RHO0E1,
     .          Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,DVDP1,DPDV1,
     .          IPLA1, UPARAM, EPX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   C01,C11,C21,C31,TC31,C41,C51,
     .   V10,V1,V1OLD,MU1,EINT1,RHO0E1,
     .   Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .   RHO1,RHO10,MAS1,DVDP1,DPDV1,MUP1,
     .   UPARAM(*),EPX
      INTEGER IPLA1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug
      my_real 
     .   AA,BB,BULK,BULK2,MUMX
C------------------------
      MUMX  = UPARAM(20)
      BULK  = UPARAM(21)
      BULK2 = UPARAM(21)
C------------------------
      RHO1=MAS1/V1
      MUP1=RHO1/RHO10
      MU1 =MUP1 - ONE

      AA = (C41 + C51*MU1)/V10
c pression sur ancien volume 
      P1I     = ( C01 + C11*MU1+ MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1)+ AA*EINT1 )
      P1I     = MAX(P1I,PM1)
      DPDV1   = (-(C11 + (C21 + C21 + TC31*MU1)*MAX(ZERO,MU1) + C51*RHO0E1 )*MUP1*MUP1 -(C41 + C51*MU1)*(P1I+PEXT) )  /    V10
!      IF(DPDV1>=ZERO)THEN
!        DPDV1 = (-(C11 + (C21 + C21 + TC31*MU1)*MAX(ZERO,MU1) + C51*RHO0E1 )*MUP1*MUP1) / V10
!      ENDIF
     
      IF(ABS(DPDV1)<EM20)THEN
        DVDP1 = ZERO
      ELSE
        DVDP1 = ONE/DPDV1
      ENDIF

      RETURN
      END
      
      
      
      
Chd|====================================================================
Chd|  POLY51                        source/materials/mat/mat051/polynomial51.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLY51 (C01,C11,C21,C31,C41,C51,GG1,
     .          VOLUME,V10,V1,V1OLD,MU1,MUP1,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E_INF,
     .          IPLA1, UPARAM, EPX ,FLAG, GRUN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   C01,C11,C21,C31,C41,C51,GG1,
     .   VOLUME,V10,V1,V1OLD,MU1,EINT1,
     .   P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .   RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,MUP1,E_INF,
     .   UPARAM(*),EPX,GRUN
      INTEGER IPLA1, FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ibug
      my_real 
     .   AA,BB,DVDP1I,
     .   BULK,BULK2,MUMX,PNE1
C------------------------
      MUMX  = UPARAM(20)
      BULK  = UPARAM(21)
      BULK2 = UPARAM(21) 
C------------------------
      DVDP1I = DVDP1
      IF (FLAG == 1) RHO1   = MAS1/V1
      MUP1   = RHO1/RHO10
      MU1    = MUP1 - ONE
      AA     = (C41 + C51*MU1)/V10
      BB     = HALF*(V1-V1OLD)
      IF (FLAG == 1)  EINT1  = EINT1 - (P1I+PEXT+PEXT)*BB
      IF (FLAG == 1) THEN
         P1 = ( C01 + C11*MU1 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + AA*EINT1 )  /  (ONE+AA*BB)
      ELSE
         P1 = ( C01 + C11*MU1 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + AA*EINT1 )
      ENDIF
      P1     = MAX(P1,PM1)
      IF (FLAG == 1)  EINT1  = EINT1 - P1*BB
      IF (FLAG == 1)  EINT1  = MAX(EINT1, E_INF*V10) 
      
      DPDV1  = (-(C11 + (C21 + C21 + THREE*C31*MU1)*MAX(ZERO,MU1) + C51*EINT1/V10 )*MUP1*MUP1 -(C41 + C51*MU1)*(P1+PEXT) ) /  V10 
      SSP1   = (-DPDV1*V1/MUP1 + TWO_THIRD*GG1)/RHO10
      SSP1   = SQRT(ABS(SSP1))
            
      GRUN = RHO10 / RHO1 * (C41 - C51) + C51
      !----------------------------------------------------------------!
      ! Linearizing dvdp (iterative Newton convergence with order 1)   !
      ! *   dvdp = [V(k)-V(k-1)] / [P(k)-P(k-1)]                       !
      ! *   dvdp < 0                                                   !
      ! *  |dvdp| is adjusted for stability                            !
      !----------------------------------------------------------------!
      !                                                                !
      !   2 dV/dP (k)                 0.5 dV/dP (k)               0    !
      !  ______|___________________________|______________________|____!
      !        |<-------dV/dP (k+1)------->|                           !
      !----------------------------------------------------------------!
!      IF(ABS(V1 - V1OLD)/VOLUME > EM06.AND.ABS(P1-P1I)>EM20)THEN
!        DVDP1 = (V1 - V1OLD) / (P1-P1I)
!      ENDIF
 
      IF(ABS(DPDV1)<EM20)THEN
        DVDP1 = ZERO
      ELSE
        DVDP1 = ONE/DPDV1
      ENDIF
      
      !   |dV/dP| is adjust for stability
      IF(DVDP1<TWO*DVDP1I)THEN
!!!        DVDP1 = TWO*DVDP1I

Cts   In some case of slow and difficult convergency it could happen :
Cts   |DVDP(k=NITER)| = 2^NITER |DVDP(k=0)|  => |P(k=NITER)| >> |P(k=0)|
Cts   unstable      ELSEIF(DVDP1>ZERO)THEN
Cts   unstable        DVDP1 = TWO*DVDP1I

      ELSEIF(DVDP1>HALF*DVDP1I)THEN
!!        DVDP1 = HALF*DVDP1I
      ENDIF
      
!      IF(DVDP1/=ZERO)THEN
!         DPDV1 = ONE / DVDP1
!      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  POLY51_EINT                   source/materials/mat/mat051/polynomial51.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLY51_EINT (C01,C11,C21,C31,C41,C51,GG1,
     .          VOLUME,V10,V1,V1OLD,MU1,MUP1,EINT1,
     .          P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .          RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1, E_INF,
     .          IPLA1, UPARAM, EPX ,FLAG, GRUN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .   C01,C11,C21,C31,C41,C51,GG1,
     .   VOLUME,V10,V1,V1OLD,MU1,EINT1,
     .   P1OLD,Q1,Q1OLD,PEXT,P1,PM1,P1I,
     .   RHO1,RHO10,MAS1,SSP1,DVDP1,DPDV1,MUP1,E_INF,
     .   UPARAM(*),EPX,GRUN
      INTEGER IPLA1, FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .   AA
      MUP1   = RHO1/RHO10
      MU1    = MUP1 - ONE
      GRUN = RHO10 / RHO1 * (C41 - C51) + C51
      IF (GRUN == ZERO) THEN
         RETURN
      ELSE
         AA     = (C41 + C51*MU1)/V10
         !P1 = ( C01 + C11*MU1 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1) + AA*EINT1 )

         EINT1 = (P1 - (C01 + C11*MU1 + MAX(MU1,ZERO)*(C21*MU1 + C31*MU1*MU1))) / AA
         DPDV1  = (-(C11 + (C21 + C21 + THREE*C31*MU1)*MAX(ZERO,MU1) + C51*EINT1/V10 )*MUP1*MUP1 -(C41 + C51*MU1)*(P1+PEXT) ) /  V10 
         SSP1   = (-DPDV1*V1/MUP1 + TWO_THIRD*GG1)/RHO10
         SSP1   = SQRT(ABS(SSP1))
      ENDIF

      RETURN
      END
