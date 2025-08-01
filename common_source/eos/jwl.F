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
!||    jwl       ../common_source/eos/jwl.F
!||--- called by ------------------------------------------------------
!||    eosmain   ../common_source/eos/eosmain.F
!||====================================================================
      SUBROUTINE JWL
     1              (IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2 , 
     2               ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3               PNEW ,DPDM ,DPDE )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of JWL EOS
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .        OFF(NEL)  ,EINT(NEL) ,MU(NEL)   , 
     .        MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  , 
     .        VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .        DPDE(NEL)
      my_real, INTENT(INOUT) :: PSH(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IBFRAC
      my_real VDET,BFRAC(NEL),
     .        RHO0 , AA , BB , R1,
     .        R2, W1,  BHE, P0, BULK,
     .        R1DF,R2DF,ER1DF,ER2DF,dPdmu
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      IF(IFLAG == 0) THEN

      ELSEIF(IFLAG == 1) THEN

      ELSEIF(IFLAG == 2) THEN
         MX     = MAT(1)
         RHO0   = PM( 1,MX)        
         AA     = PM(33,MX)        
         BB     = PM(34,MX)        
         R1     = PM(35,MX)        
         R2     = PM(36,MX)        
         W1     = PM(45,MX)        
         VDET   = PM(38,MX)        
         BHE    = PM(40,MX)        
         P0     = PM(31,MX)        
         BULK   = PM(44,MX)        
         IBFRAC = NINT(PM(41,MX))  
         PSH(1:NEL) = PM(88,MX)
         DO I=1,NEL
           BFRAC(I)=ONE
         ENDDO
         DO I=1, NEL                                                                                        
           IF (VNEW(I) > ZERO) THEN
             R1DF = R1*DF(I)
             R2DF = R2*DF(I)
             ER1DF = EXP(-R1DF)
             ER2DF = EXP(-R2DF)
             PNEW(I) =  - PSH(I) + AA*(ONE-W1/R1DF)*ER1DF + BB*(ONE-W1/R2DF)*ER2DF + W1*ESPE(I)/DF(I)
             PNEW(I) = MAX(ZERO - PSH(I), PNEW(I))
             ! dPdE  : partial derivative
             ! dPdmu : partial derivative
             ! DPDM  : total derivative
             dPdE(I) = W1/DF(I)
             dPdmu   =
     .             -AA*W1*ER1DF/R1 + AA*(ONE-W1/(R1DF))*R1DF*R1DF*ER1DF
     .             -BB*W1*ER2DF/R2 + BB*(ONE-W1/(R2DF))*R2DF*R2DF*ER2DF
     .             +W1*ESPE(I)
             DPDM(I) = dPdmu + (PNEW(I)+PSH(I))*DF(I)*DF(I)*dPdE(I)
           ENDIF
         ENDDO                                                                                              
      ENDIF
      
C-----------------------------------------------
      RETURN
      END
