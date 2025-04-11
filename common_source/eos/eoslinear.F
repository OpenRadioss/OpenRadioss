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
      !||    eoslinear   ../common_source/eos/eoslinear.F
      !||--- called by ------------------------------------------------------
      !||    eosmain     ../common_source/eos/eosmain.F
      !||====================================================================
      SUBROUTINE EOSLINEAR(IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU   ,
     2                     DVOL ,VNEW ,MAT  ,PSH  ,PNEW ,DPDM ,
     3                     DPDE )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
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
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .        OFF(NEL) ,EINT(NEL) ,MU(NEL)   , 
     .        DVOL(NEL) ,VNEW(NEL),
     .        PNEW(NEL),DPDM(NEL) ,DPDE(NEL)
      my_real,INTENT(INOUT) :: PSH(NEL)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real BULK(NEL),PC(NEL),C0(NEL)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IF(IFLAG == 0) THEN
        DO I=1,NEL
          MX = MAT(I)
          BULK(I) = PM(32,MX)
          C0(I) = PM(104,MX)
          PC(I) = PM(37,MX)
          PSH(I) = PM(88,MX)
        ENDDO
        DO I=1,NEL
          DPDM(I) = BULK(I)
          DPDE(I) = ZERO
          PNEW(I) = C0(I)+BULK(I)*MU(I)
          PNEW(I) = MAX(PC(I)-PSH(I),PNEW(I))        !law10 : yield criteria depends on P
          PNEW(I) = PNEW(I)*OFF(I)
        ENDDO

      ELSEIF(IFLAG == 1) THEN
        DO I=1,NEL
          MX     = MAT(I)
          BULK(I)= PM(32,MX)
          PC(I)  = PM(37,MX)
          C0(I)  = PM(104,MX)
          PSH(I) = PM(88,MX)
        ENDDO
        DO I=1,NEL
          PNEW(I) = C0(I)+BULK(I)*MU(I)
          PNEW(I) = MAX(PNEW(I),PC(I)-PSH(I))
          PNEW(I) = PNEW(I)*OFF(I)
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I))
          DPDE(I) = ZERO
        ENDDO


      ELSEIF (IFLAG == 2) THEN
        DO I=1, NEL                                
           MX = MAT(I)  
           C0(I) = PM(104,MX)                                    
           BULK(I) = PM(32,MX)
           PC(I) = PM(37,MX)
           PSH(I) = PM(88,MX)                       
        ENDDO                                      
        DO I=1, NEL                                
          IF (VNEW(I) > ZERO) THEN
            PNEW(I) = C0(I)+BULK(I)*MU(I)
            PNEW(I) = MAX(PNEW(I),PC(I)-PSH(I))
            PNEW(I) = PNEW(I)*OFF(I)
            DPDM(I) = BULK(I)
            DPDE(I) = ZERO
          ENDIF
        ENDDO                                      

      ENDIF
C-----------------------------------------------
      RETURN
      END
