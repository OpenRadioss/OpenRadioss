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
      FUNCTION IE_BOUND(PEXT,PM,C0,C1,C2,C3,C4,C5,E0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real IE_BOUND, PEXT,C0,C1,C2,C3,C4,C5,E0,PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real  PCRIT, P0, MU, C45,TMP, MU0, x, E0_ref, P0_ref, C1_ref
C
C----------+------+----------------------------------------------------C
C VAR      | SIZE | DEFINITION                                         C
C----------+------+----------------------------------------------------C
C IE_BOUND |  1   | LOWER LIMIT FOR RELATIVE ENERGY (NEGATIVE OR NULL) C
C PCRIT    |  1   | CRITERIA ENSURING EXISTENCE OF AN ENERGY EXTREMUM  C
C MU0      |  1   | EXPANSION STATE WHERE TO COMPUTE LOWER LIMIT       C
C          |      | AT THIS POINT PRESSURE IS ZERO AND THEN dE IS ZERO C
C P0       |  1   | SIMPLIFICATION FOR 0-DEGREE POLYNOMIAL COEFFICIENT C
C----------+------+----------------------------------------------------C
C
      P0=PEXT+C0
      MU0=-ONE
      PCRIT=ZERO
      C45=MAX(C4,C5)
      IE_BOUND=-INFINITY !default : no minimum 
      E0_ref = ZERO
      P0_ref = ZERO 
      C1_ref = ZERO

      !===================!
      !   NO MATERIAL     !
      !===================!
      IF( (C0==ZERO).AND.(C1==ZERO).AND.(C2==ZERO).AND.(C3==ZERO).AND.(C4==ZERO).AND.(C5==ZERO) )THEN
        IE_BOUND = ZERO
        RETURN      
      END IF      
      
      !===================!
      !    P(0) = 0       !
      !  =>Einf = E0      ! 
      ! dE=-(dP+Pext)dV=0 !           
      !===================!
      IF(ABS(P0+C4*E0)<EM20)THEN
        IE_BOUND = E0
        RETURN      
      END IF 
       
      !===================!
      !    PM+PEXT>0      !
      ! IE_BOUND DOES NOT !
      ! EXISTS            !
      !===================!
      IF ((PM+PEXT)>EM20) THEN
      !In this case extremum does not exists. Pressure (P+Pext) will never be zero, then dE>0 is never zero and E has no extremum. limit E(->-1) = -INFINITY
        IE_BOUND=-INFINITY         
        RETURN
      END IF
      
      
      

      !Otherwise for following cases : {PM+PEXT>0}
  
 
      !===================!
      !    PERFECT GAS    !
      !===================!
      IF((C2==ZERO).AND.(C3==ZERO).AND.(P0==C1).AND.(C4/=ZERO).AND.(C4==C5))THEN
       !this condition describes all formulation possible for a perfect gas, included relative pressure and relative energy.
        IF (C1==0)THEN
        !total energy
          IE_BOUND = ZERO
        ELSE
        !relative energy
          IE_BOUND = -P0/C4
        END IF
        RETURN
      END IF     

      !======================!
      ! INCOMPRESSIBLE GAS   !
      ! LINEAR ELASTIC SOLID !      
      !======================! 
      IF((C2==ZERO).AND.(C3==ZERO).AND.(C4==ZERO).AND.(C5==ZERO).AND.(C1/=ZERO))THEN 
        IF (P0<C1)THEN
             x=-P0/C1  !C1-P0>0 then x  > -1, extremum is well defined
             IE_BOUND = -1/(1+x)*P0+C1/(1+x)+C1*LOG(1+x)+E0+P0-C1
        ELSE
             IE_BOUND = -INFINITY  !limit E(mu=-1) = -sgn(C1) * infinity, with C1>0        
           END IF        
           RETURN
      END IF                

      !======================!
      ! MIE-GRUNEISEN        !
      ! GAMMA 1st order      !      
      !======================! 
      !  extremum estimated using one from case gamma constant

      !======================!
      ! MIE-GRUNEISEN        !
      ! GAMMA CONSTANT       !      
      !======================! 
      IF((C1>ZERO).AND.(C4>=C5).AND.(C5>ZERO))THEN
      !similar to E0=0
      !P=P0      +        C1x + C2x+C3x+(C4+C5x)(dE+E0)
      !P=P0+C4E0 + (C1+C5E0)x + C2x+C3x+(C4+C5x) dE    : solving dE then puis on ajoute E0
        E0_ref=E0
        C1_ref=C1
        P0_ref=P0
        IF(E0/=ZERO)THEN
          E0=ZERO
          P0=P0+C4*E0_ref
          C1=C1+C4*E0_ref
        END IF
      
      ! existing condition for C4/=1 et C4/=2
      IF((C4/=1).AND.(C4/=2))THEN
        !diverging test (expansion)
        IF(C1-P0<ZERO)THEN
          IE_BOUND=-INFINITY
          GOTO 100
        END IF
        !diverging test (compression)
        x=(-4*C2+2*C1+2*C4*P0+2*C4*C2-3*C4*C1-3*C4**2*P0+6*C3+C4**2*C1+C4**3*P0)/(2+C4**2-3*C4)
        IF(x<ZERO)THEN
          IE_BOUND=-INFINITY
          GOTO 100
        END IF
        !expansion extremum
        IF(P0>=ZERO)THEN
          IF (C1==P0)THEN
            IE_BOUND=-C1/C4   !limite en mu=-1
            GOTO 100
          ELSE !extremum in  ] -1 et 0 [
            x=(C4*P0+C1)/(C1-P0)  !necessarily positive
            x=exp(-LOG(x)/(1+C4))-1
            IE_BOUND=(-(1+x)**(-1-C4)*(C4*P0+C1+C1*x+C4*C1*x) / C4/(1+C4)+(C4*P0+C1)/C4/(1+C4))*(1+x)**C4
            GOTO 100
            END IF !(C1==P0)
          END IF !(P0>=ZERO)
        !extremum in compression
        IF(P0<ZERO)THEN
        !extremum estimation in compression (C2,C3 needed) :
        IE_BOUND=-INFINITY ! ameliorer
          !find an estimator
          !fixed point method ?
          GOTO 100
        END IF
        
      ELSEIF((C4==ONE).OR.(C4==TWO))THEN
        !diverging test in expansion
        IF(C1-P0<ZERO)THEN
          IE_BOUND=-INFINITY
          GOTO 100
        END IF
        !diverging test in compression
          !diverge if dominant coefficient is negative
        IF (C3<ZERO)THEN
          IE_BOUND = -INFINITY
          GOTO 100
        END IF
          !diverge if dominant coefficient is negative
        IF(C3==ZERO)THEN
          IF (C2<ZERO)THEN
            IE_BOUND=-INFINITY
            GOTO 100
          END IF
        END IF
        ! otherwise extremum does exist
        ! expansion if P0>0
        IF(P0>=ZERO)THEN
           !C1**2-P0^2 is here necessarily positive because C1-P0>0 C1>0 P0>0
          x=-(C1+P0-SQRT(C1**2-P0**2))/(C1+P0)
          IE_BOUND=(1+x)*(1/2*C1+1/2*P0)-1/2*(C1+2*C1*x+P0)/(1+x)
          GOTO 100
        END IF
        ! compression
        IF(P0<ZERO)THEN
        !extremum estimation in compression (C2,C3 needed) :
        IE_BOUND=-INFINITY ! to improve
          !find an estimator
          !fixed point method ?
          GOTO 100
        END IF
      END IF !condition for C4 (1 or 2)
      END IF !mie gruneisen gamma constant
      
  100 IF (E0_ref/=ZERO)THEN 
        IF(IE_BOUND/=-INFINITY)IE_BOUND=IE_BOUND+E0_ref   
        C1=C1_ref 
        E0=E0_ref
      END IF
      
      RETURN

      END FUNCTION  
