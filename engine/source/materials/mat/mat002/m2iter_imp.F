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
Chd|  M2ITER_IMP                    source/materials/mat/mat002/m2iter_imp.F
Chd|-- called by -----------
Chd|        M2LAW                         source/materials/mat/mat002/m2law.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M2ITER_IMP(
     1   SIG,     EPXE,    AJ2,     G,
     2   CA,      CB,      CN,      EPD,
     3   SIGMX,   EPMX,    DPLA1,   AK,
     4   QH,      SIGY,    FISOKIN, NEL)
c--------------------------------------------------------------
c
c    -------------------
c    -- parameters in --
c    -------------------
c
c    SIG   = Elastic Predictor (Deviatoric Stress Tensor)
c    EPXE  = previous time-step equivalent plastic strain  (on input)
c    AJ2   = von Mises stress at elastic predictor
c    G     = Kirhhoff's modulus (elastic shear modulus))
c
c    CA, CB, CN = coefficients of the hardening rule
c                 Sigma_Yield =   CA + CB * (equiv_pl_strn)**CN
c    EPD   = scalling fac due to velocity effect
c    SIGMX = max allowable von Mises stress   
c    EPMX  = max allowable equivalent plastic strain
c    FISOKIN= Mixed hardening ratio
c    --------------------
c    -- parameters out --      
c    --------------------
c
c    DPLA1 = equivalent plastic strain increment
c    EPXE  = updated  equivalent plastic strain (on output)
c    AK    = current yield stress
c    SIGY  = current yield stress (same as AK)
c    QH    = current hardening parameter
c
c--------------------------------------------------------------
c qz !! this subroutine will be optimized after
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL
C     REAL
      my_real
     .   SIG(NEL,6), EPXE(*),EPD(*),
     .   G(*), AK(*),QH(*), AJ2(*), 
     .   CA(*), CB(*), CN,FISOKIN,
     .   SIGMX(*),DPLA1(*),EPMX  ,SIGY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITER, NITER
C
C     REAL
      MY_REAL
     .   XSI,DXSI,LHS,RHS,ALPHA_RADIAL,BETA,G3(MVSIZ),HTOT
C
c ---- max number of iterations ----
       NITER = 10
C       
       DO 100 I=1,NEL
C
C------------------------------------------
C
       XSI = ZERO
C
C      -- PREVIOUS TIME-STEP EQUIVALENT PLASTIC STRAIN
       EPXE(I) = MAX(ZERO,EPXE(I))
       G3(I) =MAX(THREE*G(I),EM15)
C
C      -- PREVIOUS TIME-STEP YIELD STRESS !!!!! if EPXE=0!!!!!
C       AK(I)=CA(I)+CB(I)*EPXE(I)**CN(I) !..SIGMA0(EPS_P)
C
C      -- COMPARE IT WITH ELASTIC PREDICTOR
       IF(AJ2(I)<AK(I)) GO TO 90 
C
       BETA = ONE-FISOKIN
       DO 80 ITER = 1,NITER ! .. NR ITERATIONS ..
C
       IF(EPXE(I)>ZERO) THEN !..TESTING FOR ZERO EQUIV.PL.STRN
         AK(I)=CA(I)+BETA*CB(I)*EPXE(I)**CN !..SIGMA0(EPS_P)
C
C      --COMPUTE THE HARDENING PARAMETER H
C      --(JUST THE DERIVATIVE OF THE SIG-EPS_PL FUNCTION)
C
         IF(CN>ONE) THEN
           QH(I) = (CB(I)*CN*EPXE(I)**(CN - ONE))*EPD(I)
         ELSEIF(CN==ONE) THEN !.. LINEAR HARDENING
           QH(I)= CB(I)*EPD(I)
         ELSE !.. POWER LESS THAN UNITY
           QH(I) = (CB(I)*CN/EPXE(I)**(ONE -CN))*EPD(I)
         ENDIF
C
       ELSEIF(EPXE(I)==ZERO) THEN
C
         AK(I)=CA(I)
C
         IF(CN>ONE )THEN
               QH(I) = ZERO
         ELSEIF(CN==ONE) THEN 
               QH(I) = CB(I)*EPD(I)
         ELSE
               QH(I) = EP10 *EPD(I) !.. SHOULD BE +INFINITY
         ENDIF
C
       ELSE !--ERROR
CCC             WRITE(*,*)'M2LAW-NEGATIVE EQUIVALENT PLASTIC STRN',EPXE(I)
CCC             CALL ARRET(2)
C
       ENDIF !--EPXE(I)>ZERO ..END TESTING FOR ZERO EQUIV.PL.STRN
C
       HTOT = G3(I) + FISOKIN*QH(I)
       RHS = AJ2(I) -HTOT * XSI - AK(I)
       LHS = G3(I) +  QH(I)
       DXSI = RHS/LHS
       XSI = XSI + DXSI
C
       EPXE(I) = EPXE(I) + DXSI 
       EPXE(I) = MAX(ZERO,EPXE(I))
          IF( ABS(DXSI)<EM10.AND.
     $        ABS(RHS )<EM10) GO TO 90
C
 80    CONTINUE
CCC      WRITE(*,*)'M2LAW--NON-CONVERGED ITERATION', ABS(DXSI),ABS(RHS)
 90    CONTINUE
C
       IF(XSI<ZERO) XSI = ZERO
       DPLA1(I) = XSI !.. PLASTIC STRAIN MULTIPLIER "delta lambda"
CCC       IF(I==1)WRITE(*,*)'D_LAMBDA =',XSI
C
C     .. ENFORCING SOME CUT-OFFS, LEFT UNCHANGED
C  ----!!!cette partie devrait etre   interieure de boucle ---  tester
       AK(I)=AK(I)*EPD(I)
       IF(SIGMX(I)<AK(I))THEN
        AK(I)=SIGMX(I)
        QH(I)=ZERO
       ENDIF
C       SIGY(I) = AK(I)
       IF(EPXE(I)>EPMX)THEN
         AK(I)=ZERO
         QH(I)=ZERO
       ENDIF
C
C     -- RADIAL RETURN ------
C     -- ALPHA_RADIAL := PARAMETER ALPHA OF THE RADIAL RETURN
C
       ALPHA_RADIAL= MIN(ONE,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=ALPHA_RADIAL*SIG(I,1)
       SIG(I,2)=ALPHA_RADIAL*SIG(I,2)
       SIG(I,3)=ALPHA_RADIAL*SIG(I,3)
       SIG(I,4)=ALPHA_RADIAL*SIG(I,4)
       SIG(I,5)=ALPHA_RADIAL*SIG(I,5)
       SIG(I,6)=ALPHA_RADIAL*SIG(I,6)
C
 100  CONTINUE                  !..LOOP ON ELEMENTS
C
      RETURN
      END
