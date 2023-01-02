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
Chd|  LAG_MULT_SOLV                 source/tools/lagmul/lag_mult_solv.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        LAG_MULT_H                    source/tools/lagmul/lag_mult_h.F
Chd|        PRECHOL                       source/tools/lagmul/cholfact.F
Chd|        CHOLFACT                      source/tools/lagmul/cholfact.F
Chd|====================================================================
      SUBROUTINE LAG_MULT_SOLV(
     1                    NH    ,NC    ,NCR   ,A     ,V     ,
     2                    MAS   ,IADLL ,LLL   ,JLL   ,XLL   ,
     3                    IADH  ,JCIH  ,HH    ,Z     ,P     ,
     4                    R     ,Q     ,LTSM  ,HL    ,DIAG_H,
     5                    DIAG_L,WORK1 ,WORK2 ,WORK3 ,LAMBDA,
     6                    RBYL  ,NPBYL ,AR    ,VR    ,IN    ,
     7                    IADHF ,JCIHF ,ICFTAG,JCFTAG,NCF_S ,
     8                    NCF_E )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,NCR,NCF_S,NCF_E,NH 
      INTEGER LLL(*),JLL(*),IADLL(*),IADH(*),JCIH(*),IADHF(*),
     .        JCIHF(*),NPBYL(NNPBY,*),WORK2(*),WORK3(*),
     .        ICFTAG(*),JCFTAG(*)
C     REAL
      my_real
     .  MAS(*),IN(*), A(3,*),AR(3,*),V(3,*),VR(3,*),
     .  XLL(*),P(*),R(*),Q(*),LAMBDA(*),RBYL(NRBY,*),
     .  LTSM(6,*),Z(*),HH(*),HL(*),DIAG_H(*),DIAG_L(*),
     .  WORK1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,IC,JC,IH,IP,ITER,NITER,ITERMAX,NIN,LENH
      my_real
     .   AS,ASMAX,ALPHA,BETA,R2,R2NEW,RNORM,PQ,HIJ,SCALE,DD
C-----------------------------------------------
C    External Functions
C-----------------------------------------------
      INTEGER  CHOLFACT
      EXTERNAL CHOLFACT
C-----------------------------------------------
C-----------------------------------------------
C
C       | M | Lt| | a    | M ao
C       |---+---| |    = |
C       | L | 0 | | la   | bo
C
C        [M] a + [L]t la = [M] ao
C        [L] a = bo
C
C         a = -[M]-1[L]t la + ao
C        [L][M]-1[L]t la  = [L] ao - bo
C
C        [H] = [L][M]-1[L]t
C        b  = [L] ao - bo
C        [H] la = b
C
C        a = ao - [M]-1[L]t la
C-----------------------------------------------
C
C        la              : LAMBDA(NC)
C        ao              : A(3,NUMNOD)
C        [M](i)          : MAS(NUMNOD)
C        [L][M]-1[L]t la : Q(NC)
C        [L] ao - b      : B(NC)
C        [M]-1[L]t       : LTSM(3,NUMNOD) une colonne de [M]-1[L]t
C                        : Z(NC)
C
C        [L](ic,j,i)    -> XLL(IK) 
C        ic = 1..nc      : IK = IADLL(IC)...IADLL(IC+1)-1
C        j  = 1,2,3      : J  = JLL(IK)
C        i  = 1..numnod  : I  = LLL(IK)
C
C
C        NC : nombre de contact 
C
C        IC : numero du contact (1,NC)
C        IK : index de XLL,LLL,JLL
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3
C
C-----------------------------------------------
C        [H](ic,jc)     -> HH(IH)
C        ic = 1..nc      : IH = IADH(IC)...IADH(IC+1)-1
C        jc = 1..ic      : JC = JCIH(IH)
C
C        q = [H] p
C     
C        do ic=1,nc
C          q(ic) = 0
C          do jc=1,nc
C            q(ic) = q(ic) + h(ic,jc)*p(jc)
C          enddo
C        enddo
C
C        DO IC=1,NC
C          Q(IC) = 0
C          DO IH=IADH(IC),IADH(IC+1)-1
C            Q(IC) = Q(IC) - HH(IH)*P(JCIH(IH))
C          ENDDO
C        ENDDO
C-----------------------------------------------
C  evaluation de b:
C
C         Vs = Somme(Ni Vi) + vout
C         Vs_ + dt As = Somme(Ni Vi_) + Somme(dt Ni Ai) + vout
C         Somme(dt Ni Ai) - dt As =  Vs_ -Somme(Ni Vi_) - vout
C         [L] = dt {N1,N2,..,N15,-1}
C         bo = {N1,N2,..,N15,-1} a = -[L]/dt v_ - vout
C         b  = [L] ao - bo
C         b  = [L] ao + [L]/dt v_  + vout
C         b  = [L]/dt vo+  + vout
C-----------------------------------------------------------------------
C
C     calcul de [H] la = b par gradient conjugue
C
C-----------------------------------------------------------------------
C      Gradient conjugue [H] la = b avec preconditionnement [N]
C      [H]-1 = Somme([I-H]^i),i=0,1...inf    si |I-H|<1
C      [N]-1 = [2I-H]
C-----------------------------------------------------------------------
C      r(0) = b - [H] la(0)
C      for i=1,niter
C        z(i-1) = [N]-1 r(i-1)
C        rho(i-1) = r(i-1)T z(i-1)
C        if (i == 1)
C          p(1) = z(0)
C        else
C          beta = rho(i-1) / rho(i-2)
C          p(i) = z(i-1) + beta p(i-1)
C        endif
C        q(i)  = [H] p(i)
C        alpha = rho(i-1) / p(i)T q(i)
C        la(i) = la(i-1) + alpha p(i)
C        r(i)  = r(i-1)  - alpha q(i)
C     end
C======================================================================|
      SCALE = THREE_OVER_4
      ASMAX = ZEP6
      R2NEW = ZERO      
      LAG_NC= NC
C---
C---  Calcul de la matrice H
      CALL LAG_MULT_H(NC     ,LENH   ,NH     ,MAS    ,IN     ,
     2                DIAG_H ,HH     ,IADLL  ,LLL    ,JLL    ,
     3                XLL    ,LTSM   ,IADHF  ,JCIHF  ,IADH   ,
     4                JCIH   ,RBYL   ,NPBYL  ,ICFTAG ,JCFTAG ,
     5                NCF_S  ,NCF_E  ,NCR    )
C------------------------------------------
C     Normalisation du systeme
c     lagopt=0 => sans norme
c     lagopt=1 => norme L1 (diagonal)
c     lagopt=2 => norme L2 (columns)
C------------------------------------------
      IF (LAGOPT==1) THEN
        DD = ZERO
        DO IC=1,NC
          DD = DD + DIAG_H(IC)*DIAG_H(IC)
        ENDDO
        DO IC=1,NC
          Z(IC) = DD
        ENDDO
      ELSEIF (LAGOPT==2) THEN
        DO IC=1,NC
          Z(IC) = DIAG_H(IC)*DIAG_H(IC)
        ENDDO
        DO IC=1,NC
          DO IH=IADH(IC),IADH(IC+1)-1
            JC  = JCIH(IH)
            HIJ = HH(IH)*HH(IH)
            Z(IC) = Z(IC) + HIJ
            Z(JC) = Z(JC) + HIJ
          ENDDO
        ENDDO
      ENDIF
C---
      IF (LAGOPT>0) THEN
        DO IC=1,NC
          Z(IC) = SCALE/SQRT(SQRT(Z(IC)))
        ENDDO
C---      normalisation [H]        Somme([H](ic,i)^2)=1   i=1...nc
        DO IC=1,NC
          DIAG_H(IC) = DIAG_H(IC)*Z(IC)*Z(IC)
          DO IH=IADH(IC),IADH(IC+1)-1
            HH(IH) = HH(IH)*Z(IC)*Z(JCIH(IH))
          ENDDO
        ENDDO
C---      normalisation [LLL] et {R}
        DO IC=1,NC
          DO IK=IADLL(IC),IADLL(IC+1)-1
            XLL(IK)  = XLL(IK)*Z(IC)
          ENDDO
          R(IC) = R(IC)*Z(IC)
        ENDDO
      ENDIF
C------------------------------------------
C     Factorisation de [H] : Cholesky incomplet + shift iteratif
C------------------------------------------
      NITER_CHL  = 0
      IF (LAGMOD==1) THEN
        ALPHA = LAG_ALPH
        IF (LAG_ALPHS==ZERO) THEN
          DO IC=1,NC
            DIAG_L(IC) =  DIAG_H(IC)
          ENDDO
        ELSE
          DO IC=1,NC
            DIAG_L(IC) =  DIAG_H(IC) + LAG_ALPHS
          ENDDO
        ENDIF
        IP    = 1
        DO WHILE (IP>0)
          DO IH=1,LENH
            HL(IH) =  HH(IH)
          ENDDO
          IP = CHOLFACT(NC,DIAG_L,HL,IADH,JCIH,WORK1,WORK2,WORK3)
          NITER_CHL  = NITER_CHL+1  
          ALPHA = ALPHA*2
          AS    = ALPHA + LAG_ALPHS
          IF (AS>ASMAX) THEN
            IP = 0
            WRITE(IOUT,*) '***WARNING (LAGMULT : FACTORISATION FAILED )'
            WRITE(ISTDO,*)'***WARNING (LAGMULT : FACTORISATION FAILED )'
          ELSEIF (IP>0) THEN
            DO IC=1,NC
              DIAG_L(IC) =  DIAG_H(IC) + ALPHA + LAG_ALPHS
            ENDDO
          ENDIF
        ENDDO
        IF (NITER_CHL>1) print*,'FACTOR ITERATIONS = ',NITER_CHL
       ENDIF
C=======================================================================
C     calcul de  b (r=b)
C=======================================================================
      DO IC=1,NC
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = LLL(IK)
          J = JLL(IK)
          IF (J>3) THEN
            J = J-3
            R(IC) = R(IC) + XLL(IK)*(VR(J,I)/DT12+AR(J,I))
          ELSE
            R(IC) = R(IC) + XLL(IK)*(V(J,I)/DT12+A(J,I))
          ENDIF
        ENDDO
      ENDDO
C---  Initialisation du vecteur solution
      RNORM = ZERO
      DO IC=1,NC
        RNORM = RNORM + R(IC)*R(IC)
        P(IC) = LAMBDA(IC)
      ENDDO
C---  Residu initial   R(i) = R(i) - sum(H(i,j)*P(j))
      DO IC=1,NC
        R(IC) = R(IC) - DIAG_H(IC)*P(IC)
        DO IH=IADH(IC),IADH(IC+1)-1
          JC = JCIH(IH)
          R(IC) = R(IC) - HH(IH)*P(JC)
          R(JC) = R(JC) - HH(IH)*P(IC)
        ENDDO
      ENDDO
C=======================================================================
C     iterations
C=======================================================================
      ITERMAX = NC
      ITER    = 0
      NITER   = 0
      DO WHILE(ITER<ITERMAX)
        ITER  = ITER+1
        NITER = ITER
C--------------------
C       preconditionnement  z = inv(H) r
C--------------------
        IF (LAGMOD==1) THEN
C---    preconditionnement de Cholesky
          CALL PRECHOL(Z,DIAG_L,HL,R,NC,IADH,JCIH)
        ELSEIF(LAGMOD==2) THEN
C---    preconditionnement polynomial :  H=I-B,  z = [I+B] r    
          DO IC=1,NC
            Z(IC) = R(IC) + R(IC)
          ENDDO
          DO IC=1,NC
            Z(IC) = Z(IC) - DIAG_H(IC)*R(IC)
            DO IH=IADH(IC),IADH(IC+1)-1
              JC = JCIH(IH)
              Z(IC) = Z(IC) - HH(IH)*R(JC)
              Z(JC) = Z(JC) - HH(IH)*R(IC)
            ENDDO
          ENDDO
        ENDIF
C--------------------
        R2    = R2NEW
        R2NEW = ZERO
        DO IC=1,NC
          R2NEW = R2NEW + R(IC)*Z(IC)
          Q(IC) = ZERO
        ENDDO
C--------------------
        IF(ITER==1)THEN
          DO IC=1,NC
            P(IC) = Z(IC)
          ENDDO
        ELSE
          BETA = R2NEW/R2
          DO IC=1,NC
            P(IC) = Z(IC) + BETA*P(IC)
          ENDDO
        ENDIF
C-----------------------------------------------------------------------
C     calcul de  q = q_ + [H] p
C-----------------------------------------------------------------------
      DO IC=1,NC
        Q(IC) = Q(IC) + DIAG_H(IC)*P(IC)
        DO IH=IADH(IC),IADH(IC+1)-1
          JC = JCIH(IH)
          Q(IC) = Q(IC) + HH(IH)*P(JC)          
          Q(JC) = Q(JC) + HH(IH)*P(IC)          
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C     pt [H] p = pt q
C-----------------------------------------------------------------------
        PQ = ZERO
        DO IC=1,NC
          PQ = PQ + P(IC)*Q(IC)
        ENDDO
C-----------------------------------------------------------------------
        IF(PQ==ZERO)THEN
          ITER = ITERMAX
        ELSE
          ALPHA = R2NEW/PQ
          LAG_ERSQ2 = ZERO
          DO IC=1,NC
            LAMBDA(IC) = LAMBDA(IC) + ALPHA*P(IC)
            R(IC)      = R(IC)      - ALPHA*Q(IC)
            LAG_ERSQ2  = LAG_ERSQ2  + R(IC)*R(IC)
          ENDDO
C--------- test convergence --------------------------------------------
          LAG_ERSQ2 = LAG_ERSQ2/MAX(EM30,RNORM)
          IF(LAG_ERSQ2<LAGM_TOL)THEN
            ITER = ITERMAX
          ENDIF
        ENDIF
C=======================================================================
C     fin boucle gradient conjugue
C=======================================================================
      ENDDO
      NITER_GC = NITER
C---
C     a = ao + [M]-1[L]t la  
C---
      DO IC=1,NCR
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = LLL(IK)
          J = JLL(IK)
          XLL(IK) = XLL(IK)*LAMBDA(IC)
          IF(J>3) THEN
            J = J-3
            AR(J,I) = AR(J,I) - XLL(IK)/IN(I)
          ELSE         
            A(J,I)  = A(J,I)  - XLL(IK)/MAS(I)
          ENDIF         
        ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  LAG_MULT_SOLVP                source/tools/lagmul/lag_mult_solv.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        LAG_MULT_HP                   source/tools/lagmul/lag_mult_h.F
Chd|        PRECHOL                       source/tools/lagmul/cholfact.F
Chd|        CHOLFACT                      source/tools/lagmul/cholfact.F
Chd|====================================================================
      SUBROUTINE LAG_MULT_SOLVP(
     1                    NH    ,NC      ,NCR   ,A     ,V     ,
     2                    MAS   ,IADLL   ,LLL   ,JLL   ,XLL   ,
     3                    IADH  ,JCIH    ,HH    ,Z     ,P     ,
     4                    R     ,Q       ,LTSM  ,HL    ,DIAG_H,
     5                    DIAG_L,WORK1   ,WORK2 ,WORK3 ,LAMBDA,
     6                    RBYL  ,NPBYL   ,AR    ,VR    ,IN    ,
     7                    IADHF ,JCIHF   ,ICFTAG,JCFTAG,NCF_S ,
     8                    NCF_E ,INDEXLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "units_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,NCR,NCF_S,NCF_E,NH 
      INTEGER LLL(*),JLL(*),IADLL(*),IADH(*),JCIH(*),IADHF(*),
     .        JCIHF(*),NPBYL(NNPBY,*),WORK2(*),WORK3(*),
     .        ICFTAG(*),JCFTAG(*), INDEXLAG(*)
C     REAL
      my_real
     .  MAS(*),IN(*), A(3,*),AR(3,*),V(3,*),VR(3,*),
     .  XLL(*),P(*),R(*),Q(*),LAMBDA(*),RBYL(NRBY,*),
     .  LTSM(6,*),Z(*),HH(*),HL(*),DIAG_H(*),DIAG_L(*),
     .  WORK1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,IC,JC,IH,IP,ITER,NITER,ITERMAX,NIN,LENH,
     .        NZ
      my_real
     .   AS,ASMAX,ALPHA,BETA,R2,R2NEW,RNORM,PQ,HIJ,SCALE,DD
C-----------------------------------------------
C    External Functions
C-----------------------------------------------
      INTEGER  CHOLFACT
      EXTERNAL CHOLFACT
C-----------------------------------------------
C-----------------------------------------------
C
C       | M | Lt| | a    | M ao
C       |---+---| |    = |
C       | L | 0 | | la   | bo
C
C        [M] a + [L]t la = [M] ao
C        [L] a = bo
C
C         a = -[M]-1[L]t la + ao
C        [L][M]-1[L]t la  = [L] ao - bo
C
C        [H] = [L][M]-1[L]t
C        b  = [L] ao - bo
C        [H] la = b
C
C        a = ao - [M]-1[L]t la
C-----------------------------------------------
C
C        la              : LAMBDA(NC)
C        ao              : A(3,NUMNOD)
C        [M](i)          : MAS(NUMNOD)
C        [L][M]-1[L]t la : Q(NC)
C        [L] ao - b      : B(NC)
C        [M]-1[L]t       : LTSM(3,NUMNOD) une colonne de [M]-1[L]t
C                        : Z(NC)
C
C        [L](ic,j,i)    -> XLL(IK) 
C        ic = 1..nc      : IK = IADLL(IC)...IADLL(IC+1)-1
C        j  = 1,2,3      : J  = JLL(IK)
C        i  = 1..numnod  : I  = LLL(IK)
C
C
C        NC : nombre de contact 
C
C        IC : numero du contact (1,NC)
C        IK : index de XLL,LLL,JLL
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3
C
C-----------------------------------------------
C        [H](ic,jc)     -> HH(IH)
C        ic = 1..nc      : IH = IADH(IC)...IADH(IC+1)-1
C        jc = 1..ic      : JC = JCIH(IH)
C
C        q = [H] p
C     
C        do ic=1,nc
C          q(ic) = 0
C          do jc=1,nc
C            q(ic) = q(ic) + h(ic,jc)*p(jc)
C          enddo
C        enddo
C
C        DO IC=1,NC
C          Q(IC) = 0
C          DO IH=IADH(IC),IADH(IC+1)-1
C            Q(IC) = Q(IC) - HH(IH)*P(JCIH(IH))
C          ENDDO
C        ENDDO
C-----------------------------------------------
C  evaluation de b:
C
C         Vs = Somme(Ni Vi) + vout
C         Vs_ + dt As = Somme(Ni Vi_) + Somme(dt Ni Ai) + vout
C         Somme(dt Ni Ai) - dt As =  Vs_ -Somme(Ni Vi_) - vout
C         [L] = dt {N1,N2,..,N15,-1}
C         bo = {N1,N2,..,N15,-1} a = -[L]/dt v_ - vout
C         b  = [L] ao - bo
C         b  = [L] ao + [L]/dt v_  + vout
C         b  = [L]/dt vo+  + vout
C-----------------------------------------------------------------------
C
C     calcul de [H] la = b par gradient conjugue
C
C-----------------------------------------------------------------------
C      Gradient conjugue [H] la = b avec preconditionnement [N]
C      [H]-1 = Somme([I-H]^i),i=0,1...inf    si |I-H|<1
C      [N]-1 = [2I-H]
C-----------------------------------------------------------------------
C      r(0) = b - [H] la(0)
C      for i=1,niter
C        z(i-1) = [N]-1 r(i-1)
C        rho(i-1) = r(i-1)T z(i-1)
C        if (i == 1)
C          p(1) = z(0)
C        else
C          beta = rho(i-1) / rho(i-2)
C          p(i) = z(i-1) + beta p(i-1)
C        endif
C        q(i)  = [H] p(i)
C        alpha = rho(i-1) / p(i)T q(i)
C        la(i) = la(i-1) + alpha p(i)
C        r(i)  = r(i-1)  - alpha q(i)
C     end
C======================================================================|
      SCALE = THREE_OVER_4
      ASMAX = ZEP6
      R2NEW = ZERO      
      LAG_NC= NC
C---
C---  Calcul de la matrice H
      CALL LAG_MULT_HP(NC     ,LENH   ,NH     ,MAS     ,IN     ,
     2                 DIAG_H ,HH     ,IADLL  ,LLL     ,JLL    ,
     3                 XLL    ,LTSM   ,IADHF  ,JCIHF   ,IADH   ,
     4                 JCIH   ,RBYL   ,NPBYL  ,ICFTAG  ,JCFTAG ,
     5                 NCF_S  ,NCF_E  ,NCR    ,INDEXLAG)
C------------------------------------------
C     Normalisation du systeme
c     lagopt=0 => sans norme
c     lagopt=1 => norme L1 (diagonal)
c     lagopt=2 => norme L2 (columns)
C------------------------------------------
      IF (LAGOPT==1) THEN
        DD = ZERO
        DO IC=1,NC
          DD = DD + DIAG_H(IC)*DIAG_H(IC)
        ENDDO
        DO IC=1,NC
          Z(IC) = DD
        ENDDO
      ELSEIF (LAGOPT==2) THEN
        DO IC=1,NC
          Z(IC) = DIAG_H(IC)*DIAG_H(IC)
        ENDDO
        DO IC=1,NC
          DO IH=IADH(IC),IADH(IC+1)-1
            JC  = JCIH(IH)
            HIJ = HH(IH)*HH(IH)
            Z(IC) = Z(IC) + HIJ
            Z(JC) = Z(JC) + HIJ
          ENDDO
        ENDDO
      ENDIF
C---
      IF (LAGOPT>0) THEN
        DO IC=1,NC
          Z(IC) = SCALE/SQRT(SQRT(Z(IC)))
        ENDDO
C---      normalisation [H]        Somme([H](ic,i)^2)=1   i=1...nc
        DO IC=1,NC
          DIAG_H(IC) = DIAG_H(IC)*Z(IC)*Z(IC)
          DO IH=IADH(IC),IADH(IC+1)-1
            HH(IH) = HH(IH)*Z(IC)*Z(JCIH(IH))
          ENDDO
        ENDDO
C---      normalisation [LLL] et {R}
        DO IC=1,NC
          DO IK=IADLL(IC),IADLL(IC+1)-1
            XLL(IK)  = XLL(IK)*Z(IC)
          ENDDO
          R(IC) = R(IC)*Z(IC)
        ENDDO
      ENDIF
C------------------------------------------
C     Factorisation de [H] : Cholesky incomplet + shift iteratif
C------------------------------------------
      NITER_CHL  = 0
      IF (LAGMOD==1) THEN
        ALPHA = LAG_ALPH
        IF (LAG_ALPHS==ZERO) THEN
          DO IC=1,NC
            DIAG_L(IC) =  DIAG_H(IC)
          ENDDO
        ELSE
          DO IC=1,NC
            DIAG_L(IC) =  DIAG_H(IC) + LAG_ALPHS
          ENDDO
        ENDIF
        IP    = 1
        DO WHILE (IP>0)
          DO IH=1,LENH
            HL(IH) =  HH(IH)
          ENDDO
          IP = CHOLFACT(NC,DIAG_L,HL,IADH,JCIH,WORK1,WORK2,WORK3)
          NITER_CHL  = NITER_CHL+1  
          ALPHA = ALPHA*2
          AS    = ALPHA + LAG_ALPHS
          IF (AS>ASMAX) THEN
            IP = 0
            WRITE(IOUT,*) '***WARNING (LAGMULT : FACTORISATION FAILED )'
            WRITE(ISTDO,*)'***WARNING (LAGMULT : FACTORISATION FAILED )'
          ELSEIF (IP>0) THEN
            DO IC=1,NC
              DIAG_L(IC) =  DIAG_H(IC) + ALPHA + LAG_ALPHS
            ENDDO
          ENDIF
        ENDDO
        IF (NITER_CHL>1) print*,'FACTOR ITERATIONS = ',NITER_CHL
       ENDIF
C=======================================================================
C     calcul de  b (r=b)
C=======================================================================
      DO IC=1,NC
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = INDEXLAG(LLL(IK))
          J = JLL(IK)
          IF (J>3) THEN
            J = J-3
            R(IC) = R(IC) + XLL(IK)*(VR(J,I)/DT12+AR(J,I))
          ELSE
            R(IC) = R(IC) + XLL(IK)*(V(J,I)/DT12+A(J,I))
          ENDIF
        ENDDO
      ENDDO
C---  Initialisation du vecteur solution
      RNORM = ZERO
      DO IC=1,NC
        RNORM = RNORM + R(IC)*R(IC)
        P(IC) = LAMBDA(IC)
      ENDDO
C---  Residu initial   R(i) = R(i) - sum(H(i,j)*P(j))
      DO IC=1,NC
        R(IC) = R(IC) - DIAG_H(IC)*P(IC)
        DO IH=IADH(IC),IADH(IC+1)-1
          JC = JCIH(IH)
          R(IC) = R(IC) - HH(IH)*P(JC)
          R(JC) = R(JC) - HH(IH)*P(IC)
        ENDDO
      ENDDO
C=======================================================================
C     iterations
C=======================================================================
      ITERMAX = NC
      ITER    = 0
      DO WHILE(ITER<ITERMAX)
        ITER  = ITER+1
        NITER = ITER
C--------------------
C       preconditionnement  z = inv(H) r
C--------------------
        IF (LAGMOD==1) THEN
C---    preconditionnement de Cholesky
          CALL PRECHOL(Z,DIAG_L,HL,R,NC,IADH,JCIH)
        ELSEIF(LAGMOD==2) THEN
C---    preconditionnement polynomial :  H=I-B,  z = [I+B] r    
          DO IC=1,NC
            Z(IC) = R(IC) + R(IC)
          ENDDO
          DO IC=1,NC
            Z(IC) = Z(IC) - DIAG_H(IC)*R(IC)
            DO IH=IADH(IC),IADH(IC+1)-1
              JC = JCIH(IH)
              Z(IC) = Z(IC) - HH(IH)*R(JC)
              Z(JC) = Z(JC) - HH(IH)*R(IC)
            ENDDO
          ENDDO
        ENDIF
C--------------------
        R2    = R2NEW
        R2NEW = ZERO
        DO IC=1,NC
          R2NEW = R2NEW + R(IC)*Z(IC)
          Q(IC) = ZERO
        ENDDO
C--------------------
        IF(ITER==1)THEN
          DO IC=1,NC
            P(IC) = Z(IC)
          ENDDO
        ELSE
          BETA = R2NEW/R2
          DO IC=1,NC
            P(IC) = Z(IC) + BETA*P(IC)
          ENDDO
        ENDIF
C-----------------------------------------------------------------------
C     calcul de  q = q_ + [H] p
C-----------------------------------------------------------------------
      DO IC=1,NC
        Q(IC) = Q(IC) + DIAG_H(IC)*P(IC)
        DO IH=IADH(IC),IADH(IC+1)-1
          JC = JCIH(IH)
          Q(IC) = Q(IC) + HH(IH)*P(JC)          
          Q(JC) = Q(JC) + HH(IH)*P(IC)          
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C     pt [H] p = pt q
C-----------------------------------------------------------------------
        PQ = ZERO
        DO IC=1,NC
          PQ = PQ + P(IC)*Q(IC)
        ENDDO
C-----------------------------------------------------------------------
        IF(PQ==ZERO)THEN
          ITER = ITERMAX
        ELSE
          ALPHA = R2NEW/PQ
          LAG_ERSQ2 = ZERO
          DO IC=1,NC
            LAMBDA(IC) = LAMBDA(IC) + ALPHA*P(IC)
            R(IC)      = R(IC)      - ALPHA*Q(IC)
            LAG_ERSQ2  = LAG_ERSQ2  + R(IC)*R(IC)
          ENDDO
C--------- test convergence --------------------------------------------
          LAG_ERSQ2 = LAG_ERSQ2/MAX(EM30,RNORM)
          IF(LAG_ERSQ2<LAGM_TOL)THEN
            ITER = ITERMAX
          ENDIF
        ENDIF
C=======================================================================
C     fin boucle gradient conjugue
C=======================================================================
      ENDDO
      NITER_GC = NITER
C---
C     a = ao + [M]-1[L]t la  
C---
      DO IC=1,NCR
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = INDEXLAG(LLL(IK))
          J = JLL(IK)
          XLL(IK) = XLL(IK)*LAMBDA(IC)
          IF(J>3) THEN
            J = J-3
            AR(J,I) = AR(J,I) - XLL(IK)/IN(I)
          ELSE         
            A(J,I)  = A(J,I)  - XLL(IK)/MAS(I)
          ENDIF         
        ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  LAG_MULT_SDP                  source/tools/lagmul/lag_mult_solv.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        LAG_MULT_HP                   source/tools/lagmul/lag_mult_h.F
Chd|        SPMD_MUMPS_DEAL               source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_EXEC               source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_INI                source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE LAG_MULT_SDP(
     1                    NH    ,NC      ,NCR   ,A     ,V     ,
     2                    MAS   ,IADLL   ,LLL   ,JLL   ,XLL   ,
     3                    IADH  ,JCIH    ,HH    ,Z     ,P     ,
     4                    R     ,Q       ,LTSM  ,HL    ,DIAG_H,
     5                    DIAG_L,WORK1   ,WORK2 ,WORK3 ,LAMBDA,
     6                    RBYL  ,NPBYL   ,AR    ,VR    ,IN    ,
     7                    IADHF ,JCIHF   ,ICFTAG,JCFTAG,NCF_S ,
     8                    NCF_E ,INDEXLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "units_c.inc"
#ifdef MUMPS5
#include      "dmumps_struc.h"
#endif
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,NCR,NCF_S,NCF_E,NH 
      INTEGER LLL(*),JLL(*),IADLL(*),IADH(*),JCIH(*),IADHF(*),
     .        JCIHF(*),NPBYL(NNPBY,*),WORK2(*),WORK3(*),
     .        ICFTAG(*),JCFTAG(*), INDEXLAG(*)
C     REAL
      my_real
     .  MAS(*),IN(*), A(3,*),AR(3,*),V(3,*),VR(3,*),
     .  XLL(*),P(*),R(*),Q(*),LAMBDA(*),RBYL(NRBY,*),
     .  LTSM(6,*),Z(*),HH(*),HL(*),DIAG_H(*),DIAG_L(*),
     .  WORK1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IK,IC,JC,IH,IP,ITER,NITER,ITERMAX,NIN,LENH,NZ
      my_real
     .   AS,ASMAX,ALPHA,BETA,R2,R2NEW,RNORM,PQ,HIJ,SCALE,DD
#ifdef MUMPS5
      type(dmumps_struc)::MUMPS_PAR
#endif
C-----------------------------------------------
C    External Functions
C-----------------------------------------------
      INTEGER  CHOLFACT
      EXTERNAL CHOLFACT
C-----------------------------------------------
C-----------------------------------------------
C
C       | M | Lt| | a    | M ao
C       |---+---| |    = |
C       | L | 0 | | la   | bo
C
C        [M] a + [L]t la = [M] ao
C        [L] a = bo
C
C         a = -[M]-1[L]t la + ao
C        [L][M]-1[L]t la  = [L] ao - bo
C
C        [H] = [L][M]-1[L]t
C        b  = [L] ao - bo
C        [H] la = b
C
C        a = ao - [M]-1[L]t la
C-----------------------------------------------
C
C        la              : LAMBDA(NC)
C        ao              : A(3,NUMNOD)
C        [M](i)          : MAS(NUMNOD)
C        [L][M]-1[L]t la : Q(NC)
C        [L] ao - b      : B(NC)
C        [M]-1[L]t       : LTSM(3,NUMNOD) une colonne de [M]-1[L]t
C                        : Z(NC)
C
C        [L](ic,j,i)    -> XLL(IK) 
C        ic = 1..nc      : IK = IADLL(IC)...IADLL(IC+1)-1
C        j  = 1,2,3      : J  = JLL(IK)
C        i  = 1..numnod  : I  = LLL(IK)
C
C
C        NC : nombre de contact 
C
C        IC : numero du contact (1,NC)
C        IK : index de XLL,LLL,JLL
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3
C
C-----------------------------------------------
C        [H](ic,jc)     -> HH(IH)
C        ic = 1..nc      : IH = IADH(IC)...IADH(IC+1)-1
C        jc = 1..ic      : JC = JCIH(IH)
C
C        q = [H] p
C     
C        do ic=1,nc
C          q(ic) = 0
C          do jc=1,nc
C            q(ic) = q(ic) + h(ic,jc)*p(jc)
C          enddo
C        enddo
C
C        DO IC=1,NC
C          Q(IC) = 0
C          DO IH=IADH(IC),IADH(IC+1)-1
C            Q(IC) = Q(IC) - HH(IH)*P(JCIH(IH))
C          ENDDO
C        ENDDO
C-----------------------------------------------
C  evaluation de b:
C
C         Vs = Somme(Ni Vi) + vout
C         Vs_ + dt As = Somme(Ni Vi_) + Somme(dt Ni Ai) + vout
C         Somme(dt Ni Ai) - dt As =  Vs_ -Somme(Ni Vi_) - vout
C         [L] = dt {N1,N2,..,N15,-1}
C         bo = {N1,N2,..,N15,-1} a = -[L]/dt v_ - vout
C         b  = [L] ao - bo
C         b  = [L] ao + [L]/dt v_  + vout
C         b  = [L]/dt vo+  + vout
C-----------------------------------------------------------------------
C
C     calcul de [H] la = b par gradient conjugue
C
C-----------------------------------------------------------------------
C      Gradient conjugue [H] la = b avec preconditionnement [N]
C      [H]-1 = Somme([I-H]^i),i=0,1...inf    si |I-H|<1
C      [N]-1 = [2I-H]
C-----------------------------------------------------------------------
C      r(0) = b - [H] la(0)
C      for i=1,niter
C        z(i-1) = [N]-1 r(i-1)
C        rho(i-1) = r(i-1)T z(i-1)
C        if (i == 1)
C          p(1) = z(0)
C        else
C          beta = rho(i-1) / rho(i-2)
C          p(i) = z(i-1) + beta p(i-1)
C        endif
C        q(i)  = [H] p(i)
C        alpha = rho(i-1) / p(i)T q(i)
C        la(i) = la(i-1) + alpha p(i)
C        r(i)  = r(i-1)  - alpha q(i)
C     end
C======================================================================|
#ifdef MUMPS5
      IF(ISPMD==0) THEN
       SCALE = THREE_OVER_4
       ASMAX = ZEP6
       R2NEW = ZERO      
       LAG_NC= NC
C---
C---  Calcul de la matrice H
       CALL LAG_MULT_HP(NC     ,LENH   ,NH     ,MAS     ,IN     ,
     2                  DIAG_H ,HH     ,IADLL  ,LLL     ,JLL    ,
     3                  XLL    ,LTSM   ,IADHF  ,JCIHF   ,IADH   ,
     4                  JCIH   ,RBYL   ,NPBYL  ,ICFTAG  ,JCFTAG ,
     5                  NCF_S  ,NCF_E  ,NCR    ,INDEXLAG)
C------------------------------------------
C     Normalisation du systeme
c     lagopt=0 => sans norme
c     lagopt=1 => norme L1 (diagonal)
c     lagopt=2 => norme L2 (columns)
C------------------------------------------
       IF (LAGOPT==1) THEN
        DD = ZERO
        DO IC=1,NC
          DD = DD + DIAG_H(IC)*DIAG_H(IC)
        ENDDO
        DO IC=1,NC
          Z(IC) = DD
        ENDDO
       ELSEIF (LAGOPT==2) THEN
        DO IC=1,NC
          Z(IC) = DIAG_H(IC)*DIAG_H(IC)
        ENDDO
        DO IC=1,NC
          DO IH=IADH(IC),IADH(IC+1)-1
            JC  = JCIH(IH)
            HIJ = HH(IH)*HH(IH)
            Z(IC) = Z(IC) + HIJ
            Z(JC) = Z(JC) + HIJ
          ENDDO
        ENDDO
       ENDIF
C---
       IF (LAGOPT>0) THEN
        DO IC=1,NC
          Z(IC) = SCALE/SQRT(SQRT(Z(IC)))
        ENDDO
C---      normalisation [H]        Somme([H](ic,i)^2)=1   i=1...nc
        DO IC=1,NC
          DIAG_H(IC) = DIAG_H(IC)*Z(IC)*Z(IC)
          DO IH=IADH(IC),IADH(IC+1)-1
            HH(IH) = HH(IH)*Z(IC)*Z(JCIH(IH))
          ENDDO
        ENDDO
C---      normalisation [LLL] et {R}
        DO IC=1,NC
          DO IK=IADLL(IC),IADLL(IC+1)-1
            XLL(IK)  = XLL(IK)*Z(IC)
          ENDDO
          R(IC) = R(IC)*Z(IC)
        ENDDO
       ENDIF
C------------------------------------------
C     Factorisation de [H] : Cholesky incomplet + shift iteratif
C------------------------------------------
C      NITER_CHL  = 0
C      IF (LAGMOD==1) THEN
C        ALPHA = LAG_ALPH
C        IF (LAG_ALPHS==ZERO) THEN
C          DO IC=1,NC
C            DIAG_L(IC) =  DIAG_H(IC)
C          ENDDO
C        ELSE
C          DO IC=1,NC
C            DIAG_L(IC) =  DIAG_H(IC) + LAG_ALPHS
C          ENDDO
C        ENDIF
C        IP    = 1
C        DO WHILE (IP>0)
C          DO IH=1,LENH
C            HL(IH) =  HH(IH)
C          ENDDO
C          IP = CHOLFACT(NC,DIAG_L,HL,IADH,JCIH,WORK1,WORK2,WORK3)
C          NITER_CHL  = NITER_CHL+1  
C          ALPHA = ALPHA*2
C          AS    = ALPHA + LAG_ALPHS
C          IF (AS>ASMAX) THEN
C            IP = 0
C            WRITE(IOUT,*) '***WARNING (LAGMULT : FACTORISATION FAILED )'
C            WRITE(ISTDO,*)'***WARNING (LAGMULT : FACTORISATION FAILED )'
C          ELSEIF (IP>0) THEN
C            DO IC=1,NC
C              DIAG_L(IC) =  DIAG_H(IC) + ALPHA + LAG_ALPHS
C            ENDDO
C          ENDIF
C        ENDDO
C        IF (NITER_CHL>1) print*,'FACTOR ITERATIONS = ',NITER_CHL
C       ENDIF
      END IF ! fin proc0
C=======================================================================
C     MUMPS
C=======================================================================
C
C Init structure
C
      CALL SPMD_MUMPS_INI(MUMPS_PAR,1)
      MUMPS_PAR%ICNTL(18) = 0            ! donnees centralisees sur p0
C flag output
      MUMPS_PAR%ICNTL(3)=-1              ! ou iout par exemple si sorties voulues
      IF(ISPMD==0) THEN ! structure de stockage sur p0
        MUMPS_PAR%N         = NC               ! valid sur proc0 uniquement
        MUMPS_PAR%NZ        = NC+IADH(NC+1)-1     ! valid sur proc0 uniquement
        ALLOCATE(MUMPS_PAR%A(MUMPS_PAR%NZ))
        ALLOCATE(MUMPS_PAR%IRN(MUMPS_PAR%NZ))
        ALLOCATE(MUMPS_PAR%JCN(MUMPS_PAR%NZ))
        DO IC = 1, NC
          MUMPS_PAR%A(IC) = DIAG_H(IC)
          MUMPS_PAR%IRN(IC)=IC
          MUMPS_PAR%JCN(IC)=IC
        END DO
        NZ = NC
        DO IC = 1, NC
          DO IH=IADH(IC),IADH(IC+1)-1
            NZ = NZ + 1
            MUMPS_PAR%A(NZ) = HH(IH)
            MUMPS_PAR%IRN(NZ)=IC
            MUMPS_PAR%JCN(NZ)=JCIH(IH)
C            MUMPS_PAR%JCN(NZ)=JDIH(IH)
          ENDDO
        END DO  
C
C     calcul de  b (r=b)
C
        DO IC=1,NC
          DO IK=IADLL(IC),IADLL(IC+1)-1
            I = INDEXLAG(LLL(IK))
            J = JLL(IK)
            IF (J>3) THEN
              J = J-3
              R(IC) = R(IC) + XLL(IK)*(VR(J,I)/DT12+AR(J,I))
            ELSE
              R(IC) = R(IC) + XLL(IK)*(V(J,I)/DT12+A(J,I))
            ENDIF
          ENDDO
        ENDDO
        ALLOCATE(MUMPS_PAR%RHS(MUMPS_PAR%N))
        DO IC=1,NC
          MUMPS_PAR%RHS(IC) = R(IC)
        ENDDO
      END IF
C
C Resolution
C
      CALL SPMD_MUMPS_EXEC(MUMPS_PAR,1)     ! analyse et factorisation
C
      CALL SPMD_MUMPS_EXEC(MUMPS_PAR,2)     ! resolution  
C
C Resultat sur p0
C 
      IF(ISPMD==0) THEN 
        DO IC=1,NC
          LAMBDA(IC) = MUMPS_PAR%RHS(IC)
        ENDDO
      END IF
C
C Liberation de la memoire
C
      CALL SPMD_MUMPS_DEAL(MUMPS_PAR)
CC  ---  Initialisation du vecteur solution
C      RNORM = ZERO
C      DO IC=1,NC
C        RNORM = RNORM + R(IC)*R(IC)
C        P(IC) = LAMBDA(IC)
C      ENDDO
CC---  Residu initial   R(i) = R(i) - sum(H(i,j)*P(j))
C      DO IC=1,NC
C        R(IC) = R(IC) - DIAG_H(IC)*P(IC)
C        DO IH=IADH(IC),IADH(IC+1)-1
C          JC = JCIH(IH)
C          R(IC) = R(IC) - HH(IH)*P(JC)
C          R(JC) = R(JC) - HH(IH)*P(IC)
C        ENDDO
C      ENDDO
C=======================================================================
C     iterations
C=======================================================================
C      ITERMAX = NC
C      ITER    = 0
C      DO WHILE(ITER<ITERMAX)
C        ITER  = ITER+1
C        NITER = ITER
CC--------------------
CC       preconditionnement  z = inv(H) r
CC--------------------
C        IF (LAGMOD==1) THEN
CC---    preconditionnement de Cholesky
C          CALL PRECHOL(Z,DIAG_L,HL,R,NC,IADH,JCIH)
C        ELSEIF(LAGMOD==2) THEN
CC---    preconditionnement polynomial :  H=I-B,  z = [I+B] r    
C          DO IC=1,NC
C            Z(IC) = R(IC) + R(IC)
C          ENDDO
C          DO IC=1,NC
C            Z(IC) = Z(IC) - DIAG_H(IC)*R(IC)
C            DO IH=IADH(IC),IADH(IC+1)-1
C              JC = JCIH(IH)
C              Z(IC) = Z(IC) - HH(IH)*R(JC)
C              Z(JC) = Z(JC) - HH(IH)*R(IC)
C            ENDDO
C          ENDDO
C        ENDIF
CC--------------------
C        R2    = R2NEW
C        R2NEW = ZERO
C        DO IC=1,NC
C          R2NEW = R2NEW + R(IC)*Z(IC)
C          Q(IC) = ZERO
C        ENDDO
CC--------------------
C        IF(ITER==1)THEN
C          DO IC=1,NC
C            P(IC) = Z(IC)
C          ENDDO
C        ELSE
C          BETA = R2NEW/R2
C          DO IC=1,NC
C            P(IC) = Z(IC) + BETA*P(IC)
C          ENDDO
C        ENDIF
CC-----------------------------------------------------------------------
CC     calcul de  q = q_ + [H] p
CC-----------------------------------------------------------------------
C      DO IC=1,NC
C        Q(IC) = Q(IC) + DIAG_H(IC)*P(IC)
C        DO IH=IADH(IC),IADH(IC+1)-1
C          JC = JCIH(IH)
C          Q(IC) = Q(IC) + HH(IH)*P(JC)          
C          Q(JC) = Q(JC) + HH(IH)*P(IC)          
C        ENDDO
C      ENDDO
CC-----------------------------------------------------------------------
CC     pt [H] p = pt q
CC-----------------------------------------------------------------------
C        PQ = ZERO
C        DO IC=1,NC
C          PQ = PQ + P(IC)*Q(IC)
C        ENDDO
CC-----------------------------------------------------------------------
C        IF(PQ==ZERO)THEN
C          ITER = ITERMAX
C        ELSE
C          ALPHA = R2NEW/PQ
C          LAG_ERSQ2 = ZERO
C          DO IC=1,NC
C            LAMBDA(IC) = LAMBDA(IC) + ALPHA*P(IC)
C            R(IC)      = R(IC)      - ALPHA*Q(IC)
C            LAG_ERSQ2  = LAG_ERSQ2  + R(IC)*R(IC)
C          ENDDO
CC--------- test convergence --------------------------------------------
C          LAG_ERSQ2 = LAG_ERSQ2/MAX(EM30,RNORM)
C          IF(LAG_ERSQ2<LAGM_TOL)THEN
C            ITER = ITERMAX
C          ENDIF
C        ENDIF
C=======================================================================
C     fin boucle gradient conjugue
C=======================================================================
C      ENDDO
C      NITER_GC = NITER
C---
C     a = ao + [M]-1[L]t la  
C---
      IF(ISPMD==0) THEN 
       DO IC=1,NCR
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = INDEXLAG(LLL(IK))
          J = JLL(IK)
          XLL(IK) = XLL(IK)*LAMBDA(IC)
          IF(J>3) THEN
            J = J-3
            AR(J,I) = AR(J,I) - XLL(IK)/IN(I)
          ELSE         
            A(J,I)  = A(J,I)  - XLL(IK)/MAS(I)
          ENDIF         
        ENDDO
       ENDDO
      END IF
#else
        write(6,*) "Error: this feature requires the MUMPS library "
        call flush(6)
        CALL ARRET(5)
#endif
C--------------------------------------------
      RETURN
      END
