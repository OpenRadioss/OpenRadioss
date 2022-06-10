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
#ifdef MUMPS5
Chd|====================================================================
Chd|  IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|-- called by -----------
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        MAV_LTP                       source/implicit/produt_v.F    
Chd|        PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_LANZP(IPREC ,
     1                    N     ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NI    ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADM  ,JDIM  ,DIAG_M ,
     3                    LT_M  ,X     ,R     ,ITOL  ,RTOL   ,
     4                    V     ,W     ,Y     ,ITASK ,IPRINT ,
     5                    SHIFT ,KCOND ,N_MAX ,FLM   ,F_X    ,
     6                    ISTOP ,W_DDL,A     ,AR     ,
     9                    VE    ,MS    ,XE    ,D     ,DR     ,
     A                    NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     B                    NS_IMP,NE_IMP,NSREM ,NSL   ,NMONV ,
     C                    IMONV ,MONVOL ,IGRSURF ,VOLMON,
     D                    FR_MV ,IBFV   ,SKEW  ,XFRAME,IND_IMP,
     H                    XI_C  ,R0    ,NDDLI_G,INTP_C,IRBE3  ,
     E                    LRBE3 ,IRBE2 ,LRBE2 ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}---------
      INTEGER  N  ,NNZ   ,IADK(*)  ,JDIK(*),ITOL,ISTOP,
     .         NI ,ITOK(*) ,IADI(*),JDII(*),N_MAX,
     .         NNZM  ,IADM(*),JDIM(*),IPREC,ITASK,IPRINT,
     .         W_DDL(*),IBFV(*),IRBE3(*) ,LRBE3(*),NDDLI_G,
     .         IRBE2(*),LRBE2(*)
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,INTP_C,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*),IND_IMP(*) 
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*), LT_K(*) ,DIAG_M(*),LT_M(*) ,X(*), RTOL,
     .  V(*) ,W(*)  ,R(*)  ,Y(*),SHIFT,KCOND,LT_I(*),FLM,F_X
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*)  ,XFRAME(*),XI_C(*),R0

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ITN,IP,NLIM,IBID,IUPD
      my_real
     .   R2(N),B(N),CS1(2),RR,
     .   R02,ANORM,RNORM, YNORM,RBID
      my_real
     .  DDOT, DNRM2,
     .  ALFA, B1, BETA, BETA1, BSTEP, CS,
     .  CGNORM, DBAR, DELTA, DENOM, DIAG,
     .  EPS, EPSA, EPSLN, EPSR, EPSX,
     .  GAMMA, GBAR, GMAX, GMIN, GPERT,
     .  LQNORM, OLDB, QRNORM, RHS1, RHS2,
     .  S, SN, SNPROD, T, TNORM,
     .  X1CG, X1LQ, YNORM2, ZBAR, Z 
      CHARACTER*16       EXIT
      CHARACTER*11       WARR
      CHARACTER*52       MSG(-1:8)

      DATA               EXIT  /'TERMINATION WITH'/
      DATA               WARR  /'**WARRING**'/

      DATA               MSG
     . / 'BETA2 = 0.  IF M = I, F AND X ARE EIGENVECTORS OF K',
     .   'BETA1 = 0.  THE EXACT SOLUTION IS  X = 0',
     .   'REQUESTED ACCURACY ACHIEVED, AS DETERMINED BY RTOL',
     .   'REASONABLE ACCURACY ACHIEVED, GIVEN EPS',
     .   'X HAS CONVERGED TO AN EIGENVECTOR',
     .   'ACOND HAS EXCEEDED 0.1/EPS',
     .   'THE ITERATION LIMIT WAS REACHED',
     .   'APROD  DOES NOT DEFINE A SYMMETRIC MATRIX',
     .   'MSOLVE DOES NOT DEFINE A SYMMETRIC MATRIX',
     .   'MSOLVE DOES NOT DEFINE A POS-DEF PRECONDITIONER' /
      TYPE(PRGRAPH) :: GBID
      TYPE(DMUMPS_STRUC) MBID
C*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C*     based on File symmlq.f
C*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C machine  precision minimum dependant du type REAL ou DOUBLE
c      write(*,*)'flm=',flm
C-------add if (RTOL.lt.FLM) RTOL=FLM
      IUPD = 0
      NLIM=N_MAX
C*     Print heading and initialize.

      IF(IPRINT.NE.0)THEN
       WRITE(IOUT,*)'    *** BEGIN PRECONDITION LANCZOS ITERATION ***'
       WRITE(IOUT,*)
      ENDIF
      IF(IPRINT.LT.0)THEN
       WRITE(ISTDO,*)'    *** BEGIN PRECONDITION LANCZOS ITERATION ***'
       WRITE(ISTDO,*)
      ENDIF
      IP = IABS(IPRINT)
      ISTOP  = 0
      ITN     = 0
      ANORM  = ZERO
      KCOND  = ZERO
      RNORM  = ZERO
      YNORM  = ZERO

C*     Set up y for the first Lanczos vector v1.
C*     y is really beta1 * P * v1  where  P = C**(-1).
C*     y and beta1 will be zero if b = 0.

C    --R=B-A.X--- ---
       CALL MAV_LTP(
     1            N     ,NI    ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            X     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2 ,LRBE2 )
      DO I = 1, N
        B(I) = R(I) 
        R(I) = R(I) - Y(I)
      ENDDO
      CALL PREC_SOLVP(IPREC,ITASK  ,
     1                GBID ,IBID   ,IBID  , DIAG_K,LT_K   , 
     2                IADK ,JDIK   ,IBID  ,IBID  ,IBID   , 
     3                IBID ,RBID   ,IBID  ,IBID  ,MBID   ,
     4                IBID ,IBID   ,IBID  ,IBID  ,IBID   , 
     5                IBID ,IBID   ,  
     1               N    ,NNZM,IADM  ,JDIM  ,DIAG_M ,   
     2               LT_M ,R   ,Y     )
C      IF ( GOODB  ) THEN
C         B1  = Y(1)
C      ELSE
       B1  = ZERO
C      ENDIF
      CALL PRODUT_W( N  ,R   ,Y  ,W_DDL,BETA1)

C*     TEST FOR AN INDEFINITE PRECONDITIONER.

      IF (BETA1 .LT. ZERO) THEN
c         ISTOP = 8
c         GO TO 900
       BETA1  = -BETA1 
       WRITE(IOUT, 3000) WARR, MSG(8)
       WRITE(ISTDO, 3000) WARR, MSG(8)
      ENDIF

C*     IF B = 0 EXACTLY, STOP WITH X = 0.

      IF (BETA1 .EQ. ZERO) THEN
       ISTOP=-1
       GOTO 900
      ENDIF

C*     HERE AND LATER, V IS REALLY P * (THE LANCZOS V).

      IF (ITOL.EQ.2)  R02 = BETA1
      BETA1  = SQRT( BETA1 )
      S      = ONE / BETA1
C
      IF (ITOL.EQ.1) THEN
        CALL PRODUT_W(N,R,R,W_DDL,R02)
        RR = SQRT(R02)
       ELSE
        RR = BETA1
      ENDIF
      IF(IPRINT.NE.0)THEN
        IF(IPRINT.LT.0) WRITE(ISTDO,1000)ITN,RR
        WRITE(IOUT,1000)ITN,RR
      ENDIF
      DO I = 1, N
        V(I)  = S * Y(I)
      ENDDO

      CALL MAV_LTP(
     1            N     ,NI    ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            V     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2 ,LRBE2 )

C*     SET UP Y FOR THE SECOND LANCZOS VECTOR.
C*     AGAIN, Y IS BETA * P * V2  WHERE  P = C**(-1).
C*     Y AND BETA WILL BE ZERO OR VERY SMALL IF B IS AN EIGENVECTOR.
     
      IF (SHIFT.NE.ZERO) THEN
       DO I = 1, N
         Y(I) = Y(I)-SHIFT*V(I)
       ENDDO
      ENDIF
      CALL PRODUT_W( N  ,V   ,Y  ,W_DDL,ALFA)
      T=ALFA / BETA1
      DO I = 1, N
       Y(I)  = Y(I)-T*R(I)
      ENDDO

C*     MAKE SURE  R2  WILL BE ORTHOGONAL TO THE FIRST  V.

      CALL PRODUT_W( N  ,V   ,Y  ,W_DDL,Z)
      CALL PRODUT_W( N  ,V   ,V  ,W_DDL,S)
      T = Z / S
      DO I = 1, N
       Y(I)  = Y(I)-T*V(I)
      ENDDO

      DO I = 1, N
       R2(I)  = Y(I)
      ENDDO
      CALL PREC_SOLVP(IPREC,ITASK  ,
     1                GBID ,IBID   ,IBID  , DIAG_K,LT_K   , 
     2                IADK ,JDIK   ,IBID  ,IBID  ,IBID   , 
     3                IBID ,RBID   ,IBID  ,IBID  ,MBID   ,
     4                IBID ,IBID   ,IBID  ,IBID  ,IBID   , 
     5                IBID ,IBID   ,  
     1               N    ,NNZM,IADM  ,JDIM  ,DIAG_M ,   
     2               LT_M ,R2   ,Y    )
      OLDB   = BETA1
      CALL PRODUT_W( N  ,R2   ,Y  ,W_DDL,BETA)

      IF (BETA .LT. ZERO) THEN
c         ISTOP = 8
c         GO TO 900
       BETA=-BETA
       WRITE(IOUT, 3000) WARR, MSG(8)
       WRITE(ISTDO, 3000) WARR, MSG(8)
      ENDIF

C*     CAUSE TERMINATION (LATER) IF BETA IS ESSENTIALLY ZERO.

      BETA   = SQRT( BETA )
      IF (BETA .LE. FLM) ISTOP = -1

C*     SEE IF THE LOCAL REORTHOGONALIZATION ACHIEVED ANYTHING.

C      CALL PRODUT_V( N  ,R2   ,R2  ,T)
C      DENOM  = ONE/(SQRT( S*T )  +  FLM)
C      S      = Z * DENOM
C      CALL PRODUT_V( N  ,V   ,R2  ,T)
C      T      = T * DENOM

C*     INITIALIZE OTHER QUANTITIES.

      CGNORM = BETA1
      GBAR   = ALFA
      DBAR   = BETA
      RHS1   = BETA1
      RHS2   = ZERO
      BSTEP  = ZERO
      SNPROD = ONE
      TNORM  = ALFA*ALFA + BETA*BETA
      YNORM2 = ZERO
      GMAX   = ABS( ALFA ) + FLM
      GMIN   = GMAX

C      IF ( GOODB ) THEN
C       DO I = 1, N
C            W(I)  = ZERO
C       ENDDO
C      ELSE
       DO I = 1, N
         W(I)  = V(I)
       ENDDO
C      ENDIF

C*     ------------------------------------------------------------------
C*     MAIN ITERATION LOOP.
C*     ------------------------------------------------------------------

C*     ESTIMATE VARIOUS NORMS AND TEST FOR CONVERGENCE.

 300  ITN     = ITN    +  1
      ANORM  = SQRT( TNORM  )
      YNORM  = SQRT( YNORM2 )
      EPSA   = ANORM * FLM
      EPSX   = ANORM * YNORM * FLM
      EPSR   = ANORM * YNORM * RTOL
      DIAG   = GBAR
      IF (DIAG .EQ. ZERO) DIAG = EPSA

      LQNORM = SQRT( RHS1*RHS1 + RHS2*RHS2 )
      QRNORM = SNPROD * BETA1
      CGNORM = QRNORM * BETA / ABS( DIAG )

C*     ESTIMATE  COND(A).
C*     IN THIS VERSION WE LOOK AT THE DIAGONALS OF  L  IN THE
C*     FACTORIZATION OF THE TRIDIAGONAL MATRIX,  T = L*Q.
C*     SOMETIMES, T(K) CAN BE MISLEADINGLY ILL-CONDITIONED WHEN
C*     T(K+1) IS NOT, SO WE MUST BE CAREFUL NOT TO OVERESTIMATE KCOND.

      IF (LQNORM .LE. CGNORM) THEN
         KCOND  = GMAX / GMIN
      ELSE
         DENOM  = MIN( GMIN, ABS( DIAG ) )
         KCOND  = GMAX / DENOM
      ENDIF

C*     SEE IF ANY OF THE STOPPING CRITERIA ARE SATISFIED.
C*     IN RARE CASES, ISTOP IS ALREADY -1 FROM ABOVE (ABAR = CONST * I).

      IF (ISTOP .EQ. 0) THEN
         IF (ITN    .GE. NLIM   ) ISTOP = 5
         IF (KCOND  .GE.EM01/FLM) ISTOP = 4
         IF (EPSX   .GE. BETA1  ) ISTOP = 3
         IF (CGNORM .LE. EPSX   ) ISTOP = 2
         IF (CGNORM .LE. EPSR   ) ISTOP = 1
      ENDIF
C*     ==================================================================

C*     SEE IF IT IS TIME TO PRINT SOMETHING.

C*      IF (NOUT .LE.  0)          GO TO 600
C*      IF (N    .LE. 40)          GO TO 400
C*      IF (ITN  .LE. 10)          GO TO 400
C*      IF (ITN  .GE. ITNLIM - 10) GO TO 400
C*      IF (MOD(ITN,10)  .EQ.   0) GO TO 400
C*      IF (CGNORM .LE. TEN*EPSX) GO TO 400
C*      IF (CGNORM .LE. TEN*EPSR) GO TO 400
C*      IF (KCOND  .GE. EM2/FLM ) GO TO 400
C*      IF (ISTOP  .NE. 0)        GO TO 400
C*      GOTO 600
C*
C*     PRINT A LINE FOR THIS ITERATION.

        IF(IPRINT.NE.0)THEN 
         IF (MOD(ITN,IP).EQ.0)THEN
          WRITE(IOUT,1001)ITN,CGNORM
          IF(IPRINT.LT.0) WRITE(ISTDO,1001)ITN,CGNORM
         ENDIF
        ENDIF

C*  400 ZBAR   = RHS1 / DIAG
C      Z      = (SNPROD * ZBAR  +  BSTEP) / BETA1
C      X1LQ   = X(1)  +  B1 * BSTEP / BETA1
C      X1CG   = X(1)  +  W(1) * ZBAR  +  B1 * Z
C
C      IF (    ITN     .EQ. 0) WRITE(NOUT, 1200)
C      WRITE(NOUT, 1300) ITN, X1CG, CGNORM, BSTEP/BETA1, ANORM, KCOND
C      IF (MOD(ITN,10) .EQ. 0) WRITE(NOUT, 1500)
C*     ==================================================================


C*     OBTAIN THE CURRENT LANCZOS VECTOR  V = (1 / BETA)*Y
C*     AND SET UP  Y  FOR THE NEXT ITERATION.

      IF (ISTOP .NE. 0) GO TO 800
C------place CRIT_STOP ICI-----
      S      = ONE / BETA

      DO I = 1, N
        V(I)  = S * Y(I)
      ENDDO

      CALL MAV_LTP(
     1            N     ,NI    ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            V     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2 ,LRBE2 )
      IF (SHIFT.NE.ZERO) THEN
       DO I = 1, N
         Y(I)  = Y(I)-SHIFT*V(I)
       ENDDO
      ENDIF
      T=BETA / OLDB
      DO I = 1, N
       Y(I)  = Y(I)-T*R(I)
      ENDDO
      CALL PRODUT_W( N  ,V   ,Y  ,W_DDL,ALFA)
      T = ALFA / BETA
      DO I = 1, N
       Y(I)  = Y(I)-T*R2(I)
      ENDDO
      DO I = 1, N
        R(I)  = R2(I)
      ENDDO
      DO I = 1, N
        R2(I)  = Y(I)
      ENDDO
      CALL PREC_SOLVP(IPREC,ITASK  ,
     1                GBID ,IBID   ,IBID  , DIAG_K,LT_K   , 
     2                IADK ,JDIK   ,IBID  ,IBID  ,IBID   , 
     3                IBID ,RBID   ,IBID  ,IBID  ,MBID   ,
     4                IBID ,IBID   ,IBID  ,IBID  ,IBID   , 
     5                IBID ,IBID   ,  
     1               N    ,NNZM,IADM  ,JDIM  ,DIAG_M ,   
     2               LT_M ,R2   ,Y    )
      OLDB   = BETA
      CALL PRODUT_W( N  ,R2   ,Y  ,W_DDL,BETA)
      IF (BETA .LT. ZERO) THEN
c         ISTOP = 6
c         GOTO 800
       BETA=-BETA
       WRITE(IOUT, 3000) WARR, MSG(8)
       WRITE(ISTDO, 3000) WARR, MSG(8)
      ENDIF
      BETA   = SQRT( BETA )
      TNORM  = TNORM  +  ALFA**2  +  OLDB**2  +  BETA**2

C*     COMPUTE THE NEXT PLANE ROTATION FOR  Q.

      GAMMA  = SQRT( GBAR*GBAR + OLDB*OLDB )
      CS     = GBAR / GAMMA
      SN     = OLDB / GAMMA
      DELTA  = CS * DBAR  +  SN * ALFA
      GBAR   = SN * DBAR  -  CS * ALFA
      EPSLN  = SN * BETA
      DBAR   =            -  CS * BETA

C*     UPDATE  X.

      Z      = RHS1 / GAMMA
      S      = Z * CS
      T      = Z * SN

      DO I = 1, N
         X(I)  = (W(I) * S   +   V(I) * T)  +  X(I)
         W(I)  =  W(I) * SN  -   V(I) * CS
      ENDDO

C*     ACCUMULATE THE STEP ALONG THE DIRECTION  B,
C*     AND GO ROUND AGAIN.

      BSTEP  = SNPROD * CS * Z  +  BSTEP
      SNPROD = SNPROD * SN
      GMAX   = MAX( GMAX, GAMMA )
      GMIN   = MIN( GMIN, GAMMA )
      YNORM2 = Z*Z  +  YNORM2
      RHS1   = RHS2  -  DELTA * Z
      RHS2   =       -  EPSLN * Z
c         WRITE(IOUT,2001)
c     .     ANORM, SQRT(YNORM2),GMAX/GMIN
c         WRITE(IOUT,2002)
c     .     ALFA, BETA,OLDB
      GO TO 300

C*     ------------------------------------------------------------------
C*     END OF MAIN ITERATION LOOP.
C*     ------------------------------------------------------------------

C*     MOVE TO THE CG POINT IF IT SEEMS BETTER.
C*     IN THIS VERSION OF SYMMLQ, THE CONVERGENCE TESTS INVOLVE
C*     ONLY CGNORM, SO WE'RE UNLIKELY TO STOP AT AN LQ POINT,
C*     EXCEPT IF THE ITERATION LIMIT INTERFERES.

  800 IF (CGNORM .LE. LQNORM) THEN
         ZBAR   = RHS1 / DIAG
         BSTEP  = SNPROD * ZBAR  +  BSTEP
         YNORM  = SQRT( YNORM2  +  ZBAR*ZBAR )
         RNORM  = CGNORM
         DO I = 1, N
          X(I)  = X(I)+ZBAR*W(I)
         ENDDO
      ELSE
         RNORM  = LQNORM
      ENDIF

C*      IF ( GOODB ) THEN

C*        ADD THE STEP ALONG  B.

C*         BSTEP  = BSTEP / BETA1
C*         DO I = 1, N
C*          Y(I)  = B(I)
C*         ENDDO
C*         CALL PREC_SOLV(IPREC,
C*     1               N    ,NNZM,IADM  ,JDIM  ,DIAG_M ,   
C*     2               LT_M ,B   ,Y     )
C*         DO I = 1, N
C*          X(I)  = X(I)+BSTEP*Y(I)
C*         ENDDO
C*      END IF

C*     ==================================================================
C*     DISPLAY FINAL STATUS.
C*     ==================================================================
  900 CONTINUE
      IF (IPRINT.NE.0) THEN
         WRITE(IOUT,2000)  EXIT, ISTOP, ITN,
     .                     EXIT, ANORM, KCOND,
     .                     EXIT, RNORM, YNORM
         WRITE(IOUT, 3000) EXIT, MSG(ISTOP)
        IF (IPRINT.LT.0) THEN
         WRITE(ISTDO,2000) EXIT, ISTOP, ITN,
     .                     EXIT, ANORM, KCOND,
     .                     EXIT, RNORM, YNORM
         WRITE(ISTDO, 3000) EXIT, MSG(ISTOP)
        ENDIF
      ENDIF
C    --R=B-A.X--- ---
       CALL MAV_LTP(
     1            N     ,NI    ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            X     ,R     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2 ,LRBE2 )
       DO I = 1, N
        R(I) = R(I) - B(I)
       ENDDO
       IF (ITOL.EQ.1) THEN
        CALL PRODUT_W( N  ,R    ,R  ,W_DDL,R02)
        RNORM = SQRT(R02)/RR
       ENDIF
      IF (IPRINT.NE.0) THEN
       WRITE(IOUT,1002)ITN,RNORM
       IF(IPRINT.LT.0) WRITE(ISTDO,1002)ITN,RNORM
      ENDIF
      IF (ISTOP.GT.0.AND.ISTOP.LT.4) ISTOP=0
C--------POUR F*X--------
      RETURN

C*     ------------------------------------------------------------------
 1100 FORMAT(/ 1P, ' BETA1  =', E10.2, 3X, 'ALPHA1 =', E10.2
     $       / ' (V1,V2) BEFORE AND AFTER ', E14.2
     $       / ' LOCAL REORTHOGONALIZATION', E14.2)
 1200 FORMAT(// 5X, 'ITN', 7X, 'X1(CG)', 10X,
     $         'NORM(R)', 5X, 'BSTEP', 7X, 'NORM(A)', 3X, 'COND(A)')
 1300 FORMAT(1P, I8, E19.10, E11.2, E14.5, 2E10.2)
 1500 FORMAT(1X)
 2000 FORMAT(/ 1P, A, 6X, 'ISTOP =', I3,   15X, 'ITN   =', I8
     $       /     A, 6X, 'ANORM =', E12.4, 6X, 'KCOND =', E12.4
     $       /     A, 6X, 'RNORM =', E12.4, 6X, 'YNORM =', E12.4)
 3000 FORMAT(      A, 6X, A )
 1000 FORMAT(5X,'ITERATION=',I8,5X,'  INITIAL RESIDUAL NORM=',E11.4)
 1001 FORMAT(5X,'ITERATION=',I8,5X,'     C.G. RESIDUAL NORM=',E11.4)
 1002 FORMAT(3X,'TOTAL LANCZOS ITERATION=',I8,5X,
     .          ' RELATIVE RESIDUAL NORM=',E11.4)
 2001 FORMAT(/ 5X, 'WITH', 2X, 'ANORM =', E12.4, 2X, 'YNORM =', 
     .       E12.4,2X,'KCOND =', E12.4)
 2002 FORMAT(/ 5X, 'WITH', 2X, 'ALFA =', E12.4, 2X, 'BETA =', 
     .       E12.4,2X,'OLDB =', E12.4)
C*     ------------------------------------------------------------------
C*     END OF SYMMLQ
      END
#endif
