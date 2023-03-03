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
C IDEBUG=1 -> output debug information into .out file
#define IDEBUG 0
C IOCSV=1 -> output fitting to curv.csv
#define IOCSV 0
!>    @param[in]   IFUNCS     SUBROUTINE handle/ID for evaluation : 1=OGDEN0/OGDEN1
!>    @param[out]  A          updated coeeficients
!>    @param[out]  IRET       return code 
!>                            0 : optimization succeeded;
!>                            1 : optimization failed;
Chd|====================================================================
Chd|  MRQMIN                        source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|        COVSRT                        source/materials/tools/nlsqf.F
Chd|        INVERSION                     source/materials/tools/nlsqf.F
Chd|        MRQCOF                        source/materials/tools/nlsqf.F
Chd|====================================================================
      SUBROUTINE MRQMIN(X,Y,SIG,NDATA,A,IA,
     .                 MA,COVAR,ALPHA,NCA, ERRNOW, 
     .                 IFUNCS,GAMMA,IRET,ICOMP)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER MA,NCA,NDATA,IA(MA),MMAX ,ICOMP
      my_real 
     .        GAMMA,ERRNOW,A(MA),ALPHA(NCA,NCA),
     .        COVAR(NCA,NCA),X(NDATA),Y(NDATA),SIG(NDATA)
      PARAMETER (MMAX=20)  
      INTEGER J,K,L,MFIT  
      my_real ERRPRE,ATRY(MMAX),BETA(MMAX),DA(MMAX)  
      SAVE ERRPRE,ATRY,BETA,DA,MFIT  
      INTEGER IFUNCS, IRET
C=======================================================================

      IRET = 0

      IF (GAMMA < ZERO) THEN  
        MFIT=0  
        DO J=1,MA  
          IF (IA(J)/=0) MFIT=MFIT+1  
        ENDDO  

!       in Numerical Recript, 0.001 is used, however from my tests, the
!       difference is small (0.01 vs 0.001)
        GAMMA=0.01
       
        CALL MRQCOF(X,Y,SIG,NDATA,
     .              A,IA,MA,ALPHA,BETA,NCA,ERRNOW,
     .              IFUNCS,ICOMP)  

        ERRPRE=ERRNOW  
        DO J=1,MA  
          ATRY(J)=A(J)  
        ENDDO  
      ENDIF  
C
      DO J=1,MFIT  
        DO K=1,MFIT  
          COVAR(J,K)=ALPHA(J,K)  
        ENDDO  
        COVAR(J,J)=ALPHA(J,J)*(1.+GAMMA)  
        DA(J)=BETA(J)  
      ENDDO  
C
      CALL INVERSION(COVAR,MFIT,NCA,DA,1,1,IRET)  
      IF (IRET /= 0) THEN
        IF (IDEBUG > 0) THEN
          WRITE(*,*) ' IRET = ', IRET
          DO J=1,MA  
            WRITE(IOUT, *) 'J, A(J) = ', J, A(J)
          ENDDO  
        ENDIF
        RETURN
      ENDIF

      IF (GAMMA == ZERO) THEN  
        CALL COVSRT(COVAR,NCA,MA,IA,MFIT)  
        RETURN  
      ENDIF  
C
      J=0  
      DO L=1,MA  
        IF(IA(L)/=0) THEN  
          J=J+1  
          ATRY(L)=A(L)+DA(J)  
        ENDIF  
      ENDDO  

      IF (IDEBUG > 0) THEN
        WRITE(IOUT,*) __FILE__,__LINE__
        DO J=1, MA
          write(IOUT,*) 'J,ATRY(J) = ', J, ATRY(J)
        ENDDO
      ENDIF

c     limit ATRY (ALPHA) to avoid too big power (overflow)
c     |ALPHA_i| <= 20.0
      DO J=1, MA-1, 2
        if (ATRY(J+1) > TWENTY) then
          IF (IDEBUG > 0) THEN 
            write(IOUT,*) 'Setting Big ALPHA to ',TWENTY
            write(IOUT,*) '  J, ATRY(J) = ',J+1,ATRY(J+1)
          ENDIF
          ATRY(J+1) = TWENTY
        ELSEIF (ATRY(J+1) < -TWENTY) THEN
          IF (IDEBUG > 0) THEN
            write(IOUT,*) 'Setting Big ALPHA to -',TWENTY
            write(IOUT,*) '  J, ATRY(J) = ',J+1,ATRY(J+1)
          ENDIF
          ATRY(J+1) = -TWENTY
        ENDIF
      ENDDO

C
      CALL MRQCOF(X,Y,SIG,NDATA,
     .            ATRY,IA,MA,COVAR,DA,NCA,ERRNOW,
     .            IFUNCS,ICOMP)  
C
      IF (ERRNOW < ERRPRE) THEN  
        GAMMA=0.1*GAMMA  
        ERRPRE=ERRNOW  
        DO J=1,MFIT  
          DO K=1,MFIT  
            ALPHA(J,K)=COVAR(J,K)  
          ENDDO  
          BETA(J)=DA(J)  
        ENDDO  
        DO L=1,MA  
          A(L)=ATRY(L)  
        ENDDO  
      ELSE  
        GAMMA=10.*GAMMA  
        ERRNOW=ERRPRE  
      ENDIF  
C-----------
      RETURN  
      END SUBROUTINE MRQMIN
C-----------
C
C
Chd|====================================================================
Chd|  MRQCOF                        source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        MRQMIN                        source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        OGDEN0                        source/materials/tools/nlsqf.F
Chd|        OGDEN1                        source/materials/tools/nlsqf.F
Chd|====================================================================
      SUBROUTINE MRQCOF(X,Y,SIG, NDATA,
     * A,IA,MA,ALPHA,BETA,NALP,
     * ERRNOW,IFUNCS,ICOMP)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MA,NALP,NDATA,IA(MA),ICOMP 
      my_real 
     .    ERRNOW,A(MA),ALPHA(NALP,NALP),BETA(MA),
     .    X(NDATA),Y(NDATA),SIG(NDATA)
      INTEGER IFUNCS
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER MFIT,I,J,K,L,M,MMAX   
      PARAMETER (MMAX=20)  
      my_real 
     .   DY,WT,YMOD,DYDA(MMAX)

      my_real Y_MIN

      Y_MIN = EM3

C=======================================================================  
      MFIT=0  
      DO 11 J=1,MA  
        IF (IA(J)/=0) MFIT=MFIT+1  
11    CONTINUE  
      DO 13 J=1,MFIT  
        DO 12 K=1,J  
          ALPHA(J,K)=0.  
12      CONTINUE  
        BETA(J)=0.  
13    CONTINUE  
      ERRNOW=ZERO
      IF(ICOMP == 0) THEN
         DO 16 I=1,NDATA  
           IF (IFUNCS == 1) THEN
             CALL OGDEN0(X(I),A,YMOD, MA)  
             CALL OGDEN1(X(I),A,DYDA, IA, MA)  
           ELSE
!            TBD, UNKOWN IFUNCS, ERROR OUT!!!        
             WRITE(*,*) 'ERROR: UNKOWN IFUNCS = ', IFUNCS
             CALL MY_EXIT(2)
           ENDIF

           DY=Y(I)-YMOD
           ERRNOW=ERRNOW+DY*DY/(SIG(I)*SIG(I))
c          IF (Y(I)>EM5) DY=Y(I)-YMOD
c           IF (Y(I)<EM5) DY=EM5

           J=0  
           DO 15 L=1,MA  
             IF(IA(L)/=0) THEN  
               J=J+1  
                WT=DYDA(L)
                WT=DYDA(L)/(SIG(I)*SIG(I))
               K=0  
               DO 14 M=1,L  
                 IF(IA(M)/=0) THEN  
                   K=K+1  
                   ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(M)
                 ENDIF  
14             CONTINUE  
               BETA(J)=BETA(J)+DY*WT  
             ENDIF  
15         CONTINUE  
16       CONTINUE 
      ELSE
         DO  I=1,NDATA  
           IF (IFUNCS == 1) THEN
             CALL OGDEN0(X(I),A,YMOD, MA)  
             CALL OGDEN1(X(I),A,DYDA, IA, MA)  
           ELSE
!            TBD, UNKOWN IFUNCS, ERROR OUT!!!        
             WRITE(*,*) 'ERROR: UNKOWN IFUNCS = ', IFUNCS
             CALL MY_EXIT(2)
           ENDIF

           DY=Y(I)-YMOD
           ERRNOW=ERRNOW + DY*DY
           J=0  
           DO  L=1,MA  
             IF(IA(L)/=0) THEN  
               J=J+1  
               WT=DYDA(L)
               K=0  
               DO  M=1,L  
                 IF(IA(M)/=0) THEN  
                   K=K+1  
                   ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(M)
                 ENDIF  
               ENDDO ! M
               BETA(J)=BETA(J)+DY*WT  
             ENDIF  
         ENDDO  ! L
       ENDDO   ! I  
      ENDIF 
      DO 18 J=2,MFIT  
        DO 17 K=1,J-1  
          ALPHA(K,J)=ALPHA(J,K)  
17      CONTINUE  

18    CONTINUE  
      RETURN  
      END SUBROUTINE MRQCOF
C
      
Chd|====================================================================
Chd|  INVERSION                     source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        MRQMIN                        source/materials/tools/nlsqf.F
Chd|        MRQMIN_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INVERSION(A,N,NP,B,M,MP,IRET)  
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M,MP,N,NP  
      my_real A(NP,NP),B(NP,MP)  
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,NMAX,ICOL,IROW,J,K,L,LL
      PARAMETER (NMAX=50)  
      INTEGER INDXC(NMAX),INDXR(NMAX),IPIV(NMAX)  
      my_real 
     .   BIG,DUM,PIVINV  

      INTEGER IRET

      IRET = 0

      ICOL = -1
      IROW = -1

C=======================================================================
      DO 11 J=1,N  
        IPIV(J)=0  
11    CONTINUE  
      DO 22 I=1,N  
        BIG=ZERO 
        DO 13 J=1,N  
          IF(IPIV(J)/=1)THEN  
            DO 12 K=1,N  
              IF (IPIV(K)==0) THEN  
                IF (ABS(A(J,K))>=BIG)THEN  
                  BIG=ABS(A(J,K))  
                  IROW=J  
                  ICOL=K  
                ENDIF  
              ELSE IF (IPIV(K)>1) THEN  
                IRET = __LINE__
                WRITE(*,*) 'SINGULAR MATRIX IN INVERSION'
                RETURN
              ENDIF  
12          CONTINUE  
          ENDIF  
13      CONTINUE  

        IF (ICOL == -1) THEN
          IRET = __LINE__
          WRITE(*,*) 'FATAL ERROR IN INVERSION'
          RETURN
        ENDIF

        IPIV(ICOL)=IPIV(ICOL)+1  
        IF (IROW/=ICOL) THEN  
          DO 14 L=1,N  
            DUM=A(IROW,L)  
            A(IROW,L)=A(ICOL,L)  
            A(ICOL,L)=DUM  
14        CONTINUE  
          DO 15 L=1,M  
            DUM=B(IROW,L)  
            B(IROW,L)=B(ICOL,L)  
            B(ICOL,L)=DUM  
15        CONTINUE  
        ENDIF  
        INDXR(I)=IROW  
        INDXC(I)=ICOL  
        IF (A(ICOL,ICOL)==0.) THEN
          IRET = __LINE__
          WRITE(*,*) 'SINGULAR MATRIX IN INVERSION'
          RETURN
        ENDIF
        PIVINV=1./A(ICOL,ICOL)  
        A(ICOL,ICOL)=1.  
        DO 16 L=1,N  
          A(ICOL,L)=A(ICOL,L)*PIVINV  
16      CONTINUE  
        DO 17 L=1,M  
          B(ICOL,L)=B(ICOL,L)*PIVINV  
17      CONTINUE  
        DO 21 LL=1,N  
          IF(LL/=ICOL)THEN  
            DUM=A(LL,ICOL)  
            A(LL,ICOL)=0.  
            DO 18 L=1,N  
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM  
18          CONTINUE  
            DO 19 L=1,M  
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM  
19          CONTINUE  
          ENDIF  
21      CONTINUE  
22    CONTINUE  
      DO 24 L=N,1,-1  
        IF(INDXR(L)/=INDXC(L))THEN  
          DO 23 K=1,N  
            DUM=A(K,INDXR(L))  
            A(K,INDXR(L))=A(K,INDXC(L))  
            A(K,INDXC(L))=DUM  
23        CONTINUE  
        ENDIF  
24    CONTINUE  
C-----------
      RETURN  
      END SUBROUTINE INVERSION
C
C
Chd|====================================================================
Chd|  COVSRT                        source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        MRQMIN                        source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COVSRT(COVAR,SIZCOVAR,MA,IA,MFIT)  
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------------------------
      INTEGER MA,MFIT,SIZCOVAR,IA(MA)  
      my_real COVAR(SIZCOVAR,SIZCOVAR),SWAP
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,J,K  
C=======================================================================      
      DO 12 I=MFIT+1,MA  
        DO 11 J=1,I  
          COVAR(I,J)=0.  
          COVAR(J,I)=0.  
11      CONTINUE  
12    CONTINUE  
      K=MFIT  
      DO 15 J=MA,1,-1  
        IF(IA(J)/=0)THEN  
          DO 13 I=1,MA  
            SWAP=COVAR(I,K)  
            COVAR(I,K)=COVAR(I,J)  
            COVAR(I,J)=SWAP  
13        CONTINUE  
          DO 14 I=1,MA  
            SWAP=COVAR(K,I)  
            COVAR(K,I)=COVAR(J,I)  
            COVAR(J,I)=SWAP  
14        CONTINUE  
          K=K-1  
        ENDIF  
15    CONTINUE  
C-----------
      RETURN  
      END SUBROUTINE COVSRT

!>    @purpose
!>      calculate stress from stretch for OGDEN (uniaxial tension)
!>    @param[in]    X      stretch
!>    @param[in]    A(NA)  material parameters [mu_1, alpha_1,mu_2, alpha_2,...]
!>    @param[out]   Y      stress 
Chd|====================================================================
Chd|  OGDEN0                        source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|        MRQCOF                        source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE OGDEN0(X,A,Y,NA)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NA
      my_real X, A(NA), Y
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I
C=======================================================================  
      Y=ZERO 
      DO I=1,NA-1,2  
       Y = Y + A(I)*(X**(-1.+A(I+1))-X**(-0.5*A(I+1)-1.))
      ENDDO  
C-----------
      RETURN  
      END SUBROUTINE OGDEN0
 
!>    @purpose
!>      calculate d(stress)/d(A) OGDEN (uniaxial tension)
!>    @param[in]    X             stretch
!>    @param[in]    A(NA)         material parameters [mu_1, alpha_1,mu_2, alpha_2,...]
!>    @param[in]    IA(NA)        flags if derivative needs to be calculated
!>                                1=yes, 0=no
!>    @param[out]   DYDA(NA)      derivatives d(stress)/d(A)
Chd|====================================================================
Chd|  OGDEN1                        source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        MRQCOF                        source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE OGDEN1(X,A,DYDA,IA,NA)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NA
      INTEGER IA(NA)
      my_real X, A(NA),DYDA(NA) 
      my_real TMP1, TMP2
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I
C=======================================================================  
      DO I=1,NA-1,2  
       TMP1 = X**(-1.+A(I+1))
       TMP2 = X**(-0.5*A(I+1)-1.)
       IF (IA(I) /= 0) THEN
         DYDA(I)   = TMP1 - TMP2
       ENDIF

       IF (IA(I+1) /= 0) THEN
         DYDA(I+1) = A(I)*LOG(X)*(TMP1 + 0.5*TMP2)
       ENDIF
      ENDDO  

      RETURN  
      END SUBROUTINE OGDEN1

!>    @param[in]  LAWID           law_ID on /MAT/LAW69 (1=Ogden, 2=Mooney-Rivlin)
!>    @param[in]  ICHECK    checking level
!>                <=0      no constraint (IVALID always return as 1)
!>                1        SUM( mu(i) * alpha(i) ) > 0.0
!>                2        mu(i) * alpha(i) > 0.0
!>                3        Try ICHECK=2 at first, if it fails in fitting, switch to ICHECK=1, and try again
!>    @param[in]   ERRTOL      If ERRAVE < ERRTOL, data fitting converges.
!>                                 ERRAVE = ( SUM [ ABS ( ( Y_inp-Y_fit) / Y_inp )  ) / NPT
!>    @param[out]  MUAL(1:NMUAL)   optimized material properties 
Chd|====================================================================
Chd|  LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        LAW69_CHECK                   source/materials/tools/nlsqf.F
Chd|        LAW69_GUESS1                  source/materials/tools/nlsqf.F
Chd|        LAW69_GUESS_BOUNDS            source/materials/tools/nlsqf.F
Chd|        MRQMIN                        source/materials/tools/nlsqf.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        OGDEN0                        source/materials/tools/nlsqf.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAW69_NLSQF(LAWID,STRETCH,Y,MA,NMUAL,NPT,MUAL,
     $                       ICHECK, NSTART, ERRTOL,ID,TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER MAXA 
      PARAMETER (MAXA=20)

      INTEGER LAWID, NPT, MA, ICHECK, NSTART
      my_real ERRTOL
      INTEGER I,NONZERO(MAXA),IDUM,ITER,ICOUNT,J,K,NMUAL  
      my_real GAMMA,ERRNOW,GASDEV,ERRPRE,
     .        STRETCH(NPT),MUAL(10)
      my_real A(MAXA),COVAR(MAXA,MAXA),ALPHA(MAXA,MAXA),Y(NPT)
      my_real MCOF_MIN(MAXA), MCOF_MAX(MAXA)
      my_real YOGD,XOGD

      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER MINITER_LM, MAXITER_LM
      DATA MINITER_LM /5/
      SAVE MINITER_LM

      DATA MAXITER_LM /100/
      SAVE MAXITER_LM

      ! if ( (ERRNOW-ERRPRE) / ERRPRE ) < EPS_LM, CNT_HIT_EPS_LM = CNT_HIT_EPS_LM + 1,
      ! if CNT_HIT_EPS_LM == LMT_HIT_EPS_LM, it converges
      my_real EPS_LM 
      DATA EPS_LM /1E-3/
      SAVE EPS_LM

      INTEGER CNT_HIT_EPS_LM
      INTEGER LMT_HIT_EPS_LM
      DATA LMT_HIT_EPS_LM /2/
      SAVE LMT_HIT_EPS_LM

      INTEGER LMSTOP

      INTEGER IFUNCS

      INTEGER NGUESS
      my_real A0(MAXA)
      my_real ERRMIN, ERRMAX, ERRAVE, ERRAVE_MIN, ERR

      INTEGER ISTART, NPSAVED, IVALID,ICOMP

      INTEGER ICURV
      LOGICAL lopened

c     during LM optimization, if the objective is not improved, 
c     GAMMA will be increased. We should terminate the iteration once
c     GAMMA becomes very huge.
      my_real GAMMA_STOP
      data GAMMA_STOP /1E15/
      save GAMMA_STOP

c     if check the validity of initial guess (0=no, 1=yes)
c     we don't want to check, because invalid initial point could converge to valid point.
      INTEGER ICHECK_GUESS
      data ICHECK_GUESS /0/
      save ICHECK_GUESS

      INTEGER JCHECK, IFIT_SUCCESS

c     if enforce mu(i) < mu(i+1) in generating initial guess
c     we don't need this anymore since we are using random numbers instead of loop through all
c     the combinations
      INTEGER MU_INCR_GUESS
      data MU_INCR_GUESS /0/
      save MU_INCR_GUESS

      my_real MAX_ABS_YI
      my_real SMALL_FAC_ABS_YI, SMALL_ABS_YI

      my_real, DIMENSION(:), ALLOCATABLE :: SIG
      my_real  SPREADY
      INTEGER IRET
      INTEGER IRND1

      ALLOCATE (SIG(1:NPT))

      IF ( MAXA < MA ) THEN
        WRITE(*,*) 'ERROR, MAXA < MA'
        WRITE(*,*) __FILE__,__LINE__
        CALL MY_EXIT(2)
      ENDIF
      ICOMP=0
      IF(ICHECK < 0) ICOMP= 1
      ICHECK = ABS(ICHECK)
      ! IF ABS(Y(I)) <  SMALL_ABS_YI, use SMALL_ABS_YI to avoid
      ! unnecessary numerical issues when avoid divided by small value

      MAX_ABS_YI = ZERO
      DO I = 1, NPT
        MAX_ABS_YI = max( MAX_ABS_YI, ABS(Y(I)) )
      ENDDO

      SMALL_FAC_ABS_YI = EM3 

      SMALL_ABS_YI = MAX_ABS_YI * SMALL_FAC_ABS_YI

      IF (IDEBUG > 0) THEN
        WRITE(IOUT, *) ' MAX_ABS_YI, SMALL_FAC_ABS_YI, SMALL_ABS_YI = ',
     $                   MAX_ABS_YI, SMALL_FAC_ABS_YI, SMALL_ABS_YI
      ENDIF

      SPREADY=0.01
      IF(ICOMP == 0) THEN
         DO J=1,NPT
           SIG(J)=SPREADY*max(SMALL_ABS_YI, ABS(Y(J)) )
           IF (IDEBUG > 0) THEN
             WRITE(IOUT, *) 'J, SIG(J) = ', J,  SIG(J)
           ENDIF
         ENDDO   
      ELSE
        DO J=1,NPT
         SIG(J) = Y(J)
         IF(SIG(J) == ZERO)  SIG(J) = EM15
          IF (IDEBUG > 0) THEN
             WRITE(IOUT, *) 'J, SIG(J) = ', J,  SIG(J)
          ENDIF
        ENDDO  
      ENDIF      
     
      IF (MAXA < MA) THEN
!       TBD_KANG      
        WRITE(*,*) 'ERROR: MAXA < MA'
        WRITE(*,*) ' MAXA = ', MAXA
        WRITE(*,*) ' MA = ', MAXA
        WRITE(*,*) ' FILE = ', __FILE__
        WRITE(*,*) ' LINE = ', __LINE__
        CALL MY_EXIT(2)
      ENDIF

C=======================================================================      
      DO I=1,NMUAL 
        NONZERO(I)=1  
      ENDDO

      IF (LAWID == 1) then ! OGDEN
        DO I=NMUAL+1,10
          NONZERO(I)=0
          A(I)=ZERO
        ENDDO

        IFUNCS = 1 ! SUBROUTINE OGDEN0/OGDEN1

      ELSEIF (LAWID == 2) then ! Mooney-Rivlin
        DO I=NMUAL+1,MA
          NONZERO(I)=0
        ENDDO
        NONZERO(2)=0
        NONZERO(4)=0

        DO I=NMUAL+1,MA
          A(I)=ZERO
        ENDDO

        IFUNCS = 1 ! SUBROUTINE OGDEN0/OGDEN1

      ENDIF

      JCHECK =ICHECK
      IF (JCHECK == 3) THEN
        JCHECK = 2
      ENDIF

99    CONTINUE      ! location to start optimization with different JCHECK

      IFIT_SUCCESS = 0 

      ISTART = 0
      NPSAVED = 0

c     calculate MCOF_MIN, MCOF_MAX
      CALL LAW69_GUESS_BOUNDS(LAWID, JCHECK, STRETCH, Y, NPT, 
     $         NMUAL, MCOF_MIN, MCOF_MAX,ICOMP)

c     initialize random number generator in LAW69_GUESS1
      IRND1 = 205

      DO 111 WHILE ( ISTART < NSTART )
c       get one guess point A0(1:NMUAL)        
        CALL LAW69_GUESS1(
     $       LAWID, NMUAL, ICHECK_GUESS, MU_INCR_GUESS,IRND1,
     $       A0, NONZERO, MCOF_MIN, MCOF_MAX, NGUESS)

        IF (NGUESS == 0) THEN
          GOTO 112
        ENDIF

C       calculate averaged ERROR before LM         
        ERRAVE = 0.0
        IF(ICOMP == 0) THEN
          DO I=1,NPT
            CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
            ERR = ABS(Y(I) - YOGD) / max(SMALL_ABS_YI, ABS(Y(I)))
            ERRAVE = ERRAVE + ERR
          ENDDO 
        ELSE
          DO I=1,NPT
            CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
            ERR = ABS(Y(I) - YOGD) / MAX(EM15,ABS(Y(I)))
            ERRAVE = ERRAVE + ERR
          ENDDO 
        ENDIF 
C            
        ERRAVE = ERRAVE / (1.0 * NPT)

        ISTART = ISTART + 1

        IF (IDEBUG > 0) THEN
          WRITE(IOUT,*) ' ISTART = ', ISTART
          WRITE(IOUT,*) ' Before LM optimization ...'
          DO I = 1, NMUAL
            WRITE(IOUT, *) ' I, A0(I) = ', I, A0(I)
          ENDDO
          WRITE(IOUT,*) ' LM0, ERRAVE = ',  ERRAVE
        ENDIF

!       start loop of LM optimization        
        ITER = 0
        CNT_HIT_EPS_LM = 0

        ITER = ITER + 1
        GAMMA=-1
        CALL MRQMIN(STRETCH,Y,SIG,NPT,A0,NONZERO,
     $       NMUAL,COVAR,ALPHA,MA,ERRNOW, 
     $       IFUNCS,GAMMA,IRET,ICOMP)  
        IF (IRET /= 0) GOTO 111

        IF (IDEBUG > 0) THEN
          ERRAVE = 0.0
          IF(ICOMP == 0) THEN
            DO I=1,NPT
              CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
              ERR = ABS(Y(I) - YOGD) / max(SMALL_ABS_YI, ABS(Y(I)))
              ERRAVE = ERRAVE + ERR
            ENDDO 
          ELSE
            DO I=1,NPT
              CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
              ERR = ABS(Y(I) - YOGD) / MAX(EM15,ABS(Y(I)))
              ERRAVE = ERRAVE + ERR
            ENDDO 
          ENDIF       
          ERRAVE = ERRAVE / (1.0 * NPT)

          WRITE(IOUT, '(A,I4, 3E16.8)') 
     $            'ITER, ERRNOW, GAMMA, ERRAVE = ', 
     $            ITER, ERRNOW, GAMMA, ERRAVE
          DO I = 1, NMUAL
            WRITE(IOUT, *) '   - I, A0(I) = ', I, A0(I)
          ENDDO
        ENDIF

21      CONTINUE        
        ERRPRE=ERRNOW  

        ITER = ITER + 1
        CALL MRQMIN(STRETCH,Y,SIG,NPT,A0,NONZERO,
     $       MA,COVAR,ALPHA, MA, ERRNOW, 
     $       IFUNCS,GAMMA,IRET,ICOMP)  
        IF (IRET /= 0) GOTO 111

        IF (IDEBUG == 1) THEN
          ERRAVE = ZERO
          IF(ICOMP == 0) THEN
           DO I=1,NPT
             CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
             ERR = ABS(Y(I) - YOGD) / max(SMALL_ABS_YI, ABS(Y(I)))
             ERRAVE = ERRAVE + ERR
           ENDDO 
          ELSE
           DO I=1,NPT
             CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
             ERR = ABS(Y(I) - YOGD) / MAX(EM15,ABS(Y(I)))
             ERRAVE = ERRAVE + ERR
           ENDDO 
          ENDIF     
          ERRAVE = ERRAVE / (1.0 * NPT)

          WRITE(IOUT, '(A,I4, 5E16.8)') 
     $     'ITER, ERRNOW, GAMMA, ERRAVE, ERRNOW-ERRPRE,'//
     $     '(ERRNOW-ERRPRE)/ERRPRE = ', 
     $      ITER, ERRNOW, GAMMA, ERRAVE, ERRNOW-ERRPRE, 
     $      (ERRNOW-ERRPRE)/ERRPRE
          DO I = 1, NMUAL
            WRITE(IOUT, *) '   - I, A0(I) = ', I, A0(I)
          ENDDO
        ENDIF

c       IF A0(J) is too small, restart from next initial guess        
        DO J = 1, NMUAL
          IF ( ABS( A0(J) ) < 1E-20 ) THEN
             goto 111  ! restart from next initial guess
c            WRITE(IOUT, *) ' A is too small, and could result in error'
c            WRITE(IOUT,*) 'J, A0(J) = ', J, A0(J)
c            STOP
          ENDIF
        ENDDO

c       check convergence of LM optimization        
        LMSTOP = 0
        IF (IDEBUG > 0) THEN
          WRITE(IOUT, *) ' ERRNOW/(1.0*NPT) = ', ERRNOW/(1.0*NPT)
          WRITE(IOUT, *) ' ERRNOW < ERRPRE = ', (ERRNOW < ERRPRE)
        ENDIF

        IF ( ITER > MINITER_LM ) THEN
          IF (ERRNOW < ERRPRE) THEN
            IF ( ABS(ERRPRE) > ZERO) THEN
              IF  ( ABS( (ERRNOW-ERRPRE)/ ERRPRE ) < EPS_LM) THEN
                CNT_HIT_EPS_LM = CNT_HIT_EPS_LM + 1

                IF (IDEBUG > 0) THEN
                  WRITE(IOUT,*) 
     $            ' CNT_HIT_EPS_LM, ABS((ERRNOW-ERRPRE)/ERRPRE)  = ', 
     $            CNT_HIT_EPS_LM, ABS( (ERRNOW-ERRPRE)/ ERRPRE )
                ENDIF

                IF ( CNT_HIT_EPS_LM >= LMT_HIT_EPS_LM ) THEN
                  IF (IDEBUG > 0) THEN
                    WRITE(IOUT,*) 'STOP AT ', __LINE__
                  ENDIF
                  LMSTOP = 1
                ENDIF
              ENDIF
            ENDIF
          ELSEIF (ITER >= MAXITER_LM .OR. GAMMA >= GAMMA_STOP) THEN
            IF (IDEBUG > 0) THEN
              WRITE(IOUT,*) 'STOP AT ', __LINE__
            ENDIF
            LMSTOP = 1
          ENDIF  
        ENDIF

        IF (LMSTOP== 0) THEN
          GOTO 21
        ENDIF
!       end loop of LM optimization        

        ERRAVE = 0.0
        IF(ICOMP == 0) THEN
          DO I=1,NPT
            CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
            ERR = ABS(Y(I) - YOGD) / max(SMALL_ABS_YI, ABS(Y(I)))
            ERRAVE = ERRAVE + ERR
          ENDDO 
        ELSE
          DO I=1,NPT
            CALL OGDEN0(STRETCH(I), A0, YOGD, NMUAL)
            ERR = ABS(Y(I) - YOGD) / MAX(EM15,ABS(Y(I)))
            ERRAVE = ERRAVE + ERR
          ENDDO 
        ENDIF 
        ERRAVE = ERRAVE / (1.0 * NPT)

        IF (IDEBUG > 0) THEN
           WRITE(IOUT,*) ' After LM optimization ...'
           DO I = 1, MA
             WRITE(IOUT, *) ' I, A0(I) = ', I, A0(I)
           ENDDO
           WRITE(IOUT,*) ' LM1, ERRAVE = ', ERRAVE
        ENDIF

        CALL LAW69_CHECK(LAWID, A0, NMUAL, JCHECK, 0, IVALID)
        IF (IVALID > 0) THEN
          IF (   NPSAVED==0 .OR. 
     $         ( NPSAVED>0 .AND. ERRAVE<ERRAVE_MIN) ) THEN
            NPSAVED = NPSAVED + 1
            ERRAVE_MIN = ERRAVE
            DO I = 1, NMUAL
              A(I) = A0(I)
            ENDDO
          ENDIF
        ELSE
          IF (IDEBUG > 0) THEN
            WRITE(IOUT,*) __FILE__,__LINE__
            WRITE(IOUT,*) ' LM converges to invalid point'
          ENDIF
        ENDIF

        IF (NPSAVED > 0) THEN
          IF (IDEBUG > 0) THEN
            WRITE(*, *) ' ISTART, NPSAVED, ERRAVE_MIN = ', 
     $                    ISTART, NPSAVED, ERRAVE_MIN
            WRITE(IOUT, *) ' ISTART, NPSAVED, ERRAVE_MIN = ', 
     $                    ISTART, NPSAVED, ERRAVE_MIN
          ENDIF

          IF ( ERRAVE_MIN < ERRTOL ) THEN
            IFIT_SUCCESS = 1
            GOTO 112
          ENDIF
        ELSE
          IF (IDEBUG > 0) THEN
            WRITE(*, *) ' ISTART, NPSAVED ', 
     $                    ISTART, NPSAVED
            WRITE(IOUT, *) ' ISTART, NPSAVED ', 
     $                    ISTART, NPSAVED
          ENDIF
        ENDIF

111   CONTINUE  ! WHILE ( ISTART < NSTART )

112   CONTINUE

      IF (IFIT_SUCCESS == 0  .AND. ICHECK == 3) THEN
        IF (JCHECK == 2) THEN
          JCHECK = 1
          IF (IDEBUG > 0) THEN
            IF (NPSAVED > 0) THEN
              WRITE(*,'(A)') 
     $            '*** Curve fitting result of /MAT/LAW69 with'
     $            //' ICHECK=2 is not satisfactory.'
              WRITE(*,'(A,F10.2,A)') 'ERRAVE = ', ERRAVE_MIN*100.0,'%'
              WRITE(*,'(A)') ' Switching to ICHECK=1 and trying again.'
            
            ELSE
              WRITE(*,'(A)') 
     $         '*** Failed to fit /MAT/LAW69 with ICHECK=2.'
              WRITE(*,'(A)') ' Switching to ICHECK=1 and trying again.'
            ENDIF
          ENDIF
          GOTO 99
        ENDIF
      ENDIF

      DEALLOCATE (SIG)

      IF (IFIT_SUCCESS == 0) THEN
        IF (NPSAVED == 0) THEN
          CALL ANCMSG(MSGID=901,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
      ENDIF
C
      DO I=1,10
        MUAL(I)=A(I)
      ENDDO
 
      IF (LAWID == 2) THEN
        DO I=5,10
          MUAL(I) = ZERO
        ENDDO
      ENDIF

      WRITE(IOUT,'(//6X,A,/)')'FITTING RESULT COMPARISON:'
      WRITE(IOUT,'(6X,A,/)')'UNIAXIAL TEST DATA'
      WRITE(IOUT,'(A20,5X,A20,A30)') 'NOMINAL STRAIN',
     *      'NOMINAL STRESS(TEST)', 'NOMINAL STRESS(RADIOSS)'
    
c     output curcves to .csv format to simplify visualization    
      ICURV = 0
      IF (IOCSV> 0) THEN
        DO ICURV = 25, 99
          inquire (unit=ICURV, opened=lopened) 
          if (.not. lopened) goto 77
        ENDDO
77      CONTINUE
        OPEN(UNIT=ICURV, FILE='curv.csv')
        WRITE(ICURV,'(A)') 'NOMINAL STRAIN, NOMINAL STRESS(TEST), '//
     $                     'NOMINAL STRESS(RADIOSS)'
      ENDIF

      DO I=1,NPT
        CALL OGDEN0(STRETCH(I), A, YOGD, NMUAL)
!!        WRITE(IOUT,'(F18.4,F20.4,F28.4)') 
        WRITE(IOUT, '(1F18.6, 3X,1F20.13, 6X, 1F20.13)')
     *   STRETCH(I)-ONE,Y(I),YOGD
        IF (ICURV > 0) THEN
           WRITE(ICURV, '(F18.4, A, F18.4, A, F18.4)') 
     $            STRETCH(I)-ONE,',',Y(I),',',YOGD
        ENDIF
      ENDDO      

      WRITE(IOUT, *) ''
      WRITE(IOUT, '(A)') '-------------------------------------------'
      WRITE(IOUT, '(A,F10.2,A)') 'AVERAGED ERROR OF FITTING : ', 
     $                            ERRAVE_MIN*100.0, '%'

c     output curcves to .csv format to simplify visualization    
      IF ( ICURV > 0) THEN
        CLOSE(ICURV)
      ENDIF
C-----------
      RETURN
      END 

c     if abs(value) < small, it means value is very close to zero,
c     reset it to be small or -small to avoid potential numerical issue
Chd|====================================================================
Chd|  ZERO2SMALL                    source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_GUESS1                  source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZERO2SMALL(value, small)
#include      "implicit_f.inc"
      my_real value, small

      if ( abs(value) < small ) then
        if (value >= 0.0) then
          value = small
        else
          value = -small
        endif
      endif
      END SUBROUTINE ZERO2SMALL

!>    @purpose
!>      get one guess for starting Levenberg-Marquardt method
!>    @param[in]   LAWID                material type(1=Ogden, 2=Mooney-Rivlin)
!>    @param[out]  NGUESS               Number of returned guess 
!>                                      0 = no return,already go through all combinations, 
!>                                      1 = return one guess point successfully
!>    @param[out]  A0(NMUAL)             returned guess 
Chd|====================================================================
Chd|  LAW69_GUESS1                  source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|        LAW69_CHECK                   source/materials/tools/nlsqf.F
Chd|        ZERO2SMALL                    source/materials/tools/nlsqf.F
Chd|        RAND1                         source/materials/tools/nlsqf.F
Chd|====================================================================
      SUBROUTINE LAW69_GUESS1(
     $   LAWID, NMUAL, ICHECK, MU_INCR, IRND1,
     $   A0, IA, MCOF_MIN, MCOF_MAX, NGUESS)
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------

      REAL RAND1
      EXTERNAL RAND1

      INTEGER LAWID, NMUAL, ICHECK, MU_INCR, IRND1, NGUESS
      my_real A0(NMUAL)
      INTEGER IA(NMUAL)

      my_real SMALL
      data SMALL /1E-15/
      save SMALL

      my_real MCOF_MIN(NMUAL) ! lower bounds of material parameters
      my_real MCOF_MAX(NMUAL) ! lower bounds of material parameters

      REAL*4 randnum

      INTEGER I, IVALID

      NGUESS = 0

      IF (LAWID == 2) THEN
        A0(2) = 2.0
        A0(4) = -2.0
      ENDIF

      DO WHILE (.TRUE.)
        DO I = 1, NMUAL
          IF (IA(I) > 0) THEN
            randnum = RAND1(IRND1)
            A0(I) = (1.0-randnum)*MCOF_MIN(I) + randnum*MCOF_MAX(I)
            CALL ZERO2SMALL(A0(I), SMALL)
          ENDIF
        ENDDO

        CALL LAW69_CHECK( LAWID,  A0, NMUAL, 
     $                    ICHECK, MU_INCR, IVALID)
        IF (IVALID == 1) THEN
          NGUESS = 1
          GOTO 299
        ENDIF
      ENDDO

299   CONTINUE            

      END SUBROUTINE LAW69_GUESS1

!>    @purpose
!>       check if the guess of material properties is valid for LM optimization
!>    @param[in]  ICHECK    checking level
!>                <=0      no constraint (IVALID always return as 1)
!>                1        SUM( mu(i) * alpha(i) ) > 0.0
!>                2        mu(i) * alpha(i) > 0.0
!>    @param[in]  MU_INCR  if condition mu(i) <= mu(i+1) is checked              
!>                         0=no, 1=yes
!>    @param[out] IVALID   0=not valid, 1=valid
Chd|====================================================================
Chd|  LAW69_CHECK                   source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_GUESS1                  source/materials/tools/nlsqf.F
Chd|        LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAW69_CHECK(LAWID, A, NMUAL, 
     $                       ICHECK, MU_INCR, IVALID)
#include      "implicit_f.inc"
#include      "units_c.inc"
      INTEGER LAWID, NMUAL, ICHECK, MU_INCR, IVALID
      my_real A(NMUAL)

c     local vars
      INTEGER J
      my_real SUM_MU_X_AF

      IF (IDEBUG > 0) THEN
        WRITE(IOUT,*) 'LAW69_CHECK ...'
        DO J = 1, NMUAL
          WRITE(IOUT,*) 'J, A(J) = ', J, A(J)
        ENDDO
      ENDIF

      IVALID = 1
      IF (ICHECK == 0) GOTO 49

      IF (MU_INCR > 0) THEN
        IF (LAWID == 1) THEN 
          IF (NMUAL >= 4) THEN
            DO J = 1, NMUAL/2-1
              IF ( A(2*J-1) > A(2*(J+1)-1) ) THEN
                IF (IDEBUG > 0) THEN
                  WRITE(IOUT,*) 'J,2*J-1,2*(J+1)-1 = ', 
     $                          J,2*J-1,2*(J+1)-1
                  WRITE(IOUT,*) ' A(2*J-1), A(2*(J+1)-1) = ', 
     $                          A(2*J-1), A(2*(J+1)-1)
                  WRITE(IOUT,*) ' MU(i) > MU(i+1) '
                ENDIF
                IVALID = 0
                goto 49
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF

      IF (ICHECK == 1) THEN
C       CHECK IF SUM(MU_I * ALPHA_I) > 0.0                   
        SUM_MU_X_AF = 0.0
        DO J = 1, NMUAL/2
          SUM_MU_X_AF = SUM_MU_X_AF + A(2*J-1) * A(2*J)
        ENDDO
        IF ( SUM_MU_X_AF <= 0.0) THEN
          IF (IDEBUG > 0) THEN
            WRITE(IOUT,*) ' SUM_MU_X_AF = ', SUM_MU_X_AF
          ENDIF
          IVALID = 0
          GOTO 49
        ENDIF
      ELSEIF (ICHECK == 2) THEN
        DO J = 1, NMUAL/2
          IF ( ( A(2*J-1) * A(2*J) ) <= 0.0 ) THEN
            IF (IDEBUG > 0) THEN
              WRITE(IOUT,*) ' A(2*J-1) * A(2*J) <= 0 '
              WRITE(IOUT,*) ' J, A(2*J-1), A(2*J) =  ',
     $                    J, A(2*J-1), A(2*J)
            ENDIF
            IVALID = 0
            GOTO 49
          ENDIF
        ENDDO
      ENDIF

49    CONTINUE

      IF (IDEBUG > 0) THEN
        WRITE(IOUT,*) 'IVALID = ', IVALID
      ENDIF

      RETURN

      END SUBROUTINE LAW69_CHECK

!>    @purpose
!>       Esstimate lwer/upper bounds of mu_i and alpha_i 
!>       which will be used to generate initial guess
!>    @param[in]  LAWID    1=Ogden, 2=Mooney-Rivlin
!>    @param[in]  ICHECK   Checking level
!>    @param[in]  X(NPT)   stretch values of given stress/strain(stretch) curve
!>    @param[in]  Y(NPT)   engineering stress values of given stress/strain curve 
!>    @param[in]  NPT      number of points on the given stress/strain curve

!>    @param[out]  MCOF_MIN(NMUAL)     estimated lower bounds of A(1:NMUAL)
!>    @param[out]  MCOF_MAX(NMUAL)     estimated upper bounds of A(1:NMUAL)
Chd|====================================================================
Chd|  LAW69_GUESS_BOUNDS            source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_NLSQF                   source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAW69_GUESS_BOUNDS(LAWID, ICHECK, X, Y, NPT, 
     $         NMUAL, MCOF_MIN, MCOF_MAX,ICOMP)
#include      "implicit_f.inc"
#include      "units_c.inc"

       INTEGER LAWID, ICHECK, NPT,ICOMP
       my_real X(NPT), Y(NPT)
       INTEGER NMUAL 
       my_real MCOF_MIN(NMUAL), MCOF_MAX(NMUAL)

       INTEGER I

       my_real AVE_SLOPE, mu_max, dx,DY

       AVE_SLOPE = ZERO
       DO I = 1, NPT
         dx = X(I) - ONE
c        avolid dx to be too small
         IF (dx >= ZERO) THEN
           dx = MAX(dx, EM6)
         ELSE
           dx = MIN(dx, -EM6)
         ENDIF
         AVE_SLOPE = AVE_SLOPE + (Y(I) - ZERO) / dx
!ok          AVE_SLOPE = MAX(AVE_SLOPE , (Y(I) - ZERO) / dx )
       ENDDO
       AVE_SLOPE = AVE_SLOPE / (ONE * NPT)
       IF (IDEBUG > 0) THEN
         WRITE(IOUT, *) 'AVE_SLOPE = ', AVE_SLOPE
       ENDIF
       mu_max = AVE_SLOPE
       IF(ICOMP == 0 ) mu_max = max(AVE_SLOPE, 20.0)
       IF (LAWID == 1) THEN
         DO I = 1, NMUAL/2
           ! mu_i
           MCOF_MIN(2*I-1) = -mu_max
           MCOF_MAX(2*I-1) = mu_max

           ! alpha_i
           MCOF_MIN(2*I)   = -10.0
           MCOF_MAX(2*I)   = +10.0
         ENDDO
       ELSEIF (LAWID == 2) THEN

         IF (ICHECK == 2) THEN
           MCOF_MIN(1) = 0.0
           MCOF_MAX(1) = mu_max
         ELSE
           MCOF_MIN(1) = -mu_max
           MCOF_MAX(1) = mu_max
         ENDIF

         MCOF_MIN(2) = +2.0
         MCOF_MAX(2) = +2.0

         IF (ICHECK == 2) THEN
           MCOF_MIN(3) = -mu_max
           MCOF_MAX(3) = 0.0
         ELSE
           MCOF_MIN(3) = -mu_max
           MCOF_MAX(3) = mu_max
         ENDIF

         MCOF_MIN(4) = -2.0
         MCOF_MAX(4) = -2.0

       ENDIF

       IF (IDEBUG > 0) THEN
         DO I = 1, NMUAL
           WRITE(IOUT,*) 'I, MCOF_MIN(I), MCOF_MAX(I) = ',
     $      I, MCOF_MIN(I), MCOF_MAX(I)
         ENDDO
       ENDIF

      END SUBROUTINE LAW69_GUESS_BOUNDS

!>    @purpose
!>      return a random value within [0.0, 1.0) with uniform distribution
!>    @param[in]  IVAL integer for calculating next random number
!>    @note
!>       - random number generator of Park and Miller.  
!>       - Schrage's modification is used to avoid 'a*(m-1)' overflows for 32-bit. 
Chd|====================================================================
Chd|  RAND1                         source/materials/tools/nlsqf.F
Chd|-- called by -----------
Chd|        LAW69_GUESS1                  source/materials/tools/nlsqf.F
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|====================================================================
      REAL FUNCTION RAND1(IVAL)
#include      "implicit_f.inc"
      INTEGER IVAL
      INTEGER IA, IM, IQ, IR
      REAL RIM
      PARAMETER (IA=69621, IM=2147483647,RIM=1./IM,IQ=30845,IR=23902)

      INTEGER IDQ
      
      IF (IVAL <= 0) THEN
        WRITE(*,*) 'ERROR, IVAL <= 0'
        WRITE(*,*) __FILE__,__LINE__
        CALL MY_EXIT(2)
      ELSEIF (IVAL > IM) THEN
        WRITE(*,*) 'ERROR, IVAL > IM'
        WRITE(*,*) __FILE__,__LINE__
        CALL MY_EXIT(2)
      ENDIF

      IDQ = IVAL / IQ
      IVAL = IA * (IVAL - IDQ * IQ) - IR * IDQ
      if (IVAL < 0) IVAL = IVAL + IM

      RAND1 = REAL(IVAL) * RIM
      
      RETURN
      END FUNCTION RAND1

