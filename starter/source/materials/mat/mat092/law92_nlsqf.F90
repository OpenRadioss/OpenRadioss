!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
! IDEBUG=1 -> output debug information into .out file
#define IDEBUG 0
! IOCSV=1 -> output fitting to curv.csv
#define IOCSV 0
!||====================================================================
!||    law92_nlsqf_mod   ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    law100_upd_ab     ../starter/source/materials/mat/mat100/law100_upd.F
!||    law92_upd         ../starter/source/materials/mat/mat092/law92_upd.F
!||====================================================================
      module law92_nlsqf_mod
        implicit none
      contains
!>    @param[in]   ERRTOL      If ERRAVE < ERRTOL, data fitting converges.
!>                                 ERRAVE = ( SUM [ ABS ( ( Y_inp-Y_fit)  )  ) / NPT
!>    @param[out]  MUAL(1:NMUAL)   optimized material properties
!||====================================================================
!||    law92_nlsqf            ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    law100_upd_ab          ../starter/source/materials/mat/mat100/law100_upd.F
!||    law92_upd              ../starter/source/materials/mat/mat092/law92_upd.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    arruda_boyce           ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    law92_guess            ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    mrqmin_law92           ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    my_exit                ../starter/source/output/analyse/analyse.c
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
        SUBROUTINE LAW92_NLSQF(STRETCH,Y,NMULA,NPT,AMULA,NSTART, ERRTOL,ID,TITR,ITEST)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod, only : WP
          USE MESSAGE_MOD
          USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include Files
! ----------------------------------------------------------------------------------------------------------------------
#include      "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: MAXA
          PARAMETER (MAXA=2)

          INTEGER :: NPT, NMULA,  NSTART,ITEST
          real(kind=WP) :: ERRTOL
          INTEGER :: I,NONZERO(MAXA),ITER,J
          real(kind=WP) :: GAMMA,ERRNOW,ERRPRE,STRETCH(NPT),AMULA(*)
          real(kind=WP) :: A(MAXA),COVAR(MAXA,MAXA),ALPHA(MAXA,MAXA),Y(NPT)
          real(kind=WP) :: YOGD

          INTEGER :: ID
          CHARACTER(LEN=NCHARTITLE) :: TITR
          INTEGER :: MINITER_LM, MAXITER_LM
          DATA MINITER_LM /10/
          SAVE MINITER_LM

          DATA MAXITER_LM /20/
          SAVE MAXITER_LM

          real(kind=WP) :: EPS_LM
          DATA EPS_LM /1E-3/
          SAVE EPS_LM

          INTEGER :: CNT_HIT_EPS_LM
          INTEGER :: LMT_HIT_EPS_LM
          DATA LMT_HIT_EPS_LM /2/
          SAVE LMT_HIT_EPS_LM

          INTEGER :: LMSTOP

          real(kind=WP) :: A0(MAXA),Y0(NPT)
          real(kind=WP) :: ERRAVE, ERRAVE_MIN, ERR

          INTEGER :: ISTART, NPSAVED, IVALID

          INTEGER :: ICURV
          LOGICAL :: lopened

!     during LM optimization, if the objective is not improved,
!     GAMMA will be increased. We should terminate the iteration once
!     GAMMA becomes very huge.
          real(kind=WP) :: GAMMA_STOP
          data GAMMA_STOP /1E10/
          save GAMMA_STOP

!     if check the validity of initial guess (0=no, 1=yes)
!     we don't want to check, because invalid initial point could converge to valid point.
          INTEGER :: ICHECK_GUESS
          data ICHECK_GUESS /0/
          save ICHECK_GUESS

          INTEGER :: IFIT_SUCCESS

!     if enforce mu(i) < mu(i+1) in generating initial guess
!     we don't need this anymore since we are using random numbers instead of loop through all
!     the combinations
          INTEGER :: MU_INCR_GUESS
          data MU_INCR_GUESS /0/
          save MU_INCR_GUESS

          real(kind=WP) :: MAX_ABS_YI,MIN_ABS_YI
          real(kind=WP) :: SMALL_FAC_ABS_YI, SMALL_ABS_YI

          real(kind=WP), DIMENSION(:), ALLOCATABLE :: SIG
          real(kind=WP) :: SPREADY
          INTEGER :: IRET

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ERRAVE_MIN = huge(ERRAVE_MIN)
          ALLOCATE (SIG(1:NPT))

          ! IF ABS(Y(I)) <  SMALL_ABS_YI, use SMALL_ABS_YI to avoid
          ! unnecessary numerical issues when avoid divided by small value

          MAX_ABS_YI = ZERO
          MIN_ABS_YI = EP20
          DO I = 1, NPT
            Y0(I) = Y(I)
            MAX_ABS_YI = max( MAX_ABS_YI, ABS(Y(I)) )
            IF(Y(I) /= ZERO) MIN_ABS_YI = MIN( MIN_ABS_YI, ABS(Y(I)) )
          ENDDO

          SMALL_FAC_ABS_YI = EM3

          SMALL_ABS_YI = MAX_ABS_YI * SMALL_FAC_ABS_YI

          IF (IDEBUG > 0) THEN
            WRITE(IOUT, *) ' MAX_ABS_YI, SMALL_FAC_ABS_YI, SMALL_ABS_YI = ',&
              MAX_ABS_YI, SMALL_FAC_ABS_YI, SMALL_ABS_YI
          ENDIF

          SPREADY=EM02
          DO J=1,NPT
            SIG(J)=MAX(SMALL_ABS_YI, ABS(Y(J)) )
            IF(Y(J) == ZERO) Y(J) = SMALL_ABS_YI
            IF (IDEBUG > 0) THEN
              WRITE(IOUT, *) 'J, SIG(J) = ', J,  SIG(J)
            ENDIF
          ENDDO

          IF (MAXA < NMULA) THEN
            CALL MY_EXIT(2)
          ENDIF

!=======================================================================

          A0(1) = AMULA(1)
          A0(2) = AMULA(2)

          DO I=1,NMULA
            NONZERO(I)=1
          ENDDO

          IFIT_SUCCESS = 0

          ISTART = 0
          NPSAVED = 0

          NSTART = 5
          DO 111 WHILE ( ISTART < NSTART )
!       get one guess point A0(1:MA)
            CALL LAW92_GUESS(STRETCH,Y,A0,NPT)

!       calculate averaged ERROR before LM
            ERRAVE = ZERO
            DO I=1,NPT
              CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST )
!absolute error           ERR = ABS(Y(I) - YOGD)
              ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
              ERRAVE = ERRAVE + ERR
            ENDDO
            ERRAVE = ERRAVE / (1.0 * NPT)

            ISTART = ISTART + 1

            IF (IDEBUG > 0) THEN
              WRITE(IOUT,*) ' ISTART = ', ISTART
              WRITE(IOUT,*) ' Before LM optimization ...'
              DO I = 1, NMULA
                WRITE(IOUT, *) ' I, A0(I) = ', I, A0(I)
              ENDDO
              WRITE(IOUT,*) ' LM0, ERRAVE = ',  ERRAVE
            ENDIF

!       start loop of LM optimization
            ITER = 0
            CNT_HIT_EPS_LM = 0

            ITER = ITER + 1
            GAMMA=-1.

            CALL MRQMIN_LAW92(STRETCH,Y,SIG,NPT,A0, &
              COVAR,ALPHA,NMULA,ERRNOW, &
              GAMMA,IRET,ITEST)

            IF (IRET > 0) GOTO 111
            IF (IDEBUG > 0) THEN
              ERRAVE = ZERO
              DO I=1,NPT
                CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST)
                ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
!absolute error           ERR = ABS(Y(I) - YOGD)
                ERRAVE = ERRAVE + ERR
              ENDDO
              ERRAVE = ERRAVE / (1.0 * NPT)
              WRITE(IOUT, '(A,I4, 3E16.8)') &
                'ITER, ERRNOW, GAMMA, ERRAVE = ', &
                ITER, ERRNOW, GAMMA, ERRAVE
              DO I = 1, NMULA
                WRITE(IOUT, *) '   - I, A0(I) = ', I, A0(I)
              ENDDO
            ENDIF

21          CONTINUE
            ERRPRE=ERRNOW
            ITER = ITER + 1
            CALL MRQMIN_LAW92(STRETCH,Y,SIG,NPT,A0, &
              COVAR,ALPHA, NMULA, ERRNOW, &
              GAMMA,IRET,ITEST)
            IF (IRET > 0) GOTO 111

            IF (IDEBUG == 1) THEN
              ERRAVE = ZERO
              DO I=1,NPT
                CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST)
                ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
!absolute error           ERR = ABS(Y(I) - YOGD)
                ERRAVE = ERRAVE + ERR
              ENDDO
              ERRAVE = ERRAVE / (1.0 * NPT)

              WRITE(IOUT, '(A,I4, 5E16.8)') &
                'ITER, ERRNOW, GAMMA, ERRAVE, ERRNOW-ERRPRE,'// &
                '(ERRNOW-ERRPRE)/ERRPRE = ', &
                ITER, ERRNOW, GAMMA, ERRAVE, ERRNOW-ERRPRE, &
                (ERRNOW-ERRPRE)/ERRPRE
              DO I = 1, NMULA
                WRITE(IOUT, *) '   - I, A0(I) = ', I, A0(I)
              ENDDO
            ENDIF

!       IF A0(J) is too small, restart from next initial guess
            DO J = 1, NMULA
              IF ( ABS( A0(J) ) < EM20 ) THEN
                goto 111  ! restart from next initial guess
              ENDIF
            ENDDO

!       check convergence of LM optimization
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
                      WRITE(IOUT,*) &
                        ' CNT_HIT_EPS_LM, ABS((ERRNOW-ERRPRE)/ERRPRE)  = ', &
                        CNT_HIT_EPS_LM, ABS( (ERRNOW-ERRPRE)/ ERRPRE )
                    ENDIF

                    IF ( CNT_HIT_EPS_LM >= LMT_HIT_EPS_LM ) THEN
                      IF (IDEBUG > 0) THEN
                        !WRITE(IOUT,*) 'STOP AT ', __LINE__
                      ENDIF
                      LMSTOP = 1
                    ENDIF
                  ENDIF
                ENDIF
              ELSEIF (ITER >= MAXITER_LM .OR. GAMMA >= GAMMA_STOP) THEN
                IF (IDEBUG > 0) THEN
                  !WRITE(IOUT,*) 'STOP AT ', __LINE__
                ENDIF
                LMSTOP = 1
              ENDIF
            ENDIF

            IF (LMSTOP== 0) THEN
              GOTO 21
            ENDIF
!       end loop of LM optimization

            ERRAVE = ZERO
            DO I=1,NPT
              CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST)
              ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
!absolute error           ERR = ABS(Y(I) - YOGD)
              ERRAVE = ERRAVE + ERR
            ENDDO
            ERRAVE = ERRAVE / (1.0 * NPT)

            IF (IDEBUG > 0) THEN
              WRITE(IOUT,*) ' After LM optimization ...'
              DO I = 1, NMULA
                WRITE(IOUT, *) ' I, A0(I) = ', I, A0(I)
              ENDDO
              WRITE(IOUT,*) ' LM1, ERRAVE = ', ERRAVE
            ENDIF

            IF(A0(1) > ZERO .AND. A0(2) > ZERO) IVALID = 1
            IF (IVALID > 0) THEN
              IF (   NPSAVED==0 .OR.( NPSAVED>0 .AND. ERRAVE<ERRAVE_MIN) ) THEN
                NPSAVED = NPSAVED + 1
                ERRAVE_MIN = ERRAVE
                DO I = 1, NMULA
                  A(I) = A0(I)
                ENDDO
              ENDIF
            ENDIF

            IF (NPSAVED > 0) THEN
              IF (IDEBUG > 0) THEN
                WRITE(*, *) ' ISTART, NPSAVED, ERRAVE_MIN = ', ISTART, NPSAVED, ERRAVE_MIN
                WRITE(IOUT, *) ' ISTART, NPSAVED, ERRAVE_MIN = ', ISTART, NPSAVED, ERRAVE_MIN
              ENDIF

              IF ( ERRAVE_MIN < ERRTOL ) THEN
                IFIT_SUCCESS = 1
                GOTO 112
              ENDIF
            ELSE
              IF (IDEBUG > 0) THEN
                WRITE(*, *) ' ISTART, NPSAVED ', ISTART, NPSAVED
                WRITE(IOUT, *) ' ISTART, NPSAVED ', ISTART, NPSAVED
              ENDIF
            ENDIF
111       CONTINUE  ! WHILE ( ISTART < NSTART )

112       CONTINUE

          DEALLOCATE (SIG)

          IF (IFIT_SUCCESS == 0) THEN
            IF (NPSAVED == 0) THEN
              CALL ANCMSG(MSGID=901,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR)
            ENDIF
          ENDIF

          DO I=1,NMULA
            AMULA(I)=A(I)
          ENDDO

          WRITE(IOUT,'(//6X,A,/)')'FITTING RESULT COMPARISON:'
          IF(ITEST == 1)THEN
            WRITE(IOUT,'(6X,A,/)')'UNIAXIAL TEST DATA'
          ELSEIF(ITEST == 2) THEN
            WRITE(IOUT,'(6X,A,/)')'EQUIBIAXIAL TEST DATA'
          ELSEIF(ITEST == 3) THEN
            WRITE(IOUT,'(6X,A,/)')'PLANAR TEST DATA'
          ENDIF
          WRITE(IOUT,'(A20,5X,A20,A30)') 'NOMINAL STRAIN','NOMINAL STRESS(TEST)', 'NOMINAL STRESS(RADIOSS)'

!     output curcves to .csv format to simplify visualization
          ICURV = 0
          IF (IOCSV> 0) THEN
            DO ICURV = 25, 99
              inquire (unit=ICURV, opened=lopened)
              if (.not. lopened) goto 77
            ENDDO
77          CONTINUE
            OPEN(UNIT=ICURV, FILE='curv.csv')
            WRITE(ICURV,'(A)') 'NOMINAL STRAIN, NOMINAL STRESS(TEST), '//'NOMINAL STRESS(RADIOSS)'
          ENDIF

          DO I=1,NPT
            CALL ARRUDA_BOYCE(STRETCH(I), A, YOGD,ITEST)
            WRITE(IOUT,'(F18.4,F20.4,F28.4)') STRETCH(I)-ONE,Y0(I),YOGD
            IF (ICURV > 0) THEN
              WRITE(ICURV, '(F18.4, A, F18.4, A, F18.4)') STRETCH(I)-ONE,',',Y0(I),',',YOGD
            ENDIF
          ENDDO

          WRITE(IOUT, *) ' '
          WRITE(IOUT, '(A)') '-------------------------------------------'
          WRITE(IOUT, '(A,F10.2,A)') 'AVERAGED ERROR OF FITTING : ', ERRAVE_MIN*100.0, '%'

!     output curcves to .csv format to simplify visualization
          IF ( ICURV > 0) THEN
            CLOSE(ICURV)
          ENDIF
!-----------
          RETURN
        END

! Compute normal stress sig=dw/dlam_1
!||====================================================================
!||    arruda_boyce    ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    law92_nlsqf     ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    mrqcof_law92    ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        SUBROUTINE ARRUDA_BOYCE(STRETCH,A, SIG,ITEST )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: ITEST
          real(kind=WP) :: STRETCH, A(*),SIG
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: J
          real(kind=WP) :: C(5),MU,LAM,EV2,TRACE,FAC,AA,BB,CC,EV
! ----------------------------------------------------------------------------------------------------------------------
          MU   = A(1)
          LAM  = A(2)
          C(1) = HALF
          C(2) = ONE/TWENTY
          C(3) = ELEVEN/1050.D00
          C(4) = 19.D00/7000.D00
          C(5) = 519.D00/673750.D00
          SIG =ZERO

          IF(ITEST == 1) THEN
            EV2 = ONE/STRETCH
            TRACE = STRETCH **2 + TWO*EV2
            FAC = TWO*MU*(STRETCH - EV2**2)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              SIG = SIG + CC
            ENDDO
            SIG= SIG*FAC
          ELSEIF(ITEST == 2) THEN
            EV =  ONE/STRETCH/STRETCH
            TRACE = TWO*STRETCH**2 + EV**2
            FAC = FOUR*MU*(STRETCH - EV**2/STRETCH)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              SIG = SIG + CC
            ENDDO
            SIG= SIG*FAC
          ELSEIF(ITEST == 3) THEN
            EV = ONE/STRETCH
            TRACE = STRETCH **2 + ONE +  EV**2
            FAC = TWO*MU*(STRETCH - EV**2/STRETCH)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              SIG = SIG + CC
            ENDDO
            SIG= SIG*FAC
          ENDIF

          RETURN
        END SUBROUTINE ARRUDA_BOYCE
! Compute normal stress DMu=dN/dmu, dlam = dN/dlam
!||====================================================================
!||    arruda_boyce_dyda   ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    mrqcof_law92        ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        SUBROUTINE ARRUDA_BOYCE_DYDA(STRETCH,A,DYDA, ITEST)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: ITEST
          real(kind=WP) :: STRETCH, A(*),DYDA(*)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: J
          real(kind=WP) :: C(5),MU,LAM,EV2,TRACE,FAC,AA,BB,CC,DD,EV
! ----------------------------------------------------------------------------------------------------------------------
          MU = A(1)
          LAM = A(2)
          C(1) = HALF
          C(2) = ONE/TWENTY
          C(3) = ELEVEN/1050.D00
          C(4) = 19.D00/7000.D00
          C(5) = 519.D00/673750.D00

          DYDA(1) = ZERO
          DYDA(2) = ZERO
          IF(ITEST == 1) THEN
            EV2 = ONE/STRETCH
            TRACE = STRETCH**2 + TWO*EV2
            FAC = TWO*(STRETCH - EV2**2)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              DD = (2-2*J)*CC/LAM
              DYDA(1) = DYDA(1) +  CC
              DYDA(2) = DYDA(2) +  DD
            ENDDO
            DYDA(1) = DYDA(1)*FAC
            DYDA(2) = DYDA(2)*FAC*MU
          ELSEIF(ITEST == 2) THEN
            EV =  ONE/STRETCH/STRETCH
            TRACE = TWO*STRETCH**2 + EV**2
            FAC = FOUR*(STRETCH - EV**2/STRETCH)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              DD = (2-2*J)*CC/LAM
              DYDA(1) = DYDA(1) +  CC
              DYDA(2) = DYDA(2) +  DD
            ENDDO
            DYDA(1) = DYDA(1)*FAC
            DYDA(2) = DYDA(2)*FAC*MU
          ELSEIF(ITEST == 3) THEn
            EV = ONE/STRETCH
            TRACE = STRETCH **2 + ONE +  EV**2
            FAC = TWO*(STRETCH - EV**2/STRETCH)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              DD = (2-2*J)*CC/LAM
              DYDA(1) = DYDA(1) +  CC
              DYDA(2) = DYDA(2) +  DD
            ENDDO
            DYDA(1) = DYDA(1)*FAC
            DYDA(2) = DYDA(2)*FAC*MU
          ENDIF

          RETURN
        END SUBROUTINE ARRUDA_BOYCE_DYDA
!||====================================================================
!||    mrqmin_law92    ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    law92_nlsqf     ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- calls      -----------------------------------------------------
!||    inversion       ../starter/source/materials/tools/nlsqf.F
!||    mrqcof_law92    ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        SUBROUTINE MRQMIN_LAW92(X,Y,SIG,NDATA,A,COVAR,ALPHA,NCA, ERRNOW, GAMMA,IRET,ITEST)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include Files
! ----------------------------------------------------------------------------------------------------------------------
#include      "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: NCA,NDATA,MMAX,ITEST
          real(kind=WP) :: GAMMA,ERRNOW,A(*),ALPHA(NCA,NCA),COVAR(NCA,NCA),X(NDATA),Y(NDATA),SIG(NDATA)
          PARAMETER (MMAX=2)
          INTEGER :: J,K,L,MFIT
          real(kind=WP) :: ERRPRE,ATRY(MMAX),BETA(MMAX),DA(MMAX)
          SAVE ERRPRE,ATRY,BETA,DA,MFIT
          INTEGER :: IRET,IFLAG
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          IRET = 0
          IFLAG = 0
          MFIT = NCA

          IF (GAMMA < ZERO) THEN
            MFIT = NCA
!       in Numerical Recript, 0.001 is used, however from my tests, the
!       difference is small (0.01 vs 0.001)
            GAMMA=EM02
            CALL MRQCOF_LAW92(X,Y,SIG,NDATA,A,ALPHA,BETA,NCA,ERRNOW,ITEST)

            ERRPRE=ERRNOW
            DO J=1,NCA
              ATRY(J)=A(J)
            ENDDO
          ENDIF

          DO J=1,NCA
            DO K=1,NCA
              COVAR(J,K)=ALPHA(J,K)
            ENDDO
!ok too           COVAR(J,J)=ALPHA(J,J)*(ONE + GAMMA)
            COVAR(J,J)=ALPHA(J,J) + GAMMA
            DA(J)=BETA(J)
          ENDDO

          CALL INVERSION(COVAR,MFIT,NCA,DA,1,1,IRET)

          IF (IRET /= 0) THEN
            IF (IDEBUG > 0) THEN
              WRITE(*,*) ' IRET = ', IRET
              DO J=1,NCA
                WRITE(IOUT, *) 'J, A(J) = ', J, A(J)
              ENDDO
            ENDIF
            RETURN
          ENDIF

          J=0
          DO L=1,NCA
            ATRY(L)=A(L) + DA(L)
          ENDDO
          IF(ATRY(1) <= ZERO .OR. ATRY(2) <=0) THEN
            ATRY(1)= A(1)
            ATRY(2)= A(2)
          ENDIF

          IF (IDEBUG > 0) THEN
            DO J=1, NCA
              write(IOUT,*) 'J,ATRY(J) = ', J, ATRY(J)
            ENDDO
          ENDIF

          CALL MRQCOF_LAW92(X,Y,SIG,NDATA,ATRY,COVAR,DA,NCA,ERRNOW,ITEST)

          IF (ERRNOW < ERRPRE) THEN
            GAMMA=GAMMA/TEN
            ERRPRE=ERRNOW
            DO J=1,MFIT
              DO K=1,MFIT
                ALPHA(J,K)=COVAR(J,K)
              ENDDO
              BETA(J)=DA(J)
            ENDDO
            DO L=1,NCA
              A(L)=ATRY(L)
            ENDDO
          ELSE
            GAMMA=10*GAMMA
            GAMMA = MIN(GAMMA, EP10)
            ERRNOW=ERRPRE
          ENDIF
!-----------
          RETURN
        END SUBROUTINE MRQMIN_LAW92
!-----------
!||====================================================================
!||    mrqcof_law92        ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    mrqmin_law92        ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- calls      -----------------------------------------------------
!||    arruda_boyce        ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||    arruda_boyce_dyda   ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        SUBROUTINE MRQCOF_LAW92(X,Y,SIG, NDATA,A,ALPHA,BETA,NALP,ERRNOW,ITEST)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: NALP,NDATA,ITEST
          real(kind=WP) :: ERRNOW,A(*),ALPHA(NALP,NALP),BETA(*),X(NDATA),Y(NDATA),SIG(NDATA)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: MFIT,I,J,K,L,M,MMAX
          PARAMETER (MMAX=20)
          real(kind=WP)  :: DY,WT,YMOD,DYDA(MMAX)
          real(kind=WP) :: Y_MIN
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          Y_MIN = EM3
          MFIT=NALP
          DO 13 J=1,MFIT
            DO 12 K=1,J
              ALPHA(J,K)=ZERO
12          CONTINUE
            BETA(J)=ZERO
13        CONTINUE
          ERRNOW=ZERO
          DO 16 I=1,NDATA
            CALL ARRUDA_BOYCE(X(I),A,YMOD,ITEST)
            CALL ARRUDA_BOYCE_DYDA(X(I),A,DYDA,ITEST)
            DY=Y(I)-YMOD
            ERRNOW=ERRNOW+DY*DY/(SIG(I)*SIG(I))

            J=0
            DO 15 L=1,MFIT
              J=J+1
              WT=DYDA(L)/(SIG(I)*SIG(I))
              K=0
              DO 14 M=1,L
                K=K+1
                ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(M)
14            CONTINUE
              BETA(J)=BETA(J) + DY*WT
15          CONTINUE
16        CONTINUE
          DO 18 J=2,MFIT
            DO 17 K=1,J-1
              ALPHA(K,J)=ALPHA(J,K)
17          CONTINUE

18        CONTINUE
          RETURN
        END SUBROUTINE MRQCOF_LAW92
! Compute normal stress DMu=dN/dmu, dlam = dN/dlam
!||====================================================================
!||    law92_guess     ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- called by ------------------------------------------------------
!||    law92_nlsqf     ../starter/source/materials/mat/mat092/law92_nlsqf.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        SUBROUTINE LAW92_GUESS(STRETCH,SIG,A,NDATA )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: NDATA
          real(kind=WP) :: STRETCH(*), A(*),SIG(*)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: K,J
          real(kind=WP) :: C(5),MU,LAM,EV2,TRACE,FAC,AA,BB,CC,DYDA(NDATA)
!=======================================================================
          MU = A(1)
          LAM = A(2)
          C(1) = HALF
          C(2) = ONE/TWENTY
          C(3) = ELEVEN/1050.D00
          C(4) = 19.D00/7050.D00
          C(5) = 519.D00/673750.D00
          DO K=1,NDATA
            DYDA(K) = ZERO
            EV2 = ONE/STRETCH(K)
            TRACE = STRETCH(K)**2 + TWO*EV2
!cauchy stress       FAC = TWO*STRETCH(K)*(STRETCH(K) - EV2**2)
            FAC = TWO*(STRETCH(K) - EV2**2)
            DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              DYDA(K) = DYDA(K) +  CC
            ENDDO
            DYDA(K) = DYDA(K)*FAC
          ENDDO

          AA = ZERO
          BB = ZERO
          DO K=1,NDATA
            CC = DYDA(K)/SIG(K)
            BB = BB + CC
            AA = AA +  CC**2
          ENDDO
          A(1) = BB/AA

          RETURN
        END SUBROUTINE LAW92_GUESS
      end module
