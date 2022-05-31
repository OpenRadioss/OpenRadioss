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
C IDEBUG=1 -> output debug information into .out file
#define IDEBUG 0
C IOCSV=1 -> output fitting to curv.csv
#define IOCSV 0
!>    @param[in]   ERRTOL      If ERRAVE < ERRTOL, data fitting converges.
!>                                 ERRAVE = ( SUM [ ABS ( ( Y_inp-Y_fit)  )  ) / NPT
!>    @param[out]  MUAL(1:NMUAL)   optimized material properties 
Chd|====================================================================
Chd|  LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|-- called by -----------
Chd|        LAW100_UPD_AB                 source/materials/mat/mat100/law100_upd.F
Chd|        LAW92_UPD                     source/materials/mat/mat092/law92_upd.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRUDA_BOYCE                  source/materials/mat/mat092/law92_nlsqf.F
Chd|        LAW92_GUESS                   source/materials/mat/mat092/law92_nlsqf.F
Chd|        MRQMIN_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAW92_NLSQF(STRETCH,Y,NMULA,NPT,AMULA,
     $                       NSTART, ERRTOL,ID,TITR,ITEST)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com10_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "scr10_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
      INTEGER MAXA 
      PARAMETER (MAXA=2)

      INTEGER LAWID, NPT, NMULA,  NSTART,ITEST
      my_real ERRTOL
      INTEGER I,NONZERO(MAXA),IDUM,ITER,ICOUNT,J,K  
      my_real GAMMA,ERRNOW,GASDEV,ERRPRE,
     .        STRETCH(NPT),AMULA(*)
      my_real A(MAXA),COVAR(MAXA,MAXA),ALPHA(MAXA,MAXA),Y(NPT)
      my_real MCOF_MIN(MAXA), MCOF_MAX(MAXA)
      my_real YOGD,XOGD

      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER MINITER_LM, MAXITER_LM
      DATA MINITER_LM /10/
      SAVE MINITER_LM

      DATA MAXITER_LM /20/
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
      my_real A0(MAXA),Y0(NPT)
      my_real ERRMIN, ERRMAX, ERRAVE, ERRAVE_MIN, ERR,ERR2

      INTEGER ISTART, NPSAVED, IVALID

      INTEGER ICURV
      LOGICAL lopened

c     during LM optimization, if the objective is not improved, 
c     GAMMA will be increased. We should terminate the iteration once
c     GAMMA becomes very huge.
      my_real GAMMA_STOP
      data GAMMA_STOP /1E10/
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

      my_real MAX_ABS_YI,MIN_ABS_YI
      my_real SMALL_FAC_ABS_YI, SMALL_ABS_YI

      my_real, DIMENSION(:), ALLOCATABLE :: SIG
      my_real  SPREADY
      INTEGER IRET
      INTEGER IRND1
C
      ALLOCATE (SIG(1:NPT))

      IF ( MAXA .LT. NMULA ) THEN
        WRITE(*,*) 'ERROR, MAXA < MA'
        WRITE(*,*) __FILE__,__LINE__
        CALL MY_EXIT(2)
      ENDIF
      ! IF ABS(Y(I)) <  SMALL_ABS_YI, use SMALL_ABS_YI to avoid
      ! unnecessary numerical issues when avoid devided by small value

      MAX_ABS_YI = ZERO
      MIN_ABS_YI = EP20
      DO I = 1, NPT
        Y0(I) = Y(I)
        MAX_ABS_YI = max( MAX_ABS_YI, ABS(Y(I)) )
        IF(Y(I) /= ZERO) MIN_ABS_YI = MIN( MIN_ABS_YI, ABS(Y(I)) )
      ENDDO

      SMALL_FAC_ABS_YI = EM3 

      SMALL_ABS_YI = MAX_ABS_YI * SMALL_FAC_ABS_YI

      IF (IDEBUG .GT. 0) THEN
        WRITE(IOUT, *) ' MAX_ABS_YI, SMALL_FAC_ABS_YI, SMALL_ABS_YI = ',
     $                   MAX_ABS_YI, SMALL_FAC_ABS_YI, SMALL_ABS_YI
      ENDIF

      SPREADY=0.01
      DO J=1,NPT
        SIG(J)=MAX(SMALL_ABS_YI, ABS(Y(J)) )
        IF(Y(J) == ZERO) Y(J) = SMALL_ABS_YI
        IF (IDEBUG .GT. 0) THEN
          WRITE(IOUT, *) 'J, SIG(J) = ', J,  SIG(J)
        ENDIF
      ENDDO  
C     
      IF (MAXA .LT. NMULA) THEN
!       TBD_KANG      
        WRITE(*,*) 'ERROR: MAXA < MA'
        WRITE(*,*) ' MAXA = ', MAXA
        WRITE(*,*) ' MA = ', MAXA
        WRITE(*,*) ' FILE = ', __FILE__
        WRITE(*,*) ' LINE = ', __LINE__
        CALL MY_EXIT(2)
      ENDIF

C=======================================================================        
C     
      A0(1) = AMULA(1)
      A0(2) = AMULA(2)
C            
      DO I=1,NMULA 
        NONZERO(I)=1  
      ENDDO

99    CONTINUE      !

      IFIT_SUCCESS = 0 

      ISTART = 0
      NPSAVED = 0
C
      NSTART = 5
      DO 111 WHILE ( ISTART .LT. NSTART )
c       get one guess point A0(1:MA)        
         CALL LAW92_GUESS(STRETCH,Y,A0,NPT)  
!        IF (NGUESS .EQ. 0) THEN
!          GOTO 112
!        ENDIF

C       calculate averaged ERROR before LM         
        ERRAVE = 0.0
        DO I=1,NPT
          CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST )
!absolute error           ERR = ABS(Y(I) - YOGD)
           ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
           ERRAVE = ERRAVE + ERR
        ENDDO 
        ERRAVE = ERRAVE / (1.0 * NPT)
C        
        ISTART = ISTART + 1

        IF (IDEBUG .GT. 0) THEN
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
C        
        CALL MRQMIN_LAW92(STRETCH,Y,SIG,NPT,A0,
     $       COVAR,ALPHA,NMULA,ERRNOW, 
     $       GAMMA,IRET,ITEST) 
     
        IF (IRET > 0) GOTO 111
        IF (IDEBUG .GT. 0) THEN
          ERRAVE = 0.0
          DO I=1,NPT
            CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST)
             ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
!absolute error           ERR = ABS(Y(I) - YOGD)
            ERRAVE = ERRAVE + ERR
          ENDDO    
          ERRAVE = ERRAVE / (1.0 * NPT)
          WRITE(IOUT, '(A,I4, 3E16.8)') 
     $            'ITER, ERRNOW, GAMMA, ERRAVE = ', 
     $            ITER, ERRNOW, GAMMA, ERRAVE
          DO I = 1, NMULA
            WRITE(IOUT, *) '   - I, A0(I) = ', I, A0(I)
          ENDDO
        ENDIF

21      CONTINUE        
        ERRPRE=ERRNOW  
        ITER = ITER + 1
        CALL MRQMIN_LAW92(STRETCH,Y,SIG,NPT,A0,
     $       COVAR,ALPHA, NMULA, ERRNOW, 
     $       GAMMA,IRET,ITEST) 
        IF (IRET > 0) GOTO 111

       IF (IDEBUG .EQ. 1) THEN
          ERRAVE = ZERO
          DO I=1,NPT
            CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST)
            ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
!absolute error           ERR = ABS(Y(I) - YOGD)            
            ERRAVE = ERRAVE + ERR
          ENDDO      
          ERRAVE = ERRAVE / (1.0 * NPT)

          WRITE(IOUT, '(A,I4, 5E16.8)') 
     $     'ITER, ERRNOW, GAMMA, ERRAVE, ERRNOW-ERRPRE,'//
     $     '(ERRNOW-ERRPRE)/ERRPRE = ', 
     $      ITER, ERRNOW, GAMMA, ERRAVE, ERRNOW-ERRPRE, 
     $      (ERRNOW-ERRPRE)/ERRPRE
          DO I = 1, NMULA
            WRITE(IOUT, *) '   - I, A0(I) = ', I, A0(I)
          ENDDO
        ENDIF

c       IF A0(J) is too small, restart from next initial guess        
        DO J = 1, NMULA
          IF ( ABS( A0(J) ) .LT. EM20 ) THEN
             goto 111  ! restart from next initial guess
c            WRITE(IOUT, *) ' A is too small, and could result in error'
c            WRITE(IOUT,*) 'J, A0(J) = ', J, A0(J)
c            STOP
          ENDIF
        ENDDO

c       check convergence of LM optimization        
        LMSTOP = 0
        IF (IDEBUG .GT. 0) THEN
          WRITE(IOUT, *) ' ERRNOW/(1.0*NPT) = ', ERRNOW/(1.0*NPT)
          WRITE(IOUT, *) ' ERRNOW .LT. ERRPRE = ', (ERRNOW .LT. ERRPRE)
        ENDIF

        IF ( ITER .GT. MINITER_LM ) THEN
          IF (ERRNOW .LT. ERRPRE) THEN
            IF ( ABS(ERRPRE) .GT. ZERO) THEN
              IF  ( ABS( (ERRNOW-ERRPRE)/ ERRPRE ) .LT. EPS_LM) THEN
                CNT_HIT_EPS_LM = CNT_HIT_EPS_LM + 1

                IF (IDEBUG .GT. 0) THEN
                  WRITE(IOUT,*) 
     $            ' CNT_HIT_EPS_LM, ABS((ERRNOW-ERRPRE)/ERRPRE)  = ', 
     $            CNT_HIT_EPS_LM, ABS( (ERRNOW-ERRPRE)/ ERRPRE )
                ENDIF

                IF ( CNT_HIT_EPS_LM .GE. LMT_HIT_EPS_LM ) THEN
                  IF (IDEBUG .GT. 0) THEN
                    WRITE(IOUT,*) 'STOP AT ', __LINE__
                  ENDIF
                  LMSTOP = 1
                ENDIF
              ENDIF
            ENDIF
          ELSEIF (ITER .GE. MAXITER_LM .OR. GAMMA .GE. GAMMA_STOP) THEN
            IF (IDEBUG .GT. 0) THEN
              WRITE(IOUT,*) 'STOP AT ', __LINE__
            ENDIF
            LMSTOP = 1
          ENDIF  
        ENDIF

        IF (LMSTOP.EQ. 0) THEN
          GOTO 21
        ENDIF
!       end loop of LM optimization        

        ERRAVE = 0.0
        DO I=1,NPT
          CALL ARRUDA_BOYCE(STRETCH(I), A0, YOGD,ITEST)
           ERR = ABS(Y(I) - YOGD) / ABS(SIG(I))
!absolute error           ERR = ABS(Y(I) - YOGD)           
          ERRAVE = ERRAVE + ERR 
        ENDDO      
        ERRAVE = ERRAVE / (1.0 * NPT)

        IF (IDEBUG .GT. 0) THEN
           WRITE(IOUT,*) ' After LM optimization ...'
           DO I = 1, NMULA
             WRITE(IOUT, *) ' I, A0(I) = ', I, A0(I)
           ENDDO
           WRITE(IOUT,*) ' LM1, ERRAVE = ', ERRAVE
        ENDIF
C
        IF(A0(1) > ZERO .AND. A0(2) > ZERO) IVALID = 1
        IF (IVALID .GT. 0) THEN
          IF (   NPSAVED.EQ.0 .OR. 
     $         ( NPSAVED.GT.0 .AND. ERRAVE.LT.ERRAVE_MIN) ) THEN
            NPSAVED = NPSAVED + 1
            ERRAVE_MIN = ERRAVE
            DO I = 1, NMULA
              A(I) = A0(I)
            ENDDO
          ENDIF
        ELSE
          IF (IDEBUG .GT. 0) THEN
            WRITE(IOUT,*) __FILE__,__LINE__
            WRITE(IOUT,*) ' LM converges to invalid point'
          ENDIF
        ENDIF

        IF (NPSAVED .GT. 0) THEN
          IF (IDEBUG .GT. 0) THEN
            WRITE(*, *) ' ISTART, NPSAVED, ERRAVE_MIN = ', 
     $                    ISTART, NPSAVED, ERRAVE_MIN
            WRITE(IOUT, *) ' ISTART, NPSAVED, ERRAVE_MIN = ', 
     $                    ISTART, NPSAVED, ERRAVE_MIN
          ENDIF

          IF ( ERRAVE_MIN .LT. ERRTOL ) THEN
            IFIT_SUCCESS = 1
            GOTO 112
          ENDIF
        ELSE
          IF (IDEBUG .GT. 0) THEN
            WRITE(*, *) ' ISTART, NPSAVED ', 
     $                    ISTART, NPSAVED
            WRITE(IOUT, *) ' ISTART, NPSAVED ', 
     $                    ISTART, NPSAVED
          ENDIF
        ENDIF
111   CONTINUE  ! WHILE ( ISTART .LT. NSTART )

112   CONTINUE

      DEALLOCATE (SIG)

      IF (IFIT_SUCCESS .EQ. 0) THEN
        IF (NPSAVED .EQ. 0) THEN
          CALL ANCMSG(MSGID=901,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
      ENDIF
C
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
      WRITE(IOUT,'(A20,5X,A20,A30)') 'NOMINAL STRAIN',
     *      'NOMINAL STRESS(TEST)', 'NOMINAL STRESS(RADIOSS)'
    
c     output curcves to .csv format to simplify visualization    
      ICURV = 0
      IF (IOCSV.GT. 0) THEN
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
        CALL ARRUDA_BOYCE(STRETCH(I), A, YOGD,ITEST)
        WRITE(IOUT,'(F18.4,F20.4,F28.4)') 
     *   STRETCH(I)-ONE,Y0(I),YOGD
        IF (ICURV .GT. 0) THEN
           WRITE(ICURV, '(F18.4, A, F18.4, A, F18.4)') 
     $            STRETCH(I)-ONE,',',Y0(I),',',YOGD
        ENDIF
      ENDDO      

      WRITE(IOUT, *) ''
      WRITE(IOUT, '(A)') '-------------------------------------------'
      WRITE(IOUT, '(A,F10.2,A)') 'AVERAGED ERROR OF FITTING : ', 
     $                            ERRAVE_MIN*100.0, '%'

c     output curcves to .csv format to simplify visualization    
      IF ( ICURV .GT. 0) THEN
        CLOSE(ICURV)
      ENDIF
C-----------
      RETURN
      END 

C Compute normal stress sig=dw/dlam_1 
Chd|====================================================================
Chd|  ARRUDA_BOYCE                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- called by -----------
Chd|        LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|        MRQCOF_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ARRUDA_BOYCE(STRETCH,A, SIG,ITEST ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDATA,ITEST
      my_real 
     .     STRETCH, A(*),SIG 
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER K,J
      my_real 
     .  C(5),MU,LAM,EV2,TRACE,FAC,AA,BB,CC,A1,A2,A3,A4,FLAM,EV
C======================================================================= 
       MU   = A(1)
       LAM  = A(2)
       C(1) = HALF
       C(2) = ONE/TWENTY
       C(3) = ELEVEN/1050.D00
       C(4) = 19.D00/7000.D00
       C(5) = 519.D00/673750.D00
       SIG =ZERO
C       
       IF(ITEST == 1) THEN
          EV2 = ONE/STRETCH
          TRACE = STRETCH **2 + TWO*EV2
          FAC = TWO*MU*(STRETCH - EV2**2)
! tester          
!!           A1 = ONE/LAM**2
!!           A2 = A1**2
!!           A3 = A1*A2
!!           A4 = A2**2
!!           FLAM= (ONE +  THREE*A1/FIVE + EIGHTY19*A2/175.D00 
!!     .                  + 513.D00*A3/875.D00 + 42039.D00*A4/67375.D00)  
           DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)         
              SIG = SIG + CC
           ENDDO
           SIG= SIG*FAC
!!           SIG= SIG*FAC/FLAM
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
C         
      RETURN  
      END SUBROUTINE ARRUDA_BOYCE
C Compute normal stress DMu=dN/dmu, dlam = dN/dlam 
Chd|====================================================================
Chd|  ARRUDA_BOYCE_DYDA             source/materials/mat/mat092/law92_nlsqf.F
Chd|-- called by -----------
Chd|        MRQCOF_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ARRUDA_BOYCE_DYDA(STRETCH,A,DYDA, ITEST) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDATA,ITEST
      my_real 
     .     STRETCH, A(*),DYDA(*)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER K,J
      my_real 
     .  C(5),MU,LAM,EV2,TRACE,FAC,AA,BB,CC,DD,SIG,EV
C======================================================================= 
      MU = A(1)
      LAM = A(2)
      C(1) = HALF
      C(2) = ONE/TWENTY
      C(3) = ELEVEN/1050.D00
      C(4) = 19.D00/7000.D00
      C(5) = 519.D00/673750.D00
C      
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
C         
      RETURN  
      END SUBROUTINE ARRUDA_BOYCE_DYDA
Chd|====================================================================
Chd|  MRQMIN_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- called by -----------
Chd|        LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|-- calls ---------------
Chd|        INVERSION                     source/materials/tools/nlsqf.F
Chd|        MRQCOF_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|====================================================================
      SUBROUTINE MRQMIN_LAW92(X,Y,SIG,NDATA,A,
     .                 COVAR,ALPHA,NCA, ERRNOW, 
     .                 GAMMA,IRET,ITEST)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com10_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "scr10_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER NCA,NDATA,MMAX,ITEST  
      my_real 
     .        GAMMA,ERRNOW,A(*),ALPHA(NCA,NCA),
     .        COVAR(NCA,NCA),X(NDATA),Y(NDATA),SIG(NDATA),
     .        INVJTJ(2,2),DMU,DLAM,DET
      PARAMETER (MMAX=2)  
      INTEGER J,K,L,MFIT  
      my_real ERRPRE,ATRY(MMAX),BETA(MMAX),DA(MMAX),EV  
      SAVE ERRPRE,ATRY,BETA,DA,MFIT  
      INTEGER IFUNCS, IRET,IFLAG
C=======================================================================

      IRET = 0
      IFLAG = 0
      MFIT = NCA

      IF (GAMMA < ZERO) THEN  
        MFIT = NCA
!       in Numerical Recript, 0.001 is used, however from my tests, the
!       difference is small (0.01 vs 0.001)
        GAMMA=0.01
C       
        CALL MRQCOF_LAW92(X,Y,SIG,NDATA,
     .              A,ALPHA,BETA,NCA,ERRNOW,ITEST)  

        ERRPRE=ERRNOW  
        DO J=1,NCA 
          ATRY(J)=A(J)  
        ENDDO  
      ENDIF  
C
      DO J=1,NCA  
        DO K=1,NCA   
          COVAR(J,K)=ALPHA(J,K)  
        ENDDO   
!ok too           COVAR(J,J)=ALPHA(J,J)*(ONE + GAMMA)  
         COVAR(J,J)=ALPHA(J,J) + GAMMA  
         DA(J)=BETA(J)  
      ENDDO 
C    
      CALL INVERSION(COVAR,MFIT,NCA,DA,1,1,IRET)     
C      
      IF (IRET .NE. 0) THEN
        IF (IDEBUG .GT. 0) THEN
          WRITE(*,*) ' IRET = ', IRET
          DO J=1,NCA  
            WRITE(IOUT, *) 'J, A(J) = ', J, A(J)
          ENDDO  
        ENDIF
        RETURN
      ENDIF
C
      J=0  
      DO L=1,NCA 
        ATRY(L)=A(L) + DA(L)
      ENDDO  
      IF(ATRY(1) <= ZERO .OR. ATRY(2) <=0) THEN
         ATRY(1)= A(1)
         ATRY(2)= A(2)
      ENDIF 
!ok        IF(ATRY(1) <= ZERO) THEN
!ok          ATRY(1)= A(1)
!ok       ENDIF 
!ok      IF(ATRY(2) <= ZERO ) THEN
!ok        ATRY(2)= A(2)
!ok      ENDIF  
      IF (IDEBUG .GT. 0) THEN
        WRITE(IOUT,*) __FILE__,__LINE__
        DO J=1, NCA
          write(IOUT,*) 'J,ATRY(J) = ', J, ATRY(J)
        ENDDO
      ENDIF
C
      CALL MRQCOF_LAW92(X,Y,SIG,NDATA,
     .                 ATRY,COVAR,DA,NCA,ERRNOW,ITEST)
C
      IF (ERRNOW < ERRPRE) THEN 
!!        GAMMA=GAMMA/NINE 
        GAMMA=GAMMA/10. 
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
!!        GAMMA=ELEVEN*GAMMA 
        GAMMA=10*GAMMA
        GAMMA = MIN(GAMMA, EP10)
        ERRNOW=ERRPRE  
      ENDIF  
C-----------
      RETURN  
      END SUBROUTINE MRQMIN_LAW92
C-----------
Chd|====================================================================
Chd|  MRQCOF_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- called by -----------
Chd|        MRQMIN_LAW92                  source/materials/mat/mat092/law92_nlsqf.F
Chd|-- calls ---------------
Chd|        ARRUDA_BOYCE                  source/materials/mat/mat092/law92_nlsqf.F
Chd|        ARRUDA_BOYCE_DYDA             source/materials/mat/mat092/law92_nlsqf.F
Chd|====================================================================
      SUBROUTINE MRQCOF_LAW92(X,Y,SIG, NDATA,
     *                        A,ALPHA,BETA,NALP,ERRNOW,ITEST)  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALP,NDATA,ITEST
      my_real 
     .    ERRNOW,A(*),ALPHA(NALP,NALP),BETA(*),
     .    X(NDATA),Y(NDATA),SIG(NDATA)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER MFIT,I,J,K,L,M,MMAX   
      PARAMETER (MMAX=20)  
      my_real 
     .   DY,WT,YMOD,DYDA(MMAX),WT1,
     .   JTJ11,JTJ12,JTJ22,JTJ21

      my_real
     . Y_MIN

       Y_MIN = EM3

C=======================================================================  
      MFIT=NALP 
      DO 13 J=1,MFIT  
        DO 12 K=1,J  
          ALPHA(J,K)=0.  
12      CONTINUE  
        BETA(J)=0.  
13    CONTINUE  
      ERRNOW=ZERO
      DO 16 I=1,NDATA  
        CALL ARRUDA_BOYCE(X(I),A,YMOD,ITEST) 
        CALL ARRUDA_BOYCE_DYDA(X(I),A,DYDA,ITEST)
        DY=Y(I)-YMOD
        ERRNOW=ERRNOW+DY*DY/(SIG(I)*SIG(I))
!ok         ERRNOW=ERRNOW+DY*DY     
C
        J=0  
        DO 15 L=1,MFIT  
            J=J+1  
            WT=DYDA(L)/(SIG(I)*SIG(I)) 
!ok             WT=DYDA(L)
            K=0  
            DO 14 M=1,L
                K=K+1  
                ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(M)
14          CONTINUE  
            BETA(J)=BETA(J) + DY*WT   
15      CONTINUE  
16    CONTINUE  
      DO 18 J=2,MFIT  
        DO 17 K=1,J-1  
          ALPHA(K,J)=ALPHA(J,K)  
17      CONTINUE  

18    CONTINUE 
      RETURN  
      END SUBROUTINE MRQCOF_LAW92
C Compute normal stress DMu=dN/dmu, dlam = dN/dlam 
Chd|====================================================================
Chd|  LAW92_GUESS                   source/materials/mat/mat092/law92_nlsqf.F
Chd|-- called by -----------
Chd|        LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAW92_GUESS(STRETCH,SIG,A,NDATA ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDATA
      my_real 
     .     STRETCH(*), A(*),SIG(*)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER K,J
      my_real 
     .  C(5),MU,LAM,EV2,TRACE,FAC,AA,BB,CC,DD,DYDA(NDATA)
C======================================================================= 
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
Ccauchy stress       FAC = TWO*STRETCH(K)*(STRETCH(K) - EV2**2)
         FAC = TWO*(STRETCH(K) - EV2**2)
       DO J=1,5
              BB = ONE/LAM**(2*J - 2)
              AA = J*C(J)
              CC = AA*BB*TRACE**(J-1)
              DYDA(K) = DYDA(K) +  CC 
       ENDDO
            DYDA(K) = DYDA(K)*FAC
       ENDDO
C
      AA = ZERO
      BB = ZERO
      DO K=1,NDATA
        CC = DYDA(K)/SIG(K)
        BB = BB + CC
        AA = AA +  CC**2
       ENDDO
       A(1) = BB/AA                     
C         
      RETURN  
      END SUBROUTINE LAW92_GUESS
