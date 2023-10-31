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
Chd|  LAW69_NLSQF_AUTO              source/materials/tools/law69_nlsqf_auto.F
Chd|-- called by -----------
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        LAW69_CHECK                   source/materials/tools/nlsqf.F
Chd|        LAW69_GUESS1                  source/materials/tools/nlsqf.F
Chd|        LAW69_GUESS_BOUNDS            source/materials/tools/nlsqf.F
Chd|        MRQMIN                        source/materials/tools/nlsqf.F
Chd|        OGDEN0                        source/materials/tools/nlsqf.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAW69_NLSQF_AUTO(LAWID,STRETCH,Y,MA,NMUAL,NPT,MUAL,
     $                            ICHECK, NSTART, ERRTOL,ID,TITR)
      USE MESSAGE_MOD
    !-----------------------------------------------
    !   I m p l i c i t   T y p e s
    !-----------------------------------------------
#include      "implicit_f.inc"
    !-----------------------------------------------
    !   C o m m o n   B l o c k s
    !-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
    !-----------------------------------------------
    !   D u m m y   A r g u m e n t s
    !----------------------------------------------- 
      INTEGER ,  INTENT(INOUT) :: LAWID, NMUAL, MA,ICHECK
      INTEGER ,  INTENT(IN)    :: NPT, NSTART,ID
      my_real ,  INTENT(IN)    :: ERRTOL
      my_real , DIMENSION(NPT) , INTENT(IN) :: Y,STRETCH
      my_real , DIMENSION(10), INTENT(INOUT) :: MUAL
      CHARACTER*nchartitle, INTENT(IN) ::   TITR
    !-----------------------------------------------
    !   L o c a l   V a r i a b l e s
    !----------------------------------------------- 
      INTEGER MAXA 
      PARAMETER (MAXA=20)

      INTEGER I,IDUM,ITER,ICOUNT,J,K,IRET ,ISTART, NPSAVED, 
     .        IVALID,ICOMP, MMAX,MINITER_LM, MAXITER_LM,
     .        CNT_HIT_EPS_LM,LMT_HIT_EPS_LM, LMSTOP,IFUNCS,
     .        NGUESS,NMUALS,LOOP_ISTART ,ICHECK_GUESS,
     .        JCHECK, IFIT_SUCCESS,IEND_FINDING,IEND_ITER,
     .        MU_INCR_GUESS,IRND1,ISWITCH
      INTEGER NONZERO(MAXA)
      my_real GAMMA,ERRNOW,GASDEV,ERRPRE,ERRMIN, ERRMAX, 
     .        ERRAVE, ERRAVE_MIN,YOGD,XOGD,EPS_LM,MAX_ABS_YI,
     .        SMALL_FAC_ABS_YI, SMALL_ABS_YI,SPREADY,GAMMA_STOP,
     .        ERR
      my_real A(MAXA),COVAR(MAXA,MAXA),ALPHA(MAXA,MAXA),
     .        MCOF_MIN(MAXA), MCOF_MAX(MAXA),AS(10),A0(MAXA),
     .        ATRY(MAXA)

      my_real, DIMENSION(:), ALLOCATABLE :: SIG
!!=======================================================================
      
      MINITER_LM = 5
      MAXITER_LM = 100 
      EPS_LM     = EM03 
      LMT_HIT_EPS_LM = 2
      GAMMA_STOP   = EP15
      !!     if check the validity of initial guess (0=no, 1=yes)
      !!     we don't want to check, because invalid initial point
      !!     could converge to valid point
      ICHECK_GUESS = 0
      !!    if enforce mu(i) < mu(i+1) in generating initial gess
      !!    we don't need this anymore since we are using random numbers 
      !!    instead of loop through all
      !!    the combinations
      MU_INCR_GUESS =0
      
      ALLOCATE (SIG(1:NPT))
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
      SPREADY=EM02
      IF(ICOMP == 0) THEN
         DO J=1,NPT
           SIG(J)=SPREADY*max(SMALL_ABS_YI, ABS(Y(J)) )
         ENDDO   
      ELSE
        DO J=1,NPT
         SIG(J) = Y(J)
         IF(SIG(J) == ZERO)  SIG(J) = EM15
        ENDDO  
      ENDIF     
C=======================================================================           
      NPSAVED = 0
      AS(1:10) = ZERO
      IFIT_SUCCESS = 0
      IEND_ITER = 0
      IEND_FINDING = 0
      JCHECK = 2  ! stating Criteria 
      IFUNCS = 1
      LMSTOP =0
      DO WHILE(IFIT_SUCCESS == 0 )  ! f
        IF(IEND_ITER <= 1) THEN
          DO I=1,NMUAL 
             NONZERO(I)=1  
          ENDDO
          IF (LAWID == 1) then ! OGDEN
            DO I=NMUAL+1,10
              NONZERO(I)=0
              A(I)=ZERO
            ENDDO
          ELSEIF (LAWID == 2) then ! Mooney-Rivlin
            DO I=NMUAL+1,MA
              NONZERO(I)=0
            ENDDO
            NONZERO(2)=0
            NONZERO(4)=0
            DO I=NMUAL+1,MA
               A(I)=ZERO
            ENDDO
          ENDIF
        ENDIF  
        !
        ISTART = 0
        !! calculate MCOF_MIN, MCOF_MAX
        CALL LAW69_GUESS_BOUNDS(LAWID, JCHECK, STRETCH, Y, NPT, 
     .         NMUAL, MCOF_MIN, MCOF_MAX,ICOMP)

      !!     initialize random number generator in LAW69_GUESS1
        IRND1 = 205
        IEND_ITER = 0
        ATRY(1:MAXA) = ZERO ! Initialization of ATRY
        DO WHILE ( ISTART < NSTART  )     !!! 111 continue
      !!       get one guess point A0(1:NMUAL)  
          CALL LAW69_GUESS1(
     $            LAWID, NMUAL, ICHECK_GUESS, MU_INCR_GUESS,IRND1,
     $            A0, NONZERO, MCOF_MIN, MCOF_MAX, NGUESS)

          IF (NGUESS /= 0) THEN
C       calculate averaged ERROR before LM         
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
C            
            ERRAVE = ERRAVE / (ONE * NPT)
            ISTART = ISTART + 1
          ! start loop of LM optimization        
            ITER = 0
            CNT_HIT_EPS_LM = 0

            ITER = ITER + 1
            GAMMA=-1
            CALL MRQMIN(STRETCH,Y,SIG,NPT,A0,NONZERO,
     .                   NMUAL,COVAR,ALPHA,MA,ERRNOW, 
     .                   IFUNCS,GAMMA,IRET,ICOMP,MMAX,ATRY)  
            IF (IRET /= 0) CYCLE
            LMSTOP = 0 
            LOOP_ISTART  = 0
            DO WHILE (LMSTOP == 0 )        
              ERRPRE=ERRNOW  
              ITER = ITER + 1
              CALL MRQMIN(STRETCH,Y,SIG,NPT,A0,NONZERO,
     .         MA,COVAR,ALPHA, MA, ERRNOW, 
     .         IFUNCS,GAMMA,IRET,ICOMP,MMAX,ATRY)
               IF (IRET /= 0)THEN
                LOOP_ISTART = 1
                EXIT
               ENDIF 
             !! IF A0(J) is too small, restart from next initial guess        
               DO J = 1, NMUAL
                  IF ( ABS( A0(J) ) < 1E-20 ) THEN
                     LOOP_ISTART = 1
                     EXIT
                  ENDIF  
                ENDDO
                IF(LOOP_ISTART == 1) EXIT
c       check convergence of LM optimization        
                IF ( ITER > MINITER_LM ) THEN
                   IF (ERRNOW < ERRPRE) THEN
                      IF ( ABS(ERRPRE) > ZERO) THEN
                          IF  ( ABS( (ERRNOW-ERRPRE)/ ERRPRE ) < EPS_LM) THEN
                            CNT_HIT_EPS_LM = CNT_HIT_EPS_LM + 1  
                            IF ( CNT_HIT_EPS_LM >= LMT_HIT_EPS_LM ) THEN
                               LMSTOP = 1
                            ENDIF
                          ENDIF
                      ENDIF
                    ELSEIF (ITER >= MAXITER_LM .OR. GAMMA >= GAMMA_STOP) THEN
                      LMSTOP = 1
                    ENDIF  
                ENDIF
            ENDDO ! LMSTOP
            IF(LOOP_ISTART == 1) CYCLE 
!                
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
          ERRAVE = ERRAVE / (ONE * NPT)
          CALL LAW69_CHECK(LAWID, A0, NMUAL, JCHECK, 0, IVALID)
          IF (IVALID > 0) THEN
           IF (NPSAVED==0 .OR. 
     $         ( NPSAVED > 0 .AND. ERRAVE < ERRAVE_MIN) ) THEN
               NPSAVED = NPSAVED + 1
               ERRAVE_MIN = ERRAVE
               NMUALS = NMUAL
               DO I = 1, NMUAL
                A(I)  = A0(I)
                AS(I) = A0(I)
               ENDDO 
             ENDIF
           ENDIF
          ! Check the fit 
           IF (NPSAVED > 0 .AND.  ERRAVE_MIN < ERRTOL ) THEN
              IFIT_SUCCESS = 1
              EXIT 
           ENDIF
          ENDIF ! NGUESS 
         ISWITCH = 0
         IF(IFIT_SUCCESS == 0 .AND. ISTART < NSTART ) THEN
           CYCLE 
         ELSEIF(IFIT_SUCCESS == 0 .OR. (NGUESS == 0 .AND. IFIT_SUCCESS ==0)) THEN 
            ISWITCH = 1 
             IF (JCHECK == 2) THEN                                               
              JCHECK = 1                                                         
              IEND_ITER = 2                                                      
             ELSEIF(JCHECK == 1) THEN                                            
               IF(LAWID == 2) THEN                                               
                LAWID = 1                                                        
                JCHECK = 2                                                       
                IEND_ITER =1                                                     
               ELSEIF(NMUAL < 10) THEN                                           
                 NMUAL = MIN(10, NMUAL + 2)                                      
                 JCHECK = 2                                                      
                 IEND_ITER = 1 
               ELSEIF(ICOMP == 0) THEN
                 ICOMP   = 1
                 JCHECK  = 2
                 LAWID   = 2
                 NMUAL   = 2                                                  
               ELSE                                                              
                  IEND_FINDING = 1                                               
               ENDIF                                                             
             ENDIF ! JCHECK                                                      
             EXIT                                                                
         ENDIF ! IFIT_SUCCESS                                     
       ENDDO  ! WHILE ( ISTART < NSTART )
          IF(ISWITCH == 0 .AND. IFIT_SUCCESS == 0 ) THEN
            IF (JCHECK == 2) THEN                                               
              JCHECK = 1                                                         
              IEND_ITER = 2                                                      
             ELSEIF(JCHECK == 1) THEN                                            
               IF(LAWID == 2) THEN                                               
                LAWID = 1                                                        
                JCHECK = 2                                                       
                IEND_ITER =1                                                     
               ELSEIF(NMUAL < 10) THEN                                           
                 NMUAL = MIN(10, NMUAL + 2)                                      
                 JCHECK = 2                                                      
                 IEND_ITER = 1                                                   
               ELSEIF(ICOMP == 0) THEN
                 ICOMP   = 1
                 JCHECK  = 2
                 LAWID   = 2
                 NMUAL   = 2 
               ELSE                                                              
                 IEND_FINDING = 1                                               
               ENDIF                                                             
             ENDIF ! JCHECK  
          ENDIF
       IF(IFIT_SUCCESS == 1 .OR. IEND_FINDING == 1) EXIT 
      ENDDO  ! IFIT_SUCCESS
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
      NMUAL = NMUALS
      DO I=1,10
        MUAL(I)=AS(I)
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
      DO I=1,NPT
        CALL OGDEN0(STRETCH(I), AS, YOGD, NMUALS)
        WRITE(IOUT, '(1F18.6, 3X,1F20.13, 6X, 1F20.13)')
     *   STRETCH(I)-ONE,Y(I),YOGD
      ENDDO      

      WRITE(IOUT, '(A)') '-------------------------------------------'
      WRITE(IOUT, '(A,F10.2,A)') 'AVERAGED ERROR OF FITTING : ', 
     .                            ERRAVE_MIN*100.0, '%'
C-----------
      RETURN
      END  
