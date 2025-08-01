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
!||    fail_cockroft_ib   ../engine/source/materials/fail/cockroft_latham/fail_cockroft_ib.F
!||--- called by ------------------------------------------------------
!||    fail_beam18        ../engine/source/elements/beam/fail_beam18.F
!||====================================================================
      SUBROUTINE FAIL_COCKROFT_IB(                                         
     .          NEL      ,NGL      ,NUPARAM  ,UPARAM  ,
     .          TIME     ,DPLA     ,OFF      ,DFMAX,  
     .          TDEL     ,IOUT     ,ISTDO    ,EPSXX    ,
     .          IPT      ,SIGNXX   ,SIGNXY   ,SIGNXZ   ,
     .          NVARF    , UVAR    ,FOFF     ,UELR,NPG)
!====================================================================
! cockroft  failure model for integrated beams
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                     ,INTENT(IN)    :: NEL     ! size of element group
      INTEGER                     ,INTENT(IN)    :: NUPARAM ! size of parameter array
      INTEGER                     ,INTENT(IN)    :: IPT     ! current integration point
      INTEGER                     ,INTENT(IN)    :: IOUT    ! output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   ! output file unit
      INTEGER                     ,INTENT(IN)    :: NVARF
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      my_real                     ,INTENT(IN)    :: TIME    ! current time
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)    :: UPARAM  ! failure model parameter array
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: DPLA    ! plastic strain increment
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element deactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SIGNXX  ! stress component
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SIGNXY  ! stress component
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: SIGNXZ  ! stress component


      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSXX 
      INTEGER ,DIMENSION(NEL)     ,INTENT(INOUT) :: FOFF    ! integration point deactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! deactivation time
      my_real ,DIMENSION(NEL,NVARF),INTENT(INOUT) :: UVAR  
      my_real, DIMENSION(NEL)     ,INTENT(INOUT)  :: UELR 
      INTEGER                      ,INTENT(IN)    :: NPG    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDXF,failip
      INTEGER ,DIMENSION(NEL) :: INDXF
      my_real
     .                            C0,EMA,EEQ,SIG_A,SIG_FILTRE,R,Q,I1,S11,
     .                            S22 , S33,R_INTER,PHI,I2 
      my_real ,DIMENSION(NEL) ::  EPS_EQ,DAMAGE,EPSRATE,EPS11,EPSI,D_EEQ
C=======================================================================
C     UVAR(I,1) contains previous equivalent strain value increment
C     UVAR(I,2) contains the Cockroft-Latham accumulated value
c     UVAR(I,3) contains the previous first principal stress
c     UVAR(I,4) contains total strain
      C0                  = UPARAM(1)
      EMA                 = UPARAM(2)
      FAILIP  = MIN(NINT(UPARAM(3)),NPG) ! 	Number of failed integration point prior to solid element deletion. default = 1 (integer)

      NINDXF              = 0                                   
c-----------------------------   
      IF(C0 < ZERO)THEN   ! equivalent strain 
        DO I =1,NEL
          IF(OFF(I) == ONE .AND. FOFF(I) == 1  ) THEN 
            EEQ   = ABS(EPSXX(I))         
            D_EEQ(I) = EEQ - UVAR(I,1)
            IF (D_EEQ(I) <= ZERO) D_EEQ(I) = ZERO
            UVAR(I,1) = EEQ             
          ENDIF
        ENDDO
      ELSE  ! positive = plastic strain increment
        DO I =1,NEL
          IF(OFF(I) == ONE .AND. FOFF(I) == 1  ) THEN 
            D_EEQ(I) = DPLA(I)
            UVAR(I,1) = UVAR(I,1) + DPLA(I)
          ENDIF
        ENDDO
      ENDIF


      DO I =1,NEL
        IF(OFF(I) == ONE .AND. FOFF(I) == 1  ) THEN
          !   principal stress calculation
          I1 = SIGNXX(I) 
          I2 = -SIGNXY(I)*SIGNXY(I)-SIGNXZ(I)*SIGNXZ(I)
          
          Q  = (THREE*I2 - I1*I1)/NINE
          R  = (TWO*I1*I1*I1 - NINE*I1*I2 )/CINQUANTE4     
           
          R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
          PHI = ACOS(MAX(R_INTER,-ONE))
           
          S11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
          S22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
          S33 = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1

          SIG_A = MAX (S11  ,S22 )
          SIG_A = MAX (SIG_A,S33 )


          IF (SIG_A>ZERO)THEN
            SIG_FILTRE   = SIG_A * EMA + (ONE-EMA)* UVAR(I,3)
            UVAR(I,3)= SIG_FILTRE
            !integral
            UVAR(I,2) = UVAR(I,2) + MAX(SIG_FILTRE,ZERO) * D_EEQ(I)
          ENDIF

          !damage
          DFMAX(I) =  MIN(UVAR(I,2) / MAX(EM20,ABS(C0))  ,ONE)
          
          IF (DFMAX(I) >= ONE) THEN
            FOFF(I) = 0
            TDEL(I) = TIME 
            NINDXF  = NINDXF + 1  
            INDXF(NINDXF) = I
            DFMAX(I)      = ONE
            UELR(I)     = UELR(I) + ONE
            IF (NINT(UELR(I)) >= FAILIP) THEN 
              OFF(I)    = FOUR_OVER_5
            ENDIF
          ENDIF 
        ENDIF
      ENDDO
C!
      DO  J = 1,NINDXF
         I = INDXF(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IPT,TIME
          WRITE(ISTDO,1000) NGL(I),IPT,TIME
          IF (OFF(I) == FOUR_OVER_5) THEN 
            WRITE(IOUT, 1111) NGL(I),TIME
            WRITE(ISTDO,1111) NGL(I),TIME
          ENDIF  
#include "lockoff.inc" 

       ENDDO
c------------------
 1000 FORMAT(5X,'FAILURE (COCKROFT-LATHAM) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I5
     .      ,2X,'AT TIME :',1PE12.4)
 1111 FORMAT(1X,'DELETED BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
c------------------

       RETURN
       END
