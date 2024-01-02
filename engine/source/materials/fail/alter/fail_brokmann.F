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
Chd|  FAIL_BROKMANN                 source/materials/fail/alter/fail_brokmann.F
Chd|-- called by -----------
Chd|        FAIL_WIND_FRWAVE              source/materials/fail/alter/fail_wind_frwave.F
Chd|-- calls ---------------
Chd|        NEWMAN_RAJU                   source/materials/fail/alter/newman_raju.F
Chd|====================================================================
      SUBROUTINE FAIL_BROKMANN(
     .     NEL       ,NUPARAM   ,NUVAR     ,TIME      ,TIMESTEP  ,
     .     UPARAM    ,NGL       ,SIGNXX    ,SIGNYY    ,SIGNXY    ,
     .     UVAR      ,OFF       ,IPT       ,NINDXF    ,INDXF     ,
     .     TDEL      )
C-----------------------------------------------
c    Test of Ch. Brokmann criterion in glass, taking account for initial
c    statistically distributed micro cracks
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,IPT,NINDXF
      my_real TIME,TIMESTEP
c
      INTEGER, DIMENSION(NEL) :: NGL,INDXF
      my_real, DIMENSION(NEL) :: SIGNXX,SIGNYY,SIGNXY,OFF,TDEL
      my_real, DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEB,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX
c
      my_real :: ALPHA,ALPHAI,EXP_N,EXP_M,K_IC,K_TH,SIG_INI,DSIG_INI,XDAMP,V0,
     .   DA,DC,V_A,V_C,KCM,KTM,VM,SIG_COS,SIG_COSW,DSIG_N,YA,YC,K1_A,K1_C,
     .   CR_ANG,FAC_M,FAC_L,FAC_T,FAC_LENM,FAC_MPA,FAC_PI0,FAC_PI2
c--------------------------------------------------------------------
c---  state variables for Ch.Brokmann extension
c     UVAR(15) = FAIL_B   :  Failure flag, set = 1 to allow Alter test
c     UVAR(16) = CR_LEN   :  Crack length
c     UVAR(17) = CR_DEPTH :  Crack width
c     UVAR(18) = CR_ANG   :  Random crack angle
c     UVAR(19) = THK0     :  Initial thickness of the shell (in micrometers)
c     UVAR(20) = ALDT0    :  Initial width of the shell (in micrometers)
c     UVAR(21) = SIG_COS  :  Crack opening stress (saved for filtering)
C=======================================================================
      EXP_N   = UPARAM(1)
      K_IC    = UPARAM(6)
      K_TH    = UPARAM(7)
      V0      = UPARAM(8)
      ALPHA   = UPARAM(10)
      IDEB    = NINT(UPARAM(17))
      SIG_INI = UPARAM(30)
      FAC_M   = UPARAM(33) 
      FAC_L   = UPARAM(34)
      FAC_T   = UPARAM(35)
c--------------------------------------------------
c     parameter initialization and unit_conversions
c--------------------------------------------------
      FAC_LENM = EP06 * FAC_L   ! conversion to micrometers
      FAC_MPA  = EM6  * FAC_M / (FAC_L * FAC_T**2) ! stress conversion to MPa
      VM       = V0 * FAC_L / FAC_T  ! conversion to (m/s)
      KCM      = K_IC * SQRT(FAC_L)  ! conversion to [stress*sqrt(m)]
      KTM      = K_TH * SQRT(FAC_L)  ! conversion to [stress*sqrt(m)]
c-----------------------------------
      ALPHAI = ONE - ALPHA
      NINDX  = 0
c
      DSIG_INI = 2.0    ! [MPa/s]
      XDAMP    = 25.0   ! hard coded damping exponent
      EXP_M    = ONE / (ONE + XDAMP) 
      FAC_PI0  = ZERO
      FAC_PI2  = HALF
c--------------------------------------------------------------------
      DO I = 1,NEL
        IF (OFF(I) == ONE .and. UVAR(I,15) == ZERO .and. UVAR(I,16) > ZERO) THEN
          V_A  = ZERO
          V_C  = ZERO          
c
c         Crack openenig stress (stress rotated in crack direction)
          CR_ANG  = UVAR(I,18) * TWO
          SIG_COS = HALF*(SIGNXX(I)+SIGNYY(I)) 
     .            + COS(CR_ANG) * HALF*(SIGNXX(I)-SIGNYY(I))
     .            + SIN(CR_ANG) * HALF*SIGNXY(I)
          SIG_COS = SIG_COS * ALPHA + UVAR(I,21)*ALPHAI   ! filter using exp average
c
c         take into account high stress rates
          DSIG_N = (SIG_COS - UVAR(I,21)) / MAX(TIMESTEP,EM20)
          DSIG_N = DSIG_N * FAC_MPA / FAC_T  ! => (MPa/s)

          UVAR(I,21) = SIG_COS ! Save Crack Opening Stress (COS)
          IF (DSIG_N > DSIG_INI) THEN
            SIG_COS = SIG_COS * (DSIG_INI/ABS(DSIG_N))**EXP_M
          END IF          
c------------------------
c         Residual stress
c------------------------
          SIG_COSW = SIG_COS - SIG_INI   ! Substracting initial surface stress
          SIG_COSW = MAX(ZERO , SIG_COSW)
c------------------------
c         Test criteria
c------------------------
          CALL NEWMAN_RAJU(UVAR(I,17),UVAR(I,16),UVAR(I,19),UVAR(I,20),FAC_PI2,YA)
          CALL NEWMAN_RAJU(UVAR(I,17),UVAR(I,16),UVAR(I,19),UVAR(I,20),FAC_PI0,YC)     
          K1_A = YA * SIG_COSW * SQRT(PI*UVAR(I,16)*EM6)  
          K1_C = YC * SIG_COSW * SQRT(PI*UVAR(I,17)*EM6)
c
c         Check rupture criterion #1
          IF (K1_A >= KCM) THEN
            TDEL(I) = TIME            ! tag to start damage                                                          
            NINDX   = NINDX + 1                                                           
            NINDXF  = NINDXF+ 1                                                    
            INDX(NINDX)   = I                                                           
            INDXF(NINDXF) = I         ! tag to save crack direction                                                          
            UVAR(I,15)    = ONE        ! set failure flag = 1
          END IF
c         
          IF (K1_A >= KTM .and. K1_A < KCM) V_A = V0*(K1_A/KCM)**EXP_N
          IF (K1_C >= KTM .and. K1_A < KCM) V_C = V0*(K1_C/KCM)**EXP_N
c
c         Crack growth
          DA = V_A * TIMESTEP * FAC_LENM
          DC = V_C * TIMESTEP * FAC_LENM
          UVAR(I,16) = UVAR(I,16) + DA
          UVAR(I,17) = UVAR(I,17) + DC
c
c         Check rupture criterion #2
          K1_A = YA * SIG_COS * SQRT(PI*UVAR(I,16)*EM6)
          IF (K1_A >= KCM .and. UVAR(I,15) == ZERO) THEN
            TDEL(I) = TIME            ! tag to start damage                                                          
            NINDX   = NINDX + 1                                                           
            NINDXF  = NINDXF+ 1                                                    
            INDX(NINDX)   = I                                                           
            INDXF(NINDXF) = I         ! tag to save crack direction                                                          
            UVAR(I,15)    = ONE        ! set failure flag = 1
          END IF
    
        END IF
      END DO
c-----------------------------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IPT,TIME                                
          WRITE(ISTDO,1100) NGL(I),IPT,TIME                               
#include "lockoff.inc"
        ENDDO        
      ENDIF          
c-----------------------------------------------
 1000 FORMAT(1X, ' **** FAILURE due to Brokmann criterion : ELEMENT ',I10,
     .       ' INT POINT',I2,' AT TIME ',1PE12.4)
 1100 FORMAT(1X, ' **** FAILURE due to Brokmann criterion : ELEMENT ',I10,
     .       ' INT POINT',I2,' AT TIME ',1PE12.4)
c-----------
      RETURN
      END
