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
Chd|====================================================================
Chd|  BROKMANN_CRACK_INIT           source/materials/fail/alter/brokmann_crack_init.F
Chd|-- called by -----------
Chd|        FAIL_WIND_FRWAVE              source/materials/fail/alter/fail_wind_frwave.F
Chd|-- calls ---------------
Chd|        NEWMAN_RAJU                   source/materials/fail/alter/newman_raju.F
Chd|====================================================================
      SUBROUTINE BROKMANN_CRACK_INIT(
     .     NEL       ,NUPARAM   ,NUVAR     ,IPT       ,NPT       ,
     .     UPARAM    ,UVAR      ,NGL       ,THK0      ,ALDT      ,
     .     CR_LEN    ,CR_DEPTH  ,CR_ANG    )
C-----------------------------------------------
c    Initialization of micro-cracks with Weibull distribution
c    for windshield failure using Ch.Brokmmann model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,IPT,NPT
      INTEGER, DIMENSION(NEL) ,INTENT(IN)  :: NGL
      my_real, DIMENSION(NEL) ,INTENT(IN)  :: THK0,ALDT
      my_real, DIMENSION(NEL) ,INTENT(OUT) :: CR_LEN,CR_DEPTH,CR_ANG
      my_real, DIMENSION(NUPARAM)   ,INTENT(IN)    :: UPARAM
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,P_SWITCH,ITER
      my_real :: V0,VM,RANDP,A_DRB,EXP_N,K_IC,KCM,AF,AFP,AL,CRLEN,
     .   ETA,ETA1,ETA2,BETA1,BETA2,TAU1,TAU2,ETA_DRB,BETA_DRB,TAU_DRB,BETAI,
     .   P_SCALE,FF,PHI,FACN,NS1,NS2,NS3,NS4,M1,M2,THKM,ALDTM,YITER,
     .   YI1,YI2,PY,PF,PW,P0,P1,P2,P3,P4,P5,Y_DRB,Y_SHIFT,DEPTH_MAX,YTOL,DELTA_Y,
     .   DSIG_DRB,SIG0,SIG1,SIG2,SIG3,SIG4,SIG5,SIG6,SSHIFT,SIG_MPA,
     .   FSIZE,FAC_M,FAC_L,FAC_T,FAC_V,FAC_LENM,FAC_MPA,FAC_PI2
      my_real, DIMENSION(NEL) :: SIG_DRB,Y0
c-----------------------
c---  state variables for Ch.Brokmann extension
c     UVAR(15) = FAIL_B   :  Failure flag, set = 1 to allow Alter test
c     UVAR(16) = CR_LEN   :  Crack length
c     UVAR(17) = CR_DEPTH :  Crack width
c     UVAR(18) = CR_ANG   :  Random crack angle
c     UVAR(19) = THK0     :  Initial thickness of the shell (in micrometers)
c     UVAR(20) = ALDT0    :  Initial width of the shell (in micrometers)
c     UVAR(21) = SIG_COS  :  Crack opening stress (saved for filtering)
c=======================================================================
      EXP_N    = UPARAM(1)
      K_IC     = UPARAM(6)
      V0       = UPARAM(8)
      A_DRB    = UPARAM(23)
      ETA1     = UPARAM(24)
      BETA1    = UPARAM(25)
      TAU1     = UPARAM(26)
      ETA2     = UPARAM(27)
      BETA2    = UPARAM(28)
      TAU2     = UPARAM(29)
      P_SCALE  = UPARAM(31)
      P_SWITCH = NINT(UPARAM(32))
      FAC_M    = UPARAM(33) 
      FAC_L    = UPARAM(34)
      FAC_T    = UPARAM(35)
c--------------------------------------------------
c     parameter initialization and unit_conversions
c--------------------------------------------------
      FAC_V    = FAC_L / FAC_T  ! conversion to (m/s)
      FAC_LENM = EP06 * FAC_L   ! conversion to micrometers
      FAC_MPA  = EM6  * FAC_M / (FAC_L * FAC_T**2) ! stress conversion to MPa      
      VM  = V0 * FAC_V          ! (m/s)
      KCM = K_IC * SQRT(FAC_L) 
c
c     hard coded values for calculation of statistical distribution parameters
      SIG0      = 75.0   ! [MPa]
      SSHIFT    = 50.0   ! [MPa]
      DSIG_DRB  = 2.0    ! [MPa/s]
      DEPTH_MAX = 100.0   ! maximal initial crack depth in micrometers
      YI1       = 0.663
      YI2       = 1.1
      YTOL      = 0.0001
      DELTA_Y   = YTOL*TWO
      FAC_PI2   = HALF
c
      SIG1 = SIG0 + SSHIFT  ! 125 MPa
      SIG2 = SIG1 + SSHIFT  ! 175 MPa
      SIG3 = SIG2 + SSHIFT  ! 225 MPa
      SIG4 = SIG3 + SSHIFT  ! 275 MPa
      SIG5 = SIG4 + SSHIFT  ! 325 MPa
      SIG6 = SIG5 + SSHIFT  ! 375 MPa
c------------------
      NS1  = EXP_N
      NS2  = EXP_N * NS1
      NS3  = EXP_N * NS2
      NS4  = EXP_N * NS3
      FACN = (EXP_N - TWO) / (TWO*(EXP_N+ONE))
      M1   = EXP_N / (EXP_N - TWO)
      M2   = TWO  / (EXP_N - TWO)
c--------------------------------------------------------------------
c     Weibull distribution parameters on top and bottom surfaces
c--------------------------------------------------------------------
      IF (IPT == NPT) THEN           ! top surface - last integration point
        ETA      = ETA1
        BETA_DRB = BETA1
        TAU_DRB  = TAU1
      ELSE IF (IPT == 1) THEN        ! bottom surface - first integration point
        ETA      = ETA2
        BETA_DRB = BETA2
        TAU_DRB  = TAU2
      END IF
      BETAI = ONE / BETA_DRB
c
      IF (P_SWITCH == 1) THEN
        P0 = ZERO
        PF = ONE - P_SCALE
      ELSE IF (P_SWITCH == 0) THEN
        P0 = P_SCALE
        PF = ONE
      END IF
c--------------------------------------------
c       Calculation of P variable - Weibull distribution
c--------------------------------------------      
      DO I=1,NEL
        FSIZE = ALDT(I)*ALDT(I) / A_DRB
        THKM  = THK0(I) * FAC_LENM   !  initial shell thickness in micrometers
        ALDTM = ALDT(I) * FAC_LENM   !  initial shell width in micrometers
        UVAR(I,19) = THKM
        UVAR(I,20) = ALDTM
c            
        CALL RANDOM_NUMBER(RANDP)
        PW = P0 + RANDP * (PF - P0)

c       Calculate fracture stress value
        ETA_DRB = ETA * FSIZE**(-BETAI)
        FF      = LOG(ONE - PW) - (TAU_DRB / ETA_DRB)**BETA_DRB
        SIG_DRB(I) = -ETA_DRB*SIGN(ABS(FF)**BETAI,FF)   ! Fracture stress     
      END DO
c------------------------------------------
      DO I=1,NEL
c------------------------------------------
c       Initial Geometry Correction factor (Random value between YI1 and YI2
c
        CALL RANDOM_NUMBER(PY)
        Y0(I) = YI1 + (YI2-YI1)*PY
c
        SIG_MPA = SIG_DRB(I) * FAC_MPA                
c
        IF (SIG_MPA <  SIG0) THEN 
          ! Valid for P(0.663;1.1) at 50 MPa
          P1 = 3.854846e-04*NS4 -2.565276e-02*NS3 
     .        + 6.142425e-01*NS2 -6.200668e+00*NS1 + 2.450664e+01 
          P2 = -1.368162e-03*NS4 + 9.175738e-02*NS3 
     .        - 2.220515e+00*NS2 + 2.272542e+01*NS1 -9.012056e+01 
          P3 = 1.787600e-03*NS4 -1.208241e-01*NS3 
     .        + 2.955620e+00*NS2 -3.070972e+01*NS1 + 1.231156e+02 
          P4 = -1.020375e-03*NS4 + 6.954743e-02*NS3 
     .        - 1.721442e+00*NS2 + 1.820610e+01*NS1 -7.466173e+01 
          P5 = 2.130927e-04*NS4 -1.464948e-02*NS3 
     .        + 3.670756e-01*NS2 -3.957401e+00*NS1 + 1.785698e+01      
        ELSE IF (SIG_MPA >= SIG0 .and. SIG_MPA < SIG1) THEN 
          ! Valid for P(0.663;1.1) at 100 MPa
          P1 = -8.014637e-05*NS4 + 6.234394e-03*NS3 
     .        - 1.757171e-01*NS2 + 2.165919e+00*NS1 -7.843583e+00 
          P2 = 1.196065e-04*NS4 -9.673449e-03*NS3 
     .        + 2.757151e-01*NS2 -3.450433e+00*NS1 + 9.667971e+00 
          P3 = -3.978835e-05*NS4 + 3.830078e-03*NS3 
     .        - 1.122047e-01*NS2 + 1.423220e+00*NS1 + 1.058530e+00 
          P4 = 2.550177e-06*NS4 -6.309084e-04*NS3 
     .        + 1.772278e-02*NS2 -1.612462e-01*NS1 -4.326689e+00 
          P5 = -1.793787e-05*NS4 + 1.431770e-03*NS3 
     .        - 3.888580e-02*NS2 + 4.324152e-01*NS1 + 5.858975e-01      
        ELSE IF (SIG_MPA >= SIG1 .and. SIG_MPA < SIG2) THEN 
          ! Valid for P(0.663;1.1) at 150 MPa
          P1 = -2.446033e-04*NS4 + 1.828184e-02*NS3 
     .       -5.015157e-01*NS2 + 6.034614e+00*NS1 -2.490241e+01 
          P2 = 5.067851e-04*NS4 -3.734857e-02*NS3 
     .       + 1.003949e+00*NS2 -1.185853e+01*NS1 + 4.575989e+01
          P3 = -3.258463e-04*NS4 + 2.332497e-02*NS3 
     .       -5.974454e-01*NS2 + 6.706583e+00*NS1 -2.038952e+01 
          P4 = 7.386059e-05*NS4 -4.882414e-03*NS3 
     .       + 1.054173e-01*NS2 -9.057073e-01*NS1 -2.098349e+00 
          P5 = -3.790448e-05*NS4 + 2.752424e-03*NS3 
     .       -7.083969e-02*NS2 + 7.759309e-01*NS1 -8.351173e-01      
        ELSE IF (SIG_MPA >= SIG2 .and. SIG_MPA < SIG3) THEN 
          ! Valid for P(0.663;1.1) at 200 MPa
          P1 = -1.002426e-03*NS4 + 7.487536e-02*NS3 
     .       -2.056697e+00*NS2 + 2.464744e+01*NS1 -1.066757e+02 
          P2 = 3.118627e-03*NS4 -2.316144e-01*NS3 
     .       + 6.316496e+00*NS2 -7.506691e+01*NS1 + 3.214670e+02 
          P3 = -3.609067e-03*NS4 + 2.667937e-01*NS3 
     .       -7.231503e+00*NS2 + 8.529346e+01*NS1 -3.613532e+02 
          P4 = 1.870434e-03*NS4 -1.378567e-01*NS3 
     .       + 3.720583e+00*NS2 -4.361605e+01*NS1 + 1.826124e+02 
          P5 = -4.094512e-04*NS4 + 3.029387e-02*NS3 
     .       -8.210301e-01*NS2 + 9.660467e+00*NS1 -3.938020e+01      
        ELSE IF (SIG_MPA >= SIG3 .and. SIG_MPA < SIG4) THEN 
          ! Valid for P(0.663;1.1) at 250 MPa
          P1 = -1.661365e-03*NS4 + 1.251400e-01*NS3 
     .       -3.471542e+00*NS2 + 4.204654e+01*NS1 -1.854847e+02 
          P2 = 5.550941e-03*NS4 -4.160687e-01*NS3 
     .       + 1.147311e+01*NS2 -1.379743e+02*NS1 + 6.037335e+02 
          P3 = -6.851821e-03*NS4 + 5.116860e-01*NS3 
     .       -1.404451e+01*NS2 + 1.679347e+02*NS1 -7.296794e+02 
          P4 = 3.731562e-03*NS4 -2.780340e-01*NS3 
     .       + 7.608298e+00*NS2 -9.060341e+01*NS1 + 3.911483e+02 
          P5 = -8.004821e-04*NS4 + 5.976458e-02*NS3 
     .       -1.639088e+00*NS2 + 1.955932e+01*NS1 -8.338423e+01      
        ELSE IF (SIG_MPA >= SIG4 .and. SIG_MPA < SIG5) THEN 
          ! Valid for P(0.663;1.1) at 300 MPa
          P1 = -1.929460e-03*NS4 + 1.471697e-01*NS3 
     .       -4.141752e+00*NS2 + 5.098240e+01*NS1 -2.294842e+02 
          P2 = 6.697166e-03*NS4 -5.079334e-01*NS3 
     .       + 1.419800e+01*NS2 -1.733844e+02*NS1 + 7.736174e+02 
          P3 = -8.537941e-03*NS4 + 6.448618e-01*NS3 
     .       -1.793464e+01*NS2 + 2.176802e+02*NS1 -9.643518e+02 
          P4 = 4.765638e-03*NS4 -3.590089e-01*NS3 
     .       + 9.951988e+00*NS2 -1.202820e+02*NS1 + 5.297103e+02 
          P5 = -1.023363e-03*NS4 + 7.720270e-02*NS3 
     .       -2.143459e+00*NS2 + 2.594356e+01*NS1 -1.131898e+02      
        ELSE IF (SIG_MPA >= SIG5 .and. SIG_MPA < SIG6) THEN 
          ! Valid for P(0.663;1.1) at 350 MPa
          P1 = -1.839328e-03*NS4 + 1.429385e-01*NS3 
     .       -4.107221e+00*NS2 + 5.173982e+01*NS1 -2.389880e+02 
          P2 = 6.608847e-03*NS4 -5.095222e-01*NS3 
     .       + 1.450735e+01*NS2 -1.808588e+02*NS1 + 8.261478e+02 
          P3 = -8.661755e-03*NS4 + 6.640298e-01*NS3 
     .       - 1.878160e+01*NS2 + 2.323316e+02*NS1 -1.052092e+03 
          P4 = 4.935803e-03*NS4 -3.770376e-01*NS3 
     .       + 1.061849e+01*NS2 -1.306599e+02*NS1 + 5.876649e+02 
          P5 = -1.067185e-03*NS4 + 8.159522e-02*NS3 
     .       - 2.300259e+00*NS2 + 2.832827e+01*NS1 -1.262924e+02      
        ELSE IF (SIG_MPA >= SIG6) THEN 
          ! Valid for P(0.663;1.1) at 400 MPa
          P1 = -1.487196e-03*NS4 + 1.192492e-01*NS3 
     .       -3.542728e+00*NS2 + 4.624246e+01*NS1 -2.215987e+02 
          P2 = 5.590149e-03*NS4 -4.422194e-01*NS3 
     .       + 1.294667e+01*NS2 -1.663471e+02*NS1 + 7.845554e+02 
          P3 = -7.575999e-03*NS4 + 5.938425e-01*NS3 
     .       -1.720949e+01*NS2 + 2.186274e+02*NS1 -1.018855e+03 
          P4 = 4.421525e-03*NS4 -3.445888e-01*NS3 
     .       + 9.921046e+00*NS2 -1.250843e+02*NS1 + 5.777186e+02 
          P5 = -9.673316e-04*NS4 + 7.539378e-02*NS3 
     .       -2.170851e+00*NS2 + 2.736578e+01*NS1 -1.251497e+02      
        END IF 
c      
        Y_SHIFT = P1 * Y0(I)**4 + P2 * Y0(I)**3 + P3 * Y0(I)**2 + P4 * Y0(I) + P5
        Y_DRB   = Y0(I) * Y_SHIFT     ! geometry correction factor 
c
        AF  = (KCM / (Y_DRB*SIG_DRB(I)))**2 / PI
        AFP = AF**M1
        AL  = (FACN * VM * SIG_MPA / DSIG_DRB + AF)**M2
        CR_LEN(I) = AFP / AL
        CR_LEN(I) = CR_LEN(I) * EP06 !  crack length in micrometers
c
        CALL RANDOM_NUMBER(PHI)
        CR_ANG(I) = PHI * PI         !  random crack angle (0 - PI)
      ENDDO   ! NEL
c-----------------------------------
c     Crack depth calculation
c-----------------------------------
      DO I=1,NEL
        CRLEN = CR_LEN(I)
        IF (CRLEN > ZERO) THEN
          ITER  = 0
          YITER = ZERO
          CR_DEPTH(I) = CRLEN*1.1
          THKM  = UVAR(I,19)
          ALDTM = UVAR(I,20)
c        
          DO WHILE (DELTA_Y >= YTOL)
            ITER = ITER + 1
            CALL NEWMAN_RAJU(CR_DEPTH(I),CRLEN,THKM,ALDTM,FAC_PI2,YITER)

            DELTA_Y = ABS(ONE-YITER/Y0(I))

            IF (YITER < Y0(I)) THEN
              CR_DEPTH(I) = CR_DEPTH(I)*(ONE+DELTA_Y)
            ELSEIF (YITER > Y0(I)) THEN
              CR_DEPTH(I) = CR_DEPTH(I)*(ONE-DELTA_Y)
            END IF

            IF (CR_DEPTH(I) < CRLEN) THEN
              CR_DEPTH(I) = CRLEN*1.01
            END IF

            IF (ITER >= 100) EXIT
          END DO
          CR_DEPTH(I) = MAX(CR_DEPTH(I) ,DEPTH_MAX)                   
c
        ELSE
          CR_DEPTH(I) = ZERO          
        END IF ! CR_LEN > 0 
      ENDDO
c-----------
      RETURN
      END
