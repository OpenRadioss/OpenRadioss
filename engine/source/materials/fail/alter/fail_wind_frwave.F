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
Chd|  FAIL_WIND_FRWAVE              source/materials/fail/alter/fail_wind_frwave.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        BROKMANN_CRACK_INIT           source/materials/fail/alter/brokmann_crack_init.F
Chd|        FAIL_BROKMANN                 source/materials/fail/alter/fail_brokmann.F
Chd|        RUPTURE_MOD                   share/modules/rupture_mod.F   
Chd|====================================================================
      SUBROUTINE FAIL_WIND_FRWAVE(
     1     NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      ,
     2     TIME      ,TIMESTEP  ,SSP       ,ALDT      ,FWAVE_EL  ,
     3     TDEL1     ,TDEL2     ,OFF       ,OFFLY     ,FOFF      ,
     4     SIGNXX    ,SIGNYY    ,SIGNXY    ,DFMAX     ,NGL       ,
     5     ILAY      ,IPT       ,NPT       ,CRKDIR    ,DADV      ,
     6     DMG_FLAG  ,TRELAX    ,THK0      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RUPTURE_MOD
c-----------------------------------------------
c    Windshield failure model with crack propagation (ref : PhD thesis Christian Alter 2018)
c    front fawe propagation 
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
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | R | STRESS XX
C SIGNYY  | NEL     | F | R | STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NEL,NUPARAM,NUVAR,ILAY,IPT,NPT
      INTEGER ,INTENT(INOUT) :: DMG_FLAG
      INTEGER, DIMENSION(NEL) ,INTENT(IN)     :: NGL,FWAVE_EL
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT)  :: OFFLY,FOFF
c
      my_real :: TIME,TIMESTEP,TRELAX
      my_real, DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
      my_real, DIMENSION(NEL) :: SSP,ALDT,THK0,TDEL1,TDEL2,
     .   SIGNXX,SIGNYY,SIGNXY
      my_real ,DIMENSION(NEL,2)  ,INTENT(OUT)   :: CRKDIR
      my_real ,DIMENSION(NEL)    ,INTENT(INOUT) :: OFF,DFMAX,DADV
      my_real ,DIMENSION(NEL,NUVAR), TARGET, INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDXF1,NINDXF2,NINDXD1,NINDXD2,IDEB,IMOD,ISRATE,
     .   ISIDE,ITGLASS
      INTEGER ,DIMENSION(NEL) :: INDXF1,INDXF2,INDXD1,INDXD2,RFLAG
c
      my_real SIGMAX,AA,BB,CC,CR,COSX,SINX,S1,S2,S3,K_IC,K_TH,
     .   GEORED,CR_FOIL,CR_AIR,CR_CORE,CR_EDGE,ALPHA,ALPHAI,BETA,
     .   KRES1,KRES2,EXP_N,EXP_M,V0,VC,REFLEN,DMG1,DMG2,DMG3,TANPHI
      my_real ,DIMENSION(NEL) :: TPROPG,AI,FORMF,SIGP_AKT,DAM1,DAM2,
     .   SIGP1,SIGP2,SIGDT1,SIGDT2,SIG_DTF1,SIG_DTF2,SIGP_MIN,SIGP_MAX,
     .   SP1,SP2,SP3,SIN2,COS2,COSI,TFACT1,TFACT2,CR_LEN,CR_DEPTH,CR_ANG   
      my_real, DIMENSION(:), POINTER :: DIR1,DIR2
c--------------------------------------------------------------------
c---  state variables for Ch.Alter model
c     UVAR(1)  = SIGP1       ! principal stress DIR1               
c     UVAR(1)  = SIGP2       ! principal stress DIR2               
c     UVAR(3)  = SIGDT1      ! filtered principal stress rate DIR1
c     UVAR(4)  = SIGDT2      ! filtered principal stress rate DIR2
c     UVAR(5)  = SIGP_MIN                                          
c     UVAR(6)  = SIGP_MAX                                          
c     UVAR(7)  = FORMF                                             
c     UVAR(8)  = AI          ! crack depth                         
c     UVAR(9)  = SIGP_AKT                                         
c     UVAR(10) = edge element flag
c     UVAR(11) = DAM1
c     UVAR(12) = DAM2
c     UVAR(13) = SIG_DTF1
c     UVAR(14) = SIG_DTF2
c---  state variables for Ch.Brokmann extension
c     UVAR(15) = FAIL_B   :  Failure flag, set = 1 to allow Alter test
c     UVAR(16) = CR_LEN   :  Crack length
c     UVAR(17) = CR_DEPTH :  Crack width
c     UVAR(18) = CR_ANG   :  Random crack angle
c     UVAR(19) = THK0     :  Initial thickness of the shell (in micrometers)
c     UVAR(20) = ALDT0    :  Initial width of the shell (in micrometers)
c     UVAR(21) = SIG_COS  :  Crack opening stress (saved for filtering)
C=======================================================================
      RFLAG(1:NEL) = 0
      NINDXF1 = 0  
      NINDXF2 = 0  
      NINDXD1 = 0                                     
      NINDXD2 = 0   
c--------------------------------------------------------------------
      EXP_N    = UPARAM(1)
      CR_FOIL  = UPARAM(2)
      CR_AIR   = UPARAM(3)
      CR_CORE  = UPARAM(4)
      CR_EDGE  = UPARAM(5)
      K_IC     = UPARAM(6)
      K_TH     = UPARAM(7)
      V0       = UPARAM(8)
      VC       = UPARAM(9)
      ALPHA    = UPARAM(10)
      GEORED   = UPARAM(11)
      REFLEN   = UPARAM(14)
      IMOD     = NINT(UPARAM(15))
      ISRATE   = NINT(UPARAM(16))
      IDEB     = NINT(UPARAM(17))
      ISIDE    = NINT(UPARAM(18))
      TRELAX   = UPARAM(19)
      KRES1    = UPARAM(20)
      KRES2    = UPARAM(21)
      ITGLASS  = NINT(UPARAM(22))
c
      IF (TRELAX > 0) DMG_FLAG = 1
      ALPHAI = ONE - ALPHA
      EXP_M  = ONE / (ONE + EXP_N) 
c--------------------------------------------------------------------
      IF (TIME == ZERO) THEN
c       Crack depth modification and definition of Y (formfactor)
        DO I=1,NEL
          IF (UVAR(I,10) == ONE) THEN  ! edge-element
            AI(I) = CR_EDGE
            FORMF(I) = 1.12
          ELSE
            FORMF(I) = ONE
            IF (IPT == 1) THEN
              AI(I) = CR_FOIL  ! crack depth at foil side in mm (unit system: length)
            ELSEIF (IPT == NPT) THEN
              AI(I) = CR_AIR   ! crack depth at top side in mm (unit system: length)
            ELSE
              AI(I) = CR_CORE  ! crack depth inside glass
            ENDIF
          ENDIF
          
          AI(I) = AI(I) * (ONE - DFMAX(I))  ! take into account the initial damage
          SIGP_AKT(I) = (TWO*(EXP_N + ONE)*K_IC**EXP_N) /
     .              ((EXP_N - TWO)*V0*(FORMF(I)*SQRT(PI))**EXP_N*
     .               AI(I)**((EXP_N-TWO)/TWO))
          SIGP_AKT(I) = SIGP_AKT(I)**EXP_M
c
          SIGP_MIN(I) = K_TH / (FORMF(I)*SQRT(PI*AI(I)))
          SIGP_MAX(I) = K_IC / (FORMF(I)*SQRT(PI*AI(I)))
          DADV(I)   = ONE
          DAM1(I)   = ONE
          DAM2(I)   = ONE
          UVAR(I,5) = SIGP_MIN(I)
          UVAR(I,6) = SIGP_MAX(I)
          UVAR(I,7) = FORMF(I)
          UVAR(I,8) = AI(I)
          UVAR(I,9) = SIGP_AKT(I)
          UVAR(I,11)= DAM1(I)
          UVAR(I,12)= DAM2(I)
        ENDDO
      ELSE
        SIGP_MIN(1:NEL) = UVAR(1:NEL,5)
        SIGP_MAX(1:NEL) = UVAR(1:NEL,6)
        FORMF(1:NEL)    = UVAR(1:NEL,7)
        AI(1:NEL)       = UVAR(1:NEL,8)
        SIGP_AKT(1:NEL) = UVAR(1:NEL,9)
        DAM1(1:NEL)     = UVAR(1:NEL,11)
        DAM2(1:NEL)     = UVAR(1:NEL,12)
      ENDIF
c--------------------------------------------------------------------
c     Ch.Brokmann criterion - initialization 
c--------------------------------------------------------------------
      IF (ITGLASS == 1 .and. (IPT == 1 .or. IPT == NPT)) THEN
        IF (TIME == ZERO) THEN
c--------------------------------------------------------------------
c         Crack initialization on top and bottom surfaces using Weibull distribution
c--------------------------------------------------------------------
          CALL BROKMANN_CRACK_INIT(
     .     NEL       ,NUPARAM   ,NUVAR     ,IPT       ,NPT       ,
     .     UPARAM    ,UVAR      ,NGL       ,THK0      ,ALDT      ,
     .     CR_LEN    ,CR_DEPTH  ,CR_ANG    )
c
           UVAR(1:NEL,16) = CR_LEN(1:NEL)
           UVAR(1:NEL,18) = CR_ANG(1:NEL)
           UVAR(1:NEL,17) = CR_DEPTH(1:NEL)
        ENDIF        ! END Brokmann criterion initialization (TIME 0)
c--------------------------------------------------------------------
c       Test of Ch.Brokmann criterion
c--------------------------------------------------------------------      
        CALL FAIL_BROKMANN(
     .     NEL       ,NUPARAM   ,NUVAR     ,TIME      ,TIMESTEP  ,
     .     UPARAM    ,NGL       ,SIGNXX    ,SIGNYY    ,SIGNXY    ,
     .     UVAR      ,OFF       ,IPT       ,NINDXF1   ,INDXF1    ,
     .     TDEL1     )
c
      ELSE IF (TIME == ZERO) THEN   ! ITGLASS==0 or (IPT/=1 and IPT/= NPT)
c       Brokmann model not actif => test only Alter failure model
c
        UVAR(1:NEL,15) = ONE
c
      END IF   ! ITGLASS == 1 .and. (IPT == 1 .or. IPT == NPT)   
c--------------------------------------------------------------------
      DO I = 1,NEL
        IF (FWAVE_EL(I) > 0) THEN
          DADV(I)  = MIN(ONE, GEORED / SQRT(ALDT(I)/REFLEN) ) ! reduction factor for advancement
          RFLAG(I) = -1
          UVAR(I,15) = ONE
        ELSE IF (DADV(I) < ONE) THEN
          RFLAG(I) = -1
        ELSE IF (DADV(I) == ONE) THEN
          RFLAG(I) = 1
        ENDIF
        TPROPG(I) = ALDT(I) / MIN(VC,SSP(I))       
        TPROPG(I) = MAX(TPROPG(I), TIMESTEP)     
      END DO                                         
c--------------------------------------------------------------------
c     Principal stress
c--------------------------------------------------------------------
      DO I = 1,NEL
        S1 = SIGNXX(I) 
        S2 = SIGNYY(I) 
        CC = SIGNXY(I)
        AA = (S1 + S2)*HALF
        BB = (S1 - S2)*HALF
        CR = SQRT(BB**2 + CC**2)
        SIGP1(I) = AA + CR
        SIGP2(I) = AA - CR
        SP1(I)   = SIGP1(I)
        SP2(I)   = SIGP2(I)
        SP3(I)   = ZERO         
      END DO
c--------------------------------------------------------------------
c     Calculate stress rate + filtering
c--------------------------------------------------------------------
      IF (ISRATE == 0) THEN
c       exponential moving average with smoohting coefficient = alpha 
        DO I = 1,NEL
          IF (OFF(I) == ONE) THEN
            SIGDT1(I) = ABS(SIGP1(I) - UVAR(I,1)) / MAX(TIMESTEP, EM20)
            SIGDT2(I) = ABS(SIGP2(I) - UVAR(I,2)) / MAX(TIMESTEP, EM20)
            SIGDT1(I) = ALPHA * SIGDT1(I) + ALPHAI * UVAR(I,3)
            SIGDT2(I) = ALPHA * SIGDT2(I) + ALPHAI * UVAR(I,4)
          ENDIF        
        END DO
      ELSE
c       arythmetic moving average over 50 cycles
        DO I = 1,NEL
          IF (OFF(I) == ONE) THEN
            UVAR(I,21) = ABS(SIGP1(I) - UVAR(I,1)) / MAX(TIMESTEP, EM20)
            SIGDT1(I) = UVAR(I,3) + (UVAR(I,21) - UVAR(I,20+INDEX_OVER_50_CYCLES(NGR_FAIL_WIND))) / 50
            UVAR(I,20+INDEX_OVER_50_CYCLES(NGR_FAIL_WIND)) = UVAR(I,21)
c
            UVAR(I,71) = ABS(SIGP2(I) - UVAR(I,2)) / MAX(TIMESTEP, EM20)
            SIGDT2(I) = UVAR(I,4) + (UVAR(I,71) - UVAR(I,70+INDEX_OVER_50_CYCLES(NGR_FAIL_WIND))) / 50
            UVAR(I,70+INDEX_OVER_50_CYCLES(NGR_FAIL_WIND)) = UVAR(I,71)
          ELSE
            SIGDT1(I) = ZERO
            SIGDT2(I) = ZERO
          ENDIF        
        END DO 
        INDEX_OVER_50_CYCLES(NGR_FAIL_WIND) = INDEX_OVER_50_CYCLES(NGR_FAIL_WIND) - 1
        IF(INDEX_OVER_50_CYCLES(NGR_FAIL_WIND)==1) INDEX_OVER_50_CYCLES(NGR_FAIL_WIND) = 50   
      ENDIF
c--------------------------------------------------------------------
c     Max strength calculation
c--------------------------------------------------------------------
      IF (IPT == NPT) THEN   ! Top layer integration point
        DO I = 1,NEL
          SIG_DTF1(I) = SIGP_AKT(I) *ABS(SIGDT1(I))**EXP_M
          SIG_DTF2(I) = SIGP_AKT(I) *ABS(SIGDT2(I))**EXP_M      
          SIG_DTF1(I) = MAX(SIG_DTF1(I),SIGP_MIN(I))
          SIG_DTF1(I) = MIN(SIG_DTF1(I),SIGP_MAX(I))
          SIG_DTF2(I) = MAX(SIG_DTF2(I),SIGP_MIN(I))
          SIG_DTF2(I) = MIN(SIG_DTF2(I),SIGP_MAX(I))
        END DO     
      ELSEIF (IPT < NPT) THEN   ! inner integration points
        DO I = 1,NEL
          IF (UVAR(I,10) == ONE) THEN   ! edge element => stress rate dependent
            SIG_DTF1(I) = SIGP_AKT(I)*ABS(SIGDT1(I))**EXP_M
            SIG_DTF2(I) = SIGP_AKT(I)*ABS(SIGDT2(I))**EXP_M      
            SIG_DTF1(I) = MAX(SIG_DTF1(I),SIGP_MIN(I))
            SIG_DTF1(I) = MIN(SIG_DTF1(I),SIGP_MAX(I))
            SIG_DTF2(I) = MAX(SIG_DTF2(I),SIGP_MIN(I))
            SIG_DTF2(I) = MIN(SIG_DTF2(I),SIGP_MAX(I))
          ELSE IF (ISIDE == 1) THEN    ! inner  points are stress rate dependent in this case
            SIG_DTF1(I) = SIGP_AKT(I)*ABS(SIGDT1(I))**EXP_M
            SIG_DTF2(I) = SIGP_AKT(I)*ABS(SIGDT2(I))**EXP_M      
            SIG_DTF1(I) = MAX(SIG_DTF1(I),SIGP_MIN(I))
            SIG_DTF1(I) = MIN(SIG_DTF1(I),SIGP_MAX(I))
            SIG_DTF2(I) = MAX(SIG_DTF2(I),SIGP_MIN(I))
            SIG_DTF2(I) = MIN(SIG_DTF2(I),SIGP_MAX(I))
          ELSE   ! inner integration points outside edge are stress rate independent
            SIG_DTF1(I) = SIGP_MAX(I)
            SIG_DTF2(I) = SIGP_MAX(I)
          ENDIF
        END DO
      ENDIF      
c
      DO I = 1,NEL
        SIG_DTF1(I) = SIG_DTF1(I)*DADV(I)
        SIG_DTF2(I) = SIG_DTF2(I)*DADV(I)
      END DO
c--------------------------
c     test failure criteria in first direction
c--------------------------
      DO I = 1,NEL                                                                      
        IF (OFF(I) == ONE) THEN
          IF (TDEL1(I) == ZERO) THEN    ! no crack yet in the element
            IF (SIGP1(I) > SIG_DTF1(I) .and. UVAR(I,15) == ONE) THEN  ! test Alter
              TDEL1(I) = TIME                                                             
              NINDXF1 = NINDXF1+1       ! tag to save crack direction                                                     
              INDXF1(NINDXF1) = I                                                           
            ENDIF
          ENDIF
c
          IF (TDEL1(I) > ZERO) THEN   ! damage started                          
            TFACT1(I) = (TIME - TDEL1(I)) / TPROPG(I)                                      
            TFACT1(I) = MIN(ONE, TFACT1(I))                                     
            IF (UVAR(I,11) > ZERO .and. TFACT1(I) == ONE) THEN
              OFFLY(I) = -1      ! time to propagate info about failure  
            ENDIF                   
            DFMAX(I) = MAX(DFMAX(I), TFACT1(I))
            NINDXD1  = NINDXD1+1                                                         
            INDXD1(NINDXD1) = I                                                         
          ENDIF                                                                       
c
        ENDIF                                                                         
      ENDDO                                                                             
c--------------------------
c     calculate and save principal stress direction when first failure occurs
c--------------------------
      IF (NINDXF1 > 0) THEN                        
#include "vectorize.inc"                    
        DO J=1,NINDXF1                             
          I  = INDXF1(J)                           
          S1 = SIGNXY(I)                         
          S2 = SIGP1(I) - SIGNXX(I)              
          CR = SQRT(S1**2 + S2**2)               
          IF (CR > ZERO) THEN                    
            CRKDIR(I,1) = S1 / CR                
            CRKDIR(I,2) = S2 / CR                
          ELSEIF (S1 > S2) THEN                  
            CRKDIR(I,1) = ZERO                     
            CRKDIR(I,2) = ONE                   
          ELSE                                   
            CRKDIR(I,1) = ONE                   
            CRKDIR(I,2) = ZERO                     
          ENDIF                                  
        ENDDO                                    
      ENDIF                                                
c-----------------------------------------------
c     apply progressive damage and turn the stress tensor back to the local system
c-----------------------------------------------
      IF (NINDXD1 > 0) THEN
#include "vectorize.inc"                    
        DO J=1,NINDXD1                                   
          I = INDXD1(J)
          S1 = SIGNXX(I)
          S2 = SIGNYY(I)
          S3 = SIGNXY(I)
          COSX = CRKDIR(I,1)                            
          SINX = CRKDIR(I,2)                            
          COS2(I) = COSX * COSX
          SIN2(I) = SINX * SINX
          COSI(I) = COSX * SINX
c         rotate stress to previously saved crack direction            
          SP1(I) = COS2(I)*S1 + SIN2(I)*S2 + TWO*COSI(I)*S3
          SP2(I) = SIN2(I)*S1 + COS2(I)*S2 - TWO*COSI(I)*S3
          SP3(I) = COSI(I)*(S2 - S1) + (COS2(I) - SIN2(I))*S3
c          
          BETA = MAX(ZERO , ONE - TFACT1(I))
          DMG1 = MAX(KRES1, ONE - (ONE - KRES1) * TFACT1(I))   
          DMG3 = MIN(ONE, 0.6 + 0.4 * BETA)
          DAM1(I) = DMG1
c         stress reduction               
          SP1(I) = SP1(I) * DMG1
          SP3(I) = SP3(I) * DMG3
c         rotate reduced stress back to current element coordinate system            
          SIGNXX(I) = COS2(I)*SP1(I) + SIN2(I)*SP2(I) - TWO*COSI(I)*SP3(I)  
          SIGNYY(I) = SIN2(I)*SP1(I) + COS2(I)*SP2(I) + TWO*COSI(I)*SP3(I)  
          SIGNXY(I) = COSI(I)*(SP1(I) - SP2(I)) + (COS2(I) - SIN2(I))*SP3(I)
        ENDDO
      ENDIF
c--------------------------
c     test failure criteria in second direction
c--------------------------
      DO I = 1,NEL  
        IF (OFF(I) == ONE) THEN
          IF (TDEL1(I) > ZERO .and. TDEL2(I) == ZERO) THEN
            IF (SP2(I) > SIG_DTF2(I)) THEN   
              TDEL2(I) = TIME        
              NINDXF2  = NINDXF2 + 1                                                           
              INDXF2(NINDXF2) = I                                                           
            ENDIF
          ENDIF                                                                 
c
          IF (TDEL2(I) > ZERO) THEN                                                              
            TFACT2(I)   = (TIME - TDEL2(I)) / TPROPG(I)                                      
            TFACT2(I)   = MIN(ONE, TFACT2(I))
            IF (UVAR(I,12) > ZERO .and. TFACT2(I) == ONE) THEN                   
              IF (OFFLY(I) == 0) THEN        ! DIR1 already fully cracked before                                           
                OFFLY(I) = -2                                                   
              ELSE IF (OFFLY(I) == -1) THEN  ! DIR1 just cracked in this cycle                                     
                OFFLY(I) = -3                                                   
              ENDIF                                                             
            ENDIF                                                               
            NINDXD2 = NINDXD2+1                                                         
            INDXD2(NINDXD2) = I                                                         
          ENDIF                                                                     
        ENDIF                                                                     
      ENDDO                                                                             
c----------------------------
      IF (NINDXD2 > 0) THEN
#include "vectorize.inc"                    
        DO J=1,NINDXD2                                   
          I = INDXD2(J)
          BETA = MAX(ZERO , ONE - TFACT2(I))
          DMG2 = MAX(KRES2, ONE - (ONE - KRES2) * TFACT2(I))
          DMG3 = MIN(ONE, 0.6 + 0.4 * BETA)
          DAM2(I) = DMG2
c         stress reduction in 2nd direction             
          SP2(I) = SP2(I) * DMG2
          SP3(I) = SP3(I) * DMG3
c         rotate reduced stress back to current element coordinate system            
          SIGNXX(I) = COS2(I)*SP1(I) + SIN2(I)*SP2(I) - TWO*COSI(I)*SP3(I)  
          SIGNYY(I) = SIN2(I)*SP1(I) + COS2(I)*SP2(I) + TWO*COSI(I)*SP3(I)  
          SIGNXY(I) = COSI(I)*(SP1(I) - SP2(I)) + (COS2(I) - SIN2(I))*SP3(I)
          IF (DMG2 == ZERO) THEN
            FOFF(I) = 0
          ENDIF
        ENDDO
      ENDIF
c-----------------------------------------------
      DO I=1,NEL
        UVAR(I,1)  = SIGP1(I)
        UVAR(I,2)  = SIGP2(I)
        UVAR(I,3)  = SIGDT1(I)
        UVAR(I,4)  = SIGDT2(I)
        UVAR(I,11) = DAM1(I)
        UVAR(I,12) = DAM2(I)
      END DO     
c-----------------------------------------------
      IF (NINDXF1 > 0) THEN
        DO J=1,NINDXF1
          I = INDXF1(J)
#include "lockon.inc"
          IF (RFLAG(I) == 1) THEN 
c           failure initialization                                    
            WRITE(IOUT, 3000) NGL(I),ILAY,IPT                                    
            WRITE(ISTDO,3100) NGL(I),ILAY,IPT,TIME                               
          ELSEIF (RFLAG(I) == -1) THEN
c           failure advancement                                    
            WRITE(IOUT, 4000) NGL(I),ILAY,IPT                                    
            WRITE(ISTDO,4100) NGL(I),ILAY,IPT,TIME                               
          ENDIF            
          IF (IDEB == 1 .and. ABS(RFLAG(I)) == 1) THEN
            TANPHI = ATAN(CRKDIR(I,2) / SIGN(MAX(ABS(CRKDIR(I,1)),EM20),CRKDIR(I,1)))
            WRITE(IOUT,3500) NGL(I),TANPHI,DADV(I)
            WRITE(IOUT,3700) NGL(I),SIGP1(I),SIGDT1(I),SIG_DTF1(I)
          ENDIF
#include "lockoff.inc"
        ENDDO        
      ENDIF          
c-----------------------------------------------
      IF (NINDXF2 > 0) THEN
        DO J=1,NINDXF2
          I = INDXF2(J)
#include "lockon.inc"
          WRITE(IOUT, 5000) NGL(I),ILAY,IPT                                    
          WRITE(ISTDO,5100) NGL(I),ILAY,IPT,TIME                               
          IF (IDEB == 1 .and. ABS(RFLAG(I)) == 1) THEN
            WRITE(IOUT,3600) NGL(I),DADV(I)
            WRITE(IOUT,3800) NGL(I),SIGP2(I),SIGDT2(I),SIG_DTF2(I)
          ENDIF
#include "lockoff.inc"
        ENDDO        
      ENDIF          
c-----------------------------------------------
 3000 FORMAT(1X, 'FAILURE INITIALIZATION IN SHELL',I10,1X,' 1ST DIR, LAYER',I2,1X,'INT POINT',I2)
 3100 FORMAT(1X, 'FAILURE INITIALIZATION IN SHELL',I10,1X,' 1ST DIR, LAYER',I2,1X,'INT POINT',I2,
     .       1X, 'AT TIME ',1PE12.4)
 3500 FORMAT(10X,'SHELL ',I10,' MAJ ANGLE= ',1PE12.4,' STRESS REDUCTION= ',1PE12.4)
 3600 FORMAT(10X,'SHELL ',I10,' STRESS REDUCTION= ',1PE12.4)
 3700 FORMAT(10X,'SHELL ',I10,'  SIGP1=',1PE12.4,'  SIGDT1=',1PE12.4,'  SIG_DTF1=',1PE12.4)
 3800 FORMAT(10X,'SHELL ',I10,'  SIGP2=',1PE12.4,'  SIGDT2=',1PE12.4,'  SIG_DTF2=',1PE12.4)
 4000 FORMAT(1X, 'FAILURE ADVANCEMENT IN SHELL',I10,1X,' 1ST DIR, LAYER',I2,1X,'INT POINT',I2)
 4100 FORMAT(1X, 'FAILURE ADVANCEMENT IN SHELL',I10,1X,' 1ST DIR, LAYER',I2,1X,'INT POINT',I2,
     .       1X, 'AT TIME ',1PE12.4)
 5000 FORMAT(1X, 'FAILURE IN SHELL',I10,1X,' 2ND DIR, LAYER',I2,1X,'INT POINT',I2)
 5100 FORMAT(1X, 'FAILURE IN SHELL',I10,1X,' 2ND DIR, LAYER',I2,1X,'INT POINT',I2,
     .       1X, 'AT TIME ',1PE12.4)
c-----------------------------------------------
      RETURN
      END
