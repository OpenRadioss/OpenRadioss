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
Chd|  FAIL_WIND_XFEM                source/materials/fail/alter/fail_wind_xfem.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_WIND_XFEM(
     1     NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      ,
     2     TIME      ,TIMESTEP  ,SSP       ,ALDT      ,CRKLEN    ,
     3     ELCRKINI  ,TDEL      ,OFF       ,OFFLY     ,
     4     SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     5     NGL       ,IXEL      ,ILAY      ,IPT       ,NPT       ,
     6     IXFEM     ,IORTH     ,DIR_A     ,DIR1_CRK  ,DIR2_CRK  ,
     7     CRKDIR     )
C-----------------------------------------------
c    Windshield failure model for XFEM  (ref : PhD thesis Christian Alter 2018)
c    now : only for monolayer xfem 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
#include "com_xfem1.inc"
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
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,IXFEM,IORTH,IXEL,ILAY,IPT,NPT
      INTEGER ELCRKINI(NXLAYMAX,NEL),NGL(NEL)
      my_real TIME,TIMESTEP
      my_real, DIMENSION(NEL) :: SSP,ALDT,TDEL,CRKLEN,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      my_real UPARAM(NUPARAM),DIR_A(NEL,2),CRKDIR(NEL,2),DIR1_CRK(NXLAYMAX,NEL),
     .   DIR2_CRK(NXLAYMAX,NEL)
      TARGET :: DIR1_CRK,DIR2_CRK
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER OFFLY(NEL)
      my_real UVAR(NEL,NUVAR),OFF(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,NINDXD,IDEB,ISRATE
      INTEGER , DIMENSION(NEL) :: INDX,INDXD,RFLAG
      my_real SIGMAX,AA,BB,CC,S1,S2,S3,CR,K_IC,K_TH,V0,VC,TFACT,REFLEN,
     .   COSI,COSX,COSY,SINX,SINY,COS2,SIN2,DMG1,DMG3,
     .   SP1,SP2,SP3,GEORED,CR_FOIL,CR_AIR,CR_CORE,CR_EDGE,ALPHA,ALPHAI,EXP_N,EXP_M
      my_real, DIMENSION(NEL) :: DADV,TPROPG,AI,FORMF,SIGP_AKT,DMG_SCALE,
     .   SIGP1,SIGP2,SIGDT1,SIGDT2,SIG_DTF1,SIG_DTF2,SIGP_MIN,SIGP_MAX
      my_real, DIMENSION(:), POINTER :: DIR1,DIR2
      CHARACTER (LEN=3) :: XCHAR
c--------------------------------------------------------------------
c!  cm(6):  initial crack depth in contact to foil
c!  cm(7):  initial crack depth in contact to air
c!  cm(8):  initial crack depth on edge
c!  cm(9):  K_IC - fracture toughness from literature
c!  cm(10): K_TH - fatigue threshold from literature
c--------------------------------------------------------------------
c      UVAR(1) = SIGP1       ! principal stress DIR1                ! uvar 17
c      UVAR(1) = SIGP2       ! principal stress DIR2                ! uvar 18
c      UVAR(3) = SIGDT1      ! principal stress rate DIR1
c      UVAR(4) = SIGDT2      ! principal stress rate DIR2
c      UVAR(5) = SIGP_MIN                                           ! uvar 24
c      UVAR(6) = SIGP_MAX                                           ! uvar 14
c      UVAR(7) = FORMF                                              ! uvar 25
c      UVAR(8) = ai                                                 ! uvar 26
c      UVAR(9) = sigp_akt                                          
c      UVAR(10) = edge element flag
c      UVAR(11) = reduction factor flag
c      UVAR(12) = DADV
c      UVAR(13) = SIG_DTF1
c      UVAR(14) = SIG_DTF2
C=======================================================================
      XCHAR  = ' '      
      IF (IXEL > 0) THEN  ! testing phantom elements
        IF (IXEL == 1) THEN
          XCHAR = '1st'
        ELSEIF (IXEL == 2) THEN
          XCHAR = '2nd'
        ELSEIF (IXEL == 3) THEN
          XCHAR = '3rd'
        ENDIF
      ELSE
        XCHAR = 'standard'
      ENDIF
c
      RFLAG(1:NEL) = 0
      INDX(1:NEL)  = 0
      NINDX  = 0  
      NINDXD = 0                                     
c--------------------------------------------------------------------
      EXP_N   = UPARAM(1)
      CR_FOIL = UPARAM(2)
      CR_AIR  = UPARAM(3)
      CR_CORE = UPARAM(4)
      CR_EDGE = UPARAM(5)
      K_IC    = UPARAM(6)
      K_TH    = UPARAM(7)
      V0      = UPARAM(8)
      VC      = UPARAM(9)
      ALPHA   = UPARAM(10)
      GEORED  = UPARAM(11)
      REFLEN  = NINT(UPARAM(14))
      ISRATE  = NINT(UPARAM(16))
      IDEB    = NINT(UPARAM(17))
c
      ALPHAI  = ONE - ALPHA
      EXP_M   = ONE / (ONE + EXP_N) 
c---
c      IF (IXEL == 0) THEN
        DO I = 1,NEL                                   
          DADV(I)   = MIN(ONE, GEORED / SQRT(ALDT(I)/REFLEN) ) ! reduction factor for advancement
          TPROPG(I) = CRKLEN(I) / MIN(VC,SSP(I))       
          TPROPG(I) = MAX(TPROPG(I), TIMESTEP)     
          UVAR(I,12)= DADV(I)
        END DO                                         
c      ENDIF
c--------------------------------------------------------------------
      IF (TIME == ZERO) then
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
          SIGP_AKT(I) = (TWO*(EXP_N + ONE)*K_IC**EXP_N) /
     .              ((EXP_N - TWO)*V0*(FORMF(I)*SQRT(PI))**EXP_N*
     .               AI(I)**((EXP_N-TWO)/TWO))
          SIGP_AKT(I) = SIGP_AKT(I)**EXP_M
c
          SIGP_MIN(I) = K_TH / (FORMF(I)*SQRT(PI*AI(I)))
          SIGP_MAX(I) = K_IC / (FORMF(I)*SQRT(PI*AI(I)))
          UVAR(I,5) = SIGP_MIN(I)
          UVAR(I,6) = SIGP_MAX(I)
          UVAR(I,7) = FORMF(I)
          UVAR(I,8) = AI(I)
          UVAR(I,9) = SIGP_AKT(I)
        ENDDO
      ELSE
        SIGP_MIN(1:NEL) = UVAR(1:NEL,5)
        SIGP_MAX(1:NEL) = UVAR(1:NEL,6)
        FORMF(1:NEL)    = UVAR(1:NEL,7)
        AI(1:NEL)       = UVAR(1:NEL,8)
        SIGP_AKT(1:NEL) = UVAR(1:NEL,9)
      ENDIF
c--------------------------------------------------------------------
c     Principal stress
c--------------------------------------------------------------------
      DO I = 1,NEL
        AA = (SIGNXX(I) + SIGNYY(I))*HALF
        BB = (SIGNXX(I) - SIGNYY(I))*HALF
        CC = SIGNXY(I)
        CR = SQRT(BB**2 + CC**2)
        S1 = AA + CR
        S2 = AA - CR       
        SIGP1(I) = S1
        SIGP2(I) = S2      
c
c        IF (S12 == ZERO .and. S2 >= ZERO) THEN
c          MAJANG = ZERO
c        ELSEIF (S12 == ZERO .and. S2 < ZERO) THEN
c          MAJANG = HALF*PI
c        ELSE
c          MAJANG = ATAN((SIGP1(I) - SIGNXX(I)) / S12)
c        ENDIF
      END DO
c--------------------------------------------------------------------
c     Calculate stress rate + filtering
c--------------------------------------------------------------------
      IF (ISRATE == 0) THEN
c       exponential moving average with smoohting coefficient = alpha 
        DO I = 1,NEL
          SIGDT1(I) = ABS(SIGP1(I) - UVAR(I,1)) / MAX(TIMESTEP, EM20)
          SIGDT2(I) = ABS(SIGP2(I) - UVAR(I,2)) / MAX(TIMESTEP, EM20)
          SIGDT1(I) = ALPHA * SIGDT1(I) + ALPHAI * UVAR(I,3)
          SIGDT2(I) = ALPHA * SIGDT2(I) + ALPHAI * UVAR(I,4)
        END DO     
      ELSE
c       arythmetic moving average over 50 cycles
        DO I = 1,NEL
          IF (OFF(I) == ONE) THEN
            SIGDT1(I)  = ABS(SIGP1(I) - UVAR(I,1)) / MAX(TIMESTEP, EM20)
            UVAR(I,21) = SIGDT1(I) 
            DO J = 1,49
              SIGDT1(I) = SIGDT1(I) + UVAR(I,21+J) 
            ENDDO
            SIGDT1(I) = SIGDT1(I) / 50
            DO J = 70,22,-1
              UVAR(I,J) = UVAR(I,J-1) 
            ENDDO
c
            SIGDT2(I)  = ABS(SIGP2(I) - UVAR(I,2)) / MAX(TIMESTEP, EM20)
            UVAR(I,71) = SIGDT2(I) 
            DO J = 1,49
              SIGDT2(I) = SIGDT2(I) + UVAR(I,71+J) 
            ENDDO
            SIGDT2(I) = SIGDT2(I) / 50
            DO J = 120,72,-1
              UVAR(I,J) = UVAR(I,J-1) 
            ENDDO
          ELSE
            SIGDT1(I) = ZERO
            SIGDT2(I) = ZERO
          ENDIF        
        END DO     
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
          ELSE
            SIG_DTF1(I) = SIGP_MAX(I)
            SIG_DTF2(I) = SIGP_MAX(I)
          ENDIF
        END DO
      ENDIF      
c--------------------------
      IF (IXEL == 0) THEN    ! original element            
        DO I = 1,NEL                                                 
          IF (OFF(I) == ONE) THEN                              
            IF (ELCRKINI(ILAY,I) == 0) THEN   
              ! no cracks in the neibourhood => initialization
              IF (SIGP1(I) > SIG_DTF1(I) .and. TDEL(I) == ZERO .and. OFFLY(I) == 1) THEN
                ! start damage
                TDEL(I)  = TIME
                OFFLY(I) = 0
                ELCRKINI(ILAY,I) = -5   ! for crack length estimation
                NINDX = NINDX+1                                         
                INDX(NINDX) = I                                         
                RFLAG(I) = 1
              ENDIF
              IF (TDEL(I) > 0) THEN ! damage started already
                TFACT = (TIME - TDEL(I)) / TPROPG(I)
                TFACT = MIN(ONE, TFACT)                                     
                DMG_SCALE(I) = ONE - TFACT
                NINDXD = NINDXD+1                                         
                INDXD(NINDXD) = I                                   
                IF (TFACT >= ONE) THEN     ! end of damage in normal direction
                  ELCRKINI(ILAY,I) = -1   ! failure completed (by initiation)
                  OFF(I) = FOUR_OVER_5                                           
                  IF (IORTH > 0) THEN
                    COSX = DIR_A(I,1)
                    SINX = DIR_A(I,2)
                    COSY = CRKDIR(I,1)                   
                    SINY = CRKDIR(I,2)                     
                    DIR1_CRK(ILAY,I) = COSX*COSY - SINX*SINY
                    DIR2_CRK(ILAY,I) = COSX*SINY + SINX*COSY
                  ELSE
                    DIR1_CRK(ILAY,I) = CRKDIR(I,1) 
                    DIR2_CRK(ILAY,I) = CRKDIR(I,2) 
                  ENDIF
                ELSE
                ENDIF
              ENDIF
c
            ELSEIF (ELCRKINI(ILAY,I) == 2) THEN  
              ! neighbor element cut at common edge => advancement
              UVAR(I,11)  = ONE  ! reduction factor flag for output
              SIG_DTF1(I) = SIG_DTF1(I)*DADV(I)  ! apply reduction factor to criteria
              SIG_DTF2(I) = SIG_DTF2(I)*DADV(I)            
              SIG_DTF1(I) = MAX(SIG_DTF1(I),SIGP_MIN(I)*DADV(I))
              SIG_DTF1(I) = MIN(SIG_DTF1(I),SIGP_MAX(I)*DADV(I))
              SIG_DTF2(I) = MAX(SIG_DTF2(I),SIGP_MIN(I)*DADV(I))
              SIG_DTF2(I) = MIN(SIG_DTF2(I),SIGP_MAX(I)*DADV(I))
              IF (SIGP1(I) > SIG_DTF1(I) .and. TDEL(I) == ZERO .and. OFFLY(I) == 1) THEN
                TDEL(I)  = TIME
                OFFLY(I) = 0
                ELCRKINI(ILAY,I) = 5  ! tag for crack length estimation
                NINDX = NINDX+1                                         
                INDX(NINDX) = I                                         
                RFLAG(I)  =-1
              ENDIF
              IF (OFFLY(I) == 0) THEN ! damage started already
                TFACT = (TIME - TDEL(I)) / TPROPG(I)
                NINDXD = NINDXD+1                                         
                INDXD(NINDXD) = I                                   
                IF (TFACT >= ONE) THEN
                  ELCRKINI(ILAY,I) = 1      ! advance existing crack      
                  DMG_SCALE(I) = ZERO
                  OFF(I) = FOUR_OVER_5                                           
                  IF (IORTH > 0) THEN
                    COSX = DIR_A(I,1)
                    SINX = DIR_A(I,2)
                    COSY = CRKDIR(I,1)                   
                    SINY = CRKDIR(I,2)                     
                    DIR1_CRK(ILAY,I) = COSX*COSY - SINX*SINY
                    DIR2_CRK(ILAY,I) = COSX*SINY + SINX*COSY
                  ELSE
                    DIR1_CRK(ILAY,I) = CRKDIR(I,1) 
                    DIR2_CRK(ILAY,I) = CRKDIR(I,2) 
                  ENDIF
                ELSE
                  DMG_SCALE(I) = ONE - TFACT
                ENDIF
              ENDIF                                                   
            ENDIF
          ENDIF    ! OFF == 1
        ENDDO
c--------------------------
c       calculate and save principal stress direction when first failure occurs
c--------------------------
        IF (NINDX > 0) THEN
          DO J=1,NINDX
            I  = INDX(J)
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
c
          IF (IORTH > 0) THEN   ! stress is in orthotropic frame (DIR_A)
            DO J=1,NINDX
              I  = INDX(J)
              COSX = DIR_A(I,1)
              SINX = DIR_A(I,2)
              COSY = CRKDIR(I,1)                             
              SINY = CRKDIR(I,2)                             
              DIR1_CRK(ILAY,I) = COSX*COSY - SINX*SINY
              DIR2_CRK(ILAY,I) = COSX*SINY + SINX*COSY
            ENDDO
          ELSE                ! stress is in local element frame
            DO J=1,NINDX
              I  = INDX(J)
              DIR1_CRK(ILAY,I) = CRKDIR(I,1)  
              DIR2_CRK(ILAY,I) = CRKDIR(I,2)  
            ENDDO
          ENDIF
        ENDIF
c
c       apply progressive damage and turn the stress tensor back to the local system
c
        IF (NINDXD > 0) THEN
          DO J=1,NINDXD                                   
            I = INDXD(J)                                  
            S1 = SIGNXX(I)
            S2 = SIGNYY(I)
            S3 = SIGNXY(I)
            COSX = CRKDIR(I,1)                            
            SINX = CRKDIR(I,2)                            
            COS2 = COSX * COSX
            SIN2 = SINX * SINX
            COSI = COSX * SINX
c           rotate stress to previously saved crack direction            
            SP1 = COS2*S1 + SIN2*S2 + TWO*COSI*S3
            SP2 = SIN2*S1 + COS2*S2 - TWO*COSI*S3
            SP3 = COSI*(S2 - S1) + (COS2 - SIN2)*S3
          
            DMG1 = DMG_SCALE(I)
            DMG3 = MIN(ONE, 0.6 + 0.4 * DMG1)
c           stress reduction               
            SP1 = SP1 * DMG1
            SP3 = SP3 * DMG3
c           rotate reduced stress back to current element coordinate system          
            SIGNXX(I) = COS2*SP1 + SIN2*SP2 - TWO*COSI*SP3  
            SIGNYY(I) = SIN2*SP1 + COS2*SP2 + TWO*COSI*SP3  
            SIGNXY(I) = COSI*(SP1 - SP2) + (COS2 - SIN2)*SP3
          ENDDO                                           
c
          if (ideb==1) then
            DO J=1,NINDXD
              I = INDXD(J)
               write(IOUT,'(A,I10,3E16.9)') 'DAMAGE ELEMENT N, CRLEN,TPROPG,DMG_SCALE=',
     .         NGL(I),CRKLEN(I),TPROPG(I),DMG_SCALE(I)
            ENDDO
          endif
        ENDIF
c
      ELSEIF (IXEL > 0) THEN       ! phantom element                                      
        DO I = 1,NEL                                                 
          IF (OFF(I) == ONE) THEN                                     
            IF (ELCRKINI(ILAY,I) == 2) THEN  ! second crack arrived => delete phantom element 
              NINDX = NINDX+1                                          
              INDX(NINDX) = I                                          
              RFLAG(I) = 3
              OFF(I) = FOUR_OVER_5                                           
            ELSE
              SIGP_MIN(I) = K_TH / (SQRT(PI*CR_FOIL))
              SIGP_MAX(I) = K_IC / (SQRT(PI*CR_FOIL))
              SIG_DTF1(I) = SIGP_MAX(I) * DADV(I)
              IF (SIGP1(I) > SIG_DTF1(I)) THEN  
                 ! phantom element reaches criteria
c                delete phantom but should transfer information to remaining neighbor element         
                 NINDX = NINDX+1                                          
                 INDX(NINDX) = I                                          
                 RFLAG(I) = 3
                 OFF(I) = FOUR_OVER_5                                           
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF                                                       
c-----------------------------------------------
      DO I=1,NEL
        UVAR(I,1) = SIGP1(I)
        UVAR(I,2) = SIGP2(I)
        UVAR(I,3) = SIGDT1(I)
        UVAR(I,4) = SIGDT2(I)
      END DO     
c-----------------------------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 3000)NGL(I),ILAY,IPT                                    
          WRITE(ISTDO,3100)NGL(I),ILAY,IPT,TIME                               
c         initialization std element                                     
          IF (RFLAG(I) == 1)  WRITE(IOUT ,4000)               
          IF (RFLAG(I) == 1)  WRITE(ISTDO,4000)       
c         advancement std element                                        
          IF (RFLAG(I) == -1) WRITE(IOUT, 4200)             
          IF (RFLAG(I) == -1) WRITE(ISTDO,4200)             
            if (ideb==1 .and. abs(RFLAG(I))==1) then
               write(IOUT,'(A,5E16.9)') 'SIGP1,SIG_DTF1,SIGP_AKT,SIGP_MAX,SIGDT1=',
     .           SIGP1(I),SIG_DTF1(I),SIGP_AKT(I),SIGP_MAX(I),SIGDT1(I)
               write(IOUT,'(A,5E16.9)') 'SIGP2,SIG_DTF2,SIGP_AKT,SIGP_MIN,SIGDT2=',
     .           SIGP2(I),SIG_DTF2(I),SIGP_AKT(I),SIGP_MIN(I),SIGDT2(I)
              if (RFLAG(I)==-1) then
               write(IOUT,'(A,E16.9)') 'advancement => crit reduction, DADV =',DADV(I)
              endif
            endif
c         delete phantom                                                 
          IF (RFLAG(I) == 3)  WRITE(IOUT, 4400) XCHAR,NGL(I)             
          IF (RFLAG(I) == 3)  WRITE(ISTDO,4500) XCHAR,NGL(I),TIME        
#include "lockoff.inc"
        ENDDO        
      ENDIF          
c-----------------------------------------------
 3000 FORMAT(1X,'FAILURE IN SHELL',I10,1X,'LAYER',I2,1X,'INT POINT',I2)
 3100 FORMAT(1X,'FAILURE IN SHELL',I10,1X,'LAYER',I2,1X,'INT POINT',I2,
     .       1X,'AT TIME ',1PE12.4)
 4000 FORMAT(10X,'CRACK INITIALIZATION')
 4200 FORMAT(10X,'CRACK ADVANCEMENT')
 4400 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10)
 4500 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,/
     .       1X,'AT TIME :',1PE12.4)
c-----------------------------------------------
      RETURN
      END
