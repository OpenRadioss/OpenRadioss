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
Chd|  HM_READ_FAIL_ALTER            source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_ALTER(FAIL      ,FAIL_TAG  ,
     .           MAXFAIL  ,MAT_ID   ,FAIL_ID  ,IRUPT     ,
     .           IXFEM    ,IFAILWAVE,LSUBMODEL,UNITAB    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION : WINDSHIELD FAILURE MODEL (/FAIL/ALTER)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN) :: FAIL_ID         ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID          ! material law ID
      INTEGER            ,INTENT(IN) :: IRUPT           ! failure model type number
      INTEGER            ,INTENT(IN) :: MAXFAIL         ! fail model table size
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB          ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)    ! submodel table
      INTEGER            ,INTENT(INOUT) :: IXFEM        ! XFEM activation flag
      INTEGER            ,INTENT(INOUT) :: IFAILWAVE    ! fail wave propagation activation flag
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL         ! failure model data structure
      TYPE(FAIL_TAG_)    ,DIMENSION(0:MAXFAIL) ,INTENT(INOUT) :: FAIL_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NEMA,ELGR3N,ELGR4N,IDEB,IMOD,ISRATE,PERIOD,ISIDE,
     .   ITGLASS,PFLAG,NUVAR
      my_real :: EXP_N,CR_FOIL,CR_AIR,CR_CORE,CR_EDGE,K_IC,K_TH,V0,VC,
     .   ALPHA,GEORED,RLEN,FAC_L,TDELAY,KRES1,KRES2,
     .   ETA1,BETA1,TAU1,ETA2,BETA2,TAU2,A_REF,SIG_INI,PSCALE 
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
c------------------------------------------------
c  IMOD - choice of failure propagation model between neighbor elements
c  IMOD = 0 => no failure information propagation
c  IMOD = 1 => XFEM FAILURE PROPAGATION
c  IMOD = 2 => ISOTROPIC FRONTWAVE PROPAGATION 
c  IMOD = 3 => DIRECTIONAL FRONTWAVE PROPAGATION - edges only
c  IMOD = 4 => DIRECTIONAL FRONTWAVE PROPAGATION - edges and diagonals
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
c     Check crypting option
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
c--------------------------------------------------
c     Extract input Parameters
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV   ('Exp_n'    ,EXP_N    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('V0'       ,V0       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Vc'       ,VC       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV     ('EMA'      ,NEMA     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV     ('Irate'    ,ISRATE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV     ('Iside'    ,ISIDE    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV     ('mode'     ,IMOD     ,IS_AVAILABLE,LSUBMODEL)
card2
      CALL HM_GET_FLOATV   ('Cr_foil'  ,CR_FOIL  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Cr_air'   ,CR_AIR   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Cr_core'  ,CR_CORE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Cr_edge'  ,CR_EDGE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV     ('grsh4N'   ,ELGR4N   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV     ('grsh3N'   ,ELGR3N   ,IS_AVAILABLE,LSUBMODEL)
card3
      CALL HM_GET_FLOATV   ('KIC'      ,K_IC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('KTH'      ,K_TH     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Rlen'     ,RLEN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Tdel'     ,TDELAY   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV     ('OUT_FLAG' ,IDEB     ,IS_AVAILABLE,LSUBMODEL)
card4
      CALL HM_GET_FLOATV   ('Kres1'    ,KRES1    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Kres2'    ,KRES2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
card5 new input cards for Ch.Brokmann extension
      CALL HM_GET_FLOATV   ('Eta1'     ,ETA1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Beta1'    ,BETA1    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Tau1'     ,TAU1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('A_Ref'    ,A_REF    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card6
      CALL HM_GET_FLOATV   ('Eta2'     ,ETA2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Beta2'    ,BETA2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Tau2'     ,TAU2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card7
      CALL HM_GET_FLOATV   ('Sig_0'    ,SIG_INI  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV   ('Pscale'   ,PSCALE   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV     ('Pflag'    ,PFLAG    ,IS_AVAILABLE,LSUBMODEL)
c-------------------------------------------------------------------------------
c     DEFAULT VALUES
c--------------------------------------------------
c     GEORED should be initialized in fail_windshield_init depending of element type
      GEORED = ONE / SQRT(PI)  ! here adjusted for underintegrated 4N shells only
c
      IF (RLEN == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Rlen' ,FAC_L  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        RLEN = ONE*FAC_L
      ENDIF
c
      IF (IMOD >= 10) THEN
        IMOD  = IMOD-10
        IDEB  = 1
      END IF
      IF (IMOD == 1) THEN
        IXFEM = 1
      ELSE IF (IMOD > 1) THEN
        IXFEM = 0
        IFAILWAVE = IMOD-1
      ENDIF
c
c     flag for Ch. Brokmann extension
c
      IF (SIG_INI*ETA1*BETA1*TAU1 > ZERO) THEN
        ITGLASS = 1
        IF (ETA2  == ZERO) ETA2  = ETA1
        IF (BETA2 == ZERO) BETA2 = BETA1
        IF (TAU2  == ZERO) TAU2  = TAU1
      ELSE
        ITGLASS   = 0
      END IF
c--------------------------------------------------
      NUVAR  = 21
      IF (ITGLASS == 1) THEN
        ISRATE = 0
      ELSE IF (ISRATE == 1) THEN
        NUVAR = 130
      END IF
c
      IF (NEMA == 0) NEMA = 15
      IF (ISRATE == 1) THEN
        PERIOD = 50
      ELSE
        PERIOD = NEMA
      ENDIF
      ALPHA = TWO / (NEMA + 1)
c----------------------------------
      FAIL%KEYWORD = 'WINDSHIELD-ALTER' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 35
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = NUVAR
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      FAIL%UPARAM(1) = EXP_N   
      FAIL%UPARAM(2) = CR_FOIL 
      FAIL%UPARAM(3) = CR_AIR  
      FAIL%UPARAM(4) = CR_CORE 
      FAIL%UPARAM(5) = CR_EDGE 
      FAIL%UPARAM(6) = K_IC    
      FAIL%UPARAM(7) = K_TH    
      FAIL%UPARAM(8) = V0
      FAIL%UPARAM(9) = VC
      FAIL%UPARAM(10)= ALPHA
      FAIL%UPARAM(11)= GEORED 
      FAIL%UPARAM(12)= ELGR4N
      FAIL%UPARAM(13)= ELGR3N
      FAIL%UPARAM(14)= RLEN
      FAIL%UPARAM(15)= IMOD
      FAIL%UPARAM(16)= ISRATE
      FAIL%UPARAM(17)= IDEB
      FAIL%UPARAM(18)= ISIDE
      FAIL%UPARAM(19)= TDELAY
      FAIL%UPARAM(20)= KRES1
      FAIL%UPARAM(21)= KRES2
      FAIL%UPARAM(22)= ITGLASS
c
      FAIL%UPARAM(23)= A_REF  
      FAIL%UPARAM(24)= ETA1 
      FAIL%UPARAM(25)= BETA1  
      FAIL%UPARAM(26)= TAU1   
      FAIL%UPARAM(27)= ETA2   
      FAIL%UPARAM(28)= BETA2
      FAIL%UPARAM(29)= TAU2  
      FAIL%UPARAM(30)= SIG_INI 
      FAIL%UPARAM(31)= PSCALE
      FAIL%UPARAM(32)= PFLAG
      FAIL%UPARAM(33)= FAC_M_WORK
      FAIL%UPARAM(34)= FAC_L_WORK
      FAIL%UPARAM(35)= FAC_T_WORK
c---------------------------
      FAIL_TAG%LF_DIR = 2
      FAIL_TAG%LF_DAM = 1
c--------------------------------------------------
      WRITE(IOUT,1000)  
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT, 2000)
      ELSE
        WRITE(IOUT,3000) EXP_N,CR_FOIL,CR_AIR,CR_CORE,CR_EDGE,RLEN,
     .                   K_IC,K_TH,V0,VC,KRES1,KRES2,TDELAY,ELGR4N,ELGR3N,
     .                   ISRATE,PERIOD,ISIDE,IMOD,IDEB
        IF (ITGLASS == 1) THEN
          WRITE(IOUT,4000) ETA1,BETA1,TAU1,ETA2,BETA2,TAU2,SIG_INI,A_REF,
     .                   PSCALE,PFLAG
        END IF
      ENDIF
C-------------------------------------------------- 
 1000 FORMAT( 
     & 5X,'   WINDSHIELD FAILURE MODEL (Christian Alter)       ',/,
     & 5X,'  --------------------------------------------      ',/) 
 2000 FORMAT( 
     & 5X,'    CONFIDENTIAL DATA                          '/,
     & 5X,'    -----------------                          '/)
 3000 FORMAT( 
     & 5X,'CRACK GROW EXPONENT . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'FOIL SIDE CRACK DEPTH . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'AIR SIDE CRACK DEPTH. . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'CORE CRACK DEPTH. . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'EDGE ELEMENT CRACK DEPTH. . . . . . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE ELEMENT LENGTH. . . . . . . . . . . . . .=',E12.4/
     & 5X,'K_IC. . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'K_TH. . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'V_0 . . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'V_C . . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'RESIDUAL STRESS FACTOR IN DIR1. . . . . . . . . . .=',E12.4/ 
     & 5X,'RESIDUAL STRESS FACTOR IN DIR2. . . . . . . . . . .=',E12.4/ 
     & 5X,'TIME DELAY BEFORE ELEMENT SUPPRESSION . . . . . . .=',E12.4/ 
     & 5X,'EDGE 4N SHELL ELEMENT GROUP . . . . . . . . . . . .=',I10/
     & 5X,'EDGE 3N SHELL ELEMENT GROUP . . . . . . . . . . . .=',I10/
     & 5X,'STRESS RATE FILTERING OPTION. . . . . . . . . . . .=',I3/
     & 5X,'     = 0 => EXPONENTIAL SMOOTHING                   ',/
     & 5X,'     = 1 => LINEAR SMOOTHING, FIXED PERIOD = 50     ',/
     & 5X,'STRESS RATE FILTERING PERIOD (NUMBER OF  CYCLES). .=',I10/
     & 5X,'STRESS RATE DEPENDENCY FLAG FLAG (ISIDE) :  . . . .=',I3/
     & 5X,'     = 0 => AIR SIDE ONLY                           ',/
     & 5X,'     = 1 => AIR AND FOIL SIDE                       ',/
     & 5X,'FAILURE PROPAGATION FORMULATION FLAG (IMOD) :      =',I3/
     & 5X,'     = 0 => NO PROPAGATION                          ',/
     & 5X,'     = 1 => XFEM                                    ',/
     & 5X,'     = 2 => ISOTROPIC FRONTWAVE                     ',/
     & 5X,'     = 3 => DIRECTIONAL FRONTWAVE THROUGH EDGES     ',/
     & 5X,'     = 4 => DIRECTIONAL FRONTWAVE THROUGH DIAGONALS ',/
     & 5X,'EXTENDED OUTPUT INFORMATION . . . . . . . . . . . .=',I3/)
 4000 FORMAT( 
     & 5X,'   STOCHASTIC FAILURE MODEL (Christopher Brokmann)  ',/,
     & 5X,'ETA1. . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'BETA1 . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'TAU1. . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'ETA2. . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'BETA2 . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'TAU2. . . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'INITIAL SURFACE STRESS. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE ELEMENT SURFACE . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'P_SCALE . . . . . . . . . . . . . . . . . . . . . .=',E12.4/ 
     & 5X,'PFLAG . . . . . . . . . . . . . . . . . . . . . . .=',I3//)
C--------------------------------------------------------------------- 
      RETURN
      END
