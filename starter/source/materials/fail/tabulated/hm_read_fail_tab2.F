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
!||    hm_read_fail_tab2        ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_TAB2(FAIL ,
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,TITR     ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN)       :: FAIL_ID       ! failure model ID
      INTEGER            ,INTENT(IN)       :: MAT_ID        ! material law ID
      INTEGER            ,INTENT(IN)       :: IRUPT         ! failure model number
      CHARACTER(LEN=NCHARTITLE),INTENT(IN) :: TITR
      TYPE(UNIT_TYPE_)   ,INTENT(IN)       :: UNITAB        ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN)       :: LSUBMODEL(*)
      TYPE (FAIL_PARAM_) ,INTENT(INOUT)    :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: ITAB_EPSF,FAILIP,ITAB_INST,
     .            IFUN_EXP,ITAB_SIZE,IREG,IFUN_RATE,IFUN_DLIM,IFUN_TEMP  
      my_real  :: FCRIT,PTHKFAIL,DN,DCRIT,ECRIT,EXP_REF,EXPO,EL_REF,SR_REF1,
     .            FSCALE_EL,SHRF,BIAXF,SR_REF2,FSCALE_SR,CJC,FSCALE_DLIM,
     .            LENGTH_UNIT,RATE_UNIT,TEMP_REF,FSCALE_TEMP,TEMP_UNIT,VOLFRAC
C-----------------------------------------------
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT INPUT DATA
C--------------------------------------------------
Card1
      CALL HM_GET_INTV   ('EPSF_ID'     ,ITAB_EPSF   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('FCRIT'       ,FCRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('FAILIP'      ,FAILIP      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('PTHK'        ,PTHKFAIL    ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV ('VOLFRAC'     ,VOLFRAC     ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
Card2 
      CALL HM_GET_FLOATV ('N'           ,DN          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('DCRIT'       ,DCRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('INST_ID'     ,ITAB_INST   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('ECRIT'       ,ECRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
Card3
      CALL HM_GET_INTV   ('FCT_EXP'     ,IFUN_EXP    ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_FLOATV ('EXP_REF'     ,EXP_REF     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('EXP'         ,EXPO        ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_INTV   ('FCT_TEMP'    ,IFUN_TEMP   ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_FLOATV ('TEMP_REF'    ,TEMP_REF    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FSCALE_TEMP' ,FSCALE_TEMP ,IS_AVAILABLE,LSUBMODEL,UNITAB)   
Card4
      CALL HM_GET_INTV   ('TAB_EL'      ,ITAB_SIZE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('IREG'        ,IREG        ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('EL_REF'      ,EL_REF      ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV ('SR_REF1'     ,SR_REF1     ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV ('FSCALE_EL'   ,FSCALE_EL   ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
Card5
      CALL HM_GET_FLOATV ('SHRF'        ,SHRF        ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV ('BIAXF'       ,BIAXF       ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
Card6
      CALL HM_GET_INTV   ('FCT_SR'      ,IFUN_RATE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('SR_REF2'     ,SR_REF2     ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV ('FSCALE_SR'   ,FSCALE_SR   ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('CJC'         ,CJC         ,IS_AVAILABLE,LSUBMODEL,UNITAB)   
Card7
      CALL HM_GET_INTV   ('FCT_DLIM'    ,IFUN_DLIM   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('FSCALE_DLIM' ,FSCALE_DLIM ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
C--------------------------------------------------------------------------------
C     Setting default values 
C--------------------------------------------------------------------------------
      IF (ITAB_EPSF == 0 .AND. FCRIT == ZERO) THEN 
        CALL ANCMSG(MSGID=3000, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND,
     .              I1=MAT_ID,
     .              C1=TITR)        
      ENDIF
      IF (FCRIT == ZERO) FCRIT  = ONE
      IF (FAILIP == 0)   FAILIP = 1
      IF (DN <= ZERO)    DN     = ONE
      IF (DCRIT > ONE)   DCRIT  = ONE
      IF (DCRIT < ZERO)  DCRIT  = ZERO
      IF ((ITAB_INST > 0).AND.(ECRIT   == ZERO)) ECRIT = ONE
      IF ((IFUN_EXP  > 0).AND.(EXP_REF == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('EXP_REF' ,LENGTH_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        EXP_REF = ONE*LENGTH_UNIT
      ENDIF    
      IF (EXPO == ZERO) EXPO = ONE   
      IF (TEMP_REF == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('TEMP_REF' ,TEMP_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        TEMP_REF = ONE*TEMP_UNIT
      ENDIF       
      IF (FSCALE_TEMP == ZERO) FSCALE_TEMP = ONE  
      IF (IREG == 0)    IREG = 1
      IF ((ITAB_SIZE > 0).AND.(EL_REF == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('EL_REF' ,LENGTH_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        EL_REF = ONE*LENGTH_UNIT
      ENDIF
      IF ((IREG == 1).AND.(ITAB_SIZE > 0).AND.(SR_REF1 == ZERO)) THEN 
        CALL HM_GET_FLOATV_DIM('SR_REF1' ,RATE_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SR_REF1 = ONE*RATE_UNIT
      ENDIF
      IF (FSCALE_EL == ZERO) FSCALE_EL = ONE
      IF (SHRF  == ZERO)     SHRF      = -ONE
      IF (BIAXF == ZERO)     BIAXF     = ONE
      IF (((IFUN_RATE > 0).OR.(CJC > ZERO)).AND.(SR_REF2 == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('SR_REF2' ,RATE_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SR_REF2 = ONE*RATE_UNIT
      ENDIF
      IF (FSCALE_SR == ZERO)   FSCALE_SR   = ONE
      IF (FSCALE_DLIM == ZERO) FSCALE_DLIM = ONE
      IF (PTHKFAIL == ZERO) PTHKFAIL = EM06
      PTHKFAIL = MIN(PTHKFAIL, ONE)
      PTHKFAIL = MAX(PTHKFAIL,-ONE)

      VOLFRAC = MIN(VOLFRAC,ONE)
      VOLFRAC = MAX(VOLFRAC,ZERO)

      IF (VOLFRAC > ZERO) THEN  
        CALL ANCMSG(MSGID=3080, MSGTYPE=MSGINFO, ANMODE=ANINFO_BLIND,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF    

C--------------------------------------------------------------------------------
C     Filling buffer tables
C--------------------------------------------------------------------------------
      FAIL%KEYWORD = 'TAB2' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 23
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 3
      FAIL%NFUNC   = 4
      FAIL%NTABLE  = 3
      FAIL%NMOD    = 0
      FAIL%PTHK    = PTHKFAIL
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      FAIL%UPARAM(1)  = FCRIT
      FAIL%UPARAM(2)  = FAILIP
      FAIL%UPARAM(3)  = PTHKFAIL
      FAIL%UPARAM(4)  = DN
      FAIL%UPARAM(5)  = DCRIT
      FAIL%UPARAM(6)  = ECRIT
      FAIL%UPARAM(7)  = EXP_REF
      FAIL%UPARAM(8)  = EXPO
      FAIL%UPARAM(9)  = IREG
      FAIL%UPARAM(10) = EL_REF
      FAIL%UPARAM(11) = SR_REF1
      FAIL%UPARAM(12) = FSCALE_EL
      FAIL%UPARAM(13) = SHRF
      FAIL%UPARAM(14) = BIAXF
      FAIL%UPARAM(15) = SR_REF2
      FAIL%UPARAM(16) = FSCALE_SR
      FAIL%UPARAM(17) = CJC
      FAIL%UPARAM(18) = FSCALE_DLIM 
      FAIL%UPARAM(19) = TEMP_REF
      FAIL%UPARAM(20) = FSCALE_TEMP
      FAIL%UPARAM(21) = 0             ! Flag 1 for logarithmic scale in strain rate dependency
      FAIL%UPARAM(22) = 0             ! Flag 2 for logarithmic scale in strain rate dependency
      FAIL%UPARAM(23) = VOLFRAC     
c
      FAIL%IFUNC(1) = IFUN_EXP
      FAIL%IFUNC(2) = IFUN_RATE
      FAIL%IFUNC(3) = IFUN_DLIM
      FAIL%IFUNC(4) = IFUN_TEMP
c
      FAIL%TABLE(1) = ITAB_EPSF
      FAIL%TABLE(2) = ITAB_INST
      FAIL%TABLE(3) = ITAB_SIZE
c-----------------------------------------------------------------------
      IF (IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        ! Title 
        WRITE(IOUT,1000)
        ! Failure definition
        IF (ITAB_EPSF > 0) THEN 
          WRITE(IOUT,1001) ITAB_EPSF,FCRIT
        ELSE
          WRITE(IOUT,1002) FCRIT 
        ENDIF
        WRITE(IOUT,1003) DN
        ! Necking control definition
        WRITE(IOUT,1004)
        IF (ITAB_INST > 0) THEN 
          WRITE(IOUT,1005) ITAB_INST,ECRIT
        ELSEIF (ECRIT > ZERO) THEN 
          WRITE(IOUT,1006) ECRIT
        ELSE
          WRITE(IOUT,1007) DCRIT
        ENDIF
        ! Fading exponent definition
        WRITE(IOUT,1008)
        IF (IFUN_EXP > 0) THEN 
          WRITE(IOUT,1009) IFUN_EXP,EXP_REF,EXPO
        ELSE 
          WRITE(IOUT,1010) EXPO
        ENDIF
        ! Temperature scaling definition
        IF (IFUN_TEMP > 0) THEN 
          WRITE(IOUT,1020)
          WRITE(IOUT,1021) IFUN_TEMP,TEMP_REF,FSCALE_TEMP
        ENDIF        
        ! Element size scaling definition
        IF (ITAB_SIZE > 0) THEN 
          WRITE(IOUT,1011) IREG
          IF (IREG == 1) THEN 
            WRITE(IOUT,1012) ITAB_SIZE,EL_REF,SR_REF1,FSCALE_EL,SHRF,BIAXF
          ELSEIF(IREG == 2) THEN 
            WRITE(IOUT,1013) ITAB_SIZE,EL_REF,FSCALE_EL
          ENDIF
        ENDIF
        ! Strain rate scaling definition
        IF (IFUN_RATE > 0) THEN 
          WRITE(IOUT,1014)
          WRITE(IOUT,1015) IFUN_RATE,SR_REF2,FSCALE_SR
        ELSEIF (CJC > ZERO) THEN 
          WRITE(IOUT,1014)
          WRITE(IOUT,1016) SR_REF2,CJC
        ENDIF
        ! Damage limit definition
        IF (IFUN_DLIM > 0) THEN 
          WRITE(IOUT,1017)
          WRITE(IOUT,1018) IFUN_DLIM,FSCALE_DLIM
        ENDIF 
        ! Element deletion control parameters
        WRITE(IOUT,1019) FAILIP,PTHKFAIL,VOLFRAC
        WRITE(IOUT,1022)
      ENDIF  ! IS_ENCRYPTED             
c-----------------------------------------------------------------------
 1000 FORMAT(
     & 5X,' --------------------------------------------------------',/,
     & 5X,'          TABULATED FAILURE CRITERION VERSION 2          ',/,
     & 5X,' --------------------------------------------------------',/,
     & 5X,'                                                         ',/,
     & 5X,'FAILURE DEFINITION:                                      ',/,
     & 5X,'-------------------                                      ',/)
 1001 FORMAT(
     & 5X,'FAILURE PLASTIC STRAIN TABLE ID . . . . . . . . . . . . =',I10/,
     & 5X,'    SCALE FACTOR FOR FAILURE PLASTIC STRAIN TABLE . . . =',1PG20.13/)
 1002 FORMAT(
     & 5X,'CRITICAL FAILURE PLASTIC STRAIN FCRIT . . . . . . . . . =',1PG20.13/)
 1003 FORMAT(
     & 5X,'DAMAGE EVOLUTION EXPONENT N . . . . . . . . . . . . . . =',1PG20.13/)
 1004 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'NECKING CONTROL DEFINITION:                              ',/,
     & 5X,'---------------------------                              ',/) 
 1005 FORMAT(
     & 5X,'NECKING PLASTIC STRAIN TABLE ID . . . . . . . . . . . . =',I10/,
     & 5X,'    SCALE FACTOR FOR NECKING PLASTIC STRAIN . . . . . . =',1PG20.13/)
 1006 FORMAT(
     & 5X,'CRITICAL NECKING PLASTIC STRAIN VALUE ECRIT . . . . . . =',1PG20.13/)
 1007 FORMAT(
     & 5X,'CRITICAL DAMAGE VALUE DCRIT . . . . . . . . . . . . . . =',1PG20.13/)
 1008 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'STRESS SOFTENING EXPONENT DEFINITION:                    ',/,
     & 5X,'-------------------------------------                    ',/) 
 1009 FORMAT(
     & 5X,'SOFTENING EXPONENT FUNCTION ID. . . . . . . . . . . . . =',I10/,
     & 5X,'    REFERENCE ELEMENT LENGTH. . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'    SCALE FACTOR FOR SOFTENING EXPONENT FUNCTION  . . . =',1PG20.13/)
 1010 FORMAT(
     & 5X,'STRESS SOFTENING EXPONENT EXPO. . . . . . . . . . . . . =',1PG20.13/)
 1011 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'ELEMENT SIZE SCALING DEFINITION:                         ',/,
     & 5X,'--------------------------------                         ',/, 
     & 5X,'ELEMENT SIZE SCALING FLAG . . . . . . . . . . . . . . . =',I10/,
     & 5X,'     IREG = 1: FACTOR VS EL. LENGTH (VS STRAIN RATE)     ',/,
     & 5X,'     IREG = 2: FACTOR VS EL. LENGTH (VS TRIAXIALITY)     ',/) 
 1012 FORMAT(
     & 5X,'ELEMENT SIZE SCALING TABLE ID . . . . . . . . . . . . . =',I10/,
     & 5X,'    REFERENCE ELEMENT LENGTH. . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'    REFERENCE STRAIN RATE . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'    SCALE FACTOR FOR SIZE SCALING . . . . . . . . . . . =',1PG20.13/,
     & 5X,'TRIAXIALITY LOWER BOUNDARY FOR SIZE SCALING . . . . . . =',1PG20.13/,
     & 5X,'TRIAXIALITY UPPER BOUNDARY FOR SIZE SCALING . . . . . . =',1PG20.13/)
 1013 FORMAT(
     & 5X,'ELEMENT SIZE SCALING TABLE ID . . . . . . . . . . . . . =',I10/,
     & 5X,'    SCALE FACTOR FOR SIZE SCALING . . . . . . . . . . . =',1PG20.13/,
     & 5X,'    REFERENCE ELEMENT LENGTH. . . . . . . . . . . . . . =',1PG20.13/)
 1014 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'STRAIN RATE SCALING DEFINITION:                          ',/,
     & 5X,'-------------------------------                          ',/) 
 1015 FORMAT(
     & 5X,'STRAIN RATE SCALING FUNCTION  . . . . . . . . . . . . . =',I10/,
     & 5X,'    REFERENCE STRAIN RATE . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'    STRAIN RATE DEPENDENCY SCALE FACTOR . . . . . . . . =',1PG20.13/)
 1016 FORMAT(
     & 5X,'JOHNSON-COOK STRAIN RATE SCALING                         ',/,
     & 5X,'    REFERENCE STRAIN RATE . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'    JOHNSON-COOK STRAIN RATE FACTOR . . . . . . . . . . =',1PG20.13/)
 1017 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'DAMAGE LIMIT DEFINITION:                                 ',/,
     & 5X,'------------------------                                 ',/) 
 1018 FORMAT(
     & 5X,'DAMAGE LIMIT FUNCTION . . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'    SCALE FACTOR FOR DAMAGE LIMIT FUNCTION  . . . . . . =',1PG20.13/)
 1019 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'ELEMENT DELETION PARAMETERS:                             ',/,
     & 5X,'----------------------------                             ',/,
     & 5X,'NUMBER OF FAILED INTG. POINTS PRIOR TO SOLID DELETION . =',I10/,
     & 5X,'SHELL ELEMENT DELETION PARAMETER PTHICKFAIL  . . . . . .=',1PG20.13,/,
     & 5X,'  > 0.0 : FRACTION OF FAILED THICKNESS                   ',/,
     & 5X,'  < 0.0 : FRACTION OF FAILED INTG. POINTS OR LAYERS      ',/,
     & 5X,'FAILED VOLUME FRACTION PRIOR TO SOLID DELETION . . . .  =',1PG20.13/) 
 1020 FORMAT(
     & 5X,'                                                         ',/,
     & 5X,'TEMPERATURE SCALING DEFINITION:                          ',/,
     & 5X,'-------------------------------                          ',/) 
 1021 FORMAT(
     & 5X,'TEMPERATURE SCALING FUNCTION  . . . . . . . . . . . . . =',I10/,
     & 5X,'    REFERENCE TEMPERATURE . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'    TEMPERATURE DEPENDENCY SCALE FACTOR . . . . . . . . =',1PG20.13/)
 1022 FORMAT(
     & 5X,' --------------------------------------------------------',/)   
c-----------------------------------------------------------------------
      END
