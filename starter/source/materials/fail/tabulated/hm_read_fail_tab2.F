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
Chd|  HM_READ_FAIL_TAB2             source/materials/fail/tabulated/hm_read_fail_tab2.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_TAB2(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,MAT_ID ,FAIL_ID  ,
     .           TABLE    ,TITR     ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN)   :: UNITAB 
      INTEGER,INTENT(IN)             :: MAXUPARAM,MAXFUNC,FAIL_ID,MAT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT)          :: UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)            :: NUPARAM,NUVAR
      CHARACTER TITR*nchartitle
      TYPE(TTABLE) TABLE(NTABLE)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: ITAB_EPSF,INT_ITAB_EPSF,FAILIP,
     .            ITAB_INST,INT_ITAB_INST,
     .            IFUN_EXP,ITAB_SIZE,INT_ITAB_SIZE,
     .            IREG,IFUN_RATE,IFUN_DLIM
      my_real  :: FCRIT,PTHKFAIL,DN,DCRIT,ECRIT,
     .            EXP_REF,EXPO,EL_REF,SR_REF1,
     .            FSCALE_EL,SHRF,BIAXF,SR_REF2,
     .            FSCALE_SR,CJC,FSCALE_DLIM,
     .            LENGTH_UNIT,RATE_UNIT
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
Card2 
      CALL HM_GET_FLOATV ('N'           ,DN          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('DCRIT'       ,DCRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('INST_ID'     ,ITAB_INST   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('ECRIT'       ,ECRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
Card3
      CALL HM_GET_INTV   ('FCT_EXP'     ,IFUN_EXP    ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_FLOATV ('EXP_REF'     ,EXP_REF     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('EXP'         ,EXPO        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
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
C--------------------------------------------------------------------------------
C     Functions and tables
C--------------------------------------------------------------------------------
      ! -> Functions
      NFUNC    = 3 
      IFUNC(1) = IFUN_EXP
      IFUNC(2) = IFUN_RATE
      IFUNC(3) = IFUN_DLIM
      ! -> Tables IDs conversion
      INT_ITAB_EPSF = ITAB_EPSF
      IF (ITAB_EPSF > 0) THEN
        CALL MATTAB_USR2SYS(TITR,MAT_ID,TABLE,1,INT_ITAB_EPSF)
      ENDIF
      INT_ITAB_INST = ITAB_INST
      IF (ITAB_INST > 0) THEN
        CALL MATTAB_USR2SYS(TITR,MAT_ID,TABLE,1,INT_ITAB_INST)
      ENDIF       
      INT_ITAB_SIZE = ITAB_SIZE
      IF (ITAB_SIZE > 0) THEN
        CALL MATTAB_USR2SYS(TITR,MAT_ID,TABLE,1,INT_ITAB_SIZE)
      ENDIF   
C--------------------------------------------------------------------------------
C     Filling buffer tables
C--------------------------------------------------------------------------------
      NUPARAM    = 21
      UPARAM(1)  = INT_ITAB_EPSF
      UPARAM(2)  = FCRIT
      UPARAM(3)  = FAILIP
      UPARAM(4)  = PTHKFAIL
      UPARAM(5)  = DN
      UPARAM(6)  = DCRIT
      UPARAM(7)  = INT_ITAB_INST
      UPARAM(8)  = ECRIT
      UPARAM(9)  = EXP_REF
      UPARAM(10) = EXPO
      UPARAM(11) = INT_ITAB_SIZE
      UPARAM(12) = IREG
      UPARAM(13) = EL_REF
      UPARAM(14) = SR_REF1
      UPARAM(15) = FSCALE_EL
      UPARAM(16) = SHRF
      UPARAM(17) = BIAXF
      UPARAM(18) = SR_REF2
      UPARAM(19) = FSCALE_SR
      UPARAM(20) = CJC
      UPARAM(21) = FSCALE_DLIM      
c-----------------------------------------------------------------------
      NUVAR = 3
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
        ! Element size scaling definition
        IF (ITAB_SIZE > 0) THEN 
          WRITE(IOUT,1011) IREG
          IF (IREG == 1) THEN 
            WRITE(IOUT,1012) ITAB_SIZE,EL_REF,SR_REF1,FSCALE_EL,SHRF,BIAXF
          ELSEIF(IREG == 2) THEN 
            WRITE(IOUT,1013) ITAB_SIZE,EL_REF,FSCALE_EL
          ENDIF
        ENDIF
        IF (IFUN_RATE > 0) THEN 
          WRITE(IOUT,1014)
          WRITE(IOUT,1015) IFUN_RATE,SR_REF2,FSCALE_SR
        ELSEIF (CJC > ZERO) THEN 
          WRITE(IOUT,1014)
          WRITE(IOUT,1016) SR_REF2,CJC
        ENDIF
        IF (IFUN_DLIM > 0) THEN 
          WRITE(IOUT,1017)
          WRITE(IOUT,1018) IFUN_DLIM,FSCALE_DLIM
        ENDIF 
        WRITE(IOUT,1019) FAILIP,PTHKFAIL
        WRITE(IOUT,1020)
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
     & 5X,'    SCALE FACTOR STRAIN RATE SCALING  . . . . . . . . . =',1PG20.13/)
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
     & 5X,'PERCENTAGE OF FAILED LAYERS PRIOR TO SHELL DELETION . . =',1PG20.13/) 
 1020 FORMAT(
     & 5X,' --------------------------------------------------------',/)   
c-----------------------------------------------------------------------
      END
