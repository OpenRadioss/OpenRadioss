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
Chd|  HM_READ_FAIL_SYAZWAN          source/materials/fail/syazwan/hm_read_fail_syazwan.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_SYAZWAN(
     .           UPARAM    ,MAXUPARAM ,NUPARAM ,NUVAR  ,IFUNC  ,
     .           MAXFUNC   ,NFUNC     ,UNITAB  ,IMID_F ,FAIL_ID,
     .           LSUBMODEL )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE SYAZWAN
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     UNITAB          UNITS ARRAY
C     FAIL_ID         FAILURE ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C FAIL_ID  |  1      | I | W | ID OF FAILURE CRITERIA
C----------+---------+---+---+--------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      INTEGER,INTENT(IN) :: MAXUPARAM,MAXFUNC,FAIL_ID,IMID_F
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT) :: IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT) :: UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)   :: NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real C1,C2,C3,C4,C5,C6
      my_real DAM_SF,MAX_DAM,N_VAL,SOFTEXP
      my_real REF_LEN,REF_SIZ_UNIT,REG_SCALE
      INTEGER FAILIP,DINIT,INST,REG_FUNC
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------    
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS
C--------------------------------------------------
      ! 1st Line : failure criterion parameters
      CALL HM_GET_FLOATV ('C1'       ,C1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('C2'       ,C2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('C3'       ,C3      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('C4'       ,C4      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('C5'       ,C5      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! 2nd Line : failure criterion parameters + solid element deletion control
      CALL HM_GET_FLOATV ('C6'       ,C6      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('FAILIP'   ,FAILIP  ,IS_AVAILABLE,LSUBMODEL)
      ! 3rd Line : Damage initialization control
      CALL HM_GET_INTV   ('DINIT'    ,DINIT   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('DAM_SF'   ,DAM_SF  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAX_DAM'  ,MAX_DAM ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! 4th Line : Instability + stress softening
      CALL HM_GET_INTV   ('INST'     ,INST    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('N_VAL'    ,N_VAL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('SOFTEXP'  ,SOFTEXP ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! 5th Line : Element size regularization control
      CALL HM_GET_INTV   ('REG_FUNC' ,REG_FUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('REF_LEN'  ,REF_LEN ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (REG_FUNC > 0 .AND. REF_LEN == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('REF_LEN',REF_SIZ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        REF_LEN = ONE*REF_SIZ_UNIT
      ENDIF
      CALL HM_GET_FLOATV ('REG_SCALE',REG_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (REG_FUNC > 0 .AND. REG_SCALE == ZERO) REG_SCALE = ONE
C--------------------------------------------------
C CHECK VALUES
C--------------------------------------------------
      IF (FAILIP  == 0)    FAILIP  = 1
      DAM_SF = MAX(DAM_SF,ZERO)
      IF (DAM_SF  == ZERO) DAM_SF  = ONE
      IF (MAX_DAM == ZERO) MAX_DAM = ONE
      MAX_DAM = MIN(MAX_DAM,ONE)
      MAX_DAM = MAX(MAX_DAM,ZERO)
      IF(INST==1 .AND. N_VAL==ZERO) THEN
        N_VAL = FOURTH
      ENDIF
      IF (SOFTEXP == ZERO) SOFTEXP = ONE
      SOFTEXP = MAX(EM06,SOFTEXP)
C--------------------------------------------------
C SAVE PARAMETERS
C--------------------------------------------------
      ! Number of functions
      NFUNC      = 1
      IFUNC(1)   = REG_FUNC
      ! Number of parameters
      NUPARAM    = 15
      UPARAM(1)  = C1
      UPARAM(2)  = C2
      UPARAM(3)  = C3
      UPARAM(4)  = C4
      UPARAM(5)  = C5
      UPARAM(6)  = C6
      UPARAM(7)  = FAILIP
      UPARAM(8)  = DINIT
      UPARAM(9)  = DAM_SF
      UPARAM(10) = MAX_DAM
      UPARAM(11) = INST
      UPARAM(12) = N_VAL
      UPARAM(13) = SOFTEXP
      UPARAM(14) = REF_LEN
      UPARAM(15) = REG_SCALE
      ! Number of internal variables
      NUVAR      = 3
C--------------------------------------------------
C PRINT OUT PARAMETERS
C--------------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000) 
        WRITE(IOUT,1100) C1,C2,C3,C4,C5,C6
        WRITE(IOUT,1200) DINIT,DAM_SF,MAX_DAM
        WRITE(IOUT,1300) INST,N_VAL,SOFTEXP
        IF (REG_FUNC > 0) THEN 
          WRITE(IOUT,1400) REG_FUNC,REF_LEN,REG_SCALE
        ENDIF
      ENDIF
C---
C
 1000 FORMAT(
     & 5X,' --------------------------------------------------------',/,
     & 5X,'               SYAZWAN FAILURE CRITERION                 ',/,
     & 5X,' --------------------------------------------------------',/)
 1100 FORMAT(
     & 5X,'1ST FAILURE PARAMETER (C1). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'2ND FAILURE PARAMETER (C2). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'3RD FAILURE PARAMETER (C3). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'4TH FAILURE PARAMETER (C4). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'5TH FAILURE PARAMETER (C5). . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'6TH FAILURE PARAMETER (C6). . . . . . . . . . . . . . . =',1PG20.13/)
 1200 FORMAT(
     & 5X,'DAMAGE VARIABLE INITIALIZATION FLAG (DINIT) . . . . . . =',I10/,
     & 5X,'   DINIT = 0: NO DAMAGE INITIALIZATION                   ',/,
     & 5X,'   DINIT = 1: DAMAGE INITIALIZATION FROM STRAIN TENSOR   ',/,
     & 5X,'DAMAGE INITIALIZATION SCALE FACTOR. . . . . . . . . . . =',1PG20.13/
     & 5X,'DAMAGE INITIALIZATION MAXIMAL VALUE . . . . . . . . . . =',1PG20.13/)
 1300 FORMAT(
     & 5X,'INSTABILITY FLAG (INST) . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'   INST = 0: NO NECKING INSTABILITY                      ',/,
     & 5X,'   INST = 1: NECKING INSTABILITY ACTIVATED               ',/,
     & 5X,"HOLLOMON'S LAW N VALUE  . . . . . . . . . . . . . . . . =",1PG20.13/
     & 5X,'STRESS SOFTENING EXPONENT (SOFTEXP) . . . . . . . . . . =',1PG20.13/)
 1400 FORMAT(
     & 5X,'ELEMENT SIZE SCALING FUNCTION ID . . . . . . . . . . . .=',I10/
     & 5X,'   REFERENCE ELEMENT SIZE  . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'   SCALE FACTOR  . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)

      END

