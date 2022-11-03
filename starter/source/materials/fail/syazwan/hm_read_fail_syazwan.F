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
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::MAXUPARAM,MAXFUNC,FAIL_ID,IMID_F
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real C1,C2,C3,C4,C5,C6,EPSMIN,PTHCKF
      my_real DAM_SF, MAX_DAM, N_VAL, SOFTEXP
      my_real REF_LEN,REF_SIZ_UNIT
      INTEGER STRAIN2DAM, INST, REG_FUNC
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------    
!!      FAC_T = UNITAB(4) 
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV         ('C1'           ,C1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('C2'           ,C2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('C3'           ,C3      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('C4'           ,C4      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('C5'           ,C5      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('C6'           ,C6      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('EPSMIN'       ,EPSMIN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('PTHCKF'       ,PTHCKF  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('STRAIN2DAM'   ,STRAIN2DAM  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('DAM_SF'       ,DAM_SF  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('MAX_DAM'      ,MAX_DAM ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('INST'         ,INST    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('N_VAL'        ,N_VAL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('SOFTEXP'      ,SOFTEXP ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('REG_FUNC'     ,REG_FUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('REF_LEN'      ,REF_LEN ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (REG_FUNC > 0 .AND. REF_LEN == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('REF_LEN' ,REF_SIZ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        REF_LEN = ONE*REF_SIZ_UNIT
      ENDIF

!
      IF(PTHCKF==0) PTHCKF=1.0
      PTHCKF = MIN(PTHCKF,ONE)
      PTHCKF = MAX(PTHCKF,ZERO)
      IF(DAM_SF==0) DAM_SF=1.0
      IF(MAX_DAM==0) MAX_DAM=1.0
      MAX_DAM = MIN(MAX_DAM,ONE)
      MAX_DAM = MAX(MAX_DAM,ZERO)
      IF(INST==1 .AND. N_VAL==ZERO) THEN
        N_VAL = 0.25
      ENDIF
      IF(SOFTEXP==0) SOFTEXP=1
      SOFTEXP = MAX(EM06,SOFTEXP)
!
      UPARAM(1) = C1
      UPARAM(2) = C2
      UPARAM(3) = C3
      UPARAM(4) = C4
      UPARAM(5) = C5
      UPARAM(6) = C6
      UPARAM(7) = EPSMIN
      UPARAM(8) = PTHCKF
      UPARAM(9) = STRAIN2DAM
      UPARAM(10) = DAM_SF
      UPARAM(11) = MAX_DAM
      UPARAM(12) = INST
      UPARAM(13) = N_VAL
      UPARAM(14) = SOFTEXP
      UPARAM(15) = REF_LEN
      IFUNC(1) = REG_FUNC
      NUPARAM = 15
      NFUNC = 1
      IF (REG_FUNC == 0) THEN
        NUVAR = 4
      ELSE
        NUVAR = 5
      ENDIF

C----
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)C1,C2,C3,C4,C5,C6,EPSMIN,PTHCKF,REF_LEN,REG_FUNC
        WRITE(IOUT,1100)STRAIN2DAM,DAM_SF,MAX_DAM
        WRITE(IOUT,1200)INST,N_VAL,SOFTEXP
      ENDIF
C---
C
 1000 FORMAT(
     & 5X,40H  SYAZWAN DAMAGE PARAMETER              ,/,
     & 5X,40H  -----------------------------         ,/,
     & 5X,'FIRST FAILURE PARAMETER (C1).. . . . . =',E12.4/
     & 5X,'SECOND FAILURE PARAMETER(C2). . . . . .=',E12.4/
     & 5X,'THIRD FAILURE PARAMETER (C3). . . . . .=',E12.4/
     & 5X,'FORTH FAILURE PARAMETER (C4). . . . . .=',E12.4/
     & 5X,'FIFTH FAILURE PARAMETER (C5). . . . . .=',E12.4/
     & 5X,'SIXTH FAILURE PARAMETER (C6). . . . . .=',E12.4/
     & 5X,'MIN PLASTIC STRAIN (EPSMIN).  . . . . .=',E12.4/
     & 5X,'PERCENTAGE OF LAYER FAILURE (PTHCKF). .=',E12.4/
     & 5X,'REFERENCE ELEMENT LENGTH. . . . . . . .=',E12.4/
     & 5X,'REGULARIZATION FUNCTION ID. . . . . . .=',I10/)
C-----------
 1100 FORMAT(
     & 5X,'STRAIN TO DAMAGE CALCULATION FLAG . . .=',I10/
     & 5X,'DAMAGE SCALE FACTOR . . . . . . . . . .=',E12.4/
     & 5X,'MAXIMUM ALLOWABLE DAMAGE. . . . . . . .=',E12.4/)
C-----------
 1200 FORMAT(
     & 5X,'INSTABILITY FLAG. . . . . . . . . . . .=',I10/
     & 5X,'N VALUE . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'STRESS SOFTENING EXPONENT . . . . . . .=',E12.4/)

      RETURN
      END

