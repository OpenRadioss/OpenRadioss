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
Chd|  HM_READ_FAIL_VISUAL           source/materials/fail/visual/hm_read_fail_visual.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE HM_READ_FAIL_VISUAL( 
     .             UPARAM ,  MAXUPARAM,  NUPARAM,  NUVAR, NFUNC,  IFUNC,
     .             MAXFUNC,  LSUBMODEL, UNITAB )
C-----------------------------------------------
c  IRUP = 36
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C---------------------------------------------------------------------
C I m p l i c i t T y p e s
C---------------------------------------------------------------------
#include      "implicit_f.inc"
C----------+--------+---+---+-------------------------------------------
C VAR      | SIZE   |TYP|RW | DEFINITION
C----------+--------+---+---+-------------------------------------------
C IIN      | 1      | I | R | INPUT FILE UNIT (ROOT_nnn0.rad file)
C IOUT     | 1      | I | R | OUTPUT FILE UNIT (ROOT_nnn0.out file)
C UPARAM   | NUPARAM| F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM| 1      | I | R | MAXIMUM SIZE OF UPARAM
C NUPARAM  | 1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    | 1      | I | W | NUMBER OF USER VARIABLES
C----------+--------+---+---+-------------------------------------------
C IFUNC    | NFUNC  | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  | 1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    | 1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+--------+---+---+-------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C D u m m y A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)   ::UNITAB 
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      INTEGER MAXUPARAM,NUPARAM,NUVAR,MAXFUNC,NFUNC,
     . IFUNC(MAXFUNC),IUNIT
      my_real UPARAM(MAXUPARAM)           
C-----------------------------------------------
C L o c a l V a r i a b l e s
C-----------------------------------------------
       INTEGER TYPE_MAX,F_FLAG,STRDEF
       my_real C_MIN , C_MAX,EMA,BUTTER
       LOGICAL :: IS_AVAILABLE,IS_CRYPTED
C=======================================================================
C
C      Only visualization by damage of stress or strain is between to limited values
C            (no element deletion)
C
C=======================================================================
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
       
       NFUNC     = 0
       NUPARAM   = 7
       NUVAR     = 2
       
c! User variable # 1, to store the previous damage value
c! User variable # 2, to store the previous stress or strain value (for EMA filtering)
       
       TYPE_MAX  = 0
       C_MIN     = ZERO
       C_MAX     = ZERO
       BUTTER    = ZERO
c!CARD1
      CALL HM_GET_INTV   ('Type'                ,TYPE_MAX  ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_FLOATV ('C_min'               ,C_MIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('C_max'               ,C_MAX     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Alpha_Exp_Avg_Filter',EMA       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('f_cutoff'            ,EMA       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('f_flag'              ,F_FLAG    ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV   ('Strdef'              ,STRDEF    ,IS_AVAILABLE,LSUBMODEL) 



      IF (F_FLAG == 0 .OR. F_FLAG == 1 ) THEN
         F_FLAG = 1
         IF (EMA == ZERO) EMA = ONE
      ELSE
         BUTTER = EMA 
         EMA    = ONE       
         NUVAR  = 8! User variable # 3-8, Storage values for the Butterworth filter
      ENDIF

      IF (TYPE_MAX == 0) TYPE_MAX = 1
      IF (STRDEF == 0)   STRDEF   = 1
c-----------------------------------------------------------------------
      UPARAM(1) = REAL(TYPE_MAX) + 1e-4
      UPARAM(2) = C_MIN
      UPARAM(3) = C_MAX
      ! What it should be is like this:
      ! Value = USER_INPUT * 2 * Pi * DT1
      ! Alpha = Value / (Value + 1)
      ! Actual_filtered_stress = Alpha * actual_Stress + (1-Alpha) * previous_filtered_stress
      ! UPARAM(4) = EMA  * TWO * 3.14159265
      UPARAM(4) = EMA
      UPARAM(5) = BUTTER
      UPARAM(6) = REAL(F_FLAG) +0.00001
      UPARAM(7) = REAL(STRDEF)
c-----------------------------------------------------------------------
      IF (IS_CRYPTED) THEN
        WRITE (IOUT,'(5X,A,//)') 'CONFIDENTIAL DATA'
      ELSE 
       IF (TYPE_MAX == 1) THEN
         WRITE(IOUT, 1000) C_MIN, C_MAX
         IF (F_FLAG ==1 .AND. EMA /= ONE) THEN
           WRITE(IOUT, 2010) EMA
          ELSEIF(F_FLAG ==2)THEN
           WRITE(IOUT, 2020) BUTTER
          ELSE
           WRITE(IOUT, 2030)
         ENDIF
       ENDIF
       IF (TYPE_MAX == 2) THEN
         WRITE(IOUT, 2000) C_MIN, C_MAX, STRDEF
         IF (F_FLAG ==1 .AND. EMA /= ONE) THEN
           WRITE(IOUT, 2010) EMA
         ELSEIF(F_FLAG ==2)THEN
           WRITE(IOUT, 2020) BUTTER
         ELSE
           WRITE(IOUT, 2030)
         ENDIF
       ENDIF
      ENDIF
c-----------------------------------------------------------------------
 1000 FORMAT(
     & 5X,'         VISUAL FAILURE MODEL           ',/
     & 5X,' -------------------------------------- ',/
     & 5X,' TYPE IS STRESS ',/
     & 5X,' MINIMUM 1st PRINCIPAL STRESS. . . . . =',E12.4,/
     & 5X,' MAXIMUM 1st PRINCIPAL STRESS. . . . . =',E12.4,/)


 2000 FORMAT(
     & 5X,'         VISUAL FAILURE MODEL           ',/
     & 5X,' -------------------------------------- ',/
     & 5X,' TYPE IS STRAIN ',/
     & 5X,' MINIMUM 1st PRINCIPAL STRAIN. . . . . =',E12.4/
     & 5X,' MAXIMUM 1st PRINCIPAL STRAIN. . . . . =',E12.4/
     & 5X,' FAILURE STRAIN INPUT TYPE . . . . . . =',I10/
     & 5X,' =1 (STRAIN MEASURE DEFINED IN PROPERTY)',/                
     & 5X,' =2 (ENGINEERING STRAIN) . . . . . . . .',/                
     & 5X,' =3 (TRUE STRAIN). . . . . . . . . . . .')

 2010 FORMAT(
     & 5X,' EXPONENTIAL MOVING AVERAGE FILTER USED:',/
     & 5X,' F-COEFFICIENT . . . . . . . . . . . . =',E12.4,/)
 2020 FORMAT(
     & 5X,' 4-POLE BUTTERWORTH FILTER USED:',/
     & 5X,' F-COEFFICIENT . . . . . . . . . . . . =',E12.4,/)

 2030 FORMAT(
     & 5X,' NO FILTERING USED:',/)

      RETURN
      END
