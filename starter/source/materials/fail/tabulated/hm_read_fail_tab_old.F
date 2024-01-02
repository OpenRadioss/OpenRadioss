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
Chd|  HM_READ_FAIL_TAB_OLD          source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_TAB_OLD(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,IXFEM    )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   TABULATED FAILURE MODEL  (/FAIL/TAB)
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)  :: ID,FAIL_ID,MAXFUNC,MAXUPARAM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C OUTPUT ARGUMENTS
      INTEGER,INTENT(INOUT) :: NUPARAM,NUVAR,NFUNC,IXFEM
      INTEGER,INTENT(INOUT) :: IFUNC(MAXFUNC)
      my_real,INTENT(INOUT) :: UPARAM(MAXUPARAM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: I,ISHELL,ISOLID,NRATE,IFUN_SIZE,IFUN_TEMP
      my_real  :: P_THICKFAIL,SCALE_TEMP,SCALE_EL,EL_REF,DCRIT,DD,DN,DADV,
     .            FSCAL_UNIT
      my_real, DIMENSION(MAXFUNC)  :: YFAC,RATE
C-----------------------------------------------
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      DADV     = ZERO
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT INPUT DATA
C--------------------------------------------------
Card1
      CALL HM_GET_INTV   ('Ifail_sh'    ,ISHELL      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Ifail_so'    ,ISOLID      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('N_rate'      ,NRATE       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('P_thickfail' ,P_THICKFAIL ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('Ixfem'       ,IXFEM       ,IS_AVAILABLE,LSUBMODEL)
Card2
      CALL HM_GET_FLOATV ('Dcrit'       ,DCRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('D'           ,DD          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('n'           ,DN          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Dadv'        ,DADV        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
Card3
      DO I = 1,NRATE
        CALL HM_GET_INT_ARRAY_INDEX('fct_ID_TAB'     ,IFUNC(I),I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Fscale'       ,YFAC(I) ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('Epsdot'       ,RATE(I) ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDDO 
Card4
      CALL HM_GET_INTV   ('fct_IDel'    ,IFUN_SIZE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Fscale_el'   ,SCALE_EL    ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('EI_ref'      ,EL_REF      ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
Card5
      CALL HM_GET_INTV   ('fct_IDt'     ,IFUN_TEMP   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('FscaleT'     ,SCALE_TEMP  ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
c-----------------------------------------------------------------------
c     Set default parameter values
c-----------------------------------------------------------------------
      IF (DCRIT  == ZERO) DCRIT = ONE
      IF (DADV   == ZERO) DADV  = DCRIT
      IF (DADV > DCRIT) THEN
         DADV = DCRIT
         CALL ANCMSG(MSGID=974,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=ID)
      ENDIF
      IF (EL_REF == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('EI_ref'  ,FSCAL_UNIT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
        EL_REF = ONE*FSCAL_UNIT
      END IF
      IF (DD == ONE  ) DD = 0.999
      IF (DN == ZERO) DN = ONE
      IF (ISHELL == 0) ISHELL = 1
      IF (ISOLID == 0) ISOLID = 1
      IF (IXFEM /= 1 .AND. IXFEM /= 2) IXFEM = 0
      IF (IXFEM > 0) ISOLID = 0
      IF (SCALE_EL   == ZERO) SCALE_EL   = ONE
      IF (SCALE_TEMP == ZERO) SCALE_TEMP = ONE
c
      IF (P_THICKFAIL == ONE  ) P_THICKFAIL = P_THICKFAIL - EM06
c-----------------------------------------------------------------------
c     UPARAM
c-----------------------------------------------------------------------
      UPARAM(1) = ISOLID
      UPARAM(2) = ISHELL
      UPARAM(3) = P_THICKFAIL
      UPARAM(4) = DCRIT
      UPARAM(5) = DD
      UPARAM(6) = DN
      UPARAM(7) = SCALE_TEMP
      UPARAM(8) = SCALE_EL
      UPARAM(9) = EL_REF
      UPARAM(10)= IXFEM
      UPARAM(11)= DADV
      DO I = 1,NRATE
        UPARAM(11 + I) = YFAC(I)
        UPARAM(11 + I + NRATE) = RATE(I)
      ENDDO
c-----------------------------------------------------------------------
      IFUNC(NRATE+1) = IFUN_SIZE
      IFUNC(NRATE+2) = IFUN_TEMP
c-----------------------------------------------------------------------
      NFUNC   = NRATE + 2
      NUPARAM = 11 + NRATE * 2
      NUVAR   = 3
c-----------------------------------------------------------------------
      IF (IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT, 1000) FAIL_ID
        WRITE(IOUT, 1001)(IFUNC(I),YFAC(I),RATE(I),I=1,NRATE)
        WRITE(IOUT, 1002) IXFEM,DADV,UPARAM(3)
        WRITE(IOUT, 1003) DCRIT,DD,DN
        WRITE(IOUT, 1004) IFUN_SIZE,SCALE_TEMP,IFUN_TEMP,SCALE_EL,EL_REF
c       for shell
        IF (IXFEM == 0) THEN
          IF(ISHELL == 1) THEN
            WRITE(IOUT, 1100)
          ELSEIF (ISHELL == 2) THEN
            WRITE(IOUT, 1200)
          ELSEIF (ISHELL == 3) THEN
            WRITE(IOUT, 1300)
          ENDIF
        ELSE IF (IXFEM == 1) THEN      
          WRITE(IOUT, 1400)
        END IF
c       for solid      
        IF (ISOLID == 1) THEN
          WRITE(IOUT, 2100)
        ELSEIF(ISOLID == 2) THEN
          WRITE(IOUT, 2200)
        ENDIF
C
      ENDIF  ! IS_ENCRYPTED             
c-----------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT(
     & 5X,'TABULATED FAILURE CRITERIA WITH DAMAGE',//,
     & 5X,'FAILURE MODEL ID. . . . . . . . . . . . . . .=',I10/
     & 5X,'FAILURE STRAIN FUNCTIONS :                  ')
 1001 FORMAT(
     & 5X,'      YIELD STRESS FUNCTION NUMBER. . . . . .=',I10/
     & 5X,'      YIELD SCALE FACTOR. . . . . . . . . . .=',1PG20.13/
     & 5X,'      STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13)
 1002 FORMAT(
     & 5X,'FLAG XFEM. . . . . . . .. . . . . . . . . . .=',I10/
     & 5X,'CRITICAL ADVANCEMENT VALUE  . . . . . . . . .=',E12.4/
     & 5X,'PER HUNDRED OF SHELL THICKNESS FAILURE . . . . .=',E12.4)
 1003 FORMAT(
     & 5X,'CRITICAL DAMAGE VALUE . . . . . . . . . . . .=',E12.4/
     & 5X,'DAMAGE PARAMETER D. . . . . . . . . . . . . .=',E12.4/
     & 5X,'DAMAGE PARAMETER N. . . . . . . . . . . . . .=',E12.4)
 1004 FORMAT(
     & 5X,'TEMPERATURE SCALE FUNCTION. . . . . . . . . .=',I10/
     & 5X,'SCALE FACTOR OF TEMPERATURE FUNCTION. . . . .=',E12.4/
     & 5X,'ELEMENT LENGTH FUNCTION . . . . . . . . . . .=',I10/
     & 5X,'SCALE FACTOR OF LENGTH FUNCTION . . . . . . .=',E12.4/
     & 5X,'REFERENCE ELEMENT LENGTH. . . . . . . . . . .=',E12.4/
     & 5X,'REGULARIZATION FLAG . . . . . . . . . . . . .=',I10/
     & 5X,'FAILURE OPTION:')
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE OF ONE LAYER') 
 1200 FORMAT(
     & 5X,'   STRESS TENSOR IN SHELL LAYER SET TO ZERO AFTER FAILURE')   
 1300 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE OF ALL LAYERS')   
 1400 FORMAT(
     & 5X,'   SHELL ELEMENT CRACKING AFTER FAILURE')   
 2100 FORMAT(
     & 5X,'   SOLID ELEMENT DELETION AFTER FAILURE') 
 2200 FORMAT(
     & 5X,'   DEVIATORIC STRESS IN SOLID WILL VANISH AFTER FAILURE')    
c-----------------------------------------------------------------------
      END
