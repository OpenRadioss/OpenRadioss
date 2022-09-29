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
Chd|  HM_READ_FAIL_TAB1             source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_TAB1(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,IXFEM    )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ TABULATED FAILURE MODEL  (/FAIL/TAB)
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)  :: ID,FAIL_ID,MAXFUNC,MAXUPARAM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT) :: NUPARAM,NUVAR,NFUNC,IXFEM
      INTEGER,INTENT(OUT) :: IFUNC(MAXFUNC)
      my_real,INTENT(OUT) :: UPARAM(MAXUPARAM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: ISHELL,ISOLID,DMG_FLAG,INST_FLAG,
     .            IFUN_DMG,IFUN_EPSF,IFUN_STAB,IFUN_SIZE,IFUN_TEMP
      my_real  :: P_THICKFAIL,P_THINNFAIL,SCALE_TEMP,SCALE_EL,EL_REF,
     .            Y1SCALE,X1SCALE,Y2SCALE,X2SCALE,DCRIT,DD,DN,DADV,
     .            ECRIT,FADE_EXPO,FSCAL_UNIT,SHRF,BIAXF
C-----------------------------------------------
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      DADV  = ZERO
      ECRIT = ZERO
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
      CALL HM_GET_FLOATV ('P_thickfail' ,P_THICKFAIL ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('P_thinfail'  ,P_THINNFAIL ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('Ixfem'       ,IXFEM       ,IS_AVAILABLE,LSUBMODEL)
Card2
      CALL HM_GET_FLOATV ('Dcrit'       ,DCRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('D'           ,DD          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('n'           ,DN          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Dadv'        ,DADV        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('fct_IDd'     ,IFUN_DMG    ,IS_AVAILABLE,LSUBMODEL)
Card3
      CALL HM_GET_INTV   ('table1_ID'   ,IFUN_EPSF   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Xscale1'     ,Y1SCALE     ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('Xscale2'     ,X1SCALE     ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('table2_ID'   ,IFUN_STAB   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Xscale3'     ,Y2SCALE     ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('Xscale4'     ,X2SCALE     ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
Card4
      CALL HM_GET_INTV   ('fct_IDel'    ,IFUN_SIZE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Fscale_el'   ,SCALE_EL    ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('EI_ref'      ,EL_REF      ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('Inst_start'  ,ECRIT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('Fad_exp'     ,FADE_EXPO   ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('Ch_i_f'      ,INST_FLAG   ,IS_AVAILABLE,LSUBMODEL)
Card5
      CALL HM_GET_INTV   ('fct_IDt'     ,IFUN_TEMP   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('FscaleT'     ,SCALE_TEMP  ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('Shear_limit' ,SHRF        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('Biax_limit'  ,BIAXF       ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
c-----------------------------------------------------------------------
      ! Error massage: 'table1_ID' is mandatory:
      IF (IFUN_EPSF == 0) THEN
        CALL ANCMSG(MSGID=2068,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND,
     .              I1=ID)
      ENDIF
      !
      IFUNC(1) = IFUN_EPSF
      IFUNC(2) = IFUN_SIZE
      IFUNC(3) = IFUN_TEMP
      IFUNC(4) = IFUN_DMG
      IFUNC(5) = IFUN_STAB
      IF (FADE_EXPO  <  ZERO) THEN
        IFUNC(6) = INT(ABS(FADE_EXPO))
        ELSE
        IFUNC(6) = 0
      ENDIF
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
      IF (DD == ONE  ) DD = 0.999
      IF (IFUN_STAB > 0) THEN
        ECRIT = ZERO
      ELSEIF (ECRIT == ZERO) THEN
        ECRIT = DD
      ENDIF
      IF (DN == ZERO) DN = ONE
      IF (ISHELL == 0) ISHELL = 1
      IF (ISOLID == 0) ISOLID = 1
      IF (IXFEM /= 1 .AND. IXFEM /= 2) IXFEM = 0
      IF (IXFEM > 0) ISOLID = 0
      IF (Y1SCALE == ZERO)    Y1SCALE    = ONE
      IF (Y2SCALE == ZERO)    Y2SCALE    = ONE
      IF (SCALE_EL   == ZERO) SCALE_EL   = ONE
      IF (SCALE_TEMP == ZERO) SCALE_TEMP = ONE
      IF (SHRF  == ZERO) SHRF  =-ONE
      IF (BIAXF == ZERO) BIAXF = ONE
c
      IF (FADE_EXPO > ZERO .or. ECRIT /= ZERO) THEN
        DMG_FLAG = 1
      ELSE 
        DMG_FLAG = 0
      ENDIF
      IF (INST_FLAG == 0 .OR. INST_FLAG > 3) INST_FLAG = 1
      IF (P_THICKFAIL == ONE  ) P_THICKFAIL = P_THICKFAIL - EM06
c-----------------------------------------------------------------------
      CALL HM_GET_FLOATV_DIM('Xscale2' ,FSCAL_UNIT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
      IF (X1SCALE == ZERO) X1SCALE = ONE*FSCAL_UNIT
c
      CALL HM_GET_FLOATV_DIM('Xscale4' ,FSCAL_UNIT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
      IF (X2SCALE == ZERO) X2SCALE = ONE*FSCAL_UNIT
c
      CALL HM_GET_FLOATV_DIM('EI_ref'  ,FSCAL_UNIT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
      IF (EL_REF == ZERO) EL_REF = ONE*FSCAL_UNIT
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
      UPARAM(12)= Y1SCALE
      UPARAM(13)= ONE / X1SCALE
      UPARAM(14)= Y2SCALE
      UPARAM(15)= ONE / X2SCALE
      UPARAM(16)= P_THINNFAIL
      UPARAM(17)= ECRIT
      UPARAM(18)= FADE_EXPO
      UPARAM(19)= DMG_FLAG
      UPARAM(20)= INST_FLAG
      UPARAM(21)= SHRF
      UPARAM(22)= BIAXF
c
c-----------------------------------------------------------------------
      NFUNC   = 6
      NUPARAM = 22
      NUVAR   = 8
c
c     UVAR storage:
C     1 = DAMAGE
C     2 = initial shell thickness
C     3 = DCrit_NS --> instability starts
C     4 = percent from instability to failure
C     5 = initial characteristic el. length
C     6 = IPOS 1 for Table
C     7 = IPOS 2 for Table
C     8 = IPOS 1 for vinter
c-----------------------------------------------------------------------
      IF (IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT, 1001) IFUNC(1), Y1SCALE, X1SCALE
        IF (IFUNC(5) /= 0) THEN
          WRITE(IOUT, 1002) IFUNC(5), Y2SCALE, X2SCALE
        ENDIF
        IF (IXFEM > 0) WRITE(IOUT, 1003) IXFEM,DADV
        WRITE(IOUT, 1004) P_THICKFAIL,P_THINNFAIL
        IF (IFUN_DMG > 0) THEN 
          WRITE(IOUT, 1009) DCRIT,IFUN_DMG,ECRIT
        ELSE 
          WRITE(IOUT, 1005) DCRIT,DD,DN,ECRIT
        ENDIF
        IF (FADE_EXPO >= ZERO) THEN
          WRITE(IOUT, 1006) FADE_EXPO
        ELSE
          WRITE(IOUT, 1007) INT(ABS(FADE_EXPO))
        ENDIF
        WRITE(IOUT, 1008) DMG_FLAG,
     .                    IFUNC(3),SCALE_TEMP,
     .                    IFUNC(2),SCALE_EL,EL_REF,SHRF,BIAXF,INST_FLAG
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
 1001 FORMAT(
     & 5X,'TABULATED FAILURE CRITERIA WITH DAMAGE',//,
     & 5X,'STRAIN TABLE ID . . . . . . . . . . . . . . . . . . .=',I10/
     & 5X,'    SCALE FACTOR FOR FAILURE STRAIN . . . . . . . . .=',E12.4/
     & 5X,'    STRAIN RATE FACTOR FOR FAILURE STRAIN . . . . . .=',E12.4)
 1002 FORMAT(
     & 5X,'NECKING TABLE ID      . . . . . . . . . . . . . . . .=',I10/
     & 5X,'     SCALE FACTOR FOR NECKING STRAIN. . . . . . . . .=',E12.4/
     & 5X,'     STRAIN RATE FACTOR FOR NECKING STRAIN. . . . . .=',E12.4)
 1003 FORMAT(
     & 5X,'FLAG XFEM. . . . . . . .. . . . . . . . . . . . . . .=',I10/
     & 5X,'     XFEM ADVANCEMENT SOFTENING FACTOR. . . . . . . .=',E12.4)
 1004 FORMAT(    
     & 5X,'PER HUNDRED OF SHELL THICKNESS FAILURE . . . . . . . . .=',E12.4/
     & 5X,'SHELL FAILURE DUE TO THINNING . . . . . . . . . . . .=',E12.4)
 1005 FORMAT(
     & 5X,'CRITICAL DAMAGE VALUE . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'DAMAGE PARAMETER D. . . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'DAMAGE PARAMETER N. . . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'INSTABILITY STRAIN. . . . . . . . . . . . . . . . . .=',E12.4)
 1009 FORMAT(
     & 5X,'CRITICAL DAMAGE VALUE . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'DAMAGE EVOLUTION FUNCTION . . . . . . . . . . . . . .=',I10/
     & 5X,'INSTABILITY STRAIN. . . . . . . . . . . . . . . . . .=',E12.4)
 1006 FORMAT(
     & 5X,'FADE PARAMETER. . . . . . . . . . . . . . . . . . . .=',E12.4)
 1007 FORMAT(
     & 5X,'FADE FUNCTION . . . . . . . . . . . . . . . . . . . .=',I10)
 1008 FORMAT(
     & 5X,'DAMAGE FLAG . . . . . . . . . . . . . . . . . . . . .=',I10/    
     & 5X,'TEMPERATURE SCALE FUNCTION. . . . . . . . . . . . . .=',I10/
     & 5X,'SCALE FACTOR OF TEMPERATURE FUNCTION. . . . . . . . .=',E12.4/
     & 5X,'ELEMENT LENGTH FUNCTION . . . . . . . . . . . . . . .=',I10/
     & 5X,'SCALE FACTOR OF LENGTH FUNCTION . . . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE ELEMENT LENGTH. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SHEAR TRIAXIALITY LIMIT FOR ELEMENT SIZE SCALING. . .=',E12.4/
     & 5X,'BI-TRACT TRIAXIALITY LIMIT FOR ELEMENT SIZE SCALING .=',E12.4/
     & 5X,'REGULARIZATION FLAG . . . . . . . . . . . . . . . . .=',I10/
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
