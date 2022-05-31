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
Chd|  HM_READ_FAIL_BIQUAD           source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        BIQUAD_COEFFICIENTS           source/materials/fail/biquad/biquad_coefficients.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_BIQUAD(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,MAT_ID ,FAIL_ID  ,
     .           LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ TABULATED FAILURE MODEL  (/FAIL/BIQUAD)
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)  :: MAT_ID,FAIL_ID,MAXFUNC,MAXUPARAM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT) :: NUPARAM,NUVAR,NFUNC
      INTEGER,INTENT(OUT) :: IFUNC(MAXFUNC)
      my_real,INTENT(OUT) :: UPARAM(MAXUPARAM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   :: MFLAG,SFLAG,REG_FUNC
      my_real   :: C1,C2,C3,C4,C5,E1,E2,E3,E4,PTHK,INST,REF_LEN
      my_real   :: X_1(2)
      my_real   :: X_2(3)
      LOGICAL   :: IS_AVAILABLE,IS_CRYPTED
C=======================================================================
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT INPUT DATA
C--------------------------------------------------
card1
      CALL HM_GET_FLOATV ('C1'          ,C1          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('C2'          ,C2          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('C3'          ,C3          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('C4'          ,C4          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('C5'          ,C5          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
c
card2 damage accumulation parametars
c
      CALL HM_GET_FLOATV ('P_thickfail' ,PTHK        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('M_Flag'      ,MFLAG       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('S_Flag'      ,SFLAG       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Inst_start'  ,INST        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_INTV   ('fct_IDel'    ,REG_FUNC    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('EI_ref'      ,REF_LEN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
c---------------------------------------------------
c     Optional input
c---------------------------------------------------
      CALL HM_GET_FLOATV ('R1'          ,E1          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('R2'          ,E2          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('R4'          ,E3          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV ('R5'          ,E4          ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
c---------------------------------------------------
      PTHK = MIN(PTHK,ONE)
      PTHK = MAX(ZERO,PTHK)
      IF (SFLAG == 0)    SFLAG = 1
c---------------------------------------------------
c     pre definition for user-input data when only 
c     tension test data are provided
c---------------------------------------------------
      IF (C3 == ZERO) THEN
        SELECT CASE (MFLAG)
c
        CASE (1)                 ! Mild Seel
          C3 = 0.6
        CASE (2)                 ! HSS Seel light e-Body DP600
          C3 = 0.5
        CASE (3)                 ! UHSS Seel light_eBody Boron
          C3 = 0.12
        CASE (4)                 ! Aluminum light_eBody AA5182
          C3 = 0.3
        CASE (5)                 ! Aluminum light_eBody AA6082-T6
          C3 = 0.17
        CASE (6)                 ! Plastic light_eBody PA6GF30
          C3 = 0.1
        CASE (7)                 ! Plastic light_eBody PP T40
          C3 = 0.11
        CASE DEFAULT
          C3 = .6
c
        END SELECT
      ENDIF
c---------------------------------------------------
c
      CALL BIQUAD_COEFFICIENTS(C1,C2,C3,C4,C5,PTHK,MFLAG,X_1,X_2,E1,E2,E3,E4)
c
c---------------------------------------------------
      UPARAM(1)  = C2
      UPARAM(2)  = X_1(1)
      UPARAM(3)  = X_1(2)
      UPARAM(4)  = X_2(1)
      UPARAM(5)  = X_2(2)
      UPARAM(6)  = X_2(3)
      UPARAM(7)  = PTHK
      UPARAM(8)  = 0
      UPARAM(11) = SFLAG
      UPARAM(12) = INST
      UPARAM(9)  = C3
      UPARAM(10) = MFLAG
      UPARAM(13) = REF_LEN
c-------------------------
      IFUNC(1) = REG_FUNC
c-------------------------
      NFUNC   = 1
      NUPARAM = 13
c
      IF (REG_FUNC == 0) THEN
        NUVAR = 2
        IF (NPERTURB /= 0) NUVAR = 8
      ELSE
        NUVAR = 3
        IF (NPERTURB /= 0) NUVAR = 9
      ENDIF
c-----------------------------------------------
c     UVAR1   = damage due to instability (triax between 1/3 and 2/3)
c     UVAR2   = integration point
c     UVAR3-8 = perturbated parameter
c     UVAR3   (if perturbation is not used) or UVAR9 (if used) = initial element length
c---------------------------------------------------
      IF (IS_CRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT, 1000) FAIL_ID
        IF (MFLAG /= 0) WRITE(IOUT, 1100) MFLAG
        WRITE(IOUT, 1200) C1,C2,C3,C4,C5 
        WRITE(IOUT, 1300) X_1(2),X_1(1),C2
        WRITE(IOUT, 1400) X_2(3),X_2(2),X_2(1)
        IF (REG_FUNC > 0) WRITE(IOUT, 1500) REG_FUNC,REF_LEN
        IF (SFLAG == 3 .AND. INST <= ZERO)   THEN
          WRITE(IOUT, 1600)    
          SFLAG = 2
        ELSEIF (SFLAG == 3 .AND. INST >= c4) THEN
          SFLAG = 2
          WRITE(IOUT, 1700)
        ENDIF        
        WRITE(IOUT, 1800) ABS(PTHK),SFLAG
        IF (SFLAG == 3) WRITE(IOUT, 1900) INST
        WRITE(IOUT, 2000) 
      ENDIF      
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT(
     & 5X,'BI-QUADRATIC FAILURE MODEL               ',/,
     & 5X,'FAILURE MODEL ID . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,' MATERIAL PARAMETER SELECTOR            =',I10)
 1200 FORMAT(
     & 5X,'C1 . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'C2 . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'C3 . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'C4 . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'C5 . . . . . . . . . . . . . . . . . . .=',E12.4,/)
 1300 FORMAT(
     & 5X,' COEFFICIENTS OF FIRST PARABOLA ',/,
     & 5X,' ----------------------------- ',/,
     & 5X,'a. . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'b. . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'c. . . . . . . . . . . . . . . . . . . .=',E12.4,/)
 1400 FORMAT(
     & 5X,' COEFFICIENTS OF SECOND PARABOLA ',/,
     & 5X,' ----------------------------- ',/,
     & 5X,'d. . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'e. . . . . . . . . . . . . . . . . . . .=',E12.4,/
     & 5X,'f. . . . . . . . . . . . . . . . . . . .=',E12.4,/)
 1500 FORMAT(
     & 5X,'ELEMENT LENGTH REGULARIZATION USED:',/,
     & 5X,'REGULARIZATION FUNCTION ID . . . . . . .=',I10,/
     & 5X,'REFERENZE ELEMENT LENGTH . . . . . . . .=',E12.4,/)
 1600 FORMAT(
     & 5X,' Instability value less or equal zero !  ',/
     & 5X,' Instability will be ignored and selector set to 2 !',/)
 1700 FORMAT(
     & 5X,' Instability strain higher or equal plain strain value c4 !',/
     & 5X,' Instability will be ignored and selector set to 2 !',/)
 1800 FORMAT(
     & 5X,' ELEMENT DELETION :',/,
     & 5X,' Fraction of failled IPs for shell deletion . =',F5.1,/
     & 5X,' S-Flag. . . . . . . . . . . . . . . . . . . .=',I10,/)
 1900 FORMAT(
     & 5X,'Instability strain . . . . . . . . . . . . . .=',E12.4,//)
 2000 FORMAT(
     & 5X,' ---------------------------------------------- ',//)
c-----------------------------------------------------------------------
      END
