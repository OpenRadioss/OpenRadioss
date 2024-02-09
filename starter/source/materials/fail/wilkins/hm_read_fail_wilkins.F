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
Chd|  HM_READ_FAIL_WILKINS          source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        FAIL_PARAM_MOD                ../common_source/modules/mat_elem/fail_param_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_WILKINS(FAIL   ,
     .           MAT_ID   ,FAIL_ID  ,IRUPT   ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE WILKINS
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
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
      INTEGER             ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER             ,INTENT(IN) :: MAT_ID        ! material law ID
      INTEGER             ,INTENT(IN) :: IRUPT         ! failure model number
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER    ::     IMARGIN,IENG
      my_real    ::     RANI,DADV,FACT_MARGIN,FACT_LOOSEMETAL
      INTEGER    ::     IFAIL_SH, IFAIL_SO
      my_real    ::     PC,ALPHA,BETA,DF,PTHKF
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C--------------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV    ('Alpha'         ,ALPHA     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Beta_WILKINS'  ,BETA      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Plim'          ,PC        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Df'            ,DF        ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_INTV      ('Ifail_sh'      ,IFAIL_SH  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('Ifail_so'      ,IFAIL_SO    ,IS_AVAILABLE,LSUBMODEL)
            
c-----------------------------------------------------------------------
      IF (IFAIL_SH==0)  IFAIL_SH=1
      IF (IFAIL_SO==0)  IFAIL_SO=1
      IF (PC == ZERO) PC=EP20
      IF (DF <= ZERO) DF=EP20
c
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE
      ENDIF
c      
      FAIL%KEYWORD = 'WILKINS' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 6
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 1
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
      FAIL%PTHK    = PTHKF
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c      
      FAIL%UPARAM(1) = ALPHA
      FAIL%UPARAM(2) = BETA
      FAIL%UPARAM(3) = PC
      FAIL%UPARAM(4) = DF
      FAIL%UPARAM(5) = IFAIL_SH
      FAIL%UPARAM(6) = IFAIL_SO
c---------------------------
c     OUTPUT
c---------------------------
      IF(IS_ENCRYPTED)THEN
       WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE       
       WRITE(IOUT, 1000)ALPHA,BETA,PC,DF     
       IF(IFAIL_SH==1)THEN
        WRITE(IOUT, 1100)
       ELSEIF(IFAIL_SH==2)THEN
        WRITE(IOUT, 1200)
       ENDIF   
       IF(IFAIL_SO==1)THEN
        WRITE(IOUT, 2100)
       ELSEIF(IFAIL_SO==2)THEN
        WRITE(IOUT, 2200)
       ENDIF
      ENDIF         
c---------------------------
 1000 FORMAT(
     & 5X,40H    WILKINS DAMAGE MODEL                  /,
     & 5X,40H    --------------------                  /,
     & 5X,40HHYDROSTATIC PRESSURE COEFFICIENT. . . .=,1PE20.13/,
     & 5X,40HDEVIATORC COEFFICIENT   . . . . . . . .=,1PE20.13/,
     & 5X,40HLIMITE PRESSION     . . . . . . . . . .=,1PE20.13/,
     & 5X,40HCRITICAL DAMAGE     . . . . . . . . . .=,1PE20.13/     )
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION ') 
 2100 FORMAT(
     & 5X,'   SOLID ELEMENT  DELETION ') 

 1200 FORMAT(
     & 5X,'    SHELL TENSOR STRESS OF LAYER WILL BE VANISHED ')    
 2200 FORMAT(
     & 5X,'    DEVIATORIC STRESS WILL BE VANISHED ')    
c---------------------------
      RETURN
      END
