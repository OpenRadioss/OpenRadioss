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
Chd|  HM_READ_FAIL_NXT              source/materials/fail/nxt/hm_read_fail_nxt.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_NXT(
     .           FAIL     ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read NXT failure model parameters
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
      INTEGER            ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID        ! material law ID
      INTEGER            ,INTENT(IN) :: IRUPT         ! failure model type number
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  ! submodel table
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER    ::     IFAIL_SH
      INTEGER ,PARAMETER :: NFUNC = 2
      INTEGER ,DIMENSION(NFUNC) :: IFUNC
      my_real PTHKF
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
C EXTRACT DATAS  
C--------------------------------------------------
      IFAIL_SH = 0
Card1      
      CALL HM_GET_INTV ('fct_ID1'  ,IFUNC(1) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV ('fct_ID2'  ,IFUNC(2) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV ('Ifail_sh' ,IFAIL_SH ,IS_AVAILABLE,LSUBMODEL)
c-----------------------------
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE
      ENDIF
c-----------------------------------------------------
      FAIL%KEYWORD = 'NXT' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 1
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 2
      FAIL%NFUNC   = NFUNC
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
      FAIL%PTHK    = PTHKF
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
      
      FAIL%IFUNC(1:NFUNC) = IFUNC(1:NFUNC)
      FAIL%UPARAM(1) = IFAIL_SH
c-----------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE    
       IF(IFAIL_SH == 0) THEN   
        WRITE(IOUT, 1001)IFUNC(1),IFUNC(2)
       ELSE
        WRITE(IOUT, 1100)IFUNC(1),IFUNC(2)
       ENDIF
      ENDIF       
C-----------
 1000 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 1001 FORMAT(
     & 5X,40H     NXT FAILURE  CRITERIA                     /,
     & 5X,40H    ------------------------                  /,
     & 5X,'  NO SHELL ELEMENT DELETION AFTER FAILURE' /,
     & 5X,'S-R CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10/,
     & 5X,'3D  CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10//)
 1100 FORMAT(
     & 5X,40H     NXT FAILURE  CRITERIA                     /,
     & 5X,40H    ------------------------                  /,
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE' /,
     & 5X,'S-R CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10/,
     & 5X,'3D  CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10//)
C-----------
      RETURN
      END








