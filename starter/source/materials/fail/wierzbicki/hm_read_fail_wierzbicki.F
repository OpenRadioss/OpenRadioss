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
Chd|  HM_READ_FAIL_WIERZBICKI       source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
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
      SUBROUTINE HM_READ_FAIL_WIERZBICKI(FAIL , 
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Wierzbicki-Xue failure model parameters
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
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  ! submodel table 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFAIL_SH, IFAIL_SO,IMOY
      my_real C1,C2,C3,C4,CM,CN,PTHKF
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV  ('C1_WIERZBICKI',C1       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('C2_WIERZBICKI',C2       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('C3_WIERZBICKI',C3       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('C4_WIERZBICKI',C4       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('m'            ,CM       ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV  ('n_WIERZBICKI' ,CN       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV    ('Ifail_sh'     ,IFAIL_SH ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV    ('Ifail_so'     ,IFAIL_SO ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV    ('Imoy'         ,IMOY     ,IS_AVAILABLE,LSUBMODEL) 
C--------------------------------------------------
      IF (IFAIL_SH==0) IFAIL_SH=1
      IF (IFAIL_SO==0) IFAIL_SO=1
c
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE
      ENDIF
c
      FAIL%KEYWORD = 'WIERZBICKI' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 9
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 2
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
      FAIL%UPARAM(1) = C1
      FAIL%UPARAM(2) = C2
      FAIL%UPARAM(3) = C3
      FAIL%UPARAM(4) = C4
      FAIL%UPARAM(5) = CM
      FAIL%UPARAM(6) = CN
      FAIL%UPARAM(7) = IFAIL_SH
      FAIL%UPARAM(8) = IFAIL_SO
      FAIL%UPARAM(9) = IMOY 
c--------------------------------------------------
      IF(IS_ENCRYPTED)THEN
       WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE       
       WRITE(IOUT, 1000)C1,C2,C3,C4,CM,CN,IMOY     
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
c--------------------------------------------------
 1000 FORMAT(
     & 5X,40H  Xue-WIERZBICKI FAILURE MODEL          ,/,
     & 5X,40H  -----------------------------         ,/,
     & 5X,' PARAMETER (C1).. . . . . . . =',E12.4/
     & 5X,' PARAMETER (C2). . . . . . . .=',E12.4/
     & 5X,' PARAMETER (C3). . . . . . . .=',E12.4/
     & 5X,' PARAMETER (C4). . . . . . . .=',E12.4/
     & 5X,' PARAMETER (M ).   . . . . . .=',E12.4/
     & 5X,' HARDENING EXPONENT.   . . . .=',E12.4/
     & 5X,' FLAG FOR 3D FAILURE MODEL (BRICK) =',I10//)
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION ') 
 2100 FORMAT(
     & 5X,'   SOLID ELEMENT  DELETION ') 
 1200 FORMAT(
     & 5X,'    SHELL TENSOR STRESS OF LAYER WILL BE VANISHED ')    
 2200 FORMAT(
     & 5X,'    DEVIATORIC STRESS WILL BE VANISHED ')    
c-----------
      RETURN
      END
