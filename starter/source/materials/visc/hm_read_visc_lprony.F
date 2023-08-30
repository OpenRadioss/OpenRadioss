Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_VISC_LPRONY           source/materials/visc/hm_read_visc_lprony.F
Chd|-- called by -----------
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        VISC_PARAM_MOD                ../common_source/modules/mat_elem/visc_param_mod.F
Chd|====================================================================
      SUBROUTINE HM_READ_VISC_LPRONY(
     .           VISC     ,IVISC   ,MAT_ID   ,UNITAB  ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE VISC_PARAM_MOD
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
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IVISC,MAT_ID
      TYPE (VISC_PARAM_)  ,INTENT(INOUT) :: VISC
      TYPE (UNIT_TYPE_)   ,INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) ,DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NUPARAM,NIPARAM,NUVAR,NPRONY,IMOD,IVISC_FLAG,FORM
      INTEGER :: FctID_G,FctID_K,ITAB,ISHAPE,
     .           FctID_Gs,FctID_Ks,FctID_Gl,FctID_Kl
      my_real :: GAMA(100),TAU(100)
C      
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================   
      VISC%ILAW = IVISC
C     
      !  initialization
      GAMA(1:100) = ZERO
      TAU(1:100)  = ZERO
C
      ! 1st Card - Flags and prony order 
      CALL HM_GET_INTV   ('Model_Order' ,NPRONY ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV   ('FORM'        ,FORM   ,IS_AVAILABLE,LSUBMODEL) 
      CALL HM_GET_INTV   ('FLAG_VISC'   ,IVISC_FLAG   ,IS_AVAILABLE,LSUBMODEL)
C
      IF(IVISC_FLAG == 0) IVISC_FLAG = 1
      IF(FORM == 0) FORM = 1
      IF (NPRONY == 0)CALL ANCMSG(MSGID=2026,MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO_BLIND_1,I1=MAT_ID)    
      ! =======================================================================================
      ! Classical input
      ! =======================================================================================
      !  -> Itab = 0 ! classical input of prony series
     
      IF (NPRONY > 0) THEN
          DO I=1,NPRONY
            CALL HM_GET_FLOAT_ARRAY_INDEX('GAMAI' ,GAMA(I)  ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('TAUI'  ,TAU(I) ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          ENDDO
      ENDIF
c-------------------------------------------------
c     Storing parameters in UPARAM / IPARAM tables
c-------------------------------------------------
      NUVAR   = (1 + NPRONY)*6
      NIPARAM = 3
      NUPARAM = 2*NPRONY
      ALLOCATE (VISC%UPARAM(NUPARAM))
      ALLOCATE (VISC%IPARAM(NIPARAM))
      VISC%NUVAR     = NUVAR
      VISC%NUPARAM   = NUPARAM
      VISC%NIPARAM   = NIPARAM
      VISC%IPARAM(1) = NPRONY
      VISC%IPARAM(2) = FORM
      VISC%IPARAM(3) = IVISC_FLAG
      DO I=1,NPRONY 
        VISC%UPARAM(I)          = GAMA(I)
        VISC%UPARAM(NPRONY + I) = TAU(I)  
      ENDDO   
c-----------------------------------------------------------------------
c     Output
c-----------------------------------------------------------------------
      IF (IS_ENCRYPTED)THEN                                
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        IF(NPRONY > 0) THEN
         WRITE(IOUT,1000)
          DO I=1,NPRONY
            WRITE(IOUT,1100) I
            WRITE(IOUT,1200) GAMA(I),TAU(I)
          ENDDO
          write(IOUT,1300) IVISC_FLAG
          write(IOUT,1400) FORM
        ENDIF 
      ENDIF                
C-----------        
 1000 FORMAT(
     & 5X,'  PRONY SERIES MODEL  :'         ,/,
     & 5X,' --------------------- '         ,/)
 
 1100 FORMAT(
     & 5X,'ORDER OF PRONY SERIES . . . . . . . . . . . . . . . . . . . . . . . . =',I10/)    
 1200 FORMAT(
     & 5X,'SHEAR RELAXATION RATIO   . . . . . . . . . . . . . . . . . . . . . . .= '1PG20.13/
     & 5X,'RELAXATION TIME . . . . .  . . . . . . . . . . . . . . . . . . . . . .=',1PG20.13) 
 1300 FORMAT(/
     & 5X,'VISCOUS STRESS FORMULATION . . . . . . . . . . . . . . . . . . . . . . =',I8 /
     &10X,' 1 : TOTAL VISCOUS STRESS '/,
     &10X,' 2 : DEVIATORIC VISCOUS STRESS IS DEVIATORIC' )     
 1400 FORMAT(/
     & 5X,'FLAG FOR VISCOUS RIGIDITY . . . . . . . . . . . . . . . . . . . . . . =',I8 /
     &10X,' 1 : ADDED VISCOSITY   '/,
     &10X,' 2 : SUBSTRUCTED VISCOSITY' )     
      RETURN
      END
