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
Chd|  HM_READ_FAIL_TENSSTRAIN       source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
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
      SUBROUTINE HM_READ_FAIL_TENSSTRAIN(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,ID     ,FAIL_ID  ,
     .           LSUBMODEL,UNIT_ID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE TENSSTRAIN (REPLACE LECR10)
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
      TYPE (UNIT_TYPE_),INTENT(IN)   :: UNITAB 
      INTEGER,INTENT(IN)             :: ID,FAIL_ID,UNIT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          :: UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)          :: NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER    ::  ISHELL,ISOLID,IRFUN,FUNC_ID,FUNC_ID2,S_FLAG,STRDEF
      my_real    ::  RF1   ,RF2  ,EPSP1, EPSP2, SCALE_EL,
     .               EL_REF,SCALE_TEMP,FSCAL_UNIT, UNIT_T
      LOGICAL    ::  IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C
C--------------------------------------------------
      SCALE_EL  = ZERO
      SCALE_TEMP= ZERO
      EL_REF    = ZERO
      FUNC_ID   = 0
      FUNC_ID2  = 0
      IF (UNIT_ID > 0) THEN
        UNIT_T = UNITAB%FAC_T(UNIT_ID) ! USED FOR ABSCISSA OF IRFUN
      ELSE
        UNIT_T = ONE
      ENDIF
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV         ('Epsilon_t1',RF1   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_t2',RF2   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('fct_ID'    ,IRFUN ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Epsilon_f1',EPSP1 ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_f2',EPSP2 ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('S_Flag'    ,S_FLAG,IS_AVAILABLE,LSUBMODEL) 
c
      IF (S_FLAG == 2  .or. S_FLAG == 3 .or. S_FLAG == 12 .or. S_FLAG == 13 .or. 
     .    S_FLAG == 22 .or. S_FLAG == 23) THEN
        CALL HM_GET_INTV           ('fct_IDel' ,FUNC_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV         ('Fscale_el',SCALE_EL,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV         ('EI_ref'   ,EL_REF  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
        CALL HM_GET_INTV           ('fct_IDt' ,FUNC_ID2 ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV         ('FscaleT',SCALE_TEMP,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
        !units 
        CALL HM_GET_FLOATV_DIM('Fscale_el',FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (SCALE_EL == ZERO) SCALE_EL = ONE*FSCAL_UNIT
c
        CALL HM_GET_FLOATV_DIM('EI_ref',FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (EL_REF == ZERO) EL_REF = ONE *FSCAL_UNIT           
      ENDIF
c--------------------------------------------------
      IF (S_FLAG == 0) S_FLAG = 1
      IF (S_FLAG == 11 .or. S_FLAG == 12 .or. S_FLAG == 13) THEN
        S_FLAG = S_FLAG - 10
        STRDEF = 2  ! input = engineering strain
      ELSEIF (S_FLAG == 21 .or. S_FLAG == 22 .or. S_FLAG == 23) THEN
        S_FLAG = S_FLAG - 20
        STRDEF = 3  ! input = true strain
      ELSE
        STRDEF = 1  ! input = strain defined by Ismstr flag in the property
      END IF
c        
      IF (SCALE_TEMP == ZERO) SCALE_TEMP = ONE
      IF (SCALE_EL   == ZERO) SCALE_EL = ONE
      IF (EL_REF     == ZERO) EL_REF = ONE
      IF (RF1 <= ZERO) RF1 = EP20
      IF (RF2 <= ZERO) RF2 = TWO*EP20
C
      IF (RF1 > RF2) THEN
        CALL ANCMSG(MSGID=617, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .              I1=ID)
      ENDIF     
c-----------------------
      NFUNC     = 3
      NUPARAM   = 10
      NUVAR     = 1
c-----------------------
      UPARAM(1) = RF1 
      UPARAM(2) = RF2
      UPARAM(3) = EPSP1
      UPARAM(4) = EPSP2
      UPARAM(5) = SCALE_EL
      UPARAM(6) = EL_REF    
      UPARAM(7) = SCALE_TEMP
      UPARAM(8) = S_FLAG
      UPARAM(9) = UNIT_T
      UPARAM(10)= STRDEF
c
      IFUNC(1)  = IRFUN
      IFUNC(2)  = FUNC_ID
      IFUNC(3)  = FUNC_ID2
C--------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT, 1100) RF1,RF2,IRFUN,STRDEF
        IF (EPSP1 > ZERO .OR. EPSP2 > ZERO) THEN
          WRITE(IOUT, 2000)EPSP1,EPSP2
        ENDIF
        IF (S_FLAG == 2) WRITE(IOUT, 2900)
        IF (S_FLAG == 3) WRITE(IOUT, 2950)
        WRITE(IOUT, 3000) FUNC_ID,SCALE_EL,EL_REF,FUNC_ID2,SCALE_TEMP
        IF (FAIL_ID > ZERO) THEN
          WRITE(IOUT, 4000) FAIL_ID
        ENDIF
      ENDIF
      RETURN
C-------------------------------------------------- 
 1000 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 1100 FORMAT(
     & 5X,40H    TENSION STRAIN FAILURE MODEL               /,
     & 5X,40H    ------------------------                  /,
     & 5X,'MAXIMUM TENSION STRAIN 1. . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'MAXIMUM TENSION STRAIN 2. . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'MAXIMUM STRAINS SCALING FUNCTION. . . . . . . . . . . . =',I10/
     & 5X,'FAILURE STRAIN INPUT TYPE . . . . . . . . . . . . . . . =',I10/
     & 5X,'                 =1 (STRAIN MEASURE DEFINED IN PROPERTY) ',/
     & 5X,'                 =2 (ENGINEERING STRAIN) . . . . . . . . ',/
     & 5X,'                 =3 (TRUE STRAIN). . . . . . . . . . . . ')
 2000 FORMAT(
     & 5X,'MAXIMUM FIRST PRINCIPAL STRAIN FOR ELEMENT DELETION . . =',E12.4/ 
     & 5X,'MAXIMUM SECOND PRINCIPAL STRAIN FOR ELEMENT DELETION. . =',E12.4/)
 2900 FORMAT(
     & 5X,'EQUIVALENT STRAIN METHOD USED '/)
 2950 FORMAT(
     & 5X,'FIRST PRINCIPAL STRAIN METHOD USED '/)
 3000 FORMAT(
     & 5X,'ELEMENT LENGTH REGULARIZATION USED:     ',/
     & 5X,'REGULARIZATION FUNCTION . . . . . . . . . . . . . . . . =',I10/ 
     & 5X,'EL SCALING FACTOR . . . . . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'REFERENCE ELEMENT LENGTH. . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'TEMPERATURE FUNCTION. . . . . . . . . . . . . . . . . . =',I10/ 
     & 5X,'TEMP SCALING FACTOR . . . . . . . . . . . . . . . . . . =',E12.4/)
 4000 FORMAT(
     & 5X,'FAIL_ID . . . . . . . . . . . . . . . . . . . . . . . . =',I10/)
      END
