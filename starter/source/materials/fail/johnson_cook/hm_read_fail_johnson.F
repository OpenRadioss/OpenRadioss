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
Chd|  HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_JOHNSON(
     .           FAIL    ,MAT_ID   ,FAIL_ID  ,ILAW     ,IRUPT    ,IXFEM    ,
     .           TITR    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Johnoson-Cook failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
#include      "units_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN) :: FAIL_ID         ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID          ! material law ID
      INTEGER            ,INTENT(IN) :: ILAW            ! material law type number
      INTEGER            ,INTENT(IN) :: IRUPT           ! failure model type number
      CHARACTER          ,INTENT(IN) :: TITR*nchartitle ! material model title
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB          ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)    ! submodel table
      INTEGER            ,INTENT(INOUT) :: IXFEM        ! XFEM activation flag
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL         ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFAIL_SH,ISOLID
      my_real D1,D2,D3,D4,D5,EPSP0,UNIT_T,DADV,PTHKF
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------    
C     Check encryption
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C     EXTRACT PARAMETERS
C--------------------------------------------------
      CALL HM_GET_FLOATV         ('D1'           ,D1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D2'           ,D2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D3'           ,D3      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D4'           ,D4      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('D5'           ,D5      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('Epsilon_Dot_0',EPSP0   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ifail_sh'     ,IFAIL_SH  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('Ifail_so'     ,ISOLID  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('Dadv         ',DADV    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('Ixfem'        ,IXFEM   ,IS_AVAILABLE,LSUBMODEL)
!
      ! CHECK COMPATIBILITY WITH MATERIAL LAW
      IF (ILAW == 25) THEN 
        CALL ANCMSG(MSGID=3001, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND,
     .              I1=MAT_ID,
     .              C1=TITR)         
      ENDIF
!
      IF (EPSP0 == ZERO) EPSP0=EM20
      IF (IFAIL_SH == 0)   IFAIL_SH=1
      IF (ISOLID == 0)   ISOLID=1
      IF (IXFEM /= 1 .AND. IXFEM /= 2) IXFEM = 0
      IF (IXFEM > 0)     ISOLID = 0
      IF (DADV == ZERO .OR. DADV > ONE) DADV = ONE
c-----------------------------
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE
      ENDIF
c----------------------------------
      FAIL%KEYWORD = 'JOHNSON-COOK' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 11
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 0
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
      FAIL%UPARAM(1) = D1
      FAIL%UPARAM(2) = D2
      FAIL%UPARAM(3) = D3
      FAIL%UPARAM(4) = D4
      FAIL%UPARAM(5) = D5
      FAIL%UPARAM(6) = EPSP0
      FAIL%UPARAM(7) = IFAIL_SH
      FAIL%UPARAM(8) = ISOLID
      FAIL%UPARAM(9) = 0 ! not used
      FAIL%UPARAM(10)= DADV
      FAIL%UPARAM(11)= IXFEM
c---------------------------
c     OUTPUT
c---------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 

        WRITE(IOUT,500) MAT_ID,IRUPT,FAIL_ID
        
C---
C for shell      
C---
        IF (IXFEM == 0)THEN
          WRITE(IOUT, 1000)D1,D2,D3,D4,D5
          IF (IFAIL_SH == 1)THEN
            WRITE(IOUT, 1100)
          ELSEIF(IFAIL_SH == 2)THEN
            WRITE(IOUT, 1200)
          ENDIF
        ELSE
          WRITE(IOUT, 1010)D1,D2,D3,D4,D5,IXFEM,DADV
          WRITE(IOUT, 1400)
        END IF
C---
C for solid      
C---
        IF (IXFEM == 0) THEN ! XFEM is not available for solids
          IF(ISOLID == 1) THEN
            WRITE(IOUT, 2100)
          ELSEIF (ISOLID == 2) THEN
            WRITE(IOUT, 2200)
          ENDIF
        ENDIF
C
        IF (D3 > ZERO)THEN
          CALL ANCMSG(MSGID=831, MSGTYPE=MSGWARNING, ANMODE=ANINFO,
     .                I1=MAT_ID)
        ENDIF
      ENDIF ! IF (IS_ENCRYPTED)
c-----------------------------------------------------------
 500  FORMAT(//
     & 5X,'MAT_ID . . . . . . . . . . . . . . .=',I10/
     & 5X,'FAILURE MODEL. . . . . . . . . . . .=',I10/
     & 5X,'FAIL_ID. . . . . . . . . . . . . . .=',I10/)
 1000 FORMAT(
     & 5X,'     XFEM JOHNSON COOK DAMAGE MODEL     ',/,
     & 5X,'     ------------------------------     ',/,
     & 5X,'FIRST FAILURE PARAMETER (D1).. . . . . =',E12.4/
     & 5X,'SECOND FAILURE PARAMETER(D2). . . . . .=',E12.4/
     & 5X,'THIRD FAILURE PARAMETER (D3). . . . . .=',E12.4/
     & 5X,'FORTH FAILURE PARAMETER (D4). . . . . .=',E12.4/
     & 5X,'FIFTH FAILURE PARAMETER (D5).   . . . .=',E12.4//)
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE') 
 2100 FORMAT(
     & 5X,'   SOLID ELEMENT DELETION AFTER FAILURE') 

 1200 FORMAT(
     & 5X,'   STRESS TENSOR IN SHELL LAYER SET TO ZERO AFTER FAILURE')   
 1400 FORMAT(
     & 5X,'   SHELL ELEMENT CRACKING AFTER FAILURE')   
 2200 FORMAT(
     & 5X,'   DEVIATORIC STRESS IN SOLID WILL VANISH AFTER FAILURE')    
C
 1010 FORMAT(
     & 5X,'     XFEM JOHNSON COOK DAMAGE MODEL     ',/,
     & 5X,'     ------------------------------     ',/,
     & 5X,'FIRST  FAILURE PARAMETER(D1). . . . . .=',E12.4/
     & 5X,'SECOND FAILURE PARAMETER(D2). . . . . .=',E12.4/
     & 5X,'THIRD  FAILURE PARAMETER(D3). . . . . .=',E12.4/
     & 5X,'FORTH  FAILURE PARAMETER(D4). . . . . .=',E12.4/
     & 5X,'FIFTH  FAILURE PARAMETER(D5). . . . . .=',E12.4/
     & 5X,'FLAG XFEM . . . . . . . . . . . . . . .=',I10/,
     & 5X,'CRITICAL ADVANCEMENT VALUE. . . . . . .=',E12.4//)
C-----------
      RETURN
      END
