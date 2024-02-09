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
Chd|  HM_READ_FAIL_FLD              source/materials/fail/fld/hm_read_fail_fld.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        FAIL_PARAM_MOD                ../common_source/modules/mat_elem/fail_param_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_FLD(FAIL , 
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,IXFEM    ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read FLD failure model parameters
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
      INTEGER             ,INTENT(INOUT) :: IXFEM      ! XFEM activation flag
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFAIL_SH,IMARGIN,IENG
      INTEGER ,PARAMETER :: NFUNC = 2
      INTEGER ,DIMENSION(NFUNC) :: IFUNC
      my_real :: RANI,DADV,FACT_MARGIN,FACT_LOOSEMETAL,PTHKF
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
      FACT_MARGIN     = EM01  ! 0.1
      FACT_LOOSEMETAL = ZEP02 ! 0.02
c      
Card1--------------------------------------------------
      CALL HM_GET_INTV      ('fct_ID'    ,IFUNC(1) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('Ifail_sh'  ,IFAIL_SH   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('I_marg'    ,IMARGIN  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('fct_IDadv' ,IFUNC(2)  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV    ('Rani'      ,RANI     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Dadv'      ,DADV     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV      ('Istrain'   ,IENG     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('Ixfem'     ,IXFEM    ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
      IF (IMARGIN == 0) IMARGIN = 1
      IF (IMARGIN > 1 ) THEN
Card2--------------------------------------------------
        CALL HM_GET_FLOATV  ('Factor_Marginal'  ,FACT_MARGIN    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV  ('Factor_Loosemetal',FACT_LOOSEMETAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
C --- check
      IF (IFUNC(1) == 0) CALL ANCMSG(MSGID=2001,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND,
     .                               I1=MAT_ID )
C--------------------------------------------------
      IF (IFAIL_SH == 0)  IFAIL_SH = 1
      IF (IENG   /= 1)  IENG   = 0
      IF (RANI   == 0)  RANI   = ONE
      IF (DADV ==ZERO) THEN
        IF (IFUNC(2) == 0) THEN
          DADV = HALF
        ELSE
          DADV = ONE
        ENDIF
      ENDIF
      IF (IXFEM /= 1 .AND. IXFEM /= 2) IXFEM = 0
c
      IF (IFAIL_SH == 1) THEN
        PTHKF   = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF   = ONE
      ELSEIF (IFAIL_SH == 3) THEN  ! membrane criterion only
        PTHKF   = EM06
      ELSEIF (IFAIL_SH == 4) THEN  ! no element suppression
        PTHKF   = EM06
      ENDIF
c---------------------------
      FAIL%KEYWORD = 'FLD' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 8
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 1
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
c
      FAIL%UPARAM(1) = IFAIL_SH
      FAIL%UPARAM(2) = IMARGIN
      FAIL%UPARAM(3) = FACT_MARGIN
      FAIL%UPARAM(4) = IXFEM
      FAIL%UPARAM(5) = DADV
      FAIL%UPARAM(6) = IENG
      FAIL%UPARAM(7) = RANI   ! average anisotropy (for zone index output in ANIM) 
      FAIL%UPARAM(8) = FACT_LOOSEMETAL
C--------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1300)
      ELSE
        IF (IXFEM == 0)THEN
          WRITE(IOUT, 1000) IFUNC(1),RANI,IENG,IMARGIN,FACT_MARGIN,FACT_LOOSEMETAL
          IF (IFAIL_SH == 1) THEN
            WRITE(IOUT, 1100)
          ELSEIF (IFAIL_SH == 2) THEN
            WRITE(IOUT, 1200)
          ELSEIF (IFAIL_SH == 3) THEN
            WRITE(IOUT, 1250)
          ELSEIF (IFAIL_SH == 4) THEN
            WRITE(IOUT, 1260)
          ENDIF
        ELSE
          WRITE(IOUT, 1010) IFUNC(1),IFUNC(2),DADV,RANI,IENG,IXFEM,
     .                      IMARGIN,FACT_MARGIN,FACT_LOOSEMETAL
          WRITE(IOUT, 1400)
        END IF
      ENDIF
C--------------------------------------------------
      RETURN
 1300 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
C-----------
 1000 FORMAT(
     & 5X,'     FLD FAILURE MODEL                              ',/,
     & 5X,'     -----------------                              ',/,
     & 5X,'FORMING LIMIT DIAGRAM FUNCTION . . . . . . . . . . =',I10  /,
     & 5X,'AVERAGE ANISOTROPY FACTOR. . . . . . . . . . . . . =',E16.9/,
     & 5X,'ENGINEERING STRAIN INPUT FLAG. . . . . . . . . . . =',I10  /,
     & 5X,'FLAG I_MARGINAL. . . . . . . . . . . . . . . . . . =',I10  /,
     & 5X,'FACTOR MARGINAL. . . . . . . . . . . . . . . . . . =',E16.9/,
     & 5X,'FACTOR LOOSEMETAL. . . . . . . . . . . . . . . . . =',E16.9//)
 1100 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE') 
 1200 FORMAT(
     & 5X,'   STRESS TENSOR IN SHELL LAYER SET TO ZERO AFTER FAILURE')   
 1250 FORMAT(
     & 5X,'   ELEMENT IS DELETED ONLY WHEN MEMBRANE FAILS')   
 1260 FORMAT(
     & 5X,'   SHELL FAILURE IS DEACTIVATED')   
 1400 FORMAT(
     & 5X,'   SHELL ELEMENT CRACKING AFTER FAILURE')   
 1010 FORMAT(
     & 5X,'     XFEM FLD FAILURE MODEL                         ',/,
     & 5X,'     ----------------------                         ',/,
     & 5X,'FORMING LIMIT DIAGRAM FUNCTION . . . . . . . . . . =',I10  /,
     & 5X,'CRACK ADVANCEMENT LIMIT DIAGRAM. . . . . . . . . . =',I10  /,
     & 5X,'SCALE FACTOR FOR CRACK ADVANCEMENT . . . . . . . . =',E16.9/,
     & 5X,'AVERAGE ANISOTROPY FACTOR. . . . . . . . . . . . . =',E16.9/,
     & 5X,'ENGINEERING STRAIN INPUT FLAG. . . . . . . . . . . =',I10  /,
     & 5X,'FLAG XFEM. . . . . . . . . . . . . . . . . . . . . =',I10  /,
     & 5X,'FLAG I_MARGINAL. . . . . . . . . . . . . . . . . . =',I10  /,
     & 5X,'FACTOR MARGINAL. . . . . . . . . . . . . . . . . . =',E16.9/,
     & 5X,'FACTOR LOOSEMETAL. . . . . . . . . . . . . . . . . =',E16.9//)
c-----------
      RETURN
      END
