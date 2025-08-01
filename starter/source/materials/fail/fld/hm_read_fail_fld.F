Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_fail_fld         ../starter/source/materials/fail/fld/hm_read_fail_fld.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_FLD(FAIL , 
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,IXFEM    ,
     .           LSUBMODEL,UNITAB   ,FAIL_TAG )
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
      USE ELBUFTAG_MOD            
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
      TYPE (FAIL_TAG_)    ,INTENT(INOUT) :: FAIL_TAG   ! failure model tag for buffer allocation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFAIL_SH,IMARGIN,IENG
      INTEGER ,PARAMETER :: NFUNC = 2
      INTEGER ,DIMENSION(NFUNC) :: IFUNC
      my_real :: RANI,DADV,FACT_MARGIN,FACT_LOOSEMETAL,PTHKF,FCUT,ALPHA
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
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
      CALL HM_GET_INTV      ('Ifail_sh'  ,IFAIL_SH ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('I_marg'    ,IMARGIN  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('fct_IDadv' ,IFUNC(2) ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV    ('Rani'      ,RANI     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Dadv'      ,DADV     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV      ('Istrain'   ,IENG     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('Ixfem'     ,IXFEM    ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
      IF (IMARGIN == 0) IMARGIN = 1
      IF (IMARGIN > 1) THEN
Card2--------------------------------------------------
        CALL HM_GET_FLOATV  ('Factor_Marginal'  ,FACT_MARGIN    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV  ('Factor_Loosemetal',FACT_LOOSEMETAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF
      IF (IENG > 1) THEN
Card3--------------------------------------------------
        CALL HM_GET_FLOATV  ('FCUT'             ,FCUT           ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV  ('ALPHA'            ,ALPHA          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ELSE
        FCUT = ZERO
        ALPHA = ZERO
      ENDIF
C --- check
      IF (IFUNC(1) == 0) CALL ANCMSG(MSGID=2001,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND,
     .                               I1=MAT_ID )
C--------------------------------------------------
      IENG = MIN(IENG,2)
      IENG = MAX(IENG,0)
      IF (FCUT == ZERO) FCUT = 10000.0D0*UNITAB%FAC_T_WORK
      IF (ALPHA > ZERO) FCUT = ZERO
      IF (IFAIL_SH == 0)  IFAIL_SH = 1
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
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE
      ELSEIF (IFAIL_SH == 3) THEN  ! membrane criterion only
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 4) THEN  ! no element suppression
        PTHKF = EM06
      ENDIF
c---------------------------
      FAIL%KEYWORD = 'FLD' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 6
      FAIL%NIPARAM = 3
      FAIL%NUVAR   = 3
      FAIL%NFUNC   = NFUNC
      FAIL%NTABLE  = 0
      IF (IENG == 2) THEN 
        FAIL%NMOD  = 3
      ELSE
        FAIL%NMOD  = 2
      ENDIF
      FAIL%PTHK    = PTHKF
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
      ALLOCATE (FAIL%MODE  (FAIL%NMOD))
c
      FAIL%IFUNC(1:NFUNC) = IFUNC(1:NFUNC)
c
      FAIL%UPARAM(1) = FACT_MARGIN
      FAIL%UPARAM(2) = DADV
      FAIL%UPARAM(3) = RANI   ! average anisotropy (for zone index output in ANIM) 
      FAIL%UPARAM(4) = FACT_LOOSEMETAL
      FAIL%UPARAM(5) = FCUT
      FAIL%UPARAM(6) = ALPHA
c
      FAIL%IPARAM(1) = IFAIL_SH
      FAIL%IPARAM(2) = IMARGIN
      FAIL%IPARAM(3) = IENG
c
      FAIL%MODE(1) = "FLDF Damage factor"
      FAIL%MODE(2) = "FLDZ Failure zone factor"
      IF (IENG == 2) THEN 
        FAIL%MODE(3) = "Incremental Strains ratio Beta"
      ENDIF
      FAIL_TAG%LF_DAMMX = 1 + FAIL%NMOD
      FAIL_TAG%LF_DAM   = 1 ! damage factor for ANIM output
      FAIL_TAG%LF_INDX  = 1 ! FLD zone index for ANIM output
c
c--------------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IXFEM == 0)THEN
          WRITE(IOUT, 1000) 
          WRITE(IOUT, 1100) IFUNC(1),RANI,IMARGIN,FACT_MARGIN,FACT_LOOSEMETAL
          WRITE(IOUT, 1600) IENG
          IF (IENG > 1) WRITE(IOUT, 1800) FCUT,ALPHA 
          IF (IFAIL_SH == 1) THEN
            WRITE(IOUT, 1700)
          ELSEIF (IFAIL_SH == 2) THEN
            WRITE(IOUT, 1710)
          ELSEIF (IFAIL_SH == 3) THEN
            WRITE(IOUT, 1720)
          ELSEIF (IFAIL_SH == 4) THEN
            WRITE(IOUT, 1730)
          ENDIF
        ELSE
          WRITE(IOUT, 1010) 
          WRITE(IOUT, 1110) IFUNC(1),IFUNC(2),DADV,RANI,
     &                      IMARGIN,FACT_MARGIN,FACT_LOOSEMETAL
          WRITE(IOUT, 1600) IENG
          IF (IENG > 1) WRITE(IOUT, 1800) FCUT,ALPHA
          WRITE(IOUT, 1400)
        END IF
      ENDIF
C-----------
 1000 FORMAT(
     & 5X,'-----------------------------------------------------',/,
     & 5X,'      FAILURE CRITERION : FORMING LIMIT DIAGRAM      ',/,
     & 5X,'-----------------------------------------------------',/)
 1100 FORMAT(
     & 5X,'FORMING LIMIT DIAGRAM FUNCTION ID . . . . . . . . . =',I10/,
     & 5X,'AVERAGE ANISOTROPY FACTOR (RANI). . . . . . . . . . =',1PG20.13/,
     & 5X,'MARGINAL VALUE FLAG (I_MARG). . . . . . . . . . . . =',I10/,
     & 5X,'FACTOR MARGINAL . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'FACTOR LOOSEMETAL . . . . . . . . . . . . . . . . . =',1PG20.13//)
 1600 FORMAT(
     & 5X,'INPUT/FORMULATION FLAG (ISTRAIN). . . . . . . . . . =',I10/,
     & 5X,'   = 0: TRUE STRAIN (CLASSIC) INPUT                  ',/,
     & 5X,'   = 1: ENGINEERING STRAIN INPUT FLAG                ',/,
     & 5X,'   = 2: NON-LINEAR PATH FORMULATION                  ',/)
 1700 FORMAT(
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE',//)
 1710 FORMAT(
     & 5X,'   STRESS TENSOR IN SHELL LAYER SET TO ZERO AFTER FAILURE',//)
 1720 FORMAT(
     & 5X,'   ELEMENT IS DELETED ONLY WHEN MEMBRANE FAILS',//)  
 1730 FORMAT(
     & 5X,'   SHELL FAILURE IS DEACTIVATED',//)   
 1400 FORMAT(
     & 5X,'   SHELL ELEMENT CRACKING AFTER FAILURE',//)    
 1800 FORMAT(
     & 5X,'STRAIN RATIO FILTERING FREQUENCY (FCUT) . . . . . . =',1PG20.13/,
     & 5X,'STRAIN RATIO FILTERING WEIGHT (ALPHA) . . . . . . . =',1PG20.13//)     
 1010 FORMAT(
     & 5X,'-----------------------------------------------------',/,
     & 5X,'   FAILURE CRITERION : XFEM FORMING LIMIT DIAGRAM    ',/,
     & 5X,'-----------------------------------------------------',/)
 1110 FORMAT(
     & 5X,'FORMING LIMIT DIAGRAM FUNCTION ID . . . . . . . . . =',I10/,
     & 5X,'CRACK ADVANCEMENT LIMIT DIAGRAM FUNCTION ID . . . . =',I10/,
     & 5X,'SCALE FACTOR FOR CRACK ADVANCEMENT (DADV) . . . . . =',1PG20.13/,
     & 5X,'AVERAGE ANISOTROPY FACTOR (RANI). . . . . . . . . . =',1PG20.13/,
     & 5X,'MARGINAL VALUE FLAG (I_MARG). . . . . . . . . . . . =',I10/,
     & 5X,'FACTOR MARGINAL . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'FACTOR LOOSEMETAL . . . . . . . . . . . . . . . . . =',1PG20.13//)
c-----------
      END
