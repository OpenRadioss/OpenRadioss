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
!||    hm_read_fail_gene1       ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_GENE1(
     .           FAIL    ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           TITR    ,LSUBMODEL,UNITAB   ,FAIL_TAG )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read generic tabulated failure model parameters
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
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER             ,INTENT(IN) :: FAIL_ID        !< failure model ID
      INTEGER             ,INTENT(IN) :: MAT_ID         !< material law ID
      INTEGER             ,INTENT(IN) :: IRUPT          !< failure model number
      CHARACTER           ,INTENT(IN) :: TITR*500       !< material model title
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB         !< table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)   !< submodel table 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL        !< failure model data structure
      TYPE (FAIL_TAG_)    ,INTENT(INOUT) :: FAIL_TAG    !< failure model tag for buffer allocation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NSTEP,NCS,fct_IDsm,fct_IDps,fct_IDg12,fct_IDg13,
     .        fct_IDe1c,fct_IDel,Itab,tab_IDfld,Istrain,CRIT,
     .        Ismooth,NCRIT_MAX,NFUNC,NTABLE,NUPARAM,NUVAR,NMODE,FAILIP 
      INTEGER ,PARAMETER :: NSIZE = 6
      INTEGER ,DIMENSION(NSIZE) :: IFUNC
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,KF,
     .        EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,VOLFRAC,PTHK,MAXTEMP,Fscale_el,El_ref,FREQ_UNIT,
     .        PRESSURE_UNIT,LENGTH_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      CHARACTER(LEN=NCHARTITLE), DIMENSION(50) :: MODNAME
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ! Initialization of the number of criteria
      NCRIT_MAX = 0
      ! Initialization of the number of modes
      NMODE = 0
      ! Activated criteria
      CRIT  = 0
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================================================================
C EXTRACT DATAS (REAL VALUES)
C======================================================================================
!---------------
! -> Card1
!---------------
      CALL HM_GET_FLOATV ('MAT_MINPRES'  ,MINPRES   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MINPRES /= ZERO) THEN 
        CRIT = IBSET(CRIT,1)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Minimum hydrostatic pressure"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_MAXPRES'  ,MAXPRES   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MAXPRES /= ZERO) THEN 
        CRIT = IBSET(CRIT,2)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum hydrostatic pressure"
      ENDIF
      IF ((MINPRES /= ZERO).OR.(MAXPRES /= ZERO)) NCRIT_MAX = NCRIT_MAX + 1
      CALL HM_GET_FLOATV ('MAT_SIGP1'    ,SIGP1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (SIGP1 /= ZERO) THEN
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,3)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum principal stress"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_TMAX'     ,TMAX      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (TMAX  /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,4)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Failure time"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_DTMIN'    ,DTMIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (DTMIN /= ZERO) THEN
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,5)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Minimum timestep"
      ENDIF
!---------------
! -> Card2
!---------------
      CALL HM_GET_INTV   ('fct_IDsm'     ,fct_IDsm  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_EPSDSM'   ,EPSDOT_SM ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_SIGVM'    ,SIGVM     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((SIGVM /= ZERO).OR.(fct_IDsm /= 0)) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,6)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum equivalent stress"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_SIGTH'    ,SIGTH     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_KF'       ,KF        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((SIGTH /= ZERO).OR.(KF /= ZERO)) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,7)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Tuler-Butcher failure"
      ENDIF
!---------------
! -> Card3
!---------------
      CALL HM_GET_INTV   ('fct_IDps'     ,fct_IDps  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_EPSDPS'   ,EPSDOT_PS ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_MAXEPS'   ,MAXEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((MAXEPS /= ZERO).OR.(fct_IDps /= 0)) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,8)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum principal strain"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_EFFEPS'   ,EFFEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (EFFEPS /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,9)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum effective strain"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_VOLEPS'   ,VOLEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (VOLEPS /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,10)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum volumetric strain"
      ENDIF
!---------------
! -> Card4
!---------------
      CALL HM_GET_FLOATV ('MAT_MINEPS'   ,MINEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MINEPS /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,11)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Minimum principal strain"
      ENDIF
      CALL HM_GET_FLOATV ('MAT_EPSSH'    ,EPSSH     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (EPSSH  /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,12)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Critical shear strain"
      ENDIF
      CALL HM_GET_INTV   ('fct_IDg12'    ,fct_IDg12 ,IS_AVAILABLE,LSUBMODEL)
      IF (fct_IDg12 /= 0) THEN 
        CRIT = IBSET(CRIT,13)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum in-plane shear strain"
      ENDIF 
      CALL HM_GET_INTV   ('fct_IDg13'    ,fct_IDg13 ,IS_AVAILABLE,LSUBMODEL)
      IF (fct_IDg13 /= 0) THEN 
        CRIT = IBSET(CRIT,14)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum transversal shear strain"
      ENDIF 
      CALL HM_GET_INTV   ('fct_IDe1c'    ,fct_IDe1c ,IS_AVAILABLE,LSUBMODEL)
      IF (fct_IDe1c /= 0) THEN 
        CRIT = IBSET(CRIT,15)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum in-plane major strain"
      ENDIF 
      IF ((fct_IDg13 /= 0).AND.(fct_IDe1c /= 0)) THEN 
        NCRIT_MAX = NCRIT_MAX + 2
      ELSEIF ((fct_IDg12 /= 0).AND.((fct_IDg13 /= 0).OR.(fct_IDe1c /= 0))) THEN       
        NCRIT_MAX = NCRIT_MAX + 1
      ELSEIF ((fct_IDg12 == 0).AND.(fct_IDg13 == 0).AND.(fct_IDe1c == 0)) THEN 
      ELSE
        NCRIT_MAX = NCRIT_MAX + 1
      ENDIF
!---------------
! -> Card5
!---------------
      CALL HM_GET_INTV   ('tab_IDfld'    ,tab_IDfld ,IS_AVAILABLE,LSUBMODEL)
      IF (tab_IDfld /= 0) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,16)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Forming Limit Diagram"
      ENDIF
      CALL HM_GET_INTV   ('Itab'         ,Itab      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_EPSDFLD'  ,EPSDOT_FLD,IS_AVAILABLE,LSUBMODEL,UNITAB)   
      CALL HM_GET_INTV   ('MAT_NSTEP'    ,NSTEP     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Ismooth'      ,Ismooth   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Istrain'      ,Istrain   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_THIN'     ,THIN      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (THIN /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        CRIT = IBSET(CRIT,17)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Thinning failure"
      ENDIF 
!---------------
! -> Card6
!---------------
      CALL HM_GET_INTV   ('FAILIP'       ,FAILIP    ,IS_AVAILABLE,LSUBMODEL)
      IF (FAILIP == 0)  FAILIP  = 1
      CALL HM_GET_FLOATV ('MAT_VOLFRAC'  ,VOLFRAC   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Pthk'         ,PTHK      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('MAT_NCS'      ,NCS       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_MAXTEMP'  ,MAXTEMP   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MAXTEMP /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1      
        CRIT = IBSET(CRIT,18)
        NMODE = NMODE + 1 
        MODNAME(NMODE) = "Maximum temperature"
      ENDIF
!---------------
! -> Card7
!---------------
      CALL HM_GET_INTV   ('fct_IDel'     ,fct_IDel  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Fscale_el'    ,Fscale_el ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('El_ref'       ,El_ref    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C======================================================================================
C DEFAULT VALUES
C======================================================================================
      ! Card 1
      MINPRES = -ABS(MINPRES)
      IF (MINPRES == ZERO) MINPRES = -INFINITY
      MAXPRES = ABS(MAXPRES)
      IF (MAXPRES == ZERO) MAXPRES = INFINITY
      IF (SIGP1   == ZERO) SIGP1   = INFINITY
      IF (TMAX    == ZERO) TMAX    = INFINITY
      IF (DTMIN   == ZERO) DTMIN   = -INFINITY
      ! Card 2 
      IF (fct_IDsm > 0) THEN
        IF (EPSDOT_SM == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_EPSDSM' ,FREQ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          EPSDOT_SM = ONE*FREQ_UNIT
        ENDIF
        IF (SIGVM == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_SIGVM' ,PRESSURE_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          SIGVM = ONE*PRESSURE_UNIT
        ENDIF
      ELSE
        IF (SIGVM == ZERO) SIGVM = INFINITY
      ENDIF
      IF (SIGTH == ZERO) SIGTH = INFINITY
      IF (KF    == ZERO) KF    = INFINITY
      ! Card 3 
      IF (fct_IDps > 0) THEN
        IF (EPSDOT_PS == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_EPSDPS' ,FREQ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          EPSDOT_PS = ONE*FREQ_UNIT
        ENDIF        
        IF (MAXEPS == ZERO) THEN
          MAXEPS = ONE
        ENDIF
      ELSE
        IF (MAXEPS == ZERO) MAXEPS = TWO*INFINITY
      ENDIF
      IF (EFFEPS == ZERO) EFFEPS = INFINITY
      IF (VOLEPS == ZERO) VOLEPS = INFINITY
      ! Card 4 
      MINEPS = -ABS(MINEPS)
      IF (MINEPS == ZERO) MINEPS = -INFINITY
      IF (EPSSH  == ZERO) EPSSH  = INFINITY
      ! Card 5
      IF (tab_IDfld > 0) THEN 
        IF (EPSDOT_FLD == ZERO) THEN 
          CALL HM_GET_FLOATV_DIM('MAT_EPSDFLD' ,FREQ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
          EPSDOT_FLD = ONE*FREQ_UNIT
        ENDIF
      ENDIF
      IF (NSTEP   == 0)    NSTEP   = 10
      IF (Ismooth == 0)    Ismooth = 1
      IF (THIN    == ZERO) THIN    = -INFINITY
      ! Card 6 
      IF (VOLFRAC == ZERO) VOLFRAC = HALF
      VOLFRAC = MIN(VOLFRAC,ONE)
      VOLFRAC = MAX(ZERO,VOLFRAC)
      IF (PTHK == ZERO) PTHK = ONE-EM06
      PTHK = MIN(PTHK, ONE)
      PTHK = MAX(PTHK,-ONE)
      IF (NCS     == 0   ) NCS     = 1
      IF (NCS > NCRIT_MAX) THEN
        IF (NCRIT_MAX > 0) THEN 
          CALL ANCMSG(MSGID=2043,MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,I1=MAT_ID,
     .                C1=TITR,I2=NCS,I3=NCRIT_MAX,I4=NCRIT_MAX)   
          NCS = NCRIT_MAX
        ELSE 
          CALL ANCMSG(MSGID=3040,MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,I1=MAT_ID,
     .                C1=TITR)             
        ENDIF
      ENDIF 
      NCS = MAX(1,NCS)
      IF (MAXTEMP == ZERO) MAXTEMP = INFINITY
      ! Card 7 
      IF (Fscale_el == ZERO) Fscale_el = ONE
      IF (((fct_IDel > 0).OR.(fct_IDg12 > 0).OR.(fct_IDg13 > 0).OR.(fct_IDe1c > 0)
     .    .OR.(tab_IDfld > 0.AND.Itab == 2)).AND.(El_ref == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('El_ref' ,LENGTH_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        El_ref = ONE*LENGTH_UNIT
      ENDIF
C======================================================================================
c     Filling buffer tables
c-----------------------------------------------------    
      NUVAR   = 8 
      NUPARAM = 23
      NFUNC   = 6
      NTABLE  = 0
      IF (tab_IDfld > 0) NTABLE = 1
c
      FAIL%KEYWORD = 'GENE1' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = NUPARAM
      FAIL%NIPARAM = 6
      FAIL%NUVAR   = NUVAR
      FAIL%NFUNC   = NFUNC
      FAIL%NTABLE  = NTABLE
      FAIL%NMOD    = NMODE
      FAIL%PTHK    = PTHK
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      ! Modes of failure
      FAIL_TAG%LF_DAMMX = FAIL_TAG%LF_DAMMX + FAIL%NMOD
      ALLOCATE (FAIL%MODE(FAIL%NMOD))
      DO I = 1,FAIL%NMOD
        FAIL%MODE(I) = MODNAME(I)
      ENDDO 
c
      FAIL%IFUNC(1) = fct_IDsm
      FAIL%IFUNC(2) = fct_IDps
      FAIL%IFUNC(3) = fct_IDg12
      FAIL%IFUNC(4) = fct_IDg13
      FAIL%IFUNC(5) = fct_IDe1c
      FAIL%IFUNC(6) = fct_IDel
      IF (NTABLE == 1) FAIL%TABLE(1) = tab_IDfld
c
      FAIL%IPARAM(1)  = CRIT
      FAIL%IPARAM(2)  = Itab
      FAIL%IPARAM(3)  = NSTEP
      FAIL%IPARAM(4)  = NCS
      FAIL%IPARAM(5)  = Ismooth
      FAIL%IPARAM(6)  = Istrain
c
      FAIL%UPARAM(1)  = MINPRES
      FAIL%UPARAM(2)  = MAXPRES
      FAIL%UPARAM(3)  = SIGP1
      FAIL%UPARAM(4)  = TMAX
      FAIL%UPARAM(5)  = DTMIN
      FAIL%UPARAM(6)  = EPSDOT_SM
      FAIL%UPARAM(7)  = SIGVM
      FAIL%UPARAM(8)  = SIGTH
      FAIL%UPARAM(9)  = KF
      FAIL%UPARAM(10) = EPSDOT_PS
      FAIL%UPARAM(11) = MAXEPS
      FAIL%UPARAM(12) = EFFEPS
      FAIL%UPARAM(13) = VOLEPS
      FAIL%UPARAM(14) = MINEPS
      FAIL%UPARAM(15) = EPSSH
      FAIL%UPARAM(16) = EPSDOT_FLD
      FAIL%UPARAM(17) = THIN
      FAIL%UPARAM(18) = VOLFRAC
      FAIL%UPARAM(19) = PTHK
      FAIL%UPARAM(20) = MAXTEMP
      FAIL%UPARAM(21) = Fscale_el
      FAIL%UPARAM(22) = El_ref
      FAIL%UPARAM(23)= FAILIP
c--------------------------
c     Printout data
c-------------------------- 
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        ! Title of the failure criterion
        WRITE(IOUT,1000)
        ! Number of criteria setup in the input
        WRITE(IOUT,1001) NCRIT_MAX
        ! Number of criteria to reach for element deletion
        WRITE(IOUT,1002) NCS
        ! Definition of the criteria
        WRITE(IOUT,1003)
        ! Card 1
        IF (BTEST(CRIT,1)) WRITE(IOUT,1100) MINPRES
        IF (BTEST(CRIT,2)) WRITE(IOUT,1101) MAXPRES
        IF (BTEST(CRIT,1) .OR. BTEST(CRIT,2)) WRITE(IOUT,1105)
        IF (BTEST(CRIT,3)) WRITE(IOUT,1102) SIGP1
        IF (BTEST(CRIT,4)) WRITE(IOUT,1103) TMAX
        IF (BTEST(CRIT,5)) WRITE(IOUT,1104) DTMIN
        ! Card 2 
        IF (BTEST(CRIT,6)) THEN 
          IF (fct_IDsm > 0) THEN 
            WRITE(IOUT,2000) fct_IDsm,EPSDOT_SM,SIGVM
          ELSE
            WRITE(IOUT,2100) SIGVM
          ENDIF
        ENDIF
        IF (BTEST(CRIT,7)) WRITE(IOUT,2001) SIGTH,KF
        ! Card 3
        IF (BTEST(CRIT,8)) THEN 
          IF (fct_IDps > 0) THEN 
            WRITE(IOUT,3000) fct_IDps,EPSDOT_PS,MAXEPS
          ELSE
            WRITE(IOUT,3100) MAXEPS
          ENDIF
        ENDIF
        IF (BTEST(CRIT,9))  WRITE(IOUT,3001) EFFEPS
        IF (BTEST(CRIT,10)) WRITE(IOUT,3002) VOLEPS
        ! Card 4
        IF (BTEST(CRIT,11))  WRITE(IOUT,4000) MINEPS
        IF (BTEST(CRIT,12))  WRITE(IOUT,4001) EPSSH
        IF (BTEST(CRIT,13)) WRITE(IOUT,4100) fct_IDg12
        IF (BTEST(CRIT,14)) WRITE(IOUT,4200) fct_IDg13
        IF (BTEST(CRIT,15)) WRITE(IOUT,4300) fct_IDe1c
        ! Card 5
        IF (BTEST(CRIT,16)) THEN 
          WRITE(IOUT,5000) tab_IDfld,Itab,EPSDOT_FLD,
     .                     Ismooth,Istrain
        ENDIF
        IF (BTEST(CRIT,17)) WRITE(IOUT,5002) THIN
        ! Card 6
        IF (BTEST(CRIT,18)) WRITE(IOUT,6001) MAXTEMP
        ! Card 7
        IF (fct_IDel > 0) WRITE(IOUT,7000) fct_IDel,Fscale_el
        IF (((fct_IDel > 0).OR.(fct_IDg12 > 0).OR.(fct_IDg13 > 0).OR.(fct_IDe1c > 0)
     .    .OR.(tab_IDfld > 0.AND.Itab == 2))) WRITE(IOUT,7100) El_ref
        ! Element deletion properties
        WRITE(IOUT,7101)
        WRITE(IOUT,6000) VOLFRAC,PTHK,FAILIP
        WRITE(IOUT,5001) NSTEP
        WRITE(IOUT,8000)
      ENDIF
c-----------------------------------------------------
 1000 FORMAT(
     & 5X,' ----------------------------------------------------',/,
     & 5X,'              FAILURE CRITERION : GENE1              ',/,
     & 5X,' ----------------------------------------------------',/)
 1001 FORMAT(
     & 5X,'NUMBER OF CRITERIA SPECIFIED IN THE INPUT. . . . . .=',I10,/)
 1002 FORMAT(
     & 5X,'NUMBER OF CRITERIA TO REACH FOR ELEMENT DELETION . .=',I10,/)
 1003 FORMAT(
     & 5X,'SPECIFIED ELEMENT DELETION CRITERIA:',/,
     & 5X,'------------------------------------',/)
 1100 FORMAT(
     & 5X,'MINIMUM PRESSURE . . . . . . . . . . . . . . . . . .=',1PG20.13,/)
 1101 FORMAT(
     & 5X,'MAXIMUM PRESSURE . . . . . . . . . . . . . . . . . .=',1PG20.13,/) 
 1102 FORMAT(        
     & 5X,'MAXIMUM PRINCIPAL STRESS . . . . . . . . . . . . . .=',1PG20.13,/
     & 5X,'   < 0.0 : RESTRICTED TO POSITIVE STRESS TRIAXIALITIES',/, 
     & 5X,'   > 0.0 : UNRESTRICTED                               ',/)
 1103 FORMAT(
     & 5X,'FAILURE TIME . . . . . . . . . . . . . . . . . . . .=',1PG20.13,/)
 1104 FORMAT(
     & 5X,'MINIMUM TIME STEP  . . . . . . . . . . . . . . . . .=',1PG20.13,/)
 1105 FORMAT(
     & 5X,'NOTE : PRESSURE IS DEFINED SO THAT IT IS POSITIVE IN COMPRESSION',/)
 2000 FORMAT(
     & 5X,'FUNCTION ID FOR EQ. STRESS VS STRAIN-RATE  . . . . .=',I10,/,
     & 5X,'REFERENCE STRAIN-RATE  . . . . . . . . . . . . . . .=',1PG20.13,/
     & 5X,'EQUIVALENT STRESS SCALE FACTOR . . . . . . . . . . .=',1PG20.13,/)
 2001 FORMAT(
     & 5X,'INITIAL STRESS FOR TULER-BUTCHER CRITERION . . . . .=',1PG20.13,/
     & 5X,'CRITICAL TULER-BUTCHER INTEGRAL VALUE  . . . . . . .=',1PG20.13,/)
 2100 FORMAT(
     & 5X,'MAXIMUM EQUIVALENT STRESS  . . . . . . . . . . . . .=',1PG20.13,/)
 3000 FORMAT(
     & 5X,'FUNCTION ID FOR PRINC. STRAIN VS STRAIN-RATE . . . .=',I10,/,
     & 5X,'REFERENCE STRAIN-RATE  . . . . . . . . . . . . . . .=',1PG20.13,/
     & 5X,'PRINCIPAL STRAIN SCALE FACTOR  . . . . . . . . . . .=',1PG20.13,/)
 3001 FORMAT(
     & 5X,'MAXIMUM EFFECTIVE STRAIN . . . . . . . . . . . . . .=',1PG20.13,/)
 3002 FORMAT(
     & 5X,'MAXIMUM VOLUMETRIC STRAIN  . . . . . . . . . . . . .=',1PG20.13,/)
 3100 FORMAT(
     & 5X,'MAXIMUM PRINCIPAL STRAIN . . . . . . . . . . . . . .=',1PG20.13,/)
 4000 FORMAT(
     & 5X,'MINIMUM PRINCIPAL STRAIN . . . . . . . . . . . . . .=',1PG20.13,/)
 4001 FORMAT(
     & 5X,'TENSORIAL SHEAR STRAIN AT FAILURE. . . . . . . . . .=',1PG20.13,/)
 4100 FORMAT(
     & 5X,'FUNCTION ID FOR MAX.IN-PLANE SHEAR VS EL.SIZE  . . .=',I10,/)
 4200 FORMAT(
     & 5X,'FUNCTION ID FOR MAX.TRANSVERSE SHEAR VS EL.SIZE  . .=',I10,/)
 4300 FORMAT(
     & 5X,'FUNCTION ID FOR IN-PLANE MAJOR STRAIN VS EL.SIZE . .=',I10,/)
 5000 FORMAT(
     & 5X,'TABLE ID FOR FORMING LIMIT DIAGRAM . . . . . . . . .=',I10,/,
     & 5X,'FLAG FOR TABLE DEPENDENCY  . . . . . . . . . . . . .=',I10,/,
     & 5X,'   = 1 : FORMING LIMIT DIAGRAM VS STRAIN-RATE        ',/,
     & 5X,'   = 2 : FORMING LIMIT DIAGRAM VS ELEMENT SIZE       ',/,
     & 5X,'REFERENCE STRAIN-RATE  . . . . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'TABULATED FUNCTION INTERPOLATION FLAG  . . . . . . .=',I10,/,
     & 5X,'ENGINEERING / TRUE INPUT STRAIN FLAG . . . . . . . .=',I10,/)
 5001 FORMAT(
     & 5X,'NUMBER OF CYCLES BEFORE ELEMENT DELETION . . . . . .=',I10,/)
 5002 FORMAT(
     & 5X,'SHELL THINNING AT FAILURE  . . . . . . . . . . . . .=',1PG20.13,/)
 6000 FORMAT(
     & 5X,'VOLUME FRACTION FOR SOLID ELEMENT DELETION . . . . .=',1PG20.13,/,
     & 5X,'SHELL ELEMENT DELETION PARAMETER PTHICKFAIL. . . . .=',1PG20.13,/,
     & 5X,'  > 0.0 : FRACTION OF FAILED THICKNESS               ',/,
     & 5X,'  < 0.0 : FRACTION OF FAILED INTG. POINTS OR LAYERS  ',/,
     & 5X,'NUMBER OF FAILED INTG. POINTS PRIOR TO ELEM DELETION  .=',I10/)
 6001 FORMAT(
     & 5X,'MAXIMUM TEMPERATURE  . . . . . . . . . . . . . . . .=',1PG20.13,/)
 7000 FORMAT(
     & 5X,'REGULARIZATION FUNCTION ID . . . . . . . . . . . . .=',I10,/,
     & 5X,'SCALE FACTOR FOR REGULARIZATION FUNCTION . . . . . .=',1PG20.13,/)
 7100 FORMAT(
     & 5X,'REFERENCE ELEMENT LENGTH . . . . . . . . . . . . . .=',1PG20.13,/)
 7101 FORMAT(
     & 5X,'ELEMENT DELETION PROPERTIES:',/,
     & 5X,'----------------------------',/)
 8000 FORMAT(
     & 5X,' ----------------------------------------------------',//)
c----------- 
      END
