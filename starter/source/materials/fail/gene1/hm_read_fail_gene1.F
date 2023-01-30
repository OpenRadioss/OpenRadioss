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
Chd|  HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
       SUBROUTINE HM_READ_FAIL_GENE1(
     .            UPARAM  ,MAXUPARAM ,NUPARAM ,NUVAR   ,IFUNC    ,
     .            MAXFUNC ,NFUNC     ,UNITAB  ,FAIL_ID ,MAT_ID   ,
     .            TABLE   ,TITR      ,LSUBMODEL)
C-----------------------------------------------
c    Orthotropic strain  failure model
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE ORTHBIQUAD
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
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN)   :: UNITAB 
      INTEGER,INTENT(IN)             :: MAXUPARAM,MAXFUNC,FAIL_ID,MAT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT)          :: UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            :: NUPARAM,NUVAR
      CHARACTER TITR*nchartitle
      TYPE(TTABLE) TABLE(NTABLE)    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NSTEP,NCS,fct_IDsm,fct_IDps,fct_IDg12,fct_IDg13,
     .        fct_IDe1c,fct_IDel,Itab,tab_IDfld,int_tab_IDfld,Istrain,
     .        Ismooth,NCRIT_MAX,IMINPRES,IMAXPRES,IMAXSIGP,IMAXTIME,
     .        IMINDT,IMAXSIGVM,IMAXKF,IMAXEPS,IEFFEPS,IVOLEPS,IMINEPS,
     .        IEPSSH,IMINTHIN,IMAXTEMP
      my_real 
     .        MINPRES,MAXPRES,SIGP1,TMAX,DTMIN,EPSDOT_SM,SIGVM,SIGTH,KF,
     .        EPSDOT_PS,MAXEPS,EFFEPS,VOLEPS,MINEPS,EPSSH,EPSDOT_FLD,
     .        THIN,VOLFRAC,PTHK,MAXTEMP,Fscale_el,El_ref,FREQ_UNIT,
     .        PRESSURE_UNIT,LENGTH_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ! Initialization of the number of criteria
      NCRIT_MAX    = 0
      ! Initialization of output flags
      IMINPRES     = 0
      IMAXPRES     = 0    
      IMAXSIGP     = 0
      IMAXTIME     = 0
      IMINDT       = 0  
      IMAXSIGVM    = 0
      IMAXKF       = 0
      IMAXEPS      = 0
      IEFFEPS      = 0
      IVOLEPS      = 0
      IMINEPS      = 0
      IEPSSH       = 0
      IMINTHIN     = 0
      IMAXTEMP     = 0
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
      IF (MINPRES /= ZERO) IMINPRES = 1
      CALL HM_GET_FLOATV ('MAT_MAXPRES'  ,MAXPRES   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MAXPRES /= ZERO) IMAXPRES = 1
      IF ((MINPRES /= ZERO).OR.(MAXPRES /= ZERO)) NCRIT_MAX = NCRIT_MAX + 1
      CALL HM_GET_FLOATV ('MAT_SIGP1'    ,SIGP1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (SIGP1 /= ZERO) THEN
        NCRIT_MAX = NCRIT_MAX + 1
        IMAXSIGP  = 1
      ENDIF
      CALL HM_GET_FLOATV ('MAT_TMAX'     ,TMAX      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (TMAX  /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IMAXTIME  = 1
      ENDIF
      CALL HM_GET_FLOATV ('MAT_DTMIN'    ,DTMIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (DTMIN /= ZERO) THEN
        NCRIT_MAX = NCRIT_MAX + 1
        IMINDT    = 1
      ENDIF
!---------------
! -> Card2
!---------------
      CALL HM_GET_INTV   ('fct_IDsm'     ,fct_IDsm  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_EPSDSM'   ,EPSDOT_SM ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_SIGVM'    ,SIGVM     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((SIGVM /= ZERO).OR.(fct_IDsm /= 0)) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IMAXSIGVM = 1
      ENDIF
      CALL HM_GET_FLOATV ('MAT_SIGTH'    ,SIGTH     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_KF'       ,KF        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((SIGTH /= ZERO).OR.(KF /= ZERO)) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IMAXKF    = 1
      ENDIF
!---------------
! -> Card3
!---------------
      CALL HM_GET_INTV   ('fct_IDps'     ,fct_IDps  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_EPSDPS'   ,EPSDOT_PS ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_MAXEPS'   ,MAXEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((MAXEPS /= ZERO).OR.(fct_IDps /= 0)) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IMAXEPS   = 1
      ENDIF
      CALL HM_GET_FLOATV ('MAT_EFFEPS'   ,EFFEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (EFFEPS /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IEFFEPS   = 1
      ENDIF
      CALL HM_GET_FLOATV ('MAT_VOLEPS'   ,VOLEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (VOLEPS /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IVOLEPS   = 1
      ENDIF
!---------------
! -> Card4
!---------------
      CALL HM_GET_FLOATV ('MAT_MINEPS'   ,MINEPS    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MINEPS /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IMINEPS   = 1
      ENDIF
      CALL HM_GET_FLOATV ('MAT_EPSSH'    ,EPSSH     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (EPSSH  /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IEPSSH    = 1
      ENDIF
      CALL HM_GET_INTV   ('fct_IDg12'    ,fct_IDg12 ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('fct_IDg13'    ,fct_IDg13 ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('fct_IDe1c'    ,fct_IDe1c ,IS_AVAILABLE,LSUBMODEL)
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
      IF (tab_IDfld /= 0) NCRIT_MAX = NCRIT_MAX + 1
      CALL HM_GET_INTV   ('Itab'         ,Itab      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_EPSDFLD'  ,EPSDOT_FLD,IS_AVAILABLE,LSUBMODEL,UNITAB)   
      CALL HM_GET_INTV   ('MAT_NSTEP'    ,NSTEP     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Ismooth'      ,Ismooth   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Istrain'      ,Istrain   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_THIN'     ,THIN      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (THIN /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1
        IMINTHIN  = 1
      ENDIF 
!---------------
! -> Card6
!---------------
      CALL HM_GET_FLOATV ('MAT_VOLFRAC'  ,VOLFRAC   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Pthk'         ,PTHK      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('MAT_NCS'      ,NCS       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_MAXTEMP'  ,MAXTEMP   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (MAXTEMP /= ZERO) THEN 
        NCRIT_MAX = NCRIT_MAX + 1      
        IMAXTEMP  = 1
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
      IF (DTMIN   == ZERO) DTMIN   = EM20
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
      IF (PTHK    == ZERO) PTHK    = ONE
      PTHK = MIN(PTHK,ONE)
      PTHK = MAX(ZERO,PTHK)
      IF (NCS     == 0   ) NCS     = 1
      IF (NCS > NCRIT_MAX) THEN
        CALL ANCMSG(MSGID=2043,MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,
     .              C1=TITR,I2=NCS,I3=NCRIT_MAX,I4=NCRIT_MAX)   
        NCS = NCRIT_MAX
      ENDIF 
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
C======================================================================================
      ! -> Number of parameters
      NUPARAM = 32
      ! -> Number of functions
      NFUNC   = 0
      IF (fct_IDsm /= 0) THEN
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = fct_IDsm
      ENDIF
      IF (fct_IDps /= 0) THEN
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = fct_IDps
      ENDIF
      IF (fct_IDg12 /= 0) THEN
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = fct_IDg12
      ENDIF                
      IF (fct_IDg13 /= 0) THEN
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = fct_IDg13
      ENDIF 
      IF (fct_IDe1c /= 0) THEN
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = fct_IDe1c
      ENDIF 
      IF (fct_IDel /= 0) THEN
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = fct_IDel
      ENDIF
      int_tab_IDfld = tab_IDfld
      IF (tab_IDfld > 0) THEN
        CALL MATTAB_USR2SYS(TITR,MAT_ID,TABLE,1,int_tab_IDfld)
      ENDIF
      ! -> Number of user variables
      NUVAR = 8 
! c-----------------------------------------------------    
      UPARAM(1)  = MINPRES
      UPARAM(2)  = MAXPRES
      UPARAM(3)  = SIGP1
      UPARAM(4)  = TMAX
      UPARAM(5)  = DTMIN
      UPARAM(6)  = EPSDOT_SM
      UPARAM(7)  = SIGVM
      UPARAM(8)  = SIGTH
      UPARAM(9)  = KF
      UPARAM(10) = EPSDOT_PS
      UPARAM(11) = MAXEPS
      UPARAM(12) = EFFEPS
      UPARAM(13) = VOLEPS
      UPARAM(14) = MINEPS
      UPARAM(15) = EPSSH
      UPARAM(16) = int_tab_IDfld
      UPARAM(17) = Itab
      UPARAM(18) = EPSDOT_FLD
      UPARAM(19) = NSTEP
      UPARAM(20) = THIN
      UPARAM(21) = VOLFRAC
      UPARAM(22) = PTHK
      UPARAM(23) = NCS
      UPARAM(24) = MAXTEMP
      UPARAM(25) = Fscale_el
      UPARAM(26) = El_ref
      UPARAM(27) = fct_IDg12
      UPARAM(28) = fct_IDg13
      UPARAM(29) = fct_IDe1c
      UPARAM(30) = fct_IDel
      UPARAM(31) = Ismooth
      UPARAM(32) = Istrain
c-----------------------------------------------------
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
        IF (IMINPRES == 1) WRITE(IOUT,1100) MINPRES
        IF (IMAXPRES == 1) WRITE(IOUT,1101) MAXPRES
        IF (IMINPRES == 1 .OR. IMAXPRES == 1) WRITE(IOUT,1105)
        IF (IMAXSIGP == 1) WRITE(IOUT,1102) SIGP1
        IF (IMAXTIME == 1) WRITE(IOUT,1103) TMAX
        IF (IMINDT   == 1) WRITE(IOUT,1104) DTMIN
        ! Card 2 
        IF (IMAXSIGVM == 1) THEN 
          IF (fct_IDsm > 0) THEN 
            WRITE(IOUT,2000) fct_IDsm,EPSDOT_SM,SIGVM
          ELSE
            WRITE(IOUT,2100) SIGVM
          ENDIF
        ENDIF
        IF (IMAXKF == 1) WRITE(IOUT,2001) SIGTH,KF
        ! Card 3
        IF (IMAXEPS == 1) THEN 
          IF (fct_IDps > 0) THEN 
            WRITE(IOUT,3000) fct_IDps,EPSDOT_PS,MAXEPS
          ELSE
            WRITE(IOUT,3100) MAXEPS
          ENDIF
        ENDIF
        IF (IEFFEPS == 1) WRITE(IOUT,3001) EFFEPS
        IF (IVOLEPS == 1) WRITE(IOUT,3002) VOLEPS
        ! Card 4
        IF (IMINEPS == 1)  WRITE(IOUT,4000) MINEPS
        IF (IEPSSH  == 1)  WRITE(IOUT,4001) EPSSH
        IF (fct_IDg12 > 0) WRITE(IOUT,4100) fct_IDg12
        IF (fct_IDg13 > 0) WRITE(IOUT,4200) fct_IDg13
        IF (fct_IDe1c > 0) WRITE(IOUT,4300) fct_IDe1c
        ! Card 5
        IF (tab_IDfld > 0) THEN 
          WRITE(IOUT,5000) tab_IDfld,Itab,EPSDOT_FLD,
     .                     Ismooth,Istrain
        ENDIF
        IF (IMINTHIN == 1) WRITE(IOUT,5002) THIN
        ! Card 6
        IF (IMAXTEMP == 1) WRITE(IOUT,6001) MAXTEMP
        ! Card 7
        IF (fct_IDel > 0) WRITE(IOUT,7000) fct_IDel,Fscale_el
        IF (((fct_IDel > 0).OR.(fct_IDg12 > 0).OR.(fct_IDg13 > 0).OR.(fct_IDe1c > 0)
     .    .OR.(tab_IDfld > 0.AND.Itab == 2))) WRITE(IOUT,7100) El_ref
        ! Element deletion properties
        WRITE(IOUT,7101)
        WRITE(IOUT,6000) VOLFRAC,PTHK
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
     & 5X,'SHELL THICKNESS PERCENTAGE OF FAILURE  . . . . . . .=',1PG20.13,/)
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
