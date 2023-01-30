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
Chd|  HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT36(UPARAM   ,MAXUPARAM,NUPARAM   ,NUVAR  ,NVARTMP  ,
     .                         IFUNC    ,MAXFUNC  ,MFUNC     , PARMAT,UNITAB   ,
     .                         ID       ,MTAG     ,TITR     , LSUBMODEL,PM     ,
     .                         ISRATE   )                     
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
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
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: MFUNC,NUPARAM,NUVAR,NVARTMP,ISRATE
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: I,J,VP,YLDCHECK
      INTEGER :: RHOFLAG,ICOMP,NRATE1,NRATE,IPFUN,IFUNCE,ISRAT,ISMOOTH,
     .           NBLINE,NBREAD,IFAIL,OPTE,ILAW,NFUNC
      my_real :: RHO0, RHOR,E,NU,G,C1,SOUNDSP, EPSMAX,EPSR1,EPSR2,EPSF,FISOKIN,FCUT,
     .           PSCAL_UNIT,PSCALE,EINF,CE ,
     .           YFAC(MAXFUNC),RATE(MAXFUNC),STRAINRATE_UNIT(MAXFUNC),YFAC_UNIT(MAXFUNC)
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
      ILAW    = 36
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
Card1
      CALL HM_GET_FLOATV('MAT_E'    ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'  ,EPSMAX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST1',EPSR1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2',EPSR2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
Card2
      CALL HM_GET_INTV  ('NFUNC'        ,NRATE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Fsmooth'      ,ISMOOTH ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_HARD'     ,FISOKIN ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fcut'         ,FCUT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Epsilon_F',EPSF    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Vflag'        ,VP      ,IS_AVAILABLE,LSUBMODEL)
Card3   
      CALL HM_GET_INTV  ('Xr_fun'       ,IPFUN   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_FScale'   ,PSCALE  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Yr_fun'       ,IFUNCE  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EFIB'     ,EINF    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'        ,CE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
      ! Poisson's ratio check
      IF (NU < ZERO .OR. NU >= HALF) THEN
        CALL ANCMSG(MSGID=49,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU,
     .              I1=ID,
     .              C1=TITR)
      ENDIF
c
      IF(NRATE > 100)THEN
        CALL ANCMSG(MSGID=215, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=36,
     .               I2=ID,
     .               C1=TITR)
      ELSEIF (NRATE <= 0) THEN
        CALL ANCMSG(MSGID=740, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR)
      ENDIF
c
      IF (IPFUN == 0) THEN
        PSCALE = ZERO
      ELSEIF (PSCALE == ZERO) THEN
        !units
        CALL HM_GET_FLOATV_DIM('MAT_FScale' ,PSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        PSCALE = ONE * PSCAL_UNIT
      ELSE
        PSCALE = ONE /PSCALE
      ENDIF
c------------------------
      IF (NRATE > 0) THEN
        DO J=1,NRATE
          CALL HM_GET_INT_ARRAY_INDEX ('FUN_LOAD',IFUNC(J),J,IS_AVAILABLE,LSUBMODEL)
        ENDDO
c
        DO J=1,NRATE
          CALL HM_GET_FLOAT_ARRAY_INDEX    ('SCALE_LOAD',YFAC(J)     ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          IF(YFAC(J) == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_LOAD',YFAC_UNIT(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB) 
            YFAC(J)=ONE * YFAC_UNIT(J)
          ENDIF
        ENDDO
c
        DO J=1,NRATE
          CALL HM_GET_FLOAT_ARRAY_INDEX    ('STRAINRATE_LOAD',RATE(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
c
        DO I=1,NRATE-1                        
	         IF (RATE(I) >= RATE(I+1)) THEN        
            CALL ANCMSG(MSGID=478, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR)
            EXIT                         
          ENDIF                               
        ENDDO                                 
        DO I=1,NRATE
          IF (IFUNC(I) == 0) THEN
            CALL ANCMSG(MSGID=126, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IFUNC(I))
          ENDIF
        ENDDO
      ENDIF
c-------------------------
      IF (NRATE == 1) THEN
        NFUNC  = 1
        ISMOOTH= 0
        ISRAT  = 0
        FCUT   = ZERO
        VP     = 0   !!!  no plastic strain rate dependency with single static curve
      ELSE    ! NRATE > 1
        ISRAT = 1
        IF (RATE(1) == ZERO) THEN
          NFUNC = NRATE
        ELSE 
          NFUNC = NRATE+1
          DO J=NRATE,1,-1
            IFUNC(J+1) =IFUNC(J)
            RATE (J+1) =RATE(J)
            YFAC (J+1) =YFAC(J)
          ENDDO
          RATE(1) = ZERO
        ENDIF
c
        IF (FCUT == ZERO .or. VP == 1) THEN
          FCUT = 10000.0D0*FAC_T_WORK
        END IF 
      ENDIF
      ISRATE = MAX(ISRATE,ISRAT) 
c
      IF (NU == HALF) NU = ZEP499
      MFUNC = NFUNC + 1
      IFUNC(MFUNC) = IPFUN
c-----------------------------------------------
      IF (FISOKIN > ONE .OR. FISOKIN < ZERO) THEN
        CALL ANCMSG(MSGID=912, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .              I1=ID,C1='36',
     .              C2=TITR)
      END IF
C      
      IF (EPSR1 == ZERO .AND. EPSR2 == ZERO .AND. EPSF == ZERO) THEN
        IF (EPSMAX == ZERO) THEN
          IFAIL = 0
        ELSE
          IFAIL = 1
        END IF
      ELSE 
        IFAIL = 2
      ENDIF
c     IFAIL = 0 => no failure at all inside material
c     IFAIL = 1 => only failure vs max plastic strain
c     IFAIL = 2 => failure + damage vs principal tensile strain
c
      IF (IFAIL > 0) THEN 
        MTAG%G_DMG = 1
        MTAG%L_DMG = 1        
      ENDIF
      IF (EPSMAX== ZERO) EPSMAX= INFINITY
      IF (EPSR1 == ZERO) EPSR1 = INFINITY
      IF (EPSR2 == ZERO) EPSR2 = TWO*INFINITY
      IF (EPSF  == ZERO) EPSF  = THREE*INFINITY       
c     Limit max failure values
      EPSMAX = MIN(EPSMAX  ,INFINITY)
      EPSR1  = MIN(EPSR1   ,INFINITY)
      EPSR2  = MIN(EPSR2   ,TWO*INFINITY)
      EPSF   = MIN(EPSF    ,THREE*INFINITY)
c      
      IF (EPSR1 /= ZERO .AND. EPSR2 /= ZERO) THEN 
        IF (EPSR1 >= EPSR2) THEN
         CALL ANCMSG(MSGID=480, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR)
        ENDIF
      ENDIF
C------------------------------
      G = HALF*E/(ONE+NU)
      C1= E/THREE/(ONE - TWO*NU)
      SOUNDSP  = SQRT((C1 + FOUR_OVER_3*G)/RHO0)
      YLDCHECK = 0           ! check if yld function decreases to zero (set in law36_upd.F)
      OPTE = 0
C------------------------------
C------UPARAM STORAGE----------
C------------------------------
      UPARAM(1)= NFUNC
      UPARAM(2)= E
      UPARAM(3)= E/(ONE - NU*NU)
      UPARAM(4)= NU*UPARAM(3)
C---
      UPARAM(5)= G
      UPARAM(6)= NU
      DO J=1,NFUNC
        UPARAM(6 + J)= RATE(J)
      ENDDO
      DO J=1,NFUNC
        UPARAM(NFUNC + 6+J)= YFAC(J)
      ENDDO
C---
      UPARAM(2*NFUNC + 7) = EPSMAX
      UPARAM(2*NFUNC + 8) = EPSR1
      UPARAM(2*NFUNC + 9) = EPSR2
      UPARAM(2*NFUNC + 10)= TWO*G
      UPARAM(2*NFUNC + 11)= THREE*G
      UPARAM(2*NFUNC + 12)= C1
      UPARAM(2*NFUNC + 13)= SOUNDSP  ! soundspeed solids
      UPARAM(2*NFUNC + 14)= FISOKIN
      UPARAM(2*NFUNC + 15)= EPSF
      IF (IPFUN == 0) THEN
        UPARAM(2*NFUNC + 16) = 0
      ELSE
        UPARAM(2*NFUNC + 16) = MFUNC 
      ENDIF
      UPARAM(2*NFUNC + 17) = PSCALE 
c sound speed coque
      UPARAM(2*NFUNC + 18) = SQRT(E/(ONE - NU*NU)/RHO0)  ! soundspeed shells
      UPARAM(2*NFUNC + 19) = NU / (ONE-NU)
      UPARAM(2*NFUNC + 20) = THREE / (ONE+NU)
      UPARAM(2*NFUNC + 21) = ONE / (ONE-NU)
c -----------------------
      IF (IFUNCE > 0 ) OPTE = 1
      MFUNC = MFUNC + 1 
      IFUNC(MFUNC) = IFUNCE
      UPARAM(2*NFUNC + 22) = MFUNC 
      UPARAM(2*NFUNC + 23) = OPTE 
      UPARAM(2*NFUNC + 24) = EINF 
      UPARAM(2*NFUNC + 25) = CE 
      UPARAM(2*NFUNC + 26) = VP
      UPARAM(2*NFUNC + 27) = IFAIL
      UPARAM(2*NFUNC + 28) = YLDCHECK
      UPARAM(2*NFUNC + 29) = ISMOOTH
c
      NUPARAM = 2*NFUNC + 29
c--------------------------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0    
      PM(27)= SQRT(E/RHO0)  ! Sound speed for beam elements
c--------------------------------
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
      !PARMAT(6) = NOFAIL
      PARMAT(7) = EPSR1
      PARMAT(8) = EPSR2
      PARMAT(9) = EPSF 
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(C1+FOUR_OVER_3*G) ! == (1-2*nu)/(1-nu)
C----------------
      NUVAR = 0
      IF (VP == 1) THEN
        NUVAR = 3
      ENDIF
      NVARTMP = 2+ NFUNC
C-----------------------      
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      IF (FISOKIN /= ZERO) THEN
        MTAG%L_SIGB = 6
      ENDIF

C-----------------------      
      WRITE(IOUT,1001) TRIM(TITR),ID,36  
      WRITE(IOUT,1000)   
      IF (IS_ENCRYPTED)THEN                                
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100) E,NU,EPSMAX,EPSR1,EPSR2,EPSF,FISOKIN,ISRATE,FCUT,VP
        WRITE(IOUT,1200)(IFUNC(J),YFAC(J),RATE(J),J=1,NFUNC)
        WRITE(IOUT,1300) IPFUN,PSCALE, IFUNCE,EINF,CE
        WRITE(IOUT,*)' '
      ENDIF
C-----------        
      RETURN
C-----------
 1000 FORMAT(
     & 5X,'    TABULATED ELASTIC PLASTIC LAW 36    ',/,
     & 5X,'    --------------------------------    ' ,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . .=',1PG20.13/)  
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM PLASTIC STRAIN  . . . . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 1  . . . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 2  . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM TENSION FAILURE STRAIN  . . . . . .=',1PG20.13/     
     & 5X,'ISO-KINEMATIC HARDENING FACTOR. . . . . . .=',1PG20.13/
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . . . .=',I10/
     & 5X,'     0 -> NO SMOOTHING                      ',/,
     & 5X,'     1 -> SMOOTH + LINEAR INTERPOLATION     ',/,
     & 5X,'     2 -> SMOOTH + LOG_10 INTERPOLATION     ',/,
     & 5X,'     3 -> SMOOTH + LOG_N  INTERPOLATION     ',/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . . . .=',1PG20.13/
     & 5X,'PLASTIC STRAIN RATE DEPENDENCY FLAG . . . .=',I10/
     & 5X,'   FLAG_PL = 0 -> TOTAL SR DEPENDENCY       ',/,
     & 5X,'   FLAG_PL = 1 -> PLASTIC SR DEPENDENCY     ',/,
     & 5X,'STRAIN RATE INTERPOLATION FLAG. . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER. . . . . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE . . . . . . . . . . . . . . . .=',1PG20.13)
 1300 FORMAT(
     & 5X,'PRESSURE DEPENDENT YIELD FUNCTION . . . . .=',I10/
     & 5X,'PRESSURE SCALE FACTOR . . . . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS SCALE FACTOR FUNCTION . . . .=',I10/
     & 5X,'YOUNG MODULUS EINF. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'PARAMETER CE. . . . . . . . . . . . . . . .=',1PG20.13)
c-----------
      RETURN
      END
