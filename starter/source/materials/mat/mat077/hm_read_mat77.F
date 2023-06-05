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
Chd|  HM_READ_MAT77                 source/materials/mat/mat077/hm_read_mat77.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT77(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,PARMAT   ,MAT_ID   ,PM       ,
     .           ISRATE   ,IMATVIS  ,TITR     ,UNITAB   ,LSUBMODEL,
     .           MATPARAM ,JALE     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD 
      USE MATPARAM_DEF_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW77 WITH HM READER
C   this law is Ale law for air and foam is in lagrangian and is not activated thru ale option. 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)  :: NUPARAM,NUVAR,NFUNC,ISRATE,IMATVIS,JALE
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)   :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,ILAW,NRATEN,NRATEP,LFUNC,IUNLOAD,IFUNCR,IFUNCK,
     .       IFLAG,NPAR_FOAM,ICLOS,INCGAS
      my_real :: E,NU,G,RHOA,RHO0,RHOR,VISC,VISCV,EXPO,HYS,FRAC,
     .    FCUT, A1,A2,E0,EMAX,EPSMAX,P0,AA,BB,KK,TAUX,BULK,EINT0,
     .    GAMMA,PEXT,FP_INI,RHOEXT,EINT_EXT,FSCAL_UNIT
      INTEGER ,DIMENSION(15) :: FLOAD,FUNLOAD
      my_real ,DIMENSION(30) :: RATE,YFAC,RLOAD,SLOAD,RUNLOAD,SUNLOAD
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      ILAW    = 77
      IMATVIS = 2
      JALE    = 3
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
c     foam input cards
c
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      
      CALL HM_GET_FLOATV('MAT_E'      ,E0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('E_Max'      ,EMAX    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'    ,EPSMAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FP0'    ,FP_INI  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      
      CALL HM_GET_FLOATV('MAT_asrate' ,FCUT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISRATE'     ,ISRATE  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('NRATEP'     ,NRATEP  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('NRATEN'     ,NRATEN  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Iflag'  ,IUNLOAD ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_SHAPE'  ,EXPO    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HYST'   ,HYS     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      IF (NRATEP > 0) THEN
        DO I=1,NRATEP    
          CALL HM_GET_INT_ARRAY_INDEX  ('FUN_LOAD'       ,FLOAD(I),I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_LOAD',RLOAD(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_LOAD'     ,SLOAD(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
      ENDIF
c
      IF (NRATEN > 0) THEN
        DO I=1,NRATEN    
          CALL HM_GET_INT_ARRAY_INDEX  ('FUN_UNLOAD'       ,FUNLOAD(I),I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_UNLOAD',RUNLOAD(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_UNLOAD'     ,SUNLOAD(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
      ENDIF
c
c     air input cards
c
      CALL HM_GET_FLOATV('Lqud_Rho_g' ,RHOA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_P0'     ,P0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('GAMMA'      ,GAMMA   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_POROS'  ,FRAC    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('Rho_Gas'    ,RHOEXT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PEXT'       ,PEXT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISFLAG'     ,ICLOS   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Gflag'      ,INCGAS  ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_ALPHA'  ,AA      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Beta'   ,BB      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('tau_shear'  ,TAUX    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K'      ,KK      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('FUN_A1'     ,IFUNCK  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B1'     ,IFUNCR  ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV_DIM('SCALE_LOAD',FSCAL_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
c-----------------------------------------------------------------------
c     Check consistency of tabulated input data
c-----------------------------------------------------------------------
      IF (NRATEP == 0) THEN
          CALL ANCMSG(MSGID=866,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF  
      IF (NRATEN == 0 .AND. (IUNLOAD == 0 .OR. IUNLOAD == 1) ) THEN
          CALL ANCMSG(MSGID=867,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF    

c
      DO I = 1, NRATEP
        IF (SLOAD(I) == ZERO) SLOAD(I) = ONE*FSCAL_UNIT
        IFUNC(I) = FLOAD(I)
        RATE(I)  = RLOAD(I)
        YFAC(I)  = SLOAD(I)
      ENDDO                     
      NFUNC = NRATEP + NRATEN 
      DO I  = 1, NRATEN
         J  = NRATEP + I
        IF (SUNLOAD(I) == ZERO) SUNLOAD(I) = ONE*FSCAL_UNIT
        IFUNC(J) = FUNLOAD(I)
        RATE(J)  = RUNLOAD(I)
        YFAC(J)  = SUNLOAD(I)
      ENDDO                     
c
      DO I=1,NFUNC
        IF (IFUNC(I) == 0)THEN
          CALL ANCMSG(MSGID=126,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
        ENDIF
      ENDDO
c---
      IFUNC(NFUNC + 1) = IFUNCK
      IFUNC(NFUNC + 2) = IFUNCR
      NFUNC = NFUNC + 2 
c-----------------------------------------------------------------------
c     Default values
c-----------------------------------------------------------------------
      IF (EMAX == ZERO)   EMAX    = E0
      IF (EPSMAX == ZERO) EPSMAX  = ONE
      IF (GAMMA == ZERO)  GAMMA   = ONEP4
      IF (RHOEXT == 0)    RHOEXT  = RHOA
      IF (EXPO == ZERO)   EXPO    = ONE
      IF (HYS  == ZERO)   HYS     = ONE
      IF (IUNLOAD == 0)   IUNLOAD = 1
      ISRATE  = 1    ! force strain rate computation
      IF (FCUT == ZERO)   FCUT    = INFINITY
c-----------------------------------------------------------------------
      EINT0    = P0/(GAMMA - ONE)
      EINT_EXT = PEXT/(GAMMA - ONE)
      A1       = (EMAX-E0) / EPSMAX
      G        = HALF*E0 / (ONE + NU)
      BULK     = E0/THREE / (ONE - TWO*NU)
      EINT0    = P0/(GAMMA - ONE)
      EINT_EXT = PEXT/(GAMMA - ONE)
c-----------------------------------------------------------------------
      UPARAM(2) = E0
      UPARAM(3) = A1
      UPARAM(4) = EPSMAX
      UPARAM(5) = G
      UPARAM(6) = NU       
      UPARAM(7) = NRATEP
      UPARAM(8) = NRATEN
      DO I=1,NFUNC - 2
         UPARAM(I + 8)         =  RATE(I)
         UPARAM(I + 8 + NFUNC) = YFAC(I)
      END DO
      UPARAM(2*NFUNC + 9)  = IUNLOAD
      UPARAM(2*NFUNC + 10) = EXPO
      UPARAM(2*NFUNC + 11) = HYS
      UPARAM(2*NFUNC + 12) = EMAX
      NPAR_FOAM = 13 + 2*NFUNC
      UPARAM(NPAR_FOAM + 1) = RHOA
      UPARAM(NPAR_FOAM + 2) = P0
      UPARAM(NPAR_FOAM + 3) = GAMMA
      UPARAM(NPAR_FOAM + 4) = FRAC
      UPARAM(NPAR_FOAM + 5) = PEXT
      UPARAM(NPAR_FOAM + 6) = FP_INI
      UPARAM(NPAR_FOAM + 7) = EINT0
      UPARAM(NPAR_FOAM + 8) = KK
c-----------------------------------------------------------------------
      NUPARAM = NPAR_FOAM + 8  
      NUVAR   = 23
c-----------------------------------------------------------------------

      PARMAT(1)  = BULK
      PARMAT(2)  = E0
      PARMAT(3)  = NU
      PARMAT(4)  = ISRATE
      PARMAT(5)  = FCUT
      PARMAT(16) = 2  !  Formulation for solid elements time step computation
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU)       
c
      PM(192) = RHOA     
      PM(193) = FRAC     
      PM(194) = AA       
      PM(195) = BB       
      PM(196) = TAUX     
      PM(197) = KK       
      PM(198) = ICLOS    
      PM(199) = RHOEXT   
      PM(200) = EINT_EXT 
      PM(201) = INCGAS   
c--------------------------
      PM(1)  = RHOR
      PM(89) = RHO0
c----------------
      IF (NU > 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      ENDIF
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,77
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) E0,NU,EMAX,EPSMAX,FP_INI
        WRITE(IOUT,1400) FCUT,ISRATE,NRATEP,NRATEN,IUNLOAD,EXPO,HYS      
        WRITE(IOUT,1500)(IFUNC(J),RATE(J),YFAC(J),J=1,NRATEP)
        WRITE(IOUT,1600)(IFUNC(J+NRATEP),RATE(J+NRATEP),YFAC(J+NRATEP),J=1,NRATEN)
        WRITE(IOUT,2000)
        WRITE(IOUT,2100) RHOA,P0,GAMMA,FRAC,IFUNCR
        WRITE(IOUT,2200)
        WRITE(IOUT,2300) RHOEXT,PEXT,ICLOS,INCGAS
        WRITE(IOUT,3000)
        WRITE(IOUT,3100) AA,BB,TAUX,KK,IFUNCK
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT
     &(5X,'MATERIAL : TABULATED NON-LINEAR VISCO ELASTIC (LAW77) ',/,
     & 5X,'----------------------------------------------------- ',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY    . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'INITIAL YOUNG''S MODULUS . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM YOUNG''S MODULUS . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM STRAIN   . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL FOAM PRESSURE. . . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'STRAIN RATE CUTOFF FREQUENCY . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE         . . . . . . . .=',I10/,
     & 5X,'NUMBER OF LOAD STRESS FUNCTIONS  . . . . . .=',I10/,
     & 5X,'NUMBER OF UNLOAD STRESS FUNCTIONS. . . . . .=',I10/,
     & 5X,'CHOICE OF UNLOADING FORMULATION. . . . . . .=',I10/,
     & 5X,'SHAPE FACTOR FOR UNLOADING . . . . . . . . .=',1PG20.13/,
     & 5X,'HYSTERETIC UNLOADING FACTOR  . . . . . . . .=',1PG20.13     )
 1500 FORMAT(
     & 5X,'LOAD YIELD STRESS FUNCTION NUMBER. . . . . .=',I10/,
     & 5X,'STRAIN RATE. . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR . . . . . . . . . . . . . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'UNLOAD YIELD STRESS FUNCTION NUMBER. . . . .=',I10/,
     & 5X,'STRAIN RATE. . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR . . . . . . . . . . . . . . . .=',1PG20.13/)
 2000 FORMAT(
     & 5X,'  GAS PARAMETERS' ,/,
     & 5X,'  -----------------',/)  
 2100 FORMAT(
     & 5X,'DENSITY. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'P0 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'GAMMA. . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL GAS FRACTION (R) . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FUNCTION FOR GAS FRACTION R(V/V0). . .=',I10/)
 2200 FORMAT(
     & 5X,'  EXTERNAL GAS PARAMETERS' ,/,
     & 5X,'  -----------------------',/)  
 2300 FORMAT(
     & 5X,'DENSITY. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PEXT. . . . . . . .. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR CLOSED FOAM . . . . . . . . . . . .=',I10/
     & 5X,'INCOMNIG GAS FLAG. . . . . . . . . . . . . .=',I10/ )
 3000 FORMAT(
     & 5X,'  DARCY PARAMETERS ',/,
     & 5X,'  -----------------',/)  
 3100 FORMAT(
     & 5X,'A. . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BETA . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'TAUX . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL PERMEABILITY (K) . . . . . . . . . .=',1PG20.13/
     & 5X,'SCALE FUNCTION FOR PERMEABILITY K(V/V0). . .=',I10/ )
c-----------------------------------------------------------------------
      RETURN
      END
