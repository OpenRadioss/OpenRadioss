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
!||    hm_read_mat35            ../starter/source/materials/mat/mat035/hm_read_mat35.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT35(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,PM       ,
     .           ISRATE   ,ID       ,TITR     ,LSUBMODEL,IMATVIS  ,
     .           MTAG     ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE ELBUFTAG_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW35 (FOAM_VISC) WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM     
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      INTEGER, INTENT(INOUT)   :: IMATVIS
      INTEGER, INTENT(INOUT)   :: NUPARAM,NUVAR,NFUNC,ISRATE
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT) :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: FCT_ID,IFLAG,ILAW
      my_real :: RHO0,RHOR,E,E1,E2,ET,G,GT,POISSON,POISSONT,RELVEXP,MU,LAMBDA,
     .  C1,C2,C3,PMIN,P0,PHI,GAMA0,BULK,FSCAL,FSCAL_UNIT,ASRATE
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      IMATVIS = 1
      ILAW    = 35
      ISRATE  = 0
c
c------------------------------------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_E'     ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,POISSON  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E1'    ,E1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E2'    ,E2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'     ,RELVEXP  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_CO1'   ,C1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CO2'   ,C2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CO3'   ,C3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Itype'     ,IFLAG    ,IS_AVAILABLE, LSUBMODEL)      
      CALL HM_GET_FLOATV('MAT_PC'    ,PMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('FUN_A1'    ,FCT_ID   ,IS_AVAILABLE, LSUBMODEL)      
      CALL HM_GET_FLOATV('IFscale'   ,FSCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Fsmooth'   ,ISRATE   ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('Fcut'      ,ASRATE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
c
      CALL HM_GET_FLOATV('MAT_ETAN'  ,ET       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NUt'   ,POISSONT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA2'  ,MU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA1'  ,LAMBDA   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_P0'    ,P0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PHI'   ,PHI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMA0' ,GAMA0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c------------------------------------------------------------------------------
c
      CALL HM_GET_FLOATV_DIM('IFscale'  ,FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (FSCAL == ZERO) FSCAL = ONE*FSCAL_UNIT
c
      IF (MU   <= ZERO) MU   = EP20
      IF (PMIN == ZERO) PMIN =-EP20 
      IF (PMIN >  ZERO) PMIN =-PMIN
      BULK = THIRD*E/(ONE - TWO*POISSON)
C
      !----------------------------------------------------------
      ! Filtering frequency
      IF (ASRATE /= ZERO) THEN
        ! If a filtering frequency is given by the user
        ISRATE = 1
      ELSE
        ! If no filtering frequency is given but the flag is activated
        IF (ISRATE /= 0) THEN
          ASRATE  = 10000.0D0*UNITAB%FAC_T_WORK  
        ! If no filtering frequency and no flag is activated => no filtering
        ELSE
          ASRATE  = ZERO
        ENDIF
      ENDIF
      !---------------------------------------------------------- 
      ! Checking filtering and strain-rate dependency
      IF (((C1 /= ZERO).OR.(E1 /= ZERO)).AND.(ISRATE == 0)) THEN         
        CALL ANCMSG(MSGID=1220,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=ID,
     .              C1=TITR)  
      ENDIF 
C
c-----------------
      UPARAM(1)  = E
      UPARAM(2)  = POISSON
      UPARAM(3)  = E1
      UPARAM(4)  = E2
      UPARAM(5)  = ET
      UPARAM(6)  = POISSONT
      UPARAM(7)  = MU
      UPARAM(8)  = LAMBDA
      UPARAM(9)  = P0
      UPARAM(10) = PHI
      UPARAM(11) = GAMA0
      UPARAM(12) = C1
      UPARAM(13) = C2
      UPARAM(14) = C3
      UPARAM(15) = IFLAG
      UPARAM(16) = PMIN
      UPARAM(17) = RELVEXP
      UPARAM(18) = FSCAL
      UPARAM(19) = 0  
c-----------------
      IFUNC(1) = FCT_ID
c-----------------
      PARMAT(1)  = E
      PARMAT(2)  = E
      PARMAT(3)  = POISSON
      PARMAT(4)  = ISRATE
      PARMAT(5)  = ASRATE
      PARMAT(16) = 2
      PARMAT(17) = (ONE - TWO*POISSON)/(ONE - POISSON)
c-----------------
      MTAG%L_EPSD = 1
      MTAG%G_EPSD = 1
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(100)= E      
c-----------------
      NUPARAM = 19
      NFUNC   = 1
      IF (ISRATE == 0) THEN
        NUVAR = 3
      ELSE
        NUVAR = 4
      ENDIF
c-----------------
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")   
c-----------------
      WRITE(IOUT,1100) TRIM(TITR),ID,35
      WRITE(IOUT,1000)

      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,RHOR
        WRITE(IOUT,1300) E,POISSON,E1,E2,RELVEXP,C1,C2,C3,FCT_ID,FSCAL,
     .                   IFLAG,PMIN,ET,POISSONT,MU,LAMBDA,P0,PHI,GAMA0
        WRITE(IOUT,1500) ISRATE,ASRATE
      ENDIF     
C-----------------
      RETURN
C-----------------
 1000 FORMAT
     & (5X,24H  ELASTIC WITH VISCOSITY,/,
     &  5X,24H  ----------------------,//)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,   'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,   'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,   'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT
     & (5X,  'YOUNG''S MODULUS. . . . . . . . . . . .=',1PG20.13/ 
     & ,5X,  'POISSON''S RATIO. . . . . . . . . . . .=',1PG20.13/ 
     & ,5X,  'E1. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'E2. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'EXPONENT ON RELATIVE VOLUME . . . . . .=',1PG20.13//
     & ,5X,  'C1. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'C2. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'C3. . . . . . . . . . . . . . . . . . .=',1PG20.13//
     & ,5X,  'FUNCTION NUMBER FOR PRESSURE. . . . . .=',I10/
     & ,5X,  'PRESSURE FUNCTION SCALE FACTOR. . . . .=',1PG20.13//
     & ,5X,  'FLAG FOR OPEN CELL FOAM FORMULATION . .=',I10/
     & ,5X,  'CUT OFF PRESSURE IN TENSION . . . . . .=',1PG20.13//
     & ,5X,  'ET. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'POISSON''S RATIO (TANGENT). . . . . . .=',1PG20.13/ 
     & ,5X,  'VISCOSITY COEFFICIENT (IN PURE SHEAR) .=',1PG20.13/
     & ,5X,  'VISCOSITY COEFFICIENT (VOLUMETRIC). . .=',1PG20.13//
     & ,5X,  'INITIAL PRESSURE. . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'POROSITY. . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'INITIAL VOLUMETRIC STRAIN. . . .. . . .=',1PG20.13/)
 1500 FORMAT
     & (5X,  'STRAIN RATE FILTERING FLAG  . . . . . .=',I10/
     & ,5X,  'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13)
C-----------------
      END
