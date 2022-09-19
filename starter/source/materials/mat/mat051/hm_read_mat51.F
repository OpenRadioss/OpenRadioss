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
Chd|  HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_READ_MAT51_IFORM11         source/materials/mat/mat051/hm_read_mat51_iform11.F
Chd|        LECM51__CHECK_INITIAL_STATE   source/materials/mat/mat051/lecm51__check_initial_state.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT51(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   ,
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MATERIAL LAW WITH HM READER
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION
C
C     PM              MATERIAL ARRAY(REAL)
C     IPM             MATERIAL ARRAY(INTEGER)
C     ISRATE          FLAG FOR STRAIN RATE FILTERING
C     IMATVIS         FLAG FOR VISCOSITY
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE
C     UPARAM          MATERIAL BUFFER
C     PARMAT          ADDITIONAL PARAMETER FOR MATERIALS
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE MATPARAM_DEF_MOD
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
#include      "mmale51_c.inc"
#include      "alemuscl.inc"
#include      "alesolver.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      !---Global parameters
      my_real VIS,VISV,PEXT,PMIN,GAM,VIS2,VISV2
      INTEGER MIP,MLN,NBMAT,IFLAG_BAK,IDX
      !---EOS parameters
      my_real      :: RHO0, RHOR, DPDMU(4)
      my_real      :: AV(4), RHO0_(4),C0(4),C1(4), E0(4),PMIN_(4),T0(4),C2(4),C3(4),C4(4),C5(4), GG(4)
      my_real      :: B1,B2,R1,R2,W,VDET,PCJ,VCJ, P0_GLOB
      !---constitutive model
      !------ Johnson-Cook
      my_real      :: Y(4),BB(4),N(4),CC(4),EPDR(4),CM(4),TMELT(4),THETL(4),PLAMX(4),SIGMX(4),SPH(4), XKA(4),XKB(4)
      !------ units
      my_real      :: FAC_UNIT_TIME, FAC_UNIT_SPH
      !------ Drucker-PRager
      my_real      :: NU(4),A0(4),A1(4),A2(4),AMX(4),BUNL(4),MUMX(4),PSTAR(4),DET,DELTA,PTOT
      !---NRF
      my_real      :: P0(4),SSP(4),LC(4),TCARP,TCAR,ABCS
      !---INLET/OUTLET
      INTEGER      :: IOPT,IAV(4),IRHO(4),IE(4)
      INTEGER      :: I,J,IFLG,io,IERROR
      INTEGER      :: JTHE, NITER
      INTEGER      :: IEXP, N_LC, IPLA_, IPLA(4), IBFRAC
      INTEGER      :: IVEL, IMID, NJWL, EOS_TYPE
      INTEGER      :: tMID(4),SUM_MID
      !---temporary working arrays
      my_real      :: tAV(4),tR0(4),tC0(4),tC1(4),tC2(4),tC3(4),tC4(4),tC5(4),tE0(4)
      my_real      :: tPM(4), EINF(4),tA0(4),tA1(4),tA2(4),tPL(4),Pfar,Pini(4)
      my_real      :: IE_BOUND, RATIO, SumVF,VEL
      !---messages/output
      CHARACTER*64 :: chain,chain1
      CHARACTER*32 :: CAV(4),CRHO0(34),CE0(4),CPM(4),CC0(4),CSSP(4)
      CHARACTER*32 :: CPEXT,CTCARP,CTCAR
      !---booleans
      LOGICAL      :: IS_AVAILABLE,IS_ENCRYPTED, IFLG6_SUBMAT_DEFINED(4)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE=0
      IMATVIS=0
      !----------------!
      MTAG%L_SSP  = 1
      MTAG%L_SIGV = 1
      MTAG%L_EINS = 1
      MTAG%G_EINS = 1
      MTAG%L_EINT = 1
      MTAG%G_EINT = 1
      MTAG%G_TEMP = 1
      MTAG%L_TEMP = 1
      MTAG%L_VK   = 1
      NUPARAM = 280
      NFUNC = 10
      UPARAM(1:NUPARAM) = ZERO
      IFUNC(1:NFUNC)    = 0
      IF (IALEMUSCl  >  0) I_LAW = 51  ! Communicate LAW to ALEMUSC_MOD
      IFLG6_SUBMAT_DEFINED(1:4)=.TRUE.
      !----------------!
      NITER  = 10
      N_LC   = 0
      IOPT   = 0
      TRIMAT = 4 !MAX(TRIMAT,3)
      NUVAR  = (N0PHAS + TRIMAT*NVPHAS)
      PEXT   = ZERO
      PFAR   = ZERO
      VIS    = ZERO
      VISV   = ZERO
      FAC_UNIT_SPH = ONE
      FAC_UNIT_TIME = ONE
      !----------------!
      AV(1:4)=ZERO
      RHO0_(1:4)=ZERO
      E0(1:4) = ZERO
      PMIN_(1:4) = ZERO
      GG(1:4) = ZERO
      C0(1:4) = ZERO
      C1(1:4) = ZERO
      C2(1:4) = ZERO
      C3(1:4) = ZERO
      C4(1:4) = ZERO
      C5(1:4) = ZERO
      !----------------!
      SSP(1:4)= ZERO
      LC(1:4) = ZERO
      TCARP   = ZERO
      TCAR    = ZERO
      ABCS    = 0
      IAV(1:4)=0
      IRHO(1:4)=0
      IE(1:4)=0
      IVEL    = 0
      VEL     = ZERO
      IOPT = 0
      RATIO = ZERO
      !----------------!
      Y(1:4)     = ZERO
      BB(1:4)    = ZERO
      N(1:4)     = ZERO
      CC(1:4)    = ZERO
      EPDR(1:4)  = ZERO
      CM(1:4)    = ZERO
      TMELT(1:4) = ZERO
      THETL(1:4) = ZERO
      PLAMX(1:4) = ZERO
      SIGMX(1:4) = ZERO
      SPH(1:4)   = ZERO
      T0(1:4)    = ZERO
      !----------------!
      B1     = ZERO
      B2     = ZERO
      R1     = ZERO
      R2     = ZERO
      W      = ZERO
      VDET   = ZERO
      PCJ    = ZERO
      VCJ    = ZERO
      IEXP   = 0
      IBFRAC = 0
      !----------------!
      XKA(1:4)  = ZERO
      XKB(1:4)  = ZERO
      A0(1:4)   = ZERO
      A1(1:4)   = ZERO
      A2(1:4)   = ZERO
      AMX(1:4)  = ZERO
      BUNL(1:4) = ZERO
      MUMX(1:4) = ZERO
      PSTAR(1:4)=-INFINITY
      NU(1:4)   = ZERO
      EINF(1:4) =-INFINITY
      !==========================================!
      ! IFLG == ALL (0,1,2,3,4,5,10,11,12)       !
      !==========================================!
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      IPLA_     = 0  !Plasticity Flag > 0 means that at least one plastic submaterial.
      IPLA(1:4) = 0  !1:JCOOK, 2:DPRAG
      CALL HM_GET_INTV  ('MAT_Iflag'  ,IFLG        ,IS_AVAILABLE, LSUBMODEL)
            IFLAG_BAK=IFLG
      IF(IFLG == 11)THEN
        CALL HM_GET_INTV  ('Mach1'      ,IPLA(1)     ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV  ('Mach2'      ,IPLA(2)     ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV  ('Mach3'      ,IPLA(3)     ,IS_AVAILABLE, LSUBMODEL)
      ENDIF
      IF(IFLG /= 11)THEN
        IPLA(1:4)=0
      ENDIF
      DO I=1,3
        IF(IPLA(I) < 0 .OR. IPLA(I) > 2)IPLA(I)=0
      ENDDO
      !----------------!
      SELECT CASE(IFLG)
      !============================!
      ! IFLG == 0                  !
      !============================!
      CASE(0)
        !line-3
        CALL HM_GET_FLOATV('PEXT'       ,PEXT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_NU'     ,VIS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_Lamda'  ,VISV   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg0_phas'    ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg0_phas'      ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg0_phas'        ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg0_phas'        ,PMIN_(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg0_phas'       ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1_Iflg0_phas'       ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2_Iflg0_phas'       ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3_Iflg0_phas'       ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4_Iflg0_phas'       ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5_Iflg0_phas'       ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_G_Iflg0_phas'        ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('MAT_P_Iflg0_phas' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF(GG(1)+GG(2)+GG(3)==ZERO)THEN
          IPLA_=0
        ELSE
          IPLA_=1
          IF(GG(1)>ZERO)IPLA(1)=1
          IF(GG(2)>ZERO)IPLA(2)=1
          IF(GG(3)>ZERO)IPLA(3)=1
        ENDIF
      !============================!
      ! IFLG == 1                  !
      !============================!
      CASE(1)
        IFLG = 0
        !line-3
        CALL HM_GET_FLOATV('PEXT'       ,PEXT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_NU'     ,VIS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_Lamda'  ,VISV   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg1_phas'    ,AV(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg1_phas'      ,RHO0_(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg1_phas'        ,E0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg1_phas'        ,PMIN_(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg1_phas'       ,C0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1_Iflg1_phas'       ,C1(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2_Iflg1_phas'       ,C2(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3_Iflg1_phas'       ,C3(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4_Iflg1_phas'       ,C4(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5_Iflg1_phas'       ,C5(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_G1'               ,GG(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Sigma_Y1'                ,Y(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('BB1'                     ,BB(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('LAW51_N1'                ,N(I)     ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('CC1'                     ,CC(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('e01'                     ,EPDR(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('CM1'                     ,CM(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('T10'                     ,T0(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('T_melt1'                 ,TMELT(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('T_limit1'                ,THETL(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Rhocv'                   ,SPH(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('E_max1'                  ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('sigma_max1'              ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('K_A1'                    ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('K_B1'                    ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Rhocv' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF(GG(1)+GG(2)+GG(3)==ZERO)THEN
          IPLA_=0
        ELSE
          IPLA_=1
          IF(GG(1)>ZERO)IPLA(1)=1
          IF(GG(2)>ZERO)IPLA(2)=1
          IF(GG(3)>ZERO)IPLA(3)=1
        ENDIF
      !============================!
      ! IFLG == 2                  !
      !============================!
      CASE(2)
        IS_BOUNDARY_MATERIAL = .TRUE.
        CALL HM_GET_FLOATV('SCALE'       ,ABCS        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('PEXT'        ,PEXT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('VEL_in'      ,VEL         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('Fct_ID_vel'  ,IVEL        ,IS_AVAILABLE, LSUBMODEL)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg2_phas'     ,AV(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg2_phas'       ,RHO0_(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg2_phas'         ,E0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N1'                   ,IAV(I)  ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N2'                   ,IRHO(I) ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N3'                   ,IE(I)   ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1_Iflg2_phas'        ,C1(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2_Iflg2_phas'        ,C2(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3_Iflg2_phas'        ,C3(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4_Iflg2_phas'        ,C4(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5_Iflg2_phas'        ,C5(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg2_phas'         ,PMIN_(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg2_phas'        ,C0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('MAT_P_Iflg2_phas' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('SCALE'   ,FAC_UNIT_TIME  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !============================!
      ! IFLG == 3                  !
      !============================!
      CASE(3)
        !OBSOLETE use IFLG=6 instead
        IS_BOUNDARY_MATERIAL = .TRUE.
        CALL ANCMSG(MSGID   = 75              ,
     .              MSGTYPE = MSGWARNING      ,
     .              ANMODE  = ANINFO_BLIND_1
     .              )
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg3_phas'     ,AV(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg3_phas'       ,RHO0_(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg3_phas'         ,E0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg3_phas'         ,PMIN_(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg3_phas'        ,C0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('MAT_P_Iflg3_phas' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !============================!
      ! IFLG == 4                  !
      !============================!
      CASE(4)
        IS_BOUNDARY_MATERIAL = .TRUE.
        CALL HM_GET_FLOATV('SCALE'       ,ABCS        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('PEXT'        ,PEXT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg2_phas'     ,AV(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg2_phas'       ,RHO0_(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg2_phas'         ,E0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N1'                   ,IAV(I)  ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N2'                   ,IRHO(I) ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N3'                   ,IE(I)   ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg2_phas'        ,C0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1_Iflg2_phas'        ,C1(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4_Iflg2_phas'        ,C4(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          C5(I)=C4(I) ! no need to enter C5 (=C4) since 14.0
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('MAT_C0_Iflg2_phas' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('SCALE'            ,FAC_UNIT_TIME  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !============================!
      ! IFLG == 5                  !
      !============================!
      CASE(5)
        IS_BOUNDARY_MATERIAL = .TRUE.
        CALL HM_GET_FLOATV('SCALE'       ,ABCS        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('PEXT'        ,PEXT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg2_phas'     ,AV(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg2_phas'       ,RHO0_(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg2_phas'         ,E0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N1'                   ,IAV(I)  ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N2'                   ,IRHO(I) ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX  ('ABG_N3'                   ,IE(I)   ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg2_phas'        ,C0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1_Iflg2_phas'        ,C1(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg2_phas'         ,PMIN_(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('MAT_P_Iflg2_phas' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('SCALE'  ,FAC_UNIT_TIME  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !============================!
      ! IFLG == 6                  !
      !============================!
      CASE(6)
        IS_BOUNDARY_MATERIAL = .TRUE.
        M51_IFLG6 = 1
        IERROR = 0
        CALL HM_GET_FLOATV('PEXT'        ,PEXT         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_TCP'     ,TCARP        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_TCALPHA' ,TCAR         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg0_phas'     ,AV(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg0_phas'       ,RHO0_(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg0_phas'         ,E0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg0_phas'         ,PMIN_(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P0_Iflg0_phas'        ,C0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_SSP0_Iflg0_phas'      ,SSP(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF(AV(I)==ZERO .AND. RHO0_(I)==ZERO .AND. E0(I)==ZERO .AND. PMIN_(I)==ZERO .AND. C0(I)==ZERO .AND. SSP(I)==ZERO )THEN
            IFLG6_SUBMAT_DEFINED(I)=.FALSE.
          ENDIF
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('MAT_P0_Iflg0_phas' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       IF(TCAR == ZERO)TCAR=INFINITY
       CPEXT (1:32) = '  auto                          ' ;
       IF (PEXT /= ZERO) WRITE(CPEXT  ,FMT='(E12.4)')PEXT
       CTCARP(1:32) = '  auto                          ' ;
       IF (TCARP /= ZERO) WRITE(CTCARP ,FMT='(E12.4)')TCARP
       CTCAR (1:32) = '  auto                          ' ;
       IF (TCAR /= ZERO) WRITE(CTCAR  ,FMT='(E12.4)')TCAR
       DO I=1,3
         CAV  (I)= '  auto                          ' ;IF (AV(I) /= ZERO) WRITE(CAV(I)   ,FMT='(E12.4)')AV(I)
         CRHO0(I)= '  auto                          ' ;IF (RHO0_(I)/= ZERO)  WRITE(CRHO0(I) ,FMT='(E12.4)')RHO0_(I)
         CE0  (I)= '  auto                          ' ;IF (E0(I) /= ZERO) WRITE(CE0(I)   ,FMT='(E12.4)')E0(I)
         CPM  (I)= '  auto                          ' ;IF (PMIN_(I) /= ZERO) WRITE(CPM(I)   ,FMT='(E12.4)')PMIN_(I)
         CC0  (I)= '  auto                          ' ;IF (C0(I) /= ZERO) WRITE(CC0(I)   ,FMT='(E12.4)')C0(I)
         CSSP (I)= '  auto                          ' ;IF (SSP(I) /= ZERO) WRITE(CSSP(I)  ,FMT='(E12.4)')SSP(I)
       ENDDO

      !============================!
      ! IFLG == 10                 !
      !============================!
      CASE(10)
        IFLG=1
        IEXP=1
        CALL HM_GET_FLOATV('PEXT'       ,PEXT         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_NU'     ,VIS          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_Lamda'  ,VISV         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        DO I=1,3
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg10_phas' ,AV(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_RHO_Iflg10_phas'   ,RHO0_(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_E_Iflg10_phas'     ,E0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_P_Iflg10_phas'     ,PMIN_(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C0_Iflg10_phas'    ,C0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1_Iflg10_phas'    ,C1(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2_Iflg10_phas'    ,C2(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3_Iflg10_phas'    ,C3(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4_Iflg10_phas'    ,C4(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C5_Iflg10_phas'    ,C5(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MLAW51_G10'            ,GG(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Sigma_Y10'             ,Y(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('BB10'                  ,BB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('LAW51_N10'             ,N(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('CC10'                  ,CC(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('e010'                  ,EPDR(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('CM10'                  ,CM(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('T10_10'                ,T0(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('T_melt10'              ,TMELT(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('T_limit10'             ,THETL(I),I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Rhocv10'               ,SPH(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('E_max10'               ,PLAMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('sigma_max10'           ,SIGMX(I) ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('K_A1'                  ,XKA(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('K_B1'                  ,XKB(I)   ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('Rhocv10' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !High Explosive
        CALL HM_GET_FLOATV('MLAW51_ALPHA'            ,AV(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_Rho'              ,RHO0_(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_E0'               ,E0(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_Pmin'             ,PMIN_(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_C0'               ,C0(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_B1'               ,B1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_B2'               ,B2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_R1'               ,R1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_R2'               ,R2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_W'                ,W        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_D'                ,VDET     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_PCJ'              ,PCJ      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MLAW51_C14'              ,C1(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV('MAT_IBFRAC'              ,IBFRAC   ,IS_AVAILABLE, LSUBMODEL)
        IF(GG(1)+GG(2)+GG(3)==ZERO)THEN
          IPLA_=0
        ELSE
          IPLA_=1
          IF(GG(1)>ZERO)IPLA(1)=1
          IF(GG(2)>ZERO)IPLA(2)=1
          IF(GG(3)>ZERO)IPLA(3)=1
        ENDIF
      !============================!
      ! IFLG == 11                 !
      !============================!
      CASE(11)
        IFLG=1
        IEXP=1
        CALL HM_READ_MAT51_IFORM11(
     .        AV     , RHO0_     ,E0       ,  PMIN_   , C0      ,
     .        C1     , C2        ,C3       ,  C4      , C5      ,
     .        IPLA   , GG        ,Y        ,  BB      , N       ,
     .        CC     , EPDR      ,CM       ,  T0      , TMELT   ,
     .        THETL  , SPH       ,PLAMX    ,  SIGMX   , XKA     ,
     .        XKB    , NU        ,A0       ,  A1      , A2      ,
     .        AMX    , BUNL      ,MUMX     ,
     .        B1     , B2        ,R1       ,  R2,       W,
     .        VDET   , PCJ       ,IBFRAC   ,  PEXT,     VIS,
     .        VISV   , IS_ENCRYPTED,LSUBMODEL,  UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('DAMP1' ,FAC_UNIT_SPH   ,1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !Shear Modulus for DPRAG input
        IF(GG(1)==ZERO.AND.IPLA(1)==2)GG(1)=Y(1)/TWO/(ONE+NU(1))
        IF(GG(2)==ZERO.AND.IPLA(2)==2)GG(2)=Y(2)/TWO/(ONE+NU(2))
        IF(GG(3)==ZERO.AND.IPLA(3)==2)GG(3)=Y(3)/TWO/(ONE+NU(3))
        !Plasticity flags
        IPLA_=0
        IF(GG(1)+GG(2)+GG(3) /= ZERO)THEN
          IPLA_=1
          IF(GG(1)==ZERO)IPLA(1)=0
          IF(GG(2)==ZERO)IPLA(2)=0
          IF(GG(3)==ZERO)IPLA(3)=0
        ENDIF
      !============================!
      ! IFLG == 12                 !
      !============================!
      CASE(12)
        IFLG  = 12
        IEXP  = 1
        NBMAT = 0
        tAV(1:4) = ZERO
        tMID(1:4)= 0
        CALL HM_GET_FLOATV('MAT_NU'     ,VIS          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_Lamda'  ,VISV         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('MIP'       ,MIP        ,IS_AVAILABLE, LSUBMODEL)
        IMID            = 0
        SUMVF           = ZERO
        DO I=1,MIP
          CALL HM_GET_INT_ARRAY_INDEX  ('materialIds'               ,tMID(I)   ,I,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPHA_Iflg12_phas'     ,tAV(I)    ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        UPARAM(09:12) = tMID(1:4)
        UPARAM(13:16) = tAV(1:4)
        IF(MIP==0)THEN
              chain1='SUBMATERIAL DATA NOT FOUND '
              CALL ANCMSG(MSGID=99,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=MAT_ID,
     .                    C1=TITR,
     .                    C2=chain1)
        ENDIF
        DO I=1,MIP
          IF(tAV(I)<ZERO .OR. tAV(I)>ONE)THEN
              chain(1:20)='                    '
              write(chain(1:20),'(e20.14)')tAV(I)
              chain1='VOLUME FRACTION MUST BE BETWEEN 0.0 AND 1.0 , READ VALUE IS '// chain(1:20)
              CALL ANCMSG(MSGID=99,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=MAT_ID,
     .                    C1=TITR,
     .                    C2=chain1)
          ENDIF
          IF(tMID(I)  <= 0)THEN
              chain(1:10)='          '
              write(chain(1:10),'(i10)')tMID(I)
              chain1='INCORRECT MATERIAL IDENTIFIER '//chain(1:10)
              CALL ANCMSG(MSGID=99,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=MAT_ID,
     .                    C1=TITR,
     .                    C2=chain1)
              tMID(I)=0
          ENDIF
        ENDDO
        SUMVF = SUM(tAV(1:4))
        IF(SUMVF>ONE)THEN
            chain(1:20)='                    '
            write(chain(1:20),'(F20.8)')SUMVF
            chain1='SUM OF VOLUME FRACTION MUST BE EQUAL TO 1.0, CURRENT SUM IS '// chain(1:20)
            CALL ANCMSG(MSGID=99,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  C2=chain1)
        ENDIF
      !============================!
      ! IFLG == NOT VALID          !
      !============================!
      CASE DEFAULT
       !not correct iform value
           chain1='INCORRECT IFORM VALUE '
           CALL ANCMSG(MSGID=99,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=MAT_ID,
     .                 C1=TITR,
     .                 C2=chain1)
      END SELECT
      !============================!
      ! PLASTICITY DEFAULT         !
      !============================! 
      IF(RATIO<=ZERO)THEN
        RATIO = 0.25D00   !ONE is for previous formulation (permitted large volume change)
      ENDIF
      IF(IPLA_>0)THEN
        MTAG%G_EPSD  = 1
        MTAG%L_EPSD  = 1
        MTAG%G_PLA   = 1
        MTAG%L_PLA   = 1
        IF(IPLA(1) == 2 .OR. IPLA(2) == 2 .OR. IPLA(3) == 2)THEN
          MTAG%G_EPSQ = 1
          MTAG%L_EPSQ = 1
        ENDIF
      ENDIF
      IF(IEXP>0)THEN
        MTAG%G_BFRAC = 1
        MTAG%L_BFRAC = 1
      ENDIF
      IF(IFLG == 12)THEN
          MTAG%G_EPSD  = 1
          MTAG%L_EPSD  = 1
          MTAG%G_PLA   = 1
          MTAG%L_PLA   = 1
          MTAG%G_EPSQ  = 1
          MTAG%L_EPSQ  = 1
          MTAG%G_BFRAC = 1
          MTAG%L_BFRAC = 1
      ENDIF
      DO I=1,3
        IF(IPLA(I) == 2)THEN 
          IF(A2(I)==ZERO.AND.A1(I)/=ZERO)THEN   !(A21=A11=ZERO => error message)
            PSTAR(I)=-A0(I)/A1(I) 
          ELSEIF(A2(I)/=ZERO)THEN
            DELTA = A1(I)*A1(I)-FOUR*A0(I)*A2(I)
            !Si intersection avec l'axe
            IF(DELTA  >=  ZERO)THEN
              DELTA=SQRT(DELTA)
              PSTAR(I) = (-A1(I)+DELTA)/TWO/A2(I)
            
            ELSE
            !  PSTAR(I) = -A1(I)/TWO/A2(I) !no let user do what he wants 
              PSTAR(I) = -INFINITY
              chain='SUBMAT-0 : YIELD SURFACE HAS NO ROOT.                          '
              write(chain(8:8),'(i1.1)')I
              CALL ANCMSG(MSGID=829,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO,
     .                    I1=51,
     .                    I2=MAT_ID,
     .                    C1='WARNING',
     .                    C2=TITR,
     .                    C3=chain)
            ENDIF
          ELSE
            !do nothing let user do what he wants
          ENDIF
        ENDIF
      ENDDO
C     |============================!
C     | DEFAULT FLAGS              !
C     !============================!
      IF(IOPT < 0)IOPT=0 !check possible values otherwise set to debaut one.
      !IF(IOPT >= 4)IOPT=0 ! does not exist
      IF (N_LC  <= 0)THEN
        IF(IOPT==1)THEN
          N_LC=100
        ELSE
          N_LC=1000
        ENDIF
      ENDIF       
      IF(ABCS==ZERO) ABCS=  ONE*FAC_UNIT_TIME
      PMIN = -PEXT  
      DO I=1,4
        ! RhoCp
        IF(SPH(I) == ZERO) SPH(I) = ONE * FAC_UNIT_SPH
        !JCook exponent
        IF(N(I) == ZERO) N(I) = ONE
        !Initial Temperature
        IF(T0(I) == ZERO) T0(I) = THREE100
        !MAximum plastic Strain
        IF(PLAMX(I) == ZERO) PLAMX(I) = INFINITY
        !Maximum Stress
        IF(SIGMX(I) == ZERO) SIGMX(I) = INFINITY
        !Melting Temperature
        IF(TMELT(I) == ZERO) TMELT(I) = INFINITY
        !Limit Temperature
        IF(THETL(I) == ZERO) THETL(I) = INFINITY
        !Thermal condictivity
        IF(XKA(I) == ZERO)  XKA(I) = EM20
        !Johnson-Cook parameters
        IF(EPDR(I) == ZERO)  EPDR(I) = ONE
        !Drucker-Prager maximum values
        IF(AMX(I) == ZERO)  AMX(I) = INFINITY
        !Drucker-Prager Poisson's coefficient
        IF(NU(I) == ZERO)  NU(I) = ZEP2
      ENDDO
      !Drucker-Prager \B5max1, \B5max2, \B5max3
      DO I=1,3
        IF(MUMX(I) == ZERO .AND. BUNL(I) /= ZERO)THEN
         IF(C3(I) == ZERO)THEN
          IF(C2(I) == ZERO)THEN
           MUMX(I)=INFINITY
          ELSE
           MUMX(I)=(BUNL(I)-C1(I))/(TWO*C2(I))
          ENDIF
         ELSE
          DET=SQRT(C2(I)**2 + THREE*C3(I)*(BUNL(I)-C1(I)))
           MUMX(I)=(DET-C2(I))/(THREE*C3(I))
         ENDIF
        ENDIF
      ENDDO
      !Drucker-Prager unload modulus
      DO I=1,3
        IF(BUNL(I) == ZERO)  BUNL(I) = C1(I)
      ENDDO
      !Minimum Pressure (GGi must be calculated whatever is yield criteria)
      IF(IFLG  <= 1.OR.IFLG == 11)THEN
       DO I=1,3
        IF (PMIN_(I) == ZERO) THEN
          IF(GG(I) == ZERO)THEN
            PMIN_(I) = -PEXT
          ELSE
            PMIN_(I) = -INFINITY
          ENDIF
        ENDIF
       ENDDO
       IF (PMIN_(4) == ZERO) THEN
         PMIN_(4) = -INFINITY
       ENDIF
      ENDIF
      !EXPLOSIVE C14
      IF(RHO0_(4) > ZERO)THEN
        IF(C1(4)  <= ZERO)THEN
           chain1='BULK MODULUS C14 MUST BE PROVIDED FOR UNREACTED EXPLOSIVE'
           CALL ANCMSG(MSGID=99,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=MAT_ID,
     .                 C1=TITR,
     .                 C2=chain1)
        ENDIF
        IF(VDET  <= ZERO)THEN
          chain1='DETONATION VELOCITY MUST BE INPUT FOR EXPLOSIVE SUBMATERIAL'
          CALL ANCMSG(MSGID=99,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                C2=chain1)
        ENDIF
      ENDIF
      !============================!
      ! CHECKING INITIAL STATE     !
      !   *INITIAL PRESSURE        !
      !   *DENSITIES               !
      !   *VOLUMETRIC FRACTION     !
      !============================!
      !Check is done only for multimaterial law (iflg=0|1), inlet/outlet are skipped.
      IF (IFLG /= 12) THEN
        CALL LECM51__CHECK_INITIAL_STATE(
     .   AV, RHO0_,  C0,  C1,  C2, C3, C4,  C5,
     .   E0, PM, RHO0, RHOR, IEXP, JTHE,PEXT, IFLG,
     .   A0, A1, A2, IPLA,
     .   MAT_ID ,TITR,
     .   SSP(1),SSP(2),SSP(3),SSP(4),
     .   LC(1), LC(2), LC(3), LC(4),
     .   TCARP,
     .   Pini)
        !Inferior Boundary for Energy. It ensure EOS stability.
        !Inf(Eint), def=-INFINITY
        !If P0=Pext+C0 >0 then de/d>0 then extremum in expansion and can be calculated (if exists) without C2 and C3
        !otherwise C2 and C3 are non zero and extremum calculation is complicated.
        !  It is approximated with C2=C3=0 which is inferior thanks to C2, C3 sign.
        DO I=1,3
          EINF(I)=IE_BOUND(PEXT,PMIN_(I),C0(I),C1(I),C2(I),C3(I),C4(I),C5(I),E0(I))
        ENDDO
        EINF(4)=ZERO
      END IF
      !sound speed initialization loi51
      IF(IFLG == 0 .OR. IFLG == 1)THEN
        DO I=1,3
          P0(I)    = C0(I)+C4(I)*E0(I)
          DPDMU(I) = (C1(I)+C5(I)*E0(I)) + C4(I)*(PEXT+P0(I)) 
          IF(RHO0_(I) /= ZERO) SSP(I)  = SQRT( (DPDMU(I) + TWO_THIRD*GG(I)) / RHO0_(I) )   !warning GG = 2*G   =>  TWO_THIRD*GG = FOUR_OVER_3*G
        ENDDO
        P0(4)  = C0(4)
        SSP(4) = VDET
      ENDIF
      !============================!
      ! Output through UPARAM(*)   !
      !============================! 
      UPARAM(1)  = VIS   
      UPARAM(2)  = TWO*VIS
      UPARAM(3)  = (VISV-UPARAM(2))*THIRD
      UPARAM(4)  = AV(1)
      UPARAM(5)  = AV(2)
      UPARAM(6)  = AV(3)
      UPARAM(46) = AV(4)
      UPARAM(7)  = PFAR
      UPARAM(8)  = PEXT
      UPARAM(55) = IEXP
      UPARAM(31) = IFLG
      UPARAM(73) = NITER
      UPARAM(74) = RATIO
      IF (IFLG /= 12) THEN
        UPARAM(9)  = RHO0_(1)
        UPARAM(10) = RHO0_(2)
        UPARAM(11) = RHO0_(3)
        UPARAM(47) = RHO0_(4)
        UPARAM(12) = C1(1)
        UPARAM(13) = C1(2)
        UPARAM(14) = C1(3)
        UPARAM(15) = C2(1)
        UPARAM(16) = C2(2)
        UPARAM(17) = C2(3)
        UPARAM(18) = C3(1)
        UPARAM(20) = C3(2)
        UPARAM(21) = C3(3)
        UPARAM(22) = C4(1)
        UPARAM(23) = C4(2)
        UPARAM(24) = C4(3)
        UPARAM(25) = C5(1)
        UPARAM(26) = C5(2)
        UPARAM(27) = C5(3)
        UPARAM(28) = TWO*GG(1)
        UPARAM(29) = TWO*GG(2)
        UPARAM(30) = TWO*GG(3)
        UPARAM(31) = IFLG
        UPARAM(32) = E0(1)
        UPARAM(33) = E0(2)
        UPARAM(34) = E0(3)
        UPARAM(35) = C0(1)
        UPARAM(36) = C0(2)
        UPARAM(37) = C0(3)
        UPARAM(38) = ABCS
        UPARAM(39) = PMIN_(1)
        UPARAM(40) = PMIN_(2)
        UPARAM(41) = PMIN_(3)
        UPARAM(42) = VDET
        UPARAM(43) = PCJ
        IF(PCJ > EM20)THEN
          UPARAM(44) = RHO0_(4) * VDET**2 / PCJ
        ELSE
          UPARAM(44) = INFINITY
        END IF
        VCJ   = ONE - ONE/UPARAM(44)
        IF(RHO0_(4)==ZERO) UPARAM(47) = EM20 !RHO0_(4)
        UPARAM(48) = E0(4)
        UPARAM(49) = C0(4)
        UPARAM(50) = C1(4)
        UPARAM(45) = B1
        UPARAM(51) = B2
        UPARAM(52) = R1
        UPARAM(53) = R2
        UPARAM(54) = W
        UPARAM(55) = IEXP
        UPARAM(56) = PMIN_(4)
        UPARAM(57) = EINF(1)
        UPARAM(58) = EINF(2)
        UPARAM(59) = EINF(3)
        UPARAM(60) = EINF(4)               
        UPARAM(61) = IOPT  
        UPARAM(62) = ONE/N_LC
        UPARAM(63) = IPLA_
        UPARAM(64) = IPLA(1)
        UPARAM(65) = IPLA(2)
        UPARAM(66) = IPLA(3)
        UPARAM(67) = IPLA(4)
        UPARAM(68) = IBFRAC
        UPARAM(69) = RHO0_(1) * AV(1) + RHO0_(2) * AV(2) + RHO0_(3) * AV(3) + RHO0_(4) * AV(4)
        UPARAM(70) = TCARP
        UPARAM(71) = TCAR
        UPARAM(72) = INFINITY    !P0_NRF
        UPARAM(75) = VEL
        UPARAM(81) = ZERO !visc1
        UPARAM(82) = ZERO !visc2
        UPARAM(83) = ZERO !visc3
        UPARAM(84) = ZERO !visc4
        IDX=100
        DO I=1,3
          UPARAM(IDX+01) = GG(I)
          UPARAM(IDX+02) = Y(I)
          UPARAM(IDX+03) = BB(I)
          UPARAM(IDX+04) = N(I)
          UPARAM(IDX+05) = CC(I)
          UPARAM(IDX+06) = EPDR(I)
          UPARAM(IDX+07) = CM(I)
          UPARAM(IDX+08) = TMELT(I)
          UPARAM(IDX+09) = THETL(I)
          UPARAM(IDX+10) = PLAMX(I)
          UPARAM(IDX+11) = SIGMX(I)
          UPARAM(IDX+12) = SPH(I)
          UPARAM(IDX+13) = T0(I)
          UPARAM(IDX+14) = XKA(I)
          UPARAM(IDX+15) = XKB(I)
          UPARAM(IDX+16) = A0(I)
          UPARAM(IDX+17) = A1(I)
          UPARAM(IDX+18) = A2(I)
          UPARAM(IDX+19) = AMX(I)
          UPARAM(IDX+20) = MUMX(I)
          UPARAM(IDX+21) = BUNL(I)
          UPARAM(IDX+22) = NU(I)
          UPARAM(IDX+23) = PSTAR(I)
          UPARAM(IDX+24) = SSP(I)
          UPARAM(IDX+25) = LC(I)
          UPARAM(IDX+26) = RHO0_(I)*SSP(I)*SSP(I)
          IDX=IDX+50
        ENDDO
        !I=4
        UPARAM(258) = TMELT(4)
        UPARAM(259) = THETL(4)
        UPARAM(262) = SPH(4)
        UPARAM(263) = T0(4)
        UPARAM(264) = XKA(4)
        UPARAM(265) = XKB(4)
        UPARAM(273) = SSP(4)
        UPARAM(274) = LC(4)
        UPARAM(275) = RHO0_(4)*SSP(4)*SSP(4)
        UPARAM(276) = ZERO
        !bijective application (in case of new input you must retrieve phase order in legacy buffer)
        ! example jwl is necessarily id 4 in legacy buffer, but not necessarily in user input (iform=12)
        UPARAM(277) = 1
        UPARAM(278) = 2
        UPARAM(279) = 3
        UPARAM(280) = 4
        !functions
        IFUNC(01) = IAV(1)
        IFUNC(02) = IRHO(1)
        IFUNC(03) = IE(1)
        IFUNC(04) = IAV(2)
        IFUNC(05) = IRHO(2)
        IFUNC(06) = IE(2)
        IFUNC(07) = IAV(3)
        IFUNC(08) = IRHO(3)
        IFUNC(09) = IE(3)
        IFUNC(10) = IVEL
      ENDIF
      PM(38) = UPARAM(42)  !VDET
      PARMAT(1) = MAX(C1(1), C1(2), C1(3), C1(4))
      PM(27) = MAXVAL(SSP(1:4))
      !============================!
      ! Starter Listing File       !
      !============================!
      WRITE(IOUT,997) TRIM(TITR),MAT_ID,51
      WRITE(IOUT,998)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF(IFLG /= 12)THEN
          WRITE(IOUT, 900)RHO0,RHOR
          WRITE(iOUT,5002)IFLAG_BAK
          WRITE(IOUT,5001)PEXT,VIS,VISV,AV(1),AV(2),AV(3),AV(4)
        ENDIF
        !============================!
        ! IFLG == 0,1,10,11          !
        !============================!
          IF(IFLG <= 1)THEN
            P0_GLOB = P0(1)*AV(1)+P0(2)*AV(2)+P0(3)*AV(3)+P0(4)*AV(4)
            DO I=1,3
             WRITE(IOUT,5005)I
             WRITE(IOUT,5010)
             WRITE(IOUT,5011)C0(I),C1(I),C2(I),C3(I),C4(I),C5(I),E0(I),RHO0_(I),PMIN_(I),P0_GLOB
             IF(Y(I) == ZERO)IPLA(I)=0
             SELECT CASE(IPLA(I))
               CASE(0)
                 IF(GG(I)>ZERO)THEN
                   WRITE(IOUT,5015)
                   WRITE(IOUT,5016)GG(I)
                 ENDIF
               CASE(1)
                 WRITE(IOUT,5020)
                 WRITE(IOUT,5021)Y(I),BB(I),N(I),CC(I),EPDR(I),CM(I),T0(I),TMELT(I),THETL(I),SPH(I),PLAMX(I),SIGMX(I),XKA(I),XKB(I)
               CASE(2)
                 WRITE(IOUT,5025)
                 WRITE(IOUT,5026)A0(I),A1(I),A2(I),AMX(I),Y(I),NU(I),T0(I),TMELT(I),THETL(I),SPH(I),PLAMX(I),SIGMX(I),XKA(I),XKB(I)
             END SELECT
           ENDDO
           I=4
           WRITE(IOUT,5005)I
           WRITE(IOUT,5030)
           WRITE(IOUT,5031)RHO0_(4),E0(4),PMIN_(4),C0(4),C1(4),B1,B2,R1,R2,W,VDET,PCJ,VCJ,IBFRAC
        !============================!
        ! IFLG == 2                  !
        !============================!
        ELSEIF(IFLG == 2)THEN
          WRITE(IOUT,1200)ABCS,VEL,IVEL,
     &                    AV(1),RHO0_(1),E0(1),IAV(1),IRHO(1),IE(1),PMIN_(1),C0(1),C1(1),C2(1),C3(1),C4(1),C5(1),
     &                    AV(2),RHO0_(2),E0(2),IAV(2),IRHO(2),IE(2),PMIN_(2),C0(2),C1(2),C2(2),C3(2),C4(2),C5(2),
     &                    AV(3),RHO0_(3),E0(3),IAV(3),IRHO(3),IE(3),PMIN_(3),C0(3),C1(3),C2(3),C3(3),C4(3),C5(3)

        !============================!
        ! IFLG == 3                  !
        !============================!
        ELSEIF(IFLG == 3)THEN
          IF(IOPT == 1)THEN
            WRITE(IOUT,1299)IOPT
          ELSE
            WRITE(IOUT,1298)
          ENDIF
          WRITE(IOUT,1300)AV(1),RHO0_(1),E0(1),C0(1),
     &                    AV(2),RHO0_(2),E0(2),C0(2),
     &                    AV(3),RHO0_(3),E0(3),C0(3)

        !============================!
        ! IFLG == 4                  !
        !============================!
        ELSEIF(IFLG == 4)THEN
          WRITE(IOUT,1400)ABCS,
     &          AV(1),RHO0_(1),E0(1),IAV(1),IRHO(1),IE(1),PMIN_(1),C0(1),C1(1),C4(1),
     &          AV(2),RHO0_(2),E0(2),IAV(2),IRHO(2),IE(2),PMIN_(2),C0(2),C1(2),C4(2),
     &          AV(3),RHO0_(3),E0(3),IAV(3),IRHO(3),IE(3),PMIN_(3),C0(3),C1(3),C4(3)

        !============================!
        ! IFLG == 5                  !
        !============================!
        ELSEIF(IFLG == 5)THEN
          WRITE(IOUT,1500)ABCS,
     &                    AV(1),RHO0_(1),E0(1),IAV(1),IRHO(1),IE(1),PMIN_(1),C0(1),C1(1),
     &                    AV(2),RHO0_(2),E0(2),IAV(2),IRHO(2),IE(2),PMIN_(2),C0(2),C1(2),
     &                    AV(3),RHO0_(3),E0(3),IAV(3),IRHO(3),IE(3),PMIN_(3),C0(3),C1(3)
        !============================!
        ! IFLG == 6                  !
        !============================!
        ELSEIF(IFLG == 6)THEN
          WRITE(IOUT,1700)CPEXT,CTCARP,CTCAR
          IF(IFLG6_SUBMAT_DEFINED(1) .OR. IFLG6_SUBMAT_DEFINED(2) .OR. IFLG6_SUBMAT_DEFINED(3))
     &    WRITE(IOUT,1701)CAV(1) ,CRHO0(1),CE0(1) ,CPM(1) ,CC0(1) ,CSSP(1),
     &                    CAV(2) ,CRHO0(2),CE0(2) ,CPM(2) ,CC0(2) ,CSSP(2),
     &                    CAV(3) ,CRHO0(3),CE0(3) ,CPM(3) ,CC0(3) ,CSSP(3)
        !============================!
        ! IFLG == 12                  !
        !============================!
        ELSEIF(IFLG == 12)THEN
          WRITE(IOUT,5002)12
          WRITE(IOUT,4003)VIS,VISV
          WRITE(IOUT,4004)
          DO I=1,MIP
            WRITE(IOUT,4001)I,tMID(I)
          ENDDO
          WRITE(IOUT,4004)
          DO I=1,MIP
            WRITE(IOUT,4002)I,tAV(I)
          ENDDO
        ENDIF
        !============================!
        IF(NITER  /= 10)THEN
          WRITE(IOUT,901)NITER
        ENDIF   
      ENDIF  
      
      PM(38)=VDET    
      IF(IFLG==12)RHO0=ONE    
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
      PM(91)=MAXVAL(RHO0_(1:4))


      RETURN

C======================================================================|
  900 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)
  901 FORMAT(
     & 5X,'NUMBER OF ITERATION . . . . . . . . . .=',I10/)
  997 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
  998 FORMAT(
     & 5X,'  /MAT/LAW51 (MULTIMAT)',/,
     & 5X,'  ---------------------',/)
 1200 FORMAT(
     & 5X,'INLET CONDITION------------------------',/
     & 5X,'Abcissa scale factor. . . . . . . . . .=',E12.4/
     & 5X,'Velocity Scale Factor . . . . . . . . .=',E12.4/
     & 5X,'Velocity Function ID. . . . . . . . . .=',I10/
     & 5X,'SUBMAT-1 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-1 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-1 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-1 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-1 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,' P =C0 +C1 mu +C2 mu^2 +C3 mu^3 + (C4 + C5mu)Eint/Vo '/
     & 5X,'SUBMAT-1 C1. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C2. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C3. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C4. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C5. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-2 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-2 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-2 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-2 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C1. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C2. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C3. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C4. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C5. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-3 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-3 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-3 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-3 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C1. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C2. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C3. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C4. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C5. . . . . . . . . . . . . . .=',E12.4//)
 1298 FORMAT(
     & 5X,'OUTLET CONDITION-----------------------')
 1299 FORMAT(
     & 5X,'OUTLET CONDITION-----------------------',/
     & 5X,'IOPT FLAG . . . . . . . . . . . . . . .=',I10)
 1300 FORMAT(
     & 5X,'SUBMAT-1 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-1 PO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-2 PO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-3 PO initial PRESSURE . . . . . .=',E12.4//)
 1400 FORMAT(
     & 5X,'INLET STAGNATION PRESSURE (Perfect Gas)',/
     & 5X,'Abcissa scale factor. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-1 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-1 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-1 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-1 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,' P =C0 +C1 mu +C2 mu^2 +C3 mu^3 + (C4 + C5mu)Eint/Vo '/
     & 5X,'SUBMAT-1 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C1. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C4 (gamma-1). . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-2 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-2 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-2 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-2 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C1. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C4 (gamma-1). . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-3 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-3 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-3 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-3 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C1. . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C4 (gamma-1). . . . . . . . . .=',E12.4//)
 1500 FORMAT(
     & 5X,'INLET STAGNATION PRESSURE (Liquid) ----',/
     & 5X,'Abcissa scale factor. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-1 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-1 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-1 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-1 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,' P =C0 +C1 mu +C2 mu^2 +C3 mu^3 + (C4 + C5mu)Eint/Vo '/
     & 5X,'SUBMAT-1 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-1 C1 (bulk modulus) . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-2 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-2 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-2 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-2 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 C1 (bulk modulus) . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 VOLUME RATIO. . . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 reference DENSITY . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 E0 initial ENERGY/unit volume .=',E12.4/
     & 5X,'SUBMAT-3 Volume ratio FUNCTION ID . . . =',I10/
     & 5X,'SUBMAT-3 Density FUNCTION ID . . . . . .=',I10/
     & 5X,'SUBMAT-3 Energy FUNCTION ID. . . . . . .=',I10/
     & 5X,'SUBMAT-3 Minimum PRESSURE. . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 CO initial PRESSURE . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 C1 (bulk modulus) . . . . . . .=',E12.4//)
 1700 FORMAT(
     & 5X,'NON REFLECTING FRONTIER ----------------',/
     & 5X,'EXTERNAL PRESSURE  . . . . . . . . . . .=',A    /
     & 5X,'CHARACTERISTIC TIME FOR PRES RELAXATION =',A    /
     & 5X,'CHARACTERISTIC TIME FOR VFRAC RELAXATION=',A    //)
 1701 FORMAT(
     & 5X,'SUBMAT-1 AV10 VOLUME FRACTION. . . . . .=',A    /
     & 5X,'SUBMAT-1 RHO10 REFERENCE DENSITY . . . .=',A    /
     & 5X,'SUBMAT-1 E01 VOLUMETRIC ENERGY . . . . .=',A    /
     & 5X,'SUBMAT-1 PM1 CUT OFF PRESSURE. . . . . .=',A    /
     & 5X,'SUBMAT-1 P01 INITIAL PRESSURE. . . . . .=',A    /
     & 5X,'SUBMAT-1 SSP1 SOUND SPEED. . . . . . . .=',A    /
     & 5X,'SUBMAT-2 AV20 VOLUME FRACTION. . . . . .=',A    /
     & 5X,'SUBMAT-2 RHO20 REFERENCE DENSITY . . . .=',A    /
     & 5X,'SUBMAT-2 E02 VOLUMETRIC ENERGY . . . . .=',A    /
     & 5X,'SUBMAT-2 PM2 CUT OFF PRESSURE. . . . . .=',A    /
     & 5X,'SUBMAT-2 P02 INITIAL PRESSURE. . . . . .=',A    /
     & 5X,'SUBMAT-2 SSP2 SOUND SPEED. . . . . . . .=',A    /
     & 5X,'SUBMAT-3 AV30 VOLUME FRACTION. . . . . .=',A    /
     & 5X,'SUBMAT-3 RHO30 REFERENCE DENSITY . . . .=',A    /
     & 5X,'SUBMAT-3 E03 VOLUMETRIC ENERGY . . . . .=',A    /
     & 5X,'SUBMAT-3 PM3 CUT OFF PRESSURE. . . . . .=',A    /
     & 5X,'SUBMAT-3 P03 INITIAL PRESSURE. . . . . .=',A    /
     & 5X,'SUBMAT-3 SSP3 SOUND SPEED. . . . . . . .=',A    //)
 2000 FORMAT(
     & 5X,'Minimum Energies                      ',/
     & 5X,'(Computed for Numerical Stability)    ',/
     & 5X,'SUBMAT-1 Minimum ENERGY. . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-2 Minimum ENERGY. . . . . . . . .=',E12.4/
     & 5X,'SUBMAT-3 Minimum ENERGY. . . . . . . . .=',E12.4//)
 4001 FORMAT(
     & 5X,'SUBMATERIAL-',I1,' ID =   ',I10)
 4002 FORMAT(
     & 5X,'SUBMATERIAL-',I1,' Volume Fraction = ',F12.10)
 4003 FORMAT(
     & 5X,'SHEAR KINEMATIC VISCOSITY. . . . . . . =',E12.4/
     & 5X,'SPHERICAL KINEMATIC VISCOSITY. . . . . =',E12.4)
 4004 FORMAT(
     & 5X,'____________________________________________________')
 5001 FORMAT(
     & 5X,'EXTERNAL PRESSURE. . . . . . . . . . . =',E12.4/
     & 5X,'SHEAR KINEMATIC VISCOSITY. . . . . . . =',E12.4/
     & 5X,'SPHERICAL KINEMATIC VISCOSITY. . . . . =',E12.4/
     & 5X,'____________________________________________________',/
     & 5X,'VOLUME FRACTION - SUBMAT-1 . . . . . . =',F12.10/
     & 5X,'VOLUME FRACTION - SUBMAT-2 . . . . . . =',F12.10/
     & 5X,'VOLUME FRACTION - SUBMAT-3 . . . . . . =',F12.10/
     & 5X,'VOLUME FRACTION - SUBMAT-4 . . . . . . =',F12.10)
 5002 FORMAT(
     & 5X,'FORMULATION FLAG . . . . . . . . . . . =  ',I2)
 5005 FORMAT(
     & 5X,'__________________________',/,
     & 5X,'+S U B M A T E R I A L - ',I1)
 5010 FORMAT(
     & 5X,'|',/,
     & 5X,'+----POLYNOMIAL EoS',/,
     & 5X,'|    --------------')
 5011 FORMAT(
     & 5X,'|    C0 . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'|    C1. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    C2. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    C3. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    C4. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    C5. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    E0. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    RHO0. . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    PMIN. . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'|    INITIAL PRESSURE (COMPUTED) . . . =',1PG20.13)
 5015 FORMAT(
     & 5X,'|',/,
     & 5X,'+----ELASTIC SOLID',/,
     & 5X,'|    -------------')
 5016 FORMAT(
     & 5X,'|    G SHEAR MODULUS . . . . . . . . . =',E12.4)
 5020 FORMAT(
     & 5X,'|',/,
     & 5X,'+----JOHNSON-COOK YIELD CRITERIA',/,
     & 5X,'|    ---------------------------')
 5021 FORMAT(
     & 5X,'|    G      SHEAR MODULUS. . . . . . . =',E12.4/
     & 5X,'|    A      YIELD STRESS . . . . . . . =',E12.4/
     & 5X,'|    B      YIELD FACTOR . . . . . . . =',E12.4/
     & 5X,'|    N      YIELD EXPONENT . . . . . . =',E12.4/
     & 5X,'|    C      STRAIN RATE FACTOR . . . . =',E12.4/
     & 5X,'|    EPS0   REFERENCE STRAIN RATE. . . =',E12.4/
     & 5X,'|    M      THERMAL EXPONENT . . . . . =',E12.4/
     & 5X,'|    T0     INITIAL TEMPERATURE. . . . =',E12.4/
     & 5X,'|    TMELT  MELTING TEMPERATURE. . . . =',E12.4/
     & 5X,'|    TLIM   TEMPERATURE LIMIT. . . . . =',E12.4/
     & 5X,'|    RHO.CP SPECIFIC HEAT (rho0 Cv). . =',E12.4/
     & 5X,'|    MAXIMUM PLASTIC STRAIN. . . . . . =',E12.4/
     & 5X,'|    MAXIMUM STRESS. . . . . . . . . . =',E12.4/
     & 5X,'|    KA  . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    KB  . . . . . . . . . . . . . . . =',E12.4)
 5025 FORMAT(
     & 5X,'|',/,
     & 5X,'+----DRUCKER-PRAGER YIELD CRITERIA     ',/,
     & 5X,'|    -----------------------------     ')
 5026 FORMAT(
     & 5X,'|    A0     YIELD COEFFICIENT 1. . . . =',E12.4/
     & 5X,'|    A1     YIELD COEFFICIENT 2. . . . =',E12.4/
     & 5X,'|    A2     YIELD COEFFICIENT 3. . . . =',E12.4/
     & 5X,'|    A-MAX  MAXIMUM YIELD VALUE. . . . =',E12.4/
     & 5X,'|    E      YOUNG MODULUS. . . . . . . =',E12.4/
     & 5X,'|    NU     YOUNG MODULUS. . . . . . . =',E12.4/
     & 5X,'|    T0     INITIAL TEMPERATURE. . . . =',E12.4/
     & 5X,'|    TMELT  MELTING TEMPERATURE. . . . =',E12.4/
     & 5X,'|    TLIM   TEMPERATURE LIMIT. . . . . =',E12.4/
     & 5X,'|    RHO.CP SPECIFIC HEAT (rho0 Cv). . =',E12.4/
     & 5X,'|    MAXIMUM PLASTIC STRAIN. . . . . . =',E12.4/
     & 5X,'|    MAXIMUM STRESS. . . . . . . . . . =',E12.4/
     & 5X,'|    KA  . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    KB  . . . . . . . . . . . . . . . =',E12.4)
 5030 FORMAT(
     & 5X,'|',/,
     & 5X,'+----JWL EoS',/,
     & 5X,'|    -------')
 5031 FORMAT(
     & 5X,'|    RHO0 INITIAL DENSITY . . . . . . .=',E12.4/
     & 5X,'|    E0   INITIAL ENERGY/UNIT VOLUME . =',E12.4/
     & 5X,'|    PMIN MINIMUM PRESSURE. . . . . . .=',E12.4/
     & 5X,'|    C0   INITIAL PRESSURE . . . . . . =',E12.4/
     & 5X,'|    C1   UNREACTED BULK MODULUS . . . =',E12.4/
     & 5X,'|    A . . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    B . . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    R1. . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    R2. . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    W . . . . . . . . . . . . . . . . =',E12.4/
     & 5X,'|    VDET DETONATION VELOCITY. . . . . =',E12.4/
     & 5X,'|    PCJ  PRESSURE AT C-J STATE . . . .=',E12.4/
     & 5X,'|    VCJ  VOLUME AT C-J STATE . . . . .=',E12.4/
     & 5X,'|    FLAG FOR BURN FRACTION METHOD . . =',I10)
      END
C======================================================================|
