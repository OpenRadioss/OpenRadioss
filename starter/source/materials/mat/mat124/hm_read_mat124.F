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
Chd|  HM_READ_MAT124                source/materials/mat/mat124/hm_read_mat124.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT124(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MTAG     ,
     .           PARMAT   ,UNITAB   ,PM       ,LSUBMODEL,ISRATE   ,
     .           ASRATE   ,MAT_ID   ,TITR     ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD 
      USE ELBUFTAG_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e sXM
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
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, INTENT(OUT)                         :: NUPARAM,NUVAR
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT) :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT)          :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT)                 :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM 
      my_real, INTENT(INOUT)                       :: ASRATE  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ILAW,IRATE,DTYPE,DFLAG,IREG,IDEL
C     REAL ou REAL*8
      my_real
     .   RHO0,YOUNG,NU,A,G,G2,LAM,BULK,FCUT,FC,FT,GFT,
     .   AH,BH,CH,DH,HP,AS,QH0,ECC,M0,WF,WF1,FT1,DF,BS,
     .   EFC,EPSI
      my_real
     .   FC0,EPST0,EPSTMAX,DELTAS,BETAS,EPSC0,EPSCMAX,ALPHAS,GAMMAS
C        
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 124
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2 - Elasticity, flags, strain-rate effect and filtering  
      CALL HM_GET_FLOATV('MAT_E'      ,YOUNG      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('IDEL'       ,IDEL       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('IRATE'      ,IRATE      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('FCUT'       ,ASRATE     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card3 - Eccentricity, strength limits and hardening
      CALL HM_GET_FLOATV('MAT_ECC'    ,ECC        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_QH0'    ,QH0        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV('MAT_FT'     ,FT         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_FC'     ,FC         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_HP'     ,HP         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card4 - Ductility measure parameters
      CALL HM_GET_FLOATV('MAT_AH'     ,AH         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_BH'     ,BH         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_CH'     ,CH         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_DH'     ,DH         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card5 - Damage parameters (part 1)
      CALL HM_GET_FLOATV('MAT_AS'     ,AS         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_BS'     ,BS         ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV('MAT_DF'     ,DF         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('DFLAG'      ,DFLAG      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('DTYPE'      ,DTYPE      ,IS_AVAILABLE,LSUBMODEL)  
      CALL HM_GET_INTV  ('IREG'       ,IREG       ,IS_AVAILABLE,LSUBMODEL)
card6 - Damage parameters (part 2)
      CALL HM_GET_FLOATV('MAT_WF'     ,WF         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_WF1'    ,WF1        ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV('MAT_FT1'    ,FT1        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EFC'    ,EFC        ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
c-----------------------------
c     Default values and check
c-----------------------------
      ! Poisson's ratio
      IF (NU < ZERO .OR. NU >= HALF) THEN
        CALL ANCMSG(MSGID=49,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      ! Elasticity parameter
      G2   = YOUNG / (ONE + NU)
      G    = HALF * G2
      LAM  = G2 * NU /(ONE - TWO*NU)  
      BULK = THIRD * YOUNG / (ONE - NU*TWO)
      ! Bilinear second displacement threshold
      IF (WF1 == ZERO) THEN 
        WF1 = 0.15D0*WF 
      ENDIF
      ! Bilinear damage second uniaxial tensile strength
      IF (FT1 == ZERO) THEN 
        FT1 = 0.3D0*FT  
      ENDIF
      ! Initial hardening
      IF (QH0 == ZERO) THEN 
        QH0 = 0.3D0
      ENDIF
      ! Hardening ductility parameters
      IF (AH == ZERO) THEN 
        AH = 8.0D-2
      ENDIF
      IF (BH == ZERO) THEN 
        BH = 3.0D-3
      ENDIF
      IF (CH == ZERO) THEN 
        CH = 2.0D0
      ENDIF
      IF (DH == ZERO) THEN 
        DH = 1.0D-6
      ENDIF
      ! Dilation constant
      IF (DF == ZERO) THEN 
        DF = 0.85D0
      ENDIF
      ! Compressive inelastic strain threshold
      IF (EFC == ZERO) THEN 
        EFC = 1.0D-4
      ENDIF
      ! Damage parameters
      IF (AS == ZERO) THEN 
        AS = 15.0D0
      ENDIF
      IF (BS == ZERO) THEN 
        BS = ONE
      ENDIF
      ! Eccentricity
      IF (ECC == ZERO) THEN 
        EPSI = FT*((1.16D0*FC)**2 - FC**2)/(1.16D0*FC*(FC**2-FT**2))
        ECC  = (ONE + EPSI)/(TWO - EPSI)
      ENDIF
      ! Friction parameter
      M0 = THREE*(((FC**2)-(FT**2))/(FC*FT))*(ECC/(ECC + ONE))
      ! Element deletion flag check
      IF (IDEL == 0) IDEL = 1
      IDEL = MIN(IDEL,2)
      IDEL = MAX(IDEL,1)
      ! Dflag check 
      IF (DFLAG == 0) DFLAG = 1
      DFLAG = MIN(MAX(1,DFLAG),4)
      ! Dtype check
      IF (DTYPE == 0) DTYPE = 2
      DTYPE = MIN(MAX(1,DTYPE),3)
      ! Regularization flag check
      IF (IREG == 0) IREG = 2
      IREG = MIN(IREG,2)
      IREG = MAX(IREG,1)
      ! Strain rate parameters
      FC0     = TEN*EP06*UNITAB%FAC_T_WORK*UNITAB%FAC_T_WORK*UNITAB%FAC_L_WORK/UNITAB%FAC_M_WORK
      ! -> For tension
      EPST0   = 30.0D0*EM06*UNITAB%FAC_T_WORK
      EPSTMAX = ONE*UNITAB%FAC_T_WORK 
      DELTAS  = ONE / (ONE + EIGHT*(FC/FC0))
      BETAS   = EXP(SIX*DELTAS - TWO)
      ! -> For compression
      EPSC0   = 30.0D0*EM06*UNITAB%FAC_T_WORK
      EPSCMAX = 30.0D0*UNITAB%FAC_T_WORK 
      ALPHAS  = ONE / (FIVE + NINE*(FC/FC0))
      GAMMAS  = EXP(6.156D0*ALPHAS - TWO)  
      ! Strain rate effect check
      IF (IRATE == 0) IRATE = 1
      IRATE = MIN(IRATE,2)
      IRATE = MAX(IRATE,1)
      IF (IRATE > 1) THEN
        ISRATE  = 1
        ! Strain rate filtering frequency
        IF (ASRATE == ZERO) THEN 
          ASRATE = 10000.0D0*UNITAB%FAC_T_WORK  
        ENDIF
      ELSE 
        ISRATE  = 0
        ASRATE  = ZERO
      ENDIF
c
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameters
      NUPARAM = 36
      ! Number of user variables 
      NUVAR = 18
c      
      ! Material parameters
      !  -> Elastic parameters   
      UPARAM(1)  = YOUNG    ! Young modulus
      UPARAM(2)  = NU       ! Poisson's ratio
      UPARAM(3)  = G        ! Shear modulus
      UPARAM(4)  = G2       ! 2*Shear modulus
      UPARAM(5)  = LAM      ! Lame coefficient
      UPARAM(6)  = BULK     ! Bulk modulus
      !  -> Plastic parameters
      UPARAM(7)  = FT       ! Uniaxial tensile strength
      UPARAM(8)  = FC       ! Uniaxial compressive strength
      UPARAM(9)  = ECC      ! Eccentricity
      UPARAM(10) = M0       ! Friction parameter
      UPARAM(11) = QH0      ! Initial hardening
      UPARAM(12) = HP       ! Hardening modulus
      UPARAM(13) = AH       ! Hardening ductility parameter 1
      UPARAM(14) = BH       ! Hardening ductility parameter 2
      UPARAM(15) = CH       ! Hardening ductility parameter 3
      UPARAM(16) = DH       ! Hardening ductility parameter 4
      !  -> Damage parameters
      UPARAM(17) = AS       ! Damage ductility measure
      UPARAM(18) = BS       ! Damage ductility parameter
      UPARAM(19) = DF       ! Dilation parameter
      UPARAM(20) = DFLAG    ! Damage flag 
      UPARAM(21) = DTYPE    ! Tensile damage type
      UPARAM(22) = IREG     ! Regularization flag
      UPARAM(23) = WF       ! First displacement threshold
      UPARAM(24) = WF1      ! Second displacement threshold
      UPARAM(25) = FT1      ! Second uniaxial tensile strength
      UPARAM(26) = EFC      ! Compressive inelastic strain threshold
      !  -> Strain rate effect parameters
      UPARAM(27) = IRATE    ! Strain rate effect flag
      UPARAM(28) = EPST0    ! Reference tensile strain rate
      UPARAM(29) = EPSTMAX  ! Maximum tensile strain rate threshold
      UPARAM(30) = DELTAS   ! Tensile strain rate effect parameter 1
      UPARAM(31) = BETAS    ! Tensile strain rate effect parameter 2
      UPARAM(32) = EPSC0    ! Reference tensile strain rate
      UPARAM(33) = EPSCMAX  ! Maximum compressive strain rate threshold
      UPARAM(34) = ALPHAS   ! Compressive strain rate effect parameter 1
      UPARAM(35) = GAMMAS   ! Compressive strain rate effect parameter 2
      !  -> Element deletion flag
      UPARAM(36) = IDEL 
c      
      ! PARMAT table
      PARMAT(1) = BULK
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = ASRATE
c
      ! PM table
      PM(1)  = RHO0
      PM(89) = RHO0
      PM(27) = SQRT((BULK + FOUR_OVER_3*G)/RHO0)  ! sound speed estimation
      PM(100)= BULK   
c      
      ! MTAG variable activation
      MTAG%G_PLA    = 1
      MTAG%L_PLA    = 1
      MTAG%G_EPSD   = 1
      MTAG%L_EPSD   = 1
      MTAG%G_DMG    = 1
      MTAG%L_DMG    = 1
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL" )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"HOOK")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
c
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW 
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) YOUNG,NU
        WRITE(IOUT,1400) IRATE
        IF (IRATE > 0) WRITE(IOUT,1500) ASRATE
        WRITE(IOUT,1600) ECC,QH0,FT,FC,HP
        WRITE(IOUT,1700) AH,BH,CH,DH
        WRITE(IOUT,1800) AS,DF,BS
        WRITE(IOUT,1900) DFLAG
        WRITE(IOUT,2000) DTYPE
        WRITE(IOUT,2100) IREG
        WRITE(IOUT,2200) WF,WF1,FT1,EFC
        WRITE(IOUT,2300) IDEL
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT(
     & 5X,'-----------------------------------------------------------',/
     & 5X,'            CONCRETE DAMAGE PLASTICITY MODEL 2             ',/,
     & 5X,'-----------------------------------------------------------',/) 
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG (YOUNG MODULUS) . . . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'NU (POISSON RATIO). . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'STRAIN RATE EFFECT FLAG IRATE . . . . . . . . . . . . . . . .=',I3/
     & 5X,'     1: NO STRAIN RATE EFFECT (DEFAULT)                       '/
     & 5X,'     2: STRAIN RATE EFFECT ACTIVATED                          '/)
 1500 FORMAT(
     & 5X,'STRAIN RATE FILTERING CUTOFF FREQUENCY. . . . . . . . . . . .=',1PG20.13/)
 1600 FORMAT( 
     & 5X,'ECC (ECCENTRICITY). . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'QH0 (INITIAL HARDENING) . . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'FT  (UNIAXIAL TENSION STRENGTH) . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FC  (UNIAXIAL COMPRESSION STRENGTH) . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'HP  (HARDENING MODULUS) . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'AH  (HARDENING DUCTILITY PARAM 1) . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'BH  (HARDENING DUCTILITY PARAM 2) . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'CH  (HARDENING DUCTILITY PARAM 3) . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DH  (HARDENING DUCTILITY PARAM 4) . . . . . . . . . . . . . .=',1PG20.13/)
 1800 FORMAT( 
     & 5X,'AS  (DAMAGE DUCTILITY MEASURE). . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'DF  (DILATION CONSTANT) . . . . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'BS  (DAMAGE DUCTILITY PARAMETER). . . . . . . . . . . . . . .=',1PG20.13/)
 1900 FORMAT( 
     & 5X,'DFLAG (DAMAGE FLAG) . . . . . . . . . . . . . . . . . . . . .=',I3/
     & 5X,'     1: STANDARD MODEL WITH TWO DAMAGE VARIABLES (DEFAULT)    ',/
     & 5X,'     2: ISOTROPIC MODEL WITH ONE DAMAGE VARIABLE              ',/
     & 5X,'     3: MULTIPLICATIVE MODEL WITH TWO DAMAGE VARIABLES        ',/
     & 5X,'     4: NO DAMAGE EFFECT                                      ',/)
 2000 FORMAT( 
     & 5X,'DTYPE (TENSION DAMAGE SHAPE)  . . . . . . . . . . . . . . . .=',I3/
     & 5X,'     1: LINEAR SOFTENING                                      ',/
     & 5X,'     2: BILINEAR SOFTENING (DEFAULT)                          ',/
     & 5X,'     3: EXPONENTIAL SOFTENING                                 ',/)
 2100 FORMAT( 
     & 5X,'ELEMENT LENGTH REGULARIZATION FLAG. . . . . . . . . . . . . .=',I3/
     & 5X,'     1: NO REGULARIZATION                                     ',/
     & 5X,'     2: REGULARIZATION ACTIVATED (DEFAULT)                    ',/)
 2200 FORMAT( 
     & 5X,'WF  (DAMAGE DISPLACEMENT THRESHOLD 0) . . . . . . . . . . . .=',1PG20.13/
     & 5X,'WF1 (DAMAGE DISPLACEMENT THRESHOLD 1) . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FT1 (UNIAXIAL STRESS THRESHOLD 1) . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'EFC (STRAIN THRESHOLD IN COMPRESSION) . . . . . . . . . . . .=',1PG20.13/)
 2300 FORMAT( 
     & 5X,'ELEMENT DELETION FLAG. . . . . . .. . . . . . . . . . . . . .=',I3/
     & 5X,'     1: NO ELEMENT DELETION (DEFAULT)                         ',/
     & 5X,'     2: ELEMENT DELETION ACTIVATED                            ',/)
c-----------------------------------------------------------------------
      END
