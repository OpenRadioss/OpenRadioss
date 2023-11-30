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
Chd|  HM_READ_MAT110                source/materials/mat/mat110/hm_read_mat110.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT110(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NTABL    ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM       ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,ITABLE   ,MAXTABL  ,
     .           NVARTMP  ,MATPARAM )
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
#include      "sysunit.inc"
#include      "param_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)               :: ISRATE,ITABLE(MAXTABL)
      INTEGER, INTENT(INOUT)                 :: NUPARAM,NUVAR,NTABL,NVARTMP
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT) :: MATPARAM
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I, J, K, ILAW, Ivflag, NANGLE, INFO, Icrit, TAB_YLD, Ismooth,
     .         TAB_TEMP,ITER,Ires
C     REAL
      my_real
     .   RHO0,YOUNG,NU,YLD0,DSIGM,BETA,OMEGA,NEXP,EPS0,SIGSTAR,DG0,
     .   Deps0,MEXP,fBI,rhoBI,RHOR,BULK,A11,G,N1,N2,M1,M2,A1,A2,C1,C2,
     .   ASRATE,KBOLTZ,XSCALE,YSCALE,FISOKIN,TINI,rhoBI_theta,B1,B2,MU,
     .   XSCALE_UNIT,YSCALE_UNIT
      my_real
     .   F1,F2,DF1_DMU,DF2_DMU,D2F1_D2MU,D2F2_D2MU,EIG1,EIG2,
     .   DPHI_DSIG1,DPHI_DSIG2,DMU_DSIG1,DMU_DSIG2,MINEIG,THET,DA_DCOS2(2),
     .   DB_DCOS2(2),DC_DCOS2(2),D2A_D2COS2(2),D2B_D2COS2(2),D2C_D2COS2(2),
     .   DTHET_DCOS2,D2THET_D2THETCOS2,DF1_DCOS2,DF2_DCOS2,DPHI_DCOS2,
     .   D2F1_D2COS2,D2F2_D2COS2,D2F1_D2MUCOS2,D2F2_D2MUCOS2,MINEIGMU,
     .   MINEIGTHET,NUMER,DNUMER_DMU,DNUMER_DCOS2,DENOM,DDENOM_DMU,DDENOM_DCOS2,
     .   DMU_DCOS2,T(3,3),D(3,3),WEIGHT,DWEIGHT_DCOS2,D2WEIGHT_D2COS2,U,UPRIM,UPPRIM,
     .   DU_DMU,DUPRIM_DMU,V,VPRIM,VPPRIM,DV_DMU,DVPRIM_DMU,W0,fUN_AV,rLank_AV,
     .   fBI_MIN,fBI_MAX,fBI_TRANS,ADIRAC,FDIRAC,fPS_MIN,fPS_MAX,fPS_TRANS,
     .   fSH_MIN,fSH_MAX,fSH_TRANS,fH(2),fPS1,rhoONE_theta,PHI,DfPS1_DWEIGHT
      REAL*8  :: COS2(10,10),D2PHI_D2(3,3),WORK(102),WR(3),WI(3),VL(3,3),VR(3,3)
      REAL*8 , DIMENSION(:,:), ALLOCATABLE :: AMAT,BVEC
      my_real, DIMENSION(:,:), ALLOCATABLE :: fUN,fSH,fPS,HIPS,HIUN,HISH,
     .                                        Q_fSH,Q_fUN,Q_fPS,Q_HIUN,Q_HIPS,Q_HISH
      my_real, DIMENSION(:)  , ALLOCATABLE :: rLank,THETA,THETA_RAD,
     .                                        WPS,WSH,Q_WSH,Q_WPS,ALPS,RM,AG,
     .                                        EPSAG,SIGAG,CAG,EPSEQ
      INTEGER, DIMENSION(:), ALLOCATABLE   :: IPIV      
      my_real, PARAMETER :: TOL = 1.0D-6
C     
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C
      DATA  COS2/
     1 1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,   
     2 0.   ,1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     3 -1.  ,0.   ,2.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     4 0.   ,-3.  ,0.   ,4.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     5 1.   ,0.   ,-8.  ,0.   ,8.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     6 0.   ,5.   ,0.   ,-20. ,0.   ,16.  ,0.   ,0.   ,0.   ,0.   ,     
     7 -1.  ,0.   ,18.  ,0.   ,-48. ,0.   ,32.  ,0.   ,0.   ,0.   ,     
     8 0.   ,-7.  ,0.   ,56.  ,0.   ,-112.,0.   ,64.  ,0.   ,0.   ,     
     9 1.   ,0.   ,-32. ,0.   ,160. ,0.   ,-256.,0.   ,128. ,0.   ,     
     A 0.   ,9.   ,0.   ,-120.,0.   ,432. ,0.   ,-576 ,0.   ,256. /
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 110
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 - Elasticity  
      CALL HM_GET_FLOATV('MAT_E'     ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Ires'  ,Ires     ,IS_AVAILABLE, LSUBMODEL)
card3
      CALL HM_GET_INTV  ('MAT_Icrit' ,Icrit    ,IS_AVAILABLE, LSUBMODEL)
      ! Default : classical Vegter yield locus
      IF (Icrit == 0) Icrit = 1 
      IF (Icrit > 4) THEN 
        CALL ANCMSG(MSGID=1776,MSGTYPE=MSGERROR,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .    I2=Icrit)
      ENDIF
      CALL HM_GET_INTV  ('MAT_TAB_YLD',TAB_YLD  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Xscale',XSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Yscale',YSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_fBI'   ,fBI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_rhoBI' ,rhoBI    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
card4 - Hardening        
      CALL HM_GET_FLOATV('SIGMA_r'  ,YLD0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF ((TAB_YLD == 0).AND.(YLD0==ZERO)) THEN 
        CALL ANCMSG(MSGID=1777,MSGTYPE=MSGERROR,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF
      CALL HM_GET_FLOATV('MAT_DSIGM' ,DSIGM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'  ,BETA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Omega'     ,OMEGA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'     ,NEXP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
card5 - Strain-rate and temperature dependency
      CALL HM_GET_FLOATV('Epsilon_0' ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGS'  ,SIGSTAR  ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_DG0'   ,DG0      ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_Deps0' ,Deps0    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_StrainRate_m',MEXP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
card6 - Strain-rate computation
      CALL HM_GET_FLOATV('T_Initial' ,TINI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (TINI==ZERO) THEN 
        CALL ANCMSG(MSGID=1778,MSGTYPE=MSGWARNING,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF      
      CALL HM_GET_FLOATV('MAT_CHARD'  ,FISOKIN  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fcut'      ,ASRATE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Vflag'     ,Ivflag   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Ismooth',Ismooth  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_TAB_TEMP'  ,TAB_TEMP ,IS_AVAILABLE, LSUBMODEL)
card7 - Yield criterion parameters
      ! Classical Vegter yield locus
      IF (Icrit == 1) THEN     
        CALL HM_GET_INTV('MAT_refanglemax',NANGLE,IS_AVAILABLE,LSUBMODEL)
        IF (NANGLE > 10) THEN 
          CALL ANCMSG(MSGID=1779,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)      
        ENDIF
        IF (.NOT.ALLOCATED(fUN))   ALLOCATE(fUN(NANGLE,2))
        IF (.NOT.ALLOCATED(fSH))   ALLOCATE(fSH(NANGLE,2))
        IF (.NOT.ALLOCATED(fPS))   ALLOCATE(fPS(NANGLE,2))
        fPS(1:NANGLE,1:2) = ZERO
        IF (.NOT.ALLOCATED(rLank)) ALLOCATE(rLank(NANGLE))
        ! Loop over angles (must be equally distributed between 0 and pi/2)
        DO J = 1, NANGLE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fUN_THETA' ,fUN(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_R_THETA'   ,rLank(J),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF (rLank(J) == 0) rLank(J) = ONE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fPS1_THETA',fPS(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fPS2_THETA',fPS(J,2),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fSH_THETA' ,fSH(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
      ! Standard Vegter yield locus
      ELSEIF (Icrit == 2) THEN   
        CALL HM_GET_INTV('MAT_refanglemax',NANGLE,IS_AVAILABLE,LSUBMODEL)
        IF (NANGLE > 10) THEN 
          CALL ANCMSG(MSGID=1779,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)      
        ENDIF
        IF (.NOT.ALLOCATED(fUN))   ALLOCATE(fUN(NANGLE,2))
        IF (.NOT.ALLOCATED(fSH))   ALLOCATE(fSH(NANGLE,2))
        IF (.NOT.ALLOCATED(fPS))   ALLOCATE(fPS(NANGLE,2))
        fPS(1:NANGLE,1:2) = ZERO
        IF (.NOT.ALLOCATED(ALPS))  ALLOCATE(ALPS(NANGLE))
        IF (.NOT.ALLOCATED(rLank)) ALLOCATE(rLank(NANGLE))
        ! Loop over angles (must be equally distributed between 0 and pi/2)
        DO J = 1, NANGLE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fUN_THETA' ,fUN(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_R_THETA'   ,rLank(J),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF (rLank(J) == 0) rLank(J) = ONE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fPS1_THETA',fPS(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ALPS_THETA',ALPS(J) ,J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF (ALPS(J) == ZERO) ALPS(J) = HALF
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fSH_THETA' ,fSH(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
      ! Vegter - 2017 yield locus
      ELSEIF (Icrit == 3) THEN   
        ! Number of angle is fixed to three in this case
        NANGLE = 3
        IF (.NOT.ALLOCATED(RM))        ALLOCATE(RM(NANGLE))
        IF (.NOT.ALLOCATED(AG))        ALLOCATE(AG(NANGLE))
        IF (.NOT.ALLOCATED(EPSAG))     ALLOCATE(EPSAG(NANGLE))
        IF (.NOT.ALLOCATED(SIGAG))     ALLOCATE(SIGAG(NANGLE))
        IF (.NOT.ALLOCATED(CAG))       ALLOCATE(CAG(NANGLE))
        IF (.NOT.ALLOCATED(EPSEQ))     ALLOCATE(EPSEQ(NANGLE))
        IF (.NOT.ALLOCATED(fUN))       ALLOCATE(fUN(NANGLE,2))
        IF (.NOT.ALLOCATED(fSH))       ALLOCATE(fSH(NANGLE,2))
        IF (.NOT.ALLOCATED(fPS))       ALLOCATE(fPS(NANGLE,2))
        IF (.NOT.ALLOCATED(rLank))     ALLOCATE(rLank(NANGLE))
        IF (.NOT.ALLOCATED(THETA))     ALLOCATE(THETA(NANGLE))
        IF (.NOT.ALLOCATED(THETA_RAD)) ALLOCATE(THETA_RAD(NANGLE))
        CALL HM_GET_FLOATV('MAT_RM_0'  ,RM(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_RM_45' ,RM(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_RM_90' ,RM(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_AG_0'  ,AG(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_AG_45' ,AG(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_AG_90' ,AG(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_R_0'   ,rLank(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (rLank(1) == ZERO) rLank(1) = ONE
        CALL HM_GET_FLOATV('MAT_R_45'  ,rLank(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (rLank(2) == ZERO) rLank(2) = ONE
        CALL HM_GET_FLOATV('MAT_R_90'  ,rLank(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (rLank(3) == ZERO) rLank(3) = ONE
c        
        ! Computation of uniaxial reference point
        !  Uniaxial strain at AG
        EPSAG(1) = LOG(ONE + AG(1)/HUNDRED)
        EPSAG(2) = LOG(ONE + AG(2)/HUNDRED)
        EPSAG(3) = LOG(ONE + AG(3)/HUNDRED)
        !  Uniaxial stress at AG
        SIGAG(1) = RM(1)*(ONE + AG(1)/HUNDRED)
        SIGAG(2) = RM(2)*(ONE + AG(2)/HUNDRED)
        SIGAG(3) = RM(3)*(ONE + AG(3)/HUNDRED)
        !  Energy at 0 deg direction
        CAG(1)   = SIGAG(1)/(EPSAG(1)**EPSAG(1))
        CAG(2)   = SIGAG(2)/(EPSAG(2)**EPSAG(2))
        CAG(3)   = SIGAG(3)/(EPSAG(3)**EPSAG(3))  
        W0       = (CAG(1)/(EPSAG(1) + ONE))*((EPSAG(1))**(EPSAG(1)+ONE))
        !  Equivalent strain
        EPSEQ(1) = ((EPSAG(1)+ONE)*(W0/CAG(1)))**(ONE/(EPSAG(1)+ONE))
        EPSEQ(2) = ((EPSAG(2)+ONE)*(W0/CAG(2)))**(ONE/(EPSAG(2)+ONE))
        EPSEQ(3) = ((EPSAG(3)+ONE)*(W0/CAG(3)))**(ONE/(EPSAG(3)+ONE))
        !  Reference point
        fUN(1,1) = EPSAG(1)/EPSEQ(1)
        fUN(2,1) = EPSAG(1)/EPSEQ(2)
        fUN(3,1) = EPSAG(1)/EPSEQ(3)
c        
        ! Computation of biaxial reference point
        fUN_AV    = (fUN(1,1) + TWO*fUN(2,1) + fUN(3,1))/FOUR
        rLank_AV  = (rLank(1) + TWO*rLank(2) + rLank(3))/FOUR
        fBI_MIN   = 0.97D0
        fBI_MAX   = 1.14D0
        fBI_TRANS = 1.22D0
        ADIRAC    = 3.4D0
        FDIRAC    = EXP(ADIRAC*(rLank_AV-fBI_TRANS))
        fBI       = fUN_AV*((fBI_MIN/(ONE+FDIRAC)) + (fBI_MAX/(ONE+(ONE/FDIRAC))))
        IF (rhoBI == ZERO) THEN 
          rhoBI = rLank(1)/rLank(3)
        ENDIF
c        
        ! Computation of plane strain reference point first coordinate
        fPS_MIN   = 0.827D0
        fPS_MAX   = 1.315D0
        fPS_TRANS = 0.5D0
        ADIRAC    = 1.2D0
        DO J = 1,NANGLE
          FDIRAC    = EXP(ADIRAC*(rLank(J)-fPS_TRANS))
          fPS(J,1)  = fUN(J,1)*((fPS_MIN/(ONE+FDIRAC)) + (fPS_MAX/(ONE+(ONE/FDIRAC))))
        ENDDO
c        
        ! Computation of shear reference point first coordinate
        fSH_MIN   = 0.757D0
        fSH_MAX   = 0.525D0
        fSH_TRANS = ZERO
        ADIRAC    = 1.6D0
        fUN_AV    = (fUN(1,1)+fUN(3,1))/TWO
        rLank_AV  = (rLank(1)+rLank(3))/TWO
        FDIRAC    = EXP(ADIRAC*(rLank_AV - fSH_TRANS))
        fSH(1,1)  = fUN_AV*((fSH_MIN/(ONE+FDIRAC)) + (fSH_MAX/(ONE+(ONE/FDIRAC))))
        fSH(3,1)  = fUN_AV*((fSH_MIN/(ONE+FDIRAC)) + (fSH_MAX/(ONE+(ONE/FDIRAC))))
        FDIRAC    = EXP(ADIRAC*(rLank(2)-fSH_TRANS))
        fSH(2,1)  = fUN(2,1)*((fSH_MIN/(ONE+FDIRAC)) + (fSH_MAX/(ONE+(ONE/FDIRAC))))
c
        ! Computation of plane strain reference point second coordinate
        DO J = 1, NANGLE
          ! Angle theta with respect to rolling direction
          THETA(J)     = (J-1)*(90.0D0/(NANGLE-1))
          THETA_RAD(J) = THETA(J)*(PI/180.0D0)
          ! Strain-rate ratio
          rhoBI_theta  = (rhoBI + ONE) + (rhoBI - ONE)*COS(TWO*THETA_RAD(J))
          rhoBI_theta  = rhoBI_theta / ((rhoBI + ONE) - (rhoBI - ONE)*COS(TWO*THETA_RAD(J)))
          rhoONE_theta  = -rLank(J)/(rLank(J)+ONE) 
          ! Virtual hinge point
          fH(2) = (fBI + rhoBI_theta*fBI - fUN(J,1))/(rhoBI_theta - rhoONE_theta)
          fH(1) = fBI - rhoBI_theta*(fH(2)-fBI)
          ! Initialization of the computation of Weight
          MU     = HALF
          WEIGHT = ZERO
          fPS1   = (fUN(J,1)*((ONE - MU)**2) + TWO*fH(1)*MU*WEIGHT*(ONE-MU) + fBI*(MU**2))/
     .                 ((ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2)
          PHI    = fPS1-fPS(J,1)
          ! Newton iteration to compute WEIGHT
          DO ITER = 1,10
            ! Derivative of fPS1 with respect to WEIGHT
            U        = fUN(J,1)*((ONE - MU)**2) + TWO*fH(1)*MU*WEIGHT*(ONE-MU) + fBI*(MU**2) 
            UPRIM    = TWO*MU*(ONE-MU)*fH(1)
            V        = (ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2
            VPRIM    = TWO*MU*(ONE-MU)
            DfPS1_DWEIGHT = (UPRIM*V - U*VPRIM)/(MAX(V**2,EM20))         
            ! Updating the Weight
            WEIGHT   = WEIGHT - (PHI / DfPS1_DWEIGHT)
            ! Updating the fPS1 value
            fPS1   = (fUN(J,1)*((ONE - MU)**2) + TWO*fH(1)*MU*WEIGHT*(ONE-MU) + fBI*(MU**2))/
     .                 ((ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2)
            ! Updating the PHI value
            PHI    = fPS1-fPS(J,1)
          ENDDO
          ! Filling the second coordinate of the plane-strain point
          fPS(J,2) = (TWO*fH(2)*MU*WEIGHT*(ONE-MU) + fBI*(MU**2))/
     .                 ((ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2)
        ENDDO
c         
      ! Simplified Vegter lite yield locus
      ELSE
        CALL HM_GET_INTV('MAT_refanglemax',NANGLE,IS_AVAILABLE,LSUBMODEL)
        IF (NANGLE > 10) THEN 
          CALL ANCMSG(MSGID=1779,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)      
        ENDIF
        IF (.NOT.ALLOCATED(fUN))   ALLOCATE(fUN(NANGLE,2))
        IF (.NOT.ALLOCATED(rLank)) ALLOCATE(rLank(NANGLE))
        IF (.NOT.ALLOCATED(WPS))   ALLOCATE(WPS(NANGLE))
        IF (.NOT.ALLOCATED(WSH))   ALLOCATE(WSH(NANGLE))
        ! Loop over angles (must be equally distributed between 0 and pi/2)
        DO J = 1, NANGLE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_fUN_THETA' ,fUN(J,1),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_R_THETA'   ,rLank(J),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          IF (rLank(J) == 0) rLank(J) = ONE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_W_PS' ,WPS(J)  ,J,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_W_SH' ,WSH(J)  ,J,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDDO
        DO J = 1, NANGLE
          IF (WSH(J) /= WSH(NANGLE-(J-1))) THEN
            CALL ANCMSG(MSGID=1800,MSGTYPE=MSGWARNING,
     .        ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
            WSH(NANGLE-(J-1)) = WSH(J)
          ENDIF
        ENDDO
      ENDIF
c
c---------------------
c     Default values
c---------------------
      ! Density
      IF (RHOR == ZERO)  RHOR  = RHO0
      IF (Ires == 0)     Ires  = 2
      ! Poisson's ratio
      IF (NU < ZERO .OR. NU >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      ! Elasticity parameter
      BULK = YOUNG / THREE / (ONE - TWO*NU)
      A11  = YOUNG / (ONE-NU*NU)
      G    = YOUNG / TWO  / (ONE + NU)
      ! Default strain-rate computation
      IF (Ivflag == 0) Ivflag = 2
      IF (Ivflag > 3) THEN 
        CALL ANCMSG(MSGID=1802,MSGTYPE=MSGERROR,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .    I2=Ivflag)      
      ENDIF
      ! No Strain-rate dependence
      IF ((TAB_YLD == 0).AND.((Deps0 == ZERO).OR.(DG0 == ZERO).OR.(SIGSTAR == ZERO))) THEN
        SIGSTAR = ZERO
        Deps0   = ONE
        DG0     = ONE
        ISRATE  = 0
        ASRATE  = ZERO
        ! Info message
        CALL ANCMSG(MSGID=1801,MSGTYPE=MSGINFO,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ELSE
        ! Set strain-rate filtering
        ISRATE  = 1
        ! Default cutting frequency
        IF ((ASRATE == ZERO).OR.(Ivflag == 1)) ASRATE = 10000.0D0*FAC_T_WORK
      ENDIF
      ! Boltzmann constant
      KBOLTZ = 8.6173303*EM5 ! eV/K (~J/K)
      KBOLTZ = KBOLTZ/(FAC_M_WORK*FAC_L_WORK*FAC_L_WORK/(FAC_T_WORK*FAC_T_WORK))
      ! Kinematic hardening factor
      IF (FISOKIN > ONE)   FISOKIN = ONE
      IF (FISOKIN < ZERO) FISOKIN = ZERO
      ! Default interpolation computation
      IF (Ismooth == 0) Ismooth = 1
      ! Tabulated function scale factor
      IF (TAB_YLD == 0) THEN 
        XSCALE = ZERO
        YSCALE = ZERO
        TAB_TEMP = 0
      ENDIF
      IF ((TAB_TEMP > 0).AND.(TINI == ZERO)) TINI = 293.0D0
      IF ((YSCALE == ZERO).AND.(TAB_YLD>0)) THEN
        CALL HM_GET_FLOATV_DIM('MAT_Yscale' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        YSCALE = ONE * YSCALE_UNIT
      ENDIF
      IF ((XSCALE == ZERO).AND.(TAB_YLD>0)) THEN
        CALL HM_GET_FLOATV_DIM('MAT_Xscale' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        XSCALE = ONE * XSCALE_UNIT
      ENDIF
c----------------------------------------------
c     Computation of yield criterion parameter
c----------------------------------------------
      IF (.NOT.ALLOCATED(HIPS))      ALLOCATE(HIPS(NANGLE,2))
      IF (.NOT.ALLOCATED(HIUN))      ALLOCATE(HIUN(NANGLE,2))
      IF (.NOT.ALLOCATED(HISH))      ALLOCATE(HISH(NANGLE,2))
      IF (.NOT.ALLOCATED(THETA))     ALLOCATE(THETA(NANGLE))
      IF (.NOT.ALLOCATED(THETA_RAD)) ALLOCATE(THETA_RAD(NANGLE))
c
      ! Computation of angles
      DO J = 1, NANGLE
        IF (NANGLE > 1) THEN 
          THETA(J)     = (J-1)*(90.0D0/(NANGLE-1))
          THETA_RAD(J) = THETA(J)*(PI/180.0D0)
        ELSE
          THETA(J)     = ZERO
          THETA_RAD(J) = ZERO
        ENDIF
      ENDDO
c
      ! Loop over angles
      DO J = 1, NANGLE
c      
        ! Hinge points for the classical Corus-Vegter yield locus (3 hinge points)
        IF ((Icrit == 1).OR.(Icrit == 2).OR.(Icrit == 3)) THEN
          ! Computation of Hinge point between Bi-axial and Plane-strain point
          !   Plane strain point and normal
          A1 = fPS(J,1)
          A2 = fPS(J,2)
          N1 = ONE
          N2 = ZERO
          !   Biaxial point and normal
          rhoBI_theta = (rhoBI + ONE) + (rhoBI - ONE)*COS(TWO*THETA_RAD(J))
          rhoBI_theta = rhoBI_theta / ((rhoBI + ONE) - (rhoBI - ONE)*COS(TWO*THETA_RAD(J)))
          C1 = fBI
          C2 = fBI
          M1 = ONE
          M2 = rhoBI_theta
          !   Hinge point
          HIPS(J,1) = (M2*(N1*A1+N2*A2) - N2*(M1*C1+M2*C2))/(N1*M2 - M1*N2)
          HIPS(J,2) = (N1*(M1*C1+M2*C2) - M1*(N1*A1+N2*A2))/(N1*M2 - M1*N2)
c      
          ! Computation of Hinge point between Plane-strain and Uniaxial point
          !  Uniaxial point and normal 
          A1 = fUN(J,1)
          fUN(J,2) = ZERO
          A2 = fUN(J,2)
          N1 = ONE
          N2 = -rLank(J)/(rLank(J)+ONE)      
          !  Plane strain point and normal
          C1 = fPS(J,1)
          C2 = fPS(J,2)
          M1 = ONE
          M2 = ZERO
          !  Hinge point
          HIUN(J,1) = (M2*(N1*A1+N2*A2) - N2*(M1*C1+M2*C2))/(N1*M2 - M1*N2)
          HIUN(J,2) = (N1*(M1*C1+M2*C2) - M1*(N1*A1+N2*A2))/(N1*M2 - M1*N2)
c
          ! If fPS2 is not measured or if Icrit = 2
          IF ((Icrit == 1).AND.(fPS(J,2) == ZERO)) THEN
            fPS(J,2) = HIUN(J,2) + HALF*(HIPS(J,2) - HIUN(J,2))
          ELSEIF (Icrit == 2) THEN
            fPS(J,2) = HIUN(J,2) + ALPS(J)*(HIPS(J,2) - HIUN(J,2))
          ENDIF
c      
          ! Computation of Hinge point between Uniaxial point and Shear point
          !  Shear point and normal 
          A1 = fSH(J,1)
          fSH(J,2) = -fSH(NANGLE-(J-1),1)
          A2 = fSH(J,2)
          N1 = ONE
          N2 = -ONE      
          !  Plane strain point and normal
          C1 = fUN(J,1)
          C2 = ZERO
          M1 = ONE
          M2 = -rLank(J)/(rLank(J)+ONE)
          !  Hinge point
          HISH(J,1) = (M2*(N1*A1+N2*A2) - N2*(M1*C1+M2*C2))/(N1*M2 - M1*N2)
          HISH(J,2) = (N1*(M1*C1+M2*C2) - M1*(N1*A1+N2*A2))/(N1*M2 - M1*N2)
c
        ! Hinge points for the simplified Corus-Vegter lite yield locus (2 hinge points)
        ELSE
          ! Computation of Hinge point between Bi-axial and Uniaxial point
          !   Uniaxial point and normal
          A1 = fUN(J,1)
          fUN(J,2) = ZERO
          A2 = fUN(J,2)
          N1 = ONE
          N2 = -rLank(J)/(rLank(J)+ONE)
          !   Biaxial point and normal
          rhoBI_theta = (rhoBI + ONE) + (rhoBI - ONE)*COS(TWO*THETA_RAD(J))
          rhoBI_theta = rhoBI_theta / ((rhoBI + ONE) - (rhoBI - ONE)*COS(TWO*THETA_RAD(J)))
          C1 = fBI
          C2 = fBI
          M1 = ONE
          M2 = rhoBI_theta
          !   Hinge point
          HIPS(J,1) = (M2*(N1*A1+N2*A2) - N2*(M1*C1+M2*C2))/(N1*M2 - M1*N2)
          HIPS(J,2) = (N1*(M1*C1+M2*C2) - M1*(N1*A1+N2*A2))/(N1*M2 - M1*N2)
c
          ! Computation of Hinge point between Uniaxial point in tension and Uniaxial point in compression
          !  Uniaxial point in compression
          A1 = fUN(J,2)
          A2 = -fUN(J,1)
          N1 = rLank(J)/(rLank(J)+ONE)
          N2 = -ONE
          !  Uniaxial point in tension
          C1 = fUN(J,1)
          C2 = fUN(J,2)
          M1 = ONE
          M2 = -rLank(J)/(rLank(J)+ONE)
          !  Hinge point
          HISH(J,1) = (M2*(N1*A1+N2*A2) - N2*(M1*C1+M2*C2))/(N1*M2 - M1*N2)
          HISH(J,2) = (N1*(M1*C1+M2*C2) - M1*(N1*A1+N2*A2))/(N1*M2 - M1*N2)
        ENDIF
      ENDDO
c
c----------------------------------------------
c     Computation of cosine interpolation factor
c----------------------------------------------  
c
      ! Allocation of the A-MATRIX and the Pivot vector
      ALLOCATE (AMAT(NANGLE,NANGLE),IPIV(NANGLE))
c
      ! Filling the A-MATRIX
      DO J = 1,NANGLE
        DO I = 1,NANGLE
          AMAT(J,I) = ZERO
          DO K = 1,I
            AMAT(J,I) = AMAT(J,I) + COS2(K,I)*(COS(TWO*THETA_RAD(J)))**(K-1)
          ENDDO
        ENDDO
      ENDDO
c        
      ! Classical Vegter yield locus and Standard Vegter yield locus
      IF ((Icrit == 1).OR.(Icrit == 2).OR.(Icrit == 3)) THEN 
c      
        ! Allocation of factors
        ALLOCATE(Q_fSH(NANGLE,2),Q_fUN(NANGLE,2),Q_fPS(NANGLE,2),
     .          Q_HISH(NANGLE,2),Q_HIUN(NANGLE,2),Q_HIPS(NANGLE,2))      
c
        ! Initialization of tables
        Q_fSH(1:NANGLE,1:2)  = ZERO
        Q_fUN(1:NANGLE,1:2)  = ZERO
        Q_fPS(1:NANGLE,1:2)  = ZERO
        Q_HISH(1:NANGLE,1:2) = ZERO
        Q_HIUN(1:NANGLE,1:2) = ZERO
        Q_HIPS(1:NANGLE,1:2) = ZERO     
c
        ! Filling the B vector with experimental points
        ALLOCATE (BVEC(NANGLE,12))
        BVEC(1:NANGLE,1:2)   = fSH(1:NANGLE,1:2)
        BVEC(1:NANGLE,3:4)   = fUN(1:NANGLE,1:2)
        BVEC(1:NANGLE,5:6)   = fPS(1:NANGLE,1:2)
        BVEC(1:NANGLE,7:8)   = HISH(1:NANGLE,1:2)
        BVEC(1:NANGLE,9:10)  = HIUN(1:NANGLE,1:2)
        BVEC(1:NANGLE,11:12) = HIPS(1:NANGLE,1:2)
c        
        ! Initialization of the Pivot vector
        IPIV(1:NANGLE) = 0
c
        ! Solving the A-MATRIX * x = B vector system
#ifndef WITHOUT_LINALG
        CALL DGESV(NANGLE, 12, AMAT, NANGLE, IPIV, BVEC, NANGLE, INFO)
#else
        WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif
c        
        ! Recovering the solution
        Q_fSH(1:NANGLE,1:2)  = BVEC(1:NANGLE,1:2)
        Q_fUN(1:NANGLE,1:2)  = BVEC(1:NANGLE,3:4)
        Q_fPS(1:NANGLE,1:2)  = BVEC(1:NANGLE,5:6)
        Q_HISH(1:NANGLE,1:2) = BVEC(1:NANGLE,7:8)
        Q_HIUN(1:NANGLE,1:2) = BVEC(1:NANGLE,9:10)
        Q_HIPS(1:NANGLE,1:2) = BVEC(1:NANGLE,11:12)
c
      ! Simplified Vegter Lite yield locus
      ELSE
c
        ! Allocation of factors
        ALLOCATE(Q_fUN(NANGLE,2),Q_HISH(NANGLE,2),Q_HIPS(NANGLE,2),
     .           Q_WSH(NANGLE),Q_WPS(NANGLE))
c
        ! Initialization of factors
        Q_fUN(1:NANGLE,1:2)  = ZERO
        Q_HISH(1:NANGLE,1:2) = ZERO
        Q_HIPS(1:NANGLE,1:2) = ZERO
        Q_WSH(1:NANGLE)      = ZERO
        Q_WPS(1:NANGLE)      = ZERO
c
        ! Filling the B vector with experimental points
        ALLOCATE (BVEC(NANGLE,8))
        BVEC(1:NANGLE,1:2) = fUN(1:NANGLE,1:2)
        BVEC(1:NANGLE,3:4) = HISH(1:NANGLE,1:2)
        BVEC(1:NANGLE,5:6) = HIPS(1:NANGLE,1:2)
        BVEC(1:NANGLE,7)   = WSH(1:NANGLE)
        BVEC(1:NANGLE,8)   = WPS(1:NANGLE)
c        
        ! Initialization of the Pivot vector
        IPIV(1:NANGLE) = 0
c
        ! Solving the A-MATRIX * x = B vector system
#ifndef WITHOUT_LINALG
        CALL DGESV(NANGLE, 8, AMAT, NANGLE, IPIV, BVEC, NANGLE, INFO)
#else
        WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif

c        
        ! Recovering the solution
        Q_fUN(1:NANGLE,1:2)  = BVEC(1:NANGLE,1:2)
        Q_HISH(1:NANGLE,1:2) = BVEC(1:NANGLE,3:4)
        Q_HIPS(1:NANGLE,1:2) = BVEC(1:NANGLE,5:6)
        Q_WSH(1:NANGLE)      = BVEC(1:NANGLE,7)
        Q_WPS(1:NANGLE)      = BVEC(1:NANGLE,8)
c      
      ENDIF
c-----------------------------------
c     Checking yield locus convexity
c-----------------------------------  
c
      ! Classical Vegter yield locus and Standard Yield locus
      IF ((Icrit == 1).OR.(Icrit == 2).OR.(Icrit == 3)) THEN 
      
        ! ---------------------------------------------------------
        ! 1. Checking convexity between shear and uniaxial point      
        ! ---------------------------------------------------------
c      
        ! Minimum eigen value of the Hessian matrix of the yield locus
        MINEIG     = INFINITY
        MINEIGMU   = ZERO
        MINEIGTHET = ZERO
c
        ! Initialization of the THETA angle
        THET = ZERO
c        
        ! Loop over the angles
        DO J = 1,61
c
          ! Initialization of the MU parameter
          MU = ZERO
          ! Computing the reference and hinge point for a giving angle
          !   Vector point 
          A1 = ZERO
          A2 = ZERO
          B1 = ZERO
          B2 = ZERO
          C1 = ZERO
          C2 = ZERO
          !   Initialization of the first derivative with respect to COS2THET
          DA_DCOS2(1:2)   = ZERO
          DB_DCOS2(1:2)   = ZERO
          DC_DCOS2(1:2)   = ZERO
          !  Initialization of the second derivative with respect to COS2THET
          D2A_D2COS2(1:2) = ZERO
          D2B_D2COS2(1:2) = ZERO
          D2C_D2COS2(1:2) = ZERO          
          ! Loop over angles
          DO I = 1,NANGLE
            ! Computation of the reference points
            DO K = 1,I
              A1 = A1 + Q_fSH(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              A2 = A2 + Q_fSH(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B1 = B1 + Q_HISH(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B2 = B2 + Q_HISH(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              C1 = C1 + Q_fUN(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              C2 = C2 + Q_fUN(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
            ENDDO
          ENDDO
          ! If anisotropic, compute derivatives with respect to COS2THET
          IF (NANGLE > 1) THEN 
            ! Computation of their first derivative with respect to COS2THET
            DO I = 2, NANGLE
              DO K = 2,I
                DA_DCOS2(1:2) = DA_DCOS2(1:2) + Q_fSH(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HISH(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DC_DCOS2(1:2) = DC_DCOS2(1:2) + Q_fUN(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
              ENDDO              
            ENDDO
            ! Computation of their second derivative with respect to COS2THET
            IF (NANGLE > 2) THEN
              DO I = 3, NANGLE
                DO K = 3,I
                  D2A_D2COS2(1:2) = D2A_D2COS2(1:2) + Q_fSH(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2B_D2COS2(1:2) = D2B_D2COS2(1:2) + Q_HISH(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2C_D2COS2(1:2) = D2C_D2COS2(1:2) + Q_fUN(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                ENDDO              
              ENDDO            
            ENDIF
          ENDIF
c          
          !----------------------------------------------
          ! Computing the Hessian matrix and eigen values
          !----------------------------------------------
          DO I = 1,21
c
            ! Computation of F1 and F2
            F1 = ((ONE-MU)**2)*A1 + TWO*MU*(ONE-MU)*B1 + (MU**2)*C1
            F2 = ((ONE-MU)**2)*A2 + TWO*MU*(ONE-MU)*B2 + (MU**2)*C2 
            ! Derivatives of Fi with respect to MU
            DF1_DMU = -TWO*(ONE-MU)*A1 + TWO*(ONE-TWO*MU)*B1 + TWO*MU*C1
            DF2_DMU = -TWO*(ONE-MU)*A2 + TWO*(ONE-TWO*MU)*B2 + TWO*MU*C2
            ! Second derivatives of Fi with respect to MU
            D2F1_D2MU = TWO*(A1+C1-TWO*B1)
            D2F2_D2MU = TWO*(A2+C2-TWO*B2)
            ! Denominator and its derivative with respect to MU
            DENOM = F1*DF2_DMU - F2*DF1_DMU
            DENOM = SIGN(MAX(ABS(DENOM),EM20),DENOM)
            ! Derivatives of Phi with respect to SIG1 and SIG2
            DPHI_DSIG1 =  DF2_DMU/DENOM
            DPHI_DSIG2 = -DF1_DMU/DENOM
            ! Derivatives of MU with respect to SIG1 and SIG2
            DMU_DSIG1  = -F2/DENOM
            DMU_DSIG2  = F1/DENOM
c
            ! Derivatives of Fi with respect to COS2
            DF1_DCOS2  = ((ONE-MU)**2)*DA_DCOS2(1) + TWO*MU*(ONE-MU)*DB_DCOS2(1) + (MU**2)*DC_DCOS2(1)
            DF2_DCOS2  = ((ONE-MU)**2)*DA_DCOS2(2) + TWO*MU*(ONE-MU)*DB_DCOS2(2) + (MU**2)*DC_DCOS2(2)
            DPHI_DCOS2 = (DF2_DCOS2*DF1_DMU - DF1_DCOS2*DF2_DMU)/DENOM
            DMU_DCOS2  = (F2*DF1_DCOS2 - F1*DF2_DCOS2)/DENOM
c
            ! Second derivatives of Fi with respect to COS2
            D2F1_D2MUCOS2 = -TWO*(ONE-MU)*DA_DCOS2(1)  + TWO*(ONE-TWO*MU)*DB_DCOS2(1) + TWO*MU*DC_DCOS2(1)
            D2F2_D2MUCOS2 = -TWO*(ONE-MU)*DA_DCOS2(2)  + TWO*(ONE-TWO*MU)*DB_DCOS2(2) + TWO*MU*DC_DCOS2(2)
            D2F1_D2COS2   = ((ONE-MU)**2)*D2A_D2COS2(1) + TWO*MU*(ONE-MU)*D2B_D2COS2(1) + (MU**2)*D2C_D2COS2(1)
            D2F2_D2COS2   = ((ONE-MU)**2)*D2A_D2COS2(2) + TWO*MU*(ONE-MU)*D2B_D2COS2(2) + (MU**2)*D2C_D2COS2(2)
c
            ! Second derivatives of PHI with respect to SIG1 and SIG2 (Hessian matrix)
            !-------------------------------------------------------------------------
            !   FIRST LINE OF THE MATRIX
            !   D2PHI_D2SIG1
            D2PHI_D2(1,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG1
            !   D2PHI_DSIG1SIG2
            D2PHI_D2(1,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG2
            !   D2PHI_DSIG1COS2
            D2PHI_D2(1,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
c
            !   SECOND LINE OF THE MATRIX
            !   D2PHI_DSIG2SIG1
            D2PHI_D2(2,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG1
            !   D2PHI_D2SIG2
            D2PHI_D2(2,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG2
            !   D2PHI_DSIG2COS2   
            D2PHI_D2(2,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)
c
            !   THIRD LINE OF THE MATRIX
            !   D2PHI_D2COS2SIG1
            D2PHI_D2(3,1) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
            !   D2PHI_D2COS2SIG2
            D2PHI_D2(3,2) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)          
            !   D2PHI_D2COS2
            D2PHI_D2(3,3) = (-ONE/DENOM)*(DF2_DMU*(D2F1_D2COS2 + TWO*D2F1_D2MUCOS2*DMU_DCOS2 + D2F1_D2MU*(DMU_DCOS2**2)) 
     .                          - DF1_DMU*(D2F2_D2COS2 + TWO*D2F2_D2MUCOS2*DMU_DCOS2 + D2F2_D2MU*(DMU_DCOS2**2))
     .                          + TWO*(DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DCOS2)
c    
            ! Filling the T matrix
            T(1,1) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG2 - DPHI_DSIG1)  +
     .                DPHI_DCOS2*(THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG2 - DPHI_DSIG1) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 - 
     .                TWO*(SIN(TWO*THET)**3))
            T(2,1) =  T(1,2)
            T(2,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(2,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(-FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 + 
     .                TWO*(SIN(TWO*THET)**3))
            T(3,1) =  T(1,3)
            T(3,2) =  T(2,3)
            T(3,3) =  (TWO/(F1-F2))*((COS(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2)+
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(EIGHT*(SIN(TWO*THET)**2)*COS(TWO*THET) - 
     .                FOUR*(COS(TWO*THET)**3))
     
            ! Filling the rotation D matrix
            D(1,1) = HALF*(ONE+COS(TWO*THET))
            D(1,2) = HALF*(ONE-COS(TWO*THET))
            D(1,3) = SIN(TWO*THET)
            D(2,1) = HALF*(ONE-COS(TWO*THET))
            D(2,2) = HALF*(ONE+COS(TWO*THET))
            D(2,3) = -SIN(TWO*THET)
            D(3,1) = (SIN(TWO*THET)**2)/(F1-F2)
            D(3,2) = -(SIN(TWO*THET)**2)/(F1-F2)
            D(3,3) = -(TWO*SIN(TWO*THET)*COS(TWO*THET))/(F1-F2)
            
            ! Assembling the Hessian Matrix Dt*DPHI_DSIG*D + T
            D2PHI_D2 = MATMUL(D2PHI_D2,D)
            D2PHI_D2 = MATMUL(TRANSPOSE(D),D2PHI_D2)
            D2PHI_D2(1:3,1:3) = D2PHI_D2(1:3,1:3) + T(1:3,1:3)
c
            ! Computation of eigen values 
            WR   = ZERO
            WI   = ZERO
            VL   = ZERO
            VR   = ZERO
            WORK = ZERO
#ifndef WITHOUT_LINALG
            CALL DGEEV('N','N',3,D2PHI_D2,3,WR,WI,VL,3,VR,3,WORK,102,INFO)
#else
            WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif
c
            ! Storing the minimal value
            IF (MINVAL(WR)<MINEIG) THEN 
              MINEIG     = MINVAL(WR)
              MINEIGMU   = MU
              MINEIGTHET = THET*(180.0D0/PI)
            ENDIF
            ! Updating the MU parameter
            MU = MU + ONE/TWENTY
          ENDDO
          ! Updating the theta angle
          THET = THET + (PI/TWO)/60.0D0
        ENDDO
c        
        ! Convexity check and error message
        IF (MINEIG<-TOL) THEN
          CALL ANCMSG(MSGID=1803,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .      R1=MINEIG,R2=MINEIGMU,R3=MINEIGTHET)
        ENDIF
c        
        ! -------------------------------------------------------------
        ! 2. Checking convexity between uniaxial and plane strain point      
        ! -------------------------------------------------------------
c      
        ! Minimum eigen value of the Hessian matrix of the yield locus
        MINEIG     = INFINITY
        MINEIGMU   = ZERO
        MINEIGTHET = ZERO
c
        ! Initialization of the THETA angle
        THET = ZERO
c        
        ! Loop over the angles
        DO J = 1,61
c
          !   Initialization of the MU parameter
          MU = ZERO
          ! Computing the reference and hinge point for a giving angle
          !   Vector point 
          A1 = ZERO
          A2 = ZERO
          B1 = ZERO
          B2 = ZERO
          C1 = ZERO
          C2 = ZERO
          !   Initialization of the first derivative with respect to COS2THET
          DA_DCOS2(1:2)   = ZERO
          DB_DCOS2(1:2)   = ZERO
          DC_DCOS2(1:2)   = ZERO
          !  Initialization of the second derivative with respect to COS2THET
          D2A_D2COS2(1:2) = ZERO
          D2B_D2COS2(1:2) = ZERO
          D2C_D2COS2(1:2) = ZERO
          ! Loop over angles
          DO I = 1,NANGLE
            ! Computation of the reference points
            DO K = 1,I
              A1 = A1 + Q_fUN(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              A2 = A2 + Q_fUN(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B1 = B1 + Q_HIUN(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B2 = B2 + Q_HIUN(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              C1 = C1 + Q_fPS(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              C2 = C2 + Q_fPS(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
            ENDDO
          ENDDO
          ! If anisotropic, compute derivatives with respect to COS2THET
          IF (NANGLE > 1) THEN 
            ! Computation of their first derivative with respect to COS2THET
            DO I = 2, NANGLE
              DO K = 2,I
                DA_DCOS2(1:2) = DA_DCOS2(1:2) + Q_fUN(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HIUN(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DC_DCOS2(1:2) = DC_DCOS2(1:2) + Q_fPS(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
              ENDDO              
            ENDDO
            ! Computation of their second derivative with respect to COS2THET
            IF (NANGLE > 2) THEN
              DO I = 3, NANGLE
                DO K = 3,I
                  D2A_D2COS2(1:2) = D2A_D2COS2(1:2) + Q_fUN(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2B_D2COS2(1:2) = D2B_D2COS2(1:2) + Q_HIUN(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2C_D2COS2(1:2) = D2C_D2COS2(1:2) + Q_fPS(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                ENDDO              
              ENDDO            
            ENDIF
          ENDIF
c          
          !----------------------------------------------
          ! Computing the Hessian matrix and eigen values
          !----------------------------------------------
          DO I = 1,21
c
            ! Computation of F1 and F2
            F1 = ((ONE-MU)**2)*A1 + TWO*MU*(ONE-MU)*B1 + (MU**2)*C1
            F2 = ((ONE-MU)**2)*A2 + TWO*MU*(ONE-MU)*B2 + (MU**2)*C2 
            ! Derivatives of Fi with respect to MU
            DF1_DMU = -TWO*(ONE-MU)*A1 + TWO*(ONE-TWO*MU)*B1 + TWO*MU*C1
            DF2_DMU = -TWO*(ONE-MU)*A2 + TWO*(ONE-TWO*MU)*B2 + TWO*MU*C2
            ! Second derivatives of Fi with respect to MU
            D2F1_D2MU = TWO*(A1+C1-TWO*B1)
            D2F2_D2MU = TWO*(A2+C2-TWO*B2)
            ! Denominator and its derivative with respect to MU
            DENOM = F1*DF2_DMU - F2*DF1_DMU
            DENOM = SIGN(MAX(ABS(DENOM),EM20),DENOM)
            ! Derivatives of Phi with respect to SIG1 and SIG2
            DPHI_DSIG1 =  DF2_DMU/DENOM
            DPHI_DSIG2 = -DF1_DMU/DENOM
            ! Derivatives of MU with respect to SIG1 and SIG2
            DMU_DSIG1  = -F2/DENOM
            DMU_DSIG2  = F1/DENOM
c
            ! Derivatives of Fi with respect to COS2
            DF1_DCOS2  = ((ONE-MU)**2)*DA_DCOS2(1) + TWO*MU*(ONE-MU)*DB_DCOS2(1) + (MU**2)*DC_DCOS2(1)
            DF2_DCOS2  = ((ONE-MU)**2)*DA_DCOS2(2) + TWO*MU*(ONE-MU)*DB_DCOS2(2) + (MU**2)*DC_DCOS2(2)
            DPHI_DCOS2 = (DF2_DCOS2*DF1_DMU - DF1_DCOS2*DF2_DMU)/DENOM
            DMU_DCOS2  = (F2*DF1_DCOS2 - F1*DF2_DCOS2)/DENOM
c
            ! Second derivatives of Fi with respect to COS2
            D2F1_D2MUCOS2 = -TWO*(ONE-MU)*DA_DCOS2(1)  + TWO*(ONE-TWO*MU)*DB_DCOS2(1) + TWO*MU*DC_DCOS2(1)
            D2F2_D2MUCOS2 = -TWO*(ONE-MU)*DA_DCOS2(2)  + TWO*(ONE-TWO*MU)*DB_DCOS2(2) + TWO*MU*DC_DCOS2(2)
            D2F1_D2COS2   = ((ONE-MU)**2)*D2A_D2COS2(1) + TWO*MU*(ONE-MU)*D2B_D2COS2(1) + (MU**2)*D2C_D2COS2(1)
            D2F2_D2COS2   = ((ONE-MU)**2)*D2A_D2COS2(2) + TWO*MU*(ONE-MU)*D2B_D2COS2(2) + (MU**2)*D2C_D2COS2(2)
c
            ! Second derivatives of PHI with respect to SIG1 and SIG2 (Hessian matrix)
            !-------------------------------------------------------------------------
            !   FIRST LINE OF THE MATRIX
            !   D2PHI_D2SIG1
            D2PHI_D2(1,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG1
            !   D2PHI_DSIG1SIG2
            D2PHI_D2(1,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG2
            !   D2PHI_DSIG1COS2
            D2PHI_D2(1,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
c
            !   SECOND LINE OF THE MATRIX
            !   D2PHI_DSIG2SIG1
            D2PHI_D2(2,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG1
            !   D2PHI_D2SIG2
            D2PHI_D2(2,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG2
            !   D2PHI_DSIG2COS2   
            D2PHI_D2(2,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)
c
            !   THIRD LINE OF THE MATRIX
            !   D2PHI_D2COS2SIG1
            D2PHI_D2(3,1) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
            !   D2PHI_D2COS2SIG2
            D2PHI_D2(3,2) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)          
            !   D2PHI_D2COS2
            D2PHI_D2(3,3) = (-ONE/DENOM)*(DF2_DMU*(D2F1_D2COS2 + TWO*D2F1_D2MUCOS2*DMU_DCOS2 + D2F1_D2MU*(DMU_DCOS2**2)) 
     .                          - DF1_DMU*(D2F2_D2COS2 + TWO*D2F2_D2MUCOS2*DMU_DCOS2 + D2F2_D2MU*(DMU_DCOS2**2))
     .                          + TWO*(DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DCOS2)
c    
            ! Filling the T matrix
            T(1,1) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG2 - DPHI_DSIG1)  +
     .                DPHI_DCOS2*(THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG2 - DPHI_DSIG1) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 - 
     .                TWO*(SIN(TWO*THET)**3))
            T(2,1) =  T(1,2)
            T(2,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(2,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(-FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 + 
     .                TWO*(SIN(TWO*THET)**3))
            T(3,1) =  T(1,3)
            T(3,2) =  T(2,3)
            T(3,3) =  (TWO/(F1-F2))*((COS(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2)+
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(EIGHT*(SIN(TWO*THET)**2)*COS(TWO*THET) - 
     .                FOUR*(COS(TWO*THET)**3))
c     
            ! Filling the rotation D matrix
            D(1,1) = HALF*(ONE+COS(TWO*THET))
            D(1,2) = HALF*(ONE-COS(TWO*THET))
            D(1,3) = SIN(TWO*THET)
            D(2,1) = HALF*(ONE-COS(TWO*THET))
            D(2,2) = HALF*(ONE+COS(TWO*THET))
            D(2,3) = -SIN(TWO*THET)
            D(3,1) = (SIN(TWO*THET)**2)/(F1-F2)
            D(3,2) = -(SIN(TWO*THET)**2)/(F1-F2)
            D(3,3) = -(TWO*SIN(TWO*THET)*COS(TWO*THET))/(F1-F2)
c            
            ! Assembling the Hessian Matrix Dt*DPHI_DSIG*D + T
            D2PHI_D2 = MATMUL(D2PHI_D2,D)
            D2PHI_D2 = MATMUL(TRANSPOSE(D),D2PHI_D2)
            D2PHI_D2(1:3,1:3) = D2PHI_D2(1:3,1:3) + T(1:3,1:3)
c
            ! Computation of eigen values 
            WR   = ZERO
            WI   = ZERO
            VL   = ZERO
            VR   = ZERO
            WORK = ZERO
            


#ifndef WITHOUT_LINALG
            CALL DGEEV('N','N',3,D2PHI_D2,3,WR,WI,VL,3,VR,3,WORK,102,INFO)
#else 
            WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif
c
            ! Storing the minimal value
            IF (MINVAL(WR)<MINEIG) THEN 
              MINEIG     = MINVAL(WR)
              MINEIGMU   = MU
              MINEIGTHET = THET*(180.0D0/PI)
            ENDIF
            ! Updating the MU parameter
            MU = MU + ONE/TWENTY
          ENDDO
          ! Updating the theta angle
          THET = THET + (PI/TWO)/60.0D0
        ENDDO
c        
        ! Convexity check and error message
        IF (MINEIG<-TOL) THEN
          CALL ANCMSG(MSGID=1804,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .      R1=MINEIG,R2=MINEIGMU,R3=MINEIGTHET)
        ENDIF
c
        ! -------------------------------------------------------------
        ! 3. Checking convexity between plane-strain and biaxial point      
        ! -------------------------------------------------------------
c      
        ! Minimum eigen value of the Hessian matrix of the yield locus
        MINEIG     = INFINITY
        MINEIGMU   = ZERO
        MINEIGTHET = ZERO
c
        ! Initialization of the THETA angle
        THET = ZERO
c        
        ! Loop over the angles
        DO J = 1,61
c
          !   Initialization of the MU parameter
          MU = ZERO
          ! Computing the reference and hinge point for a giving angle
          !   Vector point 
          A1 = ZERO
          A2 = ZERO
          B1 = ZERO
          B2 = ZERO
          C1 = fBI
          C2 = fBI
          !   Initialization of the first derivative with respect to COS2THET
          DA_DCOS2(1:2)   = ZERO
          DB_DCOS2(1:2)   = ZERO
          DC_DCOS2(1:2)   = ZERO
          !  Initialization of the second derivative with respect to COS2THET
          D2A_D2COS2(1:2) = ZERO
          D2B_D2COS2(1:2) = ZERO
          D2C_D2COS2(1:2) = ZERO  
          ! Loop over angles
          DO I = 1,NANGLE
            ! Computation of the reference points
            DO K = 1,I
              A1 = A1 + Q_fPS(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              A2 = A2 + Q_fPS(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B1 = B1 + Q_HIPS(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B2 = B2 + Q_HIPS(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
            ENDDO
          ENDDO
          ! If anisotropic, compute derivatives with respect to COS2THET
          IF (NANGLE > 1) THEN 
            ! Computation of their first derivative with respect to COS2THET
            DO I = 2, NANGLE
              DO K = 2,I
                DA_DCOS2(1:2) = DA_DCOS2(1:2) + Q_fPS(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HIPS(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
              ENDDO              
            ENDDO
            ! Computation of their second derivative with respect to COS2THET
            IF (NANGLE > 2) THEN
              DO I = 3, NANGLE
                DO K = 3,I
                  D2A_D2COS2(1:2) = D2A_D2COS2(1:2) + Q_fPS(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2B_D2COS2(1:2) = D2B_D2COS2(1:2) + Q_HIPS(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                ENDDO              
              ENDDO            
            ENDIF
          ENDIF          
c          
          !----------------------------------------------
          ! Computing the Hessian matrix and eigen values
          !----------------------------------------------
          DO I = 1,20
c
            ! Computation of F1 and F2
            F1 = ((ONE-MU)**2)*A1 + TWO*MU*(ONE-MU)*B1 + (MU**2)*C1
            F2 = ((ONE-MU)**2)*A2 + TWO*MU*(ONE-MU)*B2 + (MU**2)*C2 
            ! Derivatives of Fi with respect to MU
            DF1_DMU = -TWO*(ONE-MU)*A1 + TWO*(ONE-TWO*MU)*B1 + TWO*MU*C1
            DF2_DMU = -TWO*(ONE-MU)*A2 + TWO*(ONE-TWO*MU)*B2 + TWO*MU*C2
            ! Second derivatives of Fi with respect to MU
            D2F1_D2MU = TWO*(A1+C1-TWO*B1)
            D2F2_D2MU = TWO*(A2+C2-TWO*B2)
            ! Denominator and its derivative with respect to MU
            DENOM = F1*DF2_DMU - F2*DF1_DMU
            DENOM = SIGN(MAX(ABS(DENOM),EM20),DENOM)
            ! Derivatives of Phi with respect to SIG1 and SIG2
            DPHI_DSIG1 =  DF2_DMU/DENOM
            DPHI_DSIG2 = -DF1_DMU/DENOM
            ! Derivatives of MU with respect to SIG1 and SIG2
            DMU_DSIG1  = -F2/DENOM
            DMU_DSIG2  = F1/DENOM
c
            ! Derivatives of Fi with respect to COS2
            DF1_DCOS2  = ((ONE-MU)**2)*DA_DCOS2(1) + TWO*MU*(ONE-MU)*DB_DCOS2(1) + (MU**2)*DC_DCOS2(1)
            DF2_DCOS2  = ((ONE-MU)**2)*DA_DCOS2(2) + TWO*MU*(ONE-MU)*DB_DCOS2(2) + (MU**2)*DC_DCOS2(2)
            DPHI_DCOS2 = (DF2_DCOS2*DF1_DMU - DF1_DCOS2*DF2_DMU)/DENOM
            DMU_DCOS2  = (F2*DF1_DCOS2 - F1*DF2_DCOS2)/DENOM
c
            ! Second derivatives of Fi with respect to COS2
            D2F1_D2MUCOS2 = -TWO*(ONE-MU)*DA_DCOS2(1)  + TWO*(ONE-TWO*MU)*DB_DCOS2(1) + TWO*MU*DC_DCOS2(1)
            D2F2_D2MUCOS2 = -TWO*(ONE-MU)*DA_DCOS2(2)  + TWO*(ONE-TWO*MU)*DB_DCOS2(2) + TWO*MU*DC_DCOS2(2)
            D2F1_D2COS2   = ((ONE-MU)**2)*D2A_D2COS2(1) + TWO*MU*(ONE-MU)*D2B_D2COS2(1) + (MU**2)*D2C_D2COS2(1)
            D2F2_D2COS2   = ((ONE-MU)**2)*D2A_D2COS2(2) + TWO*MU*(ONE-MU)*D2B_D2COS2(2) + (MU**2)*D2C_D2COS2(2)
c
            ! Second derivatives of PHI with respect to SIG1 and SIG2 (Hessian matrix)
            !-------------------------------------------------------------------------
            !   FIRST LINE OF THE MATRIX
            !   D2PHI_D2SIG1
            D2PHI_D2(1,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG1
            !   D2PHI_DSIG1SIG2
            D2PHI_D2(1,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG2
            !   D2PHI_DSIG1COS2
            D2PHI_D2(1,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
c
            !   SECOND LINE OF THE MATRIX
            !   D2PHI_DSIG2SIG1
            D2PHI_D2(2,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG1
            !   D2PHI_D2SIG2
            D2PHI_D2(2,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG2
            !   D2PHI_DSIG2COS2   
            D2PHI_D2(2,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)
c
            !   THIRD LINE OF THE MATRIX
            !   D2PHI_D2COS2SIG1
            D2PHI_D2(3,1) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
            !   D2PHI_D2COS2SIG2
            D2PHI_D2(3,2) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)          
            !   D2PHI_D2COS2
            D2PHI_D2(3,3) = (-ONE/DENOM)*(DF2_DMU*(D2F1_D2COS2 + TWO*D2F1_D2MUCOS2*DMU_DCOS2 + D2F1_D2MU*(DMU_DCOS2**2)) 
     .                          - DF1_DMU*(D2F2_D2COS2 + TWO*D2F2_D2MUCOS2*DMU_DCOS2 + D2F2_D2MU*(DMU_DCOS2**2))
     .                          + TWO*(DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DCOS2)
c    
            ! Filling the T matrix
            T(1,1) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG2 - DPHI_DSIG1)  +
     .                DPHI_DCOS2*(THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG2 - DPHI_DSIG1) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 - 
     .                TWO*(SIN(TWO*THET)**3))
            T(2,1) =  T(1,2)
            T(2,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(2,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(-FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 + 
     .                TWO*(SIN(TWO*THET)**3))
            T(3,1) =  T(1,3)
            T(3,2) =  T(2,3)
            T(3,3) =  (TWO/(F1-F2))*((COS(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2)+
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(EIGHT*(SIN(TWO*THET)**2)*COS(TWO*THET) - 
     .                FOUR*(COS(TWO*THET)**3))
     
            ! Filling the rotation D matrix
            D(1,1) = HALF*(ONE+COS(TWO*THET))
            D(1,2) = HALF*(ONE-COS(TWO*THET))
            D(1,3) = SIN(TWO*THET)
            D(2,1) = HALF*(ONE-COS(TWO*THET))
            D(2,2) = HALF*(ONE+COS(TWO*THET))
            D(2,3) = -SIN(TWO*THET)
            D(3,1) = (SIN(TWO*THET)**2)/(F1-F2)
            D(3,2) = -(SIN(TWO*THET)**2)/(F1-F2)
            D(3,3) = -(TWO*SIN(TWO*THET)*COS(TWO*THET))/(F1-F2)
            
            ! Assembling the Hessian Matrix Dt*DPHI_DSIG*D + T
            D2PHI_D2 = MATMUL(D2PHI_D2,D)
            D2PHI_D2 = MATMUL(TRANSPOSE(D),D2PHI_D2)
            D2PHI_D2(1:3,1:3) = D2PHI_D2(1:3,1:3) + T(1:3,1:3)
c
            ! Computation of eigen values 
            WR   = ZERO
            WI   = ZERO
            VL   = ZERO
            VR   = ZERO
            WORK = ZERO
#ifndef WITHOUT_LINALG
            CALL DGEEV('N','N',3,D2PHI_D2,3,WR,WI,VL,3,VR,3,WORK,102,INFO)
#else
            WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif
c
            ! Storing the minimal value
            IF (MINVAL(WR)<MINEIG) THEN 
              MINEIG     = MINVAL(WR)
              MINEIGMU   = MU
              MINEIGTHET = THET*(180.0D0/PI)
            ENDIF
c
            ! Updating the MU parameter
            MU = MU + ONE/TWENTY
          ENDDO
          ! Updating the theta angle
          THET = THET + (PI/TWO)/60.0D0
        ENDDO
c
        ! Convexity check and error message
        IF (MINEIG<-TOL) THEN
          CALL ANCMSG(MSGID=1805,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .      R1=MINEIG,R2=MINEIGMU,R3=MINEIGTHET)
        ENDIF    
c
      ! Simplified Vegter Lite yield locus
      ELSE
c
        ! ------------------------------------------------------------------------
        ! 1. Checking convexity between uniaxial points in compression and tension      
        ! ------------------------------------------------------------------------
c      
        ! Minimum eigen value of the Hessian matrix of the yield locus
        MINEIG     = INFINITY
        MINEIGMU   = ZERO
        MINEIGTHET = ZERO
c
        ! Initialization of the THETA angle
        THET = ZERO
c        
        ! Loop over the angles
        DO J = 1,61
c
          ! Initialization of the MU parameter
          MU = ZERO
          ! Computing the reference and hinge point for a giving angle
          !   Vector point 
          A1 = ZERO
          A2 = ZERO
          B1 = ZERO
          B2 = ZERO
          C1 = ZERO
          C2 = ZERO
          WEIGHT = ZERO
          !   Initialization of the first derivative with respect to COS2THET
          DA_DCOS2(1:2)   = ZERO
          DB_DCOS2(1:2)   = ZERO
          DC_DCOS2(1:2)   = ZERO
          DWEIGHT_DCOS2   = ZERO
          !  Initialization of the second derivative with respect to COS2THET
          D2A_D2COS2(1:2) = ZERO
          D2B_D2COS2(1:2) = ZERO
          D2C_D2COS2(1:2) = ZERO         
          D2WEIGHT_D2COS2 = ZERO
          ! Loop over angles
          DO I = 1,NANGLE
            ! Computation of the reference points
            DO K = 1,I
              B1 = B1 + Q_HISH(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B2 = B2 + Q_HISH(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              C1 = C1 + Q_fUN(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              C2 = C2 + Q_fUN(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              WEIGHT = WEIGHT + Q_WSH(I)*COS2(K,I)*(COS(TWO*THET))**(K-1)
            ENDDO
          ENDDO
          A1 = C2
          A2 = -C1
          ! If anisotropic, compute derivatives with respect to COS2THET
          IF (NANGLE > 1) THEN 
            ! Computation of their first derivative with respect to COS2THET
            DO I = 2, NANGLE
              DO K = 2,I
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HISH(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DC_DCOS2(1:2) = DC_DCOS2(1:2) + Q_fUN(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DWEIGHT_DCOS2 = DWEIGHT_DCOS2 + Q_WSH(I)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
              ENDDO              
            ENDDO
            DA_DCOS2(1) =  DC_DCOS2(2)
            DA_DCOS2(2) = -DC_DCOS2(1)
            ! Computation of their second derivative with respect to COS2THET
            IF (NANGLE > 2) THEN
              DO I = 3, NANGLE
                DO K = 3,I
                  D2B_D2COS2(1:2) = D2B_D2COS2(1:2) + Q_HISH(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2C_D2COS2(1:2) = D2C_D2COS2(1:2) + Q_fUN(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2WEIGHT_D2COS2 = D2WEIGHT_D2COS2 + Q_WSH(I)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                ENDDO              
              ENDDO 
              D2A_D2COS2(1) =  D2C_D2COS2(2)
              D2A_D2COS2(2) = -D2C_D2COS2(1)
            ENDIF
          ENDIF
c          
          !----------------------------------------------
          ! Computing the Hessian matrix and eigen values
          !----------------------------------------------
          DO I = 1,21
c
            ! Computation of F1 and F2
            F1 = ((ONE-MU)**2)*A1 + TWO*MU*WEIGHT*(ONE-MU)*B1 + (MU**2)*C1
            F1 = F1/(((ONE-MU)**2) + TWO*MU*WEIGHT*(ONE-MU) + (MU**2))
            F2 = ((ONE-MU)**2)*A2 + TWO*MU*WEIGHT*(ONE-MU)*B2 + (MU**2)*C2 
            F2 = F2/(((ONE-MU)**2) + TWO*MU*WEIGHT*(ONE-MU) + (MU**2))
            ! Derivatives of Fi with respect to MU
            U       = A1*((ONE - MU)**2) + TWO*B1*MU*WEIGHT*(ONE-MU) + C1*(MU**2)  
            UPRIM   = -TWO*(ONE-MU)*A1  + TWO*WEIGHT*B1*(ONE-TWO*MU)  + TWO*MU*C1
            UPPRIM  = TWO*A1 - FOUR*WEIGHT*B1 + TWO*C1
            V       = (ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2
            VPRIM   = -TWO*(ONE-MU) + TWO*WEIGHT*(ONE-TWO*MU) + TWO*MU
            VPPRIM  = TWO - FOUR*WEIGHT + TWO
            DF1_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
            D2F1_D2MU = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                   (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            U       = A2*((ONE - MU)**2) + TWO*B2*MU*WEIGHT*(ONE-MU) + C2*(MU**2)  
            UPRIM   = -TWO*(ONE-MU)*A2  + TWO*WEIGHT*B2*(ONE-TWO*MU) + TWO*MU*C2
            UPPRIM  = TWO*A2 - FOUR*WEIGHT*B2 + TWO*C2
            DF2_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)   
            D2F2_D2MU = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                   (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            ! Denominator and its derivative with respect to MU
            DENOM = F1*DF2_DMU - F2*DF1_DMU
            DENOM = SIGN(MAX(ABS(DENOM),EM20),DENOM)
            ! Derivatives of Phi with respect to SIG1 and SIG2
            DPHI_DSIG1 =  DF2_DMU/DENOM
            DPHI_DSIG2 = -DF1_DMU/DENOM
            ! Derivatives of MU with respect to SIG1 and SIG2
            DMU_DSIG1  = -F2/DENOM
            DMU_DSIG2  = F1/DENOM
c
            ! Derivatives of Fi with respect to COS2
            U          = A1*((ONE - MU)**2) + TWO*B1*MU*WEIGHT*(ONE-MU) + C1*(MU**2)  
            UPRIM      = DA_DCOS2(1)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(DWEIGHT_DCOS2*B1+
     .                       WEIGHT*DB_DCOS2(1)) + DC_DCOS2(1)*(MU**2)
            UPPRIM     = D2A_D2COS2(1)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(D2WEIGHT_D2COS2*B1+
     .                       TWO*DWEIGHT_DCOS2*DB_DCOS2(1) + WEIGHT*D2B_D2COS2(1)) + 
     .                       D2C_D2COS2(1)*(MU**2)
            DU_DMU     = -TWO*(ONE-MU)*A1  + TWO*WEIGHT*B1*(ONE-TWO*MU)  + TWO*MU*C1
            DUPRIM_DMU = -TWO*(ONE-MU)*DA_DCOS2(1) + TWO*(ONE-TWO*MU)*(DWEIGHT_DCOS2*B1+
     .                       WEIGHT*DB_DCOS2(1)) + TWO*MU*DC_DCOS2(1)
            V          = (ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2
            DV_DMU     = -TWO*(ONE-MU) + TWO*WEIGHT*(ONE-TWO*MU) + TWO*MU
            VPRIM      = TWO*MU*(ONE-MU)*DWEIGHT_DCOS2
            DVPRIM_DMU = TWO*(ONE-TWO*MU)*DWEIGHT_DCOS2
            VPPRIM     = TWO*MU*(ONE-MU)*D2WEIGHT_D2COS2
            DF1_DCOS2  = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
            D2F1_D2COS2 = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                   (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            D2F1_D2MUCOS2 = ((DUPRIM_DMU*V + UPRIM*DV_DMU - DU_DMU*VPRIM - U*DVPRIM_DMU)*(V**2) - 
     .                       (UPRIM*V - U*VPRIM)*(TWO*V*DV_DMU))/MAX((V**4),EM20)
c            
            U          = A2*((ONE - MU)**2) + TWO*B2*MU*WEIGHT*(ONE-MU) + C2*(MU**2)  
            UPRIM      = DA_DCOS2(2)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(DWEIGHT_DCOS2*B2+
     .                       WEIGHT*DB_DCOS2(2)) + DC_DCOS2(2)*(MU**2)
            UPPRIM     = D2A_D2COS2(2)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(D2WEIGHT_D2COS2*B2+
     .                       TWO*DWEIGHT_DCOS2*DB_DCOS2(2) + WEIGHT*D2B_D2COS2(2)) + 
     .                       D2C_D2COS2(2)*(MU**2)
            DU_DMU     = -TWO*(ONE-MU)*A2  + TWO*WEIGHT*B2*(ONE-TWO*MU)  + TWO*MU*C2
            DUPRIM_DMU = -TWO*(ONE-MU)*DA_DCOS2(2) + TWO*(ONE-TWO*MU)*(DWEIGHT_DCOS2*B2+
     .                       WEIGHT*DB_DCOS2(2)) + TWO*MU*DC_DCOS2(2)
            DF2_DCOS2  = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
            D2F2_D2COS2 = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                    (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            D2F2_D2MUCOS2 = ((DUPRIM_DMU*V + UPRIM*DV_DMU - DU_DMU*VPRIM - U*DVPRIM_DMU)*(V**2) - 
     .                       (UPRIM*V - U*VPRIM)*(TWO*V*DV_DMU))/MAX((V**4),EM20)
c            
            DPHI_DCOS2 = (DF2_DCOS2*DF1_DMU - DF1_DCOS2*DF2_DMU)/DENOM
            DMU_DCOS2  = (F2*DF1_DCOS2 - F1*DF2_DCOS2)/DENOM
c
            ! Second derivatives of PHI with respect to SIG1 and SIG2 (Hessian matrix)
            !-------------------------------------------------------------------------
            !   FIRST LINE OF THE MATRIX
            !   D2PHI_D2SIG1
            D2PHI_D2(1,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG1
            !   D2PHI_DSIG1SIG2
            D2PHI_D2(1,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG2
            !   D2PHI_DSIG1COS2
            D2PHI_D2(1,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
c
            !   SECOND LINE OF THE MATRIX
            !   D2PHI_DSIG2SIG1
            D2PHI_D2(2,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG1
            !   D2PHI_D2SIG2
            D2PHI_D2(2,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG2
            !   D2PHI_DSIG2COS2   
            D2PHI_D2(2,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)
c
            !   THIRD LINE OF THE MATRIX
            !   D2PHI_D2COS2SIG1
            D2PHI_D2(3,1) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
            !   D2PHI_D2COS2SIG2
            D2PHI_D2(3,2) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)          
            !   D2PHI_D2COS2
            D2PHI_D2(3,3) = (-ONE/DENOM)*(DF2_DMU*(D2F1_D2COS2 + TWO*D2F1_D2MUCOS2*DMU_DCOS2 + D2F1_D2MU*(DMU_DCOS2**2)) 
     .                          - DF1_DMU*(D2F2_D2COS2 + TWO*D2F2_D2MUCOS2*DMU_DCOS2 + D2F2_D2MU*(DMU_DCOS2**2))
     .                          + TWO*(DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DCOS2)
c    
            ! Filling the T matrix
            T(1,1) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG2 - DPHI_DSIG1)  +
     .                DPHI_DCOS2*(THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG2 - DPHI_DSIG1) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 - 
     .                TWO*(SIN(TWO*THET)**3))
            T(2,1) =  T(1,2)
            T(2,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(2,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(-FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 + 
     .                TWO*(SIN(TWO*THET)**3))
            T(3,1) =  T(1,3)
            T(3,2) =  T(2,3)
            T(3,3) =  (TWO/(F1-F2))*((COS(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2)+
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(EIGHT*(SIN(TWO*THET)**2)*COS(TWO*THET) - 
     .                FOUR*(COS(TWO*THET)**3))
     
            ! Filling the rotation D matrix
            D(1,1) = HALF*(ONE+COS(TWO*THET))
            D(1,2) = HALF*(ONE-COS(TWO*THET))
            D(1,3) = SIN(TWO*THET)
            D(2,1) = HALF*(ONE-COS(TWO*THET))
            D(2,2) = HALF*(ONE+COS(TWO*THET))
            D(2,3) = -SIN(TWO*THET)
            D(3,1) = (SIN(TWO*THET)**2)/(F1-F2)
            D(3,2) = -(SIN(TWO*THET)**2)/(F1-F2)
            D(3,3) = -(TWO*SIN(TWO*THET)*COS(TWO*THET))/(F1-F2)
            
            ! Assembling the Hessian Matrix Dt*DPHI_DSIG*D + T
            D2PHI_D2 = MATMUL(D2PHI_D2,D)
            D2PHI_D2 = MATMUL(TRANSPOSE(D),D2PHI_D2)
            D2PHI_D2(1:3,1:3) = D2PHI_D2(1:3,1:3) + T(1:3,1:3)
c
            ! Computation of eigen values 
            WR   = ZERO
            WI   = ZERO
            VL   = ZERO
            VR   = ZERO
            WORK = ZERO
#ifndef WITHOUT_LINALG
            CALL DGEEV('N','N',3,D2PHI_D2,3,WR,WI,VL,3,VR,3,WORK,102,INFO)
c
#else
        WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif

            ! Storing the minimal value
            IF (MINVAL(WR)<MINEIG) THEN 
              MINEIG     = MINVAL(WR)
              MINEIGMU   = MU
              MINEIGTHET = THET*(180.0D0/PI)
            ENDIF
            ! Updating the MU parameter
            MU = MU + ONE/TWENTY
          ENDDO
          ! Updating the theta angle
          THET = THET + (PI/TWO)/60.0D0
        ENDDO
c        
        ! Convexity check and error message
        IF (MINEIG<-TOL) THEN
          CALL ANCMSG(MSGID=1806,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .      R1=MINEIG,R2=MINEIGMU,R3=MINEIGTHET)
        ENDIF
c
        ! ------------------------------------------------------------------------
        ! 2. Checking convexity between uniaxial tension and equi-biaxial points    
        ! ------------------------------------------------------------------------
c      
        ! Minimum eigen value of the Hessian matrix of the yield locus
        MINEIG     = INFINITY
        MINEIGMU   = ZERO
        MINEIGTHET = ZERO
c
        ! Initialization of the THETA angle
        THET = ZERO
c        
        ! Loop over the angles
        DO J = 1,61
c
          ! Initialization of the MU parameter
          MU = ZERO
          ! Computing the reference and hinge point for a giving angle
          !   Vector point 
          A1 = ZERO
          A2 = ZERO
          B1 = ZERO
          B2 = ZERO
          C1 = ZERO
          C2 = ZERO
          WEIGHT = ZERO
          !   Initialization of the first derivative with respect to COS2THET
          DA_DCOS2(1:2)   = ZERO
          DB_DCOS2(1:2)   = ZERO
          DC_DCOS2(1:2)   = ZERO
          DWEIGHT_DCOS2   = ZERO
          !  Initialization of the second derivative with respect to COS2THET
          D2A_D2COS2(1:2) = ZERO
          D2B_D2COS2(1:2) = ZERO
          D2C_D2COS2(1:2) = ZERO         
          D2WEIGHT_D2COS2 = ZERO
          ! Loop over angles
          DO I = 1,NANGLE
            ! Computation of the reference points
            DO K = 1,I
              A1 = A1 + Q_fUN(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              A2 = A2 + Q_fUN(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B1 = B1 + Q_HIPS(I,1)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              B2 = B2 + Q_HIPS(I,2)*COS2(K,I)*(COS(TWO*THET))**(K-1)
              WEIGHT = WEIGHT + Q_WPS(I)*COS2(K,I)*(COS(TWO*THET))**(K-1)
            ENDDO
          ENDDO
          C1 = fBI
          C2 = fBI
          ! If anisotropic, compute derivatives with respect to COS2THET
          IF (NANGLE > 1) THEN 
            ! Computation of their first derivative with respect to COS2THET
            DO I = 2, NANGLE
              DO K = 2,I
                DA_DCOS2(1:2) = DA_DCOS2(1:2) + Q_fUN(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DB_DCOS2(1:2) = DB_DCOS2(1:2) + Q_HIPS(I,1:2)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
                DWEIGHT_DCOS2 = DWEIGHT_DCOS2 + Q_WPS(I)*(K-1)*COS2(K,I)*(COS(TWO*THET))**(K-2)
              ENDDO              
            ENDDO
            ! Computation of their second derivative with respect to COS2THET
            IF (NANGLE > 2) THEN
              DO I = 3, NANGLE
                DO K = 3,I
                  D2A_D2COS2(1:2) = D2A_D2COS2(1:2) + Q_fUN(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2B_D2COS2(1:2) = D2B_D2COS2(1:2) + Q_HIPS(I,1:2)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                  D2WEIGHT_D2COS2 = D2WEIGHT_D2COS2 + Q_WPS(I)*(K-1)*(K-2)*COS2(K,I)*(COS(TWO*THET))**(K-3)
                ENDDO              
              ENDDO
            ENDIF
          ENDIF
c          
          !----------------------------------------------
          ! Computing the Hessian matrix and eigen values
          !----------------------------------------------
          DO I = 1,20
c
            ! Computation of F1 and F2
            F1 = ((ONE-MU)**2)*A1 + TWO*MU*WEIGHT*(ONE-MU)*B1 + (MU**2)*C1
            F1 = F1/(((ONE-MU)**2) + TWO*MU*WEIGHT*(ONE-MU) + (MU**2))
            F2 = ((ONE-MU)**2)*A2 + TWO*MU*WEIGHT*(ONE-MU)*B2 + (MU**2)*C2 
            F2 = F2/(((ONE-MU)**2) + TWO*MU*WEIGHT*(ONE-MU) + (MU**2))
            ! Derivatives of Fi with respect to MU
            U       = A1*((ONE - MU)**2) + TWO*B1*MU*WEIGHT*(ONE-MU) + C1*(MU**2)  
            UPRIM   = -TWO*(ONE-MU)*A1  + TWO*WEIGHT*B1*(ONE-TWO*MU)  + TWO*MU*C1
            UPPRIM  = TWO*A1 - FOUR*WEIGHT*B1 + TWO*C1
            V       = (ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2
            VPRIM   = -TWO*(ONE-MU) + TWO*WEIGHT*(ONE-TWO*MU) + TWO*MU
            VPPRIM  = TWO - FOUR*WEIGHT + TWO
            DF1_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
            D2F1_D2MU = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                   (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            U       = A2*((ONE - MU)**2) + TWO*B2*MU*WEIGHT*(ONE-MU) + C2*(MU**2)  
            UPRIM   = -TWO*(ONE-MU)*A2  + TWO*WEIGHT*B2*(ONE-TWO*MU) + TWO*MU*C2
            UPPRIM  = TWO*A2 - FOUR*WEIGHT*B2 + TWO*C2
            DF2_DMU = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)   
            D2F2_D2MU = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                   (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            ! Denominator and its derivative with respect to MU
            DENOM = F1*DF2_DMU - F2*DF1_DMU
            DENOM = SIGN(MAX(ABS(DENOM),EM20),DENOM)
            ! Derivatives of Phi with respect to SIG1 and SIG2
            DPHI_DSIG1 =  DF2_DMU/DENOM
            DPHI_DSIG2 = -DF1_DMU/DENOM
            ! Derivatives of MU with respect to SIG1 and SIG2
            DMU_DSIG1  = -F2/DENOM
            DMU_DSIG2  = F1/DENOM
c
            ! Derivatives of Fi with respect to COS2
            U          = A1*((ONE - MU)**2) + TWO*B1*MU*WEIGHT*(ONE-MU) + C1*(MU**2)  
            UPRIM      = DA_DCOS2(1)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(DWEIGHT_DCOS2*B1+
     .                       WEIGHT*DB_DCOS2(1)) + DC_DCOS2(1)*(MU**2)
            UPPRIM     = D2A_D2COS2(1)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(D2WEIGHT_D2COS2*B1+
     .                       TWO*DWEIGHT_DCOS2*DB_DCOS2(1) + WEIGHT*D2B_D2COS2(1)) + 
     .                       D2C_D2COS2(1)*(MU**2)
            DU_DMU     = -TWO*(ONE-MU)*A1  + TWO*WEIGHT*B1*(ONE-TWO*MU)  + TWO*MU*C1
            DUPRIM_DMU = -TWO*(ONE-MU)*DA_DCOS2(1) + TWO*(ONE-TWO*MU)*(DWEIGHT_DCOS2*B1+
     .                       WEIGHT*DB_DCOS2(1)) + TWO*MU*DC_DCOS2(1)
            V          = (ONE-MU)**2 + TWO*WEIGHT*MU*(ONE-MU) + MU**2
            DV_DMU     = -TWO*(ONE-MU) + TWO*WEIGHT*(ONE-TWO*MU) + TWO*MU
            VPRIM      = TWO*MU*(ONE-MU)*DWEIGHT_DCOS2
            DVPRIM_DMU = TWO*(ONE-TWO*MU)*DWEIGHT_DCOS2
            VPPRIM     = TWO*MU*(ONE-MU)*D2WEIGHT_D2COS2
            DF1_DCOS2  = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
            D2F1_D2COS2 = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                   (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            D2F1_D2MUCOS2 = ((DUPRIM_DMU*V + UPRIM*DV_DMU - DU_DMU*VPRIM - U*DVPRIM_DMU)*(V**2) - 
     .                       (UPRIM*V - U*VPRIM)*(TWO*V*DV_DMU))/MAX((V**4),EM20)
c            
            U          = A2*((ONE - MU)**2) + TWO*B2*MU*WEIGHT*(ONE-MU) + C2*(MU**2)  
            UPRIM      = DA_DCOS2(2)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(DWEIGHT_DCOS2*B2+
     .                       WEIGHT*DB_DCOS2(2)) + DC_DCOS2(2)*(MU**2)
            UPPRIM     = D2A_D2COS2(2)*((ONE - MU)**2) + TWO*MU*(ONE-MU)*(D2WEIGHT_D2COS2*B2+
     .                       TWO*DWEIGHT_DCOS2*DB_DCOS2(2) + WEIGHT*D2B_D2COS2(2)) + 
     .                       D2C_D2COS2(2)*(MU**2)
            DU_DMU     = -TWO*(ONE-MU)*A2  + TWO*WEIGHT*B2*(ONE-TWO*MU)  + TWO*MU*C2
            DUPRIM_DMU = -TWO*(ONE-MU)*DA_DCOS2(2) + TWO*(ONE-TWO*MU)*(DWEIGHT_DCOS2*B2+
     .                       WEIGHT*DB_DCOS2(2)) + TWO*MU*DC_DCOS2(2)
            DF2_DCOS2  = (UPRIM*V - U*VPRIM)/MAX((V**2),EM20)
            D2F2_D2COS2 = ((UPPRIM*V - U*VPPRIM)*(V**2) - 
     .                    (UPRIM*V - U*VPRIM)*(TWO*V*VPRIM))/MAX((V**4),EM20)
            D2F2_D2MUCOS2 = ((DUPRIM_DMU*V + UPRIM*DV_DMU - DU_DMU*VPRIM - U*DVPRIM_DMU)*(V**2) - 
     .                       (UPRIM*V - U*VPRIM)*(TWO*V*DV_DMU))/MAX((V**4),EM20)
c            
            DPHI_DCOS2 = (DF2_DCOS2*DF1_DMU - DF1_DCOS2*DF2_DMU)/DENOM
            DMU_DCOS2  = (F2*DF1_DCOS2 - F1*DF2_DCOS2)/DENOM
c
            ! Second derivatives of PHI with respect to SIG1 and SIG2 (Hessian matrix)
            !-------------------------------------------------------------------------
            !   FIRST LINE OF THE MATRIX
            !   D2PHI_D2SIG1
            D2PHI_D2(1,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG1
            !   D2PHI_DSIG1SIG2
            D2PHI_D2(1,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG1*DMU_DSIG2
            !   D2PHI_DSIG1COS2
            D2PHI_D2(1,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
c
            !   SECOND LINE OF THE MATRIX
            !   D2PHI_DSIG2SIG1
            D2PHI_D2(2,1) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG1
            !   D2PHI_D2SIG2
            D2PHI_D2(2,2) = (-ONE/DENOM)*(DF2_DMU*D2F1_D2MU-DF1_DMU*D2F2_D2MU)*DMU_DSIG2*DMU_DSIG2
            !   D2PHI_DSIG2COS2   
            D2PHI_D2(2,3) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)
c
            !   THIRD LINE OF THE MATRIX
            !   D2PHI_D2COS2SIG1
            D2PHI_D2(3,1) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG1
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG1)
            !   D2PHI_D2COS2SIG2
            D2PHI_D2(3,2) = (-ONE/DENOM)*((DF2_DMU*(D2F1_D2MUCOS2+D2F1_D2MU*DMU_DCOS2) - 
     .                            DF1_DMU*(D2F2_D2MUCOS2+D2F2_D2MU*DMU_DCOS2))*DMU_DSIG2
     .                         + (DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DSIG2)          
            !   D2PHI_D2COS2
            D2PHI_D2(3,3) = (-ONE/DENOM)*(DF2_DMU*(D2F1_D2COS2 + TWO*D2F1_D2MUCOS2*DMU_DCOS2 + D2F1_D2MU*(DMU_DCOS2**2)) 
     .                          - DF1_DMU*(D2F2_D2COS2 + TWO*D2F2_D2MUCOS2*DMU_DCOS2 + D2F2_D2MU*(DMU_DCOS2**2))
     .                          + TWO*(DF2_DMU*DF1_DCOS2 - DF1_DMU*DF2_DCOS2)*DPHI_DCOS2)
c    
            ! Filling the T matrix
            T(1,1) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG2 - DPHI_DSIG1)  +
     .                DPHI_DCOS2*(THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(1,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG2 - DPHI_DSIG1) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 - 
     .                TWO*(SIN(TWO*THET)**3))
            T(2,1) =  T(1,2)
            T(2,2) =  (HALF/(F1-F2))*((SIN(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(-THREE/(F1-F2)**2)*COS(TWO*THET)*(SIN(TWO*THET))**2
            T(2,3) =  (ONE/(F1-F2))*(SIN(TWO*THET)*COS(TWO*THET))*(DPHI_DSIG1-DPHI_DSIG2) +
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(-FOUR*SIN(TWO*THET)*(COS(TWO*THET))**2 + 
     .                TWO*(SIN(TWO*THET)**3))
            T(3,1) =  T(1,3)
            T(3,2) =  T(2,3)
            T(3,3) =  (TWO/(F1-F2))*((COS(TWO*THET))**2)*(DPHI_DSIG1-DPHI_DSIG2)+
     .                DPHI_DCOS2*(ONE/(F1-F2)**2)*(EIGHT*(SIN(TWO*THET)**2)*COS(TWO*THET) - 
     .                FOUR*(COS(TWO*THET)**3))
     
            ! Filling the rotation D matrix
            D(1,1) = HALF*(ONE+COS(TWO*THET))
            D(1,2) = HALF*(ONE-COS(TWO*THET))
            D(1,3) = SIN(TWO*THET)
            D(2,1) = HALF*(ONE-COS(TWO*THET))
            D(2,2) = HALF*(ONE+COS(TWO*THET))
            D(2,3) = -SIN(TWO*THET)
            D(3,1) = (SIN(TWO*THET)**2)/(F1-F2)
            D(3,2) = -(SIN(TWO*THET)**2)/(F1-F2)
            D(3,3) = -(TWO*SIN(TWO*THET)*COS(TWO*THET))/(F1-F2)
            
            ! Assembling the Hessian Matrix Dt*DPHI_DSIG*D + T
            D2PHI_D2 = MATMUL(D2PHI_D2,D)
            D2PHI_D2 = MATMUL(TRANSPOSE(D),D2PHI_D2)
            D2PHI_D2(1:3,1:3) = D2PHI_D2(1:3,1:3) + T(1:3,1:3)
c
            ! Computation of eigen values 
            WR   = ZERO
            WI   = ZERO
            VL   = ZERO
            VR   = ZERO
            WORK = ZERO
#ifndef WITHOUT_LINALG
            CALL DGEEV('N','N',3,D2PHI_D2,3,WR,WI,VL,3,VR,3,WORK,102,INFO)
#else
        WRITE(6,*) "Error: Blas/Lapack required for MAT110"
#endif

c
            ! Storing the minimal value
            IF (MINVAL(WR)<MINEIG) THEN 
              MINEIG     = MINVAL(WR)
              MINEIGMU   = MU
              MINEIGTHET = THET*(180.0D0/PI)
            ENDIF
            ! Updating the MU parameter
            MU = MU + ONE/TWENTY
          ENDDO
          ! Updating the theta angle
          THET = THET + (PI/TWO)/60.0D0
        ENDDO
c        
        ! Convexity check and error message
        IF (MINEIG<-TOL) THEN
          CALL ANCMSG(MSGID=1807,MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .      R1=MINEIG,R2=MINEIGMU,R3=MINEIGTHET)
        ENDIF
c    
      ENDIF
c
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameter
      NUPARAM = 29
      IF ((Icrit == 1).OR.(Icrit == 2).OR.(Icrit == 3)) THEN
        NUPARAM = NUPARAM + 17*NANGLE
      ELSE
        NUPARAM = NUPARAM + 11*NANGLE      
      ENDIF
      ! Number of function and temp variable
      IF (TAB_YLD > 0) THEN 
        NTABL     = 1
        ITABLE(1) = TAB_YLD     ! Yield function table              = f(epsp,epsdot)
        NVARTMP   = 1
        IF (TAB_TEMP > 0) THEN
          NTABL     = 2
          ITABLE(2) = TAB_TEMP  ! Temperature scale factor table    = f(epsp,T)
          NVARTMP   = 3
        ENDIF
      ELSE
        NTABL     = 0
        NVARTMP   = 0
      ENDIF
      ! Number of user variable
      NUVAR = 1
      IF (Ires == 1) THEN 
        NUVAR = 2
      ENDIF
c      
      ! Material parameters
      UPARAM(1)  = YOUNG
      UPARAM(2)  = NU
      UPARAM(3)  = G
      UPARAM(4)  = TWO*G
      UPARAM(5)  = NU/(ONE-NU)
      UPARAM(6)  = A11
      UPARAM(7)  = A11*NU
      UPARAM(8)  = YLD0
      UPARAM(9)  = DSIGM
      UPARAM(10) = BETA
      UPARAM(11) = OMEGA
      UPARAM(12) = NEXP
      UPARAM(13) = EPS0
      UPARAM(14) = SIGSTAR
      UPARAM(15) = DG0
      UPARAM(16) = Deps0
      UPARAM(17) = MEXP
      UPARAM(18) = KBOLTZ
      IF (XSCALE /= ZERO) THEN
        UPARAM(19) = ONE/XSCALE
      ELSE
        UPARAM(19) = ZERO
      ENDIF
      UPARAM(20) = YSCALE
      UPARAM(21) = FISOKIN
      UPARAM(22) = ASRATE
      UPARAM(23) = Ivflag
      UPARAM(24) = TINI
      UPARAM(25) = fBI
      UPARAM(26) = NANGLE
      UPARAM(27) = Icrit
      UPARAM(28) = Ismooth
      UPARAM(29) = Ires
      IF ((Icrit == 1).OR.(Icrit == 2).OR.(Icrit == 3)) THEN
        DO J = 1,NANGLE
          UPARAM(30+17*(J-1)) = HIPS(J,1)
          UPARAM(31+17*(J-1)) = HIPS(J,2)
          UPARAM(32+17*(J-1)) = HIUN(J,1)
          UPARAM(33+17*(J-1)) = HIUN(J,2)
          UPARAM(34+17*(J-1)) = HISH(J,1)
          UPARAM(35+17*(J-1)) = HISH(J,2)
          UPARAM(36+17*(J-1)) = Q_fSH(J,1)
          UPARAM(37+17*(J-1)) = Q_fSH(J,2)
          UPARAM(38+17*(J-1)) = Q_fUN(J,1)
          UPARAM(39+17*(J-1)) = Q_fPS(J,1)
          UPARAM(40+17*(J-1)) = Q_fPS(J,2)
          UPARAM(41+17*(J-1)) = Q_HISH(J,1)
          UPARAM(42+17*(J-1)) = Q_HISH(J,2)
          UPARAM(43+17*(J-1)) = Q_HIUN(J,1)
          UPARAM(44+17*(J-1)) = Q_HIUN(J,2)
          UPARAM(45+17*(J-1)) = Q_HIPS(J,1)
          UPARAM(46+17*(J-1)) = Q_HIPS(J,2)
        ENDDO
      ELSE
        DO J = 1,NANGLE
          UPARAM(30+11*(J-1)) = HIPS(J,1)
          UPARAM(31+11*(J-1)) = HIPS(J,2)
          UPARAM(32+11*(J-1)) = HISH(J,1)
          UPARAM(33+11*(J-1)) = HISH(J,2)
          UPARAM(34+11*(J-1)) = Q_fUN(J,1)
          UPARAM(35+11*(J-1)) = Q_HISH(J,1)
          UPARAM(36+11*(J-1)) = Q_HISH(J,2)
          UPARAM(37+11*(J-1)) = Q_HIPS(J,1)
          UPARAM(38+11*(J-1)) = Q_HIPS(J,2)
          UPARAM(39+11*(J-1)) = Q_WSH(J)
          UPARAM(40+11*(J-1)) = Q_WPS(J)
        ENDDO      
      ENDIF
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
      PM(27) = SQRT(A11/RHO0)  ! sound speed estimation
      PM(100)= BULK   
c      
      ! MTAG variable activation
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%L_EPSD = 1
      MTAG%G_EPSD = 1
      MTAG%L_SEQ  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_TEMP = 1
      MTAG%G_TEMP = 1
      IF (FISOKIN > ZERO) THEN 
        MTAG%L_SIGA  = 3
      ENDIF
c
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM,"LARGE_STRAIN"  )
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC"   )
c 
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ORTHOTROPIC")
c
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      IF (Icrit == 1) THEN 
        WRITE(IOUT,1100)
      ELSEIF (Icrit == 2) THEN
        WRITE(IOUT,1125)
      ELSEIF (Icrit == 3) THEN
        WRITE(IOUT,1135)
      ELSE
        WRITE(IOUT,1150)
      ENDIF
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) YOUNG,NU
        WRITE(IOUT,1350) Ires
        IF (ITABLE(1)>0) THEN 
          WRITE(IOUT,1500) ITABLE(1),XSCALE,YSCALE,Ismooth,EPS0,FISOKIN,
     .                     Ivflag,ITABLE(2),TINI
        ELSE
          WRITE(IOUT,1400) YLD0,DSIGM,BETA,OMEGA,NEXP,EPS0,FISOKIN
          IF (SIGSTAR>ZERO) THEN
            WRITE(IOUT,1600) SIGSTAR,DG0,Deps0,MEXP,TINI,ASRATE,Ivflag
          ENDIF  
        ENDIF
        IF ((Icrit == 1).OR.(Icrit == 2).OR.(Icrit == 3)) THEN 
          DO J = 1,NANGLE
            WRITE(IOUT,1700) THETA(J),fSH(J,1),fSH(J,2),HISH(J,1),HISH(J,2),
     .                       fUN(J,1),fUN(J,2),rLank(J),HIUN(J,1),HIUN(J,2),
     .                       fPS(J,1),fPS(J,2),HIPS(J,1),HIPS(J,2),fBI,fBI
          ENDDO
        ELSE
          DO J = 1,NANGLE
            WRITE(IOUT,1750) THETA(J),HISH(J,1),HISH(J,2),WSH(J),
     .                       fUN(J,1),fUN(J,2),rLank(J),HIPS(J,1),
     .                       HIPS(J,2),WPS(J),fBI,fBI                  
          ENDDO          
        ENDIF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : VEGTER ELASTO-PLASTIC',/,
     & 5X,'--------------------------------------',/)
 1125 FORMAT
     &(5X,'MATERIAL MODEL : VEGTER STANDARD ELASTO-PLASTIC',/,
     & 5X,'-----------------------------------------------',/)
 1135 FORMAT
     &(5X,'MATERIAL MODEL : VEGTER 2017 ELASTO-PLASTIC',/,
     & 5X,'-------------------------------------------',/)
 1150 FORMAT
     &(5X,'MATERIAL MODEL : SIMPLIFIED VEGTER LITE ELASTO-PLASTIC',/,
     & 5X,'------------------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/)
 1350 FORMAT(
     & 5X,'RETURN MAPPING ALGORITHM FLAG . . . . .=',I3/
     & 5X,'  IRES=1  NICE EXPLICIT'/
     & 5X,'  IRES=2  NEWTON-ITERATION IMPLICIT (CUTTING PLANE)'/)
 1400 FORMAT(
     & 5X,'INITIAL YIELD STRESS  . . . . . . . . .=',1PG20.13/
     & 5X,'STRESS HARDENING INCREMENT. . . . . . .=',1PG20.13/
     & 5X,'SMALL STRAIN HARDENING PARAMETER  . . .=',1PG20.13/
     & 5X,'LARGE STRAIN HARDENING PARAMETER  . . .=',1PG20.13/
     & 5X,'HARDENING EXPONENT  . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL PLASTIC STRAIN  . . . . . . . .=',1PG20.13/
     & 5X,'KINEMATIC HARDENING FACTOR  . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABLE INTERPOLATION FLAG  . . . . . . .=',I10/ 
     & 5X,'     ISMOOTH=1  LINEAR INTERPOLATION'/
     & 5X,'     ISMOOTH=2  LOGARITHMIC INTERPOLATION BASE 10'/
     & 5X,'     ISMOOTH=3  LOGARITHMIC INTERPOLATION BASE N'/
     & 5X,'INITIAL PLASTIC STRAIN  . . . . . . . .=',1PG20.13/
     & 5X,'KINEMATIC HARDENING FACTOR  . . . . . .=',1PG20.13/
     & 5X,'STRAIN-RATE CHOICE FLAG . . . . . . . .=',I10/
     & 5X,'     VP=1  EQUIVALENT PLASTIC STRAIN RATE'/
     & 5X,'     VP=2  TOTAL STRAIN RATE (DEFAULT)'/
     & 5X,'     VP=3  DEVIATORIC STRAIN RATE'/
     & 5X,'TABULATED YIELD - TEMPERATURE TABLE ID .=',I10/
     & 5X,'INITIAL (REFERENCE) TEMPERATURE. . . . .=',1PG20.13/) 
 1600 FORMAT(
     & 5X,'LIMIT DYNAMIC FLOW STRESS . . . . . . .=',1PG20.13/
     & 5X,'MAXIMUM ACTIVATION ENTHALPY . . . . . .=',1PG20.13/
     & 5X,'THERMAL ACTIVATION LIMIT STRAIN-RATE  .=',1PG20.13/
     & 5X,'STRAIN-RATE EXPONENT  . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN-RATE CUTTING FREQUENCY . . . . .=',1PG20.13/
     & 5X,'STRAIN-RATE CHOICE FLAG . . . . . . . .=',I10/
     & 5X,'     VP=1  EQUIVALENT PLASTIC STRAIN RATE'/
     & 5X,'     VP=2  TOTAL STRAIN RATE (DEFAULT)'/
     & 5X,'     VP=3  DEVIATORIC STRAIN RATE'/)   
 1700 FORMAT(     
     & 5X,'|| DATA FOR ANGLE (DEG) . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR REFERENCE POINT . . . . . . . . . '/
     & 5X,'      FIRST  COORDINATE fSH1. . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE fSH2. . . . . .=',1PG20.13/
     & 5X,'SHEAR/UNIAXIAL HINGE POINT. . . . . . . '/
     & 5X,'      FIRST  COORDINATE HISH1 . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE HISH2 . . . . .=',1PG20.13/ 
     & 5X,'UNIAXIAL REFERENCE POINT. . . . . . . . '/
     & 5X,'      FIRST  COORDINATE fUN1. . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE fUN2. . . . . .=',1PG20.13/
     & 5X,'      LANKFORD COEFFICIENT  . . . . . .=',1PG20.13/
     & 5X,'UNIAXIAL/PLANE-STRAIN HINGE POINT . . . '/
     & 5X,'      FIRST  COORDINATE HIUN1 . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE HIUN2 . . . . .=',1PG20.13/ 
     & 5X,'PLANE-STRAIN REFERENCE POINT  . . . . . '/
     & 5X,'      FIRST  COORDINATE fPS1. . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE fPS2. . . . . .=',1PG20.13/
     & 5X,'PLANE-STRAIN/BIAXIAL HINGE POINT. . . . '/
     & 5X,'      FIRST  COORDINATE HIPS1 . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE HIPS2 . . . . .=',1PG20.13/
     & 5X,'BIAXIAL REFERENCE POINT . . . . . . . . '/
     & 5X,'      FIRST  COORDINATE fBI1. . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE fBI2. . . . . .=',1PG20.13/)
 1750 FORMAT(     
     & 5X,'|| DATA FOR ANGLE (DEG) . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR HINGE POINT . . . . . . . . . . . '/
     & 5X,'      FIRST  COORDINATE HISH1 . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE HISH2 . . . . .=',1PG20.13/ 
     & 5X,'      SHEAR WEIGHT FACTOR . . . . . . .=',1PG20.13/
     & 5X,'UNIAXIAL REFERENCE POINT. . . . . . . . '/
     & 5X,'      FIRST  COORDINATE fUN1. . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE fUN2. . . . . .=',1PG20.13/
     & 5X,'      LANKFORD COEFFICIENT  . . . . . .=',1PG20.13/
     & 5X,'PLANE-STRAIN HINGE POINT  . . . . . . . '/
     & 5X,'      FIRST  COORDINATE HIPS1 . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE HIPS2 . . . . .=',1PG20.13/
     & 5X,'      PLANE-STRAIN WEIGHT FACTOR. . . .=',1PG20.13/
     & 5X,'BIAXIAL REFERENCE POINT . . . . . . . . '/
     & 5X,'      FIRST  COORDINATE fBI1. . . . . .=',1PG20.13/
     & 5X,'      SECOND COORDINATE fBI2. . . . . .=',1PG20.13/)
c-----------------------------------------------------------------------
      RETURN
      END
