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
Chd|  HM_READ_MAT24                 source/materials/mat/mat024/hm_read_mat24.F
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
      SUBROUTINE HM_READ_MAT24(
     .           NUPARAM  ,NUVAR    ,NFUNC    ,IPM      ,PM       ,
     .           MTAG     ,MAT_ID   ,TITR     ,UNITAB   ,LSUBMODEL,
     .           ISRATE   ,MATPARAM ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW24 (CONCRETE)
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID
      INTEGER, INTENT(INOUT)                         :: NUPARAM,NUVAR,NFUNC
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)     :: IPM
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)     :: PM     
      CHARACTER(nchartitle) ,INTENT(IN)            :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ICAP,ILAW,ISRATE
      my_real :: RHO0,RHOR,YMC,ANUC,FC,FT,FB,F2D,S0,HT,DSUP1,EPSMAX,VKY,RT,
     .   RC,HBP,ALI,ALF,VMAX,ROK,RO0,HV0,YMS,Y0S,ETS,ARM1,ARM2,ARM3,
     .   ETC,BULK,EXPO,F2D0,AA,CC,BC,SQ32,BT,AC,EPS0,HVFAC
      CHARACTER*64 :: message
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 24
c
      EPS0   = ZERO  ! not yet supported in cfg
      HVFAC  = ZERO  ! not yet supported in cfg
      ISRATE = 0     ! strain rate will be calculated for output even if not used
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
c--------------------------------------------------
c     Read DATA
c--------------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_E'        ,YMC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,ANUC    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Iflag'        ,ICAP    ,IS_AVAILABLE, LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SIGY'     ,FC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FtFc'     ,FT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FbFc'     ,FB      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_F2Fc'     ,F2D     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SoFc'     ,S0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_ETAN'     ,HT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DAMAGE'   ,DSUP1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'      ,EPSMAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_BETA'     ,VKY     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PPRES'    ,RT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YPRES'    ,RC      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BPMOD'    ,HBP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETC'      ,ETC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_DIL_Y'    ,ALI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DIL_F'    ,ALF     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_COMPAC'   ,VMAX    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_CAP_BEG'  ,ROK     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CAP_END'  ,RO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TPMOD'    ,HV0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS0'     ,EPS0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HVFAC'    ,HVFAC   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_E2'       ,YMS     ,IS_AVAILABLE, LSUBMODEL, UNITAB) !young modulus
      CALL HM_GET_FLOATV('MAT_SSIG'     ,Y0S     ,IS_AVAILABLE, LSUBMODEL, UNITAB) !yield strength
      CALL HM_GET_FLOATV('MAT_SETAN'    ,ETS     ,IS_AVAILABLE, LSUBMODEL, UNITAB) !tangent modulus
c
      CALL HM_GET_FLOATV('MAT_PDIR1'    ,ARM1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR2'    ,ARM2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR3'    ,ARM3    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------
c     DEFAULT VALUES
c--------------------------------------------------
      IF (RHOR == ZERO)  RHOR  = RHO0
c
      IF (FT == ZERO) FT = EM01
      IF (FB == ZERO) FB = SIX_OVER_5
      IF (S0 == ZERO) S0 = FIVE_OVER_4
      IF (HT >= ZERO) HT =-YMC
      IF (DSUP1 == ZERO)  DSUP1 = ZEP99999
      IF (VMAX >= ZERO)   VMAX  =-ZEP35
      IF (EPSMAX <= ZERO) EPSMAX= EP20
c      
      IF (VKY  ==  ZERO) VKY = HALF
      IF (RC   ==  ZERO) RC  =-THIRD*FC
      IF (HBP  ==  ZERO) THEN
        IF(ETC /= ZERO )THEN
          IF(ETC < YMC) THEN
            HBP = YMC*ETC/(YMC-ETC)
          ELSE
            CALL ANCMSG(MSGID=2065,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)
          ENDIF
        ELSEIF (ETC == ZERO )THEN
          ETC = (ONE-VKY)*YMC*FC/(TWOEM3*YMC-VKY*FC)
          IF(ETC < YMC) THEN
            HBP = YMC*ETC/(YMC-ETC)
          ELSE
            CALL ANCMSG(MSGID=2066,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)
          ENDIF
        ENDIF
      ENDIF
      BULK = YMC/THREE/(ONE-TWO*ANUC)
      IF (ROK == ZERO) ROK = RC
      IF (ICAP == 0 .AND. INVERS < 2017) ICAP = 1
      IF (ICAP == 2) THEN
        IF (F2D  ==  ZERO) F2D  = SEVEN
        IF (RO0  == ZERO)  RO0  =-TWO*FC
        IF (HV0  == ZERO)  HV0  = HALF*BULK
        IF (EPS0 == ZERO)  EPS0 = ZEP02
        IF (HVFAC== ZERO)  HVFAC= ZEP1
        EXPO = -ONE/EPS0
      ELSE
        IF (F2D  ==  ZERO) F2D = FOUR
        IF (RO0 == ZERO) RO0 =-FOUR_OVER_5*FC
        IF (HV0 == ZERO) HV0 = YMC/FIVE
        EXPO  = ONE/HV0/VMAX
        HVFAC = ZERO
      ENDIF
      !IF(ALI == ZERO) ALI = -ONE/FIVE
      !IF(ALF == ZERO) ALF = -ONE/TEN
      IF (ALI >= ZERO) THEN
        IF (ICAP == 1) THEN
          message='CAP REQUIRES ALPHAI PARAMETER  STRICTLY <0   ' 
        ELSE
          message='ALPHAI RESET TO DEFAULT -0.2 CAP REQUIRES ALPHAI <0' 
          ALI =- ONE/FIVE
        ENDIF 
        CALL ANCMSG(MSGID=1161,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR,
     .              C2=message)
      ENDIF
c
      IF (DSUP1 >= ONE .OR. DSUP1 < ZERO) THEN
         CALL ANCMSG(MSGID=605,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)
      END IF
C-----------------------
C     OTTOSEN SURFACE PARAMETERS
C-----------------------
C     Failure surface : f = r - rf = 0
C
C     Ottosen surface defined with :
C      -> rf = 1/AA (-BB+SQRT(BB  / B2 -AA(SM-CC))
C          where BB = BB(BC,BT) = 1/2[BC(1-CS3T) + BT(1+CS3T)]
C
C     AA,BC,BT,CC are deduced from material input :
C    
      F2D0 = F2D - S0
      AA = THREE_HALF* ( S0 /(F2D0 -ONE) - FT * FB /(FB-FT))/(F2D0 - FB*FT)
      CC = FB * FT * ( F2D0 /(FB-FT) - S0 /(F2D0-ONE))/(F2D0 - FB*FT)
      BC = HALF*SQR3_2 * (CC + THIRD - TWO*THIRD*AA)
      BT = HALF*SQR3_2 * (CC/FT - THIRD - TWO*THIRD*AA*FT)
      AC = CC*AA
      AA = AA/FC
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
c
      PM(20) = YMC
      PM(21) = ANUC
      PM(22) = YMC/(TWO*(ONE+ANUC))
      PM(25) = YMC*ANUC/(ONE+ANUC)/(ONE-TWO*ANUC)
      PM(24) = PM(25) + TWO*PM(22)
      PM(26) = MAX(ZERO,DSUP1)
      PM(27) = VMAX
      PM(28) = ONE-HT/YMC
C
      PM(29) = ROK
      PM(30) = RO0
C
      PM(32) = BULK
      PM(33) = FC
      PM(34) = RT
      PM(35) = RC
      PM(36) = RT*(TWO*RC-RT)
      PM(37) = (RC-RT)**2
      PM(38) = AA
      PM(39) = BC
      PM(40) = BT
      PM(41) = AC
      PM(42) = FT*FC/YMC
      PM(43) = HBP
      PM(44) = ALI
      PM(45) = ALF
      PM(46) = VKY
      PM(47) = EPSMAX
C
      PM(48) = HV0
      PM(49) = EXPO
C
      PM(50) = YMS
      PM(51) = Y0S
      PM(52) = ETS
      PM(53) = ARM1
      PM(54) = ARM2
      PM(55) = ARM3
      PM(56) = ONE      !                                                      
      PM(57) = ICAP+EM01
      PM(58) = ONE-HVFAC
      PM(59) = ZERO    ! 
      PM(105)= TWO*PM(22)/(BULK+FOUR_OVER_3*PM(22)) ! =(1-2*Nuc)/(1-Nuc)
c---------------------
      IPM(252)= 2  ! Formulation for solid element time step computation.
c---------------------
      NUPARAM = 0
      NFUNC   = 0
      NUVAR   = 0
c-----------------
c---- Definition des variables internes (stockage elementaire)
c-----------------
      MTAG%G_GAMA  = 6  
      MTAG%G_PLA = 1
      MTAG%L_PLA = 7       ! plastic strain tensor for output
c
      MTAG%L_SIGA  = 3     
      MTAG%L_EPSA  = 3     
      MTAG%L_DAM   = 3     
      MTAG%L_ANG   = 6     
      MTAG%L_SF    = 3     
      MTAG%L_VK    = 1     ! K0 
      MTAG%L_STRA  = 6     
      MTAG%L_CRAK  = 3     
      MTAG%L_DSUM  = 1     
      MTAG%L_ROB   = 1     
      MTAG%L_SIGC  = 6     
      MTAG%L_DGLO  = 6     
      MTAG%L_RK    = 1     ! K 
      MTAG%L_EPE   = 1     ! EPSP VOL  
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH") 
c            
c-----------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,24
      WRITE(IOUT,1000)

      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'MATERIAL LAW CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        IF (ICAP == 2) THEN
          WRITE(IOUT,1300)
          WRITE(IOUT,1400) YMC,ANUC,FC,FT,FB,F2D,S0,HT,DSUP1,EPSMAX
          WRITE(IOUT,1500) VKY,RT,RC,ROK,RO0,ETC,HBP,HV0,EPS0,HVFAC,
     .                     ALI,ALF,VMAX,YMS,Y0S,ETS,ARM1,ARM2,ARM3,
     .                     AA,BC,BT,CC 
        ELSE
          WRITE(IOUT,1301)
          WRITE(IOUT,1400) YMC,ANUC,FC,FT,FB,F2D,S0,HT,DSUP1,EPSMAX
          WRITE(IOUT,1600) VKY,RT,RC,ROK,RO0,ETC,HBP,HV0,ALI,ALF,VMAX,
     .                     YMS,Y0S,ETS,ARM1,ARM2,ARM3,
     .                     AA,BC,BT,CC 
        ENDIF
      ENDIF     
C-----------------
      RETURN
C-----------------
 1000 FORMAT
     & (5X,'     MATERIAL MODEL : CONCRETE            ',/,
     &  5X,'     BRITTLE ELASTIC PLASTIC MATRIX       ',/,
     &  5X,'     ELASTIC PLASTIC  REINFORCEMENT       ',/,
     &  5X,'------------------------------------------',/)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . =',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . =',1PG20.13/)  
 1300 FORMAT(
     & 5X,'      NEW CAP FORMULATION                  ',/) 
 1301 FORMAT(
     & 5X,'      ORIGINAL CAP FORMULATION             ',/) 
 1400 FORMAT(
     & 5X,'  1. MATRIX PROPERTIES                     ',/,
     & 5X,'YMC   YOUNG MODULUS . . . . . . . . . . . =',E12.4/,
     & 5X,'NUC   POISSON RATIO . . . . . . . . . . . =',E12.4/,
     & 5X,'FC    UNIAXIAL COMPRESSION STRENGTH . . . =',E12.4/,
     & 5X,'FT/FC TENSILE STRENGTH  . . . . . . . . . =',E12.4/,
     & 5X,'FB/FC BIAXIAL STRENGTH  . . . . . . . . . =',E12.4/,
     & 5X,'F2/FC CONFINED STRENGTH . . . . . . . . . =',E12.4/,
     & 5X,'S0/FC CONFINING STRESS. . . . . . . . . . =',E12.4/,
     & 5X,'HT<0  TENSILE TANGENT MUDULUS . . . . . . =',E12.4/,
     & 5X,'DSUP1 MAXIMUM DAMAGE  . . . . . . . . . . =',E12.4/,
     & 5X,'DEPSM TOTAL FAILURE CRACK STRAIN  . . . . =',E12.4)
 1500 FORMAT(
     & 5X,'KY    INITIAL HARDENING PARAMETER . . . . =',E12.4/,
     & 5X,'RT    FAILURE/PLASTIC TRANSITION PRES . . =',E12.4/,
     & 5X,'RC    PROPORTIONAL YIELD. . . . . . . . . =',E12.4/,
     & 5X,'ROK   BEGINNING OF CAPE . . . . . . . . . =',E12.4/,
     & 5X,'RO0   END OF INITIAL CAPE . . . . . . . . =',E12.4/,
     & 5X,'ETC   PLASTIC TANGENT MODULUS . . . . . . =',E12.4/,
     & 5X,'HBP   UNIAXIAL PLASTIC MODULUS  . . . . . =',E12.4/,
     & 5X,'HV0   INITIAL TRIAXIAL PLASTIC MODULUS. . =',E12.4/,
     & 5X,'EPS0  EPS0 FOR HPV HARDENING  . . . . . . =',E12.4/,
     & 5X,'HVFAC REDUCTION COEFFICIENT FOR HPV . . . =',E12.4/, 
     & 5X,'ALI   COMPACTANCY FACTOR AT YIELD   . . . =',E12.4/,
     & 5X,'ALF   DILATANCY FACTOR AT FAILURE . . . . =',E12.4/,
     & 5X,'VMAX  MAXIMAL COMPACTION VOLUME (<0). . . =',E12.4/,
     & 5X,'  2. REINFORCEMENT PROPERTIES              ',/,
     & 5X,'YMS   YOUNG MODULUS . . . . . . . . . . . =',E12.4/,
     & 5X,'Y0S   YIELD STRESS  . . . . . . . . . . . =',E12.4/,
     & 5X,'ETS   TANGENT MODULUS . . . . . . . . . . =',E12.4/,
     & 5X,'ARM1  PERCENTAGE OF REINFORCEMENT DIR 1 . =',E12.4/,
     & 5X,'ARM2  PERCENTAGE OF REINFORCEMENT DIR 2 . =',E12.4/,
     & 5X,'ARM3  PERCENTAGE OF REINFORCEMENT DIR 3 . =',E12.4/,
     & 5X,'PARAMETER A  FOR OTTOSEN SURFACE. . . . . =',E12.4/,
     & 5X,'PARAMETER BC FOR OTTOSEN SURFACE. . . . . =',E12.4/,    
     & 5X,'PARAMETER BT FOR OTTOSEN SURFACE. . . . . =',E12.4/,    
     & 5X,'PARAMETER C  FOR OTTOSEN SURFACE. . . . . =',E12.4)
 1600 FORMAT(
     & 5X,'KY    INITIAL HARDENING PARAMETER . . . . =',E12.4/,
     & 5X,'RT    FAILURE/PLASTIC TRANSITION PRES . . =',E12.4/,
     & 5X,'RC    PROPORTIONAL YIELD. . . . . . . . . =',E12.4/,
     & 5X,'ROK   BEGINNING OF CAPE . . . . . . . . . =',E12.4/,
     & 5X,'RO0   END OF INITIAL CAPE . . . . . . . . =',E12.4/,
     & 5X,'ETC   PLASTIC TANGENT MODULUS . . . . . . =',E12.4/,
     & 5X,'HBP   UNIAXIAL PLASTIC MODULUS  . . . . . =',E12.4/,
     & 5X,'HV0   INITIAL TRIAXIAL PLASTIC MODULUS. . =',E12.4/,
     & 5X,'ALI   DILATANCY FACTOR AT YIELD . . . . . =',E12.4/,
     & 5X,'ALF   DILATANCY FACTOR AT FAILURE . . . . =',E12.4/,
     & 5X,'VMAX  MAXIMAL COMPACTION VOLUME (<0). . . =',E12.4//,
     & 5X,'  2. REINFORCEMENT PROPERTIES              ',/,
     & 5X,'YMS   YOUNG MODULUS . . . . . . . . . . . =',E12.4/,
     & 5X,'Y0S   YIELD STRESS  . . . . . . . . . . . =',E12.4/,
     & 5X,'ETS   TANGENT MODULUS . . . . . . . . . . =',E12.4/,
     & 5X,'ARM1  PERCENTAGE OF REINFORCEMENT DIR 1 . =',E12.4/,
     & 5X,'ARM2  PERCENTAGE OF REINFORCEMENT DIR 2 . =',E12.4/,
     & 5X,'ARM3  PERCENTAGE OF REINFORCEMENT DIR 3 . =',E12.4/,
     & 5X,'PARAMETER A  FOR OTTOSEN SURFACE. . . . . =',E12.4/,
     & 5X,'PARAMETER BC FOR OTTOSEN SURFACE. . . . . =',E12.4/,    
     & 5X,'PARAMETER BT FOR OTTOSEN SURFACE. . . . . =',E12.4/,    
     & 5X,'PARAMETER C  FOR OTTOSEN SURFACE. . . . . =',E12.4)
C-----------------
      RETURN
      END
