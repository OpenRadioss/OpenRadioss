Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_mat117           ../starter/source/materials/mat/mat117/hm_read_mat117.F
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
      SUBROUTINE HM_READ_MAT117(MTAG     ,UPARAM    ,MAXUPARAM,NUPARAM  ,PM       ,
     .                          PARMAT   ,NUVAR     ,MAXFUNC  ,NFUNC    ,IFUNC,
     .                          UNITAB   ,MAT_ID    ,TITR     ,LSUBMODEL,MATPARAM )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW117 WITH HM READER - connection material
C-----------------------------------------------
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
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER, INTENT(INOUT)          :: IFUNC(MAXFUNC), NFUNC, MAXFUNC
      INTEGER, INTENT(INOUT)          :: MAXUPARAM,
     .                                   NUPARAM,NUVAR
      INTEGER, INTENT(IN)             :: MAT_ID
      my_real, INTENT(INOUT)          :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)  :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ILAW,IMASS,IDEL,
     .    IRUPT,ICRITTYP,IEVOSHAP,ISYM
      my_real :: RHO0,FSCALEX,FSCALEX_UNIT,UDN,UDS,ALPHA,EXP_ALPHA,DENOM,
     .   E_ELAS_N,E_ELAS_S,TMAX_N,TMAX_S,COEF_N,COEF_T,COEF0,FUNC_ALPHA,
     .   GIC,GIIC,EXP_G,EXP_BK,DELTA0N,DELTA0S,UND,UTD,GAMA
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      ILAW = 117
!
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
!
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
Card1
      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IMASS'      ,IMASS        ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Fct_TN'     ,IFUNC(1)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Fct_TT'     ,IFUNC(2)     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Fscale_x'   ,FSCALEX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF(FSCALEX == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Fscale_x'  ,FSCALEX_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          FSCALEX = FSCALEX_UNIT
      ENDIF
      CALL HM_GET_FLOATV('MAT_E_ELAS_N'   ,E_ELAS_N     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E_ELAS_S'   ,E_ELAS_S     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX_N'     ,TMAX_N       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX_S'     ,TMAX_S       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('MAT_IRUPT'      ,IRUPT        ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_IDEL'       ,IDEL         ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_ISYM'       ,ISYM         ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_GIC'        ,GIC          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GIIC'       ,GIIC         ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_EXP_G'    ,EXP_G      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP_BK'   ,EXP_BK     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMMA'    ,GAMA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('MAT_CRIT_TYPE'    ,ICRITTYP   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_EVO_SHAPE'    ,IEVOSHAP  ,IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_FLOATV('MAT_DELTA_FN'    ,UDN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DELTA_FS'    ,UDS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA'       ,ALPHA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

c---------------------------------------------------------------------------------
      PM(1) = RHO0  ! RHOR
      PM(89)= RHO0
C-------------------------------------------
C Number of User Element Variables and Curves
C-------------------------------------------
      NUVAR = 15
      NFUNC = 2
      NUPARAM = 21
c-------------------
c     Default Values
c-------------------
      IF (IDEL  == 0)     IDEL     = 1
      IF (IMASS == 0)     IMASS    = 1
      IF (IRUPT == 0)     IRUPT    = 1
      IF (ICRITTYP == 0)   ICRITTYP  = 1
      IF (IEVOSHAP == 0)  IEVOSHAP = 1
      IF (GAMA  == ZERO)  GAMA   = ONE
      IF (ALPHA == ZERO)  ALPHA  = THREE
      IF (EXP_G  == ZERO) EXP_G   = TWO
      IF (E_ELAS_S == ZERO) E_ELAS_S = E_ELAS_N

      ! INIT PARM
      IF (IEVOSHAP == 3) THEN
          !----------------------------------------------------------
          ! Xu-Needleman exponential potential (IEVOSHAP = 3)
          ! Mandatory inputs: GIC > 0, GIIC > 0, TMAX_N > 0, TMAX_S > 0
          ! Derived: K0 = e^2 * Tn^2 / Gc,  delta_p = Gc / (e * Tn)
          ! User EN, ES, UDN, UDS, ALPHA are ignored
          !----------------------------------------------------------
          IF (GIC <= ZERO) THEN
              CALL ANCMSG(MSGID=3016,MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=MAT_ID, C1=TITR)
              GIC = ONE ! DUMMY VALUE
          ENDIF
          
          IF (GIIC <= ZERO) THEN
              CALL ANCMSG(MSGID=3017,MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=MAT_ID, C1=TITR)
              GIIC = ONE ! DUMMY VALUE 2
          ENDIF
          IF (TMAX_N <= ZERO) THEN
              CALL ANCMSG(MSGID=3017,MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=MAT_ID, C1=TITR) 
              TMAX_N = ONE
          ENDIF
          IF (TMAX_S <= ZERO) THEN
              CALL ANCMSG(MSGID=3017,MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=MAT_ID, C1=TITR) 
              TMAX_S = ONE
          ENDIF  
          
          ! Derive Xu-Needlman params
          E_ELAS_N = EXP(ONE)**2 * TMAX_N**2 / GIC
          E_ELAS_S = EXP(ONE)**2 * TMAX_S**2 / GIIC      
          
          DELTA0N  = GIC  / (EXP(ONE) * TMAX_N)
          DELTA0S  = GIIC / (EXP(ONE) * TMAX_S)
          
          UDN = -DELTA0N * LOG(EM04)
          UDS = -DELTA0S * LOG(EM04)
          
          ICRITTYP = 1
      ELSE ! IEVOSHAP = 1 or 2
          EXP_ALPHA  = EXP(-ALPHA)
          DENOM      = ONE - EXP_ALPHA
          FUNC_ALPHA = (ONE - (ONE + ALPHA) * EXP_ALPHA)/(ALPHA * DENOM)
          ! DFT VALUES for DELTA0
          DELTA0N  = TMAX_N/E_ELAS_N !single mode damage initiation normal
          DELTA0S  = TMAX_S/E_ELAS_S !single mode damage initiation tangential
          ! INIT CHECK FOR GIC and UDN
          IF (GIC == ZERO) THEN
              IF (UDN == ZERO) THEN
                  GIC = TMAX_N**2 / TWO / E_ELAS_N
                  UDN = DELTA0N
                  CALL ANCMSG(MSGID=3016,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                        I1 = MAT_ID,
     .                        C1 = TITR)
              ELSE
                  ! LINEAR Criterion
                  IF (IEVOSHAP == 1) THEN
                      GIC = HALF * UDN * TMAX_N
                  ! EXPO
                  ELSE
                      GIC = TMAX_N*DELTA0N*HALF + TMAX_N*(UDN - DELTA0N)*FUNC_ALPHA
                  ENDIF
              ENDIF
          ELSE
              IF ( GIC < (TMAX_N**2 / TWO/E_ELAS_N )  ) THEN
                  GIC = TMAX_N**2 / TWO/ E_ELAS_N
                  CALL ANCMSG(MSGID=3016,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                        I1 = MAT_ID,
     .                        C1 = TITR )
              ENDIF

              IF (UDN == ZERO) THEN
                  IF (IEVOSHAP == 1) THEN
                      UDN = TWO*GIC / (DELTA0N * E_ELAS_N)
                  ELSE
                      COEF0 = GIC - TMAX_N * DELTA0N * HALF
                      UDN = DELTA0N + COEF0 / (FUNC_ALPHA * TMAX_N)
                  ENDIF
              ENDIF
          ENDIF
          ! INIT CHECK FOR GIIC and UDS
          IF (GIIC == ZERO) THEN
              IF (UDS == ZERO) THEN
                  GIIC = TMAX_S**2 / TWO / E_ELAS_S
                  UDS  = DELTA0S
                  CALL ANCMSG(MSGID=3017,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                        I1 = MAT_ID,
     .                        C1 = TITR)
              ELSE
                  ! LINEAR Criterion
                  IF (IEVOSHAP == 1) THEN
                      GIIC = HALF * UDS * TMAX_S
                  ! EXPO
                  ELSE
                      GIIC = TMAX_S*DELTA0S*HALF + TMAX_S*(UDS - DELTA0S)*FUNC_ALPHA
                  ENDIF
              ENDIF
          ELSE
              IF ( GIIC < (TMAX_S**2 / TWO / E_ELAS_S )  ) THEN
                  GIIC =  TMAX_S**2 / TWO / E_ELAS_S
                  CALL ANCMSG(MSGID=3017,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                        I1 = MAT_ID,
     .                        C1 = TITR )
              ENDIF

              IF (UDS == ZERO) THEN
                  IF (IEVOSHAP == 1) THEN
                      UDS = TWO*GIIC / (DELTA0S * E_ELAS_S)
                  ELSE
                      COEF0 = GIIC - TMAX_S * DELTA0S * HALF
                      UDS = DELTA0S + COEF0 / (FUNC_ALPHA * TMAX_S)
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
      ! IF GIC is empty, init the GIC with elastic energy
      !IF ( GIC < (TMAX_N**2 / TWO/E_ELAS_N )  ) THEN
      !  GIC = TMAX_N**2 / TWO/E_ELAS_N
      !  CALL ANCMSG(MSGID=3016,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
      !.                I1 = MAT_ID,
      !.                C1 = TITR )
      !END IF
      !IF ( GIIC < (TMAX_S**2 / TWO/E_ELAS_S )  ) THEN
      !  GIIC = TMAX_S**2 / TWO/E_ELAS_S
      !  CALL ANCMSG(MSGID=3017,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
      !.                I1 = MAT_ID,
      !.                C1 = TITR )
      !END IF

      ! INITIATE DELTA_F depending on the DMG model (lin or exp)
      !IF (IEVOSHAP == 1) THEN
      !    UND = TWO*GIC / (DELTA0N * E_ELAS_N) ! ultimate displacement in normal direction (UND) => G = UND*TN*HALF
      !    UTD = TWO*GIIC / (DELTA0S * E_ELAS_S) ! ultimate displacement in tangential direction (UTD)
      !ELSE
      !    COEF_N = GIC - TMAX_N * DELTA0N * HALF
      !    COEF_T = GIIC - TMAX_S * DELTA0S * HALF
      !    COEF1 = ONE - (ONE + ALPHA) * EXP(-ALPHA)
      !    COEF2 = ALPHA * (ONE - EXP(-ALPHA))
      !
      !    UND = DELTA0N + (COEF_N * COEF2) / (COEF1 * TMAX_N)
      !    UTD = DELTA0S + (COEF_T * COEF2) / (COEF1 * TMAX_S)
      !END IF


C-------------------------------------------
C User Material Parameters Definition
C-------------------------------------------

      UPARAM(1)  = E_ELAS_N
      UPARAM(2)  = E_ELAS_S
      UPARAM(3)  = GAMA
      UPARAM(4)  = TMAX_N
      UPARAM(5)  = TMAX_S
      UPARAM(6)  = IRUPT
      UPARAM(7)  = GIC
      UPARAM(8)  = GIIC
      UPARAM(9)  = IDEL
      UPARAM(10) = EXP_G
      UPARAM(11) = EXP_BK
      UPARAM(12) = IMASS
      UPARAM(13) = DELTA0N        !         DISP puremode
      UPARAM(14) = DELTA0S        !         DISP puremode
      UPARAM(15) = UDN ! ultimate displacement in normal direction (UDN)
      UPARAM(16) = UDS ! ultimate displacement in tangential direction (UDS)
      UPARAM(17) = FSCALEX

      UPARAM(18) = ICRITTYP
      UPARAM(19) = IEVOSHAP
      UPARAM(20) = ALPHA
      UPARAM(21) = ISYM

C-------------------------------------------
C Interface Contact
C-------------------------------------------
C----------------
      PARMAT(1)  = MAX(E_ELAS_N,E_ELAS_S) / THREE
      PARMAT(2)  = MAX(E_ELAS_N,E_ELAS_S)
      PARMAT(17) = ONE ! (ONE - TWO*NU)/(ONE - NU), NU=0
c
      MTAG%L_EPE  = 3
      MTAG%L_DMG  = 1
      MTAG%G_DMG  = 1
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_COHESIVE")
c-------------------
c     STARTER Output
c-------------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,E_ELAS_N,E_ELAS_S,TMAX_N,TMAX_S,
     .                   IFUNC(1),IFUNC(2),FSCALEX,
     .                   IRUPT,GIC,GIIC,UDN,UDS,EXP_G,EXP_BK,GAMA,
     .                   ICRITTYP,IEVOSHAP,ALPHA,ISYM
      ENDIF
c-----------
      RETURN
C-------------------------------------------
 1000 FORMAT(
     & 10X,' MIXED MODE COHESIVE LAW 117  ',/,
     & 10X,' ---------------------------  ',/)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS NORMAL TO THE PLANE OF THE COHESIVE ELEMENT .=',1PG20.13/,
     & 5X,'STIFFNESS IN TO THE PLANE OF THE COHESIVE ELEMENT . . .=',1PG20.13/,
     & 5X,'PEAK TRACTION IN NORMAL DIRECTION . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PEAK TRACTION IN TANGENTIAL DIRECTION . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION OF THE PEAK TRACTION IN NORMAL DIRECTION. . . =',I10/,
     & 5X,'FUNCTION OF THE PEAK TRACTION IN TANGENTIAL DIRECTION .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR FOR THE PEAK TRACTION FUNCTIONS .=',1PG20.13/,
c
     & 5X,'CHOICE OF PROPAGATION CRITERION . . . . . . . . . . . .=',I10/,
     & 5X,'             = 1 => POWER LAW                   '/,
     & 5X,'             = 2 => BENZEGGAGH-KENANE           '/,
c
     & 5X,'ENERGY RELEASE RATE FOR MODE I. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ENERGY RELEASE RATE FOR MODE II . . . . . . . . . . . .=',1PG20.13/,
c
     & 5X,'MAXIMUM DISPLACEMENT (FAILURE) FOR MODE I . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM DISPLACEMENT (FAILURE) FOR MODE II. . . . . . .=',1PG20.13/,
c
     & 5X,'POWER LAW EXPONENT MU . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BENZEGGAGH-KENANE EXPONENT MU . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BENZEGGAGH-KENANE EXPONENT GAMMA. . . . . . . . . . . .=',1PG20.13/,
c
     & 5X,'CHOICE OF THE FAILURE CRITERION . . . . . . . . . . . .=',I10/,
     & 5X,'             = 1 => DISP-BASED (COHDISP)                   '/,
     & 5X,'             = 2 => ENERGY-BASED (COHENRG)                 '/,
c
     & 5X,'CHOICE OF THE DAMAGE EVOLUTION. . . . . . . . . . . . .=',I10/,
     & 5X,'             = 1 => LINEAR (LINEAR-LINEAR)                 '/,
     & 5X,'             = 2 => EXPONENTIAL (LINEAR-EXPONENTIAL)       '/,
     & 5X,'             = 3 => XU-NEEDLEMAN EXPONENTIAL (C-INFTY)     '/,
c
     & 5X,'EXPONENTIAL DAMAGE EXPONENT ALPHA . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SYMMETRIC TENSION/COMPRESSION FLAG . . . . . . . . . . =',I10/,
     & 5X,'             = 0:   ASYMMETRIC (ONLY TENSION DAMAGES)   '/,
     & 5X,'             = 1:   SYMMETRIC (TENSION = COMPRESSION)   '/)
c--------
      END
