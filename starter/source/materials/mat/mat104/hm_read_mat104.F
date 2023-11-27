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
Chd|  HM_READ_MAT104                source/materials/mat/mat104/hm_read_mat104.F
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
      SUBROUTINE HM_READ_MAT104(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .                          MAXFUNC  ,IFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .                          PM       ,TITR     ,MTAG     ,LSUBMODEL,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD 
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM,MAXFUNC
      INTEGER, INTENT(INOUT)                       :: NUPARAM,NUVAR,IFUNC(MAXFUNC),NFUNC
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB    
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(100),INTENT(INOUT)        :: PARMAT      
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER FLAGNICE,ILAW
      my_real 
     .   YOUNG,NU,NNU,NNU1,BULK,LAM,G,G2,A1,A2,C1,FCUT,ASRATE,CDR,
     .   QVOCE,BVOCE,JCC,EPSP0,MTEMP,TREF,ETA,CP,DPIS,DPAD,
     .   YLD0,HP,KDR,TINI,RHO0
C     
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 104
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'     ,RHO0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 - Elastic parameters - Flags
      CALL HM_GET_FLOATV('MAT_E'       ,YOUNG        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'      ,NU           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT104_Ires' ,FLAGNICE     ,IS_AVAILABLE, LSUBMODEL)
card3 - Hardening mdefied Voce
      CALL HM_GET_FLOATV('SIGMA_r'     ,YLD0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_H'    ,HP           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PR'      ,QVOCE        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_Bv'   ,BVOCE        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_Cdr'  ,CDR          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card4 - JC strain rate
      CALL HM_GET_FLOATV('MAT104_Cjc'  ,JCC          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_Eps0' ,EPSP0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_Fcut' ,FCUT         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card5 - thermal softening
      CALL HM_GET_FLOATV('MAT104_Tss'  ,MTEMP        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_Tref' ,TREF         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T_Initial'   ,TINI         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card6 - self-heating
      CALL HM_GET_FLOATV('MAT_ETA'      ,ETA        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SPHEAT'   ,CP         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_EpsIso',DPIS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT104_EpsAd' ,DPAD       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c---------------------------------------------------  
      ! Default values
      IF (YLD0 == ZERO) YLD0 = INFINITY
      ! Cutting frequency
      IF (FCUT == ZERO) THEN 
        FCUT = 10000.0D0*FAC_T_WORK
      ENDIF
      ASRATE = TWO*PI*FCUT
      ! If the inviscid plastic strain-rate is zero, no Johnson-Cook model
      IF(EPSP0 == ZERO) THEN 
        EPSP0 = ONE
        JCC   = ZERO
        ! Info message
        CALL ANCMSG(MSGID=1654,MSGTYPE=MSGINFO,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF
      ! Self - heating
      IF (DPIS == ZERO) DPIS = INFINITY
      IF (DPAD == ZERO) DPAD = TWO*INFINITY
      IF (DPIS > DPAD) THEN
        ! Error message
        CALL ANCMSG(MSGID=1655,MSGTYPE=MSGERROR,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF
      ! Nice return mapping by default
      IF (FLAGNICE == 0) FLAGNICE = 1
      IF (FLAGNICE > 2) THEN 
        CALL ANCMSG(MSGID=1731,MSGTYPE=MSGWARNING,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR,
     .    I2=FLAGNICE)
      ENDIF
c     FLAGNICE = 1 => Nice method (default)
c     FLAGNICE = 2 => Newton method
c--------------------------
      ! Elastic parameters
      G2 = YOUNG / (ONE + NU)
      G  = HALF * G2
      LAM  = G2 * NU /(ONE - TWO*NU)  
      BULK = THIRD * YOUNG / (ONE - NU*TWO)
      NNU  = NU  / (ONE - NU)
      NNU1 = ONE - NNU
      A1   = YOUNG / (ONE - NU*NU)
      A2   = A1 * NU
      C1   = YOUNG / THREE/(ONE - TWO*NU)
c
      ! Checking value of Drucker coeff to ensure the yield surface convexity
      IF (CDR > 2.25D0) THEN 
        CDR = 2.25D0
        ! Warning message
        CALL ANCMSG(MSGID=1651,MSGTYPE=MSGWARNING,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ELSEIF (CDR < -TWENTY7/EIGHT) THEN
        CDR = -TWENTY7/EIGHT   
        ! Warning message
        CALL ANCMSG(MSGID=1652,MSGTYPE=MSGWARNING,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF
c
      ! Value of the constant of Drucker criterion
      KDR = (ONE/TWENTY7) - CDR*(FOUR/TWENTY7/TWENTY7)
      KDR = KDR**(ONE/SIX)
      KDR = ONE/KDR
c--------------------------
      ! Storing parameters
      UPARAM(1)  = YOUNG    ! Young modulus
      UPARAM(2)  = BULK     ! Bulk modulus
      UPARAM(3)  = G        ! Shear modulus
      UPARAM(4)  = G2       ! 2*Shear modulus
      UPARAM(5)  = LAM      ! Lambda (Hook)
      UPARAM(6)  = NU       ! Poisson ratio
      UPARAM(7)  = NNU      
      UPARAM(8)  = NNU1
      UPARAM(9)  = A1
      UPARAM(10) = A2
      UPARAM(11) = FLAGNICE ! Choice of the return mapping algorithm
      UPARAM(12) = CDR      ! Drucker coefficient
      UPARAM(13) = KDR      ! Drucker K coefficient
      UPARAM(14) = TINI     ! Initial temperature
      UPARAM(15) = HP       ! Linear hardening parameter
      UPARAM(16) = YLD0     ! Initial yield stress
      UPARAM(17) = QVOCE    ! Voce parameter
      UPARAM(18) = BVOCE    ! Voce parameter
      UPARAM(19) = ONE       ! ALPHA    ! Swift / Voce interpolation coef
      UPARAM(20) = JCC      ! Johnson-Cook parameter
      UPARAM(21) = EPSP0    ! Johnson-Cook inviscid limit strain-rate
      UPARAM(22) = MTEMP    ! Thermal softening
      UPARAM(23) = TREF     ! Reference temperature
      UPARAM(24) = ETA      ! Taylor-Quinney coefficient
      UPARAM(25) = CP       ! Thermal mass capacity
      UPARAM(26) = DPIS     ! Isothermal plastic strain-rate
      UPARAM(27) = DPAD     ! Adiabatic plastic strain-rate
      UPARAM(28) = ASRATE   ! Plastic strain-rate filtering period
      UPARAM(29) = ZERO     ! Free
c
c---  reserved for Gurson damage parameters   
c
      UPARAM(30) = ZERO     ! Gurson switch flag: default = 0 => no damage model
                            !                             = 1 => local damage model
                            !                             = 2 => non local micromorph damage model
                            !                             = 3 => non local Peerling damage model
      UPARAM(31) = ZERO     ! reserved for Gurson 
      UPARAM(32) = ZERO     ! reserved for Gurson 
      UPARAM(33) = ZERO     ! reserved for Gurson 
      UPARAM(34) = ZERO     ! reserved for Gurson 
      UPARAM(35) = ZERO     ! reserved for Gurson 
      UPARAM(36) = ZERO     ! reserved for Gurson 
      UPARAM(37) = ZERO     ! reserved for Gurson 
      UPARAM(38) = ZERO     ! reserved for Gurson 
      UPARAM(39) = ZERO     ! reserved for Gurson 
      UPARAM(40) = ZERO     ! reserved for Gurson 
c---------------------------    
      NFUNC       = 0
      NUPARAM     = 40
      IF (FLAGNICE == 1) THEN 
        NUVAR = 1
      ELSE
        NUVAR = 0
      ENDIF
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%G_TEMP = 1
      MTAG%L_TEMP = 1
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
c---------------------------   
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"HOOK")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
c---------------------------    
      PARMAT(1) = C1
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
      PARMAT(4) = ZERO ! ISRATE
      PARMAT(5) = ZERO ! FCUT
c     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU)
c
      ! PM table
      PM(1)  = RHO0
      PM(89) = RHO0
c-----------------------------------------------------------------------
      WRITE(IOUT,900) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1050) RHO0
        WRITE(IOUT,1100) YOUNG,NU   ,CDR ,QVOCE,BVOCE,
     .                   YLD0, HP   ,JCC ,EPSP0,FCUT,
     .                   MTEMP,TREF ,TINI,ETA  ,CP, DPIS,DPAD,
     .                   FLAGNICE
      ENDIF
c-----------
      RETURN
c-----------------------------------------------------------------------
 900  FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1000 FORMAT
     &(5X,'MATERIAL MODEL : DRUCKER - VOCE - JOHNSON-COOK   ',/,
     & 5X,'-------------------------------------------------',/)
 1050 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . .=',1PG20.13/) 
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DRUCKER COEFFICIENT C . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD VOCE PARAMETER Q. . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD VOCE PARAMETER B  . . . . . . . . . . .=',1PG20.13/
!
     & 5X,'INITIAL YIELD STRESS YLD0 . . . . . . . . . .=',1PG20.13/
     & 5X,'LINEAR HARDENING PARAMETER. . . . . . . . . .=',1PG20.13/ 
     & 5X,'J-C STRAIN RATE COEFFICIENT C . . . . . . . .=',1PG20.13/
     & 5X,'J-C REFERENCE STRAIN RATE . . . . . . . . . .=',1PG20.13/
     & 5X,'PLASTIC STRAIN RATE CUTOFF FREQUENCY. . . . .=',1PG20.13/
!
     & 5X,'THERMAL SOFTENING SLOPE . . . . . . . . . . .=',1PG20.13/
     & 5X,'REFERENCE TEMPERATURE . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'TAYLOR-QUINNEY COEF . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SPECIFIC HEAT . . . . . . . . . . . . . . . .=',1PG20.13/
!
     & 5X,'ISOTHERMAL PLASTIC STRAIN RATE. . . . . . . .=',1PG20.13/
     & 5X,'ADIABATIC PLASTIC STRAIN RATE . . . . . . . .=',1PG20.13/
!   
     & 5X,'RETURN MAPPING ALGORITHM FLAG . . . . . . . .=',I3/
     & 5X,'  IRES=1  NICE EXPLICIT (DEFAULT)'/
     & 5X,'  IRES=2  NEWTON-ITERATION IMPLICIT (CUTTING PLANE)'/)
c-----------
      RETURN
      END
