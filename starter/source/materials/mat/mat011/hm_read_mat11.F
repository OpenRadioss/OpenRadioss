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
Chd|  HM_READ_MAT11                 source/materials/mat/mat011/hm_read_mat11.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT11(LSUBMODEL, MTAG, UNITAB, IPM, PM, 
     .     MAT_ID, TITR ,MATPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE ALE_MOD
      USE MATPARAM_DEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT)         :: MATPARAM
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
C   INITIALISATION MATERIAU 11 LOI SPECIALE CONDITIONS AUX LIMITES
C   ISENTHALPIQUES FLUIDES PARFAIT
C
C   ITYP = 0 - (GAS)   INLET USING STAGNATION POINT STATE
C   ITYP = 1 - (LIQUID)INLET USING STAGNATION POINT STATE
C   ITYP = 2 - GENERAL INLET/OUTLET APPLYING USER FUNCTIONS
C   ITYP = 3 - NON RFLECTING FRONTIER (NRF)
C  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFR, IFP , IFE , IFT, IFQ, ITYP, INOD, JTHE, ICOMP
      INTEGER ID
      my_real
     .   GAM, VCRT2, GAMRP, GAM1, P0, C1, SSP2, EF,
     .   RHO0, PSH, CARL, GAM2, E0, VCRT, RCRT, PCRT, SSP, 
     .   T0, DC, ALP0, RHOC,
     .   TSCAL, TREF,GAMA,RHOR,
     .   FAC_LENGTH_, FAC_TIME_
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      FAC_LENGTH_ = ONE
      FAC_TIME_   = ONE

C     Check input encryption
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C     Initial and reference density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (RHOR == ZERO) THEN
         RHOR = RHO0
      ENDIF
      PM(1) = RHOR
      PM(89) = RHO0 
      ALE%GLOBAL%IS_BOUNDARY_MATERIAL = .TRUE.
C    
      CARL     = ZERO
      T0       = ZERO
      GAM      = ZERO
      VCRT2    = ZERO
      GAMRP    = ZERO
      GAM1     = ZERO
      P0       = ZERO
      C1       = ZERO
      SSP      = ZERO
      SSP2     = ZERO
      EF       = ZERO 
      E0       = ZERO
      DC       = ZERO
      RHOC     = ZERO
      ALP0     = INFINITY
      IFR      = 0
      IFP      = 0
      IFE      = 0
      IFT      = 0
      IFQ      = 0
      ITYP     = 0
      INOD     = 0
      ICOMP    = 0

C     Formulation type 
      CALL HM_GET_INTV('Itype', ITYP, IS_AVAILABLE, LSUBMODEL)
C     
      CALL HM_GET_FLOATV('MAT_PSH', PSH, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SCALE', TSCAL, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('SCALE', FAC_TIME_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('h', FAC_LENGTH_, IS_AVAILABLE, LSUBMODEL, UNITAB)

C     Switch according to input type
      SELECT CASE(ITYP)
      CASE (0)
         CALL HM_GET_INTV('NODE1', INOD, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('GAMMA', GAM, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('K_cdi', DC, IS_AVAILABLE, LSUBMODEL, UNITAB)

         CALL HM_GET_INTV('FUN_A1', IFR, IS_AVAILABLE, LSUBMODEL)

         CALL HM_GET_INTV('FUN_A2', IFP, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('MAT_PScale', P0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CASE (1)
         CALL HM_GET_INTV('NODE1', INOD, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('MAT_C1', C1, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('K_cdi', DC, IS_AVAILABLE, LSUBMODEL, UNITAB)

         CALL HM_GET_INTV('FUN_A1', IFR, IS_AVAILABLE, LSUBMODEL)

         CALL HM_GET_INTV('FUN_A2', IFP, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('MAT_PScale', P0, IS_AVAILABLE, LSUBMODEL, UNITAB)
         
         CALL HM_GET_INTV('FUN_A6', IFE, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('MAT_E0', E0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CASE (2)
         CALL HM_GET_INTV('FUN_A1', IFR, IS_AVAILABLE, LSUBMODEL)

         CALL HM_GET_INTV('FUN_A2', IFP, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('MAT_PScale', P0, IS_AVAILABLE, LSUBMODEL, UNITAB)
         
         CALL HM_GET_INTV('FUN_A6', IFE, IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOATV('MAT_E0', E0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CASE (3)
         CALL HM_GET_FLOATV('MAT_C0', SSP, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('h', CARL, IS_AVAILABLE, LSUBMODEL, UNITAB)
         IF(CARL*SSP==ZERO)THEN
            CALL ANCMSG(MSGID=304,
     .           MSGTYPE=MSGERROR,
     .           ANMODE=ANINFO,
     .           I2=MAT_ID,
     .           C1=TITR)
         ENDIF
      CASE DEFAULT
         CALL ANCMSG(MSGID=1665, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .        I1=MAT_ID,
     .        I2 = ITYP,
     .        C1=TITR)
      END SELECT

      CALL HM_GET_INTV('Xt_fun', IFT, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Yt_fun', IFQ, IS_AVAILABLE, LSUBMODEL) 

C----------------------
C     DEFAULT VALUES
C----------------------
      IF(CARL==ZERO)CARL = EM01 * FAC_LENGTH_
      IF(T0==ZERO)  T0   = THREE100
      IF(TSCAL == ZERO) TSCAL = ONE * FAC_TIME_
      SELECT CASE(ITYP)
      CASE (0)
         GAM1 = ONE/(GAM - ONE)
         GAM2 = HALF/GAM
         GAMRP= GAM2*(GAM-ONE)
         E0   = GAM1*(P0+PSH)
         EF   = GAM1*(PSH)
         SSP2 = GAM*P0/RHO0
         VCRT2= TWO*GAM*(P0+PSH)/RHO0/(GAM + ONE)
         VCRT = SQRT(VCRT2)
         RCRT = (ONE-GAMRP*RHO0*VCRT2/(P0+PSH))**GAM1
         PCRT = (P0+PSH)*RCRT**GAM
         RCRT = RHO0*RCRT
      CASE (1)
         SSP2 = C1/RHO0
         GAM1 = ONE
      CASE (2)
         GAM1 = ONE
         SSP2 = SSP**2
      CASE (3)
         IF(CARL>ZERO)ALP0=HALF*SSP/CARL
         RHOC=RHO0*SSP
      CASE DEFAULT
C     Already exited at input reading
      END SELECT

C----------------------
C     PRINTOUT
C----------------------
      WRITE(IOUT, 800)TRIM(TITR),MAT_ID,11
      
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,900)
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT,1000)ITYP,PSH,TSCAL
         WRITE(IOUT, 850) RHO0,RHOR
         SELECT CASE(ITYP)
         CASE(0)
            WRITE(IOUT,1100)GAM,P0,E0,VCRT,RCRT,PCRT,INOD
            WRITE(IOUT,1350)EF,DC
            WRITE(IOUT,1300)IFR,IFP
         CASE(1)
            WRITE(IOUT,1200)C1,P0,E0,INOD
            WRITE(IOUT,1349)DC
            WRITE(IOUT,1301)IFR,IFP,IFE
         CASE(2)
            WRITE(IOUT,1400)P0,E0
            WRITE(IOUT,1302)IFR,IFP,IFE
         CASE(3)
            WRITE(IOUT,1353)SSP,CARL,INOD
         CASE DEFAULT
C     Already exited at input reading
         END SELECT
         IF(ITYP/=3)WRITE(IOUT,2001) IFT,IFQ
      ENDIF
     
C----------------------
C     STORAGE
C----------------------
      IPM(11) = IFR
      IPM(12) = IFP
      IPM(13) = IFE
      IPM(14) = IFT
      IPM(17) = IFQ
      
      PM(23)  = E0
      PM(25)  = GAM
      PM(27)  = VCRT2
      PM(28)  = GAMRP
      PM(29)  = GAM1
      PM(31)  = P0
      PM(32)  = C1
      PM(34)  = -HALF*SSP2/CARL**2
      PM(37)  = EF
      PM(40)  = ONE/TSCAL
      PM(50)  = ITYP
      PM(51)  = INOD ! USR2SYS APRES FSDCOD, IF(IFORM8==2) INOD=USR2SYS(INOD,ITABM1,MESS)
      PM(79)  = T0
      PM(80)  = EP30
      PM(88)  = PSH
      PM(97)  = RHOC
      PM(98)  = ALP0
      PM(99)  = DC+ONE

C----------------------
C     MATERIAL BUFFER
C----------------------
      MTAG%G_RK    = 1   
      MTAG%G_RE    = 1   
      MTAG%L_RK    = 1   ! NB10    
      MTAG%L_TEMP  = 1   ! NB11 
      MTAG%L_RE    = 1   ! NB12   
      MTAG%L_VK    = 1   ! NB13   
      MTAG%L_EINS  = 1   ! NB14  
      
      ! EOS/Thermo keyword
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
C--------------------------------
      RETURN
      
C----------------------
C     FORMAT
C----------------------
  800 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
  850 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)
  900 FORMAT(
     & 5X,40H  LAW FOR FLUID BOUNDARY ELEMENTS       ,/,
     & 5X,40H  -------------------------------       ,/)
 1000 FORMAT(
     & 5X,40H  LAW FOR FLUID BOUNDARY ELEMENTS       ,/,
     & 5X,40H  -------------------------------       ,/,
     & 5X,40HITYP. . . . . . . . . . . . . . . . . .=,I10/,
     & 5X,'ITYP = 0 : GAS INLET',/,
     & 5X,'ITYP = 1 : LIQUID INLET',/,
     & 5X,'ITYP = 2 : IMPOSED INLET/OUTLET - TIME DEPENDENT',/,
     & 5X,'ITYP = 3 : NON-REFLECTING BOUNDARY',/,
     & 5X,'PSH: PRESSURE SHIFT . . . . . . . . . .=',1PG20.13/,
     & 5X,'TIME SCALE FACTOR . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT(
     & 5X,40HGAMMA CONSTANT. . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSTAGNATION PRESSURE . . . . . . . . . .=,E12.4/,
     & 5X,40HSTAGNATION ENERGY . . . . . . . . . . .=,E12.4/,
     & 5X,40HCRITICAL VELOCITY . . . . . . . . . . .=,E12.4/,
     & 5X,40HCRITICAL DENSITY. . . . . . . . . . . .=,E12.4/,
     & 5X,40HCRITICAL PRESSURE . . . . . . . . . . .=,E12.4/,
     & 5X,40HREFERENCE NODE (VELOCITY) . . . . . . .=,I10/)
 1200 FORMAT(
     & 5X,40HBULK MODULUS. . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSTAGNATION PRESSURE . . . . . . . . . .=,E12.4/,
     & 5X,40HSTAGNATION ENERGY . . . . . . . . . . .=,E12.4/,
     & 5X,40HREFERENCE NODE (VELOCITY) . . . . . . .=,I10/)
 1300 FORMAT(
     & 5X,40HSTAGNATION DENSITY  LOAD CURVE. . . . .=,I10/,
     & 5X,40HSTAGNATION PRESSURE LOAD CURVE. . . . .=,I10/)
 1301 FORMAT(
     & 5X,40HSTAGNATION DENSITY  LOAD CURVE. . . . .=,I10/,
     & 5X,40HSTAGNATION PRESSURE LOAD CURVE. . . . .=,I10/,
     & 5X,40HSTAGNATION ENERGY   LOAD CURVE. . . . .=,I10/)
 1302 FORMAT(
     & 5X,40HDENSITY    LOAD CURVE . . . . . . . . .=,I10/,
     & 5X,40HPRESSURE   LOAD CURVE . . . . . . . . .=,I10/,
     & 5X,40HENERGY     LOAD CURVE . . . . . . . . .=,I10/)
 1349 FORMAT(
     & 5X,40HDISCHARGE COEFFICIENT (ENTRY LOSS). . .=,E12.4/)
 1350 FORMAT(
     & 5X,40HFINAL STAGNATION ENERGY . . . . . . . .=,E12.4/,
     & 5X,40HDISCHARGE COEFFICIENT (ENTRY LOSS). . .=,E12.4/)
 1353 FORMAT(
     & 5X,40HCHARACTERISTIC SOUND SPEED. . . . . . .=,E12.4/,
     & 5X,40HCHARACTERISTIC LENGTH . . . . . . . . .=,E12.4/,
     & 5X,40HREFERENCE NODE (VELOCITY) . . . . . . .=,I10/)
 1400 FORMAT(
     & 5X,40HINITIAL PRESSURE(P-PSH) . . . . . . . .=,E12.4/,
     & 5X,40HINITIAL ENERGY. . . . . . . . . . . . .=,E12.4/)
 2001 FORMAT(
     & 5X,'    THERMAL BOUNDARY  ',/,
     & 5X,'    ----------------  ',/,
     & 5X,'TEMPERATURE LOAD CURVE. . . . . . . . .=',I10/,
     & 5X,'FLUX LOAD CURVE. . .  . . . . . . . . .=',I10/)
C-----------
      RETURN
      END
