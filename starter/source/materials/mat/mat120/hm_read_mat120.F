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
Chd|  HM_READ_MAT120                source/materials/mat/mat120/hm_read_mat120.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT120(MTAG     ,UPARAM   ,MAXUPARAM,MAXTABL  ,NUPARAM  ,
     .                          NUVAR    ,NVARTMP  ,NUMTABL  ,ITABLE   ,PARMAT   ,
     .                          MATPARAM ,PM       ,MAT_ID   ,TITR     ,ISRATE   ,
     .                          UNITAB   ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW120 WITH HM READER - toughened adhesive polymere joint material
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C-----------------------------------------------
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C----------------------------------------------- 
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)    :: UNITAB 
      INTEGER ,INTENT(IN   )          :: MAXUPARAM,MAXTABL,MAT_ID
      INTEGER ,INTENT(INOUT)            :: NUMTABL,NUPARAM,NUVAR,NVARTMP
      INTEGER ,DIMENSION(MAXTABL) ,INTENT(INOUT)  :: ITABLE
      my_real ,INTENT(INOUT)          :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)  :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,ILAW,YTAB_ID,ISRATE,IFORM,ITRX,IDAM,NDIM_YLD
      my_real :: RHO0,RHOR,E,NU,Y0,Q,B,H,G,BULK,C11,C12,EPSPMIN,EPSPMAX,SSP,
     .           FCUT,A1F,A2F,A1H,A2H,AS,CC,D1C,D2C,D1F,D2F,D_TRX,D_JC,EXP_N,
     .           XSCALE,YSCALE,YSCALE_UNIT,UNIT_L,XSCALE_UNIT,THICK
C=======================================================================
      ILAW = 120
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c----------------
c     Read input cards
c----------------
card1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
card2 Elasticity parameters and formulation flags
      CALL HM_GET_FLOATV('MAT_E'     ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IFORM' ,IFORM    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_ITRX'  ,ITRX     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_IDAM'  ,IDAM     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_THICK' ,THICK    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card3 yield hardening function table
      CALL HM_GET_INTV  ('MAT_TAB_ID',YTAB_ID  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Xscale',XSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Yscale',YSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card4 hardening function parameters
      CALL HM_GET_FLOATV('MAT_TAU'   ,Y0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Q'     ,Q        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'     ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_H'     ,H        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card5 yield and plastic flow parameters
      CALL HM_GET_FLOATV('MAT_AF1'   ,A1F      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_AF2'   ,A2F      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_AH1'   ,A1H      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_AH2'   ,A2H      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_AS'    ,AS       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card6 Johnson-Cook strain rate factor on yield
      CALL HM_GET_FLOATV('MAT_CC'    ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAM0'  ,EPSPMIN  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMF'  ,EPSPMAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card7 Damage model parameters
      CALL HM_GET_FLOATV('MAT_D1C'   ,D1C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D2C'   ,D2C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D1F'   ,D1F      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D2F'   ,D2F      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card8 Johnson-Cook strain rate dependency on damage
      CALL HM_GET_FLOATV('D_TRX'     ,D_TRX    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('D_JC'      ,D_JC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP'   ,EXP_N    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-------------------------------------------------------------------------------
      G    = E * HALF  / (ONE + NU)
      BULK = E * THIRD / (ONE - TWO*NU)
      C11  = E * (ONE-NU) /((ONE + NU)*(ONE - TWO*NU))
      C12  = C11 * NU/(ONE - NU)
      SSP  = SQRT((BULK + FOUR_OVER_3*G) / RHO0)  ! sound speed estimation
c-------------------
c     Default values
c-------------------
      ISRATE = 0
      FCUT   = ZERO
      IF (CC > ZERO) THEN
        FCUT = 1000.0D0 * FAC_T_WORK
      END IF
      IF (EXP_N == ZERO) EXP_N = ONE  
      IF (IFORM == 0) IFORM = 1  
      IF (ITRX  == 0) ITRX  = 2  
      IF (IDAM  == 0) IDAM  = 2  
      IF (XSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_Xscale' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        XSCALE = ONE * XSCALE_UNIT
      ENDIF
      IF (YSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_Yscale' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        YSCALE = ONE * YSCALE_UNIT
      ENDIF
      IF (THICK == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_THICK' ,UNIT_L ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        THICK = ONE * UNIT_L
      ENDIF
c-----------------
c     Johnson/Cookss fracture strain parameters check
      D1F = MAX(D1C, D1F)
      D2F = MAX(D2C, D2F)
c-----------------
      NUMTABL  = 1
      NUVAR    = 5
      NVARTMP  = 1
      NUPARAM  = 34
      IF (YTAB_ID > 0) THEN ! analytical yld parameters are not used with tabulated input
        Y0 = ZERO
        Q  = ZERO
        B  = ZERO
        H  = ZERO
        EPSPMIN = ZERO
        EPSPMAX = ZERO
      ENDIF
c-------------------
      UPARAM(1)  = E
      UPARAM(2)  = NU   
      UPARAM(3)  = G    
      UPARAM(4)  = BULK 
      UPARAM(5)  = Y0  
      UPARAM(6)  = Q
      UPARAM(7)  = B  
      UPARAM(8)  = H
      UPARAM(9)  = A1F 
      UPARAM(10) = A2F 
      UPARAM(11) = A1H   
      UPARAM(12) = A2H
      UPARAM(13) = AS 
c     Johnson/Cook failure parameters
      UPARAM(14) = D1C 
      UPARAM(15) = D2C 
      UPARAM(16) = D1F 
      UPARAM(17) = D2F 
      UPARAM(18) = D_TRX
      UPARAM(19) = D_JC
      UPARAM(20) = EXP_N 
c     Parameters: Johnson & Cook rate-dependency
      UPARAM(21) = CC    
      UPARAM(22) = EPSPMIN   
      UPARAM(23) = EPSPMAX   
      UPARAM(24) = FCUT   
c 
      UPARAM(25) = IFORM 
      UPARAM(26) = ITRX   
      UPARAM(27) = IDAM 
c
      UPARAM(28) = SSP   
      UPARAM(29) = YTAB_ID
      UPARAM(30) = ONE / XSCALE   
      UPARAM(31) = YSCALE            
      UPARAM(32) = C11            
      UPARAM(33) = C12            
c
      UPARAM(34) = THICK ! For connect element  (type43)
      ITABLE(1)  = YTAB_ID     ! Yield function table = f(epsp,epsdot,temp)
c------------------
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(27) = SSP
      PM(100)= BULK      
c--------------------------------
      PARMAT(1)  = BULK
      PARMAT(2)  = E
      PARMAT(3)  = NU
      PARMAT(4)  = ISRATE
      PARMAT(5)  = FCUT
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(BULK+FOUR_OVER_3*G) ! == (1-2*nu)/(1-nu)
c-----------------
c     Element buffer variable allocation
c-----------------
      MTAG%G_PLA    = 1
      MTAG%L_PLA    = 1
      MTAG%L_EPSD   = 1
      MTAG%G_EPSD   = 1
      MTAG%L_DMG    = 1
      MTAG%G_DMG    = 1
      MTAG%L_DMGSCL = 1

      CALL INIT_MAT_KEYWORD(MATPARAM ,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL" )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"HOOK")
c-----------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1100)

      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,E,NU,THICK,IFORM
        IF (YTAB_ID > 0) THEN
          WRITE(IOUT,1300) YTAB_ID,XSCALE,YSCALE
        ELSE
          WRITE(IOUT,1400) Q,B,H,Y0,CC,EPSPMIN,EPSPMAX
        END IF
        WRITE(IOUT,1500) A1F,A2F,A1H,A2H,AS,D1C,D2C,D1F,D2F,
     .                   D_TRX,D_JC,EXP_N,ITRX,IDAM
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : TOUGHENED ADHESIVE POLYMER',/,
     & 5X,'-------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/  
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'REFERENCE THICKNESS . . . . . . . . . .=',1PG20.13/    
     & 5X,'YIELD CRITERION FLAG  . . . . . . . . .=',I3)
 1300 FORMAT(
     & 5X,'YIELD FUNCTION TABLE IDENTIFIER . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRAIN RATE. . . . . .=',1PG20.13/
     & 5X,'SCALE FACTOR FOR YLD FUNCTION . . . . .=',1PG20.13)
 1400 FORMAT(
     & 5X,'NONLINEAR HARDENING PARAMETER . . . . .=',1PG20.13/
     & 5X,'EXPONENTIAL DECAY COEFFICIENT . . . . .=',1PG20.13/
     & 5X,'LINEAR HARDENING PARAMETER. . . . . . .=',1PG20.13/
     & 5X,'INITIAL YIELD STRESS. . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE COEFFICIENT . . . . . . . .=',1PG20.13/
     & 5X,'STATIC STRAIN RATE. . . . . . . . . . .=',1PG20.13/
     & 5X,'MAXIMAL STRAIN RATE . . . . . . . . . .=',1PG20.13)
 1500 FORMAT(
     & 5X,'YIELD FUNCTION PARAMETER A1F. . . . . .=',1PG20.13/
     & 5X,'YIELD FUNCTION PARAMETER A2F. . . . . .=',1PG20.13/
     & 5X,'YIELD FUNCTION PARAMETER A1H. . . . . .=',1PG20.13/
     & 5X,'YIELD FUNCTION PARAMETER A2H. . . . . .=',1PG20.13/
     & 5X,'PLASTIC FLOW   PARAMETER AS . . . . . .=',1PG20.13/
     & 5X,'FAILURE MODEL COEFFICIENT D1C . . . . .=',1PG20.13/
     & 5X,'FAILURE MODEL COEFFICIENT D2C . . . . .=',1PG20.13/
     & 5X,'FAILURE MODEL COEFFICIENT D1F . . . . .=',1PG20.13/
     & 5X,'FAILURE MODEL COEFFICIENT D2F . . . . .=',1PG20.13/
     & 5X,'DAMAGE COEFFICIENT D_TRX. . . . . . . .=',1PG20.13/
     & 5X,'DAMAGE COEFFICIENT D_JC . . . . . . . .=',1PG20.13/
     & 5X,'EXPONENT COEFFICIENT  . . . . . . . . .=',1PG20.13/
     & 5X,'ITRX: FAILURE DEPENDENCY ON TRIAXIALITY=',I3/
     & 5X,'STRAIN RATE DEFINITION FOR DAMAGE . . .=',I3/)
c-----------
      RETURN
      END
