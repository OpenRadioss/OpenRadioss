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
Chd|  HM_READ_MAT37                 source/materials/mat/mat037/hm_read_mat37.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT37(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,ID       ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,STIFINT  ,MAT_ID)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW37 WITH HM READER ( TO BE COMPLETED )
C
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
C
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
#include      "alemuscl.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: ISRATE,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      my_real,INTENT(INOUT)           :: STIFINT
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(NSUBMOD)
      INTEGER,INTENT(IN)              :: MAT_ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      my_real 
     .        C1,C2,P0,VIS,VISV,RHO10,RHO20,A1,PMIN,GAM,VIS2,PSHIFT,
     .        VISV2,FAC_M,FAC_L,FAC_T,FAC_C,RHO0,RHOR
      INTEGER I,ISOLVER,IRHO
      character*63 chain1      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      ISRATE = 0  
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      !================================= !
      ! Communicate LAW to ALEMUSC_MOD  !
      !=================================!
      IF (IALEMUSCl > 0) I_LAW = 37
      IF(TRIMAT==0)TRIMAT = -2 !allows to detect bimaterial law BIMAT/BIPHAS (used for /ANIM/BRIC/VFRAC)

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PSH'        ,PSHIFT ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      !line-submat-1
      CALL HM_GET_FLOATV('Lqud_Rho_l'     ,RHO10   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('C_l'            ,C1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ALPHA1'         ,A1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Nu_l'           ,VIS     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bulk_Ratio_l'   ,VISV    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      !line-submat-2
      CALL HM_GET_FLOATV('Lqud_Rho_g'     ,RHO20   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Lqud_Gamma_bulk',GAM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Lqud_P0'        ,P0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Nu_g'           ,VIS2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bulk_Ratio_g'   ,VISV2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      IRHO=0
      IF(GAM*C1==0)IRHO=INT(P0)
     
      !--------------------------------!
      !     INPUT CHECK                !
      !--------------------------------!      
      IF(A1<ZERO .OR. A1>ONE)THEN
        chain1 = 'INITIAL MASSIC FRACTION MUST BE BETWEEN 0 AND 1.              '        
        CALL ANCMSG(MSGID=65,                                               
     .              MSGTYPE=MSGERROR,                                    
     .              ANMODE=ANINFO,                                         
     .              I1=37,                                                 
     .              I2=ID,                                                 
     .              C1='ERROR',                                          
     .              C2=TITR,                                               
     .              C3=chain1)
       A1 = ONE        
      ENDIF
      
      IF(RHO10<ZERO .OR. RHO20<ZERO)THEN
        chain1 = 'INITIAL DENSITY CANNOT BE NEGATIVE.                           '        
        CALL ANCMSG(MSGID=65,                                               
     .              MSGTYPE=MSGERROR,                                    
     .              ANMODE=ANINFO,                                         
     .              I1=37,                                                 
     .              I2=ID,                                                 
     .              C1='ERROR',                                          
     .              C2=TITR,                                               
     .              C3=chain1) 
        IF(RHO10<ZERO)RHO10 = EM20
        IF(RHO20<ZERO)RHO20 = EM20       
      ENDIF      
           
      !--------------------------------!
      !     STORAGE                    !
      !--------------------------------!

      !===SOLVER VERSION
      !ISOLVER = 0 :default
      !ISOLVER = 1 : historical solver
      !ISOLVER = 2 : new solver (2018.0)
      IF(INVERS_INIT>=2017)THEN
        ISOLVER = NINT(RHOR)
        IF(ISOLVER<=0 .OR. ISOLVER>=3)THEN 
          ISOLVER = 1
        ENDIF
      ELSE
        ISOLVER = 1
      ENDIF  
      IF(INT22 > 0) ISOLVER = 2

      !===NEW PRESSURE SHIFT PARAM (2018.0)
      IF(INVERS_INIT>=2017)THEN
        IF(PSHIFT==ZERO)THEN 
          PSHIFT=-P0
        ELSE
          PSHIFT = - PSHIFT
        ENDIF
      ELSE
        PSHIFT = -P0
      ENDIF      
      ! PSH  = 0       : => PSH=-P0
      ! PSH  = -P0     : relative pressure P_liq = C1.mu,      P_gas = P0(1+mu)**gamma - P0
      ! PSH  = 1e-20   : total pressure    P_liq = C0+C1.mu,   P_gas = P0(1+mu)**gamma 

      !===UNITS
      !line-1 : RHO10,C1,A1,VIS,VISV
      !line-2 : RHO20,GAM,P0,VIS2,VISV2
      !PSHIFT = PSHIFT * FAC_C
      !RHO10  = RHO10 * FAC_M/(FAC_L**3)
      !RHO20  = RHO20 * FAC_M/(FAC_L**3)
      !C1     = C1 * FAC_C
      !P0     = P0 * FAC_C
      !VIS    = VIS   * FAC_L**2 / FAC_T
      !VISV   = VISV  * FAC_L**2 / FAC_T
      !VIS2   = VIS2  * FAC_L**2 / FAC_T
      !VISV2  = VISV2 * FAC_L**2 / FAC_T

      
      !===UPARAM STORAGE
      PMIN       = - P0
      RHO0       = RHO10 * A1 + (ONE-A1)*RHO20
      RHOR       = RHO0
      PM(01)     = RHO0
      PM(89)     = RHO0
      PM(91)     = RHO10

      UPARAM(1)  = VIS                    
      UPARAM(2)  = TWO*VIS               
      UPARAM(3)  = (VISV-UPARAM(2))*THIRD 
      UPARAM(4)  = C1                     
      C2         = - PMIN * GAM
      UPARAM(5)  = GAM
      UPARAM(6)  = C1/RHO10
      UPARAM(7)  = GAM * P0 / RHO20
      UPARAM(8)  = PMIN
      UPARAM(9)  = P0
      UPARAM(10) = A1
      UPARAM(11) = RHO10
      UPARAM(12) = RHO20
      UPARAM(13) = VIS2
      UPARAM(14) = TWO*VIS2
      UPARAM(15) = (VISV2-UPARAM(14))*THIRD
      UPARAM(16) = PSHIFT  !pressure formulation : total or relative to P0
      UPARAM(17) = ISOLVER

      NUPARAM    = 17
      IFUNC(1)   = IRHO
      NFUNC      = 1
      NUVAR      = 5
      STIFINT    = C1


      !--------------------------------!
      !     PRINTOUT                   !
      !--------------------------------!
      IF(GAM*C1>0.)THEN
        WRITE(IOUT,1011) TRIM(TITR),MAT_ID,37
        WRITE(IOUT,1000)

        IF(ISOLVER==2)WRITE(IOUT,1102)
        IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
        ELSE
          WRITE(IOUT,1012) RHO0
          WRITE(IOUT,1100)RHO10,C1,A1,VIS,VISV,
     .                    RHO20,GAM,P0,VIS2,VISV2,P0,-PSHIFT
        ENDIF
      ELSE
        WRITE(IOUT,1001)
        IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
        ELSE
          WRITE(IOUT,1101)RHO10,RHO20,IRHO
        END IF
      END IF

C----------------------
      RETURN
 1000 FORMAT(
     & 5X,'  BI-PHASE LIQUID-GAS LAW',/,
     & 5X,'  -----------------------',//)
 1011 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1012 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1001 FORMAT(
     & 5X,'  BOUNDARY FOR BI-PHASE LIQUID-GAS LAW',/,
     & 5X,'  -------------------------------------',//)
 1100 FORMAT(
     & 5X,'LIQUID REFERENCE DENSITY. . . . . . . .=',1PG20.13/
     & 5X,'LIQUID BULK STIFFNESS . . . . . . . . .=',1PG20.13/
     & 5X,'LIQUID MASS RATIO . . . . . . . . . . .=',1PG20.13/
     & 5X,'  (0. = FULL OF GAZ; 1. = FULL OF LIQUID)'/
     & 5X,'LIQUID SHEAR KINEMATIC VISCOSITY . . . =',1PG20.13/
     & 5X,'LIQUID BULK  KINEMATIC VISCOSITY . . . =',1PG20.13//
     & 5X,'GAS    REFERENCE DENSITY. . . . . . . .=',1PG20.13/
     & 5X,'GAS CONSTANT. . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'GAS REFERENCE PRESSURE. . . . . . . . .=',1PG20.13/
     & 5X,'GAS SHEAR KINEMATIC VISCOSITY . . . . .=',1PG20.13/
     & 5X,'GAS BULK  KINEMATIC VISCOSITY . . . . .=',1PG20.13//
     & 5X,'REFERENCE PRESSURE. . . . . . . . . . .=',1PG20.13/     
     & 5X,'PRESSURE SHIFT. . . . . . . . . . . . .=',1PG20.13//)
 1101 FORMAT(
     & 5X,'LIQUID REFERENCE DENSITY. . . . . . . .=',1PG20.13/
     & 5X,'GAS    REFERENCE DENSITY. . . . . . . .=',1PG20.13/
     & 5X,'SCALING FUNCTION FOR GAS DENSITY. . . .=',I10)
 1102 FORMAT(
     & 5X,'ITERATIVE NEWTON SOLVER '/)
      END
