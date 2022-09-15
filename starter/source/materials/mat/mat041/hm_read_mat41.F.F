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
Chd|  HM_READ_MAT41                 source/materials/mat/mat041/hm_read_mat41.F.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT41(UPARAM ,MAXUPARAM,NUPARAM,
     .                  NUVAR,IFUNC,MAXFUNC,NFUNC    ,STIFINT,
     .                  ID   ,TITR ,UNITAB, LSUBMODEL, PM    ,
     .                  MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This subroutine read the user material parameters.
C     The material cards that are common for all materials
C     (card 1 to 7 in version 2.2) have previously been read.
C     The NUPARAM material datas have to bee stored in UPARAM array.
C     If some standard radioss functions (time function or 
C     x,y function) are needed, this NFUNC function numbers have to 
C     bee stored in IFUNC array.
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C UPARAM   | NUPARAM | F | W | USER MATERIAL PARAMETER ARRAY
CMAXNUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXNUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXNFUNC |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
C STIFINT  |  1      | F | W | STIFNESS MODULUS FOR INTERFACE
C----------+---------+---+---+--------------------------------------------
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
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
#include      "submod_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      character*40 nom
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER MAXUPARAM,NUPARAM,NUVAR,MAXFUNC,NFUNC,
     .        IFUNC(MAXFUNC),IUNIT
      my_real   UPARAM(MAXUPARAM),STIFINT

      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      my_real, DIMENSION(NPROPM), INTENT(INOUT) :: PM     
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s 
C-----------------------------------------------
      INTEGER ITER,IREAC,I
      my_real
     .     AR, BR, R1R, R2R, R3R, WR, AP, BP, R1P, R2P, R3P, WP, CVR, CVP,
     .     ENQ, EPSILON, FC, RKI, EX, RI, RKG, YG, ZG, CAPPA, CHI, TOL,
     .     CCRIT, GROW2, EX1, EX2, YG2, ZG2, FMXIG, FMXGR, FMNGR, SHR, T,
     .     RHO0, RHOR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
!     Check input encryption
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      NUPARAM = 40  
             
!     Initial and reference density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (RHOR == ZERO) THEN
         RHOR = RHO0
      ENDIF
      PM(1) = RHOR
      PM(89) = RHO0 
      
      CALL HM_GET_INTV('Ireac', IREAC, IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_FLOATV('a_r', AR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('b_r', BR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('r_1r', R1R, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('r_2r', R2R, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('r_3r', R3R, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('a_p', AP, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('b_p', BP, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('r_1p', R1P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('r_2p', R2P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('r_3p', R3P, IS_AVAILABLE, LSUBMODEL, UNITAB)
                             
      CALL HM_GET_FLOATV('C_vr', CVR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('C_vp', CVP, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('enq', ENQ, IS_AVAILABLE, LSUBMODEL, UNITAB)
    
      CALL HM_GET_INTV('NITRS', ITER, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Epsilon_0', EPSILON, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('check', FC, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                  
      CALL HM_GET_FLOATV('rki', RKI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ex', EX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ri', RI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('rkg', RKG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('yg', YG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('zg', ZG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ex1', EX1, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                     
      CALL HM_GET_FLOATV('Kn', CAPPA, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('chi', CHI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Tol', TOL, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('grow2', GROW2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ex2', EX2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('yg2', YG2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('zg2', ZG2, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                   
      CALL HM_GET_FLOATV('ccrit', CCRIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('fmxig', FMXIG, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('fmxgr', FMXGR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('fmngr', FMNGR, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_G0', SHR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T_Initial', T, IS_AVAILABLE, LSUBMODEL, UNITAB) 


      !------------------------!
      !       DEFAULTS         !
      !------------------------!
      !IREAC=1 : LEE-TARVER ORIGINAL METHOD                                  
      !IREAC=2 : MODIFIED METHOD                                             
      IF(IREAC/=1 .AND. IREAC /=2)IREAC=1                                    
      WR = R3R/CVR                                                           
      WP = R3P/CVP                                                           
      IF (EPSILON==ZERO) EPSILON = EM3                                     
      IF (ITER==0) ITER = 80                                               
      IF (FC==ZERO) FC = EM5                                               
      IF (CAPPA==ZERO) CAPPA = EIGHTY19                               
      IF (CHI==ZERO) CHI = EIGHTY19                                   
      NFUNC = 0                                                              
      NUVAR = 8                                                              
      STIFINT = SHR          

      !------------------------!
      !       STORAGE          !
      !------------------------!
      UPARAM(1) = IREAC                                                      
      UPARAM(2) = AR                                                         
      UPARAM(3) = BR                                                         
      UPARAM(4) = R1R                                                        
      UPARAM(5) = R2R                                                        
      UPARAM(6) = R3R                                                        
      UPARAM(7) = WR                                                         
      UPARAM(8) = AP                                                         
      UPARAM(9) = BP                                                         
      UPARAM(10) = R1P                                                       
      UPARAM(11) = R2P                                                       
      UPARAM(12) = R3P                                                       
      UPARAM(13) = WP                                                        
      UPARAM(14) = CVR                                                       
      UPARAM(15) = CVP                                                       
      UPARAM(16) = ENQ                                                       
      UPARAM(17) = EPSILON                                                   
      UPARAM(18) = ITER                                                      
      UPARAM(19) = FC           
      UPARAM(20) = rki                                                       
      UPARAM(21) = ex                                                        
      UPARAM(22) = ri                                                        
      UPARAM(23) = RKG                                                       
      UPARAM(24) = YG                                                        
      UPARAM(25) = ZG                                                        
      UPARAM(31) = EX1                                                       
      UPARAM(26) = CAPPA                                                     
      UPARAM(27) = CHI                                                       
      UPARAM(28) = TOL
      UPARAM(32) = EX2                                                       
      UPARAM(33) = YG2                                                       
      UPARAM(34) = ZG2                                                       
      UPARAM(30) = GROW2
      UPARAM(29) = CCRIT                                                     
      UPARAM(35) = FMXIG                                                     
      UPARAM(36) = FMXGR                                                     
      UPARAM(37) = FMNGR 
      UPARAM(38) = SHR                                                       
      UPARAM(39) = T                                                         
      UPARAM(40) = ZERO                                                            
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c-----------------
      !------------------------!
      !    LISTING OUTPUT      !
      !------------------------!
      IF(IS_ENCRYPTED)THEN                                                    
        WRITE(IOUT,7000)                                                     
      ELSE                                                                   
        WRITE(IOUT,1000)UPARAM(1),  UPARAM(2),  UPARAM(3),                    
     .     UPARAM(4),   UPARAM(5),  UPARAM(6),  UPARAM(7),                       
     .     UPARAM(8),   UPARAM(9),  UPARAM(10), UPARAM(11),                     
     .     UPARAM(12),  UPARAM(13), UPARAM(14), UPARAM(15),                   
     .     UPARAM(16),  UPARAM(17), UPARAM(18), UPARAM(19),                   
     .     UPARAM(20),  UPARAM(21), UPARAM(22), UPARAM(23),                   
     .     UPARAM(24),  UPARAM(25), UPARAM(26), UPARAM(27),                   
     .     UPARAM(28),  UPARAM(29), UPARAM(31), UPARAM(30),                   
     .     UPARAM(32),  UPARAM(33), UPARAM(34), UPARAM(35),                   
     .     UPARAM(36),  UPARAM(37), UPARAM(38), UPARAM(39)                    
      ENDIF                                                                  
      
 7000 FORMAT(
     & 5X,'  LEE TARVER REACTIVE EXPLOSIVE         ',/,
     & 5X,'  -----------------------------         ',/,
     & 5X,  'CONFIDENTIAL DATA'//) 
 1000 FORMAT(
     & 5X,'  LEE TARVER REACTIVE EXPLOSIVE         ',/,
     & 5X,'  -----------------------------         ',/,
     & 5X,'REAC(1: LEE-TARVER 2:DYNA). . . . . . . =',1PG20.13/,
     & 5X,'  REACTIVES JWL EQUATION OF STATES :    ',/,
     & 5X,'AR COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BR COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'R1R COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'R2R COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'R3R COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'WR COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  PRODUCTS JWL EQUATION OF STATES :    ',/,
     & 5X,'AP COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BP COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'R1P COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'R2P COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'R3P COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'WP COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & /,
     & 5X,'CVR REACTIVE SPECIFIC HEAT. . . . . . . =',1PG20.13/,
     & 5X,'CVP PRODUCTS SPECIFIC HEAT. . . . . . . =',1PG20.13/,
     & 5X,'ENQ REACTION ENERGY . . . . . . . . . . =',1PG20.13/,
     & /,
     & 5X,'EPSILON . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'MAXIMUM NUMBER OF ITERATIONS. . . . . . =',1PG20.13/,
     & 5X,'FC CHECK. . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  KINETICAL PARAMETERS                   :    ',/,
     & 5X,'  IGNITION PHASE :                        ',/,
     & 5X,'RKI COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'EX COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'RI COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  GROWTH PHASE (LEE-TARVER):             ',/,
     & 5X,'RKG COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'YG COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'ZG COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  NUMERICAL LIMITORS                       ',/,
     & 5X,'CAPPA . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'CHI . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'TOL . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'CCRIT . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  GROWTH PHASE  (DYNA-2D MODEL) :        ',/,
     & 5X,'EX1 COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'GROW2 . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'EX2 COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'YG2 COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'ZG2 COEFFICIENT . . . . . . . . . . . . =',1PG20.13/,
     & /,
     & 5X,'FMXIG . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'FMXGR . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'FMNGR . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & /,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE (K) . . . . . . . . =',1PG20.13//)

C-----------------------------------------------
      RETURN
      END
