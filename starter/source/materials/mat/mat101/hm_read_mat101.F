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
Chd|  HM_READ_MAT101                source/materials/mat/mat101/hm_read_mat101.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT101 (
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,IMATVIS  ,
     .           PM       ,LSUBMODEL,MAT_ID   ,TITR     ,MATPARAM )
C----------------------------------------------------------------
C  MODULES
C----------------------------------------------------------------     
      USE SUBMODEL_MOD 
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MATPARAM_DEF_MOD
C----------------------------------------------------------------
C  I M P L I C I T   T Y P E S 
C----------------------------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C----------------------------------------------------------------
C  D U M M Y   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM,MAXFUNC      
      INTEGER, INTENT(INOUT)                       :: NUPARAM,NUVAR,IMATVIS,
     .                                                IFUNC(MAXFUNC),NFUNC      
      my_real, DIMENSION(100),INTENT(INOUT)        :: PARMAT  
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB      
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM       
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     . RHO0,DR,DEREF,DE1,DPOISS,DVE1,DVE2,DEDOT_REF, 
     . GAMV_REF, ALPHAP, DQ, DV, DM, DC3, DC4, CALPHAK1, CALPHAK2, H0,
     . DES1_0, DC5, DC6, DC7, DC8, DC9, DC10, H1, DES2_0, DC11, DC12,
     . DC13, DC14, DC1, DC2, DLAMBDA, RHOA, CV, THETA0, BETA0, BTHETA0,
     . FACTOR, TEMP_OPT, THETAI, THETAG,STIFINT,
     . C1,C2,C3,C4,C5,MU,LM,D,BETA,FACPL, SCALE1, SCALE2
      CHARACTER  STRING*ncharfield
      CHARACTER KEY*nchartitle,KEYNET*ncharfield      
      INTEGER ILAW
C     
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=============================================
C=============================================   
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 101
c------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)
c------------------------------------------
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'     ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2
      CALL HM_GET_FLOATV('MAT_E'       ,DEREF    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_ALPHA1'  ,DE1      ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_NU'      ,DPOISS   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('VE1'         ,DVE1     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
card3
      CALL HM_GET_FLOATV('VE2'         ,DVE2       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('EPSILONref'  ,DEDOT_REF  ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('GAMMA0'      ,GAMV_REF   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('ALPHA_p'     ,ALPHAP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)       
card4      
      CALL HM_GET_FLOATV('DELTAH'      ,DQ         ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_VOL'     ,DV         ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_M'       ,DM         ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C3'      ,DC3        ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
card5      
      CALL HM_GET_FLOATV('MAT_C4'      ,DC4        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('ALPHAK1'     ,CALPHAK1   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('ALPHAK2'     ,CALPHAK2   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_HARD'    ,H0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)       
card6      
      CALL HM_GET_FLOATV('ZETA1i'      ,DES1_0     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C5'      ,DC5        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C6'      ,DC6        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C7'      ,DC7        ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
card7      
      CALL HM_GET_FLOATV('MAT_C8'      ,DC8        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C9'      ,DC9        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C_10'    ,DC10       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_HARD1'   ,H1         ,IS_AVAILABLE, LSUBMODEL, UNITAB)          
card8      
      CALL HM_GET_FLOATV('ZETA2i'      ,DES2_0     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C_11'    ,DC11       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C_12'    ,DC12       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C13'     ,DC13       ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
card10      
      CALL HM_GET_FLOATV('MAT_C14'     ,DC14       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C1'      ,DC1        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_C2'      ,DC2        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('LAMBDAL'     ,DLAMBDA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
card11      
      CALL HM_GET_FLOATV('MAT_RHO_REF' ,RHOA       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_CV_REF'  ,CV         ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MAT_TREF'    ,THETA0     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('ALPHA_TH'    ,BETA0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
card12
      CALL HM_GET_FLOATV('THETA_GLASS' ,THETAG     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('Omega'       ,FACTOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('THETA_FLAG'  ,TEMP_OPT   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('HEAT_T0'     ,THETAI     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      
      DR = 8.314 ! J/K/mol

      DR = DR * FAC_T_WORK* FAC_T_WORK / FAC_M_WORK /FAC_L_WORK / FAC_L_WORK
      NUPARAM = 42 
C----------------------------------------------- 
C     USER MATERIAL PARAMETERS DEFINITION   
C----------------------------------------------- 

      UPARAM(1) = DEREF
      UPARAM(2) =  DE1
      UPARAM(3) = DPOISS
      UPARAM(4) = DVE1
      UPARAM(5) = DVE2
      UPARAM(6) = DEDOT_REF
      UPARAM(7) = GAMV_REF
      UPARAM(8) = ALPHAP
      UPARAM(9) = DQ
      UPARAM(10) = DV
      UPARAM(11) = DM
      UPARAM(12) = DC3
      UPARAM(13) = DC4
      UPARAM(14) = CALPHAK1
      UPARAM(15) = CALPHAK2
      UPARAM(16) = H0
      UPARAM(17) = DES1_0
      UPARAM(18) = DC5
      UPARAM(19) = DC6
      UPARAM(20) = DC7
      UPARAM(21) = DC8
      UPARAM(22) = DC9
      UPARAM(23) = DC10
      UPARAM(24) = H1
      UPARAM(25) = DES2_0
      UPARAM(26) = DC11
      UPARAM(27) = DC12  
      UPARAM(28) = DC13
      UPARAM(29) = DC14
      UPARAM(30) = DC1
      UPARAM(31) = DC2
      UPARAM(32) = DLAMBDA
      UPARAM(33) = RHOA
      UPARAM(34) = CV
      UPARAM(35) = THETA0
      UPARAM(36) = BETA0
      !UPARAM(37) = BTHETA0
      UPARAM(38) = FACTOR
      UPARAM(39) = TEMP_OPT
      UPARAM(40) = THETAI
      UPARAM(41) = THETAG
      UPARAM(42) = DR
C------------------------------------------------- 
C     NUMBER OF USER ELEMENT VARIABLES AND CURVES  
C------------------------------------------------- 
C   
      NUVAR = 42 
      NFUNC = 0 
C used for interface (solid+shell) 
      STIFINT = DEREF*(1-DPOISS)/(1+DPOISS)/(1-2*DPOISS)
      PARMAT(1) = STIFINT/trois !RBULK
      PARMAT(2) = STIFINT
      PARMAT(3) = DPOISS
c
      ! PM table
      PM(1)   = RHO0
      PM(89)  = RHO0
      PM(100) = PARMAT(1)
      IMATVIS = 1
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
c-----------------------------------------------------------------------
      WRITE(IOUT,900) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)      
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1050) RHO0
        WRITE(IOUT,1100) DEREF, DE1, DPOISS, DVE1, DVE2, DEDOT_REF, 
     .  GAMV_REF, ALPHAP, DQ, DV, DM, DC3, DC4, CALPHAK1, CALPHAK2, H0,
     .  DES1_0, DC5, DC6, DC7, DC8, DC9, DC10, H1, DES2_0, DC11, DC12,
     .  DC13, DC14, DC1, DC2, DLAMBDA, RHOA, CV, THETA0, BETA0, 
     .  FACTOR, TEMP_OPT, THETAI, THETAG
      ENDIF
C
 900  FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1000 FORMAT(
     & 5X,'  MATERIAL MODEL : VISCOPLASTIC MODEL - BOUVARD  ',/,
     & 5X,'  ---------------------------------------------  ',/)
 1050 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/) 
 1100 FORMAT(
     & 5X,'DEREF . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DE1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DPOISS. . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DVE1. . . . . . . . . . . . . . . . . .=',1PG20.13/      
     & 5X,'DVE2. . . . . . . . . . . . . . . . . .=',1PG20.13/      
     & 5X,'DEDOT_REF . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'GAMV_REF. . . . . . . . . . . . . . . .=',1PG20.13/  
     & 5X,'ALPHAP. . . . . . . . . . . . . . . . .=',1PG20.13/  
     & 5X,'DQ. . . . . . . . . . . . . . . . . . .=',1PG20.13/       
     & 5X,'DV. . . . . . . . . . . . . . . . . . .=',1PG20.13/       
     & 5X,'DM. . . . . . . . . . . . . . . . . . .=',1PG20.13/       
     & 5X,'DC3 . . . . . . . . . . . . . . . . . .=',1PG20.13/      
     & 5X,'DC4 . . . . . . . . . . . . . . . . . .=',1PG20.13/      
     & 5X,'CALPHAK1. . . . . . . . . . . . . . . .=',1PG20.13/  
     & 5X,'CALPHAK2. . . . . . . . . . . . . . . .=',1PG20.13/  
     & 5X,'H0. . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DES1_0. . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DC5 . . . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'DC6 . . . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'DC7 . . . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'DC8 . . . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'DC9 . . . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'DC10. . . . . . . . . . . . . . . . . .=',1PG20.13/   
     & 5X,'H1. . . . . . . . . . . . . . . . . . .=',1PG20.13/   
     & 5X,'DES2_0. . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'DC11. . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'DC12. . . . . . . . . . . . . . . . . .=',1PG20.13/     
     & 5X,'DC13. . . . . . . . . . . . . . . . . .=',1PG20.13/     
     & 5X,'DC14. . . . . . . . . . . . . . . . . .=',1PG20.13/     
     & 5X,'DC1 . . . . . . . . . . . . . . . . . .=',1PG20.13/       
     & 5X,'DC2 . . . . . . . . . . . . . . . . . .=',1PG20.13/     
     & 5X,'DLAMBDA . . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'RHOA. . . . . . . . . . . . . . . . . .=',1PG20.13/     
     & 5X,'CV. . . . . . . . . . . . . . . . . . .=',1PG20.13/     
     & 5X,'THETA0. . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'BETA0 . . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'FACTOR. . . . . . . . . . . . . . . . .=',1PG20.13/    
     & 5X,'TEMP_OPT. . . . . . . . . . . . . . . .=',1PG20.13/ 
     & 5X,'THETAI. . . . . . . . . . . . . . . . .=',1PG20.13/   
     & 5X,'THETAG. . . . . . . . . . . . . . . . .=',1PG20.13/)
     
C
      RETURN
      END
