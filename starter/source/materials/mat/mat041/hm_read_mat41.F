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
Chd|  HM_READ_MAT41                 source/materials/mat/mat041/hm_read_mat41.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT41(UPARAM  ,MAXUPARAM ,NUPARAM ,
     .                        NUVAR    ,IFUNC     ,MAXFUNC ,NFUNC     ,STIFINT,
     .                        ID       ,TITR      ,UNITAB  ,LSUBMODEL ,PM    ,
     .                        MATPARAM ,MTAG)
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
C   ________________________________
C   IREAC FLAG
C
C   *** IREAC = 1 ***
C   ORIGINAL 2-TERM-MODEL   --> dF/dT = IGNITION TERM + GROWTH TERM
C     IGNITION TERM : ratei = I * (1.-F)**b * (etar-1.)**x     , if P>0
C     GROWTH TERM   : rateg = G1 * (1.-F)**b * F**d * pres**y  , if P>0
C
C   *** IREAC = 2 ***
C   EXTENDED 3-TERM-MODEL   --> dF/dT = IGNITION TERM + GROWTH TERM 1 + GROWTH TERM 2
C        IGNITION TERM :  ratei = I * (1.-F+tol)**b * abs(etar-1.-ccrit)**x     , if 0<F<Figmax & P>0 & compression thresold (etar-1>=ccrit)
C        GROWTH TERM 1 : rateg1 = G1 * (1.-F+tol)**c * (F+tol)**d * pres**y     , if 0<F<FG1max & P>0
C        GROWTH TERM 2 : rateg2 = G2 * (1.-F+tol)**e * (F+tol)**g * pres**z     , if FG2min<F<1 & P>0
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE ELBUFTAG_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER,INTENT(IN) :: MAXUPARAM,MAXFUNC,IFUNC(MAXFUNC)
      INTEGER,INTENT(INOUT) :: NUPARAM,NFUNC,NUVAR
      my_real,INTENT(INOUT) :: UPARAM(MAXUPARAM),STIFINT
      INTEGER,INTENT(IN) :: ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      my_real, INTENT(INOUT) :: PM(NPROPM)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s 
C-----------------------------------------------
      INTEGER ITER,IREAC
      my_real AR, BR, R1R, R2R, R3R, WR,
     .        AP, BP, R1P, R2P, R3P, WP,
     .        CVR, CVP,
     .        ENQ, EPSILON, FTOL, I_, B_, X_, G1, D_, Y_, CAPPA, CHI, TOL,
     .        CCRIT, G2, C_, E_, G_, Z_, Figmax, FG1max, FG2min, SHR, T,
     .        RHO0, RHOR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      NUPARAM = 40

!     Automatic Allocation for Elem Buffer : Burn Fraction
      MTAG%G_BFRAC  = 1
      MTAG%L_BFRAC  = 1
      MTAG%G_TEMP  = 1
      MTAG%L_TEMP  = 1
             
!     Initial and reference density
      CALL HM_GET_FLOATV('MAT_RHO', RHO0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHOR, IS_AVAILABLE, LSUBMODEL, UNITAB)

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
      CALL HM_GET_FLOATV('ftol', FTOL, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                  
      CALL HM_GET_FLOATV('I_', I_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('b_', B_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('x_', X_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('g1', G1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('d_', D_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('y_', Y_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('c_', C_, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                     
      CALL HM_GET_FLOATV('Kn', CAPPA, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('chi', CHI, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Tol', TOL, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('g2', G2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('e_', E_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('g_', G_, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('z_', Z_, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                   
      CALL HM_GET_FLOATV('ccrit', CCRIT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('figmax', Figmax, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('fg1max', FG1max, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('fg2min', FG2min, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_G0', SHR, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('T_Initial', T, IS_AVAILABLE, LSUBMODEL, UNITAB) 


      !------------------------!
      !       DEFAULTS         !
      !------------------------!
      !IREAC=1 : ORIGINAL 2 TERM MODEL (ignition + growth)
      !IREAC=2 : EXTENDED MODEL (3 TERM : ignition, growth1, growth2)
      IF(IREAC/=1 .AND. IREAC /=2)IREAC=1                                    
      WR = R3R/CVR   !SI : both units are J/m3/K (=Pa/K) ; WR is dimensionless (JWL EoS param)
      WP = R3P/CVP   !SI : both units are J/m3/K (=Pa/K) ; WR is dimensionless (JWL EoS param)
      IF (EPSILON==ZERO) EPSILON = EM3
      IF (ITER==0) ITER = 80
      IF (FTOL==ZERO) FTOL = EM5
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
      UPARAM(19) = FTOL
      UPARAM(20) = I_                                                       
      UPARAM(21) = B_                                                        
      UPARAM(22) = X_                                                        
      UPARAM(23) = G1                                                       
      UPARAM(24) = D_                                                        
      UPARAM(25) = Y_                                                        
      UPARAM(31) = C_                                                       
      UPARAM(26) = CAPPA                                                     
      UPARAM(27) = CHI                                                       
      UPARAM(28) = TOL
      UPARAM(32) = E_                                                       
      UPARAM(33) = G_                                                       
      UPARAM(34) = Z_                                                       
      UPARAM(30) = G2
      UPARAM(29) = CCRIT                                                     
      UPARAM(35) = Figmax                                                     
      UPARAM(36) = FG1max                                                     
      UPARAM(37) = FG2min 
      UPARAM(38) = SHR                                                       
      UPARAM(39) = T                                                         
      UPARAM(40) = ZERO                                                            
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")

      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")  
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
     & 5X,'IREAC FLAG. . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X, '   1:ORIGINAL 2-TERM-MODEL (1980)      ',/,     
     & 5X, '   2:EXTENDED 3-TERM-MODEL (1985)      ',/,     
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
     & 5X,'FTOL  . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  KINETICAL PARAMETERS                   :    ',/,
     & 5X,'  IGNITION TERM :                        ',/,
     & 5X,'I COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'b COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'x COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  GROWTH TERM 1 :                        ',/,
     & 5X,'G1 COEFFICIENT  . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'d COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'y COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  NUMERICAL LIMITORS                       ',/,
     & 5X,'CAPPA . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'CHI . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'TOL . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'a COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'  GROWTH TERM 2                          ',/,
     & 5X,'c COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'G2 COEFFICIENT. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'e COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'g COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'z COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/,
     & /,
     & 5X,'Figmax (LIMITER FOR IGNITIONT TERM) . . =',1PG20.13/,
     & 5X,'FG1max (LIMITER FOR GROWTH TERM 1). . . =',1PG20.13/,
     & 5X,'FG2min (LIMITER FOR GROWTH TERM 2). . . =',1PG20.13/,
     & /,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE (K) . . . . . . . . =',1PG20.13//)

C-----------------------------------------------
      RETURN
      END
