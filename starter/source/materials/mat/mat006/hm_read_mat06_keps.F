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
Chd|  HM_READ_MAT06_KEPS            source/materials/mat/mat006/hm_read_mat06_keps.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT06_KEPS(
     .           LSUBMODEL ,MTAG6    ,UNITAB   ,IPM     ,PM,
     .           MAT_ID    ,TITR     ,ISRATE,
     .           ITYP      ,MTAG17   ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD !MAXEOS
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW21 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(INOUT)                       :: MAT_ID
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG6,MTAG17
      INTEGER,INTENT(IN)                           :: ITYP    
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT)         :: MATPARAM   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER JTUR, I, EMPTY_LINE,COUNT,LAW6_OPT
      my_real RHO0, VIS, BID, PSH, C0, C1, C2, C3, C4, C5, PMIN, E0, RK0,
     .        SSL, CTM, SK, SE, RPR, CT1, CT2, CT3, AK, E, A, B, YP0, YP1,
     .        MU,RHOR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      COUNT = 0
      JTUR=PM(70)
!---
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!
      CALL HM_GET_INTV  ('Line_count'   ,COUNT    ,IS_AVAILABLE,LSUBMODEL)
      COUNT=COUNT+2

      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('Law6_opt'     ,LAW6_OPT ,IS_AVAILABLE,LSUBMODEL)  
      !1,2,3  (3=K-EPS)

      CALL HM_GET_FLOATV('DAMP1'        ,VIS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'       ,PMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      IF (PMIN == ZERO) PMIN=-INFINITY
      IF (RHOR == ZERO) RHOR=RHO0

      CALL HM_GET_FLOATV('RHO0_K0'      ,RK0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SSL'      ,SSL     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('C_MU'      ,CTM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SIG_k'      ,SK      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SIG_EPS'      ,SE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('P_R_ON_P_RT'      ,RPR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOATV('C_1'      ,CT1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('C_2'      ,CT2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('C_3'      ,CT3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)            
      CALL HM_GET_FLOATV('KAPPA'      ,AK      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('E'      ,E       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ALPHA'      ,A     ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
      CALL HM_GET_FLOATV('GSI_T'      ,B     ,IS_AVAILABLE, LSUBMODEL, UNITAB)            
      
      E0 = ZERO
      C0 = ZERO
      C1 = ZERO
      C2 = ZERO
      C3 = ZERO
      C4 = ZERO
      C5 = ZERO
      PSH = ZERO

      PM(1) = RHOR
      PM(89)= RHO0
      PM(23)=E0
      PM(24)=VIS
      PM(104)=C0 - PSH
      PM(32)=C1
      PM(33)=C2
      PM(34)=C3
      PM(35)=C4
      PM(36)=C5
      PM(37)=PMIN

      IF(RHOR /= ZERO) THEN
        MU = RHO0/RHOR-ONE  
      ELSE
        MU = ZERO  ! RHOR=0.0 => error 683 will be displayed in hm_read_mat.F
      ENDIf
            
      IF(MU < ZERO)THEN
        PM(31) = -PSH + C0+C1*MU + C3*MU**3+(C4+C5*MU)*E0
      ELSE
        PM(31) = -PSH + C0+C1*MU + C2*MU**2+C3*MU**3+(C4+C5*MU)*E0
      ENDIF

      PM(80)=INFINITY
      
      IF(SSL == ZERO)SSL= EP10
      PM(46)=SSL
      PM(95)=RPR      

      IF(CTM == ZERO)CTM=NINEEM2                           
      IF(CT1 == ZERO)CT1=ONEP44                             
      IF(CT2 == ZERO)CT2=ONEP92                             
      IF(CT3 == ZERO)CT3=-ZEP373                           
      IF(SK == ZERO)SK=ONE                                  
      IF(SE == ZERO)SE=ONEP3                                
      IF(RPR == ZERO)RPR=SEVEN_OVER_9                            
      !PARAMETRES MUR TURBULENT                            
      IF(AK == ZERO) AK=ZEP4187                            
      IF(E == ZERO)  E=NINEP793                            
      IF(A == ZERO)  A=HALF                              
      PM(47)=A*AK                                          
      PM(48)=E                                             
      PM(49)=A                                             
      PM(50)=B                                             
      YP0=ELEVENP5                                           
      DO I=1,100                                        
        YP1=LOG(E*YP0)/AK                                    
        IF(YP1 == YP0)EXIT                                
        YP0=YP1                                              
      ENDDO                                           
      PM(51)=YP1                                           
      PM(79)=THREE100                                      
      ! FIN PARAMS MUR TURBULENT                           
      PM(81)=CTM                                           
      PM(82)=CT1                                           
      PM(83)=CT2                                           
      PM(84)=CT3                                           
      PM(85)=SK                                            
      PM(86)=SE                                            
      PM(95)=RPR                                           
      PM(87)=RK0/RHO0  
!---------------------------------------------------------   
      MATPARAM%IEOS = 18   ! linear EOS is used by default 
      IPM(4)        = 18   ! keep this temporarily for output and fluid sections
!---------------------------------------------------------        
      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_POROUS")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")

      WRITE(IOUT,2001) TRIM(TITR),MAT_ID,6
      WRITE(IOUT,1001)
               
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT,2002)RHO0,RHOR         
         WRITE(IOUT,1300)VIS
         WRITE(IOUT,1501)PMIN 
      ENDIF
      WRITE(IOUT,1600)RK0,SSL,CTM,SK,SE,RPR,CT1,CT2,CT3  
      WRITE(IOUT,1601)AK,E,A,B       

      PM(88)=PSH
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG6%G_RK    = 1
      MTAG6%G_RE    = 1
      MTAG6%L_RK    = 1 
      MTAG6%L_RE    = 1 
      MTAG6%L_VK    = 1 
      MTAG6%G_TEMP  = 1
      MTAG6%L_TEMP  = 1
c
      MTAG17%G_RK   = 1
      MTAG17%G_RE   = 1
      MTAG17%L_RK   = 1 
      MTAG17%L_RE   = 1 
      MTAG17%L_VK   = 1
      MTAG17%G_TEMP = 1
      MTAG17%L_TEMP = 1 
C--------------------------------
 1001 FORMAT(
     & 5X,'  K-EPSILON TURBULENT FLUID ',/,
     & 5X,'  ------------------------- ',//)
 1300 FORMAT(
     & 5X,'VISCOSITY . . . . . . . . . . . . . . .=',1PG20.13)
 1501 FORMAT(
     & 5X,'PRESSURE CUTOFF . . . . . . . . . . . .=',1PG20.13//)
 2001 FORMAT(/
     & 5X,A,/,
     & 5X,'   MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'   MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 2002 FORMAT(
     &  5X,'  INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     &  5X,'  REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'RK0 INITIAL TURBULENT ENERGY. . . . . .=',1PG20.13/,
     & 5X,'SSL SUBGRID SCALE LENGTH. . . . . . . .=',1PG20.13/,
     & 5X,'CMU TURBULENT VISCOSITY COEFFICIENT . .=',1PG20.13/,
     & 5X,'SK  K DIFFUSION COEFFICIENT . . . . . .=',1PG20.13/,
     & 5X,'SE  EPSILON DIFFUSION COEFFICIENT . . .=',1PG20.13/,
     & 5X,'PR/PRT LAM./TURB. PRANDT NUMBER RATIO .=',1PG20.13/,
     & 5X,'CT1 EPSILON SOURCE COEFFICIENT C1 . . .=',1PG20.13/,
     & 5X,'CT2 EPSILON SOURCE COEFFICIENT C2 . . .=',1PG20.13/,
     & 5X,'CT3 EPSILON SOURCE COEFFICIENT C3 . . .=',1PG20.13/)
 1601 FORMAT(
     & 5X,'TURBULENT WALL COEFFICIENTS             '/,
     & 5X,'K  LOGARITHMIQUE LAW COEFFICIENT. . . .=',1PG20.13/,
     & 5X,'E  LOGARITHMIQUE LAW COEFFICIENT. . . .=',1PG20.13/,
     & 5X,'A  K,EPSILON ECCENTRICITY . . . . . . .=',1PG20.13/,
     & 5X,'B  SOURCE FLAG. . . . . . . . . . . . .=',1PG20.13/)
C--------------------------------
      RETURN
      END
