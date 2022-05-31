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
Chd|  HM_READ_MAT95                 source/materials/mat/mat095/hm_read_mat95.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT95(UPARAM   ,MAXUPARAM,NUPARAM ,NUVAR  ,MFUNC   , 
     .                         MAXFUNC  ,MTAG     ,PARMAT  ,UNITAB ,IMATVIS ,
     .                         PM       ,LSUBMODEL, ID     ,TITR   ,MATPARAM)                     
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
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
      USE HM_OPTION_READ_MOD 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM     
      my_real, DIMENSION(100)    ,INTENT(OUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)  :: UPARAM
      
      INTEGER, INTENT(INOUT)          :: MFUNC,NUPARAM,NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM 
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: I,J,NRATE,NPS,OPTE,IFUNCE,ILAW,NFUNC
      my_real :: RHO0, RHOR,E,NU,G,MU,RBULK,   C1,  
     .           C10,C01,C20,C11,C02 ,TAUREF_UNIT,
     .           C30, C21,C12,C03,SB,D1,D2,D3,A,EXPC,EXPM,KSI,TAUREF
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C-----------------------------------------------
      IMATVIS = 1                 
      ILAW    = 95
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C======================================
C
C     BERGSTROM BOYCE LAW
C
C======================================
      C10    = ZERO
      C01    = ZERO
      C20    = ZERO
      C11    = ZERO
      C02    = ZERO
      C30    = ZERO
      C21    = ZERO
      C12    = ZERO
      C03    = ZERO
      D1     = ZERO
      D2     = ZERO
      D3     = ZERO
      NU     = ZERO
      TAUREF = ZERO
      A      = ZERO
Card1        
      CALL HM_GET_FLOATV('MAT_C_10'    ,C10      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_01'    ,C01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_20'    ,C20      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_11'    ,C11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_02'    ,C02      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2      
      CALL HM_GET_FLOATV('MAT_C_30'    ,C30      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_21'    ,C21      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_12'    ,C12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C_03'    ,C03      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Sb'      ,SB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

Card3      
      CALL HM_GET_FLOATV('MAT_D_1'    ,D1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D_2'    ,D2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D_3'    ,D3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

Card4        
      CALL HM_GET_FLOATV('MLAW95_A'    ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW95_C'    ,EXPC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW95_M'    ,EXPM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MLAW95_KSI'  ,KSI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TAU_REF'     ,TAUREF   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
               

C------------------------------------------------------------------
      NUVAR = 10
      NUPARAM = 22
C------------------------------------------------------------------
      IF(D2 /=  ZERO )   D2 = ONE/D2    
      IF(D3 /=  ZERO )   D3 = ONE/D3 

      IF(TAUREF == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_TAU_REF' ,TAUREF_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        TAUREF = ONE * TAUREF_UNIT
      ENDIF


      G = TWO  * (C10 + C01) *(SB + ONE)  
      IF(D1 /= ZERO) THEN
         D1 = ONE/D1 
         RBULK= TWO*D1 *(ONE+SB) 
         NU = (THREE*RBULK -TWO*G)/(THREE*RBULK + G)/TWO
         E = NINE*RBULK*G/(THREE*RBULK + G)
      ELSE
         D2 = ZERO
         D3 = ZERO
         NU= 0.495
         RBULK = TWO_THIRD*G*(ONE + NU)/(ONE-TWO*NU)
         D1 = RBULK / TWO ! 1/D1 A VOIR SI ON SUPRIME PAS LE TERME AVEC J
         E = TWO*G*(ONE + NU)      
      ENDIF
C------------------------------------------------------------------
      UPARAM(19) = G 
      UPARAM(20) = RBULK 
C------------------------------------------------------------------
      IF(EXPM == ZERO)EXPM = ONE
      IF(EXPC == ZERO)EXPC = -0.700000000
      IF(KSI  == ZERO)KSI = EM02
C------------------------------------------------------------------
C------------------------------------------------------------------
C     
      UPARAM(1)  = C10
      UPARAM(2)  = C01
      UPARAM(3)  = C20
      UPARAM(4)  = C11
      UPARAM(5)  = C02
      UPARAM(6)  = C30
      UPARAM(7)  = C21
      UPARAM(8)  = C12
      UPARAM(9)  = C03
      UPARAM(11) = D1
      UPARAM(12) = D2
      UPARAM(13) = D3 
      UPARAM(14) = SB
      UPARAM(15) = A
      UPARAM(16) = EXPC 
      UPARAM(17) = EXPM
      UPARAM(18) = KSI 
      UPARAM(22) = TAUREF
c

c---------------------------------
C      PM PARMAT
C---------------------------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0    
      PARMAT(1) = RBULK
      PARMAT(2) = E
      PARMAT(3) = NU
C---------------------------------
      PM(100) = RBULK
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
c-----------------
      WRITE(IOUT,1001) TRIM(TITR),ID,95
      WRITE(IOUT,1000)
      IF (IS_CRYPTED)THEN                                     
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
         WRITE(IOUT,1002)RHO0
         WRITE(IOUT,1100)C10,C01,C20,C11,C02,
     .      C30, C21,C12,C03,D1,D2,D3  
         WRITE(IOUT,1300)A,EXPC,EXPM,KSI ,SB ,TAUREF
         WRITE(IOUT,1200)G,RBULK, NU
      ENDIF
C
 1000 FORMAT(
     & 5X,'  BERGSTROM BOYCE MATERIAL : ',/,
     & 5X,'  -------------------------- ',/)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT(
     & 5X,'C10 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C01 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C20 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C11 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C02 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C30 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C21 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C12 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C03 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D1  . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D2  . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D3  . . . . . . . . . . . . . . . . . .=',1PG20.13)
 1300 FORMAT(
     & 5X,'PARAMETERS FOR THE EFFECTIVE CREEP STRAIN RATE: ',/,
     & 5X,'A. . . . . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT C . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT M . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'XI . . . . . . . . . . . . . . . . . . . =',1PG20.13//    
     & 5X,'FACTOR S OF CHAIN B. . . . . . . . . . . =',1PG20.13/
     & 5X,'REFERENCE STRESS FOR CHAIN B . . . . . . =',1PG20.13)      
 1200 FORMAT(
     & 5X,'INITIAL SHEAR MODULUS .  . . . . . . . . =',1PG20.13/
     & 5X,'INITIAL BULK MODULUS. .  . . . . . . . . =',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . . =',1PG20.13//) 
C
      RETURN
 999  CALL FREERR(3)
      END









