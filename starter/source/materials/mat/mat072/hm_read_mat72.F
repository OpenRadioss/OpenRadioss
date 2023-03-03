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
Chd|  HM_READ_MAT72                 source/materials/mat/mat072/hm_read_mat72.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT72(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW43 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)          :: UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .     RHO0,RHOR,
     .     E,NU,G,G2,C11,A1,A2,LAMHOOK,
     .     CP,EPS0,SIGY,EXP,
     .     FF,GG,HH,NN,LL,MM,
     .     C1,C2,C3,DC,MEXP
      INTEGER ILAW
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 72
C
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 - Isotropic elastic parameters
      CALL HM_GET_FLOATV('MAT_E'      ,E           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card3 - Hardening parameter + Hill yield criterion parameters
      CALL HM_GET_FLOATV('SIGMA_r'    ,SIGY         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Epsilon_0'  ,EPS0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_n1_t'   ,EXP         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_F'      ,FF          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G0'     ,GG          ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
card4 - Hill yield criterion parameters
      CALL HM_GET_FLOATV('MAT_HARD'   ,HH         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'      ,NN         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Lamda'  ,LL         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'      ,MM         ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
card5 - Modified Mohr Coulomb failure criterion parameter
      CALL HM_GET_FLOATV('MAT_C1'    ,C1         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C2'    ,C2         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C3'    ,C3         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE1'  ,MEXP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Dc'    ,DC         ,IS_AVAILABLE, LSUBMODEL, UNITAB)         
C 
      !========== DEFAULT VALUES=============!              
C           
      ! Poisson's ratio
      IF (NU < ZERO .OR. NU >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      ! Critical damage 
      IF (DC == ZERO)  DC = ONE
      ! Error message, DC < 1
      IF (DC < ONE) THEN 
         CALL ANCMSG(MSGID=1894,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=DC,
     .               I1=MAT_ID,
     .               C1=TITR)      
      ENDIF
      ! Test for HILL coefficient
      IF ((FF<ZERO).OR.(GG<ZERO).OR.(HH<ZERO).OR.
     .    (LL<ZERO).OR.(MM<ZERO).OR.(NN<ZERO)) THEN
         CALL ANCMSG(MSGID=1895,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)         
      ENDIF
      ! Hardening exponent 
      IF (EXP == ZERO)  EXP  = ONE
      ! Failure criterion exponent
      IF (MEXP == ZERO) MEXP = ONE
      ! Initial yield stress
      IF (SIGY == ZERO) SIGY = INFINITY
      IF (C2 == ZERO)   C2   = SIGY
      ! Make sure initial plastic strain is strictly positive
      IF (EPS0 < ZERO) THEN 
         CALL ANCMSG(MSGID=2080,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,
     .               C1=TITR)          
      ENDIF
      EPS0 = MAX(EPS0,EM20)
C
      ! Elastic parameters
      !  -> 3D isotropic elastic matrix components
      A1      = E*(ONE-NU) /((ONE + NU)*(ONE - TWO*NU))
      A2      = A1*NU/(ONE - NU)
      !  -> Bulk modulus
      C11     = E/THREE/(ONE - TWO*NU)
      !  -> Shear modulus
      G       = HALF*E/( ONE + NU)
      G2      = TWO*G
      !  -> Hooke's lambda parameter
      LAMHOOK = G2 * NU /(ONE - TWO*NU) 
C      
      ! PM table
      RHOR   = RHO0
      PM(1)  = RHOR
      PM(89) = RHO0
C
      ! PARMAT table
      PARMAT(1)  = C11
      PARMAT(2)  = E
      PARMAT(3)  = NU
      PARMAT(4)  = ZERO
      PARMAT(5)  = ZERO
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(C11+FOUR_OVER_3*G)
C
      ! MTAG variable activation
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%G_DMG  = 1
      MTAG%L_DMG  = 1
C
      ! MATPARAM parameters
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
C
      ! No viscosity, no strain-rate filtering
      ISRATE  = 0
      IMATVIS = 0
C
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameter
      NUPARAM = 24
      ! Number of user variable      
      NUVAR   = 1
c          
      ! Filling the parameter table
      ! -> Elastic parameters
      UPARAM(1)  = E
      UPARAM(2)  = NU
      UPARAM(3)  = G  
      UPARAM(4)  = G2
      ! -> Plane stress isotropic elastic matrix components (for shells)
      UPARAM(5)  = E/(ONE - NU*NU)
      UPARAM(6)  = NU*UPARAM(4)
      ! -> 3D isotropic elastic matrix components (for solids)
      UPARAM(7)  = A1
      UPARAM(8)  = A2
      ! -> Bulk modulus
      UPARAM(9)  = C11
      ! -> Hooke's lambda parameter
      UPARAM(10) = LAMHOOK
      ! -> Hardening parameters
      UPARAM(11) = SIGY
      UPARAM(12) = EPS0
      UPARAM(13) = EXP 
      ! -> Yield criterion parameters
      UPARAM(14) = FF
      UPARAM(15) = GG
      UPARAM(16) = HH
      UPARAM(17) = NN
      UPARAM(18) = LL
      UPARAM(19) = MM
      ! -> Failure criterion parameters
      UPARAM(20) = C1
      UPARAM(21) = C2
      UPARAM(22) = C3
      UPARAM(23) = MEXP
      UPARAM(24) = DC   
C
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED )THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1300) E,NU
        WRITE(IOUT,1400) SIGY,EXP,EPS0
        WRITE(IOUT,1600) FF,GG,HH,NN,LL,MM
        WRITE(IOUT,1700) C1,C2,C3,MEXP,DC
      ENDIF
C.....ERRORS
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      RETURN
 1000 FORMAT(
     & 5X,'  HILL ANISOTROPIC PLASTICITY + MODIFIED MOHR COULOMB  '/,
     & 5X,'  ---------------------------------------------------  '//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'INITIAL YIELD STRESS SIGY . . . . . . .=',1PG20.13/,
     & 5X,'HARDENING EXPONENT N  . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL PLASTIC STRAIN. . . . . . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'HILL MATERIAL PARAMETER F . . . . . . .=',1PG20.13/,
     & 5X,'HILL MATERIAL PARAMETER G . . . . . . .=',1PG20.13/,
     & 5X,'HILL MATERIAL PARAMETER H . . . . . . .=',1PG20.13/,
     & 5X,'HILL MATERIAL PARAMETER N . . . . . . .=',1PG20.13/,
     & 5X,'HILL MATERIAL PARAMETER L . . . . . . .=',1PG20.13/,
     & 5X,'HILL MATERIAL PARAMETER M . . . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'FAILURE PARAMETER C1. . . . . . . . . .=',1PG20.13/,
     & 5X,'FAILURE PARAMETER C2. . . . . . . . . .=',1PG20.13/,
     & 5X,'FAILURE PARAMETER C3. . . . . . . . . .=',1PG20.13/,
     & 5X,'DAMAGE EXPOSANT M . . . . . . . . . . .=',1PG20.13/,
     & 5X,'CRITICAL DAMAGE DC .. . . . . . . . . .=',1PG20.13/)
      RETURN
      END
