Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_mat79            ../starter/source/materials/mat/mat079/hm_read_mat79.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT79(
     .     UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .     MAXFUNC  ,IFUNC    ,PARMAT   ,MAT_ID   ,PM       ,
     .     ISRATE   ,MTAG     ,TITR     ,UNITAB   ,LSUBMODEL,
     .     MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW79 WITH HM READER 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD    
      USE ELBUFTAG_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PARMAT(100), UPARAM(MAXUPARAM), PM(NPROPM)
      INTEGER, INTENT(INOUT)                :: IFUNC(MAXFUNC), NFUNC, MAXFUNC, MAXUPARAM, 
     .     NUPARAM, NUVAR, ISRATE
      INTEGER, INTENT(IN)                    :: MAT_ID
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)   :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MATS,IFLAG1,IFLAG2,ITEMAX,IDEL
      my_real
     .        SHEAR, AA, BB, MM, NN, CC, EPS0, SIGFMAX, TMAX, HEL, PHEL,
     .        D1, D2, K1, K2, K3, BETA, YOUNG, NU, RHO0, RHOR, ASRATE,
     .        EPSMAX
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C     S o u r c e 
C-----------------------------------------------

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C----------------------------------------------------------------
C     #RhoO rho_ref
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
C     #G
      CALL HM_GET_FLOATV('tau_shear', SHEAR, IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
C     #a b m n
      CALL HM_GET_FLOATV('MAT_A', AA, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B', BB, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M', MM, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N', NN, IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
C     #c epsp_0 sigma_f_max
      CALL HM_GET_FLOATV('MAT_C', CC, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Epsilon_F', EPS0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG1max_t', SIGFMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FCUT', ASRATE, IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
C     #T HEL PHEL
      CALL HM_GET_FLOATV('MAT_T0', TMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E', HEL, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS', PHEL, IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
C     #D1 D2 IDEL EPSMAX
      CALL HM_GET_FLOATV('D1' , D1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('D2' , D2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('IDEL',IDEL, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('EPSMAX',EPSMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
C----------------------------------------------------------------
C     #K1 K2 K3 BETA
      CALL HM_GET_FLOATV('K1', K1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('K2', K2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('K3', K3, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Beta', BETA, IS_AVAILABLE, LSUBMODEL, UNITAB)
     
      NUVAR = 2
      PM(1) = RHOR
      PM(89) = RHO0

C     Memory allocation flags
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%G_DMG  = 1
      MTAG%L_DMG  = 1
C
      !----------------------------------------------------------
      ! Activation of strain-rate filtering
      IF (ASRATE /= ZERO) THEN
        ISRATE = 1
      ELSE
        ISRATE = 0
      ENDIF
      ! Check flag for element deletion
      IDEL = MIN(IDEL,3)
      IDEL = MAX(0,IDEL)
      ! Critical plastic strain
      IF (EPSMAX == ZERO) EPSMAX = INFINITY
      !----------------------------------------------------------
C----------
C DEFAULT
C----------
      IF(CC==ZERO)   EPS0 = ONE
      IF(SIGFMAX==ZERO) SIGFMAX=INFINITY
C--------
C ERRORS
C--------
      IF(PHEL > HEL) THEN
         CALL ANCMSG(MSGID=907,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF(SHEAR <= ZERO)THEN
         CALL ANCMSG(MSGID=908,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF(K1 <= ZERO)THEN
         CALL ANCMSG(MSGID=909,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF(EPS0 <= ZERO)THEN
         CALL ANCMSG(MSGID=910,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF(BETA < ZERO .OR. BETA > ONE)THEN
         CALL ANCMSG(MSGID=911,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
C
      
C           
      UPARAM(1) = SHEAR
      UPARAM(2) = TWO*SHEAR
      UPARAM(3) = AA
      UPARAM(4) = BB
      UPARAM(5) = MM
      UPARAM(6) = NN
      UPARAM(7) = CC
      UPARAM(8) = EPS0
      UPARAM(9) = SIGFMAX
      UPARAM(10)= TMAX/PHEL
      UPARAM(11)= PHEL
      UPARAM(12)= THREE_HALF*(HEL-PHEL)
      UPARAM(13)= D1
      UPARAM(14)= D2
      UPARAM(15)= K1
      UPARAM(16)= K2
      UPARAM(17)= K3
      UPARAM(18)= BETA
      UPARAM(19)= IDEL
      UPARAM(20)= EPSMAX
      NUPARAM= 20
C
      NU=(THREE*K1-TWO*SHEAR)/(SIX*K1+TWO*SHEAR)
      YOUNG=NINE*K1*SHEAR/(THREE*K1+SHEAR)
      PARMAT(1) = K1
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = ASRATE
C
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC") 
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")       
C
      WRITE(IOUT, 900) TRIM(TITR),MAT_ID,79
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
       WRITE(IOUT,1050) RHO0
       WRITE(IOUT,1100) SHEAR, AA, BB, MM, NN, CC, EPS0, SIGFMAX
       WRITE(IOUT,1200) TMAX, HEL, PHEL, D1, D2, IDEL, EPSMAX
       WRITE(IOUT,1300) K1, K2, K3, BETA
       WRITE(IOUT,1400) YOUNG, NU 
       IF (ISRATE > 0) WRITE(IOUT,1500) ASRATE
      ENDIF
C 
      RETURN
C
  900 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1000 FORMAT(
     &  5X,'  JOHNSON HOLMQUIST MATERIAL',/,
     &  5X,'  --------------------------',//)
 1050 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . .=',1PG20.13/)  
 1100 FORMAT(
     &  5X,'SHEAR MODULUS . . . . . . . . . . . . . . .=',1PG20.13/,
     &  5X,'INTACT STRENGTH CONSTANT (A). . . . . . . .=',1PG20.13/,
     &  5X,'FRACTURED STRENGTH CONSTANT (B) . . . . . .=',1PG20.13/,
     &  5X,'FRACTURED STRENGTH EXPONENT (M) . . . . . .=',1PG20.13/,
     &  5X,'INTACT STRENGTH EXPONENT (N). . . . . . . .=',1PG20.13/,
     &  5X,'STRAIN RATE COEFFICIENT (C) . . . . . . . .=',1PG20.13/,
     &  5X,'REFERENCE STRAIN RATE . . . . . . . . . . .=',1PG20.13/,
     &  5X,'MAXIMUM NORMALIZED FRACTURED STRENGTH . . .=',1PG20.13//)
 1200 FORMAT(
     &  5X,'MAXIMUM PRESSURE TENSILE STRENGTH . . . . .=',1PG20.13/,
     &  5X,'HUGONIOT ELASTIC LIMIT (HEL). . . . . . . .=',1PG20.13/,
     &  5X,'PRESSURE AT HUGONIOT ELASTIC LIMIT. . . . .=',1PG20.13/,
     &  5X,'DAMAGE CONSTANT (D1). . . . . . . . . . . .=',1PG20.13/,
     &  5X,'DAMAGE EXPONENT (D2). . . . . . . . . . . .=',1PG20.13/,
     &  5X,'ELEMENT DELETION FLAG (IDEL). . . . . . . .=',I10/,
     &  5X,'  IDEL = 0: NO ELEMENT DELETION                        ',/,
     &  5X,'  IDEL = 1: ELEMENT DELETION IN TENSION ONLY           ',/,
     &  5X,'  IDEL = 2: ELEMENT DELETION IF PLASTIC STRAIN > EPSMAX',/,
     &  5X,'  IDEL = 3: ELEMENT DELETION IF DAMAGE = 1.0           ',/,
     &  5X,'CRITICAL PLASTIC STRAIN (EPSMAX). . . . . .=',1PG20.13/)
 1300 FORMAT(
     &  5X,'BULK MODULUS (K1) . . . . . . . . . . . . .=',1PG20.13/
     &  5X,'PRESSURE COEFFICIENT (K2) . . . . . . . . .=',1PG20.13/
     &  5X,'PRESSURE COEFFICIENT (K3) . . . . . . . . .=',1PG20.13/
     &  5X,'BULKING PRESSURE COEFFICIENT (BETA) . . . .=',1PG20.13)
 1400 FORMAT(
     &  5X,'YOUNG',1H','S MODULUS . . . . . . . . . . . . . .=',1PG20.13/,
     &  5X,'POISSON',1H','S RATIO . . . . . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     &  5X,'STRAIN RATE FILTERING FREQUENCY . . . . . .=',1PG20.13/)
C
      END
