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
Chd|  HM_READ_MAT81                 source/materials/mat/mat081/hm_read_mat81.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT81(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,
     .                         IFUNC    ,MAXFUNC  ,NFUNC    ,PARMAT   ,
     .                         MAT_ID   ,PM       ,TITR     ,UNITAB   ,
     .                         LSUBMODEL,MTAG     )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW81 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)                                :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(LUNIT,NUNITS) ,INTENT(IN)       :: UNITAB
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)          :: PM     
      CHARACTER*nchartitle ,INTENT(IN)                   :: TITR
      INTEGER, INTENT(INOUT)                             :: NUPARAM,NUVAR,NFUNC,IFUNC(MAXFUNC)
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)         :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT)                :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL 
      TYPE(MLAW_TAG_), INTENT(INOUT)                     :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real KINI,GINI,YLDINI,CAPINI,BETA,PSI,ALPHA,MAX_DILAT,EPSVINI,
     .  KWATER,POR0,SAT0,U0,MUW0,VISCFAC,TOL,RHO0,RHOR,FAC_UNIT
      INTEGER SOFT_FLAG
      INTEGER ILAW
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      ILAW    = 81
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
C
Card1
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2
      CALL HM_GET_FLOATV('K0'       ,KINI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G0'   ,GINI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_COH0'   ,YLDINI   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PB0'  ,CAPINI   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_FLOATV('MAT_Beta' ,BETA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Psi'      ,PSI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4
      CALL HM_GET_FLOATV('MAT_ALPHA',ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'  ,MAX_DILAT,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'  ,EPSVINI  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card5
      CALL HM_GET_INTV  ('FUN_A1'   ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A2'   ,IFUNC(2) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A3'   ,IFUNC(3) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A4'   ,IFUNC(4) ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Iflag'    ,SOFT_FLAG,IS_AVAILABLE, LSUBMODEL)
C
      ! Missing parameters for Radioss version < 14.0
Card6
      CALL HM_GET_FLOATV('MAT_KW' ,KWATER   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_POR0' ,POR0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SAT0' ,SAT0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE0'   ,U0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card7
      CALL HM_GET_FLOATV('MAT_TOL'  ,TOL      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_VIS' ,VISCFAC  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C      
c-----------------------------------------------------------------------
c     Default values
c-----------------------------------------------------------------------
      IF (KWATER    == ZERO) KWATER    = ONE
      IF (POR0      == ZERO) POR0      = ZERO
      IF (SAT0      == ZERO) SAT0      = ZERO
      IF (U0        == ZERO) U0        = ZERO
      IF (TOL       == ZERO) TOL       = EM04
      IF (VISCFAC   == ZERO) VISCFAC   = HALF
      IF (MAX_DILAT == ZERO) MAX_DILAT = -EP20
      MAX_DILAT = -ABS(MAX_DILAT)
      IF (ALPHA     == ZERO) ALPHA     = HALF
C-----------------------------------------------
C     DATA CHECKING
C-----------------------------------------------
      IF(KINI<=ZERO)THEN
          CALL ANCMSG(MSGID=1012,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF
      IF(GINI<=ZERO)THEN
          CALL ANCMSG(MSGID=1013,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF
C
C WHEN IMPLEMENTING in RADIOSS IF FUNC IS 0 THEN MODULI CONSTANT CAP_FUNC=0 NO CAP
C
      IF(IFUNC(1) == 0)THEN
        CALL ANCMSG(MSGID=1014,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      IF(IFUNC(2) == 0)THEN
        CALL ANCMSG(MSGID=1015,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      ! For cohesion and cap
      IF(IFUNC(3) == 0)THEN
        IF (YLDINI == ZERO) THEN 
          CALL ANCMSG(MSGID=1713,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)         
        ELSE
          CALL ANCMSG(MSGID=1016,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR)
        ENDIF
      ENDIF
      IF(IFUNC(4) == 0)THEN
        IF (CAPINI == ZERO) THEN 
          CALL ANCMSG(MSGID=1714,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)         
        ELSE
          CALL ANCMSG(MSGID=1017,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR)        
        ENDIF
      ENDIF
      ! Default value for scale factors of c and Pb
      IF (YLDINI    == ZERO) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_COH0' ,FAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        YLDINI = ONE*FAC_UNIT
      ENDIF
      IF (CAPINI    == ZERO) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_PB0' ,FAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CAPINI = ONE*FAC_UNIT
      ENDIF
C new in May 2015 by Fred Perie (implemented by Marian)
      IF(SAT0/=ZERO)THEN
       IF(KWATER <=ZERO )THEN
          CALL ANCMSG(MSGID=1085,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
       ENDIF
       IF(POR0  == ZERO )THEN
          CALL ANCMSG(MSGID=1086,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
       ELSEIF(U0 > ZERO)THEN
         MUW0=U0/KWATER
         SAT0=ONE+MUW0
       ELSE
         MUW0=SAT0-ONE
         IF(MUW0 > ZERO)THEN
           U0=KWATER*MUW0
         ELSE 
           U0=ZERO
         ENDIF
       ENDIF
      ELSE
         MUW0=-ONE
      ENDIF
C-----------------------------------------------
C     USER MATERIAL PARAMETERS DEFINITION  
C-----------------------------------------------
      UPARAM(1) = KINI
      UPARAM(2) = GINI
      UPARAM(3) = TAND(BETA)
      UPARAM(4) = TAND(PSI)
      UPARAM(5) = ALPHA
      UPARAM(6) = MAX_DILAT
      UPARAM(7) = SOFT_FLAG + EM02
      UPARAM(8) = EPSVINI
c     porosity
      UPARAM(9)  = KWATER
      UPARAM(10) = POR0
      UPARAM(11) = MUW0+ONE
      UPARAM(12) = U0
      UPARAM(13) = TOL
      UPARAM(14) = VISCFAC
c     yld and cap function scaling factors
      UPARAM(15) = YLDINI
      UPARAM(16) = CAPINI
c----------------------------------------------------------------------- 
      NUPARAM    = 16
      NFUNC      = 4
      NUVAR      = 6
      MTAG%G_PLA = 2
      MTAG%L_PLA = 2
      MTAG%G_SEQ = 1
      MTAG%L_SEQ = 1
C-----------------------------------------------
C     USED FOR SOLIDS 
C-----------------------------------------------
C used for interface (solid+shell)
C
      PARMAT(1) = KINI + FOUR_OVER_3*GINI
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*GINI/(KINI + FOUR_OVER_3*GINI) 
c-----------------------------------------------------------------------
      IF (RHOR == ZERO) RHOR = RHO0
      PM(1)      = RHOR
      PM(89)     = RHO0
c-----------------------------------------------------------------------  
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) KINI,GINI,YLDINI,CAPINI
        WRITE(IOUT,1400) BETA,PSI
        WRITE(IOUT,1500) ALPHA,MAX_DILAT,EPSVINI
        WRITE(IOUT,1600) IFUNC(1),IFUNC(2),IFUNC(3),IFUNC(4),SOFT_FLAG
        WRITE(IOUT,1700) KWATER,POR0,SAT0,U0
        WRITE(IOUT,1800) TOL,VISCFAC
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT
     &(5X,'MATERIAL : DRUCKER PRAGER LAW WITH CAP',/,
     & 5X,'--------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'INITIAL BULK MODULUS K0 . . . . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL SHEAR MODULUS G0  . . . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL MATERIAL COHESION C0  . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL CAP LIMIT PRESSURE PB0  . . . . . . =',1PG20.13/)
 1400 FORMAT(
     & 5X,'FRICTION ANGLE BETA . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'PLASTIC FLOW ANGLE PSI  . . . . . . . . . . =',1PG20.13/)
 1500 FORMAT(
     & 5X,'ALPHA : RATION PA/PB  . . . . . . . . . . . =',1PG20.13/,
     & 5X,'MAXIMUM DILATANCY . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'INITIAL PLASTIC VOLUMETRIC STRAIN . . . . . =',1PG20.13/)
 1600 FORMAT(
     & 5X,'BULK MODULUS K SCALE FUNCTION . . . . . . . =',I10     /,
     & 5X,'SHEAR MODULUS G SCALE FUNCTION  . . . . . . =',I10     /,
     & 5X,'MATERIAL COHESION C SCALE FUNCTION  . . . . =',I10     /,
     & 5X,'CAP LIMIT PRESSURE PB SCALE FUNCTION  . . . =',I10     /,
     & 5X,'FLAG: =0 SOFTENING ALLOWED, =1 NO SOFTENING =',I10     /)
 1700 FORMAT(
     & 5X,'BULK MODULUS OF WATER . . . . . . . . . . . =',1PG20.13/
     & 5X,'INITIAL POROSITY POR0 . . . . . . . . . . . =',1PG20.13/
     & 5X,'INITIAL WATER SATURATION SAT0 . . . . . . . =',1PG20.13/
     & 5X,'INITIAL PORE PRESSURE U0  . . . . . . . . . =',1PG20.13/)
 1800 FORMAT(
     & 5X,'TOLERANCE FOR THE CRITERION SHIFT . . . . . =',1PG20.13/
     & 5X,'VISCOSITY FACTOR  . . . . . . . . . . . . . =',1PG20.13/)
      END

