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
Chd|  HM_READ_MAT48                 source/materials/mat/mat048/hm_read_mat48.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT48(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,
     .                         PARMAT   ,MAT_ID   ,PM       ,TITR     ,
     .                         UNITAB   ,LSUBMODEL,ISRATE   ,NFUNC    ,
     .                         MTAG     )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW48 WITH HM READER
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
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM
      my_real, DIMENSION(LUNIT,NUNITS) ,INTENT(IN) :: UNITAB
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(OUT)  :: NUPARAM,NUVAR,ISRATE,NFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT)          :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL   
      TYPE(MLAW_TAG_),INTENT(INOUT)                :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .    E,NU,CA,CB,CN,CC,CD,CM,CE,CK,
     .    FISOKIN,FCUT, SIGM,EPS0,EPSM,EPSR1,EPSR2,
     .    G,C1,RHO0,RHOR
      INTEGER ILAW,ISRATE_LOCAL
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      ILAW    = 48
      CN      = ONE
      FISOKIN = ZERO
c------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)
c------------------------------------------
C
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card1
      CALL HM_GET_FLOATV('MAT_E'    ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2
      CALL HM_GET_FLOATV('MAT_SIGY' ,CA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'    ,CB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'    ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD' ,FISOKIN  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'  ,SIGM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_FLOATV('MAT_C'    ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D'    ,CD       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'    ,CM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E1'   ,CE       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K'    ,CK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4
      CALL HM_GET_FLOATV('MAT_E0'   ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SCALE'    ,FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card5
      CALL HM_GET_FLOATV('MAT_EPS'  ,EPSM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA1' ,EPSR1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA2' ,EPSR2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C      
c-----------------------------------------------------------------------
c     Default values
c-----------------------------------------------------------------------
      IF (NU < ZERO .OR. NU >= HALF) THEN
C        WRITE(IOUT,*)' ** ERROR : WRONG NU VALUE'
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF(CC > ZERO .AND. EPS0 > ZERO  .AND. FCUT == ZERO) THEN
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID, 
     .                C1=TITR)
      ENDIF
      
      IF (CN == ZERO .OR. CN == ONE)  CN = ONEP0001
      IF (CM == ZERO .OR. CM == ONE)  CM = ONEP0001
      IF (CK == ZERO)                CK = ONE
      IF (EPSR1 == ZERO) EPSR1 = INFINITY
      IF (EPSR2 == ZERO) EPSR2 = TWO*INFINITY
      IF (EPSM == ZERO)  EPSM  = INFINITY
      IF (SIGM == ZERO)  SIGM  = INFINITY
      IF (CC == ZERO)    EPS0  = ONE
      IF (FCUT <= ZERO)THEN
        FCUT = INFINITY
      ENDIF
      !----------------------------------------------------------------------
      ISRATE_LOCAL = 1
      ISRATE = 1
      !----------------------------------------------------------------------
      IF(EPSR2 <= EPSR1)THEN
C        WRITE(IOUT,*)' ** ERROR : EPSR2 MUST BE GREATER THAN EPSR1'
         CALL ANCMSG(MSGID=420,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .                   I1=MAT_ID,
     .                   C1=TITR)
      ENDIF
c----------------------------------------------------------------------- 
      NUPARAM = 25
c----------------------------------------------------------------------- 
      IF(NUPARAM > MAXUPARAM)THEN
C        WRITE(IOUT,*)' ** ERROR : NUPARAM GT MAXUPARAM'
C        WRITE(IOUT,*)'      NUPARAM =',NUPARAM,
C     .                ' MAXUPARAM =',MAXUPARAM
         CALL ANCMSG(MSGID=309,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               I2=NUPARAM,
     .               I3=MAXUPARAM)
      ELSE
        C1 = E / THREE / (ONE - TWO*NU)
        G  = E / TWO / (ONE + NU)
        UPARAM(1)  = E 
        UPARAM(2)  = NU
        UPARAM(3)  = CA
        UPARAM(4)  = SIGM
        UPARAM(5)  = EPSM
        UPARAM(6)  = EPSR1
        UPARAM(7)  = EPSR2
        UPARAM(8)  = CB
        UPARAM(9)  = CN
        UPARAM(10) = CC
        UPARAM(11) = CD
        UPARAM(12) = CM
        UPARAM(13) = ISRATE_LOCAL
        UPARAM(14) = FCUT
        UPARAM(15) = FISOKIN
        UPARAM(16) = G
        UPARAM(17) = TWO*G
        UPARAM(18) = THREE*G
        UPARAM(19) = C1
        UPARAM(20) = E/(ONE - NU*NU)
        UPARAM(21) = NU*UPARAM(20)
        UPARAM(22) = EPSM
        UPARAM(23) = EPS0
        UPARAM(24) = CE
        UPARAM(25) = CK
      ENDIF
c-----------------------------------------------------------------------     
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE_LOCAL
      PARMAT(5) = FCUT
c------------------------------------
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(C1+FOUR_OVER_3*G) ! == (1-2*nu)/(1-nu)
c-----------------------------------------------------------------------
      IF (RHOR == ZERO) RHOR = RHO0
      PM(1)  = RHOR
      PM(89) = RHO0
c-----------------------------------------------------------------------  
      NUVAR = 3 
      NFUNC = 0
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
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
        WRITE(IOUT,1300) E,NU
        WRITE(IOUT,1400) CA,CB,CN,FISOKIN,SIGM
        WRITE(IOUT,1500) CC,CD,CM,CE,CK  
        WRITE(IOUT,1600) EPS0,FCUT
        WRITE(IOUT,1700) EPSM,EPSR1,EPSR2
        IF(EPS0==ZERO) THEN
          WRITE(IOUT,2100)
        ENDIF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT
     &(5X,'MATERIAL : ZHAO CONSTITUTIVE LAW',/,
     & 5X,'--------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS  . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO  . . . . . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'PLASTICITY YIELD STRESS A . . . . . . . . . =',1PG20.13/,
     & 5X,'PLASTICITY HARDENING PARAMETER B  . . . . . =',1PG20.13/,
     & 5X,'PLASTICITY HARDENING EXPONENT N . . . . . . =',1PG20.13/,
     & 5X,'ISO-KINEMATIC HARDENING FACTOR CHARD  . . . =',1PG20.13/,
     & 5X,'PLASTICITY MAXIMUM STRESS SIGMA_MAX . . . . =',1PG20.13/)
 1500 FORMAT(
     & 5X,'RELATIVE STRAIN-RATE COEFFICIENT C  . . . . =',1PG20.13/,
     & 5X,'STRAIN-RATE PLASTICITY FACTOR D . . . . . . =',1PG20.13/,
     & 5X,'RELATIVE STRAIN-RATE EXPONENT N . . . . . . =',1PG20.13/,
     & 5X,'STRAIN-RATE COEFFICIENT E1  . . . . . . . . =',1PG20.13/,
     & 5X,'STRAIN-RATE EXPONENT K  . . . . . . . . . . =',1PG20.13/)
 1600 FORMAT(
     & 5X,'REFERENCE STRAIN-RATE EPS_RATE_0  . . . . . =',1PG20.13/,
     & 5X,'CUTOFF FREQUENCY FCUT . . . . . . . . . . . =',1PG20.13/)
 1700 FORMAT(
     & 5X,'FAILURE PLASTIC STRAIN EPS_MAX  . . . . . . =',1PG20.13/
     & 5X,'TENSILE FAILURE STRAIN 1 EPS_T1 . . . . . . =',1PG20.13/
     & 5X,'TENSILE FAILURE STRAIN 2 EPS_T2 . . . . . . =',1PG20.13/)
 2100 FORMAT(1X,'EPS0 SHOULD BE .GT. 0. IF CC .NE. 0.')
      END
