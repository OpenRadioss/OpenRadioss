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
Chd|  HM_READ_MAT63                 source/materials/mat/mat063/hm_read_mat63.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT63(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,
     .                         PARMAT   ,MAT_ID   ,PM       ,TITR     ,
     .                         UNITAB   ,LSUBMODEL,MTAG     ,MATPARAM )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW63 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE HM_OPTION_READ_MOD 
      USE UNITAB_MOD    
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
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)  :: NUPARAM,NUVAR
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL      
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .    E,NU,CP,A, B, Q,C,D,P, AHS,BHS,M,N,K1,K2,DH,VM0,EPS0,
     .    C1,G,TEMP,HL,COEF,RHO0,RHOR
      INTEGER ILAW
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================      
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      ILAW    = 63
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
C
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card1
      CALL HM_GET_FLOATV('MAT_E'    ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CP'       ,CP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2
      CALL HM_GET_FLOATV('MAT_A'    ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'    ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Q'        ,Q        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'    ,C        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D'    ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_FLOATV('MAT_P0'   ,P        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('A_HS'     ,AHS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('B_HS'     ,BHS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'    ,M        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'    ,N        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4
      CALL HM_GET_FLOATV('K1'       ,K1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('K2'       ,K2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('delta_H'  ,DH       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Kvm'      ,VM0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Epsilon_0',EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card5
      CALL HM_GET_FLOATV('MAT_T0'   ,TEMP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HL'   ,HL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA'     ,COEF     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C      
c-----------------------------------------------------------------------
c     Default values
c-----------------------------------------------------------------------
      IF(COEF <= ZERO .OR. COEF > ONE) COEF = ONE
      IF(VM0 == ZERO  .OR. VM0 >= ONE)  VM0=EM20
      IF (CP == ZERO) THEN
       CP = ZERO
      ELSE
       CP = ONE/CP
      ENDIF
C     
      IF (B == 0) B = -ONE
      IF ((B+1)/B>=P) THEN
        CALL ANCMSG(MSGID=301,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .               I1=63,
     .               I2=MAT_ID,
     .               C1=TITR)
      END IF
c-----------------------------------------------------------------------      
      G  = HALF*E/(ONE+NU)
      C1 = E/THREE/(ONE - TWO*NU) 
c-----------------------------------------------------------------------
      UPARAM(1)  = E
      UPARAM(2)  = E/(ONE - NU*NU)
      UPARAM(3)  = NU*UPARAM(2)
      UPARAM(4)  = G
      UPARAM(5)  = NU
      UPARAM(6)  = A
      UPARAM(7)  = B
      UPARAM(8)  = Q
      UPARAM(9)  = C
      UPARAM(10) = D
      UPARAM(11) = P
      UPARAM(12) = AHS
      UPARAM(13) = BHS
      UPARAM(14) = M
      UPARAM(15) = N
      UPARAM(16) = K1
      UPARAM(17) = K2
      UPARAM(18) = DH
      UPARAM(19) = VM0
      UPARAM(20) = EPS0
      UPARAM(21) = CP
      UPARAM(22) = TEMP           
      UPARAM(23) = HL
      UPARAM(24) = COEF     
c-----------------------------------------------------------------------
      NUPARAM = 24
      NUVAR = 4
c-----------------------------------------------------------------------
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU 
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G) 
c-----------------------------------------------------------------------
      PM(1)  = RHOR
      PM(89) = RHO0
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
c-----------------------------------------------------------------------
      ! Properties compatibility       
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC") 
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
        WRITE(IOUT,1300) E,NU,ONE/CP
        WRITE(IOUT,1400) A,B,Q,C,D     
        WRITE(IOUT,1500) P,AHS,BHS,M,N
        WRITE(IOUT,1600) K1,K2,DH,VM0,EPS0
        WRITE(IOUT,1700) TEMP,HL,COEF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 FORMAT
     &(5X,'MATERIAL : TRIP-STEELS  PLASTIC LAW',/,
     & 5X,'-----------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS  . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO  . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'THERMAL MASSIC CAPACITY  . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'PARAMETER 1  (A) . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 2  (B) . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 3  (Q) . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 4  (C) . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 5  (D) . . . . . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'PARAMETER 6  (P) . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 7  (AHS) . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 8  (BHS) . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 9  (M) . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 10 (N) . . . . . . . . . . . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'PARAMETER 11 (K1)  . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 12 (K2). . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER 13 (DH). . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL MARTENSITE VOLUME FRACTION . . . . .=',1PG20.13/,
     & 5X,'INITIAL PLASTIC STRAIN . . . . . . . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'INITIAL TEMPERATURE  . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LATENT HEAT OF MARTENSITE  . . . . . . . . .=',1PG20.13/
     & 5X,'TAYLOR-QUINNEY COEFFICIENT . . . . . . . . .=',1PG20.13/)
      END
