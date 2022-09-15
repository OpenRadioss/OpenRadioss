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
Chd|  HM_READ_MAT73                 source/materials/mat/mat073/hm_read_mat73.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT73(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         ITABLE   ,MAXTABL  ,NTABLE,PM     ,IPM      )
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
     .                                         ,NTABLE,ITABLE(MAXTABL)
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID,MAXTABL
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NRATE,NPS,IR0,ILAW
      MY_REAL 
     .         E,NU,G,C1,EPSMAX,EPSR1,EPSR2,
     .         R0,R45,R90,R,H,FISOKIN,M,
     .         EINF,CE,T0, RHOCP,X2FAC,YFAC,FAC_PRES,FAC_FREQ
      INTEGER IFUNCE, OPTE, NUM_FUNC
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      MTAG%G_PLA  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
      MTAG%L_PLA  = 1
      ISRATE=0
      IMATVIS=0
      ILAW=73
 
 
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'      ,E           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_INTV  ('Yr_fun'     ,IFUNCE      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EFIB'   ,EINF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'      ,CE          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MAT_R00'    ,R0          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R45'    ,R45         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R90'    ,R90         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CHARD'  ,FISOKIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iyield' ,IR0         ,IS_AVAILABLE, LSUBMODEL)
      !line-5
      CALL HM_GET_FLOATV('MAT_EPS'    ,EPSMAX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST1'  ,EPSR1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2'  ,EPSR2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-6
      CALL HM_GET_INTV  ('FUN_A1'      ,ITABLE(1)   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_FScale'  ,YFAC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PScale'  ,X2FAC       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      !line-7
      CALL HM_GET_FLOATV('T_Initial'   ,T0          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SPHEAT'  ,RHOCP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !unit
      CALL HM_GET_FLOATV_DIM('MAT_FScale'  ,FAC_PRES    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_PScale'  ,FAC_FREQ    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !========== DEFAULT VALUES=============!              
      RHOR=RHO0
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0
!!
      IF(YFAC == ZERO)YFAC=FAC_PRES
      IF(X2FAC == ZERO)X2FAC=FAC_FREQ
C          
C tab(1st entry = plastic strain, 2nd entry = strain rate, 3rd entry = temperature)
C
      IF(R0 == ZERO) R0    = ONE
      IF(R45 == ZERO) R45   = ONE
      IF(R90 == ZERO) R90   = ONE
      IF(EPSR1 == ZERO)EPSR1=INFINITY
      IF(EPSR2 == ZERO)EPSR2=TWO*INFINITY
      IF(T0 == ZERO) T0=TWOHUNDRED93
C
      IF(EPSR1 >= EPSR2)THEN
         CALL ANCMSG(MSGID=1044,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
       
      ENDIF
      NTABLE=1
      UPARAM(1)=FISOKIN
      UPARAM(2)=E
      UPARAM(3)=E/(ONE-NU*NU)
      UPARAM(4)=NU*UPARAM(3)
      G=HALF*E/(1.+NU)
      UPARAM(5)=G
      UPARAM(6)=NU
      R=(R0+R45+R45+R90)*FOURTH
      H=R/(ONE+R)
C-----------A1,A2,A3,A12--------
      UPARAM(7)=H*(ONE+ONE/R0)
      UPARAM(8)=H*(ONE+ONE/R90)
      UPARAM(9)=H*TWO
      UPARAM(10)=(R45*TWO + ONE)*(UPARAM(7)+UPARAM(8)-UPARAM(9))
      IF (IR0 > 0) THEN
       UPARAM(8)=UPARAM(8)/UPARAM(7)
       UPARAM(9)=UPARAM(9)/UPARAM(7)
       UPARAM(10)=UPARAM(10)/UPARAM(7)
       UPARAM(7)=ONE
      END IF
      UPARAM(11)=ONE/X2FAC
      UPARAM(12)=YFAC
      UPARAM(13)=EPSMAX
      UPARAM(14)=EPSR1
      UPARAM(15)=EPSR2
      UPARAM(16)=TWO*G
      UPARAM(17)=THREE*G
      C1=E/3./(ONE - TWO*NU)
      UPARAM(18)=C1
      UPARAM(19)=C1+ FOUR_OVER_3*G
      UPARAM(20)=T0
      IF(RHOCP==ZERO)THEN
        UPARAM(21)=ZERO
      ELSE
        UPARAM(21)=ONE/RHOCP
      END IF
c--------------------------------
      OPTE = 0
      IF (IFUNCE > 0 )OPTE = 1
      NFUNC = 1
      IFUNC(NFUNC) = IFUNCE
      UPARAM(22) = NFUNC
      UPARAM(23) = OPTE
      UPARAM(24) = EINF
      UPARAM(25) = CE
c--------------------------------
      NUPARAM   =25
C
      PARMAT(1)=C1
      PARMAT(2)=E
      PARMAT(3)=NU
      NUVAR = 7
CC     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
C      
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,73
      WRITE(IOUT,1000)  
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1002)RHO0  
        WRITE(IOUT,1100)E,NU,G,R0,R45,R90,FISOKIN
        IF (IR0 >0) WRITE(IOUT,1110)
        WRITE(IOUT,1300)EPSMAX,EPSR1,EPSR2
        WRITE(IOUT,1200)ITABLE(1),X2FAC,YFAC
        WRITE(IOUT,1400)T0,RHOCP,IFUNCE,EINF,CE
        WRITE(IOUT,*)' '
      ENDIF
C
      RETURN 
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/) 
 1000 FORMAT(
     & 5X,47H  THERMAL TABULATED HILL ORTHOTROPIC PLASTICITY,/,
     & 5X,47H  ---------------------------------------------,//)
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R00. . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R45. . . . . . . .=',1PG20.13/
     & 5X,'LANKFORD COEFFICIENT R90. . . . . . . .=',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENNING FACTOR. . . . =',1PG20.13)
 1110 FORMAT(
     & 5X,'YIELD STRESS IS SUPPOSSD IN ORTHOTROPIC DIR. 1 '/)
 1200 FORMAT(
     & 5X,'YIELD STRESS TABLE NUMBER . . . . . . .=',I10/
     & 5X,'2ND ENTRY (STRAIN RATE) SCALE FACTOR. .=',1PG20.13/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . .=',1PG20.13)
 1300 FORMAT(
     & 5X,'MAXIMUM PLASTIC STRAIN. . . . . . . . .=',1PG20.13/
     & 5X,'TENSILE FAILURE STRAIN 1. . . . . . . .=',1PG20.13/
     & 5X,'TENSILE FAILURE STRAIN 2. . . . . . . .=',1PG20.13)
 1400 FORMAT(
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . .=',1PG20.13/
     & 5X,'HEAT CAPACITY PER VOLUME UNIT . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS SCALE FACTOR FUNCTION . .=',I10/
     & 5X,'YOUNG MODULUS EINF. . . . . . . . . . .=',1PG20.13/
     & 5X,'PARAMETER CE. . . . . . . . . . . . . .=',1PG20.13)
      END
