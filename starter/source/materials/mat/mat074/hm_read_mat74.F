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
Chd|  HM_READ_MAT74                 source/materials/mat/mat074/hm_read_mat74.F
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
      SUBROUTINE HM_READ_MAT74(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         ITABLE   ,MAXTABL  ,NTABLE,PM     ,IPM       )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW74 WITH HM READER 
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
       my_real 
     .         E,NU,G,C1,EPSMAX,EPSR1,EPSR2,X2FAC,YFAC,
     .         S11Y,S22Y,S33Y,S12Y,S23Y,S31Y,
     .         FISOKIN,M,FAC_L,FAC_T,FAC_M,FAC_C,
     .         T0, RHOCP, FF, GG, HH, LL, MM, NN, FCUT,
     .         EINF,CE,FAC_PRES,FAC_STRAIN_RATE
      INTEGER IFUNCE, OPTE, NUM_FUNC
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      MTAG%G_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_EPSD = 1
      MTAG%L_PLA  = 1
      ISRATE=0
      IMATVIS=0
      ILAW=74
C 
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'        ,E           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,NU          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'      ,EPSMAX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST1'    ,EPSR1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2'    ,EPSR2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_INTV  ('Yr_fun'     ,IFUNCE      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EFIB'   ,EINF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_C'      ,CE          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_INTV  ('Fsmooth'    ,ISRATE       ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_HARD'   ,FISOKIN      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Fcut'       ,FCUT         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('MAT_SIGT1'  ,S11Y       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGT2'  ,S22Y       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGT3'  ,S33Y       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-6
      CALL HM_GET_FLOATV('MAT_SIGYT1'  ,S12Y       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGYT2'  ,S23Y       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGYT3'  ,S31Y       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-7
      CALL HM_GET_INTV  ('FUN_A1'      ,ITABLE(1)   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_FScale'  ,YFAC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PScale'  ,X2FAC       ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      !line-8
      CALL HM_GET_FLOATV('T_Initial'   ,T0          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SPHEAT'  ,RHOCP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !unit
      CALL HM_GET_FLOATV_DIM('MAT_FScale'  ,FAC_PRES         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_PScale'  ,FAC_STRAIN_RATE  ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
      !========== DEFAULT VALUES=============!              
      RHOR = RHO0
      PM(1) =RHOR
      PM(89)=RHO0
!!
      IF(YFAC == ZERO)YFAC=FAC_PRES
      IF(X2FAC == ZERO)X2FAC=FAC_STRAIN_RATE
C          
C tab(1st entry = plastic strain, 2nd entry = strain rate, 3rd entry = temperature)
C
      IF(EPSMAX == ZERO) EPSMAX=INFINITY
      IF(EPSR1  == ZERO) EPSR1=INFINITY
      IF(EPSR2  == ZERO) EPSR2=TWO*INFINITY
      IF(T0     == ZERO) T0=TWOHUNDRED93
C
      NTABLE=1
      UPARAM(1)=FISOKIN
      UPARAM(2)=E
      UPARAM(3)=E/(ONE-NU*NU)
      UPARAM(4)=NU*UPARAM(3)
      G=HALF*E/(1.+NU)
      UPARAM(5)=G
      UPARAM(6)=NU

C----------- FF...NN --------
      IF (S11Y <= ZERO) THEN
         CALL ANCMSG(MSGID=822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='S11Y')
      ENDIF
      IF (S22Y <= ZERO) THEN
         CALL ANCMSG(MSGID=822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='S22Y')
      ENDIF
      IF (S33Y <= ZERO) THEN
         CALL ANCMSG(MSGID=822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='S33Y')
      ENDIF
      IF (S12Y <= ZERO) THEN
         CALL ANCMSG(MSGID=822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='S12Y')
      ENDIF
      IF (S23Y <= ZERO) THEN
         CALL ANCMSG(MSGID=822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='S23Y')
      ENDIF
      IF (S31Y <= ZERO) THEN
         CALL ANCMSG(MSGID=822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='S31Y')
      ENDIF
C
      FF=HALF*(ONE/S22Y**2+ONE/S33Y**2-ONE/S11Y**2)
      GG=HALF*(ONE/S11Y**2+ONE/S33Y**2-ONE/S22Y**2)
      HH=HALF*(ONE/S11Y**2+ONE/S22Y**2-ONE/S33Y**2)
      LL=HALF/S23Y**2
      MM=HALF/S31Y**2
      NN=HALF/S12Y**2
      UPARAM(7)=FF
      UPARAM(8)=GG
      UPARAM(9)=HH
      UPARAM(10)=LL
      UPARAM(11)=MM
      UPARAM(12)=NN
      UPARAM(13)=ONE/X2FAC
      UPARAM(14)=YFAC
      UPARAM(15)=EPSMAX
      UPARAM(16)=EPSR1
      UPARAM(17)=EPSR2
      UPARAM(18)=TWO*G
      UPARAM(19)=THREE*G
      C1=E/THREE/(ONE - TWO*NU)
      UPARAM(20)=C1
      UPARAM(21)=C1+ FOUR_OVER_3*G
      UPARAM(22)=T0
      IF(RHOCP==ZERO)THEN
        UPARAM(23)=ZERO
      ELSE
        UPARAM(23)=ONE/RHOCP
      END IF
c--------
      OPTE = 0
      IF (IFUNCE > 0 )OPTE = 1
      NFUNC = 1
      IFUNC(NFUNC) = IFUNCE
      UPARAM(24) = NFUNC
      UPARAM(25) = OPTE
      UPARAM(26) = EINF
      UPARAM(27) = CE
c-------
      NUPARAM   =27
C
      ISRATE = 1
      PARMAT(1)=C1
      PARMAT(2)=E
      PARMAT(3)=NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
C
      NUVAR = 10
CC     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
C
        WRITE(IOUT,1001) TRIM(TITR),MAT_ID,ILAW
        WRITE(IOUT,1000)        
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1002)RHO0  
        WRITE(IOUT,1100)E,NU,G,S11Y,S22Y,S33Y,S12Y,S23Y,S31Y,FISOKIN,
     .                  ISRATE,FCUT
        WRITE(IOUT,1300)EPSMAX,EPSR1,EPSR2
        WRITE(IOUT,1200)ITABLE(1),X2FAC,YFAC
        WRITE(IOUT,1400)T0,RHOCP,IFUNCE,EINF,CE
        WRITE(IOUT,*)' '
      ENDIF
C
      RETURN
 1000 FORMAT(
     & 5X,50H  TABULATED HILL ORTHOTROPIC PLASTICITY FOR SOLIDS,/,
     & 5X,50H  ------------------------------------------------,//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/) 
 1100 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD IN DIRECTION 1. . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD IN DIRECTION 2. . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD IN DIRECTION 3. . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD IN SHEAR DIRECTION 12 . . . . . .=',1PG20.13/
     & 5X,'YIELD IN SHEAR DIRECTION 23 . . . . . .=',1PG20.13/
     & 5X,'YIELD IN SHEAR DIRECTION 31 . . . . . .=',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENNING FACTOR . . . .=',1PG20.13/
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . .=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13/)
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

 
