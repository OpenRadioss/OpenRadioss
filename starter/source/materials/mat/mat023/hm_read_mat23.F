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
Chd|  HM_READ_MAT23                 source/materials/mat/mat023/hm_read_mat23.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT23(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC     ,PARMAT   ,IMATVIS  ,IFORM    ,
     .           UNITAB   ,ID        ,TITR     ,LSUBMODEL,MTAG     ,
     .           PM       ,IPM       ,ISRATE) 
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW02 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C     IFORM           0 (PLAS_JOHNSON)    1 (PLAS_ZERILLI)    2 (PLAS_PREDEF)
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
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: ID,MAXUPARAM,MAXFUNC,IFORM
      INTEGER, INTENT(INOUT) :: IPM(NPROPMI),IMATVIS,ISRATE
      my_real, INTENT(INOUT) :: PM(NPROPM)     
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      INTEGER, INTENT(OUT) :: NUPARAM,NUVAR,NFUNC
      INTEGER, DIMENSION(MAXFUNC), INTENT(INOUT) :: IFUNC
      my_real, DIMENSION(MAXUPARAM), INTENT(INOUT) :: UPARAM
      my_real, INTENT(INOUT) :: PARMAT(*)
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: ICC,ISRAT1,IFLAG,MFLAG,ILAW
      my_real :: RHOR,RHO0
      my_real
     .   YOUNG, ANU, CA, CB, CN, EPSM, SIGM, CC, EPS0, EPSL, EL, G,
     .   E0, C0, C1, E1MN2, EN1N2, SDSP, HL, YLDL
      CHARACTER PREDEF*16
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      RHOR    = ZERO 
      NUVAR   = 0
      NUPARAM = 0
      NFUNC   = 0
      ILAW    = 23
C
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C      
       !line-1
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!!      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'    ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,ANU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_SIGY'   ,CA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'   ,CB     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'   ,CN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'    ,EPSM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'    ,SIGM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
 
       !line-4
      CALL HM_GET_FLOATV('MAT_SRC'  ,CC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'  ,EPS0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('STRFLAG'  ,ICC       ,IS_AVAILABLE, LSUBMODEL)
       !line-5
      CALL HM_GET_FLOATV('MAT_DAMAGE'      ,EPSL      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETAN'         ,EL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C 
      IF(RHOR == ZERO)RHOR = RHO0 
       PM(1) = RHOR
       PM(89)= RHO0    
C           
      IF(ANU == HALF) ANU=ZEP499
      IF(EPSL == ZERO) EPSL=ZEP15
      IF(CN  == ZERO)  CN    = ONE
      IF(EPSM == ZERO) EPSM  = EP30
      IF(SIGM == ZERO) SIGM  = EP30
      IF(CC == ZERO)  EPS0  = ONE
      G=YOUNG/(TWO*(ONE + ANU))
      E0=ZERO
      C0=ZERO
      C1=YOUNG/(THREE*(ONE - TWO*ANU))
      E1MN2=YOUNG/(ONE - ANU**2)            
      EN1N2=ANU*E1MN2
      SDSP =SQRT(YOUNG/MAX(PM(1),EM20))
      HL   =YOUNG * EL / MAX(EM20,YOUNG-EL)
      YLDL = CA+CB*EXP(CN*LOG(EPSL))
      YLDL = MIN(YLDL,SIGM)
C
      PM(20)=YOUNG
      PM(21)=ANU
      PM(22)=G
      PM(23)=E0
      PM(24)=E1MN2
      PM(25)=EN1N2
      PM(26)=FIVE_OVER_6
      PM(27)=SDSP
      PM(28)=ONE/YOUNG
      PM(29)=-ANU*PM(28)
      PM(30)=ONE/G
      PM(31)=C0
      PM(32)=C1
      PM(38)=CA
      PM(39)=CB
      PM(40)=CN
      PM(41)=EPSM
      PM(42)=SIGM
      PM(43)=CC
      PM(44)=EPS0
      PM(45)=EPSL
      PM(46)=HL
      PM(47)=YLDL
      PM(48)=EL
      PM(49)=ICC
C-----------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) = TWO*G/(C1+FOUR_OVER_3*G) ! =(1-2*Nu)/(1-Nu)
C
      WRITE(IOUT,1010) TRIM(TITR),ID,ILAW
      WRITE(IOUT,1000)     
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE 
        WRITE(IOUT,1020)RHO0 
        WRITE(IOUT,1300)YOUNG,ANU,G
        WRITE(IOUT,1400)CA,CB,CN,EPSM,SIGM
        WRITE(IOUT,1600)CC,EPS0,ICC
        WRITE(IOUT,1800)EPSL,EL
      ENDIF
C
      IF(CN>ONE) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=23,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
      IF(EPS0==ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=23,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
      IF(EL>ZERO.OR.EL<=-YOUNG.OR.EL<=-THREE*G) THEN
         CALL ANCMSG(MSGID=308,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
      ENDIF
C
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_EPSD  = 1   
      MTAG%G_PLA   = 1   
c
      MTAG%L_EPSD  = 1   
      MTAG%L_PLA   = 1   
c-------------------
      RETURN
c-------------------
 1000 FORMAT(
     & 5X,'  ELASTIC PLASTIC + DAMAGE LAW          ',/,
     & 5X,'  ----------------------------          ',//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . .=',I10/) 
 1020 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . . .=',1PG20.13/) 
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'YIELD COEFFICIENT CA. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CB. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CN. . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE ON SIG-MAX . . . .=',I10//)
 1800 FORMAT(
     & 5X,'DAMAGE STRAIN LIMIT . . . . . . . . . .=',1PG20.13/,
     & 5X,'DAMAGE TANGENT MODULUS EL . . . . . . .=',1PG20.13//)
c-------------------
      RETURN
      END
