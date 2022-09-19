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
Chd|  HM_READ_MAT32                 source/materials/mat/mat032/hm_read_mat32.F
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
      SUBROUTINE HM_READ_MAT32(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   ,
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW32 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
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
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER,INTENT(INOUT)                         :: IPM(NPROPMI)
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: ISRATE,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC,IR0

      my_real
     .   YOUNG, ANU, CA, CE, CN, EPSM, SIGM, CM, EPS0, G, E0, C0, C1,
     .   E1MN2, EN1N2, SDSP, EPST1, EPSM1, EPST2, EPSM2, DMAX1, DMAX2,
     .   AMU, R00, R45, R90, R, H, A11, A22, A1122, A12, RHO0, RHOR

      LOGICAL :: IS_ENCRYPTED,IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NFUNC=0
      NUVAR=0
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE=0
      IMATVIS=1

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E'        ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,ANU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_FLOATV('MAT_SIGY'     ,CA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'     ,CE       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'     ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'      ,EPSM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'      ,SIGM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-4
      CALL HM_GET_FLOATV('MAT_SRP'      ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRC'      ,CM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-5
      CALL HM_GET_FLOATV('MAT_R00'      ,R00      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R45'      ,R45      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R90'      ,R90      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iyield'   ,IR0      ,IS_AVAILABLE, LSUBMODEL)

      IF(RHOR==ZERO)RHOR=RHO0
      PM(01)=RHOR
      PM(89)=RHO0

      IF(ANU==HALF)ANU=ZEP499
      IF(R00==ZERO) R00   = ONE
      IF(R45==ZERO) R45   = ONE
      IF(R90==ZERO) R90   = ONE
      IF(CA==ZERO)  CA    = INFINITY
      IF(CN==ZERO)  CN    = ONE
      IF(EPSM==ZERO) EPSM  = INFINITY
      IF(SIGM==ZERO) SIGM  = INFINITY
      IF(CM==ZERO)  EPS0  = ONE
C
      G=YOUNG/(TWO*(ONE + ANU))
      E0=ZERO
      C0=ZERO
      C1=YOUNG/(THREE*(ONE  - TWO*ANU))
      E1MN2=YOUNG/(ONE-ANU**2)
      EN1N2=ANU*E1MN2
      SDSP =SQRT(YOUNG/MAX(PM(1),EM20))
      AMU=PM(17)
      R = FOURTH * (R00 + TWO*R45 + R90)
      H = R / (ONE + R)
      A11   = H * (ONE + ONE/R00)
      A22   = H * (ONE + ONE/R90)
      A1122 = H * TWO
      A12   = H * TWO * (R45 + HALF) * (ONE/R00 + ONE/R90)
      IF (IR0 > 0) THEN
       A22=A22/A11
       A1122=A1122/A11
       A12=A12/A11
       A11=ONE
      END IF
      ICC = 0
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
      PM(39)=CE
      PM(40)=CN
      PM(41)=EPSM
      PM(42)=SIGM
      PM(43)=CM
      PM(44)=EPS0
      PM(45)=A11
      PM(46)=A22
      PM(47)=A1122
      PM(48)=A12
      PM(49)=ICC
      PM(52)=ONEP414*AMU*PM(1)*SDSP
CCC-----------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) = (ONE -TWO*ANU)/(ONE - ANU)

      MTAG%G_SEQ = 1
C      
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,32
      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002)RHO0
        WRITE(IOUT,1300)YOUNG,ANU,G
        WRITE(IOUT,1400)CA,CE,CN,EPSM,SIGM
        WRITE(IOUT,1600)EPS0,CM
        WRITE(IOUT,1700)R00,R45,R90
        IF (IR0 >0) WRITE(IOUT,1110)
      ENDIF
C.....ERRORS
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF(CN>1.) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO)
      ENDIF
      IF(EPS0<=ZERO) THEN
         CALL ANCMSG(MSGID=207,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=32,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
C
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA   = 1
      MTAG%G_EPSD  = 1
c
      MTAG%LY_DIRA = 2
c
      MTAG%L_PLA   = 1
      MTAG%L_EPSD  = 1
c
      MTAG%G_SEQ   = 1
      MTAG%L_SEQ   = 1
c-------------------
      RETURN
c-------------------
 1000 FORMAT(
     & 5X,'  HILL ANISOTROPIC PLASTICITY            '/,
     & 5X,'  ---------------------------            '//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'YOUNG   MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'YIELD COEFFICIENT A . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CE. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CN. . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE COEFFICIENT CM. . . . . . .=',1PG20.13//)
 1700 FORMAT(
     & 5X,'LANKFORD COEFFICIENT R00. . . . . . . .=',1PG20.13/,
     & 5X,'LANKFORD COEFFICIENT R45. . . . . . . .=',1PG20.13/,
     & 5X,'LANKFORD COEFFICIENT R90. . . . . . . .=',1PG20.13/)
 1110 FORMAT(
     & 5X,'YIELD STRESS IN ORTHOTROPIC DIR. 1 IS SUPPOSSD '/)
 999  CALL FREERR(3)
c-----------
      RETURN
      END

