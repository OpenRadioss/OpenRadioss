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
Chd|  HM_READ_MAT26                 source/materials/mat/mat026/hm_read_mat26.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        MINTP_RE                      ../common_source/eos/mintp_re.F
Chd|        MINTP_RT                      ../common_source/eos/mintp_rt.F
Chd|        MRDSE2                        source/materials/mat/mat026/mrdse2.F
Chd|        MRDSE3                        source/materials/mat/mat026/mrdse3.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT26(
     .              MTAG     ,PM     ,MAT_ID   ,TITR  ,IPM     ,
     .              JTHE     ,BUFMAT ,MFI      ,IDF   ,LSUBMODEL,
     .              UNITAB   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT)                       :: JTHE,MFI,IDF
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(IN)                          :: MAT_ID
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      my_real, DIMENSION(LUNIT,NUNITS) ,INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT)                 :: MTAG
      my_real ,DIMENSION(*), INTENT(INOUT)         :: BUFMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I1, MAT, NR, NT, IDR, IDT, IDP, IDE, IDQ,ILAW
      my_real
     .   YOUNG, ANU, CA, CB, CN, EPSM, SIGM, E0, CC, EPS0, CM, TMELT,
     .   TMAX, G, C1, RHO, UNIT, STATUS, FORM, XNR, XNT, T0, P0, DPDR,
     .   XKL, XLAMB, ATOM, SIG, XKMAX, RHO0, DYDZ,RHOR,EPS0_UNIT

      CHARACTER(ncharline) ::  FILE, VIDE
!
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      ILAW = 26
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
!-----------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!-----------------------
C License Flag
      LIDFS = 0
      VIDE=
     .'                                                                '
!     LINE 1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 2
      CALL HM_GET_FLOATV('MAT_E'        ,YOUNG      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,ANU        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 3
      CALL HM_GET_FLOATV('MAT_SIGY'     ,CA         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'     ,CB         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'     ,CN         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'      ,EPSM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'      ,SIGM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 4
      CALL HM_GET_FLOATV('MAT_E0'       ,E0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     LINE 5
      CALL HM_GET_STRING('SSAM301'      ,FILE       ,ncharline ,IS_AVAILABLE)
!     LINE 6
      CALL HM_GET_FLOATV('MAT_SRC'      ,CC         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'      ,EPS0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_M'        ,CM         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMELT'    ,TMELT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX'     ,TMAX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!     DEFAULT UNITS
      CALL HM_GET_FLOATV_DIM('MAT_SRP'  ,EPS0_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!
      IF (CN == ZERO .OR. CN == ONE) CN = ONEP0001
      IF (EPSM == ZERO) EPSM = INFINITY
      IF (SIGM == ZERO) SIGM = INFINITY
      IF (CC == ZERO)   EPS0 = ONE*EPS0_UNIT
      IF (CM == ZERO)   CM   = ONE  ! dimensionless
      IF (TMELT == ZERO)TMELT = INFINITY
      IF (TMAX == ZERO) TMAX  = INFINITY
!
      G=YOUNG/(TWO*(ONE+ANU))
      C1=YOUNG/(THREE*(ONE-TWO*ANU))
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
      PM(20)= YOUNG
      PM(21)= ANU
      PM(22)= G
      PM(23)= E0
      PM(31)= ZERO
      PM(32)= C1
      PM(38)= CA
      PM(39)= CB
      PM(40)= CN
      PM(41)= EPSM
      PM(42)= SIGM
      PM(43)= CC
      PM(44)= EPS0
      PM(45)= CM
      PM(46)= TMELT
      PM(47)= TMAX
      PM(80)= TMELT
C--------------------------------------
C     SESAME
C--------------------------------------
!-------------
!     SESAM301
!-------------
C
      IF(LEN(TRIM(FILE))==0)THEN
        CALL ANCMSG(MSGID=19,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=MAT_ID,C1='MATERIAL',C2='MATERIAL',C3=TITR,C4=TRIM(FILE))
      ELSE
        OPEN(UNIT=31,FILE=FILE,STATUS='OLD',FORM='FORMATTED',ERR=998)
      ENDIF
C
      READ(31,*) I1,MAT
      READ(31,'(2E15.0)') XNR,XNT
      REWIND(31)
      NR = NINT(XNR)
      NT = NINT(XNT)
      PM(25)=NR
      PM(26)=NT
      PM(27)=IDF
      IDR = IDF
      IDT = IDR + NR
      IDP = IDT + NT
      IDE = IDP + NR * NT
      IDF = IDE + NR * NT
      MFI = MFI + IDF - IDR
C
      BUFMAT(IDR:IDF-1) = ZERO
C
      CALL MRDSE2(BUFMAT(IDR),NR,
     +     BUFMAT(IDT),NT,BUFMAT(IDP),BUFMAT(IDE))
      CLOSE(31)
      PM(25)=NR
      PM(26)=NT
C
       CALL MINTP_RE(BUFMAT(IDR),NR,
     +     BUFMAT(IDT),NT,BUFMAT(IDE),RHO0,T0,E0/RHO0,DYDZ)
       CALL MINTP_RT(BUFMAT(IDR),NR,
     +     BUFMAT(IDT),NT,BUFMAT(IDP),RHO0,T0,P0,DPDR)
C
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,RHOR
        WRITE(IOUT,1300) YOUNG,ANU,G
        WRITE(IOUT,1400) CA,CB,CN,EPSM,SIGM
!!        WRITE(IOUT,1500) TRIM(FILE),P0,T0,E0
        WRITE(IOUT,1500) FILE(1:LEN(TRIM(FILE))),P0,T0,E0
        WRITE(IOUT,1600) CC,EPS0,CM,TMELT,TMAX
      ENDIF
C
      IF (EPS0 == ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=26,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
C
      SIG  = ZERO
      XKMAX = INFINITY
      FILE = VIDE
!-------------
!     SESAM504
!-------------
!     LINE 7
      CALL HM_GET_STRING('SSAM504'      ,FILE       ,ncharline ,IS_AVAILABLE)
!     LINE 8
      CALL HM_GET_FLOATV('K_Lor'        ,XKL        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Lamda'    ,XLAMB      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A'        ,ATOM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K'        ,XKMAX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!
      IF (FILE /= VIDE) JTHE = 1
      IF (XKMAX ==ZERO) XKMAX = INFINITY
!
      IF (JTHE /= 0) THEN
        IF(LEN(TRIM(FILE))==0)THEN
          CALL ANCMSG(MSGID=19,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=MAT_ID,C1='MATERIAL',C2='MATERIAL',C3=TITR,C4=TRIM(FILE))
        ELSE
          OPEN(UNIT=31,FILE=FILE,STATUS='OLD',FORM='FORMATTED',ERR=998)
        ENDIF
        READ(31,*) I1,MAT
        READ(31,'(2E15.0)') XNR,XNT
        REWIND(31)
        NR = NINT(XNR)
        NT = NINT(XNT)
        PM(28)=NR
        PM(29)=NT
        PM(30)=IDF
        IDR = IDF
        IDT = IDR + NR
        IDQ = IDT + NT
        IDF = IDQ + NR * NT
        MFI = MFI + IDF - IDR
C
        BUFMAT(IDR:IDF-1) = ZERO
C
        CALL MRDSE3(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDQ))
        CLOSE(31)
        PM(35)=XKL
        PM(36)=XLAMB
        PM(37)=ATOM
!
!!        PM(69)=INFINITY
!
!!        WRITE(IOUT,1700) TRIM(FILE),XKL,XLAMB,ATOM,XKMAX
        WRITE(IOUT,1700) FILE(1:LEN(TRIM(FILE))),XKL,XLAMB,ATOM,XKMAX
      ENDIF ! IF (JTHE /= 0)
!-------------
!     SESAM502
!-------------
!     LINE 9
      CALL HM_GET_STRING('SSAM502'      ,FILE       ,ncharline ,IS_AVAILABLE)
!     LINE 10
      CALL HM_GET_FLOATV('Sigma_k'      ,SIG        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!
      IF (SIG > ZERO) THEN
        IF(LEN(TRIM(FILE))==0)THEN
          CALL ANCMSG(MSGID=19,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=MAT_ID,C1='MATERIAL',C2='MATERIAL',C3=TITR,C4=TRIM(FILE))
        ELSE
          OPEN(UNIT=31,FILE=FILE,STATUS='OLD',FORM='FORMATTED',ERR=998)
        ENDIF
        READ(31,*)I1,MAT
        READ(31,'(2E15.0)')XNR,XNT
        REWIND(31)
        NR = NINT(XNR)
        NT = NINT(XNT)
        PM(48)=NR
        PM(49)=NT
        PM(50)=IDF
        IDR = IDF
        IDT = IDR + NR
        IDQ = IDT + NT
        IDF = IDQ + NR * NT
        MFI = MFI + IDF - IDR
C
        BUFMAT(IDR:IDF-1) = ZERO
C 
        CALL MRDSE3(BUFMAT(IDR),NR,BUFMAT(IDT),NT,BUFMAT(IDQ))
        CLOSE(31)
       ENDIF
!!       IF (JTHE /= 0 .AND. .not. IS_ENCRYPTED) WRITE(IOUT,1800)TRIM(FILE),SIG
       IF (JTHE /= 0 .AND. .not. IS_ENCRYPTED) WRITE(IOUT,1800)FILE(1:LEN(TRIM(FILE))),SIG
C
      PM(51)=SIG
      PM(52)=XKMAX
!
!----------------------
      JTHE = 0 !
!----------------------
!
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA   = 1
      MTAG%G_TEMP  = 1
      MTAG%G_EPSD  = 1
c
      MTAG%L_PLA   = 1
      MTAG%L_TEMP  = 1
      MTAG%L_EPSD  = 1
      MTAG%L_SSP   = 1
      MTAG%L_Z     = 1    ! taux ionisation
c-----------
      RETURN
c-----------
  998 CONTINUE
      CALL ANCMSG(MSGID=19,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=MAT_ID,
     .            C1='MATERIAL',
     .            C2='MATERIAL',
     .            C3=TITR,
     .            C4=TRIM(FILE))
      RETURN
c-----------
 1000 FORMAT(
     & 5X,40H  JOHNSON COOK - SESAME LAW               ,/,
     & 5X,40H  -----------------------               ,//)
 1100 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,40HYOUNG'S MODULUS . . . . . . . . . . . .=,E12.4/,
     & 5X,40HPOISSON'S RATIO . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS . . . . . . . . . . . . .=,E12.4//)
 1400 FORMAT(
     & 5X,40HYIELD COEFFICIENT CA. . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD COEFFICIENT CB. . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD COEFFICIENT CN. . . . . . . . . .=,E12.4/,
     & 5X,40HEPS-MAX . . . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSIG-MAX . . . . . . . . . . . . . . . .=,E12.4//)
 1500 FORMAT(
     & 5X,'SESAME EOS TABLE :',A/,
     & 5X,40HINITIAL PRESSURE. . . . . . . . . . . .=,E12.4/,
     & 5X,40HINITIAL TEMPERATURE . . . . . . . . . .=,E12.4/,
     & 5X,40HINITIAL INTERNAL ENERGY PER UNIT VOLUME=,E12.4//)
 1600 FORMAT(
     & 5X,40HSTRAIN RATE COEFFICIENT CC. . . . . . .=,E12.4/,
     & 5X,40HREFERENCE STRAIN RATE . . . . . . . . .=,E12.4/,
     & 5X,40HTEMPERATURE EXPONENT. . . . . . . . . .=,E12.4/,
     & 5X,40HMELTING TEMPERATURE DEGREE K. . . . . .=,E12.4/,
     & 5X,40HTHETA-MAX . . . . . . . . . . . . . . .=,E12.4//)
 1700 FORMAT(
     & 5X,'ELECTRON THERMAL CONDUCTIVITY',/,
     & 5X,'-----------------------------',/,
     & 5X,'SESAME IONIZATION TABLE :',A/,
     & 5X,40HLORENTZ CONDUCTIVITY COEFFICIENT. . . .=,E12.4/,
     & 5X,40HLAMBDA COEFFICIENT. . . . . . . . . . .=,E12.4/,
     & 5X,40HATOMIC WEIGHT . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMAXIMUM CONDUCTIVITY. . . . . . . . . .=,E12.4//)
 1800 FORMAT(
     & 5X,'RADIATION',/,
     & 5X,'---------',/,
     & 5X,'SESAME ROSSELAND OPACITY TABLE :',A/,
     & 5X,40HSTEFAN-BOLTZMANN CONSTANT . . . . . . .=,E12.4//)
c-----------
 999  CALL FREERR(3)
c-----------
      RETURN
      END
c-----------
