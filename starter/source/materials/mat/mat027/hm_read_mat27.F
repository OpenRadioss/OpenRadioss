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
Chd|  HM_READ_MAT27                 source/materials/mat/mat027/hm_read_mat27.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT27(
     .           MTAG     ,PARMAT   ,UNITAB   ,PM       ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
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
      INTEGER, INTENT(IN)    :: MAT_ID
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE
      my_real, DIMENSION(100),INTENT(OUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT) :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ILAW,ICC
      my_real :: RHO0,RHOR,YOUNG,NU,CA,CB,CN,MU,SIGM,CC,EPS0,G,C1,
     .           E1MN2,EN1N2,SDSP,EPST1,EPSM1,EPST2,EPSM2,
     .           DMAX1,DMAX2,EPSF1,EPSF2,FCUT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 27
c--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
c-----------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------
      CALL HM_GET_FLOATV('MAT_E'      ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_SIGY'   ,CA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_BETA'   ,CB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'   ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'    ,SIGM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_SRC'    ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'    ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('STRFLAG'    ,ICC      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Fsmooth'    ,ISRATE   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'       ,FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPST1'  ,EPST1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSM1'  ,EPSM1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DAMAGE' ,DMAX1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSF1'  ,EPSF1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPST2'  ,EPST2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSM2'  ,EPSM2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DAMAGE2',DMAX2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSF2'  ,EPSF2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c-----------------
      IF (ISRATE == 0 .AND. FCUT /= ZERO) ISRATE = 1
      IF (FCUT == ZERO) FCUT = EP20
      IF (NU >= HALF) NU   = ZEP499
      IF (ICC == 0)   ICC = 1
      IF (CA == ZERO) CA  = EP20
      IF (CN == ZERO) CN  = ONE
      IF (SIGM == ZERO)  SIGM  = EP20
      IF (CC == ZERO)    EPS0  = ONE
      IF (EPST1 == ZERO) EPST1 = EP20
      IF (EPSM1 == ZERO) EPSM1 = ONEP1*EP20
      IF (DMAX1 == ZERO) DMAX1 = ZEP999
      IF (EPST2 == ZERO) EPST2 = EP20
      IF (EPSM2 == ZERO) EPSM2 = ONEP1*EP20
      IF (DMAX2 == ZERO) DMAX2 = ZEP999
      IF (EPSF1 == ZERO) EPSF1 = ONEP2*EP20
      IF (EPSF2 == ZERO) EPSF2 = ONEP2*EP20
c-----------------
      G  = HALF*YOUNG/(ONE + NU)
      C1 = YOUNG/(THREE*(ONE - TWO*NU))
      E1MN2 = YOUNG/(ONE-NU**2)
      EN1N2 = NU*E1MN2
      SDSP  = SQRT(YOUNG/MAX(RHO0,EM20))
      MU    = PM(17)
      IF (MU == ZERO) MU = FIVEEM2
c-----------------
      PARMAT(1) = C1
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(100)= C1      
c-----------------
      PM(9)  = FCUT*TWO*PI
      PM(17) = MU
      PM(20) = YOUNG
      PM(21) = NU
      PM(22) = G
      PM(23) = ZERO ! E0
      PM(24) = E1MN2
      PM(25) = EN1N2
      PM(26) = FIVE_OVER_6
      PM(27) = SDSP
      PM(28) = ONE/YOUNG
      PM(29) = -NU*PM(28)
      PM(30) = ONE/G
      PM(31) = ZERO ! C0
      PM(32) = C1
      PM(38) = CA
      PM(39) = CB
      PM(40) = CN
      PM(41) = EP20 ! EPSM
      PM(42) = SIGM
      PM(43) = CC
      PM(44) = EPS0
      PM(49) = ICC
      PM(52) = ONEP414*MU*PM(1)*SDSP
      PM(60) = EPST1
      PM(61) = EPST2
      PM(62) = EPSM1
      PM(63) = EPSM2
      PM(64) = DMAX1
      PM(65) = DMAX2
      PM(66) = EPSF1
      PM(67) = EPSF2
c-----------------
c     Element buffer : internal variable allocation
c-----------------
      MTAG%G_GAMA  = 6  
      MTAG%G_PLA   = 1    
c
      MTAG%L_GAMA  = 6  
      MTAG%L_PLA   = 1
      MTAG%L_DAM   = 2
      MTAG%L_CRAK  = 2
      MTAG%L_STRA  = 6
c-----------------------      
c     Output
c-----------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,27
      WRITE(IOUT,1100)     
      IF (IS_ENCRYPTED) THEN                                     
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) YOUNG,NU,G
        WRITE(IOUT,1400) CA,CB,CN,SIGM
        WRITE(IOUT,1600) CC,EPS0,ICC,ISRATE,FCUT
        WRITE(IOUT,1700) EPST1,EPSM1,DMAX1,EPSF1
        WRITE(IOUT,1800) EPST2,EPSM2,DMAX2,EPSF2
      ENDIF
c-----------------
c     Error handling
c-----------------
      IF (CN > ONE) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=27,
     .               I2=MAT_ID,
     .               C1=TITR)

      ENDIF
      IF (EPS0 <= ZERO) THEN
         CALL ANCMSG(MSGID=207,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF (EPSM1 <= EPST1) THEN
         CALL ANCMSG(MSGID=208,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF (EPSM2 <= EPST2) THEN
         CALL ANCMSG(MSGID=209,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF (DMAX1 >= ONE .OR. DMAX1 < ZERO) THEN
         CALL ANCMSG(MSGID=210,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF (DMAX2 >= ONE .OR. DMAX2 < ZERO) THEN
         CALL ANCMSG(MSGID=211,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
c-----------        
      RETURN
c-----------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT(
     & 5X,'  ELASTIC PLASTIC BRITTLE MATERIAL      '/,
     & 5X,'  --------------------------------      '//)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG   MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'YIELD COEFFICIENT CA. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CB. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT CN. . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE ON SIG-MAX . . . .=',I10//,
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . .=',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'TENSILE FAILURE STRAIN DIR-1. . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM TENSILE STRAIN DIR-1. . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM DAMAGE DIR-1. . . . . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT RUPTURE TENSILE STRAIN DIR-1. .=',1PG20.13/)
 1800 FORMAT(
     & 5X,'TENSILE FAILURE STRAIN DIR-2. . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM TENSILE STRAIN DIR-2. . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM DAMAGE DIR-2. . . . . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT RUPTURE TENSILE STRAIN DIR-2. .=',1PG20.13//)
c-----------
      END
