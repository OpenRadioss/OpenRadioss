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
Chd|  HM_READ_MAT116                source/materials/mat/mat116/hm_read_mat116.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT116(MTAG     ,UPARAM    ,MAXUPARAM,NUPARAM  ,PM       ,
     .                          PARMAT   ,NUVAR     ,IFUNC    ,NFUNC    ,MAXFUNC  ,
     .                          UNITAB   ,MAT_ID    ,TITR     ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW116 WITH HM READER
C-----------------------------------------------
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
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
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
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER, INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,
     .                                   NUPARAM,NUVAR
      INTEGER, INTENT(IN)             :: MAT_ID
      my_real, INTENT(INOUT)          :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(OUT)    :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NRATE,ILAW,IMASS,IDEL,IORDER1,IORDER2,
     .   IFAIL1,IFAIL2,ICRIT
      my_real :: RHO0,E,G,THICK,GC1_INI,GC2_INI,GC1_INF,GC2_INF,RATG1,RATG2,
     .   FG1,FG2,SIGA1,SIGA2,SIGB1,SIGB2,RATE1,RATE2,UNIT_L,ALPHA
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      ILAW = 116
!
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
!
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
Card1
      CALL HM_GET_FLOATV('MAT_RHO'     ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E'       ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G'       ,G        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_THICK'   ,THICK    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IMASS'   ,IMASS    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_IDEL'    ,IDEL     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_ICRIT'   ,ICRIT    ,IS_AVAILABLE, LSUBMODEL)
Card2
      CALL HM_GET_FLOATV('MAT_GC1_ini' ,GC1_INI  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GC1_inf' ,GC1_INF  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRATG1'  ,RATG1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FG1'     ,FG1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_FLOATV('MAT_GC2_ini' ,GC2_INI  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GC2_inf' ,GC2_INF  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRATG2'  ,RATG2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_FG2'     ,FG2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4
      CALL HM_GET_FLOATV('MAT_SIGA1'   ,SIGA1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGB1'   ,SIGB1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRATE1'  ,RATE1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_ORDER1'  ,IORDER1  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_FAIL1'   ,IFAIL1   ,IS_AVAILABLE, LSUBMODEL)
Card5
      CALL HM_GET_FLOATV('MAT_SIGA2'   ,SIGA2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGB2'   ,SIGB2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRATE2'  ,RATE2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_ORDER2'  ,IORDER2  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_FAIL2'   ,IFAIL2   ,IS_AVAILABLE, LSUBMODEL)
c---------------------------------------------------------------------------------
      PM(1) = RHO0  ! RHOR
      PM(89)= RHO0
c-------------------
c     Default Values
c-------------------
      IF (G  == ZERO) G = E
      IF (IMASS == 0)   IMASS   = 1
      IF (IDEL  == 0)   IDEL    = 1
      IF (IORDER1 == 0) IORDER1 = 1
      IF (IORDER2 == 0) IORDER2 = 1
      IF (IFAIL1  == 0) IFAIL1  = 1
      IF (IFAIL2  == 0) IFAIL2  = 1
      IF (ICRIT   == 0) ICRIT   = 1
      IF (FG1 == ZERO .or. GC1_INI == ZERO) IFAIL1 = 0
      IF (FG2 == ZERO .or. GC2_INI == ZERO) IFAIL2 = 0
      IF (THICK == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('MAT_THICK' ,UNIT_L ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        THICK = ONE * UNIT_L
      ENDIF
      ALPHA  = 0.005   ! strain rate filtering coefficient (exp average)
c-------------------
c     Check parameter values
c-------------------
      IF (IFAIL1 == 1) THEN
        IF (FG1 >= ONE - HALF*SIGA1**2 / (E * GC1_INI)) THEN
          CALL ANCMSG(MSGID=1825,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1 = MAT_ID,
     .                C1 = TITR,
     .                C2 = 'FG1',
     .                R1 = HALF*SIGA1**2 / (E * GC1_INI)  )
        END IF
      ELSE IF (IFAIL1 == 2) THEN
        IF (FG1 >= ONE) THEN
          CALL ANCMSG(MSGID=1825,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1 = MAT_ID,
     .                C1 = TITR,
     .                C2 = 'FG1',
     .                R1 = ONE)
        END IF
      END IF
c
      IF (IFAIL2 == 1) THEN
        IF (FG2 >= ONE - HALF*SIGA2**2 / (E * GC2_INI)) THEN
          CALL ANCMSG(MSGID=1825,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1 = MAT_ID,
     .                C1 = TITR,
     .                C2 = 'FG2',
     .                R1 = HALF*SIGA2**2 / (E * GC2_INI)  )
        END IF
      ELSE IF (IFAIL2 == 2) THEN
        IF (FG2 >= ONE) THEN
          CALL ANCMSG(MSGID=1825,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1 = MAT_ID,
     .                C1 = TITR,
     .                C2 = 'FG2',
     .                R1 = ONE)
        END IF
      END IF
c-------------------
      NFUNC   = 0
      NUPARAM = 25
      NUVAR   = 12
c-------------------
      UPARAM(1)  = E / THICK
      UPARAM(2)  = G / THICK
      UPARAM(3)  = IMASS
      UPARAM(4)  = IDEL
      UPARAM(5)  = GC1_INI
      UPARAM(6)  = GC1_INF
      UPARAM(7)  = RATG1
      UPARAM(8)  = FG1
      UPARAM(9)  = GC2_INI
      UPARAM(10) = GC2_INF
      UPARAM(11) = RATG2
      UPARAM(12) = FG2
      UPARAM(13) = SIGA1
      UPARAM(14) = SIGB1
      UPARAM(15) = RATE1
      UPARAM(16) = IORDER1
      UPARAM(17) = IFAIL1
      UPARAM(18) = SIGA2
      UPARAM(19) = SIGB2
      UPARAM(20) = RATE2
      UPARAM(21) = IORDER2
      UPARAM(22) = IFAIL2
      UPARAM(23) = ICRIT
      UPARAM(24) = THICK
      UPARAM(25) = ALPHA
C----------------
      PARMAT(1)  = MAX(E,G) / THREE
      PARMAT(2)  = MAX(E,G)
      PARMAT(17) = ONE ! (ONE - TWO*NU)/(ONE - NU), NU=0
c
      MTAG%G_PLA  = 2
      MTAG%G_EPSD = 1
      MTAG%L_PLA  = 2
      MTAG%L_EPSD = 1
      MTAG%L_EPE  = 3
      MTAG%L_DMG  = 1
c-------------------
c     STARTER Output
c-------------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,E,G,THICK,IMASS,IDEL,ICRIT,
     .                   GC1_INI,GC1_INF,RATG1,FG1,
     .                   GC2_INI,GC2_INF,RATG2,FG2,
     .                   SIGA1,SIGB1,RATE1,IORDER1,IFAIL1,
     .                   SIGA2,SIGB2,RATE2,IORDER2,IFAIL2
      ENDIF
c-----------
      RETURN
c-----------
 1000 FORMAT(
     & 10X,' MIXED MODE COHESIVE LAW 116  ',/,
     & 10X,' ---------------------------  ',/)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'MATERIAL DENSITY. . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS PER THICKNESS UNIT . . . .     =',1PG20.13/,
     & 5X,'SHEAR MODULUS PER THICKNESS UNIT . . . .     =',1PG20.13/,
     & 5X,'COHESIVE ELEMENT THICKNESS . . . . . . .     =',1PG20.13/,
     & 5X,'MASS CALCULATION FLAG . . . . . . . . . . . .=',I10/,
     & 5X,'             = 0 => USING VOLUME  DENSITY     '/,
     & 5X,'             = 1 => USING SURFACE DENSITY     '/,
     & 5X,'NB OF FAILING GAUSS POINTS TO DELETE ELEMENT =',I10/,
     & 5X,'ICRIT : YIELD AND DAMAGE INITIATION FLAG . . =',I10/,
     & 5X,'INITIAL ENERGY RELEASE RATE IN MOD I . . . . =',1PG20.13/,
     & 5X,'FINAL ENERGY RELEASE RATE IN MOD I   . . . . =',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE FOR GC IN MOD I. . . . =',1PG20.13/,
     & 5X,'SHAPE FACTOR FOR GC AT FAIL IN MOD I . . . . =',1PG20.13/,
     & 5X,'INITIAL ENERGY RELEASE RATE IN MOD II  . . . =',1PG20.13/,
     & 5X,'FINAL ENERGY RELEASE RATE IN MOD II  . . . . =',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE FOR GC IN MOD II . . . =',1PG20.13/,
     & 5X,'SHAPE FACTOR FOR GC AT FAIL IN MOD II  . . . =',1PG20.13/,
     & 5X,'STATIC YIELD STRESS TERM IN MODE I   . . . . =',1PG20.13/,
     & 5X,'DYNAMIC YIELD STRESS TERM IN MODE I  . . . . =',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE FOR YLD IN MODE I. . . =',1PG20.13/,
     & 5X,'ORDER OF YIELD FUNCTION IN MODE I  . . . . . =',I10/,
     & 5X,'FAILURE CRITERION FLAG IN MODE I   . . . . . =',I10/,
     & 5X,'STATIC YIELD STRESS TERM IN MODE II  . . . . =',1PG20.13/,
     & 5X,'DYNAMIC YIELD STRESS TERM IN MODE II . . . . =',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE FOR YLD IN MODE II . . =',1PG20.13/,
     & 5X,'ORDER OF YIELD FUNCTION IN MODE II . . . . . =',I10/,
     & 5X,'FAILURE CRITERION FLAG IN MODE II  . . . . . =',I10/)
c--------
      END
