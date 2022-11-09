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
Chd|  HM_READ_MAT12                 source/materials/mat/mat012/hm_read_mat12.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT12(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM     ,PM,
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS ,ISRATE,
     .           MATPARAM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW21 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXEOS
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT)                 :: MTAG
      TYPE(EOS_TAG_) , TARGET, DIMENSION(0:MAXEOS) ,INTENT(OUT) :: EOS_TAG      
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER ICC,IFLAG
      my_real
     .   N12, N23, N31, E11, E22, E33, G12, G23, G31, SIGT1, SIGT2,
     .   SIGT3, DELTA, CA, CB, CN, FMAX, SIGYT1, SIGYT2, SIGYC1,
     .   SIGYC2,SIGYT3,SIGYC3,SIGYT12, SIGYC12, SIGYT23, SIGYC23, 
     .   SIGY12T,SIGYT13, SIGYC13, F3,F33,
     .   F6,F66,F13, ALPHA, EFIB, EPSFT,
     .   EPSFC, C11, C22, C33, C12, C13, C23, DETC, D11, D12, D13, D22,
     .   D23, D33, D21, D31, D32, A11, A12, A13, A22, A23, A33, C1, SSP,
     .   F1, F2, F4, F5, F11, F22, F44, F55, F12, F23, FT1, FT2,
     .   CC,EPS0,DMIN,WPLAREF,RHOR,RHO0
      CHARACTER*64 :: chain      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_EA'       ,E11       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EB'       ,E22       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EC'       ,E33       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_PRAB'     ,N12       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PRBC'     ,N23       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PRCA'     ,N31       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_GAB'      ,G12       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'      ,G23       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA'      ,G31       ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_SIGT1'    ,SIGT1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGT2'    ,SIGT2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGT3'    ,SIGT3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DAMAGE'   ,DELTA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_BETA'     ,CB        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'     ,CN        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'      ,FMAX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('WPREF'        ,WPLAREF   ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_SIGYT1'   ,SIGYT1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_SIGYT2'   ,SIGYT2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_SIGYC1'   ,SIGYC1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_SIGYC2'   ,SIGYC2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)     

      CALL HM_GET_FLOATV('MAT_SIGT12'   ,SIGYT12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('MAT_SIGC12'   ,SIGYC12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('MAT_SIGT23'   ,SIGYT23   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('MAT_SIGC23'   ,SIGYC23   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    

      CALL HM_GET_FLOATV('MAT_SIGYT3'   ,SIGYT3    ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_SIGYC3'   ,SIGYC3    ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_SIGYT13'  ,SIGYT13   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
      CALL HM_GET_FLOATV('MAT_SIGYC13'  ,SIGYC13   ,IS_AVAILABLE, LSUBMODEL, UNITAB)    

      CALL HM_GET_FLOATV('MAT_ALPHA'    ,ALPHA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EFIB'     ,EFIB      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRC'      ,CC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP'      ,EPS0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV  ('STRFLAG'      ,ICC       ,IS_AVAILABLE,LSUBMODEL)

      IF (RHOR==ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0

      IF(E11 == ZERO .OR.E22 == ZERO .OR. E33 == ZERO) THEN
         CALL ANCMSG(MSGID=306,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=12,
     .               I2=MAT_ID,
     .               C1=TITR,
     .               C2='E11, E22 or E33')
      ENDIF
      IF(N2D > 0) THEN
         CALL ANCMSG(MSGID=305,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=12,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
C             

      
      IF(WPLAREF == ZERO) WPLAREF = ONE    
      IF(ALPHA==ONE)ALPHA= ZEP99
      IF(DELTA==ZERO)DELTA= FIVEEM2
      IF(SIGT1==ZERO)SIGT1= INFINITY
      IF(SIGT2==ZERO)SIGT2= SIGT1
      IF(SIGT3==ZERO)SIGT3= SIGT1
      IF(SIGYT1==ZERO)SIGYT1= INFINITY
      IF(SIGYC1==ZERO)SIGYC1= SIGYT1
      IF(SIGYT2==ZERO)SIGYT2= SIGYT1
      IF(SIGYC2==ZERO)SIGYC2= SIGYC1
      IF(SIGYT3 == ZERO) SIGYT3 = SIGYT2
      IF(SIGYC3 == ZERO) SIGYC3 = SIGYC2
      IF(SIGYT12 == ZERO) SIGYT12 = INFINITY
      IF(SIGYC12 == ZERO) SIGYC12 = INFINITY
      IF(SIGYT23 == ZERO) SIGYT23 = INFINITY
      IF(SIGYC23 == ZERO) SIGYC23 = INFINITY 
      IF(SIGYT13 == ZERO) SIGYT13 = SIGYT12 
      IF(SIGYC13 == ZERO) SIGYC13 = SIGYC12   
      CA=ONE
      IF(CN==ZERO)   CN   = ONE
      IF(FMAX==ZERO)FMAX= EP10
      IF(CC==ZERO)  EPS0  = ONE
      IF(ICC==0.)   ICC  = 1
      EPSFC=-INFINITY
      EPSFT= INFINITY
C-------------------------------------
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,12
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0      
        WRITE(IOUT,1100) E11,E22,E33
        WRITE(IOUT,1200) N12,N23,N31
        WRITE(IOUT,1300) G12,G23,G31
        WRITE(IOUT,1400) SIGT1,SIGT2,SIGT3,DELTA
        WRITE(IOUT,1510) CB,CN,FMAX,WPLAREF
        WRITE(IOUT,1550) SIGYT1,SIGYT2,SIGYC1,SIGYC2
        WRITE(IOUT,1560) SIGYT12,SIGYC12,SIGYT23,SIGYC23
        WRITE(IOUT,1610) ALPHA,EFIB
        WRITE(IOUT,1620) CC,EPS0,ICC
      ENDIF
C--------------------------------------------------------
      C11 = ONE/E11
      C22 = ONE/E22
      C33 = ONE/E33
      C12 =-N12/E11
      C13 =-N31/E33
      C23 =-N23/E22
C-------------------------
C     INVERSION
C-------------------------
      DETC= C11*C22*C33-C11*C23*C23-C12*C12*C33+C12*C13*C23
     +     +C13*C12*C23-C13*C22*C13
      IF(DETC <= ZERO) THEN
         CALL ANCMSG(MSGID=307,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      D11 = (C22*C33-C23*C23)/DETC
      D12 =-(C12*C33-C13*C23)/DETC
      D13 = (C12*C23-C13*C22)/DETC
      D22 = (C11*C33-C13*C13)/DETC
      D23 =-(C11*C23-C13*C12)/DETC
      D33 = (C11*C22-C12*C12)/DETC
      D21 = D12
      D31 = D13
      D32 = D23
C-----------------------
C     VERIF
      A11 = C11*D11+C12*D21+C13*D31
      A12 = C11*D12+C12*D22+C13*D32
      A13 = C11*D13+C12*D23+C13*D33
      A22 = C12*D12+C22*D22+C23*D23
      A23 = C12*D13+C22*D23+C23*D33
      A33 = C13*D13+C23*D23+C33*D33
C-----------------------------------------
      PM(20)= MAX(E11,E22,E33)
      PM(21)= THIRD*(N12+N31+N23)
      PM(22)= THIRD*(G12+G23+G31)
      PM(25)= 1.
      PM(26)= CB
      PM(27)= CN
C 
      PM(28) = MAX(ONEP0001,FMAX)
      C1  = MAX(D11,D22,D33)
      SSP = SQRT(C1/MAX(PM(1),EM20))
      PM(32)= C1
      PM(37)=-INFINITY
      PM(38)= EFIB
      PM(39)= ALPHA
C
C     STORE D-MATRIX FOR THE COMPOSITE AS A WHOLE
C
C
      PM(40)= D11
      PM(41)= D12
      PM(42)= D13
      PM(43)= D22
      PM(44)= D23
      PM(45)= D33
      PM(46)= G12
      PM(47)= G23
      PM(48)= G31
C
      PM(49)= SSP 
      PM(68)=WPLAREF
      
C
C     CONSTANTS FOR THE YIELD FUNCTION
C  
      F1  = ONE/SIGYT1-ONE/SIGYC1
      F2  = ONE/SIGYT2-ONE/SIGYC2
      F3  = ONE/SIGYT3-ONE/SIGYC3
      F4  = ONE/SIGYT12-ONE/SIGYC12
      F5  = ONE/SIGYT23-ONE/SIGYC23
      F6  = ONE/SIGYT13-ONE/SIGYC13      
      F11 = ONE/(SIGYT1*SIGYC1)
      F22 = ONE/(SIGYT2*SIGYC2)
      F33 = ONE/(SIGYT3*SIGYC3)
      F44 = ONE/(SIGYT12*SIGYC12)
      F55 = ONE/(SIGYT23*SIGYC23)
      F66 = ONE/(SIGYT13*SIGYC13)      
      F12 = -HALF/SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2)
      F23 = -HALF/SQRT(SIGYT2*SIGYC2*SIGYT3*SIGYC3)
      F13= - HALF/SQRT(SIGYT1*SIGYC1*SIGYT3*SIGYC3)
      FT1=F11*F22- FOUR*F12**2
      FT2=F22**2- FOUR*F23**2
      IF(.NOT.IS_ENCRYPTED)THEN
       WRITE(IOUT,1650) F1,F2,F3,F4,F5,F6,F11,F22,F33,F44,F55,F66,F12,F23,F13,FT1,FT2
      ENDIF
      PM(50)=CC
      PM(51)=EPS0
      PM(52)=ICC
      IF(EPS0==ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=12,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
      PM(53)=F1
      PM(54)=F2
      PM(55)=F3
      PM(56)=F4
      PM(57)=F5
      PM(58)=F6
      PM(59)=F11
      PM(60)=F22
      PM(61)=F33
      PM(62)=F44
      PM(63)=F55
      PM(64)=F66
      PM(65)=F12
      PM(66)= F23
      PM(67)= F13 
      PM(73)=C11
      PM(74)=C22
      PM(75)=C33
      PM(76)=C12
      PM(77)=C23
      PM(78)=C13
      PM(79)=DELTA
      PM(81)= SIGT1
      PM(82)= SIGT2
      PM(83)= SIGT3
      PM(84)= EPSFT
      PM(85)= EPSFC
CC     Formulation for solid elements time step computation.
      IPM(252)=1
      DMIN  = MIN(D11*D22-D12**2,D22*D33-D23**2,D11*D33-D13**2)
      PM(105) =DMIN/C1**2    
C-----------------------------------------
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA    = 1    
      MTAG%L_PLA    = 1
      MTAG%L_SIGF   = 1
      MTAG%L_EPSF   = 1
      MTAG%L_DAM    = 5
      MTAG%L_EPE    = 6  ! strain in crack directions
      MTAG%G_TSAIWU = 1
      MTAG%L_TSAIWU = 1
c-----------
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c----------------------------------------
      IF(.NOT.IS_ENCRYPTED)THEN
        WRITE(IOUT,'(A)')' COMPOSITE CONSTITUTIVE LAW'
        WRITE(IOUT,'(    1P3G20.13 )') PM(40),PM(41),PM(42)
        WRITE(IOUT,'(20X,1P2G20.13 )') PM(43),PM(44)
        WRITE(IOUT,'(40X, 1PG20.13/)') PM(45)
      ENDIF
C--------------------------------
      RETURN
C
 1000 FORMAT(
     & 5X,'  ORTHOTROPIC MATERIAL WITH FIBERS      ',/,
     & 5X,'  ---------------------------------     ',//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG MODULUS E11 . . . . . . . . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS E22 . . . . . . . . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS E33 . . . . . . . . . . .=',1PG20.13//)
 1200 FORMAT(
     & 5X,'POISSON RATIO N12 . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO N23 . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO N31 . . . . . . . . . . .=',1PG20.13//)
 1300 FORMAT(
     & 5X,'SHEAR MODULUS   G12 . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS   G23 . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS   G31 . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'MATRIX TENSILE FAILURE STRESS DIR-1 . .=',1PG20.13/,
     & 5X,'MATRIX TENSILE FAILURE STRESS DIR-2 . .=',1PG20.13/,
     & 5X,'MATRIX TENSILE FAILURE STRESS DIR-3 . .=',1PG20.13/,
     & 5X,'DAMAGE PARAMETER . . . . . . . . . . . =',1PG20.13//)
 1500 FORMAT(
     & 5X,'COMPOSITE YIELD STRESS  (A) . . . . . .=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (B). . .=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (N). . .=',1PG20.13/,
     & 5X,'COMPOSITE MAXIMUM STRESS. . . . . . . .=',1PG20.13//)
 1510 FORMAT(
     & 5X,'COMPOSITE HARDENING PARAMETER  (B). . .=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (N). . .=',1PG20.13/,
     & 5X,'COMPOSITE MAXIMUM YIELD FUNCTION FMAX .=',1PG20.13/,
     & 5X,'REFERENCE PLASTIC WORK WPLAREF        .=',1PG20.13//)
 1550 FORMAT(
     & 5X,'COMPOSITE YIELD IN TRACTION (1) . . . .=',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN TRACTION (2-3) . .  =',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN COMPRESSION (1)  .  =',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN COMPRESSION (2-3).  =',1PG20.13//)
 1560 FORMAT(
     & 5X,'COMPOSITE YIELD IN SHEAR (+12) . . . . =',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN SHEAR (-12) . . . . =',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN SHEAR (+23) . . . . =',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN SHEAR (-23) . . . . =',1PG20.13//)
 1600 FORMAT(
     & 5X,'FIBER VOLUME FRACTION . . . . . . . . .=',1PG20.13/,
     & 5X,'FIBER MODULUS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FIBER TENSILE FAILURE STRAIN. . . . . .=',1PG20.13/,
     & 5X,'FIBER COMPRESSIVE FAILURE STRAIN. . . .=',1PG20.13//)
 1610 FORMAT(
     & 5X,'FIBER VOLUME FRACTION . . . . . . . . .=',1PG20.13/,
     & 5X,'FIBER MODULUS . . . . . . . . . . . . .=',1PG20.13//)
 1620 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE ON F-MAX Wp-MAX. .=',I10//)
 1650 FORMAT(
     & 5X,'  YIELD FUNCTION CONSTANTS              ,'/,
     & 5X,'  ------------------------              ,'//,
     & 5X,'F1 , F2 , F3  . . . . . . . . . . . . .=',3E12.4/,
     & 5X,'F4 , F5 , F6. . . . . . . . . . . . . .=',3E12.4/,
     & 5X,'F11, F22, F33  . . . . . . . . . . . . =',3E12.4/,
     & 5X,'F44, F55, F66 . . . . . . . . . . . . . =',3E12.4/,
     & 5X,'F12, F23, F13 . . . . . . . . . . . . ..=',3E12.4/,
     & 5X,'FT1, FT2. . . . . . . . . . . . . . . .=',2E12.4//)
      RETURN
      END



