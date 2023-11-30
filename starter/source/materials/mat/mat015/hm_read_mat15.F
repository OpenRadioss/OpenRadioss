Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_MAT15                 source/materials/mat/mat015/hm_read_mat15.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT15(
     .             LSUBMODEL,MTAG     ,UNITAB   ,IPM      ,PM       ,
     .             MAT_ID   ,TITR     ,MATPARAM ,ISRATE   ,PARMAT   )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW15 WITH HM READER 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD    
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)   :: MTAG
      CHARACTER*nchartitle, INTENT(IN)       :: TITR
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                    :: MAT_ID
      INTEGER, INTENT(INOUT) :: IPM(NPROPMI),ISRATE
      my_real, INTENT(INOUT) :: PM(NPROPM)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IOFF,ICC,IFLAG
      my_real
     .   N12, N21, E11, E22, G12, G23, G31, EPST1, EPST2, CB, CN,
     .   FMAX, SIGYT1, SIGYT2, SIGYC1, SIGYC2, SIGYT12, SIGYC12, 
     .   C1, SSP, F1, F2, F11, F22, F33, F12, FT1, WPLAMX,
     .   EPSM1, EPSM2, DMAX, SHRDAM, SHRMAX, SHRDMAX,ALPHA,CC,EPS0,
     .   CBT1,CNT1,SIGMXT1,CCT1,CBT2,CNT2,SIGMXT2,CCT2,DETC, DE,
     .   CBC1,CNC1,SIGMXC1,CCC1,CBC2,CNC2,SIGMXC2,CCC2,FCUT,
     .   CBT12,CNT12,SIGMXT12,CCT12,CBC12,CNC12,SIGMXC12,CCC12,
     .   EPS1T1,EPS2T1,SIGRST1,WPLAMXT1,EPS1T2,EPS2T2,SIGRST2,
     .   WPLAMXT2,EPS1C1,EPS2C1,SIGRSC1,WPLAMXC1,EPS1C2,EPS2C2,
     .   SIGRSC2,WPLAMXC2,EPS1T12,EPS2T12,SIGRST12,WPLAMXT12,
     .   BETA, TMAX,S1,S2,S12,C11,C22,WPLAREF,RHO0,RHOR
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C     S o u r c e 
C-----------------------------------------------
      MTAG%L_DAM    = 2         ! DAMT => EV(NB15)
      MTAG%L_DSUM   = 6         ! SIGR => EV(NB16)

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED) 
    
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (RHOR == ZERO) THEN
         RHOR = RHO0
      ENDIF
      PM(1) = RHOR
      PM(89) = RHO0

      CB=ZERO
      CN=ZERO
      FMAX=ZERO 
C
      SIGYT1=ZERO
      SIGYT2=ZERO
      SIGYC1=ZERO   
      SIGYC2=ZERO     
      SIGYC12=ZERO
C
      EPS1T1 = ZERO
      EPS2T1 = ZERO
C
      EPS1T2=ZERO
      EPS2T2=ZERO
      ISRATE = 0
      FCUT = ZERO

      CALL HM_GET_FLOATV('MAT_EA', E11, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EB', E22, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PRAB', N12, IS_AVAILABLE, LSUBMODEL, UNITAB)
     
      CALL HM_GET_FLOATV('MAT_GAB', G12, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC', G23, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA', G31, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_BETA', CB, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD', CN, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG', FMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
                          
      CALL HM_GET_FLOATV('WPMAX', WPLAMX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('WPREF', WPLAREF, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Itype', IOFF, IS_AVAILABLE, LSUBMODEL)
                                 
      CALL HM_GET_FLOATV('MAT_SIGYT1', SIGYT1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGYT2', SIGYT2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGYC1', SIGYC1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGYC2', SIGYC2, IS_AVAILABLE, LSUBMODEL, UNITAB)                     
      CALL HM_GET_FLOATV('MAT_ALPHA', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)             

      CALL HM_GET_FLOATV('MAT_SIGC12', SIGYT12, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGT12', SIGYC12, IS_AVAILABLE, LSUBMODEL, UNITAB)               
      CALL HM_GET_FLOATV('MAT_SRC', CC, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRP', EPS0, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('STRFLAG', ICC, IS_AVAILABLE, LSUBMODEL)
        
      CALL HM_GET_FLOATV('MAT_Beta', BETA, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_TMAX', TMAX, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MCHANG_S1', S1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MCHANG_S2', S2, IS_AVAILABLE, LSUBMODEL, UNITAB)                     
      CALL HM_GET_FLOATV('MCHANG_S12', S12, IS_AVAILABLE, LSUBMODEL, UNITAB)             

      CALL HM_GET_INTV('Fsmooth', ISRATE, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut', FCUT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MCHANG_C1', C11, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MCHANG_C2', C22, IS_AVAILABLE, LSUBMODEL, UNITAB)
    
      IF(E11==ZERO .OR. E22==ZERO .OR.
     &   G12==ZERO .OR. G23==ZERO.OR. G31==ZERO) THEN
         CALL ANCMSG(MSGID=306,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=15,
     .               I2=MAT_ID,
     .               C1=TITR,
     .               C2='E11, E22, G12, G23, G31')
      ENDIF                      
C-----
       IF(CC > ZERO .AND. EPS0 > ZERO  .AND. ISRATE == 0) THEN
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID, 
     .                C1=TITR)
       ENDIF 
C-------------------------------------
       IF (ICC/=2.AND.ISRATE==0.AND.FCUT/=ZERO) ISRATE=1
       IF (ISRATE > 0 .AND. FCUT == ZERO) FCUT = INFINITY
       IF(CN==ZERO)    CN    = ONE
       IF(FMAX==ZERO)   FMAX  = INFINITY    
       IF(WPLAMX==ZERO)WPLAMX = INFINITY
       IF (WPLAREF == ZERO) WPLAREF = ONE        
       WPLAMX = WPLAMX / WPLAREF
       IF(ALPHA==ZERO)ALPHA = ONE
       IF(CC==ZERO)  EPS0  = ONE
       IF(ICC==ZERO)  ICC  = 1
       IFLAG = 0
       IF(S1==ZERO.AND.S2==ZERO.AND.S12==ZERO.AND.
     . C11==ZERO.AND.C22==ZERO) IFLAG = 1
       
       IF(S1==ZERO) S1 = INFINITY
       IF(S2==ZERO) S2 = INFINITY 
       IF(S12==ZERO) S12 = INFINITY
       IF(C11==ZERO) C11= INFINITY
       IF(C22==ZERO) C22= INFINITY   
C--------------------------------------------------------
      IF(TMAX==ZERO)WRITE(*,*)'STOP ERROR IN TMAX INPUT'   
C--------------------------------------------------------
      N21=N12*E22/E11
      DETC=ONE-N12*N21
      IF(DETC<=ZERO) THEN
         CALL ANCMSG(MSGID=307,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
C--------------------------------------------------------
      IF(CN>ONE) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=15,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
      DE = ONE
C-------------------------------------
      WRITE(IOUT, 900) TRIM(TITR),MAT_ID,15
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT, 950) RHO0
        WRITE(IOUT,1100)E11,E22,N12
        WRITE(IOUT,1300)G12,G23,G31    
C               
        WRITE(IOUT,1510)CB,CN,FMAX,WPLAMX,WPLAREF,IOFF             
        WRITE(IOUT,1550)SIGYT1,SIGYT2,SIGYC1,SIGYC2,ALPHA  
        WRITE(IOUT,1560)SIGYT12,SIGYC12                    
        WRITE(IOUT,1610)CC,EPS0,ICC                        
        WRITE(IOUT,1611) ISRATE,FCUT
        WRITE(IOUT,1600)BETA, TMAX
        WRITE(IOUT,2700) S1,S2,S12,C11,C22        
      ENDIF
C-------------------------------------------------
      PM(20)= MAX(E11,E22)/DETC
      PM(24)= PM(20)
      PM(26)=FIVE_OVER_6
      PM(33)=E11
      PM(34)=E22
      PM(35)=N12
      PM(36)=N21
      PM(37)=G12
      PM(38)=G23
      PM(39)=G31
      PM(41)=WPLAMX
      PM(42)=IOFF
      PM(44)=DE
      PM(46)=CB
      PM(47)=CN
      PM(49)=FMAX
      PM(68)=WPLAREF
      PM(22)= THIRD*(G12+G23+G31)
C-----------------------------------------
      C1  = MAX(E11,E22)/DETC
      SSP = SQRT(MAX(C1,G12,G23,G31)/MAX(PM(1),EM20))
      PM(32)= C1
      PM(27)= SSP
      PM(52)=ONEP414*PM(17)*PM(1)*SSP
C-----------------------------------------
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
C----------------------------------------
C     CONSTANTS FOR THE YIELD FUNCTION
C-------------------------------------------
      IF (SIGYT1<=0.) THEN
         CALL ANCMSG(MSGID=538,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='SIGYT1')
      ENDIF
      IF (SIGYC1<=0.) THEN
         CALL ANCMSG(MSGID=538,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='SIGYC1')
      ENDIF
      IF (SIGYT2<=0.) THEN
         CALL ANCMSG(MSGID=538,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='SIGYT2')
      ENDIF
      IF (SIGYC2<=0.) THEN
         CALL ANCMSG(MSGID=538,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='SIGYC2')
      ENDIF
      IF (SIGYT12<=0.) THEN
         CALL ANCMSG(MSGID=538,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='SIGYT12')
      ENDIF
      IF (SIGYC12<=0.) THEN
         CALL ANCMSG(MSGID=538,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               C2='SIGYC12')
      ENDIF
C
      IF((SIGYC1>0.).AND.(SIGYC2>0.))THEN
       F1  = ONE/SIGYT1-ONE/SIGYC1
       F2  = ONE/SIGYT2-ONE/SIGYC2
       F11 = ONE/(SIGYT1*SIGYC1)
       F22 = ONE/(SIGYT2*SIGYC2)
       F33 = ONE/(SIGYT12*SIGYC12)
       F12 = -ALPHA/(TWO*SQRT(SIGYT1*SIGYC1*SIGYT2*SIGYC2))
C
       FT1=F11*F22-FOUR*F12**2
       IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,1650) F1,F2,F11,F22,F33,F12
       ENDIF
C      IF (FT1<0.) WRITE (IOUT,5560) FT1
C
       PM(54)=F1
       PM(55)=F2
       PM(56)=F11
       PM(57)=F22
       PM(58)=F33
       PM(59)=F12
C
      ELSE
        F1  = 0
        F2  = 0
        F11 = 0
        F22 = 0
        F33 = 0
        F12 = 0
      ENDIF    
      PM(60)= BETA
      PM(61)= TMAX

C
      PM(50)=CC
      PM(51)=EPS0
      PM(53)=ICC
      IF(EPS0==ZERO) THEN
        CALL ANCMSG(MSGID=298,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .               I1=15,
     .               I2=MAT_ID,
     .               C1=TITR)
      ENDIF
C---    
      PM(141)= SIGYT1  
      PM(142)= SIGYT2  
      PM(143)= SIGYC1  
      PM(144)= SIGYC2  
      PM(145)= SIGYT12 
      PM(146)= SIGYC12 
      PM(147) = IFLAG
      PM(148) = S1
      PM(149) = S2
      PM(150) = C11
      PM(151) = C22
      PM(152)  =S12
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA    = 1    
      MTAG%L_PLA    = 1   ! NB10
      MTAG%G_TSAIWU = 1
      MTAG%L_TSAIWU = 1

      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC")

      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ORTHOTROPIC")
      
c-----------
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c-----------
      RETURN
  900 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 950  FORMAT(
     & 5X,'INITIAL DENSITY    . . . . . . . . . . . . .=',1PG20.13/)
 1000 FORMAT(
     & 5X,40H  ORTHOTROPIC MATERIAL FOR SHELLS       ,/,
     & 5X,40H  -------------------------------       ,//)
 1100 FORMAT(
     & 5X,40HYOUNG'S MODULUS E11 . . . . . . . . . .=,E12.4/,
     & 5X,40HYOUNG'S MODULUS E22 . . . . . . . . . .=,E12.4/,
     & 5X,40HPOISSON'S RATIO N12 . . . . . . . . . .=,E12.4/)
 1300 FORMAT(
     & 5X,40HSHEAR MODULUS   G12 . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS   G23 . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS   G31 . . . . . . . . . .=,E12.4/)
c 
 1510 FORMAT(
     & 5X,'COMPOSITE HARDENING PARAMETER  (B). . .=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (N). . .=',1PG20.13/,
     & 5X,'COMPOSITE MAXIMUM YIELD FUNCTION FMAX .=',1PG20.13/,
     & 5X,'COMPOSITE MAXIMUM PLASTIC WORK WPLAMX .=',1PG20.13/,
     & 5X,'REFERENCE PLASTIC WORK WPLAREF. . . . .=',1PG20.13/,
     & 5X,'TOTAL ELEMENT FAILURE CRITERIA    IOFF =',I10/,
     & 5X,' IOFF=0: SHELL DELETE IF WPLA >= WPLAMX FOR 1 LAYER'/,
     & 5X,' IOFF=1: SHELL DELETE IF WPLA >= WPLAMX FOR ALL LAYERS'/,
     & 5X,' IOFF=2: SHELL DELETE IF FOR EACH LAYER :'/,
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1'/,
     & 5X,' IOFF=3: SHELL DELETE IF FOR EACH LAYER :'/,
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 2'/,
     & 5X,' IOFF=4: SHELL DELETE IF FOR EACH LAYER :'/,
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1 AND 2'/,
     & 5X,' IOFF=5: SHELL DELETE IF FOR ALL LAYERS :'/,
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1'/,
     & 5X,'         OR'/,
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 2'/,
     & 5X,' IOFF=6: SHELL DELETE IF FOR EACH LAYER :'/, 
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1 OR 2'//)
 1550 FORMAT(
     & 5X,'COMPOSITE YIELD IN TENSION DIR-1  . . . . . .=',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN TENSION DIR-2  . . . . . .=',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN COMPRESSION DIR-1  . . . .=',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN COMPRESSION DIR-2  . . . .=',1PG20.13/,
     & 5X,'F12 REDUCTION FACTOR . . . . . . . .  . . . .=',1PG20.13//)
 1560 FORMAT(
     & 5X,'COMPOSITE YIELD IN SHEAR (+12) . . . .=',1PG20.13/,
     & 5X,'COMPOSITE YIELD IN SHEAR (-12) . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'SHEAR SCALING FACTOR . . . . . . . . . . .=',1PG20.13/,
     & 5X,'TIME OF RELAXATION. . . . . . . . . . . . .=',1PG20.13/)
 1650 FORMAT(
     & 5X,'  YIELD FUNCTION CONSTANTS              '/,
     & 5X,'  ------------------------              '//,
     & 5X,'F1 , F2 . . . . . . . . . . . . . . . .=',2E12.4/,
     & 5X,'F11, F22. . . . . . . . . . . . . . . .=',2E12.4/,
     & 5X,'F33 . . . . . . . . . . . . . . . . . .=', E12.4/,
     & 5X,'F12 . . . . . . . . . . . . . . . . . .=', E12.4//)
 1610 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE ON F-MAX Wp-MAX. .=',I10//)
 1611 FORMAT(
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . .=',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13/)
 2700 FORMAT(
     & 5X,'LONGITUDINAL TENSILE STRENGTH . . . . . .=',1PG20.13/,
     & 5X,'TRANSVERSE   TENSION STRENGTH . . . . . .=',1PG20.13/,
     & 5X,'SHEAR STRENGTH  . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,' LONGITUDINAL COMPRESSIVE STRENGTH  . . .=',1PG20.13/,
     & 5X,' TRANSVERSE   COMPRESSIVE STRENGTH  . . .=',1PG20.13//)
      RETURN
      END
