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
Chd|  HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT02(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,NUVAR    ,IFUNC      ,
     .           MAXFUNC  ,NFUNC     ,PARMAT   ,IMATVIS  ,IFORM      ,
     .           UNITAB   ,ID        ,TITR     ,LSUBMODEL,MTAG       ,
     .           PM       ,IPM       ,ISRATE   ,MATPARAM ) 
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
      TYPE(MATPARAM_STRUCT_),INTENT(INOUT)  :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,J  ,ISIZ 
      my_real :: RHOR,RHO0
      INTEGER ICC,ISRAT1,IFLAG,MFLAG
      my_real
     .   YOUNG, ANU, CA, CB, CN, EPSM, SIGM, CC, EPS0, G, E0, C0, C1,
     .   E1MN2, EN1N2, SDSP,C3,C4,CP,TI,TM,MT,FCUT,FISOKIN,CB0,RM,AG,CN0,
     .   FAC_DENS,FAC_PRES,EPS0_UNIT,FAC_M,FAC_L,FAC_T
      CHARACTER PREDEF*16
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      IFLAG   = 0
      MFLAG   = 0
      EPSM    = ZERO
      SIGM    = ZERO
      CC      = ZERO
      EPS0    = ZERO
      ICC     = 0
      FCUT    = ZERO
      FISOKIN = ZERO
      C3      = ZERO
      C4      = ZERO
      CP      = ZERO
      TI      = ZERO
      YOUNG   = ZERO
      ANU     = ZERO
      CA      = ZERO
      CB      = ZERO
      CN      = ZERO
      NUVAR   = 0
      NUPARAM = 0
      NFUNC   = 0
      RHOR    = ZERO
      SDSP    = ZERO

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      
      IF (IFORM==2) THEN
         !/MAT/PLAS_PREDEF
        CALL HM_GET_STRING('Material_Name_Str'  ,PREDEF     ,16, IS_AVAILABLE)
        IF     (PREDEF(1:5)  == 'STEEL')    THEN
          MFLAG = 1
        ELSEIF (PREDEF(1:3)  == 'HSS')      THEN
          MFLAG = 2
        ELSEIF (PREDEF(1:4)  == 'UHSS')     THEN
          MFLAG = 3
        ELSEIF (PREDEF(1:6)  == 'AA5182')   THEN
          MFLAG = 4
        ELSEIF (PREDEF(1:9)  == 'AA6082-T6')THEN
          MFLAG = 5
        ELSEIF (PREDEF(1:7)  == 'PA6GF30')  THEN
          MFLAG = 6
        ELSEIF (PREDEF(1:5)  == 'PPT40')    THEN
          MFLAG = 7
        ELSE
          MFLAG =999 ! to unplug parameter check with PLAS_JOHNS and PLAS_ZERIL
            CALL ANCMSG(MSGID=769,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  C2=PREDEF)
             GOTO 999
        ENDIF          
      ELSE IF (IFORM==1) THEN  ! Zerilli-Armstrong
        !line-1
        CALL HM_GET_FLOATV('RHO_I'    ,RHO0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('RHO_O'    ,RHOR   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !line-2
        CALL HM_GET_FLOATV('E'        ,YOUNG  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Nu'       ,ANU    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !line-3
        CALL HM_GET_FLOATV('C0'       ,CA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('C5'       ,CB     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('n'        ,CN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('EPS_max'  ,EPSM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('sig_max'  ,SIGM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !line-4
        CALL HM_GET_FLOATV('C1'       ,CC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('EPS_0'    ,EPS0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('ICC'      ,ICC    ,IS_AVAILABLE, LSUBMODEL) 
        CALL HM_GET_INTV  ('Fsmooth'  ,ISRATE ,IS_AVAILABLE, LSUBMODEL) 
        CALL HM_GET_FLOATV('Fcut'     ,FCUT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !line-5
        CALL HM_GET_FLOATV('C3'       ,C3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('C4'       ,C4      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('rhoC_p'   ,CP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('T_r'      ,TI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !units for default values
        CALL HM_GET_FLOATV_DIM('EPS_0'      ,EPS0_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('E'          ,FAC_PRES    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('RHO_I'      ,FAC_DENS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ELSE IF (IFORM==0) THEN  ! Johnson-Cook
        !line-1
        CALL HM_GET_FLOATV('RHO_I'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('RHO_O'  ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !line-2
        CALL HM_GET_FLOATV('E'      ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Nu'     ,ANU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('Iflag'  ,IFLAG    ,IS_AVAILABLE, LSUBMODEL) 
        !line-3
        IF (IFLAG == 1) THEN
          CALL HM_GET_FLOATV('SIG_Y'        ,CA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('UTS'          ,CB     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('EUTS'         ,CN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('EPS_p_max'    ,EPSM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('SIG_max0'     ,SIGM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
        ELSE
          CALL HM_GET_FLOATV('a'            ,CA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('b'            ,CB     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('n'            ,CN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('EPS_p_max'    ,EPSM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('SIG_max0'     ,SIGM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDIF
        !line-4
        CALL HM_GET_FLOATV('c'              ,CC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('EPS_DOT_0'      ,EPS0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('ICC'            ,ICC       ,IS_AVAILABLE, LSUBMODEL) 
        CALL HM_GET_INTV  ('Fsmooth'        ,ISRATE    ,IS_AVAILABLE, LSUBMODEL) 
        CALL HM_GET_FLOATV('F_cut'          ,FCUT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Chard'          ,FISOKIN   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !line-5
        CALL HM_GET_FLOATV('m'              ,C3        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('T_melt'         ,C4        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('rhoC_p'         ,CP        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('T_r'            ,TI        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        !units for default values
        CALL HM_GET_FLOATV_DIM('EPS_DOT_0'  ,EPS0_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('E'          ,FAC_PRES    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV_DIM('RHO_I'      ,FAC_DENS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ENDIF

      IF(MFLAG == 0) THEN
        ! Exponent must be set to one if equals to zero in the input
        ! in any case (Iflag=0 or Iflag=1)
        IF (CN == ZERO) CN = ONE
        ! If Iflag == 1, parameters B and n must be recomputed
        IF (IFLAG == 1) THEN
          CB0 = CB
          RM  = CB *(ONE+CN)
          AG  = LOG(ONE+CN)
          CN0 = CN
          CN  = RM*AG / (RM-CA)
          CB  = RM/(CN*AG**(CN-ONE))
          IF (CN > ONE) THEN
            CN = ONE
            CB = (CB0*(ONE+CN0)-CA)/(LOG(1+CN0)-CB0*(1+CN0)/YOUNG-CA/YOUNG)
            CALL ANCMSG(MSGID=277,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID, 
     .                  C1=TITR)
          ENDIF
          IF (CN < ZERO .AND. CB < ZERO) THEN
            CN = ZERO
            CB = ZERO          
            CALL ANCMSG(MSGID=278,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID, 
     .                  C1=TITR)
          ENDIF
        ENDIF
      ELSE      ! small material database, materials as in /FAIL/BiQUAD
        IF (MFLAG >= 1) THEN ! Mild Seel unit = Ton, sec, mm
          FAC_M = FAC_M_WORK                        
          FAC_L = FAC_L_WORK
          FAC_T = FAC_T_WORK
          FAC_PRES = FAC_M/ (FAC_L*FAC_T*FAC_T)                      
          FAC_DENS = FAC_M/ (FAC_L*FAC_L*FAC_L)
          SELECT CASE (MFLAG)
            CASE(1)                                ! Mild steel
c!            RHO0  = 7.85d-9          * FAC_DENS
              RHO0  = 7850d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
c!            YOUNG = 210000.0d0       * FAC_PRES
              YOUNG = 210000000000.0d0 / FAC_PRES
              ANU   = 0.3d0
              CA    = 160000000.0d0    / FAC_PRES
              CB    = 513330169.33870d0/ FAC_PRES      
              CN    = 0.3257084899598d0
            CASE(2)                                ! HSS steel
              RHO0  = 7850d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 210000000000.0d0 / FAC_PRES
              ANU   = 0.3d0
              CA    = 300000000.0d0    / FAC_PRES
              CB    = 611407465.14830d0/ FAC_PRES      
              CN    = 0.3967613457219d0
            CASE(3)
              RHO0  = 7850d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 210000000000.0d0 / FAC_PRES
              ANU   = 0.3d0
              CA    = 500000000.0d0    / FAC_PRES
              CB    = 1306278496.3090d0/ FAC_PRES      
              CN    = 6.4633693574514d-02
            CASE(4)                                   ! Aluminium AA5182
              RHO0  = 2700d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 70000000000.0d0  / FAC_PRES
              ANU   = 0.33d0
              CA    = 150000000.0d0    / FAC_PRES
              CB    = 393050051.47810d0/ FAC_PRES      
              CN    = 0.3719059188570d0
            CASE(5)                                   ! Aluminium AA6082-T6
              RHO0  = 2700d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 70000000000.0d0  / FAC_PRES
              ANU   = 0.33d0
              CA    = 300000000.0d0    / FAC_PRES
              CB    = 210717297.9723d0 / FAC_PRES      
              CN    = 0.3369645584879d0
            CASE(6)                                    ! Plastic PA6GF30  
              RHO0  = 1300d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 7000000000.0d0   / FAC_PRES
              ANU   = 0.35d0
              CA    = 50000000.0d0     / FAC_PRES
              CB    = 60557060.655832d0/ FAC_PRES      
              CN    = 3.8843615080968d-02
            CASE(7)                                    ! GENERIC PP T40
              RHO0  = 1200d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 4000000000d0     / FAC_PRES
              ANU   = 0.3d0
              CA    = 20000000.0d0     / FAC_PRES
              CB    = 18439331.380790d0/ FAC_PRES      
              CN    = 0.1570297693511d0
            CASE DEFAULT                               ! ELSE --> Mild Seel
              RHO0  = 7850d0           / FAC_DENS
              PM(1) = RHO0
              PM(89)= RHO0
              YOUNG = 210000000000d0   / FAC_PRES
              ANU   = 0.3d0
              CA    = 160000000.0d0    / FAC_PRES
              CB    = 513330169.33870d0/ FAC_PRES      
              CN    = 0.3257084899598d0
          END SELECT
        ENDIF
      ENDIF       
C-----
      IF(CC > ZERO .AND. EPS0 > ZERO  .AND. FCUT == ZERO) THEN
           CALL ANCMSG(MSGID=1220,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID, 
     .                 C1=TITR)
      ENDIF
      !FCUT = FCUT / FAC_T
      IF (FCUT == ZERO) FCUT=EP20
      IF(RHOR==ZERO) RHOR=RHO0
C-----
      IF (ANU == HALF) ANU=ZEP499
C
      IF(ICC == 0) ICC=1
C
      IF(EPSM == ZERO) EPSM  = EP20
      IF(SIGM == ZERO) SIGM  = EP20
      IF (CC == ZERO)   THEN
        EPS0   = ONE
        ISRATE = 0
      ELSE
        ISRATE = 1
      END IF
      IF(ANU<=-ONE)  THEN       
        CALL ANCMSG(MSGID=300,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=2,I2=ID,C1=TITR)
      ENDIF
C-----
      G=YOUNG/(TWO*(ONE + ANU))
      E0=ZERO
      C0=ZERO
      C1=YOUNG/(THREE*(ONE - TWO*ANU))
      E1MN2=YOUNG/(ONE - ANU**2)
      EN1N2=ANU*E1MN2
      SDSP =SQRT(YOUNG/MAX(RHOR,EM20))       

      IF (YOUNG<=ZERO) THEN
        CALL ANCMSG(MSGID=276,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=2,I2=ID,C1=TITR)
      ENDIF
C-----
      PM(1) =RHOR
      PM(89)=RHO0      
      PM(20)=YOUNG
      PM(21)=ANU
      PM(22)=G
      PM(23)=E0
      PM(24)=E1MN2
      PM(25)=EN1N2
      PM(26)=FIVE
      PM(26)=PM(26)*ONE_OVER_6
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
      PM(49)=ICC
      PM(9)=FCUT*TWO*PI   
            
C
      IF(TI<=ZERO) TI=TWOHUNDRED98
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_EPSD  = 1   
      MTAG%G_PLA   = 1   
      MTAG%G_DMG   = 1
c
      MTAG%L_EPSD  = 1   
      MTAG%L_PLA   = 1   
      MTAG%L_SIGB  = 6
      MTAG%L_DMG   = 1
c--------------------- 
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCREMENTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"LARGE_STRAIN") 
C--------------------------------
      
      IF(IFORM==0)THEN
        WRITE(IOUT,1010) TRIM(TITR),ID
      ELSEIF(IFORM==1)THEN
        WRITE(IOUT,1011) TRIM(TITR),ID
      ELSEIF(IFORM == 2) THEN
        WRITE(IOUT,1012)TRIM(TITR),ID
      ENDIF
      WRITE(IOUT,1000)
      IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1100)RHO0
        IF(MFLAG /= 0 .AND. .NOT.IS_ENCRYPTED) THEN
          IF(MFLAG == 1) THEN 
            WRITE (IOUT,1407) 'GENERIC MILD STEEL',RHO0,YOUNG,ANU,G,
     .             160000000.0d0/FAC_PRES,380000000.0d0/FAC_PRES,0.24d0
          ENDIF
          IF(MFLAG == 2) THEN
            WRITE (IOUT,1407) 'GENERIC HSS STEEL',RHO0,YOUNG,ANU,G,
     .             300000000.0d0/FAC_PRES,510000000.0d0/FAC_PRES,0.23d0
          ENDIF
          IF(MFLAG == 3) THEN
            WRITE (IOUT,1407) 'GENERIC UHSS STEEL',RHO0,YOUNG,ANU,G,
     .             500000000.0d0/FAC_PRES,1500000000.0d0/FAC_PRES,0.045d0
          ENDIF
          IF(MFLAG == 4) THEN 
            WRITE (IOUT,1407) 'GENERIC ALUMINIUM: AA5182',RHO0,YOUNG,ANU,G,
     .             150000000.0d0/FAC_PRES,300000000.0d0/FAC_PRES,0.25d0
         ENDIF
          IF(MFLAG == 5) THEN 
            WRITE (IOUT,1407) 'GENERIC ALUMINIUM: AA6082-T6',RHO0,YOUNG,ANU,G,
     .             300000000.0d0/FAC_PRES,360000000.0d0/FAC_PRES,0.08d0
          ENDIF
          IF(MFLAG == 6) THEN
            WRITE (IOUT,1407) 'GENERIC PA6GF30',RHO0,YOUNG,ANU,G,
     .             50000000.0d0/FAC_PRES,100000000.0d0/FAC_PRES,0.02d0
          ENDIF
          IF(MFLAG == 7) THEN
            WRITE (IOUT,1407) 'GENERIC PP T40',RHO0,YOUNG,ANU,G,
     .             20000000.0d0/FAC_PRES,30000000.0d0/FAC_PRES,0.06d0
          ENDIF
          IF(MFLAG > 7)  THEN
            WRITE (IOUT,1407) 'GENERIC MILD STEEL',RHO0,YOUNG,ANU,G,
     .             160000000.0d0/FAC_PRES,380000000.0d0/FAC_PRES,0.24d0
          ENDIF
        ENDIF 

        IF(IFORM /= 2)THEN
          IF(.NOT.IS_ENCRYPTED)THEN
           WRITE(IOUT,1300)YOUNG,ANU,G
          ELSE
           WRITE(IOUT,1700)
          ENDIF
        ENDIF
      IF(IFORM == 0 .OR. IFORM == 2)THEN
        IF(IFLAG == 0 .AND. IFORM /= 2)THEN
          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1400)CA,CB,CN,EPSM,SIGM,FISOKIN
          ELSE
          IF(.NOT.IS_ENCRYPTED .AND. MFLAG == 0 .AND. IFORM /= 2)THEN 
            WRITE(IOUT,1405)CA,CB0,CN0,CA,CB,CN,EPSM,SIGM,FISOKIN
          ENDIF
          IF(.NOT.IS_ENCRYPTED .AND. MFLAG /= 0 .AND. IFORM /= 2)THEN 
            WRITE(IOUT,1400)CA,CB,CN,EPSM,SIGM,FISOKIN
          ENDIF
        ENDIF  
        MT=C3
        IF(MT == ZERO)MT=ONE
        TM=C4
        IF(TM == ZERO)TM=EP20
        IF(.NOT.IS_ENCRYPTED .AND. IFORM /= 2)THEN
         ISRAT1 = 1
         WRITE(IOUT,1600)CC,EPS0,ICC,ISRAT1,FCUT,MT,TM,CP,TI
        ENDIF
        PM(50)=ZERO
        PM(51)=MT
        PM(52)=TM
      ELSE
        IF(.NOT.IS_ENCRYPTED .AND. IFORM /= 2)THEN
         WRITE(IOUT,1410)CA,CB,CN,EPSM,SIGM,FISOKIN
         ISRAT1 = 1
         WRITE(IOUT,1610)ICC,ISRAT1,FCUT,EPS0,CC,C3,C4,CP,TI
        ENDIF
        PM(50)=ONE
        PM(51)=C3
        PM(52)=C4
      ENDIF
      IF(CP<=ZERO) THEN
          PM(53)=ZERO
      ELSE
          PM(53)=ONE/CP
      ENDIF
      PM(54)=TI
C -----------------     
      PM(55)=FISOKIN
C      
      IF (FISOKIN>ONE.OR.FISOKIN<ZERO) THEN
        CALL ANCMSG(MSGID=912,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=ID,C1='2',
     .              C2=TITR)
      END IF
C
      IF(CA<=ZERO) THEN
         CALL ANCMSG(MSGID=301,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=2,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
      IF(CN>1.) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=2,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
      IF(EPS0 == ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=2,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
C-----------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) = TWO*G/(C1+FOUR_OVER_3*G)
C-----------
      RETURN
C-----------
 999  CALL FREERR(3)
      RETURN
C-----------
 1010  FORMAT(//
     & 5X,A,/,
     & 5X,40HMATERIAL NUMBER . . . . . . . . . . . .=,I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=         PLAS_JOHNS',/)
 1011  FORMAT(//
     & 5X,A,/,
     & 5X,40HMATERIAL NUMBER . . . . . . . . . . . .=,I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=         PLAS_ZERIL',/)
 1012  FORMAT(//
     & 5X,A,/,
     & 5X,40HMATERIAL NUMBER . . . . . . . . . . . .=,I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=         PLAS_PREDEF',/)
 1000 FORMAT(
     & 5X,'  ELASTIC PLASTIC LAW                   ',/,
     & 5X,'  -------------------                   ',//)
 1100 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13//)
 1400 FORMAT(
     & 5X,'JOHNSON COOK MODEL :',/,
     & 5X,'YIELD COEFFICIENT A . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT B . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT N . . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ISO-KINEMATIC HARDENING FACTOR. . . . .=',1PG20.13//)
 1405 FORMAT(
     & 5X,'JOHNSON COOK MODEL :',/,
     & 5X,'YIELD STRESS  . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ULTIMATE STRESS (UTS) . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN AT UTS (Ag). . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT A . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT B . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT N . . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ISO-KINEMATIC HARDENING FACTOR. . . . .=',1PG20.13//)
 1407 FORMAT(
     & 5X,'PREDEFINED VALUES USED FOR. . . . . . .: ',A/,
     & 5X,'DENSITY . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD STRESS. . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ULTIMATE STRESS (UTS) . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN AT UTS (Ag). . . . . . . . . . .=',1PG20.13//)
 1410 FORMAT(
     & 5X,'ZERILLI ARMSTRONG MODEL :',/,
     & 5X,'YIELD COEFFICIENT C0. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT C5. . . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD COEFFICIENT N . . . . . . . . . .=',1PG20.13/,
     & 5X,'EPS-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SIG-MAX . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ISO-KINEMATIC HARDENING FACTOR. . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE ON SIG-MAX . . . .=',I10/,
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . .=',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13/,
     & 5X,'TEMPERATURE EXPONENT. . . . . . . . . .=',1PG20.13/,
     & 5X,'MELTING TEMPERATURE K . . . . . . . . .=',1PG20.13/,
     & 5X,'SPECIFIC HEAT Rho*Cp. . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE K . . . . . . . . .=',1PG20.13//)
 1610 FORMAT(
     & 5X,'FLAG FOR STRAIN RATE ON SIG-MAX . . . .=',I10/,
     & 5X,'SMOOTH STRAIN RATE OPTION . . . . . . .=',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE COEFFICIENT C1. . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE COEFFICIENT C3. . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE COEFFICIENT C4. . . . . . .=',1PG20.13/,
     & 5X,'SPECIFIC HEAT Rho*Cp. . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE K . . . . . . . . .=',1PG20.13//)
 1700 FORMAT(5X,'CONFIDENTIAL DATA'//)
C-----------
      END
