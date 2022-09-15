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
Chd|  HM_READ_MAT80                 source/materials/mat/mat080/hm_read_mat80.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_MAT80(
     .     UPARAM      ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .     MAXFUNC     ,IFUNC    ,PARMAT   ,MAT_ID   ,PM       ,
     .     ISRATE_IN   ,MTAG     ,TITR     ,UNITAB   ,LSUBMODEL,
     .     ITABLE      ,MAXTABL  ,NUMTABL  ,NVARTMP  ,TABLE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW80 WITH HM READER 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD    
      USE ELBUFTAG_MOD          
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PARMAT(100), UPARAM(MAXUPARAM), PM(NPROPM)
      INTEGER, INTENT(INOUT)                :: IFUNC(MAXFUNC), NFUNC, MAXFUNC, MAXUPARAM, 
     .     NUPARAM, NUVAR,NVARTMP, ISRATE_IN, ITABLE(MAXTABL), NUMTABL
      INTEGER, INTENT(IN)                    :: MAT_ID, MAXTABL
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(INOUT)   :: MTAG
      TYPE(TTABLE) TABLE(NTABLE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MATS,IFLAG1,IFLAG2,ITEMAX,j,FLAGEPS,ISRATE, I,
     .        HEATFLAG,FLAG_HEAT_ID,FLAG_LOC,
     .        FLAG_TR_STRAIN,FLAG_TR_KINETICS,NDIM(5)

      my_real
     .      YSCALE1,YSCALE2,YSCALE3,YSCALE4,YSCALE5,XSCALE(5),RSCALE_UNIT(5),
     .      XSCALE2,XSCALE3,XSCALE4,XSCALE5,EFAC,UNITT,RSCALE(5),
     .      TETA2, TETA3,TETA4, TETA5,QR2,QR3,QR4,ALPHA2, TREF,  
     .      AE1, AE3,BS,MS,GSIZE,B, MO,MN,W,AL,C,CR,SI,CU,AS,
     .      CO,NI,V,P,TI,E,NU,CEPS, PEPS, BULK,CE,HFP,HB,HM,TINI,
     .      ALFA1, ALFA2,KF,KP,LAT1,LAT2,AC1,AC3,TAU1,TAU3,
     .      FCFER,FCPER,FCBAI,FGRAIN,KPER,KBAIN,T1,T2,XEQ2,CEUT,
     .      FLAGFILTRE, ALPHAEPS,xeqtest, RHO0, RHOR, FCUT,
     .      GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .      GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI

      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C     S o u r c e 
C-----------------------------------------------
      ISRATE_IN = 1
      FLAG_LOC  = 0
      
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)     

      NFUNC =7
      NUMTABL = 5 
      NVARTMP = 15

      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (RHOR == ZERO) THEN
         RHOR = RHO0
      ENDIF
      PM(1) = RHOR
      PM(89) = RHO0

      CALL HM_GET_FLOATV('MAT_E'      , E       , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     , NU      , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_fct_IDE', IFUNC(1), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('SCALE'      , EFAC    , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('time_inputunit_value' , UNITT, IS_AVAILABLE, LSUBMODEL, UNITAB)

      IF(UNITT == ZERO) UNITT = THREE*EP03+SIX*EP02

      CALL HM_GET_INTV  ('Fsmooth'    , ISRATE, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'       , FCUT  , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CAP_END', CEPS  , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'     , PEPS  , IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_INTV('FUN_A1', ITABLE(1), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A2', ITABLE(2), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A3', ITABLE(3), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A4', ITABLE(4), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('FUN_A5', ITABLE(5), IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_FLOATV('FScale11', YSCALE1, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22', YSCALE2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33', YSCALE3, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale12', YSCALE4, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale23', YSCALE5, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('scale1', XSCALE(1), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale2', XSCALE(2), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale3', XSCALE(3), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale4', XSCALE(4), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale5', XSCALE(5), IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('FScale11_2', TETA2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22_2', TETA3, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33_2', TETA4, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale12_2', TETA5, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('ALPHA1'       , ALFA1       , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ALPHA2'       , ALFA2       , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('flag_heat'    , HEATFLAG    , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FCT_flag_heat', FLAG_HEAT_ID, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('flag_loc'     , FLAG_LOC    , IS_AVAILABLE, LSUBMODEL)

      CALL HM_GET_FLOATV('qa_l'   , QR2   , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('qb_l'   , QR3   , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Q'      , QR4   , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Alpha_y', ALPHA2, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('WPREF'  , TREF  , IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF(QR2 == ZERO)QR2= 11575.
      IF(QR3 == ZERO)QR3= 13840.
      IF(QR4 == ZERO)QR4= 13588.
      IF(ALPHA2 == ZERO)ALPHA2= 0.011

      AE1 = ZERO
      AE3 = ZERO
      BS  = ZERO
      MS  = ZERO
      CALL HM_GET_FLOATV('PrMesh_Size' , GSIZE, IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_K'       , KF  , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_K_UNLOAD', KP  , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_Lamda'   , LAT1, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_Theta'   , LAT2, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('T_Initial'   , TINI, IS_AVAILABLE, LSUBMODEL, UNITAB)     

      CALL HM_GET_FLOATV('MAT_B'       , B , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_MUE1'    , MO, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_MUE2'    , MN, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_Wmax_pt1', W , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_A1'      , AL, IS_AVAILABLE, LSUBMODEL, UNITAB)     
            
      CALL HM_GET_FLOATV('MAT_C'   , C , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_c1_t', CR, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_SRE' , SI, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_c2_t', CU, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_A2'  , AS, IS_AVAILABLE, LSUBMODEL, UNITAB)     

      CALL HM_GET_FLOATV('MAT_c1_c', CO, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_NUt' , NI, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_VOL' , V , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_PR'  , P , IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('MAT_T0'  , TI, IS_AVAILABLE, LSUBMODEL, UNITAB)     


      !Parameters for austenization during heating phase
      CALL HM_GET_FLOATV('TAU1'  , TAU1, IS_AVAILABLE, LSUBMODEL, UNITAB)     
      CALL HM_GET_FLOATV('TAU3'  , TAU3, IS_AVAILABLE, LSUBMODEL, UNITAB) 

      !flag for transformation strain model
c------------------------------------------------------------------------
      CALL HM_GET_INTV  ('flag_tr_strain'    , FLAG_TR_STRAIN  , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('ID_R_aus' , IFUNC(3), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('ID_R_fer' , IFUNC(4), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('ID_R_Per' , IFUNC(5), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('ID_R_bai' , IFUNC(6), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('ID_R_Mar' , IFUNC(7), IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('FScaleA', RSCALE(1), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScaleF', RSCALE(2), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScaleP', RSCALE(3), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScaleB', RSCALE(4), IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScaleM', RSCALE(5), IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF(RSCALE(1) == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('FScaleA'  ,RSCALE_UNIT(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          RSCALE(1) = RSCALE_UNIT(1)
      ENDIF
      IF(RSCALE(2) == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('FScaleA'  ,RSCALE_UNIT(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          RSCALE(2) = RSCALE_UNIT(2)
      ENDIF
      IF(RSCALE(3) == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('FScaleA'  ,RSCALE_UNIT(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          RSCALE(3) = RSCALE_UNIT(3)
      ENDIF
      IF(RSCALE(4) == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('FScaleA'  ,RSCALE_UNIT(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          RSCALE(4) = RSCALE_UNIT(4)
      ENDIF
      IF(RSCALE(5) == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('FScaleA'  ,RSCALE_UNIT(5)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          RSCALE(5) = RSCALE_UNIT(5)
      ENDIF
c------------------------------------------------------------------------
      CALL HM_GET_INTV  ('flag_tr_kinetics'  , FLAG_TR_KINETICS, IS_AVAILABLE, LSUBMODEL)
c------------------------------------------------------------------------
C     NEW TRANSFORMATION KINETICS -reference PAUL HIPPCHEN 2015
c------------------------------------------------------------------------
      CALL HM_GET_FLOATV('GFAC_F', GFAC_F, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PHI_F' , PHI_F , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PSI_F' , PSI_F , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CR_F'  , CR_F  , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CF'    , CF    , IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('GFAC_P', GFAC_P, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PHI_P' , PHI_P , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PSI_P' , PSI_P , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CR_P'  , CR_P  , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CP'    , CP    , IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('GFAC_B', GFAC_B, IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PHI_B' , PHI_B , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PSI_B' , PSI_B , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CR_B'  , CR_B  , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CB'    , CB    , IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('PHI_M' , PHI_M , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PSI_M' , PSI_M , IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('N_M'   , N_M   , IS_AVAILABLE, LSUBMODEL, UNITAB)
c------------------------------------------------------------------------
c------------------------------------------------------------------------
      IF (FLAG_HEAT_ID /= 0) IFUNC(2) =  FLAG_HEAT_ID
      IF (ISRATE == 0) ISRATE = 1
      IF (FLAG_TR_STRAIN == 0) FLAG_TR_STRAIN = 1

      IF( TAU1 < TAU3) THEN
         CALL ANCMSG(MSGID=1740,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF



      IF(ITABLE(1)==ZERO.OR.ITABLE(2)==ZERO.OR.ITABLE(3)==ZERO.OR. 
     .     ITABLE(4)==ZERO.OR.ITABLE(5)==ZERO)THEN
         CALL ANCMSG(MSGID=1020,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
        
      ENDIF

      DO I = 1,NTABLE
        DO J=1,5
          IF (TABLE(I)%NOTABLE == ITABLE(J)) THEN
           NDIM(J) = TABLE(I)%NDIM
          ENDIF
        ENDDO
      ENDDO
      IF(NDIM(1) == 3 .OR.NDIM(2)==3 .OR.NDIM(3)==3 .OR. 
     .                                   NDIM(4)==3 .OR.NDIM(5)==3 )THEN
         IF(CEPS /= ZERO .OR. PEPS /= ZERO  ) THEN
           CEPS =  ZERO
           PEPS =  ZERO
           CALL ANCMSG(MSGID=2041,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR)
          ENDIF
        
      ENDIF
C
      NUVAR = 44
      IF (ISRATE > 0 .AND. FCUT == ZERO)  FCUT = EP05*FAC_T_WORK  ! default : force filtering
      IF (FLAG_LOC == 0) FLAG_LOC = 2
      IF (YSCALE1 == ZERO)YSCALE1 = ONE
      IF (YSCALE2 == ZERO)YSCALE2 = ONE
      IF (YSCALE3 == ZERO)YSCALE3 = ONE
      IF (YSCALE4 == ZERO)YSCALE4 = ONE
      IF (YSCALE5 == ZERO)YSCALE5 = ONE
      DO I= 1,NUMTABL
        IF (XSCALE(I) == ZERO)XSCALE(I) = ONE
      ENDDO
      BULK=E/THREE/(ONE-TWO*NU)
      CE=SQRT(BULK/RHO0)
C            
      UPARAM(1) = E
      UPARAM(2) = NU
      UPARAM(3) = IFUNC(1)
      IF (EFAC==ZERO)EFAC=ONE
      UPARAM(4) = EFAC     
      UPARAM(10)= YSCALE1
      UPARAM(11)= YSCALE2
      UPARAM(12)= YSCALE3
      UPARAM(13)= YSCALE4
      UPARAM(14)= YSCALE5
      UPARAM(15)= CEPS
      UPARAM(16)= PEPS
      UPARAM(17)= TETA2
      UPARAM(18)= TETA3
      UPARAM(19)= TETA4
      UPARAM(20)= TETA5
      UPARAM(21)= QR2
      UPARAM(22)= QR3
      UPARAM(23)= QR4
      UPARAM(24)= ALPHA2 ! =0.011
      UPARAM(25)= TREF
      AE3= 912.-203.*sqrt(C)-15.2*NI+44.7*SI+104.*V+31.5*MO+13.1*W-30.*MN-11.*CR-20.*CU+700.*P+400.*AL+120.*AS+400.*TI+273.0
      AE1= 723.-10.7*MN-16.9*NI+29.*SI+16.9*CR+290.*AS+ 6.4 *W + 273.0
      BS = 656.-58.*C-35.*MN-75.*SI-15.*NI-34.*CR-41.*MO +273.0
      MS = 561.-474.*C-33.*MN-17.*NI-17.*CR-21.*MO +273.0
      UPARAM(26)= AE1 
      UPARAM(27)= AE3 
      UPARAM(28)= BS
      UPARAM(29)= MS 
      UPARAM(30)= GSIZE
      UPARAM(31)= ALFA1  
      UPARAM(32)= ALFA2

c---- COMPOSITION FOR FERRITE PEARLITE BAINITE      
      FCFER =1/(59.6*MN+1.45*NI+67.7*CR+244.0*MO+KF*B)
      FCPER =1/(1.79+5.42*(CR+MO+FOUR*MO*NI)+KP*B)
      FCBAI =1/((2.34+10.1*C+3.8*CR+19.0*MO)*EM04)


      IF(CF == ZERO) CF = FCFER
      IF(CP == ZERO) CP = FCPER
      IF(CB == ZERO) CB = FCBAI

      FGRAIN=TWO**((GSIZE-ONE)*HALF)
      UPARAM(33)= FCFER
      UPARAM(34)= FCPER  
      UPARAM(35)= FCBAI
      UPARAM(36)= FGRAIN
      
      KPER=0.01*C+0.52*MO
      UPARAM(37)= KPER
      
      KBAIN= 1.9*C+2.5*MN+0.9*NI+1.7*CR+4*MO-2.6
      UPARAM(38)= KBAIN
      
      T1=912.0-15.2*NI+44.7*SI+104.0*V+315.0*MO+13.1*W
      T2=30.0*MN+11.0*CR+20.0*CU-700.0*P-400.0*AL-120.0*AS-400.0*TI
      CEUT= (T1-T2-AE1-273.)*(T1-T2-AE1-273.)/203.0/203.0
      XEQ2= (CEUT-C)/CEUT
      UPARAM(39)= XEQ2

      UPARAM(40)= LAT1
      UPARAM(41)= LAT2      
     
      HFP=42.+223.*C+53.*SI+30.*MN+12.*NI+7.*CR+19.*MO+(10.-19.*SI+4.*NI+8.*CR+130.*V)
      HB =259.4-254.7*C+4834.1*C*C
      HM =181.1+2031.9*C-1940.1*C*C
      UPARAM(42)= HFP
      UPARAM(43)= HB
      UPARAM(44)= HM
     
      UPARAM(45)= TINI
      UPARAM(46)= UNITT      
C
      NUPARAM= 46
      UPARAM(46+1) = 0.
      UPARAM(46+2) = 0.
      UPARAM(46+3) = 0.125
      UPARAM(46+4) = 2.530
      UPARAM(46+5) = 0.250
      UPARAM(46+6) = 4.000
      UPARAM(46+7) = 0.500
      UPARAM(46+8) = 2.760
      UPARAM(46+9) = 0.750
      UPARAM(46+10)= 1.330
      UPARAM(46+11)= 1.000
      UPARAM(46+12)= 1.000
      
      NUPARAM= NUPARAM +12 !58

      DO I= 1,NUMTABL
      UPARAM(58 +I) = ONE/XSCALE(I)
      ENDDO
      UPARAM(58 + NUMTABL + 1) = HEATFLAG

      UPARAM(58 + NUMTABL + 2) = TAU1
      UPARAM(58 + NUMTABL + 3) = TAU3
      UPARAM(58 + NUMTABL + 4) = FLAG_LOC

      UPARAM(58 + NUMTABL + 5) = FLAG_TR_STRAIN
      UPARAM(58 + NUMTABL + 6) = FLAG_TR_KINETICS 

      !UPARAM(58 + NUMTABL + 6) = IFUNC(1)!FLAG_HEAT_ID

      NUPARAM= NUPARAM   + NUMTABL  + 6 !2for transformation strain ! 58 + 5 + 6 = 69
      UPARAM(58 + NUMTABL + 7) = RSCALE(1)
      UPARAM(58 + NUMTABL + 8) = RSCALE(2)
      UPARAM(58 + NUMTABL + 9) = RSCALE(3)
      UPARAM(58 + NUMTABL +10) = RSCALE(4)
      UPARAM(58 + NUMTABL +11) = RSCALE(5) ! 58 + 11 + 5= 74 !SINCE NUMTABL = 5 
      
      NUPARAM= NUPARAM   + 5 !74



      IF (FLAG_TR_KINETICS ==2 ) THEN

        IF (GFAC_F == ZERO)GFAC_F = 0.32
        IF (PHI_F  == ZERO)PHI_F  = 0.4
        IF (PSI_F  == ZERO)PSI_F  = 0.4

        IF (GFAC_P == ZERO)GFAC_P = 0.32
        IF (PHI_P  == ZERO)PHI_P  = 0.4
        IF (PSI_P  == ZERO)PSI_P  = 0.4

        IF (GFAC_B == ZERO)GFAC_B = 0.32
        IF (PHI_B  == ZERO)PHI_B  = 0.4
        IF (PSI_B  == ZERO)PSI_B  = 0.4

        IF (PHI_M  == ZERO)PHI_M  = 0.0428
        IF (PSI_M  == ZERO)PSI_M  = 0.382
        IF (N_M    == ZERO)N_M    = 0.191

      ENDIF


      UPARAM(75) = GFAC_F
      UPARAM(76) = PHI_F
      UPARAM(77) = PSI_F
      UPARAM(78) = CR_F

      UPARAM(79) = GFAC_P
      UPARAM(80) = PHI_P
      UPARAM(81) = PSI_P
      UPARAM(82) = CR_P

      UPARAM(83) = GFAC_B
      UPARAM(84) = PHI_B
      UPARAM(85) = PSI_B
      UPARAM(86) = CR_B

      UPARAM(84) = PHI_M
      UPARAM(85) = PSI_M
      UPARAM(86) = N_M


      FGFER = TWO**(GSIZE*GFAC_F)
      FGPER = TWO**(GSIZE*GFAC_P)
      FGBAI = TWO**(GSIZE*GFAC_B)

      UPARAM(87) = FGFER
      UPARAM(88) = FGPER
      UPARAM(89) = FGBAI

      UPARAM(90) = CF
      UPARAM(91) = CP
      UPARAM(92) = CB


      NUPARAM = 92
c----------------------------

      PARMAT(1) = BULK
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
CC     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
C------------------------------   
      WRITE(IOUT, 900) TRIM(TITR),MAT_ID,80
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT, 950) RHO0
        WRITE(IOUT,1100)E,NU, IFUNC(1), EFAC,UNITT
        WRITE(IOUT,1200)ITABLE(1),ITABLE(2),ITABLE(3),
     .  ITABLE(4),ITABLE(5),YSCALE1,YSCALE2,YSCALE3,YSCALE4,
     .  YSCALE5, XSCALE(1),XSCALE(2),XSCALE(3),
     .  XSCALE(4),XSCALE(5),CEPS, PEPS,ISRATE,FCUT
        WRITE(IOUT,1300)HEATFLAG,FLAG_HEAT_ID,TAU1,TAU3,FLAG_LOC
        WRITE(IOUT,1400)TETA2, TETA3,TETA4, TETA5
        WRITE(IOUT,1500)ALFA1, ALFA2
        WRITE(IOUT,1600)QR2,QR3,QR4,ALPHA2, TREF
        WRITE(IOUT,1700)AE1, AE3,BS,MS,GSIZE,
     .    KF,KP,LAT1,LAT2,TINI
        WRITE(IOUT,1900)B, MO,MN,W,AL,C,CR,SI,CU,AS,
     .  CO,NI,V,P,TI
        WRITE(IOUT,1901)FLAG_TR_STRAIN
        IF(FLAG_TR_STRAIN == 2  )THEN
          WRITE(IOUT,2000)IFUNC(3),IFUNC(4),IFUNC(5),
     .    IFUNC(6),IFUNC(7),RSCALE(1),RSCALE(2),RSCALE(3),RSCALE(4),RSCALE(5)
        ENDIF
        WRITE(IOUT,1902)FLAG_TR_KINETICS
        IF(FLAG_TR_KINETICS == 2  )THEN
          WRITE(IOUT,3000)GFAC_F,PHI_F,PSI_F,CR_F,GFAC_P,PHI_P,PSI_P,CR_P,
     .     GFAC_B,PHI_B,PSI_B,CR_B,PHI_M,PSI_M,N_M,CF,CP,CB
        ENDIF

        WRITE(IOUT,*)' '
      ENDIF
      RETURN
  900 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 950  FORMAT(
     & 5X,'INITIAL DENSITY    . . . . . . . . . . . . .=',1PG20.13/)
1000  FORMAT(
     & 5X,40H  HOT STAMPING LAW FOR BORON STEEL      ,/,
     & 5X,40H  --------------------------------      ,//)
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YOUNG FUNCTION ID FOR T DEPENDENCE . . .=',I10/
     & 5X,'YOUNG MODULUS SCALE FACTOR. . . . . . . =',1PG20.13/
     & 5X,'TIME SCALING FOR VIVKERS HARDNESS . . . =',1PG20.13/)
 1200 FORMAT(
     & 5X,'YIELD TABLE ID AUSTENITE. . . . . . . . =',I10/
     & 5X,'YIELD TABLE ID FERRITE. . . . . . . . . =',I10/
     & 5X,'YIELD TABLE ID PEARLITE . . . . . . . . =',I10/
     & 5X,'YIELD TABLE ID BAINITE. . . . . . . . . =',I10/
     & 5X,'YIELD TABLE ID MARTENSITE . . . . . . . =',I10/
     & 5X,'YIELD SCALE FACTOR AUSTENITE . . . . . .=',1PG20.13/
     & 5X,'YIELD SCALE FACTOR FERRITE . . . . . . .=',1PG20.13/
     & 5X,'YIELD SCALE FACTOR PEARLITE. . . . . . .=',1PG20.13/
     & 5X,'YIELD SCALE FACTOR BAINITE . . . . . . .=',1PG20.13/
     & 5X,'YIELD SCALE FACTOR MARTENSITE. . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR AUSTENITE . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR FERRITE . . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR PEARLITE. . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR BAINITE . . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR MARTENSITE. . .=',1PG20.13/
     & 5X,'COWPER SYMONDS PARAMETER C . . . . . . .=',1PG20.13/
     & 5X,'COWPER SYMONDS PARAMETER P . . . . . . .=',1PG20.13/
     & 5X,'SMOOTH STRAIN RATE OPTION. . . . . . . .=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY. . . . . .=',1PG20.13/)

 1300 FORMAT(
     & 5X,'FLAG FOR HEATING OPTION . . . . . . . . =',I10/
     & 5X,'FUNCTION DEFINING HEATING FLAG VS TIME .=',I10/
     & 5X,'TAU1 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'TAU3 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FLAG DEFINING IF PHASE CHANGE IS LOCAL .=',I10/
     & 5X,'FLAG DEFINING DEFORMATION STRAIN MODEL .=',I10/)

 1400 FORMAT(
     & 5X,'MEMORY COEFFICIENT FERRITE . . . . . . .=',1PG20.13/
     & 5X,'MEMORY COEFFICIENT PEARLITE. . . . . . .=',1PG20.13/
     & 5X,'MEMORY COEFFICIENT BAINITE . . . . . . .=',1PG20.13/
     & 5X,'MEMORY COEFFICIENT MARTENSITE. . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'THERMAL EXPANSION COEF AUSTENITE . . . .=',1PG20.13/
     & 5X,'THERMAL EXPANSION COEF PRODUCTS. . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'Q/R FOR FERRITE. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'Q/R FOR PEARLITE . . . . . . . . . . . .=',1PG20.13/
     & 5X,'Q/R FOR BAINITE. . . . . . . . . . . . .=',1PG20.13/
     & 5X,'MARTENSITE MATERIAL CONSTANT . . . . . .=',1PG20.13/
     & 5X,'REFERENCE TEMPERATURE. . . . . . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'TEMPERATURE AE1.(K) . . . . . . . . . .=',1PG20.13/
     & 5X,'TEMPERATURE AE3.(K) . . . . . . . . . .=',1PG20.13/
     & 5X,'TEMPERATURE BS (K). . . . . . . . . . .=',1PG20.13/
     & 5X,'TEMPERATURE MS (K). . . . . . . . . . .=',1PG20.13/
     & 5X,'GRAIN SIZE . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'BORON CONSTANT IN FERRITE. . . . . . . =',1PG20.13/
     & 5X,'BORON CONSTANT IN PEARLITE . . . . . . =',1PG20.13/
     & 5X,'LATENT HEAT (F, P, B). . . . . . . . . =',1PG20.13/
     & 5X,'LATENT HEAT (M). . . . . . . . . . . . =',1PG20.13/
     & 5X,'INITIAL TEMPERATURE.(K). . . . . . . . =',1PG20.13/)
 1900 FORMAT(
     & 5X,'BORON. . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'MOLYBDENUM . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'MANGANESE. . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'TUNGSTEN . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'ALUMINIUM. . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'CARBON . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'CHROMIUM . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'SILICIUM . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'COPPER . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'ARSENIC. . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'COBALT . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'NICKEL . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'VANADIUM . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'PHOSPHOROuS. . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'TITANIUM . . . . . . . . . . . . . . . =',1PG20.13/)

 1901 FORMAT(
     & 5X,'FLAG FOR TRANSFORMATION STRAIN. . . . .=',I10/)
 2000 FORMAT(
     & 5X,'DENSITY FUNCTION ID AUSTENITE. . . . . . . . =',I10/
     & 5X,'DENSITY FUNCTION ID FERRITE. . . . . . . . . =',I10/
     & 5X,'DENSITY FUNCTION ID PEARLITE . . . . . . . . =',I10/
     & 5X,'DENSITY FUNCTION ID BAINITE. . . . . . . . . =',I10/
     & 5X,'DENSITY FUNCTION ID MARTENSITE . . . . . . . =',I10/
     & 5X,'DENSITY SCALE FACTOR AUSTENITE . . . . . . . =',1PG20.13/
     & 5X,'DENSITY SCALE FACTOR FERRITE . . . . . . . . =',1PG20.13/
     & 5X,'DENSITY SCALE FACTOR PEARLITE. . . . . . . . =',1PG20.13/
     & 5X,'DENSITY SCALE FACTOR BAINITE . . . . . . . . =',1PG20.13/
     & 5X,'DENSITY SCALE FACTOR MARTENSITE. . . . . . . =',1PG20.13/)
 1902 FORMAT(
     & 5X,'FLAG FOR TRANSFORMATION KINETICS. . . . . . .=',I10/)
 3000 FORMAT(
     & 5X,'FERRITE GRAIN SIZE FACTOR W_F . . . . . . . .  . . =',1PG20.13/
     & 5X,'FERRITE EVOLUTION PARAMETER FOR INCUBATION PHI . . =',1PG20.13/
     & 5X,'FERRITE EVOLUTION PARAMETER FOR TIME CONTROL PSI . =',1PG20.13/
     & 5X,'FERRITE EVOLUTION PARAMETER FOR RETARDATION CR_F . =',1PG20.13/
     & 5X,'PEARLITE GRAIN SIZE FACTOR W_F. . . . . . . .  . . =',1PG20.13/
     & 5X,'PEARLITE EVOLUTION PARAMETER FOR INCUBATION PHI. . =',1PG20.13/
     & 5X,'PEARLITE EVOLUTION PARAMETER FOR TIME CONTROL PSI. =',1PG20.13/
     & 5X,'PEARLITE EVOLUTION PARAMETER FOR RETARDATION CR_F. =',1PG20.13/
     & 5X,'BAINITE GRAIN SIZE FACTOR W_F . . . . . . . .  . . =',1PG20.13/
     & 5X,'BAINITE EVOLUTION PARAMETER FOR INCUBATION PHI . . =',1PG20.13/
     & 5X,'BAINITE EVOLUTION PARAMETER FOR TIME CONTROL PSI . =',1PG20.13/
     & 5X,'BAINITE EVOLUTION PARAMETER FOR RETARDATION CR_F . =',1PG20.13/
     & 5X,'MARTENSITE EVOLUTION PARAMETER FACTOR PHI . .  . . =',1PG20.13/
     & 5X,'MARTENSITE EVOLUTION EXPONENT KSI. . . . . . . . . =',1PG20.13/
     & 5X,'MARTENSITE EVOLUTION EXPONENT N_M. . . . . . . . . =',1PG20.13/
     & 5X,'FERRITE ALLOY DEPENDENT FACTOR CF. . . . . . . . . =',1PG20.13/
     & 5X,'PEARLITE  ALLOY DEPENDENT FACTOR CP. . . . . . . . =',1PG20.13/
     & 5X,'BAINITE   ALLOY DEPENDENT FACTOR CB. . . . . . . . =',1PG20.13/)
999    CALL FREERR(3)
      RETURN
      END
