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
Chd|  HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT100(UPARAM   ,MAXUPARAM,NUPARAM ,NUVAR  ,IFUNC     , 
     .                          MFUNC    ,MAXFUNC  ,MTAG    ,PARMAT ,UNITAB    ,
     .                          IMATVIS  ,PM       ,LSUBMODEL, ID   ,TITR      ,
     .                          MATPARAM )                     
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
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE HM_OPTION_READ_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      
      INTEGER, INTENT(INOUT)          :: MFUNC,NUPARAM,NUVAR,IMATVIS
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM 
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, MAT_ID  ! Number of declared materials
      INTEGER :: I,J,NRATE,NPS,ILAW,NFUNC,ITEST,N_NETWORK, FLAG_HE, FLAG_MUL,  EXPPL,
     .           FLAG_PL,NHYPER,NET,N,TAB,SHIFT,
     .           NMUL,NTEMP,NPLAS,NVISC(10),FLAG_VISC(10)     
      my_real :: RHO0, E,NU,G,RBULK,TAUREF_UNIT,  
     .           C1,C2,C3,C4,C5,MU,LM,D,BETA,FACPL, SCALE1, SCALE2,SCALEFAC, 
     .           C10,C01,C20,C11,C02 ,FAC_UNIT,FAC_SM,FAC_BM,
     .           C30, C21,C12,C03,SB,D1,D2,D3,FF,EPSHAT,TAUY ,
     .           A(10),EXPC(10),
     .           EXPM(10),KSI(10),STIFFN(10),B0(10),EXPN(10),TAUREF(10)
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
      IMATVIS = 1                 
      ILAW    = 100
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
      NFUNC = 0
      NUVAR = 0
      C10    = ZERO
      C01    = ZERO
      C20    = ZERO
      C11    = ZERO
      C02    = ZERO
      C30    = ZERO
      C21    = ZERO
      C12    = ZERO
      C03    = ZERO
      D1     = ZERO
      D2     = ZERO
      D3     = ZERO
      NU     = ZERO
      NHYPER = 0 ! NBRE PARAMETERS OF HYPERELASTIC LAW
      RBULK  = ZERO

      FLAG_HE = 1 
      FLAG_PL = 0
C-----------------------------------------------
card1     
      CALL HM_GET_INTV('MAT_N_net'  , N_NETWORK, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('MAT_Flag_HE', FLAG_HE  , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('MAT_Flag_Cr', FLAG_PL  , IS_AVAILABLE, LSUBMODEL)

      IF (N_NETWORK > 10 ) THEN
         CALL ANCMSG(MSGID=1567 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR)
      ENDIF
C-----------------------------------------------
c     FLAG_HE = 1 =>POLYNOMIAL
c     FLAG_HE = 2 =>ARRUDA - BOYCE
c     FLAG_HE = 3 =>NEO-HOOK
c     FLAG_HE = 4 =>MOONEY-RIVLIN
c     FLAG_HE = 5 =>YEOH
c     FLAG_HE = 13=>NEO-HOOK WITH TEMPERATURE 
c     Card FOR HYPERELASTIC MODEL    
      !--------------------------------------
      IF (FLAG_HE == 1)THEN
card      
       CALL HM_GET_FLOATV('MAT_C_10'    ,C10      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_01'    ,C01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_20'    ,C20      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_11'    ,C11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_02'    ,C02      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card      
       CALL HM_GET_FLOATV('MAT_C_30'    ,C30      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_21'    ,C21      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_12'    ,C12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_03'    ,C03      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card           
       CALL HM_GET_FLOATV('MAT_D_1'    ,D1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_D_2'    ,D2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_D_3'    ,D3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       NHYPER = 12
      !--------------------------------------
      ELSEIF (FLAG_HE == 2)THEN !ARRUDA-BOYCE
card           
       CALL HM_GET_FLOATV('MAT_MUE1'    ,MU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_D'       ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('Lambda'      ,LM       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card           
       CALL HM_GET_INTV  ('Itype'         ,ITEST    ,IS_AVAILABLE, LSUBMODEL)
       CALL HM_GET_INTV  ('MAT_fct_ID_AB' ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
       CALL HM_GET_FLOATV('MAT_NU'        ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('Fscale_AB' ,SCALEFAC ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       IF(SCALEFAC == ZERO)THEN 
        CALL HM_GET_FLOATV_DIM('Fscale_AB' ,FAC_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALEFAC = ONE * FAC_UNIT
       ENDIF       
       IF(ITEST == 0) ITEST = 1
       NHYPER = 11
       IF(IFUNC(1) /= 0) NFUNC = 1
      !--------------------------------------
      ELSEIF (FLAG_HE == 3)THEN !neo-hook
card           
       CALL HM_GET_FLOATV('MAT_C_10'    ,C10      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_D_1'     ,D1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       NHYPER = 12
      !--------------------------------------
      ELSEIF (FLAG_HE == 4)THEN ! Mooney-Rivlin
card           
       CALL HM_GET_FLOATV('MAT_C_10'    ,C10      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_01'    ,C01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_D_1'     ,D1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       NHYPER = 12
      !--------------------------------------
      ELSEIF (FLAG_HE == 5)THEN !yeoh
card           
       CALL HM_GET_FLOATV('MAT_C_10'    ,C10      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_20'    ,C20      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_C_30'    ,C30      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_D_1'     ,D1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       NHYPER = 12
      !--------------------------------------
      ELSEIF (FLAG_HE == 13)THEN !neo-hook with temperature
card           
       CALL HM_GET_INTV  ('MAT_fct_ID_SM' ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL)
       CALL HM_GET_INTV  ('MAT_fct_ID_BM' ,IFUNC(2) ,IS_AVAILABLE, LSUBMODEL)
       CALL HM_GET_FLOATV('MAT_Fscale_SM' ,SCALE1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_Fscale_BM' ,SCALE2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       IF(SCALE1 == ZERO)THEN 
        CALL HM_GET_FLOATV_DIM('MAT_Fscale_SM' ,FAC_SM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALE1 = ONE * FAC_SM
       ENDIF       
       IF(SCALE2 == ZERO)THEN 
        CALL HM_GET_FLOATV_DIM('MAT_Fscale_BM' ,FAC_BM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALE2 = ONE * FAC_BM
       ENDIF       
       NFUNC = 2
       NHYPER = 5
       NUVAR = 2
       IF(IFUNC(1)==0 .OR.  IFUNC(2)==0) THEN 
         CALL ANCMSG(MSGID=1571 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR)
       ENDIF
      !--------------------------------------
      !--------------------------------------
      ELSE
         CALL ANCMSG(MSGID=1569 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                I2=FLAG_HE)
      ENDIF
      !FIN LECTURE PARAMETRES HYPERELASTQUES
      !--------------------------------------
      !--------------------------------------
Card FOR PLASTICITY
C=======================================================================
      IF (FLAG_PL == 1)THEN
card
       CALL HM_GET_FLOATV('MAT_A_pl'      ,FACPL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_Sigma_pl'  ,TAUY     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_F_pl'      ,FF       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_Epsilon_F' ,EPSHAT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_INTV  ('MAT_N_pl'      ,EXPPL    ,IS_AVAILABLE, LSUBMODEL)

       IF(EXPPL   == 0) EXPPL=1
       IF(FACPL   == ZERO) FACPL=ONE
       IF(EPSHAT  == ZERO) EPSHAT=ONE
       IF(FF      == ZERO) FF=ONE
       IF(TAUY    == ZERO) TAUY = ONE
       ! NUVAR = NUVAR + 13
      ENDIF
C=======================================================================
Card FOR VISCOSITY 

        !NETWORK 1 OBLIGATOIRE NET = 1
        !SECONDARY NETWORKS
        !START READING PARAMETERS OF SECONDARY NETWORKS(VISCOSITY PARAMETERS )
        NUVAR = NUVAR + 13 ! variables for equilibrium network
        SHIFT = 0
        DO NET = 1, N_NETWORK    
card
          CALL HM_GET_INT_ARRAY_INDEX  ('MAT_ARR_Flag_visc',FLAG_VISC(NET),NET,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_stiffness',STIFFN(NET)   ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
          !-----------------------------
          IF (FLAG_VISC(NET) == 1)THEN
card
            NVISC(NET) = 5
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_A1' ,A(NET)        ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_C'  ,EXPC(NET)     ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_M'  ,EXPM(NET)     ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_KSI',KSI(NET)      ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('TAU_ref'    ,TAUREF(NET)   ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            SHIFT = 9 +2 !SET NUVAR
            IF(TAUREF(NET) == ZERO) THEN
              CALL HM_GET_FLOATV_DIM('TAU_ref' ,TAUREF_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
              TAUREF(NET) = ONE * TAUREF_UNIT
            ENDIF
                             
                
          !-----------------------------
          ELSEIF (FLAG_VISC(NET) == 2)THEN !hyperbolic sine
            NVISC(NET) = 3
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_A2',A(NET)      ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_B' ,B0(NET)     ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_N2' ,EXPN(NET)  ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            SHIFT = 9+2 !SET SHIFT FOR NUVAR

                !READ(LINE,ERR=999,FMT=FMT_5F) A(NET),B0(NET),EXPN(NET)                
          ELSEIF (FLAG_VISC(NET) == 3)THEN ! POWER LAW
            NVISC(NET) = 3
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_A3',A(NET)       ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_N3' ,EXPN(NET)   ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_ARR_M3' ,EXPM(NET)   ,NET,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            SHIFT = 10 +2!SET NUVAR
          ELSE
            CALL ANCMSG(MSGID=1808 ,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                I2=N_NETWORK,
     .                I3=NET)
               ! READ(LINE,ERR=999,FMT=FMT_5F) A(NET),EXPN(NET),EXPM(NET)                 
          ENDIF !FLAG_VISC
             !-----------------------------
           NUVAR = NUVAR + SHIFT

       ENDDO

       !NUVAR = NUVAR +50 !POUR DEBUG
C      
      SB = ZERO
      DO NET = 1,N_NETWORK
         SB = SB + STIFFN(NET)         ! check si ca donne un =1        
      ENDDO

      !==============HE========================      
      IF (FLAG_HE == 1 .OR. FLAG_HE == 3 .OR.FLAG_HE == 4 .OR.FLAG_HE == 5 )THEN
      !--------------------------------------
       IF(D2 /=  ZERO ) D2 = ONE/D2    
       IF(D3 /=  ZERO ) D3 = ONE/D3 
       G = TWO  * (C10 + C01) *(SB + ONE)  
       IF(D1 /= ZERO) THEN
         D1 = ONE/D1 
         RBULK= TWO*D1 *(ONE + SB) 
         NU = (THREE*RBULK -TWO*G)/(THREE*RBULK + G)/TWO
         E = NINE*RBULK*G/(THREE*RBULK + G)
       ELSE
         D2 = ZERO
         D3 = ZERO
         NU = 0.495
         RBULK = TWO_THIRD*G*(ONE + NU)/(ONE-TWO*NU)
         D1    = RBULK / TWO ! 1/D1 A VOIR SI ON SUPRIME PAS LE TERME AVEC J
         E     = TWO*G*(ONE + NU)      
       ENDIF
      !==============HE========================      
      ELSEIF (FLAG_HE == 2)THEN
      !--------------------------------------
       C1 = HALF
       C2 = ONE/TWENTY
       C3 = ELEVEN/1050.
       C4 = 19.D00/7000.
       C5 = 519.D00/673750.
       IF(D == ZERO ) D  = EM20
       IF(LM == ZERO) LM = SEVEN 
       BETA = ONE/LM/LM
       G = MU*(ONE +  THREE*BETA /FIVE + EIGHTY19*BETA*BETA/175. 
     .         + 513.*BETA**3/875. + 42039.*BETA**4/67375.)*(SB + ONE)   
       RBULK =  TWO *(ONE + SB) /D
       E = NINE*RBULK*G/(THREE*RBULK + G)
       IF(IFUNC(1) == 0)THEN
        NU = (THREE*RBULK -TWO*G)/(THREE*RBULK + G)/TWO
       ELSE  !(IFUNC(1) /= 0) THEN
        NFUNC = 1
        IF (NU == ZERO) NU= 0.495
       ENDIF             
      ENDIF ! FLAG_HE
     
C=======================================================================
C               uparam
C=======================================================================
      UPARAM(1)  = N_NETWORK
      UPARAM(2)  = FLAG_HE
      !UPARAM(3)  = FLAG_MUL treated in updmat =1 if /fail/mullins exist
      UPARAM(4)  = SB
      UPARAM(5)  = FLAG_PL
      !PRINSIPAL NETWORK
      NMUL  = 0
      NTEMP = 0
      NPLAS = 0
      UPARAM(6)  = NMUL
      UPARAM(7)  = NTEMP
      UPARAM(8)  = NPLAS
      TAB = 8 !USED IN UPDMAT AND ENGINE
      IF (FLAG_HE == 1 .OR. FLAG_HE == 3 .OR.FLAG_HE == 4 .OR.FLAG_HE == 5 ) THEN 
       UPARAM(TAB + 1)  = C10
       UPARAM(TAB + 2)  = C01
       UPARAM(TAB + 3)  = C20
       UPARAM(TAB + 4)  = C11
       UPARAM(TAB + 5)  = C02
       UPARAM(TAB + 6)  = C30
       UPARAM(TAB + 7)  = C21
       UPARAM(TAB + 8)  = C12
       UPARAM(TAB + 9)  = C03
       UPARAM(TAB + 10) = D1
       UPARAM(TAB + 11) = D2
       UPARAM(TAB + 12) = D3 
       TAB = TAB + NHYPER 
      ELSEIF (FLAG_HE == 2) THEN 
       UPARAM(TAB + 1)  = C1
       UPARAM(TAB + 2)  = C2
       UPARAM(TAB + 3)  = C3
       UPARAM(TAB + 4)  = C4
       UPARAM(TAB + 5)  = C5
       UPARAM(TAB + 6)  = MU
       UPARAM(TAB + 7)  = ONE/D
       UPARAM(TAB + 8)  = BETA
       UPARAM(TAB + 9)  = ITEST
       UPARAM(TAB +10)  = NU
       UPARAM(TAB +11)  = SCALEFAC
       TAB = TAB + NHYPER 
      ELSEIF (FLAG_HE == 13)THEN !neo-hook with temperature
       UPARAM(TAB + 1)  =  SCALE1
       UPARAM(TAB + 2)  =  SCALE2
       UPARAM(TAB + 3)  =  NFUNC
       !UPARAM(TAB+4) AND UPARAM(TAB+5) CALCULE DANS UPDMAT
       TAB = TAB + NHYPER         
      ENDIF
      !NETWORK 

      !POUR FUTUR STOCKAGE PARAMETRE MULINS THERMIQUE PLASTIQUE
      IF (FLAG_PL == 1) THEN
        NPLAS = 5 
        UPARAM(TAB + 1) =  FF 
        UPARAM(TAB + 2) =  EPSHAT
        UPARAM(TAB + 3) =  TAUY
        UPARAM(TAB + 4) =  EXPPL
        UPARAM(TAB + 5) =  FACPL
      ENDIF
         
      TAB = TAB  + NPLAS 

      DO N = 1, N_NETWORK
        UPARAM(TAB + 1) = STIFFN(N)
        UPARAM(TAB + 2) = FLAG_VISC(N)
        UPARAM(TAB + 3) = NVISC(N) 
        IF (FLAG_VISC(N) == 1)THEN  
          IF(EXPM(N) == ZERO)EXPM(N) = ONE
          IF(EXPC(N) == ZERO)EXPC(N) = -0.700000000
          IF(KSI(N)  == ZERO)KSI(N) = EM02  
          UPARAM(TAB + 4) = A(N)
          UPARAM(TAB + 5) = EXPC(N)
          UPARAM(TAB + 6) = EXPM(N)
          UPARAM(TAB + 7) = KSI(N)
          UPARAM(TAB + 8) = TAUREF(N)
          TAB = TAB + 3 + NVISC(N)

        ELSEIF (FLAG_VISC(N) == 2)THEN    !hperbolic sine
          UPARAM(TAB + 4) = A(N)
          UPARAM(TAB + 5) = B0(N)
          UPARAM(TAB + 6) = EXPN(N)
          TAB = TAB + 3 + NVISC(N)
        ELSEIF (FLAG_VISC(N) == 3)THEN    !power law
          UPARAM(TAB + 4) = A(N)
          UPARAM(TAB + 5) = EXPN(N) 
          UPARAM(TAB + 6) = EXPM(N)
          TAB = TAB + 3 + NVISC(N)
        ENDIF
      ENDDO
      IF (FLAG_HE /= 13)THEN
       UPARAM(TAB + 1) = G
       UPARAM(TAB + 2) = RBULK 
       PARMAT(1) = RBULK
       PARMAT(2) = E
       PARMAT(3) = NU
      ENDIF      
      NUPARAM = TAB + 2 
      MFUNC = NFUNC
C=======================================================================
      PM(89)  = RHO0    
      PM(100) = RBULK
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
C=======================================================================
c                        output
C=======================================================================
      WRITE(IOUT,1001) TRIM(TITR),ID,100
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED)THEN                                     
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'     
      ELSE     
         WRITE(IOUT,1002)RHO0
        WRITE(IOUT,900)N_NETWORK, FLAG_HE,FLAG_PL
        IF (FLAG_HE == 1) THEN 
         WRITE(IOUT,1100)C10,C01,C20,C11,C02,
     .      C30, C21,C12,C03,D1,D2,D3 

        ELSEIF (FLAG_HE == 2) THEN    
         WRITE(IOUT,2000)MU,D, LM
         IF(IFUNC(1) > 0) THEN
           IF(ITEST == 1) THEN 
              WRITE(IOUT,2200)IFUNC(1),SCALEFAC, NU
           ELSEIF(ITEST == 2) THEN 
              WRITE(IOUT,2300)IFUNC(1),SCALEFAC, NU
           ELSEIF(ITEST == 3) THEN 
              WRITE(IOUT,2400)IFUNC(1),SCALEFAC, NU
           ENDIF 
         ENDIF  
        ELSEIF (FLAG_HE == 3) THEN    
         WRITE(IOUT,2001)C10,D1
        ELSEIF (FLAG_HE == 4) THEN    
         WRITE(IOUT,2002)C10,C01,D1
        ELSEIF (FLAG_HE == 5) THEN    
         WRITE(IOUT,2003)C10,C20,C30,D1
        ELSEIF (FLAG_HE == 13) THEN    
         WRITE(IOUT,2005)IFUNC(1),IFUNC(2),SCALE1,SCALE2
        ENDIF 
        IF (FLAG_PL == 1) THEN 
          WRITE(IOUT,2004) FACPL , TAUY,  EXPPL,FF,EPSHAT
        ENDIF 
      DO N = 1, N_NETWORK
         WRITE(IOUT,1150)N
        IF (FLAG_VISC(N) == 1)THEN  
         WRITE(IOUT,1300)STIFFN(N),A(N),EXPC(N),EXPM(N),KSI(N) , TAUREF(N) 
        ELSEIF (FLAG_VISC(N) == 2)THEN    !hperbolic sine
         WRITE(IOUT,1400)STIFFN(N),A(N),B0(N),EXPN(N) 
        ELSEIF (FLAG_VISC(N) == 3)THEN    !power law
         WRITE(IOUT,1500)STIFFN(N),A(N),EXPN(N),EXPM(N) 
        ENDIF

      ENDDO
      IF (FLAG_HE /= 13)THEN
         WRITE(IOUT,1200)G,RBULK, NU
      endif
      ENDIF
c-------------------------------------------------------------     
 1000 FORMAT(
     & 5X,'  PARALLEL RHEOLOGICAL FRAMEWORK : ',/,
     & 5X,'  -------------------------------- ',/)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . .=',1PG20.13/)
 900  FORMAT(
     & 5X,'NUMBER OF SECONDARY NETWORKS . . . . . .= ',I10/
     & 5X,'FLAG FOR HYPERELASTIC LAW. . . . . . . .= ',I10/
     & 5X,'FLAG FOR PLASTICITY. . . . . . . . . . .= ',I10)
 1100 FORMAT(
     & 5X,'HYPERELASTIC MODEL = PPOLYNOMIAL  ',/,
     & 5X,'C10 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C01 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C20 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C11 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C02 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C30 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C21 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C12 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C03 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D1  . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D2  . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D3  . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 2000 FORMAT(
     & 5X,'HYPERELASTIC MODEL = ARRUDA-BOYCE  ',/,
     & 5X,'MATERIAL CONSTANT MU . . . . . . . . . .=',1PG20.13/
     & 5X,'VOLUMETRIC MATERIAL PARAMETER. . . . . .=',1PG20.13/
     & 5X,'LOCKING STRETCH. . . . . . . . . . . . .=',1PG20.13//) 
 2200 FORMAT(
     & 5X,'UNIAXIAL DATA TEST CURVE  .  . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRESS IN FUNCTION. . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',1PG20.13//) 
 2300 FORMAT(
     & 5X,'EQUIBIAXIAL DATA TEST CURVE. . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRESS IN FUNCTION. . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',1PG20.13//)
 2400 FORMAT(
     & 5X,'PLANAR DATA TEST CURVE  . .  . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR STRESS IN FUNCTION. . .=',1PG20.13/
     & 5X,'POISSON RATIO. . . . . . . . . . . . . .=',1PG20.13//)
 2001 FORMAT(
     & 5X,'HYPERELASTIC MODEL = NEO-HOOKEAN         ',/,
     & 5X,'C10 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D1  . . . . . . . . . . . . . . . . . .=',1PG20.13//)
 2002 FORMAT(
     & 5X,'HYPERELASTIC MODEL = MOONEY-RIVLIN        ',/,
     & 5X,'C10 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C01 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D1  . . . . . . . . . . . . . . . . . .=',1PG20.13//)
 2003 FORMAT(
     & 5X,'HYPERELASTIC MODEL = YEOH        ',/,
     & 5X,'C10 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C20 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'C30 . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'1/D1  . . . . . . . . . . . . . . . . . .=',1PG20.13//)
 2005 FORMAT(
     & 5X,'HYPERELASTIC MODEL = TEMPERATURE DEPENDENT NEO HOOK',/,
     & 5X,'FUNCTION ID FOR MU. . . . . . . . . . . .=',I10/
     & 5X,'FUNCTION ID FOR D . . . . . . . . . . . .=',I10/
     & 5X,'SCALE FACTOR FOR FUNCTION 1 . . . . . . .=',1PG20.13/
     & 5X,'SCALE FACTOR FOR FUNCTION 2 . . . . . . .=',1PG20.13//)
CFACPL , TAUY,  EXPPL,FF,EPSHAT
 2004 FORMAT(
     & 5X,'PLASTICITY PARAMETERS IN EQUILIBRIUM NETWORK',/,
     & 5X,'A FACTOR. . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'FLOW RESISTANCE . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'EXPONENT FOR FLOW RATE. . . . . . . . . .=',I10/
     & 5X,'FINAL FLOW RESISTANCE . . . . . . . . . .=',1PG20.13/
     & 5X,'CHARACTERISTIC STRAIN . . . . . . . . . .=',1PG20.13//)
 1150 FORMAT(
     & 5X,'PARAMETERS FOR VISCOUS MODEL FOR NETWORK : ',I10)
 1300 FORMAT(
     & 5X,'BERGSTROM BOYCE VISCOUS MODEL  ',/,
     & 5X,'STIFFNESS SCALING COEFFICIENT. . . . . . =',1PG20.13/
     & 5X,'A. . . . . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT C . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT M . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'KSI. . . . . . . . . . . . . . . . . . . =',1PG20.13/      
     & 5X,'REFERENCE STRESS . . . . . . . . . . . . =',1PG20.13)      
 1400 FORMAT(
     & 5X,'HYPERBOLIC SINE VISCOUS MODEL  ',/,
     & 5X,'STIFFNESS SCALING COEFFICIENT. . . . . . =',1PG20.13/
     & 5X,'A. . . . . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'COEFFICIENT B0 . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT N . . . . . . . . . . . . . . . =',1PG20.13/)
 1500 FORMAT(
     & 5X,'POWER LAW VISCOUS MODEL  ',/,
     & 5X,'STIFFNESS SCALING COEFFICIENT. . . . . . =',1PG20.13/
     & 5X,'A. . . . . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT N . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'EXPONENT M . . . . . . . . . . . . . . . =',1PG20.13/)
 1200 FORMAT(
     & 5X,'INITIAL SHEAR MODULUS .  . . . . . . . . =',1PG20.13/
     & 5X,'INITIAL BULK MODULUS. .  . . . . . . . . =',1PG20.13/
     & 5X,'POISSON RATIO  . . . . . . . . . . . . . =',1PG20.13//) 
C
      RETURN
      END










