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
C==Barlat YLD2000-2D
Chd|====================================================================
Chd|  HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT87(UPARAM   ,MAXUPARAM,NUPARAM,ISRATE ,NUVAR  , 
     .                         NFUNC ,MAXFUNC ,IFUNC    ,MTAG    ,PARMAT , 
     .                         UNITAB,PM      ,LSUBMODEL, ID     ,TITR      )                     
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
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM     
      my_real, DIMENSION(100)    ,INTENT(OUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(OUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: NFUNC,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NBMAT, MAT_ID,ISRATE,IFLAGSR,IFLAG,EXPA,FLAG_FIT ,
     .           ILAW,NRATE,NFLAG3,IYIELD,NFLAG4
      my_real :: E,NU,G,FAC_L,FAC_T,FAC_M,FAC_C,BULK,FCUT,
     .           AL1, AL2 , AL3 , AL4 ,
     .          AL5, AL6 , AL7 , AL8 ,
     .          AL9, AL10, AL11, AL12,FISOKIN,
     .          YSCALE,CEPS,EXPN,INVP , INVC,UNSPT , UNSCT,
     .          ASWIFT ,EPSO,QVOCE,BETA,KO,ALPHA,NEXP,UNSP , UNSC,
     .          RHO0,RHOR,RATE(MAXFUNC),YFAC(MAXFUNC),YFAC_UNIT(MAXFUNC),
     .          K1    , K2    , AHS   , BHS   , MHS   , EPS0HS,
     .          NHS   , HMART , TEMP0 , TREF  , ETA   , CP    , AM    ,
     .          BM    , CM    , DM    , PPM    , QM    , E0MART, VM0 ,
     .          CKH(4), AKH(4)  
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
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
      ILAW    = 87
C----------------------------------------------------------------------
      RATE(1:MAXFUNC)=ZERO
      IFLAG   = 0
      IFLAGSR = 0
      INVP  = ZERO
      INVC  = ZERO
      UNSP  = ZERO
      UNSC  = ZERO
      UNSPT = ZERO
      UNSCT = ZERO
      AL9   = ONE
      AL10  = ONE
      AL11  = ONE
      AL12  = ONE
      NRATE = 0
      ISRATE= 0
      FCUT  = ZERO
      K1    = ZERO
      K2    = ZERO
C----------------------------------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C-----------------------------------------------
C
      CALL HM_GET_FLOATV('MAT_E'    ,E      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,NU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iflag',IFLAG  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Vflag'    ,IFLAGSR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('STRAIN1'  ,INVC   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP1' ,INVP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2
      CALL HM_GET_INTV  ('Ifit'       ,FLAG_FIT,IS_AVAILABLE,LSUBMODEL)
      IF (FLAG_FIT == 1) THEN
Card2
       CALL HM_GET_FLOATV('Sigma_00' ,AL1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('Sigma_45' ,AL2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('Sigma_90' ,AL3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('Sigma_b'  ,AL4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3
       CALL HM_GET_FLOATV('r_00'     ,AL5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('r_45'     ,AL6     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('r_90'     ,AL7     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('r_b'      ,AL8     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ELSE
       CALL HM_GET_FLOATV('MAT_ALPHA1' ,AL1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_ALPHA2' ,AL2     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_ALPHA3' ,AL3     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_ALPHA4' ,AL4     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

Card3
       CALL HM_GET_FLOATV('MAT_ALPHA5' ,AL5     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_ALPHA6' ,AL6     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_ALPHA7' ,AL7     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
       CALL HM_GET_FLOATV('MAT_ALPHA8' ,AL8     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      ENDIF
Card5 
      CALL HM_GET_INTV  ('A'          ,EXPA     ,IS_AVAILABLE,LSUBMODEL)  
      CALL HM_GET_FLOATV('MAT_kin' ,FISOKIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
     

      IF (IFLAG == 0) THEN
        CALL HM_GET_INTV('MAT_NRATE'   ,NRATE    ,IS_AVAILABLE,LSUBMODEL)
        IF(NRATE == 0) THEN
          CALL ANCMSG(MSGID=1116,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)         
        ENDIF
        DO I=1,NRATE
          CALL HM_GET_INT_ARRAY_INDEX        ('FUN_LOAD'    ,IFUNC(I)    ,I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX      ('SCALE_LOAD'  ,YFAC(I)     ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          IF(YFAC(I) == ZERO) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_LOAD'  ,YFAC_UNIT(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB) 
            YFAC(I)=ONE * YFAC_UNIT(I)
          ENDIF
          CALL HM_GET_FLOAT_ARRAY_INDEX   ('STRAINRATE_LOAD',RATE(I)     ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          IF(I > 1 )THEN
            IF( RATE(I) < RATE(I-1)) THEN
            CALL ANCMSG(MSGID=478,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR)
            EXIT
           ENDIF
          ENDIF
        ENDDO
        DO I=1,NRATE
          IF(IFUNC(I)==0)THEN
            CALL ANCMSG(MSGID=126,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
          ENDIF
        ENDDO
      ELSE
        NRATE = 0
      ENDIF
      IF (IFLAG == 0 .OR.IFLAG == 1) THEN! TABULATED OR SWIFT VOCE FOR FILTERING
        CALL HM_GET_FLOATV('Fcut'        ,FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV  ('MAT_FSMOOTH' ,ISRATE   ,IS_AVAILABLE,LSUBMODEL)
      ENDIF

      IF (IFLAG == 1) THEN ! SWIFT VOCE
        CALL HM_GET_FLOATV('MAT_VOL'     ,ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('FScale33'    ,NEXP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
C 
        CALL HM_GET_FLOATV('MAT_A'        ,ASWIFT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('FScale22'     ,EPSO   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_PR'       ,QVOCE  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_T0'       ,BETA   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_NUt'      ,KO     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ENDIF
C
      IF (IFLAG == 2) THEN
        CALL HM_GET_FLOATV('MAT_AM'    , AM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_BM'    , BM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_CM'    , CM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_DM'    , DM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_PM'    , PPM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_QM'    , QM    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_E0MART', E0MART,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_VM0'   , VM0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_AHS'   , AHS   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOATV('MAT_BHS'   , BHS   ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOATV('MAT_MHS'   , MHS   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_EPS0HS', EPS0HS,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_NHS'   , NHS   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_HMART' , HMART ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_K1'    , K1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_K2'    , K2    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOATV('MAT_TEMP0' , TEMP0 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_TREF'  , TREF  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_ETA'   , ETA   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_CP'    , CP    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ENDIF
C
      IF (FISOKIN >ZERO) THEN
        CALL HM_GET_FLOATV('MAT_CRC1' , CKH(1),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRA1' , AKH(1),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRC2' , CKH(2),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRA2' , AKH(2),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRC3' , CKH(3),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRA3' , AKH(3),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRC4' , CKH(4),IS_AVAILABLE, LSUBMODEL, UNITAB)  
        CALL HM_GET_FLOATV('MAT_CRA4' , AKH(4),IS_AVAILABLE, LSUBMODEL, UNITAB)  
      ENDIF
c-------------------------------------------------------------
      NFUNC =NRATE
      IF (IFLAG == 0) THEN
       IF(NRATE == 1)THEN
        NFUNC=2
        IFUNC(2)=IFUNC(1)
        RATE(1) = ZERO
        RATE(2) = ONE
        YFAC(2) = YFAC(1)
       ELSEIF(RATE(1) == ZERO)THEN
        NFUNC=NRATE
       ELSE
        NFUNC=NRATE+1
        DO J=NRATE,1,-1
          IFUNC(J+1)=IFUNC(J)
          RATE(J+1) =RATE(J)
          YFAC(J+1) =YFAC(J)
        ENDDO
        RATE(1)= ZERO
       ENDIF  
      ENDIF    
c-------------------------------------------------------------
      IF( INVP == ZERO .OR. INVC == ZERO)THEN
        UNSP  = ZERO
        UNSC  = ZERO
        UNSPT = ZERO
        UNSCT = ZERO
      ELSEIF(IFLAGSR == 0)THEN
        UNSPT = ONE/INVP
        UNSCT = ONE/INVC  
      ELSEIF(IFLAGSR == 1)THEN
        UNSP = ONE/INVP
        UNSC = ONE/INVC  
      ENDIF
c-------------------------------------------------------------
      IF (EXPA == 0) EXPA = 2
      BULK = E/THREE/(ONE-TWO*NU)
c-------------------------------------------------------------
c     strain rate filtering
      IF(IFLAGSR==1) THEN
        ISRATE  = 1 
        FCUT    = 10000.0D0*FAC_T_WORK
      ELSE 
        IF(ISRATE == 1 .AND. FCUT == ZERO)FCUT   = 10000.0D0*FAC_T_WORK
      ENDIF
c-------------------------------------------------------------
      IF (IFLAG == 2 )THEN
        IF( K1==ZERO .OR. K2==ZERO)THEN
         K1 = ONE
         K2 = ZERO
        ENDIF
      ENDIF
c-------------------------------------------------------------
      NUVAR = 5 + NFUNC + 3 + 2  +10! +10 to be removed for testing

c
      UPARAM(1) = E
      UPARAM(2) = NU
C
      UPARAM(3) = AL1
      UPARAM(4) = AL2     
      UPARAM(5) = AL3
      UPARAM(6) = AL4
      UPARAM(7) = AL5
      UPARAM(8) = AL6
      UPARAM(9) = AL7
      UPARAM(10)= AL8
      UPARAM(11)= AL9
      UPARAM(12)= AL10
      UPARAM(13)= AL11
      UPARAM(14)= AL12
C
      UPARAM(15)= EXPA
C
      IF (IFLAG == 1) THEN
       UPARAM(16)= ALPHA
       UPARAM(17)= NEXP
       UPARAM(18)= EPSO
       UPARAM(19)= ASWIFT
      ENDIF 

      G  = HALF*E/(ONE+NU)

      UPARAM(21) = ONE/(ONE - NU*NU)
      UPARAM(22) = NU*UPARAM(21)
      UPARAM(23) = G 

      UPARAM(24) = IFLAG

      UPARAM(25) = NFUNC 
      DO I=1,NFUNC
        UPARAM(25+I)=RATE(I)
        UPARAM(25 + I + NFUNC)=YFAC(I)
      ENDDO
      IF (IFLAG == 1) THEN
         UPARAM(25+2*NFUNC+1)=QVOCE
         UPARAM(25+2*NFUNC+2)=BETA
         UPARAM(25+2*NFUNC+3)=KO
      ENDIF !

      IF (IFLAG == 0 .OR.IFLAG == 1) THEN
         UPARAM(25+2*NFUNC+4) =UNSP   
         UPARAM(25+2*NFUNC+5) =UNSC   

         UPARAM(25+2*NFUNC+6) =UNSPT   
         UPARAM(25+2*NFUNC+7) =UNSCT   
 
         UPARAM(25+2*NFUNC+8) =IFLAGSR  
      ENDIF !(IFLAG == 0 .OR.IFLAG == 1) THEN
      UPARAM(25+2*NFUNC+9) =FLAG_FIT 


      IF(IFLAG == 1) THEN
        IYIELD = 1 !swift-voce
      ELSEIF (IFLAG==2) THEN
        IYIELD = 2 !HAnsel
      ELSE
        IF(IFLAGSR == 1) THEN
         IYIELD = 3 ! tabulated + Plas Strain rate 
        ELSE
         IYIELD = 4 ! tabulated +  Strain rate 
        ENDIF
      ENDIF
c-------------------------------------------------------------
      NUPARAM = 25 + NFUNC*2+9 ! =34+2*nfunc
c-------------------------------------------------------------
      NFLAG3 = 34 + 2*NFUNC
      IF (IFLAG == 2) THEN
        UPARAM(NFLAG3+1)   = K1    
        UPARAM(NFLAG3+2)   = K2    
        UPARAM(NFLAG3+3)   = AHS   
        UPARAM(NFLAG3+4)   = BHS   
        UPARAM(NFLAG3+5)   = MHS   
        UPARAM(NFLAG3+6)   = EPS0HS
        UPARAM(NFLAG3+7)   = NHS   
        UPARAM(NFLAG3+8)   = HMART 
        UPARAM(NFLAG3+9)   = TEMP0 
        UPARAM(NFLAG3+10)  = ETA   
        UPARAM(NFLAG3+11)  = CP    
        UPARAM(NFLAG3+12)  = TREF
        UPARAM(NFLAG3+13)  = AM
        UPARAM(NFLAG3+14)  = BM        
        UPARAM(NFLAG3+15)  = CM        
        UPARAM(NFLAG3+16)  = DM 
        UPARAM(NFLAG3+17)  = QM       
        UPARAM(NFLAG3+18)  = PPM        
        UPARAM(NFLAG3+19)  = E0MART        
        UPARAM(NFLAG3+20)  = VM0              
      ENDIF !(IFLAG == 2) THEN
      UPARAM(NFLAG3+21)  = IYIELD        
c---------------------------------
C IF KINEMATIC HARDENING
c---------------------------------
      NFLAG4 = NFLAG3 + 21
      UPARAM(NFLAG4+1)  = FISOKIN        

      IF (FISOKIN >ZERO) THEN       
       UPARAM(NFLAG4+2)   =  CKH(1)
       UPARAM(NFLAG4+3)   =  AKH(1)
       UPARAM(NFLAG4+4)   =  CKH(2)
       UPARAM(NFLAG4+5)   =  AKH(2)
       UPARAM(NFLAG4+6)   =  CKH(3)
       UPARAM(NFLAG4+7)   =  AKH(3)
       UPARAM(NFLAG4+8)   =  CKH(4)
       UPARAM(NFLAG4+9)   =  AKH(4)
      ENDIF
      NUPARAM = NFLAG4 + 9 ! =34+2*nfunc
c---------------------------------
C      PM PARMAT
C---------------------------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0 
      PM(9) = TWO*PI *FCUT   

      PARMAT(1) = BULK
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
CC     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
c-------------------------------------------------------------
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%L_SEQ  = 1
      MTAG%G_SEQ  = 1
      IF ( FISOKIN/= ZERO) THEN
         MTAG%L_SIGB = 12
      ENDIF
c-------------------------------------------------------------
      WRITE(IOUT,1001) TRIM(TITR),ID,87
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED)THEN                                     
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE     
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1100)E,NU 
        IF(FLAG_FIT /=1) THEN
          WRITE(IOUT,1200)AL1, AL2, AL3, AL4 ,
     .    AL5, AL6, AL7, AL8
        ELSE
          WRITE(IOUT,1201)AL1, AL2, AL3, AL4 ,
     .    AL5, AL6, AL7, AL8
        ENDIF
        WRITE(IOUT,1202) EXPA,ISRATE,FCUT,NRATE
        WRITE(IOUT,1400) IFLAG,IFLAGSR,FISOKIN
        IF(IFLAG==1)THEN
          WRITE(IOUT,1401)INVP , INVC,QVOCE,BETA,KO,ALPHA,ASWIFT,NEXP,EPSO
        ELSEIF (IFLAG==2)THEN
          WRITE(IOUT,1402) 
     .    K1    ,K2    ,AHS   ,BHS   ,MHS   ,EPS0HS,NHS   ,HMART ,TEMP0 ,
     .    TREF  ,ETA   ,CP    ,AM    ,BM    ,CM    ,DM    ,PPM   ,QM    ,E0MART,VM0   
        ENDIF

        IF(NRATE>0.AND.IFLAG==0)
     .     WRITE(IOUT,1300)(IFUNC(I),YFAC(I),RATE(I),I=1,NFUNC)

        IF(FISOKIN > ZERO)
     .     WRITE(IOUT,1500)(CKH(I), AKH(I),I=1,4)

        WRITE(IOUT,*)' '

      ENDIF
      RETURN
 1000 FORMAT(
     & 5X,40H  BARLAT YLD2000                        ,/,
     & 5X,40H  --------------                        ,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     &  5X,'INITIAL DENSITY. . . . . . . . . . . . =',1PG20.13/)  
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO . . . . . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'FORMULATION FLAG. . . . . . . . . .=',I10/
     & 5X,'. . . . . . . . . 0 = TABULATED  ',/
     & 5X,'. . . . . . . . . 1 = SWIFT-VOCE ',/
     & 5X,'. . . . . . . . . 2 = HANSAEL '   ,/
     & 5X,'STRAIN RATE FLAG. . . . . . . . . .=',I10/
     & 5X,'. . . . . . . . . 0 = STRAIN RATE  ',/
     & 5X,'. . . . . . . . . 1 = PLASTIC STRAIN RATE ',/
     & 5X,'HARDENING COEFFICIENT. . . . . . . . . . . . .=',E12.4/)
 1401 FORMAT(
     & 5X,'COWPER SEYMONDS EXPONENT P . . . . . . . . . .=',E12.4/
     & 5X,'COWPER SEYMONDS COEFFICIENT C. . . . . . . . .=',E12.4/
     & 5X,'YIELD VOCE PARAMETER Q . . . . . . . . . . . .=',E12.4/
     & 5X,'YIELD VOCE PARAMETER B . . . . . . . . . . . .=',E12.4/
     & 5X,'YIELD VOCE PARAMETER K0. . . . . . . . . . . .=',E12.4/
     & 5X,'YIELD GLOBAL PARAMETER ALPHA . . . . . . . . .=',E12.4/
     & 5X,'YIELD SWIFT PARAMETER A. . . . . . . . . . . .=',E12.4/
     & 5X,'YIELD SWIFT EXPONENT N . . . . . . . . . . . .=',E12.4/
     & 5X,'YIELD SWIFT REFERENCE STRAIN . . . . . . . . .=',E12.4)
 1402 FORMAT(
     & 5X,'TEMPERATURE DEPENDENCY PARAMETER IN HANSEL HARDENING LAW K1. .=',E12.4/
     & 5X,'TEMPERATURE DEPENDENCY PARAMETER IN HANSEL HARDENING LAW K2. .=',E12.4/
     & 5X,'PARAMETER IN HANSEL HARDENING LAW AHS. . . . . . . . . . . . .=',E12.4/
     & 5X,'PARAMETER IN HANSEL HARDENING LAW BHS. . . . . . . . . . . . .=',E12.4/
     & 5X,'FACTOR M IN HANSEL HARDENING LAW . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'PARAMETER IN HANSEL HARDENING LAW EPSH0. . . . . . . . . . . .=',E12.4/
     & 5X,'EXPONENT N IN HANSEL HARDENING LAW NHS . . . . . . . . . . . .=',E12.4/
     & 5X,'FACTOR FOR MARTENSITE DH IN HANSEL HARDENING . . . . . . . . .=',E12.4/
     & 5X,'INITIAL TEMPERATURE. . . . . . . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'REFERENCE TEMPERATURE. . . . . . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'TAYLOR QUINEY COEFFICIENT ETA. . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'SPECIFIC HEAT FOR TEMPERATURE CALCULATION. . . . . . . . . . .=',E12.4/
     & 5X,'PARAMETER AM FOR MARTENSITE RATE EQUATION. . . . . . . . . . .=',E12.4/ 
     & 5X,'PARAMETER BM FOR MARTENSITE RATE EQUATION. . . . . . . . . . .=',E12.4/ 
     & 5X,'PARAMETER CM FOR MARTENSITE RATE EQUATION. . . . . . . . . . .=',E12.4/ 
     & 5X,'PARAMETER DM FOR MARTENSITE RATE EQUATION. . . . . . . . . . .=',E12.4/ 
     & 5X,'PARAMETER PM FOR MARTENSITE RATE EQUATION. . . . . . . . . . .=',E12.4/ 
     & 5X,'PARAMETER QM FOR MARTENSITE RATE EQUATION. . . . . . . . . . .=',E12.4/ 
     & 5X,'MARTENSITE STARTING VALUE E0MART . . . . . . . . . . . . . . .=',E12.4/      
     & 5X,'INITIAL FRACTION OF MARTENSITE VM0 . . . . . . . . . . . . . .=',E12.4)
 1200 FORMAT(
     & 5X,'ANISOTROPY COEFFICIENT ALPHA1. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA2. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA3. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA4. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA5. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA6. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA7. . . . . .=',1PG20.13/
     & 5X,'ANISOTROPY COEFFICIENT ALPHA8. . . . . .=',1PG20.13/)
 1201 FORMAT(
     & 5X,'YIELD STRESS IN DIRECTION 00 . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS IN DIRECTION 45 . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS IN DIRECTION 90 . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS FOR BIAXIAL . . . . . . . .=',1PG20.13/
     & 5X,'R-VALUE IN DIRECTION 00. . . . . . . . .=',1PG20.13/
     & 5X,'R-VALUE IN DIRECTION 45. . . . . . . . .=',1PG20.13/
     & 5X,'R-VALUE IN DIRECTION 90. . . . . . . . .=',1PG20.13/
     & 5X,'R-VALUE  FOR BIAXIAL . . . . . . . . . .=',1PG20.13/)
 1202 FORMAT(
     & 5X,'EXPONENT IN YIELD CRITERION. . . . . . .=',1PG20.13//
     & 5X,'SMOOTH STRAIN RATE OPTION. . . . . . . .=',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY. . . . . .=',1PG20.13/
     & 5X,'NUMBER OF YIELD FUNCTIONS. . . . . . . .=',I10/)
 1300 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER. . . . . . =',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . =',1PG20.13/
     & 5X,'STRAIN RATE . . . . . . . . . . . . . . =',1PG20.13/)

 1500 FORMAT(
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER C1. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER A1. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER C2. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER A2. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER C3. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER A3. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER C4. . =',1PG20.13/
     & 5X,'CHABOCHE_ROUSSELIER KINEMATIC PARAMETER A4. . =',1PG20.13)
999    CALL FREERR(3)
      RETURN
      END

