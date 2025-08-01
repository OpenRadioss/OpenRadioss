Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_mat71            ../starter/source/materials/mat/mat071/hm_read_mat71.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT71(UPARAM   ,MAXUPARAM,NUPARAM ,NUVAR  ,MFUNC  , 
     .                         MAXFUNC  ,MTAG     ,PARMAT  ,UNITAB ,
     .                         PM       ,LSUBMODEL, ID     ,TITR   ,IMATVIS,
     .                         MATPARAM )                     
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
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: ID,MAXFUNC,MAXUPARAM
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT  ! Number of declared materials
      INTEGER :: I,J,ILAW ,EFLAG
      my_real :: RHO0, RHOR,E,NU,G,C1,EPSL,GM,KM,
     .    YLD_ASS,YLD_ASF, YLD_SAS,YLD_SAF,ALPHA,
     .    LAMDA,EMART,CAS,CSA,TSAS,TFAS, TSSA,TFSA,CP,TINI

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
      ILAW    = 71
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C=======================================================================
C
C     SHAPE MEMORY 
C
C=======================================================================
Card1        
      CALL HM_GET_FLOATV('E'        ,E       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('nu'       ,NU      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('E_mart'   ,EMART   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card2  
      CALL HM_GET_FLOATV('Sig_sas'  ,YLD_ASS ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Sig_fas'  ,YLD_ASF ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Sig_ssa'  ,YLD_SAS ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Sig_fsa'  ,YLD_SAF ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Alpha'    ,ALPHA   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card3    
      CALL HM_GET_FLOATV('EpsL'     ,EPSL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CAS'      ,CAS     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CSA'      ,CSA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TSAS'     ,TSAS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TFAS'     ,TFAS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card4  
      CALL HM_GET_FLOATV('TSSA'     ,TSSA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TFSA'     ,TFSA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('CP'       ,CP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TINI'     ,TINI    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C=======================================================================
      EFLAG = 0         
      IF (YLD_ASS >= YLD_ASF) 
     .         CALL ANCMSG(MSGID=1122,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)

      IF (YLD_SAS <= YLD_SAF ) 
     .         CALL ANCMSG(MSGID=1123,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)

      IF (ALPHA > SQRT(TWO/THREE) ) 
     .         CALL ANCMSG(MSGID=1124,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)


      IF(YLD_ASS == ZERO) YLD_ASS = EM20
      IF(YLD_ASF == ZERO) YLD_ASF = EM20
      IF(YLD_SAS == ZERO) YLD_SAS = EM20
      IF(YLD_SAF == ZERO) YLD_SAF = EM20  

      IF(TSSA == ZERO) TSSA = 298.0
      IF(TFSA == ZERO) TFSA = 298.0
      IF(TSAS == ZERO) TSAS = 298.0
      IF(TFAS == ZERO) TFAS = 298.0  


      IF(CP      == ZERO) CP = EP20
      IF(TINI    == ZERO) TINI = 360.0
      IF(EMART   /= ZERO) EFLAG = 1
      IF(EMART ==    E) EFLAG = 0
      !IF(EPSL == ZERO) EPSL = un 
c HOOK 3D
      G     = HALF*E/(ONE + NU)
      LAMDA = E*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU)
      C1    = E/THREE/(ONE - TWO*NU)
c
      GM = G
      KM = C1
      IF (EFLAG == 1 ) THEN 
       GM = HALF*EMART/(ONE + NU)
       KM = EMART/THREE/(ONE - TWO*NU)
      ENDIF
c-----------------------------------
c    UPARAM
c-----------------------------------
      UPARAM(1)=E
      UPARAM(2)=NU
      UPARAM(3)= G
      UPARAM(4) = C1
      UPARAM(5) = LAMDA
      UPARAM(6) = YLD_ASS
      UPARAM(7) = YLD_ASF
      UPARAM(8) = YLD_SAS
      UPARAM(9) = YLD_SAF
      UPARAM(10) = ALPHA
      UPARAM(11) = EPSL
C HOOk 2D
      UPARAM(12 ) = E/(ONE - NU**2)
      UPARAM(13)  = NU*E/(ONE - NU**2)
      UPARAM(14)  = EMART
      UPARAM(15)  = EFLAG
      UPARAM(16)  = GM
      UPARAM(17)  = KM
      UPARAM(18)  = CAS
      UPARAM(19)  = CSA
      UPARAM(20)  = TSAS
      UPARAM(21)  = TFAS 
      UPARAM(22)  = TSSA 
      UPARAM(23)  = TFSA 
      UPARAM(24)  = CP 
      UPARAM(25)  = TINI 
c-----------------------------------
      IMATVIS = 1
c-----------------------------------
      NUPARAM = 25 
      NUVAR = 10
      MFUNC = 0
c-----------------------------------
c     PARMAT
c-----------------------------------
      PARMAT(1) = C1
      PARMAT(2) = E
      PARMAT(3) = NU  
      PARMAT(16) = 2 !Formulation for solid elements time step computation.
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU) ! ==TWO*G/(C1+sFOUR_OVER_3*G)
c--------------------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(27) = SQRT(E/MAX(RHOR,EM20)) 
c-----------------------------------
      MTAG%L_PLA  = 1
      MTAG%L_FRAC = 1   ! austenite phase fraction for output
      MTAG%L_TEMP = 1
c-----------------------------------
      MTAG%G_MAXEPS = 3
      MTAG%G_MAXFRAC= 1
c-----------------------------------
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"BEAM_INTEGRATED")
c-----------------------------------
      WRITE(IOUT,1000) TRIM(TITR),ID,71
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1001) RHO0
        WRITE(IOUT,1300)E,NU,YLD_ASS, YLD_ASF,YLD_SAS, YLD_SAF,ALPHA,EPSL,EMART
        WRITE(IOUT,1200)CAS,CSA,TSAS,TFAS,TSSA,TFSA,CP,TINI   
        ENDIF
C
      RETURN
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . =',I10/)
 1001 FORMAT(
     & 5X,'INITIAL DENSITY    . . . . . . . . . . . . . . . =',1PG20.13/)  
 1100 FORMAT(
     & 5X,'  SUPERELASTIC MATERIAL FOR SHAPE MEMORY ALLOYS LAW71 ',/,
     & 5X,'  --------------------------------------------------- ',//)
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO. . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STARTING STRESS VALUE FOR TRANSFORMATION (AS) . .=',1PG20.13/,
     & 5X,'FINAL STRESS VALUE FOR TRANSFORMATION (AS). . . .=',1PG20.13/,
     & 5X,'STARTING STRESS VALUE FOR TRANSFORMATION (SA) . .=',1PG20.13/,
     & 5X,'FINAL STRESS VALUE FOR TRANSFORMATION (SA). . . .=',1PG20.13/,
     & 5X,'PARAMETER ALPHA . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM RESIDUAL STRAIN. . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'MARTENSITE YOUNG''S MODULUS . . . . . . . . . . .=',1PG20.13/)
 1200 FORMAT(
     & 5X,'MATERIAL PARAMETER C_AS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MATERIAL PARAMETER C_SA . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE FOR TRANSFORMATION (AS) . . .=',1PG20.13/,
     & 5X,'FINAL   TEMPERATURE FOR TRANSFORMATION (AS) . . .=',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE FOR TRANSFORMATION (SA) . . .=',1PG20.13/,
     & 5X,'FINAL   TEMPERATURE FOR TRANSFORMATION (SA) . . .=',1PG20.13/,
     & 5X,'SPECIFIC HEAT CAPACITY. . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL TEMPERATURE . . . . . . . . . . . . . . .=',1PG20.13/)
C     
      END
