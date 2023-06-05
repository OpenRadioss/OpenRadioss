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
Chd|  HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
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
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT38(UPARAM   ,MAXUPARAM, NUPARAM   ,NUVAR  ,IFUNC    , 
     .                         MAXFUNC  ,NFUNC    , PARMAT   ,UNITAB ,MAT_ID    , 
     .                         MTAG     ,TITR     , LSUBMODEL,PM     ,IMATVIS   ,
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
C     MAT_ID          MATERIAL ID(INTEGER)
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
      my_real, DIMENSION(100)    ,INTENT(INOUT)   :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: NFUNC,NUPARAM,NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID,MAXFUNC,MAXUPARAM
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          I,J,MFUNC,IFLAG,KSMSTR
      INTEGER          ITOTAL,IUNLOAD,IFN,NPCURVE,KCOMPAIR
      INTEGER          IFLOAD(5),IFUNLOAD(5)
      INTEGER          KDECAY,KRECOVER,IFL(5),IFUNL(5)

      my_real :: E,VT,VC,RV, NU
      my_real :: BETA,HYSTER,RATEDAMP,GRADDAMP,THETA
      my_real :: P0,RELAXP,MAXPRES,PHI,GAMMA
      my_real :: FUNLOAD,RUNLOAD,EXPONAS,EXPONBS
      my_real :: TENSIONCUT
      my_real :: EFINAL,EPSFIN,LAMDA,VISCOSITY,TOLERANCE,PSCALE
      my_real :: GAMA0,FAC,FAC_P,RHO0,RHOR, FSCAL_TAB(5), EPS_TAB(5)
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IUNLOAD=0
      NFUNC=2
      NUVAR=31+1+1
      GAMMA = SEVEN_OVER_5
      NUPARAM=33
      UPARAM(1)=NUPARAM
      IMATVIS=1

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!LINE-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!LINE-2
      CALL HM_GET_FLOATV('MAT_E'     ,E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,VT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NUt'   ,VC    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_RV'    ,RV    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IFLAG' ,IFLAG ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('ITOTAL'    ,ITOTAL,IS_AVAILABLE,LSUBMODEL)
      !* E        = INITIAL MODULUS USED FOR INTERFACE AND TIMESTEP COMPUTATION
      !* VT       = MAXIMUM POISSON'S RATIO IN TENSION
      !* VC       = MAXIMUM POISSON'S RATIO IN COMPRESSION
      !* RV       = EXPONENT FOR POISSON'S RATIO COMPUTATION
      !* IFLAG    = FLAG FOR ANALYSIS FORMULATION TYPE
      !*          = 0 ; PRINCIPAL STRESSES
      !*          = 1 ; OCTAHEDRAL STRESSES
      !* ITOTAL   = FLAG FOR INCREMENTAL FORMULATION
      !*          = 0,1 ; TOTAL
      !*          = 2,3 ; INCREMENTAL!
!LINE-3
      !* DECAY, HYSTERESIS AND DAMPING
      CALL HM_GET_FLOATV('MAT_RELX'   ,BETA      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HYST'   ,HYSTER    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP1'      ,RATEDAMP  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Gflag'      ,KRECOVER  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Vflag'      ,KDECAY    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_Theta'  ,THETA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !* BETA     = RELAXATION RATE FOR UNLOADING
      !* HYSTER   = HYSTERESIS COEFFICIENT FOR UNLOADING
      !* RATEDAMP = DAMPING FACTOR ON STRAIN RATE
      !* KRECOVER = RECOVERY FLAG ON UNLOADING FOR HYSTERESIS
      !* KDECAY   = HYSTRESIS TYPE 
      !* THETA    = INTEGRATION COEFFICIENT FOR INSTANTANEOUS MODULE

!LINE-4
      !* AIR PRESSURE
      CALL HM_GET_INTV  ('MAT_Kair'    ,KCOMPAIR   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A4'      ,NPCURVE    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_PScale'  ,PSCALE     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !* KCOMPAIR = FLAG FOR AIR CONTENT COMPUTATION
      !*          = 0 NO AIR CONTENT
      !*          = 1 AIR CONTENT COMPUTATION ACTIVE
      !* NPCURVE  = PRESSURE CURVE NUMBER (P VS. RELATIVE VOLUME J)
      !* PSCALE   = SCALE FACTOR FOR PRESSURE CURVE
!LINE-5
      CALL HM_GET_FLOATV('MAT_P0'   ,P0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PR'   ,RELAXP  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PMAX' ,MAXPRES ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_POROS',PHI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !* P0       = ATMOSPHERIC PRESSURE
      !* MAXPRES  = MAXIMUM AIR PRESSURE
      !* PHI      = POROSITY
      !* PSCALE   = PRESSURE FUNCTION SCALE FACTOR
      
!LINE-6
      !* UNLOADING AND INTERPOLATION DATA
      CALL HM_GET_INTV  ('FUN_B4'     ,IUNLOAD    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_ALPHA6' ,FUNLOAD    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSF2'  ,RUNLOAD    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP1'   ,EXPONAS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EXP2'   ,EXPONBS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !* IUNLOAD = UNLOADING FUNCTION NUMBER
      !* FUNLOAD = UNLOADING SCALE FACTOR
      !* RUNLOAD = UNLOADING RATE
      !* EXPONAS = A EXPONENT FOR FUNCTION INTERPOLATION
      !* EXPONBS = B EXPONENT FOR FUNCTION INTERPOLATION
!LINE-7
      !* MAX. NO FUNCTION. ETC.
      CALL HM_GET_INTV  ('NFUNC'       ,MFUNC       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_CUTOFF'  ,TENSIONCUT  ,IS_AVAILABLE,LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_Iinsta'  ,KSMSTR      ,IS_AVAILABLE,LSUBMODEL)
      !* MFUNC   = MAX. NO. OF FUNCTIONS
      !* KSMSTR  = SMALL STRAIN FLAG
      !* TENSIONCUT = MAX. TENSILE STRESS

!LINE-8
      !* STABILITY DATA
      CALL HM_GET_FLOATV('MAT_Efinal'  ,EFINAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Epsfinal',EPSFIN    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Lamda'   ,LAMDA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MaxVisc' ,VISCOSITY ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Tol'     ,TOLERANCE ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !* EFINAL = FINAL MODULUS
      !* EPSFIN = STRAIN AT FINAL MODULUS
      !* LAMDA  = INTERPOLATION CONSTANT
      !* VISCOSITY = CONST. VISCOSITY USED FOR TIMESTEP ONLY
      !* TOLERANCE = TOLERANCE ON PRINCIPLE DIRECTION UPDATE
!FOR DIMENSION ONLY
      CALL HM_GET_FLOATV_DIM('MAT_E' ,FAC_P, IS_AVAILABLE, LSUBMODEL, UNITAB)


C----------------
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0    
C----------------

      IF (PSCALE == ZERO) PSCALE = ONE

      IF( VT       <= ZERO) VT =EM20
      IF( VT       >=HALF ) VT =ZEP499
      IF( VC       >=HALF ) VC =ZEP499
      IF( ITOTAL   > 3 ) ITOTAL = 0      
      IF( BETA     <= ZERO) BETA =EM20         
      IF( HYSTER   <= ZERO) HYSTER =ONE         
      IF( RATEDAMP <= ZERO) RATEDAMP = HALF  
      IF( KRECOVER <= 0 ) KRECOVER =0          
      IF( KRECOVER > 2 ) KRECOVER =0          
      IF( KDECAY   <= 0 ) KDECAY =0            
      IF( KDECAY   > 2 ) KDECAY =0            
      IF( THETA    <= ZERO) THETA =ZEP67
      IF( RELAXP   <= ZERO) THEN      
         RELAXP= EM20                        
      ENDIF                             
      IF( MAXPRES  <= ZERO) THEN      
         MAXPRES= INFINITY                    
      ENDIF                             
      IF( FUNLOAD<=ZERO) FUNLOAD = ONE 
      IF( EXPONAS==ZERO) EXPONAS = ONE 
      IF( EXPONBS==ZERO) EXPONBS = ONE 
      IF( MFUNC>5  ) MFUNC=5
      IF( TENSIONCUT<=ZERO) THEN
         TENSIONCUT =INFINITY
      ENDIF   
      IF(EPSFIN<=ZERO.OR.EPSFIN>ZERO) EPSFIN=ONE
      IF( LAMDA<=ZERO) LAMDA =ONE
      IF( TOLERANCE <=ZERO) TOLERANCE = ONE
      IF( VISCOSITY<=ZERO) THEN
         VISCOSITY =INFINITY
      ENDIF
      IF (MFUNC < 1) THEN
        CALL ANCMSG(MSGID=992,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      
      IF (EFINAL<=E) EFINAL = E
      !PSCALE = PSCALE * FAC_C
      !FUNLOAD = FUNLOAD * FAC_C
      UPARAM(2)=E
      UPARAM(3)=VT
      UPARAM(4)=VC
      UPARAM(5)=RV
      UPARAM(6)=IFLAG
      UPARAM(7)=ITOTAL

      UPARAM(8)=BETA
      UPARAM(9)=HYSTER
      UPARAM(10)=RATEDAMP
      UPARAM(11)=KRECOVER
      UPARAM(12)=KDECAY
      UPARAM(13)=THETA

      UPARAM(14)=KCOMPAIR
      UPARAM(15)=P0
      UPARAM(16)=GAMMA
      UPARAM(17)=RELAXP
      UPARAM(18)=MAXPRES
      UPARAM(19)=PHI

      UPARAM(20)=IUNLOAD
      UPARAM(21)=FUNLOAD
      UPARAM(22)=0.
      UPARAM(23)=EXPONAS
      UPARAM(24)=EXPONBS

      UPARAM(25)=MFUNC
      UPARAM(26)=KSMSTR
      UPARAM(27)=TENSIONCUT

      UPARAM(28)=EFINAL
      UPARAM(29)=EPSFIN
      UPARAM(30)=LAMDA
      UPARAM(31)=VISCOSITY
      UPARAM(32)=TOLERANCE
      UPARAM(33)=PSCALE



!LAST-LINES   
      DO I=1,MFUNC 
        CALL HM_GET_INT_ARRAY_INDEX  ('Funct_Id_Load'  ,IFLOAD(I)     ,I ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INT_ARRAY_INDEX  ('Funct_Id_UnLoad',IFUNLOAD(I)   ,I ,IS_AVAILABLE, LSUBMODEL)        
        CALL HM_GET_FLOAT_ARRAY_INDEX('Fscale_i'       ,FSCAL_TAB(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOAT_ARRAY_INDEX('Epsilon_i'      ,EPS_TAB(I)    ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)         
      ENDDO
      
      DO I=1,MFUNC
        UPARAM(NUPARAM+  I)     = FSCAL_TAB(I)
        UPARAM(NUPARAM+MFUNC+I) = EPS_TAB(I)
      ENDDO
      DO I=1,MFUNC
         IF(UPARAM(NUPARAM+I)<=0.) UPARAM(NUPARAM+I)=ONE
         UPARAM(NUPARAM+I) = UPARAM(NUPARAM+I) !* FAC_C
         UPARAM(NUPARAM+MFUNC+I) = UPARAM(NUPARAM+MFUNC+I) !/ FAC_T
      ENDDO
      RUNLOAD = MAX(RUNLOAD,UPARAM(NUPARAM+MFUNC+1))
      RUNLOAD = MIN(RUNLOAD,UPARAM(NUPARAM+MFUNC+MFUNC))
      UPARAM(22)=RUNLOAD
      NUPARAM=NUPARAM+MFUNC*2
      
      DO J=1,MFUNC
        IFUNC(J)=IFLOAD(J)
        IF (IFLOAD(J) == 0)THEN
           CALL ANCMSG(MSGID=1039,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=MFUNC)
           EXIT
        ENDIF
        IF(IFUNLOAD(J)<=0)IFUNLOAD(J)=IFLOAD(1)
        IFUNC(J+MFUNC)=IFUNLOAD(J)
      ENDDO
C-------to be consisting w/ dt in engine
      PARMAT(1)=E
      PARMAT(2)=E
      NU = MAX(VC,VT)
      PARMAT(3)=NU
c------------------------------------
C     Formulation for solid elements time step computation.
      IF(NPCURVE==0)THEN
        PARMAT(16) = 2
        PARMAT(17) = (ONE-TWO*NU)/(ONE-NU)
      ELSE
        PARMAT(16) = 0
        PARMAT(17) = ZERO
      END IF

      NFUNC=2*MFUNC+2
      IFUNC(NFUNC-1)=IUNLOAD
      IFUNC(NFUNC)=NPCURVE
c
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      IF (NU > 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c-----------------
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,38
      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,7000)
      ELSE
      WRITE(IOUT,1002)RHO0
      WRITE(IOUT,1003)E,VT,VC,RV,IFLAG,ITOTAL
      WRITE(IOUT,1100)BETA,HYSTER,RATEDAMP,KRECOVER,KDECAY,THETA
      WRITE(IOUT,1200)KCOMPAIR,NPCURVE,P0,RELAXP,MAXPRES,PHI
      WRITE(IOUT,1300)IUNLOAD,FUNLOAD,RUNLOAD,EXPONAS,EXPONBS
      WRITE(IOUT,1400)MFUNC,KSMSTR,TENSIONCUT
      WRITE(IOUT,1500)EFINAL,EPSFIN,LAMDA,VISCOSITY,TOLERANCE
      WRITE(IOUT,'(5X,''SCALE FACTORS : '')')
      WRITE(IOUT,'(1P5G20.13)') (UPARAM(NUPARAM-MFUNC*2+I),I=1,MFUNC)
      WRITE(IOUT,'(5X,''STRAIN RATES : '')')
      WRITE(IOUT,'(1P5G20.13)') (UPARAM(NUPARAM-MFUNC+I),I=1,MFUNC)
      WRITE(IOUT,'(5X,''LOADING CURVES : '')')
      WRITE(IOUT,FMT=FMW_5I) (IFUNC(J),J=1,MFUNC)
      WRITE(IOUT,'(5X,''UNLOADING CURVES : '')')
      WRITE(IOUT,FMT=FMW_5I) (IFUNC(J),J=1+MFUNC,MFUNC*2)
      WRITE(IOUT,*)
      ENDIF
      RETURN
 7000 FORMAT
     & (5X,55H  STRAIN RATE DEPENDENT INTERPOLATION FOR FOAM MATERIAL,/
     & ,5X,55H  -----------------------------------------------------,//
     & ,5X,  'CONFIDENTIAL DATA'//) 
 1000 FORMAT
     & (5X,55H  STRAIN RATE DEPENDENT INTERPOLATION FOR FOAM MATERIAL,/
     & ,5X,55H  -----------------------------------------------------,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,   'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,   'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1002 FORMAT(
     &  5X,  'INITIAL DENSITY. . . . . . . . . . . . =',1PG20.13/)  
 1003 FORMAT
     & (5X,  'INITIAL ELASTIC MODULUS . . . . . . . .=',1PG20.13/ 
     & ,5X,  'POISSON''S RATIO IN TENSION. . . . . . =',1PG20.13/ 
     & ,5X,  'POISSON''S RATIO IN COMPRESSION. . . . =',1PG20.13/
     & ,5X,  'EXPONENT FOR POISSON''S RATIO UPDATE. .=',1PG20.13/
     & ,5X,  'ANALYSIS FLAG . . . . . . . . . . . . .=',I10/
     & ,5X,  'FORMULATION (2,3=INCREMENTAL;0,1=TOTAL)=',I10//)
 1100 FORMAT
     & (5X,  'BETA. . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'HYSTERYSIS COEFFICIENT. . . . . . . . .=',1PG20.13/
     & ,5X,  'DAMPING COEFFICIENT ON STRAIN RATE. . .=',1PG20.13/
     & ,5X,  'RECOVERY FLAG ON UNLOADING. . . . . . .=',I10/
     & ,5X,  'HYSTERYSES MODEl. . . . . . . . . . . .=',I10/
     & ,5X,  'THETA (INCREMENTAL INTEGRATION) . . . .=',1PG20.13//)

 1200 FORMAT
     & (5X,  'AIR PRESSURE COMPUTATION FLAG . . . . .=',I10/
     & ,5X,  'AIR PRESSURE FUNCTION NUMBER. . . . . .=',I10/
     & ,5X,  'ATMOSPHERIC PRESSURE. . . . . . . . . .=',1PG20.13/
     & ,5X,  'RELAXATION PARAMETER. . . . . . . . . .=',1PG20.13/
     & ,5X,  'MAXIMUM PRESSURE. . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'POROSITY. . . . . . . . . . . . . . . .=',1PG20.13//)

 1300 FORMAT
     & (5X,  'UNLOADING FUNCTION NUMBER . . . . . . .=',I10/
     & ,5X,  'FACTOR FOR UNLOADING FUNCTION . . . . .=',1PG20.13/
     & ,5X,  'UNLOADING RATE. . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'EXPONENT FOR STRESS INTERPOLATION A . .=',1PG20.13/
     & ,5X,  'EXPONENT FOR STRESS INTERPOLATION B . .=',1PG20.13//) 

 1400 FORMAT
     & (5X,  'NUMBER OF FUNCTIONS DEFINING RATE DEPENDENCY =',I10/
     & ,5X,  'MATERIAL STABLISATION FLAG . . . . . . . . . =',I10/
     & ,5X,  'TENSION CUT-OFF STRESS. . . . . . . . .=',1PG20.13//)

 1500 FORMAT
     & (5X,  'MODULE AT INFINITY. . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'EPSFIN. . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'LAMDA . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'VISCOSITY_MAX. . . . . . . . . . . .  . .=',1PG20.13/
     & ,5X,  'TOLERANCE FOR PRINCIPAL DIRECTION UPDATE =',1PG20.13//)

      END

