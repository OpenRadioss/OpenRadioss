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
Chd|  HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED          source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_MAT76(UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    , 
     .                         MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,ID       , 
     .                         MTAG     ,TITR     ,LSUBMODEL,PM       ,ISRATE   ,
     .                         MATPARAM ,MAXTABL  ,NUMTABL  ,ITABLE   )                     
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
      USE TABLE_MOD
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)    :: ID,MAXFUNC,MAXTABL,MAXUPARAM
      INTEGER, INTENT(INOUT) :: NFUNC,NUMTABL,NUPARAM,NUVAR,ISRATE
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)   :: PM     
      my_real, DIMENSION(100)    ,INTENT(OUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(OUT)  :: IFUNC
      INTEGER, DIMENSION(MAXTABL)   ,INTENT(OUT)  :: ITABLE
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE (SUBMODEL_DATA),INTENT(IN)      :: LSUBMODEL(*)
      TYPE (MLAW_TAG_)    ,INTENT(INOUT)   :: MTAG
      TYPE (MATPARAM_STRUCT_) ,INTENT(OUT) :: MATPARAM
      TYPE (TTABLE) TABLE(NTABLE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFORM,ICONV,IQUAD,ICAS,ISRAT,ILAW
      my_real :: E,NU,G,RHO0,RHOR,FCUT,NUP,C1,A1,A2,EPSR,EPSF,
     .           XFAC,XFAC_UNIT
      my_real :: TFAC(3),YFAC(2),FAC_UNIT(5) 
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED,FOUND
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
      ILAW    = 76
Card1     
      CALL HM_GET_FLOATV('MAT_RHO'  ,RHO0 ,IS_AVAILABLE,LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho',RHOR ,IS_AVAILABLE,LSUBMODEL, UNITAB)
Card2      
      CALL HM_GET_FLOATV('MAT_E'    ,E    ,IS_AVAILABLE,LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'   ,NU   ,IS_AVAILABLE,LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_INTV  ('FUN_D1'         ,ITABLE(1)   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D2'         ,ITABLE(2)   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D3'         ,ITABLE(3)   ,IS_AVAILABLE,LSUBMODEL)
Card4      
      CALL HM_GET_FLOATV('FScale11'       ,TFAC(1)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'       ,TFAC(2)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33'       ,TFAC(3)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FACX'           ,XFAC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card5
      CALL HM_GET_FLOATV('MAT_NUt'        ,NUP         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_B5'         ,IFUNC(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_PScale'     ,YFAC(1)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISRATE'         ,ISRAT       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_asrate'     ,FCUT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card6
      CALL HM_GET_FLOATV('MAT_Epsilon_F'  ,EPSF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Epsilon_0'      ,EPSR        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
Card7 
      CALL HM_GET_INTV  ('FUN_A1'         ,IFUNC(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('SCALE'          ,YFAC(2)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card8
      CALL HM_GET_INTV  ('IFORM'          ,IFORM       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Iflag'      ,IQUAD       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Gflag'          ,ICONV       ,IS_AVAILABLE,LSUBMODEL)      
!-- unit
      CALL HM_GET_FLOATV_DIM('FScale11'   ,FAC_UNIT(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale22'   ,FAC_UNIT(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale33'   ,FAC_UNIT(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FACX'       ,XFAC_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_PScale' ,FAC_UNIT(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('SCALE'      ,FAC_UNIT(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C------------                                             
c    input check
C------------
c
      IF (FCUT == ZERO) THEN
        FCUT = 500.0D0*FAC_T_WORK
      END IF 
      ISRAT  = 0
      ISRATE = 0
c
      IF (ITABLE(1) > 0 .AND. ITABLE(2) > 0 .AND. ITABLE(3) > 0) THEN 
        ICONV = 1
      ELSE
        ICONV = 0
      ENDIF
c
      IF (ITABLE(1) == 0) THEN
         CALL ANCMSG(MSGID=126, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               I2=ITABLE(1))
      ENDIF
C
      IF (EPSF == ZERO) EPSF = INFINITY
      IF (EPSR == ZERO) EPSR = TWO*EPSF
      IF (IFORM == 1 .AND. IQUAD == 0) IQUAD = 1
C
c-----------------------------------------
c     icas      ifunt   | ifunc   | ifuncs
c       -1         1    |    1    |    1
c        0         1    |    0    |    0
c        1         1    |    1    |    0
c        2         1    |    0    |    1   
c-----------------------------------------
      ICAS = MIN(ITABLE(2),1) + MIN(ITABLE(3),1)
      IF (ICAS == 2) ICAS = -1
      IF (ITABLE(2) > 0 .AND. ICAS == 1) ICAS = 1
      IF (ITABLE(3) > 0 .AND. ICAS == 1) ICAS = 2
      NUP = MAX(ZERO, MIN(NUP, HALF))
      IF(ICAS==0 .AND. NUP == ZERO .AND. IFUNC(1)==0)NUP = HALF
c     
      IF (XFAC    == ZERO) XFAC    = XFAC_UNIT
      IF (TFAC(1) == ZERO) TFAC(1) = FAC_UNIT(1)
      IF (TFAC(2) == ZERO) TFAC(2) = FAC_UNIT(2)
      IF (TFAC(3) == ZERO) TFAC(3) = FAC_UNIT(3)
      IF (YFAC(1) == ZERO) YFAC(1) = FAC_UNIT(4)
      IF (YFAC(2) == ZERO) YFAC(2) = FAC_UNIT(5)
C
      G  = HALF*E/( ONE + NU)
      A1 = E*(ONE-NU) /((ONE + NU)*(ONE - TWO*NU))
      A2 = A1*NU/(ONE - NU)
      C1 = E/THREE/(ONE - TWO*NU)
c-----------------------------------------------
      UPARAM(1)  = E
      UPARAM(2)  = E/(ONE - NU*NU)
      UPARAM(3)  = NU*UPARAM(2)
      UPARAM(4)  = G
      UPARAM(5)  = NU
      UPARAM(6)  = A1
      UPARAM(7)  = A2
      UPARAM(8)  = C1
      UPARAM(9)  = NUP
      UPARAM(10) = EPSF
      UPARAM(11) = EPSR

      UPARAM(13) = IFORM
      UPARAM(14) = IQUAD
      UPARAM(15) = ICONV
      UPARAM(16) = FCUT*PI*TWO   ! ASRATE
      UPARAM(17) = ICAS
      UPARAM(18) = ONE / XFAC
      UPARAM(19) = ZERO ! EPDT_MIN
      UPARAM(20) = ZERO ! EPDT_MAX
      UPARAM(21) = ZERO ! EPDC_MIN
      UPARAM(22) = ZERO ! EPDC_MAX
      UPARAM(23) = ZERO ! EPDS_MIN
      UPARAM(24) = ZERO ! EPDS_MAX
      UPARAM(25) = TFAC(1)
      UPARAM(26) = TFAC(2)
      UPARAM(27) = TFAC(3)
      UPARAM(28) = YFAC(1)
      UPARAM(29) = YFAC(2)
c
      NUPARAM = 29 
      NUVAR   = 9
      NFUNC   = 2
      NUMTABL = 3
c                
c --------------------------
      PARMAT(1)  = C1
      PARMAT(2)  = E
      PARMAT(3)  = NU
      PARMAT(4)  = ISRATE
      PARMAT(5)  = ZERO ! FCUT
      PARMAT(16) = 2   ! Formulation for solid elements time step computation.
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
c
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0  
      !!-----------------------       
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
c
      MATPARAM%NTABLE = 3
      IF (ICAS == 0) THEN 
        CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM ,"COMPRESSIBLE")
      ENDIF
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL" )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN")
C-----------------------      
C
      WRITE(IOUT,1010) TRIM(TITR),ID,76
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE    
        WRITE(IOUT,1020) RHO0
        WRITE(IOUT,1100) E,NU
        WRITE(IOUT,1200) ITABLE(1),TFAC(1)
        WRITE(IOUT,1210) ITABLE(2),TFAC(2)
        WRITE(IOUT,1220) ITABLE(3),TFAC(3),XFAC
        WRITE(IOUT,1300) NUP,IFUNC(1),YFAC(1),ISRATE,FCUT
        WRITE(IOUT,1400) EPSF,EPSR,IFUNC(2),YFAC(2)
        WRITE(IOUT,1500) IFORM,IQUAD,ICONV
      ENDIF
c-----------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT(
     & 5X,' SEMI ANALYTICAL PLASTIC LAW 76  ',/,
     & 5X,' ------------------------------           ' ,//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . =',I10/)
 1020 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO. . . . . . . . . . . . . . .=',1PG20.13/)

 1200 FORMAT(
     & 5X,'TENSION YIELD STRESS FUNCTION NUMBER. . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13)
 1210 FORMAT(
     & 5X,'COMPRESSION YIELD STRESS FUNCTION NUMBER. . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13)
     
 1220 FORMAT(
     & 5X,'SHEAR YIELD STRESS FUNCTION NUMBER. . . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR .  . . . . . . . . .=',1PG20.13)
   
 1300 FORMAT(              
     & 5X,'PLASTIC POISSON RATIO   . . . . . . . . . .  =',1PG20.13/
     & 5X,'PLASTIC POISSON RATIO FUNCTION NUMBER . . .  =',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . .  =',1PG20.13/   
     & 5X,'SMOOTH STRAIN RATE OPTION. . . . . . . . . . =',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'FAILURE PLASTIC STRAIN  . . . . . . . . . . .=',1PG20.13/
     & 5X,'RUPTURE PLASTIC STRAIN. . . . . . . . . . . .=',1PG20.13/
     & 5X,'DAMAGE  FUNCTION NUMBER . . . . . . . . . .  =',I10/,
     & 5X,'DAMAGE SCALE FACTOR. . . . . . . . . . . . . =',1PG20.13 )
 1500 FORMAT(              
     & 5X,'FORMULATION FLAG . . . . . . . . . . . . .   =', I10,/
     & 5X,'       = 0  No associated formulation  ' ,/
     & 5X,'       = 1  VonMises associated formulation     ' ,/
     & 5X,' YIELD SURFACE FLAG . . . . . . . . . . . . .=', I10,/
     & 5X, '      = 0 Yield surface is linear in the Vonmises ',/
     & 5X, '      = 1 Yield surface is quadratic in the Vonmises',/
     & 5X, 'CONVEXITY CONDITION . . .  . . . .  . . .  . =',I10/)
c-----------------------------------------------------------------------
      END
