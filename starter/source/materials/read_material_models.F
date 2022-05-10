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
Chd|  READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FILL_BUFFER_51                source/materials/mat/mat051/fill_buffer_51.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        INI_EOS_VARS                  source/elements/elbuf_init/ini_eos_vars.F
Chd|        INI_MATPARAM                  source/materials/mat/ini_matparam.F
Chd|        INI_MLAW_VARS                 source/elements/elbuf_init/ini_mlaw_vars.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE READ_MATERIAL_MODELS(
     .           MATPARAM_TAB,MLAW_TAG ,FAIL_TAG ,VISC_TAG ,EOS_TAG  ,
     .           BUFMAT      ,BUFLEN   ,IPM      ,PM       ,UNITAB   ,
     .           MULTI_FVM   ,MAXEOS   ,FAILWAVE ,NLOC_DMG ,LSUBMODEL,
     .           TABLE       ,NPC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE HM_OPTION_READ_MOD
      USE TABLE_MOD
c-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tablen_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
#include      "alefvm.inc"
#include      "conv_c.inc"
#include      "mmale20_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: BUFLEN,MAXEOS
      INTEGER,INTENT(IN) :: NPC(*)
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT)        :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(INOUT)        :: PM
      my_real ,DIMENSION(*), INTENT(INOUT),TARGET              :: BUFMAT

      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(MLAW_TAG_) , DIMENSION(NUMMAT  ) , INTENT(INOUT)     :: MLAW_TAG
      TYPE(EOS_TAG_)  ,DIMENSION(0:MAXEOS) , INTENT(INOUT)     :: EOS_TAG
      TYPE(FAIL_TAG_) ,DIMENSION(0:MAXFAIL), INTENT(INOUT)     :: FAIL_TAG
      TYPE(VISC_TAG_) ,DIMENSION(0:MAXVISC), INTENT(INOUT)     :: VISC_TAG
      TYPE(MULTI_FVM_STRUCT)                                 :: MULTI_FVM
      TYPE(FAILWAVE_STR_)                                    :: FAILWAVE 
      TYPE(NLOCAL_STR_)                                      :: NLOC_DMG 
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(INOUT) :: MATPARAM_TAB
      TYPE(SUBMODEL_DATA) ,INTENT(IN) ::LSUBMODEL(*)
      TYPE(TTABLE) TABLE(NTABLE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ::  I,J,K,N,IADBUF,IMAT, ILAW, JALE, JTUR, JTHE, 
     .     IBID, CODCONV,CODREZO, IAD_THERM, IALEFVM_FLG, IGFLAG, MFLAG,
     .     IFUNC_ALPHA,IFORM,IGAS,IMOLE,ID,MID1,MID2,UID1,UID2, ILAW1,ILAW2,NPAR,MAT_ID,UID
      INTEGER :: KK
      LOGICAL IS_ENCRYPTED, IS_AVAILABLE
      INTEGER ,DIMENSION(NUMMAT) :: ITHERM_FOR
      my_real ::  UPWM,UPWO,FLRD,T0,SPH,AS,BS,T1,AL,BL,PUN,
     .   RBID,FSCAL_ALPHA,SPHM1,EFRAC
      CHARACTER TITR*nchartitle,KEY*ncharkey,KEY2*ncharkey,MESS*40
C
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
      INTEGER,EXTERNAL :: NINTRI
      INTEGER NRUPT
      PARAMETER (NRUPT = 10) 
      INTEGER IFUNC(MAXFUNC), NVAR_FAIL(MAXLAW,NRUPT) 
      INTEGER :: NB_EULERMAT, NB_ALEMAT  
      my_real, DIMENSION(:), POINTER :: UPARAM  
      CHARACTER(len=2048) :: LIGNE
C-----------------------------------------------
      LIGNE(1:2048) =''
      DATA IADBUF /1/
      DATA MESS/'MATERIAL DEFINITION                     '/
c-------------------------------------------------------------------===============
C     Set default parameters used in material laws
c----
      CALL INI_MLAW_VARS(MLAW_TAG ,FAIL_TAG ,VISC_TAG ,NUMMAT)  ! attention ca ne marchera pas
      CALL INI_EOS_VARS (EOS_TAG,  MAXEOS )
      CALL INI_MATPARAM (MATPARAM_TAB ,NUMMAT)
c----------      
      ILAW    = 0
      BUFLEN  = 0
      ILAG    = 0
      IALE    = 0
      IEULER  = 0
      ITHERM  = 0
      ITURB   = 0
      PUN     = EM01
c
      PM(1:100,NUMMAT)=ZERO
c
c
c     Initialise number of material in law151
      MULTI_FVM%NBMAT = 0
c     Initialization of failwave option
      FAILWAVE%WAVE_MOD = 0
      FAILWAVE%NDDL = 0

      IF (NUMMAT == 1) THEN
        ILAG=1
        RETURN
      ENDIF
c-------------------------------------------------------------------      
      WRITE(IOUT,1000)NUMMAT-1
c-------------------------------------------------------------------

c-------------------------------------------------------------------
c     Material laws
c-------------------------------------------------------------------
      
      CALL HM_READ_MAT(
     .     MATPARAM_TAB,MLAW_TAG    ,FAIL_TAG    ,EOS_TAG     ,BUFMAT      ,
     .     BUFLEN      ,IADBUF      ,MAXEOS      ,IPM         ,PM          ,
     .     MULTI_FVM   ,UNITAB      ,LSUBMODEL   ,TABLE)
c
c-------------------------------------------------------------------
c     Equations of State
c-------------------------------------------------------------------
c
      CALL HM_READ_EOS(IPM      ,PM       ,BUFMAT   ,BUFLEN   ,IADBUF   ,
     .                 EOS_TAG  ,MAXEOS   ,UNITAB   ,LSUBMODEL)
c
c
      !CALL LECEOS(IPM,PM,BUFMAT,BUFLEN,IADBUF,UNITAB,EOS_TAG,MAXEOS)
c
c-------------------------------------------------------------------
c     Failure Models
c-------------------------------------------------------------------
c      
      CALL HM_READ_FAIL(FAIL_TAG,FAILWAVE ,NLOC_DMG,IPM     ,PM       ,
     .                  BUFMAT  ,BUFLEN   ,IADBUF  ,UNITAB  ,LSUBMODEL,
     .                  NRUPT   ,NVAR_FAIL,TABLE   )
c-------------------------------------------------------------------
c     Visco elastic  Models
c-------------------------------------------------------------------
c      
      CALL HM_READ_VISC(VISC_TAG,IPM,PM,BUFMAT,BUFLEN,IADBUF,UNITAB,LSUBMODEL,TABLE)
c
c
c      CALL LECVISC(VISC_TAG,IPM,PM,BUFMAT,BUFLEN,IADBUF,UNITAB)       
c
c-------------------------------------------------------------------
c     Fabric Leakage Models
c-------------------------------------------------------------------
c
      CALL HM_READ_LEAK(IPM,PM,UNITAB,LSUBMODEL)
c
c-------------------------------------------------------------------
C     /ALE/MAT
c-------------------------------------------------------------------

        CALL HM_OPTION_COUNT('/ALE/MAT',NB_ALEMAT)
        CALL HM_OPTION_START('/ALE/MAT')
        
        DO KK=1,NB_ALEMAT
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID)
          CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)
          CALL HM_GET_FLOATV('ALE_FLRD',FLRD,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          IALEFVM_FLG = 0
          DO I=1,NUMMAT-1                     
            IF(IPM(1,I) == ID) THEN                   
              ILAW = IPM(2,I)                        
              IF(ILAW == 11)THEN                     
               IF(FLRD == ZERO) FLRD=UN              
              ELSE                                   
               FLRD=MAX(ZERO,FLRD)                   
               FLRD=MIN(UN,FLRD)                     
              ENDIF                                  
              PM(72,I) =UNP1                         
              PM(92,I) =FLRD  
              IPM(251,I)= IALEFVM_FLG                
              IF(IALEFVM_FLG.GE.2)THEN               
                WRITE(IOUT,2011)ID,IALEFVM_FLG, FLRD  
                IALEFVM = 1                          
              ELSE                                   
                WRITE(IOUT,2010)ID,FLRD               
                IALEFVM_FLG = 1                      
              ENDIF                                  
            ENDIF                                    
          ENDDO                                      
        ENDDO

c-------------------------------------------------------------------
C     /EULER/MAT
c-------------------------------------------------------------------

        CALL HM_OPTION_COUNT('/EULER/MAT',NB_EULERMAT)
        CALL HM_OPTION_START('/EULER/MAT')
        
        DO KK=1,NB_EULERMAT
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID)
          CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)
          CALL HM_GET_FLOATV('EULER_FLRD',FLRD,IS_AVAILABLE,LSUBMODEL,UNITAB)
          IALEFVM_FLG = 0
          DO I=1,NUMMAT-1                                
            IF(IPM(1,I) == ID)THEN                        
                ILAW = IPM(2,I)                          
              IF(ILAW == 11)THEN                         
                IF(FLRD == ZERO) FLRD=UN                 
              ELSE                                       
                FLRD=MAX(ZERO,FLRD)                      
                FLRD=MIN(UN,FLRD)                        
              ENDIF                                      
              PM(72,I)  = DEUXP1                         
              PM(92,I)  = FLRD      
              IPM(251,I)= IALEFVM_FLG                    
              IF(IALEFVM_FLG.GE.2)THEN                   
                WRITE(IOUT,2021)ID,IALEFVM_FLG, FLRD      
                IALEFVM = 1                              
              ELSE                                       
                WRITE(IOUT,2020)ID,FLRD                   
                IALEFVM_FLG = 1                          
              ENDIF                                      
            ENDIF                                        
          ENDDO                                          
        ENDDO

c-------------------------------------------------------------------
c     multimaterial law buffer (need to be done after lecmat and before inivol)
c         LAW51 - IFORM=12
c-------------------------------------------------------------------
     
      DO I=1,NUMMAT-1
        TITR =MATPARAM_TAB(I)%TITLE        
        ILAW   = IPM(2,I)
        IF(ILAW.NE.51)CYCLE
        IADBUF = IPM(7,I)
        NPAR   = IPM(9,I)
        IADBUF = MAX(1,IADBUF)
        UPARAM => BUFMAT(IADBUF:IADBUF+NPAR)
        IFORM = NINT(UPARAM(31))
        IPM(62, I) = IFORM
        MAT_ID=IPM(1,I)
        IF(IFORM.EQ.12)THEN
          CALL FILL_BUFFER_51( IPM, PM, UPARAM, BUFMAT, MAT_ID, TITR, I) 
          PM(38,I) = UPARAM(42)
        ENDIF
      ENDDO

c-------------------------------------------------------------------
c     Thermal parameters for materials : /HEAT/MAT
c-------------------------------------------------------------------
c
      CALL HM_READ_THERM(BUFMAT   ,BUFLEN   ,IPM      ,PM       ,
     .                   UNITAB   ,LSUBMODEL)
c
c-------------------------------------------------------------------
c     Thermal material expansion : /THERM/STRESS
c-------------------------------------------------------------------

      CALL HM_READ_THERM_STRESS(MLAW_TAG   ,IPM   ,PM  , UNITAB  ,LSUBMODEL)
c
c-------------------------------------------------------------------
c     Non-local regularization : /NONLOCAL
c-------------------------------------------------------------------
c
      CALL HM_READ_NONLOCAL(NLOC_DMG ,IPM      ,UNITAB   ,LSUBMODEL)
c
c-------------------------------------------------------------------
      !ALL ALE MATERIAL LAW : SSP STORAGE 
c-------------------------------------------------------------------
      DO I=1,NUMMAT-1   
        ILAW = IPM(2,I)          
        JALE = NINT(PM(72,I))          
        IF(JALE /=0)THEN               
          IF(ILAW.EQ.1 .OR.
     .       ILAW.EQ.2 .OR.
     .       ILAW.EQ.3 .OR.
     .       ILAW.EQ.4 .OR.
     .       ILAW.EQ.5 .OR.
     .       ILAW.EQ.6 .OR.
     .       ILAW.EQ.7 .OR.
     .       ILAW.EQ.8 .OR.
     .       ILAW.EQ.9 .OR.
     .       ILAW.EQ.10 .OR.
     .       ILAW.EQ.11 .OR.
     .       ILAW.EQ.13 .OR.
     .       ILAW.EQ.16 .OR.
     .       ILAW.EQ.17 .OR.
     .       ILAW.EQ.18 .OR.
     .       ILAW.EQ.20 .OR.
     .       ILAW.EQ.21 .OR.
     .       ILAW.EQ.22 .OR.
     .       ILAW.EQ.23 .OR.
     .       ILAW.EQ.26 .OR.
     .       ILAW.EQ.29 .OR.
     .       ILAW.EQ.30 .OR.
     .       ILAW.EQ.31 .OR.
     .       ILAW.EQ.36 .OR.
     .       ILAW.EQ.37 .OR.
     .       ILAW.EQ.41 .OR.
     .       ILAW.EQ.44 .OR.
     .       ILAW.EQ.46 .OR.
     .       ILAW.EQ.47 .OR.
     .       ILAW.EQ.49 .OR.
     .       ILAW.GE.50     )THEN     
               MLAW_TAG(I)%L_SSP    = 1 
               IF(ILAW.EQ.20)THEN
                 uID1                  = NINT(PM(21,I))
                 uID2                  = NINT(PM(22,I))
                 MID1                  = NINTRI(uID1,IPM,NPROPMI,NUMMAT,1)
                 MID2                  = NINTRI(uID2,IPM,NPROPMI,NUMMAT,1)
                 ILAW1                 = IPM(2,MID1)
                 ILAW2                 = IPM(2,MID2)               
                 MLAW_TAG(MID1)%L_SSP = 1 ! boundary layer material (ilaw updated later in sgrtail.F)
                 MLAW_TAG(MID2)%L_SSP = 1 ! boundary layer material (ilaw updated later in sgrtail.F) 
               ENDIF               
          ENDIF                        
        ENDIF 
      ENDDO!next I
c-------------------------------------------------------------------
      !ALE code convection par materiau et global
c-------------------------------------------------------------------
      DO I=1,LCONV
        CODV(I)=0
      ENDDO
      IF(IALE+IEULER.NE.0.AND.ICONVCP == 1)THEN
        WRITE(IOUT,2008)
      END IF
      ! mass and energy convection for ALE/EULER laws
      DO I=1,NUMMAT-1
        ILAW = IPM(2,I)
        JTUR = NINT(PM(70,I))
        JTHE = NINT(PM(71,I))
        JALE = NINT(PM(72,I))
        CODCONV=0
        CODREZO=0
        IF(JALE.GT.0)THEN
          IF(ILAW.EQ.1 .OR.
     .       ILAW.EQ.2 .OR.
     .       ILAW.EQ.3 .OR.
     .       ILAW.EQ.4 .OR.
     .       ILAW.EQ.5 .OR.
     .       ILAW.EQ.6 .OR.
     .       ILAW.EQ.7 .OR.
     .       ILAW.EQ.8 .OR.
     .       ILAW.EQ.9 .OR.
     .       ILAW.EQ.10 .OR.
     .       ILAW.EQ.11 .OR.
     .       ILAW.EQ.13 .OR.
     .       ILAW.EQ.16 .OR.
     .       ILAW.EQ.17 .OR.
     .       ILAW.EQ.18 .OR.
     .       ILAW.EQ.20 .OR.
     .       ILAW.EQ.21 .OR.
     .       ILAW.EQ.22 .OR.
     .       ILAW.EQ.23 .OR.
     .       ILAW.EQ.26 .OR.
     .       ILAW.EQ.29 .OR.
     .       ILAW.EQ.30 .OR.
     .       ILAW.EQ.31 .OR.
     .       ILAW.EQ.36 .OR.
     .       ILAW.EQ.37 .OR.
     .       ILAW.EQ.41 .OR.
     .       ILAW.EQ.44 .OR.
     .       ILAW.EQ.46 .OR.
     .       ILAW.EQ.47 .OR.
     .       ILAW.EQ.49 .OR.
     .       ILAW.GE.50     )THEN
                CODCONV = 11
                CODV(1) = 01
                CODV(2) = 01
          ENDIF
        ENDIF
        IF(JTUR.GT.0) THEN
          CODCONV=CODCONV + 1100
          CODV(3)=1
          CODV(4)=1
        ENDIF
        IF(ILAW == 37 .OR. ILAW == 41)THEN
          CODCONV=CODCONV+10000
          CODV(5)=1
        ENDIF
        IF(JALE.GT.0 .AND.(
     .     ILAW == 2  .OR.
     .     ILAW == 3  .OR.
     .     ILAW == 4  .OR.
     .     ILAW == 7  .OR.
     .     ILAW == 8  .OR.
     .     ILAW == 9  .OR.
     .     ILAW == 10 .OR.
     .     ILAW == 16 .OR.
     .     ILAW == 21 .OR.
     .     ILAW == 22 .OR.
     .     ILAW == 23 .OR.
     .     ILAW == 26 .OR.
     .     ILAW == 28 .OR.
     .     ILAW == 29 .OR.
     .     ILAW == 30 .OR.
     .     ILAW == 31 .OR.
     .     ILAW == 36 .OR.
     .     ILAW == 44 .OR.
     .     ILAW == 49 .OR.
     .     ILAW == 97       )  )THEN
             CODREZO=11
           ENDIF
         IF(JALE.GT.0 .AND. ILAW == 1)THEN
            CODREZO=1
         ENDIF
         
        !---ALEFVM---! 
        IF(JALE > 0 .AND. IALEFVM == 1)THEN
          !Momentum Convection : %MOM(1:3,:)
          CODCONV=CODCONV+011100000 
          CODV(6) = 1
          CODV(7) = 1
          CODV(8) = 1                    
        ENDIF
        
        !Seems no longer used since nothing was planned for NVAR=6 
        IF(JALE.GT.0 .AND. ICONVCP == 1 .AND.(
     .     ILAW == 11 .OR.
     .     ILAW == 67      ) )THEN
         !ts 12feb2016 : nothing was done in aconve for nvar=6,7,8,9,  law67 may be re-plugged later
 !          CODCONV=CODCONV+111100000
!          CODV(6) =1
!          CODV(7) =1
!          CODV(8) =1
!          CODV(9) =1
          !ts 12feb2016 : nothing was done in aconve for nvar=6,7,8,9,  law67 may be re-plugged later
        ELSEIF(JALE > 0 .AND. ITHERM > 0 .AND. ILAW == 51 )THEN
         CODCONV = CODCONV + 100000
         CODV(6) =1          
        END IF
 
        PM(10,I)=CODCONV+PUN        
        PM(11,I)=CODREZO+PUN        

      ENDDO
      NVCONV=0
      DO I=1,LCONV
         IF(CODV(I) == 1)THEN
            NVCONV=NVCONV+1
            CODV(I)=NVCONV
         ENDIF
      ENDDO

C------------------------------
      RETURN
 999  CALL ANCMSG(MSGID=55,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=KEY0(KCUR),
     .            C2=KLINE,
     .            C3=LINE)
         CALL ARRET(2)
      RETURN
c------------------------------
 1000 FORMAT(//
     .    '    MATERIAL LAWS'/
     .    '    -------------'/,
     .    '    NUMBER OF MATERIALS. . . . . . . . . =',I10//)
 2000 FORMAT(/
     & 5X,'    THERMAL MATERIAL  ',/,
     & 5X,'    ----------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . =',I10/,
     & 5X,'T0. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'SPECIFIC HEAT . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'AS. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BS. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'T1. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'AL. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'BL. . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/,
     & 5X,'HEAT TRANSFERT FORMULATION. . . . . . . . . . = ',I10/,
     & 10X,'    0: it is based on Finite Volume  Method',/        
     & 10X,'    1: it is based on Finite Element Method '/
     & 5X,'FRACTION OF STRAIN ENERGY CONVERTED INTO HEAT =',1PG20.13/)
 2008  FORMAT(
     & 5X,'    TRANSPORT of SPECIFIC HEAT for LAW67 is ON ',/,
     & 5X,'    ------------------------------------------- ',/)
 2010 FORMAT(
     & 5X,'    ALE MATERIAL  ',/,
     & 5X,'    ------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//)
 2011 FORMAT(
     & 5X,'    ALE MATERIAL  ',/,
     & 5X,'    ------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'FINITE VOLUME METHOD FOR MOMENTUM . . .=',I10/,      
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//)     
 2020 FORMAT(
     & 5X,'    EULERIAN MATERIAL  ',/,
     & 5X,'    -----------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//)
 2021 FORMAT(
     & 5X,'    EULERIAN MATERIAL  ',/,
     & 5X,'    -----------------  ',/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'FINITE VOLUME METHOD FOR MOMENTUM . . .=',I10/,     
     & 5X,'REDUCTION FACTOR ON BOUNDARY FLUXES . .=',1PG20.13//)     
c------------------------------
      END
