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
Chd|  HM_READ_MAT83                 source/materials/mat/mat083/hm_read_law83.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT83(UPARAM  ,MAXUPARAM ,NUPARAM  ,MTAG    ,PM       ,
     .                         NUVAR   ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   , 
     .                         UNITAB  ,MAT_ID    ,TITR     ,ISRATE  ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW83 WITH HM READER
C-----------------------------------------------
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
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,
     .                                   NUPARAM,NUVAR,ISRATE
      INTEGER, INTENT(IN)             :: MAT_ID
      my_real, INTENT(INOUT)          :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      CHARACTER*nchartitle,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MLAW_TAG_), INTENT(OUT)    :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NRATE,IFILTR,IDYIELD,IFUNN,IFUNT,
     .           RHOFLAG,ICOMP,IPLAS,ILAW
      my_real :: YOUNGT,YOUNGC,G,YOUNG,FCUT,XFAC,YFAC,XSCALE,RN,RS,BETA,ALPHA,
     .           ASRATE,A1,A2,AA,E0,EMAX,EPSMAX,RHO0,RHOR,YFAC_UNIT,
     .           XFAC_UNIT,XSCALE_UNIT,RN_UNIT,RS_UNIT
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
C-----------------------------------------------
      ILAW = 83
!
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
!
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
card1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2
      CALL HM_GET_FLOATV('MAT_E'        ,YOUNGT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G'        ,G           ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IMASS'    ,RHOFLAG     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('COMP_OPT'     ,ICOMP       ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('MAT_ECOMP'    ,YOUNGC      ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
card3
      CALL HM_GET_INTV  ('FUN_A1'       ,IDYIELD     ,IS_AVAILABLE ,LSUBMODEL)
      CALL HM_GET_FLOATV('FScale11'     ,YFAC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'     ,XFAC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA'    ,ALPHA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Beta'     ,BETA        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card4
      CALL HM_GET_FLOATV('MAT_R00'      ,RN          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_R45'      ,RS          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Fsmooth'      ,ISRATE      ,IS_AVAILABLE ,LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'         ,ASRATE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card5
      CALL HM_GET_INTV  ('FUN_A2'       ,IFUNN       ,IS_AVAILABLE ,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_A3'       ,IFUNT       ,IS_AVAILABLE ,LSUBMODEL)
      CALL HM_GET_FLOATV('FScale33'     ,XSCALE      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c-------------------------------------------------------------------------------------
      CALL HM_GET_FLOATV_DIM('FScale11' ,YFAC_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale22' ,XFAC_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale33' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_R00'  ,RN_UNIT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_R45'  ,RS_UNIT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C---
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
!
      IF (ASRATE  == ZERO) ASRATE = INFINITY
      IF (XFAC == ZERO) XFAC = UN*XFAC_UNIT
      IF (BETA  == ZERO) BETA = DEUX
      IF (RN  == ZERO) RN = UN*RN_UNIT
      IF (RS  == ZERO) RS = UN*RS_UNIT
      IPLAS = 0 ! hidden
      IF (IPLAS == 0)  IPLAS = 2
C---
      NFUNC = 3 
      IFUNC(1) = IFUNN
      IFUNC(2) = IFUNT
      IFUNC(3) = IDYIELD
C---
      IF (IFUNC(3) == 0) THEN
         CALL ANCMSG(MSGID=126, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .               I1=MAT_ID,
     .               C1=TITR,
     .               I2=IFUNC(3))
      ENDIF
   
      IF (YFAC == ZERO)   YFAC   = UN*YFAC_UNIT
      IF (XSCALE == ZERO) XSCALE = UN*XSCALE_UNIT
      IF( G == ZERO)  G = YOUNGT
      IF (YOUNGC == ZERO) YOUNGC = YOUNGT
      IF (ICOMP /= 1) ICOMP = 0
      YOUNG = MAX(YOUNGT,YOUNGC)
C----------------
      UPARAM(1) = YOUNGT
      UPARAM(2) = ALPHA
      UPARAM(3) = BETA
      UPARAM(4) = YFAC
      UPARAM(5) = XSCALE
      UPARAM(6) = RN
      UPARAM(7) = RS
      UPARAM(8) = XFAC
      UPARAM(9) = RHOFLAG
      UPARAM(10)= IPLAS
      UPARAM(11) = G
      UPARAM(12) = ICOMP
      UPARAM(13) = YOUNGC
C----------------
      NUPARAM = 13
      NUVAR   = 3
C----------------
      PARMAT(1) = YOUNG/TROIS
      PARMAT(2) = YOUNG
      PARMAT(5) = ASRATE
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) =  UN ! (UN - DEUX*NU)/(UN - NU), NU=0      
!
      MTAG%G_GAMA = 9
      MTAG%G_EPSD = 1  
      MTAG%G_PLA  = 1 !global
      MTAG%L_EPE  = 3
      MTAG%L_PLA  = 1
      MTAG%L_EPSD = 1
      MTAG%L_DMG  = 1
C----------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,83
      WRITE(IOUT,1000)
      IF (IS_CRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE                                                     
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) YOUNGT,YOUNGC,G,ICOMP,RHOFLAG,IDYIELD,YFAC,XFAC,ALPHA,BETA
        WRITE(IOUT,1400)IFUNN,IFUNT,XSCALE,RN,RS,ISRATE,ASRATE
      ENDIF
C-----------
      RETURN
C-----------
 1000 FORMAT(
     & 5X,'  CONNECTION MATERIAL LAW 83                        ',/,
     & 5X,'  --------------------------                        ',/)   
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . . .=',1PG20.13)
 1300 FORMAT(
     & 5X,'YOUNG MODULUS PER THICKNESS UNIT IN TENSION . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS PER THICKNESS UNIT IN COMPRESSION . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS PER THICKNESS UNIT  . . . . . . . . .=',1PG20.13/,
     & 5X,'COMPRESSION BEHAVIOR (FLAG) . . . . . . . . . . . .=',I10/,
     & 5X,'             = 0 => ELASTO-PLASTIC                  '/,
     & 5X,'             = 1 => LINEAR ELASTIC                  '/,
     & 5X,'DENSITY UNIT FLAG . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'             = 0 => VOLUME  DENSITY                 '/,
     & 5X,'             = 1 => SURFACE DENSITY                 '/,
     & 5X,'YIELD STRESS FUNCTION . . . . . . . . . . . . . . .=',I10/,
     & 5X,'SCALE FACTOR FOR YIELD FUNCTION . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR FOR YIELD FUNCTION ABSCISSA  . . . . .=',1PG20.13/,
     & 5X,'PARAMETER ALPHA IN YIELD FUNCTION . . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER BETA IN YIELD FUNCTION  . . . . . . . . .=',1PG20.13)
 1400 FORMAT(
     & 5X,'STRAIN RATE FUNCTION IN NORMAL DIRECTION. . . . . .=',I10/,
     & 5X,'STRAIN RATE FUNCTION IN TANGENT DIRECTION . . . . .=',I10/,
     & 5X,'SCALE FACTOR FOR STRAIN RATE IN FUNCTION  . . . . .=',1PG20.13/,
     & 5X,'RN VARIABLE . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'RS VARIABLE . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE FILTERING FLAG  . . . . . . . . . . . .=',I10/,
     & 5X,'CUT FREQ FOR STRAIN RATE FILTERING  . . . . . . . .=',1PG20.13/)
C--------
      END
