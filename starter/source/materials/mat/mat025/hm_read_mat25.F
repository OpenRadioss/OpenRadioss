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
Chd|  HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT25(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,UNITAB   ,LSUBMODEL,
     .           MTAG     ,ID       ,TITR     ,PM       ,ISRATE   ,
     .           PARMAT   ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW25 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
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
      INTEGER, INTENT(IN)                          :: ID
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
c
      INTEGER, INTENT(OUT)                         :: ISRATE
      my_real, DIMENSION(100)       ,INTENT(OUT)   :: PARMAT     
      my_real, DIMENSION(NPROPM)    ,INTENT(OUT)   :: PM     
      TYPE(MLAW_TAG_)               ,INTENT(OUT)   :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
c
      INTEGER :: MAXUPARAM,NUPARAM
      my_real, DIMENSION(MAXUPARAM)    :: UPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: ILAW,IOFF,ICC,IFLAG,IYEILD,IMODWP
      my_real :: RHO0,RHOR,E11,E22,E33,G12,G23,G31,N12,N21,
     .   CB,CN,EPST1,EPST2,
     .   FMAX, SIGYT1, SIGYT2, SIGYC1, SIGYC2, SIGYT12, SIGYC12, 
     .   C1, SSP, F1, F2, F11, F22, F33, F12, FT1, WPLAMX,
     .   EPSM1, EPSM2, DMAX, SHRDAM, SHRMAX, SHRDMAX,ALPHA,CC,EPS0,
     .   CBT1,CNT1,SIGMXT1,CCT1,CBT2,CNT2,SIGMXT2,CCT2,DETC, DE,
     .   CBC1,CNC1,SIGMXC1,CCC1,CBC2,CNC2,SIGMXC2,CCC2,FCUT,
     .   CBT12,CNT12,SIGMXT12,CCT12,CBC12,CNC12,SIGMXC12,CCC12,
     .   EPS1T1,EPS2T1,SIGRST1,WPLAMXT1,EPS1T2,EPS2T2,SIGRST2,
     .   WPLAMXT2,EPS1C1,EPS2C1,SIGRSC1,WPLAMXC1,EPS1C2,EPS2C2,
     .   SIGRSC2,WPLAMXC2,EPS1T12,EPS2T12,SIGRST12,WPLAMXT12,WPLAREF,
     .   EPSF1,EPSF2,RATIO,D11,D22,D12,DMIN,FAC,FSCAL_UNIT
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 25
C 
      CB=ZERO          
      CN=ZERO          
      FMAX=ZERO        
      SHRDAM =ZERO     
      SHRMAX =ZERO     
      SHRDMAX=ZERO     
      WPLAREF=ZERO
C
      SIGYT1=ZERO      
      CBT1=ZERO        
      CNT1=ZERO        
      SIGMXT1=ZERO     
      CCT1=ZERO        
C
      SIGYT2=ZERO      
      CBT2=ZERO        
      CNT2=ZERO        
      SIGMXT2=ZERO     
      CCT2=ZERO        
C
      SIGYC1=ZERO      
      CBC1=ZERO        
      CNC1=ZERO        
      SIGMXC1=ZERO     
      CCC1=ZERO        
C
      SIGYC2=ZERO      
      CBC2=ZERO        
      CNC2=ZERO        
      SIGMXC2=ZERO     
      CCC2=ZERO        
C
      SIGYC12=ZERO     
      CBC12=ZERO       
      CNC12=ZERO       
      SIGMXC12=ZERO    
      CCC12=ZERO       
C
      EPS1T1=ZERO      
      EPS2T1=ZERO      
      SIGRST1=ZERO     
      WPLAMXT1=ZERO    
C
      EPS1T2=ZERO      
      EPS2T2=ZERO      
      SIGRST2=ZERO     
      WPLAMXT2=ZERO    
C
      EPS1C1=ZERO      
      EPS2C1=ZERO      
      SIGRSC1=ZERO     
      WPLAMXC1=ZERO    
C
      EPS1C2=ZERO      
      EPS2C2=ZERO      
      SIGRSC2=ZERO     
      WPLAMXC2=ZERO    
C
      EPS1T12=ZERO     
      EPS2T12=ZERO     
      SIGRST12=ZERO    
      WPLAMXT12=ZERO
c
      ISRATE = 0
      FCUT   = ZERO
      RATIO  = ZERO
      IMODWP = 0
c--------------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
c--------------------------------------------------------
c     Common input fields
c--------------------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     elasticity and failure limits
c
c
      CALL HM_GET_FLOATV('MAT_EA'    ,E11      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EB'    ,E22      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EC'    ,E33      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_PRAB'  ,N12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAB'   ,G12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'   ,G23      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA'   ,G31      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPSF1' ,EPSF1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSF2' ,EPSF2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPST1' ,EPST1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSM1' ,EPSM1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPST2' ,EPST2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPSM2' ,EPSM2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DAMAGE',DMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     composite plasticity hardening
c
      CALL HM_GET_FLOATV('WPMAX'     ,WPLAMX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Itype'     ,IOFF     ,IS_AVAILABLE, LSUBMODEL)      
      CALL HM_GET_FLOATV('MAT_R00'   ,RATIO    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c     
c     formulation flag    
c     
      CALL HM_GET_INTV  ('MAT_Iflag' ,IFLAG    ,IS_AVAILABLE,LSUBMODEL)      
c
c--------------------------------------------------------
      SELECT CASE (IFLAG)
c--------------------------------------------------------
        CASE (0)   ! Tsai-Wu formulation
c
c-----    composite plasticity hardening
c
          CALL HM_GET_FLOATV('WPREF'        ,WPLAREF  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_BETA'     ,CB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_HARD'     ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIG'      ,FMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-----    composite plasticity in tension / compression
c
          CALL HM_GET_FLOATV('MAT_SIGYT1'   ,SIGYT1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGYT2'   ,SIGYT2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGYC1'   ,SIGYC1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGYC2'   ,SIGYC2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_ALPHA '   ,ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-----    yield stress in shear and strain rate
c
          CALL HM_GET_FLOATV('MAT_SIGC12'   ,SIGYC12  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGT12'   ,SIGYT12  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SRC'      ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SRP'      ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_INTV  ('STRFLAG'      ,ICC      ,IS_AVAILABLE,LSUBMODEL)      
c
c-----
        CASE (1)   ! Crasurv formulation
c-----
          CALL HM_GET_INTV  ('WP_FAIL'  ,IMODWP    ,IS_AVAILABLE,LSUBMODEL)      
c
c-----    global composite plasticity parameters
c
          CALL HM_GET_FLOATV('MAT_SRC'      ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SRP'      ,EPS0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_ALPHA'    ,ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_INTV  ('STRFLAG'      ,ICC      ,IS_AVAILABLE,LSUBMODEL)      
c
c-----    composite plasticity in tension, directions 1 and 2
c
          CALL HM_GET_FLOATV('MAT_SIGYT1'   ,SIGYT1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_b1_t'     ,CBT1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_n1_t'     ,CNT1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIG1max_t',SIGMXT1  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_c1_t'     ,CCT1     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
          CALL HM_GET_FLOATV('EPS_1t1'      ,EPS1T1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('EPS_2t1'      ,EPS2T1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGres_t1',SIGRST1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_Wmax_pt1' ,WPLAMXT1  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
          CALL HM_GET_FLOATV('MAT_SIGYT2'   ,SIGYT2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_b2_t'     ,CBT2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_n2_t'     ,CNT2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIG2max_t',SIGMXT2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_c2_t'     ,CCT2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
          CALL HM_GET_FLOATV('MAT_EPS1_t2'  ,EPS1T2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_EPS2_t2'  ,EPS2T2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGres_t2',SIGRST2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_Wmax_pt2' ,WPLAMXT2  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-----    composite plasticity in compression, directions 1 and 2
c
          CALL HM_GET_FLOATV('MAT_SIG1_yc'  ,SIGYC1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_b1_c'     ,CBC1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_n1_c'     ,CNC1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIG1max_c',SIGMXC1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_c1_c'     ,CCC1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
          CALL HM_GET_FLOATV('MAT_EPS1_c1'  ,EPS1C1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_EPS2_c1'  ,EPS2C1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGres_c1',SIGRSC1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_Wmax_pc1' ,WPLAMXC1  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c   
          CALL HM_GET_FLOATV('MAT_SIG2_yc'  ,SIGYC2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_b2_c'     ,CBC2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_n2_c'     ,CNC2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIG2max_c',SIGMXC2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_c2_c'     ,CCC2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
          CALL HM_GET_FLOATV('MAT_EPS1_c2'  ,EPS1C2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_EPS2_c2'  ,EPS2C2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGres_c2',SIGRSC2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_Wmax_pc2' ,WPLAMXC2  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-----    composite plasticity in shear
c
          CALL HM_GET_FLOATV('MAT_SIG12_yt'  ,SIGYT12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_b12_t'     ,CBT12     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_n12_t'     ,CNT12     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIG12max_t',SIGMXT12  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_c12_t'     ,CCT12     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
          SIGYC12 = SIGYT12
c
          CALL HM_GET_FLOATV('MAT_EPS1_t12'  ,EPS1T12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_EPS2_t12'  ,EPS2T12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_SIGres_t12',SIGRST12  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('MAT_Wmax_pt12' ,WPLAMXT12 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c--------------------------------------------------------
      END SELECT
c--------------------------------------------------------
c     Common input fields
c--------------------------------------------------------
c     Delamination
c
      CALL HM_GET_FLOATV('MAT_GAMAi' ,SHRDAM   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMAm' ,SHRMAX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DAMm'  ,SHRDMAX  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     Strain rate filtering
c
      CALL HM_GET_INTV  ('Fsmooth'   ,ISRATE    ,IS_AVAILABLE, LSUBMODEL)      
      CALL HM_GET_FLOATV('Fcut'      ,FCUT      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c--------------------------------------------------------
      IF (IFLAG == 1) THEN
        IF((CC  > ZERO  .OR.  CCT1 > ZERO .OR. CCT2  > ZERO  .OR.
     .      CCC1 > ZERO .OR.  CCC2 > ZERO .OR. CCT12 > ZERO) .AND. 
     .      EPS0 > ZERO .AND. FCUT == ZERO ) THEN
     
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID, 
     .                C1=TITR)
        ENDIF 
      ELSE
        IF (CC > ZERO .AND. EPS0 > ZERO  .AND. FCUT == ZERO) THEN
           CALL ANCMSG(MSGID=1220,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID, 
     .                C1=TITR)
        ENDIF           
      
      ENDIF
c
      IF (E33 <= ZERO) E33 = MAX(E11, E22)
c      
      IF (E11 == ZERO .OR. E22 == ZERO .OR.
     .    G12 == ZERO .OR. G23 == ZERO .OR. G31 == ZERO) THEN
         CALL ANCMSG(MSGID=306,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=25,
     .               I2=ID,
     .               C1=TITR,
     .               C2='E11, E22, G12, G23, G31')
      ENDIF
c--------------------------------
      IF (ISRATE == ZERO) ISRATE = 1    ! for backward compatibility 
      IF (FCUT == ZERO)   FCUT   = EP20
      IF (CN == ZERO)     CN     = ONE 
      IF (DMAX  == ZERO)  DMAX   = ZEP999
      IF (FMAX  == ZERO)  FMAX   = EP20
      IF (EPST1 == ZERO)  EPST1  = EP20
      IF (EPST2 == ZERO)  EPST2  = EP20
      IF (EPSM1 == ZERO)  EPSM1  = ONEP1*EP20
      IF (EPSM2 == ZERO)  EPSM2  = ONEP1*EP20
      IF (EPSF1 == ZERO)  EPSF1  = ONEP2*EP20
      IF (EPSF2 == ZERO)  EPSF2  = ONEP2*EP20
      FMAX   = MIN(FMAX  ,EP20)
      EPST1  = MIN(EPST1 ,EP20)
      EPST2  = MIN(EPST2 ,EP20)      
      EPSM1  = MIN(EPSM1 ,ONEP1*EP20)
      EPSM2  = MIN(EPSM2 ,ONEP1*EP20)
      EPSF1  = MIN(EPSF1 ,ONEP2*EP20)
      EPSF2  = MIN(EPSF2 ,ONEP2*EP20)
c
      IF (WPLAMX == ZERO) WPLAMX  = EP20
      IF (SHRDAM == ZERO) SHRDAM  = EP20
      IF (SHRMAX == ZERO) SHRMAX  = ONEP1*EP20
      IF (SHRDMAX == ZERO)SHRDMAX = ONE
      IF (ALPHA == ZERO)  ALPHA   = ONE
      IF (EPS0 ==  ZERO)  EPS0    = ONE 
      IF (ICC == ZERO)    ICC     = 1
c--------------------------------
      IF (((EPST1 >= EPSM1) .AND.((EPST1 /= ZERO) .AND.
     .     (EPSM1 /= ZERO))).OR. ((EPST2 >= EPSM2).AND.(EPST2 /= ZERO).AND.
     .     (EPSM2 /= ZERO))) THEN
        CALL ANCMSG(MSGID=562, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .              I1=ID,
     .              C1=TITR)
      ENDIF
c      
      IF ((EPSF1 <= EPST1 .AND. EPST1 /= ZERO) .OR.
     .    (EPSF2 <= EPST2 .AND. EPST2 /= ZERO)) THEN
        CALL ANCMSG(MSGID=616, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .              I1=ID,
     .              C1=TITR)
      END IF
c
c--------------------------------------------------------
      IF (IFLAG == 1) THEN  ! crasurv
        CALL HM_GET_FLOATV_DIM('WPMAX',FSCAL_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        WPLAREF = ONE *FSCAL_UNIT
c
        IF (CNT1 == ZERO)    CNT1  = ONE
        IF (CNT2 == ZERO)    CNT2  = CNT1
        IF (CNC1 == ZERO)    CNC1  = CNT2
        IF (CNC2 == ZERO)    CNC2  = CNC1
        IF (CNT12 == ZERO)   CNT12 = CNC2
        IF (CBT1 == ZERO)    CBT1  = CBT1
        IF (CBT2 == ZERO)    CBT2  = CBT1
        IF (CBC1 == ZERO)    CBC1  = CBT2
        IF (CBC2 == ZERO)    CBC2  = CBC1
        IF (CBT12 == ZERO)   CBT12 = CBC2
        IF (SIGMXT1 == ZERO) SIGMXT1  = EP20
        IF (SIGMXT2 == ZERO) SIGMXT2  = EP20
        IF (SIGMXC1 == ZERO) SIGMXC1  = EP20
        IF (SIGMXC2 == ZERO) SIGMXC2  = EP20
        IF (SIGMXT12 == ZERO)SIGMXT12 = EP20
C
        IF (EPS1T1 == ZERO)  EPS1T1  = EP20
        IF (EPS2T1 == ZERO)  EPS2T1  = SIX_OVER_5*EPS1T1
        IF (EPS1T2 == ZERO)  EPS1T2  = EP20
        IF (EPS2T2 == ZERO)  EPS2T2  = SIX_OVER_5*EPS1T2
        IF (EPS1C1 == ZERO)  EPS1C1  = EP20
        IF (EPS2C1 == ZERO)  EPS2C1  = SIX_OVER_5*EPS1C1
        IF (EPS1C2 == ZERO)  EPS1C2  = EP20
        IF (EPS2C2 == ZERO)  EPS2C2  = SIX_OVER_5*EPS1C2
        IF (EPS1T12 == ZERO) EPS1T12 = EP20
        IF (EPS2T12 == ZERO) EPS2T12 = SIX_OVER_5*EPS1T12     
C
        IF (SIGRST1 == ZERO) SIGRST1  = EM3*SIGYT1
        IF (SIGRST2 == ZERO) SIGRST2  = EM3*SIGYT2
        IF (SIGRSC1 == ZERO) SIGRSC1  = EM3*SIGYC1
        IF (SIGRSC2 == ZERO) SIGRSC2  = EM3*SIGYC2
        IF (SIGRST12 == ZERO)SIGRST12 = EM3*SIGYT12
C
        IF (WPLAMXT1 == ZERO) WPLAMXT1  = EP20
        IF (WPLAMXT2 == ZERO) WPLAMXT2  = EP20
        IF (WPLAMXC1 == ZERO) WPLAMXC1  = EP20
        IF (WPLAMXC2 == ZERO) WPLAMXC2  = EP20
        IF (WPLAMXT12 == ZERO)WPLAMXT12 = EP20
C
        WPLAMXT1  = WPLAMXT1  / WPLAREF
        WPLAMXT2  = WPLAMXT2  / WPLAREF
        WPLAMXC1  = WPLAMXC1  / WPLAREF
        WPLAMXC2  = WPLAMXC2  / WPLAREF
        WPLAMXT12 = WPLAMXT12 / WPLAREF
      ELSE  ! Tsai-Wu
        CALL HM_GET_FLOATV_DIM('WPREF',FSCAL_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (WPLAREF == ZERO) WPLAREF = ONE *FSCAL_UNIT
      ENDIF
      WPLAMX = WPLAMX / WPLAREF
c
      IF (RATIO == ZERO .OR. RATIO > ONE ) RATIO = ONE
      IF (RATIO < ZERO) RATIO = -ONE
c--------------------------------
      N21  = N12*E22/E11
      DETC = ONE-N12*N21
      IF (DETC <= ZERO) THEN
         CALL ANCMSG(MSGID=307,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
      ENDIF
C
      IF (CN > ONE) THEN
         CALL ANCMSG(MSGID=213,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=25,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
C-------------------------------------
      WRITE(IOUT,1050) TRIM(TITR),ID,25
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1060) RHO0
        WRITE(IOUT,1100) E11,E22,N12,E33
        WRITE(IOUT,1300) G12,G23,G31
        WRITE(IOUT,1500) EPST1,EPSM1,EPSF1,EPST2,EPSM2,EPSF2,DMAX
C----
        IF (IFLAG == 1) THEN
c---      CRASURV
          WRITE(IOUT,1804) WPLAMX
          WRITE(IOUT,1807) IOFF,IMODWP,RATIO
          WRITE(IOUT,1810) CC,EPS0,ALPHA,ICC
          WRITE(IOUT,1850)'TENSION DIR-1       ',
     .                  SIGYT1,CBT1,CNT1,SIGMXT1,CCT1,
     .                  EPS1T1,EPS2T1,SIGRST1,WPLAMXT1
          WRITE(IOUT,1850)'TENSION DIR-2       ',
     .                  SIGYT2,CBT2,CNT2,SIGMXT2,CCT2,
     .                  EPS1T2,EPS2T2,SIGRST2,WPLAMXT2
          WRITE(IOUT,1850)'COMPRESSION DIR-1   ',
     .                  SIGYC1,CBC1,CNC1,SIGMXC1,CCC1,
     .                  EPS1C1,EPS2C1,SIGRSC1,WPLAMXC1
          WRITE(IOUT,1850)'COMPRESSION DIR-2   ',
     .                  SIGYC2,CBC2,CNC2,SIGMXC2,CCC2,
     .                  EPS1C2,EPS2C2,SIGRSC2,WPLAMXC2
          WRITE(IOUT,1850)'SHEAR DIR-12        ',
     .                  SIGYT12,CBT12,CNT12,SIGMXT12,CCT12,
     .                  EPS1T12,EPS2T12,SIGRST12,WPLAMXT12
        ELSE   
c---      Tsai-Wu
          WRITE(IOUT,1805) WPLAMX,WPLAREF
          WRITE(IOUT,1807) IOFF,IMODWP,RATIO
          WRITE(IOUT,1510) CB,CN,FMAX
          WRITE(IOUT,1550) SIGYT1,SIGYT2,SIGYC1,SIGYC2,ALPHA
          WRITE(IOUT,1560) SIGYT12,SIGYC12
          WRITE(IOUT,1610) CC,EPS0,ICC
        ENDIF
c
        WRITE(IOUT,1600) SHRDAM,SHRMAX,SHRDMAX
        WRITE(IOUT,1611) ISRATE,FCUT
      ENDIF
C-------------------------------------------------
      PM(1)  = RHOR
      PM(89) = RHO0
c-----------------
      PM(9)  = FCUT*TWO*PI
      PM(20) = MAX(E11,E22)/DETC
      PM(24) = PM(20)
      PM(26) = FIVE_OVER_6
      PM(33) = E11
      PM(34) = E22
      PM(35) = N12
      PM(36) = N21
      PM(37) = G12
      PM(38) = G23
      PM(39) = G31
      PM(40) = IFLAG
      PM(41) = WPLAMX
      PM(42) = IOFF
      DE     = ONE
      PM(44) = DE
      PM(46) = CB
      PM(47) = CN
      PM(49) = FMAX
      PM(21) = SQRT(N12*N21)
      PM(22) = MAX(G12,G23,G31)
C-----------------------------------------
      C1  = MAX(E11,E22)/DETC
      SSP = SQRT(MAX(C1,G12,G23,G31)/ RHO0)
      PM(32)= C1
      PM(27)= SSP
C-----------------------------------------
      PARMAT(16) = 1   ! IFORMDT  (solid element timestep computation flag)
C----------------------------------------
C     CONSTANTS FOR THE YIELD FUNCTION
C----------------------------------------
      IF (SIGYT1 <= ZERO) THEN
         CALL ANCMSG(MSGID=198,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2='SIGYT1')
      ENDIF
      IF (SIGYC1 <= ZERO) THEN
         CALL ANCMSG(MSGID=198,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2='SIGYC1')
      ENDIF
      IF (SIGYT2 <= ZERO) THEN
         CALL ANCMSG(MSGID=198,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2='SIGYT2')
      ENDIF
      IF (SIGYC2 <= ZERO) THEN
         CALL ANCMSG(MSGID=198,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2='SIGYC2')
      ENDIF
      IF (SIGYT12 <= ZERO) THEN
         CALL ANCMSG(MSGID=198,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2='SIGYT12')
      ENDIF
      IF (SIGYC12 <= ZERO) THEN
         CALL ANCMSG(MSGID=198,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR,
     .               C2='SIGYC12')
      ENDIF      
C
      F1  = ONE / SIGYT1 - ONE / SIGYC1
      F2  = ONE / SIGYT2 - ONE / SIGYC2
      F11 = ONE / MIN(EP20,(SIGYT1*SIGYC1))
      F22 = ONE / MIN(EP20,(SIGYT2*SIGYC2))
      F33 = ONE / MIN(EP20,(SIGYT12*SIGYC12))
      F12 = -ALPHA/(TWO*SQRT(MIN(EP20,SIGYT1*SIGYC1*SIGYT2*SIGYC2)))    
      FT1 = F11*F22 - FOUR*F12**2
      WRITE(IOUT,1650) F1,F2,F11,F22,F33,F12
c------------------------------------------------------------
      PM(54)=F1
      PM(55)=F2
      PM(56)=F11
      PM(57)=F22
      PM(58)=F33
      PM(59)=F12
C
      PM(60)=EPST1
      PM(61)=EPST2
      PM(62)=EPSM1
      PM(63)=EPSM2
      PM(64)=DMAX
C
      PM(65)=SHRDAM
      PM(66)=SHRMAX
      PM(67)=SHRDMAX
C
      PM(50)=CC
      PM(51)=EPS0
      PM(53)=ICC
C
      PM(68)=WPLAREF
C
      PM(98)=EPSF1      
      PM(99)=EPSF2      
C
      IF(EPS0 == ZERO) THEN
         CALL ANCMSG(MSGID=298,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=25,
     .               I2=ID,
     .               C1=TITR)
      ENDIF
C-----------------------------------------
      IF (IFLAG == 1) THEN
        PM(59)=ALPHA
        PM(141)=SIGYT1
        PM(142)=CBT1
        PM(143)=CNT1
        PM(144)=SIGMXT1
        PM(145)=CCT1
        PM(146)=SIGYT2
        PM(147)=CBT2
        PM(148)=CNT2
        PM(149)=SIGMXT2
        PM(150)=CCT2
        PM(151)=SIGYC1
        PM(152)=CBC1
        PM(153)=CNC1
        PM(154)=SIGMXC1
        PM(155)=CCC1
        PM(156)=SIGYC2
        PM(157)=CBC2
        PM(158)=CNC2
        PM(159)=SIGMXC2
        PM(160)=CCC2
        PM(161)=SIGYT12
        PM(162)=CBT12
        PM(163)=CNT12
        PM(164)=SIGMXT12
        PM(165)=CCT12
        PM(166)=EPS1T1
        PM(167)=EPS2T1
        PM(168)=SIGRST1
        PM(169)=WPLAMXT1
        PM(170)=EPS1T2
        PM(171)=EPS2T2
        PM(172)=SIGRST2
        PM(173)=WPLAMXT2
        PM(174)=EPS1C1
        PM(175)=EPS2C1
        PM(176)=SIGRSC1
        PM(177)=WPLAMXC1
        PM(178)=EPS1C2
        PM(179)=EPS2C2
        PM(180)=SIGRSC2
        PM(181)=WPLAMXC2
        PM(182)=EPS1T12
        PM(183)=EPS2T12
        PM(184)=SIGRST12
        PM(185)=WPLAMXT12
      ENDIF
      PM(186) = E33
      IYEILD  = 0           
      PM(187) = IYEILD
      PM(188) = RATIO
      PM(189) = IMODWP
CC    Formulation for solid elements time step computation.
      FAC = ONE/(ONE - N12*N21)
      D11  = E11*FAC
      D22  = E22*FAC
      D12 = N21*D11
      DMIN  = D11*D22  - D12**2
      DMAX  = MAX(D11, D22)   
      PM(105) = DMIN/DMAX/DMAX
c
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_IERR  = 1  ! tag element for projection error message
      MTAG%G_GAMA  = 6  
      MTAG%G_PLA   = 1    
      MTAG%G_EPSD  = 1    
      MTAG%G_DAMDL = 1    
c
      MTAG%L_GAMA  = 6  
      MTAG%L_PLA   = 1
      MTAG%L_EPSD  = 1
      MTAG%L_DAM   = 2
      MTAG%L_CRAK  = 2
      MTAG%L_STRA  = 6
c-----------
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
c-----------
      RETURN
c-----------
 1000 FORMAT(
     & 5X,A,/,
     & 5X,40H  ORTHOTROPIC MATERIAL FOR SHELLS       ,/,
     & 5X,40H  -------------------------------       ,//)
 1050 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . .=',I10/)
 1060 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . .=',1PG20.13/)  
 1100 FORMAT(
     & 5X,40HYOUNG'S MODULUS E11 . . . . . . . . . .=,E12.4/,
     & 5X,40HYOUNG'S MODULUS E22 . . . . . . . . . .=,E12.4/,
     & 5X,40HPOISSON'S RATIO N12 . . . . . . . . . .=,E12.4/,
     & 5X,40HYOUNG'S MODULUS E33 . . . . . . . . . .=,E12.4/)
 1300 FORMAT(
     & 5X,40HSHEAR MODULUS   G12 . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS   G23 . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS   G31 . . . . . . . . . .=,E12.4/)
 1500 FORMAT(
     & 5X,'TENSILE FAILURE STRAIN DIR-1. . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM TENSILE STRAIN DIR-1. . . . . .=',1PG20.13/,
     & 5X,'TOTAL FAILURE TENSILE STRAIN DIR-1. . .=',1PG20.13/,
     & 5X,'TENSILE FAILURE STRAIN DIR-2. . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM TENSILE STRAIN DIR-2. . . . . .=',1PG20.13/,
     & 5X,'TOTAL FAILURE TENSILE STRAIN DIR-2. . .=',1PG20.13/,
     & 5X,'MAXIMUM DAMAGE DIR. . . . . . . . . . .=',1PG20.13/)
 1510 FORMAT(
     & 5X,'COMPOSITE HARDENING PARAMETER  (B). . .=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (N). . .=',1PG20.13/,
     & 5X,'COMPOSITE MAXIMUM YIELD FUNCTION FMAX .=',1PG20.13//)
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
     & 5X,'SHEAR DELAMINATION                      '/,
     & 5X,'INITIATION STRAIN . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM STRAIN. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM DAMAGE. . . . . . . . . . . . .=',1PG20.13/)
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
 1700 FORMAT(1X,'N SHOULD BE  <=  1.')
 2000 FORMAT(//' MODULI CANNOT BE ZERO')
 2100 FORMAT(1X,'EPS0 SHOULD BE > 0. IF CC /= 0.')
 2200 FORMAT(//' DETERMINANT <= 0 -> CHECK CONSTITUTIVE LAW')
 1804 FORMAT(
     & 5X,'COMPOSITE MAXIMUM PLASTIC WORK WPLAMX .=',1PG20.13)
 1805 FORMAT(
     & 5X,'COMPOSITE MAXIMUM PLASTIC WORK WPLAMX .=',1PG20.13/,
     & 5X,'REFERENCE PLASTIC WORK WPLAREF        .=',1PG20.13)
 1807 FORMAT(
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
     & 5X,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1 OR 2'//,
     & 5X,' DIRECTIONAL WPMAX FAILURE FORMULATION    =', I10 //,     
     & 5X,'LAYER FAILURE RATIO FOR ELEMENT DELETION. =',E12.4 )
     )
 1810 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT CC FOR WPLAMX .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN RATE . . . . . . . . .=',1PG20.13/,
     & 5X,'F12 REDUCTION FACTOR . . . . . . . .  .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE ON SIG-MAX Wp-MAX =',I10//)
 1850 FORMAT(
     & 5X,'COMPOSITE YIELD IN ',A,'=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (B). . .=',1PG20.13/,
     & 5X,'COMPOSITE HARDENING PARAMETER  (N). . .=',1PG20.13/,
     & 5X,'COMPOSITE MAXIMUM STRESS. . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1PG20.13/,
     & 5X,'INITIAL SOFTENING PLASTIC STRAIN. . . .=',1PG20.13/,
     & 5X,'MAXIMUM SOFTENING PLASTIC STRAIN. . . .=',1PG20.13/,
     & 5X,'RESIDUAL STRESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM PLASTIC WORK FOR FAILURE. . . .=',1PG20.13//)
C-----------------
      END
