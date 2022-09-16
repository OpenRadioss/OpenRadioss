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
Chd|  HM_READ_MAT115                source/materials/mat/mat115/hm_read_mat115.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT115(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NTABL    ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM       ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,ITABLE   ,MAXTABL  ,
     .           NVARTMP  ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD 
      USE ELBUFTAG_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e sXM
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE,ITABLE(MAXTABL)
      INTEGER, INTENT(OUT)                         :: NUPARAM,NUVAR,NTABL,NVARTMP
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT)          :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT)                 :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(OUT)          :: MATPARAM  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, ILAW, Ivflag, NANGLE, INFO, Icrit, TAB_YLD, Ismooth,
     .        TAB_TEMP,ITER,Ires,Istat
C     REAL
      my_real
     .   RHO0,YOUNG,NU,ALPHA,GAMMA,EPSD,ALPHA2,BETA,SIGP,CFAIL,PFAIL,
     .   G,G2,LAM,BULK,NNU,NNU1,RHOF0,SIGP_C0,SIGP_C1,SIGP_N,ALPHA2_C0,
     .   ALPHA2_C1,ALPHA2_N,GAMMA_C0,GAMMA_C1,GAMMA_N,BETA_C0,BETA_C1,BETA_N
C        
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 115
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 - Elasticity  
      CALL HM_GET_FLOATV('MAT_E'     ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IRES'  ,Ires     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_ISTAT' ,Istat    ,IS_AVAILABLE, LSUBMODEL)
c
      ! Constant parameter
      IF (Istat == 0) THEN
card3
        CALL HM_GET_FLOATV('MAT_ALPHA' ,ALPHA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_CFAIL' ,CFAIL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_PFAIL' ,PFAIL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card4      
        CALL HM_GET_FLOATV('MAT_SIGP'  ,SIGP     ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
        CALL HM_GET_FLOATV('MAT_GAMMA' ,GAMMA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_EPSD'  ,EPSD     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_ALPHA2',ALPHA2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_BETA'  ,BETA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      ! Statistical variation parameter
      ELSE
card3
        CALL HM_GET_FLOATV('MAT_ALPHA'     ,ALPHA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_CFAIL'     ,CFAIL     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_PFAIL'     ,PFAIL     ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        CALL HM_GET_FLOATV('MAT_RHOF0'     ,RHOF0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
        
        CALL HM_GET_FLOATV('MAT_SIGP_C0'   ,SIGP_C0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_SIGP_C1'   ,SIGP_C1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_SIGP_N'    ,SIGP_N    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        
        CALL HM_GET_FLOATV('MAT_ALPHA2_C0' ,ALPHA2_C0 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_ALPHA2_C1' ,ALPHA2_C1 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_ALPHA2_N'  ,ALPHA2_N  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        
        CALL HM_GET_FLOATV('MAT_GAMMA_C0'  ,GAMMA_C0  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_GAMMA_C1'  ,GAMMA_C1  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_GAMMA_N'   ,GAMMA_N   ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
       
        CALL HM_GET_FLOATV('MAT_BETA_C0'   ,BETA_C0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_BETA_C1'   ,BETA_C1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_BETA_N'    ,BETA_N    ,IS_AVAILABLE, LSUBMODEL, UNITAB)    
             
      ENDIF
c
c---------------------
c     Default values
c---------------------
      ! Density
      IF ((Ires == 0).OR.(Ires > 2)) Ires  = 2
      ! Poisson's ratio
      IF (NU < ZERO .OR. NU >= HALF) THEN
        CALL ANCMSG(MSGID=49,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      ! Elasticity parameter
      G2   = YOUNG / (ONE + NU)
      G    = HALF * G2
      LAM  = G2 * NU /(ONE - TWO*NU)  
      BULK = THIRD * YOUNG / (ONE - NU*TWO)
      NNU  = NU  / (ONE - NU)
      NNU1 = ONE - NNU
      ! Plastic data initialization
      IF (Istat == 0) THEN
        ! Initial yield stress
        IF (SIGP == ZERO) SIGP = INFINITY
        IF (SIGP < ZERO) THEN 
          CALL ANCMSG(MSGID=1901,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                R1=SIGP,
     .                I1=MAT_ID,
     .                C1=TITR)             
        ENDIF      
        ! Densification strain
        IF (EPSD == ZERO) EPSD = INFINITY
        IF (EPSD < ZERO) THEN 
          CALL ANCMSG(MSGID=1900,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                R1=EPSD,
     .                I1=MAT_ID,
     .                C1=TITR)             
        ENDIF 
        ! Beta parameter
        IF (BETA == ZERO) BETA = ONE
      ENDIF
      ! Yield function shape parameter
      IF ((ALPHA < ZERO).OR.(ALPHA > SQRT(4.5D0))) THEN
        CALL ANCMSG(MSGID=1897,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=ALPHA,
     .              I1=MAT_ID,
     .              C1=TITR)      
      ENDIF
      ! Tensile volumetric strain at failure
      IF (CFAIL < ZERO) THEN
        CALL ANCMSG(MSGID=1898,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=CFAIL,
     .              I1=MAT_ID,
     .              C1=TITR)          
      ENDIF
      ! Maximum principal strain at failure
      IF (PFAIL < ZERO) THEN 
        CALL ANCMSG(MSGID=1899,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=PFAIL,
     .              I1=MAT_ID,
     .              C1=TITR)    
      ENDIF
      ! Check for /PERTURB/PART/SOLID card
      IF (Istat == 1) THEN
        ! Warning if no cards are detected
        IF (NPERTURB == 0) THEN 
          CALL ANCMSG(MSGID=1916,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR)           
        ENDIF
      ENDIF
c
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameter
      IF (Istat == 0) THEN 
        NUPARAM = 20
      ELSE
        NUPARAM = 28
      ENDIF
      ! Number of function and temp variable
      NTABL   = 0
      NVARTMP = 0
      ! Number of user variable 
      IF (Ires == 1) THEN
        IF (CFAIL > ZERO) THEN 
          NUVAR = 2
        ELSE
          NUVAR = 1
        ENDIF
      ELSE
        IF (CFAIL > ZERO) THEN 
          NUVAR = 1
        ELSE
          NUVAR = 0
        ENDIF
      ENDIF
c      
      ! Material parameters
      UPARAM(1)  = YOUNG       ! Young modulus
      UPARAM(2)  = BULK        ! Bulk modulus
      UPARAM(3)  = G           ! Shear modulus
      UPARAM(4)  = G2          ! 2*Shear modulus
      UPARAM(5)  = LAM         ! Lambda (Hook)
      UPARAM(6)  = NU          ! Poisson ratio
      UPARAM(7)  = NNU      
      UPARAM(8)  = NNU1    
      UPARAM(11) = Ires        ! Choice of the return mapping algorithm
      UPARAM(12) = Istat       ! Choice of the statistical variation
      UPARAM(13) = ALPHA       ! Yield function shape parameter
      UPARAM(14) = CFAIL       ! Tensile volumic strain at failure
      UPARAM(15) = PFAIL       ! Principal stress at failure
      IF (Istat == 0) THEN 
        UPARAM(16) = GAMMA     ! Yield stress parameter
        UPARAM(17) = EPSD      ! Densification strain
        UPARAM(18) = ALPHA2    ! Yield stress parameter
        UPARAM(19) = BETA      ! Yield stress parameter
        UPARAM(20) = SIGP      ! Initial yield stress
      ELSE
        UPARAM(16) = RHOF0     ! Yield stress parameter
        UPARAM(17) = SIGP_C0   ! Densification strain
        UPARAM(18) = SIGP_C1   ! Yield stress parameter
        UPARAM(19) = SIGP_N    ! Yield stress parameter
        UPARAM(20) = ALPHA2_C0 ! Initial yield stress
        UPARAM(21) = ALPHA2_C1 ! Tensile volumic strain at failure
        UPARAM(22) = ALPHA2_N  ! Principal stress at failure  
        UPARAM(23) = GAMMA_C0  ! Initial yield stress
        UPARAM(24) = GAMMA_C1  ! Tensile volumic strain at failure
        UPARAM(25) = GAMMA_N   ! Principal stress at failure 
        UPARAM(26) = BETA_C0   ! Initial yield stress
        UPARAM(27) = BETA_C1   ! Tensile volumic strain at failure
        UPARAM(28) = BETA_N    ! Principal stress at failure 
      ENDIF
c      
      ! PARMAT table
      PARMAT(1) = BULK
      PARMAT(2) = YOUNG
      PARMAT(3) = NU
      PARMAT(4) = ZERO
      PARMAT(5) = ZERO
      ISRATE    = 0
c
      ! PM table
      PM(1)  = RHO0
      PM(89) = RHO0
      PM(27) = SQRT((BULK + FOUR_OVER_3*G)/RHO0)  ! sound speed estimation
      PM(100)= BULK   
c      
      ! MTAG variable activation
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%L_SEQ  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_RHO  = 2
      MTAG%G_RHO  = 2
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL" )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN")
c
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW 
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) YOUNG,NU
        WRITE(IOUT,1350) Ires
        WRITE(IOUT,1375) Istat
        IF (Istat == 0) THEN 
          WRITE(IOUT,1400) ALPHA,GAMMA,EPSD,ALPHA2,BETA,SIGP,CFAIL,PFAIL
        ELSE
          WRITE(IOUT,1450) ALPHA,RHOF0,SIGP_C0,SIGP_C1,SIGP_N,ALPHA2_C0,
     .                     ALPHA2_C1,ALPHA2_N,GAMMA_C0,GAMMA_C1,GAMMA_N,
     .                     BETA_C0,BETA_C1,BETA_N,CFAIL,PFAIL
        ENDIF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : DESHPANDE AND FLECK FOAM',/,
     & 5X,'-----------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/)
 1350 FORMAT(
     & 5X,'RETURN MAPPING ALGORITHM FLAG . . . . .=',I3/
     & 5X,'  IRES=1  NICE EXPLICIT'/
     & 5X,'  IRES=2  NEWTON-ITERATION IMPLICIT (CUTTING PLANE)'/)
 1375 FORMAT(
     & 5X,'STATISTICAL VARIATION FLAG  . . . . . .=',I3/)
 1400 FORMAT(
     & 5X,'YIELD SURFACE SHAPE PARAMETER ALPHA . .=',1PG20.13/
     & 5X,'LINEAR HARDENING MODULUS GAMMA  . . . .=',1PG20.13/
     & 5X,'DENSIFICATION STRAIN EPSD . . . . . . .=',1PG20.13/
     & 5X,'NON-LINEAR HARDENING MODULUS ALPHA2 . .=',1PG20.13/
     & 5X,'NON-LINEAR HARDENING EXPONENT BETA  . .=',1PG20.13/
     & 5X,'INITIAL YIELD STRESS SIGP . . . . . . .=',1PG20.13/
     & 5X,'TENSILE VOLUMETRIC STRAIN AT FAILURE  .=',1PG20.13/
     & 5X,'MAX. PRINCIPAL STRESS AT FAILURE  . . .=',1PG20.13/)
 1450 FORMAT(
     & 5X,'YIELD SURFACE SHAPE PARAMETER ALPHA . .=',1PG20.13/
     & 5X,'DENSITY OF BASE MATERIAL  . . . . . . .=',1PG20.13/
     & 5X,'INITIAL YIELD STRESS PARAM. SIGP_C0 . .=',1PG20.13/     
     & 5X,'INITIAL YIELD STRESS PARAM. SIGP_C1 . .=',1PG20.13/   
     & 5X,'INITIAL YIELD STRESS EXPO.  SIGP_N  . .=',1PG20.13/ 
     & 5X,'NON-LINEAR HARDENING PARAM. ALPHA2_C0 .=',1PG20.13/
     & 5X,'NON-LINEAR HARDENING PARAM. ALPHA2_C1 .=',1PG20.13/
     & 5X,'NON-LINEAR HARDENING EXPO.  ALPHA2_N  .=',1PG20.13/
     & 5X,'LINEAR HARDENING PARAM. GAMMA_C0  . . .=',1PG20.13/
     & 5X,'LINEAR HARDENING PARAM. GAMMA_C1  . . .=',1PG20.13/
     & 5X,'LINEAR HARDENING PARAM. GAMMA_N . . . .=',1PG20.13/     
     & 5X,'NON-LINEAR HARDENING EXPO. BETA_C0  . .=',1PG20.13/
     & 5X,'NON-LINEAR HARDENING EXPO. BETA_C1  . .=',1PG20.13/
     & 5X,'NON-LINEAR HARDENING EXPO. BETA_N   . .=',1PG20.13/
     & 5X,'TENSILE VOLUMETRIC STRAIN AT FAILURE  .=',1PG20.13/
     & 5X,'MAX. PRINCIPAL STRESS AT FAILURE  . . .=',1PG20.13/)
c-----------------------------------------------------------------------
      RETURN
      END