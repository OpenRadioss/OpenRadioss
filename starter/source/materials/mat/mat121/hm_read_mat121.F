Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_MAT121                source/materials/mat/mat121/hm_read_mat121.F
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
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT121(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MAXFUNC  ,
     .           NFUNC    ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM       ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           MATPARAM )
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
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE,IFUNC(MAXFUNC)
      INTEGER, INTENT(INOUT)                         :: NUPARAM,NUVAR,NFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT) :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT)          :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ILAW,Ires,Ivisc,Ifail,Fct_SIG0,
     .        Fct_YOUN,Fct_TANG,Fct_FAIL
C     REAL
      my_real
     .   RHO0,YOUNG,NU,A,G,G2,LAM,BULK,NNU,NNU1,Fcut,DTMIN,
     .   Xscale_SIG0,Yscale_SIG0,Xscale_YOUN,Yscale_YOUN,
     .   Xscale_TANG,Xscale_FAIL,Yscale_FAIL,
     .   XSCALE_UNIT,YSCALE_UNIT,A11,A12,TANG
C        
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 121
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2 - Elasticity, flags, strain-rate filtering  
      CALL HM_GET_FLOATV('MAT_E'      ,YOUNG      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'     ,NU         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('MAT_Ires'   ,Ires       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Ivisc'  ,Ivisc      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'       ,Fcut       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TDEL'       ,DTMIN      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card3 - Function for Initial yield stress versus strain-rate
      CALL HM_GET_INTV  ('Fct_SIG0'   ,Fct_SIG0   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Xscale_SIG0',Xscale_SIG0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_SIG0 /= 0).AND.(Xscale_SIG0 == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Xscale_SIG0',XSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Xscale_SIG0 = ONE * XSCALE_UNIT
      ENDIF
      CALL HM_GET_FLOATV('Yscale_SIG0',Yscale_SIG0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_SIG0 /= 0).AND.(Yscale_SIG0 == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Yscale_SIG0',YSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Yscale_SIG0 = ONE * YSCALE_UNIT
      ENDIF
card4 - Function for Young modulus versus strain-rate
      CALL HM_GET_INTV  ('Fct_YOUN'   ,Fct_YOUN   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Xscale_YOUN',Xscale_YOUN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_YOUN /= 0).AND.(Xscale_YOUN == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Xscale_YOUN',XSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Xscale_YOUN = ONE * XSCALE_UNIT
      ENDIF
      CALL HM_GET_FLOATV('Yscale_YOUN',Yscale_YOUN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_YOUN /= 0).AND.(Yscale_YOUN == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Yscale_YOUN',YSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Yscale_YOUN = ONE * YSCALE_UNIT
      ENDIF
card5 - Function for Initial yield stress versus strain-rate
      CALL HM_GET_INTV  ('Fct_TANG'   ,Fct_TANG   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Xscale_TANG',Xscale_TANG,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_TANG /= 0).AND.(Xscale_TANG == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Xscale_TANG',XSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Xscale_TANG = ONE * XSCALE_UNIT
      ENDIF
      CALL HM_GET_FLOATV('MAT_TANG'   ,TANG       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_TANG /= 0).AND.(TANG == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('MAT_TANG',YSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        TANG = ONE * YSCALE_UNIT
      ENDIF
card6 - Function for Initial yield stress versus strain-rate
      CALL HM_GET_INTV  ('Fct_FAIL'   ,Fct_FAIL   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Ifail'  ,Ifail      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Xscale_FAIL',Xscale_FAIL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_FAIL /= 0).AND.(Xscale_FAIL == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Xscale_FAIL',XSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Xscale_FAIL = ONE * XSCALE_UNIT
      ENDIF
      CALL HM_GET_FLOATV('Yscale_FAIL',Yscale_FAIL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF ((Fct_FAIL /= 0).AND.(Yscale_FAIL == ZERO)) THEN
        CALL HM_GET_FLOATV_DIM('Yscale_FAIL',YSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
        Yscale_FAIL = ONE * YSCALE_UNIT
      ENDIF
c
c-----------------------------
c     Default values and check
c-----------------------------
      ! Elasticity parameter
      G2   = YOUNG / (ONE + NU)
      G    = HALF * G2
      LAM  = G2 * NU /(ONE - TWO*NU)  
      BULK = THIRD * YOUNG / (ONE - NU*TWO)
      NNU  = NU  / (ONE - NU)
      NNU1 = ONE - NNU
      A11  = YOUNG / (ONE - NU*NU)
      A12  = A11 * NU
      ! Return mapping algorithm
      IF ((Ires == 0).OR.(Ires > 2)) Ires = 2
      ! Strain rate dependency formulation flag
      Ivisc = MIN(MAX(Ivisc,0),1)
      ! Failure criterion flag
      Ifail = MIN(MAX(Ifail,0),3)
      ! Check initial yield stress function is defined 
      IF (Fct_SIG0 == 0) THEN 
        CALL ANCMSG(MSGID=2060,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=MAT_ID,
     .              C1=TITR)  
      ENDIF
      ! Check young
      IF ((Ivisc == 1).AND.(Fct_YOUN > 0)) THEN
        CALL ANCMSG(MSGID=2061,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=MAT_ID,
     .              C1=TITR,
     .              I2=Fct_YOUN)        
        Ivisc = 0
      ENDIF
      ! Poisson's ratio
      IF (NU < ZERO .OR. NU >= HALF) THEN
        CALL ANCMSG(MSGID=49,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      ! Strain-rate filtering frequency
      IF (Fcut == ZERO) THEN 
        Fcut = 10000.0D0*UNITAB%FAC_T_WORK  
      ENDIF
      ! Timestep failure criterion
      IF (DTMIN == ZERO) DTMIN = -INFINITY
c
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameters
      NUPARAM = 23
      ! Number of functions
      NFUNC   = 4
      ! Number of user variables 
      IF (Ires == 1) THEN 
        IF (Ivisc == 0) THEN 
          NUVAR = 1
        ELSE
          NUVAR = 3
        ENDIF
      ELSE
        NUVAR = 0
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
      UPARAM(9)  = A11
      UPARAM(10) = A12
      UPARAM(11) = Ires
      UPARAM(12) = Ivisc
      UPARAM(13) = Ifail
      UPARAM(14) = Fcut
      UPARAM(15) = DTMIN
      UPARAM(16) = Xscale_SIG0
      UPARAM(17) = Yscale_SIG0
      UPARAM(18) = Xscale_YOUN
      UPARAM(19) = Yscale_YOUN
      UPARAM(20) = Xscale_TANG
      UPARAM(21) = TANG
      UPARAM(22) = Xscale_FAIL
      UPARAM(23) = Yscale_FAIL
c
      ! Function IDs
      IFUNC(1) = Fct_SIG0
      IFUNC(2) = Fct_YOUN
      IFUNC(3) = Fct_TANG
      IFUNC(4) = Fct_FAIL
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
      MTAG%L_EPSD = 1
      MTAG%G_EPSD = 1
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL" )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"HOOK")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")
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
        WRITE(IOUT,1360) Ivisc
        WRITE(IOUT,1370) Ifail
        IF (Ivisc == 0) THEN 
          WRITE(IOUT,1380) Fcut
        ENDIF
        WRITE(IOUT,1400) Fct_SIG0,Xscale_SIG0,Yscale_SIG0
        IF (Fct_YOUN > 0) THEN 
          WRITE(IOUT,1500) Fct_YOUN,Xscale_YOUN,Yscale_YOUN
        ENDIF
        IF (Fct_TANG > 0) THEN 
          WRITE(IOUT,1600) Fct_TANG,Xscale_TANG,TANG
        ELSE
          WRITE(IOUT,1650) TANG
        ENDIF
        IF (Fct_FAIL > 0) THEN 
          WRITE(IOUT,1700) Fct_FAIL,Xscale_FAIL,Yscale_FAIL
        ENDIF
        IF (DTMIN > ZERO) THEN 
          WRITE(IOUT,1800) DTMIN
        ENDIF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : RATE DEPENDENT PLASTICITY',/,
     & 5X,'------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/)
 1350 FORMAT(
     & 5X,'RETURN MAPPING ALGORITHM FLAG . . . . .=',I3/
     & 5X,'  IRES=1  NICE EXPLICIT'/
     & 5X,'  IRES=2  NEWTON-ITERATION IMPLICIT (CUTTING PLANE)'/)
 1360 FORMAT(
     & 5X,'VISCOSITY FORMULATION FLAG. . . . . . .=',I3/
     & 5X,'  IVISC=0  SCALED YIELD STRESS FORMULATION'/
     & 5X,'  IVISC=1  FULL VISCOPLASTIC FORMULATION'/)
 1370 FORMAT(
     & 5X,'FAILURE CRITERION FLAG  . . . . . . . .=',I3/
     & 5X,'  IFAIL=0  MAXIMUM VON MISES STRESS'/
     & 5X,'  IFAIL=1  MAXIMUM PLASTIC STRAIN'/
     & 5X,'  IFAIL=2  MAX. PRINC STRESS + ABSOLUTE VALUE OF MIN. PRINC. STRESS'/
     & 5X,'  IFAIL=3  MAXIMUM PRINCIPAL STRESS'/)
 1380 FORMAT(
     & 5X,'STRAIN-RATE FILTERING CUTOFF FREQUENCY.=',1PG20.13/)
 1400 FORMAT(
     & 5X,'INITIAL YIELD STRESS FUNCTION ID. . . .=',I10/
     & 5X,'STRAIN-RATE SCALE FACTOR. . . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS SCALE FACTOR . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'YOUNG MODULUS FUNCTION ID . . . . . . .=',I10/
     & 5X,'STRAIN-RATE SCALE FACTOR. . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS SCALE FACTOR  . . . . . .=',1PG20.13/)
 1600 FORMAT(
     & 5X,'TANGENT MODULUS FUNCTION ID . . . . . .=',I10/
     & 5X,'STRAIN-RATE SCALE FACTOR. . . . . . . .=',1PG20.13/
     & 5X,'TANGENT MODULUS SCALE FACTOR  . . . . .=',1PG20.13/)
 1650 FORMAT(
     & 5X,'CONSTANT TANGENT MODULUS  . . . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'FAILURE CRITERION STRESS FUNCTION ID  .=',I10/
     & 5X,'STRAIN-RATE SCALE FACTOR. . . . . . . .=',1PG20.13/
     & 5X,'ORDINATE SCALE FACTOR . . . . . . . . .=',1PG20.13/)
 1800 FORMAT(
     & 5X,'MINIMUM TIMESTEP FOR ELEMENT DELETION .=',1PG20.13/)
c-----------------------------------------------------------------------
      END
