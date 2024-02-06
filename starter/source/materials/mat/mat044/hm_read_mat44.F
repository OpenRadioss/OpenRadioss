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
Chd|  HM_READ_MAT44                 source/materials/mat/mat044/hm_read_mat44.F
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
      SUBROUTINE HM_READ_MAT44(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM       ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,IFUNC    ,MAXFUNC  ,
     .           NVARTMP  ,MATPARAM )
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
C   READ MAT LAW44 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
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
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)               :: ISRATE,IFUNC(MAXFUNC)
      INTEGER, INTENT(INOUT)                 :: NUPARAM,NUVAR,NFUNC,NVARTMP
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: ILAW,ICC,ISRAT,Ivflag
      my_real :: RHO0,RHOR,E,NU,CA,CB,CN,G,A1,BULK,EPSM,SIGM,EPSR1,EPSR2,
     .           EPSP_REF,FISOKIN,CP,CC,ASRATE,EPSGM,YLD_SCALE,YLD_SCALE_UNIT
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW   = 44
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_E'     ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_SIGY'  ,CA       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'     ,CB       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N'     ,CN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HARD'  ,FISOKIN  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'   ,SIGM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_SRC'   ,EPSP_REF ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SRE'   ,CP       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('STRFLAG'   ,ICC      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Fsmooth'   ,ISRAT    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fcut'      ,ASRATE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Vflag'     ,Ivflag   ,IS_AVAILABLE, LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_EPS'   ,EPSM     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA1'  ,EPSR1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ETA2'  ,EPSR2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_INTV  ('YLD_FUNC'  ,IFUNC(1) ,IS_AVAILABLE, LSUBMODEL) 
      CALL HM_GET_FLOATV('YLD_SCALE' ,YLD_SCALE,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------
c     default values
c---------------------
      IF (RHOR == ZERO)  RHOR  = RHO0
c
      IF (NU < ZERO .OR. NU >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      IF (EPSP_REF == ZERO) THEN
        CC = ZERO
      ELSE
        CC = ONE / EPSP_REF
      END IF
      IF (CC > ZERO .AND. CP > ZERO .AND. 
     .   ISRAT == 0 .AND. ASRATE == ZERO .AND. Ivflag /= 1) THEN
           CALL ANCMSG(MSGID=1881,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID, 
     .                C1=TITR)
      ENDIF     
      IF (EPSR1 == ZERO) EPSR1 = INFINITY
      IF (EPSR2 == ZERO) EPSR2 = TWO*INFINITY
      IF (EPSM == ZERO)  EPSM  = INFINITY
      IF (SIGM == ZERO)  SIGM  = INFINITY   
      IF (ICC == 0)      ICC   = 1
      IF (CP == ZERO)    CP    = ONE
      CP = ONE / CP 
      IF (Ivflag == 0) THEN
        Ivflag = 2
      ENDIF
      !----------------------------------------------------------
      ! Filtering frequency
      IF (Ivflag == 1) THEN
        ! If plastic strain is chosen, filtering by default
        ISRAT     = 1
        ISRATE    = MAX(ISRATE,ISRAT) 
        ASRATE    = 10000.0D0*FAC_T_WORK
      ELSE
        IF (ASRATE /= ZERO) THEN
          ! If a filtering frequency is given by the user
          ISRAT     = 1
          ISRATE    = MAX(ISRATE,ISRAT)
        ELSE
          ! If no filtering frequency is given but the flag is activated
          IF (ISRAT /= 0) THEN
            ISRAT     = 1
            ISRATE    = MAX(ISRATE,ISRAT) 
            ASRATE    = 10000.0D0*FAC_T_WORK  
          ! If no filtering frequency and no flag is activated => no filtering
          ELSE
            ISRAT     = 0
            ISRATE    = 0
            ASRATE    = ZERO
          ENDIF
        ENDIF
      ENDIF
      !---------------------------------------------------------
      IF (CN /= ZERO .and. CB /= ZERO) THEN
        EPSGM = ((SIGM - CA) / CB)**(ONE/CN)
      ELSE
        EPSGM = INFINITY
      ENDIF
      IF (IFUNC(1) == 0) THEN 
        YLD_SCALE = ZERO
      ELSEIF (YLD_SCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('YLD_SCALE' ,YLD_SCALE_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        YLD_SCALE = ONE * YLD_SCALE_UNIT
      ENDIF
      ! If IFUNC > 0 and CA > 0 and Ivflag =/ 1
      IF (((IFUNC(1) > 0).AND.(CA /= ZERO)).AND.(Ivflag /= 1)) THEN
        CALL ANCMSG(MSGID=1880,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID, 
     .              C1=TITR)        
        CA = ZERO
        ! IFUNC > 0 and CA > 0 can only be set with Ivflag = 1
      ENDIF
c-----------------
      BULK = E / THREE / (ONE - TWO*NU)
      A1   = E / (ONE-NU*NU)
      G    = E / TWO  / (ONE + NU)
c-----------------
      NUPARAM = 24
      IF (IFUNC(1) > 0) THEN 
        NFUNC   = 1
        NVARTMP = 1
      ELSE
        NFUNC   = 0
        NVARTMP = 0
      ENDIF
      NUVAR   = 1 
c-----------------
      UPARAM(1)  = E 
      UPARAM(2)  = NU
      UPARAM(3)  = CA
      UPARAM(4)  = SIGM
      UPARAM(5)  = EPSM
      UPARAM(6)  = EPSR1
      UPARAM(7)  = EPSR2
      UPARAM(8)  = CB
      UPARAM(9)  = CN
      UPARAM(10) = ICC
      UPARAM(11) = CC
      UPARAM(12) = CP
      UPARAM(13) = ISRAT
      UPARAM(14) = ASRATE
      UPARAM(15) = FISOKIN
      UPARAM(16) = G
      UPARAM(17) = TWO*G
      UPARAM(18) = THREE*G
      UPARAM(19) = BULK
      UPARAM(20) = A1
      UPARAM(21) = A1*NU
      UPARAM(22) = EPSGM
      UPARAM(23) = Ivflag
      UPARAM(24) = YLD_SCALE
c-----------------
      PARMAT(1) = BULK
      PARMAT(2) = E
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = ASRATE
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G/(BULK+FOUR_OVER_3*G) ! == (1-2*nu)/(1-nu)
C     
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(27) = SQRT(A1/RHO0)  ! sound speed estimation
      PM(100)= BULK      
c-----------------
c     Element buffer variable allocation
c-----------------
      MTAG%G_PLA   = 1
      MTAG%L_PLA   = 1
      MTAG%L_EPSD  = 1
      MTAG%G_EPSD  = 1
      IF (FISOKIN > ZERO) THEN 
        MTAG%L_SIGB  = 3
      ENDIF
C
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC") 
      CALL INIT_MAT_KEYWORD(MATPARAM,"BEAM_ALL")   
      CALL INIT_MAT_KEYWORD(MATPARAM,"TRUSS")         
C
      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")
c-----------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,44
      WRITE(IOUT,1100)

      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300)E,NU,CA,CB,CN,FISOKIN,SIGM,
     .      EPSP_REF,CP,ICC,ISRATE,ASRATE,Ivflag,EPSM,
     .      EPSR1,EPSR2,IFUNC(1),YLD_SCALE
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : ELASTIC-PLASTIC (COWPER-SYMONDS)',/,
     & 5X,'-------------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'HARDENING PARAMETER. . . . . . . . . . =',1PG20.13/
     & 5X,'HARDENING EXPONENT . . . . . . . . . . =',1PG20.13/
     & 5X,'ISO-KINEMATIC HARDENNING FACTOR. . . . =',1PG20.13/
     & 5X,'MAXIMUM STRESS . . . . . . . . . . . . =',1PG20.13/
     & 5X,'REFERENCE STRAIN RATE CC . . . . . . . =',1PG20.13/
     & 5X,'STRAIN RATE EXPONENT (1/P) . . . . . . =',1PG20.13/
     & 5X,'FLAG FOR STRAIN RATE ON SIG-MAX. . . . =',I10/
     & 5X,'SMOOTH STRAIN RATE OPTION. . . . . . . =',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY. . . . . =',1PG20.13/
     & 5X,'STRAIN RATE CHOICE FLAG  . . . . . . . =',I10/
     & 5X,'     VP=1  EQUIVALENT PLASTIC STRAIN RATE'/
     & 5X,'     VP=2  TOTAL STRAIN RATE (DEFAULT)'/
     & 5X,'     VP=3  DEVIATORIC STRAIN RATE'/
     & 5X,'FAILURE PLASTIC STRAIN . . . . . . . . =',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 1 . . . . . . . =',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 2 . . . . . . . =',1PG20.13/
     & 5X,'YIELD FUNCTION IDENTIFIER  . . . . . . =',I10/
     & 5X,'YIELD FUNCTION SCALE FACTOR  . . . . . =',1PG20.13/)
c-----------------------------------------------------------------------
      RETURN
      END
