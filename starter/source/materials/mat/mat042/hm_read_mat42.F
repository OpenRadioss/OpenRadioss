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
      !||====================================================================
      !||    hm_read_mat42              ../starter/source/materials/mat/mat042/hm_read_mat42.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim          ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword           ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_MAT42(
     .           MATPARAM ,NUVAR    ,MAXFUNC  ,NFUNC    ,IFUNC    ,
     .           PARMAT   ,IMATVIS  ,UNITAB   ,LSUBMODEL,ID       ,
     .           TITR     ,PM       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW42 WITH HM READER
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
      INTEGER, INTENT(IN)      :: ID
      INTEGER, INTENT(IN)      :: MAXFUNC
      INTEGER, INTENT(INOUT)   :: NUVAR
      INTEGER, INTENT(INOUT)   :: NFUNC
      INTEGER, INTENT(INOUT)   :: IMATVIS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM     
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT) :: IFUNC
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,IFORM,IFUNC_BLK,NPRONY,NUPARAM,NIPARAM,NORDER,ILAW
      my_real :: RHO0,RHOR,BULK,SMAX,GS,MU0,NU,GVMAX,C1,ZEP495,FSCAL,FSCAL_UNIT
      my_real, DIMENSION(10)  :: MU,AL
      my_real, DIMENSION(100) :: GI,TAUX
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW         = 42  
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
!card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'    ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'  ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card2 - Poisson's ratior - Bulk definition - Flags   
      CALL HM_GET_FLOATV('MAT_NU'     ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGCUT' ,SMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_BULK'   ,IFUNC_BLK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_FScale' ,FSCAL    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ORDER'      ,NPRONY   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('IFORM'      ,IFORM    ,IS_AVAILABLE,LSUBMODEL)      
!card3 - Shear hyperelastic modulus parameters
      CALL HM_GET_FLOATV('MAT_MUE1'   ,MU(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE2'   ,MU(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE3'   ,MU(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE4'   ,MU(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE5'   ,MU(5)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
!card4 - Shear hyperelastic modulus parameters
      CALL HM_GET_FLOATV('MAT_MUE6'   ,MU(6)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE7'   ,MU(7)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE8'   ,MU(8)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE9'   ,MU(9)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE10'  ,MU(10)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
!card5 - Material exponents
      CALL HM_GET_FLOATV('MAT_ALPHA11',AL(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA22',AL(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA33',AL(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA44',AL(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA55',AL(5)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card6 - Material exponents
      CALL HM_GET_FLOATV('MAT_ALPHA6 ',AL(6)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA7 ',AL(7)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA8 ',AL(8)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA9 ',AL(9)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_ALPHA10',AL(10)   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card7 - Prony series      
      IF (NPRONY > 0) THEN
        DO I=1,NPRONY    
          CALL HM_GET_FLOAT_ARRAY_INDEX('Gamma_arr',GI(I)  ,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
        DO I=1,NPRONY     
          CALL HM_GET_FLOAT_ARRAY_INDEX('Tau_arr'  ,TAUX(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
      ENDIF
c
c-----------------------------
c     Check and default values
c-----------------------------
c
      ! Reference density 
      IF (RHOR == ZERO)  RHOR  = RHO0
c 
      ! Poisson's ratio 
      ZEP495  = ZEP4 + NINE*EM02 + FIVE*EM03
      IF (NU == ZERO ) NU = ZEP495
c 
      ! Tensile stress limit
      IF (SMAX <= ZERO)  SMAX  = EP20
c
      ! Scale factor for bulk tabulation
      CALL HM_GET_FLOATV_DIM('MAT_FScale',FSCAL_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (FSCAL == ZERO) FSCAL = ONE*FSCAL_UNIT
c 
      ! Check strain energy density formulation flag
      IF (IFORM == 0) IFORM = 1
      IFORM = MIN(2,IFORM)
      IFORM = MAX(1,IFORM)
c
      ! Viscosity parameters
      IMATVIS = 1
      IF (NPRONY > 0 ) THEN
        IMATVIS = 3
      ENDIF
      ! Ground state viscous shear modulus
      GVMAX = ZERO
      DO I=1,NPRONY
        GVMAX = GVMAX + GI(I)
      ENDDO      
!------------------------------------------------------------
!     Check Ogden parameter order (skip zero values of mu)
!------------------------------------------------------------
      NORDER = 0
      DO I = 1,10
        IF (MU(I) /= ZERO) THEN
          NORDER = NORDER + 1
          MU(NORDER) = MU(I)
          AL(NORDER) = AL(I)
        END IF
      END DO
!
      GS = ZERO
      DO I = 1,NORDER
        GS = GS + MU(I)*AL(I)
      END DO
      IF (GS <= ZERO) THEN
        CALL ANCMSG(MSGID=828, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              I1=ID,
     .              C1=TITR)
      END IF
c 
      ! Initial shear and bulk modulus
      MU0  = GS/TWO
      BULK = GS*(ONE+NU)/MAX(EM20,THREE*(ONE-TWO*NU))
c--------------------------
c     Filling buffer tables
c-------------------------- 
      NFUNC   = 1               ! Number of functions
      NUVAR   = 12 + 6*NPRONY   ! Number of user variables
      NIPARAM = 3
      NUPARAM = 24 + 2*NPRONY   ! Number of material parameters
!
      MATPARAM%NIPARAM = NIPARAM
      MATPARAM%NUPARAM = NUPARAM
      ALLOCATE (MATPARAM%IPARAM(NIPARAM))
      ALLOCATE (MATPARAM%UPARAM(NUPARAM))
c          
      ! Material parameters
      MATPARAM%IPARAM(1)  = NORDER
      MATPARAM%IPARAM(2)  = NPRONY
      MATPARAM%IPARAM(3)  = IFORM
      
      MATPARAM%UPARAM(1)  = MU(1)
      MATPARAM%UPARAM(2)  = MU(2)
      MATPARAM%UPARAM(3)  = MU(3)
      MATPARAM%UPARAM(4)  = MU(4)
      MATPARAM%UPARAM(5)  = MU(5)
      MATPARAM%UPARAM(6)  = MU(6)
      MATPARAM%UPARAM(7)  = MU(7)
      MATPARAM%UPARAM(8)  = MU(8)
      MATPARAM%UPARAM(9)  = MU(9)
      MATPARAM%UPARAM(10) = MU(10)
      MATPARAM%UPARAM(11) = AL(1)   
      MATPARAM%UPARAM(12) = AL(2)   
      MATPARAM%UPARAM(13) = AL(3)   
      MATPARAM%UPARAM(14) = AL(4)   
      MATPARAM%UPARAM(15) = AL(5)   
      MATPARAM%UPARAM(16) = AL(6)   
      MATPARAM%UPARAM(17) = AL(7)   
      MATPARAM%UPARAM(18) = AL(8)   
      MATPARAM%UPARAM(19) = AL(9)   
      MATPARAM%UPARAM(20) = AL(10)  
      MATPARAM%UPARAM(21) = BULK
      MATPARAM%UPARAM(22) = NU
      MATPARAM%UPARAM(23) = SMAX
      MATPARAM%UPARAM(24) = FSCAL
      IF (NPRONY > 0) THEN
        DO I=1,NPRONY
          MATPARAM%UPARAM(24 + I) = GI(I) 
          MATPARAM%UPARAM(24 + NPRONY + I) = TAUX(I)
        ENDDO
      ENDIF
c
      ! Function IDs
      IFUNC(1) = IFUNC_BLK
c      
      ! PARMAT table
      PARMAT(1) = GS
      PARMAT(2) = GS*(ONE+NU)
      PARMAT(3) = NU
      PARMAT(6) = BULK
      C1 = THIRD*GS*(ONE + NU)/(ONE - TWO*NU)
      PARMAT(16) = 2
      PARMAT(17) = GS/(C1 + TWO_THIRD*GS)
c
      ! PM /matparam common parameters
      
      MATPARAM%RHO  = RHOR
      MATPARAM%RHO0 = RHO0
      PM(100)= BULK      
c      
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")  
c
c--------------------------
c     Parameters printout
c--------------------------
      WRITE(IOUT,1100) TRIM(TITR),ID,42
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) NU,SMAX,IFUNC_BLK,FSCAL,IFORM
        WRITE(IOUT,1400) NORDER
        DO I = 1,NORDER
          WRITE(IOUT,1500) I,MU(I),AL(I) 
        ENDDO 
        WRITE(IOUT,1600) MU0,BULK
        IF (NPRONY > 0) THEN 
          WRITE(IOUT,1700) NPRONY
          DO I = 1, NPRONY
            WRITE(IOUT,1800) I,GI(I),TAUX(I) 
          ENDDO 
        ENDIF
      ENDIF     
C-----------------
      RETURN
C-----------------
 1000 FORMAT(
     &  5X,'------------------------------------------',/
     &  5X,'  MATERIAL MODEL : GREEN-ELASTIC (OGDEN)  ',/,
     &  5X,'------------------------------------------',/)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'POISSON RATIO NU. . . . . . . . . . . . . . . . .=',1PG20.13/, 
     & 5X,'CUT-OFF STRESS IN TENSION SIGMA CUT . . . . . . .=',1PG20.13/,
     & 5X,'BULK FUNCTION NUMBER ID . . . . . . . . . . . . .=',I10/,
     & 5X,'SCALE FACTOR FOR BULK FUNCTION. . . . . . . . . .=',1PG20.13/, 
     & 5X,'INCOMP. FORMULATION FLAG IFORM. . . . . . . . . .=',I2/,
     & 5X,'  IFORM = 1: STANDARD STRAIN ENERGY DENSITY (DEFAULT)',/,
     & 5X,'  IFORM = 2: MODIFIED STRAIN ENERGY DENSITY          ',/)
 1400 FORMAT(
     & 5X,'OGDEN MODEL PARAMETERS:                           ',/,
     & 5X,'------------------------                          ',/,
     & 5X,'                                                  ',/,
     & 5X,'NUMBER OF TERMS IN OGDEN MODEL. . . . . . . . . .=',I8/)
 1500 FORMAT(
     & 5X,'OGDEN TERM NUMBER # '                              ,I8/,
     & 5X,'GROUND SHEAR MODULUS. . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'EXPONENT ALPHA. . . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1600 FORMAT(
     & 5X,'INITIAL SHEAR MODULUS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BULK MODULUS  . . . . . . . . . . . . . . . . . .=',1PG20.13/) 
 1700 FORMAT(
     & 5X,'PRONY SERIES PARAMETERS:                          ',/,
     & 5X,'------------------------                          ',/,
     & 5X,'                                                  ',/,
     & 5X,'NUMBER OF TERMS IN PRONY SERIES M . . . . . . . .=',I8/)
 1800 FORMAT(
     & 5X,'PRONY TERM NUMBER # '                              ,I8/,
     & 5X,'SHEAR STIFFNESS . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'RELAXATION TIME . . . . . . . . . . . . . . . . .=',1PG20.13/)  
C-----------------
      END
