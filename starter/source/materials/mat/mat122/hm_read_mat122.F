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
Chd|  HM_READ_MAT122                source/materials/mat/mat122/hm_read_mat122.F
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
      SUBROUTINE HM_READ_MAT122(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MAXFUNC  ,  
     .           NFUNC    ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM       ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           MATPARAM ,NVARTMP  )
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
#include      "nchar_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT) :: UPARAM
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM,MAXFUNC
      INTEGER, INTENT(INOUT)                       :: NUPARAM,NUVAR,NFUNC,NVARTMP
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, DIMENSION(MAXFUNC), INTENT(INOUT)   :: IFUNC
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      my_real, DIMENSION(100),INTENT(INOUT)        :: PARMAT
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB 
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM   
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL  
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ILAW,ISH,ITR,IRES,IBUCK,IFUNCD1,IFUNCD2,
     .        IFUNCD2C,LTYPE11,LTYPE12,LTYPER0
      my_real
     .   RHO0,E1,E2,E3,G12,G23,G31,NU12,NU23,NU31,NU21,NU32,NU13,
     .   E1C,GAMMA,SIGY0,BETA,M,A,EFTI,EFTU,DFTU,EFCI,EFCU,DFCU,
     .   DSAT1,Y0,YC,B,DMAX,YR,YSP,DSAT2,Y0P,YCP,DSAT2C,Y0PC,YCPC,
     .   EPSD11,D11E,N11E,D11U,N11U,EPSD12,D22E,N22E,D12E,N12E,
     .   EPSDR0,DR0,NR0,FCUT
      my_real 
     .   DETC,FAC,A11,A22,A12,C11,C22,C12,C33,C13,C23,D11,D22,D33,
     .   D12,D13,D23,DMN,DMX 
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW         = 122
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
!card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card2 - Young's moduli + Shear moduli
      CALL HM_GET_FLOATV('MAT_E1'    ,E1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E2'    ,E2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E3'    ,E3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G12'   ,G12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G23'   ,G23      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card3 - Shear modulus + Poisson's ratio
      CALL HM_GET_FLOATV('MAT_G31'   ,G31      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU12'  ,NU12     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU23'  ,NU23     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU31'  ,NU31     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card4 - Compression elasticity + flags
      CALL HM_GET_FLOATV('MAT_E1C'   ,E1C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMMA' ,GAMMA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISH'       ,ISH      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('ITR'       ,ITR      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('IRES'      ,IRES     ,IS_AVAILABLE, LSUBMODEL)
!card5 - Plasticity parameter
      CALL HM_GET_FLOATV('SIGY0'     ,SIGY0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('BETA'      ,BETA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('M'         ,M        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('A'         ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card6 - Fiber tensile damage parameters
      CALL HM_GET_FLOATV('MAT_EFTI'  ,EFTI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EFTU'  ,EFTU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DFTU'  ,DFTU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card7 - Fiber compression damage parameters
      CALL HM_GET_FLOATV('MAT_EFCI'  ,EFCI     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EFCU'  ,EFCU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DFCU'  ,DFCU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('IBUCK'     ,IBUCK    ,IS_AVAILABLE, LSUBMODEL)
!card8 - Matrix in-plane shear damage
      CALL HM_GET_INTV  ('IFUNCD1'   ,IFUNCD1  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_DSAT1' ,DSAT1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Y0'    ,Y0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YC'    ,YC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_B'     ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card9 - Shear common damage parameters
      CALL HM_GET_FLOATV('MAT_DMAX'  ,DMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YR'    ,YR       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YSP'   ,YSP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card10 - Matrix transverse shear tensile damage
      CALL HM_GET_INTV  ('IFUNCD2'   ,IFUNCD2  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_DSAT2' ,DSAT2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Y0P'   ,Y0P      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YCP'   ,YCP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card11 - Matrix transverse shear compression damage
      CALL HM_GET_INTV  ('IFUNCD2C'  ,IFUNCD2C ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_DSAT2C',DSAT2C   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_Y0PC'  ,Y0PC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_YCPC'  ,YCPC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card12 - Fiber direction strain rate dependency parameters
      CALL HM_GET_FLOATV('MAT_EPSD11',EPSD11   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D11'   ,D11E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N11'   ,N11E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D11U'  ,D11U     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N11U'  ,N11U     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card13 - Shear and transverse directions strain rate dependency parameters
      CALL HM_GET_FLOATV('MAT_EPSD12',EPSD12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D22'   ,D22E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N22'   ,N22E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D12'   ,D12E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_N12'   ,N12E     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!card14 - Initial yield stress strain rate dependency parameters + law types
      CALL HM_GET_FLOATV('MAT_EPSDR0',EPSDR0   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DR0'   ,DR0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NR0'   ,NR0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('LTYPE11'   ,LTYPE11  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('LTYPE12'   ,LTYPE12  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('LTYPER0'   ,LTYPER0  ,IS_AVAILABLE, LSUBMODEL)
!card15 - Equivalent strain rate cutoff frequency 
      CALL HM_GET_FLOATV('FCUT'      ,FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c-----------------------------
c     Check and default values
c-----------------------------
      ! Poisson's ratio
      IF (NU12 < ZERO .OR. NU12 >= HALF) THEN
        CALL ANCMSG(MSGID=3032,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU12,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF    
      NU21 = NU12*E2/E1
      IF (NU21 < ZERO .OR. NU21 >= HALF) THEN
        CALL ANCMSG(MSGID=3033,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU21,
     .              I1=MAT_ID,
     .              C1=TITR)   
      ENDIF
      IF (NU23 < ZERO .OR. NU23 >= HALF) THEN
        CALL ANCMSG(MSGID=3034,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU23,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      NU32 = NU23*E3/E2
      IF (NU32 < ZERO .OR. NU32 >= HALF) THEN
        CALL ANCMSG(MSGID=3035,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU32,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      IF (NU31 < ZERO .OR. NU31 >= HALF) THEN
        CALL ANCMSG(MSGID=3036,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU31,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF 
      NU13 = NU31*E1/E3
      IF (NU13 < ZERO .OR. NU13 >= HALF) THEN
        CALL ANCMSG(MSGID=3037,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              R1=NU13,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF 
      ! Young modulus
      IF (E3 == ZERO)  E3  = E2
      ! Shear modulus
      IF (G31 == ZERO) G31 = G12
c
      ! Checking Poisson's ratio 
      DETC = ONE - NU12*NU21
      IF (DETC <= ZERO) THEN
        CALL ANCMSG(MSGID=307,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF 
c
      ! Elasticity matrix for 2D plane stress
      FAC = ONE/(ONE - NU12*NU21)
      A11 = E1*FAC
      A12 = NU21*A11
      A22 = E2*FAC
      ! Compliance matrix for 3D
      C11 = ONE/E1
      C22 = ONE/E2
      C33 = ONE/E3
      C12 =-NU12/E1
      C13 =-NU31/E3
      C23 =-NU23/E2      
      ! Checking input
      DETC= C11*C22*C33-C11*C23*C23-C12*C12*C33+C12*C13*C23
     .     +C13*C12*C23-C13*C22*C13
      IF(DETC<=ZERO) THEN
         CALL ANCMSG(MSGID=307,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      ! 3D elastic matrix
      D11  = (C22*C33-C23*C23)/DETC
      D12  =-(C12*C33-C13*C23)/DETC
      D13  = (C12*C23-C13*C22)/DETC
      D22  = (C11*C33-C13*C13)/DETC
      D23  =-(C11*C23-C13*C12)/DETC
      D33  = (C11*C22-C12*C12)/DETC  
      DMN  = MIN(D11*D22 -D12**2, D11*D33 - D13**2, D22*D33 - D23**2 )      
      DMX  = MAX(D11,D22,D33)
c 
      ! Default compression Young modulus in fiber direction
      IF (E1C == ZERO) E1C = E1
c 
      ! Default return mapping algorithm : Newton iteration
      IF (IRES == 0) IRES = 2
      IRES = MIN(IRES,2)
      IRES = MAX(IRES,1)
      ! Damage type
      IF (ISH == 0) ISH = 1
      ISH  = MIN(ISH,3)
      ISH  = MAX(ISH,1)
      IF (ITR == 0) ITR = 1
      ITR  = MIN(ITR,3)
      ITR  = MAX(ITR,1)
      ! Buckling effect
      IF (IBUCK == 0) IBUCK = 1
      IBUCK = MIN(IBUCK,2)
      IBUCK = MAX(IBUCK,1)
c
      ! Yield stress
      IF (SIGY0 == ZERO) SIGY0 = INFINITY
c 
      ! Fiber damage
      IF (EFTI == ZERO) EFTI = INFINITY
      IF (EFTU == ZERO) EFTU = TWO*INFINITY
      DFTU = MAX(DFTU,ZERO)
      DFTU = MIN(DFTU,ONE)
      IF (EFCI == ZERO) EFCI = INFINITY
      IF (EFCU == ZERO) EFCU = TWO*INFINITY
      DFCU = MAX(DFCU,ZERO)
      DFCU = MIN(DFCU,ONE)
c
      ! Matrix damage
      DMAX   = MIN(DMAX,ONE)
      DMAX   = MAX(DMAX,ZERO)
      DSAT1  = MIN(DSAT1,ONE)
      DSAT1  = MAX(DSAT1,ZERO)
      DSAT2  = MIN(DSAT2,ONE)
      DSAT2  = MAX(DSAT2,ZERO)
      DSAT2C = MIN(DSAT2C,ONE)
      DSAT2C = MAX(DSAT2C,ZERO)
      ! Shear/transverse damage coupling factor 
      IF (B == ZERO) B = E2/G12
c 
      ! Check scale factors in case of tabulated function for damage
      IF ((IFUNCD1 > 0).AND.(Y0 == ZERO)) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_Y0',Y0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ELSEIF (Y0 == ZERO) THEN 
        Y0 = INFINITY
      ENDIF
      IF ((IFUNCD2 > 0).AND.(Y0P == ZERO)) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_Y0P',Y0P,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ELSEIF (Y0P == ZERO) THEN 
        Y0P = INFINITY
      ENDIF
      IF ((IFUNCD2C > 0).AND.(Y0PC == ZERO)) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_Y0PC',Y0PC,IS_AVAILABLE,LSUBMODEL,UNITAB)   
      ENDIF
      IF (YR  == ZERO) YR  = INFINITY
      IF (YSP == ZERO) YSP = INFINITY
c 
      ! Check reference strain rate
      IF (EPSD11 == ZERO) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_EPSD11',EPSD11,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF    
      IF (EPSD12 == ZERO) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_EPSD12',EPSD12,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF   
      IF (EPSDR0 == ZERO) THEN 
        CALL HM_GET_FLOATV_DIM('MAT_EPSDR0',EPSDR0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF  
c 
      ! Strain rate dependency law type flags
      IF (LTYPE11 == 0) LTYPE11 = 1
      LTYPE11 = MIN(LTYPE11,4)
      LTYPE11 = MAX(LTYPE11,1)
      IF (LTYPE12 == 0) LTYPE12 = 1
      LTYPE12 = MIN(LTYPE12,4)
      LTYPE12 = MAX(LTYPE12,1)
      IF (LTYPER0 == 0) LTYPER0 = 1
      LTYPER0 = MIN(LTYPER0,4)
      LTYPER0 = MAX(LTYPER0,1) 
c 
      ! Default strain rate cutoff frequency
      IF (FCUT == ZERO) FCUT = 5000.0D0*UNITAB%FAC_T_WORK
c
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameters
      NUPARAM = 63
      ! Number of functions
      NFUNC   = 3
      ! Number of user variables 
      NUVAR   = 17
      ! Number of temporary variable for interpolation
      NVARTMP = 3
c          
      ! Material parameters
      UPARAM(1)  = E1    
      UPARAM(2)  = E2    
      UPARAM(3)  = E3    
      UPARAM(4)  = NU12    
      UPARAM(5)  = NU21
      UPARAM(6)  = NU13
      UPARAM(7)  = NU31    
      UPARAM(8)  = NU23    
      UPARAM(9)  = NU32
      UPARAM(10) = G12    
      UPARAM(11) = G23    
      UPARAM(12) = G31
      UPARAM(13) = E1C
      UPARAM(14) = GAMMA
      UPARAM(15) = ISH
      UPARAM(16) = ITR
      UPARAM(17) = IRES
      UPARAM(18) = SIGY0
      UPARAM(19) = BETA 
      UPARAM(20) = M 
      UPARAM(21) = A 
      UPARAM(22) = EFTI 
      UPARAM(23) = EFTU
      UPARAM(24) = DFTU 
      UPARAM(25) = EFCI 
      UPARAM(26) = EFCU 
      UPARAM(27) = DFCU 
      UPARAM(28) = IBUCK
      UPARAM(29) = DSAT1
      UPARAM(30) = Y0
      UPARAM(31) = YC
      UPARAM(32) = B
      UPARAM(33) = DMAX
      UPARAM(34) = YR
      UPARAM(35) = YSP 
      UPARAM(36) = DSAT2
      UPARAM(37) = Y0P 
      UPARAM(38) = YCP
      UPARAM(39) = DSAT2C
      UPARAM(40) = Y0PC 
      UPARAM(41) = YCPC
      UPARAM(42) = EPSD11
      UPARAM(43) = D11E
      UPARAM(44) = N11E
      UPARAM(45) = D11U
      UPARAM(46) = N11U 
      UPARAM(47) = EPSD12
      UPARAM(48) = D22E
      UPARAM(49) = N22E
      UPARAM(50) = D12E
      UPARAM(51) = N12E
      UPARAM(52) = EPSDR0
      UPARAM(53) = DR0
      UPARAM(54) = NR0
      UPARAM(55) = LTYPE11
      UPARAM(56) = LTYPE12      
      UPARAM(57) = LTYPER0
      UPARAM(58) = D11
      UPARAM(59) = D12
      UPARAM(60) = D13
      UPARAM(61) = D22
      UPARAM(62) = D23
      UPARAM(63) = D33
c
      ! Function IDs
      IFUNC(1)  = IFUNCD1 
      IFUNC(2)  = IFUNCD2
      IFUNC(3)  = IFUNCD2C
c      
      ! PARMAT table
      ISRATE     = 1
      PARMAT(1)  = MAX(A11,A22,D11,D22,D33)
      PARMAT(2)  = MAX(E1,E2,E3)
      PARMAT(3)  = MAX(NU12,NU13,NU23)
      PARMAT(4)  = ISRATE
      PARMAT(5)  = FCUT
      PARMAT(16) = 1
      PARMAT(17) = DMN/DMX/DMX
c
      ! PM table
      PM(1)  = RHO0
      PM(89) = RHO0
c      
      ! MTAG variable activation
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
      MTAG%G_DMG  = 6
      MTAG%L_DMG  = 6
c
      ! Number of output modes 
      ! -> Stored in DMG(:,2:4)
      MATPARAM%NMOD = 3
      ALLOCATE(MATPARAM%MODE(MATPARAM%NMOD))
      MATPARAM%MODE(1) = "Fiber damage"
      MATPARAM%MODE(2) = "Shear matrix damage"
      MATPARAM%MODE(3) = "Transverse matrix damage"
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"HOOK")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ORTHOTROPIC")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ORTHOTROPIC")
c
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1050)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0
        WRITE(IOUT,1300) E1,E2,E3,G12,G23,G31,NU12,NU23,NU31,E1C,GAMMA
        WRITE(IOUT,1400) SIGY0,BETA,M,A,IRES
        WRITE(IOUT,1500) EFTI,EFTU,DFTU,EFCI,EFCU,DFCU,IBUCK 
        WRITE(IOUT,1600) ISH,IFUNCD1,DSAT1,Y0,YC,B
        WRITE(IOUT,1700) DMAX,YR,YSP
        WRITE(IOUT,1800) ITR,IFUNCD2,DSAT2,Y0P,YCP,IFUNCD2C,DSAT2C,Y0PC,YCPC
        WRITE(IOUT,1900) EPSD11,D11E,N11E,D11U,N11U,LTYPE11 
        WRITE(IOUT,2000) EPSD12,D22E,N22E,D12E,N12E,LTYPE12 
        WRITE(IOUT,2100) EPSDR0,DR0,NR0,LTYPER0
        WRITE(IOUT,2200) FCUT
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1050 FORMAT
     &(5X,'MATERIAL MODEL : MODIFIED LADEVEZE',/,
     & 5X,'----------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . .=',1PG20.13/) 
 1300 FORMAT(
     & 5X,'ELASTICITY PARAMETERS:                            ',/
     & 5X,'----------------------                            ',/
     & 5X,'YOUNG MODULUS IN DIR. 1 (FIBER)  E1 . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS IN DIR. 2 (MATRIX) E2 . . . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS IN DIR. 3 (MATRIX) E3 . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS IN PLANE 12 G12 . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS IN PLANE 23 G23 . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS IN PLANE 31 G31 . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO IN PLANE 12 NU12. . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO IN PLANE 23 NU23. . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO IN PLANE 31 NU31. . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL COMPRESSION YOUNG MODULUS IN DIR. 1 E1C .=',1PG20.13/
     & 5X,'COMPRESSION YOUNG MODULUS FACTOR GAMMA. . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'PLASTICITY PARAMETERS:                            ',/
     & 5X,'---------------------                             ',/
     & 5X,'INITIAL YIELD STRESS SIGY0. . . . . . . . . . . .=',1PG20.13/     
     & 5X,'HARDENING MODULUS BETA. . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'HARDENING EXPONENT M. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'PLASTICITY COUPLING FACTOR A. . . . . . . . . . .=',1PG20.13/
     & 5X,'RETURN MAPPING TYPE FLAG. . . . . . . . . . . . .=',I3/
     & 5X,'  IRES = 1  NICE EXPLICIT                         '/
     & 5X,'  IRES = 2  CUTTING PLANE SEMI-IMPLICIT (DEFAULT) '/)
 1500 FORMAT(
     & 5X,'FIBER (DIR. 1) DAMAGE PARAMETERS:                 ',/
     & 5X,'---------------------------------                 ',/
     & 5X,'TENSILE FIBER INITIAL DAMAGE STRAIN EFTI. . . . .=',1PG20.13/     
     & 5X,'TENSILE FIBER ULTIMATE DAMAGE STRAIN EFTU . . . .=',1PG20.13/
     & 5X,'TENSILE FIBER ULTIMATE DAMAGE DFTU. . . . . . . .=',1PG20.13/
     & 5X,'COMPRESSION FIBER INITIAL DAMAGE STRAIN EFCI. . .=',1PG20.13/     
     & 5X,'COMPRESSION FIBER ULTIMATE DAMAGE STRAIN EFCU . .=',1PG20.13/
     & 5X,'COMPRESSION FIBER ULTIMATE DAMAGE DFCU. . . . . .=',1PG20.13/
     & 5X,'BUCKLING DAMAGE MATRIX ON FIBER EFFECT FLAG . . .=',I3/
     & 5X,'  IBUCK = 1 NOT ACTIVATED (DEFAULT)'/
     & 5X,'  IBUCK = 2 ACTIVATED'/)
 1600 FORMAT(
     & 5X,'SHEAR MATRIX DAMAGE PARAMETERS:                   ',/
     & 5X,'-------------------------------                   ',/
     & 5X,'SHEAR DAMAGE TYPE FLAG. . . . . . . . . . . . . .=',I3/
     & 5X,'  ISH = 1   LINEAR (DEFAULT)                      '/
     & 5X,'  ISH = 2   EXPONENTIAL                           '/
     & 5X,'  ISH = 3   TABULATED CURVE                       '/
     & 5X,'DAMAGE TABULATED FUNCTION IFUNCD1 . . . . . . . .=',I10/     
     & 5X,'DAMAGE SATURATION DSAT1 . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL DAMAGE ENERGY THRESHOLD Y0. . . . . . . .=',1PG20.13/
     & 5X,'CRITICAL DAMAGE ENERGY LIMIT YC . . . . . . . . .=',1PG20.13/     
     & 5X,'FACTOR B. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1700 FORMAT(
     & 5X,'COMMON DAMAGE PARAMETERS:                         ',/
     & 5X,'-------------------------                         ',/
     & 5X,'DAMAGE MAXIMAL ALLOWED VALUE DMAX . . . . . . . .=',1PG20.13/
     & 5X,'ELEMENTARY SHEAR DAMAGE VALUE YR. . . . . . . . .=',1PG20.13/
     & 5X,'BRITTLE TRANSVERSE DAM. LIMIT FOR INTERFACE YSP .=',1PG20.13/)
 1800 FORMAT(
     & 5X,'TRANSVERSE MATRIX DAMAGE PARAMETERS:              ',/
     & 5X,'------------------------------------              ',/
     & 5X,'TRANSVERSE DAMAGE TYPE FLAG . . . . . . . . . . .=',I3/
     & 5X,'  ITR = 1   LINEAR (DEFAULT)                      '/
     & 5X,'  ITR = 2   EXPONENTIAL                           '/
     & 5X,'  ITR = 3   TABULATED CURVE                       '/
     & 5X,'TENSILE DAMAGE TABULATED FUNCTION IFUNCD2 . . . .=',I10/     
     & 5X,'TENSILE DAMAGE SATURATION DSAT2 . . . . . . . . .=',1PG20.13/
     & 5X,'TENSILE INITIAL DAMAGE ENERGY THRESHOLD Y0P . . .=',1PG20.13/
     & 5X,'TENSILE CRITICAL DAMAGE ENERGY LIMIT YCP. . . . .=',1PG20.13/  
     & 5X,'(COMPRESSION PARAMETERS FOR SHELLS ONLY)          ',/
     & 5X,'COMPRESSION DAMAGE TABULATED FUNCTION IFUNCD2 . .=',I10/     
     & 5X,'COMPRESSION DAMAGE SATURATION DSAT2C. . . . . . .=',1PG20.13/
     & 5X,'COMPRESSION INITIAL DAMAGE ENERGY THRESHOLD Y0PC.=',1PG20.13/
     & 5X,'COMPRESSION CRITICAL DAMAGE ENERGY LIMIT YCP. . .=',1PG20.13/)
 1900 FORMAT(
     & 5X,'FIBER DIR. 1 STRAIN RATE DEPENDENCY PARAMETERS:   ',/
     & 5X,'-----------------------------------------------   ',/   
     & 5X,'REFERENCE STRAIN RATE EPSD11. . . . . . . . . . .=',1PG20.13/
     & 5X,'1ST PARAMETER FOR ELASTICITY RATE DEPENDENCY D11.=',1PG20.13/
     & 5X,'2ND PARAMETER FOR ELASTICITY RATE DEPENDENCY N11.=',1PG20.13/  
     & 5X,'1ST PARAMETER FOR RUPTURE RATE DEPENDENCY D11U. .=',1PG20.13/  
     & 5X,'2ND PARAMETER FOR RUPTURE RATE DEPENDENCY N11U. .=',1PG20.13/  
     & 5X,'RATE DEPENDENCY LAW TYPE. . . . . . . . . . . . .=',I3/     
     & 5X,'  LTYPE11 = 1 POWER LAW (DEFAULT)                 ',/   
     & 5X,'  LTYPE11 = 2 LINEAR LAW                          ',/   
     & 5X,'  LTYPE11 = 3 LOGARITHMIC LAW                     ',/   
     & 5X,'  LTYPE11 = 4 TANGENT HYPERBOLIC LAW              ',/)
 2000 FORMAT(
     & 5X,'TRANVERSE/SHEAR STRAIN RATE DEPENDENCY PARAMETERS:',/
     & 5X,'--------------------------------------------------',/   
     & 5X,'REFERENCE STRAIN RATE EPSD12. . . . . . . . . . .=',1PG20.13/
     & 5X,'1ST PARAMETER FOR ELASTICITY RATE DEPENDENCY D22.=',1PG20.13/
     & 5X,'2ND PARAMETER FOR ELASTICITY RATE DEPENDENCY N22.=',1PG20.13/  
     & 5X,'1ST PARAMETER FOR ELASTICITY RATE DEPENDENCY D12.=',1PG20.13/  
     & 5X,'2ND PARAMETER FOR ELASTICITY RATE DEPENDENCY N12.=',1PG20.13/  
     & 5X,'RATE DEPENDENCY LAW TYPE. . . . . . . . . . . . .=',I3/     
     & 5X,'  LTYPE12 = 1 POWER LAW (DEFAULT)                 ',/   
     & 5X,'  LTYPE12 = 2 LINEAR LAW                          ',/   
     & 5X,'  LTYPE12 = 3 LOGARITHMIC LAW                     ',/   
     & 5X,'  LTYPE12 = 4 TANGENT HYPERBOLIC LAW              ',/)
 2100 FORMAT(
     & 5X,'YIELD STRESS STRAIN RATE DEPENDENCY PARAMETERS:   ',/
     & 5X,'-----------------------------------------------   ',/   
     & 5X,'REFERENCE STRAIN RATE EPSDR0. . . . . . . . . . .=',1PG20.13/
     & 5X,'1ST PARAM. FOR YIELD STRESS RATE DEPENDENCY DR0 .=',1PG20.13/
     & 5X,'2ND PARAM. FOR YIELD STRESS RATE DEPENDENCY NR0 .=',1PG20.13/
     & 5X,'RATE DEPENDENCY LAW TYPE. . . . . . . . . . . . .=',I3/     
     & 5X,'  LTYPER0 = 1 POWER LAW  (DEFAULT)                ',/   
     & 5X,'  LTYPER0 = 2 LINEAR LAW                          ',/   
     & 5X,'  LTYPER0 = 3 LOGARITHMIC LAW                     ',/   
     & 5X,'  LTYPER0 = 4 TANGENT HYPERBOLIC LAW              ',/)
 2200 FORMAT(
     & 5X,'STRAIN RATE FILTERING CUTOFF FREQUENCY FCUT . . .=',1PG20.13/)
c-----------------------------------------------------------------------
      END
