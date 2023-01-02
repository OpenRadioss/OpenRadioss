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
Chd|  HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
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
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT93(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM ,NVARTMP  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW93 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)          :: UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,
     .                                         MAXFUNC,MAXUPARAM,NUPARAM,NUVAR,IMATVIS ,
     .                                         NVARTMP
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER*nchartitle,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(OUT)   :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NRATE,I,ILAW,VP
      my_real 
     .    E11,E22,E33,NU12,NU23,NU13,G12,G13,G23,QR1,QR2,CR1,CR2,
     .    SIGY,R11,R22,R33,R12,R13,R23,A1,A2,A3,HH,FF,GG,LL,MM,NN,
     .    D11,D22,D33,D12,D13,D23,A11,A22,A12,C11,C22,C33,C12,C13,
     .    C23,NU21,NU31,NU32,DETC,FAC,YFAC(100),RATE(100),DMIN,DMAX,
     .    YSCALE_UNIT,RHO0,RHOR,ASRATE
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 93
c
c-----------------------------------------------             
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c-----------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO',RHO0  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 - Orthotropic elastic parameters
      CALL HM_GET_FLOATV('LAW93_E11' ,E11  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_E22' ,E22  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_E33' ,E33  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_G12' ,G12  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_Nu12',NU12 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card3 - Orthotropic elastic parameters
      CALL HM_GET_FLOATV('LAW93_G13' ,G13  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_G23' ,G23  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_Nu13',NU13 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_Nu23',NU23 ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card4 - Number of curves for each rate
      CALL HM_GET_INTV  ('LAW93_NL'  ,NRATE  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('FCUT'      ,ASRATE ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('VP'        ,VP     ,IS_AVAILABLE, LSUBMODEL)
card5 - Curves parameters
      IF (NRATE > 0) THEN 
        DO I=1,NRATE
          CALL HM_GET_INT_ARRAY_INDEX  ('LAW93_arr1',IFUNC(I) ,I ,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('LAW93_arr2',YFAC(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('LAW93_arr3',RATE(I)  ,I ,IS_AVAILABLE, LSUBMODEL, UNITAB)     
          IF (YFAC(I) == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('LAW93_arr2' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            YFAC(I) = ONE * YSCALE_UNIT        
          ENDIF
        ENDDO
      ENDIF
card6 - Continuous hardening yield stress of Voce combination
      CALL HM_GET_FLOATV('LAW93_Sigma_y',SIGY  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_QR1'    ,QR1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_CR1'    ,CR1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_QR2'    ,QR2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_CR2'    ,CR2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card7 - Hill parameters (normalized yield stresses)
      CALL HM_GET_FLOATV('LAW93_R11'    ,R11   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_R22'    ,R22   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_R12'    ,R12   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card8 - Hill parameters (normalized yield stresses)
      CALL HM_GET_FLOATV('LAW93_R33'    ,R33   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_R13'    ,R13   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('LAW93_R23'    ,R23   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      
      !========== DEFAULT VALUES=============!
C
      ! Default value for functions 
      NFUNC = NRATE
      IF (NRATE > 1) THEN
        IF (RATE(1) == ZERO) THEN
          NFUNC = NRATE
        ELSE
          NFUNC = NRATE + 1
          DO J = NRATE,1,-1
            IFUNC(J+1) = IFUNC(J)
            RATE(J+1)  = RATE(J)
            YFAC(J+1)  = YFAC(J)
          ENDDO
          RATE(1) = ZERO
        ENDIF  
      ENDIF    
C            
      ! Yield stresses
      IF(SIGY == ZERO) SIGY = INFINITY
      IF(R11 == ZERO)  R11  = ONE
      IF(R22 == ZERO)  R22  = ONE
      IF(R33 == ZERO)  R33  = ONE
      IF(R12 == ZERO)  R12  = ONE
      IF(R23 == ZERO)  R23  = ONE
      IF(R13 == ZERO)  R13  = ONE
C
      ! Elastic parameters
      !   Poisson's ratio
      IF (NU12 < ZERO .OR. NU12 >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU12,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF  
      IF (NU13 < ZERO .OR. NU13 >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU13,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF   
      IF (NU23 < ZERO .OR. NU23 >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU23,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      ! Young modulus
      IF (E22 == ZERO) E22 = E11
      IF (E33 == ZERO) E33 = E22
      ! Shear modulus
      IF (G13 == ZERO) G13 = G12
      IF (G23 == ZERO) G23 = G12
      ! Remaining Poisson's ratio
      NU21 = NU12*E22/E11
      NU31 = NU13*E33/E11  
      NU32 = NU23*E33/E22    
C
      ! Checking Poisson's ratio 
      DETC = ONE - NU12*NU21
      IF (DETC <= ZERO) THEN
        CALL ANCMSG(MSGID=307,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF 
C
      ! Hill coefficient
      A1 = ONE/R11/R11
      A2 = ONE/R22/R22
      A3 = ONE/R33/R33
      FF = HALF*(A2 + A3 - A1)
      GG = HALF*(A3 + A1 - A2)
      HH = HALF*(A1 + A2 - A3)
      LL = THREE_HALF/R23/R23
      MM = THREE_HALF/R13/R13
      NN = THREE_HALF/R12/R12
C
      ! Elasticity matrix for 2D plane stress
      FAC = ONE/(ONE - NU12*NU21)
      A11 = E11*FAC
      A12 = NU21*A11
      A22 = E22*FAC
      ! Compliance matrix for 3D
      C11 = ONE/E11
      C22 = ONE/E22
      C33 = ONE/E33
      C12 =-NU12/E11
      C13 =-NU31/E33
      C23 =-NU23/E22      
      ! Checking input
      DETC= C11*C22*C33-C11*C23*C23-C12*C12*C33+C12*C13*C23
     +     +C13*C12*C23-C13*C22*C13
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
      DMIN = MIN(D11*D22 -D12**2, D11*D33 - D13**2, D22*D33 - D23**2 )      
      DMAX = MAX(D11,D22,D33)
C
      ! Strain-rate filtering
      IF (NFUNC > 1) THEN
        ISRATE = 1
        IF (VP == 0) VP = 2
        IF (VP == 1) THEN
          ASRATE = 1.0D4*FAC_T_WORK
        ELSE
          IF (ASRATE == ZERO) ASRATE = 1.0D4*FAC_T_WORK
        ENDIF  
      ELSE
        ISRATE = 0
        ASRATE = ZERO
      ENDIF
C
      ! PM table
      RHOR   = ZERO
      PM(1)  = RHOR
      PM(89) = RHO0
C
      ! PARMAT table
      PARMAT(1)  = MAX(A11,A22,D11,D22,D33)
      PARMAT(2)  = MAX(E11,E22,E33)
      PARMAT(3)  = MAX(NU12,NU13,NU23)
      PARMAT(4)  = ISRATE
      PARMAT(5)  = ASRATE
      PARMAT(16) = 1
      PARMAT(17) = DMIN/DMAX/DMAX 
C
      ! MTAG variable activation      
      MTAG%G_PLA  = 1 
      MTAG%L_PLA  = 1
      MTAG%G_SEQ  = 1
      MTAG%L_SEQ  = 1
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
C
      ! MATPARAM parameters
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
C
      ! No viscosity
      IMATVIS = 0
C
      ! Number of user variable
      IF ((NRATE > 1).AND.(VP /= 2)) THEN 
        NUVAR = 1
      ELSE
        NUVAR = 0
      ENDIF
      
      ! Number of material parameter
      NUPARAM = 30 + 2*NFUNC
      NVARTMP = NFUNC
c          
      ! Filling the parameter table
      ! -> Elastic parameters      
      UPARAM(1)  = A11    
      UPARAM(2)  = A22    
      UPARAM(3)  = A12    
      UPARAM(4)  = D11    
      UPARAM(5)  = D12    
      UPARAM(6)  = D13    
      UPARAM(7)  = D22    
      UPARAM(8)  = D23    
      UPARAM(9)  = D33    
      UPARAM(10) = G12   
      UPARAM(11) = G13   
      UPARAM(12) = G23
      UPARAM(13) = E11
      UPARAM(14) = E22
      UPARAM(15) = E33
      UPARAM(16) = NU12
      UPARAM(17) = NU13
      UPARAM(18) = NU23
      ! -> Yield criterion parameters
      UPARAM(19) = FF      
      UPARAM(20) = GG     
      UPARAM(21) = HH     
      UPARAM(22) = LL     
      UPARAM(23) = MM     
      UPARAM(24) = NN  
      ! -> Continuous hardening parameters
      UPARAM(25) = SIGY   
      UPARAM(26) = QR1    
      UPARAM(27) = CR1    
      UPARAM(28) = QR2
      UPARAM(29) = CR2
      ! -> Strain-rate computation flag
      UPARAM(30) = VP
      ! -> Tabulated hardening parameters 
      IF (NFUNC > 0) THEN
        DO J=1,NFUNC
          UPARAM(30 + J)         = RATE(J)   
          UPARAM(30 + NFUNC + J) = YFAC(J)  
        ENDDO 
      ENDIF
c
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0
        WRITE(IOUT,1300) E11,E22,E33,G12,G13,G23,NU12,NU13,NU23
        IF (NRATE == 0) THEN
          WRITE(IOUT,1450)
          WRITE(IOUT,1400) SIGY,QR1,CR1,QR2,CR2
        ELSE 
          WRITE(IOUT,1550)
          DO J=1,NFUNC
            WRITE(IOUT,1500) IFUNC(J),YFAC(J),RATE(J)
          ENDDO
          IF (NRATE > 1) THEN
            WRITE(IOUT,1575) ASRATE,VP
          ENDIF
        ENDIF
        WRITE(IOUT,1600) R11,R22,R33,R12,R13,R23
      ENDIF
C-----------------------------------------------------------------
 1000 FORMAT(
     & 5X,'  ORTHOTROPIC ELASTIC + HILL CRITERION   '/,
     & 5X,'  ------------------------------------   '//)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'YOUNG MODULUS IN 11 DIRECTION . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS IN 22 DIRECTION . . . .=',1PG20.13/,
     & 5X,'YOUNG MODULUS IN 33 DIRECTION . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS IN 12 DIRECTION . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS IN 13 DIRECTION . . . .=',1PG20.13/,
     & 5X,'SHEAR MODULUS IN 23 DIRECTION . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO 12. . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO 13. . . . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON RATIO 23. . . . . . . . . . .=',1PG20.13//) 
 1550 FORMAT(
     & 5X,'--------------------------------------'/,
     & 5X,'TABULATED YIELD STRESS                '/,
     & 5X,'--------------------------------------'//)     
 1500 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER. . . . .=',I10/,
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE . . . . . . . . . . . . .=',1PG20.13/)
 1575 FORMAT(
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . .=',1PG20.13/
     & 5X,'STRAIN RATE CHOICE FLAG . . . . . . .=',I10/
     & 5X,'     VP=1  EQUIVALENT PLASTIC STRAIN RATE'/
     & 5X,'     VP=2  TOTAL STRAIN RATE (DEFAULT)'/
     & 5X,'     VP=3  DEVIATORIC STRAIN RATE'/)
 1450 FORMAT(
     & 5X,'--------------------------------------'/,
     & 5X,'CONTINUOUS YIELD STRESS               '/,
     & 5X,'--------------------------------------'//) 
 1400 FORMAT(
     & 5X,'INITIAL YIELD STRESS. . . . . . . . .=',1PG20.13/,
     & 5X,'PARAMETER QR1 OF  HARDENING . . . . .=',1PG20.13/,
     & 5X,'PARAMETER CR1 OF  HARDENING . . . . .=',1PG20.13/,
     & 5X,'PARAMETER QR2 OF  HARDENING . . . . .=',1PG20.13/,
     & 5X,'PARAMETER CR2 OF  HARDENING . . . . .=',1PG20.13/,
     & 5X,'REFERENCE STRAIN. . . . . . . . . . .=',1PG20.13//)
 1600 FORMAT(
     & 5X,'RATIO YIELD PARAMETER R11 . . . . . .=',1PG20.13/,
     & 5X,'RATIO YIELD PARAMETER R22 . . . . . .=',1PG20.13/,
     & 5X,'RATIO YIELD PARAMETER R33 . . . . . .=',1PG20.13/,
     & 5X,'RATIO YIELD PARAMETER R12 . . . . . .=',1PG20.13/,
     & 5X,'RATIO YIELD PARAMETER R13 . . . . . .=',1PG20.13/,
     & 5X,'RATIO YIELD PARAMETER R23 . . . . . .=',1PG20.13/)
      RETURN
      END