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
Chd|  HM_READ_MAT107                source/materials/mat/mat107/hm_read_mat107.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT107(
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
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)               :: ISRATE,ITABLE(MAXTABL)
      INTEGER, INTENT(OUT)                 :: NUPARAM,NUVAR,NTABL,NVARTMP
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(OUT) :: MATPARAM
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I, J, ILAW, Ires, Itab, Ismooth
       INTEGER TAB_YLD1,TAB_TEMP1,TAB_YLD2,TAB_TEMP2,TAB_YLD3,TAB_TEMP3,
     .         TAB_YLD4,TAB_TEMP4,TAB_YLD5,TAB_TEMP5,TAB_YLD1C,TAB_TEMP1C,
     .         TAB_YLD2C,TAB_TEMP2C,TAB_YLDT,TAB_TEMPT,TAB_YLDC,TAB_TEMPC,
     .         TAB_YLDS,TAB_TEMPS
C     REAL ou REAL*8
      my_real
     .   RHO0,RHOR,
     .   YOUNG1,YOUNG2,YOUNG3,NU12,NU21,G12,G23,G31,
     .   K1,K2,K3,K4,K5,K6,G1C,D1,D2,SIGY1,
     .   CINI1,S1,SIGY2,CINI2,S2,SIGY1C,CINI1C,
     .   S1C,SIGY2C,CINI2C,S2C,SIGYT,CINIT,ST,
     .   FCUT,C1,SSP,A11,A12,A21,A22,XI1,XI2,
     .   DMIN,DMAX,
     .   K,E3C,CC,NU1P,NU2P,NU4P,NU5P,
     .   S01,A01,B01,C01,S02,A02,B02,C02,
     .   S03,A03,B03,C03,S04,A04,B04,C04,
     .   S05,A05,B05,C05,ASIG,BSIG,CSIG,
     .   TAU0,ATAU,BTAU,
     .   XSCALE1,YSCALE1,XSCALE2,YSCALE2,XSCALE3,YSCALE3,
     .   XSCALE4,YSCALE4,XSCALE5,YSCALE5,XSCALEC,YSCALEC,
     .   XSCALES,YSCALES,XSCALE1C,YSCALE1C,XSCALE2C,YSCALE2C,
     .   XSCALET,YSCALET,XSCALE_UNIT,YSCALE_UNIT,ASRATE,TREF
C     
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW   = 107
c------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)
c------------------------------------------
c
card1 - Density
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2 - Young moduli - Flags
      CALL HM_GET_FLOATV('MAT_E1'    ,YOUNG1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E2'    ,YOUNG2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E3'    ,YOUNG3   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('MAT_IRES'  ,Ires     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_ITAB'  ,Itab     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_SMOOTH',Ismooth  ,IS_AVAILABLE, LSUBMODEL)
      IF (Ismooth == 0) Ismooth = 1
card3 - Coulomb moduli - Poissson's ratio  
      CALL HM_GET_FLOATV('MAT_NU21'  ,NU21     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G12'   ,G12      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G23'   ,G23      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G13'   ,G31      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card5 - Coupling parameter - Plastic potential parameter
      CALL HM_GET_FLOATV('MAT_XI1'  ,XI1      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_XI2'  ,XI2      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_G1C'  ,G1C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_D1'   ,D1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)        
      CALL HM_GET_FLOATV('MAT_D2'   ,D2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)      
card6 - Plastic potential parameter        
      CALL HM_GET_FLOATV('MAT_K1'   ,K1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K2'   ,K2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K3'   ,K3       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card7 - Plastic potential parameter 
      CALL HM_GET_FLOATV('MAT_K4'   ,K4       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K5'   ,K5       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K6'   ,K6       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ! Choice in yield stresses input
      IF (Itab == 0) THEN         
card8 - Yield stress in tension in direction 1 (MD)
        CALL HM_GET_FLOATV('MAT_SIGY1',SIGY1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (SIGY1 == ZERO) SIGY1 = INFINITY
        CALL HM_GET_FLOATV('MAT_CINI1',CINI1    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (CINI1 == ZERO) CINI1 = INFINITY
        CALL HM_GET_FLOATV('MAT_S1'   ,S1       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card9 - Yield stress in tension in direction 2 (CD)
        CALL HM_GET_FLOATV('MAT_SIGY2',SIGY2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (SIGY2 == ZERO) SIGY2 = INFINITY
        CALL HM_GET_FLOATV('MAT_CINI2',CINI2    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (CINI2 == ZERO) CINI2 = INFINITY
        CALL HM_GET_FLOATV('MAT_S2'   ,S2       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card10 - Yield stress in compression in direction 1 (MD)  
        CALL HM_GET_FLOATV('MAT_SIGY1C',SIGY1C   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (SIGY1C == ZERO) SIGY1C = INFINITY
        CALL HM_GET_FLOATV('MAT_CINI1C',CINI1C   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (CINI1C == ZERO) CINI1C = INFINITY
        CALL HM_GET_FLOATV('MAT_S1C'   ,S1C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card11 - Yield stress in compression in direction 2 (CD)       
        CALL HM_GET_FLOATV('MAT_SIGY2C',SIGY2C   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (SIGY2C == ZERO) SIGY2C = INFINITY
        CALL HM_GET_FLOATV('MAT_CINI2C',CINI2C   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (CINI2C == ZERO) CINI2C = INFINITY
        CALL HM_GET_FLOATV('MAT_S2C'   ,S2C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card12 - Yield stress in shear       
        CALL HM_GET_FLOATV('MAT_SIGYT',SIGYT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (SIGYT == ZERO) SIGYT = INFINITY
        CALL HM_GET_FLOATV('MAT_CINIT',CINIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (CINIT == ZERO) CINIT = INFINITY
        CALL HM_GET_FLOATV('MAT_ST'   ,ST       ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      ELSE
card8 - Yield stress in tension in direction 1 (MD)        
        CALL HM_GET_INTV  ('TAB_YLD1'    ,TAB_YLD1  ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('MAT_Xscale1' ,XSCALE1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE1 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale1' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE1 = ONE * XSCALE_UNIT
        ENDIF
        CALL HM_GET_FLOATV('MAT_Yscale1' ,YSCALE1   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE1 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale1' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE1 = ONE * YSCALE_UNIT
        ENDIF 
card9 - Yield stress in tension in direction 2 (CD)          
        CALL HM_GET_INTV  ('TAB_YLD2'    ,TAB_YLD2  ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('MAT_Xscale2' ,XSCALE2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE2 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale2' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE2 = ONE * XSCALE_UNIT
        ENDIF
        CALL HM_GET_FLOATV('MAT_Yscale2' ,YSCALE2   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE2 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale2' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE2 = ONE * YSCALE_UNIT
        ENDIF 
card10 - Yield stress in compression in direction 1 (MD)
        CALL HM_GET_INTV  ('TAB_YLD1C'   ,TAB_YLD1C ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('MAT_Xscale1C',XSCALE1C  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE1C == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale1C' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE1C = ONE * XSCALE_UNIT
        ENDIF
        CALL HM_GET_FLOATV('MAT_Yscale1C',YSCALE1C  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE1C == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale1C' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE1C = ONE * YSCALE_UNIT
        ENDIF 
card11 - Yield stress in compression in direction 2 (CD)       
        CALL HM_GET_INTV  ('TAB_YLD2C'   ,TAB_YLD2C ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('MAT_Xscale2C',XSCALE2C  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE2C == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale2C' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE2C = ONE * XSCALE_UNIT
        ENDIF
        CALL HM_GET_FLOATV('MAT_Yscale2C',YSCALE2C  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE2C == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale2C' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE2C = ONE * YSCALE_UNIT
        ENDIF 
card12 - Yield stress in shear       
        CALL HM_GET_INTV  ('TAB_YLDT'    ,TAB_YLDT  ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('MAT_XscaleT' ,XSCALET   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALET == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_XscaleT' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALET = ONE * XSCALE_UNIT
        ENDIF
        CALL HM_GET_FLOATV('MAT_YscaleT' ,YSCALET   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALET == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_YscaleT' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALET = ONE * YSCALE_UNIT
        ENDIF
      ENDIF
c---------------------
c     Default values
c---------------------
      ! Default algorithm : Cutting plane
      IF (Ires == 0)  Ires = 2
      ! Poisson's ratio
      IF (NU21 < ZERO .OR. NU21 >= HALF) THEN
         CALL ANCMSG(MSGID=49,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU21,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      NU12 = NU21*YOUNG1/YOUNG2
      ! Restriction for positive plastic work
      IF (K1<ZERO) THEN 
         CALL ANCMSG(MSGID=1815,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)        
      ENDIF
      IF (K2>ZERO) THEN 
         CALL ANCMSG(MSGID=1816,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
      IF (K3<ZERO) THEN 
         CALL ANCMSG(MSGID=1817,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
      IF (K4<ZERO) THEN 
         CALL ANCMSG(MSGID=1818,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
      IF (K5<ZERO) THEN 
         CALL ANCMSG(MSGID=1819,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
      IF (K6<ZERO) THEN 
         CALL ANCMSG(MSGID=1820,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
      IF (ABS(K1)<=ABS(K2)) THEN
         CALL ANCMSG(MSGID=1821,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
      IF (ABS(K2)<=ABS(K4)) THEN
        CALL ANCMSG(MSGID=1822,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               C1=TITR)  
      ENDIF
c
      ! Strain-rate and filtering
      ISRATE    = 0
      PARMAT(4) = ZERO
      PARMAT(5) = ZERO
      IF (ITAB > 0) ASRATE = 10000.0D0*FAC_T_WORK
c
      ! Elasticity parameter
      !  Plane stress elastic constant
      A11 = YOUNG1/(ONE - NU12*NU21)
      A12 = NU12*YOUNG2/(ONE - NU12*NU21)
      A21 = NU21*YOUNG1/(ONE - NU12*NU21)
      A22 = YOUNG2/(ONE - NU12*NU21)
c
c--------------------------
c     Filling buffer tables
c-------------------------- 
      ! Number of material parameter
      IF (Itab == 0) THEN
        NUPARAM = 40
        NTABL   = 0
        NVARTMP = 0
      ELSE
        NUPARAM   = 35
        NTABL     = 5
        NVARTMP   = 5
        ITABLE(1) = TAB_YLD1     ! Yield function table              = f(epsp,epsdot)
        ITABLE(2) = TAB_YLD2     ! Yield function table              = f(epsp,epsdot)
        ITABLE(3) = TAB_YLD1C    ! Yield function table              = f(epsp,epsdot)
        ITABLE(4) = TAB_YLD2C    ! Yield function table              = f(epsp,epsdot)
        ITABLE(5) = TAB_YLDT     ! Yield function table              = f(epsp,epsdot)   
      ENDIF
      ! Number of user variable      
      IF (Ires == 1) THEN 
        NUVAR = 1
      ELSE
        NUVAR = 0
      ENDIF
c          
      ! Filling the parameter table
      UPARAM(1)  = YOUNG1
      UPARAM(2)  = YOUNG2
      UPARAM(3)  = YOUNG3
      UPARAM(4)  = NU12
      UPARAM(5)  = NU21
      UPARAM(6)  = A11
      UPARAM(7)  = A12
      UPARAM(8)  = A21
      UPARAM(9)  = A22
      UPARAM(10) = G12
      UPARAM(11) = G23
      UPARAM(12) = G31
      UPARAM(13) = Ires
      UPARAM(14) = Itab
      UPARAM(15) = XI1
      UPARAM(16) = XI2
      UPARAM(17) = K1
      UPARAM(18) = K2
      UPARAM(19) = K3
      UPARAM(20) = K4
      UPARAM(21) = K5
      UPARAM(22) = K6
      UPARAM(23) = G1C
      IF (Itab == 0) THEN 
        UPARAM(24) = D1
        UPARAM(25) = D2
        UPARAM(26) = SIGY1
        UPARAM(27) = CINI1
        UPARAM(28) = S1
        UPARAM(29) = SIGY2
        UPARAM(30) = CINI2
        UPARAM(31) = S2
        UPARAM(32) = SIGY1C
        UPARAM(33) = CINI1C
        UPARAM(34) = S1C 
        UPARAM(35) = SIGY2C
        UPARAM(36) = CINI2C
        UPARAM(37) = S2C 
        UPARAM(38) = SIGYT
        UPARAM(39) = CINIT
        UPARAM(40) = ST 
      ELSE
        IF (XSCALE1 /= ZERO) THEN
          UPARAM(24) = ONE/XSCALE1
        ELSE
          UPARAM(24) = ZERO
        ENDIF
        UPARAM(25) = YSCALE1
        IF (XSCALE2 /= ZERO) THEN
          UPARAM(26) = ONE/XSCALE2
        ELSE
          UPARAM(26) = ZERO
        ENDIF
        UPARAM(27) = YSCALE2      
        IF (XSCALE1C /= ZERO) THEN
          UPARAM(28) = ONE/XSCALE1C
        ELSE
          UPARAM(28) = ZERO
        ENDIF
        UPARAM(29) = YSCALE1C
        IF (XSCALE2C /= ZERO) THEN
          UPARAM(30) = ONE/XSCALE2C
        ELSE
          UPARAM(30) = ZERO
        ENDIF
        UPARAM(31) = YSCALE2C
        IF (XSCALET /= ZERO) THEN
          UPARAM(32) = ONE/XSCALET
        ELSE
          UPARAM(32) = ZERO
        ENDIF
        UPARAM(33) = YSCALET
        UPARAM(34) = ASRATE
        UPARAM(35) = Ismooth        
      ENDIF
c      
      ! PARMAT table
      PARMAT(1)  = MAX(YOUNG1,YOUNG2,YOUNG3,G12,G23,G31)
      PARMAT(2)  = MAX(YOUNG1,YOUNG2,YOUNG3)
      PARMAT(3)  = MAX(NU12,NU21)
      PARMAT(16) = 1
      DMIN       = YOUNG1*YOUNG2   
      DMAX       = MAX(YOUNG1,YOUNG2)
      PARMAT(17) = DMIN/DMAX/DMAX
c
      ! PM table
      PM(1)  = RHOR
      PM(89) = RHO0
      SSP    = SQRT(MAX(A11,A12,A22,A21,YOUNG3,G12,G23,G31)/ RHO0)
      PM(27) = SSP
c      
      ! MTAG variable activation
      MTAG%G_PLA  = 6
      MTAG%L_PLA  = 6    
      MTAG%G_EPSD = 6
      MTAG%L_EPSD = 6
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
c
c--------------------------
c     Parameters printout
c-------------------------- 
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      WRITE(IOUT,1100)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,RHOR
        WRITE(IOUT,1300) YOUNG1,YOUNG2,YOUNG3,NU12,NU21,G12,G23,G31
        WRITE(IOUT,1350) Ires
        WRITE(IOUT,1450) XI1,XI2,K1,K2,K3,K4,K5,K6,G1C,D1,D2
        IF (Itab == 0) THEN
          WRITE(IOUT,1550) SIGY1,CINI1,S1,SIGY2,CINI2,S2,SIGY1C,CINI1C,S1C,
     .                     SIGY2C,CINI2C,S2C,SIGYT,CINIT,ST
        ELSE
          WRITE(IOUT,1575) TAB_YLD1,XSCALE1,YSCALE1,TAB_YLD2,XSCALE2,YSCALE2,
     .                     TAB_YLD1C,XSCALE1C,YSCALE1C,TAB_YLD2C,XSCALE2C,YSCALE2C,
     .                     TAB_YLDT,XSCALET,YSCALET,Ismooth      
        ENDIF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : PAPERBOARD LIGHT (PFEIFFER,2019)',/,
     & 5X,'-------------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',1PG20.13/) 
 1300 FORMAT(
     & 5X,'YOUNG MODULUS IN DIRECTION 1. . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS IN DIRECTION 2. . . . . .=',1PG20.13/
     & 5X,'YOUNG MODULUS IN DIRECTION 3. . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO NU12  . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON RATIO NU21  . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS IN 12 . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS IN 23 . . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR MODULUS IN 13 . . . . . . . . . .=',1PG20.13/)
 1350 FORMAT(
     & 5X,'RETURN MAPPING ALGORITHM FLAG . . . . .=',I3/
     & 5X,'  IRES=1  NICE EXPLICIT'/
     & 5X,'  IRES=2  NEWTON-ITERATION IMPLICIT (CUTTING PLANE)'/)
 1450 FORMAT(
     & 5X,'XI1 COUPLING PARAMETER IN DIRECTION 1 .=',1PG20.13/
     & 5X,'XI2 COUPLING PARAMETER IN DIRECTION 2 .=',1PG20.13/
     & 5X,'PLASTIC POTENTIAL PARAMETER K1  . . . .=',1PG20.13/
     & 5X,'PLASTIC POTENTIAL PARAMETER K2  . . . .=',1PG20.13/
     & 5X,'PLASTIC POTENTIAL PARAMETER K3  . . . .=',1PG20.13/
     & 5X,'PLASTIC POTENTIAL PARAMETER K4  . . . .=',1PG20.13/
     & 5X,'PLASTIC POTENTIAL PARAMETER K5  . . . .=',1PG20.13/
     & 5X,'PLASTIC POTENTIAL PARAMETER K6  . . . .=',1PG20.13/
     & 5X,'G1C CORRECTION PARAMETER FOR R1C. . . .=',1PG20.13/
     & 5X,'D1 SHEAR YIELD STRESS PARAMETER . . . .=',1PG20.13/
     & 5X,'D2 SHEAR YIELD STRESS PARAMETER . . . .=',1PG20.13/)  
 1550 FORMAT(
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN TENSION IN DIRECTION 1 (MD)    ',/,
     & 5X,'-----------------------------------------------',/,
     & 5X,'INITIAL YIELD STRESS SIGY1  . . . . . .=',1PG20.13/
     & 5X,'VALUE AT ORDINATE AXIS CINI1. . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS SLOPE S1 . . . . . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN TENSION IN DIRECTION 2 (CD)    ',/,
     & 5X,'-----------------------------------------------',/,     
     & 5X,'INITIAL YIELD STRESS SIGY2  . . . . . .=',1PG20.13/
     & 5X,'VALUE AT ORDINATE AXIS CINI2. . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS SLOPE S2 . . . . . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN COMPRESSION IN DIRECTION 1 (MD)',/,
     & 5X,'-----------------------------------------------',/,  
     & 5X,'INITIAL YIELD STRESS SIGY1C . . . . . .=',1PG20.13/
     & 5X,'VALUE AT ORDINATE AXIS CINI1C . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS SLOPE S1C  . . . . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN COMPRESSION IN DIRECTION 2 (CD)',/,
     & 5X,'-----------------------------------------------',/,  
     & 5X,'INITIAL YIELD STRESS SIGY2C . . . . . .=',1PG20.13/
     & 5X,'VALUE AT ORDINATE AXIS CINI2C . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS SLOPE S2C  . . . . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN SHEAR                          ',/,
     & 5X,'-----------------------------------------------',/,     
     & 5X,'INITIAL YIELD STRESS SIGYT  . . . . . .=',1PG20.13/
     & 5X,'VALUE AT ORDINATE AXIS CINIT  . . . . .=',1PG20.13/
     & 5X,'YIELD STRESS SLOPE ST . . . . . . . . .=',1PG20.13/)
 1575 FORMAT(
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD STRESS IN TENSION IN DIRECTION 1 (MD)    ',/,
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD STRESS IN TENSION IN DIRECTION 2 (CD)    ',/,
     & 5X,'---------------------------------------------------------',/,     
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD STRESS IN COMPRESSION IN DIRECTION 1 (MD)',/,
     & 5X,'---------------------------------------------------------',/,  
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD STRESS IN COMPRESSION IN DIRECTION 2 (CD)',/,
     & 5X,'---------------------------------------------------------',/,  
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD STRESS IN SHEAR                          ',/,
     & 5X,'---------------------------------------------------------',/,     
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'---------------------------------------------------------',/,
     & 5X,'DATA FOR STRAIN-RATE COMPUTATION                         ',/,
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABLE INTERPOLATION FLAG  . . . . . . .=',I10/ 
     & 5X,'     ISMOOTH=1  LINEAR INTERPOLATION'/
     & 5X,'     ISMOOTH=2  LOGARITHMIC INTERPOLATION BASE 10'/
     & 5X,'     ISMOOTH=3  LOGARITHMIC INTERPOLATION BASE N'/)
c-----------------------------------------------------------------------
      END