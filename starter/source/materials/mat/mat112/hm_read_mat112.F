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
Chd|  HM_READ_MAT112                source/materials/mat/mat112/hm_read_mat112.F
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
      SUBROUTINE HM_READ_MAT112(
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
#include      "sysunit.inc"
#include      "param_c.inc"
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)                 :: UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID,MAXUPARAM,MAXTABL
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE,ITABLE(MAXTABL)
      INTEGER, INTENT(INOUT)                         :: NUPARAM,NUVAR,NTABL,NVARTMP
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT)        :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I, J, ILAW, Ires, Itab, Ismooth
       INTEGER TAB_YLD1,TAB_YLD2,TAB_YLD3,TAB_YLD4,TAB_YLD5,
     .         TAB_YLDC,TAB_YLDS
C     REAL
      my_real
     .   RHO0,RHOR,
     .   YOUNG1,YOUNG2,YOUNG3,NU12,NU21,G12,G23,G31,
     .   FCUT,C1,SSP,A11,A12,A21,A22,XI1,XI2,
     .   DMIN,DMAX,K,E3C,CC,NU1P,NU2P,NU4P,NU5P,
     .   S01,A01,B01,C01,S02,A02,B02,C02,
     .   S03,A03,B03,C03,S04,A04,B04,C04,
     .   S05,A05,B05,C05,ASIG,BSIG,CSIG,
     .   TAU0,ATAU,BTAU,DETC,
     .   XSCALE1,YSCALE1,XSCALE2,YSCALE2,XSCALE3,YSCALE3,
     .   XSCALE4,YSCALE4,XSCALE5,YSCALE5,XSCALEC,YSCALEC,
     .   XSCALES,YSCALES,XSCALE_UNIT,YSCALE_UNIT,ASRATE,TREF
C     
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW         = 112
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
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
c
card5 - Yield criterion and compressive parameter
      CALL HM_GET_FLOATV('MAT_K'    ,K        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (K == ZERO)   K = ONE
      CALL HM_GET_FLOATV('MAT_E3C'  ,E3C      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (E3C == ZERO) E3C = YOUNG3
      CALL HM_GET_FLOATV('MAT_CC'   ,CC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (CC == ZERO)  CC = ONE
card6 - Plastic potential parameter       
      CALL HM_GET_FLOATV('MAT_NU1P' ,NU1P     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU2P' ,NU2P     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU4P' ,NU4P     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU5P' ,NU5P     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ! Choice in yield stresses input
      IF (Itab == 0) THEN 
card7 - Yield stress in tension in direction 1 (MD)
        CALL HM_GET_FLOATV('MAT_S01'  ,S01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (S01 == ZERO) S01 = INFINITY
        CALL HM_GET_FLOATV('MAT_A01'  ,A01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_B01'  ,B01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_C01'  ,C01      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card8 - Yield stress in compression in direction 1 (MD)  
        CALL HM_GET_FLOATV('MAT_S02'  ,S02      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (S02 == ZERO) S02 = INFINITY
        CALL HM_GET_FLOATV('MAT_A02'  ,A02      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_B02'  ,B02      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_C02'  ,C02      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card9 - Yield stress in positive shear
        CALL HM_GET_FLOATV('MAT_S03'  ,S03      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (S03 == ZERO) S03 = INFINITY
        CALL HM_GET_FLOATV('MAT_A03'  ,A03      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_B03'  ,B03      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_C03'  ,C03      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card10 - Yield stress in tension in direction 2 (CD)    
        CALL HM_GET_FLOATV('MAT_S04'  ,S04      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (S04 == ZERO) S04 = INFINITY
        CALL HM_GET_FLOATV('MAT_A04'  ,A04      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_B04'  ,B04      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_C04'  ,C04      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card11 - Yield stress in compression in direction 2 (CD)   
        CALL HM_GET_FLOATV('MAT_S05'  ,S05      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (S05 == ZERO) S05 = INFINITY
        CALL HM_GET_FLOATV('MAT_A05'  ,A05      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_B05'  ,B05      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_C05'  ,C05      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card12 - Yield stress in out-of-plane direction 
        CALL HM_GET_FLOATV('MAT_ASIG' ,ASIG     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (ASIG == ZERO) ASIG = INFINITY
        CALL HM_GET_FLOATV('MAT_BSIG' ,BSIG     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_CSIG' ,CSIG     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card13 - Yield stress in transverse shear       
        CALL HM_GET_FLOATV('MAT_TAU0' ,TAU0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (TAU0 == ZERO) TAU0 = INFINITY
        CALL HM_GET_FLOATV('MAT_ATAU' ,ATAU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('MAT_BTAU' ,BTAU     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ELSE
card7 - Yield stress in tension in direction 1 (MD)        
        CALL HM_GET_INTV  ('TAB_YLD1'     ,TAB_YLD1  ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLD1 == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=1)
        ENDIF
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
card8 - Yield stress in compression in direction 1 (MD)
        CALL HM_GET_INTV  ('TAB_YLD2'     ,TAB_YLD2  ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLD2 == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=2)
        ENDIF
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
card9 - Yield stress in positive shear
        CALL HM_GET_INTV  ('TAB_YLD3'     ,TAB_YLD3  ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLD3 == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=3)
        ENDIF
        CALL HM_GET_FLOATV('MAT_Xscale3' ,XSCALE3   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE3 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale3' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE3 = ONE * XSCALE_UNIT
        ENDIF 
        CALL HM_GET_FLOATV('MAT_Yscale3' ,YSCALE3   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE3 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale3' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE3 = ONE * YSCALE_UNIT
        ENDIF
card10 - Yield stress in tension in direction 2 (CD) 
        CALL HM_GET_INTV  ('TAB_YLD4'     ,TAB_YLD4  ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLD4 == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=4)
        ENDIF
        CALL HM_GET_FLOATV('MAT_Xscale4' ,XSCALE4   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE4 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale4' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE4 = ONE * XSCALE_UNIT
        ENDIF 
        CALL HM_GET_FLOATV('MAT_Yscale4' ,YSCALE4   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE4 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale4' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE4 = ONE * YSCALE_UNIT
        ENDIF
card11 - Yield stress in compression in direction 2 (CD)  
        CALL HM_GET_INTV  ('TAB_YLD5'     ,TAB_YLD5 ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLD5 == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=5)
        ENDIF
        CALL HM_GET_FLOATV('MAT_Xscale5' ,XSCALE5   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALE5 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Xscale5' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALE5 = ONE * XSCALE_UNIT
        ENDIF 
        CALL HM_GET_FLOATV('MAT_Yscale5' ,YSCALE5   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALE5 == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_Yscale5' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALE5 = ONE * YSCALE_UNIT
        ENDIF
card12 - Yield stress in out-of-plane direction
        CALL HM_GET_INTV  ('TAB_YLDC'     ,TAB_YLDC  ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLDC == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=6)
        ENDIF
        CALL HM_GET_FLOATV('MAT_XscaleC' ,XSCALEC   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALEC == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_XscaleC' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALEC = ONE * XSCALE_UNIT
        ENDIF 
        CALL HM_GET_FLOATV('MAT_YscaleC' ,YSCALEC   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALEC == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_YscaleC' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALEC = ONE * YSCALE_UNIT
        ENDIF 
card12 - Yield stress in out-of-plane direction
        CALL HM_GET_INTV  ('TAB_YLDS'     ,TAB_YLDS  ,IS_AVAILABLE, LSUBMODEL)
        IF (TAB_YLDS == 0) THEN 
          CALL ANCMSG(MSGID=2056,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=7)
        ENDIF
        CALL HM_GET_FLOATV('MAT_XscaleS' ,XSCALES   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (XSCALES == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_XscaleS' ,XSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          XSCALES = ONE * XSCALE_UNIT
        ENDIF
        CALL HM_GET_FLOATV('MAT_YscaleS' ,YSCALES   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        IF (YSCALES == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('MAT_YscaleS' ,YSCALE_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          YSCALES = ONE * YSCALE_UNIT
        ENDIF
      ENDIF
c---------------------
c     Default values
c---------------------
      ! Default algorithm : Cutting plane
      IF (Ires == 0)  Ires = 2
      ! Poisson's ratio
      IF (NU21 < ZERO .OR. NU21 >= HALF) THEN
         CALL ANCMSG(MSGID=2057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU21,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
      NU12 = NU21*YOUNG1/YOUNG2
      IF (NU12 < ZERO .OR. NU12 >= HALF) THEN
         CALL ANCMSG(MSGID=2058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               R1=NU12,
     .               I1=MAT_ID,
     .               C1=TITR)
      ENDIF
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
        NUPARAM   = 47
        NTABL     = 0
        NVARTMP   = 0
      ELSE
        NUPARAM   = 37
        NTABL     = 7
        NVARTMP   = 7
        ITABLE(1) = TAB_YLD1     ! Yield function table              = f(epsp,epsdot,T)
        ITABLE(2) = TAB_YLD2     ! Yield function table              = f(epsp,epsdot)
        ITABLE(3) = TAB_YLD3     ! Yield function table              = f(epsp,epsdot)
        ITABLE(4) = TAB_YLD4     ! Yield function table              = f(epsp,epsdot)
        ITABLE(5) = TAB_YLD5     ! Yield function table              = f(epsp,epsdot)
        ITABLE(6) = TAB_YLDC     ! Yield function table              = f(epsp,epsdot)
        ITABLE(7) = TAB_YLDS     ! Yield function table              = f(epsp,epsdot)
      ENDIF
      ! Number of user variable      
      IF (Ires == 1) THEN 
        NUVAR = 3
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
      UPARAM(15) = TWO*K
      UPARAM(16) = E3C
      UPARAM(17) = CC
      UPARAM(18) = NU1P
      UPARAM(19) = NU2P
      UPARAM(20) = NU4P
      UPARAM(21) = NU5P
      IF (Itab == 0) THEN
        UPARAM(22) = S01
        UPARAM(23) = A01
        UPARAM(24) = B01
        UPARAM(25) = C01
        UPARAM(26) = S02
        UPARAM(27) = A02
        UPARAM(28) = B02
        UPARAM(29) = C02
        UPARAM(30) = S03
        UPARAM(31) = A03
        UPARAM(32) = B03
        UPARAM(33) = C03
        UPARAM(34) = S04
        UPARAM(35) = A04
        UPARAM(36) = B04
        UPARAM(37) = C04
        UPARAM(38) = S05
        UPARAM(39) = A05
        UPARAM(40) = B05 
        UPARAM(41) = C05
        UPARAM(42) = ASIG
        UPARAM(43) = BSIG
        UPARAM(44) = CSIG        
        UPARAM(45) = TAU0
        UPARAM(46) = ATAU
        UPARAM(47) = BTAU
      ELSE
        IF (XSCALE1 /= ZERO) THEN
          UPARAM(22) = ONE/XSCALE1
        ELSE
          UPARAM(22) = ZERO
        ENDIF
        UPARAM(23) = YSCALE1
        IF (XSCALE2 /= ZERO) THEN
          UPARAM(24) = ONE/XSCALE2
        ELSE
          UPARAM(24) = ZERO
        ENDIF
        UPARAM(25) = YSCALE2      
        IF (XSCALE3 /= ZERO) THEN
          UPARAM(26) = ONE/XSCALE3
        ELSE
          UPARAM(26) = ZERO
        ENDIF
        UPARAM(27) = YSCALE3
        IF (XSCALE4 /= ZERO) THEN
          UPARAM(28) = ONE/XSCALE4
        ELSE
          UPARAM(28) = ZERO
        ENDIF
        UPARAM(29) = YSCALE4
        IF (XSCALE5 /= ZERO) THEN
          UPARAM(30) = ONE/XSCALE5
        ELSE
          UPARAM(30) = ZERO
        ENDIF
        UPARAM(31) = YSCALE5
        IF (XSCALEC /= ZERO) THEN
          UPARAM(32) = ONE/XSCALEC
        ELSE
          UPARAM(32) = ZERO
        ENDIF
        UPARAM(33) = YSCALEC
        IF (XSCALES /= ZERO) THEN
          UPARAM(34) = ONE/XSCALES
        ELSE
          UPARAM(34) = ZERO
        ENDIF
        UPARAM(35) = YSCALES
        UPARAM(36) = ASRATE
        UPARAM(37) = Ismooth
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
      MTAG%G_PLA  = 4
      MTAG%L_PLA  = 4
      MTAG%G_EPSD = 4
      MTAG%L_EPSD = 4
c
      CALL INIT_MAT_KEYWORD(MATPARAM ,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM ,"INCREMENTAL"   )
      CALL INIT_MAT_KEYWORD(MATPARAM ,"LARGE_STRAIN"  )
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
        WRITE(IOUT,1300) YOUNG1,YOUNG2,YOUNG3,NU12,NU21,G12,G23,G31
        WRITE(IOUT,1350) Ires
        WRITE(IOUT,1400) K,E3C,CC,NU1P,NU2P,NU4P,NU5P
        IF (Itab == 0) THEN
          WRITE(IOUT,1500) S01,A01,B01,C01,S02,A02,B02,C02,S03,A03,B03,C03,
     .                     S04,A04,B04,C04,S05,A05,B05,C05,ASIG,BSIG,CSIG,
     .                     TAU0,ATAU,BTAU
        ELSE
          WRITE(IOUT,1525) TAB_YLD1,XSCALE1,YSCALE1,TAB_YLD2,XSCALE2,YSCALE2,
     .                     TAB_YLD3,XSCALE3,YSCALE3,TAB_YLD4,XSCALE4,YSCALE4,
     .                     TAB_YLD5,XSCALE5,YSCALE5,TAB_YLDC,XSCALEC,YSCALEC,
     .                     TAB_YLDS,XSCALES,YSCALES,Ismooth
        ENDIF
      ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1050 FORMAT
     &(5X,'MATERIAL MODEL : PAPERBOARD (XIA,2002)',/,
     & 5X,'--------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/) 
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
 1400 FORMAT(
     & 5X,'YIELD CRITERION EXPONENT K  . . . . . .=',1PG20.13/
     & 5X,'ELASTIC COMPRESSION PARAMETER . . . . .=',1PG20.13/
     & 5X,'ELASTIC COMPRESSION EXPONENT  . . . . .=',1PG20.13/
     & 5X,'TENSILE PLASTIC POISSON RATIO IN DIR 1.=',1PG20.13/
     & 5X,'COMPRES PLASTIC POISSON RATIO IN DIR 1.=',1PG20.13/
     & 5X,'TENSILE PLASTIC POISSON RATIO IN DIR 2.=',1PG20.13/
     & 5X,'COMPRES PLASTIC POISSON RATIO IN DIR 2.=',1PG20.13/)
 1500 FORMAT(
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN TENSION IN DIRECTION 1 (MD)    ',/,
     & 5X,'-----------------------------------------------',/,
     & 5X,'1ST PLASTICITY PARAMETER S01  . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER A01  . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER B01  . . . . .=',1PG20.13/
     & 5X,'4TH PLASTICITY PARAMETER C01  . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN TENSION IN DIRECTION 2 (CD)    ',/,
     & 5X,'-----------------------------------------------',/,     
     & 5X,'1ST PLASTICITY PARAMETER S02  . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER A02  . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER B02  . . . . .=',1PG20.13/
     & 5X,'4TH PLASTICITY PARAMETER C02  . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN POSITIVE SHEAR                 ',/,
     & 5X,'-----------------------------------------------',/,  
     & 5X,'1ST PLASTICITY PARAMETER S03  . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER A03  . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER B03  . . . . .=',1PG20.13/
     & 5X,'4TH PLASTICITY PARAMETER C03  . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN COMPRESSION IN DIRECTION 1 (MD)',/,
     & 5X,'-----------------------------------------------',/,
     & 5X,'1ST PLASTICITY PARAMETER S04  . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER A04  . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER B04  . . . . .=',1PG20.13/
     & 5X,'4TH PLASTICITY PARAMETER C04  . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN COMPRESSION IN DIRECTION 2 (CD)',/,
     & 5X,'-----------------------------------------------',/,  
     & 5X,'1ST PLASTICITY PARAMETER S05  . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER A05  . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER B05  . . . . .=',1PG20.13/
     & 5X,'4TH PLASTICITY PARAMETER C05  . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN OUT-OF-PLANE DIRECTION         ',/,
     & 5X,'-----------------------------------------------',/,     
     & 5X,'1ST PLASTICITY PARAMETER ASIG . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER BSIG . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER CSIG . . . . .=',1PG20.13/
     & 5X,'-----------------------------------------------',/,
     & 5X,'YIELD STRESS IN TRANSVERSE SHEAR               ',/,
     & 5X,'-----------------------------------------------',/,     
     & 5X,'1ST PLASTICITY PARAMETER TAU0 . . . . .=',1PG20.13/
     & 5X,'2ND PLASTICITY PARAMETER ATAU . . . . .=',1PG20.13/
     & 5X,'3RD PLASTICITY PARAMETER BTAU . . . . .=',1PG20.13/)
 1525 FORMAT(
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
     & 5X,'TABULATED YIELD STRESS IN POSITIVE SHEAR                 ',/,
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
     & 5X,'TABULATED YIELD STRESS IN OUT-OF-PLANE DIRECTION         ',/,
     & 5X,'---------------------------------------------------------',/,     
     & 5X,'TABULATED YIELD - STRAIN RATE TABLE ID.=',I10/ 
     & 5X,'TABULATED YIELD X FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'TABULATED YIELD Y FACTOR  . . . . . . .=',1PG20.13/
     & 5X,'---------------------------------------------------------',/,
     & 5X,'TABULATED YIELD STRESS IN TRANSVERSE SHEAR               ',/,
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