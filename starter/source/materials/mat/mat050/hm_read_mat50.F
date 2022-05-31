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
Chd|  HM_READ_MAT50                 source/materials/mat/mat050/hm_read_mat50.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
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
      SUBROUTINE HM_READ_MAT50(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM       ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           MATPARAM )
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
C   READ MAT LAW50 WITH HM READER
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
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)    :: PM     
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      INTEGER, INTENT(INOUT)                       :: ISRATE
      INTEGER, INTENT(OUT)                         :: NUPARAM,NUVAR,NFUNC
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(OUT)   :: IFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(OUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
      INTEGER :: I,ILAW,ICC,ISRAT,N11,N22,N33,N12,N23,N31,IFLAG1,IFLAG2
      INTEGER ,DIMENSION(5) :: I11,I22,I33,I12,I23,I31
      my_real :: RHO0,RHOR,ASRATE,DMIN,DMAX,E11,E22,E33,G12,G23,G31,
     .           EMX11,EMX22,EMX33,EMX12,EMX23,EMX31,PRESS_SCALE
      my_real ,DIMENSION(5) ::  EPSP11,EPSP22,EPSP33,EPSP12,EPSP23,EPSP31,
     .                          FAC11,FAC22,FAC33,FAC12,FAC23,FAC31
C=======================================================================
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW   = 50
      ISRATE = 1
c
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
c
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c      
      CALL HM_GET_FLOATV('MAT_EA'       ,E11       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EB'       ,E22       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EC'       ,E33       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_GAB'      ,G12       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GBC'      ,G23       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GCA'      ,G31       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_asrate'   ,ASRATE    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c    
c     normal direction    
c    
      CALL HM_GET_INTV  ('Gflag'        ,IFLAG1    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EPS_max11',EMX11     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS_max22',EMX22     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS_max33',EMX33     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     direction 11
c
      CALL HM_GET_INTV  ('MAT_YFUN11_1' ,I11(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN11_2' ,I11(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN11_3' ,I11(3)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN11_4' ,I11(4)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN11_5' ,I11(5)    ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SFAC11_1' ,FAC11(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC11_2' ,FAC11(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC11_3' ,FAC11(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC11_4' ,FAC11(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC11_5' ,FAC11(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPS11_1'  ,EPSP11(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS11_2'  ,EPSP11(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS11_3'  ,EPSP11(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS11_4'  ,EPSP11(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS11_5'  ,EPSP11(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     direction 22
c
      CALL HM_GET_INTV  ('MAT_YFUN22_1' ,I22(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN22_2' ,I22(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN22_3' ,I22(3)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN22_4' ,I22(4)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN22_5' ,I22(5)    ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SFAC22_1' ,FAC22(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC22_2' ,FAC22(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC22_3' ,FAC22(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC22_4' ,FAC22(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC22_5' ,FAC22(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPS22_1'  ,EPSP22(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS22_2'  ,EPSP22(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS22_3'  ,EPSP22(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS22_4'  ,EPSP22(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS22_5'  ,EPSP22(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     direction 33
c
      CALL HM_GET_INTV  ('MAT_YFUN33_1' ,I33(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN33_2' ,I33(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN33_3' ,I33(3)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN33_4' ,I33(4)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN33_5' ,I33(5)    ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SFAC33_1' ,FAC33(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC33_2' ,FAC33(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC33_3' ,FAC33(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC33_4' ,FAC33(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC33_5' ,FAC33(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPS33_1'  ,EPSP33(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS33_2'  ,EPSP33(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS33_3'  ,EPSP33(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS33_4'  ,EPSP33(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS33_5'  ,EPSP33(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c    
c     shear
c    
      CALL HM_GET_INTV  ('Vflag'        ,IFLAG2    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_EPS_max12',EMX12     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS_max23',EMX23     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS_max31',EMX31     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     direction 12
c
      CALL HM_GET_INTV  ('MAT_YFUN12_1' ,I12(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN12_2' ,I12(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN12_3' ,I12(3)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN12_4' ,I12(4)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN12_5' ,I12(5)    ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SFAC12_1' ,FAC12(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC12_2' ,FAC12(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC12_3' ,FAC12(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC12_4' ,FAC12(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC12_5' ,FAC12(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPS12_1'  ,EPSP12(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS12_2'  ,EPSP12(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS12_3'  ,EPSP12(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS12_4'  ,EPSP12(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS12_5'  ,EPSP12(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     direction 23
c
      CALL HM_GET_INTV  ('MAT_YFUN23_1' ,I23(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN23_2' ,I23(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN23_3' ,I23(3)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN23_4' ,I23(4)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN23_5' ,I23(5)    ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SFAC23_1' ,FAC23(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC23_2' ,FAC23(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC23_3' ,FAC23(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC23_4' ,FAC23(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC23_5' ,FAC23(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPS23_1'  ,EPSP23(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS23_2'  ,EPSP23(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS23_3'  ,EPSP23(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS23_4'  ,EPSP23(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS23_5'  ,EPSP23(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     direction 31
c
      CALL HM_GET_INTV  ('MAT_YFUN31_1' ,I31(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN31_2' ,I31(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN31_3' ,I31(3)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN31_4' ,I31(4)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_YFUN31_5' ,I31(5)    ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_FLOATV('MAT_SFAC31_1' ,FAC31(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC31_2' ,FAC31(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC31_3' ,FAC31(3)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC31_4' ,FAC31(4)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SFAC31_5' ,FAC31(5)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_EPS31_1'  ,EPSP31(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS31_2'  ,EPSP31(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS31_3'  ,EPSP31(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS31_4'  ,EPSP31(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS31_5'  ,EPSP31(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
c     get pressure unit scale factor
c
      CALL HM_GET_FLOATV_DIM('MAT_EA',PRESS_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Default values
c--------------------------------------------------
      IF (ASRATE == ZERO) ASRATE = EP20
      IF (EMX11  == ZERO) EMX11  = EP10
      IF (EMX22  == ZERO) EMX22  = EP10
      IF (EMX33  == ZERO) EMX33  = EP10
      IF (EMX12  == ZERO) EMX12  = EP10
      IF (EMX23  == ZERO) EMX23  = EP10
      IF (EMX31  == ZERO) EMX31  = EP10  
c
      DO I=1,5
        IF (FAC11(I) == ZERO) FAC11(I) = ONE*PRESS_SCALE
        IF (FAC22(I) == ZERO) FAC22(I) = ONE*PRESS_SCALE
        IF (FAC33(I) == ZERO) FAC33(I) = ONE*PRESS_SCALE
        IF (FAC12(I) == ZERO) FAC12(I) = ONE*PRESS_SCALE
        IF (FAC23(I) == ZERO) FAC23(I) = ONE*PRESS_SCALE
        IF (FAC31(I) == ZERO) FAC31(I) = ONE*PRESS_SCALE
      ENDDO
c
      N11 = 0
      N22 = 0
      N33 = 0
      N12 = 0
      N23 = 0
      N31 = 0
      IF (I11(1) > 0) N11 = 1
      IF (I22(1) > 0) N22 = 1
      IF (I33(1) > 0) N33 = 1
      IF (I12(1) > 0) N12 = 1
      IF (I23(1) > 0) N23 = 1
      IF (I31(1) > 0) N31 = 1
      DO I=2,5
        IF ((I11(I)*EPSP11(I)) > EM20 .and. N11 == I-1) N11=I
        IF ((I22(I)*EPSP22(I)) > EM20 .and. N22 == I-1) N22=I
        IF ((I33(I)*EPSP33(I)) > EM20 .and. N33 == I-1) N33=I
        IF ((I12(I)*EPSP12(I)) > EM20 .and. N12 == I-1) N12=I
        IF ((I23(I)*EPSP23(I)) > EM20 .and. N23 == I-1) N23=I
        IF ((I31(I)*EPSP31(I)) > EM20 .and. N31 == I-1) N31=I
      ENDDO
c
      IF (N11 <= 1 .and. I11(1) /= 0) THEN
         N11=2
         I11(2)=I11(1)
         EPSP11(1)=ZERO
         EPSP11(2)=EP10
      ENDIF
      IF (N22 <= 1 .and. I22(1) /= 0) THEN
         N22=2
         I22(2)=I22(1)
         EPSP22(1)=ZERO
         EPSP22(2)=EP10
      ENDIF
      IF (N33 <= 1 .and. I33(1) /= 0) THEN
         N33=2
         I33(2)=I33(1)
         EPSP33(1)=ZERO
         EPSP33(2)=EP10
      ENDIF
      IF (N12 <= 1 .and. I12(1) /= 0) THEN
         N12=2
         I12(2)=I12(1)
         EPSP12(1)=ZERO
         EPSP12(2)=EP10
      ENDIF
      IF (N23 <= 1 .and. I23(1) /= 0) THEN
         N23=2
         I23(2)=I23(1)
         EPSP23(1)=ZERO
         EPSP23(2)=EP10
      ENDIF
      IF (N31 <= 1 .and. I31(1) /= 0) THEN
         N31=2
         I31(2)=I31(1)
         EPSP31(1)=ZERO
         EPSP31(2)=EP10
      ENDIF
c
c--------------------------------------------------
c
      UPARAM(1)  = E11
      UPARAM(2)  = E22
      UPARAM(3)  = E33
      UPARAM(4)  = G12
      UPARAM(5)  = G23
      UPARAM(6)  = G31
      UPARAM(7)  = IFLAG1
      UPARAM(8)  = IFLAG2
      UPARAM(9)  = EMX11
      UPARAM(10) = EMX22
      UPARAM(11) = EMX33
      UPARAM(12) = EMX12
      UPARAM(13) = EMX23
      UPARAM(14) = EMX31
c
      DO I=1,5
        UPARAM(14+I) = EPSP11(I)
        UPARAM(19+I) = EPSP22(I)
        UPARAM(24+I) = EPSP33(I)
        UPARAM(29+I) = EPSP12(I)
        UPARAM(34+I) = EPSP23(I)
        UPARAM(39+I) = EPSP31(I)
        UPARAM(51+I) = FAC11(I) 
        UPARAM(56+I) = FAC22(I) 
        UPARAM(61+I) = FAC33(I) 
        UPARAM(66+I) = FAC12(I) 
        UPARAM(71+I) = FAC23(I) 
        UPARAM(76+I) = FAC31(I) 
      ENDDO
      UPARAM(45) = N11
      UPARAM(46) = N22
      UPARAM(47) = N33
      UPARAM(48) = N12
      UPARAM(49) = N23
      UPARAM(50) = N31
      UPARAM(51) = ASRATE
c---------------------------
      DO I=1,5
        IFUNC(I)     = I11(I)
        IFUNC(I+5)   = I22(I)
        IFUNC(I+10)  = I33(I)
        IFUNC(I+15)  = I12(I)
        IFUNC(I+20)  = I23(I)
        IFUNC(I+25)  = I31(I)
      ENDDO
c---------------------------
      NFUNC   = 30
      NUPARAM = 81
      NUVAR   = 10
c-----------------
      DMIN       = MIN(E11*E22, E22*E33,E11*E33)
      DMAX       = MAX(E11,E22,E33)
c
      PARMAT(1)  = MAX(E11,E22,E33,G12,G23,G31)
      PARMAT(2)  = ZERO
      PARMAT(3)  = ZERO
      PARMAT(4)  = ZERO
      PARMAT(16) = 1
      PARMAT(17) = DMIN/DMAX/DMAX      
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(100)= PARMAT(1)    
c-----------------
c     Element buffer variable allocation
c-----------------
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
c--------------------------------------------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SMALL_STRAIN")
c--------------------------------------------------
c     Starter output
c--------------------------------------------------
      WRITE(IOUT,1000) TRIM(TITR),MAT_ID,ILAW
      IF (IS_CRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1100)
        WRITE(IOUT,1200) RHO0,RHOR
        WRITE(IOUT,1300) E11,E22,E33,G12,G23,G31,ASRATE
c
        WRITE(IOUT,1001)
        WRITE(IOUT,1600) EPSP11(1),I11(1),FAC11(1)
        IF (I11(2) > 0) WRITE(IOUT,1600) EPSP11(2),I11(2),FAC11(2)
        IF (I11(3) > 0) WRITE(IOUT,1600) EPSP11(3),I11(3),FAC11(3)
        IF (I11(4) > 0) WRITE(IOUT,1600) EPSP11(4),I11(4),FAC11(4)
        IF (I11(5) > 0) WRITE(IOUT,1600) EPSP11(5),I11(5),FAC11(5)
c
        WRITE(IOUT,1002)
        WRITE(IOUT,1600) EPSP22(1),I22(1),FAC22(1)
        IF (I22(2) > 0) WRITE(IOUT,1600) EPSP22(2),I22(2),FAC22(2)
        IF (I22(3) > 0) WRITE(IOUT,1600) EPSP22(3),I22(3),FAC22(3)
        IF (I22(4) > 0) WRITE(IOUT,1600) EPSP22(4),I22(4),FAC22(4)
        IF (I22(5) > 0) WRITE(IOUT,1600) EPSP22(5),I22(5),FAC22(5)
c
        WRITE(IOUT,1003)
        WRITE(IOUT,1600) EPSP33(1),I33(1),FAC33(1)
        IF (I33(2) > 0) WRITE(IOUT,1600) EPSP33(2),I33(2),FAC33(2)
        IF (I33(3) > 0) WRITE(IOUT,1600) EPSP33(3),I33(3),FAC33(3)
        IF (I33(4) > 0) WRITE(IOUT,1600) EPSP33(4),I33(4),FAC33(4)
        IF (I33(5) > 0) WRITE(IOUT,1600) EPSP33(5),I33(5),FAC33(5)
c
        WRITE(IOUT,1004)
        WRITE(IOUT,1600) EPSP12(1),I12(1),FAC12(1)
        IF (I12(2) > 0) WRITE(IOUT,1600) EPSP12(2),I12(2),FAC12(2)
        IF (I12(3) > 0) WRITE(IOUT,1600) EPSP12(3),I12(3),FAC12(3)
        IF (I12(4) > 0) WRITE(IOUT,1600) EPSP12(4),I12(4),FAC12(4)
        IF (I12(5) > 0) WRITE(IOUT,1600) EPSP12(5),I12(5),FAC12(5)
c
        WRITE(IOUT,1005)
        WRITE(IOUT,1600) EPSP23(1),I23(1),FAC23(1)
        IF (I23(2) > 0) WRITE(IOUT,1600) EPSP23(2),I23(2),FAC23(2)
        IF (I23(3) > 0) WRITE(IOUT,1600) EPSP23(3),I23(3),FAC23(3)
        IF (I23(4) > 0) WRITE(IOUT,1600) EPSP23(4),I23(4),FAC23(4)
        IF (I23(5) > 0) WRITE(IOUT,1600) EPSP23(5),I23(5),FAC23(5)
c
        WRITE(IOUT,1006)
        WRITE(IOUT,1600) EPSP31(1),I31(1),FAC31(1)
        IF (I31(2) > 0) WRITE(IOUT,1600) EPSP31(2),I31(2),FAC31(2)
        IF (I31(3) > 0) WRITE(IOUT,1600) EPSP31(3),I31(3),FAC31(3)
        IF (I31(4) > 0) WRITE(IOUT,1600) EPSP31(4),I31(4),FAC31(4)
        IF (I31(5) > 0) WRITE(IOUT,1600) EPSP31(5),I31(5),FAC31(5)
c
        IF (IFLAG1+IFLAG2 /= 0) WRITE(IOUT,1400) IFLAG1,IFLAG2
        IF (EMX11+EMX22+EMX33+EMX12+EMX23+EMX31 /= 0) WRITE(IOUT,1500)
     .   EMX11,EMX22,EMX33,EMX12,EMX23,EMX31
       ENDIF     
c-----------------------------------------------------------------------
 1000 FORMAT(/
     & 5X,A,/,
     & 5X,'   MATERIAL NUMBER. . . . . . . . . . .=',I10/,
     & 5X,'   MATERIAL LAW . . . . . . . . . . . .=',I10/)
 1100 FORMAT
     &(5X,'MATERIAL MODEL : HONEYCOMB (VISC_HONEY) ',/,
     & 5X,'-------------------------------------------------',/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'EOS REFERENCE DENSITY . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT(
     & 5X,'E11 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E22 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'E33 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G12 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G23 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'G31 . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE FILTERING CUT OFF FREQUENCY=',1PG20.13)
 1400 FORMAT(
     & 5X,'YIELD FUNCTION 11,22,33 FLAG .  . . . .=',I10/
     & 5X,'YIELD FUNCTION 12,23,31 FLAG .  . . . .=',I10//)
 1500 FORMAT(
     & 5X,'TENSION FAILURE STRAIN 11 . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 22 . . . . . . .=',1PG20.13/
     & 5X,'TENSION FAILURE STRAIN 33 . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 12 . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 23 . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR FAILURE STRAIN 31 . . . . . . . .=',1PG20.13//)
 1600 FORMAT(
     & 8X,'STRAIN RATE . . . . . . . . . . . . . . .=',1PG20.13/
     & 8X,'  FUNCTION NUMBER . . . . . . . . . . . .=',I10/
     & 8X,'  SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13)
 1001 FORMAT(
     & 5X,'YIELD STRESS 11')
 1002 FORMAT(
     & 5X,'YIELD STRESS 22')
 1003 FORMAT(
     & 5X,'YIELD STRESS 33')
 1004 FORMAT(
     & 5X,'YIELD STRESS 12')
 1005 FORMAT(
     & 5X,'YIELD STRESS 23')
 1006 FORMAT(
     & 5X,'YIELD STRESS 31')
c-----------------------------------------------------------------------
      RETURN
      END
