Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    hm_read_mat21            ../starter/source/materials/mat/mat021/hm_read_mat21.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT21(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM  ,PM,
     .           MAT_ID    ,TITR     ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW21 WITH HM READER
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
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: MAT_ID
      INTEGER, DIMENSION(NPROPMI) ,INTENT(INOUT)   :: IPM
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)             :: TITR
      my_real, DIMENSION(NPROPM)  ,INTENT(INOUT)   :: PM
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT)               :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT)        :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      INTEGER :: I,ILAW,IFUNC
      my_real :: RHO0,RHOR,E,NU,A0,A1,A2,AMX,C1,FAC_Y,PMIN,PEXT,BUNL,XMUMX,E0,G,PSTAR,DELTA,FSCAL_UNIT
      CHARACTER*64 :: chain      
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW   = 21
      PSTAR  = -INFINITY  
!---
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
!
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_E'        ,E          ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,NU         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A0'       ,A0         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A1'       ,A1         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_A2'       ,A2         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_AMAX'     ,AMX        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A1'       ,IFUNC      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_BULK'     ,C1         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PFscale'      ,FAC_Y      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PC'       ,PMIN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('PEXT'         ,PEXT       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_K_UNLOAD' ,BUNL       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_SIG'      ,XMUMX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('PFscale'  ,FSCAL_UNIT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!---
      IF (RHOR==ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0
!
      E0=ZERO
      IF(PMIN==ZERO) PMIN  =-INFINITY

      IF(BUNL==ZERO) BUNL  = C1      
      IF(AMX==ZERO)  AMX   = EP20
      IF(XMUMX == ZERO)XMUMX=EP20

      IF(A1 < ZERO .AND. A2 == ZERO)THEN
        chain='INVERTED YIELD SURFACE. CHECK A1 SIGN.                          '
        CALL ANCMSG(MSGID=829, MSGTYPE=MSGWARNING, ANMODE=ANINFO, I1=21, I2=MAT_ID, C1='WARNING', C2=TITR, C3=chain)
      ENDIF

      IF(A2 < ZERO)THEN
        chain='UNTYPICAL YIELD SURFACE. CHECK A2 SIGN.                         '
        CALL ANCMSG(MSGID=829, MSGTYPE=MSGWARNING, ANMODE=ANINFO, I1=21, I2=MAT_ID, C1='WARNING', C2=TITR, C3=chain)
      ENDIF

      IF(C1<=ZERO) THEN
        chain='TENSILE BULK MODULUS IS LOWER OR EQUAL TO 0.                    '      
        CALL ANCMSG(MSGID=829, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=21, I2=MAT_ID, C1='ERROR', C2=TITR, C3=chain)
      END IF

      IF(BUNL==ZERO) THEN
        chain='UNLOADING BULK MODULUS HAS NO USER VALUE. IT IS SET TO C1'
        BUNL  = C1
        CALL ANCMSG(MSGID=829, MSGTYPE=MSGWARNING, ANMODE=ANINFO, I1=21, I2=MAT_ID, C1='WARNING', C2=TITR, C3=chain)
      ENDIF

      IF(A2==ZERO.AND.A1/=ZERO)THEN   !(A2=A1=ZERO => error message)
        PSTAR=-A0/A1 
      ELSEIF(A2/=ZERO)THEN
        DELTA = A1*A1-FOUR*A0*A2
        !Si intersection avec l'axe
        IF(DELTA >= ZERO)THEN
          DELTA=SQRT(DELTA)
          PSTAR = (-A1+DELTA)/TWO/A2
        ELSE
        !  PSTAR = -A1/TWO/A2 !no let user do what he wants 
          PSTAR = -INFINITY
          chain='YIELD SURFACE HAS NO ROOT.                                     '      
          CALL ANCMSG(MSGID=829, MSGTYPE=MSGWARNING, ANMODE=ANINFO, I1=21, I2=MAT_ID, C1='WARNING', C2=TITR, C3=chain)
        ENDIF
      ELSE
        !do nothing let user do what he wants
      ENDIF
C      
      IF (FAC_Y == ZERO) FAC_Y = ONE*FSCAL_UNIT
      G=E/(TWO*(ONE + NU))
C
      PM(20)=E
      PM(21)=NU
      PM(22)=G
      PM(23)=E0     
      PM(31)=ZERO
      PM(32)=C1
      PM(33)=ZERO
      PM(34)=ZERO
      PM(35)=BUNL
      PM(36)=XMUMX
      PM(37)=PMIN
      PM(38)=A0
      PM(39)=A1
      PM(40)=A2
      PM(41)=AMX
      PM(42)=FAC_Y
      PM(43)=PEXT      
      PM(44)=PSTAR             
      IPM(11)=IFUNC
C-----------
C     Formulation for solid elements time step computation.
      IPM(252)= 2
      PM(105) = TWO*G/(C1+FOUR_OVER_3*G) ! =(1-2*Nu)/(1-Nu)
c-------------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,21
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0,RHOR
        WRITE(IOUT,1300)E,NU,G
        WRITE(IOUT,1400)A0,A1,A2,AMX
        WRITE(IOUT,1500)IFUNC,FAC_Y,C1,BUNL,XMUMX,PMIN,PEXT,PSTAR
      ENDIF
C
C---- Definition des variables internes (stockage elementaire)
c
      MTAG%G_PLA   = 1
      MTAG%G_EPSQ  = 1
      MTAG%G_MU    = 1 !unloading history (volumetric plastic strain)
c
      MTAG%L_PLA   = 1     ! /TH(VPLA)
      MTAG%L_EPSQ  = 1     ! /TH(EPSP)
      MTAG%L_MU    = 1 !unloading history
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")

      ! Material compatibility with /EOS option
      CALL INIT_MAT_KEYWORD(MATPARAM,"EOS")

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")
c
c-------------------
      RETURN
c-------------------
 1000 FORMAT(
     & 5X,40H  SOIL & CONCRETE                       ,/,
     & 5X,40H  ---------------                       ,//)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY  .  . . . . . . . . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,40HYOUNG'S MODULUS . . . . . . . . . . . .=,E12.4/,
     & 5X,40HPOISSON'S RATIO . . . . . . . . . . . .=,E12.4/,
     & 5X,40HSHEAR MODULUS . . . . . . . . . . . . .=,E12.4//)
 1400 FORMAT(
     & 5X,40HYIELD COEFFICIENT A0. . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD COEFFICIENT A1. . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD COEFFICIENT A2. . . . . . . . . .=,E12.4/,
     & 5X,40HA-MAX . . . . . . . . . . . . . . . . .=,E12.4//)
 1500 FORMAT(
     & 5X,40HPRESSURE FUNCTION NUMBER. . . . . . . .=,I8/,
     & 5X,40HPRESSURE FUNCTION SCALE FACTOR. . . . .=,E12.4/,
     & 5X,40HTENSILE BULK. . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HUNLOADING BULK. . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMAX VOLUMIC COMPRESSION . . . . . . . .=,E12.4/,
     & 5X,40HFRACTURE PRESSURE . . . . . . . . . . .=,E12.4/,
     & 5X,40HEXTERNAL PRESSURE . . . . . . . . . . .=,E12.4/,
     & 5X,40HYIELD SURFACE PRESSURE ROOT . . . . . .=,E12.4//)
c-------------------
      RETURN
      END
