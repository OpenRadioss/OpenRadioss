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
!||    hm_read_mat34            ../starter/source/materials/mat/mat034/hm_read_mat34.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT34(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW33 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPM             MATERIAL ARRAY(INTEGER)
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
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      my_real, DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      INTEGER, DIMENSION(MAXFUNC)   ,INTENT(INOUT)  :: IFUNC
      INTEGER, INTENT(INOUT)          :: ISRATE,IMATVIS,NFUNC,MAXFUNC,MAXUPARAM,NUPARAM,NUVAR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
      INTEGER,INTENT(IN)              :: MAT_ID
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)  :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: BULK,YOUNG
      my_real :: G0,GI,BETA
      my_real :: P0,PHI,GAMA0 
      my_real :: RHOR,RHO0,NU   
      LOGICAL :: IS_ENCRYPTED,IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ISRATE=0
      IMATVIS=1
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.


      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      
      CALL HM_GET_FLOATV('MAT_BULK'     ,BULK     ,IS_AVAILABLE, LSUBMODEL, UNITAB)


      CALL HM_GET_FLOATV('MAT_G0'       ,G0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GI'       ,GI       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_DECAY'    ,BETA     ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      CALL HM_GET_FLOATV('MAT_P0'       ,P0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_PHI'      ,PHI      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMA0'    ,GAMA0    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

        !GAMA0 = GAMA0 * FAC_L * FAC_L * FAC_L

      
      IF (RHOR==ZERO) RHOR=RHO0

      NUPARAM=7
      NFUNC=0
      NUVAR=8

      UPARAM(1)=BULK
      UPARAM(2)=G0
      UPARAM(3)=GI
      UPARAM(4)=BETA
      UPARAM(5)=P0
      UPARAM(6)=PHI
      UPARAM(7)=GAMA0
      
      YOUNG = (NINE*BULK*G0)/(THREE*BULK + G0)

      PARMAT(1) = BULK
!      PARMAT(1) = TWO*GI
      PARMAT(2) = YOUNG
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = TWO*G0/(BULK + FOUR_OVER_3*G0) 
      
      PM(1)  = RHOR
      PM(89) = RHO0
      PM(12) = SQRT(MAX(ZERO, G0)) !done by default in lecmuser.F for law34
      PM(22) = G0
      PM(27) = SQRT(YOUNG/RHO0)  ! Sound speed   
c
      ! MATPARAM keywords
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
c
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")   
      CALL INIT_MAT_KEYWORD(MATPARAM,"TRUSS")
      CALL INIT_MAT_KEYWORD(MATPARAM,"BEAM_INTEGRATED")
c
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,34
      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1002) RHO0           
        WRITE(IOUT,1100) BULK,G0,GI,BETA,P0,PHI,GAMA0
      ENDIF
       
      RETURN

 1000 FORMAT
     & (5X,14H  VISCOELASTIC,/,
     &  5X,14H  ------------,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,   'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,   'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1002 FORMAT(
     &  5X,  'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)
 1100 FORMAT
     & (5X,  'BULK MODULUS. . . . . . . . . . . . . .=',1PG20.13/
     & ,5X,  'SHEAR MODULUS (SHORT TIME). . . . . . .=',1PG20.13/ 
     & ,5X,  'SHEAR MODULUS (LONG TIME) . . . . . . .=',1PG20.13/
     & ,5X,  'DECAY CONSTANT. . . . . . . . . . . . .=',1PG20.13//
     & ,5X,  'INITIAL FOAM PRESSURE . . . . . . . . .=',1PG20.13/
     & ,5X,  'RATIO OF FOAM TO POLYMER DENSITY. . . .=',1PG20.13/
     & ,5X,  'INITIAL VOLUMETRIC STRAIN. . . .. . . .=',1PG20.13//)

      END

