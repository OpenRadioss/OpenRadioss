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
!||    hm_read_mat46            ../starter/source/materials/mat/mat046/hm_read_mat46.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT46(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,IPM      ,MATPARAM )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW46 WITH HM READER
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD    
      USE UNITAB_MOD
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
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: IPM(NPROPMI),ISRATE,IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM,NUPARAM, NUVAR,IMATVIS
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real SSP,VIS,SMAG,SMAG2,C1,CPS,CA,FAC_M,FAC_L,FAC_T,FAC_C
      INTEGER I,ISGS
      my_real :: RHO0, RHOR
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------      
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ISRATE = 0
      IMATVIS = 0
      NUPARAM = 5
      NFUNC = 0
      NUVAR = 2
      MTAG%NUVAR = NUVAR

      !===READING 
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      !line+1
      CALL HM_GET_FLOATV('MAT_RHO'      ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'    ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_C'        ,SSP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'       ,VIS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-3
      CALL HM_GET_INTV  ('Istf'         ,ISGS     ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_C5'       ,SMAG     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_CO1'      ,CPS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)

      !===DEFAULT                
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0  
      IF(ISGS==0)SMAG2=ZERO
      IF(ISGS>=1)THEN
       IF(SMAG==ZERO)THEN
        SMAG=EM01
        SMAG2=EM02
       ELSE
        SMAG2=SMAG**2
       ENDIF
      ENDIF
      IF(ISGS>=2)THEN
        IF(CPS==0)CPS=SMAG
        CA=(CPS/SMAG)**2
      ELSE
        CA=ZERO
      ENDIF
      C1=PM(1)*SSP**2

      !===STORAGE      
      UPARAM(1) = VIS
      UPARAM(2) = C1
      UPARAM(3) = ISGS+EM01
      UPARAM(4) = SMAG2
      UPARAM(5) = CA
      !      
      PARMAT(1) = C1

      ! MATPARAM keywords

      ! EOS/Thermo keyword for pressure treatment in elements
      CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")

      ! Properties compatibility 
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_POROUS")     

      !===OUTPUT 
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,46
      WRITE(IOUT,'(5X,A,//)')'  LES FLUID'
      WRITE(IOUT,'(5X,A,//)')'  ---------'      
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF(ISGS==0)WRITE(IOUT,1000)RHO0,RHOR,SSP,VIS
        IF(ISGS==1)WRITE(IOUT,1100)RHO0,RHOR,SSP,VIS,SMAG
        IF(ISGS>=2)WRITE(IOUT,1200)RHO0,RHOR,SSP,VIS,SMAG,CPS
      ENDIF
      
c-----------
 1000 FORMAT(
     & 5X,40H  LES FLUID                             ,/,
     & 5X,40H  ---------                             ,//
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',E12.4/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',E12.4/,    
     & 5X,40HSOUND SPEED . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMOLECULAR DYNAMIC VISCOSITY . . . . . .=,E12.4/)
 1001 FORMAT(
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . . =',I10/)
 1100 FORMAT(
     & 5X,40H  LES WITH SMAGORINSKY SGS MODEL        ,/,
     & 5X,40H  ------------------------------        ,//
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',E12.4/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',E12.4/,     
     & 5X,40HSOUND SPEED . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMOLECULAR DYNAMIC VISCOSITY . . . . . .=,E12.4/,
     & 5X,40HSMAGORINSKY CONSTANT  . . . . . . . . .=,E12.4//)
 1200 FORMAT(
     & 5X,40H  LES WITH ACOUSTIC SMAGORINSKY SGS     ,/,
     & 5X,40H  ---------------------------------     ,//
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',E12.4/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . .=',E12.4/,     
     & 5X,40HSOUND SPEED . . . . . . . . . . . . . .=,E12.4/,
     & 5X,40HMOLECULAR DYNAMIC VISCOSITY . . . . . .=,E12.4/,
     & 5X,40HSMAGORINSKY CONSTANT  . . . . . . . . .=,E12.4/,
     & 5X,40HPRESSURE DAMPING. . . . . . . . . . . .=,E12.4//)
c-----------
     
      RETURN
      END SUBROUTINE
