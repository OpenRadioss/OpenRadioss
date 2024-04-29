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
Chd|  HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT25(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,UNITAB   ,LSUBMODEL,
     .           MTAG     ,ID       ,TITR     ,PM       ,ISRATE   ,
     .           PARMAT   ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE read_mat25_crasurv_mod
      USE read_mat25_tsaiwu_mod
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW25 WITH HM READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     MAT_ID              MATERIAL MAT_ID(INTEGER)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)                          :: ID
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
c
      INTEGER, INTENT(INOUT)                         :: ISRATE
      my_real, DIMENSION(100)       ,INTENT(INOUT)   :: PARMAT     
      my_real, DIMENSION(NPROPM)    ,INTENT(INOUT)   :: PM     
      TYPE(MLAW_TAG_)               ,INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
c
      INTEGER :: MAXUPARAM,NUPARAM
      my_real, DIMENSION(MAXUPARAM)    :: UPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ILAW,IFLAG
      LOGICAL :: IS_AVAILABLE
C=======================================================================
      IS_AVAILABLE = .FALSE.
      ILAW = 25
c--------------------------------------------------------
c     Formulation type
c--------------------------------------------------------
c     
      CALL HM_GET_INTV  ('MAT_Iflag' ,IFLAG    ,IS_AVAILABLE,LSUBMODEL)
      
      IF (iflag == 0) then   ! Tsai-Wu formulation
        CALL READ_MAT25_TSAIWU(
     .           MATPARAM ,PARMAT   ,UNITAB   ,LSUBMODEL,
     .           ID       ,TITR     ,PM       ,ISRATE   ,
     .           IOUT     ,NPROPM   )
        MATPARAM%NMOD = 3
      ELSE     
       CALL READ_MAT25_CRASURV(
     .           MATPARAM ,PARMAT   ,UNITAB   ,LSUBMODEL,
     .           ID       ,TITR     ,PM       ,ISRATE   ,
     .           IOUT     ,NPROPM   )
        MATPARAM%NMOD = 6
      END IF
c-------------------------------------------------------------
c---- Element buffer variable activation (for automatic allocation)
c-------------------------------------------------------------
c
      MTAG%G_IERR   = 1  ! tag element for projection error message
      MTAG%G_GAMA   = 6  
      MTAG%G_PLA    = 1    
      MTAG%G_EPSD   = 1    
      MTAG%G_DAMDL  = 1  
      MTAG%G_TSAIWU = 1 
      MTAG%G_DMG    = 1 + MATPARAM%NMOD 
c
      MTAG%L_GAMA   = 6  
      MTAG%L_PLA    = 1
      MTAG%L_EPSD   = 1
      MTAG%L_CRAK   = 2
      MTAG%L_STRA   = 6
      MTAG%L_TSAIWU = 1
      MTAG%L_DMG    = 1 + MATPARAM%NMOD
c
      ALLOCATE(MATPARAM%MODE(MATPARAM%NMOD))
      MATPARAM%MODE(1) = "Tensile damage in direction 1"
      MATPARAM%MODE(2) = "Tensile damage in direction 2"
      MATPARAM%MODE(3) = "Global maximum plastic work"
      IF (iflag > 0) THEN 
        MATPARAM%MODE(4) = "Failure index in direction 1"
        MATPARAM%MODE(5) = "Failure index in direction 2"
        MATPARAM%MODE(6) = "Failure index in shear plane 12"
      ENDIF
c
c-----------
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      CALL INIT_MAT_KEYWORD(MATPARAM,"ORTHOTROPIC")
c-----------
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ORTHOTROPIC")
      IF (IFLAG == 0) THEN 
        CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_ORTHOTROPIC")
      ELSE 
        CALL INIT_MAT_KEYWORD(MATPARAM,"SHELL_COMPOSITE")    
      ENDIF
c--------
      RETURN
      END
