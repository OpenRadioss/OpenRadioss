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
Chd|  INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|-- called by -----------
Chd|        ALE_BOX_COLORATION            source/initial_conditions/inivol/ale_box_coloration.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        ST_QAPRINT_INIVOL             source/output/qaprint/st_qaprint_inivol.F
Chd|        SURFACE_MIN_MAX_COMPUTATION   source/initial_conditions/inivol/surface_min_max_computation.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INIVOL_DEF_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
        TYPE CONTAINER_
          INTEGER   :: SURF_ID
          INTEGER   :: SUBMAT_ID  ! Phase of the multi-material ALE to fill the Part
          my_real   :: VFRAC     ! Filling ratio
          INTEGER   :: ICUMU     ! Flag for cumulate volume filling
          INTEGER   :: IREVERSED ! Filling option parameter for filling action (inside or outside the container) 
                       ! 0 ! filling the side along normal direction 
                       ! 1 ! filling the side against normal direction
          LOGICAL   :: REQUIRED_MONTE_CARLO_METHOD
          LOGICAL   :: REQUIRED_2D_POLYGON_CLIPPING
        END TYPE CONTAINER_
        
C-----------------------------------------------

        TYPE INIVOL_STRUCT_
          CHARACTER :: TITLE*nchartitle
          INTEGER   :: ID
          INTEGER   :: PART_ID
          INTEGER   :: SIZE
          INTEGER   :: NUM_CONTAINER
          LOGICAL   :: REQUIRED_MONTE_CARLO_METHOD   !set to true if at least one container is using this method
          LOGICAL   :: REQUIRED_2D_POLYGON_CLIPPING  !set to true if at least one container is using this method
          TYPE (CONTAINER_) ,DIMENSION(:) ,ALLOCATABLE :: CONTAINER
        END TYPE INIVOL_STRUCT_

C-----------------------------------------------
C   D a t a   S t r u c t u r e
C-----------------------------------------------

        TYPE(INIVOL_STRUCT_),DIMENSION(:),ALLOCATABLE :: INIVOL   !  SIZE NUM_INIVOL
        INTEGER NUM_INIVOL
        INTEGER SKVOL

C-----------------------------------------------
      END MODULE INIVOL_DEF_MOD
C-----------------------------------------------



Chd|====================================================================
Chd|  INIVOL_INTERF_MOD             share/modules1/inivol_mod.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INIVOL_INTERF_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   B l o c k   I n t e r f a c e
C------------------------------------------------
      INTERFACE
         SUBROUTINE HM_READ_INIVOL(INIVOL_STRUCT , KVOL, IGRSURF ,IPART  ,MULTI_FVM, BUFMAT, IPM, NSUBMAT, LSUBMODEL, UNITAB)
           USE UNITAB_MOD
           USE MULTI_FVM_MOD
           USE GROUPDEF_MOD
           USE INIVOL_DEF_MOD
           USE SUBMODEL_MOD
           USE SETDEF_MOD
#include "tabsiz_c.inc"           
#include "com04_c.inc"  
              TYPE(INIVOL_STRUCT_),INTENT(INOUT),DIMENSION(:),ALLOCATABLE  :: INIVOL_STRUCT
              my_real,DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: KVOL
              INTEGER,DIMENSION(SIPART) :: IPART
              TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
              INTEGER,INTENT(IN),DIMENSION(SIPM) :: IPM
              my_real,INTENT(IN),TARGET :: BUFMAT(*)
              TYPE (SURF_), DIMENSION(NSURF+NSETS) :: IGRSURF
              TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
             TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB
             INTEGER,INTENT(INOUT) :: NSUBMAT
           END SUBROUTINE HM_READ_INIVOL
         END INTERFACE
      END MODULE INIVOL_INTERF_MOD
C-----------------------------------------------
