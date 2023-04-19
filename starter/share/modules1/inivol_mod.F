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
Chd|  INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|-- called by -----------
Chd|        ALE_BOX_COLORATION            source/initial_conditions/inivol/ale_box_coloration.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        ST_QAPRINT_INIVOL             source/output/qaprint/st_qaprint_inivol.F
Chd|        SURFACE_MIN_MAX_COMPUTATION   source/initial_conditions/inivol/surface_min_max_computation.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
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
        TYPE CONTY_
          INTEGER   :: IPHASE ! Phase of the multi-material ALE to fill the Part
          my_real   :: FILL_RATIO ! Filling ratio
          INTEGER   :: ICUMU ! Flag for cumulate volume filling
          INTEGER   :: IFILL ! Filling option parameter for filling action (inside or outside the container) 
                       ! 0 ! filling the side along normal direction 
                       ! 1 ! filling the side against normal direction
        END TYPE CONTY_
C-----------------------------------------------
        TYPE INIVOL_
          INTEGER   :: ID ! - INIVOL IDENTIFIER
          INTEGER   :: SIPHASE          
          CHARACTER :: TITLE*nchartitle ! INIVOL title
          INTEGER   :: NBCONTY ! NUMBER of INIVOL containers
          INTEGER, DIMENSION(:),ALLOCATABLE :: SURFCONTY ! Surface defining containers of the INIVOL
          INTEGER   :: IPARTFILL ! INIVOL part to be filled
          !INTEGER   :: ISURF ! INIVOL CONTAINER SURFACE IDENTIFIER
          TYPE (CONTY_) ,DIMENSION(:) ,ALLOCATABLE :: CONTY ! CONTAINER to be
        END TYPE INIVOL_
        
C-----------------------------------------------
      END MODULE INIVOL_DEF_MOD
C-----------------------------------------------



Chd|====================================================================
Chd|  INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|-- called by -----------
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
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|-- calls ---------------
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|====================================================================
      MODULE INIVOL_ARRAY_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   S o u r c e   L i n e s
C------------------------------------------------
        USE INIVOL_DEF_MOD
        TYPE(INIVOL_)   ,TARGET,DIMENSION(:),ALLOCATABLE :: USER_INIVOL   !  NINIVOL
        INTEGER NINIVOL
C-----------------------------------------------
      END MODULE INIVOL_ARRAY_MOD
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
         SUBROUTINE HM_READ_INIVOL(INIVOL_STRUCT , KVOL, IGRSURF ,IPART  ,MULTI_FVM, BUFMAT, IPM, ITAB, NSUBMAT, LSUBMODEL, UNITAB)
         USE UNITAB_MOD
         USE MULTI_FVM_MOD
         USE GROUPDEF_MOD
         USE INIVOL_DEF_MOD
         USE SUBMODEL_MOD 
         USE SETDEF_MOD   
#include "tabsiz_c.inc"           
#include "com04_c.inc"  
#include "submod_c.inc"  
         TYPE (INIVOL_)  , INTENT(INOUT), DIMENSION(:), ALLOCATABLE  :: INIVOL_STRUCT
              my_real, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: KVOL
              INTEGER, DIMENSION(SIPART) :: IPART
              TYPE(MULTI_FVM_STRUCT),INTENT(IN) :: MULTI_FVM
              INTEGER,INTENT(IN), DIMENSION(SIPM) :: IPM
              INTEGER,INTENT(IN),DIMENSION(NUMNOD) :: ITAB
              my_real,INTENT(IN),TARGET :: BUFMAT(*)
              TYPE (SURF_)   , DIMENSION(NSURF+NSETS)   :: IGRSURF
              TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
             TYPE (UNIT_TYPE_),INTENT(IN)      :: UNITAB 
             INTEGER,INTENT(INOUT) :: NSUBMAT
           END SUBROUTINE HM_READ_INIVOL
         END INTERFACE
      END MODULE INIVOL_INTERF_MOD
C-----------------------------------------------
