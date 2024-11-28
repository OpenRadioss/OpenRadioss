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
      !||====================================================================
      !||    inivol_def_mod                ../starter/share/modules1/inivol_mod.F
      !||--- called by ------------------------------------------------------
      !||    ale_box_coloration            ../starter/source/initial_conditions/inivol/ale_box_coloration.F
      !||    contrl                        ../starter/source/starter/contrl.F
      !||    hm_read_mat151                ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    inisoldist                    ../starter/source/initial_conditions/inivol/inisoldist.F
      !||    init_inivol                   ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_inivol_2d_polygons       ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||    initia                        ../starter/source/elements/initia/initia.F
      !||    lectur                        ../starter/source/starter/lectur.F
      !||    qgrtails                      ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    r2r_group                     ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_prelec                    ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    ratio_fill                    ../starter/source/initial_conditions/inivol/ratio_fill.F
      !||    sgrtails                      ../starter/source/elements/solid/solide/sgrtails.F
      !||    st_qaprint_inivol             ../starter/source/output/qaprint/st_qaprint_inivol.F
      !||    surface_min_max_computation   ../starter/source/initial_conditions/inivol/surface_min_max_computation.F
      !||    t3grtails                     ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      MODULE INIVOL_DEF_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE NAMES_AND_TITLES_MOD , ONLY : nchartitle
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
        TYPE CONTAINER_
          INTEGER   :: SURF_ID
          INTEGER   :: GRNOD_ID
          INTEGER   :: SUBMAT_ID  ! Phase of the multi-material ALE to fill the Part
          my_real   :: VFRAC     ! Filling ratio
          INTEGER   :: ICUMU     ! Flag for cumulate volume filling
          INTEGER   :: IREVERSED ! Filling option parameter for filling action (inside or outside the container) 
                       ! 0 ! filling the side along normal direction 
                       ! 1 ! filling the side against normal direction
        END TYPE CONTAINER_
        
C-----------------------------------------------

        TYPE INIVOL_STRUCT_
          CHARACTER(len=nchartitle) :: TITLE
          INTEGER :: ID
          INTEGER :: PART_ID
          INTEGER :: SIZE
          INTEGER :: NUM_CONTAINER
          TYPE (CONTAINER_) ,DIMENSION(:) ,ALLOCATABLE :: CONTAINER
          my_real :: XYZ(6) !global min,max
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
