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
      !||    detonators_mod               ../starter/share/modules1/detonators_mod.F
      !||--- called by ------------------------------------------------------
      !||    binit2                       ../starter/source/ale/bimat/binit2.F
      !||    contrl                       ../starter/source/starter/contrl.F
      !||    detcord                      ../starter/source/initial_conditions/detonation/detcord.F
      !||    detcord0                     ../starter/source/initial_conditions/detonation/detcord0.F
      !||    ecran1                       ../starter/source/initial_conditions/detonation/ecran1.F
      !||    ecran2                       ../starter/source/initial_conditions/detonation/ecran2.F
      !||    ig3dinit3                    ../starter/source/elements/ige3d/ig3dinit3.F
      !||    initia                       ../starter/source/elements/initia/initia.F
      !||    iombr                        ../starter/source/initial_conditions/detonation/iombr.F
      !||    lectur                       ../starter/source/starter/lectur.F
      !||    m105init                     ../starter/source/materials/mat/mat105/m105init.F
      !||    m51init                      ../starter/source/materials/mat/mat051/m51init.F
      !||    m5in2                        ../starter/source/initial_conditions/detonation/m5in2.F
      !||    m5in2t                       ../starter/source/initial_conditions/detonation/m5in2t.F
      !||    m5in3                        ../starter/source/initial_conditions/detonation/m5in3.F
      !||    m97init                      ../starter/source/materials/mat/mat097/m97init.F
      !||    matini                       ../starter/source/materials/mat_share/matini.F
      !||    multifluid_init2             ../starter/source/multifluid/multifluid_init2.F
      !||    multifluid_init2t            ../starter/source/multifluid/multifluid_init2t.F
      !||    multifluid_init3             ../starter/source/multifluid/multifluid_init3.F
      !||    multifluid_init3t            ../starter/source/multifluid/multifluid_init3t.F
      !||    prelecdet                    ../starter/source/initial_conditions/detonation/prelecdet.F
      !||    printstsz                    ../starter/source/spmd/node/st_array_size.F
      !||    q4init2                      ../starter/source/elements/solid_2d/quad4/q4init2.F
      !||    qinit2                       ../starter/source/elements/solid_2d/quad/qinit2.F
      !||    r2r_group                    ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_prelec                   ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    read_detonators              ../starter/source/initial_conditions/detonation/read_detonators.F
      !||    read_dfs_detcord             ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline             ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan             ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint            ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper         ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    s10init3                     ../starter/source/elements/solid/solide10/s10init3.F
      !||    s16init3                     ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s20init3                     ../starter/source/elements/solid/solide20/s20init3.F
      !||    s4init3                      ../starter/source/elements/solid/solide4/s4init3.F
      !||    s6cinit3                     ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s8cinit3                     ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8zinit3                     ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    scinit3                      ../starter/source/elements/thickshell/solidec/scinit3.F
      !||    sinit3                       ../starter/source/elements/solid/solide/sinit3.F
      !||    spinit3                      ../starter/source/elements/sph/spinit3.F
      !||    st_qaprint_dfs_detonators    ../starter/source/output/qaprint/st_qaprint_dfs_detonators.F
      !||    st_qaprint_driver            ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_internal_groups   ../starter/source/output/qaprint/st_qaprint_internal_groups.F
      !||    starter0                     ../starter/source/starter/starter0.F
      !||    suinit3                      ../starter/source/elements/elbuf_init/suinit3.F
      !||====================================================================
      MODULE DETONATORS_MOD
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------    

        ! --- /DFS/DETPOINT DATA STRUCTURE
        TYPE DETONATOR_POINT_STRUCT_
          LOGICAL :: IS_MAT_VALID = .FALSE.
          INTEGER :: MAT = 0
          my_real :: TDET
          my_real :: XDET
          my_real :: YDET
          my_real :: ZDET
          !optional list of nodes
          INTEGER :: GRNOD_ID
          INTEGER :: NNOD
          INTEGER,DIMENSION(:),ALLOCATABLE :: NODLIST
        END TYPE DETONATOR_POINT_STRUCT_

        ! --- /DFS/DETLINE DATA STRUCTURE
        TYPE DETONATOR_LINE_STRUCT_
          LOGICAL :: IS_MAT_VALID = .FALSE.
          INTEGER :: MAT = 0
          my_real :: TDET
          my_real :: XDET_1
          my_real :: YDET_1
          my_real :: ZDET_1
          my_real :: XDET_2
          my_real :: YDET_2
          my_real :: ZDET_2
        END TYPE DETONATOR_LINE_STRUCT_

       ! --- /DFS/DETPLANE DATA STRUCTURE
        TYPE DETONATOR_PLANE_STRUCT_
          LOGICAL :: IS_MAT_VALID = .FALSE.
          INTEGER :: MAT = 0
          my_real :: TDET
          my_real :: XDET
          my_real :: YDET
          my_real :: ZDET
          my_real :: NX
          my_real :: NY
          my_real :: NZ
        END TYPE DETONATOR_PLANE_STRUCT_

        ! --- /DFS/WAVE_SHAPER DATA STRUCTURE
        TYPE DETONATOR_WAVE_SHAPER_STRUCT_
          LOGICAL :: IS_MAT_VALID = .FALSE.
          INTEGER :: MAT = 0
          INTEGER :: NUMNOD
          my_real :: XDET
          my_real :: YDET
          my_real :: ZDET
          my_real :: VDET
          my_real :: TDET
          INTEGER,DIMENSION(:),ALLOCATABLE :: NODES
          my_real,DIMENSION(:),ALLOCATABLE :: TIME
          INTEGER,DIMENSION(:),ALLOCATABLE :: FLAG
          INTEGER,DIMENSION(:),ALLOCATABLE :: IORDR
        END TYPE DETONATOR_WAVE_SHAPER_STRUCT_

        ! --- /DFS/DETCORD DATA STRUCTURE
        TYPE DETONATOR_CORD_STRUCT_
          LOGICAL :: IS_MAT_VALID = .FALSE.
          INTEGER :: MAT = 0
          INTEGER :: IOPT
          INTEGER :: NUMNOD
          INTEGER,DIMENSION(:),ALLOCATABLE :: NODES
          my_real,DIMENSION(:),ALLOCATABLE :: TDET_PATH
          my_real :: TDET
          my_real :: VDET
        END TYPE DETONATOR_CORD_STRUCT_

        ! --- MAIN DATA STRUCTURE
        TYPE DETONATORS_STRUCT_
          !numbering
          INTEGER :: N_DET !sum of all detonators
          INTEGER :: N_DET_POINT !number of /DFS/DETPOINT
          INTEGER :: N_DET_LINE !number of /DFS/DETLINE
          INTEGER :: N_DET_WAVE_SHAPER !number of /DFS/WAVE_SHAPER
          INTEGER :: N_DET_CORD !number of /DFS/DETCORD
          INTEGER :: N_DET_PLANE !number of /DFS/DETPLANE
          !all detonators
          TYPE(DETONATOR_POINT_STRUCT_), DIMENSION(:), ALLOCATABLE :: POINT
          TYPE(DETONATOR_LINE_STRUCT_), DIMENSION(:), ALLOCATABLE :: LINE
          TYPE(DETONATOR_CORD_STRUCT_), DIMENSION(:), ALLOCATABLE :: CORD
          TYPE(DETONATOR_WAVE_SHAPER_STRUCT_), DIMENSION(:), ALLOCATABLE :: WAVE_SHAPER
          TYPE(DETONATOR_PLANE_STRUCT_), DIMENSION(:), ALLOCATABLE :: PLANE
        END TYPE DETONATORS_STRUCT_
        
      CONTAINS

      !||====================================================================
      !||    deallocate_detonators   ../starter/share/modules1/detonators_mod.F
      !||--- called by ------------------------------------------------------
      !||    lectur                  ../starter/source/starter/lectur.F
      !||====================================================================
      SUBROUTINE DEALLOCATE_DETONATORS(DETONATORS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(DETONATORS_STRUCT_),INTENT(INOUT)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDET
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !working arrays for detonation point
      IF(DETONATORS%N_DET_POINT > 0)THEN
        DO IDET=1,DETONATORS%N_DET_POINT
          IF(ALLOCATED(DETONATORS%POINT(IDET)%NODLIST))DEALLOCATE(DETONATORS%POINT(IDET)%NODLIST)
        ENDDO
      ENDIF

      !working arrays for detonating cord
      IF(DETONATORS%N_DET_CORD > 0)THEN
        DO IDET=1,DETONATORS%N_DET_CORD
          IF(ALLOCATED(DETONATORS%CORD(IDET)%NODES))DEALLOCATE(DETONATORS%CORD(IDET)%NODES)
          IF(ALLOCATED(DETONATORS%CORD(IDET)%TDET_PATH))DEALLOCATE(DETONATORS%CORD(IDET)%TDET_PATH)
        ENDDO
      ENDIF

      !working arrays for wave shaper
      IF(DETONATORS%N_DET_WAVE_SHAPER > 0)THEN
        DO IDET=1,DETONATORS%N_DET_WAVE_SHAPER
          IF(ALLOCATED(DETONATORS%WAVE_SHAPER(IDET)%NODES))DEALLOCATE(DETONATORS%WAVE_SHAPER(IDET)%NODES)
          IF(ALLOCATED(DETONATORS%WAVE_SHAPER(IDET)%TIME))DEALLOCATE(DETONATORS%WAVE_SHAPER(IDET)%TIME)
          IF(ALLOCATED(DETONATORS%WAVE_SHAPER(IDET)%FLAG))DEALLOCATE(DETONATORS%WAVE_SHAPER(IDET)%FLAG)
          IF(ALLOCATED(DETONATORS%WAVE_SHAPER(IDET)%IORDR))DEALLOCATE(DETONATORS%WAVE_SHAPER(IDET)%IORDR)
        ENDDO
      ENDIF

      !general data structure
      IF(ALLOCATED(DETONATORS%POINT)) DEALLOCATE(DETONATORS%POINT)
      IF(ALLOCATED(DETONATORS%PLANE)) DEALLOCATE(DETONATORS%PLANE)
      IF(ALLOCATED(DETONATORS%LINE))  DEALLOCATE(DETONATORS%LINE)
      IF(ALLOCATED(DETONATORS%CORD))  DEALLOCATE(DETONATORS%CORD)
      IF(ALLOCATED(DETONATORS%WAVE_SHAPER)) DEALLOCATE(DETONATORS%WAVE_SHAPER)

      END SUBROUTINE DEALLOCATE_DETONATORS
      
      END MODULE DETONATORS_MOD

