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
Chd|  DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|        DETCORD0                      source/initial_conditions/detonation/detcord0.F
Chd|        ECRAN1                        source/initial_conditions/detonation/ecran1.F
Chd|        ECRAN2                        source/initial_conditions/detonation/ecran2.F
Chd|        FREFORM                       source/starter/freform.F      
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        M105INIT                      source/materials/mat/mat105/m105init.F
Chd|        M51INIT                       source/materials/mat/mat051/m51init.F
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN2T                        source/initial_conditions/detonation/m5in2t.F
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|        M97INIT                       source/materials/mat/mat097/m97init.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|        MULTIFLUID_INIT2T             source/multifluid/multifluid_init2t.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|        PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|        PRINTSTSZ                     source/spmd/node/st_array_size.F
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SPINIT3                       source/elements/sph/spinit3.F 
Chd|        STARTER0                      source/starter/starter0.F     
Chd|        ST_QAPRINT_DFS_DETONATORS     source/output/qaprint/st_qaprint_dfs_detonators.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_INTERNAL_GROUPS    source/output/qaprint/st_qaprint_internal_groups.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|        IOMBR                         source/initial_conditions/detonation/iombr.F
Chd|-- calls ---------------
Chd|====================================================================
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

Chd|====================================================================
Chd|  DEALLOCATE_DETONATORS         share/modules1/detonators_mod.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
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

