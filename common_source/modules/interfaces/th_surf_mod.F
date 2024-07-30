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

!> \brief OPTION /TH/SURF outputs of Pressure and Area needed Tabs 

      !||====================================================================
      !||    th_surf_mod             ../common_source/modules/interfaces/th_surf_mod.F
      !||--- called by ------------------------------------------------------
      !||    alemain                 ../engine/source/ale/alemain.F
      !||    dyna_ina                ../engine/source/implicit/imp_dyna.F
      !||    dyna_wex                ../engine/source/implicit/imp_dyna.F
      !||    ebcs10                  ../engine/source/boundary_conditions/ebcs/ebcs10.F
      !||    ebcs_main               ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    force                   ../engine/source/loads/general/force.F90
      !||    force_imp               ../engine/source/loads/general/force_imp.F
      !||    hist2                   ../engine/source/output/th/hist2.F
      !||    imp_chkm                ../engine/source/implicit/imp_solv.F
      !||    imp_solv                ../engine/source/implicit/imp_solv.F
      !||    load_pressure           ../engine/source/loads/general/load_pressure/load_pressure.F
      !||    multi_ebcs              ../engine/source/multifluid/multi_ebcs.F
      !||    multi_inlet_ebcs        ../engine/source/multifluid/multi_inlet_ebcs.F
      !||    multi_nrf_ebcs          ../engine/source/multifluid/multi_nrf_ebcs.F
      !||    multi_timeevolution     ../engine/source/multifluid/multi_timeevolution.F
      !||    pblast                  ../engine/source/loads/pblast/pblast.F
      !||    pblast_1                ../engine/source/loads/pblast/pblast_1.F
      !||    pblast_2                ../engine/source/loads/pblast/pblast_2.F
      !||    pblast_3                ../engine/source/loads/pblast/pblast_3.F
      !||    pfluid                  ../engine/source/loads/general/pfluid/pfluid.F
      !||    sortie_main             ../engine/source/output/sortie_main.F
      !||    surf_mass_monv          ../engine/source/output/th/surf_mass.F
      !||    th_surf_load_pressure   ../starter/source/output/th/th_surf_load_pressure.F
      !||    thsurf                  ../engine/source/output/th/thsurf.F
      !||    time_history_mod        ../common_source/modules/output/time_history_mod.F
      !||    w_th_surf_loadp         ../starter/source/restart/ddsplit/w_th_surf_loadp.F
      !||    w_th_surf_pload         ../starter/source/restart/ddsplit/w_th_surf_pload.F
      !||====================================================================
        MODULE TH_SURF_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
       implicit  none
!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

C----------------------------------------------
C   /TH/SURF outputs of Pressure and Area
C           needed Tabs 
C---------------------------------------------

          TYPE TH_SURF_
            INTEGER  IOK         !< Flag for /TH/SURF + Load pressure
            INTEGER  NSURF       !< Number of surfaces /TH/SURF
            INTEGER  PLOAD_FLAG  !< Flag if th surface include segments where /PLOAD is applied
            INTEGER  LOADP_FLAG  !< Flag if th surface include segments where /PFLUID or /PBLAST or /LOADP_HYD is applied

            INTEGER  NSEGLOADP   !< Number of segments for which loadp Load_Pressure is applied
            INTEGER  NSEGLOADPF  !< Number of segments for which loadp Pfluid is applied
            INTEGER  NSEGLOADPB  !< Number of segments for which loadp Pblast is applied

            INTEGER  S_PLOAD_KSEGS  !< Size table of ids of th surfaces to which each segment of pload is included 
            INTEGER  S_PLOAD_SEGS   !< Size table of ids of th surfaces to which each segment of pload is included 
            INTEGER, DIMENSION(:), ALLOCATABLE :: PLOAD_KSEGS  !< ids of th surfaces to which each segment of pload is included 
            INTEGER, DIMENSION(:), ALLOCATABLE :: PLOAD_SEGS   !< list of th surfaces to which each segment of pload is included 

            INTEGER  S_LOADP_KSEGS  !< Size table of ids of th surfaces to which each segment of load pressure is included 
            INTEGER  S_LOADP_SEGS   !< Size table of ids of th surfaces to which each segment of load pressure is included 
            INTEGER, DIMENSION(:), ALLOCATABLE :: LOADP_KSEGS  !< ids of th surfaces to which each segment of load pressure is included 
            INTEGER, DIMENSION(:), ALLOCATABLE :: LOADP_SEGS   !< list of th surfaces to which each segment of load pressure is included 

            my_real, DIMENSION(:,:), ALLOCATABLE :: CHANNELS   !< channels for /TH/SURF output (1:TH_SURF_NUM_CHANNEL, 1:NSURF)
                                                               !
          END TYPE TH_SURF_

          INTEGER, PARAMETER :: TH_SURF_NUM_CHANNEL = 6        !< number ot /TH/SURF channels : AREA, VELOCITY, MASSFLOW, P A, MASS

        CONTAINS

!> \brief Writting TH/SURF tabs in restart file 

      !||====================================================================
      !||    thsurf_write_restart   ../common_source/modules/interfaces/th_surf_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                ../starter/source/restart/ddsplit/ddsplit.F
      !||    wrrestp                ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_i_c              ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE THSURF_WRITE_RESTART(TH_SURF,IFLAG)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
      INTEGER , INTENT(IN) :: IFLAG
C-----------------------------------------------
C         Writting TH/SURF tabs in restart file
C-----------------------------------------------

      CALL WRITE_I_C(TH_SURF%IOK,1)

      IF(TH_SURF%IOK > 0) THEN

        CALL WRITE_I_C(TH_SURF%NSURF,1)
C
        CALL WRITE_I_C(TH_SURF%PLOAD_FLAG,1)
        CALL WRITE_I_C(TH_SURF%LOADP_FLAG,1)
C
        CALL WRITE_I_C(TH_SURF%NSEGLOADP,1)
        CALL WRITE_I_C(TH_SURF%NSEGLOADPF,1)
        CALL WRITE_I_C(TH_SURF%NSEGLOADPB,1)
C

        IF(IFLAG == 2 ) THEN
          IF(TH_SURF%PLOAD_FLAG > 0) THEN
            CALL WRITE_I_C(TH_SURF%S_PLOAD_KSEGS,1)
            IF(TH_SURF%S_PLOAD_KSEGS > 0) CALL WRITE_I_C(TH_SURF%PLOAD_KSEGS,TH_SURF%S_PLOAD_KSEGS)
            CALL WRITE_I_C(TH_SURF%S_PLOAD_SEGS,1)
            IF(TH_SURF%S_PLOAD_SEGS  > 0) CALL WRITE_I_C(TH_SURF%PLOAD_SEGS,TH_SURF%S_PLOAD_SEGS)
          ENDIF
          IF(TH_SURF%LOADP_FLAG > 0) THEN
            CALL WRITE_I_C(TH_SURF%S_LOADP_KSEGS,1)
            IF(TH_SURF%S_LOADP_KSEGS > 0) CALL WRITE_I_C(TH_SURF%LOADP_KSEGS,TH_SURF%S_LOADP_KSEGS)
            CALL WRITE_I_C(TH_SURF%S_LOADP_SEGS,1)
            IF(TH_SURF%S_LOADP_SEGS > 0 ) CALL WRITE_I_C(TH_SURF%LOADP_SEGS,TH_SURF%S_LOADP_SEGS)
          ENDIF
        ENDIF

      ENDIF

C-----------------------------------------------
      RETURN
      END SUBROUTINE THSURF_WRITE_RESTART

!> \brief Reading TH/SURF tabs in restart file 

      !||====================================================================
      !||    thsurf_read_restart   ../common_source/modules/interfaces/th_surf_mod.F
      !||--- called by ------------------------------------------------------
      !||    rdresb                ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    read_i_c              ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    message_mod           ../engine/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE THSURF_READ_RESTART(TH_SURF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (TH_SURF_) , INTENT(INOUT) :: TH_SURF
C-----------------------------------------------
C         READ TH/SURF tabs in restart file
C-----------------------------------------------

      CALL READ_I_C(TH_SURF%IOK,1)

      IF(TH_SURF%IOK > 0) THEN

        CALL READ_I_C(TH_SURF%NSURF,1)
        CALL READ_I_C(TH_SURF%PLOAD_FLAG,1)
        CALL READ_I_C(TH_SURF%LOADP_FLAG,1)

        CALL READ_I_C(TH_SURF%NSEGLOADP,1)
        CALL READ_I_C(TH_SURF%NSEGLOADPF,1)
        CALL READ_I_C(TH_SURF%NSEGLOADPB,1)

        IF(TH_SURF%PLOAD_FLAG > 0) THEN
          CALL READ_I_C(TH_SURF%S_PLOAD_KSEGS,1)
          IF(TH_SURF%S_PLOAD_KSEGS > 0) THEN
            ALLOCATE(TH_SURF%PLOAD_KSEGS(TH_SURF%S_PLOAD_KSEGS))
            CALL READ_I_C(TH_SURF%PLOAD_KSEGS,TH_SURF%S_PLOAD_KSEGS)
          ENDIF
          CALL READ_I_C(TH_SURF%S_PLOAD_SEGS,1)
          IF(TH_SURF%S_PLOAD_SEGS > 0) THEN
            ALLOCATE(TH_SURF%PLOAD_SEGS(TH_SURF%S_PLOAD_SEGS))
            CALL READ_I_C(TH_SURF%PLOAD_SEGS,TH_SURF%S_PLOAD_SEGS)
          ENDIF
        ENDIF

        IF(TH_SURF%LOADP_FLAG > 0) THEN
          CALL READ_I_C(TH_SURF%S_LOADP_KSEGS,1)
          IF(TH_SURF%S_LOADP_KSEGS > 0) THEN
            ALLOCATE(TH_SURF%LOADP_KSEGS(TH_SURF%S_LOADP_KSEGS))
            CALL READ_I_C(TH_SURF%LOADP_KSEGS,TH_SURF%S_LOADP_KSEGS)
          ENDIF
          CALL READ_I_C(TH_SURF%S_LOADP_SEGS,1)
          IF(TH_SURF%S_LOADP_SEGS > 0) THEN
            ALLOCATE(TH_SURF%LOADP_SEGS(TH_SURF%S_LOADP_SEGS))
            CALL READ_I_C(TH_SURF%LOADP_SEGS,TH_SURF%S_LOADP_SEGS)
          ENDIF
        ENDIF
      ELSE ! initialization to 0
        TH_SURF%NSURF = 0
        TH_SURF%PLOAD_FLAG = 0
        TH_SURF%LOADP_FLAG = 0
        TH_SURF%NSEGLOADP = 0
        TH_SURF%NSEGLOADPF = 0
        TH_SURF%NSEGLOADPB = 0

      ENDIF

C-----------------------------------------------
      RETURN
      END  SUBROUTINE THSURF_READ_RESTART

C
      END MODULE TH_SURF_MOD
