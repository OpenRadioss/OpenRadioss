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

!> \brief OPTION /TH/SURF outputs of Pressure and Area needed Tabs 

Chd|====================================================================
Chd|  PARAMETERS_MOD                modules/interfaces/parameters_mod.F
Chd|-- called by -----------
Chd|        INTERFACES_MOD                common_source/modules/interfaces/interfaces_mod.F
Chd|        HM_READ_INTER_TYPE24          starter/source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          starter/source/interfaces/int25/hm_read_inter_type25.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        WRCOMR                        starter/source/restart/ddsplit/wrcomm.F
Chd|        I24COR3                       engine/source/interfaces/int24/i24cor3.F
Chd|        I25COR3E                      engine/source/interfaces/int25/i25cor3e.F
Chd|        I25COR3_3                     engine/source/interfaces/int25/i25cor3.F
Chd|        I25COR3_E2S                   engine/source/interfaces/int25/i25cor3_e2s.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        SPMD_INITFI                   engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|-- calls ---------------
Chd|====================================================================
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

          END TYPE TH_SURF_

        CONTAINS

!> \brief Writting TH/SURF tabs in restart file 

Chd|====================================================================
Chd|  PBLAST_WRITE_STARTER          modules/loads/pblast_mod.F    
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|        WRITE_I_C                     starter/source/output/tools/write_routines.c
Chd|        WRITE_I_C                     engine/source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   starter/share/message_module/message_mod.F
Chd|        MESSAGE_MOD                   engine/share/message_module/message_mod.F
Chd|====================================================================
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

Chd|====================================================================
Chd|  PBLAST_WRITE_STARTER          modules/loads/pblast_mod.F    
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|        WRITE_I_C                     starter/source/output/tools/write_routines.c
Chd|        WRITE_I_C                     engine/source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   starter/share/message_module/message_mod.F
Chd|        MESSAGE_MOD                   engine/share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE THSURF_READ_RESTART(TH_SURF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
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
            MY_ALLOCATE(TH_SURF%PLOAD_KSEGS,TH_SURF%S_PLOAD_KSEGS)
            CALL READ_I_C(TH_SURF%PLOAD_KSEGS,TH_SURF%S_PLOAD_KSEGS)
          ENDIF
          CALL READ_I_C(TH_SURF%S_PLOAD_SEGS,1)
          IF(TH_SURF%S_PLOAD_SEGS > 0) THEN
            MY_ALLOCATE(TH_SURF%PLOAD_SEGS,TH_SURF%S_PLOAD_SEGS)
            CALL READ_I_C(TH_SURF%PLOAD_SEGS,TH_SURF%S_PLOAD_SEGS)
          ENDIF
        ENDIF

        IF(TH_SURF%LOADP_FLAG > 0) THEN
          CALL READ_I_C(TH_SURF%S_LOADP_KSEGS,1)
          IF(TH_SURF%S_LOADP_KSEGS > 0) THEN
            MY_ALLOCATE(TH_SURF%LOADP_KSEGS,TH_SURF%S_LOADP_KSEGS)
            CALL READ_I_C(TH_SURF%LOADP_KSEGS,TH_SURF%S_LOADP_KSEGS)
          ENDIF
          CALL READ_I_C(TH_SURF%S_LOADP_SEGS,1)
          IF(TH_SURF%S_LOADP_SEGS > 0) THEN
            MY_ALLOCATE(TH_SURF%LOADP_SEGS,TH_SURF%S_LOADP_SEGS)
            CALL READ_I_C(TH_SURF%LOADP_SEGS,TH_SURF%S_LOADP_SEGS)
          ENDIF
        ENDIF

      ENDIF

C-----------------------------------------------
      RETURN
      END  SUBROUTINE THSURF_READ_RESTART

C
      END MODULE TH_SURF_MOD
