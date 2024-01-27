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
Chd|  NAMES_AND_TITLES_MOD          modules/names_and_titles_mod.F
Chd|-- called by -----------
Chd|        FAIL_PARAM_MOD                common_source/modules/mat_elem/fail_param_mod.F
Chd|        MATPARAM_DEF_MOD              common_source/modules/mat_elem/matparam_def_mod.F
Chd|        VISC_PARAM_MOD                common_source/modules/mat_elem/visc_param_mod.F
Chd|        WRITE_FAILPARAM               starter/source/materials/fail/write_failparam.F
Chd|        WRITE_MATPARAM                starter/source/materials/mat/write_matparam.F
Chd|        WRITE_VISCPARAM               starter/source/materials/mat/write_viscparam.F
Chd|        READ_FAILPARAM                engine/source/output/restart/read_failparam.F
Chd|        READ_MATPARAM                 engine/source/output/restart/read_matparam.F
Chd|        READ_VISCPARAM                engine/source/output/restart/read_viscparam.F
Chd|        WRITE_FAILPARAM               engine/source/output/restart/write_failparam.F
Chd|        WRITE_MATPARAM                engine/source/output/restart/write_matparam.F
Chd|        WRITE_VISCPARAM               engine/source/output/restart/write_viscparam.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE NAMES_AND_TITLES_MOD
C-----------------------------------------------
            ! Global parameters for strings
            PARAMETER (NCHARKEY      = 20)
            PARAMETER (NCHARLINE     = 100)
            PARAMETER (NCHARTITLE    = 500)
            PARAMETER (LINE120       = 120)
            PARAMETER (NCHARFIELD    = 20)

            ! Parameters for NAMES_AND_TITLES type
            PARAMETER (LTITLE        = 80)
            PARAMETER (LROOTNAME     = 80)
            PARAMETER (LCHRUN        = 4)
            PARAMETER (LCHRUN0       = 2)
            PARAMETER (LPATH         = 2048)
            PARAMETER (LVERS         = 32)
            PARAMETER (LIMVW_REFPATH = 100)

C-----------------------------------------------
            TYPE NAMES_AND_TITLES_
                CHARACTER(LEN=LTITLE)        :: TITLE           ! Title of Model from /TITLE option.
                CHARACTER(LEN=LROOTNAME)     :: ROOTNAME        ! ROOTNAME : Name of Input Deck
                CHARACTER(LEN=LCHRUN)        :: CHRUN           ! Run Number : 0 in starter [1-N] in Engine
                CHARACTER(LEN=LCHRUN0)       :: CHRUN0          ! Run Number0 
                CHARACTER(LEN=LVERS)         :: VERS            ! Radioss version number
                CHARACTER(LEN=LIMVW_REFPATH) :: IMVW_REFPATH    ! Refsta Directory
                INTEGER                      :: ROOTNAME_LENGTH ! LEN of ROOTNAME as in input
            END TYPE NAMES_AND_TITLES_


      CONTAINS
Chd|====================================================================
Chd|  NAMES_AND_TITLE_INIT          modules/names_and_titles_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE NAMES_AND_TITLE_INIT(NAMES_AND_TITLES)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          TYPE(NAMES_AND_TITLES_),INTENT(OUT) :: NAMES_AND_TITLES
C-----------------------------------------------
          NAMES_AND_TITLES%TITLE=' '
          NAMES_AND_TITLES%ROOTNAME=' '
          NAMES_AND_TITLES%ROOTNAME_LENGTH=0
          NAMES_AND_TITLES%CHRUN=' '
          NAMES_AND_TITLES%CHRUN0=' '
          NAMES_AND_TITLES%VERS=' '
        END SUBROUTINE NAMES_AND_TITLE_INIT

      END MODULE NAMES_AND_TITLES_MOD

      