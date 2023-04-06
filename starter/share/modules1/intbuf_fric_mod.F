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
Chd|  INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|        I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25STI_EDG                    source/interfaces/inter3d1/i25sti_edg.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        ININTR_ORTHDIRFRIC            source/interfaces/interf1/inintr_orthdirfric.F
Chd|        INTBUF_FRIC_COPY              source/interfaces/interf1/intbuf_fric_copy.F
Chd|        INTBUF_FRIC_INI_STARTER       source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        INTBUF_INI_STARTER            source/interfaces/intbuf/intbuf_ini_starter.F
Chd|        INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|        INTFRIC_WRESTI                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        INTFRIC_WRESTR                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_FRICTION           source/output/qaprint/st_qaprint_friction.F
Chd|        TRIINTFRIC                    source/interfaces/interf1/trintfric.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTBUF_FRIC_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE INTBUF_FRIC_STRUCT_  
      INTEGER ::    NSETPRTS       ! number of couple of parts 
      INTEGER ::    FRICMOD        ! the friction model
      INTEGER ::    FRICFORM        ! the friction formulation ( viscous or incremental)
      INTEGER ::    IFFILTER       !Friction filtering flag 
      INTEGER ::    S_TABPARTS_FRIC  ! number of parts  
      INTEGER ::    IORTHFRIC          ! Flag for orthotropic friction 0 isotropic / 1 orthotropic like main material / 2 orthotropic and orthotropy axes are defined in friction model
      my_real ::    XFILTR_FRIC        !Filtering coefficient

      INTEGER, DIMENSION(:), ALLOCATABLE :: TABCOUPLEPARTS_FRIC    ! Table of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: TABPARTS_FRIC          ! Table of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADPARTS_FRIC           ! Table of adress of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRICORTH              ! Table of Orthotropic type of couple of parts 
      my_real, DIMENSION(:), ALLOCATABLE :: TABCOEF_FRIC           ! Table of friction coefficients

      END TYPE INTBUF_FRIC_STRUCT_

      END MODULE INTBUF_FRIC_MOD
