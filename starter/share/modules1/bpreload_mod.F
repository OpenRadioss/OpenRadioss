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
      !||    bpreload_mod            ../starter/share/modules1/bpreload_mod.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_preload_axial   ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    initia                  ../starter/source/elements/initia/initia.F
      !||    lectur                  ../starter/source/starter/lectur.F
      !||    pgrtails                ../starter/source/elements/beam/pgrtails.F
      !||    pinit3                  ../starter/source/elements/beam/pinit3.F
      !||    rgrtails                ../starter/source/elements/spring/rgrtails.F
      !||    rinit3                  ../starter/source/elements/spring/rinit3.F
      !||    s10init3                ../starter/source/elements/solid/solide10/s10init3.F
      !||    s4init3                 ../starter/source/elements/solid/solide4/s4init3.F
      !||    s8zinit3                ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    sinit3                  ../starter/source/elements/solid/solide/sinit3.F
      !||    tgrtails                ../starter/source/elements/truss/tgrtails.F
      !||    tinit3                  ../starter/source/elements/truss/tinit3.F
      !||====================================================================
      MODULE BPRELOAD_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      INTEGER,
     .   DIMENSION(:),ALLOCATABLE :: IPRELOAD
      INTEGER,
     .   DIMENSION(:),ALLOCATABLE :: IFLAG_BPRELOAD
      my_real,
     .   DIMENSION(:),ALLOCATABLE :: PRELOAD
      my_real,
     .   DIMENSION(:),ALLOCATABLE :: VPRELOAD
      INTEGER,
     .   DIMENSION(:),ALLOCATABLE :: IPRELOAD1
!--------------
      TYPE PREL1D_
        INTEGER   :: sens_id, fun_id
        my_real   :: preload, damp
      END TYPE PREL1D_

C-----------------------------------------------
      END MODULE BPRELOAD_MOD
