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
!||    icontact_mod   ../engine/share/modules/icontact_mod.F
!||--- called by ------------------------------------------------------
!||    i17buce        ../engine/source/interfaces/int17/i17buce.F
!||    i17buce_pena   ../engine/source/interfaces/int17/i17buce.F
!||    i17for3        ../engine/source/interfaces/int17/i17for3.F
!||    i17lll4_pena   ../engine/source/interfaces/int17/i17for3.F
!||    i17sto         ../engine/source/interfaces/int17/i17tri.F
!||    i20for3c       ../engine/source/interfaces/int20/i20for3.F
!||    resol          ../engine/source/engine/resol.F
!||====================================================================
      MODULE ICONTACT_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: ADCHAINE
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: CHAINE
      INTEGER  MX_AD, NME17, NSKYI17, NRSKYI17
      INTEGER       ,DIMENSION(:), ALLOCATABLE :: ISKYI17, IRSKYI17
      my_real,
     .         DIMENSION(:,:), ALLOCATABLE :: FSKYI17, FRSKYI17
      my_real,
     .         DIMENSION(:), ALLOCATABLE :: EMINX
C-----------------------------------------------
      END MODULE ICONTACT_MOD
