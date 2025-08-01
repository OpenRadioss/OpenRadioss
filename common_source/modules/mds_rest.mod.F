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
!||    mds_restart_mod   ../common_source/modules/mds_rest.mod.F
!||--- called by ------------------------------------------------------
!||    radioss2          ../engine/source/engine/radioss2.F
!||    resol             ../engine/source/engine/resol.F
!||====================================================================
      MODULE MDS_RESTART_MOD 
         INTEGER MDS_NMAT
         INTEGER, DIMENSION(:), ALLOCATABLE ::   MDS_MATID
         INTEGER, DIMENSION(:), ALLOCATABLE :: MDS_NDEPSVAR
         CHARACTER (LEN=64), DIMENSION(:,:), ALLOCATABLE ::  MDS_LABEL
         CHARACTER, DIMENSION(:,:,:), ALLOCATABLE :: MDS_FILES
      END MODULE
