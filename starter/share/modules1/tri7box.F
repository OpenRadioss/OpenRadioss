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
!||    tri7box          ../starter/share/modules1/tri7box.F
!||--- called by ------------------------------------------------------
!||    i11buc_vox1      ../starter/source/interfaces/inter3d1/i11buc1.F
!||    i11resetvoxel1   ../starter/source/interfaces/inter3d1/i11trivox1.F
!||    i11trivox1       ../starter/source/interfaces/inter3d1/i11trivox1.F
!||    i25buc_vox1      ../starter/source/interfaces/inter3d1/i25buc_vox1.F
!||    i25buce_edg      ../starter/source/interfaces/inter3d1/i25buce_edg.F
!||    i25cor3_e2s      ../starter/source/interfaces/inter3d1/i25cor3_e2s.F
!||    i25cor3e         ../starter/source/interfaces/inter3d1/i25cor3e.F
!||    i25cor3t         ../starter/source/interfaces/inter3d1/i25cor3t.F
!||    i25pen3_e2s      ../starter/source/interfaces/inter3d1/i25pen3_e2s.F
!||    i25pen3_edg      ../starter/source/interfaces/inter3d1/i25pen3_edg.F
!||    i25sto           ../starter/source/interfaces/inter3d1/i25sto.F
!||    i25sto_e2s       ../starter/source/interfaces/inter3d1/i25sto_e2s.F
!||    i25sto_edg       ../starter/source/interfaces/inter3d1/i25sto_edg.F
!||    i25trivox1       ../starter/source/interfaces/inter3d1/i25trivox1.F
!||    i25trivox_edg    ../starter/source/interfaces/inter3d1/i25trivox_edg.F
!||    i7buc_vox1       ../starter/source/interfaces/inter3d1/i7buc_vox1.F
!||    i7trivox1        ../starter/source/interfaces/inter3d1/i7trivox1.F
!||    spbuc31          ../starter/source/elements/sph/spbuc31.F
!||    sptrivox         ../starter/source/elements/sph/sptrivox.F
!||====================================================================
      MODULE TRI7BOX
C Temporary array for VOXEL SORTING
        INTEGER , DIMENSION(:), ALLOCATABLE :: NEXT_NOD
        INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_ELEM,CHAIN_NEXT
        INTEGER , DIMENSION(:), ALLOCATABLE :: CHAIN_LAST 
        INTEGER LVOXEL
        PARAMETER(LVOXEL = 8000000)
        INTEGER INIVOXEL, VOXEL1(LVOXEL)
        INTEGER LRVOXEL
        PARAMETER(LRVOXEL = 31)
        INTEGER CRVOXEL(0:LRVOXEL,0:LRVOXEL)
        DATA INIVOXEL /1/
      END MODULE TRI7BOX
