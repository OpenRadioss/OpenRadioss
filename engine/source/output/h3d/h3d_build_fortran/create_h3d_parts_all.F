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
!||    create_h3d_parts_all   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
!||--- called by ------------------------------------------------------
!||    lech3d                 ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||--- calls      -----------------------------------------------------
!||    arret                  ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod                ../engine/share/modules/h3d_mod.F
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE CREATE_H3D_PARTS_ALL(H3D_DATA,IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_DATABASE) :: H3D_DATA
      INTEGER IPART(LIPART1,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100) :: CARTE,CARTE1
      INTEGER I,J,L,N_H3D_PART
C=========================================================================

      ALLOCATE(H3D_DATA%PARTS(1)%PART(NPART))
c
      DO I=1,NPART
        H3D_DATA%PARTS(1)%PART(I) = 1
c        print *,'I,PARTS(1)%PART(I)',I,H3D_DATA%PARTS(1)%PART(I)
      ENDDO


      RETURN
 999  print *,'error lecture'
      CALL ARRET(0)
      END
