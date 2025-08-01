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
!||    nolib_usermat99        ../engine/source/user_interface/nolib_usermat99.F
!||--- called by ------------------------------------------------------
!||    usermat_shell          ../engine/source/materials/mat_share/usermat_shell.F
!||    usermat_solid          ../engine/source/materials/mat_share/usermat_solid.F
!||--- uses       -----------------------------------------------------
!||    law_usersh             ../engine/source/user_interface/law_usersh.F
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE NOLIB_USERMAT99(ILAW_USER, IUSER_KEY)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine returns IUSER_KEY='USER**'  where ** is ILAW_USER
C
! example : ILAW_USER==3  --> IUSER_KEY='USER03'
! example : ILAW_USER==31 --> IUSER_KEY='USER31'
!
! valid range for ILAW_USER (precondition) : 
!    ILAW_USER \in [1,99]
!    (if ILAW_USER is out of range then IUSER_KEY is unchanged)
!
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE LAW_USERSH
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------      
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARLINE100),INTENT(INOUT) :: IUSER_KEY
      INTEGER,INTENT(IN) :: ILAW_USER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(ILAW_USER >= 1 .AND. ILAW_USER <= 9) THEN
        IUSER_KEY = 'USER00'       
        WRITE(IUSER_KEY(6:6),FMT='(I1)')ILAW_USER
      ELSEIF(ILAW_USER >= 10 .AND. ILAW_USER <= 99) THEN
        IUSER_KEY = 'USER00'
        WRITE(IUSER_KEY(5:6),FMT='(I2)')ILAW_USER
      ENDIF
C-----------------------------------------------
      RETURN
      END
