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
C
!||====================================================================
!||    mrdse2               ../starter/source/materials/mat/mat026/mrdse2.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos_sesame   ../starter/source/materials/eos/hm_read_eos_sesame.F
!||    hm_read_mat26        ../starter/source/materials/mat/mat026/hm_read_mat26.F
!||====================================================================
      SUBROUTINE MRDSE2(RR,NR,TT,NT,PP,EE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NR, NT
C     REAL
      my_real
     .   RR(NR), TT(NT), PP(NR,NT), EE(NR,NT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L
C     REAL
      my_real
     .   B1, B2
C
      READ(31,*)
      READ(31,'(5E15.0)')B1,B2,
     +  (RR(I),I=1,NR),(TT(J),J=1,NT),
     +  ((PP(K,L),K=1,NR),L=1,NT),
     +  ((EE(K,L),K=1,NR),L=1,NT)
      RETURN
      END
