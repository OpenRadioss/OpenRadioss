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
!||    copy_elbuf     ../engine/source/elements/elbuf/copy_elbuf.F
!||--- called by ------------------------------------------------------
!||    cp_impbuf      ../engine/source/implicit/produt_v.F
!||--- calls      -----------------------------------------------------
!||    copy_elbuf_1   ../engine/source/elements/elbuf/copy_elbuf_1.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE COPY_ELBUF(ELBUF_SRC,ELBUF_TGT,IPARG,NGROUP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUP
      INTEGER IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_SRC,ELBUF_TGT
      TARGET :: ELBUF_SRC,ELBUF_TGT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, 
     . NPGGTYPNLOC
      INTEGER ! global variables 
     . G_POSZXYZ,
     . G_VR_REPCVTN,
     . G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ
      INTEGER  ! layer variables
     . LY_UELR1PG
      INTEGER  ! local variables
     . L_VKVLADB
c-------------------------------------------------
c      copy the element buffer : ELBUF_SRC -> ELBUF_TGT
c      target must be allocated
C=======================================================================
      DO NG = 1,NGROUP      
        CALL COPY_ELBUF_1(ELBUF_SRC,ELBUF_TGT,IPARG,NGROUP,NG)
      ENDDO   ! NG = 1,NGROUP
C-----------------------------------------------
      RETURN
      END      

