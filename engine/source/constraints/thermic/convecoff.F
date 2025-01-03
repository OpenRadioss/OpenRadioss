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
      !||    convecoff        ../engine/source/constraints/thermic/convecoff.F
      !||--- called by ------------------------------------------------------
      !||    desacti          ../engine/source/elements/desacti.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||====================================================================
      SUBROUTINE CONVECOFF(IBCV, FCONV, IPARG, IGROUPS, ELBUF_TAB, GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type (glob_therm_) ,intent(inout)   :: glob_therm
      INTEGER IBCV(GLOB_THERM%NICONV,*), IPARG(NPARG,*), IGROUPS(NUMELS)
C
      my_real FCONV(GLOB_THERM%LFACTHER,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JJ, NG, NEL, MLW, NFT
      my_real, DIMENSION(:), POINTER :: OFFG
C======================================================================|
C------------------------------------------------
C   ACTIVATION/DESACTIVATION DE LA CONVECTION
C------------------------------------------------
      DO N=1,GLOB_THERM%NUMCONV
         IF (IBCV(7,N) == 1) THEN
C        ELEMENTS SOLIDES
           JJ = IBCV(8,N)
           NG = IGROUPS(JJ)
           MLW= IPARG(1,NG)
           NEL= IPARG(2,NG)
           NFT= IPARG(3,NG)
           IF (MLW == 0 .OR. MLW == 13) CYCLE         
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           FCONV(6,N)=OFFG(IBCV(GLOB_THERM%NICONV,N))
         ENDIF
      ENDDO
c-----------
      RETURN
      END
