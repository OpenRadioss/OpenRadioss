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
Chd|  NLOCAL_INCR                   source/assembly/nlocal_incr.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE NLOCAL_INCR(NLOC_DMG, NODFT   ,NODLT)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE NLOCAL_REG_MOD
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT
      TYPE(NLOCAL_STR_), TARGET :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N,NDDL,NPOS
      my_real, POINTER, DIMENSION(:) :: 
     .   DNL,UNL,VNL
C-----------------------------------------------
c     calcul increment et nouvelle valeur de la variable non locale
C=======================================================================
      ! Recovering increments, cumulated and velocities
      DNL => NLOC_DMG%DNL(1:NLOC_DMG%L_NLOC)
      UNL => NLOC_DMG%UNL(1:NLOC_DMG%L_NLOC)
      VNL => NLOC_DMG%VNL(1:NLOC_DMG%L_NLOC)
#include      "vectorize.inc"
      ! Loop over current nodes
      DO N = NODFT,NODLT
        ! Position of the first additional dof
        NPOS = NLOC_DMG%POSI(N)
        ! Number of additional degrees of freedom
        NDDL = NLOC_DMG%POSI(N+1) - NPOS
        ! Updating increment and velocities
        DNL(NPOS:NPOS+NDDL-1) = DT2*VNL(NPOS:NPOS+NDDL-1)
        UNL(NPOS:NPOS+NDDL-1) = UNL(NPOS:NPOS+NDDL-1) + DNL(NPOS:NPOS+NDDL-1)
      ENDDO
c-----------
      RETURN
      END
      
