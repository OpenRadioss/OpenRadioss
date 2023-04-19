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
Chd|  MATPARAM_R2R_VOID             source/elements/elbuf_init/matparam_r2r_void.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE MATPARAM_R2R_VOID(MATPARAM_TAB, MATPARAM_INI, NUMMAT0  ,NUMMAT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMMAT0,NUMMAT
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT ) :: MATPARAM_TAB
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT0) :: MATPARAM_INI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
c     Copy matparam from standard materials
c
      DO I = 1,NUMMAT0
        MATPARAM_TAB(I)%ILAW               = MATPARAM_INI(I)%ILAW
        MATPARAM_TAB(I)%MAT_ID             = MATPARAM_INI(I)%MAT_ID
        MATPARAM_TAB(I)%COMPRESSIBILITY    = MATPARAM_INI(I)%COMPRESSIBILITY
        MATPARAM_TAB(I)%SMSTR              = MATPARAM_INI(I)%SMSTR
        MATPARAM_TAB(I)%STRAIN_FORMULATION = MATPARAM_INI(I)%STRAIN_FORMULATION
        MATPARAM_TAB(I)%NTABLE             = MATPARAM_INI(I)%NTABLE           
      ENDDO
c
c     Fill matparam for additional rad2rad void materials
c
      DO I = NUMMAT0+1,NUMMAT
        MATPARAM_TAB(I)%ILAW               = 0
        MATPARAM_TAB(I)%MAT_ID             = 0
        MATPARAM_TAB(I)%COMPRESSIBILITY    = 0   
        MATPARAM_TAB(I)%SMSTR              = 0             
        MATPARAM_TAB(I)%STRAIN_FORMULATION = 0
        MATPARAM_TAB(I)%NTABLE             = 0            
      ENDDO
c-----------------------------------------------
      RETURN
      END
