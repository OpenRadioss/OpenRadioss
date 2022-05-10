Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  XFEM2VARS_MOD                 share/modules/xfem2vars_mod.F 
Chd|-- called by -----------
Chd|        CRKXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      MODULE XFEM2VARS_MOD
C-----------------------------------------------
c     Global XFEM variables and structures declarations 
C=======================================================================
      USE CRACKXFEM_MOD
c----------------------
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: INDX_CRK
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: XEDGE4N
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: XEDGE3N
c
      TYPE (XFEM_EDGE_)   ,DIMENSION(:), ALLOCATABLE :: CRKEDGE
c
      TYPE (XFEM_SKY_)    ,DIMENSION(:), ALLOCATABLE :: CRKSKY

c----------------------
!      TYPE(XFEM_LVSET_) , DIMENSION(:), ALLOCATABLE :: CRKLVSET

!      TYPE(XFEM_AVX_)   , DIMENSION(:), ALLOCATABLE :: CRKAVX

!      TYPE (XFEM_NODES_), DIMENSION(:), ALLOCATABLE :: CRKNOD
 
!      TYPE (XFEM_SHELL_), DIMENSION(:), ALLOCATABLE :: CRKSHELL

C NCRKPART & IND_CRK are global values shared by all processors
!      INTEGER NCRKPART -> com_xfem1.inc
c-----------------------------
      END MODULE XFEM2VARS_MOD
