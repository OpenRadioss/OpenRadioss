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
Chd|  WRITE_ELGROUP_PARAM           source/restart/ddsplit/write_elgroup_param.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_ELGROUP_PARAM(GROUP_PARAM_TAB,IPARG,PROC,NGROUP_L,LEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NGROUP_L, LEN
      INTEGER IPARG(NPARG,*)
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP) :: GROUP_PARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,NG_L
      my_real, DIMENSION(:,:), ALLOCATABLE :: RBUF
C=======================================================================
      LEN = 2
      ALLOCATE(RBUF(NGROUP_L,LEN))
      RBUF(:,:) = ZERO
c
      NG_L = 0
      DO NG = 1, NGROUP
        IF (IPARG(32,NG) == PROC) THEN
          NG_L = NG_L + 1
          RBUF(NG_L,1) = GROUP_PARAM_TAB(NG)%VISC_DM
          RBUF(NG_L,2) = GROUP_PARAM_TAB(NG)%VISC_DN
        ENDIF
      ENDDO
c
      CALL WRITE_DB(RBUF,LEN*NGROUP_L)
      LEN = LEN*NGROUP_L
      DEALLOCATE( RBUF )
c-----------
      RETURN
      END
