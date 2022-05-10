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
Chd|  READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        TABLE_RRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_RRESTR_MAT              source/tools/curve/table_tools.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE READ_MATPARAM(MATPARAM_TAB,NUMMAT)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NUMMAT
      TYPE (MATPARAM_STRUCT_), DIMENSION(NUMMAT), TARGET :: MATPARAM_TAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IMAT,NUMTABL,IAD,LEN
      my_real :: HEAD
      my_real, DIMENSION(:), ALLOCATABLE :: RBUF
      TYPE (MATPARAM_STRUCT_) , POINTER  :: MATPARAM
c=======================================================================
c     read MATPARAM data
c
      CALL READ_DB(HEAD, 1)
      LEN = NINT(HEAD)
      ALLOCATE (RBUF(LEN) )
      CALL READ_DB(RBUF, LEN)
c      
      IAD = 0
      DO IMAT=1,NUMMAT
        MATPARAM => MATPARAM_TAB(IMAT)
        IAD = IAD+1
          MATPARAM%ILAW = NINT(RBUF(IAD))
        IAD = IAD+1
          MATPARAM%MAT_ID = NINT(RBUF(IAD))
        IAD = IAD+1
          MATPARAM%COMPRESSIBILITY = NINT(RBUF(IAD))
        IAD = IAD+1
          MATPARAM%SMSTR = NINT(RBUF(IAD))
        IAD = IAD+1
          MATPARAM%STRAIN_FORMULATION = NINT(RBUF(IAD))
        IAD = IAD+1
          MATPARAM%NTABLE = NINT(RBUF(IAD))
      END DO
c
      DEALLOCATE( RBUF )
c
c     read material law tables if necessary
      
      DO IMAT=1,NUMMAT
        MATPARAM => MATPARAM_TAB(IMAT)
        NUMTABL  = MATPARAM%NTABLE
        
        IF (NUMTABL > 0) THEN
          ALLOCATE (MATPARAM%TABLE(NUMTABL))
          CALL TABLE_RRESTI_MAT(MATPARAM%TABLE, NUMTABL)
          CALL TABLE_RRESTR_MAT(MATPARAM%TABLE, NUMTABL)
        END IF
      END DO      
c-----------
      RETURN
      END
