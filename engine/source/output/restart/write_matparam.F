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
Chd|  WRITE_MATPARAM                source/output/restart/write_matparam.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        TABLE_WRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_MATPARAM(MATPARAM_TAB,NUMMAT)
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
      INTEGER , INTENT(IN)    :: NUMMAT
      TYPE(MATPARAM_STRUCT_) , DIMENSION(NUMMAT), TARGET :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT,IAD,LEN,NPARAM,NUMTABL
      my_real, DIMENSION(:), ALLOCATABLE :: RBUF
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
C=======================================================================
      NPARAM = 6
      LEN = NPARAM*NUMMAT + 1
      ALLOCATE (RBUF(LEN) )
c
      IAD = 1
      RBUF(IAD) = LEN-1
c
      DO IMAT=1,NUMMAT
        MATPARAM => MATPARAM_TAB(IMAT)
        IAD = IAD+1
          RBUF(IAD) = MATPARAM%ILAW
        IAD = IAD+1
          RBUF(IAD) = MATPARAM%MAT_ID
        IAD = IAD+1
          RBUF(IAD) = MATPARAM%COMPRESSIBILITY
        IAD = IAD+1
          RBUF(IAD) = MATPARAM%SMSTR
        IAD = IAD+1
          RBUF(IAD) = MATPARAM%STRAIN_FORMULATION
        IAD = IAD+1
          RBUF(IAD) = MATPARAM%NTABLE
      END DO
c
      CALL WRITE_DB(RBUF,LEN)
c
      DEALLOCATE( RBUF )

c     write material law tables if necessary

      DO IMAT=1,NUMMAT
        MATPARAM => MATPARAM_TAB(IMAT)
        NUMTABL  = MATPARAM%NTABLE

        IF (NUMTABL > 0) THEN
          CALL TABLE_WRESTI_MAT(MATPARAM%TABLE, NUMTABL)
          CALL TABLE_WRESTR_MAT(MATPARAM%TABLE, NUMTABL)
        END IF
      END DO
c-----------
      RETURN
      END
