Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd/        HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_bucher/hm_read_fail_tbutcher.F
Chd|====================================================================
Chd|  FAIL_INIT                     source/materials/fail/fail_init.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_INIT(MAT_PARAM ,NUMMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT,IFL,NFAIL
c======================================================================- 
      DO IMAT = 1,NUMMAT-1
        NFAIL = MAT_PARAM(IMAT)%NFAIL
        ALLOCATE (MAT_PARAM(IMAT)%FAIL(NFAIL))
        IF (NFAIL > 0) THEN
          DO IFL = 1,NFAIL
            MAT_PARAM(IMAT)%FAIL(IFL)%KEYWORD = ' '
            MAT_PARAM(IMAT)%FAIL(IFL)%IRUPT   = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%FAIL_ID = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%NUPARAM = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%NIPARAM = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%NUVAR   = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%NFUNC   = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%NTABLE  = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%NMOD    = 0
c
            MAT_PARAM(IMAT)%FAIL(IFL)%FAIL_IP = 0
            MAT_PARAM(IMAT)%FAIL(IFL)%PTHK    = ONE
          END DO
        END IF      
      END DO
c-----------       
      RETURN
      END
