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
Chd|  UPDFAIL                       source/materials/updfail.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FAIL_FUN2SYS                  source/materials/tools/fail_fun2sys.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE UPDFAIL(MAT_PARAM ,NUMMAT ,NFUNCT ,NTABLE ,FUNC_ID ,TABLE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT   ! total number of material models 
      INTEGER ,INTENT(IN) :: NFUNCT   ! total number of system functions   
      INTEGER ,INTENT(IN) :: NTABLE   ! total number of system function tables   
      INTEGER ,DIMENSION(NFUNCT)      ,INTENT(IN) :: FUNC_ID
      TYPE(TTABLE), DIMENSION(NTABLE) ,INTENT(IN) :: TABLE
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(INOUT), TARGET :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT,IR,NFAIL
C=======================================================================
      DO IMAT=1,NUMMAT
        NFAIL  = MAT_PARAM(IMAT)%NFAIL
        IF (NFAIL > 0) THEN
          DO IR = 1,NFAIL
            IF (MAT_PARAM(IMAT)%FAIL(IR)%NFUNC > 0) THEN
              CALL FAIL_FUN2SYS(MAT_PARAM(IMAT)%FAIL(IR) ,
     .                          MAT_PARAM(IMAT)%TITLE    ,
     .                          MAT_PARAM(IMAT)%MAT_ID   ,
     .                          NFUNCT  ,FUNC_ID  )
            END IF
            IF (MAT_PARAM(IMAT)%FAIL(IR)%NTABLE > 0) THEN
              CALL MATTAB_USR2SYS(MAT_PARAM(IMAT)%TITLE    ,
     .                            MAT_PARAM(IMAT)%MAT_ID, 
     .                            NTABLE ,TABLE ,
     .                            MAT_PARAM(IMAT)%FAIL(IR)%NTABLE ,
     .                            MAT_PARAM(IMAT)%FAIL(IR)%TABLE  )
            END IF
          ENDDO
        ENDIF
      ENDDO
c-----------
      RETURN
      END
