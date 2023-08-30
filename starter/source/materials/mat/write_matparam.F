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
Chd|  WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        TABLE_WRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|        WRITE_C_C                     source/output/tools/write_routines.c
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRITE_VISCPARAM               source/materials/mat/write_viscparam.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        NAMES_AND_TITLES_MOD          ../common_source/modules/names_and_titles_mod.F
Chd|        VISC_PARAM_MOD                ../common_source/modules/mat_elem/visc_param_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_MATPARAM(MAT_PARAM,NUMMAT,LEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE VISC_PARAM_MOD
      USE NAMES_AND_TITLES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NUMMAT
      INTEGER ,INTENT(INOUT) :: LEN
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IMAT,IAD,NFIX,NUPARAM,NIPARAM,NUMTABL,LENI,LENR
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
C=======================================================================
      NFIX = 16
      LEN = NFIX*NUMMAT + 1
      ALLOCATE (IBUF(LEN) )
c
      IAD = 1
      IBUF(IAD) = LEN-1
c
      DO IMAT=1,NUMMAT
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%ILAW 
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%MAT_ID
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%NUPARAM
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%NIPARAM
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%NFUNC
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%NTABLE
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%NSUBMAT
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%NFAIL
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%IVISC
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%IEOS
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%ITHERM
c
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%COMPRESSIBILITY
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%SMSTR
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%STRAIN_FORMULATION
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%IPRES
        IAD = IAD+1
          IBUF(IAD) = MAT_PARAM(IMAT)%ORTHOTROPY
c
      END DO
c
      CALL WRITE_I_C(IBUF,LEN)
      DEALLOCATE(IBUF)
      
c     write material title

      DO IMAT=1,NUMMAT      
        DO I=1,NCHARTITLE
          NAME(I) = ICHAR(MAT_PARAM(IMAT)%TITLE(I:I))
        END DO
        CALL WRITE_C_C(NAME,NCHARTITLE)
      END DO
c           
c     write material parameter array

      DO IMAT=1,NUMMAT      
        NUPARAM = MAT_PARAM(IMAT)%NUPARAM
        NIPARAM = MAT_PARAM(IMAT)%NIPARAM
        IF (NUPARAM > 0) THEN
          CALL WRITE_DB(MAT_PARAM(IMAT)%UPARAM ,NUPARAM)
        END IF      
        IF (NIPARAM > 0) THEN
          CALL WRITE_I_C(MAT_PARAM(IMAT)%IPARAM ,NIPARAM)
        END IF      
        LEN = LEN + NUPARAM + NIPARAM
      END DO
      
c     write material law tables if necessary
      
      DO IMAT=1,NUMMAT
        NUMTABL  = MAT_PARAM(IMAT)%NTABLE
        IF (NUMTABL > 0) THEN
          CALL TABLE_WRESTI_MAT(MAT_PARAM(IMAT)%TABLE, NUMTABL, LENI)
          CALL TABLE_WRESTR_MAT(MAT_PARAM(IMAT)%TABLE, NUMTABL, LENR)
          LEN = LEN + LENI + LENR
        END IF
      END DO      
c           
c     write viscosity model parameters
c
      DO IMAT=1,NUMMAT
        IF (MAT_PARAM(IMAT)%IVISC > 0) THEN
          CALL WRITE_VISCPARAM(MAT_PARAM(IMAT)%VISC,LEN)
        END IF
      END DO      
c-----------
      RETURN
      END
