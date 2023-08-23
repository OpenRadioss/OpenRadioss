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
      SUBROUTINE WRITE_MATPARAM(MAT_ELEM,LEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
      USE NAMES_AND_TITLES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER         ,INTENT(INOUT) :: LEN
      TYPE(MAT_ELEM_) ,INTENT(IN)    :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,NUMMAT,IMAT,NUPARAM,NIPARAM,NUMTABL,NFAIL,
     .           IAD,NFIX,LENI,LENR
      INTEGER ,DIMENSION(NCHARTITLE)     :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
C=======================================================================
      NFIX = 19
      NUMMAT = MAT_ELEM%NUMMAT
      LENI   = NFIX*NUMMAT
      ALLOCATE (IBUF(LENI) )
c
      IAD = 0
      DO IMAT = 1,NUMMAT
        IBUF(IAD+ 1) = MAT_ELEM%MAT_PARAM(IMAT)%ILAW 
        IBUF(IAD+ 2) = MAT_ELEM%MAT_PARAM(IMAT)%MAT_ID
        IBUF(IAD+ 3) = MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM
        IBUF(IAD+ 4) = MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM
        IBUF(IAD+ 5) = MAT_ELEM%MAT_PARAM(IMAT)%NFUNC
        IBUF(IAD+ 6) = MAT_ELEM%MAT_PARAM(IMAT)%NTABLE
        IBUF(IAD+ 7) = MAT_ELEM%MAT_PARAM(IMAT)%NSUBMAT
        IBUF(IAD+ 8) = MAT_ELEM%MAT_PARAM(IMAT)%NFAIL
        IBUF(IAD+ 9) = MAT_ELEM%MAT_PARAM(IMAT)%IVISC
        IBUF(IAD+10) = MAT_ELEM%MAT_PARAM(IMAT)%IEOS
        IBUF(IAD+11) = MAT_ELEM%MAT_PARAM(IMAT)%ITHERM
c
        IBUF(IAD+12) = MAT_ELEM%MAT_PARAM(IMAT)%COMPRESSIBILITY
        IBUF(IAD+13) = MAT_ELEM%MAT_PARAM(IMAT)%SMSTR
        IBUF(IAD+14) = MAT_ELEM%MAT_PARAM(IMAT)%STRAIN_FORMULATION
        IBUF(IAD+15) = MAT_ELEM%MAT_PARAM(IMAT)%IPRES
        IBUF(IAD+16) = MAT_ELEM%MAT_PARAM(IMAT)%ORTHOTROPY
c
        IBUF(IAD+17) = MAT_ELEM%MAT_PARAM(IMAT)%NLOC
        IBUF(IAD+18) = MAT_ELEM%MAT_PARAM(IMAT)%IFAILWAVE
        IBUF(IAD+19) = MAT_ELEM%MAT_PARAM(IMAT)%IXFEM
c
        IAD = IAD + NFIX
      END DO
c
      CALL WRITE_I_C(IBUF,LENI)
      LEN = LEN + LENI
      DEALLOCATE(IBUF)
c      
c     write material title
c
      DO IMAT = 1,NUMMAT      
        DO I=1,NCHARTITLE
          NAME(I) = ICHAR(MAT_ELEM%MAT_PARAM(IMAT)%TITLE(I:I))
        END DO
        CALL WRITE_C_C(NAME,NCHARTITLE)
      END DO
c           
c     write material parameter array
c
      DO IMAT = 1,NUMMAT      
        NUPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM
        NIPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM
        IF (NUPARAM > 0) THEN
          CALL WRITE_DB(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM ,NUPARAM)
        END IF      
        IF (NIPARAM > 0) THEN
          CALL WRITE_I_C(MAT_ELEM%MAT_PARAM(IMAT)%IPARAM ,NIPARAM)
        END IF      
        LEN = LEN + NUPARAM + NIPARAM
      END DO
c
c     write function tables
c      
      DO IMAT = 1,NUMMAT
        NUMTABL = MAT_ELEM%MAT_PARAM(IMAT)%NTABLE
        IF (NUMTABL > 0) THEN
          CALL TABLE_WRESTI_MAT(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL, LENI)
          CALL TABLE_WRESTR_MAT(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL, LENR)
          LEN = LEN + LENI + LENR
        END IF
      END DO      
c           
c     write viscosity model parameters
c
      DO IMAT = 1,NUMMAT
        IF (MAT_ELEM%MAT_PARAM(IMAT)%IVISC > 0) THEN
          CALL WRITE_VISCPARAM(MAT_ELEM%MAT_PARAM(IMAT)%VISC,LEN)
        END IF
      END DO      
c      
c     write parameters of failure models per material
c          
      DO IMAT = 1,NUMMAT
        NFAIL = MAT_ELEM%MAT_PARAM(IMAT)%NFAIL
        IF (NFAIL > 0) THEN
          DO I = 1,NFAIL
            CALL WRITE_FAILPARAM(MAT_ELEM%MAT_PARAM(IMAT)%FAIL,LEN)
          END DO      
        END IF
      END DO      
c-----------
      RETURN
      END
