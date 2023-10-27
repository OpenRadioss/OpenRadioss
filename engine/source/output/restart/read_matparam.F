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
Chd|  READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_C_C                      source/output/tools/sortie_c.c
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_FAILPARAM                source/output/restart/read_failparam.F
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        READ_VISCPARAM                source/output/restart/read_viscparam.F
Chd|        TABLE_RRESTI_MAT              source/materials/tools/table_rresti_mat.F
Chd|        TABLE_RRESTR_MAT              source/materials/tools/table_rrestr_mat.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NAMES_AND_TITLES_MOD          ../common_source/modules/names_and_titles_mod.F
Chd|====================================================================
      SUBROUTINE READ_MATPARAM(MAT_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MAT_ELEM_MOD
      USE NAMES_AND_TITLES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IMAT,NUMMAT,NUMTABL,NUPARAM,NIPARAM,NFAIL,IAD,NFIX,LEN
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
C=======================================================================
c     read MATPARAM data
      NFIX   = 19
      NUMMAT = MAT_ELEM%NUMMAT
      ALLOCATE(MAT_ELEM%MAT_PARAM(NUMMAT))
c
      LEN = NFIX*NUMMAT
      ALLOCATE (IBUF(LEN) )
      CALL READ_I_C(IBUF, LEN)
c      
      IAD = 0
      DO IMAT = 1,NUMMAT
        MAT_ELEM%MAT_PARAM(IMAT)%ILAW               = IBUF(IAD+ 1)
        MAT_ELEM%MAT_PARAM(IMAT)%MAT_ID             = IBUF(IAD+ 2)
        MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM            = IBUF(IAD+ 3)
        MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM            = IBUF(IAD+ 4)
        MAT_ELEM%MAT_PARAM(IMAT)%NFUNC              = IBUF(IAD+ 5)
        MAT_ELEM%MAT_PARAM(IMAT)%NTABLE             = IBUF(IAD+ 6)
        MAT_ELEM%MAT_PARAM(IMAT)%NSUBMAT            = IBUF(IAD+ 7)
        MAT_ELEM%MAT_PARAM(IMAT)%NFAIL              = IBUF(IAD+ 8)
        MAT_ELEM%MAT_PARAM(IMAT)%IVISC              = IBUF(IAD+ 9)
        MAT_ELEM%MAT_PARAM(IMAT)%IEOS               = IBUF(IAD+10)
        MAT_ELEM%MAT_PARAM(IMAT)%ITHERM             = IBUF(IAD+11)
c
        MAT_ELEM%MAT_PARAM(IMAT)%COMPRESSIBILITY    = IBUF(IAD+12)
        MAT_ELEM%MAT_PARAM(IMAT)%SMSTR              = IBUF(IAD+13)
        MAT_ELEM%MAT_PARAM(IMAT)%STRAIN_FORMULATION = IBUF(IAD+14)
        MAT_ELEM%MAT_PARAM(IMAT)%IPRES              = IBUF(IAD+15)
        MAT_ELEM%MAT_PARAM(IMAT)%ORTHOTROPY         = IBUF(IAD+16)
c
        MAT_ELEM%MAT_PARAM(IMAT)%NLOC               = IBUF(IAD+17)
        MAT_ELEM%MAT_PARAM(IMAT)%IFAILWAVE          = IBUF(IAD+18)
        MAT_ELEM%MAT_PARAM(IMAT)%IXFEM              = IBUF(IAD+19)
        IAD = IAD + NFIX
      END DO
      DEALLOCATE( IBUF )
c
c     read material title
c
      DO IMAT = 1,NUMMAT      
        CALL READ_C_C(NAME,NCHARTITLE)
        DO I=1,NCHARTITLE
          MAT_ELEM%MAT_PARAM(IMAT)%TITLE(I:I) = CHAR(NAME(I))
        END DO
      END DO
c      
c     read material parameter array
c
      DO IMAT = 1,NUMMAT      
        NUPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM
        NIPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM
        ALLOCATE(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM(NUPARAM))
        IF (NUPARAM > 0) THEN
          CALL READ_DB(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM ,NUPARAM)
        END IF      
        IF (NIPARAM > 0) THEN
          CALL READ_I_C(MAT_ELEM%MAT_PARAM(IMAT)%IPARAM ,NIPARAM)
        END IF      
      END DO
c
c     read material law tables
c      
      DO IMAT = 1,NUMMAT
        NUMTABL  = MAT_ELEM%MAT_PARAM(IMAT)%NTABLE        
        IF (NUMTABL > 0) THEN
          ALLOCATE (MAT_ELEM%MAT_PARAM(IMAT)%TABLE(NUMTABL))
          CALL TABLE_RRESTI_MAT(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL)
          CALL TABLE_RRESTR_MAT(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL)
        END IF
      END DO      
c           
c     read viscosity model parameters
c
      DO IMAT = 1,NUMMAT
        IF (MAT_ELEM%MAT_PARAM(IMAT)%IVISC > 0) THEN
          CALL READ_VISCPARAM(MAT_ELEM%MAT_PARAM(IMAT)%VISC)
        END IF
      END DO      
c      
c     read parameters of failure models per material
c          
      DO IMAT = 1,NUMMAT
        NFAIL = MAT_ELEM%MAT_PARAM(IMAT)%NFAIL        
        IF (NFAIL > 0) THEN
          ALLOCATE (MAT_ELEM%MAT_PARAM(IMAT)%FAIL(NFAIL))
          DO I = 1,NFAIL
            CALL READ_FAILPARAM(MAT_ELEM%MAT_PARAM(IMAT)%FAIL)
          END DO      
        END IF
      END DO      
c-----------
      RETURN
      END
