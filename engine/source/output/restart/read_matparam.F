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
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        TABLE_RRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_RRESTR_MAT              source/tools/curve/table_tools.F
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
      INTEGER :: I,IMAT,NUMMAT,NUMTABL,NUPARAM,IAD,LEN
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
C=======================================================================
c     read MATPARAM data
      NUMMAT = MAT_ELEM%NUMMAT
      ALLOCATE(MAT_ELEM%MAT_PARAM(NUMMAT))
c
      CALL READ_I_C(LEN, 1)
      ALLOCATE (IBUF(LEN) )
      CALL READ_I_C(IBUF, LEN)
c      
      IAD = 0
      DO IMAT=1,NUMMAT
        
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%ILAW               = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%MAT_ID             = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM            = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM            = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%NFUNC              = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%NTABLE             = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%NSUBMAT            = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%NFAIL              = IBUF(IAD)

        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%COMPRESSIBILITY    = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%SMSTR              = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%STRAIN_FORMULATION = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%IPRES              = IBUF(IAD)
        IAD = IAD+1
          MAT_ELEM%MAT_PARAM(IMAT)%ORTHOTROPY         = IBUF(IAD)
      END DO
c
      DEALLOCATE( IBUF )
c
c     read material title

      DO IMAT=1,NUMMAT      
        CALL READ_C_C(NAME,NCHARTITLE)
        DO I=1,NCHARTITLE
          MAT_ELEM%MAT_PARAM(IMAT)%TITLE(I:I) = CHAR(NAME(I))
        END DO
      END DO
      
c     read material parameter array

      DO IMAT=1,NUMMAT
      
        NUPARAM = MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM
        ALLOCATE(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM(NUPARAM))
        IF (NUPARAM > 0) THEN
          CALL READ_DB(MAT_ELEM%MAT_PARAM(IMAT)%UPARAM ,NUPARAM)
          LEN = LEN + NUPARAM
        END IF
      
      END DO
c
c     read material law tables if necessary
      
      DO IMAT=1,NUMMAT
        NUMTABL  = MAT_ELEM%MAT_PARAM(IMAT)%NTABLE
        
        IF (NUMTABL > 0) THEN
          ALLOCATE (MAT_ELEM%MAT_PARAM(IMAT)%TABLE(NUMTABL))
          CALL TABLE_RRESTI_MAT(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL)
          CALL TABLE_RRESTR_MAT(MAT_ELEM%MAT_PARAM(IMAT)%TABLE, NUMTABL)
        END IF
      END DO      
c-----------
      RETURN
      END
