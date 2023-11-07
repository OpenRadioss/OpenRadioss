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
Chd|  READ_VISCPARAM                source/output/restart/read_viscparam.F
Chd|-- called by -----------
Chd|        READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|-- calls ---------------
Chd|        READ_C_C                      source/output/tools/sortie_c.c
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        TABLE_RRESTI_MAT              source/materials/tools/table_rresti_mat.F
Chd|        TABLE_RRESTR_MAT              source/materials/tools/table_rrestr_mat.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NAMES_AND_TITLES_MOD          ../common_source/modules/names_and_titles_mod.F
Chd|        VISC_PARAM_MOD                ../common_source/modules/mat_elem/visc_param_mod.F
Chd|====================================================================
      SUBROUTINE READ_VISCPARAM(VISC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE VISC_PARAM_MOD
      USE NAMES_AND_TITLES_MOD
      USE MY_ALLOC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(VISC_PARAM_) ,INTENT(INOUT) :: VISC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,NUPARAM,NIPARAM,NUMTABL,IAD,LEN
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(1) :: ILEN
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
C-----------------------------------------------
c     read viscosity model parameters
C=======================================================================
      CALL READ_I_C(ILEN, 1)
      LEN = ILEN(1)
      ALLOCATE (IBUF(LEN) )
      CALL READ_I_C(IBUF, LEN)
      IAD = 0
c      
      IAD = IAD+1
        VISC%ILAW    = IBUF(IAD)
      IAD = IAD+1
        VISC%NUPARAM = IBUF(IAD)
      IAD = IAD+1
        VISC%NIPARAM = IBUF(IAD)
      IAD = IAD+1
        VISC%NUVAR   = IBUF(IAD)
      IAD = IAD+1
        VISC%NFUNC   = IBUF(IAD)
      IAD = IAD+1
        VISC%NTABLE  = IBUF(IAD)
c
      DEALLOCATE( IBUF )
      
      IF (VISC%ILAW > 0) THEN
c
c       read material title

        CALL READ_C_C(NAME,NCHARTITLE)
        DO I=1,NCHARTITLE
          VISC%TITLE(I:I) = CHAR(NAME(I))
        END DO
      
c       read viscosity parameter arrays
        NUPARAM = VISC%NUPARAM
        NIPARAM = VISC%NIPARAM
        CALL MY_ALLOC(VISC%UPARAM ,NUPARAM)
        CALL MY_ALLOC(VISC%IPARAM ,NIPARAM)

        IF (NUPARAM > 0) THEN
          CALL READ_DB(VISC%UPARAM  ,NUPARAM)
        END IF
        IF (NIPARAM > 0) THEN
          CALL READ_I_C(VISC%IPARAM ,NIPARAM)
        END IF
c
c       read viscosity law function tables
      
        NUMTABL  = VISC%NTABLE        
        IF (NUMTABL > 0) THEN
          ALLOCATE (VISC%TABLE(NUMTABL))
          CALL READ_MAT_TABLE(VISC%TABLE, NUMTABL)
        END IF
c      
      ELSE ! initialize VISC structure 
c
        VISC%TITLE = " "
        LEN = 0
        ALLOCATE (VISC%UPARAM(LEN))
        ALLOCATE (VISC%IPARAM(LEN))
        ALLOCATE (VISC%FUNC(LEN))
        ALLOCATE (VISC%TABLE(LEN))
        VISC%UPARAM = 0
        VISC%IPARAM = 0

      END IF    
c-----------
      RETURN
      END
