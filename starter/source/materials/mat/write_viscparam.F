Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    write_viscparam        ../starter/source/materials/mat/write_viscparam.F
!||--- called by ------------------------------------------------------
!||    write_matparam         ../starter/source/materials/mat/write_matparam.F
!||--- calls      -----------------------------------------------------
!||    write_mat_table        ../starter/source/materials/tools/write_mat_table.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE WRITE_VISCPARAM(VISC,LEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE VISC_PARAM_MOD
      USE NAMES_AND_TITLES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(VISC_PARAM_) ,INTENT(IN)    :: VISC
      INTEGER           ,INTENT(INOUT) :: LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IAD,NFIX,LENI,LENR,NUPARAM,NIPARAM,NUMTABL
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBUF
C=======================================================================
      NFIX = 6
      ALLOCATE (IBUF(NFIX + 1))
c
      IAD = 1
      IBUF(IAD) = NFIX
c
      IAD = IAD+1
        IBUF(IAD) = VISC%ILAW
      IAD = IAD+1
        IBUF(IAD) = VISC%NUPARAM
      IAD = IAD+1
        IBUF(IAD) = VISC%NIPARAM
      IAD = IAD+1
        IBUF(IAD) = VISC%NUVAR
      IAD = IAD+1
        IBUF(IAD) = VISC%NFUNC
      IAD = IAD+1
        IBUF(IAD) = VISC%NTABLE
      IAD = IAD+1
c
      CALL WRITE_I_C(IBUF,NFIX+1)
      DEALLOCATE(IBUF)
      
c     write viscosity model title

      DO I=1,NCHARTITLE
        NAME(I) = ICHAR(VISC%TITLE(I:I))
      END DO
      CALL WRITE_C_C(NAME,NCHARTITLE)
c           
c     write viscosity parameter array

      NUPARAM = VISC%NUPARAM
      NIPARAM = VISC%NIPARAM
      IF (NUPARAM > 0) THEN
        CALL WRITE_DB(VISC%UPARAM ,NUPARAM)
      END IF      
      IF (NIPARAM > 0) THEN
        CALL WRITE_I_C(VISC%IPARAM ,NIPARAM)
      END IF      
      LEN = LEN + NUPARAM + NIPARAM
c      
c     write viscosity law tables if necessary
c
      NUMTABL  = VISC%NTABLE
      IF (NUMTABL > 0) THEN
        CALL WRITE_MAT_TABLE(VISC%TABLE, NUMTABL)
        LEN = LEN + NFIX
      END IF
c-----------
      RETURN
      END
