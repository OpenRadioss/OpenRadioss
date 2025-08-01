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
!||    c_crkxfem   ../starter/source/restart/ddsplit/c_crkxfem.F
!||--- called by ------------------------------------------------------
!||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
!||====================================================================
      SUBROUTINE C_CRKXFEM(CEP,P,IEL_CRKXFEM,IEL_L,NUMEL,
     .                     NUMEL_L,NELCRKXFE_L,NUMEDGES_L,IEDGESH,
     .                     IDIM,TAGEDGE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CEP(*),P,NELCRKXFE_L,NUMEL,IEL_CRKXFEM(*),
     .        IEL_L(*),NUMEL_L,NUMEDGES_L,IDIM,IEDGESH(IDIM,*),
     .        TAGEDGE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,NEL_L,NEL1_L,IED_GL,K
C
      IEL_L(1:NUMEL_L) = 0 
      NEL_L = 0
      NEL1_L = 1
C            
      DO I = 1,NUMEL
        IF(CEP(I)==P)THEN
          II = IEL_CRKXFEM(I)
          IF(IDIM == 3)II = II - ECRKXFEC
          IF(II  > 0)THEN
            NEL_L = NEL_L + 1
            IEL_L(NEL1_L) = NEL_L
C tag local edges:
            DO K=1,IDIM
              IED_GL = IEDGESH(K,II)
CC              IED_GL = IEDGESH(K,NEL_L)
              IF(IED_GL > 0 .AND. TAGEDGE(IED_GL) == 0)THEN
                NUMEDGES_L = NUMEDGES_L + 1
                TAGEDGE(IED_GL) = 1
              ENDIF
            END DO
          ENDIF
          NEL1_L = NEL1_L + 1
        ENDIF  
      ENDDO
C
      NELCRKXFE_L = NEL_L
C      
      RETURN
      END
