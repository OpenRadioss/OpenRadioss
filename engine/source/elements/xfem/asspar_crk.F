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
Chd|  ASSPAR_CRK                    source/elements/xfem/asspar_crk.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE ASSPAR_CRK(
     .           ADDCNE_CRK,INOD_CRK  ,CRKSKY    ,NODFT     ,NODLT     ,
     .           NODENR    ,NODLEVXF  ,ITAB      )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT
      INTEGER ADDCNE_CRK(*),INOD_CRK(*),NODENR(*),NODLEVXF(*),ITAB(NUMNOD)
      TYPE(XFEM_SKY_), DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,NSX,NC_CRK,NCT_CRK,EN0,ILEV,NLEV
      my_real, DIMENSION(:,:),ALLOCATABLE :: XFAC0
C=======================================================================
      ALLOCATE(XFAC0(6,IENRNOD))
c
      DO I = NODFT,NODLT
        NSX = INOD_CRK(I)
        IF (NSX <= 0) CYCLE
        XFAC0 = ZERO
        NCT_CRK = ADDCNE_CRK(NSX)-1
        NC_CRK  = ADDCNE_CRK(NSX+1)-ADDCNE_CRK(NSX)
        NLEV    = NODLEVXF(NSX)
c
        DO ILEV=1,NLEV
          DO KK = NCT_CRK+1, NCT_CRK+NC_CRK
            EN0 = CRKLVSET(ILEV)%ENR0(2,KK)
            IF (EN0 > 0) THEN
              XFAC0(1,EN0) = XFAC0(1,EN0) + CRKSKY(ILEV)%FSKY(1,KK)
              XFAC0(2,EN0) = XFAC0(2,EN0) + CRKSKY(ILEV)%FSKY(2,KK)
              XFAC0(3,EN0) = XFAC0(3,EN0) + CRKSKY(ILEV)%FSKY(3,KK)
              XFAC0(4,EN0) = XFAC0(4,EN0) + CRKSKY(ILEV)%FSKY(4,KK)
              XFAC0(5,EN0) = XFAC0(5,EN0) + CRKSKY(ILEV)%FSKY(5,KK)
              XFAC0(6,EN0) = XFAC0(6,EN0) + CRKSKY(ILEV)%FSKY(6,KK)
            END IF
C---
          END DO
        END DO
C---
        DO ILEV=1,NLEV
          DO KK = NCT_CRK+1, NCT_CRK+NC_CRK
            EN0 = CRKLVSET(ILEV)%ENR0(2,KK)
            IF (EN0 > 0) THEN
              CRKSKY(ILEV)%FSKY(1,KK) = XFAC0(1,EN0)
              CRKSKY(ILEV)%FSKY(2,KK) = XFAC0(2,EN0)
              CRKSKY(ILEV)%FSKY(3,KK) = XFAC0(3,EN0)
              CRKSKY(ILEV)%FSKY(4,KK) = XFAC0(4,EN0)
              CRKSKY(ILEV)%FSKY(5,KK) = XFAC0(5,EN0)
              CRKSKY(ILEV)%FSKY(6,KK) = XFAC0(6,EN0)
            END IF
          END DO
        END DO
C---
      END DO
C---
      DEALLOCATE(XFAC0)
C-----------
      RETURN
      END
