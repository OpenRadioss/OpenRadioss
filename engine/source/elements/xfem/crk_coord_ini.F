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
c
Chd|====================================================================
Chd|  CRK_COORD_INI                 source/elements/xfem/crk_coord_ini.F
Chd|-- called by -----------
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRK_COORD_INI(ADDCNE_CRK,INOD_CRK,NODFT,NODLT,X,NODLEVXF)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,ADDCNE_CRK(*),INOD_CRK(*),NODLEVXF(*)
      my_real X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,N,NX,NCT,NC,ILEV,NLEV
C=======================================================================
      DO N = NODFT,NODLT
        NX = INOD_CRK(N)   ! no node systeme xfem
        IF (NX > 0) THEN
C---
          NCT = ADDCNE_CRK(NX)-1 ! premiere adresse sky du node NX (sys_xfem)
          NC  = ADDCNE_CRK(NX+1)-ADDCNE_CRK(NX)  ! nb elements connectes au noeud
C---
c         initialisation coord noeuds phantomes par ply
c
          NLEV = NODLEVXF(NX)    ! mono/multi (nb ilev /node)
          DO KK = NCT+1, NCT+NC
            DO ILEV=1,NLEV
              CRKAVX(ILEV)%X(1,KK) = X(1,N)
              CRKAVX(ILEV)%X(2,KK) = X(2,N)
              CRKAVX(ILEV)%X(3,KK) = X(3,N)
            END DO
          END DO
        ENDIF
      ENDDO
C-----------
      RETURN
      END
