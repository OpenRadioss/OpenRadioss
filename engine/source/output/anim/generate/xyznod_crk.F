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
Chd|  XYZNOD_CRK0                   source/output/anim/generate/xyznod_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE XYZNOD_CRK0(ILEV)
C-----------------------------------------------
      USE CRACKXFEM_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILEV
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER ND,IAD
C=======================================================================
c     Copy phantom node coordinates to work table for crack intersections
C-----------------------------------------------
      DO ND=1,CRKNOD(ILEV)%CRKNUMNODS
        IAD = CRKNOD(ILEV)%NOD2IAD(ND)
        CRKAVX(ILEV)%XX(1,ND) = CRKAVX(ILEV)%X(1,IAD)
        CRKAVX(ILEV)%XX(2,ND) = CRKAVX(ILEV)%X(2,IAD)
        CRKAVX(ILEV)%XX(3,ND) = CRKAVX(ILEV)%X(3,IAD)
      END DO
C-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  XYZNOD_CRK                    source/output/anim/generate/xyznod_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_WRT_CRK_XYZNOD           source/mpi/anim/spmd_wrt_crk_xyznod.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE XYZNOD_CRK(ICRK,NFNOD_CRKXFEMG,NODGLOBXFE)
C-----------------------------------------------
      USE CRACKXFEM_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICRK,NFNOD_CRKXFEMG,NODGLOBXFE(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER ND,NNODE
      REAL, DIMENSION(:,:), ALLOCATABLE :: XNODARRAY
C=======================================================================
c     Write phantom node coordinates to ANIM file
C-----------------------------------------------
      IF (NSPMD == 1) THEN
        NNODE = CRKNOD(ICRK)%CRKNUMNODS
        ALLOCATE (XNODARRAY(3,NNODE))
        DO ND=1,NNODE
           XNODARRAY(1,ND) = CRKAVX(ICRK)%XX(1,ND)
           XNODARRAY(2,ND) = CRKAVX(ICRK)%XX(2,ND)
           XNODARRAY(3,ND) = CRKAVX(ICRK)%XX(3,ND)
        END DO
        CALL WRITE_R_C(XNODARRAY,3*NNODE)
        DEALLOCATE(XNODARRAY)
      ELSE
         IF (ISPMD == 0) THEN
           CALL SPMD_WRT_CRK_XYZNOD(ICRK,NFNOD_CRKXFEMG,NODGLOBXFE)
         ELSE
           CALL SPMD_WRT_CRK_XYZNOD(ICRK,1,NODGLOBXFE)
         ENDIF
      ENDIF
C-----------
      RETURN
      END
