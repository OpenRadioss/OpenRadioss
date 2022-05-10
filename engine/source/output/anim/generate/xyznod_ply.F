Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  XYZNOD_PLY                    source/output/anim/generate/xyznod_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_ANIM_PLY_XYZNOD          source/mpi/anim/spmd_anim_ply_xyznod.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE XYZNOD_PLY(IPLY,IDPLY,NOD_PXFEM,X,ZI_PLY,NODGLOB,
     *                      EMPSIZPL )
C 
      USE PLYXFEM_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPLY,IDPLY,NOD_PXFEM(*),NBF_PXFEMG,NODGLOB(*),EMPSIZPL
      my_real
     .   X(3,*),ZI_PLY(NPLYXFE,*),NORM
      REAL R4
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,N,II,ND,ARSZ,EMPL
      REAL, DIMENSION(:,:), ALLOCATABLE :: XNODARRAY
      my_real
     .   VN
C
       II = IDPLY
      IF (NSPMD == 1) THEN
        ARSZ = PLYNOD(IPLY)%PLYNUMNODS
        ALLOCATE (XNODARRAY(3,ARSZ))

        DO ND=1,PLYNOD(IPLY)%PLYNUMNODS
             I = PLYNOD(IPLY)%NODES(ND)
             EMPL = PLYNOD(IPLY)%PLYNODID(ND)-EMPSIZPL
             N = NOD_PXFEM(I)
             NORM = SQRT(VN_NOD(1,N)**2 + VN_NOD(2,N)**2 + VN_NOD(3,N)**2)
             NORM = UN/MAX(EM20, NORM)
             VN = VN_NOD(1,N)*NORM
             XNODARRAY(1,EMPL)=X(1,I)+ZI_PLY(N,IPLY)*VN+PLY(IPLY)%U(1,N)
             VN = VN_NOD(2,N)*NORM
             XNODARRAY(2,EMPL)=X(2,I)+ZI_PLY(N,IPLY)*VN+PLY(IPLY)%U(2,N)
             VN = VN_NOD(3,N)*NORM
             XNODARRAY(3,EMPL)=X(3,I)+ZI_PLY(N,IPLY)*VN+PLY(IPLY)%U(3,N)
        END DO
        CALL WRITE_R_C(XNODARRAY,3*ARSZ)
        DEALLOCATE(XNODARRAY)
        EMPSIZPL = EMPSIZPL + PLYNOD(IPLY)%PLYNUMNODS
      ELSE
        CALL SPMD_ANIM_PLY_XYZNOD( NODGLOB,IPLY, IDPLY,
     *                             NOD_PXFEM, X, ZI_PLY,EMPSIZPL )
      ENDIF
C-----------------------------------------------
      RETURN
      END

