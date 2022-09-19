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
Chd|  TRI25EBOX                     share/modules/tri25ebox.F     
Chd|-- called by -----------
Chd|        I25ASSE2                      source/interfaces/int25/i25asse.F
Chd|        I25COR3E                      source/interfaces/int25/i25cor3e.F
Chd|        I25COR3_E2S                   source/interfaces/int25/i25cor3_e2s.F
Chd|        I25DST3_E2S                   source/interfaces/int25/i25dst3_e2s.F
Chd|        I25FOR3E                      source/interfaces/int25/i25for3e.F
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I25OPTCD_E2S                  source/interfaces/intsort/i25optcd_e2s.F
Chd|        I25OPTCD_EDG                  source/interfaces/intsort/i25optcd_edg.F
Chd|        I25STO_E2S                    source/interfaces/intsort/i25sto_e2s.F
Chd|        I25STO_EDG                    source/interfaces/intsort/i25sto_edg.F
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_EXCH_PRESS               source/mpi/interfaces/spmd_exch_press.F
Chd|        SPMD_GET_STIF25_EDG           source/mpi/interfaces/spmd_getstif25_edg.F
Chd|        SPMD_I25FRONT_NOR             source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUM25_EDGE              source/mpi/interfaces/spmd_rnum25_edge.F
Chd|        SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI25EGAT                source/mpi/interfaces/spmd_tri25egat.F
Chd|        SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|        SPMD_TRI25VOX0                source/mpi/interfaces/spmd_tri25vox0.F
Chd|        SPMD_TRI25VOX0_EDGE           source/mpi/interfaces/spmd_tri25vox0.F
Chd|        UPGRADE_REM_2RY               source/mpi/interfaces/spmd_i7tool.F
Chd|-- calls ---------------
Chd|        POINTERDEF                    share/modules/pointerdef_mod.F
Chd|====================================================================
      MODULE TRI25EBOX
      USE POINTERDEF
#include "my_real.inc"
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IREM_EDGE
      my_real, DIMENSION(:,:), ALLOCATABLE :: XREM_EDGE

      TYPE(int_pointer) , DIMENSION(:), ALLOCATABLE :: EDGE_FI
! CRVOXEL25 has one more dimension than CRVOXEL: edge + node
      INTEGER, PARAMETER :: LRVOXEL25 = 31
      INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE :: CRVOXEL25 ! CRVOXEL


      INTEGER :: NEDGE_REMOTE

! size of MPI buffers
      INTEGER :: ISLEN25,IRLEN25
      INTEGER :: ISLEN25E,IRLEN25E
      INTEGER :: ISLEN25T,IRLEN25T
      INTEGER :: ISLEN25ET,IRLEN25ET

      INTEGER :: NFSKYIE

      TYPE(real4_pointer3), DIMENSION(:), ALLOCATABLE :: EDG_BISECTOR_FIE
      TYPE(real4_pointer3), DIMENSION(:), ALLOCATABLE :: VTX_BISECTOR_FIE
      TYPE(real_pointer3),  DIMENSION(:), ALLOCATABLE :: X_SEG_FIE
      TYPE(real_pointer),   DIMENSION(:), ALLOCATABLE :: GAPE_L_FIE
C     TYPE(real_pointer),   DIMENSION(:), ALLOCATABLE :: MAIN_FIE 
      TYPE(int_pointer2),   DIMENSION(:), ALLOCATABLE :: LEDGE_FIE


! Inacti-like data
      INTEGER, DIMENSION(:), ALLOCATABLE :: RENUM_EDGE ! index of old edges
      INTEGER, DIMENSION(:), ALLOCATABLE :: OLDNUM_EDGE ! revesed index

      INTEGER, DIMENSION(:), ALLOCATABLE :: NSNFIEOLD ! old number of edges per SPMD
      INTEGER :: NEDGE_REMOTE_OLD ! total number of old edges

C tableau de pointeurs NISUBSFIE de taille NINTER
C indiquant pour chaque interface type7, 10 le nbre d elt dans LISUBSFIE
C par processeur
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NISUBSFIE
C tableau de pointeurs LISUBSFIE de taille NINTER
C indiquant pour chaque interface type7, 10, 24, 25 et pour chaque noeud remote de cette interface
C la liste des sous interfaces
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: LISUBSFIE
C tableau de pointeurs INFLG_SUBSFIE de taille NINTER
C indiquant pour chaque interface type25 et pour chaque noeud remote de cette interface
C l appartenance a S1, S2, ou GRNOD
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: INFLG_SUBSFIE
C tableau de pointeurs ADDSUBSFIE de taille NINTER
C indiquant pour chaque interface type7, 10, 24, 25, et pour chaque noeud remote de cette interface
C son adresse de depart dans LISUBSFIE et INFLG_SUBSFIE (tableau dimensionne a NSNR+1 (noeud remote + 1)
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: ADDSUBSFIE

      END MODULE TRI25EBOX
