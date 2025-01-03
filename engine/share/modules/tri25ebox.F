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
      !||    tri25ebox                 ../engine/share/modules/tri25ebox.F
      !||--- called by ------------------------------------------------------
      !||    i25asse2                  ../engine/source/interfaces/int25/i25asse.F
      !||    i25cor3_e2s               ../engine/source/interfaces/int25/i25cor3_e2s.F
      !||    i25cor3e                  ../engine/source/interfaces/int25/i25cor3e.F
      !||    i25dst3_e2s               ../engine/source/interfaces/int25/i25dst3_e2s.F
      !||    i25for3_e2s               ../engine/source/interfaces/int25/i25for3_e2s.F
      !||    i25for3e                  ../engine/source/interfaces/int25/i25for3e.F
      !||    i25main_tri               ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25mainf                  ../engine/source/interfaces/int25/i25mainf.F
      !||    i25optcd_e2s              ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25optcd_edg              ../engine/source/interfaces/intsort/i25optcd_edg.F
      !||    i25sto_e2s                ../engine/source/interfaces/intsort/i25sto_e2s.F
      !||    i25sto_edg                ../engine/source/interfaces/intsort/i25sto_edg.F
      !||    i25trivox_edg             ../engine/source/interfaces/intsort/i25trivox_edg.F
      !||    intmass_update            ../engine/source/interfaces/interf/intmass_update.F
      !||    mpp_init                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_exch_efric           ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_press           ../engine/source/mpi/interfaces/spmd_exch_press.F
      !||    spmd_exch_sorting_efric   ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_get_stif25_edg       ../engine/source/mpi/interfaces/spmd_getstif25_edg.F
      !||    spmd_i25front_nor         ../engine/source/mpi/interfaces/spmd_i25front.F
      !||    spmd_i7fcom_poff          ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon           ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7xvcom2             ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront               ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_initfi               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_rnum25_edge          ../engine/source/mpi/interfaces/spmd_rnum25_edge.F
      !||    spmd_savefi               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_tri25egat            ../engine/source/mpi/interfaces/spmd_tri25egat.F
      !||    spmd_tri25vox             ../engine/source/mpi/interfaces/spmd_tri25vox.F
      !||    spmd_tri25vox0            ../engine/source/mpi/interfaces/spmd_tri25vox0.F
      !||    spmd_tri25vox0_edge       ../engine/source/mpi/interfaces/spmd_tri25vox0.F
      !||    upgrade_rem_2ry           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||--- uses       -----------------------------------------------------
      !||    pointerdef                ../engine/share/modules/pointerdef_mod.F
      !||====================================================================
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
