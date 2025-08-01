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
!||    diffusion_mod       ../engine/share/modules/diffusion_mod.F
!||--- called by ------------------------------------------------------
!||    alemain             ../engine/source/ale/alemain.F
!||    ns_fvm_diffusion    ../engine/source/multifluid/ns_fvm_diffusion.F
!||    radioss2            ../engine/source/engine/radioss2.F
!||    resol               ../engine/source/engine/resol.F
!||    resol_head          ../engine/source/engine/resol_head.F
!||--- uses       -----------------------------------------------------
!||    linear_solver_mod   ../engine/share/modules/linear_solver_mod.F
!||    matrix_mod          ../common_source/linearalgebra/matrix_mod.F
!||    vector_mod          ../common_source/linearalgebra/vector_mod.F
!||====================================================================
      MODULE DIFFUSION_MOD
      USE LINEAR_SOLVER_MOD
      USE MATRIX_MOD
      USE VECTOR_MOD
      IMPLICIT NONE
#include "my_real.inc"
!     .___________________________.   !
!     |                           |   !
!     |   **********************  |   !
!     |   ** Type definitions **  |   !
!     |   **********************  |   !
!     |___________________________|   !

      TYPE :: T_DIFFUSION
      CLASS(T_LINEAR_SOLVER), PRIVATE, POINTER :: LINEAR_SOLVER
      TYPE(T_CFS_MATRIX) :: MAT
      TYPE(T_VECTOR) :: RHS
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SOL
      INTEGER, DIMENSION(:), ALLOCATABLE :: FLAG_OUTLET
      MY_REAL, DIMENSION(:), ALLOCATABLE :: NU
      INTEGER :: NRHS
      LOGICAL :: OUTLET_FLAGGED = .FALSE.
      CONTAINS
      PROCEDURE, PASS :: INIT_DIFFUSION
      PROCEDURE, PASS :: SOLVE_DIFFUSION
      PROCEDURE, PASS :: TERMINATE_DIFFUSION
      PROCEDURE, PASS :: GET_SOLUTION
      END TYPE T_DIFFUSION

!     ._____________________________.   !
!     |                             |   !
!     |   ************************  |   !
!     |   ** Interface routines **  |   !
!     |   ************************  |   !
!     |_____________________________|   !

      CONTAINS
!||====================================================================
!||    init_diffusion         ../engine/share/modules/diffusion_mod.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    linear_solver_mod      ../engine/share/modules/linear_solver_mod.F
!||    spmd_comm_world_mod    ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE INIT_DIFFUSION(this, LINSOL, NRHS, IPARG, ALE_CONNECT)
      USE LINEAR_SOLVER_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc" 
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
!     NSPMD
#include      "com01_c.inc"
!     NPARG
#include      "param_c.inc"
!     NUMELS
#include      "com04_c.inc"
!     NSVOIS
#include      "spmd_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      CLASS(T_DIFFUSION) :: this
      CLASS(T_LINEAR_SOLVER), INTENT(IN), TARGET :: LINSOL
      INTEGER, DIMENSION(NPARG, NGROUP), INTENT(IN) :: IPARG
      INTEGER, INTENT(IN) :: NRHS
      TYPE(T_ALE_CONNECTIVITY), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: MAT_NZ, NG, II, NEL, ITY, MATLAW, NFT, I, JJ, KK
      INTEGER :: GLOB_DIM, MAX_ID, IAD, LGTH
#ifdef MPI
      INTEGER IERR
#endif

      this%NRHS = NRHS
!     number of non zero values for the diffusion matrix elem <-> elem matrix
      MAT_NZ = 0
      DO NG = 1, NGROUP
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            DO II = 1, NEL
               I = II + NFT
               MAT_NZ = MAT_NZ + 1
               IAD = ALE_CONNECT%EE_CONNECT%IAD_CONNECT(I)
               LGTH = ALE_CONNECT%EE_CONNECT%IAD_CONNECT(I+1)-ALE_CONNECT%EE_CONNECT%IAD_CONNECT(I)
               DO JJ = 1, LGTH
                  KK = ALE_CONNECT%EE_CONNECT%CONNECTED(IAD + JJ - 1)
                  IF (KK > 0) THEN
                     MAT_NZ = MAT_NZ + 1
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO
!     create matrix
      CALL this%MAT%MATRIX_CREATE(MAT_NZ)
!     create rhs
      CALL this%RHS%CREATE(NRHS * NUMELS)
!     global system size
      GLOB_DIM = NUMELS
      MAX_ID = MAXVAL(ALE_CONNECT%IDGLOB%ID(1:NUMELS + NSVOIS))
#ifdef MPI
      IF (NSPMD > 1) THEN
         CALL MPI_ALLREDUCE(MAX_ID, GLOB_DIM, 1, MPI_INT, MPI_MAX, SPMD_COMM_WORLD, IERR)
      ENDIF
#endif
!     solution
      ALLOCATE(this%SOL(3 * GLOB_DIM))
!     init solver
      this%LINEAR_SOLVER => LINSOL
      CALL this%LINEAR_SOLVER%INIT_SOLVER(GLOB_DIM)
!     set rhs and matrix
      CALL this%LINEAR_SOLVER%SET_RHS(3, this%RHS)
      CALL this%LINEAR_SOLVER%SET_MATRIX(this%MAT)
!     diffusion coefficient, outlet flag
      this%OUTLET_FLAGGED = .FALSE.
      IF (N2D == 0) THEN
         ALLOCATE(this%NU(NUMELS + NSVOIS))
         this%NU(1:NUMELS + NSVOIS) = ZERO
         ALLOCATE(this%FLAG_OUTLET(6 * NUMELS))
         this%FLAG_OUTLET(1:6 * NUMELS) = 0
      ELSE
         ALLOCATE(this%NU(NUMELQ + NUMELTG + NQVOIS + NTGVOIS))
         this%NU(1:NUMELQ + NUMELTG + NQVOIS + NTGVOIS) = ZERO
         ALLOCATE(this%FLAG_OUTLET(4 * NUMELQ + 3 * NUMELTG))
         this%FLAG_OUTLET(1:4 * NUMELQ + 3 * NUMELTG) = 0
      ENDIF

      END SUBROUTINE INIT_DIFFUSION

!||====================================================================
!||    solve_diffusion       ../engine/share/modules/diffusion_mod.F
!||--- calls      -----------------------------------------------------
!||    get_global_dim        ../engine/share/modules/linear_solver_mod.F
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SOLVE_DIFFUSION(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc" 
      CLASS(T_DIFFUSION) :: this
      INTEGER :: GLOB_DIM
      
      GLOB_DIM = this%LINEAR_SOLVER%GET_GLOBAL_DIM()
      CALL this%LINEAR_SOLVER%SOLVE(this%SOL, this%NRHS * GLOB_DIM)
      
      END SUBROUTINE SOLVE_DIFFUSION

!||====================================================================
!||    get_solution          ../engine/share/modules/diffusion_mod.F
!||--- calls      -----------------------------------------------------
!||    get_global_dim        ../engine/share/modules/linear_solver_mod.F
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE GET_SOLUTION(this, SOL, GLOB_DIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc" 
      CLASS(T_DIFFUSION), INTENT(INOUT), TARGET :: this
      INTEGER, INTENT(OUT) :: GLOB_DIM
      
      DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT), POINTER :: SOL
      SOL => this%SOL
      GLOB_DIM = this%LINEAR_SOLVER%GET_GLOBAL_DIM()
      END SUBROUTINE GET_SOLUTION

!||====================================================================
!||    terminate_diffusion   ../engine/share/modules/diffusion_mod.F
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE TERMINATE_DIFFUSION(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc" 
      CLASS(T_DIFFUSION), INTENT(INOUT) :: this
      
    
      CALL this%LINEAR_SOLVER%TERMINATE()
      CALL this%MAT%MATRIX_DESTROY()
      CALL this%RHS%DESTROY()
      IF (ALLOCATED(this%SOL)) DEALLOCATE(this%SOL)
      IF (ALLOCATED(this%NU)) DEALLOCATE(this%NU)
      IF (ALLOCATED(this%FLAG_OUTLET)) DEALLOCATE(this%FLAG_OUTLET)
      END SUBROUTINE TERMINATE_DIFFUSION
      END MODULE DIFFUSION_MOD
