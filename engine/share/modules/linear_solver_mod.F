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
Chd|====================================================================
Chd|  LINEAR_SOLVER_MOD             share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|        INIT_DIFFUSION                share/modules/diffusion_mod.F 
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|-- calls ---------------
Chd|        MATRIX_MOD                    ../common_source/linearalgebra/matrix_mod.F
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      MODULE LINEAR_SOLVER_MOD
      USE MATRIX_MOD
      USE VECTOR_MOD
      implicit none
#ifdef MUMPS5
#include "dmumps_struc.h"
#endif
#include "my_real.inc"

!     .___________________________.   !
!     |                           |   !
!     |   **********************  |   !
!     |   ** Type definitions **  |   !
!     |   **********************  |   !
!     |___________________________|   !

!     *********************    !
!     Generic Linear Solver    !
!     *********************    !

      TYPE T_LINEAR_SOLVER
        INTEGER, PRIVATE :: GLOBAL_DIMENSION
        CONTAINS
          PROCEDURE, PASS :: INIT_SOLVER
          PROCEDURE, PASS :: SET_MATRIX
          PROCEDURE, PASS :: SET_RHS
          PROCEDURE, PASS :: SOLVE
          PROCEDURE, PASS :: TERMINATE
          PROCEDURE, NOPASS :: ERROR
          PROCEDURE, PASS :: GET_GLOBAL_DIM
      END TYPE T_LINEAR_SOLVER

!     ********************    !
!     MUMPS Linear Solver     !
!     ********************    !
#ifdef MUMPS5
      TYPE, EXTENDS(T_LINEAR_SOLVER) :: T_MUMPS_SOLVER
        TYPE(DMUMPS_STRUC), PRIVATE :: MUMPS_PAR
        LOGICAL :: JOB_1_DONE = .FALSE.
        CONTAINS
          PROCEDURE, PASS :: INIT_SOLVER_MUMPS
          PROCEDURE, PASS :: SET_MATRIX_MUMPS
          PROCEDURE, PASS :: SET_RHS_MUMPS
          PROCEDURE, PASS :: SOLVE_MUMPS
          PROCEDURE, PASS :: TERMINATE_MUMPS
      END TYPE T_MUMPS_SOLVER
#endif

!     *************************    !
!     Conjugate gradient solver    !
!     *************************    !
      TYPE, EXTENDS(T_LINEAR_SOLVER) :: T_CG_SOLVER
        TYPE(T_CFS_MATRIX), POINTER :: MAT
        TYPE(T_VECTOR), POINTER :: RHS
        TYPE(T_VECTOR) :: SOL_VEC, R, RNEW, TEMP, P
        MY_REAL, DIMENSION(:), ALLOCATABLE :: DIAG
        INTEGER :: NRHS
        CONTAINS
          PROCEDURE, PASS :: INIT_SOLVER_CG
          PROCEDURE, PASS :: SET_MATRIX_CG
          PROCEDURE, PASS :: SET_RHS_CG
          PROCEDURE, PASS :: SOLVE_CG
          PROCEDURE, PASS :: TERMINATE_CG
      END TYPE T_CG_SOLVER
      
      CONTAINS
      
!     ._____________________________.   !
!     |                             |   !
!     |   ************************  |   !
!     |   ** Interface routines **  |   !
!     |   ************************  |   !
!     |_____________________________|   !

!     ****************     !
!     Generic routines     !
!     ****************     !

!     Error
!     -----
Chd|====================================================================
Chd|  ERROR                         share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ERROR()
        PRINT*, "ERROR"
      END SUBROUTINE ERROR
      
      FUNCTION GET_GLOBAL_DIM(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_LINEAR_SOLVER), INTENT(IN) :: this
      INTEGER :: GET_GLOBAL_DIM
      GET_GLOBAL_DIM = this%GLOBAL_DIMENSION
      END FUNCTION GET_GLOBAL_DIM

!     Solver initialization
!     ---------------------
Chd|====================================================================
Chd|  INIT_SOLVER                   share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_SOLVER(this, MAT_DIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_LINEAR_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: MAT_DIM
      this%GLOBAL_DIMENSION = MAT_DIM
      SELECT TYPE(this)
#ifdef MUMPS5
      TYPE IS (T_MUMPS_SOLVER)
      CALL this%INIT_SOLVER_MUMPS(MAT_DIM)
#endif
      TYPE IS (T_CG_SOLVER)
      CALL this%INIT_SOLVER_CG(MAT_DIM)
      CLASS DEFAULT
      CALL this%ERROR()
      END SELECT
      END SUBROUTINE INIT_SOLVER

!     Set matrix
!     ----------
Chd|====================================================================
Chd|  SET_MATRIX                    share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MATRIX_MOD                    ../common_source/linearalgebra/matrix_mod.F
Chd|====================================================================
      SUBROUTINE SET_MATRIX(this, MAT)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MATRIX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_LINEAR_SOLVER), INTENT(INOUT) :: this
      TYPE(T_CFS_MATRIX), INTENT(INOUT) :: MAT
      SELECT TYPE(this)
#ifdef MUMPS5
      TYPE IS (T_MUMPS_SOLVER)
      CALL this%SET_MATRIX_MUMPS(MAT)
#endif
      TYPE IS (T_CG_SOLVER)
      CALL this%SET_MATRIX_CG(MAT)
      CLASS DEFAULT
      CALL this%ERROR()
      END SELECT
      END SUBROUTINE SET_MATRIX

!     Set right hand side
!     -------------------
Chd|====================================================================
Chd|  SET_RHS                       share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      SUBROUTINE SET_RHS(this, NRHS, RHS)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE VECTOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_LINEAR_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: NRHS
      TYPE(T_VECTOR), INTENT(INOUT) :: RHS
      SELECT TYPE(this)
#ifdef MUMPS5
      TYPE IS (T_MUMPS_SOLVER)
      CALL this%SET_RHS_MUMPS(NRHS, RHS)
#endif
      TYPE IS (T_CG_SOLVER)
      CALL this%SET_RHS_CG(NRHS, RHS)
      CLASS DEFAULT
      CALL this%ERROR()
      END SELECT
      END SUBROUTINE SET_RHS

!     Solve the linear system
!     -----------------------
Chd|====================================================================
Chd|  SOLVE                         share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SOLVE(this, SOL, DIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_LINEAR_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: DIM
      DOUBLE PRECISION, DIMENSION(DIM), INTENT(OUT) :: SOL
      SELECT TYPE(THIS)
#ifdef MUMPS5
      TYPE IS (T_MUMPS_SOLVER)
      CALL this%SOLVE_MUMPS(SOL, DIM)
#endif
      TYPE IS (T_CG_SOLVER)
      CALL this%SOLVE_CG(SOL, DIM)
      CLASS DEFAULT
      CALL this%ERROR()
      END SELECT
      END SUBROUTINE SOLVE

!     End the solver instance
!     -----------------------
Chd|====================================================================
Chd|  TERMINATE                     share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TERMINATE(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_LINEAR_SOLVER), INTENT(INOUT) :: this
      SELECT TYPE(this)
#ifdef MUMPS5
      TYPE IS (T_MUMPS_SOLVER)
      CALL this%TERMINATE_MUMPS()
#endif
      TYPE IS (T_CG_SOLVER)
      CALL this%TERMINATE_CG()
      CLASS DEFAULT
      CALL this%ERROR()
      END SELECT
      END SUBROUTINE TERMINATE

!     **************     !
!     MUMPS routines     !
!     **************     !

!     Solver initialization
!     ---------------------
#ifdef MUMPS5
Chd|====================================================================
Chd|  INIT_SOLVER_MUMPS             share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_SOLVER_MUMPS(this, MAT_DIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      CLASS (T_MUMPS_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: MAT_DIM
      this%MUMPS_PAR%PAR = 1
#ifdef MPI      
      this%MUMPS_PAR%COMM = MPI_COMM_WORLD
#else 
      this%MUMPS_PAR%COMM = -1
#endif
      this%mumps_par%job = -1
      this%mumps_par%sym = 0
      call dmumps(this%mumps_par)

!     matrice globale
      this%MUMPS_PAR%ICNTL(5) = 0
!     matrice distribuee
      this%MUMPS_PAR%ICNTL(18) = 3
!     taille de la matrice
      this%MUMPS_PAR%N = MAT_DIM
!     matrice sym  trique d  finie positive
      this%MUMPS_PAR%SYM = 1
!     distributed rhs
      this%MUMPS_PAR%ICNTL(20) = 10
!     distributed solution
      !mumps_par%icntl(21) = 1
!     un-distributed solution
      this%MUMPS_PAR%ICNTL(21) = 0
!     info on solution
      this%MUMPS_PAR%ICNTL(11) = 1
!     dump matrice
!      this%mumps_par%write_problem = "mat"
!     verbosity
      this%MUMPS_PAR%ICNTL(4) = 0
      
      END SUBROUTINE INIT_SOLVER_MUMPS
      
!     Set matrix
!     ----------
Chd|====================================================================
Chd|  SET_MATRIX_MUMPS              share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MATRIX_MOD                    ../common_source/linearalgebra/matrix_mod.F
Chd|====================================================================
      SUBROUTINE SET_MATRIX_MUMPS(this, MAT)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MATRIX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_MUMPS_SOLVER), INTENT(INOUT) :: this
      TYPE(T_CFS_MATRIX), INTENT(INOUT) :: MAT
      CALL MAT%MATRIX_ASSOCIATE(this%MUMPS_PAR%IRN_LOC, this%MUMPS_PAR%JCN_LOC, this%MUMPS_PAR%A_LOC)
      this%MUMPS_PAR%NNZ_LOC = MAT%GET_DIM()
      END SUBROUTINE SET_MATRIX_MUMPS

!     Set right hand side
!     -------------------
Chd|====================================================================
Chd|  SET_RHS_MUMPS                 share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      SUBROUTINE SET_RHS_MUMPS(this, NRHS, RHS)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE VECTOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_MUMPS_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: NRHS
      TYPE(T_VECTOR), INTENT(INOUT) :: RHS
C-----------------------------------------------
C     D u m m y   a r g u m e n t s
C-----------------------------------------------
      INTEGER :: DIM

      CALL RHS%ASSOCIATE(this%MUMPS_PAR%IRHS_LOC, this%MUMPS_PAR%RHS_LOC)
      DIM = RHS%GET_DIM() / NRHS
      this%MUMPS_PAR%NRHS = NRHS
      this%MUMPS_PAR%NLOC_RHS = DIM
      this%MUMPS_PAR%LRHS_LOC = DIM
      END SUBROUTINE SET_RHS_MUMPS

!     Solve the linear system
!     -----------------------
Chd|====================================================================
Chd|  SOLVE_MUMPS                   share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SOLVE_MUMPS(this, SOL, DIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      CLASS (T_MUMPS_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: DIM
!      my_real, dimension(dim), intent(out), target :: sol
      DOUBLE PRECISION, DIMENSION(DIM), INTENT(OUT), TARGET :: SOL
C-----------------------------------------------     
C     L o c a l   V a r i a b l e s 
C----------------------------------------------- 
      INTEGER :: IERR
      IF (DIM /= this%MUMPS_PAR%N * this%MUMPS_PAR%NRHS) THEN
         PRINT*, "*** DIMENSION MISMATCH IN SOLUTION VECTOR"
         RETURN
      ELSE
         IF (.NOT. this%JOB_1_DONE) THEN
! analysis
            this%MUMPS_PAR%JOB = 1
            CALL DMUMPS(this%MUMPS_PAR)
            this%JOB_1_DONE = .TRUE.
         ENDIF
!     factorization
         this%MUMPS_PAR%JOB = 2
         CALL DMUMPS(this%MUMPS_PAR)

!     solve
         this%MUMPS_PAR%RHS => SOL
         this%MUMPS_PAR%LRHS = this%MUMPS_PAR%N
         this%MUMPS_PAR%JOB = 3
         CALL DMUMPS(this%MUMPS_PAR)

#ifdef MPI
!     Sent to all procs
         IF (NSPMD > 1) THEN
            CALL MPI_BCAST(SOL, DIM, REAL, 0, MPI_COMM_WORLD, IERR)
         ENDIF
#endif     
      ENDIF
      END SUBROUTINE SOLVE_MUMPS

!     End the solver instance
!     -----------------------
Chd|====================================================================
Chd|  TERMINATE_MUMPS               share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TERMINATE_MUMPS(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      CLASS (T_MUMPS_SOLVER), INTENT(INOUT) :: this

      this%MUMPS_PAR%JOB = -2
      CALL DMUMPS(this%MUMPS_PAR)
 
      END SUBROUTINE TERMINATE_MUMPS
#endif
C END OF MUMPS5 SPECIFIC CODE

!     ***********     !
!     CG routines     !
!     ***********     !
!     Solver initialization
!     ---------------------
Chd|====================================================================
Chd|  INIT_SOLVER_CG                share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INIT_SOLVER_CG(this, MAT_DIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_CG_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: MAT_DIM

      CALL this%SOL_VEC%CREATE(MAT_DIM)
      CALL this%R%CREATE(MAT_DIM)
      CALL this%RNEW%CREATE(MAT_DIM)
      CALL this%TEMP%CREATE(MAT_DIM)
      CALL this%P%CREATE(MAT_DIM)
      ALLOCATE(this%DIAG(MAT_DIM))

      END SUBROUTINE INIT_SOLVER_CG
      
!     Set matrix
!     ----------
Chd|====================================================================
Chd|  SET_MATRIX_CG                 share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MATRIX_MOD                    ../common_source/linearalgebra/matrix_mod.F
Chd|====================================================================
      SUBROUTINE SET_MATRIX_CG(this, MAT)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MATRIX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_CG_SOLVER), INTENT(INOUT) :: this
      TYPE(T_CFS_MATRIX), INTENT(INOUT), TARGET :: MAT
      
      this%MAT => MAT
      
      END SUBROUTINE SET_MATRIX_CG

!     Set right hand side
!     -------------------
Chd|====================================================================
Chd|  SET_RHS_CG                    share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      SUBROUTINE SET_RHS_CG(this, NRHS, RHS)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE VECTOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      CLASS (T_CG_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: NRHS
      TYPE(T_VECTOR), INTENT(INOUT), TARGET :: RHS

      INTEGER :: II
      INTEGER :: LGTH
      
      this%RHS => RHS
      this%NRHS = NRHS
      
      
      END SUBROUTINE SET_RHS_CG

!     Solve the linear system
!     -----------------------
Chd|====================================================================
Chd|  SOLVE_CG                      share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        VECTOR_MOD                    ../common_source/linearalgebra/vector_mod.F
Chd|====================================================================
      SUBROUTINE SOLVE_CG(this, SOL, DIM)
      USE VECTOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
      CLASS (T_CG_SOLVER), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: DIM
      DOUBLE PRECISION, DIMENSION(DIM), INTENT(OUT), TARGET :: SOL
C-----------------------------------------------     
C     L o c a l   V a r i a b l e s 
C----------------------------------------------- 
      INTEGER :: ITER, SYSTEM_SIZE, IRHS, II, I, J, MAT_NNZ
      MY_REAL :: ERROR, NORM_INIT
      MY_REAL :: ALPHA, BETA
      ! a sortir sous forme de parametre
      INTEGER :: MAX_ITER
      MY_REAL :: TOL

      TOL = 1.D-8
      
      SYSTEM_SIZE = DIM / this%NRHS
      MAX_ITER = SYSTEM_SIZE

!     diaginal matrix made of inverse of square root of diagonal elements of the 
!     system matrix
      MAT_NNZ = this%MAT%GET_DIM()
      DO II = 1, MAT_NNZ
         I = this%MAT%IROW(II)
         J = this%MAT%JCOL(II)
         IF (I == J) THEN
            this%DIAG(I) = ONE / SQRT(this%MAT%VAL(II))
         ENDIF
      ENDDO
      

      DO IRHS = 1, this%NRHS
         this%SOL_VEC%VAL(1:SYSTEM_SIZE) = ZERO     
!     initialisation du solver
         CALL PROD_VEC(this%MAT, this%SOL_VEC, this%TEMP)
         this%R%VAL(1:SYSTEM_SIZE) = this%RHS%VAL(SYSTEM_SIZE * (IRHS - 1) + 1 : SYSTEM_SIZE * (IRHS - 1) + SYSTEM_SIZE) - 
     .        this%TEMP%VAL(1:SYSTEM_SIZE)
         this%P%VAL(1:SYSTEM_SIZE) = this%R%VAL(1:SYSTEM_SIZE)
         NORM_INIT = this%R%NORM()
         ERROR = NORM_INIT
         ITER = 0
         DO WHILE (ITER <= MAX_ITER .AND. ERROR > TOL)
            ITER = ITER + 1
            CALL PROD_VEC(this%MAT, this%P, this%TEMP)
            ALPHA = DOT_PRODUCT(this%R%VAL(1:SYSTEM_SIZE),this% R%VAL(1:SYSTEM_SIZE)) /
     .           DOT_PRODUCT(this%TEMP%VAL(1:SYSTEM_SIZE), this%P%VAL(1:SYSTEM_SIZE))
            DO II = 1, SYSTEM_SIZE
               this%SOL_VEC%VAL(II) = this%SOL_VEC%VAL(II) + ALPHA * this%P%VAL(II)
               this%RNEW%VAL(II) = this%R%VAL(II) - ALPHA * this%TEMP%VAL(II)
            ENDDO
            BETA = DOT_PRODUCT(this%RNEW%VAL(1:SYSTEM_SIZE), this%RNEW%VAL(1:SYSTEM_SIZE)) / 
     .           DOT_PRODUCT(this%R%VAL(1:SYSTEM_SIZE), this%R%VAL(1:SYSTEM_SIZE))
            DO II = 1, SYSTEM_SIZE
               this%P%VAL(II) = this%RNEW%VAL(II) + BETA * this%P%VAL(II)
               this%R%VAL(II) =  this%RNEW%VAL(II)
            ENDDO
            ERROR = this%R%NORM() / NORM_INIT
         ENDDO
         SOL(SYSTEM_SIZE * (IRHS - 1) + 1:SYSTEM_SIZE * (IRHS - 1) + SYSTEM_SIZE) = 
     .        this%SOL_VEC%VAL(1:SYSTEM_SIZE)
      ENDDO

      
      END SUBROUTINE SOLVE_CG

!     End the solver instance
!     -----------------------
Chd|====================================================================
Chd|  TERMINATE_CG                  share/modules/linear_solver_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TERMINATE_CG(this)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
        CLASS (T_CG_SOLVER), INTENT(INOUT) :: this
        CALL this%SOL_VEC%DESTROY()
        CALL this%R%DESTROY()
        CALL this%TEMP%DESTROY()
        CALL this%P%DESTROY()
        CALL this%RNEW%DESTROY()
        DEALLOCATE(this%DIAG)
      END SUBROUTINE TERMINATE_CG

      END MODULE LINEAR_SOLVER_MOD
