---
agent: agent
model: Claude Sonnet 4.5
description: Refactor legacy Fortran code to modern F90 standards
---

# Refactor Legacy Code

Modernize legacy Fortran code to follow OpenRadioss standards while maintaining functionality.

## Modernization Strategy

### Phase 1: Assessment
1. Identify legacy constructs in the code
2. Check for COMMON blocks, GOTO statements, assumed-size arrays
3. Note dependencies and calling routines
4. Plan refactoring approach

### Phase 2: Preparation
1. Ensure existing test coverage or create regression tests
2. Document current behavior
3. Set up comparison framework for validation

### Phase 3: Refactoring
1. Apply transformations incrementally
2. Test after each change
3. Verify results match original

## Legacy to Modern Transformations

### 1. Fixed-Form to Free-Form

**Convert .F (fixed) to .F90 (free-form):**

```fortran
! OLD: Fixed-form (.F) with column restrictions
      SUBROUTINE OLD_SUB(A,B,N)
C     Comment with C in column 1
      INTEGER N
      DOUBLE PRECISION A(N),B(N)
      DO 10 I=1,N
        A(I)=A(I)+B(I)
   10 CONTINUE
      RETURN
      END

! NEW: Free-form (.F90)
      module old_sub_mod
        implicit none
      contains
        subroutine old_sub(a, b, n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                 intent(in)    :: n
          real(kind=WP),           intent(inout) :: a(n)
          real(kind=WP),           intent(in)    :: b(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          do i = 1, n
            a(i) = a(i) + b(i)
          end do
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine old_sub
      end module old_sub_mod
```
use template/template.F90 as an example for indentation

### 2. Replace COMMON Blocks with Modules

```fortran
! OLD: COMMON blocks
      COMMON /PARAMS/ TOL, MAXITER, CONV
      DOUBLE PRECISION TOL
      INTEGER MAXITER
      LOGICAL CONV

! NEW: Module with derived type
      module solver_params_mod
        use precision_mod, only : WP
        implicit none
        
        type :: solver_params_type
          real(kind=WP) :: tolerance
          integer :: max_iterations
          logical :: converged
        end type solver_params_type
        
        ! Module-level instance if needed
        type(solver_params_type) :: solver_params
        
      end module solver_params_mod
```

### 3. Eliminate GOTO Statements

```fortran
! OLD: GOTO spaghetti
      IF (ERROR) GOTO 100
      CALL PROCESS()
      IF (DONE) GOTO 200
      GOTO 100
100   CONTINUE
      CALL CLEANUP()
200   CONTINUE
      RETURN

! NEW: Structured control flow
          if (error) then
            call cleanup()
            return
          end if

          call process()

          if (.not. done) then
            call cleanup()
            return
          end if
```

### 4. Replace Assumed-Size Arrays

```fortran
! OLD: Assumed-size arrays
      SUBROUTINE PROCESS(ARRAY,N)
      INTEGER N
      DOUBLE PRECISION ARRAY(*)
      DO I=1,N
        ARRAY(I)=ARRAY(I)*2.D0
      ENDDO
      END

! NEW: Explicit-size arrays
      module process_mod
        implicit none
      contains
        subroutine process(array, n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,       intent(in)    :: n
          real(kind=WP), intent(inout) :: array(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          do i = 1, n
            array(i) = array(i) * 2.0_WP
          end do
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine process
      end module process_mod
```

### 5. Modernize Type Declarations

```fortran
! OLD: Implicit typing and old declarations
      DOUBLE PRECISION A,B,C
      INTEGER*4 I,J,K
      REAL*8 X,Y,Z

! NEW: Explicit modern typing
          use precision_mod, only : WP
          implicit none
          real(kind=WP) :: a, b, c
          integer :: i, j, k
          real(kind=WP) :: x, y, z
```

### 6. Replace DO-CONTINUE with Modern DO

```fortran
! OLD: Labeled DO-CONTINUE
      DO 10 I=1,N
        DO 20 J=1,M
          A(I,J)=B(I,J)+C(I,J)
   20   CONTINUE
   10 CONTINUE

! NEW: Modern DO-END DO
          do i = 1, n
            do j = 1, m
              a(i,j) = b(i,j) + c(i,j)
            end do
          end do
```

### 7. Modernize Intent Declarations

```fortran
! OLD: No intent specifications
      SUBROUTINE CALC(INPUT,OUTPUT,WORK,N)
      INTEGER N
      DOUBLE PRECISION INPUT(N),OUTPUT(N),WORK(N)

! NEW: Explicit intent
      module calc_mod
        implicit none
      contains
        subroutine calc(input, output, work, n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,       intent(in)    :: n
          real(kind=WP), intent(in)    :: input(n)
          real(kind=WP), intent(out)   :: output(n)
          real(kind=WP), intent(inout) :: work(n)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine calc
      end module calc_mod
```

### 8. Replace EQUIVALENCE with Proper Data Structures

```fortran
! OLD: EQUIVALENCE for memory overlay
      DOUBLE PRECISION A(100)
      DOUBLE PRECISION B(100)
      EQUIVALENCE (A(1),B(1))

! NEW: Use explicit copy or reshape as needed
          real(kind=WP) :: a(100)
          real(kind=WP) :: b(100)
          ! If shared data needed, use pointers or pass arguments
          b = a  ! Explicit copy if needed
```

### 9. Replace SAVE with Module Variables

```fortran
! OLD: SAVE statement
      SUBROUTINE COUNTER()
      INTEGER COUNT
      SAVE COUNT
      DATA COUNT/0/
      COUNT=COUNT+1
      END

! NEW: Module variable
      module counter_mod
        implicit none
        integer, private :: count = 0
      contains
        
        subroutine increment_counter()
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          count = count + 1
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine increment_counter
        
      end module counter_mod
```

### 10. Modernize Preprocessor Usage

```fortran
! OLD: MY_REAL preprocessor macro
#ifdef SINGLE_PRECISION
#define MY_REAL real(kind=4)
#else
#define MY_REAL double precision
#endif
      MY_REAL :: value

! NEW: Use WP from module
          use precision_mod, only : WP
          real(kind=WP) :: value
```

## Step-by-Step Refactoring Process

### Step 1: Create Wrapper Module
```fortran
! Wrap existing subroutine in module
      module old_routine_mod
        implicit none
      contains
        
        ! Original routine goes here (initially unchanged)
        
      end module old_routine_mod
```

### Step 2: Update Calling Code
```fortran
! Update all call sites to use module
          use old_routine_mod, only : old_routine
```

### Step 3: Modernize Implementation
- Add proper intent declarations
- Replace legacy constructs
- Update variable declarations
- Improve formatting and comments

### Step 4: Compile and Test Thoroughly
```bash
# Compile starter with 5 threads
cd starter
./build_script.sh -arch=linux64_gf -mpi=ompi -nt=5

# Compile engine with 5 threads
cd ../engine
./build_script.sh -arch=linux64_gf -mpi=ompi -nt=5

# Run QA tests
cd ../qa-tests/scripts
./or_qa_script ./../../exec/engine_linux64_gf

# Compare outputs if needed
diff old_results.out new_results.out
```

## Common Pitfalls

**⚠️ Precision Changes**
- Be careful changing DOUBLE PRECISION to real(kind=WP)
- Verify literal constants have proper kind: `1.0_WP` not `1.0D0`
- Check for mixed precision operations

**⚠️ Array Indexing**
- Verify loop bounds are correct after changes
- Check for off-by-one errors
- Ensure array sizes are consistent

**⚠️ Implicit Behavior**
- Old code may rely on implicit typing
- Add `implicit none` carefully and declare all variables
- Watch for variables starting with I-N (implicitly integer)

**⚠️ Side Effects**
- COMMON blocks may have hidden dependencies
- SAVE variables maintain state between calls
- Be careful preserving intended behavior

## Validation Checklist

After refactoring:
- [ ] Code compiles without errors or warnings
- [ ] All regression tests pass
- [ ] Results match original within tolerance
- [ ] No performance degradation
- [ ] Follows OpenRadioss coding standards
- [ ] Comments and documentation updated
- [ ] Module dependencies clear and minimal
- [ ] No legacy constructs remain

## Tools to Help

**Automated Formatting:**
```bash
# Use fprettify or similar tools for initial formatting
fprettify -i 2 --strict-indent --case 1 1 1 1 file.F90
```

**Static Analysis:**
```bash
# Check for common issues
gfortran -Wall -Wextra -pedantic -fsyntax-only file.F90
```

**Code Review:**
- Review changes with experienced team member
- Check against template structure
- Verify compliance with coding guidelines

## When NOT to Refactor

Consider leaving code as-is if:
- Code is stable and rarely modified
- Extensive testing would be required
- Performance-critical section needs optimization first
- Dependencies are too complex to untangle safely
- Resources for thorough validation not available

Always weigh benefits against risks!
