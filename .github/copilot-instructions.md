# Fortran Coding Standards

## Table of Contents
1. [File Types and Formats](#file-types-and-formats)
2. [Naming Conventions](#naming-conventions)
3. [Coding Rules](#coding-rules)
4. [Template Structure](#template-structure)
5. [Code Examples](#code-examples)
6. [Common Pitfalls](#common-pitfalls)

## File Types and Formats

### Legacy Files (.F)
- Fixed-form format with 132 character line limit
- Uppercase file extension required
- Use only for maintaining existing legacy code

### New Files (.F90)
- Free-form format (recommended for all new development)
- Modern Fortran 90+ features available
- More flexible syntax and readability

## Naming Conventions

### File Names
- **Subroutines**: `<subroutine_name>.F90`
- **Modules**: `<module_name>_mod.F90`
- Use lowercase with underscores for separation

### Variable and Procedure Names
- Use descriptive, clear names
- Separate words with underscores
- Constants in UPPERCASE
- Module names in lowercase with `_mod` suffix

## Coding Rules

| **DO** | **DON'T** |
|--------|-----------|
| Use Fortran 90+ features | Runtime polymorphism, type-bound procedures |
| Use `*.F` (fixed, 132 chars) for legacy files | Use tabs for indentation |
| Indent using 2 spaces | Use `COMMON`, `EQUIVALENCE`, `SAVE` |
| Use modules and derived types | Use global variables |
| Pass variables as dummy arguments | Use `GOTO`, multiple `RETURN` statements |
| Look for clarity in code | Use assumed-size arrays `A(*)` |
| Explicit array sizes: `INTEGER, INTENT(IN) :: A(LEN)` | Use `DOUBLE PRECISION` systematically |
| Use bounds for array operations | Perform `A = B + C` without bounds checking |
| Use the `MY_REAL` type for real numbers | Use pointers when allocatable is possible |
| Use `ALLOCATABLE` arrays | Use large automatic arrays |
| Use `MY_ALLOC` and check allocation status | Rely on automatic deallocation |
| Deallocate arrays as soon as possible | Leave arrays allocated unnecessarily |

### Additional Rules
- **Line Length**: Keep lines under 120 characters for readability
- **Comments**: Use `!` for inline comments, `!!` for documentation
- **Intent Declarations**: Always specify `INTENT(IN)`, `INTENT(OUT)`, or `INTENT(INOUT)`
- **Routine Length**: 
  - Leaf routines: ≤ 200 lines
  - Main routines: ≤ 1000 lines
- **DO NOT USE POINTER** when this can be avoided. In particular pointers to noncontiguous data are forbidden because of performance issues.

## Template Structure

### Standard .F90 File Template

```fortran
module my_subroutine_mod
  implicit none
contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Brief description of the routine
!! \details More detailed description if needed, including:
!!          - Purpose and functionality  
!!          - Input/output parameter descriptions
!!          - Any special considerations or limitations
subroutine subroutine_example(intbuf_tab, buffer, buffer_size, acceleration, acceleration_size)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
! Module names must be in uppercase (will change later)
! ONLY is mandatory, note the space before the comma
  use INTBUF_DEF_MOD, only : intbuf_struct
  use CONSTANT_MOD, only : PI
  use PRECISION_MOD, only : WP
  use MVSIZ_MOD, only : MVSIZ
  use NAMES_AND_TITLES_MOD, only : ncharline100
  use MY_ALLOC_MOD, only : my_alloc

! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none

! ----------------------------------------------------------------------------------------------------------------------
!                                                   INCLUDED FILES
! ----------------------------------------------------------------------------------------------------------------------
! No comments on the same line as #include, #define, #ifdef, #endif
! Generally speaking, #include is forbidden with few exceptions

! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  type(intbuf_struct), intent(in)    :: intbuf_tab                    !< Input buffer structure
  integer,             intent(in)    :: buffer_size                   !< Size of the buffer array
  integer,             intent(inout) :: buffer(buffer_size)           !< Working buffer array
  integer,             intent(in)    :: acceleration_size             !< Size of acceleration array
  real(kind=WP),       intent(in)    :: acceleration(3,acceleration_size) !< 3D acceleration vectors

! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  integer :: i, l, k, m    ! Loop counters and small integers
  integer :: pos           ! Position index for buffer operations
  real(kind=WP), allocatable :: work_array(:)  ! Temporary work array

! ----------------------------------------------------------------------------------------------------------------------
!                                                   EXTERNAL FUNCTIONS
! ----------------------------------------------------------------------------------------------------------------------
! External functions must be kept to minimum
! Prefer internal procedures or module procedures

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
! Code must be indented with 2 spaces
! Code must be well commented
! Use separators between logical blocks of code

  ! Initialize local variables
  pos = 1
  
  ! Allocate work array using MY_ALLOC
  call my_alloc(work_array, buffer_size, 'work_array in subroutine_example')
  
  ! Main processing loop with proper bounds
  do i = 1, min(buffer_size, acceleration_size)
    ! Process each element with bounds checking
    if (i <= size(buffer)) then
      buffer(i) = buffer(i) + int(acceleration(1,i) * PI)
    end if
  end do
  
  ! Clean up allocated memory
  if (allocated(work_array)) then
    deallocate(work_array)
  end if

! ----------------------------------------------------------------------------------------------------------------------
end subroutine subroutine_example

end module my_subroutine_mod

## Common Pitfalls

### Avoid These Patterns

**❌ Poor Array Handling:**
```fortran
! Don't use assumed-size arrays
subroutine bad_example(array, n)
  integer :: array(*)  ! Avoid this - assumed size
  
! Don't use assumed-shape arrays in dummy arguments  
subroutine also_bad(array, n)
  integer :: array(:)  ! Avoid this - assumed shape not allowed

! Don't perform unchecked array operations
array = other_array + third_array  ! No bounds checking
```

**❌ Old Fortran Constructs:**
```fortran
! Avoid COMMON blocks
common /data_block/ x, y, z

! Avoid GOTO statements
if (condition) goto 100
! ...
100 continue

! Avoid multiple returns
if (error1) return
if (error2) return
if (error3) return
```

**❌ Poor Memory Management:**
```fortran
! Don't use large automatic arrays
subroutine bad_memory(n)
  real :: huge_array(n)  ! Risky for large n
  
! Don't forget to deallocate
allocate(temp_array(n))
! ... use array but forget to deallocate
```

### ✅ Best Practices

**Modern Variable Declarations:**
```fortran
! Use specific kinds
use precision_mod, only : WP  ! Working precision
real(kind=WP) :: temperature
integer :: counter

! Use meaningful names
integer :: num_particles, max_iterations
logical :: converged, file_exists
```

## Type Definitions and Custom Procedures

### Precision Control

**Working Precision (WP):**
```fortran
! WP is defined in precision_mod and can be either:
integer, parameter :: WP = 4 ! Single precision build
! OR
integer, parameter :: WP = 8  ! Double precision build
```

**MY_REAL Type:**
- In modern `.F90` files: Use `real(kind=WP)` 
- In legacy `.F` files: Defined via preprocessor as:
  ```fortran
  #ifdef SINGLE_PRECISION
  #define MY_REAL real(kind=4)
  #else  
  #define MY_REAL double precision
  #endif
  ```
- **Recommendation**: Migrate legacy files to use `real(kind=WP)` when possible


## MPI

use `SPMD_MOD.F90` instead that is a wrapper around the MPI functions. 
`SPMD_*` functions match the `MPI_*` functions but with a more Fortran-like interface, optional arguments (status, communicator), and better error handling.

## Building

OpenRadioss is made of two binaries: the starter and the engine. Run the following command `./build_script.sh -arch=linux64_gf -mpi=ompi` in the engine or the starter subdirectory to build the binaries. 

---

*This document should be reviewed and updated regularly to reflect evolving best practices and project-specific requirements.*
