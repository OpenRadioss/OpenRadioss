---
agent: agent
model: Claude Sonnet 4.5
description: Guidelines and coding style for Fortran code in OpenRadioss
---

# OpenRadioss Fortran Coding Guidelines

Review the current file for adherence to OpenRadioss coding standards and suggest improvements.

## File Types and Formats

### Legacy Files (.F)
- Fixed-form format with 132 character line limit
- Uppercase file extension required (`.F`)
- Use **only** for maintaining existing legacy code

### New Files (.F90)
- Free-form format (recommended for all new development)
- Modern Fortran 90+ features available
- See `template/template.F90` for the standard template structure

## Naming Conventions

### File Names
- **Subroutines**: `<subroutine_name>.F` or `<subroutine_name>.F90`
- **Modules**: `<module_name>_mod.F90`
- Use lowercase with underscores for separation
- **DON'T** use lowercase extensions: `*.f` or `*.f90`

### Variables and Procedures
- Use descriptive, clear names with underscores separating words
- Constants in UPPERCASE
- Module names in lowercase with `_mod` suffix
- Always specify `INTENT(IN)`, `INTENT(OUT)`, or `INTENT(INOUT)` for dummy arguments

## Coding Rules

### Best Practices ✅

**Language Features**
- Use Fortran 90+ features (modules, derived types, allocatable arrays)
- Pass variables as dummy arguments with explicit `INTENT` declarations
- Use explicit array sizes: `integer, intent(in) :: A(LEN)`
- Apply bounds checking for array operations

**Formatting and Style**
- Indent with 2 spaces (no tabs)
- Keep lines under 120 characters
- Use `!` for inline comments, `!!` for documentation blocks
- Write clear, descriptive code prioritizing readability

**Data Types and Precision**
- Use `real(kind=WP)` for real numbers in `.F90` files
- Use `MY_REAL` type in legacy `.F` files
- Maintain consistent precision throughout

**Memory Management**
- Prefer `ALLOCATABLE` arrays over pointers
- Use `MY_ALLOC` and verify allocation status
- Deallocate arrays as soon as they're no longer needed
- Avoid large automatic arrays

### Anti-Patterns ❌

**Obsolete Constructs**
- `COMMON`, `EQUIVALENCE`, `SAVE` statements
- `GOTO` statements
- Multiple `RETURN` statements
- Global variables

**Array Issues**
- Assumed-size arrays `A(*)`
- Assumed-shape arrays `A(:)` in dummy arguments
- Array operations without bounds checking

**Type and Precision**
- `DOUBLE PRECISION` keyword (use `real(kind=WP)` or `MY_REAL` instead)
- Mixing precision types inconsistently

**Advanced Features**
- Runtime polymorphism
- Type-bound procedures
- Pointers when allocatable arrays suffice

**Other**
- Tab characters for indentation
- Relying on automatic deallocation
- Exceeding line length limits
- Uncommented code

### Additional Rules
- **Routine Length**: 
  - Leaf routines: ≤ 200 lines
  - Main routines: ≤ 1000 lines
- **Pointers**: Avoid when possible, especially for non-contiguous data (performance issues)
- **Comments**: Write clear English comments explaining intent, not just describing code

## Performance Guidelines

### Vectorization
- Use `#include "vectorize.inc"` which contains the IVDEP directive
- When possible, work on arrays of size `MVSIZ`
- Avoid `IF/THEN/ELSE` inside `DO` loops when possible
- Avoid `EXIT` and `CYCLE` in computationally intensive loops
- Avoid calling functions/subroutines inside computationally intensive loops

### Array Data Locality (2D Arrays)
- **Large arrays** (≥ MVSIZ or 128): Put largest dimension last → `X(3,NUMNOD)`
- **Small arrays** (≤ MVSIZ or 128): Put largest dimension first → `C(MVSIZ,5)`

### Memory and Computation
- No aliasing of dummy arguments (different arguments must point to different memory locations)
- Prefer `POINT%X(1:NBPOINT)` over `POINT(1:NBPOINT)%X` (avoid large arrays of small datatype)
- Avoid flushing entire arrays to zeros when not needed (costly initialization)
- Use **integer exponents**: `A**2` instead of `A**2.0`

## Type Definitions

### Working Precision (WP)
- **For .F90 files**: Use `real(kind=WP)` where WP is defined in `precision_mod`
- **For .F legacy files**: Use `MY_REAL` type (preprocessor-defined based on build configuration)
- WP can be single (kind=4) or double (kind=8) precision depending on build flags

## Common Errors to Check

1. Typos in comments and documentation
2. French or non-English comments (convert to English)
3. Use of `DOUBLE PRECISION` instead of `real(kind=WP)` or `MY_REAL`
4. Assumed-size arrays `A(*)` instead of explicit sizes
5. Tabs instead of 2-space indentation
6. Multiple `RETURN` statements
7. Real exponents like `**2.0` instead of `**2`
8. Missing `INTENT` declarations
9. Unnecessary pointer usage when allocatable would work