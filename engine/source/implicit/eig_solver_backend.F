Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you:
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    eig_solver_backend
!||
!|| Backend-neutral public /EIG solve interface.  K and M are supplied as
!|| symmetric sparse operators: a diagonal plus a one-based compressed lower
!|| triangle.  Assembly, constraints, recovery, and output remain outside the
!|| backend so every solver consumes exactly the same mechanical problem.
!||====================================================================
      SUBROUTINE EIG_SOLVE_BACKEND(NLOCAL,NGLOBAL,NOWNED,ROW_START,
     .          COMM_F,GLOBAL_DOF,
#if defined(PUBLIC_EIG_SLEPC)
     .          GLOBAL_NODE,GLOBAL_COMPONENT,GLOBAL_COORD,
#endif
     .          NSPMD,K_IAD,K_JD,K_DIAG,K_LOWER,
     .          M_IAD,M_JD,M_DIAG,M_LOWER,NREQ,RANGE_ALL,
     .          OUTPUT_RIGID,RETAIN_MODE_SHAPES,
     .          SHIFT2,CUTFREQ,TOL,TOL_RIGID,TOL_PIVOT_UPPER,
     .          TOL_PIVOT_POSITIVE,
     .          MAX_ITER,SUBSPACE_FACTOR,
     .          SOLVER_HANDLE,
     .          EVAL,EVECS,NFOUND,BACKEND,INFO,MESSAGE)
#include      "implicit_f.inc"
      INTEGER NLOCAL,NGLOBAL,NOWNED,COMM_F,NSPMD,
#if defined(PUBLIC_EIG_SLEPC)
     .        GLOBAL_NODE(*),GLOBAL_COMPONENT(*),
#endif
     .        K_IAD(*),K_JD(*),
     .        M_IAD(*),M_JD(*),NREQ,RANGE_ALL,NFOUND,INFO,
     .        MAX_ITER,SUBSPACE_FACTOR,OUTPUT_RIGID,RETAIN_MODE_SHAPES
      INTEGER*8 ROW_START,GLOBAL_DOF(*),SOLVER_HANDLE
      my_real K_DIAG(*),K_LOWER(*),M_DIAG(*),M_LOWER(*),SHIFT2,
     .        CUTFREQ,TOL,TOL_RIGID,TOL_PIVOT_UPPER,
     .        TOL_PIVOT_POSITIVE,
     .        EVAL(*),EVECS(MAX(1,NLOCAL),*)
#if defined(PUBLIC_EIG_SLEPC)
      my_real GLOBAL_COORD(3,*)
#endif
      CHARACTER*(*) BACKEND,MESSAGE
      CHARACTER*32 REQUEST
      INTEGER I,IC,LENGTH,STATUS
C=======================================================================
      REQUEST='auto'
      CALL GET_ENVIRONMENT_VARIABLE('OPENRADIOSS_EIG_BACKEND',REQUEST,
     .                              LENGTH,STATUS)
      IF (STATUS.NE.0.OR.LENGTH.EQ.0) REQUEST='auto'
      DO I=1,LEN_TRIM(REQUEST)
        IC=IACHAR(REQUEST(I:I))
        IF (IC.GE.IACHAR('A').AND.IC.LE.IACHAR('Z'))
     .    REQUEST(I:I)=ACHAR(IC+IACHAR('a')-IACHAR('A'))
      ENDDO
C
      IF (TRIM(REQUEST).EQ.'auto') THEN
#if defined(PUBLIC_EIG_SLEPC)
        REQUEST='slepc'
#else
        REQUEST='dense'
#endif
      ENDIF
C
      BACKEND=TRIM(REQUEST)
      INFO=0
      MESSAGE=''
      NFOUND=0
      IF (NREQ.LT.1.OR.TOL.LT.ZERO.OR.MAX_ITER.LT.0.OR.
     .    SUBSPACE_FACTOR.LT.0) THEN
        INFO=21
        MESSAGE='invalid /EIG solver controls'
        RETURN
      ENDIF
      IF (TOL_RIGID.LE.ZERO.OR.TOL_PIVOT_UPPER.LE.ZERO.OR.
     .    TOL_PIVOT_POSITIVE.LE.ZERO) THEN
        INFO=21
        MESSAGE='invalid /EIG threshold tolerance'
        RETURN
      ENDIF
      IF (RANGE_ALL.LT.0.OR.RANGE_ALL.GT.3) THEN
        INFO=21
        MESSAGE='invalid /EIG extraction mode'
        RETURN
      ENDIF
      IF (OUTPUT_RIGID.LT.0.OR.OUTPUT_RIGID.GT.2) THEN
        INFO=21
        MESSAGE='invalid /EIG mode-output controls'
        RETURN
      ENDIF
      IF (RETAIN_MODE_SHAPES.LT.0.OR.RETAIN_MODE_SHAPES.GT.1) THEN
        INFO=21
        MESSAGE='invalid /EIG mode-shape retention control'
        RETURN
      ENDIF
      IF (NGLOBAL.LT.1.OR.NLOCAL.LT.0.OR.NOWNED.LT.0.OR.
     .    NOWNED.GT.NGLOBAL.OR.
     .    ROW_START.LT.0.OR.
     .    ROW_START.GT.INT(NGLOBAL,8)-INT(NOWNED,8).OR.
     .    COMM_F.LT.0) THEN
        INFO=13
        MESSAGE='invalid /EIG distributed DOF ownership'
        RETURN
      ENDIF
      IF (NLOCAL.GT.0) THEN
        IF (GLOBAL_DOF(1).LT.0) THEN
          INFO=13
          MESSAGE='invalid /EIG distributed DOF ownership'
          RETURN
        ENDIF
      ENDIF
      IF (TRIM(REQUEST).EQ.'dense') THEN
        SOLVER_HANDLE=0
        IF (RANGE_ALL.EQ.3) THEN
          INFO=8
          MESSAGE='rigid-only /EIG output requires the SLEPc backend'
          RETURN
        ELSEIF (NSPMD.GT.1) THEN
          INFO=9
          MESSAGE='dense /EIG backend is not distributed'
          RETURN
        ELSEIF (NGLOBAL.GT.5000) THEN
          INFO=6
          WRITE(MESSAGE,'(A,I0,A)')'dense /EIG limit is 5000 DOFs; ',
     .                              NGLOBAL,' requested'
          RETURN
        ENDIF
        CALL EIG_SOLVE_DENSE(NLOCAL,K_IAD,K_JD,K_DIAG,K_LOWER,
     .       M_IAD,M_JD,M_DIAG,M_LOWER,NREQ,RANGE_ALL,
     .       RETAIN_MODE_SHAPES,SHIFT2,CUTFREQ,TOL,TOL_RIGID,
     .       EVAL,EVECS,NFOUND,INFO,MESSAGE)
      ELSEIF (TRIM(REQUEST).EQ.'slepc') THEN
#if defined(PUBLIC_EIG_SLEPC)
        CALL EIG_SOLVE_SLEPC(NLOCAL,NGLOBAL,NOWNED,ROW_START,
     .       COMM_F,GLOBAL_DOF,GLOBAL_NODE,GLOBAL_COMPONENT,GLOBAL_COORD,
     .       K_IAD,K_JD,K_DIAG,K_LOWER,
     .       M_IAD,M_JD,M_DIAG,M_LOWER,NREQ,RANGE_ALL,
     .       OUTPUT_RIGID,RETAIN_MODE_SHAPES,
     .       SHIFT2,CUTFREQ,TOL,TOL_RIGID,TOL_PIVOT_UPPER,
     .       TOL_PIVOT_POSITIVE,
     .       MAX_ITER,SUBSPACE_FACTOR,
     .       SOLVER_HANDLE,
     .       EVAL,EVECS,NFOUND,INFO,MESSAGE)
#else
        INFO=7
        MESSAGE='SLEPc backend was not enabled in this build'
#endif
      ELSE
        INFO=8
        MESSAGE='unknown OPENRADIOSS_EIG_BACKEND='//TRIM(REQUEST)
      ENDIF
      RETURN
      END

#if defined(PUBLIC_EIG_SLEPC)
!||====================================================================
!||    eig_log_line_c
!||
!|| Rank-zero C diagnostics use this bridge to write the same complete line
!|| to the official Engine output file.  The C side owns MPI rank filtering
!|| and line assembly, so this routine only performs the Fortran I/O.
!||====================================================================
      SUBROUTINE EIG_LOG_LINE_C(TEXT,TEXT_LENGTH)
     . BIND(C,NAME='eig_log_line_c')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR,C_INT
#include      "units_c.inc"
      CHARACTER(KIND=C_CHAR), INTENT(IN) :: TEXT(*)
      INTEGER(C_INT), VALUE, INTENT(IN) :: TEXT_LENGTH
      CHARACTER*8192 LINE
      INTEGER I,N
C=======================================================================
      N=MIN(MAX(INT(TEXT_LENGTH),0),LEN(LINE))
      IF (N.GT.0) THEN
        DO I=1,N
          LINE(I:I)=TEXT(I)
        ENDDO
        WRITE(IOUT,'(A)') LINE(1:N)
      ELSE
        WRITE(IOUT,'(A)') ' '
      ENDIF
      FLUSH(IOUT)
      RETURN
      END

!||====================================================================
!||    eig_solve_slepc
!||====================================================================
      SUBROUTINE EIG_SOLVE_SLEPC(NLOCAL,NGLOBAL,NOWNED,ROW_START,
     .          COMM_F,GLOBAL_DOF,GLOBAL_NODE,GLOBAL_COMPONENT,GLOBAL_COORD,
     .          K_IAD,K_JD,K_DIAG,K_LOWER,
     .          M_IAD,M_JD,M_DIAG,M_LOWER,NREQ,RANGE_ALL,
     .          OUTPUT_RIGID,RETAIN_MODE_SHAPES,
     .          SHIFT2,CUTFREQ,TOL,TOL_RIGID,TOL_PIVOT_UPPER,
     .          TOL_PIVOT_POSITIVE,
     .          MAX_ITER,SUBSPACE_FACTOR,
     .          SOLVER_HANDLE,
     .          EVAL,EVECS,NFOUND,INFO,MESSAGE)
#include      "implicit_f.inc"
      INTEGER NLOCAL,NGLOBAL,NOWNED,COMM_F,GLOBAL_NODE(*),
     .        GLOBAL_COMPONENT(*),
     .        K_IAD(*),K_JD(*),
     .        M_IAD(*),M_JD(*),NREQ,RANGE_ALL,NFOUND,INFO,
     .        MAX_ITER,SUBSPACE_FACTOR,OUTPUT_RIGID,RETAIN_MODE_SHAPES
      INTEGER*8 ROW_START,GLOBAL_DOF(*),SOLVER_HANDLE
      my_real K_DIAG(*),K_LOWER(*),M_DIAG(*),M_LOWER(*),SHIFT2,
     .        CUTFREQ,TOL,TOL_RIGID,TOL_PIVOT_UPPER,
     .        TOL_PIVOT_POSITIVE,
     .        EVAL(*),EVECS(MAX(1,NLOCAL),*),GLOBAL_COORD(3,*)
      CHARACTER*(*) MESSAGE
C=======================================================================
      CALL EIG_SOLVE_SLEPC_C(NLOCAL,NGLOBAL,NOWNED,ROW_START,
     .     COMM_F,GLOBAL_DOF,GLOBAL_NODE,GLOBAL_COMPONENT,GLOBAL_COORD,
     .     K_IAD,K_JD,K_DIAG,K_LOWER,
     .     M_IAD,M_JD,M_DIAG,M_LOWER,NREQ,RANGE_ALL,
     .     OUTPUT_RIGID,RETAIN_MODE_SHAPES,
     .     SHIFT2,CUTFREQ,TOL,TOL_RIGID,TOL_PIVOT_UPPER,
     .     TOL_PIVOT_POSITIVE,
     .     MAX_ITER,SUBSPACE_FACTOR,
     .     SOLVER_HANDLE,
     .     EVAL,EVECS,NFOUND,INFO)
      IF (INFO.EQ.10) THEN
        MESSAGE='unable to allocate retained SLEPc result context'
      ELSEIF (INFO.EQ.11) THEN
        MESSAGE='no SLEPc modes found in requested frequency range'
      ELSEIF (INFO.EQ.12) THEN
        MESSAGE='SLEPc iteration diverged before requested modes converged'
      ELSEIF (INFO.EQ.13) THEN
        MESSAGE='invalid distributed SLEPc DOF ownership'
      ELSEIF (INFO.EQ.14) THEN
        MESSAGE='unable to assemble distributed SLEPc stiffness'
      ELSEIF (INFO.EQ.15) THEN
        MESSAGE='unable to assemble distributed SLEPc mass'
      ELSEIF (INFO.EQ.16) THEN
        MESSAGE='distributed SLEPc mass has non-positive diagonal'
      ELSEIF (INFO.EQ.19) THEN
        MESSAGE='SLEPc retained result is ready'
      ELSEIF (INFO.EQ.20) THEN
        MESSAGE='invalid retained SLEPc result context'
      ELSEIF (INFO.EQ.21) THEN
        MESSAGE='invalid /EIG solver controls'
      ELSEIF (INFO.GE.2000) THEN
        WRITE(MESSAGE,'(A,I0)')'SLEPc/PETSc call failed with code ',
     .                          INFO-2000
      ELSEIF (INFO.NE.0) THEN
        WRITE(MESSAGE,'(A,I0)')'SLEPc setup failed with code ',INFO
      ENDIF
      RETURN
      END

!||====================================================================
!||    eig_slepc_result_metadata
!||====================================================================
      SUBROUTINE EIG_SLEPC_RESULT_METADATA(SOLVER_HANDLE,CAPACITY,
     .          EVAL,NFOUND,INFO,MESSAGE)
#include      "implicit_f.inc"
      INTEGER CAPACITY,NFOUND,INFO
      INTEGER*8 SOLVER_HANDLE
      my_real EVAL(*)
      CHARACTER*(*) MESSAGE
C=======================================================================
      INFO=0
      MESSAGE=''
      CALL EIG_SLEPC_RESULT_METADATA_C(SOLVER_HANDLE,CAPACITY,
     .     EVAL,NFOUND,INFO)
      IF (INFO.EQ.20) THEN
        MESSAGE='invalid retained SLEPc result metadata request'
      ELSEIF (INFO.GE.2000) THEN
        WRITE(MESSAGE,'(A,I0)')'SLEPc/PETSc call failed with code ',
     .                          INFO-2000
      ENDIF
      RETURN
      END

!||====================================================================
!||    eig_slepc_result_batch
!||====================================================================
      SUBROUTINE EIG_SLEPC_RESULT_BATCH(SOLVER_HANDLE,FIRST_MODE,
     .          MODE_COUNT,NLOCAL,EVECS,INFO,MESSAGE)
#include      "implicit_f.inc"
      INTEGER FIRST_MODE,MODE_COUNT,NLOCAL,INFO
      INTEGER*8 SOLVER_HANDLE
      my_real EVECS(MAX(1,NLOCAL),*)
      CHARACTER*(*) MESSAGE
C=======================================================================
      INFO=0
      MESSAGE=''
      CALL EIG_SLEPC_RESULT_BATCH_C(SOLVER_HANDLE,FIRST_MODE,
     .     MODE_COUNT,EVECS,INFO)
      IF (INFO.EQ.20) THEN
        MESSAGE='invalid retained SLEPc result batch request'
      ELSEIF (INFO.GE.2000) THEN
        WRITE(MESSAGE,'(A,I0)')'SLEPc/PETSc call failed with code ',
     .                          INFO-2000
      ENDIF
      RETURN
      END

!||====================================================================
!||    eig_slepc_result_release
!||====================================================================
      SUBROUTINE EIG_SLEPC_RESULT_RELEASE(SOLVER_HANDLE,INFO,MESSAGE)
      INTEGER INFO
      INTEGER*8 SOLVER_HANDLE
      CHARACTER*(*) MESSAGE
C=======================================================================
      INFO=0
      MESSAGE=''
      CALL EIG_SLEPC_RESULT_RELEASE_C(SOLVER_HANDLE,INFO)
      IF (INFO.GE.2000) THEN
        WRITE(MESSAGE,'(A,I0)')'SLEPc/PETSc cleanup failed with code ',
     .                          INFO-2000
      ENDIF
      RETURN
      END
#endif

!||====================================================================
!||    eig_solve_dense
!||====================================================================
      SUBROUTINE EIG_SOLVE_DENSE(N,K_IAD,K_JD,K_DIAG,K_LOWER,
     .          M_IAD,M_JD,M_DIAG,M_LOWER,NREQ,RANGE_ALL,
     .          RETAIN_MODE_SHAPES,SHIFT2,CUTFREQ,TOL,TOL_RIGID,
     .          EVAL_OUT,EVECS,NFOUND,INFO,MESSAGE)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
      INTEGER N,K_IAD(*),K_JD(*),M_IAD(*),M_JD(*),NREQ,RANGE_ALL,
     .        RETAIN_MODE_SHAPES,NFOUND,INFO
      my_real K_DIAG(*),K_LOWER(*),M_DIAG(*),M_LOWER(*),SHIFT2,
     .        CUTFREQ,TOL,TOL_RIGID,EVAL_OUT(*),EVECS(N,*)
      CHARACTER*(*) MESSAGE
      CHARACTER*1 JOBZ
      INTEGER I,J,COL,IOS,LWORK,LAPACK_INFO,MODE,NZERO,
     .        NEG_INDEX,POS_INDEX
      my_real BOUND_REL,LOWER,UPPER,ANGULAR,DTOL,ZERO_UPPER,
     .        SPECTRAL_SCALE,ROUND_OFF_FLOOR,CANDIDATE_UPPER,
     .        PREVIOUS_MAG,RATIO
      my_real, DIMENSION(:), ALLOCATABLE :: EVAL,WORK
      my_real, DIMENSION(:,:), ALLOCATABLE :: DENSE_K,DENSE_M
C=======================================================================
      INFO=0
      MESSAGE=''
      NFOUND=0
      JOBZ='V'
      IF (RETAIN_MODE_SHAPES.EQ.0) JOBZ='N'
C Keep inclusive decimal input boundaries stable when the computed mode is a
C few solver ulps to either side. Dense extraction has no separate search
C interval, so use the same final acceptance margin as the SLEPc path rather
C than SLEPc's deliberately wider 64x search guard.
      DTOL=TOL
      IF (DTOL.LE.ZERO) DTOL=1.0E-8
      BOUND_REL=MIN(MAX(DTOL,64.0*EPSILON(ONE)),1.0E-8)
      LOWER=SHIFT2-BOUND_REL*MAX(ABS(SHIFT2),ONE)
      UPPER=HUGE(ONE)
      IF (CUTFREQ.GT.ZERO) THEN
        ANGULAR=TWO*PI*CUTFREQ
        UPPER=ANGULAR*ANGULAR
        UPPER=UPPER+BOUND_REL*MAX(ABS(UPPER),ONE)
      ENDIF
#if defined(MYREAL8) && defined(PUBLIC_EIG_LAPACK)
      ALLOCATE(DENSE_K(N,N),DENSE_M(N,N),EVAL(N),WORK(1),
     .         STAT=IOS)
      IF (IOS.NE.0) THEN
        INFO=1
        MESSAGE='unable to allocate dense /EIG solver workspace'
        RETURN
      ENDIF
      DENSE_K=ZERO
      DENSE_M=ZERO
      DO I=1,N
        DENSE_K(I,I)=K_DIAG(I)
        DO J=K_IAD(I),K_IAD(I+1)-1
          COL=K_JD(J)
          DENSE_K(I,COL)=DENSE_K(I,COL)+K_LOWER(J)
          DENSE_K(COL,I)=DENSE_K(COL,I)+K_LOWER(J)
        ENDDO
        DENSE_M(I,I)=M_DIAG(I)
        DO J=M_IAD(I),M_IAD(I+1)-1
          COL=M_JD(J)
          DENSE_M(I,COL)=DENSE_M(I,COL)+M_LOWER(J)
          DENSE_M(COL,I)=DENSE_M(COL,I)+M_LOWER(J)
        ENDDO
      ENDDO
C
      LWORK=-1
      CALL DSYGV(1,JOBZ,'L',N,DENSE_K,N,DENSE_M,N,
     .           EVAL,WORK,LWORK,LAPACK_INFO)
      IF (LAPACK_INFO.NE.0) THEN
        INFO=2
        WRITE(MESSAGE,'(A,I0)')
     .       'DSYGV workspace query failed with INFO=',LAPACK_INFO
        DEALLOCATE(DENSE_K,DENSE_M,EVAL,WORK)
        RETURN
      ENDIF
      LWORK=MAX(3*N-1,INT(WORK(1)))
      DEALLOCATE(WORK)
      ALLOCATE(WORK(LWORK),STAT=IOS)
      IF (IOS.NE.0) THEN
        INFO=1
        MESSAGE='unable to allocate dense /EIG LAPACK workspace'
        DEALLOCATE(DENSE_K,DENSE_M,EVAL)
        RETURN
      ENDIF
      CALL DSYGV(1,JOBZ,'L',N,DENSE_K,N,DENSE_M,N,
     .           EVAL,WORK,LWORK,LAPACK_INFO)
      IF (LAPACK_INFO.NE.0) THEN
        IF (LAPACK_INFO.GT.N) THEN
          INFO=3
          WRITE(MESSAGE,'(A,I0)')
     .      'reduced mass is not positive definite; leading minor ',
     .      LAPACK_INFO-N
        ELSE
          INFO=4
          WRITE(MESSAGE,'(A,I0)')
     .      'DSYGV generalized eigensolve failed with INFO=',LAPACK_INFO
        ENDIF
        DEALLOCATE(DENSE_K,DENSE_M,EVAL,WORK)
        RETURN
      ENDIF
C
      ZERO_UPPER=ZERO
      IF (SHIFT2.LE.ZERO) THEN
C DSYGV returns the complete algebraic spectrum.  Merge its negative and
C non-negative halves by absolute magnitude into the now-unused WORK array,
C then apply the same scale-bounded 100x gap rule as the distributed path.
C A gap is a nullspace boundary only while its lower value is inside the
C configured near-null band; scanning the complete spectrum would discard a
C genuine soft mode followed by an unrelated large physical frequency gap.
        SPECTRAL_SCALE=MAX(MAXVAL(ABS(EVAL(1:N))),ONE)
        ROUND_OFF_FLOOR=MAX(EPSILON(ONE)*SPECTRAL_SCALE,TINY(ONE))
        CANDIDATE_UPPER=MAX(ROUND_OFF_FLOOR,
     .    MAX(TOL_RIGID,128.0*EPSILON(ONE))*SPECTRAL_SCALE)
        ZERO_UPPER=ROUND_OFF_FLOOR
        NEG_INDEX=COUNT(EVAL(1:N).LT.ZERO)
        POS_INDEX=NEG_INDEX+1
        DO MODE=1,N
          IF (NEG_INDEX.LT.1) THEN
            WORK(MODE)=ABS(EVAL(POS_INDEX))
            POS_INDEX=POS_INDEX+1
          ELSEIF (POS_INDEX.GT.N) THEN
            WORK(MODE)=ABS(EVAL(NEG_INDEX))
            NEG_INDEX=NEG_INDEX-1
          ELSEIF (ABS(EVAL(NEG_INDEX)).LE.ABS(EVAL(POS_INDEX))) THEN
            WORK(MODE)=ABS(EVAL(NEG_INDEX))
            NEG_INDEX=NEG_INDEX-1
          ELSE
            WORK(MODE)=ABS(EVAL(POS_INDEX))
            POS_INDEX=POS_INDEX+1
          ENDIF
        ENDDO
        NZERO=COUNT(WORK(1:N).LE.ROUND_OFF_FLOOR)
        RATIO=ZERO
        DO I=MAX(NZERO,1),N-1
          IF (WORK(I).GT.CANDIDATE_UPPER) EXIT
          PREVIOUS_MAG=MAX(WORK(I),ROUND_OFF_FLOOR)
          RATIO=MAX(WORK(I+1),ROUND_OFF_FLOOR)/PREVIOUS_MAG
          IF (RATIO.GE.100.0) THEN
            NZERO=I
            ZERO_UPPER=PREVIOUS_MAG
            EXIT
          ENDIF
        ENDDO
        WRITE(6,'(A,1PE18.10,A,1PE18.10,A,1PE12.5,A,I0)')
     .    'Public /EIG dense zero-cluster upper eigenvalue=',
     .    ZERO_UPPER,', candidate ceiling=',CANDIDATE_UPPER,
     .    ', gap=',RATIO,', members=',NZERO
        WRITE(IOUT,'(A,1PE18.10,A,1PE18.10,A,1PE12.5,A,I0)')
     .    'Public /EIG dense zero-cluster upper eigenvalue=',
     .    ZERO_UPPER,', candidate ceiling=',CANDIDATE_UPPER,
     .    ', gap=',RATIO,', members=',NZERO
      ENDIF
C
      IF (RANGE_ALL.NE.0) THEN
        MODE=0
        DO I=1,N
          IF (EVAL(I).GE.LOWER.AND.EVAL(I).GT.ZERO_UPPER) THEN
            IF (EVAL(I).LE.UPPER) MODE=MODE+1
          ENDIF
        ENDDO
        WRITE(6,'(A,I0,A)')
     .    'Public /EIG bounded interval contains ',MODE,
     .    ' eigenvalues'
        WRITE(IOUT,'(A,I0,A)')
     .    'Public /EIG bounded interval contains ',MODE,
     .    ' eigenvalues'
        IF (RANGE_ALL.EQ.1.AND.MODE.GT.NREQ) THEN
          NFOUND=MODE
          INFO=22
          MESSAGE='dense interval result capacity is too small'
          DEALLOCATE(DENSE_K,DENSE_M,EVAL,WORK)
          RETURN
        ENDIF
        IF (RANGE_ALL.EQ.2.AND.MODE.LT.NREQ) THEN
          WRITE(6,'(A,I0,A,I0,A)')
     .      'WARNING: /EIG requested ',NREQ,
     .      ' modes, but the bounded interval contains ',MODE,
     .      '; returning all available modes'
          WRITE(IOUT,'(A,I0,A,I0,A)')
     .      'WARNING: /EIG requested ',NREQ,
     .      ' modes, but the bounded interval contains ',MODE,
     .      '; returning all available modes'
        ENDIF
      ENDIF
C
      MODE=0
      DO I=1,N
        IF (EVAL(I).GE.LOWER.AND.EVAL(I).GT.ZERO_UPPER) THEN
          IF (EVAL(I).LE.UPPER) THEN
            MODE=MODE+1
            EVAL_OUT(MODE)=EVAL(I)
            IF (RETAIN_MODE_SHAPES.NE.0)
     .        EVECS(1:N,MODE)=DENSE_K(1:N,I)
            IF (MODE.EQ.NREQ) EXIT
          ENDIF
        ENDIF
      ENDDO
      NFOUND=MODE
      IF (NFOUND.EQ.0) THEN
        INFO=11
        MESSAGE='no eigenmodes found in the requested frequency range'
      ELSEIF (RANGE_ALL.NE.0) THEN
        WRITE(6,'(A,I0,A)')'Public /EIG solved ',NFOUND,
     .    ' modes in the requested range in one dense pass'
        WRITE(IOUT,'(A,I0,A)')'Public /EIG solved ',NFOUND,
     .    ' modes in the requested range in one dense pass'
      ELSEIF (NFOUND.LT.NREQ) THEN
        WRITE(6,'(A,I0,A,I0,A,A)')
     .    'WARNING: /EIG requested ',NREQ,
     .    ' modes, but only ',NFOUND,
     .    ' exist at or above the requested minimum; returning all ',
     .    'available modes'
        WRITE(IOUT,'(A,I0,A,I0,A,A)')
     .    'WARNING: /EIG requested ',NREQ,
     .    ' modes, but only ',NFOUND,
     .    ' exist at or above the requested minimum; returning all ',
     .    'available modes'
      ENDIF
      DEALLOCATE(DENSE_K,DENSE_M,EVAL,WORK)
#else
      INFO=7
      MESSAGE='dense backend requires double precision and bundled LAPACK'
#endif
      RETURN
      END
