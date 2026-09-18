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
!||    eig_matrix_export
!||
!|| Solve the public /EIG matrix interface.  IMP_K_EIG has
!|| already assembled the tangent stiffness and removed ordinary model
!|| constraints.  This routine applies the /EIG node selection and its
!|| optional clamped degrees of freedom, compacts the stiffness and physical
!|| mass operators for the selected backend, and retains the solved modes.
!||====================================================================
      SUBROUTINE EIG_MATRIX_EXPORT(EIGIPM,EIGIBUF,EIGRPM,
     .          NDDL0,NDDL,
     .          DIAG_K,LT_K,IADK,JDIK,DIAG_M,LT_M,IADM,JDIM,
     .          IKC,IDDL,NDOF,NODGLOB,X,RETAIN_MODE_SHAPES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EIG_MOD, ONLY : PUBLIC_EIG_READY,PUBLIC_EIG_REDUCED,
     .     PUBLIC_EIG_SOLVER_HANDLE,PUBLIC_EIG_EXPORT_NLOCAL,
     .     PUBLIC_EIG_EXPORT_ROW,PUBLIC_EIG_STREAMED,
     .     CLEAR_PUBLIC_EIG_RESULTS
      USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "eigcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EIGIPM(*),EIGIBUF(*),NDDL0,NDDL,IADK(*),JDIK(*),
     .        IADM(*),JDIM(*),IKC(*),IDDL(*),NDOF(*),NODGLOB(*)
      my_real DIAG_K(*),LT_K(*),DIAG_M(*),LT_M(*),EIGRPM(*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ND,IC,IR,NN,NNB,NOUT,NCOND,NZERO,
     .        IOS,ROW,EROW,ECOL,BDOF(6),
     .        NREQ,NFOUND,INFO,NKIN,POS,NNZKLOW,NNZMLOW,
     .        NGLOBAL,NOWNED,MAP_INFO,RANGE_ALL,NVEC_CAPACITY,
     .        OUTPUT_RIGID,RETAIN_MODE_SHAPES
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODE_TAG,ORIG_NODE,
     .        ORIG_COMP,ORIG_TO_COND,COND_TO_EXPORT,EXPORT_ORIG,
     .        K_IAD,K_JD,M_IAD,M_JD,EXPORT_GLOBAL_NODE,
     .        EXPORT_COMPONENT
      INTEGER*8 ROW_START,SOLVER_HANDLE
      INTEGER*8, DIMENSION(:), ALLOCATABLE :: GLOBAL_DOF
      my_real, DIMENSION(:), ALLOCATABLE :: EVAL,K_DIAG_S,K_LOWER_S,
     .        M_DIAG_S,M_LOWER_S
      my_real, DIMENSION(:,:), ALLOCATABLE :: EVECS
      my_real, DIMENSION(:,:), ALLOCATABLE :: EXPORT_COORD
      my_real SHIFT2,CUTFREQ,TOL_EIG,TOL_RIGID,TOL_PIVOT_UPPER,
     .        TOL_PIVOT_POSITIVE
      CHARACTER*32 BACKEND
      CHARACTER*256 MESSAGE
      LOGICAL DIRECT_LAYOUT,STREAMED_RESULTS
C=======================================================================
      CALL EIG_RELEASE_STREAMED_RESULTS()
      CALL CLEAR_PUBLIC_EIG_RESULTS()
      STREAMED_RESULTS=.FALSE.
      NN=EIGIPM(10)
      NNB=EIGIPM(11)
C
      ALLOCATE(NODE_TAG(NUMNOD),ORIG_NODE(NDDL0),
     .         ORIG_COMP(NDDL0),ORIG_TO_COND(NDDL0),
     .         COND_TO_EXPORT(NDDL),EXPORT_ORIG(NDDL),
     .         STAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to allocate /EIG mapping workspace'
        WRITE(IOUT,*)'ERROR: unable to allocate /EIG mapping workspace'
        CALL ARRET(5)
      ENDIF
      NODE_TAG=0
      ORIG_NODE=0
      ORIG_COMP=0
      ORIG_TO_COND=0
      COND_TO_EXPORT=0
      EXPORT_ORIG=0
C
      DO I=1,NN
        N=EIGIBUF(I)
        IF (N.LT.1.OR.N.GT.NUMNOD) GOTO 910
        NODE_TAG(N)=1
      ENDDO
      DO I=1,NNB
        N=EIGIBUF(NN+I)
        IF (N.LT.1.OR.N.GT.NUMNOD) GOTO 910
        NODE_TAG(N)=2
      ENDDO
C
C Recreate the original scalar-DOF ownership before condensation.
      DO N=1,NUMNOD
        DO J=1,NDOF(N)
          ND=IDDL(N)+J
          IF (ND.GE.1.AND.ND.LE.NDDL0) THEN
            ORIG_NODE(ND)=N
            ORIG_COMP(ND)=J
          ENDIF
        ENDDO
      ENDDO
      DO ND=1,NDDL0
        IF (ORIG_NODE(ND).EQ.0) THEN
          WRITE(ISTDO,*)'ERROR: incomplete /EIG DOF map at ',ND
          WRITE(IOUT,*)'ERROR: incomplete /EIG DOF map at ',ND
          CALL ARRET(5)
        ENDIF
      ENDDO
C
C Map original scalar DOFs to the already model-constrained matrix.
      NCOND=0
      NKIN=0
      DO ND=1,NDDL0
        IF (IKC(ND).LT.1) THEN
          NCOND=NCOND+1
          ORIG_TO_COND(ND)=NCOND
        ELSEIF (IKC(ND).GT.1) THEN
          NKIN=NKIN+1
        ENDIF
      ENDDO
      IF (NCOND.NE.NDDL) THEN
        WRITE(ISTDO,*)'ERROR: /EIG condensed DOF count mismatch ',
     .                 NCOND,NDDL
        WRITE(IOUT,*)'ERROR: /EIG condensed DOF count mismatch ',
     .                NCOND,NDDL
        CALL ARRET(5)
      ENDIF
C
C A selected submodel with kinematic constraints needs dependency-closure
C semantics, while clamping a dependent coordinate needs an additional
C linear constraint.  Neither operation may be approximated by deleting a
C reduced row, so retain the established behavior only for unconstrained
C models and require the complete model for kinematic constraints for now.
      IF (NKIN.GT.0.AND.(NN.NE.NUMNOD.OR.NNB.GT.0)) THEN
        WRITE(ISTDO,*)'ERROR: /EIG kinematic constraints require all ',
     .                 'nodes and no /EIG clamp'
        WRITE(IOUT,*)'ERROR: /EIG kinematic constraints require all ',
     .                'nodes and no /EIG clamp'
        CALL ARRET(5)
      ENDIF
C
C Decode the /EIG translational and rotational clamp bit patterns.
      BDOF=0
      IC=EIGIPM(3)
      IR=EIGIPM(4)
      IF (IC.LT.0.OR.IC.GT.7.OR.IR.LT.0.OR.IR.GT.7) THEN
        WRITE(ISTDO,*)'ERROR: invalid /EIG clamp codes ',IC,IR
        WRITE(IOUT,*)'ERROR: invalid /EIG clamp codes ',IC,IR
        CALL ARRET(5)
      ENDIF
      IF (IAND(IC,4).NE.0) BDOF(1)=1
      IF (IAND(IC,2).NE.0) BDOF(2)=1
      IF (IAND(IC,1).NE.0) BDOF(3)=1
      IF (IAND(IR,4).NE.0) BDOF(4)=1
      IF (IAND(IR,2).NE.0) BDOF(5)=1
      IF (IAND(IR,1).NE.0) BDOF(6)=1
C
C Apply /EIG selection and clamp elimination without modifying K.
      NOUT=0
      DO ND=1,NDDL0
        IF (IKC(ND).LT.1) THEN
          N=ORIG_NODE(ND)
          J=ORIG_COMP(ND)
          IF (NODE_TAG(N).GT.0) THEN
            IF (NODE_TAG(N).NE.2.OR.J.GT.6.OR.BDOF(J).EQ.0) THEN
              NOUT=NOUT+1
              COND_TO_EXPORT(ORIG_TO_COND(ND))=NOUT
              EXPORT_ORIG(NOUT)=ND
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C Compact the selected constrained operators into the backend-neutral
C symmetric sparse representation.  Keeping a diagonal and only the lower
C triangle preserves the Engine storage semantics and avoids dense or
C backend-specific matrix copies at this boundary.
      NNZKLOW=0
      NNZMLOW=0
      DO ROW=1,NDDL
        EROW=COND_TO_EXPORT(ROW)
        IF (EROW.GT.0) THEN
          DO J=IADK(ROW),IADK(ROW+1)-1
            IF (COND_TO_EXPORT(JDIK(J)).GT.0) NNZKLOW=NNZKLOW+1
          ENDDO
          DO J=IADM(ROW),IADM(ROW+1)-1
            IF (COND_TO_EXPORT(JDIM(J)).GT.0.AND.LT_M(J).NE.ZERO)
     .        NNZMLOW=NNZMLOW+1
          ENDDO
        ENDIF
      ENDDO
      ALLOCATE(K_IAD(NOUT+1),K_JD(MAX(1,NNZKLOW)),
     .         K_DIAG_S(NOUT),K_LOWER_S(MAX(1,NNZKLOW)),
     .         M_IAD(NOUT+1),M_JD(MAX(1,NNZMLOW)),
     .         M_DIAG_S(NOUT),M_LOWER_S(MAX(1,NNZMLOW)),STAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to allocate /EIG sparse operators'
        WRITE(IOUT,*)'ERROR: unable to allocate /EIG sparse operators'
        CALL ARRET(5)
      ENDIF
      POS=1
      DO ROW=1,NDDL
        EROW=COND_TO_EXPORT(ROW)
        IF (EROW.GT.0) THEN
          K_IAD(EROW)=POS
          K_DIAG_S(EROW)=DIAG_K(ROW)
          DO J=IADK(ROW),IADK(ROW+1)-1
            ECOL=COND_TO_EXPORT(JDIK(J))
            IF (ECOL.GT.0) THEN
              K_JD(POS)=ECOL
              K_LOWER_S(POS)=LT_K(J)
              POS=POS+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      K_IAD(NOUT+1)=POS
      POS=1
      DO ROW=1,NDDL
        EROW=COND_TO_EXPORT(ROW)
        IF (EROW.GT.0) THEN
          M_IAD(EROW)=POS
          M_DIAG_S(EROW)=DIAG_M(ROW)
          DO J=IADM(ROW),IADM(ROW+1)-1
            ECOL=COND_TO_EXPORT(JDIM(J))
            IF (ECOL.GT.0.AND.LT_M(J).NE.ZERO) THEN
              M_JD(POS)=ECOL
              M_LOWER_S(POS)=LT_M(J)
              POS=POS+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      M_IAD(NOUT+1)=POS
C
C Assign each (global node, component) key to one rank.  Duplicate frontier
C DOFs receive the same global number; the solver layer subsequently sums
C their rank-local K and M contributions at that owner.
      ALLOCATE(EXPORT_GLOBAL_NODE(MAX(1,NOUT)),
     .         EXPORT_COMPONENT(MAX(1,NOUT)),
     .         EXPORT_COORD(3,MAX(1,NOUT)),
     .         GLOBAL_DOF(MAX(1,NOUT)),STAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to allocate /EIG distributed map'
        WRITE(IOUT,*)'ERROR: unable to allocate /EIG distributed map'
        CALL ARRET(5)
      ENDIF
      DO I=1,NOUT
        ND=EXPORT_ORIG(I)
        N=ORIG_NODE(ND)
        EXPORT_GLOBAL_NODE(I)=NODGLOB(N)
        EXPORT_COMPONENT(I)=ORIG_COMP(ND)
        EXPORT_COORD(1:3,I)=X(1:3,N)
      ENDDO
      CALL EIG_BUILD_GLOBAL_DOF_MAP_C(SPMD_COMM_WORLD,NOUT,
     .     EXPORT_GLOBAL_NODE,EXPORT_COMPONENT,GLOBAL_DOF,NGLOBAL,
     .     NOWNED,ROW_START,MAP_INFO)
      IF (MAP_INFO.NE.0.OR.NGLOBAL.LE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to build /EIG global DOF map ',
     .                 MAP_INFO
        WRITE(IOUT,*)'ERROR: unable to build /EIG global DOF map ',
     .                MAP_INFO
        CALL ARRET(5)
      ENDIF
      NZERO=0
      DO I=1,NOUT
        IF (M_DIAG_S(I).LE.ZERO) NZERO=NZERO+1
      ENDDO
C
      IF (NSPMD.EQ.1.AND.NZERO.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: /EIG mass has ',NZERO,
     .                 ' non-positive diagonal entries'
        WRITE(IOUT,*)'ERROR: /EIG mass has ',NZERO,
     .                ' non-positive diagonal entries'
        CALL ARRET(5)
      ENDIF
      SHIFT2=MAX(ZERO,EIGRPM(3))
      CUTFREQ=EIGRPM(4)
      RANGE_ALL=0
      OUTPUT_RIGID=1
      IF (NEIPM.GE.18) OUTPUT_RIGID=EIGIPM(18)
      TOL_EIG=1.0E-8
      TOL_RIGID=1.0E-12
      TOL_PIVOT_UPPER=1.0E-12
      TOL_PIVOT_POSITIVE=1.0E-12
      IF (EIGRPM(1).GT.ZERO) TOL_EIG=EIGRPM(1)
      IF (NERPM.GE.5.AND.EIGRPM(5).GT.ZERO) TOL_RIGID=EIGRPM(5)
      IF (NERPM.GE.7.AND.EIGRPM(7).GT.ZERO)
     .  TOL_PIVOT_UPPER=EIGRPM(7)
      IF (NERPM.GE.8.AND.EIGRPM(8).GT.ZERO)
     .  TOL_PIVOT_POSITIVE=EIGRPM(8)
      SOLVER_HANDLE=0
      IF (OUTPUT_RIGID.EQ.2) THEN
C Irigid=2 solves only a tolerance-sized near-null interval and writes modes
C accepted by both SLEPc and the physical rigid-template subspace.
        RANGE_ALL=3
        NREQ=1
      ELSEIF (EIGIPM(5).LT.0.AND.CUTFREQ.GT.ZERO) THEN
        RANGE_ALL=1
C The serial dense verifier is limited to 5000 DOFs, so reserving its full
C result capacity is cheaper than repeating a complete dense eigensolve.
C Large SLEPc problems use an exact-size retained-context handshake instead.
        IF (NSPMD.EQ.1.AND.NGLOBAL.LE.5000) THEN
          NREQ=NGLOBAL
        ELSE
          NREQ=1
        ENDIF
      ELSEIF (EIGIPM(5).GT.0.AND.CUTFREQ.GT.ZERO) THEN
C A fixed-count bounded request uses native spectrum slicing, then keeps
C the lowest requested modes from the complete interval result.
        RANGE_ALL=2
        NREQ=MIN(EIGIPM(5),NGLOBAL)
      ELSE
        NREQ=MIN(MAX(EIGIPM(5),0),NGLOBAL)
      ENDIF
      IF (NREQ.LE.0) THEN
        WRITE(ISTDO,*)'ERROR: /EIG requested no eigenmodes'
        WRITE(IOUT,*)'ERROR: /EIG requested no eigenmodes'
        CALL ARRET(5)
      ENDIF
      IF (ISPMD.EQ.0) THEN
        WRITE(ISTDO,'(A,1PE12.5,A,1PE12.5,A,1PE12.5,A,1PE12.5)')
     .    'Public /EIG tolerances: eig=',TOL_EIG,', rigid=',
     .    TOL_RIGID,', pivot_interval=',TOL_PIVOT_UPPER,
     .    ', pivot_target=',TOL_PIVOT_POSITIVE
        WRITE(IOUT,'(A,1PE12.5,A,1PE12.5,A,1PE12.5,A,1PE12.5)')
     .    'Public /EIG tolerances: eig=',TOL_EIG,', rigid=',
     .    TOL_RIGID,', pivot_interval=',TOL_PIVOT_UPPER,
     .    ', pivot_target=',TOL_PIVOT_POSITIVE
      ENDIF
C Distributed and large SLEPc solves return a retained context and never
C populate this legacy direct-result argument. Keep one valid column for that
C ABI instead of allocating an unused local-DOF by requested-mode array. A
C small serial problem may still select the dense verifier, which writes all
C requested vectors directly and therefore retains full capacity.
      NVEC_CAPACITY=NREQ
      IF (RETAIN_MODE_SHAPES.EQ.0.OR.NSPMD.GT.1.OR.NGLOBAL.GT.5000)
     .  NVEC_CAPACITY=1
      ALLOCATE(EVAL(NREQ),EVECS(MAX(1,NOUT),NVEC_CAPACITY),
     .         STAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to allocate /EIG result vectors'
        WRITE(IOUT,*)'ERROR: unable to allocate /EIG result vectors'
        CALL ARRET(5)
      ENDIF
      CALL EIG_SOLVE_BACKEND(NOUT,NGLOBAL,NOWNED,ROW_START,
     .     SPMD_COMM_WORLD,GLOBAL_DOF,
#if defined(PUBLIC_EIG_SLEPC)
     .     EXPORT_GLOBAL_NODE,EXPORT_COMPONENT,EXPORT_COORD,NSPMD,
#else
     .     NSPMD,
#endif
     .     K_IAD,K_JD,K_DIAG_S,K_LOWER_S,
     .     M_IAD,M_JD,M_DIAG_S,M_LOWER_S,NREQ,RANGE_ALL,
     .     OUTPUT_RIGID,RETAIN_MODE_SHAPES,
     .     SHIFT2,CUTFREQ,TOL_EIG,TOL_RIGID,
     .     TOL_PIVOT_UPPER,TOL_PIVOT_POSITIVE,
     .     EIGIPM(7),EIGIPM(6),
     .     SOLVER_HANDLE,
     .     EVAL,EVECS,NFOUND,BACKEND,INFO,MESSAGE)
      IF (ISPMD.EQ.0) THEN
        WRITE(ISTDO,'(A,A)')'Public /EIG selected backend: ',
     .                       TRIM(BACKEND)
        WRITE(IOUT,'(A,A)')'Public /EIG selected backend: ',
     .                      TRIM(BACKEND)
      ENDIF
      IF (INFO.EQ.19) THEN
        IF (NFOUND.LT.1.OR.NFOUND.GT.NGLOBAL) THEN
          WRITE(ISTDO,*)'ERROR: invalid /EIG interval mode count ',
     .                  NFOUND
          WRITE(IOUT,*)'ERROR: invalid /EIG interval mode count ',
     .                 NFOUND
          CALL ARRET(5)
        ENDIF
        IF (ISPMD.EQ.0) THEN
          WRITE(ISTDO,*)'Public /EIG found ',NFOUND,
     .                  ' retained output modes'
          WRITE(IOUT,*)'Public /EIG found ',NFOUND,
     .                 ' retained output modes'
        ENDIF
        NREQ=NFOUND
        DEALLOCATE(EVAL,EVECS)
        ALLOCATE(EVAL(NREQ),STAT=IOS)
        IF (IOS.NE.0) THEN
          WRITE(ISTDO,*)'ERROR: unable to allocate /EIG mode metadata'
          WRITE(IOUT,*)'ERROR: unable to allocate /EIG mode metadata'
          CALL ARRET(5)
        ENDIF
#if defined(PUBLIC_EIG_SLEPC)
        CALL EIG_SLEPC_RESULT_METADATA(SOLVER_HANDLE,NREQ,EVAL,
     .       NFOUND,INFO,MESSAGE)
        STREAMED_RESULTS=INFO.EQ.0
#else
        INFO=7
        MESSAGE='retained result requires SLEPc'
#endif
      ENDIF
      IF (INFO.NE.0) THEN
        IF (INFO.EQ.11) THEN
          WRITE(ISTDO,*)'ERROR: /EIG found no modes in requested range'
          WRITE(IOUT,*)'ERROR: /EIG found no modes in requested range'
        ELSE
          WRITE(ISTDO,*)'ERROR: /EIG ',TRIM(BACKEND),' backend: ',
     .                   TRIM(MESSAGE)
          WRITE(IOUT,*)'ERROR: /EIG ',TRIM(BACKEND),' backend: ',
     .                  TRIM(MESSAGE)
        ENDIF
        CALL ARRET(5)
      ENDIF
      CALL EIG_RETAIN_RESULTS(EIGIPM,EIGIBUF,EVAL,NFOUND)
      DIRECT_LAYOUT=NOUT.EQ.NDDL
      IF (DIRECT_LAYOUT) THEN
        DO ROW=1,NDDL
          IF (COND_TO_EXPORT(ROW).NE.ROW) THEN
            DIRECT_LAYOUT=.FALSE.
            EXIT
          ENDIF
        ENDDO
      ENDIF
      IF (STREAMED_RESULTS) THEN
        PUBLIC_EIG_SOLVER_HANDLE=SOLVER_HANDLE
        SOLVER_HANDLE=0
        PUBLIC_EIG_EXPORT_NLOCAL=NOUT
        PUBLIC_EIG_STREAMED=.TRUE.
        IF (.NOT.DIRECT_LAYOUT) THEN
          ALLOCATE(PUBLIC_EIG_EXPORT_ROW(NDDL),STAT=IOS)
          IF (IOS.NE.0) THEN
            WRITE(ISTDO,*)
     .        'ERROR: unable to retain /EIG export row map'
            WRITE(IOUT,*)
     .        'ERROR: unable to retain /EIG export row map'
            CALL ARRET(5)
          ENDIF
          PUBLIC_EIG_EXPORT_ROW=COND_TO_EXPORT
        ENDIF
      ELSEIF (RETAIN_MODE_SHAPES.EQ.0) THEN
        DEALLOCATE(EVECS)
      ELSEIF (DIRECT_LAYOUT) THEN
        CALL MOVE_ALLOC(EVECS,PUBLIC_EIG_REDUCED)
      ELSE
        ALLOCATE(PUBLIC_EIG_REDUCED(NDDL,NFOUND),STAT=IOS)
        IF (IOS.NE.0) THEN
          WRITE(ISTDO,*)'ERROR: unable to retain /EIG reduced modes'
          WRITE(IOUT,*)'ERROR: unable to retain /EIG reduced modes'
          CALL ARRET(5)
        ENDIF
        PUBLIC_EIG_REDUCED=ZERO
        DO I=1,NFOUND
          DO ROW=1,NDDL
            EROW=COND_TO_EXPORT(ROW)
            IF (EROW.GT.0) PUBLIC_EIG_REDUCED(ROW,I)=EVECS(EROW,I)
          ENDDO
        ENDDO
      ENDIF
      PUBLIC_EIG_READY=.TRUE.
      IF (SOLVER_HANDLE.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: retained /EIG context was not consumed'
        WRITE(IOUT,*)'ERROR: retained /EIG context was not consumed'
        CALL ARRET(5)
      ENDIF
      DEALLOCATE(EVAL)
      IF (ALLOCATED(EVECS)) DEALLOCATE(EVECS)
      DEALLOCATE(NODE_TAG,ORIG_NODE,ORIG_COMP,ORIG_TO_COND,
     .           COND_TO_EXPORT,EXPORT_ORIG,K_IAD,K_JD,K_DIAG_S,
     .           K_LOWER_S,M_IAD,M_JD,M_DIAG_S,M_LOWER_S,
     .           EXPORT_GLOBAL_NODE,EXPORT_COMPONENT,EXPORT_COORD,
     .           GLOBAL_DOF)
      RETURN
C
  910 CONTINUE
      WRITE(ISTDO,*)'ERROR: /EIG contains invalid local node ',N
      WRITE(IOUT,*)'ERROR: /EIG contains invalid local node ',N
      CALL ARRET(5)
      RETURN
      END

!||====================================================================
!||    eig_retain_results
!||
!|| Copy the native solver result into the recovery/output hand-off.
!||====================================================================
      SUBROUTINE EIG_RETAIN_RESULTS(EIGIPM,EIGIBUF,EVAL,NFOUND)
      USE EIG_MOD, ONLY : PUBLIC_EIG_NMODE,PUBLIC_EIG_NNODE,
     .     PUBLIC_EIG_NODE,
     .     PUBLIC_EIG_FREQ,
     .     CLEAR_PUBLIC_EIG_RESULTS
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
      INTEGER EIGIPM(*),EIGIBUF(*),NFOUND
      my_real EVAL(*)
      INTEGER I,NN,IOS
C=======================================================================
      CALL CLEAR_PUBLIC_EIG_RESULTS()
      NN=EIGIPM(10)
      PUBLIC_EIG_NMODE=NFOUND
      PUBLIC_EIG_NNODE=NN
      ALLOCATE(PUBLIC_EIG_NODE(NN),
     .         PUBLIC_EIG_FREQ(NFOUND),STAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to retain /EIG modes'
        WRITE(IOUT,*)'ERROR: unable to retain /EIG modes'
        CALL ARRET(5)
      ENDIF
      PUBLIC_EIG_NODE(1:NN)=EIGIBUF(1:NN)
      DO I=1,NFOUND
        IF (EVAL(I).EQ.ZERO) THEN
          PUBLIC_EIG_FREQ(I)=ZERO
        ELSE
          PUBLIC_EIG_FREQ(I)=SQRT(MAX(EVAL(I),ZERO))/(TWO*PI)
        ENDIF
        IF (ISPMD.EQ.0) THEN
          IF (EVAL(I).EQ.ZERO) THEN
            WRITE(ISTDO,'(A,I3.3,A)')
     .        'Public /EIG exact rigid E',I,': frequency_hz=0'
            WRITE(IOUT,'(A,I3.3,A)')
     .        'Public /EIG exact rigid E',I,': frequency_hz=0'
          ELSE
            WRITE(ISTDO,'(A,I3.3,A,1PE18.10,A,1PE18.10)')
     .        'Public /EIG mode ',I,': eigenvalue=',
     .      EVAL(I),', frequency_hz=',PUBLIC_EIG_FREQ(I)
            WRITE(IOUT,'(A,I3.3,A,1PE18.10,A,1PE18.10)')
     .        'Public /EIG mode ',I,': eigenvalue=',
     .      EVAL(I),', frequency_hz=',PUBLIC_EIG_FREQ(I)
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END

!||====================================================================
!||    eig_load_mode_batch
!||
!|| Materialize only the current Nbloc batch from retained distributed
!|| SLEPc vectors.  Dense/small solves already own their complete reduced
!|| result and therefore need no work here.
!||====================================================================
      SUBROUTINE EIG_LOAD_MODE_BATCH(NDDL,MODE_FIRST,MODE_LAST)
      USE EIG_MOD, ONLY : PUBLIC_EIG_NMODE,PUBLIC_EIG_REDUCED,
     .     PUBLIC_EIG_SOLVER_HANDLE,PUBLIC_EIG_EXPORT_NLOCAL,
     .     PUBLIC_EIG_EXPORT_ROW,PUBLIC_EIG_STREAMED
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
      INTEGER NDDL,MODE_FIRST,MODE_LAST
      INTEGER MODE_COUNT,IOS,INFO,ROW,EXPORT_ROW
      my_real, DIMENSION(:,:), ALLOCATABLE :: EXPORT_BATCH
      CHARACTER*256 MESSAGE
C=======================================================================
      IF (.NOT.PUBLIC_EIG_STREAMED) RETURN
      IF (PUBLIC_EIG_SOLVER_HANDLE.EQ.0.OR.
     .    PUBLIC_EIG_EXPORT_NLOCAL.LT.0.OR.
     .    MODE_FIRST.LT.1.OR.MODE_LAST.GT.PUBLIC_EIG_NMODE.OR.
     .    MODE_LAST.LT.MODE_FIRST) THEN
        WRITE(ISTDO,*)'ERROR: invalid streamed /EIG batch request'
        WRITE(IOUT,*)'ERROR: invalid streamed /EIG batch request'
        CALL ARRET(5)
      ENDIF
      MODE_COUNT=MODE_LAST-MODE_FIRST+1
      IF (ALLOCATED(PUBLIC_EIG_REDUCED))
     .  DEALLOCATE(PUBLIC_EIG_REDUCED)
      ALLOCATE(PUBLIC_EIG_REDUCED(MAX(1,NDDL),MODE_COUNT),STAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: unable to allocate streamed /EIG batch'
        WRITE(IOUT,*)'ERROR: unable to allocate streamed /EIG batch'
        CALL ARRET(5)
      ENDIF
      IF (.NOT.ALLOCATED(PUBLIC_EIG_EXPORT_ROW)) THEN
        IF (PUBLIC_EIG_EXPORT_NLOCAL.NE.NDDL) THEN
          WRITE(ISTDO,*)'ERROR: inconsistent streamed /EIG layout'
          WRITE(IOUT,*)'ERROR: inconsistent streamed /EIG layout'
          CALL ARRET(5)
        ENDIF
#if defined(PUBLIC_EIG_SLEPC)
        CALL EIG_SLEPC_RESULT_BATCH(PUBLIC_EIG_SOLVER_HANDLE,
     .       MODE_FIRST,MODE_COUNT,NDDL,PUBLIC_EIG_REDUCED,
     .       INFO,MESSAGE)
#else
        INFO=7
        MESSAGE='streamed /EIG result requires SLEPc support'
#endif
      ELSE
        ALLOCATE(EXPORT_BATCH(MAX(1,PUBLIC_EIG_EXPORT_NLOCAL),
     .           MODE_COUNT),STAT=IOS)
        IF (IOS.NE.0) THEN
          WRITE(ISTDO,*)
     .      'ERROR: unable to allocate streamed /EIG export batch'
          WRITE(IOUT,*)
     .      'ERROR: unable to allocate streamed /EIG export batch'
          CALL ARRET(5)
        ENDIF
#if defined(PUBLIC_EIG_SLEPC)
        CALL EIG_SLEPC_RESULT_BATCH(PUBLIC_EIG_SOLVER_HANDLE,
     .       MODE_FIRST,MODE_COUNT,PUBLIC_EIG_EXPORT_NLOCAL,
     .       EXPORT_BATCH,INFO,MESSAGE)
#else
        INFO=7
        MESSAGE='streamed /EIG result requires SLEPc support'
#endif
        IF (INFO.EQ.0) THEN
          PUBLIC_EIG_REDUCED=ZERO
          DO ROW=1,NDDL
            EXPORT_ROW=PUBLIC_EIG_EXPORT_ROW(ROW)
            IF (EXPORT_ROW.GT.0) PUBLIC_EIG_REDUCED(ROW,1:MODE_COUNT)=
     .        EXPORT_BATCH(EXPORT_ROW,1:MODE_COUNT)
          ENDDO
        ENDIF
        DEALLOCATE(EXPORT_BATCH)
      ENDIF
      IF (INFO.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: streamed /EIG batch failed: ',
     .                 TRIM(MESSAGE)
        WRITE(IOUT,*)'ERROR: streamed /EIG batch failed: ',
     .                TRIM(MESSAGE)
        CALL ARRET(5)
      ENDIF
      RETURN
      END

!||====================================================================
!||    eig_release_streamed_results
!||====================================================================
      SUBROUTINE EIG_RELEASE_STREAMED_RESULTS()
      USE EIG_MOD, ONLY : PUBLIC_EIG_SOLVER_HANDLE,
     .                    PUBLIC_EIG_STREAMED
#include      "units_c.inc"
      INTEGER INFO
      CHARACTER*256 MESSAGE
C=======================================================================
      IF (PUBLIC_EIG_SOLVER_HANDLE.EQ.0) RETURN
#if defined(PUBLIC_EIG_SLEPC)
      CALL EIG_SLEPC_RESULT_RELEASE(PUBLIC_EIG_SOLVER_HANDLE,
     .                              INFO,MESSAGE)
#else
      INFO=7
      MESSAGE='streamed /EIG result requires SLEPc support'
#endif
      PUBLIC_EIG_STREAMED=.FALSE.
      IF (INFO.NE.0) THEN
        WRITE(ISTDO,*)'ERROR: streamed /EIG cleanup failed: ',
     .                 TRIM(MESSAGE)
        WRITE(IOUT,*)'ERROR: streamed /EIG cleanup failed: ',
     .                TRIM(MESSAGE)
        CALL ARRET(5)
      ENDIF
      RETURN
      END

!||====================================================================
!||    eig_expand_mode
!||
!|| Expand one reduced eigenvector with RECUDIS and RECUKIN, the same recovery
!|| path used by the production implicit and buckling solvers.  No constraint
!|| transformation is reproduced in the public /EIG implementation.
!||====================================================================
      SUBROUTINE EIG_EXPAND_MODE(EIGIPM,NDDL,IDDL,NDOF,IKC,INLOC,
     .          RBY,LPBY,NPBY,SKEW,ISKEW,ITAB,WEIGHT,MS,IN,
     .          IBFV,VEL,ICODT,ICODR,NRBYAC,IRBYAC,NINT2,IINT2,
     .          IPARI,INTBUF_TAB,X,XFRAME,LJ,IXR,IXC,IXTG,
     .          SH4TREE,SH3TREE,IRBE3,LRBE3,FRBE3,IRBE2,LRBE2,
     .          MODE_SLOT,D,DR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE EIG_MOD, ONLY : PUBLIC_EIG_NNODE,PUBLIC_EIG_NODE,
     .    PUBLIC_EIG_REDUCED
      USE SPMD_MOD
      USE INTBUFDEF_MOD
      USE ELEMENT_MOD, ONLY : NIXC,NIXR,NIXTG
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER EIGIPM(*),NDDL,IDDL(*),NDOF(*),IKC(*),INLOC(*),
     .        LPBY(*),NPBY(NNPBY,*),ISKEW(*),ITAB(*),WEIGHT(*),
     .        IBFV(NIFV,*),ICODT(*),ICODR(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),LJ(*),IXR(NIXR,*),
     .        IXC(NIXC,*),IXTG(NIXTG,*),SH4TREE(*),SH3TREE(*),
     .        IRBE3(NRBE3L,*),LRBE3(*),IRBE2(*),LRBE2(*),
     .        MODE_SLOT
      my_real RBY(NRBY,*),SKEW(LSKEW,*),MS(*),IN(*),
     .        VEL(LFXVELR,*),X(3,*),XFRAME(NXFRAME,*),FRBE3(*),
     .        D(3,*),DR(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
      INTEGER I,N
      my_real VMAX,VMAX_GLOBAL
C=======================================================================
      IF (.NOT.ALLOCATED(PUBLIC_EIG_REDUCED)) THEN
        WRITE(ISTDO,*)'ERROR: missing reduced /EIG mode batch'
        WRITE(IOUT,*)'ERROR: missing reduced /EIG mode batch'
        CALL ARRET(5)
      ENDIF
      IF (MODE_SLOT.LT.1.OR.
     .    MODE_SLOT.GT.SIZE(PUBLIC_EIG_REDUCED,2)) THEN
        WRITE(ISTDO,*)'ERROR: invalid /EIG recovery batch request'
        WRITE(IOUT,*)'ERROR: invalid /EIG recovery batch request'
        CALL ARRET(5)
      ENDIF
      D(1:3,1:NUMNOD)=ZERO
      DR(1:3,1:NUMNOD)=ZERO
      CALL RECUDIS(NDDL,IDDL,NDOF,IKC,
     .             PUBLIC_EIG_REDUCED(1,MODE_SLOT),D,DR,INLOC)
      CALL RECUKIN(RBY,LPBY,NPBY,SKEW,ISKEW,
     1               ITAB,WEIGHT,MS,IN,
     2               IBFV,VEL,ICODT,ICODR,
     3               NRBYAC,IRBYAC,NINT2,IINT2,IPARI,
     4               INTBUF_TAB,NDOF,D,DR,
     5               X,XFRAME,LJ,IXR,IXC,
     6               IXTG,SH4TREE,SH3TREE,IRBE3,LRBE3,
     7               FRBE3,IRBE2,LRBE2)
      IF (EIGIPM(15).EQ.1) THEN
        VMAX=ZERO
        DO I=1,PUBLIC_EIG_NNODE
          N=PUBLIC_EIG_NODE(I)
          VMAX=MAX(VMAX,MAXVAL(ABS(D(1:3,N))))
          IF (NDOF(N).GT.3)
     .      VMAX=MAX(VMAX,MAXVAL(ABS(DR(1:3,N))))
        ENDDO
        IF (NSPMD.GT.1) THEN
          CALL SPMD_ALLREDUCE(VMAX,VMAX_GLOBAL,1,SPMD_MAX)
          VMAX=VMAX_GLOBAL
        ENDIF
        IF (VMAX.GT.ZERO) THEN
          D(1:3,1:NUMNOD)=D(1:3,1:NUMNOD)/VMAX
          DR(1:3,1:NUMNOD)=DR(1:3,1:NUMNOD)/VMAX
        ENDIF
      ENDIF
      RETURN
      END
