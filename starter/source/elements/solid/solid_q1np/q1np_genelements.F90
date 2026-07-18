!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!Chd|====================================================================
!Chd|  GENQ1NP                       source/elements/solid/solid_q1np/genq1np.F90
!Chd|====================================================================
!C=======================================================================
!C
!C   This routine generates Q1Np (enriched hexahedral) elements
!C   from a surface-based definition.
!C
!C   Algorithm:
!C   1. Validate the requested surface ID and count its segments.
!C   2. Build a structured surface grid (NX x NY) from segment connectivity.
!C   3. Map surface segments to underlying HEX8 elements.
!C   4. Extract bulk face nodes from the associated HEX8 elements.
!C   5. Set up NURBS knot vectors and weights and fit NURBS control points
!C      to the surface via least-squares / Tikhonov (depending on method).
!C   6. Generate Q1Np element connectivity (NURBS control points + bulk nodes).
!C
!C   Note: Currently uses a fixed surface ID (ISURF_ID=1) and NURBS degrees
!C         P=Q=2. These can be generalized in future extensions.
!C=======================================================================
!||====================================================================
!||    genq1np_mod               ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- uses       -----------------------------------------------------
!||    findhex8fromsurface_mod   ../starter/source/elements/solid/solid_q1np/q1np_findhex8fromsurface.F90
!||    message_mod               ../starter/share/message_module/message_mod.F
!||    q1np_cholesky_mod         ../starter/source/elements/solid/solid_q1np/q1np_cholesky.F90
!||    q1np_export_csv_mod       ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||    q1np_surf_grid_mod        ../starter/source/elements/solid/solid_q1np/q1np_surf_grid.F90
!||    q1np_volume_mod           ../starter/source/elements/solid/solid_q1np/q1np_volume.F90
!||    setupnurbsq1np_mod        ../starter/source/elements/solid/solid_q1np/q1np_setupnurbs.F90
!||====================================================================
      module genq1np_mod
        use message_mod
        use groupdef_mod
        use q1np_restart_mod
        use q1np_volume_mod
        use q1np_cholesky_mod, only : cholesky_solve_q1np
        use q1np_export_csv_mod
        use q1np_surf_grid_mod
        use setupnurbsq1np_mod
        use findhex8fromsurface_mod, only : findhex8fromsurf
        use precision_mod, only : WP
        use constant_mod, only : ZERO, ONE, HALF, TWO
        implicit none
  !C     Control of optional CSV debug export for visualization
        logical :: q1np_export_csv = .FALSE.
      contains
!C
!C=======================================================================
!C   genq1np: Generate Q1Np elements
!C=======================================================================
!||====================================================================
!||    genq1np                             ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                              ../starter/source/output/message/message.F
!||    findhex8fromsurf                    ../starter/source/elements/solid/solid_q1np/q1np_findhex8fromsurface.F90
!||    q1np_build_surf_grid                ../starter/source/elements/solid/solid_q1np/q1np_surf_grid.F90
!||    q1np_check_element_orientation      ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_compute_volume_element         ../starter/source/elements/solid/solid_q1np/q1np_volume.F90
!||    q1np_export_bulk_nodes_csv          ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||    q1np_export_nurbs_csv               ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||    q1np_fit_control_points             ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_rebuild_bulk_from_hex_geom     ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_report_fit_error               ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_select_global_cp_orientation   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    setupnurbsq1np                      ../starter/source/elements/solid/solid_q1np/q1np_setupnurbs.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine genq1np(igrsurf, ixs, x, iparts,&
     &      nsurf, nixs, numnod, numels, iout,&
     &      isurf_id_in, knot_set_id_in, &
     &      kq1np_tab, iq1np_tab, iq1np_bulk_tab,&
     &      q1np_wtab, q1np_ktab, q1np_cptab, nweight_max, csv_surf_id_in, &
     &      numelq1np_out, kq1np_ielem0)
        use q1np_geom_mod, only : q1np_get_knot_vectors
        use my_alloc_mod,   only : my_alloc
        use my_dealloc_mod, only : my_dealloc
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
  !C     IGRSURF    - Surface definitions array (input)
  !C     IXS        - HEX8 element connectivity array (input)
  !C     X          - Node coordinates array (input)
  !C     KQ1NP_TAB  - Q1Np element properties array (output)
  !C                  KQ1NP_TAB(1,*) = Material ID
  !C                  KQ1NP_TAB(2,*) = Property ID
  !C                  KQ1NP_TAB(3,*) = Number of control points
  !C                  KQ1NP_TAB(4,*) = Starting index in IQ1NP_TAB for element control-point connectivity
  !C                  KQ1NP_TAB(5,*) = Element ID
  !C                  KQ1NP_TAB(6,*) = Element index in u direction (0-based)
  !C                  KQ1NP_TAB(7,*) = Element index in v direction (0-based)
  !C                  KQ1NP_TAB(8,*) = NURBS degree p
  !C                  KQ1NP_TAB(9,*) = NURBS degree q
  !C                  KQ1NP_TAB(12,*)= Surface grid element count NX (u) for knot sizing
  !C                  KQ1NP_TAB(13,*)= Surface grid element count NY (v) for knot sizing
  !C                  KQ1NP_TAB(14,*)= Offset to bulk nodes in IQ1NP_BULK_TAB
  !C     IQ1NP_TAB  - Control point connectivity array (output)
  !C                  Stores (p+1)*(q+1) control-point IDs per element
  !C     IQ1NP_BULK_TAB - Bulk node connectivity array (output)
  !C                  Stores the 4 HEX8 bulk nodes per element
  !C     Q1NP_WTAB  - NURBS weights array (output, all 1.0 for non-rational)
  !C     Q1NP_KTAB  - NURBS knot vectors array (output)
  !C                  First NKNOT_U entries: U knot vector
  !C                  Next NKNOT_V entries: V knot vector
  !C     NUMELQ1NP_OUT  - Number of generated Q1Np elements (output)
  !C-----------------------------------------------
  !C     CP fitting method:
  !C       0 = 3D least-squares fitting with subdivision samples
  !C       1 = Tikhonov-regularized least-squares using grid nodes only
  !C-----------------------------------------------
  !C   P a r a m e t e r s
  !C-----------------------------------------------
      INTEGER, PARAMETER :: IQ1NP_CP_METHOD = 0
      REAL(KIND=WP) :: Q1NP_GRID_NODE_WEIGHT = 10.0_WP
      INTEGER, PARAMETER :: DIV = 2 ! subdivision per quad for method 0 (fitting samples)
      REAL(KIND=WP), PARAMETER :: Q1NP_DETJ_THRESHOLD = 1.0E-12_WP
      INTEGER, PARAMETER :: Q1NP_MAX_FIT_RETRIES = 3
      INTEGER, PARAMETER :: P = 2 ! NURBS degrees (quadratic in u and v)
      INTEGER, PARAMETER :: Q = 2 ! NURBS degrees (quadratic in u and v)
      INTEGER, PARAMETER :: NBULKQ1NP = 4 ! Number of bulk nodes per Q1Np element
      INTEGER, PARAMETER :: NKQ1NP = 15 ! Number of fields in KQ1NP_TAB per element
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
      type(surf_), dimension(nsurf), intent(in) :: igrsurf
      integer, intent(in) :: ixs(nixs, numels)
      integer, intent(in) :: iparts(:)
      real(kind=WP), intent(in) :: x(3, numnod)
      integer, intent(in) :: nsurf 
      integer, intent(in) :: nixs
      integer, intent(in) :: numnod
      integer, intent(in) :: numels
      integer, intent(in) :: iout
      integer, intent(in) :: isurf_id_in
      integer, intent(in) :: knot_set_id_in
      integer, intent(in) :: nweight_max
      integer, intent(in) :: csv_surf_id_in
      integer, intent(inout) :: numelq1np_out
      integer, intent(in), optional :: kq1np_ielem0
      integer, intent(inout) :: kq1np_tab(:,:)
      integer, intent(inout) :: iq1np_tab(:)
      integer, intent(inout) :: iq1np_bulk_tab(:)
      real(kind=WP), intent(inout) :: q1np_wtab(:)
      real(kind=WP), intent(inout) :: q1np_ktab(:)
      real(kind=WP), intent(inout) :: q1np_cptab(:,:)
  !C-----------------------------------------------
  !   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
      INTEGER ISEG,IEL,IEL_HEX8
      INTEGER KQ1NP_KCOL0
      INTEGER ISURF_ID            ! Selected internal surface index for generation
      INTEGER NODES_SURF(4)       ! Surface segment node IDs
      INTEGER NODES_BULK(4)       ! Bulk face node IDs from HEX8
      INTEGER NSEG                ! Number of surface segments
      INTEGER NX,NY               ! Grid dimensions (elements in u,v directions)
      INTEGER NCP_U,NCP_V         ! Number of control points in u,v directions
      INTEGER NCTRL               ! Control points per element: (p+1)*(q+1)
      INTEGER I,J,K,II,JJ         ! Loop indices
      INTEGER IEL_Q1NP            ! Q1Np element counter = 0
      INTEGER OFFSET_CTRL         ! Offset in IQ1NP_TAB for control points = 1
      INTEGER OFFSET_BULK         ! Offset in IQ1NP_BULK_TAB for bulk nodes = 1
      INTEGER MID,PID             ! Material and property IDs = passed from underlying HEX8
      INTEGER ELEM_ID             ! Re-used element ID from HEX8
      INTEGER IEL_ORIG            ! Original HEX8 element local index

  !C  Control point map: maps (i,j) control-point grid position to linear CP index
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: CP_MAP
      INTEGER MAX_CP_U,MAX_CP_V   ! Dimensions of control point map
      INTEGER CP_COUNTER

  !C  Method least-squares fit: surface grid, data points, normal equations
      INTEGER NDATA,NCP,NKNOT_U,NKNOT_V

  !C  Surface grid from connectivity (segment -> (I,J), grid corner -> node)
      INTEGER, ALLOCATABLE :: SEG_I(:), SEG_J(:)
      INTEGER, ALLOCATABLE :: GRID_NODE(:,:), GRID_TO_SEG(:,:)
      INTEGER IERR_GRID

  !C  Post-fit Jacobian quality check and retry loop
      REAL(KIND=WP) :: VOL_CHECK, DETJ_MIN_EL, WEIGHT_CURRENT
      INTEGER :: NJAC_FAIL, IRETRY
      REAL(KIND=WP) :: ORIENT_TOP_MAG, ORIENT_BOT_MAG
      REAL(KIND=WP) :: ORIENT_TOP_BOT_DOT, ORIENT_TOP_BOT_COS
      REAL(KIND=WP) :: BULK_MATCH_SCORE
      INTEGER :: IERR_ORIENT
      INTEGER :: IERR_BULK
      REAL(KIND=WP), ALLOCATABLE :: U_KNOT_GEN(:), V_KNOT_GEN(:)

  !C=======================================================================
  !C   Step 1: Validate surface ID and get number of segments
  !C=======================================================================
      ISURF_ID = isurf_id_in

      IF (ISURF_ID < 1 .OR. ISURF_ID > NSURF) THEN
        WRITE(IOUT,'(A,I8,A,I8)') &
     &      ' Q1NP ERROR: surface id out of range: surface=', &
     &      ISURF_ID, ' nsurf=', NSURF
        WRITE(*,'(A,I8,A,I8)') &
     &      ' Q1NP ERROR: surface id out of range: surface=', &
     &      ISURF_ID, ' nsurf=', NSURF
        CALL ANCMSG(MSGID=364, &
     &      MSGTYPE=MSGERROR, &
     &      ANMODE=ANINFO, &
     &      C1='Q1NP surface id out of range')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF

      NSEG = IGRSURF(ISURF_ID)%NSEG

  !C     For now, check both NSEG and NSEG_IGE fields
      IF (NSEG <= 0 .AND. IGRSURF(ISURF_ID)%NSEG_IGE <= 0) THEN
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF

  !C=======================================================================
  !C   Step 2: Determine grid dimensions and segment/grid mapping from connectivity
  !C=======================================================================
  !C     Allocate work arrays for surface grid: segment indices (SEG_I, SEG_J),
  !C     grid-to-segment map (GRID_TO_SEG), and grid node connectivity (GRID_NODE).
      CALL MY_ALLOC(SEG_I, NSEG, "SEG_I", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='SEG_I')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      CALL MY_ALLOC(SEG_J, NSEG, "SEG_J", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='SEG_J')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      CALL MY_ALLOC(GRID_TO_SEG, NSEG, NSEG, "GRID_TO_SEG", stat=IEL)
      IF (IEL .NE. 0) THEN
        IF (ALLOCATED(SEG_I)) CALL MY_DEALLOC(SEG_I)
        IF (ALLOCATED(SEG_J)) CALL MY_DEALLOC(SEG_J)
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='GRID_TO_SEG')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
  !C     GRID_NODE: leading dimension NSEG*4 so (NX+1,NY+1) always fits (NX*NY=NSEG).
      CALL MY_ALLOC(GRID_NODE, NSEG*4, NSEG*4, "GRID_NODE", stat=IEL)
      GRID_NODE(1:NSEG*4,1:NSEG*4) = 0
      IF (IEL .NE. 0) THEN
        IF (ALLOCATED(SEG_I)) CALL MY_DEALLOC(SEG_I)
        IF (ALLOCATED(SEG_J)) CALL MY_DEALLOC(SEG_J)
        IF (ALLOCATED(GRID_TO_SEG)) CALL MY_DEALLOC(GRID_TO_SEG)
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='GRID_NODE')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF

  !C     Build surface grid from segment connectivity; fills GRID_NODE(1:NX+1,1:NY+1)
  !C     and GRID_TO_SEG(1:NX,1:NY). NX, NY are the element counts in u and v.
      CALL Q1NP_BUILD_SURF_GRID(IGRSURF(ISURF_ID), NSEG, &
     &      NX, NY, SEG_I, SEG_J, &
     &      GRID_NODE, GRID_TO_SEG, IERR_GRID)

      IF (IERR_GRID .NE. 0) THEN
        IF (ALLOCATED(SEG_I)) CALL MY_DEALLOC(SEG_I)
        IF (ALLOCATED(SEG_J)) CALL MY_DEALLOC(SEG_J)
        IF (ALLOCATED(GRID_NODE)) CALL MY_DEALLOC(GRID_NODE)
        IF (ALLOCATED(GRID_TO_SEG)) CALL MY_DEALLOC(GRID_TO_SEG)
        WRITE(IOUT,'(A,I8,A,I8)') &
     &      ' Q1NP ERROR: surface grid build failed: surface=', &
     &      ISURF_ID, ' ierr=', IERR_GRID
        WRITE(IOUT,'(A)') &
     &      '  IERR meanings: 3=corner ordering mismatch, 4=disconnected, 5=non-rectangular/inconsistent, 7=bounds'
        WRITE(*,'(A,I8,A,I8)') &
     &      ' Q1NP ERROR: surface grid build failed: surface=', &
     &      ISURF_ID, ' ierr=', IERR_GRID
        WRITE(*,'(A)') &
     &      '  IERR meanings: 3=corner ordering mismatch, 4=disconnected, 5=non-rectangular/inconsistent, 7=bounds'
        CALL ANCMSG(MSGID=364, &
     &      MSGTYPE=MSGERROR, &
     &      ANMODE=ANINFO, &
     &      C1='Q1NP surface grid build failed')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF

  !C=======================================================================
  !C   Step 3: Calculate NURBS parameters
  !C=======================================================================
  !C     Number of control points: ncp = ne + p
  !C     For ne elements and degree p, we need ne+p control points
      NCP_U = NX + P
      NCP_V = NY + Q
  !C     Number of control points per element (tensor product)
      NCTRL = (P+1)*(Q+1)

      NKNOT_U = NX + 2*P + 1
      NKNOT_V = NY + 2*Q + 1

  !C=======================================================================
  !C   Set up NURBS knot vectors and weights (baseline for fitting)
  !C=======================================================================
      CALL SETUPNURBSQ1NP(NX,NY,P,Q, &
     &      Q1NP_KTAB,NCP_U,NCP_V, &
     &      Q1NP_WTAB,NWEIGHT_MAX)
  !C     Q1NP_KTAB: U then V knot vectors; Q1NP_WTAB: weights per global CP

      ! Both U and V knot vectors are stored in Q1NP_KTAB
      ! NCP_U and NCP_V are the number of control points in u and v directions
      ! NCP is the total number of control points
      NCP = NCP_U * NCP_V

      KQ1NP_KCOL0 = 0
      IF (PRESENT(kq1np_ielem0)) KQ1NP_KCOL0 = kq1np_ielem0

  !C=======================================================================
  !C   Step 3b: Validate passed storage against actual surface topology
  !C=======================================================================
      IF (SIZE(KQ1NP_TAB,1) < NKQ1NP .OR. &
     &    SIZE(KQ1NP_TAB,2) < KQ1NP_KCOL0 + NSEG) THEN
        WRITE(IOUT,'(A,4I10)') &
     &    ' Q1NP ERROR: KQ1NP_TAB too small, need/have = ', &
     &    NKQ1NP, KQ1NP_KCOL0 + NSEG, SIZE(KQ1NP_TAB,1), SIZE(KQ1NP_TAB,2)
        WRITE(*,'(A,4I10)') &
     &    ' Q1NP ERROR: KQ1NP_TAB too small, need/have = ', &
     &    NKQ1NP, KQ1NP_KCOL0 + NSEG, SIZE(KQ1NP_TAB,1), SIZE(KQ1NP_TAB,2)
        CALL ANCMSG(MSGID=364, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
     &      C1='Q1NP KQ1NP_TAB too small')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      IF (SIZE(IQ1NP_TAB) < NSEG * NCTRL) THEN
        WRITE(IOUT,'(A,2I10)') &
     &    ' Q1NP ERROR: IQ1NP_TAB too small, need/have = ', &
     &    NSEG * NCTRL, SIZE(IQ1NP_TAB)
        WRITE(*,'(A,2I10)') &
     &    ' Q1NP ERROR: IQ1NP_TAB too small, need/have = ', &
     &    NSEG * NCTRL, SIZE(IQ1NP_TAB)
        CALL ANCMSG(MSGID=364, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
     &      C1='Q1NP IQ1NP_TAB too small')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      IF (SIZE(IQ1NP_BULK_TAB) < NSEG * NBULKQ1NP) THEN
        WRITE(IOUT,'(A,2I10)') &
     &    ' Q1NP ERROR: IQ1NP_BULK_TAB too small, need/have = ', &
     &    NSEG * NBULKQ1NP, SIZE(IQ1NP_BULK_TAB)
        WRITE(*,'(A,2I10)') &
     &    ' Q1NP ERROR: IQ1NP_BULK_TAB too small, need/have = ', &
     &    NSEG * NBULKQ1NP, SIZE(IQ1NP_BULK_TAB)
        CALL ANCMSG(MSGID=364, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
     &      C1='Q1NP IQ1NP_BULK_TAB too small')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      IF (SIZE(Q1NP_CPTAB,2) < NCP) THEN
        WRITE(IOUT,'(A,2I10)') &
     &    ' Q1NP ERROR: Q1NP_CPTAB too small, need/have = ', &
     &    NCP, SIZE(Q1NP_CPTAB,2)
        WRITE(*,'(A,2I10)') &
     &    ' Q1NP ERROR: Q1NP_CPTAB too small, need/have = ', &
     &    NCP, SIZE(Q1NP_CPTAB,2)
        CALL ANCMSG(MSGID=364, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
     &      C1='Q1NP Q1NP_CPTAB too small')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      IF (SIZE(Q1NP_WTAB) < NCP) THEN
        WRITE(IOUT,'(A,2I10)') &
     &    ' Q1NP ERROR: Q1NP_WTAB too small, need/have = ', &
     &    NCP, SIZE(Q1NP_WTAB)
        WRITE(*,'(A,2I10)') &
     &    ' Q1NP ERROR: Q1NP_WTAB too small, need/have = ', &
     &    NCP, SIZE(Q1NP_WTAB)
        CALL ANCMSG(MSGID=364, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
     &      C1='Q1NP Q1NP_WTAB too small')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF

  !C     Subdivision for sampling (method 0 only): DIV*DIV interior sample points
  !C     per quad (ensures NDATA >= NCP for fitting). NDATA is the number of data
  !C     points for the least-squares fitting. Note: DIV=1 already yields one
  !C     interior sample at element center (no duplicates with grid nodes)

      IF (IQ1NP_CP_METHOD .EQ. 0) THEN
        NDATA = (NX+1)*(NY+1) + NX*NY*DIV*DIV
      ELSE
  !C       Method 1 (Tikhonov) uses only surface grid nodes as data points
        NDATA = (NX+1)*(NY+1)
      ENDIF

  !C=======================================================================
  !C   Step 4: Allocate and initialize control point map
  !C=======================================================================
  !C     CP_MAP(i,j) = linear control-point index 1..NCP for grid (i,j).
  !C     Used for element connectivity (which CPs belong to each Q1Np element).
      MAX_CP_U = NCP_U
      MAX_CP_V = NCP_V
      CALL MY_ALLOC(CP_MAP, MAX_CP_U, MAX_CP_V, "CP_MAP", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='CP_MAP')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      CP_COUNTER = 0
      DO J=1,NCP_V
        DO I=1,NCP_U
          CP_COUNTER = CP_COUNTER + 1
          CP_MAP(I,J) = CP_COUNTER
        ENDDO
      ENDDO

  !C=======================================================================
  !C   Step 5: Least-squares fitting to derive the position of the NURBS control points
  !C=======================================================================
      WEIGHT_CURRENT = Q1NP_GRID_NODE_WEIGHT
      CALL Q1NP_FIT_CONTROL_POINTS( NX, NY, P, Q,           &
     &     NCP_U, NCP_V, NCP, NDATA, NKNOT_U, NKNOT_V, NUMNOD, &
     &     GRID_NODE, X, Q1NP_KTAB, Q1NP_CPTAB,     &
     &     IQ1NP_CP_METHOD, DIV, WEIGHT_CURRENT, &
     &     CSV_SURF_ID_IN )

  !C=======================================================================
  !C   Step 5b: Select a global NURBS parameter orientation.
  !C   Only the top NURBS indexing is flipped; bulk-node ordering stays fixed.
  !C=======================================================================
      CALL Q1NP_SELECT_GLOBAL_CP_ORIENTATION(NX, NY, P, Q, NCP_U, NCP_V, NCP, NUMNOD, &
     &     CP_MAP, Q1NP_CPTAB, Q1NP_KTAB, GRID_NODE, GRID_TO_SEG, IXS, &
     &     NIXS, NUMELS, X, IOUT, MAX_CP_U,MAX_CP_V)

  !C=======================================================================
  !C   Step 5c: Report top-surface fit quality against original surface grid
  !C=======================================================================
      CALL Q1NP_REPORT_FIT_ERROR(NX, NY, P, Q, NCP_U, NCP_V, NCP, NUMNOD, &
     &     GRID_NODE, X, Q1NP_KTAB, Q1NP_CPTAB, IOUT)
  !C=======================================================================
  !C   Step 6: Generate Q1Np elements from fitted NURBS surface and HEX8 bulk mesh
  !C=======================================================================
      CALL MY_ALLOC(U_KNOT_GEN, NX + 2*P + 1, "U_KNOT_GEN", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='Q1NP gen knot vectors')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      CALL MY_ALLOC(V_KNOT_GEN, NY + 2*Q + 1, "V_KNOT_GEN", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='Q1NP gen knot vectors')
        NUMELQ1NP_OUT = 0
        RETURN
      ENDIF
      CALL Q1NP_GET_KNOT_VECTORS(NX, NY, P, Q, Q1NP_KTAB, U_KNOT_GEN, V_KNOT_GEN)

  !C     Offsets into IQ1NP_TAB (control points) and IQ1NP_BULK_TAB (bulk nodes)
      OFFSET_CTRL = 1
      OFFSET_BULK = 1
      IEL_Q1NP = 0

  !C     Loop over grid positions to create Q1Np elements
      elem_loop: do j = 1, ny
        do i = 1, nx
          ISEG = GRID_TO_SEG(I,J)
          IF (ISEG .LE. 0) exit elem_loop

  !C         Use structured surface-grid corner order so the HEX face mapping
  !C         follows Q1NP parametric corners: (-1,-1),(+1,-1),(+1,+1),(-1,+1)
          NODES_SURF(1) = GRID_NODE(I  ,J  )
          NODES_SURF(2) = GRID_NODE(I+1,J  )
          NODES_SURF(3) = GRID_NODE(I+1,J+1)
          NODES_SURF(4) = GRID_NODE(I  ,J+1)

  !C         Find corresponding HEX8 element; returned bulk nodes follow the
  !C         same ordered corner sequence as NODES_SURF above.
          CALL findhex8fromsurf(NODES_SURF,IXS,IEL_HEX8,NODES_BULK,NIXS,NUMELS)

          IF (IEL_HEX8 <= 0) THEN
  !C           Error: no matching HEX8 element found
            WRITE(IOUT,'(A,I8)') &
     &        ' Q1NP ERROR: no matching HEX8 element found for surface segment ', ISEG
            WRITE(*,'(A,I8)') &
     &        ' Q1NP ERROR: no matching HEX8 element found for surface segment ', ISEG
            CALL ANCMSG(MSGID=364, &
     &      MSGTYPE=MSGERROR, &
     &      ANMODE=ANINFO, &
     &      C1='Q1NP surface segment has no matching HEX8')
            exit elem_loop
          ENDIF

          CALL Q1NP_REBUILD_BULK_FROM_HEX_GEOM(I, J, P, Q, NCP_U, NCP_V, NCP, NUMNOD, &
     &        NIXS, NUMELS, CP_MAP, 0, U_KNOT_GEN, V_KNOT_GEN, Q1NP_CPTAB, X, IXS, IEL_HEX8, &
     &        NODES_BULK, BULK_MATCH_SCORE, IERR_BULK)
          IF (IERR_BULK .NE. 0) THEN
            WRITE(IOUT,'(A,I8,A,2I6,A,I8,A,I4)') &
     &        ' Q1NP ERROR: failed to rebuild bulk face for element ', &
     &        IEL_Q1NP + 1, ' grid=', I-1, J-1, ' hex_gid=', IXS(NIXS,IEL_HEX8), &
     &        ' ierr=', IERR_BULK
            WRITE(*,'(A,I8,A,2I6,A,I8,A,I4)') &
     &        ' Q1NP ERROR: failed to rebuild bulk face for element ', &
     &        IEL_Q1NP + 1, ' grid=', I-1, J-1, ' hex_gid=', IXS(NIXS,IEL_HEX8), &
     &        ' ierr=', IERR_BULK
            CALL ANCMSG(MSGID=364, &
     &        MSGTYPE=MSGERROR, &
     &        ANMODE=ANINFO, &
     &        C1='Q1NP bulk-face reconstruction failed')
            NUMELQ1NP_OUT = 0
            RETURN
          ENDIF

  !C         Get material and property IDs from HEX8 element
  !C         Q1Np elements inherit these from the underlying HEX8
          MID = IXS(1,IEL_HEX8)
          PID = IXS(10,IEL_HEX8)

  !C         Increment element counter and assign element ID
          IEL_Q1NP = IEL_Q1NP + 1
          IEL_ORIG = IEL_HEX8
          ELEM_ID = IXS(NIXS,IEL_HEX8)

  !C=======================================================================
  !C         Step 6a: Signed orientation check between fitted top patch and
  !C         matched bulk face. A non-positive sign means the local (u,v)
  !C         ordering is inconsistent through thickness and will lead to a
  !C         Jacobian sign change in the engine.
  !C=======================================================================
          CALL Q1NP_CHECK_ELEMENT_ORIENTATION(I, J, P, Q, MAX_CP_U, MAX_CP_V, NCP, NUMNOD, &
     &      CP_MAP, Q1NP_CPTAB(1:3,1:NCP), NODES_BULK, X(1:3,1:NUMNOD), ORIENT_TOP_MAG, ORIENT_BOT_MAG, &
     &      ORIENT_TOP_BOT_DOT, ORIENT_TOP_BOT_COS, IERR_ORIENT)

          IF (IERR_ORIENT .NE. 0) THEN
            WRITE(IOUT,'(A,I8,A,2I6,A,I8,A,I4,A,1P,4E14.6)') &
     &        ' Q1NP ERROR: signed top/bottom orientation check failed for element ', &
     &        IEL_Q1NP, ' grid=', I-1, J-1, ' hex_gid=', ELEM_ID, ' ierr=', IERR_ORIENT, &
     &        ' top_mag/bot_mag/dot/cos=', ORIENT_TOP_MAG, ORIENT_BOT_MAG, &
     &        ORIENT_TOP_BOT_DOT, ORIENT_TOP_BOT_COS
            WRITE(*,'(A,I8,A,2I6,A,I8,A,I4,A,1P,4E14.6)') &
     &        ' Q1NP ERROR: signed top/bottom orientation check failed for element ', &
     &        IEL_Q1NP, ' grid=', I-1, J-1, ' hex_gid=', ELEM_ID, ' ierr=', IERR_ORIENT, &
     &        ' top_mag/bot_mag/dot/cos=', ORIENT_TOP_MAG, ORIENT_BOT_MAG, &
     &        ORIENT_TOP_BOT_DOT, ORIENT_TOP_BOT_COS
            CALL ANCMSG(MSGID=364, &
     &        MSGTYPE=MSGERROR, &
     &        ANMODE=ANINFO, &
     &        C1='Q1NP top/bottom orientation check failed')
            NUMELQ1NP_OUT = 0
            RETURN
          END IF

  !C=======================================================================
  !C         Step 6b:   Store element properties in KQ1NP_TAB (main INT table)
  !C=======================================================================
          KQ1NP_TAB(1,IEL_Q1NP + KQ1NP_KCOL0) = MID              ! Material ID
          KQ1NP_TAB(2,IEL_Q1NP + KQ1NP_KCOL0) = PID              ! Property ID
          KQ1NP_TAB(3,IEL_Q1NP + KQ1NP_KCOL0) = NCTRL            ! Number of control points
          KQ1NP_TAB(4,IEL_Q1NP + KQ1NP_KCOL0) = OFFSET_CTRL      ! Offset to control points
          KQ1NP_TAB(5,IEL_Q1NP + KQ1NP_KCOL0) = ELEM_ID          ! Element ID (original HEX)
          KQ1NP_TAB(6,IEL_Q1NP + KQ1NP_KCOL0) = I-1              ! Element index u (0-based)
          KQ1NP_TAB(7,IEL_Q1NP + KQ1NP_KCOL0) = J-1              ! Element index v (0-based)
          KQ1NP_TAB(8,IEL_Q1NP + KQ1NP_KCOL0) = P                ! NURBS degree p
          KQ1NP_TAB(9,IEL_Q1NP + KQ1NP_KCOL0) = Q                ! NURBS degree q
          KQ1NP_TAB(14,IEL_Q1NP + KQ1NP_KCOL0) = OFFSET_BULK     ! Offset into IQ1NP_BULK_TAB
          KQ1NP_TAB(10,IEL_Q1NP + KQ1NP_KCOL0) = IEL_ORIG        ! Local HEX8 index
          KQ1NP_TAB(11,IEL_Q1NP + KQ1NP_KCOL0) = IPARTS(IEL_ORIG)! Owning part ID
          KQ1NP_TAB(12,IEL_Q1NP + KQ1NP_KCOL0) = NX              ! Surface grid element count in u (engine knot sizing)
          KQ1NP_TAB(13,IEL_Q1NP + KQ1NP_KCOL0) = NY              ! Surface grid element count in v (engine knot sizing)
          KQ1NP_TAB(15,IEL_Q1NP + KQ1NP_KCOL0) = knot_set_id_in  ! Knot-set selection for per-element knot vectors

  !C=======================================================================
  !C         Step 6c: Store control point connectivity in IQ1NP_TAB (CP node IDs)
  !C=======================================================================
  !C         Element at (i,j) uses control points: cp_map(i+ii, j+jj)
  !C         for ii in [0,p], jj in [0,q] (Fortran: ii=0..p, jj=0..q)
          DO JJ=0,Q
            DO II=0,P
              IF (I+II <= MAX_CP_U .AND. J+JJ <= MAX_CP_V) THEN
                IQ1NP_TAB(OFFSET_CTRL) = CP_MAP(I+II,J+JJ)
              ELSE
                IQ1NP_TAB(OFFSET_CTRL) = 0
              ENDIF
              OFFSET_CTRL = OFFSET_CTRL + 1
            ENDDO
          ENDDO

  !C=======================================================================
  !C         Step 6d: Store HEX8 bulk face nodes in IQ1NP_BULK_TAB (4 nodes / element)
  !C         as the bottom bilinear shape functions.
  !C=======================================================================
          IQ1NP_BULK_TAB(OFFSET_BULK)   = NODES_BULK(1)
          IQ1NP_BULK_TAB(OFFSET_BULK+1) = NODES_BULK(2)
          IQ1NP_BULK_TAB(OFFSET_BULK+2) = NODES_BULK(3)
          IQ1NP_BULK_TAB(OFFSET_BULK+3) = NODES_BULK(4)
          OFFSET_BULK = OFFSET_BULK + 4

        end do   ! i
      end do elem_loop   ! j

      NUMELQ1NP_OUT = IEL_Q1NP
  !C=======================================================================
  !C   Step 7: Post-fit Jacobian quality check with auto-retry
  !C   IRETRY=0: check only (fit from Step 5). IRETRY>=1: halve weight, refit, check.
  !C   One element loop per attempt (no duplicate DO I blocks).
  !C=======================================================================
      step7_retry: DO IRETRY = 0, Q1NP_MAX_FIT_RETRIES
        IF (IRETRY > 0) THEN
          WEIGHT_CURRENT = WEIGHT_CURRENT * HALF
          WRITE(IOUT,'(A,I2,A,1PE10.3)') &
     &     '  ** Q1NP: retry ', IRETRY, &
     &     ' with reduced grid weight = ', WEIGHT_CURRENT
          CALL Q1NP_FIT_CONTROL_POINTS( NX, NY, P, Q,           &
     &       NCP_U, NCP_V, NCP, NDATA, NKNOT_U, NKNOT_V, NUMNOD, &
     &       GRID_NODE, X, Q1NP_KTAB, Q1NP_CPTAB,       &
     &       IQ1NP_CP_METHOD, DIV, WEIGHT_CURRENT, &
     &       CSV_SURF_ID_IN )
          CALL Q1NP_REPORT_FIT_ERROR(NX, NY, P, Q, NCP_U, NCP_V, &
     &       NCP, NUMNOD, GRID_NODE, X, Q1NP_KTAB, Q1NP_CPTAB, IOUT)
        ENDIF
        NJAC_FAIL = 0
        DO I = 1, NUMELQ1NP_OUT
          CALL Q1NP_COMPUTE_VOLUME_ELEMENT(I + KQ1NP_KCOL0, KQ1NP_TAB, IQ1NP_TAB, &
     &       IQ1NP_BULK_TAB, Q1NP_KTAB, X, NX, NY, VOL_CHECK, &
     &       DETJ_MIN_EL, Q1NP_CPTAB)
          IF (VOL_CHECK <= ZERO .OR. DETJ_MIN_EL <= Q1NP_DETJ_THRESHOLD) THEN
            NJAC_FAIL = NJAC_FAIL + 1
            IF (IRETRY == 0) THEN
              WRITE(IOUT,'(A,I8,A,1PE12.5,A,1PE12.5)') &
     &         '  ** Q1NP WARNING: element ', I, &
     &         ' detj_min=', DETJ_MIN_EL, ' vol=', VOL_CHECK
            ENDIF
          ENDIF
        ENDDO
        IF (NJAC_FAIL == 0) THEN
          IF (IRETRY > 0) THEN
            WRITE(IOUT,'(A,I2,A)') &
     &       '  ** Q1NP: retry ', IRETRY, &
     &       ' succeeded - all Jacobians positive.'
          ENDIF
          EXIT step7_retry
        ENDIF
        IF (IRETRY == 0) THEN
          WRITE(IOUT,'(A,I6,A,I6,A)') &
     &     '  ** Q1NP WARNING: ', NJAC_FAIL, ' of ', NUMELQ1NP_OUT, &
     &     ' elements have non-positive Jacobian after CP fitting.'
        ELSE
          WRITE(IOUT,'(A,I6,A)') &
     &     '  ** Q1NP: still ', NJAC_FAIL, &
     &     ' elements with non-positive Jacobian.'
        ENDIF
      ENDDO step7_retry
  !C=======================================================================
  !C   Optional: export NURBS control points and bulk nodes to CSV (visualization)
  !C=======================================================================
      IF (q1np_export_csv) THEN
        CALL Q1NP_EXPORT_NURBS_CSV(NCP_U,NCP_V,MAX_CP_U,MAX_CP_V, &
     &      Q1NP_CPTAB,CP_MAP, &
     &      NUMELQ1NP_OUT,&
     &      KQ1NP_TAB(:, 1 + KQ1NP_KCOL0 : NUMELQ1NP_OUT + KQ1NP_KCOL0), &
     &      IQ1NP_TAB,IQ1NP_BULK_TAB,NUMNOD,CSV_SURF_ID_IN)
        CALL Q1NP_EXPORT_BULK_NODES_CSV(NUMELQ1NP_OUT, NUMNOD, &
     &      KQ1NP_TAB(:, 1 + KQ1NP_KCOL0 : NUMELQ1NP_OUT + KQ1NP_KCOL0), &
     &      IQ1NP_BULK_TAB,X,CSV_SURF_ID_IN)
      ENDIF

  !C     Inform Q1NP_RESTART_MOD of total control-point buffer length (for restart I/O).
  !C     In multi-surface prototype mode, GENQ1NP may be called multiple times with
  !C     sliced IQ1NP_TAB arrays; therefore only ever increase TABVINT_LEN_G.
      IF (TABVINT_LEN_G < OFFSET_CTRL-1) THEN
        CALL SET_Q1NP_TABVINT_LEN(OFFSET_CTRL-1)
      END IF

  !C=======================================================================
  !C   Print Q1Np element information
  !C=======================================================================
  !C     Always print Q1Np section header for visibility
      WRITE(IOUT,300)

      IF (NUMELQ1NP_OUT > 0) THEN
        WRITE(IOUT,301) NUMELQ1NP_OUT, NX, NY, NCP_U, NCP_V
        WRITE(IOUT,302)
        DO I=1,MIN(50,NUMELQ1NP_OUT)
          WRITE(IOUT,303) I, KQ1NP_TAB(5,I + KQ1NP_KCOL0), KQ1NP_TAB(1,I + KQ1NP_KCOL0), &
     &      KQ1NP_TAB(2,I + KQ1NP_KCOL0), KQ1NP_TAB(6,I + KQ1NP_KCOL0), &
     &      KQ1NP_TAB(7,I + KQ1NP_KCOL0)
        ENDDO
        IF (NUMELQ1NP_OUT > 50) THEN
          WRITE(IOUT,304) NUMELQ1NP_OUT - 50
        ENDIF
      ELSE
        WRITE(IOUT,305)
      ENDIF

 300  FORMAT(/' Q1NP ENRICHED ELEMENTS'/ &
     &        ' ----------------------'/ &
     &        ' Generated from surface definition')
 301  FORMAT(' Number of Q1Np elements: ',I10/ &
     &       ' Grid dimensions: ',I3,' x ',I3/ &
     &       ' Control points: ',I3,' x ',I3)
 302  FORMAT('    LOC-EL     GLO-EL      MATER       GEOM    ELEM-U    ELEM-V'/ &
     &       '    NODES LIST')
 303  FORMAT(6(I10,1X))
 304  FORMAT(' ... and ',I10,' more elements')
 305  FORMAT(/' Q1NP ENRICHED ELEMENTS'/ &
     &        ' ----------------------'/ &
     &        ' No Q1Np elements generated')

  !C=======================================================================
  !C   Cleanup: deallocate grid and CP map work arrays
  !C=======================================================================
      IF (ALLOCATED(CP_MAP)) CALL MY_DEALLOC(CP_MAP)
      IF (ALLOCATED(SEG_I)) CALL MY_DEALLOC(SEG_I)
      IF (ALLOCATED(SEG_J)) CALL MY_DEALLOC(SEG_J)
      IF (ALLOCATED(GRID_NODE)) CALL MY_DEALLOC(GRID_NODE)
      IF (ALLOCATED(GRID_TO_SEG)) CALL MY_DEALLOC(GRID_TO_SEG)
      IF (ALLOCATED(U_KNOT_GEN)) CALL MY_DEALLOC(U_KNOT_GEN)
      IF (ALLOCATED(V_KNOT_GEN)) CALL MY_DEALLOC(V_KNOT_GEN)
      RETURN
        end subroutine genq1np
!C
!C=======================================================================
!C   Q1NP_FIT_CONTROL_POINTS: Fit NURBS control points to the surface (least-squares / Tikhonov)
!C=======================================================================
!||====================================================================
!||    q1np_fit_control_points         ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    genq1np                         ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                          ../starter/source/output/message/message.F
!||    cholesky_solve_q1np             ../starter/source/elements/solid/solid_q1np/q1np_cholesky.F90
!||    q1np_export_surface_nodes_csv   ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||--- uses       -----------------------------------------------------
!||    q1np_cholesky_mod               ../starter/source/elements/solid/solid_q1np/q1np_cholesky.F90
!||    q1np_export_csv_mod             ../starter/source/elements/solid/solid_q1np/q1np_export_csv.F90
!||====================================================================
           SUBROUTINE Q1NP_FIT_CONTROL_POINTS( NX, NY, P, Q,           &
     &     NCP_U, NCP_V, NCP, NDATA, NKNOT_U, NKNOT_V, NUMNOD,    &
     &     GRID_NODE, X, Q1NP_KTAB, Q1NP_CPTAB,           &
     &     ICP_METHOD_IN, DIV_IN, WEIGHT_IN,           &
     &     CSV_SURF_ID_IN )

        USE precision_mod,   ONLY : WP
        USE constant_mod,    ONLY : ZERO, ONE, HALF, TWO
        USE q1np_cholesky_mod, ONLY : cholesky_solve_q1np
        USE q1np_export_csv_mod
        USE my_alloc_mod,   ONLY : my_alloc
        USE my_dealloc_mod, ONLY : my_dealloc
        IMPLICIT NONE

        INTEGER, INTENT(IN) :: NX, NY, P, Q
        INTEGER, INTENT(IN) :: NCP_U, NCP_V, NCP, NDATA
        INTEGER, INTENT(IN) :: NKNOT_U, NKNOT_V, NUMNOD
        INTEGER, INTENT(IN) :: GRID_NODE(:,:)
        REAL(WP), INTENT(IN) :: X(3,NUMNOD)
        REAL(WP), INTENT(IN) :: Q1NP_KTAB(:)   
        INTEGER, INTENT(IN) :: ICP_METHOD_IN, DIV_IN
        REAL(WP), INTENT(IN) :: WEIGHT_IN
        INTEGER, INTENT(IN) :: CSV_SURF_ID_IN
        REAL(WP), INTENT(INOUT) :: Q1NP_CPTAB(3,NCP)

        ! Local arrays
        REAL(WP), DIMENSION(:,:,:),  ALLOCATABLE :: X_GRID
        INTEGER,  DIMENSION(:),      ALLOCATABLE :: GRID_NODE_RES
        REAL(WP), DIMENSION(:,:),    ALLOCATABLE :: DATA_PT
        REAL(WP), DIMENSION(:),      ALLOCATABLE :: U_PARAM, V_PARAM
        REAL(WP), DIMENSION(:,:),    ALLOCATABLE :: ATA, ATD, C_CP
        REAL(WP), DIMENSION(:),      ALLOCATABLE :: A_ROW
        REAL(WP), DIMENSION(:),      ALLOCATABLE :: U_KNOT, V_KNOT
        REAL(WP), DIMENSION(:,:),    ALLOCATABLE :: L_REG, LTL, ATA_REG
        REAL(WP), DIMENSION(:,:),    ALLOCATABLE :: C_CP_BEST
        REAL(WP), DIMENSION(:),      ALLOCATABLE :: TMPVEC

        ! Local scalars
        INTEGER :: IEL
        INTEGER :: I, J, K
        INTEGER :: IDATA, COL, COL1, COL2
        INTEGER :: II_SUB, JJ_SUB
        INTEGER :: NLAM, ILAM
        INTEGER :: NODE_ID, CP_COUNTER
        REAL(WP) :: UU, VV, S, T, WGT
        REAL(WP) :: DTD, RES2, RMS, BEST_RMS, LAMBDA_CUR, LAMBDA_BEST


  !C  Allocate arrays for least-squares fit
      ! X_GRID: surface grid
      ! DATA_PT: data points
      ! ATA: left-hand side of normal equations
      ! ATD: right-hand side of normal equations
      ! A_ROW: basis functions at a given data point
      ! C_CP: control points
      ! U_KNOT: knot vector for u
      ! V_KNOT: knot vector for v

      CALL MY_ALLOC(X_GRID, 3, NX+1, NY+1, "X_GRID", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='X_GRID')
        RETURN
      ENDIF

      CALL MY_ALLOC(GRID_NODE_RES, (NX+1)*(NY+1), "GRID_NODE_RES", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='GRID_NODE_RES')
        CALL MY_DEALLOC(X_GRID)
        RETURN
      ENDIF

      CALL MY_ALLOC(DATA_PT, 3, NDATA, "DATA_PT", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='DATA_PT/U_PARAM/V_PARAM')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        RETURN
      ENDIF
      CALL MY_ALLOC(U_PARAM, NDATA, "U_PARAM", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='DATA_PT/U_PARAM/V_PARAM')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        RETURN
      ENDIF
      CALL MY_ALLOC(V_PARAM, NDATA, "V_PARAM", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='DATA_PT/U_PARAM/V_PARAM')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        RETURN
      ENDIF

      CALL MY_ALLOC(ATA, NCP, NCP, "ATA", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ATA/ATD/A_ROW/C_CP')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        CALL MY_DEALLOC(V_PARAM)
        RETURN
      ENDIF
      CALL MY_ALLOC(ATD, NCP, 3, "ATD", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ATA/ATD/A_ROW/C_CP')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        CALL MY_DEALLOC(V_PARAM)
        CALL MY_DEALLOC(ATA)
        RETURN
      ENDIF
      CALL MY_ALLOC(A_ROW, NCP, "A_ROW", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ATA/ATD/A_ROW/C_CP')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        CALL MY_DEALLOC(V_PARAM)
        CALL MY_DEALLOC(ATA)
        CALL MY_DEALLOC(ATD)
        RETURN
      ENDIF
      CALL MY_ALLOC(C_CP, NCP, 3, "C_CP", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ATA/ATD/A_ROW/C_CP')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        CALL MY_DEALLOC(V_PARAM)
        CALL MY_DEALLOC(ATA)
        CALL MY_DEALLOC(ATD)
        CALL MY_DEALLOC(A_ROW)
        RETURN
      ENDIF

      CALL MY_ALLOC(U_KNOT, NKNOT_U, "U_KNOT", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='U_KNOT/V_KNOT')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        CALL MY_DEALLOC(V_PARAM)
        CALL MY_DEALLOC(ATA)
        CALL MY_DEALLOC(ATD)
        CALL MY_DEALLOC(A_ROW)
        CALL MY_DEALLOC(C_CP)
        RETURN
      ENDIF
      CALL MY_ALLOC(V_KNOT, NKNOT_V, "V_KNOT", stat=IEL)
      IF (IEL .NE. 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='U_KNOT/V_KNOT')
        CALL MY_DEALLOC(X_GRID)
        CALL MY_DEALLOC(GRID_NODE_RES)
        CALL MY_DEALLOC(DATA_PT)
        CALL MY_DEALLOC(U_PARAM)
        CALL MY_DEALLOC(V_PARAM)
        CALL MY_DEALLOC(ATA)
        CALL MY_DEALLOC(ATD)
        CALL MY_DEALLOC(A_ROW)
        CALL MY_DEALLOC(C_CP)
        CALL MY_DEALLOC(U_KNOT)
        RETURN
      ENDIF

  !C  Fill X_GRID and GRID_NODE_RES from connectivity-derived grid (GRID_NODE)
      K=1
      DO J=1,NY+1
        DO I=1,NX+1
          NODE_ID = GRID_NODE(I,J)
          IF (NODE_ID .GT. 0) THEN
            X_GRID(1,I,J) = X(1,NODE_ID)
            X_GRID(2,I,J) = X(2,NODE_ID)
            X_GRID(3,I,J) = X(3,NODE_ID)
            GRID_NODE_RES(K) = NODE_ID
            K = K + 1
          ENDIF
        ENDDO
      ENDDO

  !C=======================================================================
  !C   Optional: export original surface nodes to CSV (for visualization/debug)
  !C=======================================================================
      IF (q1np_export_csv) THEN
        CALL Q1NP_EXPORT_SURFACE_NODES_CSV(NX,NY,X_GRID,GRID_NODE_RES, &
     &      CSV_SURF_ID_IN)
      ENDIF

  !C     Fill DATA_PT with grid node coordinates; U_PARAM, V_PARAM = parametric (u,v) in [0,1]
      IDATA = 0
      DO J=1,NY+1
        DO I=1,NX+1
          IDATA = IDATA + 1
          DATA_PT(1,IDATA) = X_GRID(1,I,J)
          DATA_PT(2,IDATA) = X_GRID(2,I,J)
          DATA_PT(3,IDATA) = X_GRID(3,I,J)
          IF (NX > 0) U_PARAM(IDATA) = REAL(I-1)/REAL(NX)
          IF (NY > 0) V_PARAM(IDATA) = REAL(J-1)/REAL(NY)
        ENDDO
      ENDDO

  !C     Extract knot vectors for basis function evaluation
      CALL Q1NP_GET_KNOT_VECTORS(NX, NY, P, Q, Q1NP_KTAB, U_KNOT, V_KNOT)

  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  !C   METHOD 0: 3D plain least-squares (subdivision points)
  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF (ICP_METHOD_IN  .EQ. 0) THEN

  !C     ------------------------------------------------------------------
  !C     For method 0, add subdivision samples (DIV x DIV per quad) to
  !C     DATA_PT, U_PARAM, V_PARAM for a richer least-squares fit. Method 1
  !C     (Tikhonov) uses only the grid-node data filled above.
  !C
  !C     For subdivision points inside each quad (cell):
  !C       - Parametric domain in u and v: [0,1]
  !C       - S,T in [0,1] parameterize position in each quad (cell)
  !C         S = local u within quad, T = local v within quad
  !C         S = (II_SUB + 0.5) / DIV,    T = (JJ_SUB + 0.5) / DIV
  !C       - The global surface parameters U_PARAM, V_PARAM are:
  !C           U_PARAM = (I-1 + S) / NX,   V_PARAM = (J-1 + T) / NY
  !C         where (I,J) are the quad indices (1-based), NX, NY = surface grid dims
  !C
  !C     The surface position at each (S,T) is bilinearly interpolated from the four corner nodes of the quad:
  !C         X(S,T) = (1-S) * (1-T) * node (I,   J  )
  !C                + S     * (1-T) * node (I+1, J  )
  !C                + (1-S) * T     * node (I,   J+1)
  !C                + S     * T     * node (I+1, J+1)

  !C     The goal here is to assemble the normal equations for a least-squares NURBS surface fit.
  !C     - ATA  (matrix): Accumulates the outer products of the NURBS basis
  !C       functions evaluated at every data point.
  !C     - ATD  (matrix): Accumulates products of NURBS basis functions with
  !C       data-point coordinates (X,Y,Z).
  !C
  !C     In least-squares surface fitting, we are solving for the control points C such that:
  !C         minimize || A * C - D ||^2,
  !C     where:
  !C       - A is the matrix of basis functions evaluated at all sampled (u,v) parametric points,
  !C       - D is the corresponding data point coordinates in space,
  !C       - C is the set of unknown NURBS control points (to be found).
  !C
  !C     The normal equations come from setting the derivative to zero and yield:
  !C         (A^T * A) * C = (A^T * D)
  !C     or expressed in code variables:
  !C         ATA * C_CP = ATD
  !C
  !C     The loops below form ATA and ATD by summing over all sampled points, evaluating how much each basis function is
  !C     present at each point (A_ROW), and weighting appropriately by the data's coordinates.
  !C     ------------------------------------------------------------------
  !C     Fill DATA_PT with subdivision points (bilinear interpolation in each quad)
        do j = 1, ny
          do i = 1, nx
            DO JJ_SUB=0,DIV_IN-1
              DO II_SUB=0,DIV_IN-1
                S = (REAL(II_SUB,WP) + HALF)/REAL(DIV_IN,WP)
                T = (REAL(JJ_SUB,WP) + HALF)/REAL(DIV_IN,WP)
                IDATA = IDATA + 1

                DATA_PT(1,IDATA) = &
     &      (ONE-S)*(ONE-T)*X_GRID(1,I,J) + &
     &      S*(ONE-T)*X_GRID(1,I+1,J) + &
     &      (ONE-S)*T*X_GRID(1,I,J+1) + &
     &      S*T*X_GRID(1,I+1,J+1)

                DATA_PT(2,IDATA) = &
     &      (ONE-S)*(ONE-T)*X_GRID(2,I,J) + &
     &      S*(ONE-T)*X_GRID(2,I+1,J) + &
     &      (ONE-S)*T*X_GRID(2,I,J+1) + &
     &      S*T*X_GRID(2,I+1,J+1)

                DATA_PT(3,IDATA) = &
     &      (ONE-S)*(ONE-T)*X_GRID(3,I,J) + &
     &      S*(ONE-T)*X_GRID(3,I+1,J) + &
     &      (ONE-S)*T*X_GRID(3,I,J+1) + &
     &      S*T*X_GRID(3,I+1,J+1)

                IF (NX > 0) U_PARAM(IDATA) = (REAL(I-1)+S)/REAL(NX)
                IF (NY > 0) V_PARAM(IDATA) = (REAL(J-1)+T)/REAL(NY)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

  !C     Initialize ATA and ATD
      DO COL1=1,NCP
        DO COL2=1,NCP
          ATA(COL1,COL2) = ZERO
        ENDDO
        ATD(COL1,1) = ZERO
        ATD(COL1,2) = ZERO
        ATD(COL1,3) = ZERO
      ENDDO

  !C     Update ATA and ATD
  !C     Surface-grid nodes are weighted higher than subdivision samples so the
  !C     fitted surface stays close to the original mesh while interior samples
  !C     still suppress oscillatory control nets.
      DO IDATA=1,NDATA
        UU = U_PARAM(IDATA)
        VV = V_PARAM(IDATA)

        IF (IDATA <= (NX+1)*(NY+1)) THEN
          WGT = WEIGHT_IN
        ELSE
          WGT = ONE
        END IF

        ! A_ROW is the basis functions at the data point (UU,VV)
        CALL Q1NP_BASIS_ROW_AT_UV(UU, VV, U_KNOT, V_KNOT, P, Q, &
     &      NCP_U, NCP_V, A_ROW)

        ! Update ATA and ATD
        DO COL1=1,NCP
          DO COL2=1,NCP
            ATA(COL1,COL2) = ATA(COL1,COL2) + &
     &      WGT * A_ROW(COL1)*A_ROW(COL2)
          ENDDO
          ATD(COL1,1) = ATD(COL1,1) + WGT*A_ROW(COL1)*DATA_PT(1,IDATA)
          ATD(COL1,2) = ATD(COL1,2) + WGT*A_ROW(COL1)*DATA_PT(2,IDATA)
          ATD(COL1,3) = ATD(COL1,3) + WGT*A_ROW(COL1)*DATA_PT(3,IDATA)
        ENDDO
      ENDDO

  !C     Solve normal equations ATA * C_CP = ATD via Cholesky (3 RHS = X,Y,Z)
      CALL CHOLESKY_SOLVE_Q1NP(NCP, ATA, NCP, ATD, 3, C_CP)
      !return value is C_CP, (C_CP = inverse of ATA * ATD)

  !C     Write solved control points into Q1NP_CPTAB (CP_MAP already set in Step 4)
      DO CP_COUNTER=1,NCP
        Q1NP_CPTAB(1,CP_COUNTER) = C_CP(CP_COUNTER,1)
        Q1NP_CPTAB(2,CP_COUNTER) = C_CP(CP_COUNTER,2)
        Q1NP_CPTAB(3,CP_COUNTER) = C_CP(CP_COUNTER,3)
      ENDDO

  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  !C   METHOD 1: Tikhonov-regularized least-squares (grid nodes only)
  !C   Minimizes ||A*C - D||^2 + lambda*||L*C||^2; L = 2D discrete Laplacian on CP grid.
  !C   Lambda is chosen from a log-spaced set to minimize RMS fit error.
  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       ELSE IF (ICP_METHOD_IN  .EQ. 1) THEN

  !C       Allocate regularization arrays: L_REG (second-diff), LTL = L^T*L, ATA_REG, C_CP_BEST, TMPVEC
        CALL MY_ALLOC(L_REG, NCP, NCP, "L_REG")
        CALL MY_ALLOC(LTL, NCP, NCP, "LTL")
        CALL MY_ALLOC(ATA_REG, NCP, NCP, "ATA_REG")
        CALL MY_ALLOC(C_CP_BEST, NCP, 3, "C_CP_BEST")
        CALL MY_ALLOC(TMPVEC, NCP, "TMPVEC")

  !C       Build normal equations ATA, ATD and data norm DTD (NDATA = (NX+1)*(NY+1) for method 1)
        DO COL1=1,NCP
          DO COL2=1,NCP
            ATA(COL1,COL2) = ZERO
          ENDDO
          ATD(COL1,1) = ZERO
          ATD(COL1,2) = ZERO
          ATD(COL1,3) = ZERO
        ENDDO
        DTD = ZERO

        DO IDATA=1,NDATA
          UU = U_PARAM(IDATA)
          VV = V_PARAM(IDATA)

          CALL Q1NP_BASIS_ROW_AT_UV(UU, VV, U_KNOT, V_KNOT, P, Q, &
     &      NCP_U, NCP_V, A_ROW)

          DO COL1=1,NCP
            DO COL2=1,NCP
              ATA(COL1,COL2) = ATA(COL1,COL2) + &
     &      A_ROW(COL1)*A_ROW(COL2)
            ENDDO
            ATD(COL1,1) = ATD(COL1,1) + A_ROW(COL1)*DATA_PT(1,IDATA)
            ATD(COL1,2) = ATD(COL1,2) + A_ROW(COL1)*DATA_PT(2,IDATA)
            ATD(COL1,3) = ATD(COL1,3) + A_ROW(COL1)*DATA_PT(3,IDATA)
          ENDDO

          DTD = DTD &
     &      + DATA_PT(1,IDATA)*DATA_PT(1,IDATA) &
     &      + DATA_PT(2,IDATA)*DATA_PT(2,IDATA) &
     &      + DATA_PT(3,IDATA)*DATA_PT(3,IDATA)
        ENDDO

  !C       L_REG = 2D discrete Laplacian on (NCP_U x NCP_V) CP grid;
  !C       LTL = L_REG^T * L_REG.  Each row applies -NNEIGHBOR on diagonal
  !C       and +1 on each existing u/v neighbor (boundary-adapted).
        DO COL1=1,NCP
          DO COL2=1,NCP
            L_REG(COL1,COL2) = ZERO
            LTL(COL1,COL2)   = ZERO
          ENDDO
        ENDDO
        DO J=1,NCP_V
          DO I=1,NCP_U
            COL1 = (J-1)*NCP_U + I
            COL = 0
            IF (I .GT. 1) THEN
              L_REG(COL1, COL1-1)     = ONE
              COL = COL + 1
            ENDIF
            IF (I .LT. NCP_U) THEN
              L_REG(COL1, COL1+1)     = ONE
              COL = COL + 1
            ENDIF
            IF (J .GT. 1) THEN
              L_REG(COL1, COL1-NCP_U) = ONE
              COL = COL + 1
            ENDIF
            IF (J .LT. NCP_V) THEN
              L_REG(COL1, COL1+NCP_U) = ONE
              COL = COL + 1
            ENDIF
            L_REG(COL1, COL1) = -REAL(COL, KIND=WP)
          ENDDO
        ENDDO
        DO COL1=1,NCP
          DO COL2=1,NCP
            DO COL=1,NCP
              LTL(COL1,COL2) = LTL(COL1,COL2) &
     &      + L_REG(COL,COL1)*L_REG(COL,COL2)
            ENDDO
          ENDDO
        ENDDO

  !C       Tikhonov solve (ATA + lambda*LTL)*C = ATD; try small log-spaced lambdas.
  !C       For Q1NP we prioritize reproducing the original top surface closely;
  !C       strong regularization smooths the patch but may seed a geometric mismatch at boundaries.
        NLAM        = 15
        LAMBDA_BEST = -ONE
        BEST_RMS    = -ONE
        DO ILAM = 1, NLAM
          LAMBDA_CUR = 1.0D-12 * &
     &      ( (1.0D-4/1.0D-12) ** &
     &      ( REAL(ILAM-1,kind(ONE)) / REAL(NLAM-1,kind(ONE)) ) )
          DO COL1=1,NCP
            DO COL2=1,NCP
              ATA_REG(COL1,COL2) = &
     &      ATA(COL1,COL2) + LAMBDA_CUR * LTL(COL1,COL2)
            ENDDO
          ENDDO
          CALL CHOLESKY_SOLVE_Q1NP(NCP, ATA_REG, NCP, ATD, 3, C_CP)

  !C         Residual ||A*C - D||^2 = C^T*ATA*C - 2*C^T*ATD + DTD (per dimension, then sum)
          RES2 = ZERO
          DO COL=1,3
            DO COL1=1,NCP
              TMPVEC(COL1) = ZERO
            ENDDO
            DO COL1=1,NCP
              DO COL2=1,NCP
                TMPVEC(COL1) = TMPVEC(COL1) + &
     &      ATA(COL1,COL2)*C_CP(COL2,COL)
              ENDDO
            ENDDO
            RES2 = RES2 &
     &      + DOT_PRODUCT(C_CP(1:NCP,COL),TMPVEC(1:NCP)) &
     &      - TWO*DOT_PRODUCT(C_CP(1:NCP,COL),ATD(1:NCP,COL))
          ENDDO
          RES2 = RES2 + DTD
          IF (RES2 .GT. ZERO) THEN
            RMS = SQRT(RES2 / REAL(3*NDATA,kind(ONE)))
          ELSE
            RMS = ZERO
          ENDIF

          IF (BEST_RMS .LT. ZERO .OR. RMS .LT. BEST_RMS) THEN
            BEST_RMS    = RMS
            LAMBDA_BEST = LAMBDA_CUR
            DO COL1=1,NCP
              C_CP_BEST(COL1,1) = C_CP(COL1,1)
              C_CP_BEST(COL1,2) = C_CP(COL1,2)
              C_CP_BEST(COL1,3) = C_CP(COL1,3)
            ENDDO
          ENDIF
        ENDDO

  !C       Store best control points in Q1NP_CPTAB (same layout as Method 0)
        DO CP_COUNTER=1,NCP
          Q1NP_CPTAB(1,CP_COUNTER) = C_CP_BEST(CP_COUNTER,1)
          Q1NP_CPTAB(2,CP_COUNTER) = C_CP_BEST(CP_COUNTER,2)
          Q1NP_CPTAB(3,CP_COUNTER) = C_CP_BEST(CP_COUNTER,3)
        ENDDO
        CALL MY_DEALLOC(L_REG)
        CALL MY_DEALLOC(LTL)
        CALL MY_DEALLOC(ATA_REG)
        CALL MY_DEALLOC(C_CP_BEST)
        CALL MY_DEALLOC(TMPVEC)
      ENDIF

  !C     Deallocate fit and working arrays
      CALL MY_DEALLOC(X_GRID)
      CALL MY_DEALLOC(GRID_NODE_RES)
      CALL MY_DEALLOC(DATA_PT)
      CALL MY_DEALLOC(U_PARAM)
      CALL MY_DEALLOC(V_PARAM)
      CALL MY_DEALLOC(ATA)
      CALL MY_DEALLOC(ATD)
      CALL MY_DEALLOC(A_ROW)
      CALL MY_DEALLOC(C_CP)
      CALL MY_DEALLOC(U_KNOT)
      CALL MY_DEALLOC(V_KNOT)
      END SUBROUTINE Q1NP_FIT_CONTROL_POINTS
!C
!C=======================================================================
!C   Q1NP_REPORT_FIT_ERROR:
!C   Compute RMS / MAX geometric mismatch between fitted NURBS top surface
!C   and original surface-grid nodes; print diagnostic summary to IOUT.
!C=======================================================================
!||====================================================================
!||    q1np_report_fit_error   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    genq1np                 ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE Q1NP_REPORT_FIT_ERROR(NX, NY, P, Q, NCP_U, NCP_V, NCP, NUMNOD, &
     &     GRID_NODE, X, Q1NP_KTAB, Q1NP_CPTAB, IOUT)
  !C-----------------------------------------------
  !C   M o d u l e s
  !C-----------------------------------------------
        USE precision_mod, ONLY : WP
        USE constant_mod,  ONLY : ZERO, ONE
        USE q1np_geom_mod, ONLY : q1np_get_knot_vectors, q1np_basis_row_at_uv
        USE my_alloc_mod,  ONLY : my_alloc
        USE my_dealloc_mod, ONLY : my_dealloc
        IMPLICIT NONE
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
        INTEGER,  INTENT(IN) :: NX, NY, P, Q, NCP_U, NCP_V, NCP, NUMNOD, IOUT
        INTEGER,  INTENT(IN) :: GRID_NODE(NX+1,NY+1)
        REAL(WP), INTENT(IN) :: X(3,NUMNOD), Q1NP_KTAB(:), Q1NP_CPTAB(3,NCP)
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
        INTEGER :: NKNOT_U, NKNOT_V
        INTEGER :: I, J, K, NODE_ID, NPTS
        INTEGER :: I_MAX, J_MAX, NODE_MAX
        REAL(WP), ALLOCATABLE :: U_KNOT(:), V_KNOT(:), A_ROW(:)
        REAL(WP) :: UU, VV
        REAL(WP) :: XFIT(3), DX(3), ERR2, ERR, ERR_MAX, ERR_RMS

        NKNOT_U = NX + 2*P + 1
        NKNOT_V = NY + 2*Q + 1
        CALL MY_ALLOC(U_KNOT, NKNOT_U, "U_KNOT")
        CALL MY_ALLOC(V_KNOT, NKNOT_V, "V_KNOT")
        CALL MY_ALLOC(A_ROW, NCP, "A_ROW")

        CALL Q1NP_GET_KNOT_VECTORS(NX, NY, P, Q, Q1NP_KTAB, U_KNOT, V_KNOT)

        ERR_MAX  = ZERO
        ERR_RMS  = ZERO
        I_MAX    = 0
        J_MAX    = 0
        NODE_MAX = 0
        NPTS     = 0

        DO J = 1, NY + 1
          DO I = 1, NX + 1
            NODE_ID = GRID_NODE(I, J)
            IF (NODE_ID <= 0) CYCLE

            IF (NX > 0) THEN
              UU = REAL(I - 1, KIND=WP) / REAL(NX, KIND=WP)
            ELSE
              UU = ZERO
            END IF
            IF (NY > 0) THEN
              VV = REAL(J - 1, KIND=WP) / REAL(NY, KIND=WP)
            ELSE
              VV = ZERO
            END IF

            CALL Q1NP_BASIS_ROW_AT_UV(UU, VV, U_KNOT, V_KNOT, P, Q, &
     &           NCP_U, NCP_V, A_ROW)

            XFIT = ZERO
            DO K = 1, NCP
              XFIT(1) = XFIT(1) + A_ROW(K) * Q1NP_CPTAB(1, K)
              XFIT(2) = XFIT(2) + A_ROW(K) * Q1NP_CPTAB(2, K)
              XFIT(3) = XFIT(3) + A_ROW(K) * Q1NP_CPTAB(3, K)
            END DO

            DX(1) = XFIT(1) - X(1, NODE_ID)
            DX(2) = XFIT(2) - X(2, NODE_ID)
            DX(3) = XFIT(3) - X(3, NODE_ID)
            ERR2  = DX(1)*DX(1) + DX(2)*DX(2) + DX(3)*DX(3)
            ERR   = SQRT(ERR2)

            ERR_RMS = ERR_RMS + ERR2
            NPTS    = NPTS + 1

            IF (ERR > ERR_MAX) THEN
              ERR_MAX  = ERR
              I_MAX    = I
              J_MAX    = J
              NODE_MAX = NODE_ID
            END IF
          END DO
        END DO

        IF (NPTS > 0) THEN
          ERR_RMS = SQRT(ERR_RMS / REAL(NPTS, KIND=WP))
        END IF

        WRITE(IOUT, 306) ERR_RMS, ERR_MAX, NODE_MAX, I_MAX, J_MAX

        CALL MY_DEALLOC(U_KNOT)
        CALL MY_DEALLOC(V_KNOT)
        CALL MY_DEALLOC(A_ROW)
        RETURN
  306    FORMAT(' Q1NP top-surface fit error: RMS=',1PE12.5, &
     &         ' MAX=',1PE12.5,' at node ',I10,' grid(',I4,',',I4,')')
      END SUBROUTINE Q1NP_REPORT_FIT_ERROR
!C
!C=======================================================================
!C   Rebuild the bottom bulk face from the parent HEX8 and fitted top patch.
!C   This keeps starter-side bulk connectivity consistent for both single-
!C   and multi-surface smoothing setups.
!C=======================================================================
!||====================================================================
!||    q1np_rebuild_bulk_from_hex_geom     ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    genq1np                             ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_select_global_cp_orientation   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||    iface                               ../starter/source/ale/ale3d/iface.F
!||    q1np_cp_map_lookup                  ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_eval_top_patch_point           ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE Q1NP_REBUILD_BULK_FROM_HEX_GEOM(I_ELEM, J_ELEM, P_ELEM, Q_ELEM, NCP_U, NCP_V, &
     &     NCP, NUMNOD, NIXS, NUMELS, CP_MAP, IFLIP, U_KNOT, V_KNOT, Q1NP_CPTAB, X, IXS, &
     &     IEL_HEX8, NODES_BULK_OUT, MATCH_SCORE_OUT, IERR)
  !C-----------------------------------------------
  !C   M o d u l e s
  !C-----------------------------------------------
        USE precision_mod, ONLY : WP
        USE constant_mod, ONLY : ZERO, ONE
        IMPLICIT NONE
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
        INTEGER, INTENT(IN) :: I_ELEM, J_ELEM, P_ELEM, Q_ELEM
        INTEGER, INTENT(IN) :: NCP_U, NCP_V, NCP, NUMNOD, NIXS, NUMELS, IEL_HEX8
        INTEGER, INTENT(IN) :: CP_MAP(:,:), IFLIP, IXS(NIXS,NUMELS)
        INTEGER, INTENT(INOUT) :: NODES_BULK_OUT(4)
        REAL(WP), INTENT(IN) :: U_KNOT(:), V_KNOT(:), Q1NP_CPTAB(3,NCP), X(3,NUMNOD)
        REAL(WP), INTENT(INOUT) :: MATCH_SCORE_OUT
        INTEGER, INTENT(INOUT) :: IERR
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
        INTEGER, PARAMETER :: FACE_IXS(4,6) = RESHAPE( (/ &
     &      2,3,4,5, 6,7,8,9, 2,3,7,6, 3,4,8,7, 4,5,9,8, 5,2,6,9 /), (/ 4,6 /) )
        INTEGER, PARAMETER :: OPPOSITE(6) = (/ 2,1,5,6,3,4 /)
        INTEGER, PARAMETER :: OPP_PAIR(4,6) = RESHAPE( (/ &
     &      1,2,3,4, 1,2,3,4, 2,1,4,3, 2,1,4,3, 2,1,4,3, 2,1,4,3 /), (/ 4,6 /) )
        INTEGER, PARAMETER :: CORNER_PERM(4,8) = RESHAPE( (/ &
     &      1,2,3,4, &
     &      2,3,4,1, &
     &      3,4,1,2, &
     &      4,1,2,3, &
     &      1,4,3,2, &
     &      4,3,2,1, &
     &      3,2,1,4, &
     &      2,1,4,3 /), (/ 4,8 /) )
        INTEGER :: NCTRL_ELEM, CP_ID, IDX, II, JJ, IFACE, IFOPP, IPERM, K, JCORNER
        INTEGER :: NODES_FACE(4), NODES_OPP(4), CANDIDATE_BULK(4)
        REAL(WP) :: TOP_CP(3,(P_ELEM+1)*(Q_ELEM+1))
        REAL(WP) :: TOP_CORNER(3,4), DIFF(3), SCORE_LOCAL, BEST_SCORE

        NODES_BULK_OUT = 0
        MATCH_SCORE_OUT = HUGE(ONE)
        IERR = 1
        IF (IEL_HEX8 <= 0 .OR. IEL_HEX8 > NUMELS) RETURN

        NCTRL_ELEM = (P_ELEM + 1) * (Q_ELEM + 1)
        IDX = 1
        DO JJ = 0, Q_ELEM
          DO II = 0, P_ELEM
            CP_ID = Q1NP_CP_MAP_LOOKUP(I_ELEM + II, J_ELEM + JJ, NCP_U, NCP_V, CP_MAP, IFLIP)
            IF (CP_ID <= 0 .OR. CP_ID > NCP) THEN
              IERR = 2
              RETURN
            ENDIF
            TOP_CP(1:3,IDX) = Q1NP_CPTAB(1:3,CP_ID)
            IDX = IDX + 1
          ENDDO
        ENDDO

        CALL Q1NP_EVAL_TOP_PATCH_POINT(P_ELEM, Q_ELEM, U_KNOT, V_KNOT, I_ELEM-1, J_ELEM-1, &
     &       TOP_CP, -ONE, -ONE, TOP_CORNER(1:3,1))
        CALL Q1NP_EVAL_TOP_PATCH_POINT(P_ELEM, Q_ELEM, U_KNOT, V_KNOT, I_ELEM-1, J_ELEM-1, &
     &       TOP_CP,  ONE, -ONE, TOP_CORNER(1:3,2))
        CALL Q1NP_EVAL_TOP_PATCH_POINT(P_ELEM, Q_ELEM, U_KNOT, V_KNOT, I_ELEM-1, J_ELEM-1, &
     &       TOP_CP,  ONE,  ONE, TOP_CORNER(1:3,3))
        CALL Q1NP_EVAL_TOP_PATCH_POINT(P_ELEM, Q_ELEM, U_KNOT, V_KNOT, I_ELEM-1, J_ELEM-1, &
     &       TOP_CP, -ONE,  ONE, TOP_CORNER(1:3,4))

        BEST_SCORE = HUGE(ONE)
        DO IFACE = 1, 6
          IFOPP = OPPOSITE(IFACE)
          DO K = 1, 4
            NODES_FACE(K) = IXS(FACE_IXS(K,IFACE), IEL_HEX8)
            NODES_OPP(K) = IXS(FACE_IXS(K,IFOPP), IEL_HEX8)
          ENDDO

          IF (MINVAL(NODES_FACE) <= 0 .OR. MAXVAL(NODES_FACE) > NUMNOD) CYCLE
          IF (MINVAL(NODES_OPP) <= 0 .OR. MAXVAL(NODES_OPP) > NUMNOD) CYCLE

          DO IPERM = 1, 8
            SCORE_LOCAL = ZERO
            DO K = 1, 4
              JCORNER = CORNER_PERM(K,IPERM)
              DIFF(1:3) = TOP_CORNER(1:3,K) - X(1:3,NODES_FACE(JCORNER))
              SCORE_LOCAL = SCORE_LOCAL + DOT_PRODUCT(DIFF, DIFF)
              CANDIDATE_BULK(K) = NODES_OPP(OPP_PAIR(JCORNER,IFACE))
            ENDDO

            IF (SCORE_LOCAL < BEST_SCORE) THEN
              BEST_SCORE = SCORE_LOCAL
              NODES_BULK_OUT(1:4) = CANDIDATE_BULK(1:4)
            ENDIF
          ENDDO
        ENDDO

        IF (MINVAL(NODES_BULK_OUT) <= 0 .OR. MAXVAL(NODES_BULK_OUT) > NUMNOD) THEN
          IERR = 3
          RETURN
        ENDIF

        MATCH_SCORE_OUT = BEST_SCORE
        IERR = 0
      END SUBROUTINE Q1NP_REBUILD_BULK_FROM_HEX_GEOM
!C
!C=======================================================================
!C   Evaluate one fitted top-surface point on the current parametric cell.
!C=======================================================================
!||====================================================================
!||    q1np_eval_top_patch_point         ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    q1np_rebuild_bulk_from_hex_geom   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE Q1NP_EVAL_TOP_PATCH_POINT(P_ELEM, Q_ELEM, U_KNOT, V_KNOT, ELEM_U, ELEM_V, &
     &     TOP_CP, XI, ETA, XYZ_OUT)
  !C-----------------------------------------------
  !C   M o d u l e s
  !C-----------------------------------------------
        USE precision_mod, ONLY : WP
        USE constant_mod, ONLY : ZERO, ONE
        USE q1np_geom_mod, ONLY : q1np_shape_functions
        IMPLICIT NONE
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
        INTEGER, INTENT(IN) :: P_ELEM, Q_ELEM, ELEM_U, ELEM_V
        REAL(WP), INTENT(IN) :: U_KNOT(:), V_KNOT(:)
        REAL(WP), INTENT(IN) :: TOP_CP(3,(P_ELEM+1)*(Q_ELEM+1))
        REAL(WP), INTENT(IN) :: XI, ETA
        REAL(WP), INTENT(INOUT) :: XYZ_OUT(3)
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
        INTEGER :: NCTRL_ELEM, NNODE_ELEM, K
        REAL(WP) :: N_LOCAL((P_ELEM+1)*(Q_ELEM+1) + 4)
        REAL(WP) :: DN_LOCAL((P_ELEM+1)*(Q_ELEM+1) + 4,3)

        NCTRL_ELEM = (P_ELEM + 1) * (Q_ELEM + 1)
        NNODE_ELEM = NCTRL_ELEM + 4
        N_LOCAL = ZERO
        DN_LOCAL = ZERO

        CALL Q1NP_SHAPE_FUNCTIONS(XI, ETA, ONE, P_ELEM, Q_ELEM, U_KNOT, V_KNOT, ELEM_U, ELEM_V, &
     &       N_LOCAL(1:NNODE_ELEM), DN_LOCAL(1:NNODE_ELEM,1:3))

        XYZ_OUT = ZERO
        DO K = 1, NCTRL_ELEM
          XYZ_OUT(1:3) = XYZ_OUT(1:3) + N_LOCAL(K) * TOP_CP(1:3,K)
        ENDDO
      END SUBROUTINE Q1NP_EVAL_TOP_PATCH_POINT
!C
!C=======================================================================
!C   Q1NP_CP_MAP_LOOKUP:
!C   Return CP index at (IU,IV) after applying global orientation flip.
!C=======================================================================
!||====================================================================
!||    q1np_cp_map_lookup                  ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    q1np_rebuild_bulk_from_hex_geom     ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_select_global_cp_orientation   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||====================================================================
      INTEGER FUNCTION Q1NP_CP_MAP_LOOKUP(IU, IV, NCP_U, NCP_V, CP_MAP, IFLIP)
        IMPLICIT NONE
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
        INTEGER, INTENT(IN) :: IU, IV, NCP_U, NCP_V, CP_MAP(:,:), IFLIP
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
        INTEGER :: II_LOC, JJ_LOC

        II_LOC = IU
        JJ_LOC = IV
        SELECT CASE (IFLIP)
        CASE (1)
          II_LOC = NCP_U - IU + 1
        CASE (2)
          JJ_LOC = NCP_V - IV + 1
        CASE (3)
          II_LOC = NCP_U - IU + 1
          JJ_LOC = NCP_V - IV + 1
        END SELECT

        Q1NP_CP_MAP_LOOKUP = CP_MAP(II_LOC, JJ_LOC)
      END FUNCTION Q1NP_CP_MAP_LOOKUP
!C
!C=======================================================================
!C   Q1NP_SELECT_GLOBAL_CP_ORIENTATION
!C   Test the four global (U,V) flip states and select the one that gives
!C   the best consistent top-vs-bulk orientation over all active patches.
!C=======================================================================
!||====================================================================
!||    q1np_select_global_cp_orientation   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    genq1np                             ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||    findhex8fromsurf                    ../starter/source/elements/solid/solid_q1np/q1np_findhex8fromsurface.F90
!||    q1np_cp_map_lookup                  ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_rebuild_bulk_from_hex_geom     ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE Q1NP_SELECT_GLOBAL_CP_ORIENTATION(NX, NY, P, Q, NCP_U, NCP_V, NCP, NUMNOD, &
     &     CP_MAP, Q1NP_CPTAB, Q1NP_KTAB, GRID_NODE, GRID_TO_SEG, IXS, NIXS, NUMELS, X, IOUT, &
     &     MAX_CP_U,MAX_CP_V)
  !C-----------------------------------------------
  !C   M o d u l e s
  !C-----------------------------------------------
        USE precision_mod, ONLY : WP
        USE constant_mod, ONLY : ZERO, ONE
        USE q1np_geom_mod, ONLY : q1np_get_knot_vectors
        USE my_alloc_mod,  ONLY : my_alloc
        USE my_dealloc_mod, ONLY : my_dealloc
        IMPLICIT NONE
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
        INTEGER, INTENT(IN) :: NX, NY, P, Q, NCP_U, NCP_V, NCP, NUMNOD, NIXS, NUMELS, IOUT
        INTEGER, INTENT(IN) :: MAX_CP_U,MAX_CP_V
        INTEGER, INTENT(INOUT) :: CP_MAP(MAX_CP_U,MAX_CP_V)
        INTEGER, INTENT(IN) :: GRID_NODE(NX+1,NY+1), GRID_TO_SEG(NX,NY), IXS(NIXS,NUMELS)
        REAL(KIND=WP), INTENT(IN) :: Q1NP_CPTAB(3,NCP), Q1NP_KTAB(:), X(3,NUMNOD)
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
        INTEGER :: I, J, K, IFLIP, BEST_FLIP, IEL_HEX8
        INTEGER :: NKNOT_U, NKNOT_V, IERR_BULK
        INTEGER :: NODES_SURF(4), NODES_BULK(4), CP_CORNER(4)
        INTEGER, ALLOCATABLE :: CP_MAP_TMP(:,:)
        LOGICAL :: VALID, FOUND_VALID
        REAL(KIND=WP) :: TOP_NODE(3,4), BOT_NODE(3,4), DIFF(3)
        REAL(KIND=WP) :: UTOP(3), VTOP(3), UBOT(3), VBOT(3)
        REAL(KIND=WP) :: NTOP(3), NBOT(3), DENOM
        REAL(KIND=WP) :: TOP_MAG, BOT_MAG, TOP_BOT_DOT
        REAL(KIND=WP) :: SCORE, BEST_SCORE, COS_SUM, BEST_COS_SUM, BULK_MATCH_SCORE
        REAL(KIND=WP), PARAMETER :: ORIENT_TOL = 1.0E-12_WP
        REAL(KIND=WP), ALLOCATABLE :: U_KNOT(:), V_KNOT(:)

        BEST_FLIP = 0
        FOUND_VALID = .FALSE.
        BEST_SCORE = HUGE(ONE)
        BEST_COS_SUM = -HUGE(ONE)
        NKNOT_U = NX + 2*P + 1
        NKNOT_V = NY + 2*Q + 1
        CALL MY_ALLOC(U_KNOT, NKNOT_U, "U_KNOT")
        CALL MY_ALLOC(V_KNOT, NKNOT_V, "V_KNOT")
        CALL Q1NP_GET_KNOT_VECTORS(NX, NY, P, Q, Q1NP_KTAB, U_KNOT, V_KNOT)

        DO IFLIP = 0, 3
          VALID = .TRUE.
          SCORE = ZERO
          COS_SUM = ZERO

          DO J = 1, NY
            DO I = 1, NX
              IF (GRID_TO_SEG(I,J) <= 0) CYCLE

              NODES_SURF(1) = GRID_NODE(I  ,J  )
              NODES_SURF(2) = GRID_NODE(I+1,J  )
              NODES_SURF(3) = GRID_NODE(I+1,J+1)
              NODES_SURF(4) = GRID_NODE(I  ,J+1)

              CALL findhex8fromsurf(NODES_SURF, IXS, IEL_HEX8, NODES_BULK, NIXS, NUMELS)
              IF (IEL_HEX8 <= 0) THEN
                VALID = .FALSE.
                EXIT
              ENDIF

              CALL Q1NP_REBUILD_BULK_FROM_HEX_GEOM(I, J, P, Q, NCP_U, NCP_V, NCP, NUMNOD, &
     &            NIXS, NUMELS, CP_MAP, IFLIP, U_KNOT, V_KNOT, Q1NP_CPTAB, X, IXS, IEL_HEX8, &
     &            NODES_BULK, BULK_MATCH_SCORE, IERR_BULK)
              IF (IERR_BULK /= 0) THEN
                VALID = .FALSE.
                EXIT
              ENDIF

              CP_CORNER(1) = Q1NP_CP_MAP_LOOKUP(I,   J,   NCP_U, NCP_V, CP_MAP, IFLIP)
              CP_CORNER(2) = Q1NP_CP_MAP_LOOKUP(I+P, J,   NCP_U, NCP_V, CP_MAP, IFLIP)
              CP_CORNER(3) = Q1NP_CP_MAP_LOOKUP(I+P, J+Q, NCP_U, NCP_V, CP_MAP, IFLIP)
              CP_CORNER(4) = Q1NP_CP_MAP_LOOKUP(I,   J+Q, NCP_U, NCP_V, CP_MAP, IFLIP)

              IF (MINVAL(CP_CORNER) <= 0 .OR. MINVAL(NODES_BULK) <= 0) THEN
                VALID = .FALSE.
                EXIT
              ENDIF

              DO K = 1, 4
                TOP_NODE(1:3,K) = Q1NP_CPTAB(1:3, CP_CORNER(K))
                BOT_NODE(1:3,K) = X(1:3, NODES_BULK(K))
              ENDDO

              UTOP(1:3) = TOP_NODE(1:3,2) - TOP_NODE(1:3,1)
              VTOP(1:3) = TOP_NODE(1:3,4) - TOP_NODE(1:3,1)
              UBOT(1:3) = BOT_NODE(1:3,2) - BOT_NODE(1:3,1)
              VBOT(1:3) = BOT_NODE(1:3,4) - BOT_NODE(1:3,1)

              NTOP(1) = UTOP(2)*VTOP(3) - UTOP(3)*VTOP(2)
              NTOP(2) = UTOP(3)*VTOP(1) - UTOP(1)*VTOP(3)
              NTOP(3) = UTOP(1)*VTOP(2) - UTOP(2)*VTOP(1)
              NBOT(1) = UBOT(2)*VBOT(3) - UBOT(3)*VBOT(2)
              NBOT(2) = UBOT(3)*VBOT(1) - UBOT(1)*VBOT(3)
              NBOT(3) = UBOT(1)*VBOT(2) - UBOT(2)*VBOT(1)

              TOP_MAG = SQRT(MAX(DOT_PRODUCT(NTOP, NTOP), ZERO))
              BOT_MAG = SQRT(MAX(DOT_PRODUCT(NBOT, NBOT), ZERO))
              DENOM = TOP_MAG * BOT_MAG
              IF (DENOM <= ORIENT_TOL) THEN
                VALID = .FALSE.
                EXIT
              ENDIF

              TOP_BOT_DOT = DOT_PRODUCT(NTOP, NBOT)
              IF (TOP_BOT_DOT <= ORIENT_TOL * DENOM) THEN
                VALID = .FALSE.
                EXIT
              ENDIF

              COS_SUM = COS_SUM + MIN(ONE, MAX(-ONE, TOP_BOT_DOT / DENOM))
              DO K = 1, 4
                DIFF(1:3) = TOP_NODE(1:3,K) - BOT_NODE(1:3,K)
                SCORE = SCORE + DOT_PRODUCT(DIFF, DIFF)
              ENDDO
            ENDDO
            IF (.NOT. VALID) EXIT
          ENDDO

          IF (.NOT. VALID) CYCLE

          IF ((.NOT. FOUND_VALID) .OR. SCORE < BEST_SCORE .OR. &
     &        (ABS(SCORE-BEST_SCORE) <= ORIENT_TOL .AND. COS_SUM > BEST_COS_SUM)) THEN
            FOUND_VALID = .TRUE.
            BEST_FLIP = IFLIP
            BEST_SCORE = SCORE
            BEST_COS_SUM = COS_SUM
          ENDIF
        ENDDO

        IF (.NOT. FOUND_VALID .OR. BEST_FLIP == 0) THEN
          CALL MY_DEALLOC(U_KNOT)
          CALL MY_DEALLOC(V_KNOT)
          RETURN
        ENDIF

        CALL MY_ALLOC(CP_MAP_TMP, SIZE(CP_MAP,1), SIZE(CP_MAP,2), "CP_MAP_TMP")
        CP_MAP_TMP(:,:) = CP_MAP(:,:)

        DO J = 1, NCP_V
          DO I = 1, NCP_U
            CP_MAP_TMP(I,J) = Q1NP_CP_MAP_LOOKUP(I, J, NCP_U, NCP_V, CP_MAP, BEST_FLIP)
          ENDDO
        ENDDO

        CP_MAP(1:NCP_U,1:NCP_V) = CP_MAP_TMP(1:NCP_U,1:NCP_V)
        CALL MY_DEALLOC(CP_MAP_TMP)

        SELECT CASE (BEST_FLIP)
        CASE (1)
          WRITE(IOUT,'(A)') '  ** Q1NP: flipped global NURBS orientation in U.'
          WRITE(*,'(A)')    '  ** Q1NP: flipped global NURBS orientation in U.'
        CASE (2)
          WRITE(IOUT,'(A)') '  ** Q1NP: flipped global NURBS orientation in V.'
          WRITE(*,'(A)')    '  ** Q1NP: flipped global NURBS orientation in V.'
        CASE (3)
          WRITE(IOUT,'(A)') '  ** Q1NP: flipped global NURBS orientation in U and V.'
          WRITE(*,'(A)')    '  ** Q1NP: flipped global NURBS orientation in U and V.'
        END SELECT
        CALL MY_DEALLOC(U_KNOT)
        CALL MY_DEALLOC(V_KNOT)
      END SUBROUTINE Q1NP_SELECT_GLOBAL_CP_ORIENTATION
!C
!C=======================================================================
!C   Q1NP_CHECK_ELEMENT_ORIENTATION
!C   Compute top/bottom patch normals for one candidate element and return
!C   magnitudes and signed alignment metrics used by orientation screening.
!C=======================================================================
!||====================================================================
!||    q1np_check_element_orientation   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- called by ------------------------------------------------------
!||    genq1np                          ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE Q1NP_CHECK_ELEMENT_ORIENTATION(I_ELEM, J_ELEM, P_ELEM, Q_ELEM, &
     &     MAX_CP_U, MAX_CP_V, NCP, NUMNOD, CP_MAP, Q1NP_CPTAB, NODES_BULK, X, &
     &     TOP_MAG, BOT_MAG, TOP_BOT_DOT, TOP_BOT_COS, IERR)
  !C-----------------------------------------------
  !C   M o d u l e s
  !C-----------------------------------------------
        USE precision_mod, ONLY : WP
        USE constant_mod, ONLY : ZERO, ONE
        IMPLICIT NONE
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
        INTEGER, INTENT(IN) :: I_ELEM, J_ELEM, P_ELEM, Q_ELEM
        INTEGER, INTENT(IN) :: MAX_CP_U, MAX_CP_V, NCP, NUMNOD
        INTEGER, INTENT(IN) :: CP_MAP(MAX_CP_U,MAX_CP_V)
        INTEGER, INTENT(IN) :: NODES_BULK(4)
        REAL(KIND=WP), INTENT(IN) :: Q1NP_CPTAB(3,NCP), X(3,NUMNOD)
        REAL(KIND=WP), INTENT(INOUT) :: TOP_MAG, BOT_MAG, TOP_BOT_DOT, TOP_BOT_COS
        INTEGER, INTENT(INOUT) :: IERR
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
        INTEGER :: CP_CORNER(4)
        REAL(KIND=WP) :: TOP1(3), TOP2(3), TOP4(3)
        REAL(KIND=WP) :: BOT1(3), BOT2(3), BOT4(3)
        REAL(KIND=WP) :: UTOP(3), VTOP(3), UBOT(3), VBOT(3)
        REAL(KIND=WP) :: NTOP(3), NBOT(3), DENOM
        REAL(KIND=WP), PARAMETER :: ORIENT_TOL = 1.0E-12_WP

        TOP_MAG = ZERO
        BOT_MAG = ZERO
        TOP_BOT_DOT = ZERO
        TOP_BOT_COS = ZERO
        IERR = 0

        IF (I_ELEM + P_ELEM > MAX_CP_U .OR. J_ELEM + Q_ELEM > MAX_CP_V) THEN
          IERR = 1
          RETURN
        END IF

        CP_CORNER(1) = CP_MAP(I_ELEM,          J_ELEM)
        CP_CORNER(2) = CP_MAP(I_ELEM + P_ELEM, J_ELEM)
        CP_CORNER(3) = CP_MAP(I_ELEM + P_ELEM, J_ELEM + Q_ELEM)
        CP_CORNER(4) = CP_MAP(I_ELEM,          J_ELEM + Q_ELEM)

        IF (MINVAL(CP_CORNER) <= 0 .OR. MINVAL(NODES_BULK) <= 0) THEN
          IERR = 1
          RETURN
        END IF

        TOP1(1:3) = Q1NP_CPTAB(1:3, CP_CORNER(1))
        TOP2(1:3) = Q1NP_CPTAB(1:3, CP_CORNER(2))
        TOP4(1:3) = Q1NP_CPTAB(1:3, CP_CORNER(4))

        BOT1(1:3) = X(1:3, NODES_BULK(1))
        BOT2(1:3) = X(1:3, NODES_BULK(2))
        BOT4(1:3) = X(1:3, NODES_BULK(4))

        UTOP(1:3) = TOP2(1:3) - TOP1(1:3)
        VTOP(1:3) = TOP4(1:3) - TOP1(1:3)
        UBOT(1:3) = BOT2(1:3) - BOT1(1:3)
        VBOT(1:3) = BOT4(1:3) - BOT1(1:3)

        NTOP(1) = UTOP(2)*VTOP(3) - UTOP(3)*VTOP(2)
        NTOP(2) = UTOP(3)*VTOP(1) - UTOP(1)*VTOP(3)
        NTOP(3) = UTOP(1)*VTOP(2) - UTOP(2)*VTOP(1)

        NBOT(1) = UBOT(2)*VBOT(3) - UBOT(3)*VBOT(2)
        NBOT(2) = UBOT(3)*VBOT(1) - UBOT(1)*VBOT(3)
        NBOT(3) = UBOT(1)*VBOT(2) - UBOT(2)*VBOT(1)

        TOP_MAG = SQRT(MAX(DOT_PRODUCT(NTOP, NTOP), ZERO))
        BOT_MAG = SQRT(MAX(DOT_PRODUCT(NBOT, NBOT), ZERO))
        DENOM = TOP_MAG * BOT_MAG
        IF (DENOM <= ORIENT_TOL) THEN
          IERR = 2
          RETURN
        ENDIF

        TOP_BOT_DOT = DOT_PRODUCT(NTOP, NBOT)
        TOP_BOT_COS = TOP_BOT_DOT / DENOM
        TOP_BOT_COS = MIN(ONE, MAX(-ONE, TOP_BOT_COS))
        IF (TOP_BOT_DOT <= ORIENT_TOL * DENOM) THEN
          IERR = 3
          RETURN
        ENDIF
      END SUBROUTINE Q1NP_CHECK_ELEMENT_ORIENTATION
      
      end module genq1np_mod
