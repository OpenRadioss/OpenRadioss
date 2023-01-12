Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  WRITE_NLOC_STRUCT             source/restart/ddsplit/write_nloc_struct.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_NLOC_STRUCT(NLOC_DMG ,NUMNOD_L ,NODGLOB  ,NODLOC   ,
     .                             CEL      ,CEP      ,PROC     ,IXS      ,
     .                             IXC      ,IXTG     ,NUMELS_L ,NUMELC_L ,
     .                             NUMELTG_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN)    :: NUMNOD_L, CEL(*), CEP(*),
     .                           IXS(NIXS,*),PROC,IXC(NIXC,*),
     .                           IXTG(NIXTG,*),NUMELS_L,NUMELC_L,
     .                           NUMELTG_L
      INTEGER , DIMENSION(NUMNOD_L) , INTENT(IN) :: NODGLOB
      INTEGER , DIMENSION(NUMNOD)   , INTENT(IN) :: NODLOC
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILOC,NNOD,NNOD_L,NG,NL,NN, LNLOC_L,ND,NP,NM,N1,
     .        N2,NNO,CC,CC_L,NUMG,NUML,PROC_L,K,SHFT,TESTVAL,
     .        L_NLOC,NDDMAX_L,OFF,LENBIS,LCNENL_L,MATSIZE
      INTEGER, DIMENSION(NUMNOD_L)   :: INDX_L, NDDL, IDXI_L
      INTEGER, DIMENSION(NUMNOD_L+1) :: POSI
      my_real, DIMENSION(NLOC_DMG%L_NLOC) :: MASS,UNL,MASS0
      my_real, DIMENSION(:), ALLOCATABLE  :: ZERO_VEC
      INTEGER, DIMENSION(8) :: HEAD
      INTEGER, DIMENSION(:), ALLOCATABLE   :: ADDCNE_L,SOLTAG,SHTAG,TGTAG,
     .                                        PROCNE_L
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IADS,IADC,IADTG
C=======================================================================
      ! Flag for non-local computation 
      ILOC   = NLOC_DMG%IMOD
c
      ! Flag = 0, no non-local computation
      IF (ILOC == 0) THEN
        HEAD(1:8) = 0
        CALL WRITE_I_C(HEAD,8)
c
      ! Else, non-local computation
      ELSE
c
        ! Non-local global variables
        NNOD   = NLOC_DMG%NNOD
        L_NLOC = NLOC_DMG%L_NLOC
c        
        NNOD_L       = 0 ! Initialization of the number of non-local nodes (local)
        LNLOC_L      = 0 ! Local length of non-local vectors
c
        INDX_L(1:NUMNOD_L) = 0
        IDXI_L(1:NUMNOD_L) = 0
        NDDL(1:NUMNOD_L)   = 0
        POSI(1:NUMNOD_L+1) = 0
        MASS(1:NLOC_DMG%L_NLOC)  = ZERO
        MASS0(1:NLOC_DMG%L_NLOC) = ZERO
        UNL(1:NLOC_DMG%L_NLOC)   = ZERO
c
        ! Loop over local number of nodes
        DO NL = 1,NUMNOD_L
          NG = NODGLOB(NL)                                           ! Corresponding global node
          NN = NLOC_DMG%IDXI(NG)                                     ! Corresponding number of the non-local node
          IF (NN > 0) THEN                                           ! If the node is non-local
            NP = NLOC_DMG%POSI(NN)                                   !   Position of the first d.o.f of the node
            ND = NLOC_DMG%POSI(NN+1) - NP                            !   Number of additional d.o.fs
            NNOD_L         = NNOD_L + 1                              !   Counter of local non-local nodes
            INDX_L(NNOD_L) = NL                                      !   Local table INDX
            IDXI_L(NL)     = NNOD_L                                  !   Local table INDXI
            NDDL(NNOD_L)   = ND                                      !   Local table NDDL
            POSI(NNOD_L)   = LNLOC_L + 1                             !   Local table POSI
            MASS(LNLOC_L+1:LNLOC_L+ND)  = NLOC_DMG%MASS(NP:NP+ND-1)  !   Local table MASS
            MASS0(LNLOC_L+1:LNLOC_L+ND) = NLOC_DMG%MASS0(NP:NP+ND-1) !   Local table initial MASS0
            UNL(LNLOC_L+1:LNLOC_L+ND)   = NLOC_DMG%UNL(NP:NP+ND-1)   !   Local table UNL
            LNLOC_L = LNLOC_L + ND                                   !   Local size of the non-locals vectors (UNL,VNL,FNL...)
          ENDIF
        ENDDO
        POSI(NNOD_L + 1) = LNLOC_L + 1
c
        NDDMAX_L = MAXVAL(NDDL(1:NNOD_L))
c
        ! PARITH/ON
        IF (IPARI0 == 1) THEN 
c
          ! Length of the PROCNE_L table
          LCNENL_L = 0
          DO I = 1, NNOD_L
            NL       = INDX_L(I)                                   ! Number of the local node in the domain (all node NUMNOD_L)
            NG       = NODGLOB(NL)                                 ! Number of the corresponding global node
            NN       = NLOC_DMG%IDXI(NG)                           ! Number of the corresponding non-local nodes 
            N1       = NLOC_DMG%ADDCNE(NN)                         ! Number of the position in the FSKY vector
            N2       = NLOC_DMG%ADDCNE(NN+1)                       ! Number of the following position in the FSKY vector            
            LCNENL_L = LCNENL_L + N2-N1
          ENDDO  
c
          ! Allocation of the local ADDCNE_L table
          ALLOCATE(ADDCNE_L(NNOD_L + 1))
          ADDCNE_L(1:NNOD_L + 1) = 0
          ALLOCATE(PROCNE_L(LCNENL_L))
          PROCNE_L(1:LCNENL_L)   = 0
          ALLOCATE(IADS(8,NUMELS_L))
          IADS(1:8,1:NUMELS_L)   = 0
          ALLOCATE(IADC(4,NUMELC_L))
          IADC(1:4,1:NUMELC_L)   = 0
          ALLOCATE(IADTG(3,NUMELTG_L))
          IADTG(1:3,1:NUMELTG_L) = 0
          ALLOCATE(SOLTAG(NUMELS))
          SOLTAG(1:NUMELS)       = 0
          ALLOCATE(SHTAG(NUMELC))
          SHTAG(1:NUMELC)        = 0
          ALLOCATE(TGTAG(NUMELTG))
          TGTAG(1:NUMELTG)       = 0
c
          ! Filling the ADDCNE_L table
          ADDCNE_L(1) = 1
          CC_L        = 0 ! Counter of local element 
c
          ! Loop over non-local local nodes
          DO I = 1, NNOD_L
            NL = INDX_L(I)                                        ! Number of the local node in the domain (all node NUMNOD_L)
            NG = NODGLOB(NL)                                      ! Number of the corresponding global node
            NN = NLOC_DMG%IDXI(NG)                                ! Number of the corresponding non-local nodes 
            N1 = NLOC_DMG%ADDCNE(NN)                              ! Number of the position in the FSKY vector
            N2 = NLOC_DMG%ADDCNE(NN+1)                            ! Number of the following position in the FSKY vector
            ADDCNE_L(I+1) = ADDCNE_L(I) + N2-N1                   ! Filling the corresponding case of ADDCNE_L
            DO CC = N1,N2-1                                       ! Loop over attached element
              NUMG   = NLOC_DMG%CNE(CC)                           !   Corresponding global number of the element
              NUML   = CEL(NUMG)                                  !   Local number of the element
              PROC_L = CEP(NUMG)+1                                !   Processor of the element
              CC_L   = CC_L + 1                                   !   Local element counter
              PROCNE_L(CC_L) = PROC_L                             !   Processor on which the element is located                                           
              IF (PROC==PROC_L) THEN                            !   If the current proc equals the processor of the element, filling the IADX table
                IF (NUMG<=NUMELS) THEN                          !     If the element is solid
                  DO K = 1,8                                      !       Loop over the nodes of the brick
                    SHFT    = ISHFT(1,K-1)                        !         Shift
                    TESTVAL = IAND(SOLTAG(NUMG),SHFT)             !         Testval
                    IF (IXS(K+1,NUMG)==NG.AND.TESTVAL==0) THEN  !         Filling IADS
                      IADS(K,NUML) = CC_L
                      SOLTAG(NUMG) = SOLTAG(NUMG)+SHFT
                    ENDIF
                  ENDDO
                ELSEIF (NUMG<=NUMELS+NUMELQ) THEN 
                  ! This case should not occur
                  WRITE(*,*) "Error in non-local decomp"   
                  WRITE(*,*) "Quad element error"
                  STOP 
                ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC) THEN                  !   If the element is a shell
                  NUMG = NUMG - (NUMELS+NUMELQ)                             !     Offset on NUMG
                  DO K=1,4                                                  !     Loop over the nodes of the shell
                    SHFT    = ISHFT(1,K-1)                                  !       Shift
                    TESTVAL = IAND(SHTAG(NUMG),SHFT)                        !       Testval
                    ! Filling IADC
                    IF (IXC(K+1,NUMG)==NG.AND.TESTVAL==0) THEN
                      IADC(K,NUML) = CC_L
                      SHTAG(NUMG)  = SHTAG(NUMG)+SHFT
                    ENDIF
                  ENDDO
                ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT) THEN
                  ! This case should not occur
                  WRITE(*,*) "Error in non-local decomp"
                  WRITE(*,*) "Truss element error"
                  STOP
                ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP) THEN
                  ! This case should not occur
                  WRITE(*,*) "Error in non-local decomp"
                  WRITE(*,*) "Poutre element error"
                  STOP
                ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR) THEN
                  ! This case should not occur
                  WRITE(*,*) "Error in non-local decomp"
                  WRITE(*,*) "Ressort element error"
                  STOP
                ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+          !   If the element is a triangle shell
     .            NUMELR+NUMELTG) THEN
                  NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR) !     Offset on NUMG
                  DO K=1,3                                                  !     Loop over the nodes of the shell
                    SHFT    = ISHFT(1,K-1)                                  !       Shift
                    TESTVAL = IAND(TGTAG(NUMG),SHFT)                        !       Testval
                    IF (IXTG(K+1,NUMG)==NG.AND.TESTVAL==0) THEN           !       Filling IADTG 
                      IADTG(K,NUML) = CC_L
                      TGTAG(NUMG)   = TGTAG(NUMG)+SHFT
                    ENDIF
                  ENDDO
                ELSE
                  ! This case should not occur
                  WRITE(*,*) "Error in non-local decomp"
                  STOP
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
c
        HEAD(1) = ILOC
        HEAD(2) = NNOD_L 
        HEAD(3) = LNLOC_L
        HEAD(4) = NUMELS_L
        HEAD(5) = NUMELC_L
        HEAD(6) = NUMELTG_L
        HEAD(7) = NDDMAX_L
        HEAD(8) = LCNENL_L
C
        IF (NSUBDOM > 0) THEN
C--       multidomains - origianl nummat is used
          MATSIZE = NUMMAT0 
        ELSE
          MATSIZE = NUMMAT 
        ENDIF
C
        CALL WRITE_I_C(HEAD,8)
c
        CALL WRITE_DB(NLOC_DMG%DENS,MATSIZE)         ! DENS
c
        CALL WRITE_DB(NLOC_DMG%DAMP,MATSIZE)         ! DAMP
c
        CALL WRITE_DB(NLOC_DMG%LEN,MATSIZE)          ! LEN
c
        CALL WRITE_DB(NLOC_DMG%LE_MAX,MATSIZE)       ! LEN
c
        CALL WRITE_DB(NLOC_DMG%SSPNL,MATSIZE)        ! SSPNL
c   
        CALL WRITE_I_C(INDX_L,NNOD_L)               ! INDX_L(NNOD_L)
c  
        CALL WRITE_I_C(POSI,NNOD_L+1)               ! POSI(NNOD_L+1)
c        
        CALL WRITE_I_C(IDXI_L,NUMNOD_L)             ! IDXI_L(NUMNOD_L)
c
        ! If PARITH/ON
        IF (IPARI0 == 1) THEN
c
          CALL WRITE_I_C(ADDCNE_L,NNOD_L+1)         ! ADDCNE_L(NNOD_L+1)
c
          CALL WRITE_I_C(PROCNE_L,LCNENL_L)         ! PROCNE_L(LCNENL_L)
c
          CALL WRITE_I_C(IADS,8*NUMELS_L)           ! IADS(8,NUMELS_L)
c
          CALL WRITE_I_C(IADC,4*NUMELC_L)           ! IADC(4,NUMELC_L)
c
          CALL WRITE_I_C(IADTG,3*NUMELTG_L)         ! IADTG(3,NUMELTG_L)
c
        ENDIF
c
        CALL WRITE_DB(MASS,LNLOC_L)                 ! MASS
c
        CALL WRITE_DB(MASS0,LNLOC_L)                ! MASS0
c
        IF (.NOT.ALLOCATED(ZERO_VEC)) ALLOCATE(ZERO_VEC(4*LNLOC_L))
        ZERO_VEC(1:4*LNLOC_L) = ZERO
        CALL WRITE_DB(ZERO_VEC,4*LNLOC_L)           ! FNL (ZERO), VNL (ZERO), VNL_OLD (ZERO), DNL (ZERO)
c
        CALL WRITE_DB(UNL,LNLOC_L)                  ! UNL
c
        ! Deallocation of tables
        IF (ALLOCATED(SOLTAG))   DEALLOCATE(SOLTAG)
        IF (ALLOCATED(SHTAG))    DEALLOCATE(SHTAG)
        IF (ALLOCATED(TGTAG))    DEALLOCATE(TGTAG)
        IF (ALLOCATED(ADDCNE_L)) DEALLOCATE(ADDCNE_L)
        IF (ALLOCATED(PROCNE_L)) DEALLOCATE(PROCNE_L)
        IF (ALLOCATED(IADS))     DEALLOCATE(IADS)
        IF (ALLOCATED(IADC))     DEALLOCATE(IADC)
        IF (ALLOCATED(IADTG))    DEALLOCATE(IADTG)
        IF (ALLOCATED(ZERO_VEC)) DEALLOCATE(ZERO_VEC)
c
      ENDIF
c--------------------------------
      RETURN
      END
