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
!||    i22trivox        ../engine/source/interfaces/intsort/i22trivox.F
!||--- called by ------------------------------------------------------
!||    i22buce          ../engine/source/interfaces/intsort/i22buce.F
!||--- calls      -----------------------------------------------------
!||    i22sto           ../engine/source/interfaces/intsort/i22sto.F
!||    ireallocate      ../engine/share/modules/realloc_mod.F
!||    isonsh3n         ../engine/source/interfaces/int22/i22intersect.F
!||    my_barrier       ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    element_mod      ../common_source/modules/elements/element_mod.F90
!||    i22bufbric_mod   ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22edge_mod      ../common_source/modules/interfaces/cut-cell-buffer_mod.F
!||    i22tri_mod       ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    realloc_mod      ../engine/share/modules/realloc_mod.F
!||====================================================================
      SUBROUTINE I22TRIVOX(
     1      NSN    ,RENUM  ,NSHELR_L,ISZNSNR  ,I_MEM    ,
     2      IRECT  ,X      ,STF     ,STFN     ,BMINMA   ,
     3      NSV    ,II_STOK,CAND_B  ,ESHIFT   ,CAND_E   ,
     4      MULNSN ,NOINT  ,TZINF   ,
     5      VOXEL  ,NBX    ,NBY     ,NBZ      ,
     6      CAND_P   ,
     7      NSHEL_T,
     8      MARGE    ,
     9      NIN    ,ITASK  ,IXS      ,BUFBRIC  ,
     A      NBRIC  ,ITAB   ,NSHEL_L)
C============================================================================
C   P r e c o n d i t i o n s
C-----------------------------------------------
C   VOXEL(*) : initialise a 0
C   I_MEM : 0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
      USE I22EDGE_MOD
      USE REALLOC_MOD
      USE I22BUFBRIC_MOD
      use element_mod , only : nixs
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   role of the routine:
C   ===================
C   RECHERCHE DE CANDIDATES AU CALCULATION D'INTERSECTION : (BRIQUE, FACETTE)
C
C STEP DESCRIPTIONS
C=======================================================================
C 0   DATA PRE-TREATMENT
C=======================================================================
C=======================================================================
C 1   VOXEL FILLING
C=======================================================================
C=======================================================================
C 2   CANDIDATE SEARCHING
C=======================================================================
C=======================================================================
C 3   ...
C=======================================================================

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     IRECT(4,*)     : ARRAY OF CONEC FACETTES        E
C     X(3,*)         : COORDONNEES NODALES               E
C     NSV            : NOS SYSTEMES DES NODES E
C     Xmax: larger abcisse existing e
C     YMAX: greater order.existing E
C     Zmax: larger existing side E
C     I_STOK         : storage level of pairs
C                                CANDIDATES impact    E/S
C     CAND_B         : boites resultats bricks
C     Cand_e: Addresses of the Facets result boxes
C                      MULNSN = MULTIMP*NSN maximum size allowed now for the
C                      COUPLES NODES,ELT CANDIDATES
C     NOINT          : INTERFACE USER NUMBER
C     TZINF          : TAILLE ZONE INFLUENCE
C
C     VOXEL(ix,iy,iz): contains the address of the first link in the chain array for the concerned voxel.
C     LCHAIN_LAST     : contains the address of the last link in the chain array for the concerned voxel.
C     LCHAIN_NEXT (*): (*, 1) ID ENTITE, (*, 2) Next address.
C     LCHAIN_ELEM(*)  : storage of brick IDs for each voxel (requires start address via VOXEL(ix,iy,iz))
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,ESHIFT,NSN,ISZNSNR,NSHEL_T,NIN,ITASK,
     .        MULNSN,NOINT,NSHELR_L,IGAP,NBX,NBY,NBZ,NBRIC,
     .        NSV(*),CAND_B(*),CAND_E(*),RENUM(*),
     .        IRECT(4,*), IXS(NIXS,*),
     .        BUFBRIC(NBRIC),
     .        VOXEL(NBX+2,NBY+2,NBZ+2),ITAB(*),NSHEL_L,II_STOK

      my_real
     .    ,TARGET :: X(3,*)

      my_real
     .   BMINMA(6),CAND_P(*), STF(*),STFN(*),
     .   TZINF,MARGE

      my_real, DIMENSION(SIZ_XREM, NSHEL_T+1: NSHEL_T+NSHELR_L) ::
     .   XREM

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,K,L,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,NS,NCAND_PROV,J_STOK,II,JJ,TT,
     .        OLDNUM(ISZNSNR), NSNF, NSNL,
     .        PROV_B(2*MVSIZ), PROV_E(2*MVSIZ), LAST_NE,
     .        VOXBND(2*MVSIZ,0:1,1:3)                           !voxel bounds storage for shell: comp1=id,  comp2=lbound/ubound, comp3=direction.

      my_real
     .   DX,DY,DZ,XS,YS,ZS,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS, POINT(3),
     .   ON1(3),N1N2(3)

      INTEGER  IX,IY,IZ,NEXT,M1,M2,M3,M4,M5,M6,M7,M8,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2,IBUG,IBUG2,I_LOC,
     .         BIX1(NBRIC),BIY1(NBRIC),BIZ1(NBRIC),
     .         BIX2(NBRIC),BIY2(NBRIC),BIZ2(NBRIC),
     .         FIRST_ADD, PREV_ADD, LCHAIN_ADD, I_STOK

      INTEGER :: NC, I_STOK_BAK, IPA,IPB
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   DXB,DYB,DZB,
     .   AAA, DAAA, DMAX

      LOGICAL, DIMENSION(NBRIC) :: TAGB
!      Logical, dimension (12*nbric) :: ledge
      LOGICAL :: BOOL(NIRECT_L)
      INTEGER NBCUT, DEJA, ISONSHELL, ISONSH3N
      INTEGER :: COUNTER, NEDGE, NFACE, NODES8(8), COUNTER_BRICK(NBRIC)

c      INTEGER, DIMENSION(2,24)  :: iEDGE  !12 without diagonals, 24 with diagonals
c      INTEGER, DIMENSION(2,2,6) :: iFACE
      INTEGER :: iN1, iN2, iN1a, iN2a, iN1b, iN2b , iN3, iN4
      INTEGER :: POS, IAD, IB     , NBF, NBL
      INTEGER :: I_12bits, nbits, npqts, pqts(4), SUM, SECTION
      INTEGER :: I_bits(12), MAX_ADD, IMIN_LOC, IMAX_LOC

      my_real ::
     .           AERADIAG,XX(8),YY(8),ZZ(8),DIAG(4)

      CHARACTER*12 :: sectype
      LOGICAL      :: IsSecDouble, IsSTO

      CHARACTER(LEN=1) filenum

      INTEGER ::
     .        MIN_IX_LOC, MIN_IY_LOC, MIN_IZ_LOC,            !indice voxel min utilise
     .        MAX_IX_LOC, MAX_IY_LOC, MAX_IZ_LOC             !Voxel Max index uses

      INTEGER, ALLOCATABLE, DIMENSION(:) :: order, VALUE

      INTEGER R2,MIN2





C-----------------------------------------------
C=======================================================================
C -1  INITIALIZATION
C=======================================================================

!-----------debug---------------
      IF(ibug22_trivox==1 .AND. ITASK==0)THEN
        print *, "  i22trivox:entering routine"
        print *, ""
        print *, "------------------BRICKS DOMAIN--------------------"
        print *, "  BMINMAL_I22TRIVOX=", BMINMA(4:6),BMINMA(1:3)
        print *, "  NBX,NBY,NBZ=", NBX,NBY,NBZ
        print *, "---------------------------------------------------"
        print *, ""
        print *, ""
        print *, "  |-----------i22trivox.F---------|"
        print *, "  |       DOMAIN INFORMATION      |"
        print *, "  |-------------------------------|"
        print *, "  MPI      =",ISPMD +1
        print *, "   NT      =",ITASK+1
        print *, "  NCYCLE   =", NCYCLE
        print *, "  ITASK    =", ITASK
        print *, "  NIRECT_L =", NIRECT_L
        print *, "  local bricks   :", NBRIC
        print *, "  tableau briques du domaine local :"
        print *, IXS(11,BUFBRIC(1:NBRIC))
        print *, "  local faces   :",NSHEL_L
        print *, "  tableau facettes du domaine local :"
        DO I=1, NIRECT_L-NSHELR_L
          print *, I,NINT(IRECT_L(1:4, I))
        END DO
        print *, "  +remotes:"
        DO I=NIRECT_L-NSHELR_L+1, NIRECT_L
          print *, I,IRECT_L(1:4, I)
        END DO
        print *, "  |-------------------------------|"
        print *, ""
        print *, "    |-----i22trivox.F--------|"
        print *, "    |   THREAD INFORMATION   |"
        print *, "    |------------------------|"
!        print *, "    THREAD/NTHREAD=",ITASK+1,NTHREAD
        print *, "    cple candidats max : ", MULNSN
        print *, "    ESHIFT=", ESHIFT
        print *, "    |------------------------|"
        print *, ""
      end if
      CALL MY_BARRIER
!-----------debug---------------

C=======================================================================
C 0   DATA PRE-TREATMENT
C=======================================================================

      MAX_ADD = MULNSN  !12*NIRECT_L ! a optimiser eventuellmeent
      AAA     = ZERO

      !---------------------------------------------------------!
      ! Dynamic Allocations                                     !
      !---------------------------------------------------------!
      !---------------------------------------------------------!
      ! + Storing Min/Max for coordinates and voxel indexes     !
      !---------------------------------------------------------!
      IF(ITASK == 0)THEN
        !Possible reallocate for Lchain _*() ()
        NULLIFY (LCHAIN_LAST, LCHAIN_NEXT, LCHAIN_ELEM)
        ALLOCATE(LCHAIN_ELEM(MAX_ADD))
        ALLOCATE(LCHAIN_NEXT(MAX_ADD))
        ALLOCATE(LCHAIN_LAST(MAX_ADD))
        MIN_IX = NBX+2
        MIN_IY = NBY+2
        MIN_IZ = NBZ+2
        MAX_IX = 0
        MAX_IY = 0
        MAX_IZ = 0
        CURRENT_ADD = 1 ! first address in the common chain array
      END IF
      IF(ITASK==NTHREAD-1)THEN
        ALLOCATE(EIX1(NIRECT_L),EIY1(NIRECT_L),EIZ1(NIRECT_L))
        ALLOCATE(EIX2(NIRECT_L),EIY2(NIRECT_L),EIZ2(NIRECT_L))
        EIX1=NBX+2  !a initialiser car si pas de candidat et ftrapuv alors min/max globaux erratics
        EIY1=NBX+2  !set to nthread -1
        EIZ1=NBX+2
        EIX2=0
        EIY2=0
        EIZ2=0
      END IF

      CALL MY_BARRIER ! All thread have to wait for common initialization.

      !---------------------------------------------------------!
      ! Domain bounds reading                                   !
      !---------------------------------------------------------!
      !field terminal: intersection between
      !  domaine fluide local
      !  Global lag domain
      XMINB = BMINMA(4)
      YMINB = BMINMA(5)
      ZMINB = BMINMA(6)
      XMAXB = BMINMA(1)
      YMAXB = BMINMA(2)
      ZMAXB = BMINMA(3)
      AAA   = TZINF !MARGE TO EXTEND SEARCH. MUST BE LARGE ENOUGH TO INCLUDE ADJACENT UNCUT CELLS
      !already done in i22main_tri for lagrangian domains
      XMINB = XMINB - AAA
      YMINB = YMINB - AAA
      ZMINB = ZMINB - AAA
      XMAXB = XMAXB + AAA
      YMAXB = YMAXB + AAA
      ZMAXB = ZMAXB + AAA

      DXB   = XMAXB-XMINB
      DYB   = YMAXB-YMINB
      DZB   = ZMAXB-ZMINB

      !If AAA=0 then voxel domain can be degenerated. Example : 1shell in plane XY => DZB=0
      DAAA =  ( (BMINMA(1)-BMINMA(4))+(BMINMA(2)-BMINMA(5))+
     .          (BMINMA(3)-BMINMA(6)) ) / THREE/HUNDRED
      DMAX = MAX(MAX(DXB,DYB),DZB)

      IF(DXB/DMAX<EM06)DXB=DAAA
      IF(DYB/DMAX<EM06)DYB=DAAA
      IF(DZB/DMAX<EM06)DZB=DAAA

      !we partition the sweep of the global shell node array (IRECT_L(1:NIRECT_L)) over different threads (multithreading)
      NBF = 1+ITASK*NIRECT_L/NTHREAD
      NBL = (ITASK+1)*NIRECT_L/NTHREAD

c      if(itask==0.and.ibug22_trivox==1)print *,
c     ."Voxel filling with the following Shell:"

      DO NE=NBF,NBL
        IF(IRECT_L(23,NE)==ZERO)CYCLE
        IF(((XMAXE(NE)< XMINB).OR.(XMINE(NE)>XMAXB)).OR.
     .     ((YMAXE(NE)< YMINB).OR.(YMINE(NE)>YMAXB)).OR.
     .     ((ZMAXE(NE)< ZMINB).OR.(ZMINE(NE)>ZMAXB)))THEN
          IRECT_L(23,NE)=ZERO
          !print *, "skip shell=", NE
          CYCLE
        END IF
        !-------------------------------------------!
        !  VOXEL OCCUPIED BY THE BRICK              !
        !-------------------------------------------!
        !Voxel_lower_left_bound for this element---+
        IX1=INT(NBX*(IRECT_L(17,NE)-AAA-XMINB)/DXB)
        IY1=INT(NBY*(IRECT_L(18,NE)-AAA-YMINB)/DYB)
        IZ1=INT(NBZ*(IRECT_L(19,NE)-AAA-ZMINB)/DZB)
        EIX1(NE)=MAX(1,2+MIN(NBX,IX1))
        EIY1(NE)=MAX(1,2+MIN(NBY,IY1))
        EIZ1(NE)=MAX(1,2+MIN(NBZ,IZ1))
        !Voxel_upper_right_bound for this element---+
        IX2=INT(NBX*(IRECT_L(20,NE)+AAA-XMINB)/DXB)
        IY2=INT(NBY*(IRECT_L(21,NE)+AAA-YMINB)/DYB)
        IZ2=INT(NBZ*(IRECT_L(22,NE)+AAA-ZMINB)/DZB)
        EIX2(NE)=MAX(1,2+MIN(NBX,IX2))
        EIY2(NE)=MAX(1,2+MIN(NBY,IY2))
        EIZ2(NE)=MAX(1,2+MIN(NBZ,IZ2))
      END DO
      !-------------------------------------------!
      !  VOXEL INDEX RANGE FOR VOXEL RESETTING     !
      !-------------------------------------------!
      !for voxel reset
      MIN_IX_LOC = MIN(MIN_IX,MINVAL(EIX1(NBF:NBL)))
      MIN_IY_LOC = MIN(MIN_IY,MINVAL(EIY1(NBF:NBL)))
      MIN_IZ_LOC = MIN(MIN_IZ,MINVAL(EIZ1(NBF:NBL)))
      MAX_IX_LOC = MAX(MAX_IX,MAXVAL(EIX2(NBF:NBL)))
      MAX_IY_LOC = MAX(MAX_IY,MAXVAL(EIY2(NBF:NBL)))
      MAX_IZ_LOC = MAX(MAX_IZ,MAXVAL(EIZ2(NBF:NBL)))
      !----------------------------------------------!
      !   GLOBAL MIN/MAX VOXEL INDEX RANGE FOR RESET !
      !----------------------------------------------!
#include "lockon.inc"
      MIN_IX = MIN (MIN_IX_LOC,MIN_IX)
      MIN_IY = MIN (MIN_IY_LOC,MIN_IY)
      MIN_IZ = MIN (MIN_IZ_LOC,MIN_IZ)
      MAX_IX = MAX (MAX_IX_LOC,MAX_IX)
      MAX_IY = MAX (MAX_IY_LOC,MAX_IY)
      MAX_IZ = MAX (MAX_IZ_LOC,MAX_IZ)
#include "lockoff.inc"
      CALL MY_BARRIER ! waiting for EIX1, ...,EIZ2

      !optimization: if no candidate: the default values of min max voxel indices for this thread are constraining for reinitialization.

C=======================================================================
C 1   VOXEL FILLING with faces data below
C=======================================================================
      !----------------------------------------------!
      ! SHELL STORAGE FOR EACH VOXEL (CHAINED ARRAY) !
      !----------------------------------------------!
C
C        VOXEL(*,*,*) LCHAIN_LAST(FIRST)
C        +-----------+------------+
C        |  FIRST    |   LAST     |
C        +--+--------+--+---------+
C           |           |
C           |           |
C           |           |
C           |           |   LCHAIN_ELEM(*) LCHAIN_NEXT(*)
C           |           |   +------------+-----------+
C           +-------------->| elemid     |   iadd 3  |  1:FIRST --+
C                       |   +------------+-----------+            |
C                       |   |            |           |  2         |
C                       |   +------------+-----------+            |
C                       |   | elemid     |   iadd 4  |  3 <-------+
C                       |   +------------+-----------+            |
C                       |   | elemid     |   iadd 6  |  4 <-------+
C                       |   +------------+-----------+            |
C                       |   |            |           |  5         |
C                       |   +------------+-----------+            |
C                       +-->| elemid     |   0       |  6:LAST <--+
C                           +------------+-----------+
C                           |            |           |  7
C                           +------------+-----------+


      !----------------------------------------------!
      !              VOXEL FILLING                   !
      !----------------------------------------------!
      IF(ITASK==0)THEN
        DO NE=1,NIRECT_L
          IF(IRECT_L(23,NE)==ZERO)CYCLE !stiffness
!--------------debug
          if(itask==0.and.ibug22_trivox==1)then
            print *, "  traitement shell",NINT(IRECT_L((/1,3/),NE)),
     .      "indices",EIX1(NE),EIX2(NE), EIY1(NE),EIY2(NE),EIZ1(NE),EIZ2(NE)
            print *, "           xmin/xmax=", IRECT_L((/17,20/),NE)
            print *, "           ymin/ymax=", IRECT_L((/18,21/),NE)
            print *, "           zmin/zmax=", IRECT_L((/19,22/),NE)
          end if
!--------------debug
          DO IZ = EIZ1(NE),EIZ2(NE)
            DO IY = EIY1(NE),EIY2(NE)
              DO IX = EIX1(NE),EIX2(NE)
                FIRST_ADD = VOXEL(IX,IY,IZ)
                IF(FIRST_ADD == 0)THEN
                  !Empty Cell
                  VOXEL(IX,IY,IZ)          = CURRENT_ADD           ! address in the chain array
                  LCHAIN_LAST(CURRENT_ADD) = CURRENT_ADD           ! last = current
                  LCHAIN_ELEM(CURRENT_ADD) = NE                    ! coque ID
                  LCHAIN_NEXT(CURRENT_ADD) = 0                     ! no next because last in the list   !
                ELSE
                  !Box containing several elements, Jump to the Last Node of the Cell
                  PREV_ADD                 = LCHAIN_LAST(FIRST_ADD)! becomes the penultimate
                  LCHAIN_LAST(FIRST_ADD)   = CURRENT_ADD           ! update of the last
                  LCHAIN_ELEM(CURRENT_ADD) = NE                    ! coque ID
                  LCHAIN_NEXT(PREV_ADD)    = CURRENT_ADD           ! maj du suivant 0 -> CURRENT_ADD
                  LCHAIN_NEXT(CURRENT_ADD) = 0                     ! no next because last in the list
                ENDIF
                CURRENT_ADD = CURRENT_ADD+1
                IF( CURRENT_ADD>=MAX_ADD)THEN
                  !Optimization: DEALLOCATE/GOTO DEBUT SUPRRESION.
                  !REALLOCATE SI PAS ASSEZ DE PLACE : inutile de recommencer de 1 a MAX_ADD-1, on poursuit de MAX_ADD a 2*MAX_ADD
                  MAX_ADD = 2 * MAX_ADD
                  if(ibug22_trivox==1)print *, "reallocate"
                  LCHAIN_NEXT => IREALLOCATE(LCHAIN_NEXT, MAX_ADD)
                  LCHAIN_ELEM => IREALLOCATE(LCHAIN_ELEM, MAX_ADD)
                  LCHAIN_LAST => IREALLOCATE(LCHAIN_LAST, MAX_ADD)
                ENDIF
              ENDDO !IX
            ENDDO !IY
          ENDDO !IZ
        END DO !I=1,NIRECT_L
      END IF
      CALL MY_BARRIER

!------ post ---- debug
      IF(ITASK==0.and.ibug22_trivox==1)
     .print *, "  i22trivox:voxel filled"
!------ post ---- debug

C=======================================================================
C 2   from the voxels occupied by a brick, we are able
C     to know all the shells in its neighborhood.
C     On creer alors les couples CANDIDATES.
C=======================================================================
      NC      = 0
      I_STOK  = 0
      LAST_NE = 0
      NBF =   1+ITASK*NBRIC/NTHREAD
      NBL = (ITASK+1)*NBRIC/NTHREAD

      DO I=NBF,NBL !1,NBRIC

c        if(ibug22_trivox==1)print *,
c     .     "  i22trivox : BOUCLE BRIQUE, I=",IXS(11,BUFBRIC(I))
        !-------------------------------------------!
        !  VOXEL OCCUPIED BY THE BRICK              !
        !-------------------------------------------!
        !Voxel_lower_left_bound for this element---+
        IX1=INT(NBX*(XMINS(I)-XMINB)/DXB)
        IY1=INT(NBY*(YMINS(I)-YMINB)/DYB)
        IZ1=INT(NBZ*(ZMINS(I)-ZMINB)/DZB)
        BIX1(I)=MAX(1,2+MIN(NBX,IX1))
        BIY1(I)=MAX(1,2+MIN(NBY,IY1))
        BIZ1(I)=MAX(1,2+MIN(NBZ,IZ1))
        !Voxel_upper_right_bound for this element---+
        IX2=INT(NBX*(XMAXS(I)-XMINB)/DXB)
        IY2=INT(NBY*(YMAXS(I)-YMINB)/DYB)
        IZ2=INT(NBZ*(ZMAXS(I)-ZMINB)/DZB)
        BIX2(I)=MAX(1,2+MIN(NBX,IX2))
        BIY2(I)=MAX(1,2+MIN(NBY,IY2))
        BIZ2(I)=MAX(1,2+MIN(NBZ,IZ2))


        !-------------------------------------------!
        !  NEIGHBORS SEARCH                         !
        !-------------------------------------------!
        ! a brick can occupy several voxels, by looking in the voxels
        ! Occupied we can therefore find the same facet several times.We avoid repeat
        !with a BOOL(I) tag.
        DO IZ = BIZ1(I),BIZ2(I)
          DO IY = BIY1(I),BIY2(I)
            DO IX = BIX1(I),BIX2(I)
              LCHAIN_ADD = VOXEL(IX,IY,IZ)
              DO WHILE(LCHAIN_ADD /= 0)             ! loop on shells of current voxel
                NE = LCHAIN_ELEM(LCHAIN_ADD)        ! ID COQUE DU VOXEL COURANT
                BOOL(NE)=.FALSE.
                LCHAIN_ADD = LCHAIN_NEXT(LCHAIN_ADD)
              ENDDO ! WHILE(LCHAIN_ADD /= 0)        ! BOOL(I)=true indique que l'id coque a deja ete traite pour la brique courante
            ENDDO !nbz
          ENDDO  !nby
        ENDDO   !nbx

        IsSTO = .FALSE.                     ! if I22sto is called then we switch to true. this is the signal to execute lockoff and allow processing of another brick on other threads

        DO IZ = BIZ1(I),BIZ2(I)
          DO IY = BIY1(I),BIY2(I)
            DO IX = BIX1(I),BIX2(I)
              LCHAIN_ADD = VOXEL(IX,IY,IZ)        ! address of the ID of the first brick in the voxel
              DO WHILE(LCHAIN_ADD /= 0)           ! loop on bricks of current voxel
                NE = LCHAIN_ELEM(LCHAIN_ADD)      ! ID BRICK DU VOXEL COURANT
                ! non-intersection criterion
                ! the two cartesian bounding volumes are disjoint.
                IF(BOOL(NE))THEN
                  LCHAIN_ADD = LCHAIN_NEXT(LCHAIN_ADD)
                  CYCLE
                END IF
                J = NE
                NS      = BUFBRIC(I)
                XX(1:8) = X(1,IXS(2:9,NS))
                YY(1:8) = X(2,IXS(2:9,NS))
                ZZ(1:8) = X(3,IXS(2:9,NS))
                DIAG(1) = SQRT((XX(1)-XX(7))**2 + (YY(1)-YY(7))**2 + (ZZ(1)-ZZ(7))**2)
                DIAG(2) = SQRT((XX(3)-XX(5))**2 + (YY(3)-YY(5))**2 + (ZZ(3)-ZZ(5))**2)
                DIAG(3) = SQRT((XX(2)-XX(8))**2 + (YY(2)-YY(8))**2 + (ZZ(2)-ZZ(8))**2)
                DIAG(4) = SQRT((XX(4)-XX(6))**2 + (YY(4)-YY(6))**2 + (ZZ(4)-ZZ(6))**2)
                AAA     = 1.2D00*MAXVAL(DIAG(1:4),1)

                ! we ignore the element if intersection with brick is null: margin ensures taking neighboring bricks for cut cell buffer extension
                IF( (IRECT_L(17,NE)-AAA>XMAXS(I)).OR.    !XMINE-AAA > XMAXS
     .              (IRECT_L(20,NE)+AAA<XMINS(I)).OR.    !XMAXE+AAA < XMINS
     .              (IRECT_L(18,NE)-AAA>YMAXS(I)).OR.    !YMINE-AAA > YMAXS                !Optimization : +/-AAA deja calculee, stoquer et reprendre (gain 6 operations par iteration)
     .              (IRECT_L(21,NE)+AAA<YMINS(I)).OR.    !YMAXE+AAA < YMINS
     .              (IRECT_L(19,NE)-AAA>ZMAXS(I)).OR.    !ZMINE-AAA > ZMAXS
     .              (IRECT_L(22,NE)+AAA<ZMINS(I)) ) THEN !ZMAXE+AAA < ZMINS
                  LCHAIN_ADD = LCHAIN_NEXT(LCHAIN_ADD)
                  CYCLE
                END IF
                BOOL(NE)       =.TRUE.  ! from here we consider that the shell has already been processed with the current brick. if we find it in another voxel we will not consider the pair a second time.
                I_STOK         = I_STOK + 1
                PROV_B(I_STOK) = I  !brique
                PROV_E(I_STOK) = NE !facette
                LCHAIN_ADD     = LCHAIN_NEXT(LCHAIN_ADD)
                TAGB(I)        = .TRUE.
                !SI SANS MARGE, INTERSECTION NULLE, ALORS SKIP
                IF( (IRECT_L(17,NE) >XMAXS(I)).OR.
     .              (IRECT_L(20,NE) <XMINS(I)).OR.
     .              (IRECT_L(18,NE) >YMAXS(I)).OR.
     .              (IRECT_L(21,NE) <YMINS(I)).OR.
     .              (IRECT_L(19,NE) >ZMAXS(I)).OR.
     .              (IRECT_L(22,NE) <ZMINS(I)) ) PROV_E(I_STOK) = -PROV_E(I_STOK)    !intersection nulle
                !-------------------------------------------!
                !  COUPLE STORAGE  (siz=mvsiz)              !
                !-------------------------------------------!
                IF(I_STOK>=NVSIZ)THEN
c              if(ibug22_trivox==1)print *,
c     .             "  i22trivox.F:purge des CANDIDATES prov",
c     .             II_STOK+I_STOK, "CORE=",ITASK+1, "BRIQUE=",
c     .             IXS(11,BUFBRIC(I))
                  CALL I22STO(
     1                        I_STOK ,IRECT  ,X       , II_STOK, CAND_B,
     2                        CAND_E ,MULNSN ,NOINT   , MARGE  , I_MEM ,
     3                        PROV_B ,PROV_E ,ESHIFT  , ITASK  , NC    ,
     4                        IXS    ,BUFBRIC ,NBRIC  , IsSTO   )
                  I_STOK = 0
                  IF(I_MEM==2) THEN
                    if(ibug22_trivox==1)then
                      print *, "  i22trivox.F:too much candidates on thread=",
     .                           ITASK+1
                      print *, "  i22trivox.F:II_STOK=", II_STOK,MULNSN
                    end if
                    GOTO 1000
                  END IF!(I_MEM==2)
                ENDIF!(I_STOK>=NVSIZ)
                !-------------------------------------------!
              ENDDO ! WHILE(LCHAIN_ADD /= 0)
            ENDDO !nbz
          ENDDO !nby
        ENDDO !nbz
        !-------------------------------------------!
        !  COUPLE STORAGE  (siz<mvsiz)              !
        !-------------------------------------------!
        IF(I_STOK/=0)THEN
c          if(ibug22_trivox==1)print *, "  i22trivox.F:purge<MVSIZ",
c     .             II_STOK+I_STOK, "CORE=",ITASK+1, "BRIQUE=",I
          CALL I22STO(
     1                      I_STOK ,IRECT   ,X      , II_STOK ,CAND_B,
     2                      CAND_E ,MULNSN  ,NOINT  , MARGE   ,I_MEM ,
     3                      PROV_B ,PROV_E  ,ESHIFT , ITASK   ,NC    ,
     4                      IXS    ,BUFBRIC ,NBRIC  , IsSTO )
          I_STOK = 0
          IF(I_MEM==2) THEN
c            if(ibug22_trivox==1)then
c              print *, "  i22trivox.F:too much candidates on thread=",
c     .                  ITASK+1
c              print *, "  i22trivox.F:II_STOK=", II_STOK,MULNSN
c            end if
            GOTO 1000
          END IF!(I_MEM==2)
        END IF
        !-------------------------------------------!
        ! UNLOCK NEEDED IF CURRENT BRICK WAS USED   !
        ! FOR STORAGE COUPLE                        !
        !-------------------------------------------!
        IF(IsSTO)THEN
c          if(ibug22_trivox==1)print *, "  i22trivox.F:lockoff", ITASK,
c     .                     "bric:",    I, "IsSTO=", IsSTO
#include "lockoff.inc"
        !this allows having connectivity in the array (/CAND_B(I),CAND_E(I)/) of pairs for a given brick. (for multi-threading of subsequent intersection calculations)
        END IF
        !-------------------------------------------!
      END DO !next I (1,NBRIC)

C-------------------------------------------------------------------------
C     end of search
C-------------------------------------------------------------------------


C=======================================================================
C 3   VOXEL RESET
C=======================================================================
 1000 CONTINUE

      CALL MY_BARRIER ! All threads need to finish its work with common Voxel before resetting it.

      if(itask==0.AND.ibug22_trivox==1) print *,
     .    "  i22trivox.F:nb de candidats:" , II_STOK, ITASK

      IF(ITASK==0)THEN
        !RESET VOXEL WITHIN USED RANGE ONLY
        DO K= MIN_IZ , MAX_IZ
          DO J= MIN_IY,MAX_IY
            DO I= MIN_IX,MAX_IX
              VOXEL(I,J,K) = 0
            END DO
          END DO
        END DO
      ENDIF



      !-------------------------------------------!
      !  DEALLOCATE                               !
      !-------------------------------------------!
      IF(ITASK == 0)THEN
        DEALLOCATE(LCHAIN_LAST, LCHAIN_NEXT, LCHAIN_ELEM )
        DEALLOCATE(EIX1, EIY1, EIZ1, EIX2, EIY2, EIZ2)
        NULLIFY (LCHAIN_LAST, LCHAIN_NEXT, LCHAIN_ELEM)
      ENDIF

!------ post ---- debug
      if(ibug22_trivox==1)CALL MY_BARRIER !(all threads must reinit before testing)
      if(itask==0.AND.ibug22_trivox==1)then
        DO ix=1,(nbx+2)
          DO iy=1,(nby+2)
            DO iz=1,(nbz+2)
              if (VOXEL(ix,iy,iz)/=0) then
                print *, "  i22trivox.F:error raz voxel",VOXEL(ix,iy,iz)
                print *, "  i22trivox.F:ix,iy,iz=", ix,iy,iz
                stop
              end if
            END DO
          END DO
        END DO
        print *, "  i22trivox.F:raz voxel ok."
      end if
      if(I_MEM==2)then
        if(itask==0.AND.ibug22_trivox==1)
     .    print *,
     .       "  i22trivox.F:returning i22buce (too much candidate)"
        GOTO 2000
      end if
      if(itask==0.AND.ibug22_trivox==1)
     .  print *, "  i22trivox.F:fin recherche des candidats, nb=",
     .           II_STOK

      if(itask==0.AND.ibug22_trivox==1)then
        allocate(order(ii_stok) ,VALUE(II_STOK))
        MIN2 = MINVAL(ABS(CAND_E(1:II_STOK)))
        R2   = MAXVAL(ABS(CAND_E(1:II_STOK))) - MIN2
        DO I=1,II_STOK
          VALUE(I) = CAND_B(I)*(R2+1)+ABS(CAND_E(I))-MIN2
        ENDDO
        order=0
        !CALL QUICKSORT_I2 !(ORDER,II_STOK,VALUE)
        !two column sorting (CAND_B,CAND_E) matrix by giving a vaue to each couple


        print *, "  II_STOK=", II_STOK
        print *, "  IXS(11,BUFBRIC(CAND_B)) ) =", IXS(11, BUFBRIC(CAND_B(ORDER(1:II_STOK))))
        print *, "  BUFBRIC(CAND_B) =", BUFBRIC(CAND_B(ORDER(1:II_STOK)))
        print *, "  CAND_B =", CAND_B(ORDER(1:II_STOK))
        print *, "  CAND_E =", CAND_E(ORDER(1:II_STOK))

        deallocate(order,VALUE)
      endif





!------ post ---- debug
 2000 CONTINUE
      CALL MY_BARRIER ! waiting vor voxel reset (and common deallocations)

      RETURN
      END SUBROUTINE


