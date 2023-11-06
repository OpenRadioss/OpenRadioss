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
Chd|  I25TRIVOX_EDG                 source/interfaces/inter3d1/i25trivox_edg.F
Chd|-- called by -----------
Chd|        I25BUCE_EDG                   source/interfaces/inter3d1/i25buce_edg.F
Chd|-- calls ---------------
Chd|        I25STO_E2S                    source/interfaces/inter3d1/i25sto_e2s.F
Chd|        I25STO_EDG                    source/interfaces/inter3d1/i25sto_edg.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|        REALLOC_MOD                   share/modules1/realloc_mod.F
Chd|        TRI11                         share/modules1/tri11_mod.F
Chd|        TRI7BOX                       share/modules1/tri7box.F
Chd|====================================================================
      SUBROUTINE I25TRIVOX_EDG(
     1      I_MEM   ,VMAXDT  ,INACTI ,
     2      IRECT   ,X       ,STF    ,XYZM     ,
     3      II_STOK ,CANDS_E2E ,ESHIFT  ,NEDGE_T  ,CANDM_E2E   ,
     4      MULNSNE,NOINT   ,BGAPEMX ,SSHIFT   ,NRTM_T   ,
     5      VOXEL  ,NBX     ,NBY     ,NBZ      ,
     6      IGAP   ,GAP_M   ,GAP_M_L ,DRAD     ,MARGE   ,
     7      ITASK   ,ITAB   ,LL_STOK ,MULNSNS  ,
     8      MBINFLG ,EBINFLG,ILEV    ,CAND_A   ,CAND_P  ,
     9      FLAGREMNODE,KREMNODE,REMNODE  ,
     A      IEDGE  ,NEDGE  ,LEDGE   ,MSEGTYP   ,IGAP0   ,
     B      ADMSR,EDG_BISECTOR,VTX_BISECTOR,
     C      CANDM_E2S,CANDS_E2S,CAND_B,CAND_PS,GAPE ,
     D      GAP_E_L  ,DGAPLOAD)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
        USE REALLOC_MOD
        USE TRI7BOX
        USE TRI11
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
c     parameter setting the size for the vector (orig version is 128)
        INTEGER NVECSZ
        PARAMETER (NVECSZ = MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES EDGES DANS DES BOITES
C   RECHERCHE POUR CHAQUE FACETTE DES BOITES CONCERNES
C   RECHERCHE DES CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER I_MEM(2),INACTI,ITASK,IGAP,IEDGE,NEDGE,ESHIFT,NEDGE_T,SSHIFT,NRTM_T,IGAP0,
     .          MULNSNE,MULNSNS,NOINT,NBX,NBY,NBZ,
     .          CANDS_E2E(*),CANDM_E2E(*),
     .          IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),II_STOK,LL_STOK,ITAB(*),
     .          MBINFLG(*),EBINFLG(*),ILEV,CAND_A(*),LEDGE(NLEDGE,*),ADMSR(4,*),MSEGTYP(*),
     .          CANDM_E2S(*),CANDS_E2S(*),CAND_B(*),
     .          FLAGREMNODE,KREMNODE(*),REMNODE(*)
C     REAL
        my_real
     .     X(3,*),XYZM(6),STF(*),GAP_M(*),GAP_M_L(*),GAPE(*),GAP_E_L(*),CAND_P(*), CAND_PS(*),
     .     MARGE,BGAPEMX,VMAXDT,DRAD
        my_real , INTENT(IN) :: DGAPLOAD
        REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,I_STOK, SOL_EDGE, SH_EDGE,
     .          N1,N2,NN,NE,K,L,J_STOK,II,JJ,NA,NB,
     .          PROV_S(MVSIZ),PROV_M(MVSIZ),
     .          M,NS1,NS2,NSE,NS,SIZE,Z_FIRST,Z_LAST
C     REAL
        my_real
     .     DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .     XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .     XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .     D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS
c provisoire
        INTEGER  TAGEDG(NEDGE)
        INTEGER  IX,IY,IZ,IEDG,
     .           M1, M2, M3, M4, MM1,MM2,MM3,MM4,SS1,SS2,
     .           IMS1,IMS2,ISS1,ISS2,
     .           AM1,AM2,AS1,AS2,
     .           IX1,IY1,IZ1,IX2,IY2,IZ2
        INTEGER, DIMENSION(3) :: TMIN,TMAX
        my_real
     .     XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,AAA,DRAD2,
     .     XMAX_EDGS, XMIN_EDGS,   !cotes min/max des aretes seconds et mains
     .     YMAX_EDGS, YMIN_EDGS,
     .     ZMAX_EDGS, ZMIN_EDGS,
     .     XMAX_EDGM, XMIN_EDGM,
     .     YMAX_EDGM, YMIN_EDGM,
     .     ZMAX_EDGM, ZMIN_EDGM
        INTEGER FIRST_ADD, PREV_ADD, CHAIN_ADD, CURRENT_ADD, MAX_ADD
        INTEGER BITGET
        INTEGER WORK(70000)
        INTEGER,  DIMENSION(:,:),ALLOCATABLE :: IIXYZ,LEDGE_TMP
        INTEGER,  DIMENSION(:),ALLOCATABLE :: INDEX
        EXTERNAL BITGET
C-----------------------------------------------
C
        INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGREMLINE
C-----------------------------------------------
C
        DRAD2 = ZERO
C--- IREMGAP - array for tag of deactivated lines
        IF(FLAGREMNODE==2)THEN
          ALLOCATE(TAGREMLINE(NEDGE))
          TAGREMLINE(1:NEDGE) = 0
        ENDIF

        SOL_EDGE =IEDGE/10 ! solids
        SH_EDGE  =IEDGE-10*SOL_EDGE ! shells

        MIN_IX=NBX+2
        MIN_IY=NBY+2
        MIN_IZ=NBZ+2
        MAX_IX=1
        MAX_IY=1
        MAX_IZ=1

        !---------------------------------------------------------!
        ! Allocation des tableaux chaines                         !
        !---------------------------------------------------------!
        MAX_ADD = MAX(1,4*NEDGE)
        ALLOCATE(LCHAIN_ELEM(1:MAX_ADD))
        ALLOCATE(LCHAIN_NEXT(1:MAX_ADD))
        ALLOCATE(LCHAIN_LAST(1:MAX_ADD))

        ALLOCATE(IIXYZ(4,NEDGE),LEDGE_TMP(NLEDGE,NEDGE),INDEX(2*NEDGE))
C     Barrier to wait init voxel and allocation
C     CALL MY_BARRIER
C
        XMIN = XYZM(1)
        YMIN = XYZM(2)
        ZMIN = XYZM(3)
        XMAX = XYZM(4)
        YMAX = XYZM(5)
        ZMAX = XYZM(6)

c     dev future: xminb plus grand que xmin...
        XMINB = XMIN
        YMINB = YMIN
        ZMINB = ZMIN
        XMAXB = XMAX
        YMAXB = YMAX
        ZMAXB = ZMAX

C=======================================================================
C 1   mise des edges dans les boites
C=======================================================================
        IF(ITASK == 0)THEN

          IIXYZ(1:4,1:NEDGE)=0
          DO I=1,NEDGE
            INDEX(I)  =I
          END DO

          DO I=1,NEDGE

            NE  =LEDGE(1,I)
            IF(STF(NE)==0) CYCLE   ! on ne retient pas les facettes detruites

C       IF(LEDGE(3,I)/=0) CYCLE   ! on ne retient que les aretes de bord

            N1 = LEDGE(5,I)
            N2 = LEDGE(6,I)

            !-------------------------------------------!
            XX1=X(1,N1)
            XX2=X(1,N2)
            YY1=X(2,N1)
            YY2=X(2,N2)
            ZZ1=X(3,N1)
            ZZ2=X(3,N2)
            IF(IGAP0 == 0)THEN
              XMAX_EDGS=MAX(XX1,XX2); IF(XMAX_EDGS < XMIN) CYCLE
              XMIN_EDGS=MIN(XX1,XX2); IF(XMIN_EDGS > XMAX) CYCLE
              YMAX_EDGS=MAX(YY1,YY2); IF(YMAX_EDGS < YMIN) CYCLE
              YMIN_EDGS=MIN(YY1,YY2); IF(YMIN_EDGS > YMAX) CYCLE
              ZMAX_EDGS=MAX(ZZ1,ZZ2); IF(ZMAX_EDGS < ZMIN) CYCLE
              ZMIN_EDGS=MIN(ZZ1,ZZ2); IF(ZMIN_EDGS > ZMAX) CYCLE
            ELSE
              XMAX_EDGS=MAX(XX1,XX2)+GAP_M(NE); IF(XMAX_EDGS < XMIN) CYCLE
              XMIN_EDGS=MIN(XX1,XX2)-GAP_M(NE); IF(XMIN_EDGS > XMAX) CYCLE
              YMAX_EDGS=MAX(YY1,YY2)+GAP_M(NE); IF(YMAX_EDGS < YMIN) CYCLE
              YMIN_EDGS=MIN(YY1,YY2)-GAP_M(NE); IF(YMIN_EDGS > YMAX) CYCLE
              ZMAX_EDGS=MAX(ZZ1,ZZ2)+GAP_M(NE); IF(ZMAX_EDGS < ZMIN) CYCLE
              ZMIN_EDGS=MIN(ZZ1,ZZ2)-GAP_M(NE); IF(ZMIN_EDGS > ZMAX) CYCLE
            END IF

            !-------------------------------------------!
            !  VOXEL OCCUPIED BY THE EDGE        !
            !-------------------------------------------!
            !Voxel_lower_left_bound for this edge
            IX1=INT(NBX*(XMIN_EDGS-XMINB)/(XMAXB-XMINB))
            IY1=INT(NBY*(YMIN_EDGS-YMINB)/(YMAXB-YMINB))
            IZ1=INT(NBZ*(ZMIN_EDGS-ZMINB)/(ZMAXB-ZMINB))
            IX1=MAX(1,2+MIN(NBX,IX1))
            IY1=MAX(1,2+MIN(NBY,IY1))
            IZ1=MAX(1,2+MIN(NBZ,IZ1))
            !Voxel_upper_right_bound for this edge
            IX2=INT(NBX*(XMAX_EDGS-XMINB)/(XMAXB-XMINB))
            IY2=INT(NBY*(YMAX_EDGS-YMINB)/(YMAXB-YMINB))
            IZ2=INT(NBZ*(ZMAX_EDGS-ZMINB)/(ZMAXB-ZMINB))
            IX2=MAX(1,2+MIN(NBX,IX2))
            IY2=MAX(1,2+MIN(NBY,IY2))
            IZ2=MAX(1,2+MIN(NBZ,IZ2))

c       moins bon (cf snapfit, etc)
c  IIXYZ(1,I)=NE
c  IIXYZ(2,I)=LEDGE(3,I)
c  IIXYZ(3,I)=0
c  IIXYZ(4,I)=0
C
            IIXYZ(1,I)=MIN(N1,N2)
            IIXYZ(2,I)=0
            IIXYZ(3,I)=0
            IIXYZ(4,I)=0
C  IF(LEDGE(7,I) < 0) THEN ! larete nest pas une arete second
C    IIXYZ(1,I)=1
C  ELSE
C    IIXYZ(1,I)=0
C  END IF
C  IIXYZ(2,I)=N1
C  IIXYZ(3,I)=N2
C  IIXYZ(4,I)=0
c
c        IF(LEDGE(7,I) < 0) THEN
c          IIXYZ(1,I)=1
c        ELSE
c          IIXYZ(1,I)=0
c        END IF   ! larete nest pas une arete second
c  IIXYZ(2,I)=IX1
c  IIXYZ(3,I)=IY1
c  IIXYZ(4,I)=IZ1
            INDEX(I)  =I

          END DO

          CALL MY_ORDERS(0,WORK,IIXYZ,INDEX,NEDGE,4)
          LEDGE_TMP(1:NLEDGE,1:NEDGE)=LEDGE(1:NLEDGE,1:NEDGE)
          DO I=1,NEDGE
            K=INDEX(I)
            LEDGE(1:NLEDGE,I)=LEDGE_TMP(1:NLEDGE,K)
c        write(500,'(3I10)') I,LEDGE(5,I),LEDGE(6,I)
          END DO
          DEALLOCATE(IIXYZ,INDEX,LEDGE_TMP)

          CURRENT_ADD=1 ! premiere adresse

          DO I=1,NEDGE

            NE  =LEDGE(1,I)
            IF(STF(NE)==ZERO) CYCLE   ! on ne retient pas les facettes detruites

            IF(LEDGE(7,I) < 0) CYCLE   ! larete nest pas une arete second

            N1 = LEDGE(5,I)
            N2 = LEDGE(6,I)

            !-------------------------------------------!
            XX1=X(1,N1)
            XX2=X(1,N2)
            YY1=X(2,N1)
            YY2=X(2,N2)
            ZZ1=X(3,N1)
            ZZ2=X(3,N2)
            IF(IGAP0 == 0)THEN
              XMAX_EDGS=MAX(XX1,XX2); IF(XMAX_EDGS < XMIN) CYCLE
              XMIN_EDGS=MIN(XX1,XX2); IF(XMIN_EDGS > XMAX) CYCLE
              YMAX_EDGS=MAX(YY1,YY2); IF(YMAX_EDGS < YMIN) CYCLE
              YMIN_EDGS=MIN(YY1,YY2); IF(YMIN_EDGS > YMAX) CYCLE
              ZMAX_EDGS=MAX(ZZ1,ZZ2); IF(ZMAX_EDGS < ZMIN) CYCLE
              ZMIN_EDGS=MIN(ZZ1,ZZ2); IF(ZMIN_EDGS > ZMAX) CYCLE
            ELSE
              XMAX_EDGS=MAX(XX1,XX2)+GAPE(I); IF(XMAX_EDGS < XMIN) CYCLE
              XMIN_EDGS=MIN(XX1,XX2)-GAPE(I); IF(XMIN_EDGS > XMAX) CYCLE
              YMAX_EDGS=MAX(YY1,YY2)+GAPE(I); IF(YMAX_EDGS < YMIN) CYCLE
              YMIN_EDGS=MIN(YY1,YY2)-GAPE(I); IF(YMIN_EDGS > YMAX) CYCLE
              ZMAX_EDGS=MAX(ZZ1,ZZ2)+GAPE(I); IF(ZMAX_EDGS < ZMIN) CYCLE
              ZMIN_EDGS=MIN(ZZ1,ZZ2)-GAPE(I); IF(ZMIN_EDGS > ZMAX) CYCLE
            END IF

            !-------------------------------------------!
            !  VOXEL OCCUPIED BY THE EDGE        !
            !-------------------------------------------!
            !Voxel_lower_left_bound for this edge
            IX1=INT(NBX*(XMIN_EDGS-XMINB)/(XMAXB-XMINB))
            IY1=INT(NBY*(YMIN_EDGS-YMINB)/(YMAXB-YMINB))
            IZ1=INT(NBZ*(ZMIN_EDGS-ZMINB)/(ZMAXB-ZMINB))
            IX1=MAX(1,2+MIN(NBX,IX1))
            IY1=MAX(1,2+MIN(NBY,IY1))
            IZ1=MAX(1,2+MIN(NBZ,IZ1))
            !Voxel_upper_right_bound for this edge
            IX2=INT(NBX*(XMAX_EDGS-XMINB)/(XMAXB-XMINB))
            IY2=INT(NBY*(YMAX_EDGS-YMINB)/(YMAXB-YMINB))
            IZ2=INT(NBZ*(ZMAX_EDGS-ZMINB)/(ZMAXB-ZMINB))
            IX2=MAX(1,2+MIN(NBX,IX2))
            IY2=MAX(1,2+MIN(NBY,IY2))
            IZ2=MAX(1,2+MIN(NBZ,IZ2))

            !pour reset des voxel
            MIN_IX = MIN(MIN_IX,IX1)
            MIN_IY = MIN(MIN_IY,IY1)
            MIN_IZ = MIN(MIN_IZ,IZ1)
            MAX_IX = MAX(MAX_IX,IX2)
            MAX_IY = MAX(MAX_IY,IY2)
            MAX_IZ = MAX(MAX_IZ,IZ2)

            !----------------------------------------------!
            ! EDGE STORAGE FOR EACH VOXEL (CHAINED ARRAY)  !
            !----------------------------------------------!
C
C       VOXEL(i,j,k) LCHAIN_LAST(FIRST)
C       +-----------+------------+
C       |  =>FIRST  |  =>LAST   |
C       +--+--------+--+---------+
C          |         |
C          |         |
C          |         |
C          |         |   LCHAIN_ELEM(*) LCHAIN_NEXT(*)
C          |         |   +------------+-----------+
C          +-------------->| edge_id  |   iadd 3  |  1:FIRST --+
C                |   +------------+-----------+     |
C                |   |    |         |  2        |
C                |   +------------+-----------+     |
C                |   | edge_id  |   iadd 4  |  3 <-------+
C                |   +------------+-----------+     |
C                |   | edge_id  |   iadd 6  |  4 <-------+
C                |   +------------+-----------+     |
C              |   |        |        |  5      |
C                |   +------------+-----------+     |
C                +-->| edge_id  |   0      |  6:LAST <--+
C            +------------+-----------+
C              |    |         |  MAX_ADD
C              +------------+-----------+
            DO IZ = IZ1,IZ2
              DO IY = IY1,IY2
                DO IX = IX1,IX2

                  FIRST_ADD = VOXEL(IX,IY,IZ)

                  IF(FIRST_ADD == 0)THEN
                    !voxel encore vide
                    VOXEL(IX,IY,IZ)          = CURRENT_ADD ! adresse dans le tableau chaine de la premiere eddge trouvee occupant le voxel
                    LCHAIN_LAST(CURRENT_ADD) = CURRENT_ADD ! dernier=adresse pour l edge courante
                    LCHAIN_ELEM(CURRENT_ADD) = I               ! edge ID
                    LCHAIN_NEXT(CURRENT_ADD) = 0               ! pas de suivant car dernier de la liste   !
                  ELSE
                    !voxel contenant deja une edge
                    PREV_ADD                 = LCHAIN_LAST(FIRST_ADD) ! devient l'avant-dernier
                    LCHAIN_LAST(FIRST_ADD)   = CURRENT_ADD                ! maj du dernier
                    LCHAIN_ELEM(CURRENT_ADD) = I                          ! edge ID
                    LCHAIN_NEXT(PREV_ADD)    = CURRENT_ADD                ! maj du suivant 0 -> CURRENT_ADD
                    LCHAIN_NEXT(CURRENT_ADD) = 0                          ! pas de suivant car dernier de la liste
                  ENDIF

                  CURRENT_ADD = CURRENT_ADD+1

                  IF( CURRENT_ADD>=MAX_ADD)THEN
                    !OPTIMISATION : suprresion du deallocate/GOTO debut.
                    !REALLOCATE SI PAS ASSEZ DE PLACE : inutile de recommencer de 1 a MAX_ADD-1, on poursuit de MAX_ADD a 2*MAX_ADD
                    MAX_ADD = 2 * MAX_ADD
                    !print *, "reallocate"
                    LCHAIN_NEXT => IREALLOCATE(LCHAIN_NEXT, MAX_ADD)
                    LCHAIN_ELEM => IREALLOCATE(LCHAIN_ELEM, MAX_ADD)
                    LCHAIN_LAST => IREALLOCATE(LCHAIN_LAST, MAX_ADD)
                  ENDIF

                ENDDO !IX
              ENDDO !IY
            ENDDO !IZ

          ENDDO

        END IF

        TAGEDG(1:NEDGE)=0
C Barrier to wait task0 treatment
C     CALL MY_BARRIER
C=======================================================================
C     Sorting vs main shell edges
C=======================================================================
        IF(SH_EDGE==0) GOTO 300
C=======================================================================
C 3   A partir des voxels occupes par une edge main, on est en
C     mesure de connaitre toutes les edges escalves dans ce voisinage.
C     Ce qui permet de creer des couples cancidats pour le contact
C     Si la penetration est positive.
C=======================================================================

        J_STOK = 0

        DO I=1,NEDGE_T

          IEDG=ESHIFT+I

          NE=LEDGE(1,IEDG)
          IF(STF(NE)==ZERO) CYCLE   ! on ne retient pas les facettes detruites

          IF(IABS(LEDGE(7,IEDG))==1) CYCLE ! Main solid edge

          !-------------------------------------------!
          !    (N1,N2) is the current main edge     !
          !-------------------------------------------!

          AAA = MARGE+BGAPEMX+GAPE(IEDG)+DGAPLOAD

          N1 = LEDGE(5,IEDG)
          N2 = LEDGE(6,IEDG)
          MM1 = ITAB(N1)
          MM2 = ITAB(N2)
          AM1 = MIN(MM1,MM2)
          AM2 = MAX(MM1,MM2)

          IF(ILEV==2)THEN
            IMS1 = BITGET(EBINFLG(IEDG),0)
            IMS2 = BITGET(EBINFLG(IEDG),1)
          END IF

          !-------------------------------------------!
          !     X-coordinates of the four nodes       !
          !-------------------------------------------!

          XX1=X(1,N1)
          XX2=X(1,N2)
          YY1=X(2,N1)
          YY2=X(2,N2)
          ZZ1=X(3,N1)
          ZZ2=X(3,N2)
          XMAX_EDGM=MAX(XX1,XX2)+GAPE(IEDG) ! +TZINF
          XMIN_EDGM=MIN(XX1,XX2)-GAPE(IEDG) ! -TZINF
          YMAX_EDGM=MAX(YY1,YY2)+GAPE(IEDG) ! +TZINF
          YMIN_EDGM=MIN(YY1,YY2)-GAPE(IEDG) ! -TZINF
          ZMAX_EDGM=MAX(ZZ1,ZZ2)+GAPE(IEDG) ! +TZINF
          ZMIN_EDGM=MIN(ZZ1,ZZ2)-GAPE(IEDG) ! -TZINF
          !-------------------------------------------!
          !  VOXEL OCCUPIED BY THE BRICK              !
          !-------------------------------------------!
          !Voxel_lower_left_bound for this element---+
          IX1=INT(NBX*(XMIN_EDGM-AAA-XMINB)/(XMAXB-XMINB))
          IY1=INT(NBY*(YMIN_EDGM-AAA-YMINB)/(YMAXB-YMINB))
          IZ1=INT(NBZ*(ZMIN_EDGM-AAA-ZMINB)/(ZMAXB-ZMINB))
          IX1=MAX(1,2+MIN(NBX,IX1))
          IY1=MAX(1,2+MIN(NBY,IY1))
          IZ1=MAX(1,2+MIN(NBZ,IZ1))
          !Voxel_upper_right_bound for this element---+
          IX2=INT(NBX*(XMAX_EDGM+AAA-XMINB)/(XMAXB-XMINB))
          IY2=INT(NBY*(YMAX_EDGM+AAA-YMINB)/(YMAXB-YMINB))
          IZ2=INT(NBZ*(ZMAX_EDGM+AAA-ZMINB)/(ZMAXB-ZMINB))
          IX2=MAX(1,2+MIN(NBX,IX2))
          IY2=MAX(1,2+MIN(NBY,IY2))
          IZ2=MAX(1,2+MIN(NBZ,IZ2))

C--- IREMGAP - tag of deactivated lines
          IF(FLAGREMNODE==2)THEN
            K = KREMNODE(IEDG)
            L = KREMNODE(IEDG+1)-1
            DO M=K,L
              TAGREMLINE(REMNODE(M)) = 1
            ENDDO
          ENDIF

          DO IZ = IZ1,IZ2
            DO IY = IY1,IY2
              DO IX = IX1,IX2

                CHAIN_ADD = VOXEL(IX,IY,IZ)       ! adresse dans le tableau chaine de la premiere edge stoquee dans le voxel
                DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT
                  JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant

                  IF(TAGEDG(JJ)/=0)THEN ! edge deja traitee vs cette arete main
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  END IF
                  TAGEDG(JJ)=1

                  !secnd edge nodes, exclure couples avec noeud commun
                  IF (JJ<=NEDGE)THEN
                    SS1= ITAB(LEDGE(5,JJ))
                    SS2= ITAB(LEDGE(6,JJ))
C             ELSE
C               SS1=IREM(2,JJ-NEDGE)
C               SS2=IREM(3,JJ-NEDGE)
                  END IF

                  IF( (SS1==MM1).OR.(SS1==MM2).OR.
     .                (SS2==MM1).OR.(SS2==MM2)    )THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  END IF

                  IF(ILEV==2)THEN
                    IF(JJ <= NEDGE) THEN
                      ISS1=BITGET(EBINFLG(JJ),0)
                      ISS2=BITGET(EBINFLG(JJ),1)
C               ELSE
C                 ISS1 = BITGET(IREM(I24IREMP+6,JJ-NEDGE),0)
C                 ISS2 = BITGET(IREM(I24IREMP+6,JJ-NEDGE),1)
                    ENDIF
                    IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .                (IMS2 == 1 .and. ISS1==1)))THEN
                      CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                      CYCLE
                    ENDIF
                  ENDIF


                  IF(IABS(LEDGE(7,IEDG))/=1 .AND. LEDGE(7,JJ)/=1)THEN
                    ! attention les traitements de i25dst3e pour les solides
                    ! ne sont pas symetriques main second.
                    AS1 = MIN(SS1,SS2)
                    AS2 = MAX(SS1,SS2)
                    ! unicite des couples
                    IF(AM1 < AS1 .OR. (AM1 == AS1 .AND. AM2 < AS2))THEN
                      CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                      CYCLE
                    ENDIF
                  ENDIF

C             IREMPGAP - Tagged lines are removed
                  IF(FLAGREMNODE==2)THEN
                    IF(TAGREMLINE(JJ)==1) THEN
                      CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                      CYCLE
                    ENDIF
                  ENDIF

                  J_STOK = J_STOK + 1 !on dispose d'un candidat
                  PROV_S(J_STOK) = JJ      !edge secnd
                  PROV_M(J_STOK) = IEDG    !edge main

c              IF(DEJA==0) NEDG = NEDG + 1     !nombre d edges candidate au calcul de contact (debug)
c              DEJA=1                          !l edge main IEDG fait l'objet d'une ecriture de candidat. On compte les edges main faisant l'objet dun couple candidate : on ne doit plus incrementer NEDG pour les autres edge secnd testees.
                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
C-----------------------------------------------------
                  IF(J_STOK==NVSIZ)THEN
                    CALL I25STO_EDG(
     1                NVSIZ ,IRECT  ,X     ,II_STOK,INACTI,
     2                CANDS_E2E,CANDM_E2E ,MULNSNE,NOINT  ,MARGE ,
     3                I_MEM(1) ,PROV_S ,PROV_M ,IGAP0,CAND_A,
     4                NEDGE ,LEDGE  ,ITAB   ,DRAD2  ,IGAP  ,
     5                GAPE  ,GAP_E_L,ADMSR  ,EDG_BISECTOR,VTX_BISECTOR ,
     6                             CAND_P,DGAPLOAD)
                    IF(I_MEM(1)/=0) GOTO 300
                    J_STOK = 0
                  ENDIF
C-----------------------------------------------------

                ENDDO ! WHILE(CHAIN_ADD /= 0)

              ENDDO   !NEXT IZ
            ENDDO    !NEXT IY
          ENDDO     !NEXT IZ

C       Reset TAGEDG
          DO IZ = IZ1,IZ2
            DO IY = IY1,IY2
              DO IX = IX1,IX2

                CHAIN_ADD = VOXEL(IX,IY,IZ)
                DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT

                  JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant
                  TAGEDG(JJ)=0

                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)

                END DO

              ENDDO   !NEXT IZ
            ENDDO    !NEXT IY
          ENDDO     !NEXT IZ

C--- IREMGAP - clean of tagremline
          IF(FLAGREMNODE==2)THEN
            K = KREMNODE(IEDG)
            L = KREMNODE(IEDG+1)-1
            DO M=K,L
              TAGREMLINE(REMNODE(M)) = 0
            ENDDO
          ENDIF


        ENDDO       !NEXT IEDG


C-------------------------------------------------------------------------
C     FIN DU TRI vs Main shell edges
C-------------------------------------------------------------------------
        IF(J_STOK/=0)CALL I25STO_EDG(
     1    J_STOK ,IRECT  ,X     ,II_STOK,INACTI,
     2    CANDS_E2E,CANDM_E2E ,MULNSNE,NOINT  ,MARGE ,
     3    I_MEM(1) ,PROV_S ,PROV_M ,IGAP0,CAND_A,
     4    NEDGE ,LEDGE  ,ITAB   ,DRAD2  ,IGAP  ,
     5    GAPE  ,GAP_E_L,ADMSR  ,EDG_BISECTOR,VTX_BISECTOR ,
     6                   CAND_P,DGAPLOAD)

 300    CONTINUE
C=======================================================================
C     Sorting vs main solid edges
C=======================================================================
        IF(SOL_EDGE==0) GOTO 400
C=======================================================================
C 3bis A partir des voxels occupes par une edge main, on est en
C     mesure de connaitre toutes les edges escalves dans ce voisinage.
C     Ce qui permet de creer des couples cancidats pour le contact
C     Si la penetration est positive.
C=======================================================================

        J_STOK = 0

        DO I=1,NRTM_T

          NE  =SSHIFT+I

          IF(MSEGTYP(NE)/=0) CYCLE ! not a solid edge
          IF(STF(NE)==ZERO) CYCLE  ! on ne retient pas les facettes detruites

          M1 = IRECT(1,NE)
          M2 = IRECT(2,NE)
          M3 = IRECT(3,NE)
          M4 = IRECT(4,NE)

          MM1= ITAB(M1)
          MM2= ITAB(M2)
          MM3= ITAB(M3)
          MM4= ITAB(M4)

          XX1=X(1,M1)
          YY1=X(2,M1)
          ZZ1=X(3,M1)
          XX2=X(1,M2)
          YY2=X(2,M2)
          ZZ2=X(3,M2)
          XX3=X(1,M3)
          YY3=X(2,M3)
          ZZ3=X(3,M3)
          XX4=X(1,M4)
          YY4=X(2,M4)
          ZZ4=X(3,M4)

          XMAX_EDGM=MAX(XX1,XX2,XX3,XX4) ! +TZINF
          XMIN_EDGM=MIN(XX1,XX2,XX3,XX4) ! -TZINF
          YMAX_EDGM=MAX(YY1,YY2,YY3,YY4) ! +TZINF
          YMIN_EDGM=MIN(YY1,YY2,YY3,YY4) ! -TZINF
          ZMAX_EDGM=MAX(ZZ1,ZZ2,ZZ3,ZZ4) ! +TZINF
          ZMIN_EDGM=MIN(ZZ1,ZZ2,ZZ3,ZZ4) ! -TZINF
          DX=EM02*(XMAX_EDGM-XMIN_EDGM)
          DY=EM02*(YMAX_EDGM-YMIN_EDGM)
          DZ=EM02*(ZMAX_EDGM-ZMIN_EDGM)
          XMAX_EDGM=XMAX_EDGM+DX
          XMIN_EDGM=XMIN_EDGM-DX
          YMAX_EDGM=YMAX_EDGM+DY
          YMIN_EDGM=YMIN_EDGM-DY
          ZMAX_EDGM=ZMAX_EDGM+DZ
          ZMIN_EDGM=ZMIN_EDGM-DZ

          AAA = MARGE+BGAPEMX +DGAPLOAD ! filtrer vs GAPE(JJ) dans i25pen3_edg !

          !-------------------------------------------!
          !  VOXEL OCCUPIED BY THE BRICK              !
          !-------------------------------------------!
          !Voxel_lower_left_bound for this element---+
          IX1=INT(NBX*(XMIN_EDGM-AAA-XMINB)/(XMAXB-XMINB))
          IY1=INT(NBY*(YMIN_EDGM-AAA-YMINB)/(YMAXB-YMINB))
          IZ1=INT(NBZ*(ZMIN_EDGM-AAA-ZMINB)/(ZMAXB-ZMINB))
          IX1=MAX(1,2+MIN(NBX,IX1))
          IY1=MAX(1,2+MIN(NBY,IY1))
          IZ1=MAX(1,2+MIN(NBZ,IZ1))
          !Voxel_upper_right_bound for this element---+
          IX2=INT(NBX*(XMAX_EDGM+AAA-XMINB)/(XMAXB-XMINB))
          IY2=INT(NBY*(YMAX_EDGM+AAA-YMINB)/(YMAXB-YMINB))
          IZ2=INT(NBZ*(ZMAX_EDGM+AAA-ZMINB)/(ZMAXB-ZMINB))
          IX2=MAX(1,2+MIN(NBX,IX2))
          IY2=MAX(1,2+MIN(NBY,IY2))
          IZ2=MAX(1,2+MIN(NBZ,IZ2))

          IF(ILEV==2)THEN
            IMS1 = BITGET(MBINFLG(NE),0)
            IMS2 = BITGET(MBINFLG(NE),1)
          END IF

          DO IZ = IZ1,IZ2
            DO IY = IY1,IY2
              DO IX = IX1,IX2

                CHAIN_ADD = VOXEL(IX,IY,IZ)       ! adresse dans le tableau chaine de la premiere edge stoquee dans le voxel
                DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT
                  JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant

                  IF(TAGEDG(JJ)/=0)THEN ! edge deja traitee vs cette arete main
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  END IF
                  TAGEDG(JJ)=1

                  !secnd edge nodes, exclure couples avec noeud commun
                  IF (JJ<=NEDGE)THEN
                    SS1= ITAB(LEDGE(5,JJ))
                    SS2= ITAB(LEDGE(6,JJ))
C             ELSE
C               SS1=IREM(2,JJ-NEDGE)
C               SS2=IREM(3,JJ-NEDGE)
                  END IF

                  IF((SS1==MM1).OR.(SS1==MM2).OR.(SS1==MM3).OR.(SS1==MM4).OR.
     .               (SS2==MM1).OR.(SS2==MM2).OR.(SS2==MM3).OR.(SS2==MM4))THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  END IF

                  IF(ILEV==2)THEN
                    IF(JJ <= NEDGE) THEN
                      ISS1=BITGET(EBINFLG(JJ),0)
                      ISS2=BITGET(EBINFLG(JJ),1)
C               ELSE
C                 ISS1 = BITGET(IREM(I24IREMP+6,JJ-NEDGE),0)
C                 ISS2 = BITGET(IREM(I24IREMP+6,JJ-NEDGE),1)
                    ENDIF
                    IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .                (IMS2 == 1 .and. ISS1==1)))THEN
                      CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                      CYCLE
                    ENDIF
                  ENDIF

C-----------------------------------------------------

                  J_STOK = J_STOK + 1 !on dispose d'un candidat
                  PROV_S(J_STOK) = JJ   !edge secnd
                  PROV_M(J_STOK) = NE     !segment main

C-----------------------------------------------------
                  IF(J_STOK==NVSIZ)THEN
                    CALL I25STO_E2S(
     1                   NVSIZ ,IRECT  ,X        ,LL_STOK,INACTI,
     2                   CANDS_E2S,CANDM_E2S,MULNSNS,NOINT  ,MARGE ,
     3                   I_MEM(2) ,PROV_S ,PROV_M ,IGAP0,CAND_B,
     4                   NEDGE ,LEDGE  ,ITAB   ,DRAD2  ,IGAP  ,
     5                   GAP_M ,GAP_M_L,GAPE  ,GAP_E_L,ADMSR  ,
     6                   EDG_BISECTOR,VTX_BISECTOR ,CAND_PS,DGAPLOAD)
                    IF(I_MEM(2)/=0) GOTO 400
                    J_STOK = 0
                  ENDIF
C-----------------------------------------------------

                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD) ! Next RTM

                ENDDO ! WHILE(CHAIN_ADD /= 0)

              ENDDO   !NEXT IZ
            ENDDO    !NEXT IY
          ENDDO     !NEXT IZ

C       Reset TAGEDG
          DO IZ = IZ1,IZ2
            DO IY = IY1,IY2
              DO IX = IX1,IX2

                CHAIN_ADD = VOXEL(IX,IY,IZ)
                DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT

                  JJ = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant
                  TAGEDG(JJ)=0

                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)

                END DO

              ENDDO   !NEXT IZ
            ENDDO    !NEXT IY
          ENDDO     !NEXT IZ

        ENDDO       !NEXT IEDG
C-------------------------------------------------------------------------
C     FIN DU TRI vs Main solid edges
C-------------------------------------------------------------------------
        IF(J_STOK/=0)CALL I25STO_E2S(
     1                   J_STOK ,IRECT  ,X        ,LL_STOK,INACTI,
     2                   CANDS_E2S,CANDM_E2S,MULNSNS,NOINT  ,MARGE ,
     3                   I_MEM(2) ,PROV_S ,PROV_M ,IGAP0,CAND_B,
     4                   NEDGE ,LEDGE  ,ITAB   ,DRAD2  ,IGAP  ,
     5                   GAP_M ,GAP_M_L,GAPE  ,GAP_E_L,ADMSR  ,
     6                   EDG_BISECTOR,VTX_BISECTOR ,CAND_PS,DGAPLOAD)

C=======================================================================
C 4   remise a zero des noeuds dans les boites
C=======================================================================
 400    CONTINUE

C Barrier to avoid reinitialization before end of sorting
C     CALL MY_BARRIER

        TMIN(1) = MIN_IX
        TMIN(2) = MIN_IY
        TMIN(3) = MIN_IZ

        TMAX(1) = MAX_IX
        TMAX(2) = MAX_IY
        TMAX(3) = MAX_IZ

        IF (ITASK==0)THEN
          !RESET VOXEL WITHIN USED RANGE ONLY
          DO K= TMIN(3),TMAX(3)
            DO J= TMIN(2),TMAX(2)
              DO I= TMIN(1),TMAX(1)
                VOXEL(I,J,K) = 0
              END DO
            END DO
          END DO
          !CHAINED LIST DEALLOCATION
          DEALLOCATE(LCHAIN_NEXT)
          DEALLOCATE(LCHAIN_ELEM)
          DEALLOCATE(LCHAIN_LAST)
          IF(FLAGREMNODE==2) DEALLOCATE(TAGREMLINE)
        ENDIF
C=======================================================================

        RETURN
      END
