Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    i25trivox      ../engine/source/interfaces/intsort/i25trivox.F
!||--- called by ------------------------------------------------------
!||    i25buce        ../engine/source/interfaces/intsort/i25buce.F
!||--- calls      -----------------------------------------------------
!||    i25sto         ../engine/source/interfaces/intsort/i25sto.F
!||    my_barrier     ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod   ../common_source/tools/memory/my_alloc.F90
!||    tri7box        ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I25TRIVOX(
     1      NSN    ,NSNR    ,ISZNSNR ,I_MEM   ,VMAXDT   ,
     2      IRECT  ,X       ,STF     ,STFN    ,XYZM     ,
     3      NSV    ,II_STOK ,CAND_N  ,ESHIFT  ,CAND_E   ,
     4      MULNSN ,NOINT   ,V       ,BGAPSMX  ,
     5      VOXEL  ,NBX     ,NBY     ,NBZ     ,PMAX_GAP ,
     6      NRTM   ,GAP_S   ,GAP_M   ,MARGE   ,CURV_MAX ,
     7      NIN    ,ITASK   ,PENE_OLD,ITAB    ,NBINFLG  ,
     8      MBINFLG,ILEV    ,MSEGTYP ,
     9      FLAGREMNODE,KREMNOD,REMNOD  ,
     A      IGAP   ,GAP_S_L,GAP_M_L  ,ICODT    ,ISKEW    ,
     B      DRAD   ,DGAPLOAD )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      use my_alloc_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   role of the routine:
C   ===================
C   classify nodes into boxes
C   search for concerned boxes for each facet
C   search for candidates
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     IRECT(4,*)   ARRAY OF CONEC FACETTES        E
C     X(3,*)       COORDONNEES NODALES               E
C     NSV          NOS SYSTEMES DES NODES E
C     Xmax larger abcisse existing e
C     XMAX largest order.existing E
C     Xmax larger existing side E
C     i_stok       storage level of pairs
C                                CANDIDATES impact    E/S
C     CAND_N       boites resultats nodes C     CAND_E       adresses des boites resultat elements
C                  mulnsn = multimp*nsn max size now allowed for the
C                  COUPLES NODES,ELT CANDIDATES
C     NOINT        INTERFACE USER NUMBER
C
C     Prov_n Provisional Cand_n (static variable in i7tri)
C     PROV_E       CAND_E provisoire (variable static in i7tri)

C     voxel(ix,iy,iz) contains the local number of the first node of
C                  the box
C     Next_nod (i) Next node in the same box (if /= 0)
C     Last_nod (i) Last node in the same box (if /= 0)
C                  used only to go directly from the first
C                       node at the last
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NIN,ITASK,IGAP,
     .        MULNSN,NOINT,NSNR,NBX,NBY,NBZ,
     .        NSV(*),CAND_N(*),CAND_E(*),
     .        IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),II_STOK,ITAB(*),
     .        NBINFLG(*),MBINFLG(*),ILEV,MSEGTYP(*),
     .        FLAGREMNODE,KREMNOD(*),REMNOD(*), ICODT(*), ISKEW(*)
C     REAL
      my_real
     .   X(3,*),V(3,*),XYZM(6),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   CURV_MAX(*),PENE_OLD(5,NSN),GAP_S_L(*),GAP_M_L(*),
     .   MARGE,BGAPSMX,PMAX_GAP,VMAXDT
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,
     .        NN,NE,K,L,J_STOK,JJ,
     .        PROV_N(MVSIZ),PROV_E(MVSIZ),
     .        NSNF, NSNL,M,DELNOD
C     REAL
      my_real
     .   XS,YS,ZS,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2
c 
      INTEGER  IX,IY,IZ,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE,save :: LAST_NOD
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA
      INTEGER FIRST,LAST
      SAVE IIX,IIY,IIZ
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAG

      integer :: node_id
      integer, dimension(:), allocatable, save :: local_nb_cand
      integer, dimension(:), allocatable :: local_cand
C-----------------------------------------------
      CALL MY_ALLOC(TAG,NUMNOD)
      
      IF(ITASK == 0)THEN
        ALLOCATE(NEXT_NOD(NSN+NSNR))
        ALLOCATE(IIX(NSN+NSNR))
        ALLOCATE(IIY(NSN+NSNR))
        ALLOCATE(IIZ(NSN+NSNR))
        allocate(local_nb_cand(nthread))
        allocate(last_nod(nsn+nsnr))
      END IF
      allocate(local_cand(nsn))
C Barrier to wait init voxel and allocation NEX_NOD
      CALL MY_BARRIER
C initial construction phase of bpe and bpn moved from i7buce => i7tri
C
      XMIN = XYZM(1)
      YMIN = XYZM(2)
      ZMIN = XYZM(3)
      XMAX = XYZM(4)
      YMAX = XYZM(5)
      ZMAX = XYZM(6)

c     Dev Future: xminb larger than Xmin ...
      XMINB = XMIN
      YMINB = YMIN
      ZMINB = ZMIN
      XMAXB = XMAX
      YMAXB = YMAX
      ZMAXB = ZMAX

      local_nb_cand(itask+1) = 0
!$omp do schedule(guided)      
      do i=1,nsn
        iix(i) = 0
        iiy(i) = 0
        iiz(i) = 0
        if(stfn(i) <= zero) cycle
        node_id = nsv(i)
        if(x(1,node_id)<xmin) cycle
        if(x(1,node_id)>xmax) cycle
        if(x(2,node_id)<ymin) cycle
        if(x(2,node_id)>ymax) cycle
        if(x(3,node_id)<zmin) cycle
        if(x(3,node_id)>zmax) cycle
        local_nb_cand(itask+1) = local_nb_cand(itask+1) + 1
        local_cand(local_nb_cand(itask+1)) = i
        iix(i) = int(nbx*(x(1,node_id)-xminb)/(xmaxb-xminb))
        iiy(i) = int(nby*(x(2,node_id)-yminb)/(ymaxb-yminb))
        iiz(i) = int(nbz*(x(3,node_id)-zminb)/(zmaxb-zminb))

        iix(i) = max(1,2+min(nbx,iix(i)))
        iiy(i) = max(1,2+min(nby,iiy(i)))
        iiz(i) = max(1,2+min(nbz,iiz(i)))
      enddo
!$omp end do
      call my_barrier( )     
!$omp single
      do i=1,nsn
        if(iix(i)==0) cycle
        first = voxel(iix(i),iiy(i),iiz(i))
        if(first==0)then
c         Empty Cell
          voxel(iix(i),iiy(i),iiz(i)) = i ! first
          next_nod(i)                 = 0 ! last one
          last_nod(i)                 = 0 ! no last
        elseif(last_nod(first)==0)then
c         cell containing one node
c         add as next node
          next_nod(first) = i ! next
          last_nod(first) = i ! last
          next_nod(i)     = 0 ! last one
        else
c         jump to the last node of the cell
          last = last_nod(first) ! last node in this voxel
          next_nod(last)  = i ! next
          last_nod(first) = i ! last
          next_nod(i)     = 0 ! last one
        endif
      enddo
!$omp end single no wait      

!$omp do schedule(guided)
      do i=1,nsnr
        iix(nsn+i) = int(nbx*(xrem(1,i)-xminb)/(xmaxb-xminb))
        iiy(nsn+i) = int(nby*(xrem(2,i)-yminb)/(ymaxb-yminb))
        iiz(nsn+i) = int(nbz*(xrem(3,i)-zminb)/(zmaxb-zminb))
        iix(nsn+i) = max(1,2+min(nbx,iix(nsn+i)))
        iiy(nsn+i) = max(1,2+min(nby,iiy(nsn+i)))
        iiz(nsn+i) = max(1,2+min(nbz,iiz(nsn+i)))
      enddo
!$omp end do

      call my_barrier( )

!$omp single
      do j = 1, nsnr
        first = voxel(iix(nsn+j),iiy(nsn+j),iiz(nsn+j))
        if(first==0)then
c         Empty Cell
          voxel(iix(nsn+j),iiy(nsn+j),iiz(nsn+j)) = nsn+j ! first
          next_nod(nsn+j)     = 0 ! last one
          last_nod(nsn+j)     = 0 ! no last
        elseif(last_nod(first)==0)then
c         cell containing one node
c         add as next node
          next_nod(first) = nsn+j  ! next
          last_nod(first) = nsn+j  ! last
          next_nod(nsn+j)  = 0     ! last one
        else
c         jump to the last node of the cell
          last = last_nod(first)  ! last node in this voxel
          next_nod(last)  = nsn+j ! next
          last_nod(first) = nsn+j ! last
          next_nod(nsn+j)     = 0 ! last one
        endif
      enddo
!$omp end single      

C=======================================================================
C 3 Searching boxes concerning each facet
C     and creation of candidates
C=======================================================================
      J_STOK = 0
C
      IF(FLAGREMNODE == 2)THEN
        DO I=1,NUMNOD
          TAG(I) = 0
        ENDDO

        DO NE=1,NRTM
C don't keep deleted segments
          IF(STF(NE) <= ZERO)CYCLE
          K = KREMNOD(2*(NE-1)+1)+1
          L = KREMNOD(2*(NE-1)+2)
          DO I=K,L
            TAG(REMNOD(I)) = 1
          ENDDO
c
          AAA = MARGE+CURV_MAX(NE)+MAX(MAX(BGAPSMX+GAP_M(NE),PMAX_GAP)+DGAPLOAD,DRAD)+VMAXDT
     +


c     It is possible to improve the algo by cutting the facet
c     in 2 (4,3,6,9 ...) if the facet is large in front of AAA and inclinee

          M1 = IRECT(1,NE)
          M2 = IRECT(2,NE)
          M3 = IRECT(3,NE)
          M4 = IRECT(4,NE)

          XX1=X(1,M1)
          XX2=X(1,M2)
          XX3=X(1,M3)
          XX4=X(1,M4)
          XMAXE=MAX(XX1,XX2,XX3,XX4)
          XMINE=MIN(XX1,XX2,XX3,XX4)

          YY1=X(2,M1)
          YY2=X(2,M2)
          YY3=X(2,M3)
          YY4=X(2,M4)
          YMAXE=MAX(YY1,YY2,YY3,YY4)
          YMINE=MIN(YY1,YY2,YY3,YY4)

          ZZ1=X(3,M1)
          ZZ2=X(3,M2)
          ZZ3=X(3,M3)
          ZZ4=X(3,M4)
          ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
          ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)


c        surface calculation (for future elimination of candidates)

          SX = (YY3-YY1)*(ZZ4-ZZ2) - (ZZ3-ZZ1)*(YY4-YY2)
          SY = (ZZ3-ZZ1)*(XX4-XX2) - (XX3-XX1)*(ZZ4-ZZ2)
          SZ = (XX3-XX1)*(YY4-YY2) - (YY3-YY1)*(XX4-XX2)
          S2 = SX*SX + SY*SY + SZ*SZ

c        index of voxels occupied by the facet

          IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
          IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
          IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

          IX1=MAX(1,2+MIN(NBX,IX1))
          IY1=MAX(1,2+MIN(NBY,IY1))
          IZ1=MAX(1,2+MIN(NBZ,IZ1))

          IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
          IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
          IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

          IX2=MAX(1,2+MIN(NBX,IX2))
          IY2=MAX(1,2+MIN(NBY,IY2))
          IZ2=MAX(1,2+MIN(NBZ,IZ2))

cc         nbpelem = 0
cc'nnpelem = 0
cc'nnr0pelem = 0
cc'nnrpelem = 0

          DO IZ = IZ1,IZ2
            DO IY = IY1,IY2
              DO IX = IX1,IX2

cc             nbpelem = nbpelem + 1

                JJ = VOXEL(IX,IY,IZ)

                DO WHILE(JJ /= 0)
                  DELNOD = 0
cc             nnpelem = nnpelem + 1

                  IF(JJ<=NSN)THEN
                    NN=NSV(JJ)
                    IF(NN == M1)GOTO 200
                    IF(NN == M2)GOTO 200
                    IF(NN == M3)GOTO 200
                    IF(NN == M4)GOTO 200
                    IF(TAG(NN) == 1)GOTO 200
                    XS = X(1,NN)
                    YS = X(2,NN)
                    ZS = X(3,NN)
c PMAX_GAP is a global overestimate penetration
c NEED to communicate in SPMD
c VMAXDT is a local overestimate of relative incremental displacement
c NO need to communicate in SPMD

                    AAA = MARGE + CURV_MAX(NE)
     +                  + MAX(GAP_S(JJ)+GAP_M(NE)+DGAPLOAD,DRAD)
     +                       +VMAXDT
                  ELSE
                    J=JJ-NSN

                    K = KREMNOD(2*(NE-1)+2) + 1
                    L = KREMNOD(2*(NE-1)+3)

                    DO M=K,L
                      IF(REMNOD(M) == -IREM(2,J) ) THEN
                        DELNOD = DELNOD + 1
                        EXIT
                      ENDIF
                    ENDDO

                    IF(DELNOD /= 0)GOTO 200

                    XS = XREM(1,J)
                    YS = XREM(2,J)
                    ZS = XREM(3,J)
                    AAA = MARGE+CURV_MAX(NE)
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c                                      +EDGE_L2(JJ) remote
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     +              + MAX(XREM(IGAPXREMP,J)+GAP_M(NE)+DGAPLOAD,DRAD)
     +              + VMAXDT
                  ENDIF

                  IF(XS<=XMINE-AAA)GOTO 200
                  IF(XS>=XMAXE+AAA)GOTO 200
                  IF(YS<=YMINE-AAA)GOTO 200
                  IF(YS>=YMAXE+AAA)GOTO 200
                  IF(ZS<=ZMINE-AAA)GOTO 200
                  IF(ZS>=ZMAXE+AAA)GOTO 200

c    underestimation of distance**2 for candidate elimination

cc'nnr0pelem = nnr0pelem + 1

                  D1X = XS - XX1
                  D1Y = YS - YY1
                  D1Z = ZS - ZZ1
                  D2X = XS - XX2
                  D2Y = YS - YY2
                  D2Z = ZS - ZZ2
                  DD1 = D1X*SX+D1Y*SY+D1Z*SZ
                  DD2 = D2X*SX+D2Y*SY+D2Z*SZ
                  IF(DD1*DD2 > ZERO)THEN
                    D2 = MIN(DD1*DD1,DD2*DD2)
                    A2 = AAA*AAA*S2
                    IF(D2 > A2)GOTO 200
                  ENDIF

cc             nnrpelem = nnrpelem + 1

                  J_STOK = J_STOK + 1
                  PROV_N(J_STOK) = JJ
                  PROV_E(J_STOK) = NE
                  IF(J_STOK == NVSIZ)THEN

                    CALL I25STO(
     1                 NVSIZ ,IRECT  ,X     ,NSV   ,II_STOK,
     2                 CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3                 I_MEM ,PROV_N ,PROV_E,ESHIFT,V      ,
     4                 NSN   ,NRTM   ,GAP_S  ,GAP_M ,CURV_MAX,NIN  ,
     5                 PENE_OLD,NBINFLG  ,MBINFLG,ILEV,MSEGTYP,
     6                 ITAB  ,IGAP ,GAP_S_L,GAP_M_L,ICODT,ISKEW,
     7                 DRAD  ,DGAPLOAD)
                    IF(I_MEM==2)GOTO 100
                    J_STOK = 0
                  ENDIF

  200             CONTINUE

                  JJ = NEXT_NOD(JJ)

                ENDDO ! WHILE(JJ /= 0)

              ENDDO
            ENDDO
          ENDDO
          K = KREMNOD(2*(NE-1)+1)+1
          L = KREMNOD(2*(NE-1)+2)
          DO I=K,L
            TAG(REMNOD(I)) = 0
          ENDDO

cc             nbpelg = nbpelg + nbpelem
cc             nnpelg = nnpelg + nnpelem
cc             nnrpelg = nnrpelg + nnrpelem
cc             nnr0pelg = nnr0pelg + nnr0pelem
        ENDDO

      ELSE !FLAGREMNODE
        DO NE=1,NRTM
C don't keep deleted segments
          IF(STF(NE) <= ZERO)CYCLE
c
          AAA = MARGE+CURV_MAX(NE)+MAX(MAX(BGAPSMX+GAP_M(NE),PMAX_GAP)+DGAPLOAD,DRAD)+VMAXDT
     +


c     It is possible to improve the algo by cutting the facet
c     in 2 (4,3,6,9 ...) if the facet is large in front of AAA and inclinee

          M1 = IRECT(1,NE)
          M2 = IRECT(2,NE)
          M3 = IRECT(3,NE)
          M4 = IRECT(4,NE)

          XX1=X(1,M1)
          XX2=X(1,M2)
          XX3=X(1,M3)
          XX4=X(1,M4)
          XMAXE=MAX(XX1,XX2,XX3,XX4)
          XMINE=MIN(XX1,XX2,XX3,XX4)

          YY1=X(2,M1)
          YY2=X(2,M2)
          YY3=X(2,M3)
          YY4=X(2,M4)
          YMAXE=MAX(YY1,YY2,YY3,YY4)
          YMINE=MIN(YY1,YY2,YY3,YY4)

          ZZ1=X(3,M1)
          ZZ2=X(3,M2)
          ZZ3=X(3,M3)
          ZZ4=X(3,M4)
          ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
          ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)


c        surface calculation (for future elimination of candidates)

          SX = (YY3-YY1)*(ZZ4-ZZ2) - (ZZ3-ZZ1)*(YY4-YY2)
          SY = (ZZ3-ZZ1)*(XX4-XX2) - (XX3-XX1)*(ZZ4-ZZ2)
          SZ = (XX3-XX1)*(YY4-YY2) - (YY3-YY1)*(XX4-XX2)
          S2 = SX*SX + SY*SY + SZ*SZ

c        index of voxels occupied by the facet

          IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
          IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
          IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

          IX1=MAX(1,2+MIN(NBX,IX1))
          IY1=MAX(1,2+MIN(NBY,IY1))
          IZ1=MAX(1,2+MIN(NBZ,IZ1))

          IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
          IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
          IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

          IX2=MAX(1,2+MIN(NBX,IX2))
          IY2=MAX(1,2+MIN(NBY,IY2))
          IZ2=MAX(1,2+MIN(NBZ,IZ2))

cc         nbpelem = 0
cc'nnpelem = 0
cc'nnr0pelem = 0
cc'nnrpelem = 0

          DO IZ = IZ1,IZ2
            DO IY = IY1,IY2
              DO IX = IX1,IX2

cc             nbpelem = nbpelem + 1

                JJ = VOXEL(IX,IY,IZ)

                DO WHILE(JJ /= 0)

cc             nnpelem = nnpelem + 1

                  IF(JJ<=NSN)THEN
                    NN=NSV(JJ)
                    IF(NN == M1)GOTO 300
                    IF(NN == M2)GOTO 300
                    IF(NN == M3)GOTO 300
                    IF(NN == M4)GOTO 300
                    XS = X(1,NN)
                    YS = X(2,NN)
                    ZS = X(3,NN)
c PMAX_GAP is a global overestimate penetration
c NEED to communicate in SPMD
c VMAXDT is a local overestimate of relative incremental displacement
c NO need to communicate in SPMD

                    AAA = MARGE + CURV_MAX(NE)
     +                  + MAX(GAP_S(JJ)+GAP_M(NE)+DGAPLOAD,DRAD)
     +                       +VMAXDT
                  ELSE
                    J=JJ-NSN

                    XS = XREM(1,J)
                    YS = XREM(2,J)
                    ZS = XREM(3,J)
                    AAA = MARGE+CURV_MAX(NE)
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c                                      +EDGE_L2(JJ) remote
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     +              + MAX(XREM(IGAPXREMP,J)+GAP_M(NE)+DGAPLOAD,DRAD)
     +              + VMAXDT
                  ENDIF

                  IF(XS<=XMINE-AAA)GOTO 300
                  IF(XS>=XMAXE+AAA)GOTO 300
                  IF(YS<=YMINE-AAA)GOTO 300
                  IF(YS>=YMAXE+AAA)GOTO 300
                  IF(ZS<=ZMINE-AAA)GOTO 300
                  IF(ZS>=ZMAXE+AAA)GOTO 300

c    underestimation of distance**2 for candidate elimination

cc'nnr0pelem = nnr0pelem + 1

                  D1X = XS - XX1
                  D1Y = YS - YY1
                  D1Z = ZS - ZZ1
                  D2X = XS - XX2
                  D2Y = YS - YY2
                  D2Z = ZS - ZZ2
                  DD1 = D1X*SX+D1Y*SY+D1Z*SZ
                  DD2 = D2X*SX+D2Y*SY+D2Z*SZ
                  IF(DD1*DD2 > ZERO)THEN
                    D2 = MIN(DD1*DD1,DD2*DD2)
                    A2 = AAA*AAA*S2
                    IF(D2 > A2)GOTO 300
                  ENDIF

cc             nnrpelem = nnrpelem + 1

                  J_STOK = J_STOK + 1
                  PROV_N(J_STOK) = JJ
                  PROV_E(J_STOK) = NE
                  IF(J_STOK == NVSIZ)THEN

                    CALL I25STO(
     1                 NVSIZ ,IRECT  ,X     ,NSV   ,II_STOK,
     2                 CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3                 I_MEM ,PROV_N ,PROV_E,ESHIFT,V      ,
     4                 NSN   ,NRTM   ,GAP_S  ,GAP_M ,CURV_MAX,NIN  ,
     5                 PENE_OLD,NBINFLG  ,MBINFLG,ILEV,MSEGTYP,
     6                 ITAB  ,IGAP ,GAP_S_L,GAP_M_L,ICODT,ISKEW,
     7                 DRAD  ,DGAPLOAD)
                    IF(I_MEM==2)GOTO 100
                    J_STOK = 0
                  ENDIF

  300             CONTINUE

                  JJ = NEXT_NOD(JJ)

                ENDDO ! WHILE(JJ /= 0)

              ENDDO
            ENDDO
          ENDDO

cc             nbpelg = nbpelg + nbpelem
cc             nnpelg = nnpelg + nnpelem
cc             nnrpelg = nnrpelg + nnrpelem
cc             nnr0pelg = nnr0pelg + nnr0pelem
        ENDDO
      END IF !FLAGREMNODE
C-------------------------------------------------------------------------
C     end of sorting
C-------------------------------------------------------------------------
      IF(J_STOK/=0)CALL I25STO(
     1              J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2              CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3              I_MEM ,PROV_N ,PROV_E,ESHIFT,V      ,
     4              NSN   ,NRTM   ,GAP_S  ,GAP_M ,CURV_MAX,NIN  ,
     5              PENE_OLD,NBINFLG,MBINFLG,ILEV ,MSEGTYP,
     6              ITAB  ,IGAP ,GAP_S_L,GAP_M_L,ICODT,ISKEW,
     7              DRAD  ,DGAPLOAD)

C=======================================================================
C 4   reset nodes to zero in boxes
C=======================================================================
  100 CONTINUE

C Barrier to avoid reinitialization before end of sorting
      CALL MY_BARRIER
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD

      DO I=NSNF,NSNL
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
C=======================================================================
C 5   reset nodes to zero in boxes
C     non -local candidates in SPMD
C=======================================================================
      NSNF = 1 + ITASK*NSNR / NTHREAD
      NSNL = (ITASK+1)*NSNR / NTHREAD
      DO J = NSNF, NSNL
        VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J))=0
      ENDDO

C
      CALL MY_BARRIER()
      IF(ITASK == 0)THEN
        DEALLOCATE(NEXT_NOD)
        DEALLOCATE(IIX)
        DEALLOCATE(IIY)
        DEALLOCATE(IIZ)
        deallocate(local_nb_cand)
        deallocate(last_nod)
      ENDIF

      DEALLOCATE(TAG)
      RETURN
      END

