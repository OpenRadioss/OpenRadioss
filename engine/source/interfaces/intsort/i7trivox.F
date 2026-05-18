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
!||    i7trivox        ../engine/source/interfaces/intsort/i7trivox.F
!||--- called by ------------------------------------------------------
!||    i7buce_vox      ../engine/source/interfaces/intsort/i7buce.F
!||--- calls      -----------------------------------------------------
!||    ancmsg          ../engine/source/output/message/message.F
!||    arret           ../engine/source/system/arret.F
!||    i7sto           ../engine/source/interfaces/intsort/i7sto.F
!||    my_barrier      ../engine/source/system/machine.F
!||    spmd_oldnumcd   ../engine/source/mpi/interfaces/spmd_i7tool.F
!||--- uses       -----------------------------------------------------
!||    message_mod     ../engine/share/message_module/message_mod.F
!||    tri7box         ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I7TRIVOX(
     1      NSN    ,RENUM       ,NSNR    ,ISZNSNR  ,I_MEM    ,
     2      IRECT  ,X           ,STF     ,STFN     ,XYZM     ,
     3      NSV    ,II_STOK     ,CAND_N  ,ESHIFT   ,CAND_E   ,
     4      MULNSN ,NOINT       ,TZINF   ,GAP_S_L  ,GAP_M_L  ,
     5      VOXEL  ,NBX         ,NBY     ,NBZ      ,INTTH    ,
     6      INACTI ,IFQ         ,CAND_A  ,CAND_P   ,IFPEN    ,
     7      NRTM   ,NSNROLD     ,IGAP    ,GAP      ,GAP_S    ,
     8      GAP_M  ,GAPMIN      ,GAPMAX  ,MARGE    ,CURV_MAX ,
     9      NIN    ,ITASK       ,BGAPSMX ,KREMNOD  ,REMNOD   ,
     A      ITAB   ,FLAGREMNODE ,DRAD    ,ITIED    ,CAND_F   ,
     B      DGAPLOAD,r_node_nb,r_node_list,
     C      TOTAL_NB_NRTM,INTHEAT,IDT_THERM,NODADT_THERM)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   ROUTINE PURPOSE:
C   ================
C   CLASSIFIES NODES INTO BOXES
C   SEARCHES FOR BOXES CONCERNED FOR EACH FACET
C   SEARCHES FOR CANDIDATES
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NAME         DESCRIPTION                       I/O
C
C     IRECT(4,*)   FACET CONNECTIVITY ARRAY          I
C     X(3,*)       NODAL COORDINATES                 I
C     NSV          NODE NUMBERS                      I
C     XMAX         largest existing abscissa         I
C     XMAX         largest existing ordinate         I
C     XMAX         largest existing coordinate       I
C     I_STOK       storage level of candidate
C                                I/O impact couples
C     CAND_N       node result boxes
C     CAND_E       element result box addresses
C                  MULNSN = MULTIMP*NSN MAX SIZE ALLOWED NOW FOR
C                  NODE,ELEMENT CANDIDATE COUPLES
C     NOINT        USER INTERFACE NUMBER
C     TZINF        INFLUENCE ZONE SIZE
C
C     PROV_N       provisional CAND_N (static variable in i7tri)
C     PROV_E       provisional CAND_E (static variable in i7tri)

C     VOXEL(ix,iy,iz) contains the local number of the first node in
C                  the box
C     NEXT_NOD(i)  next node in the same box (if /= 0)
C     LAST_NOD(i)  last node in the same box (if /= 0)
C                  used only to go directly from first
C                       node to last
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,ESHIFT,NSN,ISZNSNR,NSNROLD,NIN,ITASK,
     .        MULNSN,NOINT,INACTI,IFQ,NSNR,IGAP,NBX,NBY,NBZ,
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        INTTH,IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),II_STOK,
     .        KREMNOD(*),REMNOD(*),ITAB(*),FLAGREMNODE,ITIED
      INTEGER, INTENT(in) :: NRTM !< number of segments per threads
      INTEGER, INTENT(in) :: TOTAL_NB_NRTM !< total number of segments
      INTEGER, INTENT(IN) :: INTHEAT
      INTEGER, INTENT(IN) :: IDT_THERM
      INTEGER, INTENT(IN) :: NODADT_THERM
      my_real
     .   X(3,*),XYZM(12),CAND_P(*),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MARGE,GAP,GAPMIN,GAPMAX,BGAPSMX,
     .   CURV_MAX(*),GAP_S_L(*),GAP_M_L(*),CAND_F(*)
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
      INTEGER, INTENT(inout) :: r_node_nb
      INTEGER, DIMENSION(NSNR), INTENT(inout) :: r_node_list
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,
     .        NN,NE,K,L,J_STOK,JJ,
     .        PROV_N(MVSIZ),PROV_E(MVSIZ),
     .        OLDNUM(ISZNSNR), NSNF, NSNL,DELNOD,M
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGREMNODE
      my_real
     .   XS,YS,ZS,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2
c temporary
      INTEGER  IX,IY,IZ,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA
      INTEGER CPT_VOX0
      LOGICAL TEST_V0
      INTEGER FIRST,LAST,IERROR
      LOGICAL DBG_type18_fvm
      SAVE IIX,IIY,IIZ
      integer :: node_id,ijk,cpt_vox0_local
      integer,dimension(:), allocatable, save :: last_nod
      integer, dimension(:), allocatable :: local_r_node_list
      integer,dimension(:), allocatable, save :: local_nsnr,nsnr_address
C-----------------------------------------------
      IF(ITASK == 0)THEN
        CPT_VOX0 = 0
        r_node_nb = 0
        ALLOCATE(NEXT_NOD(NSN+NSNR),STAT=IERROR)
        allocate(last_nod(nsn+nsnr),stat=ierror)
        allocate(local_nsnr(nthread))
        allocate(nsnr_address(nthread))
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .        C1='(/INTER/TYPE7)')
          CALL ARRET(2)
        ENDIF
        ALLOCATE(IIX(NSN+NSNR),IIY(NSN+NSNR),IIZ(NSN+NSNR),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .        C1='(/INTER/TYPE7)')
          CALL ARRET(2)
        ENDIF
      END IF

      allocate(local_r_node_list(nsnr))

      CALL MY_BARRIER !Barrier to wait init voxel and allocation NEXT_NOD

! Initial phase of BPE and BPN construction moved from I7BUCE => I7TRI

      XMIN = XYZM(1)
      YMIN = XYZM(2)
      ZMIN = XYZM(3)
      XMAX = XYZM(4)
      YMAX = XYZM(5)
      ZMAX = XYZM(6)

      !dev future: xminb larger than xmin...
      IF( TO_TRIM(NIN) ) THEN
        XMINB = XYZM(7)
        YMINB = XYZM(8)
        ZMINB = XYZM(9)
        XMAXB = XYZM(10)
        YMAXB = XYZM(11)
        ZMAXB = XYZM(12)
      ELSE
! we use the global bounding box
! the box reduction seemed inefficient
! at the first sorting
! because too many SECONDARY nodes were outside
! the reduced box
        XMINB = XMIN
        YMINB = YMIN
        ZMINB = ZMIN
        XMAXB = XMAX
        YMAXB = YMAX
        ZMAXB = ZMAX
      ENDIF

      ! In SPMD, for inacti or IFQ, retrieves old numbering of non-local candidates
      IF(NSPMD>1.AND.(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.ITIED/=0)) THEN
        CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD,INTHEAT,IDT_THERM,NODADT_THERM)
      END IF

C=======================================================================
C 1   NODE CLUSTERING
C=======================================================================
      cpt_vox0_local = 0
      test_v0 = first_test(nin)
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
        iix(i) = int(nbx*(x(1,node_id)-xminb)/(xmaxb-xminb))
        iiy(i) = int(nby*(x(2,node_id)-yminb)/(ymaxb-yminb))
        iiz(i) = int(nbz*(x(3,node_id)-zminb)/(zmaxb-zminb))

        iix(i) = max(1,2+min(nbx,iix(i)))
        iiy(i) = max(1,2+min(nby,iiy(i)))
        iiz(i) = max(1,2+min(nbz,iiz(i)))

        if(test_v0) then
          ! count the number of SECONDARY nodes outside the reduced box
          if(iix(i)==1.or.iiy(i)==1.or.iiz(i)==1.or.
     .       iix(i)==nbx+2.or.iiy(i)==nby+2.or.iiz(i)==nbz+2) then
            cpt_vox0_local = cpt_vox0_local + 1
          endif
        endif
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

      local_nsnr(itask+1) = 0
!$omp do schedule(guided)
      do i=1,nsnr      
        if(xrem(1,i)<xmin) cycle
        if(xrem(1,i)>xmax) cycle
        if(xrem(2,i)<ymin) cycle
        if(xrem(2,i)>ymax) cycle
        if(xrem(3,i)<zmin) cycle
        if(xrem(3,i)>zmax) cycle
        local_nsnr(itask+1) = local_nsnr(itask+1) + 1
        local_r_node_list(local_nsnr(itask+1)) = i
        iix(nsn+i) = int(nbx*(xrem(1,i)-xminb)/(xmaxb-xminb))
        iiy(nsn+i) = int(nby*(xrem(2,i)-yminb)/(ymaxb-yminb))
        iiz(nsn+i) = int(nbz*(xrem(3,i)-zminb)/(zmaxb-zminb))
        iix(nsn+i) = max(1,2+min(nbx,iix(nsn+i)))
        iiy(nsn+i) = max(1,2+min(nby,iiy(nsn+i)))
        iiz(nsn+i) = max(1,2+min(nbz,iiz(nsn+i)))
        if(test_v0) then
          ! count the number of SECONDARY nodes outside the reduced box
          if(iix(nsn+i)==1.or.iiy(nsn+i)==1.or.iiz(nsn+i)==1.or.
     .       iix(nsn+i)==nbx+2.or.iiy(nsn+i)==nby+2.or.iiz(nsn+i)==nbz+2) then
            cpt_vox0_local = cpt_vox0_local + 1
          endif
        endif        
      enddo
!$omp end do

      call my_barrier( )

!$omp atomic
      cpt_vox0 = cpt_vox0_local + cpt_vox0

!$omp single
      do ijk=1,nthread
        nsnr_address(ijk) = r_node_nb        
        r_node_nb = r_node_nb + local_nsnr(ijk)
      enddo
!$omp end single

      do ijk=1,local_nsnr(itask+1)
        r_node_list(nsnr_address(itask+1)+ijk) = local_r_node_list(ijk)
      enddo

      call my_barrier( )

!$omp single      
      do ijk = 1, r_node_nb
        j = r_node_list(ijk)
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

      CALL MY_BARRIER ! Barrier to wait task0 treatment

      IF(FIRST_TEST(NIN)) THEN
!During the first sorting, we use the reduced box
!If too many (>5 %) nodes are outside the reduced box
!then the box is not reduced anymore
        CALL MY_BARRIER
        IF(ITASK == 0) THEN
          IF(CPT_VOX0 > 5*(r_node_nb + NSN)/100) TO_TRIM(NIN) = .FALSE.
          FIRST_TEST(NIN) = .FALSE.
        ENDIF
        CALL MY_BARRIER
      ENDIF


C=======================================================================
C 3   FACE RECOVERY AND ENUMERATION OF CANDIDATE COUPLES
C=======================================================================
      J_STOK = 0
      IF(FLAGREMNODE == 2) THEN
        ALLOCATE(TAGREMNODE(NUMNOD+NUMFAKENODIGEO))
        DO I=1,NUMNOD+NUMFAKENODIGEO
          TAGREMNODE(I) = 0
        ENDDO
      ENDIF
      DO NE=1,NRTM
        IF(STF(NE) == ZERO)CYCLE ! do not keep destroyed facets
        IF(FLAGREMNODE == 2) THEN
          K = KREMNOD(2*(NE-1)+1)+1
          L = KREMNOD(2*(NE-1)+2)
          DO I=K,L
            TAGREMNODE(REMNOD(I)) = 1
          ENDDO
        ENDIF
        IF(IGAP == 0)THEN
          AAA = TZINF+CURV_MAX(NE)
        ELSE
          AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE)))+DGAPLOAD,DRAD)
        ENDIF
        !it is possible to improve the algorithm by subdividing the facet
        !into 2(4,3,6,9...) if the facet is large compared to AAA and inclined
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

        !surface calculation (for future candidate elimination)
        SX = (YY3-YY1)*(ZZ4-ZZ2) - (ZZ3-ZZ1)*(YY4-YY2)
        SY = (ZZ3-ZZ1)*(XX4-XX2) - (XX3-XX1)*(ZZ4-ZZ2)
        SZ = (XX3-XX1)*(YY4-YY2) - (YY3-YY1)*(XX4-XX2)
        S2 = SX*SX + SY*SY + SZ*SZ

        !index of voxels occupied by the facet
        IF(NBX>1) THEN
          IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
          IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
        ELSE
          IX1=-2
          IX2=1
        ENDIF

        IF(NBY>1) THEN
          IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
          IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
        ELSE
          IY1=-2
          IY2=1
        ENDIF

        IF(NBZ>1) THEN
          IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))
          IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))
        ELSE
          IZ1=-2
          IZ2=1
        ENDIF

        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))

        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))

        !nbpelem = 0
        !nnpelem = 0
        !nnr0pelem = 0
        !nnrpelem = 0

        DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
            DO IX = IX1,IX2

              ! nbpelem = nbpelem + 1

              JJ = VOXEL(IX,IY,IZ)
              DO WHILE(JJ /= 0)         
                DELNOD = 0

                !nnpelem = nnpelem + 1
                IF(JJ<=NSN)THEN
                  NN=NSV(JJ)

                  IF(NN == M1)GOTO 200
                  IF(NN == M2)GOTO 200
                  IF(NN == M3)GOTO 200
                  IF(NN == M4)GOTO 200

                  IF(FLAGREMNODE == 2) THEN
                    IF( TAGREMNODE(NSV(JJ)) == 1)GOTO 200
                  ENDIF
                  XS = X(1,NN)
                  YS = X(2,NN)
                  ZS = X(3,NN)
                  IF(IGAP /= 0)THEN
                    AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,GAP_S(JJ)+GAP_M(NE)))+DGAPLOAD,DRAD)
                  ENDIF
                ELSE
                  J=JJ-NSN
                  IF(FLAGREMNODE == 2) THEN
                    NN = IREM(1,J)
                    K = KREMNOD(2*(NE-1)+2) + 1
                    L = KREMNOD(2*(NE-1)+3)
                    DO M=K,L
                      IF(REMNOD(M) == -IREM(2,J) ) THEN
                        DELNOD = DELNOD + 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF(DELNOD /= 0)GOTO 200
                  ENDIF

                  XS = XREM(1,J)
                  YS = XREM(2,J)
                  ZS = XREM(3,J)
                  IF(IGAP /= 0)THEN
                    AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,XREM(9,J)+GAP_M(NE)))+DGAPLOAD,DRAD)
                  ENDIF
                ENDIF

                IF(XS<=XMINE-AAA)GOTO 200
                IF(XS>=XMAXE+AAA)GOTO 200
                IF(YS<=YMINE-AAA)GOTO 200
                IF(YS>=YMAXE+AAA)GOTO 200
                IF(ZS<=ZMINE-AAA)GOTO 200
                IF(ZS>=ZMAXE+AAA)GOTO 200

                !underestimation of distance**2 for candidate elimination
                !nnr0pelem = nnr0pelem + 1

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

                !nnrpelem = nnrpelem + 1

                J_STOK = J_STOK + 1
                PROV_N(J_STOK) = JJ
                PROV_E(J_STOK) = NE
                IF(J_STOK == NVSIZ)THEN
                  CALL I7STO(
     1               NVSIZ ,IRECT  ,X     ,NSV   ,II_STOK,
     2               CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3               I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4               IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5               OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6               GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7               GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8               CAND_F ,DGAPLOAD)
                  IF(I_MEM==2)GOTO 100
                  J_STOK = 0
                ENDIF
  200           CONTINUE
                JJ = NEXT_NOD(JJ)
              ENDDO ! WHILE(JJ /= 0)
            ENDDO ! X
          ENDDO  ! Y
        ENDDO   ! Z
        IF(FLAGREMNODE == 2) THEN
          K = KREMNOD(2*(NE-1)+1)+1
          L = KREMNOD(2*(NE-1)+2)
          DO I=K,L
            TAGREMNODE(REMNOD(I)) = 0
          ENDDO
        ENDIF
      ENDDO
C=======================================================================
C 4   STORAGE
C=======================================================================
      IF(J_STOK/=0)CALL I7STO(
     1              J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2              CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3              I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4              IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5              OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6              GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7              GAP_S_L,GAP_M_L,INTTH,DRAD  ,ITIED    ,
     8              CAND_F ,DGAPLOAD)

C=======================================================================
C 5   VOXEL CLUSTERING  RESET
C=======================================================================
  100 CONTINUE
      ! Barrier to avoid reinitialization before end of sorting
      CALL MY_BARRIER
      IF(TOTAL_NB_NRTM>0) THEN
        NSNF = 1 + ITASK*NSN / NTHREAD
        NSNL = (ITASK+1)*NSN / NTHREAD
        DO I=NSNF,NSNL
            IF(IIX(I)/=0)THEN
              VOXEL(IIX(I),IIY(I),IIZ(I))=0
            ENDIF
        ENDDO
        NSNF = 1 + ITASK*r_node_nb / NTHREAD
        NSNL = (ITASK+1)*r_node_nb / NTHREAD
        IF(ITASK+1==NTHREAD) NSNL=r_node_nb
        DO JJ = NSNF, NSNL
            J = r_node_list(JJ)
            VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J))=0
        ENDDO
      ENDIF
      CALL MY_BARRIER()

C=======================================================================
C 6   DEBUG OUTPUT
C=======================================================================

      DBG_type18_fvm=.false.
      if(inacti==7 .AND. DBG_type18_fvm)then
        write(*,FMT='(A)')"------------------------------------------"
        write(*,*)"RESULT : Search Algorithm with VOXEL partitioning"
        write(*,*)"  Number of couples =", II_STOK
        if(II_STOK>0)then
          write(*,FMT='(A,(I10))')"  --> SECONDARY Node ids: ", CAND_N(1:II_STOK)
          write(*,FMT='(A,(I10))')"  --> Local Face ids: ", CAND_E(1:II_STOK)
        endif
        write(*,*)" Structure domain :"
        write(*,FMT='(A,F30.16,A,F30.16)')" Xmin=",XMIN,"  Xmax=",XMAX
        write(*,FMT='(A,F30.16,A,F30.16)')" Ymin=",YMIN,"  Ymax=",YMAX
        write(*,FMT='(A,F30.16,A,F30.16)')" Zmin=",ZMIN,"  Zmax=",ZMAX
        write(*,*)" Partitioning domain :"
        write(*,*)"   TZINF,AAA=",TZINF,AAA
        write(*,FMT='(A,F30.16,A,F30.16)')" Xmin=",XMIN-AAA,"  Xmax=",XMAX+AAA
        write(*,FMT='(A,F30.16,A,F30.16)')" Ymin=",YMIN-AAA,"  Ymax=",YMAX+AAA
        write(*,FMT='(A,F30.16,A,F30.16)')" Zmin=",ZMIN-AAA,"  Zmax=",ZMAX+AAA
        write(*,FMT='(A)')"------------------------------------------"
      endif

C=======================================================================
C 7   DEALLOCATE
C=======================================================================
      IF(ITASK == 0)THEN
        DEALLOCATE(NEXT_NOD)
        DEALLOCATE(IIX)
        DEALLOCATE(IIY)
        DEALLOCATE(IIZ)
        deallocate(last_nod)
        deallocate(local_nsnr)
        deallocate(nsnr_address)
        deallocate(local_r_node_list)
      ENDIF
      IF(FLAGREMNODE == 2) THEN
        IF(ALLOCATED(TAGREMNODE)) DEALLOCATE(TAGREMNODE)
      ENDIF


      RETURN
      END



