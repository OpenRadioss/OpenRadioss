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
      !||    i7trivox1                  ../starter/source/interfaces/inter3d1/i7trivox1.F
      !||--- called by ------------------------------------------------------
      !||    i7buc_vox1                 ../starter/source/interfaces/inter3d1/i7buc_vox1.F
      !||--- calls      -----------------------------------------------------
      !||    i7cor3                     ../starter/source/interfaces/inter3d1/i7cor3.F
      !||    i7dst3                     ../starter/source/interfaces/inter3d1/i7dst3.F
      !||    i7pen3                     ../starter/source/interfaces/inter3d1/i7pen3.F
      !||    inter_save_candidate       ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
      !||--- uses       -----------------------------------------------------
      !||    inter_save_candidate_mod   ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
      !||    tri7box                    ../starter/share/modules1/tri7box.F
      !||====================================================================
      SUBROUTINE I7TRIVOX1(
     1      NSN    ,I_MEM   ,IRECT    ,X       ,STF     ,
     2      STFN   ,XYZM    ,NSV      ,
     3      MULNSN ,NOINT   ,TZINF    ,GAP_S_L ,GAP_M_L ,
     4      VOXEL  ,NBX     ,NBY      ,NBZ     ,NRTM_L    ,
     5      IGAP   ,GAP     ,GAP_S    ,GAP_M   ,GAPMIN  ,
     6      GAPMAX ,MARGE   ,CURV_MAX ,BGAPSMX ,ISTF    ,
     7      I_STOK ,nin,
     8      ID     ,TITR    ,DRAD     ,INDEX   ,
     9      IREMNODE,FLAGREMNODE,KREMNODE,REMNODE,
     1      DGAPLOAD,ipari,intbuf_tab,
     2      iix,iiy,iiz,local_next_nod,nrtm )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE TRI7BOX
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use inter_save_candidate_mod , only : inter_save_candidate
      use array_mod 
      use intbufdef_mod 
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
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES NOEUDS DANS DES BOITES
C   RECHERCHE POUR CHAQUE FACETTE DES BOITES CONCERNES
C   RECHERCHE DES CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     IRECT(4,*)   TABLEAU DES CONEC FACETTES        E
C     X(3,*)       COORDONNEES NODALES               E
C     NSV          NOS SYSTEMES DES NOEUDS           E
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     I_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     CAND_N       boites resultats noeuds
C     CAND_E       adresses des boites resultat elements
C                  MULNSN = MULTIMP*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C
C     PROV_N       CAND_N provisoire (variable static dans i7tri)
C     PROV_E       CAND_E provisoire (variable static dans i7tri)

C     VOXEL(ix,iy,iz) contient le numero local du premier noeud de
C                  la boite
C     LOCAL_NEXT_NOD(i)  noeud suivant dans la meme boite (si /= 0)
C     LAST_NOD(i)  dernier noeud dans la meme boite (si /= 0)
C                  utilise uniquement pour aller directement du premier
C                       noeud au dernier
C     INDEX        index of IRECTM active for this domain
C     NRTM_L         number of active segments for this domain
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: nrtm !< number of segment
      integer, intent(in) :: nrtm_l !< number of segment on the current proc
      INTEGER I_MEM,NSN,
     .        MULNSN,NOINT,IGAP,NBX,NBY,NBZ,IREMNODE,FLAGREMNODE,
     .        NSV(NSN),
     .        IRECT(4,NRTM), VOXEL(NBX+2,NBY+2,NBZ+2),ISTF,
     .        I_STOK ,J_STOK,
     .        INDEX(*),KREMNODE(*),REMNODE(*)
      my_real
     .   X(3,*),XYZM(6,2),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MARGE,GAP,GAPMIN,GAPMAX,BGAPSMX,DRAD,
     .   CURV_MAX(*),GAP_S_L(*),GAP_M_L(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE)::TITR
      integer, intent(in) :: nin !< interface index
      integer, dimension(npari), intent(inout) :: ipari !< interface data
      type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface data
      INTEGER,  dimension(nsn), intent(inout) :: iix,iiy,iiz,local_next_nod
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,II,JJ,KK,
     .        NSNF, NSNL, I_BID,DELNOD
C     REAL
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS,GAPV(MVSIZ)
      INTEGER  LAST_NOD(NSN)
      INTEGER*8 meanjj8
      INTEGER  IX,IY,IZ,NEXT,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      integer, dimension(mvsiz) :: prov_n,prov_e
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA,TSTART,TSTOP
      my_real , INTENT(IN) :: DGAPLOAD
c      my_ream
c     .   maxaaa
      INTEGER FIRST,NEW,LAST,M
      INTEGER, DIMENSION(MVSIZ) :: IX11,IX12,IX13,IX14,NSVG
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ) :: LC1,LC2,LC3,LC4
      my_real, DIMENSION(MVSIZ) :: N11,N21,N31
      my_real, DIMENSION(MVSIZ) :: STIF,PENE

      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGNOD

      integer , external :: omp_get_thread_num,omp_get_num_threads
      integer :: itask,nthreads
      integer :: my_old_size,my_address
      integer :: local_i_stok,multimp
      integer :: local_cand_n_size,local_cand_e_size

      integer, save :: i_stok_old
      integer, dimension(:), allocatable, save :: cand_n_size,cand_e_size
      integer, dimension(:), allocatable, save :: address_cand_n,address_cand_e
      type(array_type_int_1d) :: local_cand_n
      type(array_type_int_1d) :: local_cand_e

      integer :: my_size,mode
      integer, dimension(:), allocatable :: my_index
      integer, dimension(:,:), allocatable :: sort_array,save_array
      integer, dimension(70000) :: work
C-----------------------------------------------
      ! allocation of local arrays 
      itask = omp_get_thread_num()
      nthreads = omp_get_num_threads()
      local_cand_n_size = size(intbuf_tab%cand_n) / nthreads
      local_cand_e_size = size(intbuf_tab%cand_e) / nthreads
      local_i_stok = 0
      local_cand_n%size_int_array_1d = local_cand_n_size
      local_cand_e%size_int_array_1d = local_cand_e_size
      call alloc_1d_array(local_cand_n)
      call alloc_1d_array(local_cand_e)

      XMIN = XYZM(1,1)
      YMIN = XYZM(2,1)
      ZMIN = XYZM(3,1)
      XMAX = XYZM(4,1)
      YMAX = XYZM(5,1)
      ZMAX = XYZM(6,1)

      XMINB = XYZM(1,2)
      YMINB = XYZM(2,2)
      ZMINB = XYZM(3,2)
      XMAXB = XYZM(4,2)
      YMAXB = XYZM(5,2)
      ZMAXB = XYZM(6,2)

C=======================================================================
C 1   mise des noeuds dans les boites
C=======================================================================

!$OMP MASTER

      allocate( cand_n_size(nthreads+1),cand_e_size(nthreads+1) )
      allocate( address_cand_n(nthreads+1),address_cand_e(nthreads+1) )
      cand_n_size(1:nthreads+1) = 0 
      cand_e_size(1:nthreads+1) = 0
      address_cand_n(1:nthreads+1) = 0 
      address_cand_e(1:nthreads+1) = 0

      DO I=1,NSN
        IIX(I)=0
        IIY(I)=0
        IIZ(I)=0
        IF(STFN(I) == ZERO)CYCLE
        J=NSV(I)

C First test vs CRVOXEL
        IX=INT(LRVOXEL*(X(1,J)-XMIN)/(XMAX-XMIN))
        IF(IX < 0 .OR. IX > LRVOXEL) CYCLE
        IY=INT(LRVOXEL*(X(2,J)-YMIN)/(YMAX-YMIN))
        IF(IY < 0 .OR. IY > LRVOXEL) CYCLE
        IZ=INT(LRVOXEL*(X(3,J)-ZMIN)/(ZMAX-ZMIN))
        IF(IZ < 0 .OR. IZ > LRVOXEL) CYCLE
        IF(.NOT.(BTEST(CRVOXEL(IY,IZ),IX))) CYCLE

C Second test vs reduced Box
        IF( (X(1,J)-XMINB)/(XMAXB-XMINB) > ONE ) THEN
          IIX(I) = NBX
        ELSE
          IIX(I)=INT(MAX(NBX*(X(1,J)-XMINB)/(XMAXB-XMINB),-ONE))
        ENDIF
        IF( (X(2,J)-YMINB)/(YMAXB-YMINB) > ONE ) THEN
          IIY(I) = NBY
        ELSE
          IIY(I)=INT(MAX(NBY*(X(2,J)-YMINB)/(YMAXB-YMINB),-ONE))
        ENDIF
        IF( (X(3,J)-ZMINB)/(ZMAXB-ZMINB) > ONE ) THEN
          IIZ(I) = NBZ
        ELSE
          IIZ(I)=INT(MAX(NBZ*(X(3,J)-ZMINB)/(ZMAXB-ZMINB),-ONE))
        ENDIF


        IIX(I)=MAX(1,2+IIX(I))
        IIY(I)=MAX(1,2+IIY(I))
        IIZ(I)=MAX(1,2+IIZ(I))

        FIRST = VOXEL(IIX(I),IIY(I),IIZ(I))
        IF(FIRST == 0)THEN
c         empty cell
          VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
          LOCAL_NEXT_NOD(I)                 = 0 ! last one
          LAST_NOD(I)                 = 0 ! no last
        ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         cell containing one node
c         add as next node
          LOCAL_NEXT_NOD(FIRST) = I ! next
          LAST_NOD(FIRST) = I ! last
          LOCAL_NEXT_NOD(I)     = 0 ! last one
        ELSE
c
c         jump to the last node of the cell
          LAST = LAST_NOD(FIRST) ! last node in this voxel
          LOCAL_NEXT_NOD(LAST)  = I ! next
          LAST_NOD(FIRST) = I ! last
          LOCAL_NEXT_NOD(I)     = 0 ! last one
        ENDIF
      ENDDO
!$OMP END MASTER

!$OMP BARRIER
C=======================================================================
C 3   recherche des boites concernant chaque facette
C     et creation des candidats
C=======================================================================

      ALLOCATE(TAGNOD(NUMNOD+NUMFAKENODIGEO))
      TAGNOD(1:NUMNOD+NUMFAKENODIGEO) = 0
      local_i_stok = 0
      j_stok = 0
!$OMP DO SCHEDULE(guided)
      DO KK=1,NRTM_L
        NE=INDEX(KK)
        IF(STF(NE) == ZERO)CYCLE

        IF(FLAGREMNODE==2.AND.IREMNODE==2)THEN
          K = KREMNODE(NE)+1
          L = KREMNODE(NE+1)
          DO M=K,L
            TAGNOD(REMNODE(M)) = 1
          ENDDO
        ENDIF

        IF(IGAP == 0)THEN
          AAA = TZINF+CURV_MAX(NE)
        ELSE
          AAA = MARGE+CURV_MAX(NE)+
     .      MAX(MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE)))+DGAPLOAD,DRAD)
        ENDIF

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


c        calcul de la surface (pour elimination future de candidats)

        SX = (YY3-YY1)*(ZZ4-ZZ2) - (ZZ3-ZZ1)*(YY4-YY2)
        SY = (ZZ3-ZZ1)*(XX4-XX2) - (XX3-XX1)*(ZZ4-ZZ2)
        SZ = (XX3-XX1)*(YY4-YY2) - (YY3-YY1)*(XX4-XX2)
        S2 = SX*SX + SY*SY + SZ*SZ

c        indice des voxels occupes par la facette
        IF( (XMINE - AAA - XMINB)/(XMAXB-XMINB) > ONE ) THEN
          IX1 = NBX
        ELSE
          IX1=INT(MAX(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB),-ONE))
        ENDIF
        IF( (YMINE - AAA - YMINB)/(YMAXB-YMINB) > ONE ) THEN
          IY1 = NBY
        ELSE
          IY1=INT(MAX(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB),-ONE))
        ENDIF
        IF( (ZMINE - AAA - ZMINB)/(ZMAXB-ZMINB) > ONE ) THEN
          IZ1 = NBZ
        ELSE
          IZ1=INT(MAX(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB),-ONE))
        ENDIF

        IX1=MAX(1,2+IX1)
        IY1=MAX(1,2+IY1)
        IZ1=MAX(1,2+IZ1)

        IF( (XMAXE + AAA - XMINB)/(XMAXB-XMINB) > ONE ) THEN
          IX2 = NBX
        ELSE
          IX2=INT(MAX(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB),-ONE))
        ENDIF
        IF( (YMAXE + AAA - YMINB)/(YMAXB-YMINB) > ONE ) THEN
          IY2 = NBY
        ELSE
          IY2=INT(MAX(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB),-ONE))
        ENDIF
        IF( (ZMAXE + AAA - ZMINB)/(ZMAXB-ZMINB) > ONE ) THEN
          IZ2 = NBZ
        ELSE
          IZ2=INT(MAX(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB),-ONE))
        ENDIF

        IX2=MAX(1,2+IX2)
        IY2=MAX(1,2+IY2)
        IZ2=MAX(1,2+IZ2)

        DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
            DO IX = IX1,IX2

              JJ = VOXEL(IX,IY,IZ)

              DO WHILE(JJ /= 0)

                NN=NSV(JJ)

                IF(TAGNOD(NN) == 1 ) GOTO 300
                IF(NN == M1)GOTO 300
                IF(NN == M2)GOTO 300
                IF(NN == M3)GOTO 300
                IF(NN == M4)GOTO 300

                XS = X(1,NN)
                YS = X(2,NN)
                ZS = X(3,NN)
                IF(IGAP /= 0)THEN
                  AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,
     .              GAP_S(JJ)+GAP_M(NE)))+DGAPLOAD,DRAD)
                ENDIF
                IF(XS<=XMINE-AAA)GOTO 300
                IF(XS>=XMAXE+AAA)GOTO 300
                IF(YS<=YMINE-AAA)GOTO 300
                IF(YS>=YMAXE+AAA)GOTO 300
                IF(ZS<=ZMINE-AAA)GOTO 300
                IF(ZS>=ZMAXE+AAA)GOTO 300

c    sousestimation de la distance**2 pour elimination de candidats

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
                J_STOK = J_STOK + 1
                PROV_N(J_STOK) = JJ
                PROV_E(J_STOK) = NE
                IF(J_STOK == NVSIZ) THEN
                  CALL I7COR3(X    ,IRECT,NSV  ,PROV_E ,PROV_N,
     .                        STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .                        GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .                        GAP_S_L,GAP_M_L  ,DRAD   ,IX11,IX12,
     5                        IX13    ,IX14    ,NSVG,X1     ,X2     ,
     6                        X3     ,X4     ,Y1  ,Y2      ,Y3      ,
     7                        Y4     ,Z1     ,Z2  ,Z3      ,Z4      ,
     8                        XI     ,YI     ,ZI  ,STIF    ,DGAPLOAD,
     9                        J_STOK)
                  CALL I7DST3(IX13,IX14,X1 ,X2 ,X3 ,
     1                 X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2                 Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3                 YI ,ZI ,X0 ,Y0 ,Z0 ,
     4                 NX1,NY1,NZ1,NX2,NY2,
     5                 NZ2,NX3,NY3,NZ3,NX4,
     6                 NY4,NZ4,P1 ,P2 ,P3 ,
     7                 P4 ,LB1,LB2,LB3,LB4,
     8                 LC1,LC2,LC3,LC4,J_STOK)
                  CALL I7PEN3(MARGE,GAPV,N11,N21,N31,
     1                 PENE ,NX1 ,NY1,NZ1,NX2,
     2                 NY2  ,NZ2 ,NX3,NY3,NZ3,
     3                 NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4                 P3   ,P4,J_STOK)
                  call inter_save_candidate( local_i_stok,j_stok,prov_n,prov_e,
     .                                       pene,local_cand_n,local_cand_e )
                  J_STOK = 0
                ENDIF

  300           CONTINUE
                JJ = LOCAL_NEXT_NOD(JJ)
              ENDDO ! WHILE(JJ /= 0)
            ENDDO
          ENDDO
        ENDDO

        IF(FLAGREMNODE==2.AND.IREMNODE==2)THEN
          K = KREMNODE(NE)+1
          L = KREMNODE(NE+1)
          DO M=K,L
            TAGNOD(REMNODE(M)) = 0
          ENDDO
        ENDIF

      ENDDO
!$OMP END DO
      DEALLOCATE(TAGNOD)

      IF(J_STOK/=0) THEN
        CALL I7COR3(X    ,IRECT,NSV  ,PROV_E ,PROV_N,
     .              STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .              GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .              GAP_S_L,GAP_M_L  ,DRAD   ,IX11,IX12,
     5              IX13    ,IX14    ,NSVG,X1     ,X2     ,
     6              X3     ,X4     ,Y1  ,Y2      ,Y3      ,
     7              Y4     ,Z1     ,Z2  ,Z3      ,Z4      ,
     8              XI     ,YI     ,ZI  ,STIF    ,DGAPLOAD,
     9              J_STOK)
        CALL I7DST3(IX13,IX14,X1 ,X2 ,X3 ,
     1              X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2              Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3              YI ,ZI ,X0 ,Y0 ,Z0 ,
     4              NX1,NY1,NZ1,NX2,NY2,
     5              NZ2,NX3,NY3,NZ3,NX4,
     6              NY4,NZ4,P1 ,P2 ,P3 ,
     7              P4 ,LB1,LB2,LB3,LB4,
     8              LC1,LC2,LC3,LC4,J_STOK)
        CALL I7PEN3(MARGE,GAPV,N11,N21,N31,
     1              PENE ,NX1 ,NY1,NZ1,NX2,
     2              NY2  ,NZ2 ,NX3,NY3,NZ3,
     3              NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4              P3   ,P4,J_STOK)
        call inter_save_candidate( local_i_stok,j_stok,prov_n,prov_e,
     .                             pene,local_cand_n,local_cand_e )
        J_STOK = 0
      ENDIF

      ! save the local number of candidates
      cand_n_size(itask+1) = local_i_stok
      cand_e_size(itask+1) = local_i_stok
!$OMP BARRIER


!$OMP SINGLE
      ! compute the address for each threads & the total number of candidates
      address_cand_n(1) = 0
      address_cand_e(1) = 0
      ! ------------
      do i=1,nthreads
        address_cand_n(i+1) = cand_n_size(i) + address_cand_n(i) ! address for cand_n
        address_cand_e(i+1) = cand_e_size(i) + address_cand_e(i) ! address for cand_e

        cand_n_size(nthreads+1) = cand_n_size(nthreads+1) + cand_n_size(i) ! total number of cand_n for this proc
        cand_e_size(nthreads+1) = cand_e_size(nthreads+1) + cand_e_size(i) ! total number of cand_e for this proc
      enddo
      ! ------------

      ! ------------
      ! check the size of global cand_n & cand_e
      my_old_size = ipari(18)*ipari(23)
      i_stok_old = i_stok 
      i_stok =  i_stok + cand_n_size(nthreads+1) ! total number of cand_n/cand_e (= sum( cand_n/e per proc))
      if(i_stok > my_old_size) then ! total number of candidates > size of cand_n/e --> need to extend the 2 arrays
        multimp = i_stok/ipari(18) + 1
        call upgrade_multimp(nin,multimp,intbuf_tab)
      endif
      ! ------------
!$OMP END SINGLE

!$OMP BARRIER

      ! ------------
      my_address = i_stok_old + address_cand_n(itask+1) ! get the address for each thread
      intbuf_tab%cand_n(my_address+1:my_address+local_i_stok) = local_cand_n%int_array_1d(1:local_i_stok) ! save the cand_n into the global array
      my_address = i_stok_old + address_cand_e(itask+1) ! get the address for each thread
      intbuf_tab%cand_e(my_address+1:my_address+local_i_stok) = local_cand_e%int_array_1d(1:local_i_stok) ! save the cand_e into the global array
      ! ------------


      call dealloc_1d_array(local_cand_n)
      call dealloc_1d_array(local_cand_e)

!$OMP BARRIER

      ! Sort the candidates to ensure the same domain decomposition
!$OMP SINGLE
      ! ------------
      my_size = cand_n_size(nthreads+1)
      allocate(my_index(2*my_size))
      allocate(sort_array(2,my_size))
      allocate(save_array(2,my_size))
      
      my_address = i_stok_old + address_cand_n(1) ! get the address of the first pair of candidate
      sort_array(1,1:my_size) = intbuf_tab%cand_n(my_address+1:my_address+my_size)
      my_address = i_stok_old + address_cand_e(1) ! get the address of the first pair of candidate
      sort_array(2,1:my_size) = intbuf_tab%cand_e(my_address+1:my_address+my_size)
      do i=1,my_size
        my_index(i) = i
      enddo
      save_array(1:2,1:my_size) = sort_array(1:2,1:my_size)
      mode = 0

      call my_orders( mode,work,sort_array,my_index,my_size,2)
      my_address = i_stok_old + address_cand_n(1) ! get the address of the first pair of candidate
      do i=1,my_size 
        intbuf_tab%cand_n(my_address+i) = save_array(1,my_index(i))
      enddo 
      my_address = i_stok_old + address_cand_e(1) ! get the address of the first pair of candidate
      do i=1,my_size
        intbuf_tab%cand_e(my_address+i) = save_array(2,my_index(i))
      enddo
      deallocate(my_index)
      deallocate(sort_array)
      deallocate(save_array)
      ! ------------
!$OMP END SINGLE    

!$OMP DO SCHEDULE(guided)
      DO I=1,NSN
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
!$OMP END DO

!$OMP SINGLE
      deallocate( cand_n_size,cand_e_size )
      deallocate( address_cand_n,address_cand_e )
!$OMP END SINGLE

      RETURN
      END
