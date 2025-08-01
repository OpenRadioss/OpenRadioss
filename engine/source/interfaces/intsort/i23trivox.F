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
!||    i23trivox       ../engine/source/interfaces/intsort/i23trivox.F
!||--- called by ------------------------------------------------------
!||    i23buce         ../engine/source/interfaces/intsort/i23buce.F
!||--- calls      -----------------------------------------------------
!||    i23sto          ../engine/source/interfaces/int23/i23sto.F
!||    my_barrier      ../engine/source/system/machine.F
!||    spmd_oldnumcd   ../engine/source/mpi/interfaces/spmd_i7tool.F
!||--- uses       -----------------------------------------------------
!||    tri7box         ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I23TRIVOX(
     1      NSN    ,RENUM  ,NSNR    ,ISZNSNR  ,I_MEM    ,
     2      IRECT  ,X      ,STF     ,STFN     ,XYZM     ,
     3      NSV    ,II_STOK,CAND_N  ,ESHIFT   ,CAND_E   ,
     4      MULNSN ,NOINT  ,TZINF   ,MSR      ,
     5      VOXEL  ,NBX    ,NBY     ,NBZ      ,
     6      INACTI ,CAND_A  ,CAND_P   ,IFPEN    ,
     7      NRTM   ,NSNROLD,IGAP    ,GAP      ,GAP_S    ,
     8      GAP_M  ,GAPMIN ,GAPMAX  ,MARGE    ,CURV_MAX ,
     9      NIN    ,ITASK  ,BGAPSMX ,INTHEAT,IDT_THERM,NODADT_THERM)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "task_c.inc"
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
C     NEXT_NOD(i)  noeud suivant dans la meme boite (si /= 0)
C     LAST_NOD(i)  dernier noeud dans la meme boite (si /= 0)
C                  utilise uniquement pour aller directement du premier
C                       noeud au dernier
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NSNROLD,NIN,ITASK,
     .        MULNSN,NOINT,INACTI,NSNR,IGAP,NBX,NBY,NBZ,
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2), MSR(*),II_STOK
      INTEGER, INTENT(IN) :: INTHEAT
      INTEGER, INTENT(IN) :: IDT_THERM
      INTEGER, INTENT(IN) :: NODADT_THERM
C     REAL
      my_real
     .   X(3,*),XYZM(6),CAND_P(*),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MARGE,GAP,GAPMIN,GAPMAX,BGAPSMX,
     .   CURV_MAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,J_STOK,II,JJ,
     .        PROV_N(MVSIZ),PROV_E(MVSIZ),
     .        OLDNUM(ISZNSNR), NSNF, NSNL
C     REAL
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS
c provisoire
      INTEGER  LAST_NOD(NSN+NSNR)
      INTEGER  IX,IY,IZ,NEXT,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA
      INTEGER FIRST,NEW,LAST
      SAVE IIX,IIY,IIZ
C-----------------------------------------------
      IF(ITASK == 0)THEN
        ALLOCATE(NEXT_NOD(NSN+NSNR))
        ALLOCATE(IIX(NSN+NSNR))
        ALLOCATE(IIY(NSN+NSNR))
        ALLOCATE(IIZ(NSN+NSNR))
      END IF
C Barrier to wait init voxel and allocation NEX_NOD
      CALL MY_BARRIER
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
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


C En SPMD, retrouve ancienne numerotation des candidats non locaux
      IF(NSPMD>1) THEN
        CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD,INTHEAT,IDT_THERM,NODADT_THERM)
      END IF

C=======================================================================
C 1   mise des noeuds dans les boites
C=======================================================================
      IF(ITASK == 0)THEN
cc       BGAPSMX = ZERO
        DO I=1,NSN
          IIX(I)=0
          IIY(I)=0
          IIZ(I)=0
          IF(STFN(I) == ZERO)CYCLE
          J=NSV(I)
C Optimisation // recherche les noeuds compris dans xmin xmax des
C elements du processeur
          IF(X(1,J) < XMIN)  CYCLE
          IF(X(1,J) > XMAX)  CYCLE
          IF(X(2,J) < YMIN)  CYCLE
          IF(X(2,J) > YMAX)  CYCLE
          IF(X(3,J) < ZMIN)  CYCLE
          IF(X(3,J) > ZMAX)  CYCLE

          IIX(I)=INT(NBX*(X(1,J)-XMINB)/(XMAXB-XMINB))
          IIY(I)=INT(NBY*(X(2,J)-YMINB)/(YMAXB-YMINB))
          IIZ(I)=INT(NBZ*(X(3,J)-ZMINB)/(ZMAXB-ZMINB))

          IIX(I)=MAX(1,2+MIN(NBX,IIX(I)))
          IIY(I)=MAX(1,2+MIN(NBY,IIY(I)))
          IIZ(I)=MAX(1,2+MIN(NBZ,IIZ(I)))

          FIRST = VOXEL(IIX(I),IIY(I),IIZ(I))
          IF(FIRST == 0)THEN
c         empty cell
            VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
            NEXT_NOD(I)                 = 0 ! last one
            LAST_NOD(I)                 = 0 ! no last
          ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         cell containing one node
c         add as next node
            NEXT_NOD(FIRST) = I ! next
            LAST_NOD(FIRST) = I ! last
            NEXT_NOD(I)     = 0 ! last one
          ELSE
c
c         jump to the last node of the cell
            LAST = LAST_NOD(FIRST) ! last node in this voxel
            NEXT_NOD(LAST)  = I ! next
            LAST_NOD(FIRST) = I ! last
            NEXT_NOD(I)     = 0 ! last one
          ENDIF
        ENDDO
C=======================================================================
C 2   mise des noeuds dans les boites
C     candidats non locaux en SPMD
C=======================================================================
        DO J = 1, NSNR
          IIX(NSN+J)=INT(NBX*(XREM(1,J)-XMINB)/(XMAXB-XMINB))
          IIY(NSN+J)=INT(NBY*(XREM(2,J)-YMINB)/(YMAXB-YMINB))
          IIZ(NSN+J)=INT(NBZ*(XREM(3,J)-ZMINB)/(ZMAXB-ZMINB))
cc        IF(IGAP/=0) BGAPSMX = MAX(BGAPSMX,XREM(12,J))
          IIX(NSN+J)=MAX(1,2+MIN(NBX,IIX(NSN+J)))
          IIY(NSN+J)=MAX(1,2+MIN(NBY,IIY(NSN+J)))
          IIZ(NSN+J)=MAX(1,2+MIN(NBZ,IIZ(NSN+J)))

          FIRST = VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J))
          IF(FIRST == 0)THEN
c         empty cell
            VOXEL(IIX(NSN+J),IIY(NSN+J),IIZ(NSN+J)) = NSN+J ! first
            NEXT_NOD(NSN+J)     = 0 ! last one
            LAST_NOD(NSN+J)     = 0 ! no last
          ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         cell containing one node
c         add as next node
            NEXT_NOD(FIRST) = NSN+J  ! next
            LAST_NOD(FIRST) = NSN+J  ! last
            NEXT_NOD(NSN+J)  = 0     ! last one
          ELSE
c
c         jump to the last node of the cell
            LAST = LAST_NOD(FIRST)  ! last node in this voxel
            NEXT_NOD(LAST)  = NSN+J ! next
            LAST_NOD(FIRST) = NSN+J ! last
            NEXT_NOD(NSN+J)     = 0 ! last one
          ENDIF
        ENDDO
cc       BGAPSMXG=BGAPSMX
      END IF
C Barrier to wait task0 treatment
      CALL MY_BARRIER
cc      BGAPSMX=BGAPSMXG
C=======================================================================
C 3   recherche des boites concernant chaque facette
C     et creation des candidats
C=======================================================================
      J_STOK = 0

      DO NE=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(NE) == ZERO)CYCLE

        IF(IGAP == 0)THEN
          AAA = TZINF+SQRT(THREE)*CURV_MAX(NE)
        ELSE
          AAA = MARGE+SQRT(THREE)*(CURV_MAX(NE)+
     .      MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE))))
        ENDIF

c     il est possible d'ameliorer l'algo en decoupant la facette
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee

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
cc         nnpelem = 0
cc         nnr0pelem = 0
cc         nnrpelem = 0

        DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
            DO IX = IX1,IX2

cc             nbpelem = nbpelem + 1

              JJ = VOXEL(IX,IY,IZ)

              DO WHILE(JJ /= 0)

cc             nnpelem = nnpelem + 1

                IF(JJ<=NSN)THEN
                  NN=NSV(JJ)
                  IF(NN == M1)GOTO 200
                  IF(NN == M2)GOTO 200
                  IF(NN == M3)GOTO 200
                  IF(NN == M4)GOTO 200
                  XS = X(1,NN)
                  YS = X(2,NN)
                  ZS = X(3,NN)
                  IF(IGAP /= 0)THEN
                    AAA = MARGE+
     .                SQRT(THREE)*(CURV_MAX(NE)+MIN(GAPMAX,MAX(GAPMIN,
     .                GAP_S(JJ)+GAP_M(NE))))
                  ENDIF
                ELSE
                  J=JJ-NSN
                  XS = XREM(1,J)
                  YS = XREM(2,J)
                  ZS = XREM(3,J)
                  IF(IGAP /= 0)THEN
                    AAA = MARGE+
     .                SQRT(THREE)*(CURV_MAX(NE)+MIN(GAPMAX,MAX(GAPMIN,
     .                XREM(9,J)+GAP_M(NE))))
                  ENDIF
                ENDIF


                IF(XS<=XMINE-AAA)GOTO 200
                IF(XS>=XMAXE+AAA)GOTO 200
                IF(YS<=YMINE-AAA)GOTO 200
                IF(YS>=YMAXE+AAA)GOTO 200
                IF(ZS<=ZMINE-AAA)GOTO 200
                IF(ZS>=ZMAXE+AAA)GOTO 200

c    sousestimation de la distance**2 pour elimination de candidats

cc             nnr0pelem = nnr0pelem + 1

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
c                   CALL I7STOVOX(
                  CALL I23STO(
     1            NVSIZ   ,IRECT  ,X        ,NSV     ,II_STOK,
     2            CAND_N ,CAND_E ,MULNSN     ,NOINT   ,MARGE  ,
     3            I_MEM  ,PROV_N ,PROV_E     ,ESHIFT  ,INACTI ,
     4            IGAP     ,GAP  ,GAP_S      ,GAP_M   ,GAPMIN ,
     5            GAPMAX ,CURV_MAX ,MSR      ,NSN     ,OLDNUM ,
     6            NSNROLD,CAND_A   ,IFPEN    ,CAND_P  )
                  IF(I_MEM==2)GOTO 100
                  J_STOK = 0
                ENDIF

  200           CONTINUE

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

C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
c      IF(J_STOK/=0)CALL I7STOVOX(
      IF(J_STOK/=0)CALL I23STO(
     1      J_STOK   ,IRECT  ,X        ,NSV     ,II_STOK,
     2      CAND_N ,CAND_E ,MULNSN     ,NOINT   ,MARGE  ,
     3      I_MEM  ,PROV_N ,PROV_E     ,ESHIFT  ,INACTI ,
     4      IGAP     ,GAP  ,GAP_S      ,GAP_M   ,GAPMIN ,
     5      GAPMAX ,CURV_MAX ,MSR      ,NSN     ,OLDNUM ,
     6      NSNROLD,CAND_A   ,IFPEN    ,CAND_P  )

C=======================================================================
C 4   remise a zero des noeuds dans les boites
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
C 5   remise a zero des noeuds dans les boites
C     candidats non locaux en SPMD
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
      ENDIF

      RETURN
      END

