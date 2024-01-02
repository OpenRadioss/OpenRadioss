Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SPTRIVOX                      source/elements/sph/sptrivox.F
Chd|-- called by -----------
Chd|        SPBUC31                       source/elements/sph/spbuc31.F 
Chd|-- calls ---------------
Chd|        SPPRO31                       source/elements/sph/spbuc31.F 
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE SPTRIVOX(
     1      NSN    ,X       ,BMINMA  ,NOD2SP  ,
     2      NBX    ,NBY     ,NBZ     ,
     3      NLIST  ,SPBUF   ,JVOIS   ,JSTOR   ,JPERM   ,
     4      DVOIS  ,IREDUCE ,NSPHACTF,NSPHACTL,VOXEL   ,
     5      KXSP   ,IXSP    ,KREDUCE )
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
      INTEGER NSPHACTF, NSPHACTL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
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
C     X(3,*)       COORDONNEES NODALES               E
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     VOXEL(ix,iy,iz) contient le numero local du premier noeud de
C                  la boite
C     NEXT_NOD(i)  noeud suivant dans la meme boite (si /= 0)
C     LAST_NOD(i)  dernier noeud dans la meme boite (si /= 0)
C                  utilise uniquement pour aller directement du premier
C                       noeud au dernier
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NBX,NBY,NBZ,
     .        NLIST(*),NOD2SP(*) ,
     .        VOXEL(NBX+2,NBY+2,NBZ+2),JVOIS(*)  ,JSTOR(*), JPERM(*) ,
     .        IREDUCE,KXSP(NISP,*), IXSP(KVOISPH,*), KREDUCE(*)
C     REAL
      my_real
     .   X(3,*),BMINMA(6),
     .   SPBUF(NSPBUF,*),DVOIS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,II,JJ,JS,NS,N,
     .        NSNF, NSNL,NVOIS, IG, IL
C     REAL
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,XN,YN,ZN,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ,
     .   D1X,D1Y,D1Z,D2,A2,ALPHA_MARGE,DISTMAX
c provisoire
      INTEGER  LAST_NOD(NSN)
      INTEGER  IX,IY,IZ,NEXT,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA,BBB,
     .   AAA2
      INTEGER FIRST,NEW,LAST
      SAVE IIX,IIY,IIZ,DISTMAX
C-----------------------------------------------
C      IF(ITASK == 0)THEN
      ALLOCATE(NEXT_NOD(NSN))
      ALLOCATE(IIX(NSN))
      ALLOCATE(IIY(NSN))
      ALLOCATE(IIZ(NSN))
C      END IF
C Barrier to wait init voxel and allocation NEX_NOD
C      CALL MY_BARRIER
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
C
      ALPHA_MARGE = SQRT(ONE +SPASORT)

      XMAX = BMINMA(1)
      YMAX = BMINMA(2)
      ZMAX = BMINMA(3)
      XMIN = BMINMA(4)
      YMIN = BMINMA(5)
      ZMIN = BMINMA(6)

c     dev future: xminb plus grand que xmin...
      XMINB = XMIN
      YMINB = YMIN
      ZMINB = ZMIN
      XMAXB = XMAX
      YMAXB = YMAX
      ZMAXB = ZMAX

C=======================================================================
C 1   mise des noeuds dans les boites
C=======================================================================
C     IF(ITASK == 0)THEN

      DISTMAX = ZERO

      DO I=1,NSN
        IIX(I)=0
        IIY(I)=0
        IIZ(I)=0

        J=NLIST(I)
        NN = KXSP(3,J)

        DISTMAX = MAX(DISTMAX,SPBUF(1,J))
C Optimisation // recherche les noeuds compris dans xmin xmax des
C elements du processeur
        IF(X(1,NN) < XMIN)  CYCLE
        IF(X(1,NN) > XMAX)  CYCLE
        IF(X(2,NN) < YMIN)  CYCLE
        IF(X(2,NN) > YMAX)  CYCLE
        IF(X(3,NN) < ZMIN)  CYCLE
        IF(X(3,NN) > ZMAX)  CYCLE

        IIX(I)=INT(NBX*(X(1,NN)-XMINB)/(XMAXB-XMINB))
        IIY(I)=INT(NBY*(X(2,NN)-YMINB)/(YMAXB-YMINB))
        IIZ(I)=INT(NBZ*(X(3,NN)-ZMINB)/(ZMAXB-ZMINB))

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
C     END IF
C Barrier to wait task0 treatment
C     CALL MY_BARRIER
C=======================================================================
C 3   recherche des boites concernant chaque facette
C     et creation des candidats
C=======================================================================
      DO NE = NSPHACTF,NSPHACTL
C on ne retient pas les facettes detruites
c        IF(STF(NE) == ZERO)CYCLE

        J=NLIST(NE)
        NN = KXSP(3,J)

c a revoir !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AAA = (SPBUF(1,J)+DISTMAX)* ALPHA_MARGE

c        indice des voxels occupes par la facette

        IX1=INT(NBX*(X(1,NN)-AAA-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(X(2,NN)-AAA-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(X(3,NN)-AAA-ZMINB)/(ZMAXB-ZMINB))

        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))

        IX2=INT(NBX*(X(1,NN)+AAA-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(X(2,NN)+AAA-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(X(3,NN)+AAA-ZMINB)/(ZMAXB-ZMINB))

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

                JS=NLIST(JJ)
                NS=KXSP(3,JS)
                IF(JJ == NE)GOTO 200
                XS = X(1,NS)
                YS = X(2,NS)
                ZS = X(3,NS)

                AAA = SPBUF(1,J)+SPBUF(1,JS)

                BBB = AAA * ALPHA_MARGE

                IF(XS<=X(1,NN)-BBB)GOTO 200
                IF(XS>=X(1,NN)+BBB)GOTO 200
                IF(YS<=X(2,NN)-BBB)GOTO 200
                IF(YS>=X(2,NN)+BBB)GOTO 200
                IF(ZS<=X(3,NN)-BBB)GOTO 200
                IF(ZS>=X(3,NN)+BBB)GOTO 200

cc             nnr0pelem = nnr0pelem + 1

                D1X = XS - X(1,NN)
                D1Y = YS - X(2,NN)
                D1Z = ZS - X(3,NN)
                D2 = D1X*D1X+D1Y*D1Y+D1Z*D1Z
                A2 = BBB*BBB
                IF(JS==J.or.D2 > A2)GOTO 200

cc             nnrpelem = nnrpelem + 1
                AAA2 = AAA*AAA
                NVOIS=KXSP(5,J)+1
                JVOIS(NVOIS)=JS
                DVOIS(NVOIS)=D2/AAA2

                KXSP(5,J)  =NVOIS

  200           CONTINUE

                JJ = NEXT_NOD(JJ)

              ENDDO ! WHILE(JJ /= 0)

            ENDDO
          ENDDO
        ENDDO

cc       nbpelg = nbpelg + nbpelem
cc       nnpelg = nnpelg + nnpelem
cc       nnrpelg = nnrpelg + nnrpelem
cc       nnr0pelg = nnr0pelg + nnr0pelem
        CALL SPPRO31(J    ,KXSP  ,IXSP ,NOD2SP,JVOIS,
     .               JSTOR,JPERM ,DVOIS,IREDUCE,KREDUCE)

      ENDDO

C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
C=======================================================================
C 4   remise a zero des noeuds dans les boites
C=======================================================================
  100 CONTINUE

C Barrier to avoid reinitialization before end of sorting
C     CALL MY_BARRIER

      DO I=NSPHACTF,NSPHACTL
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
C=======================================================================
C
C     CALL MY_BARRIER()
C     IF(ITASK == 0)THEN
      DEALLOCATE(NEXT_NOD)
      DEALLOCATE(IIX)
      DEALLOCATE(IIY)
      DEALLOCATE(IIZ)
C     ENDIF

      RETURN
      END

