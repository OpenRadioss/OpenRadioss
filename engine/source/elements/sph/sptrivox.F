Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_SPHGETDK                 source/mpi/elements/spmd_sph.F
Chd|        SPPRO3                        source/elements/sph/sppro3.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPTRIVOX(
     1      NSN    ,NSNR    ,X       ,BMINMA  ,NOD2SP  ,
     2      NBX    ,NBY     ,NBZ     ,MARGE   ,ITASK   ,
     3      NLIST  ,SPBUF   ,JVOIS   ,JSTOR   ,JPERM   ,
     4      DVOIS  ,IREDUCE ,NSP2SORTF,NSP2SORTL,VOXEL ,
     5      KXSP   ,IXSP    ,KREDUCE ,LGAUGE  ,GAUGE   ,
     6      KXSPR  ,IXSPR   )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
c     parameter setting the size for the vector (orig version is 128)
      INTEGER NVECSZ ,NSP2SORTF, NSP2SORTL
      PARAMETER (NVECSZ = MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
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
      INTEGER NSN,ITASK,NSNR,NBX,NBY,NBZ,
     .        NLIST(*),NOD2SP(*) ,
     .        VOXEL(NBX+2,NBY+2,NBZ+2),JVOIS(*)  ,JSTOR(*), JPERM(*) ,
     .        IREDUCE,KXSP(NISP,*), IXSP(KVOISPH,*), KREDUCE(*),
     .        LGAUGE(3,*),KXSPR(*),IXSPR(KVOISPH,*)
C     REAL
      my_real
     .   X(3,*),BMINMA(12),
     .   MARGE ,SPBUF(NSPBUF,*), DVOIS(*), GAUGE(LLGAUGE,*)
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
      my_real, DIMENSION(:), ALLOCATABLE :: TAB_DK
c provisoire
      INTEGER  LAST_NOD(NSN+NSNR)
      INTEGER  IX,IY,IZ,NEXT,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA,BBB,
     .   AAA2
      INTEGER FIRST,NEW,LAST,REQ_RECV(NSPMD)
      SAVE IIX,IIY,IIZ,DISTMAX,TAB_DK
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: NEXT_NOD_LOCAL
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: VOXEL_LOCAL
C-----------------------------------------------
      IF(ITASK == 0)THEN
        ALLOCATE(NEXT_NOD(NSN+NSNR))
        ALLOCATE(IIX(NSN+NSNR))
        ALLOCATE(IIY(NSN+NSNR))
        ALLOCATE(IIZ(NSN+NSNR))
        ALLOCATE(TAB_DK(NUMSPH))
        ALLOCATE(NEXT_NOD_LOCAL(NSN))
        ALLOCATE( VOXEL_LOCAL(NBX+2,NBY+2,NBZ+2) )
      END IF
C Barrier to wait init voxel and allocation NEX_NOD
      CALL MY_BARRIER
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
C
      ALPHA_MARGE = SQRT(ONE +SPASORT)

      XMIN = BMINMA(4)
      YMIN = BMINMA(5)
      ZMIN = BMINMA(6)
      XMAX = BMINMA(1)
      YMAX = BMINMA(2)
      ZMAX = BMINMA(3)

      XMINB = BMINMA(10)
      YMINB = BMINMA(11)
      ZMINB = BMINMA(12)
      XMAXB = BMINMA(7)
      YMAXB = BMINMA(8)
      ZMAXB = BMINMA(9)

C=======================================================================
C 1   mise des noeuds dans les boites
C=======================================================================
      IF(ITASK == 0)THEN

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
c         boite vide
          VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
          NEXT_NOD(I)                 = 0 ! pas de next
          LAST_NOD(I)                 = 0 ! pas de last
        ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         boite contenant un seul noeud
c         on ajoute comme suivant
          NEXT_NOD(FIRST) = I ! next
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(I)     = 0 ! pas de next apres I
        ELSE
c         boite contenant plusieurs noeuds
c         on saute directement au dernier
          LAST = LAST_NOD(FIRST) ! dernier noeud de la boite
          NEXT_NOD(LAST)  = I ! next apres precedant
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(I)     = 0 ! pas de next apres I
        ENDIF
       ENDDO
       VOXEL_LOCAL(1:NBX+2,1:NBY+2,1:NBZ+2) = VOXEL(1:NBX+2,1:NBY+2,1:NBZ+2)
       NEXT_NOD_LOCAL(1:NSN) = NEXT_NOD(1:NSN)
C=======================================================================
C 2   mise des noeuds dans les boites
C     candidats non locaux en SPMD
C=======================================================================
       DO J = 1, NSNR
        I = J+NSN
        N = NLIST(I)-NUMSPH
        DISTMAX = MAX(DISTMAX,XSPHR(2,N))
        IIX(I)=INT(NBX*(XSPHR(3,N)-XMINB)/(XMAXB-XMINB))
        IIY(I)=INT(NBY*(XSPHR(4,N)-YMINB)/(YMAXB-YMINB))
        IIZ(I)=INT(NBZ*(XSPHR(5,N)-ZMINB)/(ZMAXB-ZMINB))
        IIX(I)=MAX(1,2+MIN(NBX,IIX(I)))
        IIY(I)=MAX(1,2+MIN(NBY,IIY(I)))
        IIZ(I)=MAX(1,2+MIN(NBZ,IIZ(I)))

        FIRST = VOXEL(IIX(I),IIY(I),IIZ(I))
        IF(FIRST == 0)THEN
c         boite vide
          VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
          NEXT_NOD(I)     = 0 ! pas de next
          LAST_NOD(I)     = 0 ! pas de last
        ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         boite contenant un seul noeud
c         on ajoute comme suivant
          NEXT_NOD(FIRST) = I  ! next
          LAST_NOD(FIRST) = I  ! last
          NEXT_NOD(NSN+J) = 0     ! pas de next apres I
        ELSE
c         boite contenant plusieurs noeuds
c         on saute directement au dernier
          LAST = LAST_NOD(FIRST)  ! dernier noeud de la boite
          NEXT_NOD(LAST)  = I ! next apres precedant
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(NSN+J) = 0 ! pas de next apres I
        ENDIF
       ENDDO
      END IF
C ==============================================================
C  Prepare reception of DK after reduction to remotes
C ==============================================================
      CALL MY_BARRIER
      IF(ITASK == 0 ) THEN
        CALL SPMD_SPHGETDK(TAB_DK,1,REQ_RECV)
      ENDIF
      CALL MY_BARRIER

C=======================================================================
C 3   recherche des boites concernant chaque facette
C     et creation des candidats
C=======================================================================
      DO NE = NSP2SORTF,NSP2SORTL
C on ne retient pas les facettes detruites
c        IF(STF(NE) == ZERO)CYCLE

         J=NLIST(NE)
         NN = KXSP(3,J)

c a revoir !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         AAA = TWO*SPBUF(1,J)* ALPHA_MARGE

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
               IF(JJ<=NSN)THEN
                 IF(SPBUF(1,JS) > SPBUF(1,J) .OR.
     .             (SPBUF(1,JS) == SPBUF(1,J) .AND. KXSP(8,JS)>=KXSP(8,J)))GOTO 200

                 NS=KXSP(3,JS)	
                 XS = X(1,NS)
                 YS = X(2,NS)
                 ZS = X(3,NS)

                 AAA = SPBUF(1,J)+SPBUF(1,JS)
               ELSE
                 N = NLIST(JJ)-NUMSPH
                 IF(XSPHR(2,N) > SPBUF(1,J).OR.
     .               (XSPHR(2,N) == SPBUF(1,J) .AND.(NINT(XSPHR(6,N))>=KXSP(8,J) ) ) ) GOTO 200

                 XS = XSPHR(3,N)
                 YS = XSPHR(4,N)
                 ZS = XSPHR(5,N)
                 AAA = SPBUF(1,J)+XSPHR(2,N)
               ENDIF

               BBB = AAA * ALPHA_MARGE

               IF(XS<=X(1,NN)-BBB)GOTO 200
               IF(XS>=X(1,NN)+BBB)GOTO 200
               IF(YS<=X(2,NN)-BBB)GOTO 200
               IF(YS>=X(2,NN)+BBB)GOTO 200
               IF(ZS<=X(3,NN)-BBB)GOTO 200
               IF(ZS>=X(3,NN)+BBB)GOTO 200

cc             nnr0pelem = nnr0pelem + 1

C symetrie parfaite <=> XJ-XI
               D1X = XS - X(1,NN)
               D1Y = YS - X(2,NN)
               D1Z = ZS - X(3,NN)
               D2 = D1X*D1X+D1Y*D1Y+D1Z*D1Z
               A2 = BBB*BBB
               IF(D2 > A2)GOTO 200

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

cc             nbpelg = nbpelg + nbpelem
cc             nnpelg = nnpelg + nnpelem
cc             nnrpelg = nnrpelg + nnrpelem
cc             nnr0pelg = nnr0pelg + nnr0pelem
         CALL SPPRO3(J    ,KXSP  ,IXSP ,NOD2SP,JVOIS,
     .               JSTOR,JPERM ,DVOIS,IREDUCE,KREDUCE,
     .               KXSPR,IXSPR,TAB_DK)

      ENDDO

C ==============================================================
C  Communicate DK after reduction to remotes
C ==============================================================
      CALL MY_BARRIER
      IF(ITASK == 0 ) THEN
        CALL SPMD_SPHGETDK(TAB_DK,2,REQ_RECV)
      ENDIF
      CALL MY_BARRIER

C=======================================================================
C 3   recherche des boites concernant chaque facette
C     et creation des candidats wrt particules remote (sym  trie)
C=======================================================================
       DO J = ITASK+1, NSNR, NTHREAD

        I = J+NSN
        N = NLIST(I)-NUMSPH

         AAA = TWO * XSPHR(2,N) * ALPHA_MARGE

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XSPHR(3,N)-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(XSPHR(4,N)-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(XSPHR(5,N)-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(1,2+MIN(NBX,IX1))
         IY1=MAX(1,2+MIN(NBY,IY1))
         IZ1=MAX(1,2+MIN(NBZ,IZ1))

         IX2=INT(NBX*(XSPHR(3,N)+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(XSPHR(4,N)+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(XSPHR(5,N)+AAA-ZMINB)/(ZMAXB-ZMINB))

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

             JJ = VOXEL_LOCAL(IX,IY,IZ)

             DO WHILE(JJ /= 0)

cc             nnpelem = nnpelem + 1

               JS=NLIST(JJ)
               IF(JJ<=NSN)THEN
                 IF(XSPHR(2,N) < SPBUF(1,JS).OR.
     .              (XSPHR(2,N)==SPBUF(1,JS).AND.NINT(XSPHR(6,N))<KXSP(8,JS)) )GOTO 250

                 NS=KXSP(3,JS)
                 XS = X(1,NS)
                 YS = X(2,NS)
                 ZS = X(3,NS)

                 AAA = XSPHR(2,N)+SPBUF(1,JS)


                 BBB = AAA * ALPHA_MARGE

                 IF(XS<=XSPHR(3,N)-BBB)GOTO 250
                 IF(XS>=XSPHR(3,N)+BBB)GOTO 250
                 IF(YS<=XSPHR(4,N)-BBB)GOTO 250
                 IF(YS>=XSPHR(4,N)+BBB)GOTO 250
                 IF(ZS<=XSPHR(5,N)-BBB)GOTO 250
                 IF(ZS>=XSPHR(5,N)+BBB)GOTO 250

cc               nnr0pelem = nnr0pelem + 1

C symetrie parfaite <=> XI-XJ
                 D1X = XSPHR(3,N) - XS
                 D1Y = XSPHR(4,N) - YS
                 D1Z = XSPHR(5,N) - ZS
                 D2 = D1X*D1X+D1Y*D1Y+D1Z*D1Z
                 A2 = BBB*BBB
                 IF(D2 > A2)GOTO 250

cc               nnrpelem = nnrpelem + 1
                   AAA2 = AAA*AAA
                   NVOIS=KXSPR(J)+1
                   JVOIS(NVOIS)=JS
                   DVOIS(NVOIS)=D2/AAA2

                   KXSPR(J)  =NVOIS

 250             CONTINUE
               END IF

               JJ = NEXT_NOD_LOCAL(JJ)

             ENDDO ! WHILE(JJ /= 0)

           ENDDO
          ENDDO
         ENDDO

cc             nbpelg = nbpelg + nbpelem
cc             nnpelg = nnpelg + nnpelem
cc             nnrpelg = nnrpelg + nnrpelem
cc             nnr0pelg = nnr0pelg + nnr0pelem
         CALL SPPRO3(J+NUMSPH,KXSP  ,IXSP ,NOD2SP,JVOIS,
     .               JSTOR,JPERM ,DVOIS,IREDUCE,KREDUCE,
     .               KXSPR,IXSPR,TAB_DK)

      ENDDO
C-------------------------------------------------------------------------
C     GAUGES
C-------------------------------------------------------------------------

!$OMP DO SCHEDULE(DYNAMIC,1)
      DO IG = 1,NBGAUGE

         IF(LGAUGE(1,IG) > -(NUMELS+1))CYCLE

         J = NUMSPH+IG
         XN=GAUGE(2,IG)
         YN=GAUGE(3,IG)
         ZN=GAUGE(4,IG)
c a revoir !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         AAA = (TWO*DISTMAX)* ALPHA_MARGE

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XN-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(YN-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(ZN-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(1,2+MIN(NBX,IX1))
         IY1=MAX(1,2+MIN(NBY,IY1))
         IZ1=MAX(1,2+MIN(NBZ,IZ1))

         IX2=INT(NBX*(XN+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(YN+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(ZN+AAA-ZMINB)/(ZMAXB-ZMINB))

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

             JJ = VOXEL_LOCAL(IX,IY,IZ)

             DO WHILE(JJ /= 0)

cc             nnpelem = nnpelem + 1

               JS=NLIST(JJ)
               IF(JJ<=NSN)THEN
                 NS=KXSP(3,JS)	
                 XS = X(1,NS)
                 YS = X(2,NS)
                 ZS = X(3,NS)

                 AAA = TWO*SPBUF(1,JS)
               ELSE
                 GOTO 300
               ENDIF

               BBB = AAA * ALPHA_MARGE

               IF(XS<=XN-BBB)GOTO 300
               IF(XS>=XN+BBB)GOTO 300
               IF(YS<=YN-BBB)GOTO 300
               IF(YS>=YN+BBB)GOTO 300
               IF(ZS<=ZN-BBB)GOTO 300
               IF(ZS>=ZN+BBB)GOTO 300

cc             nnr0pelem = nnr0pelem + 1

               D1X = XS - XN
               D1Y = YS - YN
               D1Z = ZS - ZN
               D2 = D1X*D1X+D1Y*D1Y+D1Z*D1Z
               A2 = BBB*BBB
               IF(D2 > A2)GOTO 300

cc             nnrpelem = nnrpelem + 1
                 AAA2 = AAA*AAA
                 NVOIS=KXSP(5,J)+1
                 JVOIS(NVOIS)=JS
                 DVOIS(NVOIS)=D2/AAA2

                 KXSP(5,J)  =NVOIS

 300           CONTINUE

               JJ = NEXT_NOD_LOCAL(JJ)

             ENDDO ! WHILE(JJ /= 0)

           ENDDO
          ENDDO
         ENDDO

cc             nbpelg = nbpelg + nbpelem
cc             nnpelg = nnpelg + nnpelem
cc             nnrpelg = nnrpelg + nnrpelem
cc             nnr0pelg = nnr0pelg + nnr0pelem
         IL=-J
         CALL SPPRO3(IL   ,KXSP  ,IXSP ,NOD2SP,JVOIS,
     .               JSTOR,JPERM ,DVOIS,IREDUCE,KREDUCE,
     .               KXSPR,IXSPR, TAB_DK)

      ENDDO
!$OMP END DO

C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
C=======================================================================
C 4   remise a zero des noeuds dans les boites
C=======================================================================
 100  CONTINUE

C Barrier to avoid reinitialization before end of sorting
      CALL MY_BARRIER

      DO I=NSP2SORTF,NSP2SORTL
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
C=======================================================================
C 5   remise a zero des noeuds dans les boites
C     candidats non locaux en SPMD
C================================================================       
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
        DEALLOCATE(TAB_DK)
        DEALLOCATE( NEXT_NOD_LOCAL )
        DEALLOCATE( VOXEL_LOCAL )
      ENDIF

      RETURN
      END

