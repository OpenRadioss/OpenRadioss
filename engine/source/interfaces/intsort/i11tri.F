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
Chd|  I11TRI                        source/interfaces/intsort/i11tri.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        I11STO                        source/interfaces/intsort/i11sto.F
Chd|        I7DSTK                        source/interfaces/intsort/i7dstk.F
Chd|        I11INSID                      source/interfaces/intsort/i11tri.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11TRI(
     1      ADD   ,
     2      IRECTS,IRECTM,X     ,NRTM     ,NRTSR,
     3      XYZM  ,I_ADD ,MAXSIZ,II_STOK  ,CAND_S,
     4      CAND_M,NSN4  ,NOINT ,TZINF    ,MAXBOX,
     5      MINBOX,I_MEM ,NB_N_B,I_ADD_MAX,ESHIFT,
     6      ADDCM ,CHAINE,NRTS  ,ITAB     ,NB_OLD,
     7      STFS  ,STFM  ,IAUTO ,NIN      ,IFPEN ,
     8      IFORM)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES ELETS DE BPE ET LES NOEUDS DE BPN EN DEUX ZONES
C   > OU < A UNE FRONTIERE ICI DETERMINEE ET SORT LE TOUT
C   DANS bpe,hpe, et bpn,hpn
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     BPE          TABLEAU DES FACETTES A TRIER      => Local
C                  ET DU RESULTAT COTE MAX            
C     PE           TABLEAU DES FACETTES              => Local
C                  RESULTAT COTE MIN
C     BPN          TABLEAU DES NOEUDS A TRIER        => Local
C                  ET DU RESULTAT COTE MAX            
C     PN           TABLEAU DES NOEUDS                => Local
C                  RESULTAT COTE MIN
C     ADD(2,*)     TABLEAU DES ADRESSES              E/S 
C          1.......ADRESSES NOEUDS
C          2.......ADRESSES ELEMENTS
C     ZYZM(6,*)     TABLEAU DES XYZMIN               E/S 
C          1.......XMIN BOITE
C          2.......YMIN BOITE
C          3.......ZMIN BOITE
C          4.......XMAX BOITE
C          5.......YMAX BOITE
C          6.......ZMAX BOITE
C     IRECTM(2,*)   TABLEAU DES CONEC         E
C     IRECTS(2,*)   TABLEAU DES CONEC         E
C     X(3,*)       COORDONNEES NODALES               E
C     NB_NC        NOMBRE DE NOEUDS CANDIDATS        => Local
C     NB_EC        NOMBRE D'ELTS CANDIDATS           => Local
C     I_ADD        POSITION DANS LE TAB DES ADRESSES E/S
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     I_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     ADNSTK       adresse courante dans la boite des noeuds
C     CAND_S       boites resultats noeuds
C     ADESTK       adresse courante dans la boite des elements
C     CAND_M       adresses des boites resultat elements
C     NSN4         4*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C     MAXBOX       TAILLE MAX BUCKET
C     MINBOX       TAILLE MIN BUCKET
C
C     PROV_S       CAND_S provisoire (variable static dans i7tri)
C     PROV_M       CAND_M provisoire (variable static dans i7tri)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NRTSR,I_ADD,MAXSIZ,I_MEM,ESHIFT,NRTS,
     .        NSN4,NB_N_B,NOINT,I_ADD_MAX,IAUTO ,NIN,
     .        ADD(2,*),IRECTS(2,*),IRECTM(2,*),
     .        CAND_S(*),CAND_M(*),ADDCM(*),CHAINE(2,*),ITAB(*),
     .        NB_OLD(2,*),IFPEN(*),IFORM,II_STOK
C     REAL ou REAL*8
      my_real
     .   X(3,*),XYZM(6,*),STFS(*),STFM(*),
     .   TZINF,MAXBOX,MINBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,NN1,NN2,
     .        N1,N2,N3,N4,NN,NE,K_STOK,K,L,NCAND_PROV,J_STOK,NI,
     .        ISTOP,NB_ECN1,PROV_S(2*MVSIZ),PROV_M(2*MVSIZ),
     .        NB_NC_OLD, NB_EC_OLD, NB_NC, NB_EC,JJ,KK,
C BPE : utilise sur NRTM et non NRTM + 100
C BPN : utilise sur NRTS et non NRTS + 100
     .        BPE(NRTM+100),PE(MAXSIZ),BPN(NRTS+NRTSR+100),PN(MAXSIZ)
C     REAL ou REAL*8
      my_real
     .        DX,DY,DZ,DSUP,SEUIL,SEUILS,SEUILI, XX1, XX2, XX3, XX4,
     .        XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, XX,YY,ZZ,
     .        XMINS,YMINS,ZMINS,XMAXS,YMAXS,ZMAXS,
     .        YY1,YY2,ZZ1,ZZ2,DMX,DMY,DMZ,
     .        XY1,XY2,XZ1,XZ2,XIMIN,XIMAX,XJMIN,XJMAX,XKMIN,XKMAX,
     .        TIMIN,TIMAX,TJMIN,TJMAX,TKMIN,TKMAX,TSMIN,TSMAX,
     .        TXMIN, TXMAX,TYMIN, TYMAX,TZMIN, TZMAX
      EXTERNAL I11INSID
      LOGICAL I11INSID
C-----------------------------------------------
C
C Phase initiale de construction de BPE et BPN deplacee de I11BUCE => I11TRI
C
C recuperation des bornes du domaine
C
      XMIN = XYZM(1,I_ADD)
      YMIN = XYZM(2,I_ADD)
      ZMIN = XYZM(3,I_ADD)
      XMAX = XYZM(4,I_ADD)
      YMAX = XYZM(5,I_ADD)
      ZMAX = XYZM(6,I_ADD)
C Copie des nos de segments et de noeuds dans BPE et BPN
C
      NB_EC = 0
      DO I=1,NRTM
C on ne retient plus les facettes detruites
        IF(STFM(I).NE.ZERO)THEN
          NB_EC = NB_EC + 1
          BPE(NB_EC) = I
        END IF
      ENDDO
C
C Optimisation // recherche les noeuds compris dans xmin xmax des 
C elements du processeur
C
      NB_NC = 0
      DO I=1,NRTS
C on ne retient pas les facettes detruites
       IF(STFS(I).NE.ZERO)THEN
        N1=IRECTS(1,I)
        N2=IRECTS(2,I)
        XMINS = MIN(X(1,N1),X(1,N2))
        YMINS = MIN(X(2,N1),X(2,N2))
        ZMINS = MIN(X(3,N1),X(3,N2))
        XMAXS = MAX(X(1,N1),X(1,N2))
        YMAXS = MAX(X(2,N1),X(2,N2))
        ZMAXS = MAX(X(3,N1),X(3,N2))
        IF(XMAXS.GE.XMIN.AND.XMINS.LE.XMAX.AND.
     .     YMAXS.GE.YMIN.AND.YMINS.LE.YMAX.AND.
     .     ZMAXS.GE.ZMIN.AND.ZMINS.LE.ZMAX)THEN
          NB_NC = NB_NC + 1
          BPN(NB_NC) = I
        ENDIF
       END IF
      ENDDO
C
C Prise en compte candidats non locaux en SPMD
C
      DO I = NRTS+1, NRTS+NRTSR
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C     GOTO 200:
C     INTERFACE AVEC 1 SEGMENT ET 1 NOEUD + INITIALISATION DX DY DZ
C
      J_STOK = 0
      ISTOP = 0
      NB_NC_OLD = 0
      NB_EC_OLD = 0
C
      NB_OLD(1,I_ADD) = 0
      NB_OLD(2,I_ADD) = 0

      DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
      DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
      DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
      DSUP= MAX(DX,DY,DZ) 
      GOTO 200
C===============
 100  CONTINUE
C===============
C-----------------------------------------------------------
C
C
C    1- PHASE DE TRI SUR LA MEDIANE SELON LA + GDE DIRECTION
C                    
C                   
C-----------------------------------------------------------
C
C    1- DETERMINER LA DIRECTION A DIVISER X,Y OU Z
C
      XMIN = 1.E30
      XMAX = -1.E30

      YMIN = 1.E30
      YMAX = -1.E30

      ZMIN = 1.E30
      ZMAX = -1.E30

      DO I=1,NB_EC
        NE = BPE(I)
        XX1=X(1, IRECTM(1,NE))
        XX2=X(1, IRECTM(2,NE))
        XMIN=MIN(XMIN,XX1,XX2)
        XMAX=MAX(XMAX,XX1,XX2)

        YY1=X(2, IRECTM(1,NE))
        YY2=X(2, IRECTM(2,NE))
        YMIN=MIN(YMIN,YY1,YY2)
        YMAX=MAX(YMAX,YY1,YY2)

        ZZ1=X(3, IRECTM(1,NE))
        ZZ2=X(3, IRECTM(2,NE))
        ZMIN=MIN(ZMIN,ZZ1,ZZ2)
        ZMAX=MAX(ZMAX,ZZ1,ZZ2)
      ENDDO

c     reduction de la taille de boite:
c     on garde une marge de TZINF quand on reduit la taille de boite
c     pour eviter d'oublier des esclaves
c
c      |      Tzinf Tzinf                 |Tzinf
c      |     <-----x----->                |<---->
c      |     .............................|............Tymax ^
c      |     .                            |     .            |
c      |     .           #################|#####.##          | Tzinf
c      |     .           #////////////////|/////./#          |
c -----+----------------------------------+---------Ymax=    v
c      |     .     |\\\\\#///  espace   //|/////./# Ymax_old
c      |     .     |\\\\\#/// occupe par//|/////./#
c      |     .     |\\\\\#///les maitres//|/////./#
c      |     .     |\\\\\#////////////////|/////./#
c      |     .     |\\\\\#////////////////|/////./#
c      |     .     |\\\\\#################|#####.##          ^
c      |     .     |\\\  espace retenu  \\|     .            |
c      |     .     |\\\pour les esclaves\\|     .            | Tzinf
c      |     .     |\\\(nouvelle boite) \\|     .            |
c      |     .     +----------------------|     ....Ymin     x
c      |     .                            |     .            |
c      |     .    (boite de recherche maitre)   .            | Tzinf
c      |     .                            |     .            |
c      |     .............................|.........Tymin    v
c      |     .     .                      |     .
c      |     .     .                      |     .
c      |   (ancienne boite)               |     .
c      |     .     .                      |     .
c      |     .     .                      |     .
c -----+----------------------------------+---------Ymin_old
c      |     .     .                      |     .
c      |     .     .                    Xmax=   .
c Xmin_old   .     .                 Xmax_old   .
c            .   Xmin                        Txmax
c          Txmin
c
c       si la boite est reduite du cote de Xmin on pourrait utiliser:
c           Txmin = Xmin  avec Xmin = min(Xmaster)-Tzinf > Xmin_old
c       
c       mais en utilisant:
c           Txmin = Xmin-Tzinf (= min(Xmaster) - 2*Tzinf) 
c       on ne penalise pas I11INSIND 
c       (il n'y a pas de maitre dans la zone surestimee)
c       et le calcul de Xmin, Txmin ... est plus simple


      XMIN = MAX(XMIN - TZINF , XYZM(1,I_ADD))
      YMIN = MAX(YMIN - TZINF , XYZM(2,I_ADD))
      ZMIN = MAX(ZMIN - TZINF , XYZM(3,I_ADD))
      XMAX = MIN(XMAX + TZINF , XYZM(4,I_ADD))
      YMAX = MIN(YMAX + TZINF , XYZM(5,I_ADD))
      ZMAX = MIN(ZMAX + TZINF , XYZM(6,I_ADD))

      TXMIN = XMIN - TZINF
      TYMIN = YMIN - TZINF
      TZMIN = ZMIN - TZINF
      TXMAX = XMAX + TZINF
      TYMAX = YMAX + TZINF
      TZMAX = ZMAX + TZINF

      DMX = XMAX-XMIN 
      DMY = YMAX-YMIN
      DMZ = ZMAX-ZMIN

      DSUP = MAX (DMX,DMY,DMZ)

      IF(DMY.EQ.DSUP) THEN
        DIR = 2
        JJ  = 3
        KK  = 1
        SEUIL = (YMIN+YMAX)*0.5
        XIMIN = YMIN
        XJMIN = ZMIN
        XKMIN = XMIN
        XIMAX = YMAX
        XJMAX = ZMAX
        XKMAX = XMAX
        TIMIN = TYMIN
        TJMIN = TZMIN
        TKMIN = TXMIN
        TIMAX = TYMAX
        TJMAX = TZMAX
        TKMAX = TXMAX
      ELSE IF(DMZ.EQ.DSUP) THEN
        DIR = 3
        JJ  = 1
        KK  = 2
        SEUIL = (ZMIN+ZMAX)*0.5
        XIMIN = ZMIN
        XJMIN = XMIN
        XKMIN = YMIN
        XIMAX = ZMAX
        XJMAX = XMAX
        XKMAX = YMAX
        TIMIN = TZMIN
        TJMIN = TXMIN
        TKMIN = TYMIN
        TIMAX = TZMAX
        TJMAX = TXMAX
        TKMAX = TYMAX
      ELSE
        DIR = 1
        JJ  = 2
        KK  = 3
        SEUIL = (XMIN+XMAX)*0.5
        XIMIN = XMIN
        XJMIN = YMIN
        XKMIN = ZMIN
        XIMAX = XMAX
        XJMAX = YMAX
        XKMAX = ZMAX
        TIMIN = TXMIN
        TJMIN = TYMIN
        TKMIN = TZMIN
        TIMAX = TXMAX
        TJMAX = TYMAX
        TKMAX = TZMAX
      ENDIF

      TSMIN = SEUIL - TZINF
      TSMAX = SEUIL + TZINF

C
C    2- DIVISER LES ESCLAVES EN DEUX ZONES 
C

c            +-----------+-----------+--Xjmax  
c            |           |           |         
c            |           |           |         
c            |           |           |         
c            |           |           |         
c            +-----------+-----------+--Xjmin  
c            |           |           |        
c          Ximin       Seuil       Ximax  
c            



      NB_NCN= 0
      NB_NCN1= 0
      ADDNN= ADD(1,I_ADD)
#include "vectorize.inc"
      DO I=1,NB_NC
        NN = BPN(I)
        IF(NN.LE.NRTS) THEN
          XX1=X(DIR,IRECTS(1,NN))
          XX2=X(DIR,IRECTS(2,NN))
          XY1=X(JJ, IRECTS(1,NN))
          XY2=X(JJ, IRECTS(2,NN))
          XZ1=X(KK, IRECTS(1,NN))
          XZ2=X(KK, IRECTS(2,NN))
        ELSE
          NI = NN-NRTS
          XX1=XREM(DIR,NI)
          XX2=XREM(DIR+7,NI)
          XY1=XREM(JJ ,NI)
          XY2=XREM(JJ+7 ,NI)
          XZ1=XREM(KK ,NI)
          XZ2=XREM(KK+7 ,NI)
        END IF
        XMAX=MAX(XX1,XX2)
        XMIN=MIN(XX1,XX2)
        IF(XMIN.LT.SEUIL.AND.XMAX.GE.XIMIN) THEN
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        XIMIN,SEUIL,XJMIN,XJMAX,XKMIN,XKMAX)) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = NN
         END IF
        END IF
      ENDDO
#include "vectorize.inc"
      DO I=1,NB_NC
        NN = BPN(I)
        IF(NN.LE.NRTS) THEN
          XX1=X(DIR,IRECTS(1,NN))
          XX2=X(DIR,IRECTS(2,NN))
          XY1=X(JJ, IRECTS(1,NN))
          XY2=X(JJ, IRECTS(2,NN))
          XZ1=X(KK, IRECTS(1,NN))
          XZ2=X(KK, IRECTS(2,NN))
        ELSE
          NI = NN-NRTS
          XX1=XREM(DIR,NI)
          XX2=XREM(DIR+7,NI)
          XY1=XREM(JJ ,NI)
          XY2=XREM(JJ+7 ,NI)
          XZ1=XREM(KK ,NI)
          XZ2=XREM(KK+7 ,NI)
        END IF
        XMAX=MAX(XX1,XX2)
        XMIN=MIN(XX1,XX2)
        IF(XMAX.GE.SEUIL.AND.XMIN.LE.XIMAX) THEN
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        SEUIL,XIMAX,XJMIN,XJMAX,XKMIN,XKMAX)) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = NN
         ENDIF
        ENDIF
      ENDDO
C
C    3- DIVISER LES MAITRES
C

c        Tzinf       Tzinf Tzinf      Tzinf
c        <---->      <----x---->      <---->
c        ............,.,.,.,.,..,,,,,,,,,,,,--Tjmax   ^
c        .           ,         .           ,          | Tzinf
c        .           ,         .           ,          |
c        .    +------,----+----.------+    ,--Xjmax   v
c        .    |      ,    |    .      |    ,
c        .    |      ,    |    .      |    ,
c        .    |      ,    |    .      |    ,
c        .    |      ,    |    .      |    ,
c        .    +------,----+----.------+    ,--Xjmin   ^
c        .           ,         .           ,          | Tzinf
c        .           ,         .           ,          |
c        ............,.,.,.,.,..,,,,,,,,,,,,--Tjmin   v
c        |    |      |    |    |      |    |    
c        |  Ximin    |  Seuil  |    Ximax  |
c      Timin       Tsmin     Tsmax       Timax
c
c        si la boite a ete reduite(Cf 1)
c        il est possible que Timin=Ximin ... 


      NB_ECN= 0
      NB_ECN1= 0
      ADDNE= ADD(2,I_ADD)
      IF(NB_NCN1.EQ.0) THEN
#include "vectorize.inc"
       DO I=1,NB_EC
        NE = BPE(I)
        XX1=X(DIR, IRECTM(1,NE))
        XX2=X(DIR, IRECTM(2,NE))
        IF(MAX(XX1,XX2).GE.TSMIN) THEN
         XY1=X(JJ, IRECTM(1,NE))
         XY2=X(JJ, IRECTM(2,NE))
         XZ1=X(KK, IRECTM(1,NE))
         XZ2=X(KK, IRECTM(2,NE))
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        TSMIN,TIMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDIF
       ENDDO
      ELSEIF(NB_NCN.EQ.0) THEN
#include "vectorize.inc"
       DO I=1,NB_EC
        NE = BPE(I)
        XX1=X(DIR, IRECTM(1,NE))
        XX2=X(DIR, IRECTM(2,NE))
        IF(MIN(XX1,XX2).LT.TSMAX) THEN
         XY1=X(JJ, IRECTM(1,NE))
         XY2=X(JJ, IRECTM(2,NE))
         XZ1=X(KK, IRECTM(1,NE))
         XZ2=X(KK, IRECTM(2,NE))
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        TIMIN,TSMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          NB_ECN1= NB_ECN1 + 1
          PE(ADDNE) = NE
         ENDIF
        ENDIF
       ENDDO
      ELSE
#include "vectorize.inc"
       DO I=1,NB_EC
        NE = BPE(I)
        XX1=X(DIR, IRECTM(1,NE))
        XX2=X(DIR, IRECTM(2,NE))
        IF(MIN(XX1,XX2).LT.TSMAX) THEN
         XY1=X(JJ, IRECTM(1,NE))
         XY2=X(JJ, IRECTM(2,NE))
         XZ1=X(KK, IRECTM(1,NE))
         XZ2=X(KK, IRECTM(2,NE))
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        TIMIN,TSMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          NB_ECN1= NB_ECN1 + 1
          PE(ADDNE) = NE
         ENDIF
        ENDIF
       ENDDO
#include "vectorize.inc"
       DO I=1,NB_EC
        NE = BPE(I)
        XX1=X(DIR, IRECTM(1,NE))
        XX2=X(DIR, IRECTM(2,NE))
        IF(MAX(XX1,XX2).GE.TSMIN) THEN
         XY1=X(JJ, IRECTM(1,NE))
         XY2=X(JJ, IRECTM(2,NE))
         XZ1=X(KK, IRECTM(1,NE))
         XZ2=X(KK, IRECTM(2,NE))
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        TSMIN,TIMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C
C    4- REMPLIR LES TABLEAUX D'ADRESSES
C
      ADD(1,I_ADD+1) = ADDNN
      ADD(2,I_ADD+1) = ADDNE
C-----on remplit les min de la boite suivante et les max de la courante
C     (i.e. seuil est un max pour la courante)
C     on va redescendre et donc on definit une nouvelle boite
C     on remplit les max de la nouvelle boite
C     initialises dans i7buc1 a 1.E30 comme ca on recupere
c     soit XMAX soit le max de la boite
      XYZM(1,I_ADD+1) = XYZM(1,I_ADD) 
      XYZM(2,I_ADD+1) = XYZM(2,I_ADD)
      XYZM(3,I_ADD+1) = XYZM(3,I_ADD)
      XYZM(4,I_ADD+1) = XYZM(4,I_ADD)
      XYZM(5,I_ADD+1) = XYZM(5,I_ADD)
      XYZM(6,I_ADD+1) = XYZM(6,I_ADD)

      XYZM(DIR  ,I_ADD)   = XIMIN
      XYZM(DIR+3,I_ADD)   = SEUIL
      XYZM(DIR  ,I_ADD+1) = SEUIL
      XYZM(DIR+3,I_ADD+1) = XIMAX
C
      NB_OLD(1,I_ADD)=NB_NC
      NB_OLD(2,I_ADD)=NB_EC
      NB_OLD(1,I_ADD+1)=NB_NC
      NB_OLD(2,I_ADD+1)=NB_EC
C
      NB_NC = NB_NCN
      NB_EC = NB_ECN
C     on incremente le niveau de descente avant de sortir
      I_ADD = I_ADD + 1
      IF(I_ADD+1.GE.I_ADD_MAX) THEN
        I_MEM = 3
        RETURN
      ENDIF
C===============
 200  CONTINUE
C===============
C-----------------------------------------------------------
C
C
C    2- TEST ARRET = BOITE VIDE
C                    BOITE TROP PETITE 
C                    BOITE NE CONTENANT QU'UN NOEUD
C                    PLUS DE MEMOIRE DISPONIBLE
C                    LE DECOUPAGE NE REDUIT PAS LES CANDIDATS
C
C-------------------TEST SUR MEMOIRE DEPASSEE------------
C
      IF(ADD(1,I_ADD)+NB_NC.GT.MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES COTES ESCLAVES BOITES TROP PETITES
        I_MEM = 1
        RETURN
      ENDIF
      IF(ADD(2,I_ADD)+NB_EC.GT.MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES COTES MAITRES BOITES TROP PETITES
        I_MEM = 1
        RETURN
      ENDIF
C
C--------------------TEST SUR BOITE VIDES-------------- 
C
      IF(NB_EC.NE.0.AND.NB_NC.NE.0) THEN
C
        DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
        DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
        DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
        DSUP= MAX(DX,DY,DZ)
C
C-------------------TEST SUR FIN DE BRANCHE ------------
C       1- STOCKAGE DU OU DES  NOEUD CANDIDAT ET DES ELTS CORRESP.
C          VIRER LES INUTILES
C
        IF(NB_EC+NB_NC.LE.128) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = 129
        ENDIF
C
         NB_NC_OLD = NB_OLD(1,I_ADD)
         NB_EC_OLD = NB_OLD(2,I_ADD)

        IF(DSUP.LT.MINBOX.OR.
     .     NB_NC.LE.NB_N_B.OR.NB_EC.LE.NB_N_B.OR.
     .     NCAND_PROV.LE.128.OR.(NB_EC.EQ.NB_EC_OLD
     .     .AND.NB_NC.EQ.NB_NC_OLD)) THEN
C
          NCAND_PROV = NB_EC*NB_NC
          DO K=1,NCAND_PROV,NVSIZ
            DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
              I = 1+(L-1)/NB_NC
              J = L-(I-1)*NB_NC
              NE = BPE(I)
              NN = BPN(J)
              N1=IRECTM(1,NE)
              N2=IRECTM(2,NE)
              IF(NN.LE.NRTS) THEN
                NN1=IRECTS(1,NN)
                NN2=IRECTS(2,NN)
                IF(IAUTO==0 .OR. ITAB(N1)>ITAB(NN1) )THEN
                 IF(NN1.NE.N1.AND.NN1.NE.N2.AND.
     .               NN2.NE.N1.AND.NN2.NE.N2) THEN
                      J_STOK = J_STOK + 1
                      PROV_S(J_STOK) = NN
                      PROV_M(J_STOK) = NE
                 ENDIF
                ENDIF
              ELSE
                NI = NN-NRTS
                NN1 = IREM(2,NI)             
                NN2 = IREM(3,NI)
                N1 = ITAB(N1)
                N2 = ITAB(N2)
                IF(IAUTO==0 .OR. N1>NN1 )THEN
                 IF(NN1.NE.N1.AND.NN1.NE.N2.AND.
     .             NN2.NE.N1.AND.NN2.NE.N2) THEN
                    J_STOK = J_STOK + 1
                    PROV_S(J_STOK) = NN
                    PROV_M(J_STOK) = NE
                 ENDIF
                ENDIF
              END IF
            ENDDO
            IF(J_STOK.GE.NVSIZ)THEN
              CALL I11STO(
     1              NVSIZ,IRECTS,IRECTM,X     ,II_STOK,
     2              CAND_S,CAND_M,NSN4  ,NOINT ,TZINF ,
     3              I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM ,
     4              CHAINE,NRTS  ,ITAB  ,IFPEN ,IFORM )
              IF(I_MEM.EQ.2)RETURN
              J_STOK = J_STOK-NVSIZ
#include "vectorize.inc"
              DO J=1,J_STOK
                PROV_S(J) = PROV_S(J+NVSIZ)
                PROV_M(J) = PROV_M(J+NVSIZ)
              ENDDO
            ENDIF
          ENDDO 
        ELSE
C==================
          GOTO 100
C==================
        ENDIF
      ENDIF
C-------------------------------------------------------------------------
C       BOITE VIDE OU
C       FIN DE BRANCHE 
C       on decremente le niveau de descente avant de recommencer
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
      IF (I_ADD.NE.0) THEN
C-------------------------------------------------------------------------
C         IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C         AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
C-------------------------------------------------------------------------
          CALL I7DSTK(NB_NC,NB_EC,ADD(1,I_ADD),BPN,PN,BPE,PE)
C==================
          GOTO 200
C==================
      ENDIF
C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
      IF(J_STOK.NE.0)CALL I11STO(
     1              J_STOK,IRECTS,IRECTM,X     ,II_STOK,
     2              CAND_S,CAND_M,NSN4  ,NOINT ,TZINF  ,
     3              I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM  ,
     4              CHAINE,NRTS  ,ITAB  ,IFPEN ,IFORM  )
C-------------------------------------------------------------------------
      RETURN
      END



Chd|====================================================================
Chd|  I11INSID                      source/interfaces/intsort/i11tri.F
Chd|-- called by -----------
Chd|        I11TRI                        source/interfaces/intsort/i11tri.F
Chd|        I20TRI_EDGE                   source/interfaces/intsort/i20tri.F
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION I11INSID(X1,X2,Y1,Y2,Z1,Z2,
     .                   XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX)
#include      "implicit_f.inc"
      INTEGER N1,N2
      my_real
     .        X1,X2,Y1,Y2,Z1,Z2,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
      INTEGER K
      my_real
     .        AA,XX,YY,ZZ

c
c     elimination segments externes a la boite
c

c
c 1- conservation: au moins un noeud dans la boite
c
c              Xmin        Xmax  
c                |           |   
c                |           |   
c    Ymax -------+-----------+
c                |       0 N2|
c                |  N1  /    |
c                | O   /     |
c                |  \ O N1   |
c    Ymin -------+---\-------+
c                     \
c                      \
c                       \ N2
c                        O

      I11INSID = .true.


c     test si N1 ou N2  dans la boite                   

      IF(X1>=XMIN.and.X1<=XMAX.and.                     
     .   Y1>=YMIN.and.Y1<=YMAX.and.                     
     .   Z1>=ZMIN.and.Z1<=ZMAX) RETURN                   
 
      IF(X2>=XMIN.and.X2<=XMAX.and.                     
     .   Y2>=YMIN.and.Y2<=YMAX.and.                     
     .   Z2>=ZMIN.and.Z2<=ZMAX) RETURN                  
   

c
c 2- elimination: segment ne coupe pas la boite
c
c              Xmin        Xmax  
c                |           |   
c                |           |   
c    Ymax -------+-----------+
c                |           |
c            N1  |           |
c           O    |           |
c            \   |           |
c    Ymin ----\--+-----------+ 
c              \ |            
c               \|            
c                \
c                |\ N2
c                | O       

      I11INSID = .false.

c     projection de N1 sur XMIN ou XMAX                 

      XX = MIN(MAX(X1,XMIN),XMAX)                       

      IF(XX /= X1)THEN 
        IF(X1==X2)RETURN                                 
        IF(Y2>YMAX)THEN                                
          AA = (XX-X1)/(X2-X1)                                        
          IF(  Y1 + AA * (Y2-Y1) > YMAX)RETURN         
          IF(Z2>ZMAX)THEN                              
            IF(Z1 + AA * (Z2-Z1) > ZMAX)RETURN         
          ELSEIF(Z2<ZMIN)THEN                          
            IF(Z1 + AA * (Z2-Z1) < ZMIN)RETURN         
          ENDIF                                         
        ELSEIF(Y2<YMIN)THEN                            
          AA = (XX-X1)/(X2-X1)                                        
          IF(  Y1 + AA * (Y2-Y1) < YMIN)RETURN         
          IF(Z2>ZMAX)THEN                              
            IF(Z1 + AA * (Z2-Z1) > ZMAX)RETURN         
          ELSEIF(Z2<ZMIN)THEN                          
            IF(Z1 + AA * (Z2-Z1) < ZMIN)RETURN         
          ENDIF                                         
        ELSE                                            
          IF(Z2>ZMAX)THEN                              
            AA = (XX-X1)/(X2-X1)                                        
            IF(Z1 + AA * (Z2-Z1) > ZMAX)RETURN         
          ELSEIF(Z2<ZMIN)THEN                          
            AA = (XX-X1)/(X2-X1)                                        
            IF(Z1 + AA * (Z2-Z1) < ZMIN)RETURN         
          ENDIF                                         
        ENDIF                                           
      ENDIF                                             

c     projection de N1 sur YMIN ou YMAX               

      YY = MIN(MAX(Y1,YMIN),YMAX)                     

      IF(YY /= Y1)THEN                                
        IF(Y1==Y2)RETURN                                 
        IF(Z2>ZMAX)THEN                              
          AA = (YY-Y1)/(Y2-Y1)                                        
          IF(  Z1 + AA * (Z2-Z1) > ZMAX)RETURN       
          IF(X2>XMAX)THEN                            
            IF(X1 + AA * (X2-X1) > XMAX)RETURN       
          ELSEIF(X2<XMIN)THEN                        
            IF(X1 + AA * (X2-X1) < XMIN)RETURN       
          ENDIF                                       
        ELSEIF(Z2<ZMIN)THEN                          
          AA = (YY-Y1)/(Y2-Y1)                                        
          IF(  Z1 + AA * (Z2-Z1) < ZMIN)RETURN       
          IF(X2>XMAX)THEN                            
            IF(X1 + AA * (X2-X1) > XMAX)RETURN       
          ELSEIF(X2<XMIN)THEN                        
            IF(X1 + AA * (X2-X1) < XMIN)RETURN       
          ENDIF                                       
        ELSE                                          
          IF(X2>XMAX)THEN                            
            AA = (YY-Y1)/(Y2-Y1)                                        
            IF(X1 + AA * (X2-X1) > XMAX)RETURN       
          ELSEIF(X2<XMIN)THEN                        
            AA = (YY-Y1)/(Y2-Y1)                                        
            IF(X1 + AA * (X2-X1) < XMIN)RETURN       
          ENDIF                                       
        ENDIF                                         
      ENDIF                                           

c     projection de N1 sur ZMIN ou ZMAX                    

      ZZ = MIN(MAX(Z1,ZMIN),ZMAX)                          

      IF(ZZ /= Z1)THEN                                     
        IF(Z1==Z2)RETURN                                 
        IF(X2>XMAX)THEN                                   
          AA = (ZZ-Z1)/(Z2-Z1)                                        
          IF(  X1 + AA * (X2-X1) > XMAX)RETURN            
          IF(Y2>YMAX)THEN                                 
            IF(Y1 + AA * (Y2-Y1) > YMAX)RETURN            
          ELSEIF(Y2<YMIN)THEN                             
            IF(Y1 + AA * (Y2-Y1) < YMIN)RETURN            
          ENDIF                                            
        ELSEIF(X2<XMIN)THEN                               
          AA = (ZZ-Z1)/(Z2-Z1)                                        
          IF(  X1 + AA * (X2-X1) < XMIN)RETURN            
          IF(Y2>YMAX)THEN                                 
            IF(Y1 + AA * (Y2-Y1) > YMAX)RETURN            
          ELSEIF(Y2<YMIN)THEN                             
            IF(Y1 + AA * (Y2-Y1) < YMIN)RETURN            
          ENDIF                                            
        ELSE                                               
          IF(Y2>YMAX)THEN                                 
            AA = (ZZ-Z1)/(Z2-Z1)                                        
            IF(Y1 + AA * (Y2-Y1) > YMAX)RETURN            
          ELSEIF(Y2<YMIN)THEN                             
            AA = (ZZ-Z1)/(Z2-Z1)                                        
            IF(Y1 + AA * (Y2-Y1) < YMIN)RETURN            
          ENDIF                                            
        ENDIF                                              
      ENDIF                                                

c
c 3- autres cas: segment coupe la boite
c
c
c              Xmin        Xmax  
c                |           |   
c                |           |   
c    Ymax -------+-----------+
c           N1 0 |           |
c               \|           |
c                \           |
c                |\          |
c    Ymin -------+-\---------+
c                   \      
c                    \ N2 
c                     O   

      I11INSID = .true.
      
      RETURN
      END
