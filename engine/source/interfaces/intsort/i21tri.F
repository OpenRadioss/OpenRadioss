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
Chd|  I21TRI                        source/interfaces/intsort/i21tri.F
Chd|-- called by -----------
Chd|        I21BUCE                       source/interfaces/intsort/i21buce.F
Chd|-- calls ---------------
Chd|        I21STO                        source/interfaces/int21/i21sto.F
Chd|        I7DSTK                        source/interfaces/intsort/i7dstk.F
Chd|====================================================================
      SUBROUTINE I21TRI(
     1      ADD      ,NSN     ,IRECT  ,XLOC     ,STF     ,
     2      STFN     ,XYZM    ,I_ADD  ,MAXSIZ  ,II_STOK  ,
     3      CAND_N  ,CAND_E ,MULNSN   ,NOINT   ,TZINF    ,
     4      MAXBOX  ,MINBOX ,I_MEM    ,NB_N_B  ,I_ADD_MAX,
     5      ESHIFT  ,INACTI ,NRTM     ,IGAP    ,GAP      ,
     7      GAP_S   ,GAPMIN ,GAPMAX   ,MARGE   ,CURV_MAX ,
     8      XM0     ,NOD_NORMAL,DEPTH ,DRAD    ,DGAPLOAD )
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
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES ELETS DE BPE ET LES NOEUDS DE BPN EN TWO ZONES
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
C     IRECT(4,*)   TABLEAU DES CONEC FACETTES        E
C     XLOC(3,*)    COORDONNEES NODALES LOCALES       E
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
C     CAND_N       boites resultats noeuds
C     ADESTK       adresse courante dans la boite des elements
C     CAND_E       adresses des boites resultat elements
C                  MULNSN = MULTIMP*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C     MAXBOX       TAILLE MAX BUCKET
C     MINBOX       TAILLE MIN BUCKET
C
C     PROV_N       CAND_N provisoire (variable static dans i7tri)
C     PROV_E       CAND_E provisoire (variable static dans i7tri)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_ADD,MAXSIZ,I_MEM,ESHIFT,NSN,NRTM,
     .        MULNSN,NB_N_B,NOINT,I_ADD_MAX,INACTI,IGAP,
     .        ADD(2,*),IRECT(4,*),
     .        CAND_N(*),CAND_E(*),II_STOK
C     REAL
      my_real
     .   XLOC(3,*),XYZM(6,*),STF(*),STFN(*),GAP_S(*), 
     .   XM0(3,*), NOD_NORMAL(3,*),
     .   TZINF,MAXBOX,MINBOX,MARGE,GAP,GAPMIN,GAPMAX, 
     .   DEPTH
      my_real , INTENT(IN) :: DGAPLOAD,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,J_STOK,II,JJ,
     .        PROV_N(2*MVSIZ),PROV_E(2*MVSIZ),
     .        TN1(NVECSZ),TN2(NVECSZ),TN3(NVECSZ),TN4(NVECSZ),
C BPE : utilise sur NRTM et non NRTM + 100 en toute rigueur (ici MAXSIZ = NRTM + 100)
     .        BPE(MAXSIZ/3),PE(MAXSIZ),BPN(NSN),PN(NSN)
C     REAL
      my_real
     .   DX,DY,DZ,DSUP,SEUIL,SEUILS,SEUILI, XX1, XX2, XX3, XX4,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, BGAPSMX, GAPL,
     .   TXX1(3,NVECSZ), TXX2(3,NVECSZ), TXX3(3,NVECSZ), TXX4(3,NVECSZ),
     .   TXMAX(NVECSZ),TXMIN(NVECSZ),TYMAX(NVECSZ),
     .   TYMIN(NVECSZ),TZMAX(NVECSZ),TZMIN(NVECSZ),
     .   CURV_MAX(*)
C-----------------------------------------------
C
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
C
      XMIN = XYZM(1,I_ADD)
      YMIN = XYZM(2,I_ADD)
      ZMIN = XYZM(3,I_ADD)
      XMAX = XYZM(4,I_ADD)
      YMAX = XYZM(5,I_ADD)
      ZMAX = XYZM(6,I_ADD)
C
C Copie des nos de segments et de noeuds dans BPE ET BPN
C
      NB_EC = 0
      DO I=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(I)/=ZERO)THEN
          NB_EC = NB_EC + 1
          BPE(NB_EC) = I
        ENDIF
      ENDDO
C
C Optimisation // recherche les noeuds compris dans xmin xmax des 
C elements du processeur
C
      NB_NC = 0
      DO I=1,NSN
        IF(STFN(I)/=ZERO) THEN
         IF(XLOC(1,I)>=XMIN.AND.XLOC(1,I)<=XMAX.AND.
     .      XLOC(2,I)>=YMIN.AND.XLOC(2,I)<=YMAX.AND.
     .      XLOC(3,I)>=ZMIN.AND.XLOC(3,I)<=ZMAX)THEN
          NB_NC=NB_NC+1
          BPN(NB_NC) = I
         ENDIF
        ENDIF
      ENDDO
C
      J_STOK = 0
      GOTO 200
C=======================================================================
 100  CONTINUE
C=======================================================================
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
      DIR = 1
      IF(DY==DSUP) THEN
        DIR = 2
      ELSE IF(DZ==DSUP) THEN
        DIR = 3
      ENDIF
      SEUIL =(XYZM(DIR+3,I_ADD)+XYZM(DIR,I_ADD))*0.5
C
C    2- DIVISER LES NOEUDS EN TWO ZONES
C
      NB_NCN= 0
      NB_NCN1= 0
      ADDNN= ADD(1,I_ADD)
      IF(IGAP==0)THEN
#include "vectorize.inc"
        DO I=1,NB_NC
         IF(XLOC(DIR,BPN(I))<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = BPN(I)
         ENDIF
        ENDDO
C
#include "vectorize.inc"
        DO I=1,NB_NC
         IF(XLOC(DIR,BPN(I))>=SEUIL) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = BPN(I)
         ENDIF
        ENDDO
      ELSE
        GAPSMX = ZERO
#include "vectorize.inc"
        DO I=1,NB_NC
         IF(XLOC(DIR,BPN(I))<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = BPN(I)
          GAPSMX = MAX(GAPSMX,MAX(GAP_S(BPN(I))+DGAPLOAD,DEPTH,DRAD))
         ENDIF
        ENDDO
C
        BGAPSMX = ZERO
#include "vectorize.inc"
        DO I=1,NB_NC
         IF(XLOC(DIR,BPN(I))>=SEUIL) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = BPN(I)
          BGAPSMX = MAX(BGAPSMX,MAX(GAP_S(BPN(I))+DGAPLOAD,DEPTH,DRAD))
         ENDIF
        ENDDO
      ENDIF
C
C    3- DIVISER LES ELEMENTS
C
      IF(IGAP==0) THEN
       NB_ECN= 0
       ADDNE= ADD(2,I_ADD)
       IF(NB_NCN1==0) THEN
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMAX=MAX(XX1,XX2,XX3,XX4)+TZINF
         IF(XMAX>=SEUIL) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDDO
       ELSEIF(NB_NCN==0) THEN
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMIN=MIN(XX1,XX2,XX3,XX4)-TZINF
         IF(XMIN<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
       ELSE
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMIN=MIN(XX1,XX2,XX3,XX4)-TZINF
         IF(XMIN<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
C
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMAX=MAX(XX1,XX2,XX3,XX4)+TZINF
         IF(XMAX>=SEUIL) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDDO
       ENDIF
C Optimisation gap variable
      ELSE
       NB_ECN= 0
       ADDNE= ADD(2,I_ADD)
       IF(NB_NCN1==0) THEN
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMAX=MAX(XX1,XX2,XX3,XX4)
     +       +MAX(MIN(MAX(BGAPSMX,GAPMIN),GAPMAX)+DGAPLOAD,DEPTH,DRAD)
     +       +MARGE
         IF(XMAX>=SEUIL) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDDO
       ELSEIF(NB_NCN==0) THEN
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMIN=MIN(XX1,XX2,XX3,XX4)
     -       -MAX(MIN(MAX(GAPSMX,GAPMIN),GAPMAX)+DGAPLOAD,DEPTH,DRAD)
     -       -MARGE
         IF(XMIN<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
       ELSE
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMIN=MIN(XX1,XX2,XX3,XX4)
     -       -MAX(MIN(MAX(GAPSMX,GAPMIN),GAPMAX)+DGAPLOAD,DEPTH,DRAD)
     -       -MARGE
         IF(XMIN<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
C
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XM0(DIR, IRECT(1,NE))
         XX2=XM0(DIR, IRECT(2,NE))
         XX3=XM0(DIR, IRECT(3,NE))
         XX4=XM0(DIR, IRECT(4,NE))
         XMAX=MAX(XX1,XX2,XX3,XX4)
     +       +MAX(MIN(MAX(BGAPSMX,GAPMIN),GAPMAX)+DGAPLOAD,DEPTH,DRAD)
     +       +MARGE
         IF(XMAX>=SEUIL) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDDO
       ENDIF
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
      XYZM(DIR,I_ADD+1) = SEUIL
      XYZM(DIR+3,I_ADD) = SEUIL
C
      NB_NC = NB_NCN
      NB_EC = NB_ECN
C     on incremente le niveau de descente avant de sortir
      I_ADD = I_ADD + 1
      IF(I_ADD+1>=I_ADD_MAX) THEN
        I_MEM = 3
        RETURN
      ENDIF
C=======================================================================
 200  CONTINUE
C=======================================================================
C-----------------------------------------------------------
C
C
C    2- TEST ARRET = BOITE VIDE
C                    BOITE TROP PETITE
C                    BOITE NE CONTENANT QU'ONE NOEUD
C                    PLUS DE MEMOIRE DISPONIBLE
C
C-------------------TEST SUR MEMOIRE DEPASSEE------------
C
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS BOITES TROP PETITES
        I_MEM = 1
        RETURN
      ENDIF
C
C--------------------TEST SUR BOITE VIDES--------------
C
      IF(NB_EC/=0.AND.NB_NC/=0) THEN
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
C       NCAND_PROV=NB_EC*NB_NC
Cel NCAND_PROV negatif qd NB_EC*NB_NC > 2e31
C
        IF(NB_EC+NB_NC<=NVECSZ) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = NVECSZ+1
        ENDIF
C
        IF(DSUP<MINBOX.OR.(NB_NC<=NB_N_B)
     &     .OR.(NCAND_PROV<=NVECSZ)) THEN
Cel necessaire qd NB_NC<=NB_N_B ou DSUP<MINBOX et NB_EC+NB_NC>128
          NCAND_PROV = NB_EC*NB_NC
          IF(IVECTOR==1.AND.NCAND_PROV<=NVECSZ)THEN
           IF(IGAP==0)THEN
            DO I = 1, NB_EC
              NE = BPE(I)
              TN1(I)=IRECT(1,NE)
              TN2(I)=IRECT(2,NE)
              TN3(I)=IRECT(3,NE)
              TN4(I)=IRECT(4,NE)
              TXX1(1,I)=XM0(1, TN1(I))
              TXX2(1,I)=XM0(1, TN2(I))
              TXX3(1,I)=XM0(1, TN3(I))
              TXX4(1,I)=XM0(1, TN4(I))
              TXMAX(I)=MAX(TXX1(1,I),TXX2(1,I),TXX3(1,I),TXX4(1,I))
     +                +TZINF
              TXMIN(I)=MIN(TXX1(1,I),TXX2(1,I),TXX3(1,I),TXX4(1,I))
     -                -TZINF
              TXX1(2,I)=XM0(2, TN1(I))
              TXX2(2,I)=XM0(2, TN2(I))
              TXX3(2,I)=XM0(2, TN3(I))
              TXX4(2,I)=XM0(2, TN4(I))
              TYMAX(I)=MAX(TXX1(2,I),TXX2(2,I),TXX3(2,I),TXX4(2,I))
     +                +TZINF
              TYMIN(I)=MIN(TXX1(2,I),TXX2(2,I),TXX3(2,I),TXX4(2,I))
     -                -TZINF
              TXX1(3,I)=XM0(3, TN1(I))
              TXX2(3,I)=XM0(3, TN2(I))
              TXX3(3,I)=XM0(3, TN3(I))
              TXX4(3,I)=XM0(3, TN4(I))
              TZMAX(I)=MAX(TXX1(3,I),TXX2(3,I),TXX3(3,I),TXX4(3,I))
     +                +TZINF
              TZMIN(I)=MIN(TXX1(3,I),TXX2(3,I),TXX3(3,I),TXX4(3,I))
     -                -TZINF
            ENDDO
            DO K=1,NCAND_PROV,NVSIZ
              DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
               I = 1+(L-1)/NB_NC
               J = L-(I-1)*NB_NC
               NN=BPN(J)
               IF(XLOC(1,NN)>TXMIN(I).AND.XLOC(1,NN)<TXMAX(I).AND.
     &            XLOC(2,NN)>TYMIN(I).AND.XLOC(2,NN)<TYMAX(I).AND.
     &          XLOC(3,NN)>TZMIN(I).AND.XLOC(3,NN)<TZMAX(I) ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = BPN(J)
                 PROV_E(J_STOK) = BPE(I)
               ENDIF
              ENDDO
             IF(J_STOK>=NVSIZ)THEN
              CALL I21STO(
     1              NVSIZ ,IRECT  ,XLOC  ,II_STOK,CAND_N,
     2              CAND_E ,MULNSN,NOINT ,MARGE  ,I_MEM ,
     3              PROV_N ,PROV_E,ESHIFT,INACTI ,NSN   ,
     4              IGAP   ,GAP   ,GAP_S ,GAPMIN ,GAPMAX,
     5              CURV_MAX ,XM0 ,NOD_NORMAL,DEPTH ,DRAD,
     6              DGAPLOAD)
              IF(I_MEM==2)RETURN
              J_STOK = J_STOK-NVSIZ
#include "vectorize.inc"
              DO J=1,J_STOK
                PROV_N(J) = PROV_N(J+NVSIZ)
                PROV_E(J) = PROV_E(J+NVSIZ)
              ENDDO
             ENDIF
            ENDDO
           ELSE
            DO I = 1, NB_EC
              NE = BPE(I)
              TN1(I)=IRECT(1,NE)
              TN2(I)=IRECT(2,NE)
              TN3(I)=IRECT(3,NE)
              TN4(I)=IRECT(4,NE)
              TXX1(1,I)=XM0(1, TN1(I))
              TXX2(1,I)=XM0(1, TN2(I))
              TXX3(1,I)=XM0(1, TN3(I))
              TXX4(1,I)=XM0(1, TN4(I))
              TXMAX(I)=MAX(TXX1(1,I),TXX2(1,I),TXX3(1,I),TXX4(1,I))
     +                +MARGE
              TXMIN(I)=MIN(TXX1(1,I),TXX2(1,I),TXX3(1,I),TXX4(1,I))
     -                -MARGE
              TXX1(2,I)=XM0(2, TN1(I))
              TXX2(2,I)=XM0(2, TN2(I))
              TXX3(2,I)=XM0(2, TN3(I))
              TXX4(2,I)=XM0(2, TN4(I))
              TYMAX(I)=MAX(TXX1(2,I),TXX2(2,I),TXX3(2,I),TXX4(2,I))
     +                +MARGE
              TYMIN(I)=MIN(TXX1(2,I),TXX2(2,I),TXX3(2,I),TXX4(2,I))
     -                -MARGE
              TXX1(3,I)=XM0(3, TN1(I))
              TXX2(3,I)=XM0(3, TN2(I))
              TXX3(3,I)=XM0(3, TN3(I))
              TXX4(3,I)=XM0(3, TN4(I))
              TZMAX(I)=MAX(TXX1(3,I),TXX2(3,I),TXX3(3,I),TXX4(3,I))
     +                +MARGE
              TZMIN(I)=MIN(TXX1(3,I),TXX2(3,I),TXX3(3,I),TXX4(3,I))
     -                -MARGE
            ENDDO
            DO K=1,NCAND_PROV,NVSIZ
              DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
               I = 1+(L-1)/NB_NC
               J = L-(I-1)*NB_NC
               NN=BPN(J)
               GAPL=MAX(MAX(MIN(GAP_S(BPN(J)),GAPMAX),GAPMIN)+DGAPLOAD,DEPTH,DRAD)
               IF(XLOC(1,NN)>TXMIN(I)-GAPL.AND.
     &            XLOC(1,NN)<TXMAX(I)+GAPL.AND.
     &            XLOC(2,NN)>TYMIN(I)-GAPL.AND.
     &            XLOC(2,NN)<TYMAX(I)+GAPL.AND.
     &            XLOC(3,NN)>TZMIN(I)-GAPL.AND.
     &            XLOC(3,NN)<TZMAX(I)+GAPL ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = BPN(J)
                 PROV_E(J_STOK) = BPE(I)
               ENDIF
              ENDDO
              IF(J_STOK>=NVSIZ)THEN
               CALL I21STO(
     1              NVSIZ ,IRECT  ,XLOC  ,II_STOK,CAND_N,
     2              CAND_E ,MULNSN,NOINT ,MARGE  ,I_MEM ,
     3              PROV_N ,PROV_E,ESHIFT,INACTI ,NSN   ,
     4              IGAP   ,GAP   ,GAP_S ,GAPMIN ,GAPMAX,
     5              CURV_MAX ,XM0 ,NOD_NORMAL,DEPTH,DRAD,
     6              DGAPLOAD)
              IF(I_MEM==2)RETURN
              J_STOK = J_STOK-NVSIZ
#include "vectorize.inc"
              DO J=1,J_STOK
                PROV_N(J) = PROV_N(J+NVSIZ)
                PROV_E(J) = PROV_E(J+NVSIZ)
              ENDDO
             ENDIF
            ENDDO
           END IF
          ELSE
           DO K=1,NCAND_PROV,NVSIZ
             IF(IGAP==0) THEN
              DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
               I = 1+(L-1)/NB_NC
               J = L-(I-1)*NB_NC
               NE = BPE(I)
               N1=IRECT(1,NE)
               N2=IRECT(2,NE)
               N3=IRECT(3,NE)
               N4=IRECT(4,NE)
               XX1=XM0(1, N1)
               XX2=XM0(1, N2)
               XX3=XM0(1, N3)
               XX4=XM0(1, N4)
               XMAX=MAX(XX1,XX2,XX3,XX4)+TZINF
               XMIN=MIN(XX1,XX2,XX3,XX4)-TZINF
               XX1=XM0(2, N1)
               XX2=XM0(2, N2)
               XX3=XM0(2, N3)
               XX4=XM0(2, N4)
               YMAX=MAX(XX1,XX2,XX3,XX4)+TZINF
               YMIN=MIN(XX1,XX2,XX3,XX4)-TZINF
               XX1=XM0(3, N1)
               XX2=XM0(3, N2)
               XX3=XM0(3, N3)
               XX4=XM0(3, N4)
               ZMAX=MAX(XX1,XX2,XX3,XX4)+TZINF
               ZMIN=MIN(XX1,XX2,XX3,XX4)-TZINF

               NN=BPN(J)
               IF(XLOC(1,NN)>XMIN.AND.XLOC(1,NN)<XMAX.AND.
     &            XLOC(2,NN)>YMIN.AND.XLOC(2,NN)<YMAX.AND.
     &            XLOC(3,NN)>ZMIN.AND.XLOC(3,NN)<ZMAX ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = BPN(J)
                 PROV_E(J_STOK) = NE
               ENDIF
              ENDDO
             ELSE
              DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
               I = 1+(L-1)/NB_NC
               J = L-(I-1)*NB_NC
               NE = BPE(I)
               N1=IRECT(1,NE)
               N2=IRECT(2,NE)
               N3=IRECT(3,NE)
               N4=IRECT(4,NE)
               XX1=XM0(1, N1)
               XX2=XM0(1, N2)
               XX3=XM0(1, N3)
               XX4=XM0(1, N4)
               TZ=MAX(MAX(MIN(GAP_S(BPN(J)),GAPMAX),GAPMIN)+DGAPLOAD,DEPTH,DRAD)
     +           +MARGE
               XMAX=MAX(XX1,XX2,XX3,XX4)+TZ
               XMIN=MIN(XX1,XX2,XX3,XX4)-TZ
               XX1=XM0(2, N1)
               XX2=XM0(2, N2)
               XX3=XM0(2, N3)
               XX4=XM0(2, N4)
               YMAX=MAX(XX1,XX2,XX3,XX4)+TZ
               YMIN=MIN(XX1,XX2,XX3,XX4)-TZ
               XX1=XM0(3, N1)
               XX2=XM0(3, N2)
               XX3=XM0(3, N3)
               XX4=XM0(3, N4)
               ZMAX=MAX(XX1,XX2,XX3,XX4)+TZ
               ZMIN=MIN(XX1,XX2,XX3,XX4)-TZ

               NN=BPN(J)
               IF(XLOC(1,NN)>XMIN.AND.XLOC(1,NN)<XMAX.AND.
     &            XLOC(2,NN)>YMIN.AND.XLOC(2,NN)<YMAX.AND.
     &            XLOC(3,NN)>ZMIN.AND.XLOC(3,NN)<ZMAX ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = BPN(J)
                 PROV_E(J_STOK) = NE
               ENDIF
              ENDDO
             END IF
             IF(J_STOK>=NVSIZ)THEN
               CALL I21STO(
     1               NVSIZ,IRECT   ,XLOC  ,II_STOK,CAND_N,
     2               CAND_E ,MULNSN,NOINT ,MARGE  ,I_MEM ,
     3               PROV_N ,PROV_E,ESHIFT,INACTI ,NSN    ,
     4               IGAP  ,GAP   ,GAP_S  ,GAPMIN ,GAPMAX ,
     5               CURV_MAX ,XM0 ,NOD_NORMAL,DEPTH,DRAD ,
     6               DGAPLOAD)
               IF(I_MEM==2)RETURN
                 J_STOK = J_STOK-NVSIZ
#include "vectorize.inc"
               DO J=1,J_STOK
                 PROV_N(J) = PROV_N(J+NVSIZ)
                 PROV_E(J) = PROV_E(J+NVSIZ)
               ENDDO
             ENDIF
           ENDDO
         ENDIF
        ELSE
C=======================================================================
          GOTO 100
C=======================================================================
        ENDIF
      ENDIF
C-------------------------------------------------------------------------
C       BOITE VIDE OU
C       FIN DE BRANCHE
C       on decremente le niveau de descente avant de recommencer
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
      IF (I_ADD/=0) THEN
C-------------------------------------------------------------------------
C         IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C         AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
C-------------------------------------------------------------------------
          CALL I7DSTK(NB_NC,NB_EC,ADD(1,I_ADD),BPN,PN,BPE,PE)
C=======================================================================
          GOTO 200
C=======================================================================
      ENDIF
C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
      IF(J_STOK/=0)CALL I21STO(
     1              J_STOK,IRECT  ,XLOC  ,II_STOK,CAND_N,
     2              CAND_E ,MULNSN,NOINT ,MARGE  ,I_MEM ,
     3              PROV_N ,PROV_E,ESHIFT,INACTI ,NSN   ,
     4              IGAP  ,GAP   ,GAP_S  ,GAPMIN ,GAPMAX,
     5              CURV_MAX ,XM0,NOD_NORMAL,DEPTH,DRAD ,
     6              DGAPLOAD)
C-------------------------------------------------------------------------
      RETURN
      END
