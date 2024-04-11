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
Chd|  I20TRI                        source/interfaces/intsort/i20tri.F
Chd|-- called by -----------
Chd|        I20BUCE                       source/interfaces/intsort/i20buce.F
Chd|-- calls ---------------
Chd|        I20STO                        source/interfaces/intsort/i20sto.F
Chd|        I7DSTK                        source/interfaces/intsort/i7dstk.F
Chd|        SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20TRI(
     1      ADD   ,NSN    ,RENUM  ,NSNR     ,ISZNSNR  ,
     2      IRECT ,XA     ,STF    ,STFA     ,XYZM     ,
     3      I_ADD ,NSV    ,MAXSIZ ,II_STOK  ,CAND_N   ,
     4      CAND_E,MULNSN ,NOINT  ,TZINF    ,MAXBOX   ,
     5      MINBOX,I_MEM  ,NB_N_B ,I_ADD_MAX,ESHIFT   ,
     6      INACTI,IFQ    ,CAND_A ,CAND_P   ,IFPEN    ,
     7      NRTM  ,NSNROLD,IGAP   ,GAP      ,GAP_S    ,
     6      GAP_M ,GAPMIN ,GAPMAX ,MARGE    ,CURV_MAX ,
     7      NIN   ,GAP_SH ,NBINFLG,MBINFLG  ,ISYM     )
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
C     X(3,*)       COORDONNEES NODALES               E
C     NB_NC        NOMBRE DE NOEUDS CANDIDATS        => Local
C     NB_EC        NOMBRE D'ELTS CANDIDATS           => Local
C     I_ADD        POSITION DANS LE TAB DES ADRESSES E/S
C     NSV          NOS SYSTEMES DES NOEUDS           E
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
C     PROV_N       CAND_N provisoire (variable static dans i20tri)
C     PROV_E       CAND_E provisoire (variable static dans i20tri)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_ADD,MAXSIZ,I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NSNROLD,
     .        MULNSN,NB_N_B,NOINT,I_ADD_MAX,INACTI,IFQ,NSNR,IGAP,
     .        ADD(2,*),IRECT(4,*), NIN,NBINFLG(*),MBINFLG(*),ISYM,
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        II_STOK
C     REAL
      my_real
     .   XA(3,*),XYZM(6,*),CAND_P(*),STF(*),STFA(*),GAP_S(*),GAP_M(*),
     .   TZINF,MAXBOX,MINBOX,MARGE,GAP,GAPMIN,GAPMAX,
     .   CURV_MAX(*), GAP_SH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,J_STOK,II,JJ,
     .        PROV_N(2*MVSIZ),PROV_E(2*MVSIZ),
     .        TN1(NVECSZ),TN2(NVECSZ),TN3(NVECSZ),TN4(NVECSZ),
C BPE : utilise sur NRTM et non NRTM + 100 en toute rigueur (ici MAXSIZ = NRTM + 100)
     .        BPE(MAXSIZ/3),PE(MAXSIZ),BPN(NSN+NSNR),PN(NSN+NSNR),
     .        OLDNUM(ISZNSNR)
C     REAL
      my_real
     .   DX,DY,DZ,DSUP,SEUIL,SEUILS,SEUILI, XX1, XX2, XX3, XX4,
     .   YY1, YY2, YY3, YY4, ZZ1, ZZ2, ZZ3, ZZ4,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, BGAPSMX, GAPL,
     .   TXX1(3,NVECSZ), TXX2(3,NVECSZ), TXX3(3,NVECSZ), TXX4(3,NVECSZ),
     .   TXMAX(NVECSZ),TXMIN(NVECSZ),TYMAX(NVECSZ),
     .   TYMIN(NVECSZ),TZMAX(NVECSZ),TZMIN(NVECSZ),SMOINS,SPLUS,XX
C-----------------------------------------------
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
C
      BGAPSMX = ZERO
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
        J=NSV(I)
        IF(STFA(J)/=ZERO) THEN
         IF(XA(1,J)>=XMIN.AND.XA(1,J)<=XMAX.AND.
     .      XA(2,J)>=YMIN.AND.XA(2,J)<=YMAX.AND.
     .      XA(3,J)>=ZMIN.AND.XA(3,J)<=ZMAX)THEN
          NB_NC=NB_NC+1
          BPN(NB_NC) = I
         ENDIF
        ENDIF
      ENDDO
C
C Prise en compte candidats non locaux en SPMD
C
      DO I = NSN+1, NSN+NSNR
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C En SPMD, pour inacti ou IFQ, retrouve ancienne numerotation des candidats non locaux
C
      IF(NSPMD>1.AND.
     +  (INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0)) THEN
        CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD)
      END IF
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
      SMOINS = XYZM(DIR,I_ADD)
      SPLUS  = XYZM(DIR+3,I_ADD)
      SEUIL =(SMOINS+SPLUS)*HALF
C
C    2- DIVISER LES NOEUDS EN TWO ZONES
C
      NB_NCN= 0
      NB_NCN1= 0
      ADDNN= ADD(1,I_ADD)
      IF(IGAP==0)THEN
        DO I=1,NB_NC
          J = BPN(I)
          IF(J <= NSN) THEN
            XX = XA(DIR,NSV(J))
          ELSE
            XX = XREM(DIR,J-NSN)
          ENDIF
          IF(XX < SEUIL) THEN
C           ON STOCKE DANS LE BAS DE LA PILE BP
            NB_NCN1 = NB_NCN1 + 1
            ADDNN = ADDNN + 1
            PN(ADDNN) = J
            SMOINS = MAX(SMOINS,XX)
          ELSE
C           ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
            NB_NCN = NB_NCN + 1
            BPN(NB_NCN) = J
            SPLUS = MIN(SPLUS,XX)
          ENDIF
        ENDDO

      ELSE !IF(IGAP == 0)

        GAPSMX = ZERO
        DO I=1,NB_NC
          J = BPN(I)
          IF(J <= NSN) THEN
            XX = XA(DIR,NSV(J))
            IF(XX < SEUIL) THEN
C             ON STOCKE DANS LE BAS DE LA PILE BP
              NB_NCN1 = NB_NCN1 + 1
              ADDNN = ADDNN + 1
              PN(ADDNN) = J
              GAPSMX = MAX(GAPSMX,GAP_S(J))
              SMOINS = MAX(SMOINS,XX)
            ELSE
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
              NB_NCN = NB_NCN + 1
              BPN(NB_NCN) = J
              BGAPSMX = MAX(BGAPSMX,GAP_S(J))
              SPLUS = MIN(SPLUS,XX)
            ENDIF
          ELSE
            XX = XREM(DIR,J-NSN)
            IF(XX < SEUIL) THEN
C             ON STOCKE DANS LE BAS DE LA PILE BP
              NB_NCN1 = NB_NCN1 + 1
              ADDNN = ADDNN + 1
              PN(ADDNN) = J
              GAPSMX = MAX(GAPSMX,XREM(13,J-NSN))
              SMOINS = MAX(SMOINS,XX)
            ELSE
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
              NB_NCN = NB_NCN + 1
              BPN(NB_NCN) = J
              BGAPSMX = MAX(BGAPSMX,XREM(13,J-NSN))
              SPLUS = MIN(SPLUS,XX)
            ENDIF
          ENDIF
        ENDDO

      END IF
C
C    3- DIVISER LES ELEMENTS
C
      IF(IGAP==0) THEN
        NB_ECN= 0
        ADDNE= ADD(2,I_ADD)
        IF(NB_NCN1==0) THEN
          DO I=1,NB_EC
            NE = BPE(I)
            XX1=XA(DIR, IRECT(1,NE))
            XX2=XA(DIR, IRECT(2,NE))
            XX3=XA(DIR, IRECT(3,NE))
            XX4=XA(DIR, IRECT(4,NE))
            XMAX=MAX(XX1,XX2,XX3,XX4)+TZINF+CURV_MAX(NE)
            IF(XMAX >= SPLUS) THEN
C            ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
             NB_ECN = NB_ECN + 1
             BPE(NB_ECN) = NE
            ENDIF
          ENDDO
        ELSEIF(NB_NCN==0) THEN
          DO I=1,NB_EC
            NE = BPE(I)
            XX1=XA(DIR, IRECT(1,NE))
            XX2=XA(DIR, IRECT(2,NE))
            XX3=XA(DIR, IRECT(3,NE))
            XX4=XA(DIR, IRECT(4,NE))
            XMIN=MIN(XX1,XX2,XX3,XX4)-TZINF-CURV_MAX(NE)
            IF(XMIN < SMOINS) THEN
C            ON STOCKE DANS LE BAS DE LA PILE BP
             ADDNE = ADDNE + 1
             PE(ADDNE) = NE
            ENDIF
          ENDDO
        ELSE
          DO I=1,NB_EC
            NE = BPE(I)
            XX1=XA(DIR, IRECT(1,NE))
            XX2=XA(DIR, IRECT(2,NE))
            XX3=XA(DIR, IRECT(3,NE))
            XX4=XA(DIR, IRECT(4,NE))
            XMIN=MIN(XX1,XX2,XX3,XX4)-TZINF-CURV_MAX(NE)
            IF(XMIN < SMOINS) THEN
C             ON STOCKE DANS LE BAS DE LA PILE BP
              ADDNE = ADDNE + 1
              PE(ADDNE) = NE
            ENDIF
            XMAX=MAX(XX1,XX2,XX3,XX4)+TZINF+CURV_MAX(NE)
            IF(XMAX >= SPLUS) THEN
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
              NB_ECN = NB_ECN + 1
              BPE(NB_ECN) = NE
            ENDIF
          ENDDO
        ENDIF
C
C Optimisation gap variable
      ELSE
        NB_ECN= 0
        ADDNE= ADD(2,I_ADD)
        IF(NB_NCN1==0) THEN
          DO I=1,NB_EC
            NE = BPE(I)
            XX1=XA(DIR, IRECT(1,NE))
            XX2=XA(DIR, IRECT(2,NE))
            XX3=XA(DIR, IRECT(3,NE))
            XX4=XA(DIR, IRECT(4,NE))
            XMAX=MAX(XX1,XX2,XX3,XX4)
     +          +MIN(MAX(BGAPSMX+GAP_M(NE),GAPMIN),GAPMAX)
     +          +MARGE+CURV_MAX(NE)+TWO*GAP_SH(NE)
            IF(XMAX >= SPLUS) THEN
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
              NB_ECN = NB_ECN + 1
              BPE(NB_ECN) = NE
            ENDIF
          ENDDO
        ELSEIF(NB_NCN==0) THEN
          DO I=1,NB_EC
            NE = BPE(I)
            XX1=XA(DIR, IRECT(1,NE))
            XX2=XA(DIR, IRECT(2,NE))
            XX3=XA(DIR, IRECT(3,NE))
            XX4=XA(DIR, IRECT(4,NE))
            XMIN=MIN(XX1,XX2,XX3,XX4)
     -          -MIN(MAX(GAPSMX+GAP_M(NE),GAPMIN),GAPMAX)
     -          -MARGE-CURV_MAX(NE)-TWO*GAP_SH(NE)
            IF(XMIN < SMOINS) THEN
C             ON STOCKE DANS LE BAS DE LA PILE BP
              ADDNE = ADDNE + 1
              PE(ADDNE) = NE
            ENDIF
          ENDDO
        ELSE
          DO I=1,NB_EC
            NE = BPE(I)
            XX1=XA(DIR, IRECT(1,NE))
            XX2=XA(DIR, IRECT(2,NE))
            XX3=XA(DIR, IRECT(3,NE))
            XX4=XA(DIR, IRECT(4,NE))
            XMIN=MIN(XX1,XX2,XX3,XX4)
     -          -MIN(MAX(GAPSMX+GAP_M(NE),GAPMIN),GAPMAX)
     -          -MARGE-CURV_MAX(NE)-TWO*GAP_SH(NE)
            IF(XMIN < SMOINS) THEN
C             ON STOCKE DANS LE BAS DE LA PILE BP
              ADDNE = ADDNE + 1
              PE(ADDNE) = NE
            ENDIF
            XMAX=MAX(XX1,XX2,XX3,XX4)
     +          +MIN(MAX(BGAPSMX+GAP_M(NE),GAPMIN),GAPMAX)
     +          +MARGE+CURV_MAX(NE)+TWO*GAP_SH(NE)
            IF(XMAX >= SPLUS) THEN
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
              NB_ECN = NB_ECN + 1
              BPE(NB_ECN) = NE
            ENDIF
          ENDDO
C
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
      XYZM(DIR,I_ADD+1) = SPLUS
      XYZM(DIR+3,I_ADD) = SMOINS
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
C   NCAND_PROV negatif qd NB_EC*NB_NC > 2e31
C
        IF(NB_EC+NB_NC<=NVECSZ) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = NVECSZ+1
        ENDIF

        IF(DSUP<MINBOX.OR.(NB_NC<=NB_N_B)
     &     .OR.(NCAND_PROV<=NVECSZ)) THEN
          NCAND_PROV = NB_EC*NB_NC
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
               XX1=XA(1, N1)
               XX2=XA(1, N2)
               XX3=XA(1, N3)
               XX4=XA(1, N4)
               XMAX=MAX(XX1,XX2,XX3,XX4)+TZINF+CURV_MAX(NE)
               XMIN=MIN(XX1,XX2,XX3,XX4)-TZINF-CURV_MAX(NE)
               XX1=XA(2, N1)
               XX2=XA(2, N2)
               XX3=XA(2, N3)
               XX4=XA(2, N4)
               YMAX=MAX(XX1,XX2,XX3,XX4)+TZINF+CURV_MAX(NE)
               YMIN=MIN(XX1,XX2,XX3,XX4)-TZINF-CURV_MAX(NE)
               XX1=XA(3, N1)
               XX2=XA(3, N2)
               XX3=XA(3, N3)
               XX4=XA(3, N4)
               ZMAX=MAX(XX1,XX2,XX3,XX4)+TZINF+CURV_MAX(NE)
               ZMIN=MIN(XX1,XX2,XX3,XX4)-TZINF-CURV_MAX(NE)
               JJ = BPN(J)
               IF(JJ<=NSN) THEN
                 NN=NSV(JJ)
                 IF(NN/=N1.AND.NN/=N2.AND.NN/=N3.AND.NN/=N4.AND.
     &              XA(1,NN)>XMIN.AND.XA(1,NN)<XMAX.AND.
     &              XA(2,NN)>YMIN.AND.XA(2,NN)<YMAX.AND.
     &              XA(3,NN)>ZMIN.AND.XA(3,NN)<ZMAX ) THEN

                   J_STOK = J_STOK + 1
                   PROV_N(J_STOK) = JJ
                   PROV_E(J_STOK) = NE
                 ENDIF
               ELSE
                 II = JJ-NSN
                 IF(XREM(1,II)>XMIN.AND.
     &              XREM(1,II)<XMAX.AND.
     &              XREM(2,II)>YMIN.AND.
     &              XREM(2,II)<YMAX.AND.
     &              XREM(3,II)>ZMIN.AND.
     &              XREM(3,II)<ZMAX ) THEN
                   J_STOK = J_STOK + 1
                   PROV_N(J_STOK) = JJ
                   PROV_E(J_STOK) = NE
                 ENDIF
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
               XX1=XA(1, N1)
               XX2=XA(1, N2)
               XX3=XA(1, N3)
               XX4=XA(1, N4)
               YY1=XA(2, N1)
               YY2=XA(2, N2)
               YY3=XA(2, N3)
               YY4=XA(2, N4)
               ZZ1=XA(3, N1)
               ZZ2=XA(3, N2)
               ZZ3=XA(3, N3)
               ZZ4=XA(3, N4)
               JJ = BPN(J)
               IF(JJ<=NSN) THEN
                 TZ=MAX(MIN(GAP_S(JJ)+GAP_M(NE),GAPMAX),GAPMIN)
     +             +MARGE+CURV_MAX(NE)+TWO*GAP_SH(NE)
                 XMAX=MAX(XX1,XX2,XX3,XX4)+TZ
                 XMIN=MIN(XX1,XX2,XX3,XX4)-TZ
                 YMAX=MAX(YY1,YY2,YY3,YY4)+TZ
                 YMIN=MIN(YY1,YY2,YY3,YY4)-TZ
                 ZMAX=MAX(ZZ1,ZZ2,ZZ3,ZZ4)+TZ
                 ZMIN=MIN(ZZ1,ZZ2,ZZ3,ZZ4)-TZ
                 NN=NSV(JJ)
                 IF(NN/=N1.AND.NN/=N2.AND.NN/=N3.AND.NN/=N4.AND.
     &              XA(1,NN)>XMIN.AND.XA(1,NN)<XMAX.AND.
     &              XA(2,NN)>YMIN.AND.XA(2,NN)<YMAX.AND.
     &              XA(3,NN)>ZMIN.AND.XA(3,NN)<ZMAX ) THEN
                   J_STOK = J_STOK + 1
                   PROV_N(J_STOK) = JJ
                   PROV_E(J_STOK) = NE
                 ENDIF
               ELSE
                 II = JJ-NSN
                 TZ=MAX(MIN(XREM(13,II)+GAP_M(NE),GAPMAX),GAPMIN)
     +             +MARGE+CURV_MAX(NE)+TWO*GAP_SH(NE)
                 XMAX=MAX(XX1,XX2,XX3,XX4)+TZ
                 XMIN=MIN(XX1,XX2,XX3,XX4)-TZ
                 YMAX=MAX(YY1,YY2,YY3,YY4)+TZ
                 YMIN=MIN(YY1,YY2,YY3,YY4)-TZ
                 ZMAX=MAX(ZZ1,ZZ2,ZZ3,ZZ4)+TZ
                 ZMIN=MIN(ZZ1,ZZ2,ZZ3,ZZ4)-TZ
                 IF(XREM(1,II)>XMIN.AND.
     &              XREM(1,II)<XMAX.AND.
     &              XREM(2,II)>YMIN.AND.
     &              XREM(2,II)<YMAX.AND.
     &              XREM(3,II)>ZMIN.AND.
     &              XREM(3,II)<ZMAX ) THEN
                   J_STOK = J_STOK + 1
                   PROV_N(J_STOK) = JJ
                   PROV_E(J_STOK) = NE
                 ENDIF
               ENDIF
              ENDDO
            END IF
            IF(J_STOK>=NVSIZ)THEN
               CALL I20STO(
     1               NVSIZ,IRECT   ,XA    ,NSV   ,II_STOK,
     2               CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3               I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4               IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5               OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6               GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7               GAP_SH,NBINFLG,MBINFLG,ISYM )

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
      IF(J_STOK/=0)CALL I20STO(
     1              J_STOK,IRECT  ,XA    ,NSV   ,II_STOK,
     2              CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3              I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4              IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5              OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6              GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7              GAP_SH,NBINFLG,MBINFLG,ISYM)
C-------------------------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I20TRI_EDGE                   source/interfaces/intsort/i20tri.F
Chd|-- called by -----------
Chd|        I20BUC_EDGE                   source/interfaces/intsort/i20buce.F
Chd|-- calls ---------------
Chd|        I20STO_EDGE                   source/interfaces/intsort/i20sto.F
Chd|        I7DSTK                        source/interfaces/intsort/i7dstk.F
Chd|        I11INSID                      source/interfaces/intsort/i11tri.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I20TRI_EDGE(
     1      ADD   ,XA    ,NLG   ,
     2      IXLINS,IXLINM,NLINMA,NLINSR,
     3      XYZM  ,I_ADD ,MAXSIZ,II_STOKE ,CAND_S,
     4      CAND_M,NSN4  ,NOINT ,TZINF    ,MAXBOX,
     5      MINBOX,I_MEM ,NB_N_B,I_ADD_MAX,ESHIFT,
     6      ADDCM ,CHAINE,NLINSA ,ITAB     ,NB_OLD,
     7      STFS  ,STFM  ,IAUTO  ,NIN      )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T Y p e s
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
#include      "param_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES ELETS DE BPE ET LES NOEUDS DE BPN EN TWO ZONES
C   > OU < A UNE FRONTIERE ICI DETERMINEE ET SORT LE TOUT
C   DANS bpe,hpe, et bpn,hpn
C-----------------------------------------------
C   D u m m Y   A r g u m e n t s
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
C     IXLINM(2,*)   TABLEAU DES CONEC         E
C     IXLINS(2,*)   TABLEAU DES CONEC         E
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
C   D u m m Y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLINMA,NLINSR,I_ADD,MAXSIZ,I_MEM,ESHIFT,NLINSA,
     .        NSN4,NB_N_B,NOINT,I_ADD_MAX,IAUTO ,NIN,
     .        ADD(2,*),IXLINS(2,*),IXLINM(2,*),
     .        CAND_S(*),CAND_M(*),ADDCM(*),CHAINE(2,*),ITAB(*),
     .        NB_OLD(2,*),NLG(*),II_STOKE
C     REAL
      my_real
     .   XA(3,*),XYZM(6,*),STFS(*),STFM(*),
     .   TZINF,MAXBOX,MINBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,NN1,NN2,
     .        N1,N2,N3,N4,NN,NE,K_STOK,K,L,NCAND_PROV,J_STOK,NI,
     .        ISTOP,NB_ECN1,PROV_S(2*MVSIZ),PROV_M(2*MVSIZ),
     .        NB_NC_OLD, NB_EC_OLD, NB_NC, NB_EC,JJ,KK,
C BPE : utilise sur NLINMA et non NLINMA + 100
C BPN : utilise sur NLINSA et non NLINSA + 100
     .        BPE(NLINMA+100),PE(MAXSIZ),BPN(NLINSA+NLINSR+100),
     .        PN(MAXSIZ)
C     REAL
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

      NB_EC = 0
      DO I=1,NLINMA
C on ne retient plus les facettes detruites
        IF(STFM(I)/=ZERO)THEN
          NB_EC = NB_EC + 1
          BPE(NB_EC) = I
        END IF
      ENDDO
C
C Optimisation // recherche les noeuds compris dans xmin xmax des 
C elements du processeur
C
      NB_NC = 0
      DO I=1,NLINSA
C on ne retient pas les facettes detruites
       IF(STFS(I)/=ZERO)THEN
        N1=IXLINS(1,I)
        N2=IXLINS(2,I)
        XMINS = MIN(XA(1,N1),XA(1,N2))
        YMINS = MIN(XA(2,N1),XA(2,N2))
        ZMINS = MIN(XA(3,N1),XA(3,N2))
        XMAXS = MAX(XA(1,N1),XA(1,N2))
        YMAXS = MAX(XA(2,N1),XA(2,N2))
        ZMAXS = MAX(XA(3,N1),XA(3,N2))
        IF(XMAXS>=XMIN.AND.XMINS<=XMAX.AND.
     .     YMAXS>=YMIN.AND.YMINS<=YMAX.AND.
     .     ZMAXS>=ZMIN.AND.ZMINS<=ZMAX)THEN
          NB_NC = NB_NC + 1
          BPN(NB_NC) = I
        ENDIF
       END IF
      ENDDO
C
C Prise en compte candidats non locaux en SPMD
C
      DO I = NLINSA+1, NLINSA+NLINSR
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
      XMIN = 1.E30
      XMAX = -1.E30

      YMIN = 1.E30
      YMAX = -1.E30

      ZMIN = 1.E30
      ZMAX = -1.E30

      DO I=1,NB_EC
        NE = BPE(I)
        XX1=XA(1, IXLINM(1,NE))
        XX2=XA(1, IXLINM(2,NE))
        XMIN=MIN(XMIN,XX1,XX2)
        XMAX=MAX(XMAX,XX1,XX2)

        YY1=XA(2, IXLINM(1,NE))
        YY2=XA(2, IXLINM(2,NE))
        YMIN=MIN(YMIN,YY1,YY2)
        YMAX=MAX(YMAX,YY1,YY2)

        ZZ1=XA(3, IXLINM(1,NE))
        ZZ2=XA(3, IXLINM(2,NE))
        ZMIN=MIN(ZMIN,ZZ1,ZZ2)
        ZMAX=MAX(ZMAX,ZZ1,ZZ2)
      ENDDO

c     reduction de la taille de boite:
c     on garde une marge de TZINF quand on reduit la taille de boite
c     pour eviter d'oublier des seconds
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
c      |     .     |\\\\\#///les mains//|/////./#
c      |     .     |\\\\\#////////////////|/////./#
c      |     .     |\\\\\#////////////////|/////./#
c      |     .     |\\\\\#################|#####.##          ^
c      |     .     |\\\  espace retenu  \\|     .            |
c      |     .     |\\\pour les seconds\\|     .            | Tzinf
c      |     .     |\\\(nouvelle boite) \\|     .            |
c      |     .     +----------------------|     ....Ymin     x
c      |     .                            |     .            |
c      |     .    (boite de recherche main)   .            | Tzinf
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
c           Txmin = Xmin  avec Xmin = min(Xmain)-Tzinf > Xmin_old
c       
c       mais en utilisant:
c           Txmin = Xmin-Tzinf (= min(Xmain) - 2*Tzinf) 
c       on ne penalise pas I11INSIND 
c       (il n'y a pas de main dans la zone surestimee)
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

      IF(DMY==DSUP) THEN
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
      ELSE IF(DMZ==DSUP) THEN
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
C    2- DIVISER LES SECONDS EN TWO ZONES 
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
      DO I=1,NB_NC
        NN = BPN(I)
        IF(NN<=NLINSA) THEN
          XX1=XA(DIR,IXLINS(1,NN))
          XX2=XA(DIR,IXLINS(2,NN))
          XY1=XA(JJ, IXLINS(1,NN))
          XY2=XA(JJ, IXLINS(2,NN))
          XZ1=XA(KK, IXLINS(1,NN))
          XZ2=XA(KK, IXLINS(2,NN))
        ELSE
          NI = NN-NLINSA
          XX1=XREM(DIR+1,NI)
          XX2=XREM(DIR+9,NI)
          XY1=XREM(JJ+1 ,NI)
          XY2=XREM(JJ+9 ,NI)
          XZ1=XREM(KK+1 ,NI)
          XZ2=XREM(KK+9 ,NI)
        END IF
        XMAX=MAX(XX1,XX2)
        XMIN=MIN(XX1,XX2)
        IF(XMIN<SEUIL.AND.XMAX>=XIMIN) THEN
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        XIMIN,SEUIL,XJMIN,XJMAX,XKMIN,XKMAX)) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = NN
         END IF
        END IF
        IF(XMAX>=SEUIL.AND.XMIN<=XIMAX) THEN
         IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .        SEUIL,XIMAX,XJMIN,XJMAX,XKMIN,XKMAX)) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = NN
         ENDIF
        ENDIF
      ENDDO
C
C    3- DIVISER LES MAINS
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
      IF(NB_NCN1==0) THEN
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XA(DIR, IXLINM(1,NE))
         XX2=XA(DIR, IXLINM(2,NE))
         IF(MAX(XX1,XX2)>=TSMIN) THEN
          XY1=XA(JJ, IXLINM(1,NE))
          XY2=XA(JJ, IXLINM(2,NE))
          XZ1=XA(KK, IXLINM(1,NE))
          XZ2=XA(KK, IXLINM(2,NE))
          IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .         TSMIN,TIMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C          ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
           NB_ECN = NB_ECN + 1
           BPE(NB_ECN) = NE
          ENDIF
         ENDIF
        ENDDO
      ELSEIF(NB_NCN==0) THEN
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XA(DIR, IXLINM(1,NE))
         XX2=XA(DIR, IXLINM(2,NE))
         IF(MIN(XX1,XX2)<TSMAX) THEN
          XY1=XA(JJ, IXLINM(1,NE))
          XY2=XA(JJ, IXLINM(2,NE))
          XZ1=XA(KK, IXLINM(1,NE))
          XZ2=XA(KK, IXLINM(2,NE))
          IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .         TIMIN,TSMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C          ON STOCKE DANS LE BAS DE LA PILE BP
           ADDNE = ADDNE + 1
           NB_ECN1= NB_ECN1 + 1
           PE(ADDNE) = NE
          ENDIF
         ENDIF
        ENDDO
      ELSE
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=XA(DIR, IXLINM(1,NE))
         XX2=XA(DIR, IXLINM(2,NE))
         XY1=XA(JJ, IXLINM(1,NE))
         XY2=XA(JJ, IXLINM(2,NE))
         XZ1=XA(KK, IXLINM(1,NE))
         XZ2=XA(KK, IXLINM(2,NE))
         IF(MIN(XX1,XX2)<TSMAX) THEN
          IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .         TIMIN,TSMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C          ON STOCKE DANS LE BAS DE LA PILE BP
           ADDNE = ADDNE + 1
           NB_ECN1= NB_ECN1 + 1
           PE(ADDNE) = NE
          ENDIF
         ENDIF
         IF(MAX(XX1,XX2)>=TSMIN) THEN
          IF(I11INSID(XX1,XX2,XY1,XY2,XZ1,XZ2,
     .         TSMIN,TIMAX,TJMIN,TJMAX,TKMIN,TKMAX)) THEN
C          ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
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
C                    LE DECOUPAGE NE REDUIT PAS LES CANDIDATS
C
C-------------------TEST SUR MEMOIRE DEPASSEE------------
C
      IF(ADD(1,I_ADD)+NB_NC>MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES COTES SECONDS BOITES TROP PETITES
        I_MEM = 1
        RETURN
      ENDIF
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES COTES MAINS BOITES TROP PETITES
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
        IF(NB_EC+NB_NC<=128) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = 129
        ENDIF
C
         NB_NC_OLD = NB_OLD(1,I_ADD)
         NB_EC_OLD = NB_OLD(2,I_ADD)

        IF(DSUP<MINBOX.OR.
     .     NB_NC<=NB_N_B.OR.NB_EC<=NB_N_B.OR.
     .     NCAND_PROV<=128.OR.(NB_EC==NB_EC_OLD
     .     .AND.NB_NC==NB_NC_OLD)) THEN
C
          NCAND_PROV = NB_EC*NB_NC
          DO K=1,NCAND_PROV,NVSIZ
           DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
              I = 1+(L-1)/NB_NC
              J = L-(I-1)*NB_NC
              NE = BPE(I)
              NN = BPN(J)
              N1=IXLINM(1,NE)
              N2=IXLINM(2,NE)
              IF(NN<=NLINSA) THEN
                NN1=IXLINS(1,NN)
                NN2=IXLINS(2,NN)
                IF(IAUTO==0 .OR. ITAB(NLG(N1))>ITAB(NLG(NN1)) )THEN
                 IF(NN1/=N1.AND.NN1/=N2.AND.
     .               NN2/=N1.AND.NN2/=N2) THEN
                      J_STOK = J_STOK + 1
                      PROV_S(J_STOK) = NN
                      PROV_M(J_STOK) = NE
                 ENDIF
                ENDIF
              ELSE
                NI = NN-NLINSA
                IF(IR4R8 == 2) THEN
                  NN1 = NINT(XREM(9,NI))              
                  NN2 = NINT(XREM(17,NI))
                ELSE
                  NN1 = IREM(1,NI)             
                  NN2 = IREM(2,NI)
                END IF
                N1 = ITAB(NLG(N1))
                N2 = ITAB(NLG(N2))
                IF(IAUTO==0 .OR. N1>NN1 )THEN
                 IF(NN1/=N1.AND.NN1/=N2.AND.
     .             NN2/=N1.AND.NN2/=N2) THEN
                    J_STOK = J_STOK + 1
                    PROV_S(J_STOK) = NN
                    PROV_M(J_STOK) = NE
                 ENDIF
                ENDIF
              END IF
           ENDDO
           IF(J_STOK>=NVSIZ)THEN
              CALL I20STO_EDGE(
     1              NVSIZ,IXLINS,IXLINM,XA    ,II_STOKE,
     2              CAND_S,CAND_M,NSN4  ,NOINT ,TZINF ,
     3              I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM ,
     4              CHAINE,NLINSA ,NIN   )
              IF(I_MEM==2)RETURN
              J_STOK = J_STOK-NVSIZ
#include "vectorize.inc"
              DO J=1,J_STOK
                PROV_S(J) = PROV_S(J+NVSIZ)
                PROV_M(J) = PROV_M(J+NVSIZ)
              ENDDO
           ENDIF
          ENDDO 
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
      IF(J_STOK/=0)CALL I20STO_EDGE(
     1              J_STOK,IXLINS,IXLINM,XA    ,II_STOKE,
     2              CAND_S,CAND_M,NSN4  ,NOINT ,TZINF  ,
     3              I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM  ,
     4              CHAINE,NLINSA ,NIN   )
C-------------------------------------------------------------------------
      RETURN
      END

