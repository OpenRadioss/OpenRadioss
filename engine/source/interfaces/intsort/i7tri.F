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
Chd|  I7TRI                         source/interfaces/intsort/i7tri.F
Chd|-- called by -----------
Chd|        I7BUCE                        source/interfaces/intsort/i7buce.F
Chd|-- calls ---------------
Chd|        I7DSTK                        source/interfaces/intsort/i7dstk.F
Chd|        I7STO                         source/interfaces/intsort/i7sto.F
Chd|        SPMD_OLDNUMCD                 source/mpi/interfaces/spmd_i7tool.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7TRI(
     1      ADD   ,NSN    ,RENUM  ,NSNR     ,ISZNSNR  ,
     2      IRECT ,X      ,STF    ,STFN     ,XYZM     ,
     3      I_ADD ,NSV    ,MAXSIZ ,II_STOK  ,CAND_N   ,
     4      CAND_E,MULNSN ,NOINT  ,TZINF    ,MAXBOX   ,
     5      MINBOX,I_MEM  ,NB_N_B ,I_ADD_MAX,ESHIFT   ,
     6      INACTI,IFQ    ,CAND_A ,CAND_P   ,IFPEN    ,
     7      NRTM  ,NSNROLD,IGAP,GAP      ,GAP_S    ,
     8      GAP_M ,GAPMIN ,GAPMAX ,MARGE    ,CURV_MAX ,
     9      NIN   ,GAP_S_L,GAP_M_L,INTTH   ,DRAD  ,ITIED    ,
     A      CAND_F ,KREMNOD  ,REMNOD  ,FLAGREMNODE)
C=======================================================================
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
#include      "ige3d_c.inc"
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
C     PROV_N       CAND_N provisoire (variable static dans i7tri)
C     PROV_E       CAND_E provisoire (variable static dans i7tri)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_ADD,MAXSIZ,I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NSNROLD,
     .        MULNSN,NB_N_B,NOINT,I_ADD_MAX,INACTI,IFQ,NSNR,IGAP,NIN,
     .        ADD(2,*),IRECT(4,*),
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        INTTH,II_STOK,ITIED
      INTEGER KREMNOD(*),REMNOD(*),FLAGREMNODE
C     REAL ou REAL*8
      my_real
     .   X(3,*),XYZM(6,*),CAND_P(*),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MAXBOX,MINBOX,MARGE,GAP,GAPMIN,GAPMAX,
     .   CURV_MAX(*),GAP_S_L(*),GAP_M_L(*),CAND_F(*),DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,J_STOK,II,JJ,
     .        PROV_N(2*MVSIZ),PROV_E(2*MVSIZ),
     .        TN1(NVECSZ),TN2(NVECSZ),TN3(NVECSZ),TN4(NVECSZ),
C BPE : utilise sur NRTM et non NRTM + 100 en toute rigueur (ici MAXSIZ = NRTM + 100)
     .        BPE(MAXSIZ/3),PE(MAXSIZ),BPN(NSN+NSNR),PN(NSN+NSNR),
     .        OLDNUM(ISZNSNR),IADD
C     REAL ou REAL*8
      my_real
     .   AAA,
     .   DX,DY,DZ,DSUP,TRHRESHOLD, XX1, XX2, XX3, XX4,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, BGAPSMX, GAPL,
     .   TXX1(3,NVECSZ), TXX2(3,NVECSZ), TXX3(3,NVECSZ), TXX4(3,NVECSZ),
     .   TXMAX(NVECSZ),TXMIN(NVECSZ),TYMAX(NVECSZ),
     .   TYMIN(NVECSZ),TZMAX(NVECSZ),TZMIN(NVECSZ),SMOINS,SPLUS,XX
C REMNODE
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGREMNODE
      INTEGER DELNOD,M

C-----------------------------------------------
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
C

      IF(FLAGREMNODE == 2) ALLOCATE(TAGREMNODE(NUMNOD+NUMFAKENODIGEO))

      XMIN = XYZM(1,I_ADD)
      YMIN = XYZM(2,I_ADD)
      ZMIN = XYZM(3,I_ADD)
      XMAX = XYZM(4,I_ADD)
      YMAX = XYZM(5,I_ADD)
      ZMAX = XYZM(6,I_ADD)
C
C     DRAD = ZERO
C
C Copie des nos de segments et de noeuds dans BPE ET BPN
C
      NB_EC = 0
      DO I=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(I).NE.ZERO)THEN
          NB_EC = NB_EC + 1
          BPE(NB_EC) = I
        ENDIF
      ENDDO

      IF(IGAP.EQ.3) THEN
        IADD = 10
      ENDIF
C
C Optimisation // recherche les noeuds compris dans xmin xmax des 
C elements du processeur
C
      NB_NC = 0
      DO I=1,NSN
        J=NSV(I)
        IF(STFN(I).NE.ZERO) THEN

         IF(X(1,J).GE.XMIN.AND.X(1,J).LE.XMAX.AND.
     .     X(2,J).GE.YMIN.AND.X(2,J).LE.YMAX.AND.
     .     X(3,J).GE.ZMIN.AND.X(3,J).LE.ZMAX)THEN

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
      IF(NSPMD.GT.1.AND.
     +  (INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0.OR.
     +   ITIED.NE.0)) THEN
        CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD)
      END IF
C
      J_STOK = 0
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
      DIR = 1
      IF(DY.EQ.DSUP) THEN
        DIR = 2
      ELSE IF(DZ.EQ.DSUP) THEN
        DIR = 3
      ENDIF
      SMOINS = XYZM(DIR,I_ADD)
      SPLUS  = XYZM(DIR+3,I_ADD)
      TRHRESHOLD =(SMOINS+SPLUS)*HALF
C
C    2- DIVISER LES NOEUDS EN TWO ZONES
C
      NB_NCN= 0
      NB_NCN1= 0
      ADDNN= ADD(1,I_ADD)
C
      GAPSMX = ZERO
#include "vectorize.inc"
      DO I=1,NB_NC
       J = BPN(I)
       IF(J <= NSN) THEN
        XX = X(DIR,NSV(J))
        IF(XX < TRHRESHOLD) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = J
          IF(IGAP .NE.0) GAPSMX = MAX(GAPSMX,GAP_S(J))
          SMOINS = MAX(SMOINS,XX)
        ENDIF
       ENDIF
      ENDDO
#include "vectorize.inc"
      DO I=1,NB_NC
       J = BPN(I)
       IF(J > NSN) THEN
        XX = XREM(DIR,J-NSN)
        IF(XX < TRHRESHOLD) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = J
          IF(IGAP.NE.0) GAPSMX = MAX(GAPSMX,XREM(9,J-NSN))
          SMOINS = MAX(SMOINS,XX)
        ENDIF
       ENDIF
      ENDDO
      BGAPSMX = ZERO
#include "vectorize.inc"
      DO I=1,NB_NC
       J = BPN(I)
       IF(J <= NSN) THEN
        XX = X(DIR,NSV(J))
        IF(XX >= TRHRESHOLD) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = J
          IF(IGAP.NE.0) BGAPSMX = MAX(BGAPSMX,GAP_S(J))
          SPLUS = MIN(SPLUS,XX)
        ENDIF
       ENDIF
      ENDDO
#include "vectorize.inc"
      DO I=1,NB_NC
       J = BPN(I)
       IF(J > NSN) THEN
        XX = XREM(DIR,J-NSN)
        IF(XX >= TRHRESHOLD) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = J
          IF(IGAP .NE. 0) BGAPSMX = MAX(BGAPSMX,XREM(9,J-NSN))
          SPLUS = MIN(SPLUS,XX)
        ENDIF
       ENDIF
      ENDDO
C
C    3- DIVISER LES ELEMENTS
C
      NB_ECN= 0
      ADDNE= ADD(2,I_ADD)
      IF(NB_NCN1.EQ.0) THEN
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF(IGAP == 0) THEN
           AAA = TZINF+CURV_MAX(NE)
         ELSEIF(IGAP == 3) THEN
           AAA =  MAX(DRAD,MIN(MAX(BGAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +       +MARGE+CURV_MAX(NE)
         ELSE
           AAA = MAX(DRAD,MIN(MAX(BGAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
     +       +MARGE+CURV_MAX(NE)
         ENDIF
         XMAX = MAX(XX1,XX2,XX3,XX4) + AAA
         IF(XMAX >= SPLUS) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
        ENDDO
       ELSEIF(NB_NCN == 0) THEN 
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF( IGAP == 0 ) THEN
          AAA = -TZINF-CURV_MAX(NE)
         ELSEIF(IGAP == 3) THEN
          AAA = -MAX(DRAD,MIN(MAX(GAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +          -MARGE-CURV_MAX(NE)
         ELSE
          AAA = -MAX(DRAD,MIN(MAX(GAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
     -           -MARGE-CURV_MAX(NE)
         ENDIF
         XMIN = MIN(XX1,XX2,XX3,XX4) + AAA

         IF(XMIN < SMOINS) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
       ELSE
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF( IGAP == 0 ) THEN
           AAA=-TZINF-CURV_MAX(NE)
         ELSEIF(IGAP == 3) THEN
           AAA=  - MAX(DRAD,MIN(MAX(GAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +       -MARGE-CURV_MAX(NE)
         ELSE
           AAA=  -MAX(DRAD,MIN(MAX(GAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
     -       -MARGE-CURV_MAX(NE)
         ENDIF
         XMIN =  MIN(XX1,XX2,XX3,XX4) + AAA 
         IF(XMIN < SMOINS) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
C
#include "vectorize.inc"
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF( IGAP == 0) THEN
            AAA =TZINF+CURV_MAX(NE)
         ELSEIF( IGAP==3 ) THEN
            AAA=  MAX(DRAD,MIN(MAX(BGAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +       +MARGE+CURV_MAX(NE)
         ELSE
          AAA = MAX(DRAD,MIN(MAX(BGAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
     +       +MARGE+CURV_MAX(NE)
         ENDIF
         XMAX = MAX(XX1,XX2,XX3,XX4) + AAA

         IF(XMAX >= SPLUS) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = NE
         ENDIF
       ENDDO
      ENDIF !NBNC1
C
C    4- REMPLIR LES TABLEAUX D'ADRESSES
C
      ADD(1,I_ADD+1) = ADDNN
      ADD(2,I_ADD+1) = ADDNE
C-----on remplit les min de la boite suivante et les max de la courante
C     (i.e. TRHRESHOLD est un max pour la courante)
C     on va redescendre et donc on definit une nouvelle boite
C     on remplit les max de la nouvelle boite
C     initialises dans i7buc1 a 1.E30 comme ca on recupere
C     soit XMAX soit le max de la boite
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
      IF(I_ADD+1.GE.I_ADD_MAX) THEN
c       WRITE(6,*) __LINE__,__LINE__
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
C                    BOITE NE CONTENANT QU'ONE NOEUD
C                    PLUS DE MEMOIRE DISPONIBLE
C
C-------------------TEST SUR MEMOIRE DEPASSEE------------
C
      IF(ADD(2,I_ADD)+NB_EC.GT.MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS BOITES TROP PETITES
         WRITE(6,*) __LINE__,__LINE__

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
C       NCAND_PROV=NB_EC*NB_NC
Cel NCAND_PROV negatif qd NB_EC*NB_NC > 2e31
C
        IF(NB_EC+NB_NC.LE.NVECSZ) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = NVECSZ+1
        ENDIF
        IF(DSUP.LT.MINBOX.OR.(NB_NC.LE.NB_N_B)
     &     .OR.(NCAND_PROV.LE.NVECSZ)) THEN
          NCAND_PROV = NB_EC*NB_NC

          IF(FLAGREMNODE==2) THEN
            DO I=1,NUMNOD+NUMFAKENODIGEO
              TAGREMNODE(I) = 0
            ENDDO
          ENDIF

          DO K=1,NCAND_PROV,NVSIZ
            DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
             I = 1+(L-1)/NB_NC
             J = L-(I-1)*NB_NC
             NE = BPE(I)
             N1=IRECT(1,NE)
             N2=IRECT(2,NE)
             N3=IRECT(3,NE)
             N4=IRECT(4,NE)

             IF(FLAGREMNODE==2) THEN
               DO M= KREMNOD(2*(NE-1)+1)+1, KREMNOD(2*(NE-1)+2)
                 TAGREMNODE(REMNOD(M)) = 1
               ENDDO
             ENDIF
             JJ = BPN(J)
             IF( JJ.LE.NSN ) THEN
               IF( IGAP == 0 ) THEN
                 TZ = TZINF+CURV_MAX(NE)
               ELSEIF( IGAP == 3 ) THEN
                 TZ = MAX(DRAD,MAX(MIN(GAP_S_L(JJ)+GAP_M_L(NE),GAPMAX),GAPMIN)
     .           +MARGE+CURV_MAX(NE))
               ELSE
                 TZ=MAX(DRAD,MAX(MIN(GAP_S(JJ)+GAP_M(NE),GAPMAX),GAPMIN)
     +           +MARGE+CURV_MAX(NE))
               ENDIF
             ELSE
               II = JJ-NSN
               IF( IGAP == 0 ) THEN
                 TZ = TZINF+CURV_MAX(NE)
               ELSEIF( IGAP == 3 ) THEN
                 TZ = MAX(DRAD,MAX(MIN(XREM(IADD,II)+GAP_M_L(NE)
     .           ,GAPMAX),GAPMIN))+MARGE+CURV_MAX(NE)
               ELSE
                 TZ = MAX(DRAD,MAX(MIN(XREM(9,II)+GAP_M(NE),GAPMAX),GAPMIN))
     +           +MARGE+CURV_MAX(NE)
               ENDIF
             ENDIF
             XX1=X(1, N1)
             XX2=X(1, N2)
             XX3=X(1, N3)
             XX4=X(1, N4)
             XMAX=MAX(XX1,XX2,XX3,XX4)+TZ
             XMIN=MIN(XX1,XX2,XX3,XX4)-TZ
             XX1=X(2, N1)
             XX2=X(2, N2)
             XX3=X(2, N3)
             XX4=X(2, N4)
             YMAX=MAX(XX1,XX2,XX3,XX4)+TZ
             YMIN=MIN(XX1,XX2,XX3,XX4)-TZ
             XX1=X(3, N1)
             XX2=X(3, N2)
             XX3=X(3, N3)
             XX4=X(3, N4)
             ZMAX=MAX(XX1,XX2,XX3,XX4)+TZ
             ZMIN=MIN(XX1,XX2,XX3,XX4)-TZ
             IF(JJ.LE.NSN) THEN

               IF(FLAGREMNODE==2) THEN
                 IF(TAGREMNODE(NSV(JJ)) == 1) CYCLE
               ENDIF
               NN=NSV(JJ)
               IF(NN.NE.N1.AND.NN.NE.N2.AND.NN.NE.N3.AND.NN.NE.N4.AND.
     &           X(1,NN).GT.XMIN.AND.X(1,NN).LT.XMAX.AND.
     &           X(2,NN).GT.YMIN.AND.X(2,NN).LT.YMAX.AND.
     &           X(3,NN).GT.ZMIN.AND.X(3,NN).LT.ZMAX ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = JJ
                 PROV_E(J_STOK) = NE
               ENDIF
             ELSE
               II = JJ-NSN
               IF(FLAGREMNODE==2) THEN
                 DO M= KREMNOD(2*(NE-1)+2) + 1,  KREMNOD(2*(NE-1)+3)
                   IF(REMNOD(M) == -IREM(2,II) ) THEN
                     DELNOD = DELNOD + 1
                     EXIT
                   ENDIF
                 ENDDO
                 IF(DELNOD .NE. 0) CYCLE
               ENDIF
               IF(XREM(1,II).GT.XMIN.AND.
     &            XREM(1,II).LT.XMAX.AND.
     &            XREM(2,II).GT.YMIN.AND.
     &            XREM(2,II).LT.YMAX.AND.
     &            XREM(3,II).GT.ZMIN.AND.
     &            XREM(3,II).LT.ZMAX ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = JJ
                 PROV_E(J_STOK) = NE
               ENDIF
             ENDIF
            ENDDO ! L=K,MIN(K-1+NVSIZ,NCAND_PROV)

            IF(J_STOK.GE.NVSIZ)THEN
              CALL I7STO(
     1             NVSIZ,IRECT   ,X     ,NSV   ,II_STOK,
     2             CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3             I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4             IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5             OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6             GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7             GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8             CAND_F)
               IF(I_MEM.EQ.2) THEN
                 RETURN
               ENDIF
               J_STOK = J_STOK-NVSIZ
#include "vectorize.inc"
               DO J=1,J_STOK
                 PROV_N(J) = PROV_N(J+NVSIZ)
                 PROV_E(J) = PROV_E(J+NVSIZ)
               ENDDO
            ENDIF ! J_STOK >= NVSIZ

          ENDDO ! LOOP OVER POSSIBLE CANDIDATES
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
      IF(J_STOK.NE.0)CALL I7STO(
     1              J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2              CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3              I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4              IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5              OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6              GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7              GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8              CAND_F )
C-------------------------------------------------------------------------
      IF(FLAGREMNODE==2) THEN
        DEALLOCATE(TAGREMNODE)
      ENDIF
      RETURN
      END
