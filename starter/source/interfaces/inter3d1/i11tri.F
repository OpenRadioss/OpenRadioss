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
Chd|  I11TRI                        source/interfaces/inter3d1/i11tri.F
Chd|-- called by -----------
Chd|        I11BUC1                       source/interfaces/inter3d1/i11buc1.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I11STO                        source/interfaces/inter3d1/i11sto.F
Chd|        I7DSTK                        source/interfaces/inter3d1/i7dstk.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I11TRI(
     1      BPE   ,PE    ,BPN   ,PN    ,ADD,
     2      IRECTS,X     ,NB_SC ,NB_MC ,XYZM,
     3      I_ADD ,IRECTM,I_AMAX,ISTOP  ,
     4      MAXSIZ,I_STOK,I_MEM ,NB_N_B,IADFIN,
     5      CAND_S,CAND_M,NSN   ,NOINT ,TZINF ,
     6      MAXBOX,MINBOX,J_STOK,ADDCM,CHAINE,
     7      PROV_S,PROV_M,II_STOK,MULTIMP,ID,TITR)
      USE MESSAGE_MOD
C============================================================================
C  cette routine est appelee par : I11BUC1(/inter3d1/i11buc1.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I11STO(/inter3d1/i11sto.F)
C                          I7DSTK(/inter3d1/i7dstk.F)
C                          ARRET(/sortie1/arret.F)
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB_SC,NB_MC,I_ADD,MAXSIZ,I_STOK,J_STOK,I_MEM
      INTEGER I_BID, I_AMAX,NB_N_B, NOINT, NSN,MULTIMP,ISTOP,
     .        IADFIN,II_STOK
      INTEGER ADD(2,0:*),IRECTS(2,*),IRECTM(2,*),BPE(*),PE(*)
      INTEGER CAND_S(*),CAND_M(*),BPN(*),PN(*)
      INTEGER ADDCM(*),CHAINE(*)
      INTEGER PROV_S(2*MVSIZ),PROV_M(2*MVSIZ)
C     REAL
      my_real
     .   X(3,*),XYZM(6,*),TZINF,DBUC,
     .   MAXBOX,MINBOX
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_SCN1,NB_MCN1,NB_SCN,NB_MCN,ADDNN,ADDNE,IPOS,
     .        I,IP,J,K,L
      INTEGER INF,SUP,DIR,N1,N2,NN1,NN2,NN,NE,MEMX,NCAND_PROV
C     REAL
      my_real
     .   DX,DY,DZ,DSUP,SEUIL,XMX,XMN,XX1,XX2,XMIN, XMAX
CC      integer idb1,idb2,idb3,idb4
CC      save idb1,idb2,idb3,idb4
C-----------------------------------------------
      DATA MEMX/0/
CCctmp
CC      data idb1/-1/
CC      data idb2/-1/
CC      data idb3/-1/
CC      data idb4/-1/
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
C     BPE          TABLEAU DES FACETTES A TRIER      E/S
C                  ET DU RESULTAT COTE MAX            
C     PE           TABLEAU DES FACETTES              S
C                  RESULTAT COTE MIN
C     BPN          TABLEAU DES NOEUDS A TRIER        E/S
C                  ET DU RESULTAT COTE MAX            
C     PN           TABLEAU DES NOEUDS                S
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
C     NB_SC        NOMBRE DE NOEUDS CANDIDATS        E/S
C     NB_MC        NOMBRE D'ELTS CANDIDATS           E/S
C     I_ADD        POSITION DANS LE TAB DES ADRESSES E/S
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     I_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     CAND_S       boites resultats noeuds
C     CAND_M       adresses des boites resultat elements
C     NSN          4*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C     MAXBOX       TAILLE MAX BUCKET
C     MINBOX       TAILLE MIN BUCKET
C=======================================================================
C
C
C    1- TEST ARRET = BOITE VIDE
C                    BOITE TROP PETITE 
C                    BOITE NE CONTENANT QU'ONE NOEUD
C                    PLUS DE MEMOIRE DISPONIBLE
C
C-----------------------------------------------------------
C
C      IF(MEMX.GT.ADD(2,1)+NB_MC)THEN
C        WRITE(ISTDO,*)' *******MEM MAX=',MEMX
C        MEMX=-1
C      ELSEIF(MEMX.NE.-1)THEN
C        MEMX=ADD(2,1)+NB_MC
C      ENDIF
C--------------------TEST SUR BOITE VIDES-------------- 
C
      IF(NB_MC.EQ.0.OR.NB_SC.EQ.0) THEN
C       write(6,*)" BOITE VIDE"
C       IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C       AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
Cgw006        CALL I7DSTK(I_ADD,NB_SC,NB_MC,ADD,BPN,PN,BPE,PE)
        CALL I7DSTK(I_ADD,NB_SC,NB_MC,ADD(1,I_ADD-1),BPN,PN,BPE,PE)
        RETURN
      ENDIF
C
C-------------------TEST SUR FIN DE BRANCHE / MEMOIRE DEPASSEE------------
C
      DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
      DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
      DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
      DSUP= MAX(DX,DY,DZ)
C
Cgw006      IF(ADD(1,I_ADD)+NB_SC.GE.MAXSIZ.OR.ADD(2,1)+NB_MC.GE.MAXSIZ) THEN
      IF(ADD(1,I_ADD)+NB_SC.GE.MAXSIZ.OR.
     .   ADD(2,I_ADD)+NB_MC.GE.MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS BOITES TROP PETITES
        IF ( NB_N_B .EQ. MAXSIZ/3) THEN
C          WRITE(IOUT,*)'***ERROR INFINITE LOOP DETECTED '
C          WRITE(ISTDO,*)'***ERROR INFINITE LOOP DETECTED '
C          CALL ARRET(2)
          CALL ANCMSG(MSGID=685,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
        I_MEM = 1
        RETURN
      ENDIF
      NCAND_PROV=NB_MC*NB_SC
      IF(DSUP.LT.MINBOX.OR.ISTOP.EQ.1.OR.
     .   (NB_SC.LE.NB_N_B.AND.DSUP.LT.MAXBOX).OR.   
     .   (NB_SC.LE.NB_N_B.AND.NB_MC.EQ.1).OR.
     .   (NB_MC.LE.NB_N_B.AND.DSUP.LT.MAXBOX).OR.   
     .   (NB_MC.LE.NB_N_B.AND.NB_SC.EQ.1)) THEN
          ISTOP = 0
C
C       write(6,*)" NOUVELLE BOITE "
C       1- STOCKAGE DU OU DES  NOEUD CANDIDAT ET DES ELTS CORRESP.
C       VIRER LES INUTILES
          DO K=1,NCAND_PROV,NVSIZ
            DO L=K,MIN(K-1+NVSIZ,NCAND_PROV)
              I = 1+(L-1)/NB_SC
              J = L-(I-1)*NB_SC
              NE = BPE(I)
              NN = BPN(J)
CCctmp
CC       if(idb1.eq.nn.and.idb2.eq.ne)then
CC          idb3=-1
CC       endif
              N1=IRECTM(1,NE)
              N2=IRECTM(2,NE)
              NN1=IRECTS(1,NN)
              NN2=IRECTS(2,NN)
              IF(NN1.NE.N1.AND.NN1.NE.N2.AND.
     .           NN2.NE.N1.AND.NN2.NE.N2) THEN
                J_STOK = J_STOK + 1
                PROV_S(J_STOK) = NN
                PROV_M(J_STOK) = NE
              ENDIF
            ENDDO
            IF(J_STOK.GE.NVSIZ)THEN
              CALL I11STO(
     1              NVSIZ,IRECTS,IRECTM,X     ,II_STOK,
     2              CAND_S,CAND_M,NSN   ,NOINT ,TZINF ,
     3              I_MEM ,PROV_S,PROV_M,MULTIMP,ADDCM,
     4              CHAINE,IADFIN)
              IF(I_MEM.EQ.2)RETURN
              J_STOK = J_STOK-NVSIZ
              DO J=1,J_STOK
                PROV_S(J) = PROV_S(J+NVSIZ)
                PROV_M(J) = PROV_M(J+NVSIZ)
              ENDDO
            ENDIF
          ENDDO 
C       IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C       AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
Cgw006        CALL I7DSTK(I_ADD,NB_SC,NB_MC,ADD,BPN,PN,BPE,PE)
        CALL I7DSTK(I_ADD,NB_SC,NB_MC,ADD(1,I_ADD-1),BPN,
     .              PN,BPE,PE)
        RETURN
      ENDIF
C
C-----------------------------------------------------------
C
C
C    2- PHASE DE TRI SUR LA MEDIANE SELON LA + GDE DIRECTION
C                    
C                   
C-----------------------------------------------------------
C
C
C    1- DETERMINER LA DIRECTION A DIVISER X,Y OU Z
C
      DIR = 1
      IF(DY.EQ.DSUP) THEN
        DIR = 2
      ELSE IF(DZ.EQ.DSUP) THEN
        DIR = 3
      ENDIF
      SEUIL =(XYZM(DIR+3,I_ADD)+XYZM(DIR,I_ADD))/2
C
C    2- DIVISER LES SECONDS EN TWO ZONES 
C
CC      idb3=-1
      NB_SCN= 0
      NB_SCN1= 0
Cgw006      ADDNN= ADD(1,1)
      ADDNN= ADD(1,I_ADD)
      INF = 0
      SUP = 0
      DO 70 I=1,NB_SC
        NN = BPN(I)
CC        if(nn.eq.idb1)then
CC          idb3=0
CC        endif
        XX1=X(DIR, IRECTS(1,NN))
        XX2=X(DIR, IRECTS(2,NN))
        XMAX=MAX(XX1,XX2)+TZINF
        XMIN=MIN(XX1,XX2)-TZINF
        IF(XMIN.LT.SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_SCN1 = NB_SCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = BPN(I)
          INF = 1
CC        if(BPN(I).eq.idb1)then
CC          idb4=-1
CC        endif
        ENDIF
        IF(XMAX.GE.SEUIL) THEN
          NB_SCN = NB_SCN + 1
          BPN(NB_SCN) = BPN(I)
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          SUP = 1
CC        if(BPN(I).eq.idb1)then
CC          idb4=-1
CC        endif
        ENDIF
   70 CONTINUE
CC
CC    3- DIVISER LES ELEMENTS 
CC
      NB_MCN= 0
      NB_MCN1= 0
Cgw006      ADDNE= ADD(2,1)
      ADDNE= ADD(2,I_ADD)
      DO I=1,NB_MC
        NN = BPE(I)
        XX1=X(DIR, IRECTM(1,NN))
        XX2=X(DIR, IRECTM(2,NN))
        XMAX=MAX(XX1,XX2)+TZINF
        XMIN=MIN(XX1,XX2)-TZINF
CC        if(nn.eq.idb2)then
CC          if(idb3.eq.0)then
CC            idb4=-1
CC          endif
CC        endif
        IF(XMIN.LT.SEUIL.AND.INF.EQ.1) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          NB_MCN1 = NB_MCN1 + 1
          ADDNE = ADDNE + 1
          PE(ADDNE) = BPE(I)
CC        if(nn.eq.idb2)then
CC          if(idb3.eq.0)then
CC            idb4=-1
CC          endif
CC        endif
        ENDIF
        IF(XMAX.GE.SEUIL.AND.SUP.EQ.1) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_MCN = NB_MCN + 1
          BPE(NB_MCN) = BPE(I)
CC        if(nn.eq.idb2)then
CC          if(idb3.eq.0)then
CC            idb4=-1
CC          endif
CC        endif
        ENDIF
      ENDDO
C
C    4- REMPLIR LES TABLEAUX D'ADRESSES
C
Cgw006      ADD(1,2) = ADDNN
Cgw006      ADD(2,2) = ADDNE
      ADD(1,I_ADD+1) = ADDNN
      ADD(2,I_ADD+1) = ADDNE
C-----on remplit les min de la boite suivante et les max de la courante
C     (i.e. seuil est un max pour la courante)
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
      XYZM(DIR,I_ADD+1) = SEUIL
      XYZM(DIR+3,I_ADD) = SEUIL
C
      IF( ((NB_SCN.EQ.NB_SC .AND. NB_MCN1.EQ.NB_MC) .OR.
     .    (NB_SCN1.EQ.NB_SC .AND. NB_MCN.EQ.NB_MC)) .AND.
     .   MIN(NB_SCN,NB_SCN1).GT.0.AND.
     .   MIN(NB_MCN,NB_MCN1).GT.0) ISTOP = ISTOP + 1
C
      NB_SC = NB_SCN
      NB_MC = NB_MCN
C     on incremente le niveau de descente avant de sortir
      I_ADD = I_ADD + 1
      IF(I_ADD.GE.1000) THEN
C       TROP NIVEAUX PILE ON VAS LES PRENDRE PLUS GRANDES...
        IF ( NB_N_B .EQ. MAXSIZ/3) THEN
C          WRITE(IOUT,*)'***COMPUTATION STOPED WHILE INFINITELY LOOPING'
C          WRITE(ISTDO,*)'***COMPUTATION STOPED WHILE INFINITELY LOOPING'
C          CALL ARRET(2)
          CALL ANCMSG(MSGID=83,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
        I_MEM = 1
        RETURN
      ENDIF
C
C     ce return n'est atteint que dans le cas ok = 0
      RETURN
      END
