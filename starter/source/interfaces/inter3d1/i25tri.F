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
Chd|  I25TRI                        source/interfaces/inter3d1/i25tri.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I25S1S2                       source/interfaces/inter3d1/i25sto.F
Chd|        I7CMP3                        source/interfaces/inter3d1/i7cmp3.F
Chd|        I7COR3                        source/interfaces/inter3d1/i7cor3.F
Chd|        I7DST3                        source/interfaces/inter3d1/i7dst3.F
Chd|        I7DSTK                        source/interfaces/inter3d1/i7dstk.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25TRI(
     1      BPE    ,PE    ,BPN   ,PN    ,ADD   ,
     2      IRECT  ,X     ,NB_NC ,NB_EC ,XYZM  ,
     3      I_ADD  ,NSV   ,I_AMAX,XMAX  ,YMAX  ,
     4      ZMAX   ,MAXSIZ,I_STOK,I_MEM ,NB_N_B,
     5      CAND_N ,CAND_E,NSN   ,NOINT ,TZINF ,
     6      MAXBOX ,MINBOX,STF   ,STFN  ,J_STOK,
     7      MULTIMP,ISTF  ,ITAB  ,GAP   ,GAP_S ,
     8      GAP_M  ,IGAP  ,GAPMIN,GAPMAX,MARGE ,
     9      GAP_S_L,GAP_M_L,ID   ,TITR  ,ILEV  ,
     A      NBINFLG,MBINFLG,IXS  ,
     B      IXS10  ,IXS16 ,IXS20 , IPARTNS,
     C      INACTI ,MSEGTYP,MARGE_SH,NRTM,
     D      IX1,IX2,IX3,IX4,NSVG,
     E      X1,X2,X3,X4,Y1,
     F      Y2,Y3,Y4,Z1,Z2,
     G      Z3,Z4,XI,YI,ZI,
     H      X0,Y0,Z0,STIF,NX1,
     I      NY1,NZ1,NX2,NY2,NZ2,
     J      NX3,NY3,NZ3,NX4,NY4,
     K      NZ4,P1,P2,P3,P4,
     L      LB1,LB2,LB3,LB4,LC1,
     M      LC2,LC3,LC4,PENE,PROV_N,
     N      PROV_E,N11,N21,N31,IREMNODE,
     O      FLAGREMNODE,KREMNODE,REMNODE)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "vect07_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB_NC,NB_EC,I_ADD,MAXSIZ,I_STOK,J_STOK,I_MEM,ISTF,
     .        IREMNODE,FLAGREMNODE
      INTEGER I_BID, I_AMAX,NB_N_B, NOINT, NSN,MULTIMP, IGAP
      INTEGER ADD(2,0:*),IRECT(4,*),BPE(*),PE(*),BPN(*),PN(*)
      INTEGER NSV(*),CAND_N(*),CAND_E(*), ITAB(*),NBINFLG(*),MBINFLG(*),
     *        ILEV,IPARTNS(*),INACTI,NRTM,KREMNODE(*),REMNODE(*)
      INTEGER IXS(NIXS,*), IXS10(6,*), IXS16(8,*), IXS20(12,*)
C     REAL
      my_real
     .   X(3,*),XYZM(6,*),TZINF,DBUC,STF(*),STFN(*),
     .   MAXBOX,MINBOX, XMAX, YMAX, ZMAX,
     .   GAP, GAP_S(*), GAP_M(*),
     .   GAPMIN, GAPMAX, MARGE, GAPSMX, BGAPSMX,
     .   GAP_S_L(*),GAP_M_L(*),MARGE_SH
      INTEGER ID,MSEGTYP(*)
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) ::PROV_N,PROV_E
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X0,Y0,Z0,STIF
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N11,N21,N31,PENE
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LC1,LC2,LC3,LC4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_ECN,ADDNN,ADDNE,IPOS,I,IP,J
      INTEGER INF,SUP,DIR,N1,N2,N3,N4,NN,NE,INS,K,L,M
C     REAL
      my_real
     .   BID,DX,DY,DZ,DSUP,SEUIL,XMX,XMN,GAPSMAX,
     .   GAPV(MVSIZ),MARGE_E
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGNOD
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
C     NB_NC        NOMBRE DE NOEUDS CANDIDATS        E/S
C     NB_EC        NOMBRE D'ELTS CANDIDATS           E/S
C     I_ADD        POSITION DANS LE TAB DES ADRESSES E/S
C     NSV          NOS SYSTEMES DES NOEUDS           E
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     I_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     CAND_N       boites resultats noeuds
C     CAND_E       adresses des boites resultat elements
C                  MULTIMP*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
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
C      IF(MEMX>ADD(2,1)+NB_EC)THEN
C        WRITE(ISTDO,*)' *******MEM MAX=',MEMX
C        MEMX=-1
C      ELSEIF(MEMX/=-1)THEN
C        MEMX=ADD(2,1)+NB_EC
C      ENDIF
C--------------------TEST SUR BOITE VIDES-------------- 
C
      IF(NB_EC==0.OR.NB_NC==0) THEN
C       write(6,*)" BOITE VIDE"
C       IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C       AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
        CALL I7DSTK(I_ADD,NB_NC,NB_EC,ADD,BPN,PN,BPE,PE)
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
      IF(ADD(2,1)+NB_EC>=MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS BOITES TROP PETITES
        IF ( NB_N_B == NUMNOD) THEN
          CALL ANCMSG(MSGID=83,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
        I_MEM = 1
        RETURN
      ENDIF
      IF(DSUP<MINBOX.OR.
     .   NB_NC<=NB_N_B.AND.DSUP<MAXBOX.OR.   
     .   NB_NC<=NB_N_B.AND.NB_EC==1) THEN
C
C       write(6,*)" NOUVELLE BOITE "
C       1- STOCKAGE DU OU DES  NOEUD CANDIDAT ET DES ELTS CORRESP.
C       VIRER LES INUTILES
c        IF(FLAGREMNODE==2) THEN
            ALLOCATE(TAGNOD(NUMNOD))
            TAGNOD(1:NUMNOD) = 0
c        ENDIF
        DO  I=1,NB_EC
            NE = BPE(I)
            N1=IRECT(1,NE) 
            N2=IRECT(2,NE) 
            N3=IRECT(3,NE) 
            N4=IRECT(4,NE) 

            IF(FLAGREMNODE==2.AND.IREMNODE==3)THEN
               K = KREMNODE(NE)+1
               L = KREMNODE(NE+1)
               DO M=K,L
                  TAGNOD(REMNODE(M)) = 1
               ENDDO
            ENDIF
            IF (MSEGTYP(NE)==0.OR.MSEGTYP(NE)>NRTM) THEN
             MARGE_E = MARGE
            ELSE
             MARGE_E = MARGE_SH
            END IF 
            DO J=1,NB_NC
              NN=NSV(BPN(J))
C-----------------  
C--------   not sure if use Inacti----
C              INS = 1
C              IF (INACTI > 0)        
C     .        CALL INSOLBOX(X    ,MVOISN(1,NE),MVOISN(2,NE),NOINT ,IXS,
C     .                     IXS10 ,IXS16   ,IXS20  ,NN    ,GAP    ,
C     .                     MVOISN(3,NE),IPARTNS(BPN(J)),IPEN0,INS    )
              IF(NN/=N1.AND.NN/=N2.AND.NN/=N3.AND.NN/=N4.AND.TAGNOD(NN)==0) THEN
                J_STOK = J_STOK + 1
                PROV_N(J_STOK) = BPN(J)
                PROV_E(J_STOK) = NE
                IF(J_STOK==NVSIZ) THEN
                  LFT = 1
                  LLT = NVSIZ
                  NFT = 0
                  J_STOK = 0
                  CALL I7COR3(X    ,IRECT,NSV  ,PROV_E ,PROV_N,
     .                        STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .                        GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .                        GAP_S_L,GAP_M_L  ,ZERO   ,IX1     ,IX2   ,
     5                        IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6                        X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7                        Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8                        XI     ,YI     ,ZI  ,STIF, ZERO)
                  CALL I7DST3(IX3,IX4,X1 ,X2 ,X3 ,
     1                  X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2                  Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3                  YI ,ZI ,X0 ,Y0 ,Z0 ,
     4                  NX1,NY1,NZ1,NX2,NY2,
     5                  NZ2,NX3,NY3,NZ3,NX4,
     6                  NY4,NZ4,P1 ,P2 ,P3 ,
     7                  P4 ,LB1,LB2,LB3,LB4,
     8                  LC1,LC2,LC3,LC4)
                  CALL I7PEN3(MARGE_E,GAPV,N11,N21,N31,
     1                  PENE ,NX1 ,NY1,NZ1,NX2,
     2                  NY2  ,NZ2 ,NX3,NY3,NZ3,
     3                  NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4                  P3   ,P4)
                  IF (ILEV==2) CALL I25S1S2(J_STOK,PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
                  IF(I_STOK+NVSIZ<MULTIMP*NSN) THEN
                    CALL I7CMP3(I_STOK,CAND_E  ,CAND_N,1,PENE,
     1                  PROV_N,PROV_E)
                  ELSE
                    I_BID = 0
                    CALL I7CMP3(I_BID,CAND_E,CAND_N,0,PENE,
     1                  PROV_N,PROV_E)
                    IF(I_STOK+I_BID<MULTIMP*NSN) THEN
                      CALL I7CMP3(I_STOK,CAND_E,CAND_N,1,PENE,
     1                  PROV_N,PROV_E)
                    ELSE
                      I_MEM = 2
c                      CALL ANSTCKI(NOINT)
c                      CALL ANCWARN(103,ANINFO_BLIND_2)
                      RETURN
                    ENDIF
                  ENDIF
                ENDIF
C               write(6,*)"Noeud candidat",BPN(J)
C               write(6,*)"Element  candidat",NE
              ENDIF  

            ENDDO
            IF(FLAGREMNODE==2.AND.IREMNODE==3)THEN
               K = KREMNODE(NE)+1
               L = KREMNODE(NE+1)
               DO M=K,L
                  TAGNOD(REMNODE(M)) = 0
               ENDDO
            ENDIF

       ENDDO 

        DEALLOCATE(TAGNOD)
C       IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C       AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
        CALL I7DSTK(I_ADD,NB_NC,NB_EC,ADD,BPN,PN,BPE,PE)
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
      IF(DY==DSUP) THEN
        DIR = 2
      ELSE IF(DZ==DSUP) THEN
        DIR = 3
      ENDIF
      SEUIL =(XYZM(DIR+3,I_ADD)+XYZM(DIR,I_ADD))/2
C
C    2- DIVISER LES NOEUDS EN TWO ZONES 
C
      NB_NCN= 0
      ADDNN= ADD(1,1)
      INF = 0
      SUP = 0
      IF(IGAP==0)THEN
       DO I=1,NB_NC
        IF(X(DIR,NSV(BPN(I)))<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNN = ADDNN + 1
          PN(ADDNN) = BPN(I)
          INF = 1
        ELSE
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = BPN(I)
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          SUP = 1
        ENDIF
       END DO
      ELSE
       GAPSMX  = ZERO
       BGAPSMX = ZERO
       DO I=1,NB_NC
        IF(X(DIR,NSV(BPN(I)))<SEUIL) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNN = ADDNN + 1
          PN(ADDNN) = BPN(I)
          GAPSMX = MAX(GAPSMX,GAP_S(BPN(I)))
          INF = 1
        ELSE
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = BPN(I)
          BGAPSMX = MAX(BGAPSMX,GAP_S(BPN(I)))
          SUP = 1
        ENDIF
       END DO
      END IF

CC
CC    3- DIVISER LES ELEMENTS 
CC
C      NB_ECN= 0
C      ADDNE= ADD(2,1)
C      SEUILI = SEUIL-TZINF
C      SEUILS = SEUIL+TZINF
C      DO 85 I=1,NB_EC
C        INF = 0
C        SUP = 0
C        DO 80 J=1,4
C          IP = IRECT(J,BPE(I))
C          IF(X(DIR,IP)<SEUILS) THEN
C            INF = 1
C            IF(SUP==1) GOTO 81
C          ENDIF
C          IF(X(DIR,IP)>=SEUILI) THEN
C            SUP = 1
C            IF(INF==1) GOTO 81
C          ENDIF
C   80   CONTINUE
C
C   81   CONTINUE
C        IF(INF==1) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
C          ADDNE = ADDNE + 1
C          PE(ADDNE) = BPE(I)
C        ENDIF
C        IF(SUP==1) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
C          NB_ECN = NB_ECN + 1
C          BPE(NB_ECN) = BPE(I)
C        ENDIF
C   85 CONTINUE
C
C    3- DIVISER LES ELEMENTS
C
C 2 LIGNES PROV POUR TEST
C      INF = 1
C      SUP = 1
C
      NB_ECN= 0
      ADDNE= ADD(2,1)
      IF(IGAP==0)THEN
       DO I=1,NB_EC
        XMX = MAX(X(DIR,IRECT(1,BPE(I))),X(DIR,IRECT(2,BPE(I))),
     .            X(DIR,IRECT(3,BPE(I))),X(DIR,IRECT(4,BPE(I))))
     .      + TZINF
        XMN = MIN(X(DIR,IRECT(1,BPE(I))),X(DIR,IRECT(2,BPE(I))),
     .            X(DIR,IRECT(3,BPE(I))),X(DIR,IRECT(4,BPE(I))))
     .      - TZINF
        IF(XMN<SEUIL.AND.INF==1) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = BPE(I)
        ENDIF
        IF(XMX>=SEUIL.AND.SUP==1) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = BPE(I)
        ENDIF
       ENDDO
      ELSE       
       DO I=1,NB_EC
          NE = BPE(I)
          IF (MSEGTYP(NE)==0.OR.MSEGTYP(NE)>NRTM) THEN
           MARGE_E = MARGE
          ELSE
           MARGE_E = MARGE_SH
          END IF 
        XMN = MIN(X(DIR,IRECT(1,NE)),X(DIR,IRECT(2,NE)),
     .            X(DIR,IRECT(3,NE)),X(DIR,IRECT(4,NE)))
     .      - MAX(MIN(GAPSMX+GAP_M(NE),GAPMAX),GAPMIN)-MARGE_E
        IF(XMN<SEUIL.AND.INF==1) THEN
C         ON STOCKE DANS LE BAS DE LA PILE BP
          ADDNE = ADDNE + 1
          PE(ADDNE) = BPE(I)
        ENDIF
        XMX = MAX(X(DIR,IRECT(1,NE)),X(DIR,IRECT(2,NE)),
     .            X(DIR,IRECT(3,NE)),X(DIR,IRECT(4,NE)))
     .      + MAX(MIN(BGAPSMX+GAP_M(NE),GAPMAX),GAPMIN)+MARGE_E
        IF(XMX>=SEUIL.AND.SUP==1) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
          NB_ECN = NB_ECN + 1
          BPE(NB_ECN) = BPE(I)
        ENDIF
       ENDDO
      END IF
C
C    4- REMPLIR LES TABLEAUX D'ADRESSES
C
      ADD(1,2) = ADDNN
      ADD(2,2) = ADDNE
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
      NB_NC = NB_NCN
      NB_EC = NB_ECN
C     on incremente le niveau de descente avant de sortir
      I_ADD = I_ADD + 1
      IF(I_ADD>=1000) THEN
C       TROP NIVEAUX PILE ON VAS LES PRENDRE PLUS GRANDES...
        IF ( NB_N_B == NUMNOD) THEN
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
