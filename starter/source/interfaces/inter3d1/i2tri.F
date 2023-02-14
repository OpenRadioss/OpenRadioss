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
Chd|  I2TRI                         source/interfaces/inter3d1/i2tri.F
Chd|-- called by -----------
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I2COR3                        source/interfaces/inter3d1/i2cor3.F
Chd|        I2DST3                        source/interfaces/inter3d1/i2dst3.F
Chd|        I2DST3_27                     source/interfaces/inter3d1/i2dst3_27.F
Chd|        I7DSTK                        source/interfaces/inter3d1/i7dstk.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I2TRI(
     1      BPE   ,PE    ,BPN   ,PN    ,ADD   ,
     2      IRECT ,X     ,NB_NC ,NB_EC ,XYZM  ,
     3      I_ADD ,NSV   ,I_AMAX,XMAX  ,YMAX  ,
     4      ZMAX  ,MAXSIZ,I_MEM ,NB_N_B,NSN   ,
     5      NOINT ,TZINF ,MAXBOX,MINBOX,J_STOK,
     6      IRTL  ,ST    ,DMIN  ,IGNORE,THK   ,
     7      KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC,
     8      NOD2ELTG,NINT,IXC,
     9      IXTG  ,THK_PART,IPARTC  ,GEO  ,IXS,
     A      IXS10 ,PM      ,IXS16 ,IXS20  ,IPARTTG,
     B      ID    ,TITR  ,IGEO  ,DSEARCH , PM_STACK,
     C      IWORKSH ,
     D      IX1    ,IX2    ,IX3,IX4 ,NSVG      ,
     1      PROV_N ,PROV_E ,N11,N12 ,N13       ,
     2      X1     ,X2     ,X3 ,X4  ,STIF      ,
     3      Y1     ,Y2     ,Y3 ,Y4  ,Z1        ,
     4      Z2     ,Z3     ,Z4 ,XI  ,YI        ,
     5      ZI     ,X0     ,Y0 ,Z0  ,NX1       ,
     6      NY1    ,NZ1    ,NX2,NY2 ,NZ2       ,
     7      NX3    ,NY3    ,NZ3,NX4 ,NY4       ,
     8      NZ4    ,P1     ,P2 ,P3  ,P4        ,
     9      LB1    ,LB2    ,LB3,LB4 ,LC1       ,
     1      LC2    ,LC3    ,LC4,S   ,T         ,
     2      ILEV)
      USE MESSAGE_MOD
C============================================================================
C  cette routine est appelee par : I2BUC1(/inter3d1/i2buc1.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I7DSTK(/inter3d1/i7dstk.F)
C                          I2DST3(/inter3d1/i2dst3.F)
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
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "vect07_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB_NC,NB_EC,I_ADD,MAXSIZ,J_STOK,I_MEM,ILEV
      INTEGER I_BID, I_AMAX,NB_N_B, NOINT, NSN, IGNORE,NINT
      INTEGER ADD(2,0:*),IRECT(4,*),BPE(*),PE(*),BPN(*),PN(*)
      INTEGER NSV(*),IRTL(*),KNOD2ELS(*), KNOD2ELC(*), 
     .     KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .     IXC(*),IXTG(*),IPARTC(*),IXS(*),
     .     IXS10(*), IXS16(*), IXS20(*),IPARTTG(*),IGEO(*),
     .     IWORKSH(*)
C     REAL
      my_real
     .   X(3,*),XYZM(6,*),ST(*),DMIN(*),TZINF,
     .   MAXBOX,MINBOX, XMAX, YMAX, ZMAX,THK(*),THK_PART(*),GEO(*),
     .   PM(*),DSEARCH,PM_STACK(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: PROV_N,PROV_E,NSVG
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N11,N12,N13
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LC1,LC2,LC3,LC4,STIF
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: S,T
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_ECN,ADDNN,ADDNE,IPOS,I,IP,J
      INTEGER INF,SUP,DIR,N1,N2,N3,N4,NN,NE,MEMX,IFLAG
C     REAL
      my_real
     .   BID,DX,DY,DZ,DSUP,SEUIL,XMX,XMN,GAPV(MVSIZ)
C-----------------------------------------------
!      EQUIVALENCE (PROV_N,IV(1,6)) ,(PROV_E,IV(1,7)) 
      DATA MEMX/0/
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
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR)
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
        DO 20 I=1,NB_EC
            NE = BPE(I)
            N1=IRECT(1,NE) 
            N2=IRECT(2,NE) 
            N3=IRECT(3,NE) 
            N4=IRECT(4,NE) 
            DO 20 J=1,NB_NC
              NN=NSV(BPN(J))
              IF(NN/=N1.AND.NN/=N2.AND.NN/=N3.AND.NN/=N4) THEN
                J_STOK = J_STOK + 1
                PROV_N(J_STOK) = BPN(J)
                PROV_E(J_STOK) = NE
                IF(J_STOK==NVSIZ) THEN
                  LFT = 1
                  LLT = NVSIZ
                  NFT = 0
                  J_STOK = 0
                  IFLAG = 0
                  CALL I2COR3(X     ,IRECT ,NSV   ,PROV_E  ,PROV_N,
     1                        BID   ,BID   ,GAPV  ,0       ,TZINF,
     2                        BID   ,BID   ,0     ,NINT    ,IXC   , 
     4                        IXTG  ,THK_PART,IPARTC,GEO   , NOINT,
     5                        IXS   ,IXS10 ,PM    ,THK     ,KNOD2ELS,
     6                        KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC,NOD2ELTG,
     7                        IGNORE,IXS16 ,IXS20 ,IPARTTG,IGEO,DSEARCH ,
     8                        PM_STACK , IWORKSH ,IX1     ,IX2   ,
     5                        IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6                        X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7                        Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8                        XI     ,YI     ,ZI  ,STIF    ,IFLAG )
                  IF (ILEV == 27) THEN
C--               Improved projections on triangles for spotflag27
                    CALL I2DST3_27(GAPV,PROV_E ,PROV_N,TZINF,IRTL,ST,DMIN,
     .                        IGNORE,THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     .                        NOD2ELS,NOD2ELC,NOD2ELTG,X,IRECT,
     .                        NINT,IXC ,IXTG,THK_PART,IPARTC,GEO,
     .                        NOINT,IXS,IXS10,PM,IX3,
     1                        IX4,X1 ,X2 ,X3 ,X4 ,
     1                        Y1 ,Y2 ,Y3 ,Y4 ,Z1 ,
     2                        Z2 ,Z3 ,Z4 ,XI ,YI ,
     3                        ZI ,X0 ,Y0 ,Z0 ,NX1,
     4                        NY1,NZ1,NX2,NY2,NZ2,
     5                        NX3,NY3,NZ3,NX4,NY4,
     6                        NZ4,P1 ,P2 ,P3 ,P4 ,
     7                        LB1,LB2,LB3,LB4,LC1,
     8                        LC2,LC3,LC4,S  ,T  )
                  ELSE
                    CALL I2DST3(GAPV,PROV_E ,PROV_N,TZINF,IRTL,ST,DMIN,
     .                        IGNORE,THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     .                        NOD2ELS,NOD2ELC,NOD2ELTG,X,IRECT,
     .                        NINT,IXC ,IXTG,THK_PART,IPARTC,GEO,
     .                        NOINT,IXS,IXS10,PM,IX3,
     1                        IX4,X1 ,X2 ,X3 ,X4 ,
     1                        Y1 ,Y2 ,Y3 ,Y4 ,Z1 ,
     2                        Z2 ,Z3 ,Z4 ,XI ,YI ,
     3                        ZI ,X0 ,Y0 ,Z0 ,NX1,
     4                        NY1,NZ1,NX2,NY2,NZ2,
     5                        NX3,NY3,NZ3,NX4,NY4,
     6                        NZ4,P1 ,P2 ,P3 ,P4 ,
     7                        LB1,LB2,LB3,LB4,LC1,
     8                        LC2,LC3,LC4,S  ,T  )
                  ENDIF
                ENDIF
C               write(6,*)"Noeud candidat",BPN(J)
C               write(6,*)"Element  candidat",NE
              ENDIF  
   20   CONTINUE 
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
      DO 70 I=1,NB_NC
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
   70 CONTINUE
CC
CC    3- DIVISER LES ELEMENTS 
CC
C
C 2 LIGNES PROV POUR TEST
C      INF = 1
C      SUP = 1
C
      NB_ECN= 0
      ADDNE= ADD(2,1)
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
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
        I_MEM = 1
        RETURN
      ENDIF
C
C     ce return n'est atteint que dans le cas ok = 0
      RETURN
      END
