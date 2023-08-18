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
Chd|  I16TRI                        source/interfaces/int16/i16tri.F
Chd|-- called by -----------
Chd|        I16BUCE                       source/interfaces/int16/i16buce.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I16CUT                        source/interfaces/int16/i16tri.F
Chd|        I16STO                        source/interfaces/int16/i16tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I16TRI(
     1      BPE    ,PE      ,BPN      ,PN       ,NSN      ,
     2      TZINF  ,IXS     ,IXS16    ,IXS20    ,NELEM    ,
     3      NSV    ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,                       
     5      CONT   ,NB_N_B  ,EMINX    ,I_STOK_GLOB,NME    ,
     6      ITASK  ,NOINT   ,X        ,V        ,A        ,
     7      MX_CAND,IXS10   ,ESH_T    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAXSIZ,NB_N_B,I_STOK_GLOB,NME,NSN ,ITASK,NOINT ,MX_CAND,
     .        BPE(*),PE(*),BPN(*),PN(*),IXS(NIXS,*),IXS16(8,*),
     .        NSV(*),CAND_N(*),CAND_E(*),NELEM(*),IXS20(12,*),
     .        ESH_T, IXS10(6,*)
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*),EMINX(6,*),
     .   MINBOX,TZINF,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
      INTEGER I,J,I_ADD,I_STOK,L,NB_NC,NB_EC,CONT,NE
      INTEGER ADD(2,I_ADD_MAX) ,PROV_N(MVSIZ),PROV_E(MVSIZ)
      my_real
     .        XYZM(6,I_ADD_MAX-1)
C
C------------------------------------
C---------------------------------
C     CALCUL DES BORNES DU DOMAINE
C---------------------------------
      XMIN = EP30
      YMIN = EP30
      ZMIN = EP30
      XMAX = -EP30
      YMAX = -EP30
      ZMAX = -EP30      
C
      NB_EC = NME
      DO L=1,NB_EC
        I = L + ESH_T
        BPE(L) = I
C
        XMIN = MIN( XMIN , EMINX(1,I) )
        YMIN = MIN( YMIN , EMINX(2,I) )
        ZMIN = MIN( ZMIN , EMINX(3,I) )
        XMAX = MAX( XMAX , EMINX(4,I) )
        YMAX = MAX( YMAX , EMINX(5,I) )
        ZMAX = MAX( ZMAX , EMINX(6,I) )
C
      ENDDO
C
        XMIN = XMIN - TZINF
        YMIN = YMIN - TZINF
        ZMIN = ZMIN - TZINF
        XMAX = XMAX + TZINF
        YMAX = YMAX + TZINF
        ZMAX = ZMAX + TZINF
C
C-----INITIALISATION DES ADRESSES ET X,Y,Z
C
C     ADDE     ADDN     X      Y      Z
C     1        1        XMIN   YMIN   ZMIN
C     1        1        XMAX   YMAX   ZMAX
      I_STOK = 0
      ADD(1,1) = 0
      ADD(2,1) = 0
      ADD(1,2) = 0
      ADD(2,2) = 0
      I_ADD = 1
      XYZM(1,I_ADD) = XMIN
      XYZM(2,I_ADD) = YMIN
      XYZM(3,I_ADD) = ZMIN
      XYZM(4,I_ADD) = XMAX
      XYZM(5,I_ADD) = YMAX
      XYZM(6,I_ADD) = ZMAX
      NB_NC = 0
      DO I=1,NSN
        J=NSV(I)
        IF(X(1,J)+DT2*(V(1,J)+DT12*A(1,J))>=XMIN.AND.
     .     X(1,J)+DT2*(V(1,J)+DT12*A(1,J))<=XMAX.AND.
     .     X(2,J)+DT2*(V(2,J)+DT12*A(2,J))>=YMIN.AND.
     .     X(2,J)+DT2*(V(2,J)+DT12*A(2,J))<=YMAX.AND.
     .     X(3,J)+DT2*(V(3,J)+DT12*A(3,J))>=ZMIN.AND.
     .     X(3,J)+DT2*(V(3,J)+DT12*A(3,J))<=ZMAX)THEN
          NB_NC=NB_NC+1
          BPN(NB_NC) = I
        ENDIF
      ENDDO
ctmp+++
c       WRITE(istdo,*)'CONT = ',CONT
c       WRITE(istdo,*)'I_ADD = ',I_ADD
c       WRITE(istdo,*)'ADD(2,I_ADD) = ',ADD(2,I_ADD)
c       WRITE(istdo,*)'NB_EC = ',NB_EC
c       WRITE(istdo,*)'NB_NC = ',NB_NC
c       WRITE(istdo,*)'XYZM(1,I_ADD) = ',XYZM(1,I_ADD)
c       WRITE(istdo,*)'XYZM(2,I_ADD) = ',XYZM(2,I_ADD)
c       WRITE(istdo,*)'XYZM(3,I_ADD) = ',XYZM(3,I_ADD)
c       WRITE(istdo,*)'XYZM(4,I_ADD) = ',XYZM(4,I_ADD)
c       WRITE(istdo,*)'XYZM(5,I_ADD) = ',XYZM(5,I_ADD)
c       WRITE(istdo,*)'XYZM(6,I_ADD) = ',XYZM(6,I_ADD)
c       WRITE(istdo,*)'tzinf = ',tzinf
c       WRITE(istdo,*)'eminx(4,i) = ',(eminx(4,i),i=1,NB_EC)
ctmp---
C-----------------------------------------------
C     Boucle sur les boites
C-----------------------------------------------
      DO WHILE (CONT==1)
C-----------------------------------------------
C       Decoupage de l'espace en 2 suivant X Y ou Z
C-----------------------------------------------
        CALL I16CUT(
     1      BPE        ,PE      ,BPN      ,PN       ,ADD      ,
     2      X          ,NB_NC   ,NB_EC    ,XYZM     ,I_ADD    ,
     3      NSV        ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,                       
     4      CONT       ,NB_N_B  ,I_ADD_MAX,EMINX    ,NELEM    ,
     5      I_STOK     ,IXS     ,IXS16    ,IXS20    ,TZINF    ,
     6      I_STOK_GLOB,PROV_N  ,PROV_E   ,V        ,A        ,
     7      MX_CAND    ,IXS10   )
      ENDDO
C-----------------------------------------------
C       test de fin ou d'erreur
C-----------------------------------------------
C     CONT = 0  ==> FIN
C     CONT = -1 ==> PAS ASSEZ DE MEMOIRE PILE
C     CONT = -2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     CONT = -3 ==> TROP NIVEAUX PILE 
      IF(CONT==0)THEN
        IF(I_STOK/=0)CALL I16STO(
     1      I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2      CONT  ,MX_CAND    )
        RETURN
      ENDIF
      IF(CONT==-1)THEN
          CALL ANCMSG(MSGID=85,ANMODE=ANINFO,I1=NOINT)
          CALL ARRET(2)
      ELSEIF(CONT==-2) THEN
          CALL ANCMSG(MSGID=86,ANMODE=ANINFO,I1=NOINT)
          CALL ARRET(2)
      ELSEIF(CONT==-3)THEN
          CALL ANCMSG(MSGID=90,ANMODE=ANINFO,I1=NOINT)
          CALL ARRET(2)
      ENDIF
C
C
      RETURN
      END
Chd|====================================================================
Chd|  I16CUT                        source/interfaces/int16/i16tri.F
Chd|-- called by -----------
Chd|        I16TRI                        source/interfaces/int16/i16tri.F
Chd|-- calls ---------------
Chd|        I16STO                        source/interfaces/int16/i16tri.F
Chd|====================================================================
      SUBROUTINE I16CUT(
     1      BPE        ,PE      ,BPN      ,PN       ,ADD      ,
     2      X          ,NB_NC   ,NB_EC    ,XYZM     ,I_ADD    ,
     3      NSV        ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,                       
     4      CONT       ,NB_N_B  ,I_ADD_MAX,EMINX    ,NELEM    ,
     5      I_STOK     ,IXS     ,IXS16    ,IXS20    ,TZINF    ,
     6      I_STOK_GLOB,PROV_N  ,PROV_E   ,V        ,A        ,
     7      MX_CAND    ,IXS10   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
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
C     EMINX(6,*)   TABLEAU DES COORD ELEM MIN/MAX    E
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
C                  COUPLES NOEUDS,ELT CANDIDATS
C     MINBOX       TAILLE MIN BUCKET
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB_NC,NB_EC,I_ADD,MAXSIZ,I_STOK_GLOB,I_STOK,MX_CAND,
     .        NB_N_B,I_ADD_MAX,CONT  ,IXS(NIXS,*),IXS16(8,*),
     .        ADD(2,*),BPE(*),PE(*),BPN(*),PN(*),
     .        NSV(*),CAND_N(*),CAND_E(*),NELEM(*),
     .        PROV_N(*)  ,PROV_E(*)  ,IXS20(12,*), IXS10(6,*)
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*),XYZM(6,*),EMINX(6,*),
     .   MINBOX,TZINF,DIST    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,
     .        NN,NE,LE,K,L,NCAND_PROV,N16,N20,N8,N10
C     REAL
      my_real
     .   DX,DY,DZ,DSUP,SEUIL,XX,YY,ZZ
C
C-----------------------------------------------------------
C
C
C    1- TEST ARRET = BOITE VIDE
C                    BOITE TROP PETITE 
C                    BOITE NE CONTENANT QU'ONE NOEUD
C                    PLUS DE MEMOIRE DISPONIBLE
C
C-------------------TEST SUR MEMOIRE DEPASSEE------------
C
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS BOITES TROP PETITES
        CONT = -1
ctmp+++
c       WRITE(istdo,*)'MAXSIZ = ',MAXSIZ
c       WRITE(istdo,*)'ADD(2,I_ADD) = ',ADD(2,I_ADD)
c       WRITE(istdo,*)'NB_EC = ',NB_EC
ctmp---
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
C      1.1- STOCKAGE DU OU DES  NOEUD CANDIDAT ET DES ELTS CORRESP.
C          VIRER LES INUTILES
C
C       NCAND_PROV=NB_EC*NB_NC
C   NCAND_PROV negatif qd NB_EC*NB_NC > 2e31
C
        IF(NB_EC+NB_NC<=128) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = 129
        ENDIF
C
        IF(DSUP<MINBOX.OR.NB_NC<=NB_N_B.OR.NCAND_PROV<=128)THEN
C   necessaire qd NB_NC<=NB_N_B ou DSUP<MINBOX et NB_EC+NB_NC>128
            NCAND_PROV = NB_EC*NB_NC
            DO L=1,NCAND_PROV
              I = 1+(L-1)/NB_NC
              J = L-(I-1)*NB_NC
              LE = BPE(I)
              NE = NELEM(LE)
              N8  = NE
              N10 = N8-NUMELS8
              N20 = N10-NUMELS10
              N16 = N20-NUMELS20
              NN = NSV(BPN(J))
              XX = X(1,NN)+DT2*(V(1,NN)+DT12*A(1,NN))
              YY = X(2,NN)+DT2*(V(2,NN)+DT12*A(2,NN))
              ZZ = X(3,NN)+DT2*(V(3,NN)+DT12*A(3,NN))
              DIST = 0.
              DIST = MAX(EMINX(1,LE)-XX,XX-EMINX(4,LE),DIST)
              DIST = MAX(EMINX(2,LE)-YY,YY-EMINX(5,LE),DIST)
              DIST = MAX(EMINX(3,LE)-ZZ,ZZ-EMINX(6,LE),DIST)
              IF(DIST<TZINF)THEN
                IF(N8>=1.AND.N8<=NUMELS8)THEN
                  IF(NN/=IXS(2,NE).AND.NN/=IXS(3,NE).AND.
     &               NN/=IXS(4,NE).AND.NN/=IXS(5,NE).AND.
     &               NN/=IXS(6,NE).AND.NN/=IXS(7,NE).AND.
     &               NN/=IXS(8,NE).AND.NN/=IXS(9,NE))THEN
                    I_STOK = I_STOK + 1
                    PROV_N(I_STOK) = BPN(J)
                    PROV_E(I_STOK) = LE
                    IF(I_STOK==MVSIZ-1)CALL I16STO(
     1                 I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2                 CONT  ,MX_CAND    )
                    IF(CONT==-2)RETURN
                  ENDIF
                ELSEIF(N10>=1.AND.N10<=NUMELS8)THEN
                  IF(NN/=IXS(2,NE).AND.NN/=IXS(4,NE).AND.
     &               NN/=IXS(7,NE).AND.NN/=IXS(6,NE).AND.
     &               NN/=IXS10(1,N10).AND.NN/=IXS10(2,N10).AND.
     &               NN/=IXS10(3,N10).AND.NN/=IXS10(4,N10).AND.
     &               NN/=IXS10(5,N10).AND.NN/=IXS10(6,N10))THEN
                    I_STOK = I_STOK + 1
                    PROV_N(I_STOK) = BPN(J)
                    PROV_E(I_STOK) = LE
                    IF(I_STOK==MVSIZ-1)CALL I16STO(
     1                 I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2                 CONT  ,MX_CAND    )
                    IF(CONT==-2)RETURN
                  ENDIF
                ELSEIF(N16>=1.AND.N16<=NUMELS16)THEN
                  IF(NN/=IXS(2,NE).AND.NN/=IXS(3,NE).AND.
     &               NN/=IXS(4,NE).AND.NN/=IXS(5,NE).AND.
     &               NN/=IXS(6,NE).AND.NN/=IXS(7,NE).AND.
     &               NN/=IXS(8,NE).AND.NN/=IXS(9,NE).AND.
     &               NN/=IXS16(1,N16).AND.NN/=IXS16(2,N16).AND.
     &               NN/=IXS16(3,N16).AND.NN/=IXS16(4,N16).AND.
     &               NN/=IXS16(5,N16).AND.NN/=IXS16(6,N16).AND.
     &               NN/=IXS16(7,N16).AND.NN/=IXS16(8,N16))THEN
                    I_STOK = I_STOK + 1
                    PROV_N(I_STOK) = BPN(J)
                    PROV_E(I_STOK) = LE
                    IF(I_STOK==MVSIZ-1)CALL I16STO(
     1                 I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2                 CONT  ,MX_CAND    )
                    IF(CONT==-2)RETURN
                  ENDIF
                ELSEIF(N20>=1.AND.N20<=NUMELS20)THEN
                  IF(NN/=IXS(2,NE).AND.NN/=IXS(3,NE).AND.
     &               NN/=IXS(4,NE).AND.NN/=IXS(5,NE).AND.
     &               NN/=IXS(6,NE).AND.NN/=IXS(7,NE).AND.
     &               NN/=IXS(8,NE).AND.NN/=IXS(9,NE).AND.
     &               NN/=IXS20(1,N20) .AND.NN/=IXS20(2,N20) .AND.
     &               NN/=IXS20(3,N20) .AND.NN/=IXS20(4,N20) .AND.
     &               NN/=IXS20(5,N20) .AND.NN/=IXS20(6,N20) .AND.
     &               NN/=IXS20(7,N20) .AND.NN/=IXS20(8,N20) .AND.
     &               NN/=IXS20(9,N20) .AND.NN/=IXS20(10,N20).AND.
     &               NN/=IXS20(11,N20).AND.NN/=IXS20(12,N20))THEN
                    I_STOK = I_STOK + 1
                    PROV_N(I_STOK) = BPN(J)
                    PROV_E(I_STOK) = LE
                    IF(I_STOK==MVSIZ-1)CALL I16STO(
     1                 I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2                 CONT  ,MX_CAND    )
                    IF(CONT==-2)RETURN
                  ENDIF
                ENDIF
              ENDIF
            ENDDO 
C-----------------------------------------------------------
        ELSE
C-----------------------------------------------------------
C
C
C    2- PHASE DE TRI SUR LA MEDIANE SELON LA + GDE DIRECTION
C                    
C                   
C-----------------------------------------------------------
C
C    2.1- DETERMINER LA DIRECTION A DIVISER X,Y OU Z
C
          DIR = 1
          IF(DY==DSUP) THEN
            DIR = 2
          ELSE IF(DZ==DSUP) THEN
            DIR = 3
          ENDIF
          SEUIL =(XYZM(DIR+3,I_ADD)+XYZM(DIR,I_ADD))*0.5
C
C    2.2- DIVISER LES NOEUDS EN TWO ZONES 
C
          NB_NCN= 0
          NB_NCN1= 0
          ADDNN= ADD(1,I_ADD)
#include "vectorize.inc"
          DO I=1,NB_NC
            IF(X(DIR,NSV(BPN(I)))<SEUIL) THEN
C             ON STOCKE DANS LE BAS DE LA PILE BP
              NB_NCN1 = NB_NCN1 + 1
              ADDNN = ADDNN + 1
              PN(ADDNN) = BPN(I)
            ENDIF
          ENDDO
#include "vectorize.inc"
          DO I=1,NB_NC
            IF(X(DIR,NSV(BPN(I)))>=SEUIL) THEN
              NB_NCN = NB_NCN + 1
              BPN(NB_NCN) = BPN(I)
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
            ENDIF
          ENDDO
C
C    2.3- DIVISER LES ELEMENTS
C
          NB_ECN= 0
          ADDNE= ADD(2,I_ADD)
          IF(NB_NCN1==0) THEN
C pas de noeuds dans la deuxieme boite
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR+3,LE)+TZINF>=SEUIL) THEN
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
              NB_ECN = NB_ECN + 1
              BPE(NB_ECN) = LE
            ENDIF
           ENDDO
          ELSEIF(NB_NCN==0) THEN
C pas de noeuds dans la premiere boite
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR,LE)-TZINF<SEUIL) THEN
C             ON STOCKE DANS LA PILE PE
              ADDNE = ADDNE + 1
              PE(ADDNE) = LE
            ENDIF
           ENDDO
          ELSE
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR,LE)-TZINF<SEUIL) THEN
C             ON STOCKE DANS LA PILE PE
              ADDNE = ADDNE + 1
              PE(ADDNE) = LE
            ENDIF
            IF(EMINX(DIR+3,LE)+TZINF>=SEUIL) THEN
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
              NB_ECN = NB_ECN + 1
              BPE(NB_ECN) = LE
            ENDIF
           ENDDO
          ENDIF
C
C    2.4- REMPLIR LES TABLEAUX D'ADRESSES
C
          ADD(1,I_ADD+1) = ADDNN
          ADD(2,I_ADD+1) = ADDNE
C-----on remplit les min de la boite suivante et les max de la courante
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
            CONT = -3
            RETURN
          ENDIF
C=======================================================================
          CONT=1
ctmp+++
c       WRITE(istdo,*)'CONT = ',CONT
c       WRITE(istdo,*)'I_ADD = ',I_ADD
c       WRITE(istdo,*)'ADD(2,I_ADD) = ',ADD(2,I_ADD)
c       WRITE(istdo,*)'NB_EC = ',NB_EC
c       WRITE(istdo,*)'NB_NC = ',NB_NC
c       WRITE(istdo,*)'dir seuil = ',dir, seuil
c       WRITE(istdo,*)'Xmin = ',XYZM(1,I_ADD)
c       WRITE(istdo,*)'Ymin = ',XYZM(2,I_ADD)
c       WRITE(istdo,*)'Zmin = ',XYZM(3,I_ADD)
c       WRITE(istdo,*)'Xmax = ',XYZM(4,I_ADD)
c       WRITE(istdo,*)'Ymax = ',XYZM(5,I_ADD)
c       WRITE(istdo,*)'Zmax = ',XYZM(6,I_ADD)
ctmp---
          RETURN
C=======================================================================
        ENDIF
      ENDIF
C-------------------------------------------------------------------------
C     TEST FIN DU TRI
C-------------------------------------------------------------------------
      IF (I_ADD==1) THEN
        CONT = 0
        RETURN
      ENDIF
C-----------------------------------------------------------
C
C    3- FIN DE BRANCHE ou BOITE VIDE
C                    
C-----------------------------------------------------------
C-------------------------------------------------------------------------
C       on decremente le niveau de descente avant de recommencer
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
C-------------------------------------------------------------------------
C         IL FAUT COPIER LES BAS DES PILES DANS BAS_DE_PILE CORRESPONDANTS
C         AVANT DE REDESCENDRE DANS LA BRANCHE MITOYENNE
C-------------------------------------------------------------------------
C     3.1- PILE DES NOEUDS
C
      NB_NC = ADD(1,I_ADD+1) - ADD(1,I_ADD)
      DO I=1,NB_NC
        BPN(I) = PN(ADD(1,I_ADD)+I)
      ENDDO
C
C     3.2- PILE DES ELEMENTS
C
      NB_EC = ADD(2,I_ADD+1) - ADD(2,I_ADD)
      DO I=1,NB_EC
        BPE(I) = PE(ADD(2,I_ADD)+I)
      ENDDO
C=======================================================================
      CONT=1
      RETURN
C=======================================================================
      END
Chd|====================================================================
Chd|  I16STO                        source/interfaces/int16/i16tri.F
Chd|-- called by -----------
Chd|        I16CUT                        source/interfaces/int16/i16tri.F
Chd|        I16TRI                        source/interfaces/int16/i16tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I16STO(
     1     I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2     CONT  ,MX_CAND    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,I_STOK_GLOB,CONT  ,MX_CAND,
     .        PROV_N(*),CAND_N(*),PROV_E(*),CAND_E(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J_STOK_GLOB
C-----------------------------------------------
#include "lockon.inc"
        J_STOK_GLOB = I_STOK_GLOB
        IF(I_STOK_GLOB + I_STOK<=MX_CAND)THEN
          I_STOK_GLOB = I_STOK_GLOB + I_STOK
        ELSE
          CONT = -2
        ENDIF
#include "lockoff.inc"
      IF(CONT==-2)RETURN
C
      DO I=1,I_STOK
        CAND_N(I+J_STOK_GLOB)=PROV_N(I)
        CAND_E(I+J_STOK_GLOB)=PROV_E(I)
      ENDDO
C
      I_STOK = 0
C-----------------------------------------------
      RETURN
      END
