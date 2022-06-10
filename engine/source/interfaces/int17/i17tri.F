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
Chd|  I17TRI                        source/interfaces/int17/i17tri.F
Chd|-- called by -----------
Chd|        I17BUCE                       source/interfaces/int17/i17buce.F
Chd|        I17BUCE_PENA                  source/interfaces/int17/i17buce.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I17CUT                        source/interfaces/int17/i17tri.F
Chd|        I17STO                        source/interfaces/int17/i17tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I17TRI(
     2      TZINF  ,IXS     ,IXS16    ,IXS20    ,NELEM    ,
     3      NELES  ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,                       
     5      CONT   ,NB_N_B  ,EMINX    ,I_STOK_GLOB,NME    ,
     6      ITASK  ,NOINT   ,X        ,V        ,A        ,
     7      MX_CAND,EMINXS  ,ESH_T    ,MAXSIZS  ,I_ADD_MAX,
     8      XYZM   ,NMES    ,NMESR    ,NIN      )
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     PARAMETER (I_ADD_MAX = 1001) declare dans I17BUCE
      INTEGER I_ADD_MAX, NMESR, NIN,
     .        MAXSIZ,NB_N_B,I_STOK_GLOB,NME,NMES ,ITASK,NOINT ,MX_CAND,
     .        IXS(NIXS,*),IXS16(8,*),
     .        NELES(*),CAND_N(*),CAND_E(*),NELEM(*),IXS20(12,*),ESH_T,
     .        MAXSIZS  
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*),EMINX(6,*),EMINXS(6,*),
     .   MINBOX,TZINF,
     .   XYZM(6,I_ADD_MAX-1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,I_ADD,I_STOK,L,NB_NC,NB_EC,CONT,NE 
      INTEGER ADD(2,I_ADD_MAX) ,PROV_N(MVSIZ),PROV_E(MVSIZ),
     .        BPE(MAXSIZ/3), PE(MAXSIZ), BPN(MAXSIZS/3), PN(MAXSIZS)
C en toute rigueur MAXSIZ = NME et MAXSIZS = NMES+NMESR
      my_real
     .        XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C------------------------------------
C---------------------------------
C     Init des boite main
C---------------------------------
C
      NB_EC = NME
      DO L=1,NB_EC
        I = L + ESH_T
        BPE(L) = I
      ENDDO
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
      XMIN = XYZM(1,I_ADD)
      YMIN = XYZM(2,I_ADD)
      ZMIN = XYZM(3,I_ADD)
      XMAX = XYZM(4,I_ADD)
      YMAX = XYZM(5,I_ADD)
      ZMAX = XYZM(6,I_ADD)
      NB_NC = 0
      DO I=1,NMES
C
        IF(EMINXS(4,I).GT.XMIN.AND.
     .     EMINXS(5,I).GT.YMIN.AND.
     .     EMINXS(6,I).GT.ZMIN.AND.
     .     EMINXS(1,I).LT.XMAX.AND.
     .     EMINXS(2,I).LT.YMAX.AND.
     .     EMINXS(3,I).LT.ZMAX)THEN
          NB_NC=NB_NC+1
          BPN(NB_NC) = I
        ENDIF
      ENDDO
C
C Prise en compte candidats non locaux en SPMD
C
      DO I = NMES+1, NMES+NMESR
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C-----------------------------------------------
C     Boucle sur les boites
C-----------------------------------------------
      DO WHILE (CONT.EQ.1)
C-----------------------------------------------
C       Decoupage de l'espace en 2 suivant X Y ou Z
C-----------------------------------------------
        CALL I17CUT(
     1      BPE        ,PE      ,BPN      ,PN       ,ADD      ,
     2      X          ,NB_NC   ,NB_EC    ,XYZM     ,I_ADD    ,
     3      NELES      ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,                       
     4      CONT       ,NB_N_B  ,I_ADD_MAX,EMINX    ,NELEM    ,
     5      I_STOK     ,IXS     ,IXS16    ,IXS20    ,TZINF    ,
     6      I_STOK_GLOB,PROV_N  ,PROV_E   ,V        ,A        ,
     7      MX_CAND    ,EMINXS  ,MAXSIZS  ,NMES     ,NIN      )
      ENDDO
C-----------------------------------------------
C       test de fin ou d'erreur
C-----------------------------------------------
C     CONT = 0  ==> FIN
C     CONT = -1 ==> PAS ASSEZ DE MEMOIRE PILE
C     CONT = -2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     CONT = -3 ==> TROP NIVEAUX PILE 
      IF(CONT.EQ.0)THEN
        IF(I_STOK.NE.0)CALL I17STO(
     1      I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2      CONT  ,MX_CAND    )
      ENDIF
      IF(CONT.EQ.-1) THEN
          CALL ANCMSG(MSGID=86,ANMODE=ANINFO,I1=NOINT)
          CALL ARRET(2)
      ELSEIF(CONT.EQ.-2) THEN
          CALL ANCMSG(MSGID=86,ANMODE=ANINFO,I1=NOINT)
          CALL ARRET(2)
      ELSEIF(CONT.EQ.-3)THEN
          CALL ANCMSG(MSGID=90,ANMODE=ANINFO,I1=NOINT)
          CALL ARRET(2)
      ENDIF
C
C
      RETURN
      END
Chd|====================================================================
Chd|  I17CUT                        source/interfaces/int17/i17tri.F
Chd|-- called by -----------
Chd|        I17TRI                        source/interfaces/int17/i17tri.F
Chd|-- calls ---------------
Chd|        I17STO                        source/interfaces/int17/i17tri.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I17CUT(
     1      BPE        ,PE      ,BPN      ,PN       ,ADD      ,
     2      X          ,NB_NC   ,NB_EC    ,XYZM     ,I_ADD    ,
     3      NELES      ,MAXSIZ  ,CAND_N   ,CAND_E   ,MINBOX   ,                       
     4      CONT       ,NB_N_B  ,I_ADD_MAX,EMINX    ,NELEM    ,
     5      I_STOK     ,IXS     ,IXS16    ,IXS20    ,TZINF    ,
     6      I_STOK_GLOB,PROV_N  ,PROV_E   ,V        ,A        ,
     7      MX_CAND    ,EMINXS  ,MAXSIZS  ,NMES     ,NIN      )
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
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
      INTEGER NB_NC,NB_EC,I_ADD,MAXSIZ,I_STOK_GLOB,I_STOK,MX_CAND,NIN,
     .        NB_N_B,I_ADD_MAX,CONT  ,IXS(NIXS,*),IXS16(8,*),
     .        ADD(2,*),BPE(*),PE(*),BPN(*),PN(*),
     .        CAND_N(*),CAND_E(*),NELEM(*),NELES(*),
     .        PROV_N(*)  ,PROV_E(*)  ,IXS20(12,*),MAXSIZS, NMES
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*),XYZM(6,*),EMINX(6,*),EMINXS(6,*),
     .   MINBOX,TZINF    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,
     .        NES,NE,LE,LES,K,L,NCAND_PROV,N16,N20,LESL
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
      IF(ADD(2,I_ADD)+NB_EC.GT.MAXSIZ) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS MAINS BOITES TROP PETITES
        CONT = -1
ctmp+++
c       WRITE(istdo,*)'MAXSIZ = ',MAXSIZ
c       WRITE(istdo,*)'ADD(2,I_ADD) = ',ADD(2,I_ADD)
c       WRITE(istdo,*)'NB_EC = ',NB_EC
ctmp---
        RETURN
      ENDIF
      IF(ADD(1,I_ADD)+NB_NC.GT.MAXSIZS) THEN
C       PLUS DE PLACE DANS LA PILE DES ELEMENTS SECONDS BOITES TROP PETITES
        CONT = -1
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
C      1.1- STOCKAGE DU OU DES  NOEUD CANDIDAT ET DES ELTS CORRESP.
C          VIRER LES INUTILES
C
C       NCAND_PROV=NB_EC*NB_NC
Cel NCAND_PROV negatif qd NB_EC*NB_NC > 2e31
C
        IF(NB_EC+NB_NC.LE.128) THEN
          NCAND_PROV = NB_EC*NB_NC
        ELSE
          NCAND_PROV = 129
        ENDIF
C
        IF(DSUP.LT.MINBOX.OR.NB_NC.LE.NB_N_B.OR.NCAND_PROV.LE.128)THEN
Cel necessaire qd NB_NC.LE.NB_N_B ou DSUP.LT.MINBOX et NB_EC+NB_NC.GT.128
            NCAND_PROV = NB_EC*NB_NC
            DO L=1,NCAND_PROV
              I = 1+(L-1)/NB_NC
              J = L-(I-1)*NB_NC
              LE = BPE(I)
              LES = BPN(J)
              NE  = NELEM(LE)
              IF(LES.LE.NMES)THEN        ! candidat local
                NES = NELES(LES)
                IF(NE.NE.NES.AND.
     .             EMINXS(4,LES).GT.EMINX(1,LE)-TZINF.AND.
     .             EMINXS(5,LES).GT.EMINX(2,LE)-TZINF.AND.
     .             EMINXS(6,LES).GT.EMINX(3,LE)-TZINF.AND.
     .             EMINXS(1,LES).LT.EMINX(4,LE)+TZINF.AND.
     .             EMINXS(2,LES).LT.EMINX(5,LE)+TZINF.AND.
     .             EMINXS(3,LES).LT.EMINX(6,LE)+TZINF)THEN
                  I_STOK = I_STOK + 1
                  PROV_N(I_STOK) = LES
                  PROV_E(I_STOK) = LE
                  IF(I_STOK.EQ.MVSIZ-1)CALL I17STO(
     1               I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2               CONT  ,MX_CAND    )
                  IF(CONT.EQ.-2)RETURN
                ENDIF
              ELSE       ! partie complementaire SPMD
                LESL = LES-NMES
                IF(XREM(4,LESL).GT.EMINX(1,LE)-TZINF.AND.
     .             XREM(5,LESL).GT.EMINX(2,LE)-TZINF.AND.
     .             XREM(6,LESL).GT.EMINX(3,LE)-TZINF.AND.
     .             XREM(1,LESL).LT.EMINX(4,LE)+TZINF.AND.
     .             XREM(2,LESL).LT.EMINX(5,LE)+TZINF.AND.
     .             XREM(3,LESL).LT.EMINX(6,LE)+TZINF)THEN
                  I_STOK = I_STOK + 1
                  PROV_N(I_STOK) = LES
                  PROV_E(I_STOK) = LE
                  IF(I_STOK.EQ.MVSIZ-1)CALL I17STO(
     1               I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2               CONT  ,MX_CAND    )
                  IF(CONT.EQ.-2)RETURN
                ENDIF
              END IF
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
          IF(DY.EQ.DSUP) THEN
            DIR = 2
          ELSE IF(DZ.EQ.DSUP) THEN
            DIR = 3
          ENDIF
          SEUIL =(XYZM(DIR+3,I_ADD)+XYZM(DIR,I_ADD))*HALF
C
C    2.2- DIVISER LES ELEMENTS SECONDS EN TWO ZONES 
C
          NB_NCN= 0
          NB_NCN1= 0
          ADDNN= ADD(1,I_ADD)
          NB_ECN= 0
          ADDNE= ADD(2,I_ADD)
#include "vectorize.inc"
           DO I=1,NB_NC
            LES = BPN(I)
            IF(LES.LE.NMES)THEN
              IF(EMINXS(DIR,LES).LT.SEUIL) THEN
C               ON STOCKE DANS LA PILE PN
                NB_NCN1 = NB_NCN1 + 1
                ADDNN = ADDNN + 1
                PN(ADDNN) = LES
              ENDIF
            END IF
           ENDDO
           IF(NSPMD.GT.1)THEN           ! partie complementaire SPMD
#include "vectorize.inc"
             DO I=1,NB_NC
               LES = BPN(I)
               IF(LES.GT.NMES)THEN
                 LESL = LES-NMES
                 IF(XREM(DIR,LESL).LT.SEUIL) THEN
C                  ON STOCKE DANS LA PILE PN
                   NB_NCN1 = NB_NCN1 + 1
                   ADDNN = ADDNN + 1
                   PN(ADDNN) = LES
                 END IF
               END IF
             END DO
           END IF
C
#include "vectorize.inc"
           DO I=1,NB_NC
            LES = BPN(I)
            IF(LES.LE.NMES)THEN
              IF(EMINXS(DIR+3,LES).GE.SEUIL) THEN
C               ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
                NB_NCN = NB_NCN + 1
                BPN(NB_NCN) = LES
              ENDIF
            ENDIF
           ENDDO
           IF(NSPMD.GT.1)THEN           ! partie complementaire SPMD
#include "vectorize.inc"
             DO I=1,NB_NC
               LES = BPN(I)
               IF(LES.GT.NMES)THEN
                 LESL = LES-NMES
                 IF(XREM(DIR+3,LESL).GE.SEUIL) THEN
C                  ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
                   NB_NCN = NB_NCN + 1
                   BPN(NB_NCN) = LES
                 ENDIF
               ENDIF
             ENDDO
           END IF
C
C    2.3- DIVISER LES ELEMENTS MAINS
C
          NB_ECN= 0
          ADDNE= ADD(2,I_ADD)
          IF(NB_NCN1.EQ.0) THEN
C pas d'elements seconds dans la deuxieme boite
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR+3,LE)+TZINF.GE.SEUIL) THEN
C             ON STOCKE EN ECRASANT PROGRESSIVEMENT BPE
              NB_ECN = NB_ECN + 1
              BPE(NB_ECN) = LE
            ENDIF
           ENDDO
          ELSEIF(NB_NCN.EQ.0) THEN
C pas d'elements seconds dans la premiere boite
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR,LE)-TZINF.LT.SEUIL) THEN
C             ON STOCKE DANS LA PILE PE
              ADDNE = ADDNE + 1
              PE(ADDNE) = LE
            ENDIF
           ENDDO
          ELSE
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR,LE)-TZINF.LT.SEUIL) THEN
C             ON STOCKE DANS LA PILE PE
              ADDNE = ADDNE + 1
              PE(ADDNE) = LE
            ENDIF
            IF(EMINX(DIR+3,LE)+TZINF.GE.SEUIL) THEN
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
          IF(I_ADD+1.GE.I_ADD_MAX) THEN
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
      IF (I_ADD.EQ.1) THEN
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
Chd|  I17STO                        source/interfaces/int17/i17tri.F
Chd|-- called by -----------
Chd|        I17CUT                        source/interfaces/int17/i17tri.F
Chd|        I17TRI                        source/interfaces/int17/i17tri.F
Chd|-- calls ---------------
Chd|        ICONTACT_MOD                  share/modules/icontact_mod.F  
Chd|====================================================================
      SUBROUTINE I17STO(
     1     I_STOK,I_STOK_GLOB,PROV_N  ,CAND_N,PROV_E,CAND_E,
     2     CONT  ,MX_CAND    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ICONTACT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,I_STOK_GLOB,CONT  ,MX_CAND,
     .        PROV_N(*),CAND_N(*),PROV_E(*),CAND_E(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J_STOK_GLOB
      INTEGER LE,LES,NEXT,IC,J
C-----------------------------------------------
C     Recherche de doublons
      J=0

#include "lockon.inc"

      DO I=1,I_STOK
        LES = PROV_N(I)
        LE  = PROV_E(I)
        NEXT = ADCHAINE(LES)
        IF(NEXT == 0)THEN
            J=J+1
            PROV_N(J) = LES
            PROV_E(J) = LE
            MX_AD = MX_AD + 1
            IF(MX_AD > MX_CAND)THEN
              CONT = -2
#include "lockoff.inc"
              RETURN
            ENDIF
            ADCHAINE(LES)   = MX_AD
            CHAINE(1,MX_AD) = LE
            CHAINE(2,MX_AD) = 0
            goto 200
        ENDIF
 100    continue
        IC = NEXT
        IF(LE == CHAINE(1,IC))goto 200
          NEXT = CHAINE(2,IC)
          IF(NEXT /= 0)goto 100
            J=J+1
            PROV_N(J) = LES
            PROV_E(J) = LE
            MX_AD = MX_AD + 1
            IF(MX_AD > MX_CAND)THEN
              CONT = -2
#include "lockoff.inc"
              RETURN
            ENDIF
            CHAINE(2,IC)    = MX_AD
            CHAINE(1,MX_AD) = LE
            CHAINE(2,MX_AD) = 0
 200    continue
      ENDDO
      I_STOK = J

        J_STOK_GLOB = I_STOK_GLOB
        IF(I_STOK_GLOB + I_STOK.LE.MX_CAND)THEN
          I_STOK_GLOB = I_STOK_GLOB + I_STOK
        ELSE
          CONT = -2
        ENDIF
#include "lockoff.inc"
      IF(CONT.EQ.-2)RETURN
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
