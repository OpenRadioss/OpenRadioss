Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i17tri         ../engine/source/interfaces/int17/i17tri.F
!||--- called by ------------------------------------------------------
!||    i17buce        ../engine/source/interfaces/int17/i17buce.F
!||    i17buce_pena   ../engine/source/interfaces/int17/i17buce.F
!||--- calls      -----------------------------------------------------
!||    ancmsg         ../engine/source/output/message/message.F
!||    arret          ../engine/source/system/arret.F
!||    i17cut         ../engine/source/interfaces/int17/i17tri.F
!||    i17sto         ../engine/source/interfaces/int17/i17tri.F
!||--- uses       -----------------------------------------------------
!||    message_mod    ../engine/share/message_module/message_mod.F
!||====================================================================
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
      use element_mod , only : nixs
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
ctmp+1
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     Parameter (i_add_max = 1001) declare in i17buce
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
C in all rigor maxsiz = nme and maxsizs = nmes+nmesr
      my_real
     .        XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C------------------------------------
C---------------------------------
C     init of main boxes
C---------------------------------
C
      NB_EC = NME
      DO L=1,NB_EC
        I = L + ESH_T
        BPE(L) = I
      ENDDO
C
Cinitialization of addresses and x, y, z
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
        IF(EMINXS(4,I)>XMIN.AND.
     .     EMINXS(5,I)>YMIN.AND.
     .     EMINXS(6,I)>ZMIN.AND.
     .     EMINXS(1,I)<XMAX.AND.
     .     EMINXS(2,I)<YMAX.AND.
     .     EMINXS(3,I)<ZMAX)THEN
          NB_NC=NB_NC+1
          BPN(NB_NC) = I
        ENDIF
      ENDDO
C
C Non -local candidate account in SPMD
C
      DO I = NMES+1, NMES+NMESR
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C-----------------------------------------------
C     loop over boxes
C-----------------------------------------------
      DO WHILE (CONT==1)
C-----------------------------------------------
C       Decouption of space by 2 following x y or z
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
C       end or error test
C-----------------------------------------------
C     CONT = 0  ==> FIN
C     CONT = -1 ==> PAS ASSEZ DE MEMOIRE PILE
C     CONT = -2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
C     CONT = -3 ==> TROP NIVEAUX PILE 
      IF(CONT==0)THEN
        IF(I_STOK/=0)CALL I17STO(
     1      I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2      CONT  ,MX_CAND    )
      ENDIF
      IF(CONT==-1) THEN
          CALL ANCMSG(MSGID=86,ANMODE=ANINFO,I1=NOINT)
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
!||====================================================================
!||    i17cut    ../engine/source/interfaces/int17/i17tri.F
!||--- called by ------------------------------------------------------
!||    i17tri    ../engine/source/interfaces/int17/i17tri.F
!||--- calls      -----------------------------------------------------
!||    i17sto    ../engine/source/interfaces/int17/i17tri.F
!||--- uses       -----------------------------------------------------
!||    tri7box   ../engine/share/modules/tri7box.F
!||====================================================================
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
      use element_mod , only : nixs
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
#include      "com01_c.inc"
C-----------------------------------------------
C   Routine Role:
C   ===================
C   classifies the bpe elts and bpn nodes into two zones
C   > or < to a boundary determined here and outputs everything
C   in bpe, hpe, and bpn, hpn
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     BPE          TABLEAU DES FACETTES A TRIER      E/S
C                  and of the result on the max side
C     PE           TABLEAU DES FACETTES              S
C                  RESULTAT COTE MIN
C     BPN          TABLEAU DES NOEUDS A TRIER        E/S
C                  and of the result on the max side
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
C     i_add position in the i/o address table
C     NSV          NOS SYSTEMES DES NOEUDS           E
C     Xmax larger abcisse existing e
C     XMAX largest order.existing E
C     Xmax larger existing side E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     i_stok storage level of pairs
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
C                    No More Available Memory
C
Cmemory exceeded test
C
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       no more space in the main elements stack, boxes too small
        CONT = -1
ctmp+++
c       WRITE(istdo,*)'MAXSIZ = ',MAXSIZ
c       WRITE(istdo,*)'ADD(2,I_ADD) = ',ADD(2,I_ADD)
c       WRITE(istdo,*)'NB_EC = ',NB_EC
ctmp---
        RETURN
      ENDIF
      IF(ADD(1,I_ADD)+NB_NC>MAXSIZS) THEN
C       no more space in the secondary elements stack, boxes too small
        CONT = -1
        RETURN
      ENDIF
C
Cempty box test
C
      IF(NB_EC/=0.AND.NB_NC/=0) THEN
C
        DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
        DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
        DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
        DSUP= MAX(DX,DY,DZ)
C
Cend of branch test
C      1.1- storage of candidate node(s) and corresponding elts
C          remove the useless ones
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
C   necessary qd nb_nc <= nb_n_b or dsup <minbox and nb_ec+nb_nc> 128
            NCAND_PROV = NB_EC*NB_NC
            DO L=1,NCAND_PROV
              I = 1+(L-1)/NB_NC
              J = L-(I-1)*NB_NC
              LE = BPE(I)
              LES = BPN(J)
              NE  = NELEM(LE)
              IF(LES<=NMES)THEN        ! candidat local
                NES = NELES(LES)
                IF(NE/=NES.AND.
     .             EMINXS(4,LES)>EMINX(1,LE)-TZINF.AND.
     .             EMINXS(5,LES)>EMINX(2,LE)-TZINF.AND.
     .             EMINXS(6,LES)>EMINX(3,LE)-TZINF.AND.
     .             EMINXS(1,LES)<EMINX(4,LE)+TZINF.AND.
     .             EMINXS(2,LES)<EMINX(5,LE)+TZINF.AND.
     .             EMINXS(3,LES)<EMINX(6,LE)+TZINF)THEN
                  I_STOK = I_STOK + 1
                  PROV_N(I_STOK) = LES
                  PROV_E(I_STOK) = LE
                  IF(I_STOK==MVSIZ-1)CALL I17STO(
     1               I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2               CONT  ,MX_CAND    )
                  IF(CONT==-2)RETURN
                ENDIF
              ELSE       ! SPMD complementary part
                LESL = LES-NMES
                IF(XREM(4,LESL)>EMINX(1,LE)-TZINF.AND.
     .             XREM(5,LESL)>EMINX(2,LE)-TZINF.AND.
     .             XREM(6,LESL)>EMINX(3,LE)-TZINF.AND.
     .             XREM(1,LESL)<EMINX(4,LE)+TZINF.AND.
     .             XREM(2,LESL)<EMINX(5,LE)+TZINF.AND.
     .             XREM(3,LESL)<EMINX(6,LE)+TZINF)THEN
                  I_STOK = I_STOK + 1
                  PROV_N(I_STOK) = LES
                  PROV_E(I_STOK) = LE
                  IF(I_STOK==MVSIZ-1)CALL I17STO(
     1               I_STOK,I_STOK_GLOB,PROV_N,CAND_N,PROV_E,CAND_E,
     2               CONT  ,MX_CAND    )
                  IF(CONT==-2)RETURN
                ENDIF
              END IF
            ENDDO 
C-----------------------------------------------------------
        ELSE
C-----------------------------------------------------------
C
C
C    2- sorting phase on the median according to the largest direction
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
            IF(LES<=NMES)THEN
              IF(EMINXS(DIR,LES)<SEUIL) THEN
C               stored in the pn stack
                NB_NCN1 = NB_NCN1 + 1
                ADDNN = ADDNN + 1
                PN(ADDNN) = LES
              ENDIF
            END IF
           ENDDO
           IF(NSPMD>1)THEN           ! SPMD complementary part
#include "vectorize.inc"
             DO I=1,NB_NC
               LES = BPN(I)
               IF(LES>NMES)THEN
                 LESL = LES-NMES
                 IF(XREM(DIR,LESL)<SEUIL) THEN
C                  stored in the pn stack
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
            IF(LES<=NMES)THEN
              IF(EMINXS(DIR+3,LES)>=SEUIL) THEN
C               ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
                NB_NCN = NB_NCN + 1
                BPN(NB_NCN) = LES
              ENDIF
            ENDIF
           ENDDO
           IF(NSPMD>1)THEN           ! SPMD complementary part
#include "vectorize.inc"
             DO I=1,NB_NC
               LES = BPN(I)
               IF(LES>NMES)THEN
                 LESL = LES-NMES
                 IF(XREM(DIR+3,LESL)>=SEUIL) THEN
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
          IF(NB_NCN1==0) THEN
C no secondary elements in the second box
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
C no secondary elements in the first box
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR,LE)-TZINF<SEUIL) THEN
C             stored in the pe stack
              ADDNE = ADDNE + 1
              PE(ADDNE) = LE
            ENDIF
           ENDDO
          ELSE
#include "vectorize.inc"
           DO I=1,NB_EC
            LE = BPE(I)
            IF(EMINX(DIR,LE)-TZINF<SEUIL) THEN
C             stored in the pe stack
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
Cfill the min of the next box and the max of the current one
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
C     increment the descent level before exiting
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
C     end of sorting test
C-------------------------------------------------------------------------
      IF (I_ADD==1) THEN
        CONT = 0
        RETURN
      ENDIF
C-----------------------------------------------------------
C
C    3- end of branch or empty box
C                    
C-----------------------------------------------------------
C-------------------------------------------------------------------------
C       decrement the descent level before restarting
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
C-------------------------------------------------------------------------
C         need to copy the bottom of the stacks into the corresponding bottom_of_stack
C         before descending into the neighboring branch
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
!||====================================================================
!||    i17sto         ../engine/source/interfaces/int17/i17tri.F
!||--- called by ------------------------------------------------------
!||    i17cut         ../engine/source/interfaces/int17/i17tri.F
!||    i17tri         ../engine/source/interfaces/int17/i17tri.F
!||--- uses       -----------------------------------------------------
!||    icontact_mod   ../engine/share/modules/icontact_mod.F
!||====================================================================
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
C     duplicate search
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
            chaine(1,mx_ad) = le
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
            chaine(1,mx_ad) = le
            CHAINE(2,MX_AD) = 0
 200    continue
      ENDDO
      I_STOK = J

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
