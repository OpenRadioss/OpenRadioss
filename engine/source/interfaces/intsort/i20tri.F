Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    i20tri          ../engine/source/interfaces/intsort/i20tri.F
!||--- called by ------------------------------------------------------
!||    i20buce         ../engine/source/interfaces/intsort/i20buce.F
!||--- calls      -----------------------------------------------------
!||    i20sto          ../engine/source/interfaces/intsort/i20sto.F
!||    i7dstk          ../engine/source/interfaces/intsort/i7dstk.F
!||    spmd_oldnumcd   ../engine/source/mpi/interfaces/spmd_i7tool.F
!||--- uses       -----------------------------------------------------
!||    tri7box         ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I20TRI(
     1      ADD   ,NSN    ,RENUM  ,NSNR     ,ISZNSNR  ,
     2      IRECT ,XA     ,STF    ,STFA     ,XYZM     ,
     3      I_ADD ,NSV    ,MAXSIZ ,II_STOK  ,CAND_N   ,
     4      CAND_E,MULNSN ,NOINT  ,TZINF    ,MAXBOX   ,
     5      MINBOX,I_MEM  ,NB_N_B ,I_ADD_MAX,ESHIFT   ,
     6      INACTI,IFQ    ,CAND_A ,CAND_P   ,IFPEN    ,
     7      NRTM  ,NSNROLD,IGAP   ,GAP      ,GAP_S    ,
     6      GAP_M ,GAPMIN ,GAPMAX ,MARGE    ,CURV_MAX ,
     7      NIN   ,GAP_SH ,NBINFLG,MBINFLG  ,ISYM     ,
     8      INTHEAT,IDT_THERM,NODADT_THERM)
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
C   Purpose routine:
C   ===================
C   classify the elements of BPE and the nodes of BPN into two zones
C   > or < to a boundary determined here and sorts everything
C   in bpe,hpe, and bpn,hpn
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     BPE          ARRAY OF FACETTES TO SORT      => Local
C                  and of the result max side
C     PE           ARRAY OF FACETTES              => Local
C                  RESULTAT COTE MIN
C     BPN          SORTED NODES ARRAY        => Local
C                  and of the result max side
C     PN           NODES ARRAY => Local
C                  RESULTAT COTE MIN
C     ADD(2,*)     ARRAY OF ADRESSES              E/S 
C          1.......ADRESSES NODES C          2.......ADRESSES ELEMENTS
C     ZYZM(6,*)     ARRAY OF XYZMIN               E/S 
C          1.......XMIN BOITE
C          2.......YMIN BOITE
C          3.......ZMIN BOITE
C          4.......XMAX BOITE
C          5.......YMAX BOITE
C          6.......ZMAX BOITE
C     IRECT(4,*)   ARRAY OF CONEC FACETTES        E
C     X(3,*)       COORDONNEES NODALES               E
C     NB_NC        NUMBER OF CANDIDATE NODES        => Local
C     NB_EC        NUMBER OF CANDIDATE ELEMENTS           => Local
C     I_ADD        position in the i/o address table
C     NSV          NOS SYSTEMES DES NODES E
C     Xmax larger abcisse existing e
C     XMAX largest order.existing E
C     Xmax larger existing side E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     I_STOK       storage level of pairs
C                                CANDIDATES impact    E/S
C     ADNSTK       current address in the node box
C     CAND_N       boites resultats nodes C     ADESTK       current address in the element box
C     CAND_E       adresses des boites resultat elements
C                  MULNSN = MULTIMP*NSN max size allowed now for the
C                  COUPLES NODES,ELT CANDIDATES
C     NOINT        INTERFACE USER NUMBER
C     TZINF        TAILLE ZONE INFLUENCE
C     MAXBOX       TAILLE MAX BUCKET
C     MINBOX       TAILLE MIN BUCKET
C
C     Prov_n Provisional Cand_n (static variable in i20tri)
C     PROV_E       CAND_E provisoire (variable static in i20tri)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_ADD,MAXSIZ,I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NSNROLD,
     .        MULNSN,NB_N_B,NOINT,I_ADD_MAX,INACTI,IFQ,NSNR,IGAP,
     .        ADD(2,*),IRECT(4,*), NIN,NBINFLG(*),MBINFLG(*),ISYM,
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        II_STOK
      INTEGER, INTENT(IN) :: INTHEAT
      INTEGER, INTENT(IN) :: IDT_THERM
      INTEGER, INTENT(IN) :: NODADT_THERM
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
     .        
C BPE: Use on nrtm and not nrtm + 100 rigorously (here maxsiz = nrtm + 100)
     .        BPE(MAXSIZ/3),PE(MAXSIZ),BPN(NSN+NSNR),PN(NSN+NSNR),
     .        OLDNUM(ISZNSNR)
C     REAL
      my_real
     .   DX,DY,DZ,DSUP,SEUIL, XX1, XX2, XX3, XX4,
     .   YY1, YY2, YY3, YY4, ZZ1, ZZ2, ZZ3, ZZ4,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, BGAPSMX,
     .      
     .   
     .   SMOINS,SPLUS,XX
C-----------------------------------------------
C initial construction phase of BPE and BPN moved from I7BUCE => I7TRI
C
      BGAPSMX = ZERO
      XMIN = XYZM(1,I_ADD)
      YMIN = XYZM(2,I_ADD)
      ZMIN = XYZM(3,I_ADD)
      XMAX = XYZM(4,I_ADD)
      YMAX = XYZM(5,I_ADD)
      ZMAX = XYZM(6,I_ADD)
C
C copy of segment and node numbers in BPE and BPN
C
      NB_EC = 0
      DO I=1,NRTM
C We do not retain the Destruit facets
        IF(STF(I)/=ZERO)THEN
          NB_EC = NB_EC + 1
          BPE(NB_EC) = I
        ENDIF
      ENDDO
C
C optimization // search for nodes within xmin xmax of
C processor elements
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
C Non -local candidate account in SPMD
C
      DO I = NSN+1, NSN+NSNR
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C in SPMD, for inacti or IFQ, retrieves old numbering of non-local candidates
C
      IF(NSPMD>1.AND.
     +  (INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0)) THEN
        CALL SPMD_OLDNUMCD(RENUM,OLDNUM,ISZNSNR,NSNROLD,INTHEAT,IDT_THERM,NODADT_THERM)
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
C    1- sorting phase on the median along the largest direction
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
C    2- DIVISER LES NODES EN TWO ZONES
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
C           store at the bottom of the BP stack
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
C             store at the bottom of the BP stack
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
C             store at the bottom of the BP stack
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
C    3- divide the elements
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
C            store at the bottom of the BP stack
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
C             store at the bottom of the BP stack
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
C             store at the bottom of the BP stack
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
C             store at the bottom of the BP stack
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
Cfill the min of the next box and the max of the current one
C     (i.e. threshold is a max for the current one)
C     We're going to go down and so we define a new box
C     fill the max of the new box
C     initialises in i7buc1 a 1.E30 comme ca on recupere
c     either XMAX or the max of the box
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
C     increment the descent level before exiting
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
C                    BOITE NE CONTENANT QU'ONE NODE C                    NO MORE MEMORY AVAILABLE
C
C-------------------TEST ON MEMORY EXCEEDED------------
C
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       NO MORE SPACE IN THE STACK OF TOO SMALL BOX ELEMENTS
        I_MEM = 1
        RETURN
      ENDIF
C
C--------------------TEST ON EMPTY BOXES--------------
C
      IF(NB_EC/=0.AND.NB_NC/=0) THEN
C
        DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
        DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
        DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
        DSUP= MAX(DX,DY,DZ)
C
C-------------------TEST ON END OF BRANCH ------------
C       1- STORAGE OF THE CANDIDATE NODE(S) AND CORRESPONDING ELEMENTS
C          REMOVE THE UNNECESSARY ONES
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
C       EMPTY BOX OR
C       END OF BRANCH
C       decrement the descent level before restarting
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
      IF (I_ADD/=0) THEN
C-------------------------------------------------------------------------
C         MUST COPY THE BOTTOM OF STACKS INTO CORRESPONDING STACK_BOTTOM
C         BEFORE DESCENDING INTO THE ADJACENT BRANCH
C-------------------------------------------------------------------------
          CALL I7DSTK(NB_NC,NB_EC,ADD(1,I_ADD),BPN,PN,BPE,PE)
C=======================================================================
          GOTO 200
C=======================================================================
      ENDIF
C-------------------------------------------------------------------------
C     END OF SORTING
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
!||====================================================================
!||    i20tri_edge   ../engine/source/interfaces/intsort/i20tri.F
!||--- called by ------------------------------------------------------
!||    i20buc_edge   ../engine/source/interfaces/intsort/i20buce.F
!||--- calls      -----------------------------------------------------
!||    i11insid      ../engine/source/interfaces/intsort/i11tri.F
!||    i20sto_edge   ../engine/source/interfaces/intsort/i20sto.F
!||    i7dstk        ../engine/source/interfaces/intsort/i7dstk.F
!||--- uses       -----------------------------------------------------
!||    tri7box       ../engine/share/modules/tri7box.F
!||====================================================================
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
C   ROLE OF THE ROUTINE:
C   ===================
C   CLASSIFIES THE ELEMENTS OF BPE AND THE NODES OF BPN IN TWO ZONES
C   > OR < TO A BOUNDARY DETERMINED HERE AND OUTPUTS EVERYTHING
C   IN bpe,hpe, and bpn,hpn
C-----------------------------------------------
C   D u m m Y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     BPE          ARRAY OF FACETTES TO SORT      => Local
C                  AND OF THE RESULT ON MAX SIDE
C     PE           ARRAY OF FACETTES              => Local
C                  RESULTAT COTE MIN
C     BPN          SORTED NODES ARRAY        => Local
C                  AND OF THE RESULT ON MAX SIDE
C     PN           NODES ARRAY => Local
C                  RESULTAT COTE MIN
C     ADD(2,*)     ARRAY OF ADRESSES              E/S 
C          1.......ADRESSES NODES C          2.......ADRESSES ELEMENTS
C     ZYZM(6,*)     ARRAY OF XYZMIN               E/S 
C          1.......XMIN BOITE
C          2.......YMIN BOITE
C          3.......ZMIN BOITE
C          4.......XMAX BOITE
C          5.......YMAX BOITE
C          6.......ZMAX BOITE
C     IXLINM(2,*)   ARRAY OF CONEC         E
C     IXLINS(2,*)   ARRAY OF CONEC         E
C     X(3,*)       COORDONNEES NODALES               E
C     NB_NC        NUMBER OF CANDIDATE NODES        => Local
C     NB_EC        NUMBER OF CANDIDATE ELEMENTS           => Local
C     I_ADD        POSITION IN THE I/O ADDRESS TABLE
C     Xmax larger abcisse existing e
C     XMAX largest order.existing E
C     Xmax larger existing side E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     I_STOK       storage level for pairs
C                                CANDIDATES impact    E/S
C     ADNSTK       current address in the node box
C     CAND_S       boites resultats nodes C     ADESTK       current address in the element box
C     CAND_M       adresses des boites resultat elements
C     NSN4         4*NSN MAX SIZE ALLOWED NOW FOR THE
C                  COUPLES NODES,ELT CANDIDATES
C     NOINT        INTERFACE USER NUMBER
C     TZINF        TAILLE ZONE INFLUENCE
C     MAXBOX       TAILLE MAX BUCKET
C     MINBOX       TAILLE MIN BUCKET
C
C     Prov_s Provisional Cand_s (static variable in i7tri)
C     PROV_M       CAND_M provisoire (variable static in i7tri)
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
     .        N1,N2,NN,NE,K,L,NCAND_PROV,J_STOK,NI,
     .        ISTOP,NB_ECN1,PROV_S(2*MVSIZ),PROV_M(2*MVSIZ),
     .        NB_NC_OLD, NB_EC_OLD, NB_NC, NB_EC,JJ,KK,
C BPE : used on NLINMA and not NLINMA + 100
C BPN : used on NLINSA and not NLINSA + 100
     .        BPE(NLINMA+100),PE(MAXSIZ),BPN(NLINSA+NLINSR+100),
     .        PN(MAXSIZ)
C     REAL
      my_real
     .        DX,DY,DZ,DSUP,SEUIL, XX1, XX2,
     .        XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX,
     .        XMINS,YMINS,ZMINS,XMAXS,YMAXS,ZMAXS,
     .        YY1,YY2,ZZ1,ZZ2,DMX,DMY,DMZ,
     .        XY1,XY2,XZ1,XZ2,XIMIN,XIMAX,XJMIN,XJMAX,XKMIN,XKMAX,
     .        TIMIN,TIMAX,TJMIN,TJMAX,TKMIN,TKMAX,TSMIN,TSMAX,
     .        TXMIN, TXMAX,TYMIN, TYMAX,TZMIN, TZMAX
      EXTERNAL I11INSID
      LOGICAL I11INSID
C-----------------------------------------------
C
C Initial construction phase of BPE and BPN moved from I11BUCE => I11TRI
C
C
C retrieving domain boundaries
C
      XMIN = XYZM(1,I_ADD)
      YMIN = XYZM(2,I_ADD)
      ZMIN = XYZM(3,I_ADD)
      XMAX = XYZM(4,I_ADD)
      YMAX = XYZM(5,I_ADD)
      ZMAX = XYZM(6,I_ADD)

C Copy segment and node numbers into BPE and BPN

      NB_EC = 0
      DO I=1,NLINMA
C We no longer retain the Destruit facets
        IF(STFM(I)/=ZERO)THEN
          NB_EC = NB_EC + 1
          BPE(NB_EC) = I
        END IF
      ENDDO
C
C Optimization // search for nodes included in xmin xmax of
C processor elements
C
      NB_NC = 0
      DO I=1,NLINSA
C We do not retain the Destruit facets
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
C Non -local candidate account in SPMD
C
      DO I = NLINSA+1, NLINSA+NLINSR
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C     GOTO 200:
C     INTERFACE WITH 1 SEGMENT ET 1 NODE + INITIALISATION DX DY DZ
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
C    1- SORTING PHASE ON THE MEDIAN ALONG THE LARGEST DIRECTION
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

c     box size reduction:
c     keep a margin of TZINF when reducing box size
c     to avoid forgetting secondary elements
c
c      |      Tzinf Tzinf                 |Tzinf
c      |     <-----x----->                |<---->
c      |     .............................|............Tymax ^
c      |     .                            |     .            |
c      |     .           #################|#####.##          | Tzinf
c      |     .           #////////////////|/////./#          |
c -----+----------------------------------+---------Ymax=    v
c      |     .     |\\\\\#///  espace   //|/////./# Ymax_old
c      |     .     |\\\#/// occupied by//|/////./#
c      |     .     |\\\#///the mains//|/////./#
c      |     .     |\\\\\#////////////////|/////./#
c      |     .     |\\\\\#////////////////|/////./#
c      |     .     |\\\\\#################|#####.##          ^
c      |     .     |\\\  espace retenu  \\|     .            |
c      |     .     |\or the secondaries\|     .            | Tzinf
c      |.| \\\ (new box) \\ |.|
c      |     .     +----------------------|     ....Ymin     x
c      |     .                            |     .            |
c      |     .    (boite de recherche main)   .            | Tzinf
c      |     .                            |     .            |
c      |     .............................|.........Tymin    v
c      |     .     .                      |     .
c      |     .     .                      |     .
c      |(old box) |.
c      |     .     .                      |     .
c      |     .     .                      |     .
c -----+----------------------------------+---------Ymin_old
c      |     .     .                      |     .
c      |     .     .                    Xmax=   .
c Xmin_old   .     .                 Xmax_old   .
c            .   Xmin                        Txmax
c          Txmin
c
c       if the box is reduced on the Xmin side we could use:
c           Txmin = Xmin  with Xmin = min(Xmain)-Tzinf > Xmin_old
c       
c       But using:
c           Txmin = Xmin-Tzinf (= min(Xmain) - 2*Tzinf) 
c       on ne penalise pas I11INSIND 
c       (there is no main in the overestimated zone)
c       and the calculation of Xmin, Txmin ... is simpler


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
C         STORE AT THE BOTTOM OF THE BP STACK
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
C    3- DIVIDE THE MAINS
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
c        If the box has been reworked (see 1)
c        it is possible that Timin=Ximin ...


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
C          STORE AT THE BOTTOM OF THE BP STACK
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
C          STORE AT THE BOTTOM OF THE BP STACK
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
Cfill the min of the next box and the max of the current one
C     (i.e. threshold is a max for the current one)
C     We're going to go down and so we define a new box
C     fill the max of the new box
C     initialises in i7buc1 a 1.E30 comme ca on recupere
c     either XMAX or the max of the box
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
C     increment the descent level before exiting
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
C                    BOITE NE CONTENANT QU'ONE NODE C                    NO MORE MEMORY AVAILABLE
C                    THE PARTITIONING DOES NOT REDUCE THE CANDIDATES
C
C-------------------TEST ON MEMORY EXCEEDED------------
C
      IF(ADD(1,I_ADD)+NB_NC>MAXSIZ) THEN
C       NO MORE SPACE IN THE STACK OF TOO SMALL SECONDARY BOX SIDES
        I_MEM = 1
        RETURN
      ENDIF
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       NO MORE SPACE IN THE STACK OF TOO SMALL MAIN BOX SIDES
        I_MEM = 1
        RETURN
      ENDIF
C
C--------------------TEST ON EMPTY BOXES--------------
C
      IF(NB_EC/=0.AND.NB_NC/=0) THEN
C
        DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
        DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
        DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
        DSUP= MAX(DX,DY,DZ)
C
C-------------------TEST ON END OF BRANCH ------------
C       1- storage of candidate node(s) and corresponding elements
C          remove the useless ones
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
C       empty box or
C       end of branch
C       decrement the descent level before restarting
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
      IF (I_ADD/=0) THEN
C-------------------------------------------------------------------------
C         need to copy the bottoms of stacks into corresponding bas_de_pile
C         before going down into the adjacent branch
C-------------------------------------------------------------------------
          CALL I7DSTK(NB_NC,NB_EC,ADD(1,I_ADD),BPN,PN,BPE,PE)
C=======================================================================
          GOTO 200
C=======================================================================
      ENDIF
C-------------------------------------------------------------------------
C     end of sorting
C-------------------------------------------------------------------------
      IF(J_STOK/=0)CALL I20STO_EDGE(
     1              J_STOK,IXLINS,IXLINM,XA    ,II_STOKE,
     2              CAND_S,CAND_M,NSN4  ,NOINT ,TZINF  ,
     3              I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM  ,
     4              CHAINE,NLINSA ,NIN   )
C-------------------------------------------------------------------------
      RETURN
      END

