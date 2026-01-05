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
!||    i7tri           ../engine/source/interfaces/intsort/i7tri.F
!||--- called by ------------------------------------------------------
!||    i7buce          ../engine/source/interfaces/intsort/i7buce.F
!||--- calls      -----------------------------------------------------
!||    i7dstk          ../engine/source/interfaces/intsort/i7dstk.F
!||    i7sto           ../engine/source/interfaces/intsort/i7sto.F
!||    spmd_oldnumcd   ../engine/source/mpi/interfaces/spmd_i7tool.F
!||--- uses       -----------------------------------------------------
!||    tri7box         ../engine/share/modules/tri7box.F
!||====================================================================
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
     A      CAND_F ,KREMNOD  ,REMNOD  ,FLAGREMNODE,DGAPLOAD,
     B      INTHEAT,IDT_THERM,NODADT_THERM)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   role of the routine:
C   ===================
C   classify BPE elements and BPN nodes in two zones
C   > or < to a boundary determined here and output all
C   in bpe,hpe, and bpn,hpn
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     BPE          ARRAY OF FACETTES TO SORT      => Local
C                  and from the result max side
C     PE           ARRAY OF FACETTES              => Local
C                  RESULTAT COTE MIN
C     BPN          SORTED NODES ARRAY        => Local
C                  and from the result max side
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
C     I_ADD        position in the table of i/o addresses
C     NSV          NOS SYSTEMES DES NODES E
C     Xmax larger abcisse existing e
C     XMAX largest order.existing E
C     Xmax larger existing side E
C     MAXSIZ       TAILLE MEMOIRE MAX POSSIBLE       E
C     I_STOK       storage level for pairs
C                                CANDIDATES impact    E/S
C     ADNSTK       current address in the node box
C     CAND_N       boites resultats nodes C     ADESTK       current address in the element box
C     CAND_E       adresses des boites resultat elements
C                  MULNSN = MULTIMP*NSN maximum size now allowed for the
C                  COUPLES NODES,ELT CANDIDATES
C     NOINT        INTERFACE USER NUMBER
C     TZINF        TAILLE ZONE INFLUENCE
C     MAXBOX       TAILLE MAX BUCKET
C     MINBOX       TAILLE MIN BUCKET
C
C     Prov_n Provisional Cand_n (static variable in i7tri)
C     PROV_E       CAND_E provisoire (variable static in i7tri)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_ADD,MAXSIZ,I_MEM,ESHIFT,NSN,ISZNSNR,NRTM,NSNROLD,
     .        MULNSN,NB_N_B,NOINT,I_ADD_MAX,INACTI,IFQ,NSNR,IGAP,NIN,
     .        ADD(2,*),IRECT(4,*),
     .        NSV(*),CAND_N(*),CAND_E(*),CAND_A(*),IFPEN(*),RENUM(*),
     .        INTTH,II_STOK,ITIED
      INTEGER KREMNOD(*),REMNOD(*),FLAGREMNODE
      INTEGER, INTENT(IN) :: INTHEAT
      INTEGER, INTENT(IN) :: IDT_THERM
      INTEGER, INTENT(IN) :: NODADT_THERM
C     REAL
      my_real
     .   X(3,*),XYZM(6,*),CAND_P(*),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MAXBOX,MINBOX,MARGE,GAP,GAPMIN,GAPMAX,
     .   CURV_MAX(*),GAP_S_L(*),GAP_M_L(*),CAND_F(*)
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,ADDNN,ADDNE,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,J_STOK,II,JJ,
     .        PROV_N(2*MVSIZ),PROV_E(2*MVSIZ),
     .        
C BPE: Use on nrtm and not nrtm + 100 rigorously (here maxsiz = nrtm + 100)
     .        BPE(MAXSIZ/3),PE(MAXSIZ),BPN(NSN+NSNR),PN(NSN+NSNR),
     .        OLDNUM(ISZNSNR),IADD
C     REAL
      my_real
     .   AAA,
     .   DX,DY,DZ,DSUP,TRHRESHOLD, XX1, XX2, XX3, XX4,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, BGAPSMX,
     .      
     .   
     .   SMOINS,SPLUS,XX
C REMNODE
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGREMNODE
      INTEGER DELNOD,M

C-----------------------------------------------
C initial construction phase of BPE and BPN moved from I7BUCE => I7TRI
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

      IF(IGAP==3) THEN
        IADD = 10
      ENDIF
C
C optimization // search for nodes within xmin xmax of the
C processor elements
C
      NB_NC = 0
      DO I=1,NSN
        J=NSV(I)
        IF(STFN(I)/=ZERO) THEN

         IF(X(1,J)>=XMIN.AND.X(1,J)<=XMAX.AND.
     .     X(2,J)>=YMIN.AND.X(2,J)<=YMAX.AND.
     .     X(3,J)>=ZMIN.AND.X(3,J)<=ZMAX)THEN

          NB_NC=NB_NC+1
          BPN(NB_NC) = I
         ENDIF
        ENDIF
      ENDDO
C
C Non -local candidate account in SPMD
C
      DO I = NSN+1, NSN+NSNR
        IF( XREM(1,I-NSN)<XMIN) CYCLE
        IF( XREM(1,I-NSN)>XMAX) CYCLE
        IF( XREM(2,I-NSN)<YMIN) CYCLE
        IF( XREM(2,I-NSN)>YMAX) CYCLE
        IF( XREM(3,I-NSN)<ZMIN) CYCLE
        IF( XREM(3,I-NSN)>ZMAX) CYCLE
        NB_NC = NB_NC + 1
        BPN(NB_NC) = I
      ENDDO
C
C in SPMD, for inacti or IFQ, recovers old numbering of non-local candidates
C
      IF(NSPMD>1.AND.
     +  (INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.
     +   ITIED/=0)) THEN
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
C    1- sorting phase on the median according to the largest direction
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
      TRHRESHOLD =(SMOINS+SPLUS)*HALF
C
C    2- DIVISER LES NODES EN TWO ZONES
C
      NB_NCN= 0
      NB_NCN1= 0
      ADDNN= ADD(1,I_ADD)
C
      GAPSMX = ZERO
      DO I=1,NB_NC
       J = BPN(I)
       IF(J <= NSN) THEN
        XX = X(DIR,NSV(J))
        IF(XX < TRHRESHOLD) THEN
C         we store at the bottom of the BP stack
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = J
          IF(IGAP /=0) GAPSMX = MAX(GAPSMX,GAP_S(J))
          SMOINS = MAX(SMOINS,XX)
        ENDIF
       ENDIF
      ENDDO
      DO I=1,NB_NC
       J = BPN(I)
       IF(J > NSN) THEN
        XX = XREM(DIR,J-NSN)
        IF(XX < TRHRESHOLD) THEN
C         we store at the bottom of the BP stack
          NB_NCN1 = NB_NCN1 + 1
          ADDNN = ADDNN + 1
          PN(ADDNN) = J
          IF(IGAP/=0) GAPSMX = MAX(GAPSMX,XREM(9,J-NSN))
          SMOINS = MAX(SMOINS,XX)
        ENDIF
       ENDIF
      ENDDO
      BGAPSMX = ZERO
      DO I=1,NB_NC
       J = BPN(I)
       IF(J <= NSN) THEN
        XX = X(DIR,NSV(J))
        IF(XX >= TRHRESHOLD) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = J
          IF(IGAP/=0) BGAPSMX = MAX(BGAPSMX,GAP_S(J))
          SPLUS = MIN(SPLUS,XX)
        ENDIF
       ENDIF
      ENDDO
      DO I=1,NB_NC
       J = BPN(I)
       IF(J > NSN) THEN
        XX = XREM(DIR,J-NSN)
        IF(XX >= TRHRESHOLD) THEN
C         ON STOCKE EN ECRASANT PROGRESSIVEMENT BPN
          NB_NCN = NB_NCN + 1
          BPN(NB_NCN) = J
          IF(IGAP /= 0) BGAPSMX = MAX(BGAPSMX,XREM(9,J-NSN))
          SPLUS = MIN(SPLUS,XX)
        ENDIF
       ENDIF
      ENDDO
C
C    3- divide the elements
C
      NB_ECN= 0
      ADDNE= ADD(2,I_ADD)
      IF(NB_NCN1==0) THEN
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF(IGAP == 0) THEN
           AAA = TZINF+CURV_MAX(NE)
         ELSEIF(IGAP == 3) THEN
           AAA =  MAX(DRAD,DGAPLOAD+MIN(MAX(BGAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +       +MARGE+CURV_MAX(NE)
         ELSE
           AAA = MAX(DRAD,DGAPLOAD+MIN(MAX(BGAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
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
          AAA = -MAX(DRAD,DGAPLOAD+MIN(MAX(GAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +          -MARGE-CURV_MAX(NE)
         ELSE
          AAA = -MAX(DRAD,DGAPLOAD+MIN(MAX(GAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
     -           -MARGE-CURV_MAX(NE)
         ENDIF
         XMIN = MIN(XX1,XX2,XX3,XX4) + AAA

         IF(XMIN < SMOINS) THEN
C         we store at the bottom of the BP stack
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
       ELSE
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF( IGAP == 0 ) THEN
           AAA=-TZINF-CURV_MAX(NE)
         ELSEIF(IGAP == 3) THEN
           AAA=  - MAX(DRAD,DGAPLOAD+MIN(MAX(GAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +       -MARGE-CURV_MAX(NE)
         ELSE
           AAA=  -MAX(DRAD,DGAPLOAD+MIN(MAX(GAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
     -       -MARGE-CURV_MAX(NE)
         ENDIF
         XMIN =  MIN(XX1,XX2,XX3,XX4) + AAA 
         IF(XMIN < SMOINS) THEN
C         we store at the bottom of the BP stack
          ADDNE = ADDNE + 1
          PE(ADDNE) = NE
         ENDIF
        ENDDO
C
        DO I=1,NB_EC
         NE = BPE(I)
         XX1=X(DIR, IRECT(1,NE))
         XX2=X(DIR, IRECT(2,NE))
         XX3=X(DIR, IRECT(3,NE))
         XX4=X(DIR, IRECT(4,NE))
         IF( IGAP == 0) THEN
            AAA =TZINF+CURV_MAX(NE)
         ELSEIF( IGAP==3 ) THEN
            AAA=  MAX(DRAD,DGAPLOAD+MIN(MAX(BGAPSMX+MAX(GAP_M(NE),GAP_M_L(NE)),GAPMIN),GAPMAX))
     +       +MARGE+CURV_MAX(NE)
         ELSE
          AAA = MAX(DRAD,DGAPLOAD+MIN(MAX(BGAPSMX+GAP_M(NE),GAPMIN),GAPMAX))
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
C-----we fill the min of the next box and the max of the current one
C     (i.e. THRESHOLD is a max for the current one)
C     We're going to go down and so we define a new box
C     we fill the max of the new box
C     initialises in i7buc1 a 1.E30 comme ca on recupere
C     either XMAX or the max of the box
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
C     we increment the descent level before exiting
      I_ADD = I_ADD + 1
      IF(I_ADD+1>=I_ADD_MAX) THEN
c       WRITE(6,*) __LINE__,__LINE__
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
C                    BOITE NE CONTENANT QU'ONE NODE C                    No More Memory Available
C
C-------------------test for memory exceeded------------
C
      IF(ADD(2,I_ADD)+NB_EC>MAXSIZ) THEN
C       no more room in the stack of elements boxes too small
         WRITE(6,*) __LINE__,__LINE__

        I_MEM = 1
        RETURN
      ENDIF
C
C--------------------test for empty boxes--------------
C
      IF(NB_EC/=0.AND.NB_NC/=0) THEN
C
        DX = XYZM(4,I_ADD) - XYZM(1,I_ADD)
        DY = XYZM(5,I_ADD) - XYZM(2,I_ADD)
        DZ = XYZM(6,I_ADD) - XYZM(3,I_ADD)
        DSUP= MAX(DX,DY,DZ)
C
C-------------------test for end of branch ------------
C       1- storage of candidate node(s) and corresponding elements
C          remove the useless ones
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
             IF( JJ<=NSN ) THEN
               IF( IGAP == 0 ) THEN
                 TZ = TZINF+CURV_MAX(NE)
               ELSEIF( IGAP == 3 ) THEN
                 TZ = MAX(DRAD,DGAPLOAD+MAX(MIN(GAP_S_L(JJ)+GAP_M_L(NE),GAPMAX),GAPMIN)
     .           +MARGE+CURV_MAX(NE))
               ELSE
                 TZ=MAX(DRAD,DGAPLOAD+MAX(MIN(GAP_S(JJ)+GAP_M(NE),GAPMAX),GAPMIN)
     +           +MARGE+CURV_MAX(NE))
               ENDIF
             ELSE
               II = JJ-NSN
               IF( IGAP == 0 ) THEN
                 TZ = TZINF+CURV_MAX(NE)
               ELSEIF( IGAP == 3 ) THEN
                 TZ = MAX(DRAD,DGAPLOAD+MAX(MIN(XREM(IADD,II)+GAP_M_L(NE)
     .           ,GAPMAX),GAPMIN))+MARGE+CURV_MAX(NE)
               ELSE
                 TZ = MAX(DRAD,DGAPLOAD+MAX(MIN(XREM(9,II)+GAP_M(NE),GAPMAX),GAPMIN))
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
             IF(JJ<=NSN) THEN

               IF(FLAGREMNODE==2) THEN
                 IF(TAGREMNODE(NSV(JJ)) == 1) CYCLE
               ENDIF
               NN=NSV(JJ)
               IF(NN/=N1.AND.NN/=N2.AND.NN/=N3.AND.NN/=N4.AND.
     &           X(1,NN)>XMIN.AND.X(1,NN)<XMAX.AND.
     &           X(2,NN)>YMIN.AND.X(2,NN)<YMAX.AND.
     &           X(3,NN)>ZMIN.AND.X(3,NN)<ZMAX ) THEN
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
                 IF(DELNOD /= 0) CYCLE
               ENDIF
               IF(XREM(1,II)>XMIN.AND.
     &            XREM(1,II)<XMAX.AND.
     &            XREM(2,II)>YMIN.AND.
     &            XREM(2,II)<YMAX.AND.
     &            XREM(3,II)>ZMIN.AND.
     &            XREM(3,II)<ZMAX ) THEN
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = JJ
                 PROV_E(J_STOK) = NE
               ENDIF
             ENDIF
            ENDDO ! L=K,MIN(K-1+NVSIZ,NCAND_PROV)

            IF(J_STOK>=NVSIZ)THEN
              CALL I7STO(
     1             NVSIZ,IRECT   ,X     ,NSV   ,II_STOK,
     2             CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3             I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4             IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5             OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6             GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7             GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8             CAND_F,DGAPLOAD)
               IF(I_MEM==2) THEN
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
C=======================================================================
          GOTO 100
C=======================================================================
        ENDIF
      ENDIF
C-------------------------------------------------------------------------
C       empty box or
C       end of branch
C       we decrement the descent level before restarting
C-------------------------------------------------------------------------
      I_ADD = I_ADD - 1
      IF (I_ADD/=0) THEN
C-------------------------------------------------------------------------
C         the bottom of the stacks must be copied into corresponding stack bottoms
C         before going back down into the adjacent branch
C-------------------------------------------------------------------------
          CALL I7DSTK(NB_NC,NB_EC,ADD(1,I_ADD),BPN,PN,BPE,PE)
C=======================================================================
          GOTO 200
C=======================================================================
      ENDIF
C-------------------------------------------------------------------------
C     end of sorting
C-------------------------------------------------------------------------
      IF(J_STOK/=0)CALL I7STO(
     1              J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2              CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3              I_MEM ,PROV_N ,PROV_E,ESHIFT,INACTI ,
     4              IFQ   ,CAND_A ,CAND_P,IFPEN ,NSN    ,
     5              OLDNUM,NSNROLD,IGAP  ,GAP   ,GAP_S  ,
     6              GAP_M ,GAPMIN ,GAPMAX,CURV_MAX,NIN  ,
     7              GAP_S_L,GAP_M_L,INTTH,DRAD,ITIED    ,
     8              CAND_F ,DGAPLOAD)
C-------------------------------------------------------------------------
      IF(FLAGREMNODE==2) THEN
        DEALLOCATE(TAGREMNODE)
      ENDIF
      RETURN
      END
