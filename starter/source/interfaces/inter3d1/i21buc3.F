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
Chd|  I21BUC1                       source/interfaces/inter3d1/i21buc3.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I21COR3T                      source/interfaces/inter3d1/i21cor3t.F
Chd|        I21TRI                        source/interfaces/inter3d1/i21tri.F
Chd|        I7CMP3                        source/interfaces/inter3d1/i7cmp3.F
Chd|        I7DST3                        source/interfaces/inter3d1/i7dst3.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I21BUC1(
     1   X     ,IRECT ,NSV   ,BUMULT,NSEG    ,
     2   NMN   ,NRTM ,MWA    ,NSN   ,CAND_E  ,
     3   CAND_N,GAP  ,XYZM   ,NOINT ,I_STOK  ,
     4   DIST  ,TZINF,MAXBOX ,MINBOX,MSR     ,   
     5   STF   ,MULTIMP,ITAB  ,GAP_S  ,IGAP  ,
     6   GAPMIN  ,GAPMAX,INACTI,NRTS  ,IRECTS,
     7   XM0   ,DEPTH  ,MARGEREF ,DRAD,ID,TITR,
     8   I_MEM ,
     9   IX1 ,IX2    ,IX3    ,IX4    ,NSVG   ,
     1   X1  ,X2     ,X3     ,X4     ,Y1     ,
     2   Y2  ,Y3     ,Y4     ,Z1     ,Z2     ,
     3   Z3  ,Z4     ,XI     ,YI     ,ZI     ,
     4   X0  ,Y0     ,Z0     ,STIF   ,NX1    ,
     5   NY1 ,NZ1    ,NX2    ,NY2    ,NZ2    ,
     6   NX3 ,NY3    ,NZ3    ,NX4    ,NY4    ,
     7   NZ4 ,P1     ,P2     ,P3     ,P4     ,
     8   LB1 ,LB2    ,LB3    ,LB4    ,LC1    ,
     9   LC2 ,LC3    ,LC4    ,PENE   ,PROV_N ,
     1   PROV_E,N11  ,N21    ,N31    ,DGAPLOAD)
      USE MESSAGE_MOD
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
#include      "com04_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,I_STOK,MULTIMP,IGAP,INACTI,
     .        NRTS,I_MEM
      INTEGER IRECT(4,*),IRECTS(4,*),NSV(*),MSR(*),NSEG(*),MWA(*)
      INTEGER CAND_E(*),CAND_N(*),MAXSIZ
      INTEGER ITAB(*)
C     REAL
      my_real
     .   STF(*),X(3,*),XYZM(6,*),GAP_S(*), XM0(3,*),
     .   DIST,BUMULT,GAP,TZINF,MAXBOX,MINBOX,GAPMIN,GAPMAX, DEPTH, 
     .   MARGEREF
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) ::PROV_N,PROV_E
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: X0,Y0,Z0,STIF
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N11,N21,N31,PENE
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LC1,LC2,LC3,LC4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK, IBID
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B
C     REAL
      my_real
     .   DX1,DY1,DZ1,
     .   DX3,DY3,DZ3,
     .   DX4,DY4,DZ4,
     .   DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,DD0,XMIN,YMIN,ZMIN,
     .   XMAX,YMAX,ZMAX,TZINF0,MINBOX_ST,MAXBOX_ST,GAPSMAX,
     .   BID,TZINF_ST,MARGE_ST,GAPV(MVSIZ)
C-----------------------------------------------
C
C     1-CALCUL TAILLE DES ZONES INFLUENCES
C     DD EST LA LONGEUR MOYENNE ELEMENT
      DD=ZERO
      DO 10 L=1,NRTS
C      CONNECIVITES ELEMENT
       N1=IRECTS(1,L)
       N2=IRECTS(2,L)
       N3=IRECTS(3,L)
       N4=IRECTS(4,L)
C      LONGUEUR COTE 1
       DX1=(X(1,N1)-X(1,N2))
       DY1=(X(2,N1)-X(2,N2))
       DZ1=(X(3,N1)-X(3,N2))
       DD1=SQRT(DX1**2+DY1**2+DZ1**2)
C      LONGUEUR COTE 2
       DX3=(X(1,N1)-X(1,N4))
       DY3=(X(2,N1)-X(2,N4))
       DZ3=(X(3,N1)-X(3,N4))
       DD2=SQRT(DX3**2+DY3**2+DZ3**2)
C      LONGUEUR COTE 3
       DX4=(X(1,N3)-X(1,N2))
       DY4=(X(2,N3)-X(2,N2))
       DZ4=(X(3,N3)-X(3,N2))
       DD3=SQRT(DX4**2+DY4**2+DZ4**2)
C      LONGUEUR COTE 4
       DX6=(X(1,N4)-X(1,N3))
       DY6=(X(2,N4)-X(2,N3))
       DZ6=(X(3,N4)-X(3,N3))
       DD4=SQRT(DX6**2+DY6**2+DZ6**2)
       DD=DD+ (DD1+DD2+DD3+DD4)
  10  CONTINUE
C     TAILLE ZINF = .1*TAILLE MOYENNE ELEMENT DE CHAQUE COTE
C     TAILLE BUCKET MIN = TZINF  * BUMULT
      DD0= DD/NRTS/FOUR
C     DD = MAX(DD0,ONEP251*GAP)
      DD = MAX(DD0,ONEP251*(GAP+DGAPLOAD))
      DD = MAX(DD0,ONEP251*DEPTH)
      DD = MAX(DD ,ONEP251*DRAD)
C     TZINF = BUMULT*DD
      MARGEREF = BUMULT*DD
C calcul de TZINF en fct de la marge et non le contraire
C   peut etre ameliore pour engine (debut de run) : TZINF = MARGEREF + Sup(gap) !!!
      TZINF = MARGEREF + MAX(DEPTH,DRAD,(GAP+DGAPLOAD))
C     Depth >= max(Gapv) dans le starter
C MARGE_ST : marge independante du BUMULT en input pour trouver les memes pene initiales (cas delete ou chgt coord.)
      MARGE_ST = BMUL0*DD
      TZINF_ST = MARGE_ST + MAX(DEPTH,DRAD,(GAP+DGAPLOAD))
C     MAXBOX= ZEP9*(DD - TZINF)
C DD + 2*TZINF : taille element augmentee de zone influence
      MAXBOX= HALF*(DD + 2*TZINF)
      MINBOX= HALF*MAXBOX
      MAXBOX_ST= HALF*(DD + 2*TZINF_ST)
      MINBOX_ST= HALF*MAXBOX_ST
C     MIS A ZERO POUR FAIRE SEARCH COMPLET CYCLE 0 ENGINE
      DIST = ZERO     
C--------------------------------
C     CALCUL DES BORNES DU DOMAINE
C--------------------------------
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30 
C
      DO 20 I=1,NMN
         J=MSR(I) 
         XMIN= MIN(XMIN,X(1,J))
         YMIN= MIN(YMIN,X(2,J))
         ZMIN= MIN(ZMIN,X(3,J))
         XMAX= MAX(XMAX,X(1,J))
         YMAX= MAX(YMAX,X(2,J))
         ZMAX= MAX(ZMAX,X(3,J))
 20   CONTINUE
      XMIN=XMIN-TZINF_ST
      YMIN=YMIN-TZINF_ST
      ZMIN=ZMIN-TZINF_ST
      XMAX=XMAX+TZINF_ST
      YMAX=YMAX+TZINF_ST
      ZMAX=ZMAX+TZINF_ST
      DO 25 I=1,NSN
       J=NSV(I)
       XMIN= MIN(XMIN,X(1,J))
       YMIN= MIN(YMIN,X(2,J))
       ZMIN= MIN(ZMIN,X(3,J))
       XMAX= MAX(XMAX,X(1,J))
       YMAX= MAX(YMAX,X(2,J))
       ZMAX= MAX(ZMAX,X(3,J))
 25   CONTINUE
C
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
      NB_N_B = 1
      I_MEM = 0
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
 100  CONTINUE
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NRTM
C     P2........Elt PILE            2*NRTM
C     P21.......BPN                 NSN
C     P22.......PN                  NSN
C     P31.......ADDI                2000
C
C  POUR ONE MAILLAGE DE TOPOLOGIE CARRE LA TAILLE DE P2 PEUT ETRE ESTIMEE A:
C 4n:     NUMELC + 6*SQRT(NUMELC) + 8*LOG2(NUMELC) 
C 3n:     NUMELTG + 6*SQRT(2*NUMELTG) + 8*LOG2(NUMELTG)
C 
C 4n:     NUMELC + 6*SQRT(NUMELC) + 12*LOG(NUMELC) +
C 3n:     NUMELTG + 6*SQRT(2*NUMELTG) + 12*LOG(NUMELTG)
C 
C NUMELC + NUMELTG + 6*SQRT(NUMELC+2*NUMELTG) + 12*LOG(NUMELC+NUMELTG)
C 
C  POUR ONE MAILLAGE DE TOPOLOGIE LINEAIRE LA TAILLE DE P2 PEUT ETRE ESTIMEE A:
C NUMELC + NUMELTG + (NUMELC+NUMELTG)*(1 + 1/2 + 1/4 +...) + LOG2(NUMELC+NUMELTG)
C 3*(NUMELC+NUMELTG) + LOG2(NUMELC+NUMELTG)
C 3*(NUMELC+NUMELTG) + 300
C
      MAXSIZ = MAX(NUMNOD,NRTM+100)
      IP1 = 1
      IP2 = IP1+MAXSIZ
C      IP21= IP2+2*MAXSIZ
      IP21= IP2+3*MAXSIZ
      IP22= IP21+NUMNOD
      IP31= IP22+NUMNOD
C     IFIN= IP22+2000
C
C-----INITIALISATION DES ADRESSES ET X,Y,Z
C
C     ADDE     ADDN     X      Y      Z
C     1        1        XMIN   YMIN   ZMIN
C     1        1        XMAX   YMAX   ZMAX
C
      MWA(IP31) = 0
      MWA(IP31+1) = 0
      MWA(IP31+2) = 0
      MWA(IP31+3) = 0
      I_ADD = 1
      I_AMAX = 1
      XYZM(1,I_ADD) = XMIN
      XYZM(2,I_ADD) = YMIN
      XYZM(3,I_ADD) = ZMIN
      XYZM(4,I_ADD) = XMAX
      XYZM(5,I_ADD) = YMAX
      XYZM(6,I_ADD) = ZMAX
      I_STOK = 0
      J_STOK = 0
      ADNSTK = 0
      ADESTK = 0
      NB_NC = NSN
      NB_EC = NRTM
C
C-----COPIE DES NOS DE SEGMENTS ET DE NOEUDS DANS MWA(IP1) ET IP21
C
      DO 120 I=1,NB_EC
        MWA(IP1+I-1) = I
  120 CONTINUE
      DO 140 I=1,NB_NC
        MWA(IP21+I-1) = I
  140 CONTINUE
C
C-----DEBUT DE LA PHASE DE TRI 
C 
C     TANT QUE IL RESTE UNE ADRESSE A TRIER
C------------------
  200 CONTINUE
C------------------
C       SEPARER B ET N EN TWO
        CALL I21TRI(
     1   MWA(IP1),MWA(IP2),MWA(IP21),MWA(IP22),MWA(IP31+2*(I_ADD-2)),
     2   IRECT   ,X       ,NB_NC    ,NB_EC    ,XYZM,
     3   I_ADD   ,NSV     ,I_AMAX   ,XMAX     ,YMAX,
     4   ZMAX    ,3*MAXSIZ,I_STOK   ,I_MEM    ,NB_N_B, 
     5   CAND_N  ,CAND_E  ,NSN      ,NOINT    ,TZINF_ST,
     6   MAXBOX_ST,MINBOX_ST,J_STOK   ,MSR      ,XM0    ,
     7   MULTIMP ,ITAB    ,GAP      ,GAP_S    ,IGAP   ,
     8   GAPMIN   ,GAPMAX   ,MARGE_ST,DEPTH   ,DRAD   ,
     9   ID      ,TITR,
     1   IX1 ,IX2    ,IX3    ,IX4    ,NSVG   ,
     2   X1  ,X2     ,X3     ,X4     ,Y1     ,
     3   Y2  ,Y3     ,Y4     ,Z1     ,Z2     ,
     4   Z3  ,Z4     ,XI     ,YI     ,ZI     ,
     5   X0  ,Y0     ,Z0     ,STIF   ,NX1    ,
     6   NY1 ,NZ1    ,NX2    ,NY2    ,NZ2    ,
     7   NX3 ,NY3    ,NZ3    ,NX4    ,NY4    ,
     8   NZ4 ,P1     ,P2     ,P3     ,P4     ,
     9   LB1 ,LB2    ,LB3    ,LB4    ,LC1    ,
     1   LC2 ,LC3    ,LC4    ,PENE   ,PROV_N ,
     2   PROV_E,N11  ,N21    ,N31    ,DGAPLOAD)
C------------------
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
        IF (I_MEM == 2)THEN
          RETURN
        ENDIF
     
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        I_MEM = 0
        GO TO 100
      ELSE IF(I_MEM==2) THEN
        MARGE_ST = THREE_OVER_4*MARGE_ST
        TZINF_ST = MARGE_ST + MAX(DEPTH,DRAD,(GAP+DGAPLOAD))
        MAXBOX_ST= HALF*(DD + 2*TZINF_ST)
        MINBOX_ST= HALF*MAXBOX_ST
        I_MEM = 0
        IF(MARGE_ST<EM03) THEN
C             WRITE(ISTDO,*)' ***ERROR INFINITE LOOP DETECTED(I7BUC)'
C             WRITE(IOUT,*)' **ERROR INFINITE LOOP DETECTED(I7BUC)'
C             CALL ARRET(2)
           IF (ISTAMPING == 1) THEN
             CALL ANCMSG(MSGID=776,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR)
           ELSE
             CALL ANCMSG(MSGID=685,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR)
           ENDIF
        ENDIF
        GO TO 100
      ENDIF
      IF(I_ADD/=0) GO TO 200
C     FIN BOUCLE TANT QUE
C---------------------------------
      IF(J_STOK/=0)THEN
         LFT = 1
         LLT = J_STOK
         CALL I21COR3T(X    ,IRECT ,NSV  ,PROV_E ,PROV_N,
     2               GAPV  ,IGAP  ,GAP  ,GAP_S  ,GAPMIN ,
     3               GAPMAX,XM0   ,DEPTH,DRAD   ,IX1    ,
     4                   IX2   ,IX3   ,IX4   ,NSVG    ,X1     ,
     5                   X2    ,X3    ,X4    ,Y1      ,Y2     ,
     6                   Y3    ,Y4    ,Z1    ,Z2      ,Z3     ,
     7                   Z4    ,XI    ,YI    ,ZI      ,DGAPLOAD)
         CALL I7DST3(IX3,IX4,X1 ,X2 ,X3 ,
     1                  X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2                  Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3                  YI ,ZI ,X0 ,Y0 ,Z0 ,
     4                  NX1,NY1,NZ1,NX2,NY2,
     5                  NZ2,NX3,NY3,NZ3,NX4,
     6                  NY4,NZ4,P1 ,P2 ,P3 ,
     7                  P4 ,LB1,LB2,LB3,LB4,
     8                  LC1,LC2,LC3,LC4)
         CALL I7PEN3(MARGE_ST,GAPV,N11,N21,N31,
     1                  PENE ,NX1 ,NY1,NZ1,NX2,
     2                  NY2  ,NZ2 ,NX3,NY3,NZ3,
     3                  NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4                  P3   ,P4)
         IF(I_STOK+J_STOK<MULTIMP*NSN) THEN
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
            MARGE_ST = THREE_OVER_4*MARGE_ST
            TZINF_ST = MARGE_ST + MAX(DEPTH,DRAD,(GAP+DGAPLOAD))
            MAXBOX_ST= HALF*(DD + 2*TZINF_ST)
            MINBOX_ST= HALF*MAXBOX_ST
	    I_MEM = 0
            IF(MARGE_ST<EM03) THEN
C             WRITE(ISTDO,*)' ***ERROR INFINITE LOOP DETECTED(I7BUC)'
C             WRITE(IOUT,*)' **ERROR INFINITE LOOP DETECTED(I7BUC)'
C             CALL ARRET(2)
              IF (ISTAMPING == 1) THEN
                CALL ANCMSG(MSGID=776,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR)
              ELSE
                CALL ANCMSG(MSGID=685,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR)
              ENDIF
            ENDIF
            GO TO 100
          ENDIF
        ENDIF
      ENDIF
      IF(NSN/=0)THEN
      WRITE(IOUT,*)' POSSIBLE IMPACT NUMBER:',I_STOK,' (<=',
     . 1+(I_STOK-1)/NSN,'*NSN)'
      ELSE
       CALL ANCMSG(MSGID=552,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=ID,
     .             C1=TITR)
      ENDIF
C
C     MISE A ZERO DE TAG POUR I21PWR3
C
      DO I=1,NUMNOD
        MWA(I)=0
      ENDDO
C
      RETURN
      END
