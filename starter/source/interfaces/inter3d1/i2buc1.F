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
Chd|  I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I2COR3                        source/interfaces/inter3d1/i2cor3.F
Chd|        I2DST3                        source/interfaces/inter3d1/i2dst3.F
Chd|        I2DST3_27                     source/interfaces/inter3d1/i2dst3_27.F
Chd|        I2TRI                         source/interfaces/inter3d1/i2tri.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I2BUC1(
     1   X     ,IRECT,NSV   ,NSEG  ,IRTL,
     2   NMN   ,NRTM ,MWA   ,NSN   ,XYZM  ,
     3   NOINT ,MSR  ,ST    ,DMIN  ,TZINF05,
     4   IGNORE,THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     5   NOD2ELS,NOD2ELC,NOD2ELTG,
     6   NINT   ,IXC ,IXTG  ,THK_PART,IPARTC     ,
     7   GEO    ,IXS ,IXS10 ,PM      ,IXS16      ,
     8   IXS20  ,IPARTTG ,ID  ,TITR  ,IGEO       ,
     9   PM_STACK , IWORKSH,
     1   IX1    ,IX2    ,IX3,IX4 ,NSVG      ,
     2   PROV_N ,PROV_E ,N11,N12 ,N13       ,
     3   X1     ,X2     ,X3 ,X4  ,STIF      ,
     4   Y1     ,Y2     ,Y3 ,Y4  ,Z1        ,
     5   Z2     ,Z3     ,Z4 ,XI  ,YI        ,
     6   ZI     ,X0     ,Y0 ,Z0  ,NX1       ,
     7   NY1    ,NZ1    ,NX2,NY2 ,NZ2       ,
     8   NX3    ,NY3    ,NZ3,NX4 ,NY4       ,
     9   NZ4    ,P1     ,P2 ,P3  ,P4        ,
     1   LB1    ,LB2    ,LB3,LB4 ,LC1       ,
     2   LC2    ,LC3    ,LC4,S   ,T         ,
     2   ILEV)
      USE MESSAGE_MOD
C============================================================================
C  cette routine est appelee par : ININT3(/inter3d1/inint3.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I2TRI(/inter3d1/i2tri.F)
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr08_c.inc"
#include      "scr17_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT, IGNORE, NINT,ILEV
      INTEGER IRECT(4,*),NSV(*),NSEG(*),MWA(*)
      INTEGER MSR(*),IRTL(*),MAXSIZ,KNOD2ELS(*), KNOD2ELC(*), 
     .   KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),
     .   IXS(NIXS,*),IXS10(*), IXS16(*), IXS20(*),IPARTTG(*),IGEO(*),
     .   IWORKSH(*)
C     REAL
      my_real
     .   X(3,*),XYZM(6,*),ST(*),DMIN(*),TZINF05,THK(*),THK_PART(*),
     .   GEO(NPROPG,*),PM(*),PM_STACK(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: PROV_N,PROV_E,NSVG
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N11,N12,N13
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LC1,LC2,LC3,LC4,STIF
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: S,T
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX,I_MEM
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK,IEL,N
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B,IS,IAD,
     .   MG,IP,NELS,NELC,NELTG,JJ,JJJ,IFLAG
C     REAL
      my_real
     .   DX1,DY1,DZ1,DX3,DY3,DZ3,DX4,DY4,DZ4,DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,XMIN,YMIN,ZMIN,MAXBOX,MINBOX,XMAX,YMAX,ZMAX,
     .   BID,TZINFMIN,THKSECND,THKMAIN,AREA,VOL,GAPV(MVSIZ),DSEARCH
C
!      EQUIVALENCE (PROV_N,IV(1,6)) ,(PROV_E,IV(1,7))
C=======================================================================
C     1-CALCUL TAILLE DES ZONES INFLUENCES
c
      DD = ZERO
      DSEARCH = TZINF05
      IFLAG = 1
      IF (IGNORE >= 2) THEN
         THKSECND = ZERO
         THKMAIN = ZERO
C        DD EST LA LONGEUR MAX DIAGONALES
        DO 5 L=1,NRTM
C        CONNECTIVITES ELEMENT
         N1=IRECT(1,L)
         N2=IRECT(2,L)
         N3=IRECT(3,L)
         N4=IRECT(4,L)
C        LONGUEUR DIAG 1
         DX1=(X(1,N1)-X(1,N3))
         DY1=(X(2,N1)-X(2,N3))
         DZ1=(X(3,N1)-X(3,N3))
         DD=MAX(DD,SQRT(DX1**2+DY1**2+DZ1**2))
C        LONGUEUR DIAG 2
         DX3=(X(1,N2)-X(1,N4))
         DY3=(X(2,N2)-X(2,N4))
         DZ3=(X(3,N2)-X(3,N4))
         DD=MAX(DD,SQRT(DX1**2+DY1**2+DZ1**2))
  5     CONTINUE
        DO I=1,NSN
          IS = NSV(I)
C--         Cooques 4n
          DO IAD = KNOD2ELC(IS)+1,KNOD2ELC(IS+1)
            IEL  = NOD2ELC(IAD)
            MG=IXC(6,IEL)
            IP = IPARTC(IEL)
            IF ( THK_PART(IP) /= ZERO) THEN  
              THKSECND = MAX(THKSECND,THK_PART(IP))
            ELSEIF ( THK(IEL) /= ZERO) THEN
              THKSECND = MAX(THKSECND,THK(IEL))
            ELSE
              THKSECND = MAX(THKSECND,GEO(1,MG))
            ENDIF
          ENDDO 
C--         Cooques 3n
          DO IAD = KNOD2ELTG(IS)+1,KNOD2ELTG(IS+1)
            IEL  = NOD2ELTG(IAD)  
            MG=IXTG(5,IEL)
            IP = IPARTTG(IEL)
            IF ( THK_PART(IP) /= ZERO) THEN  
              THKSECND = MAX(THKSECND,THK_PART(IP))
            ELSEIF ( THK(IEL) /= ZERO) THEN
              THKSECND = MAX(THKSECND,THK(IEL))
            ELSE
              THKSECND = MAX(THKSECND,GEO(1,MG))
            ENDIF
          ENDDO
        ENDDO
        DO I=1,NRTM
          NELS = 0
          NELC = 0
          NELTG = 0
          CALL INSOL3(X,IRECT,IXS,NINT,NELS,I,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .            IXS16,IXS20)
          CALL INCOQ3(IRECT,IXC ,IXTG ,NINT   ,NELC     ,
     .            NELTG,I,GEO  ,PM     ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,2,IGEO ,
     .            PM_STACK , IWORKSH)
          IF (NELC /= 0) THEN
            MG=IXC(6,NELC)
            IP = IPARTC(NELC) 
            IF ( THK_PART(IP) /= ZERO) THEN  
              THKMAIN = MAX(THKMAIN,THK_PART(IP))
            ELSEIF ( THK(NELC) /= ZERO) THEN
              THKMAIN = MAX(THKMAIN,THK(NELC))
            ELSE
              THKMAIN = MAX(THKMAIN,GEO(1,MG))
            ENDIF
          ELSEIF (NELTG /= 0)THEN
            MG=IXTG(5,NELTG)
            IP = IPARTTG(NELTG) 
            IF ( THK_PART(IP) /= ZERO) THEN  
              THKMAIN = MAX(THKMAIN,THK_PART(IP))
            ELSEIF ( THK(NUMELC+NELTG) /= ZERO) THEN
              THKMAIN = MAX(THKMAIN,THK(NUMELC+NELTG))
            ELSE
              THKMAIN = MAX(THKMAIN,GEO(1,MG))
            ENDIF
          ELSEIF(NELS/=0 .AND. NELS <= NUMELS8 .AND. IGNORE == 2) THEN
            DO JJ=1,8
              JJJ=IXS(JJ+1,NELS)
              XC(JJ)=X(1,JJJ)
              YC(JJ)=X(2,JJJ)
              ZC(JJ)=X(3,JJJ)
            END DO
            CALL VOLINT(VOL)
C
            THKMAIN = MAX(THKMAIN,VOL/AREA)
          ENDIF
        ENDDO
C       TAILLE BUCKET MIN = TZINF05
        IF(TZINF05==ZERO)
     .           TZINF05 = MAX(ZEP05*DD,0.6*(THKMAIN+THKSECND))
        MAXBOX= TWO*TZINF05
        MINBOX= HALF*MAXBOX
        TZINFMIN = TZINF05
      ELSE
C     DD EST LA LONGEUR MOYENNE ELEMENT
        DO 10 L=1,NRTM
C        CONNECTIVITES ELEMENT
         N1=IRECT(1,L)
         N2=IRECT(2,L)
         N3=IRECT(3,L)
         N4=IRECT(4,L)
C        LONGUEUR COTE 1
         DX1=(X(1,N1)-X(1,N2))
         DY1=(X(2,N1)-X(2,N2))
         DZ1=(X(3,N1)-X(3,N2))
         DD1=(DX1**2+DY1**2+DZ1**2)
C        LONGUEUR COTE 2
         DX3=(X(1,N1)-X(1,N4))
         DY3=(X(2,N1)-X(2,N4))
         DZ3=(X(3,N1)-X(3,N4))
         DD2=(DX3**2+DY3**2+DZ3**2)
C        LONGUEUR COTE 3
         DX4=(X(1,N3)-X(1,N2))
         DY4=(X(2,N3)-X(2,N2))
         DZ4=(X(3,N3)-X(3,N2))
         DD3=(DX4**2+DY4**2+DZ4**2)
C        LONGUEUR COTE 4
         DX6=(X(1,N4)-X(1,N3))
         DY6=(X(2,N4)-X(2,N3))
         DZ6=(X(3,N4)-X(3,N3))
         DD4=(DX6**2+DY6**2+DZ6**2)
         DD=DD+ (DD1+DD2+DD3+DD4)
  10    CONTINUE
C       TAILLE BUCKET MIN = TZINF05
        DD = SQRT(DD/NRTM/FOUR)
        IF(TZINF05==ZERO)TZINF05 = DD
        MAXBOX= TWO*TZINF05
        MINBOX= HALF*MAXBOX
        TZINFMIN = TZINF05*EM01
      ENDIF
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
      XMIN=XMIN-TZINF05
      YMIN=YMIN-TZINF05
      ZMIN=ZMIN-TZINF05
      XMAX=XMAX+TZINF05
      YMAX=YMAX+TZINF05
      ZMAX=ZMAX+TZINF05
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
        CALL I2TRI(
     1   MWA(IP1),MWA(IP2),MWA(IP21),MWA(IP22),MWA(IP31+2*(I_ADD-2)),
     2   IRECT   ,X       ,NB_NC    ,NB_EC    ,XYZM,
     3   I_ADD   ,NSV     ,I_AMAX   ,XMAX     ,YMAX,
     4   ZMAX    ,3*MAXSIZ,I_MEM    ,NB_N_B   ,NSN , 
     5   NOINT   ,TZINF05 ,MAXBOX   ,MINBOX   ,J_STOK,
     6   IRTL    ,ST       ,DMIN    ,IGNORE   ,THK   ,
     7   KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS  ,NOD2ELC,
     8   NOD2ELTG,NINT    ,IXC    ,
     9   IXTG    ,THK_PART ,IPARTC  ,GEO      ,IXS    ,
     A   IXS10   ,PM       ,IXS16   ,IXS20    ,IPARTTG,
     B   ID      ,TITR     ,IGEO    ,DSEARCH  ,PM_STACK ,
     C   IWORKSH ,
     D   IX1    ,IX2    ,IX3,IX4 ,NSVG      ,
     1   PROV_N ,PROV_E ,N11,N12 ,N13       ,
     2   X1     ,X2     ,X3 ,X4  ,STIF      ,
     3   Y1     ,Y2     ,Y3 ,Y4  ,Z1        ,
     4   Z2     ,Z3     ,Z4 ,XI  ,YI        ,
     5   ZI     ,X0     ,Y0 ,Z0  ,NX1       ,
     6   NY1    ,NZ1    ,NX2,NY2 ,NZ2       ,
     7   NX3    ,NY3    ,NZ3,NX4 ,NY4       ,
     8   NZ4    ,P1     ,P2 ,P3  ,P4        ,
     9   LB1    ,LB2    ,LB3,LB4 ,LC1       ,
     1   LC2    ,LC3    ,LC4,S   ,T         ,
     2   ILEV)
C------------------
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
      IF(I_MEM==1)THEN
        NB_N_B = NB_N_B + 1
        I_MEM = 0
        GO TO 100
      ELSE IF(I_MEM==2) THEN
        TZINF05 = THREE_OVER_4*TZINF05
C        ne pas dimunuer la taille des boite
C        MINBOX= THREE_OVER_4*MINBOX
C        MAXBOX= THREE_OVER_4*MAXBOX
        I_MEM = 0
        IF( TZINF05<TZINFMIN ) THEN
          CALL ANCMSG(MSGID=83,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
        GO TO 100
      ENDIF
      IF(I_ADD/=0) GO TO 200
C     FIN BOUCLE TANT QUE
C---------------------------------
      IF(J_STOK/=0)THEN
         LFT = 1
         LLT = J_STOK
         CALL I2COR3(X  ,IRECT ,NSV  ,PROV_E ,PROV_N,
     .               BID ,BID ,GAPV  ,0      ,TZINF05,
     .               BID ,BID ,0     ,NINT    ,IXC   , 
     4               IXTG  ,THK_PART,IPARTC,GEO   , NOINT,
     5               IXS   ,IXS10 ,PM    ,THK     ,KNOD2ELS,
     6               KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC,NOD2ELTG,
     7               IGNORE,IXS16 ,IXS20  ,IPARTTG,IGEO,DSEARCH ,
     8               PM_STACK , IWORKSH ,IX1     ,IX2   ,
     5               IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6               X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7               Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8               XI     ,YI     ,ZI  ,STIF    ,IFLAG )
C
         IF (ILEV == 27) THEN
C--        Improved projections on triangles for spotflag27
           CALL I2DST3_27(GAPV,PROV_E ,PROV_N,TZINF05,IRTL,ST,DMIN,IGNORE,
     .                THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS,
     .                NOD2ELC,NOD2ELTG,X,IRECT,
     .                NINT,IXC ,IXTG  ,THK_PART,IPARTC,GEO,
     .                NOINT,IXS,IXS10,PM,IX3,
     1                IX4,X1 ,X2 ,X3 ,X4 ,
     1                Y1 ,Y2 ,Y3 ,Y4 ,Z1 ,
     2                Z2 ,Z3 ,Z4 ,XI ,YI ,
     3                ZI ,X0 ,Y0 ,Z0 ,NX1,
     4                NY1,NZ1,NX2,NY2,NZ2,
     5                NX3,NY3,NZ3,NX4,NY4,
     6                NZ4,P1 ,P2 ,P3 ,P4 ,
     7                LB1,LB2,LB3,LB4,LC1,
     8                LC2,LC3,LC4,S  ,T  ) 
         ELSE
           CALL I2DST3(GAPV,PROV_E ,PROV_N,TZINF05,IRTL,ST,DMIN,IGNORE,
     .                THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS,
     .                NOD2ELC,NOD2ELTG,X,IRECT,
     .                NINT,IXC ,IXTG  ,THK_PART,IPARTC,GEO,
     .                NOINT,IXS,IXS10,PM,IX3,
     1                IX4,X1 ,X2 ,X3 ,X4 ,
     1                Y1 ,Y2 ,Y3 ,Y4 ,Z1 ,
     2                Z2 ,Z3 ,Z4 ,XI ,YI ,
     3                ZI ,X0 ,Y0 ,Z0 ,NX1,
     4                NY1,NZ1,NX2,NY2,NZ2,
     5                NX3,NY3,NZ3,NX4,NY4,
     6                NZ4,P1 ,P2 ,P3 ,P4 ,
     7                LB1,LB2,LB3,LB4,LC1,
     8                LC2,LC3,LC4,S  ,T  )
         ENDIF  
C
      ENDIF
C
C
      RETURN
      END
