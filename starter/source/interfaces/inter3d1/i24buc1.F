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
Chd|  I24BUC1                       source/interfaces/inter3d1/i24buc1.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        INT24                         hypermesh/int24/int24.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I24S1S2                       source/interfaces/inter3d1/i24buc1.F
Chd|        I24TRI                        source/interfaces/inter3d1/i24tri.F
Chd|        I24XFIC_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|        I7CMP3                        source/interfaces/inter3d1/i7cmp3.F
Chd|        I7COR3                        source/interfaces/inter3d1/i7cor3.F
Chd|        I7DST3                        source/interfaces/inter3d1/i7dst3.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24BUC1(
     1   X     ,IRECT,NSV    ,BUMULT,NSEG    ,
     2   NMN   ,NRTM ,MWA    ,NSN   ,CAND_E  ,
     3   CAND_N,GAP  ,XYZM   ,NOINT ,I_STOK  ,
     4   DIST  ,TZINF,MAXBOX ,MINBOX,MSR     ,   
     5   STF   ,STFN ,MULTIMP,ISTF  ,IDDLEVEL,
     6   ITAB  ,GAP_S,GAP_M  ,IGAP  ,GAPMIN  ,
     7   GAPMAX,INACTI,GAP_S_L,GAP_M_L,I_MEM ,
     8   MARGE ,ID   ,TITR   ,NBINFLG,MBINFLG,
     9   ILEV  ,MSEGTYP,GAP_N ,MVOISN,IXS   ,
     A   IXS10 ,IXS16  ,IXS20 ,IPARTNS,IPEN0 ,
     B   PENMAX,IRTSE ,IS2SE  ,IS2PT ,XFIC   ,
     C   NRTSE ,NSNE  ,PROV_N ,PROV_E,NSVG   ,
     1   IX1,IX2,IX3,IX4,X1 ,
     2   X2 ,X3 ,X4  ,Y1 ,Y2 ,
     3   Y3 ,Y4 ,Z1  ,Z2 ,Z3 ,
     4   Z4 ,XI ,YI  ,ZI ,X0 ,
     5   Y0 ,Z0 ,STIF,PENE,NX1,
     6   NY1,NZ1,NX2,NY2,NZ2,
     7   NX3,NY3,NZ3,NX4,NY4,
     8   NZ4,P1 ,P2 ,P3 ,P4 ,
     9   LB1,LB2,LB3,LB4,LC1,
     1   LC2,LC3,LC4,N11,N21,
     2   N31)
#ifndef HYPERMESH_LIB
      USE MESSAGE_MOD
#endif
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
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,I_STOK,MULTIMP,ISTF,IGAP,
     .        INACTI,MVOISN(4,*),IPARTNS(*),IPEN0,IRTSE(*),
     .        IS2SE(*) ,IS2PT(*),NRTSE ,NSNE
      INTEGER IRECT(4,*),NSV(*),NSEG(*),MWA(*)
      INTEGER CAND_E(*),CAND_N(*),MSR(*),MAXSIZ,IDDLEVEL
      INTEGER ITAB(*),NBINFLG(*),MBINFLG(*),ILEV,MSEGTYP(*)
      INTEGER IXS(*), IXS10(6,*), IXS16(8,*), IXS20(12,*)
C     REAL ou REAL*8
      my_real
     .   STF(*),STFN(*),X(3,*),XYZM(6,*),GAP_S(*),GAP_M(*),
     .   DIST,BUMULT,GAP,TZINF,MAXBOX,MINBOX,GAPMIN,GAPMAX,
     .   GAP_S_L(*),GAP_M_L(*),MARGE,GAP_N(12,*),PENMAX,XFIC(3,*)
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
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX,I_MEM,N_SOL
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK, IBID
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B,
     +         NPT_E,NSN0,LWORK,NUMNODT
C     REAL ou REAL*8
      my_real
     .   DX1,DY1,DZ1,
     .   DX3,DY3,DZ3,
     .   DX4,DY4,DZ4,
     .   DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,DD0,XMIN,YMIN,ZMIN,
     .   XMAX,YMAX,ZMAX,TZINF0,MINBOX0,MAXBOX0,GAPSMAX,
     .   BID,TZINF_ST,MARGE_ST,GAPV(MVSIZ),DD_ST,D_MAX,PENSOL,D_MOY
      INTEGER ,
     .         DIMENSION(:),ALLOCATABLE :: IWORK
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: XTEN
C
!      EQUIVALENCE (PROV_N,IV(1,6)) ,(PROV_E,IV(1,7)) 
      
      GAPMAX=EP30
      GAPMIN=ZERO
C----edge fictive nodes----- 
      NSN0 = NSN - NSNE 
      IF (NSNE >0) THEN
       NUMNODT =NUMNOD+NSNE
       ALLOCATE(XTEN(3,NUMNODT))
       XTEN=ZERO
       XTEN(1:3,1:NUMNOD) = X(1:3,1:NUMNOD)
       NPT_E = 3
       CALL I24XFIC_INI(NRTSE   ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                  NSN     ,NSV     ,X       ,XFIC    ,NPT_E    )
       XTEN(1:3,NUMNOD+1:NUMNODT) = XFIC(1:3,1:NSNE)
       MAXSIZ = MAX(NUMNODT,NRTM+100)
       IP1 = 1
       IP2 = IP1+MAXSIZ
C      IP21= IP2+2*MAXSIZ
       IP21= IP2+3*MAXSIZ
       IP22= IP21+NUMNODT
       IP31= IP22+NUMNODT
C------don't use no more MWA which is not sufficient; I_ADD<=1000   
       LWORK = IP31 + 2000
       ALLOCATE(IWORK(LWORK))
      END IF
C-------use temporaly GAP_N(1,*)=V/A; MVOISN(1,*)-> MTYPE(solid),MVOISN(2,*)-> E_id
C
C
C     1-CALCUL TAILLE DES ZONES INFLUENCES
C     DD EST LA LONGEUR MOYENNE ELEMENT
C     DD_ST EST LA LONGEUR MAX ELEMENT
      DD=ZERO
      DD_ST=ZERO
      PENSOL=EP30
      D_MOY = ZERO
      N_SOL = 0
      DO 10 L=1,NRTM
C      CONNECIVITES ELEMENT
       N1=IRECT(1,L)
       N2=IRECT(2,L)
       N3=IRECT(3,L)
       N4=IRECT(4,L)
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
C-------only for solid---  and coating shell
       IF (MSEGTYP(L)==0.OR.MSEGTYP(L)>NRTM) THEN
        D_MAX=MAX(DD1,DD2,DD3,DD4)
        D_MAX=MIN(D_MAX,GAP_N(1,L))
C--------correction of too huge GAP_N(1,L) w/ irregular mesh         
        GAP_N(1,L)=D_MAX
        DD_ST=MAX(DD_ST,D_MAX)
        N_SOL = N_SOL + 1
        D_MOY = D_MOY + D_MAX
       END IF
C       IF (MSEGTYP(L)==0) DD_ST=MAX(DD_ST,DD1,DD2,DD3,DD4)
  10  CONTINUE
C     TAILLE ZINF = .1*TAILLE MOYENNE ELEMENT DE CHAQUE COTE
C     TAILLE BUCKET MIN = TZINF  * BUMULT
C     DD = DD/NRTM/4 
      DD0=DD/NRTM/QUATRE
      DD=DD0
C      DD = MAX(DD0,UNP251*GAP)
C     TZINF = BUMULT*DD
      MARGE = BUMULT*DD
C calcul de TZINF en fct de la marge et non le contraire
      TZINF = MARGE + GAP
C MARGE_ST : marge independante du BUMULT en input pour trouver les memes pene initiales (cas delete ou chgt coord.)
      MARGE_ST = BMUL0*DD
C--------suppose PEN_ini< DD_ST/2 (half thickness of solids )   
      IF (INACTI /=0 ) THEN
C-------input PENMAX will fix the PENMAX value----- 
       IF (PENMAX /= ZERO) THEN
        MARGE_ST  = MAX(MARGE_ST,PENMAX)
#ifndef HYPERMESH_LIB
        IF (IDDLEVEL == 1 ) WRITE(IOUT,2400) PENMAX
#endif
       ELSE
        IF (N_SOL>0) THEN
         D_MOY  = D_MOY/N_SOL
         DD_ST = D_MOY
        END IF 
        PENSOL  = MIN(UNDEMI*DD_ST,PENSOL)
        MARGE_ST  = MAX(MARGE_ST,PENSOL)
C--------should also think about shells---- 
        PENSOL = MAX(PENSOL,UNDEMI*GAP)
        PENMAX = PENSOL
#ifndef HYPERMESH_LIB
        IF (IDDLEVEL == 1 ) WRITE(IOUT,2500) PENMAX
#endif
       END IF!(PENMAX/=ZERO)
      ELSE
C-----il faut pas eliminer tiny penetration----      
       PENMAX = MAX(PENSOL,GAP)
      END IF
C 1er passage avec marge x2 pour trouver plus de candidats
C      IF(IMACH.EQ.3.AND.IDDLEVEL.EQ.0) MARGE_ST = 2*MARGE_ST
      IF(IMACH.EQ.3.AND.IDDLEVEL.EQ.0) MARGE_ST = MARGE
      TZINF_ST = MARGE_ST + GAP
      BID = QCINQ*DD
      IF (INACTI.NE.7.AND.TZINF.GT.BID) THEN
        IBID = NINT(TZINF/DD0)
        IBID =(2*IBID+4)*IBID*2
      ENDIF
C      MAXBOX= ZEP9*(DD - TZINF)
C DD + 2*TZINF : taille element augmentee de zone influence
      MAXBOX= UNDEMI*(DD + 2*TZINF)
      MINBOX= UNDEMI*MAXBOX
      TZINF0 = TZINF
      MINBOX0 = MINBOX
      MAXBOX0 = MAXBOX
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
      DO 25 I=1,NSN0
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
C  POUR UN MAILLAGE DE TOPOLOGIE CARRE LA TAILLE DE P2 PEUT ETRE ESTIMEE A:
C 4n:     NUMELC + 6*SQRT(NUMELC) + 8*LOG2(NUMELC) 
C 3n:     NUMELTG + 6*SQRT(2*NUMELTG) + 8*LOG2(NUMELTG)
C 
C 4n:     NUMELC + 6*SQRT(NUMELC) + 12*LOG(NUMELC) +
C 3n:     NUMELTG + 6*SQRT(2*NUMELTG) + 12*LOG(NUMELTG)
C 
C NUMELC + NUMELTG + 6*SQRT(NUMELC+2*NUMELTG) + 12*LOG(NUMELC+NUMELTG)
C 
C  POUR UN MAILLAGE DE TOPOLOGIE LINEAIRE LA TAILLE DE P2 PEUT ETRE ESTIMEE A:
C NUMELC + NUMELTG + (NUMELC+NUMELTG)*(1 + 1/2 + 1/4 +...) + LOG2(NUMELC+NUMELTG)
C 3*(NUMELC+NUMELTG) + LOG2(NUMELC+NUMELTG)
C 3*(NUMELC+NUMELTG) + 300
C
C     IFIN= IP22+2000
C
C-----INITIALISATION DES ADRESSES ET X,Y,Z
C
C     ADDE     ADDN     X      Y      Z
C     1        1        XMIN   YMIN   ZMIN
C     1        1        XMAX   YMAX   ZMAX
C
      IF (NSNE >0) THEN
        IWORK(IP31) = 0
        IWORK(IP31+1) = 0
        IWORK(IP31+2) = 0
        IWORK(IP31+3) = 0
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
        DO I=1,NB_EC
         IWORK(IP1+I-1) = I
        END DO
        DO I=1,NB_NC
         IWORK(IP21+I-1) = I
        END DO
C
      ELSE
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
      END IF !(NSNE >0) THEN
C-----DEBUT DE LA PHASE DE TRI 
C 
C     TANT QUE IL RESTE UNE ADRESSE A TRIER
C------------------
  200 CONTINUE
C------------------
C       SEPARER B ET N EN DEUX
       IF (NSNE >0) THEN
        CALL I24TRI(
     1   IWORK(IP1),IWORK(IP2),IWORK(IP21),IWORK(IP22),
     +   IWORK(IP31+2*(I_ADD-2)),
     2   IRECT   ,XTEN    ,NB_NC    ,NB_EC    ,XYZM     ,
     3   I_ADD   ,NSV     ,I_AMAX   ,XMAX     ,YMAX     ,
     4   ZMAX    ,3*MAXSIZ,I_STOK   ,I_MEM    ,NB_N_B   , 
     5   CAND_N  ,CAND_E  ,NSN      ,NOINT    ,TZINF_ST ,
     6   MAXBOX  ,MINBOX  ,STF      ,STFN     ,J_STOK   ,
     7   MULTIMP ,ISTF    , ITAB    ,GAP      ,GAP_S    ,
     8   GAP_M   ,IGAP    ,GAPMIN   ,GAPMAX   ,MARGE_ST ,
     9   GAP_S_L,GAP_M_L  ,ID       ,TITR     ,ILEV     ,
     A   NBINFLG,MBINFLG  ,MVOISN   ,IXS      ,IXS10    ,
     B   IXS16  ,IXS20    ,IPARTNS  ,IPEN0    ,INACTI   ,
     C   MSEGTYP,MARGE    ,NRTM ,IRTSE ,IS2SE    ,
     D   IX1    ,IX2    ,IX3    ,IX4    ,NSVG   ,
     E   X1     ,X2     ,X3     ,X4     ,Y1     ,
     F   Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     G   Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     H   X0     ,Y0     ,Z0     ,STIF   ,NX1    ,
     I   NY1    ,NZ1    ,NX2    ,NY2    ,NZ2    ,
     J   NX3    ,NY3    ,NZ3    ,NX4    ,NY4    ,
     K   NZ4    ,P1     ,P2     ,P3     ,P4     ,
     L   LB1    ,LB2    ,LB3    ,LB4    ,LC1    ,
     M   LC2    ,LC3    ,LC4    ,PENE   ,PROV_N ,
     N   PROV_E ,N11    ,N21    ,N31)

       ELSE
        CALL I24TRI(
     1   MWA(IP1),MWA(IP2),MWA(IP21),MWA(IP22),MWA(IP31+2*(I_ADD-2)),
     2   IRECT   ,X       ,NB_NC    ,NB_EC    ,XYZM     ,
     3   I_ADD   ,NSV     ,I_AMAX   ,XMAX     ,YMAX     ,
     4   ZMAX    ,3*MAXSIZ,I_STOK   ,I_MEM    ,NB_N_B   , 
     5   CAND_N  ,CAND_E  ,NSN      ,NOINT    ,TZINF_ST ,
     6   MAXBOX  ,MINBOX  ,STF      ,STFN     ,J_STOK   ,
     7   MULTIMP ,ISTF    , ITAB    ,GAP      ,GAP_S    ,
     8   GAP_M   ,IGAP    ,GAPMIN   ,GAPMAX   ,MARGE_ST ,
     9   GAP_S_L,GAP_M_L  ,ID       ,TITR     ,ILEV     ,
     A   NBINFLG,MBINFLG  ,MVOISN   ,IXS      ,IXS10    ,
     B   IXS16  ,IXS20    ,IPARTNS  ,IPEN0    ,INACTI   ,
     C   MSEGTYP,MARGE    ,NRTM ,IRTSE ,IS2SE   ,         
     D   IX1    ,IX2    ,IX3    ,IX4    ,NSVG   ,
     E   X1     ,X2     ,X3     ,X4     ,Y1     ,
     F   Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     G   Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     H   X0     ,Y0     ,Z0     ,STIF   ,NX1    ,
     I   NY1    ,NZ1    ,NX2    ,NY2    ,NZ2    ,
     J   NX3    ,NY3    ,NZ3    ,NX4    ,NY4    ,
     K   NZ4    ,P1     ,P2     ,P3     ,P4     ,
     L   LB1    ,LB2    ,LB3    ,LB4    ,LC1    ,
     M   LC2    ,LC3    ,LC4    ,PENE   ,PROV_N ,
     N   PROV_E ,N11    ,N21    ,N31)
       END IF !(NSNE >0) THEN
C------------------
      IF (I_MEM == 2)THEN
        IF (NSNE >0) DEALLOCATE(XTEN,IWORK)
        RETURN
      ENDIF
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
      IF(I_MEM.EQ.1)THEN
        NB_N_B = NB_N_B + 1
        I_MEM = 0
        GO TO 100
      ENDIF
      IF(I_ADD.NE.0) GO TO 200
C     FIN BOUCLE TANT QUE
C---------------------------------
      IF(J_STOK.NE.0)THEN
         LFT = 1
         LLT = J_STOK
        IF (NSNE >0) THEN
         CALL I7COR3(XTEN ,IRECT,NSV  ,PROV_E ,PROV_N,
     .               STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .               GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .               GAP_S_L,GAP_M_L  ,ZERO   ,IX1     ,IX2   ,
     5               IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6               X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7               Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8               XI     ,YI     ,ZI  ,STIF)
        ELSE
         CALL I7COR3(X    ,IRECT,NSV  ,PROV_E ,PROV_N,
     .               STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .               GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .               GAP_S_L,GAP_M_L  ,ZERO   ,IX1     ,IX2   ,
     5               IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6               X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7               Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8               XI     ,YI     ,ZI  ,STIF)
        END IF !(NSNE >0) THEN
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

         IF (ILEV==2) CALL I24S1S2(PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
         IF(I_STOK+J_STOK.LT.MULTIMP*NSN) THEN
          CALL I7CMP3(I_STOK,CAND_E  ,CAND_N,1,PENE,
     1                  PROV_N,PROV_E)
         ELSE
          I_BID = 0
          CALL I7CMP3(I_BID,CAND_E,CAND_N,0,PENE,
     1                  PROV_N,PROV_E)
          IF(I_STOK+I_BID.LT.MULTIMP*NSN) THEN
            CALL I7CMP3(I_STOK,CAND_E,CAND_N,1,PENE,
     1                  PROV_N,PROV_E)
          ELSE
            I_MEM = 2
            IF (NSNE >0) DEALLOCATE(XTEN,IWORK)
            RETURN
          ENDIF
        ENDIF
      ENDIF
C

#ifndef HYPERMESH_LIB
      IF(NSN.NE.0)THEN
       WRITE(IOUT,*)' POSSIBLE IMPACT NUMBER, NSN:',I_STOK,NSN
C
      ELSE
       CALL ANCMSG(MSGID=552,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=ID,
     .             C1=TITR)
      ENDIF
#endif

C
C     MISE A ZERO DE TAG POUR I24PEN3
C
      DO I=1,NUMNOD+NSNE
        MWA(I)=0
      ENDDO

#ifndef HYPERMESH_LIB
 2400 FORMAT(2X,/,'USER-DEFINED(IPEN_MAX)SEARCHING DISTANCE FOR INITIAL PENETRATIONS ',
     +            1PG20.13,'IS USED',/)
 2500 FORMAT(2X,/,'COMPUTED SEARCHING DISTANCE FOR INITIAL PENETRATIONS ',1PG20.13,
     +            'IS USED',/)
#endif                    

C
      IF (NSNE >0) DEALLOCATE(XTEN,IWORK)
      RETURN
      END
Chd|====================================================================
Chd|  I24S1S2                       source/interfaces/inter3d1/i24buc1.F
Chd|-- called by -----------
Chd|        I24BUC1                       source/interfaces/inter3d1/i24buc1.F
Chd|        I24TRI                        source/interfaces/inter3d1/i24tri.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|====================================================================
      SUBROUTINE I24S1S2(PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROV_N(*),PROV_E(*),NBINFLG(*),MBINFLG(*)
C     REAL ou REAL*8
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NE,IMS1,IMS2,ISS1,ISS2
C     REAL ou REAL*8
C-----------------------------------------------
C
!      EQUIVALENCE (PENE,BV(1,22))
      INTEGER BITGET
      EXTERNAL BITGET
C=======================================================================
         DO I=LFT,LLT
           N  = PROV_N(I)
           NE = PROV_E(I)
           IMS1 = BITGET(MBINFLG(NE),0)
           IMS2 = BITGET(MBINFLG(NE),1)
           ISS1 = BITGET(NBINFLG(N),0)
           ISS2 = BITGET(NBINFLG(N),1)
           IF((IMS1 == 1 .and. ISS1==1).or.
     .        (IMS2 == 1 .and. ISS2==1))THEN
             PENE(I)=ZERO
           ENDIF
         ENDDO
C
      RETURN
      END
