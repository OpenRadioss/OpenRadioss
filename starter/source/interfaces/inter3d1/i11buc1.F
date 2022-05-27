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
Chd|  I11BUC_VOX1                   source/interfaces/inter3d1/i11buc1.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I11TRIVOX1                    source/interfaces/inter3d1/i11trivox1.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI11                         share/modules1/tri11_mod.F    
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
       SUBROUTINE I11BUC_VOX1(
     1   X      ,IRECTS  ,IRECTM ,NRTS    ,NMN     ,
     2   NRTM   ,NSN     ,CAND_M ,CAND_S  ,MAXGAP  ,
     3   NOINT  ,II_STOK ,TZINF  ,MAXBOX  ,MINBOX  ,
     4   NCONTACT, MULTIMP, MSR,
     5   ADDCM  ,CHAINE  ,ITAB, NSV    ,
     6   IAUTO  , I_MEM  ,ID,TITR,IDDLEVEL,BUMULT ,
     7   DRAD,INTERCEP   ,IGAP   ,GAP_S   , GAP_M ,
     8   GAP_S_L,GAP_M_L ,GAPMIN ,FLAGREMNODE,KREMNODE,
     9   REMNODE,DGAPLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TRI7BOX 
      USE TRI11   
      USE FRONT_MOD

C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,IDT,NRTS,IDDLEVEL,
     .        IAUTO, I_MEM, IGAP
      INTEGER IRECTS(2,*),IRECTM(2,*),ADDCM(*),CHAINE(2,*)
      INTEGER CAND_M(*),CAND_S(*)
      INTEGER NCONTACT,  ITAB(*),MSR(*),NSV(*),
     .        II_STOK,MULTIMP,FLAGREMNODE,KREMNODE(*),REMNODE(*)
C     REAL
      my_real
     .   BUMULT,MAXGAP,GAPMIN,TZINF,MAXBOX,MINBOX,DRAD
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .   X(3,*), 
     .   GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)

C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
C
      INTEGER I, J, N1, N2, I_ADD, MAXSIZ,JJ,
     .        ADD(2,I_ADD_MAX), N
      my_real
     .        XYZM(6,I_ADD_MAX-1), MARGE, AAA
      INTEGER NB_OLD(2,I_ADD_MAX+1)
      INTEGER NBX,NBY,NBZ
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8 
      INTEGER NB_N_B
      INTEGER IADFIN,II,L
      my_real :: XMAX,YMAX,ZMAX
      my_real :: XMIN,YMIN,ZMIN,XTMP
      my_real :: BMINMA(6)
      my_real :: DD,DX1,DY1,DZ1,DD1,MARGE_ST,TZINF_ST


C-----------------------------------------------
C   definition from TRI7BOX module
C-----------------------------------------------      
C              
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------------
C
C----- TRI PAR BOITES 
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NRTM
C     P2........Elt PILE            3*NRTM
C     P21.......Elt Bas Pile        NRTS
C     P22.......Elt PILE            3*NRTS
      MAXSIZ = 3*(MAX(NRTM,NRTS)+100)
C
C-----INITIALISATION DES ADRESSES ET X,Y,Z
C
C     ADDE     ADDN     X      Y      Z
C     1        1        XMIN   YMIN   ZMIN
C     1        1        XMAX   YMAX   ZMAX
C
      ADD(1,1) = 0
      ADD(2,1) = 0
      ADD(1,2) = 0
      ADD(2,2) = 0
      I_ADD = 1
      IADFIN = 0
      NB_N_B = 1

C
C
C     CALCULER LES BMINMA ICI
C
C

      BMINMA(1)=-EP30
      BMINMA(2)=-EP30
      BMINMA(3)=-EP30
      BMINMA(4)=EP30
      BMINMA(5)=EP30
      BMINMA(6)=EP30



      I_MEM = 0
      II_STOK = 0
     
C
c     IF (IFORM .NE. 2) THEN
        DO I=1,NRTM
          ADDCM(I)=0
        ENDDO 
c     ELSE
c     ENDIF    
 
C-----DEBUT DE LA PHASE DE TRI 

      DD=ZERO
      DO L=1,NRTS
C      CONNECIVITES ELEMENT
       N1=IRECTS(1,L)
       N2=IRECTS(2,L)
C      LONGUEUR COTE 1
       DX1=(X(1,N1)-X(1,N2))
       DY1=(X(2,N1)-X(2,N2))
       DZ1=(X(3,N1)-X(3,N2))
       DD1=SQRT(DX1**2+DY1**2+DZ1**2)
       DD=DD+ DD1
      ENDDO
      DO L=1,NRTM
C      CONNECIVITES ELEMENT
       N1=IRECTM(1,L)
       N2=IRECTM(2,L)
C      LONGUEUR COTE 1
       DX1=(X(1,N1)-X(1,N2))
       DY1=(X(2,N1)-X(2,N2))
       DZ1=(X(3,N1)-X(3,N2))
       DD1=SQRT(DX1**2+DY1**2+DZ1**2)
       DD=DD+ DD1
      ENDDO
C     TAILLE ZINF = .1*TAILLE MOYENNE ELEMENT DE CHAQUE COTE
C     TAILLE BUCKET MIN = TZINF  * BUMULT
C     DD = MAX(DD/(NRTS+NRTM),1.251*MAXGAP)
      DD = DD/(NRTS+NRTM)
C      TZINF = BUMULT*DD
      MARGE = BUMULT*DD
      TZINF = MARGE + MAX(MAXGAP+DGAPLOAD,DRAD)
C MARGE_ST : marge independante du BUMULT en input pour trouver les memes pene initiales
      MARGE_ST = BMUL0*DD
C 1er passage avec marge x2 pour trouver plus de candidats (revient a prendre bmul0=0.4 au lieu de 0.2)
C      IF(IDDLEVEL.EQ.0) MARGE_ST = 2*MARGE_ST
c     WRITE(6,*) __FILE__,__LINE__,MARGE
      IF(IDDLEVEL.EQ.0) MARGE_ST = MARGE
      TZINF_ST = MARGE_ST + MAX(MAXGAP+DGAPLOAD,DRAD)

      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30
C
      DO I=1,NMN
         J=MSR(I)
        IF(J.GT.0) THEN 
          XMIN= MIN(XMIN,X(1,J))
          YMIN= MIN(YMIN,X(2,J))
          ZMIN= MIN(ZMIN,X(3,J))
          XMAX= MAX(XMAX,X(1,J))
          YMAX= MAX(YMAX,X(2,J))
          ZMAX= MAX(ZMAX,X(3,J))
        END IF
      END DO

      XMIN=XMIN-TZINF_ST
      YMIN=YMIN-TZINF_ST
      ZMIN=ZMIN-TZINF_ST
      XMAX=XMAX+TZINF_ST
      YMAX=YMAX+TZINF_ST
      ZMAX=ZMAX+TZINF_ST
C      DO 25 I=1,NSN
C       J=NSV(I)
C       XMIN= MIN(XMIN,X(1,J))
C       YMIN= MIN(YMIN,X(2,J))
C       ZMIN= MIN(ZMIN,X(3,J))
C       XMAX= MAX(XMAX,X(1,J))
C       YMAX= MAX(YMAX,X(2,J))
C       ZMAX= MAX(ZMAX,X(3,J))
C 25   CONTINUE
C      XMIN=XMIN-TZINF_ST
C      YMIN=YMIN-TZINF_ST
C      ZMIN=ZMIN-TZINF_ST
C      XMAX=XMAX+TZINF_ST
C      YMAX=YMAX+TZINF_ST
C      ZMAX=ZMAX+TZINF_ST

      BMINMA(1) = MAX(BMINMA(1),XMAX)
      BMINMA(2) = MAX(BMINMA(2),YMAX)
      BMINMA(3) = MAX(BMINMA(3),ZMAX)
      BMINMA(4) = MIN(BMINMA(4),XMIN)
      BMINMA(5) = MIN(BMINMA(5),YMIN)
      BMINMA(6) = MIN(BMINMA(6),ZMIN)

c      WRITE(6,*) "X",XMIN,XMAX
c      WRITE(6,*) "Y",YMIN,YMAX
c      WRITE(6,*) "Z",ZMIN,ZMAX 


      XYZM(1,I_ADD) = BMINMA(4)
      XYZM(2,I_ADD) = BMINMA(5)
      XYZM(3,I_ADD) = BMINMA(6)
      XYZM(4,I_ADD) = BMINMA(1)
      XYZM(5,I_ADD) = BMINMA(2)
      XYZM(6,I_ADD) = BMINMA(3)
C 
   
c     MARGE = TZINF - MAX(MAXGAP,DRAD)

      AAA = SQRT(1.0D0* NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)

      NBX8=NBX
      NBY8=NBY
      NBZ8=NBZ
      RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      LVOXEL8 = LVOXEL      

      IF(RES8 > LVOXEL8)THEN
        AAA = LVOXEL
        AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
        AAA = AAA**(THIRD)
        NBX = INT((NBX+2)*AAA)-2
        NBY = INT((NBY+2)*AAA)-2
        NBZ = INT((NBZ+2)*AAA)-2
        NBX = MAX(NBX,1)
        NBY = MAX(NBY,1)
        NBZ = MAX(NBZ,1)
        NBX8 = NBX
        NBY8 = NBY
        NBZ8 = NBZ
        RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
      END IF
          
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(100,MAX(NBX8,1))
        NBY = MIN(100,MAX(NBY8,1))
        NBZ = MIN(100,MAX(NBZ8,1))
      END IF
      
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
      
      !print *, "voxel search"
      
        CALL I11TRIVOX1(
     1   IRECTS  ,IRECTM  ,X       ,NRTM    ,
     2   XYZM    ,II_STOK ,CAND_S  ,CAND_M  , NSN,
     3   NOINT   ,TZINF_ST,I_MEM   ,ADDCM   , IADFIN,
     4   CHAINE  ,NRTS    ,ITAB    ,MULTIMP,
     5   IAUTO   ,VOXEL1  ,NBX     ,NBY     ,NBZ     ,
     7   GAPMIN  ,DRAD    ,MARGE_ST,GAP_S   ,GAP_M   ,
     8   GAP_S_L ,GAP_M_L ,IGAP    ,FLAGREMNODE,KREMNODE,
     3   REMNODE ,DGAPLOAD)


C
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS

      IF (I_MEM == 2) RETURN
      IF(I_MEM ==1 .OR. I_MEM == 3)THEN
        NB_N_B = NB_N_B + 1
        IF ( NB_N_B .GT. MAX(NRTM,NRTS)) THEN
          CALL ANCMSG(MSGID=83,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO ,
     .                I1=ID,
     .                C1=TITR)

        ENDIF
      ENDIF

      IF ((NSN.NE.0)) THEN
      WRITE(IOUT,*)' POSSIBLE IMPACT NUMBER:',II_STOK,' (<=',
     . 1+(II_STOK-1)/NSN,'*NSN)'
      ELSEIF(NSN.EQ.0) THEN
       CALL ANCMSG(MSGID=552,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=ID,
     .             C1=TITR)
      ENDIF

C      DO I = 1, II_STOK
C        WRITE(800+IDDLEVEL,*) NOINT,ITAB(IRECTS(1,CAND_S(I)))
C     . ,ITAB(IRECTS(2,CAND_S(I))),ITAB(IRECTM(1,CAND_M(I))),ITAB(IRECTM(2,CAND_M(I)))
C       ENDDO
C       CALL FLUSH(800+IDDLEVEL)

C
      RETURN
      END





Chd|====================================================================
Chd|  I11BUC1                       source/interfaces/inter3d1/i11buc1.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I11STO                        source/interfaces/inter3d1/i11sto.F
Chd|        I11TRI                        source/interfaces/inter3d1/i11tri.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I11BUC1(
     1   X     ,IRECTM,IRECTS,BUMULT,NRTS,
     2   NMN   ,NRTM ,MWA   ,NSN   ,CAND_M,
     3   CAND_S,GAP  ,XYZM  ,NOINT ,I_STOK,
     4   DIST  ,TZINF,MAXBOX,MINBOX,MSR   ,   
     5   NSV   ,MULTIMP,ADDCM,CHAINE,I_MEM,
     6   ID,TITR,IDDLEVEL,DRAD,IT19)
      USE MESSAGE_MOD
C============================================================================
C  cette routine est appelee par : ININT3(/inter3d1/inint3.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I11TRI(/inter3d1/i11tri.F)
C                          I11STO(/inter3d1/i1chk3.F)
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
#include      "vect07_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, NOINT,I_STOK,NRTS,MULTIMP,MAXSIZ,I_MEM
      INTEGER IRECTS(2,*),IRECTM(2,*),MWA(*)
      INTEGER CAND_M(*),CAND_S(*),MSR(*),NSV(*),ADDCM(*),CHAINE(2,*),
     *        II_STOK, IDDLEVEL, IT19
C     REAL
      my_real
     .   X(3,*),XYZM(6,*),DIST,
     .   BUMULT,GAP,TZINF,MAXBOX,MINBOX,DRAD
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_ADD_MAX
      PARAMETER (I_ADD_MAX = 1001)
C
      INTEGER PROV_S(2*MVSIZ),PROV_M(2*MVSIZ)
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX,IADFIN
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK,ISTOP, IBID 
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B,
     .        ADD(2,0:I_ADD_MAX)
C     REAL
      my_real
     .   DX1,DY1,DZ1,
     .   DX3,DY3,DZ3,
     .   DX4,DY4,DZ4,
     .   DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,XMIN,YMIN,ZMIN,
     .   XMAX,YMAX,ZMAX,TZINF0,MINBOX0,MAXBOX0,
     .   BID,MARGE,TZINF_ST,MARGE_ST
C
C
C
C     1-CALCUL TAILLE DES ZONES INFLUENCES
C     DD EST LA LONGEUR MOYENNE ELEMENT
      DD=ZERO
      DO L=1,NRTS
C      CONNECIVITES ELEMENT
       N1=IRECTS(1,L)
       N2=IRECTS(2,L)
C      LONGUEUR COTE 1
       DX1=(X(1,N1)-X(1,N2))
       DY1=(X(2,N1)-X(2,N2))
       DZ1=(X(3,N1)-X(3,N2))
       DD1=SQRT(DX1**2+DY1**2+DZ1**2)
       DD=DD+ DD1
      ENDDO
      DO L=1,NRTM
C      CONNECIVITES ELEMENT
       N1=IRECTM(1,L)
       N2=IRECTM(2,L)
C      LONGUEUR COTE 1
       DX1=(X(1,N1)-X(1,N2))
       DY1=(X(2,N1)-X(2,N2))
       DZ1=(X(3,N1)-X(3,N2))
       DD1=SQRT(DX1**2+DY1**2+DZ1**2)
       DD=DD+ DD1
      ENDDO
C     TAILLE ZINF = .1*TAILLE MOYENNE ELEMENT DE CHAQUE COTE
C     TAILLE BUCKET MIN = TZINF  * BUMULT
C     DD = MAX(DD/(NRTS+NRTM),1.251*GAP)
      DD = DD/(NRTS+NRTM)
C      TZINF = BUMULT*DD
      MARGE = BUMULT*DD
      TZINF = MARGE + MAX(GAP,DRAD)
C MARGE_ST : marge independante du BUMULT en input pour trouver les memes pene initiales
      MARGE_ST = BMUL0*DD
C 1er passage avec marge x2 pour trouver plus de candidats (revient a prendre bmul0=0.4 au lieu de 0.2)
C      IF(IDDLEVEL.EQ.0) MARGE_ST = 2*MARGE_ST
c     WRITE(6,*) __FILE__,__LINE__,"IDDLEVEL=",IDDLEVEL
c     WRITE(6,*) __FILE__,__LINE__,MARGE
c     WRITE(6,*) __FILE__,__LINE__,MINBOX,MAXBOX,MULTIMP
c     WRITE(6,*) __FILE__,__LINE__,DRAD,GAP,BMUL0



      IF(IDDLEVEL.EQ.0) MARGE_ST = MARGE
      TZINF_ST = MARGE_ST + MAX(GAP,DRAD)
C
      MINBOX= DD + TZINF
      MAXBOX= TWO*MINBOX
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
c     DO 25 I=1,NSN
c      J=NSV(I)
c      XMIN= MIN(XMIN,X(1,J))
c      YMIN= MIN(YMIN,X(2,J))
c      ZMIN= MIN(ZMIN,X(3,J))
c      XMAX= MAX(XMAX,X(1,J))
c      YMAX= MAX(YMAX,X(2,J))
c      ZMAX= MAX(ZMAX,X(3,J))
c25   CONTINUE
c     XMIN=XMIN-TZINF_ST
c     YMIN=YMIN-TZINF_ST
c     ZMIN=ZMIN-TZINF_ST
c     XMAX=XMAX+TZINF_ST
c     YMAX=YMAX+TZINF_ST
c     ZMAX=ZMAX+TZINF_ST

c     WRITE(6,*) "X",XMIN,XMAX
c     WRITE(6,*) "Y",YMIN,YMAX
c     WRITE(6,*) "Z",ZMIN,ZMAX

C
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
      NB_N_B = 1
      I_MEM = 0
      II_STOK = 0
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
 100  CONTINUE
C     POINTEUR  NOM                 TAILLE
C     P1........Elt Bas Pile        NRTM
C     P2........Elt PILE            3*NRTM
C     P21.......Elt Bas Pile        NRTS
C     P22.......Elt PILE            3*NRTS
C
C
      MAXSIZ = 3*(MAX(NRTM,NRTS)+100)
      IP1 = 1
      IP2 = IP1+NRTM+100
      IP21= IP2+MAXSIZ
      IP22= IP21+NRTS+100

C-----INITIALISATION DES ADRESSES ET X,Y,Z
C
C     ADDE     ADDN     X      Y      Z
C     1        1        XMIN   YMIN   ZMIN
C     1        1        XMAX   YMAX   ZMAX
C
      ADD(1,1) = 0
      ADD(2,1) = 0
      ADD(1,2) = 0
      ADD(2,2) = 0
      I_ADD = 1
      I_AMAX = 1
      XYZM(1,I_ADD) = XMIN
      XYZM(2,I_ADD) = YMIN
      XYZM(3,I_ADD) = ZMIN
      XYZM(4,I_ADD) = XMAX
      XYZM(5,I_ADD) = YMAX
      XYZM(6,I_ADD) = ZMAX
      I_STOK = 0
      II_STOK = 0
      J_STOK = 0
      ADNSTK = 0
      ADESTK = 0
      NB_NC = NRTS
      NB_EC = NRTM
      ISTOP = 0
      IADFIN = 0
C
C-----COPIE DES NOS DE SEGMENTS ET DE NOEUDS DANS MWA(IP1) ET IP21
C
      DO 120 I=1,NB_EC
        ADDCM(I)=0
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
c       WRITE(6,*) __FILE__,__LINE__
C------------------
C       SEPARER B ET N EN TWO
        CALL I11TRI(
     1   MWA(IP1),MWA(IP2),MWA(IP21),MWA(IP22),ADD,
     2   IRECTS  ,X       ,NB_NC    ,NB_EC    ,XYZM,
     3   I_ADD   ,IRECTM  ,I_AMAX   ,ISTOP    ,
     4   MAXSIZ  ,I_STOK  ,I_MEM    ,NB_N_B   ,IADFIN, 
     5   CAND_S  ,CAND_M  ,NSN      ,NOINT    ,TZINF_ST,
     6   MAXBOX  ,MINBOX  ,J_STOK   ,ADDCM    ,CHAINE,
     7   PROV_S  ,PROV_M  ,II_STOK  ,MULTIMP,ID,TITR)
C------------------
      IF (I_MEM == 2) RETURN
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
C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
      I_STOK=II_STOK
      IF(J_STOK.NE.0)CALL I11STO(
     1              J_STOK,IRECTS,IRECTM,X     ,II_STOK,
     2              CAND_S,CAND_M,NSN   ,NOINT ,TZINF_ST,
     3              I_MEM ,PROV_S,PROV_M,MULTIMP,ADDCM,
     4              CHAINE,IADFIN)
      IF (I_MEM == 2) RETURN

      I_STOK=II_STOK
      IF ((NSN.NE.0).AND.(IT19.EQ.0)) THEN
      WRITE(IOUT,*)' POSSIBLE IMPACT NUMBER:',I_STOK,' (<=',
     . 1+(I_STOK-1)/NSN,'*NSN)'
c     DO I = 1, I_STOK
c       WRITE(700+IDDLEVEL,*) CAND_S(I),CAND_M(I)
c     ENDDO
c     CALL FLUSH(700+IDDLEVEL)

      ELSEIF(NSN.EQ.0) THEN
       CALL ANCMSG(MSGID=552,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=ID,
     .             C1=TITR)
      ENDIF
C
      RETURN
      END
