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
Chd|  I7TRIVOX1                     source/interfaces/inter3d1/i7trivox1.F
Chd|-- called by -----------
Chd|        I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|-- calls ---------------
Chd|        I7CMP3                        source/interfaces/inter3d1/i7cmp3.F
Chd|        I7COR3                        source/interfaces/inter3d1/i7cor3.F
Chd|        I7DST3                        source/interfaces/inter3d1/i7dst3.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I7TRIVOX1(
     1      NSN    ,I_MEM   ,IRECT    ,X       ,STF     ,
     2      STFN   ,XYZM    ,NSV      ,CAND_N  ,CAND_E  ,
     3      MULNSN ,NOINT   ,TZINF    ,GAP_S_L ,GAP_M_L ,
     4      VOXEL  ,NBX     ,NBY      ,NBZ     ,NRTM    ,
     5      IGAP   ,GAP     ,GAP_S    ,GAP_M   ,GAPMIN  ,
     6      GAPMAX ,MARGE   ,CURV_MAX ,BGAPSMX ,ISTF    ,
     7      I_STOK ,MULTIMP ,J_STOK   ,PROV_N  ,PROV_E  ,
     8      ID     ,TITR    ,DRAD     ,INDEX   ,
     1      IX11  ,IX12    ,IX13  ,IX14  ,NSVG ,
     2      X1    ,X2      ,X3    ,X4    ,Y1    ,
     3      Y2    ,Y3      ,Y4    ,Z1    ,Z2    ,
     4      Z3    ,Z4      ,XI    ,YI    ,ZI    ,
     5      X0    ,Y0      ,Z0    ,NX1   ,NY1   ,
     6      NZ1   ,NX2     ,NY2   ,NZ2   ,NX3   ,
     7      NY3   ,NZ3     ,NX4   ,NY4   ,NZ4   ,
     8      P1    ,P2      ,P3    ,P4    ,LB1   ,
     9      LB2   ,LB3     ,LB4   ,LC1   ,LC2   ,
     1      LC3   ,LC4     ,N11   ,N21   ,N31   ,
     2      STIF  ,PENE    ,IREMNODE,FLAGREMNODE,
     3      KREMNODE,REMNODE)
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "sms_c.inc"
#include      "vect07_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   CLASSE LES NOEUDS DANS DES BOITES 
C   RECHERCHE POUR CHAQUE FACETTE DES BOITES CONCERNES
C   RECHERCHE DES CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     IRECT(4,*)   TABLEAU DES CONEC FACETTES        E
C     X(3,*)       COORDONNEES NODALES               E
C     NSV          NOS SYSTEMES DES NOEUDS           E
C     XMAX         plus grande abcisse existante     E
C     XMAX         plus grande ordonn. existante     E
C     XMAX         plus grande cote    existante     E
C     I_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     CAND_N       boites resultats noeuds
C     CAND_E       adresses des boites resultat elements
C                  MULNSN = MULTIMP*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C
C     PROV_N       CAND_N provisoire (variable static dans i7tri)
C     PROV_E       CAND_E provisoire (variable static dans i7tri)

C     VOXEL(ix,iy,iz) contient le numero local du premier noeud de 
C                  la boite 
C     NEXT_NOD(i)  noeud suivant dans la meme boite (si /= 0)
C     LAST_NOD(i)  dernier noeud dans la meme boite (si /= 0) 
C                  utilise uniquement pour aller directement du premier
C                       noeud au dernier
C     INDEX        index of IRECTM active for this domain
C     NRTM         number of active segments for this domain
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,NSN,NRTM,
     .        MULNSN,NOINT,IGAP,NBX,NBY,NBZ,IREMNODE,FLAGREMNODE,
     .        NSV(*),CAND_N(*),CAND_E(*),
     .        IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),ISTF,
     .        I_STOK ,MULTIMP,PROV_N(*),PROV_E(*),J_STOK,
     .        INDEX(*),KREMNODE(*),REMNODE(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),XYZM(6,2),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   TZINF,MARGE,GAP,GAPMIN,GAPMAX,BGAPSMX,DRAD,
     .   CURV_MAX(*),GAP_S_L(*),GAP_M_L(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX11,IX12,IX13,IX14,NSVG
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
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LC1,LC2,LC3,LC4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N11,N21,N31
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STIF,PENE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,NCAND_PROV,II,JJ,KK,
     .        NSNF, NSNL, I_BID,DELNOD,TAG(NUMNOD)
C     REAL ou REAL*8
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS,GAPV(MVSIZ)
      INTEGER  LAST_NOD(NSN)
c     .  maxvox, meanvox, ttvox, maxjj, ttjj, numjj, jj1, jj2, jj3,iii,
c     .  nbbox1,nbin1,nbin2,nbin3
      INTEGER*8 meanjj8     
      INTEGER  IX,IY,IZ,NEXT,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA,TSTART,TSTOP
c      my_ream
c     .   maxaaa
      INTEGER FIRST,NEW,LAST,M
      SAVE IIX,IIY,IIZ
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGNOD
C-----------------------------------------------

      ALLOCATE(NEXT_NOD(NSN))
      ALLOCATE(IIX(NSN))
      ALLOCATE(IIY(NSN))
      ALLOCATE(IIZ(NSN))

C
      XMIN = XYZM(1,1)  
      YMIN = XYZM(2,1)
      ZMIN = XYZM(3,1)
      XMAX = XYZM(4,1)
      YMAX = XYZM(5,1)
      ZMAX = XYZM(6,1)

c     dev future: xminb plus grand que xmin...
      XMINB = XYZM(1,2)  
      YMINB = XYZM(2,2)
      ZMINB = XYZM(3,2)
      XMAXB = XYZM(4,2)
      YMAXB = XYZM(5,2)
      ZMAXB = XYZM(6,2)
      
c      XMINB = XMIN
c      YMINB = YMIN
c      ZMINB = ZMIN
c      XMAXB = XMAX
c      YMAXB = YMAX
c      ZMAXB = ZMAX

c      print *,NOINT,'before loop:',XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB
c      print *,NOINT,'box big:',XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C================================================================
C 1   mise des noeuds dans les boites
C================================================================

      DO I=1,NSN
        IIX(I)=0
        IIY(I)=0
        IIZ(I)=0
        IF(STFN(I) == ZERO)CYCLE
        J=NSV(I)
        
C First test vs CRVOXEL
        IX=INT(LRVOXEL*(X(1,J)-XMIN)/(XMAX-XMIN))
        IF(IX < 0 .OR. IX > LRVOXEL) CYCLE
        IY=INT(LRVOXEL*(X(2,J)-YMIN)/(YMAX-YMIN))
        IF(IY < 0 .OR. IY > LRVOXEL) CYCLE
        IZ=INT(LRVOXEL*(X(3,J)-ZMIN)/(ZMAX-ZMIN))
        IF(IZ < 0 .OR. IZ > LRVOXEL) CYCLE
        IF(.NOT.(BTEST(CRVOXEL(IY,IZ),IX))) CYCLE

C Second test vs reduced Box
        IF( (X(1,J)-XMINB)/(XMAXB-XMINB) > UN ) THEN
          IIX(I) = NBX  
        ELSE
          IIX(I)=INT(MAX(NBX*(X(1,J)-XMINB)/(XMAXB-XMINB),-UN))
        ENDIF
        IF( (X(2,J)-YMINB)/(YMAXB-YMINB) > UN ) THEN
          IIY(I) = NBY  
        ELSE
          IIY(I)=INT(MAX(NBY*(X(2,J)-YMINB)/(YMAXB-YMINB),-UN))
        ENDIF
        IF( (X(3,J)-ZMINB)/(ZMAXB-ZMINB) > UN ) THEN
          IIZ(I) = NBZ  
        ELSE
          IIZ(I)=INT(MAX(NBZ*(X(3,J)-ZMINB)/(ZMAXB-ZMINB),-UN))
        ENDIF


        IIX(I)=MAX(1,2+IIX(I))
        IIY(I)=MAX(1,2+IIY(I))
        IIZ(I)=MAX(1,2+IIZ(I))

        FIRST = VOXEL(IIX(I),IIY(I),IIZ(I))
        IF(FIRST == 0)THEN
c         boite vide
          VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
          NEXT_NOD(I)                 = 0 ! pas de next
          LAST_NOD(I)                 = 0 ! pas de last
        ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         boite contenant un seul noeud
c         on ajoute comme suivant
          NEXT_NOD(FIRST) = I ! next
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(I)     = 0 ! pas de next apres I
        ELSE
c         boite contenant plusieurs noeuds
c         on saute directement au dernier
          LAST = LAST_NOD(FIRST) ! dernier noeud de la boite
          NEXT_NOD(LAST)  = I ! next apres precedant
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(I)     = 0 ! pas de next apres I
        ENDIF
      ENDDO
C================================================================
C 3   recherche des boites concernant chaque facette
C     et creation des candidats
C================================================================

c      print*,'loop nrtm',NRTM
c      maxvox=0
c      meanvox=0
c      ttvox=0
c      maxjj=0
c      meanjj8=0
c      ttjj=0
c      jj1=0
c      jj2=0
c      jj3=0
c      maxaaa=0.
       ALLOCATE(TAGNOD(NUMNOD+NUMFAKENODIGEO))
       TAGNOD(1:NUMNOD+NUMFAKENODIGEO) = 0

      DO KK=1,NRTM
         NE=INDEX(KK)
C on ne retient pas les facettes detruites
         IF(STF(NE) == ZERO)CYCLE

         IF(FLAGREMNODE==2.AND.IREMNODE==2)THEN
            K = KREMNODE(NE)+1
            L = KREMNODE(NE+1)
            DO M=K,L
               TAGNOD(REMNODE(M)) = 1
            ENDDO
         ENDIF

         IF(IGAP == 0)THEN
           AAA = TZINF+CURV_MAX(NE)
         ELSE
           AAA = MARGE+CURV_MAX(NE)+
     .       MAX(MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE))),DRAD)
         ENDIF
c        maxaaa=max(maxaaa,aaa)
c     il est possible d'ameliorer l'algo en decoupant la facette 
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee 

         M1 = IRECT(1,NE)
         M2 = IRECT(2,NE)
         M3 = IRECT(3,NE)
         M4 = IRECT(4,NE)

         XX1=X(1,M1)
         XX2=X(1,M2)
         XX3=X(1,M3)
         XX4=X(1,M4)
         XMAXE=MAX(XX1,XX2,XX3,XX4)
         XMINE=MIN(XX1,XX2,XX3,XX4)

         YY1=X(2,M1)
         YY2=X(2,M2)
         YY3=X(2,M3)
         YY4=X(2,M4)
         YMAXE=MAX(YY1,YY2,YY3,YY4)
         YMINE=MIN(YY1,YY2,YY3,YY4)

         ZZ1=X(3,M1)
         ZZ2=X(3,M2)
         ZZ3=X(3,M3)
         ZZ4=X(3,M4)
         ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
         ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)


c        calcul de la surface (pour elimination future de candidats)

         SX = (YY3-YY1)*(ZZ4-ZZ2) - (ZZ3-ZZ1)*(YY4-YY2)
         SY = (ZZ3-ZZ1)*(XX4-XX2) - (XX3-XX1)*(ZZ4-ZZ2)
         SZ = (XX3-XX1)*(YY4-YY2) - (YY3-YY1)*(XX4-XX2)
         S2 = SX*SX + SY*SY + SZ*SZ

c        indice des voxels occupes par la facette
         IF( (XMINE - AAA - XMINB)/(XMAXB-XMINB) > UN ) THEN
           IX1 = NBX  
         ELSE
           IX1=INT(MAX(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB),-UN))
         ENDIF
         IF( (YMINE - AAA - YMINB)/(YMAXB-YMINB) > UN ) THEN
           IY1 = NBY  
         ELSE
           IY1=INT(MAX(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB),-UN))
         ENDIF
         IF( (ZMINE - AAA - ZMINB)/(ZMAXB-ZMINB) > UN ) THEN
           IZ1 = NBZ  
         ELSE
           IZ1=INT(MAX(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB),-UN))
         ENDIF

         
            
         IX1=MAX(1,2+IX1)
         IY1=MAX(1,2+IY1)
         IZ1=MAX(1,2+IZ1)

         
         IF( (XMAXE + AAA - XMINB)/(XMAXB-XMINB) > UN ) THEN
           IX2 = NBX  
         ELSE
           IX2=INT(MAX(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB),-UN))
         ENDIF
         IF( (YMAXE + AAA - YMINB)/(YMAXB-YMINB) > UN ) THEN
           IY2 = NBY  
         ELSE
           IY2=INT(MAX(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB),-UN))
         ENDIF
         IF( (ZMAXE + AAA - ZMINB)/(ZMAXB-ZMINB) > UN ) THEN
           IZ2 = NBZ  
         ELSE
           IZ2=INT(MAX(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB),-UN))
         ENDIF

         IX2=MAX(1,2+IX2)
         IY2=MAX(1,2+IY2)
         IZ2=MAX(1,2+IZ2)

c        maxvox = max(maxvox,(IZ2-IZ1)*(IY2-IY1)*(IX2-IX1))
c        ttvox=ttvox+1
c        meanvox=meanvox+(IZ2-IZ1)*(IY2-IY1)*(IX2-IX1)
         DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
           DO IX = IX1,IX2

             JJ = VOXEL(IX,IY,IZ)
cc           numjj = 0

             DO WHILE(JJ /= 0)

               NN=NSV(JJ)
C
cc             numjj=numjj+1
c
               IF(TAGNOD(NN) == 1 ) GOTO 300
               IF(NN == M1)GOTO 300
               IF(NN == M2)GOTO 300
               IF(NN == M3)GOTO 300
               IF(NN == M4)GOTO 300
               
c

               XS = X(1,NN)
               YS = X(2,NN)
               ZS = X(3,NN)
               IF(IGAP /= 0)THEN
                 AAA = MARGE+CURV_MAX(NE)+MAX(MIN(GAPMAX,MAX(GAPMIN,
     .             GAP_S(JJ)+GAP_M(NE))),DRAD)
               ENDIF
               IF(XS<=XMINE-AAA)GOTO 300
               IF(XS>=XMAXE+AAA)GOTO 300
               IF(YS<=YMINE-AAA)GOTO 300
               IF(YS>=YMAXE+AAA)GOTO 300
               IF(ZS<=ZMINE-AAA)GOTO 300
               IF(ZS>=ZMAXE+AAA)GOTO 300

c    sousestimation de la distance**2 pour elimination de candidats

               D1X = XS - XX1
               D1Y = YS - YY1
               D1Z = ZS - ZZ1
               D2X = XS - XX2
               D2Y = YS - YY2
               D2Z = ZS - ZZ2
               DD1 = D1X*SX+D1Y*SY+D1Z*SZ
               DD2 = D2X*SX+D2Y*SY+D2Z*SZ
               IF(DD1*DD2 > ZERO)THEN
                 D2 = MIN(DD1*DD1,DD2*DD2)                 
                 A2 = AAA*AAA*S2
                 IF(D2 > A2)GOTO 300
               ENDIF
                 J_STOK = J_STOK + 1
                 PROV_N(J_STOK) = JJ
                 PROV_E(J_STOK) = NE
                 IF(J_STOK == NVSIZ) THEN
                   LFT = 1
                   LLT = NVSIZ
                   NFT = 0
                   J_STOK = 0
                   CALL I7COR3(X    ,IRECT,NSV  ,PROV_E ,PROV_N,
     .                         STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .                         GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .                         GAP_S_L,GAP_M_L  ,DRAD   ,IX11    ,IX12   ,
     5                         IX13    ,IX14    ,NSVG,X1      ,X2    ,
     6                         X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7                         Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8                         XI     ,YI     ,ZI  ,STIF)
                   CALL I7DST3(IX13,IX14,X1 ,X2 ,X3 ,
     1                  X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2                  Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3                  YI ,ZI ,X0 ,Y0 ,Z0 ,
     4                  NX1,NY1,NZ1,NX2,NY2,
     5                  NZ2,NX3,NY3,NZ3,NX4,
     6                  NY4,NZ4,P1 ,P2 ,P3 ,
     7                  P4 ,LB1,LB2,LB3,LB4,
     8                  LC1,LC2,LC3,LC4)
                   CALL I7PEN3(MARGE,GAPV,N11,N21,N31,
     1                  PENE ,NX1 ,NY1,NZ1,NX2,
     2                  NY2  ,NZ2 ,NX3,NY3,NZ3,
     3                  NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4                  P3   ,P4)
                   IF(I_STOK+NVSIZ.LT.MULTIMP*NSN) THEN
                     CALL I7CMP3(I_STOK,CAND_E  ,CAND_N,1,PENE,
     1                           PROV_N,PROV_E)
                   ELSE
                     I_BID = 0
                     CALL I7CMP3(I_BID,CAND_E,CAND_N,0,PENE,
     1                           PROV_N,PROV_E)
                     IF(I_STOK+I_BID.LT.MULTIMP*NSN) THEN
                       CALL I7CMP3(I_STOK,CAND_E,CAND_N,1,PENE,
     1                             PROV_N,PROV_E)
                     ELSE
                       I_MEM = 2
                       J_STOK = 0
                       I_STOK = 0
                       GOTO 100
                     ENDIF
                   ENDIF
                 ENDIF

 300           CONTINUE
               JJ = NEXT_NOD(JJ)
             ENDDO ! WHILE(JJ /= 0)
c            maxjj=max(maxjj,numjj)
c            meanjj8=meanjj8+numjj
c            ttjj=ttjj+1
c            if(numjj>100)jj1=jj1+1
c            if(numjj>1000)jj2=jj2+1
c            if(numjj>5000)jj3=jj3+1
           ENDDO
          ENDDO
         ENDDO

         IF(FLAGREMNODE==2.AND.IREMNODE==2)THEN
            K = KREMNODE(NE)+1
            L = KREMNODE(NE+1)
            DO M=K,L
               TAGNOD(REMNODE(M)) = 0
            ENDDO
         ENDIF

      ENDDO

      DEALLOCATE(TAGNOD)

c     meanjj8=nint(float(meanjj8)/max(1,ttjj))
c     meanvox=nint(float(meanvox)/max(1,ttvox))
c     print*,'max tzvar  :',maxaaa
c     print*,'result nb vox  :',maxvox,meanvox,ttvox
c     print*,'result nb nodes:',maxjj,meanjj8,ttjj
c     print*,'result >100 >1000 > 5000:',jj1,jj2,jj3
C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
C================================================================
C 4   remise a zero des noeuds dans les boites
C================================================================
 100  CONTINUE
      DO I=1,NSN
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
      DEALLOCATE(NEXT_NOD)
      DEALLOCATE(IIX)
      DEALLOCATE(IIY)
      DEALLOCATE(IIZ)
    
      RETURN
      END
