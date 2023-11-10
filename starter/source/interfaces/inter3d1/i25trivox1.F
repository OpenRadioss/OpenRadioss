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
Chd|  I25TRIVOX1                    source/interfaces/inter3d1/i25trivox1.F
Chd|-- called by -----------
Chd|        I25BUC_VOX1                   source/interfaces/inter3d1/i25buc_vox1.F
Chd|-- calls ---------------
Chd|        I25STO                        source/interfaces/inter3d1/i25sto.F
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25TRIVOX1(
     1      NSN    ,I_MEM   ,IRECT  ,X       ,STF     ,
     2      STFN   ,XYZM    ,NSV    ,II_STOK ,CAND_N  ,
     3      ESHIFT ,CAND_E  ,MULNSN ,NOINT   ,BGAPSMX ,
     4      VOXEL  ,NBX     ,NBY    ,NBZ     ,NRTM    ,
     5      GAP_S  ,GAP_M   ,MARGE  ,ITAB    ,
     6      NBINFLG,MBINFLG ,ILEV   ,MSEGTYP ,
     7      IGAP   ,GAP_S_L ,GAP_M_L,EDGE_L2 ,LEDGMAX ,
     8      IREMNODE,FLAGREMNODE,KREMNODE,REMNODE,INACTI,
     9      IPARTS  ,NPARTNS ,LPARTNS ,IELEM ,ICODE   ,
     A      ISKEW   ,DRAD, IS_LARGE_NODE, LARGE_NODE  ,
     B      NB_LARGE_NODES,PEN_OLD      , DGAPLOAD,NRTMT)
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
#include      "com04_c.inc"
#include      "param_c.inc"
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
C
C     PROV_N       CAND_N provisoire (variable static dans i7tri)
C     PROV_E       CAND_E provisoire (variable static dans i7tri)

C     VOXEL(ix,iy,iz) contient le numero local du premier noeud de
C                  la boite
C     NEXT_NOD(i)  noeud suivant dans la meme boite (si /= 0)
C     LAST_NOD(i)  dernier noeud dans la meme boite (si /= 0)
C                  utilise uniquement pour aller directement du premier
C                       noeud au dernier
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,ESHIFT,NSN,NRTM,IGAP,
     .        MULNSN,NOINT,NBX,NBY,NBZ,
     .        NSV(*),CAND_N(*),CAND_E(*),
     .        IRECT(4,*), VOXEL(NBX+2,NBY+2,NBZ+2),II_STOK,ITAB(*),
     .        NBINFLG(*),MBINFLG(*),ILEV,MSEGTYP(*),
     .        IREMNODE,FLAGREMNODE,KREMNODE(*),REMNODE(*),
     .        INACTI, IPARTS(*), NPARTNS(*), LPARTNS(*), IELEM(*), ICODE(*), ISKEW(*)
C     REAL
      my_real
     .   X(3,*),XYZM(6),STF(*),STFN(*),GAP_S(*),GAP_M(*),
     .   GAP_S_L(*),GAP_M_L(*), EDGE_L2(*),PEN_OLD(5,NSN)
      my_real
     .   LEDGMAX, MARGE, BGAPSMX
      my_real , INTENT(IN) :: DRAD, DGAPLOAD
      INTEGER :: LARGE_NODE(NSN) ! list of nodes that have large research distance wrt solids
      INTEGER :: IS_LARGE_NODE(NSN) ! tag nodes that have large research distance wrt solids
      INTEGER :: NB_LARGE_NODES
      INTEGER , INTENT(IN) :: NRTMT

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,K,L,J_STOK,II,JJ,
     .        PROV_N(MVSIZ),PROV_E(MVSIZ),
     .        M,NS,
     .        IEM,IPM,IPS
C     REAL
      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX,
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS
C
      INTEGER  LAST_NOD(NSN)
      INTEGER  IX,IY,IZ,NEXT,M1,M2,M3,M4,
     .         IX1,IY1,IZ1,IX2,IY2,IZ2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,AAA,NSNM
      INTEGER FIRST,NEW,LAST
      INTEGER, DIMENSION(NUMNOD) :: TAGNOD
C-----------------------------------------------
      ALLOCATE(NEXT_NOD(NSN))
      ALLOCATE(IIX(NSN))
      ALLOCATE(IIY(NSN))
      ALLOCATE(IIZ(NSN))
C
C Phase initiale de construction de BPE et BPN deplacee de I7BUCE => I7TRI
C
      XMIN = XYZM(1)
      YMIN = XYZM(2)
      ZMIN = XYZM(3)
      XMAX = XYZM(4)
      YMAX = XYZM(5)
      ZMAX = XYZM(6)

c     dev future: xminb plus grand que xmin...
      XMINB = XMIN
      YMINB = YMIN
      ZMINB = ZMIN
      XMAXB = XMAX
      YMAXB = YMAX
      ZMAXB = ZMAX

C=======================================================================
C 1   mise des noeuds dans les boites
C=======================================================================
      DO I=1,NSN
        IIX(I)=0
        IIY(I)=0
        IIZ(I)=0
        IF(STFN(I) == ZERO)CYCLE
        J=NSV(I)
C Optimisation // recherche les noeuds compris dans xmin xmax des
C elements du processeur
        IF(X(1,J) < XMIN)  CYCLE
        IF(X(1,J) > XMAX)  CYCLE
        IF(X(2,J) < YMIN)  CYCLE
        IF(X(2,J) > YMAX)  CYCLE
        IF(X(3,J) < ZMIN)  CYCLE
        IF(X(3,J) > ZMAX)  CYCLE

        IIX(I)=INT(NBX*(X(1,J)-XMINB)/(XMAXB-XMINB))
        IIY(I)=INT(NBY*(X(2,J)-YMINB)/(YMAXB-YMINB))
        IIZ(I)=INT(NBZ*(X(3,J)-ZMINB)/(ZMAXB-ZMINB))

        IIX(I)=MAX(1,2+MIN(NBX,IIX(I)))
        IIY(I)=MAX(1,2+MIN(NBY,IIY(I)))
        IIZ(I)=MAX(1,2+MIN(NBZ,IIZ(I)))

        FIRST = VOXEL(IIX(I),IIY(I),IIZ(I))
        IF(FIRST == 0)THEN
c         empty cell
          VOXEL(IIX(I),IIY(I),IIZ(I)) = I ! first
          NEXT_NOD(I)                 = 0 ! last one
          LAST_NOD(I)                 = 0 ! no last
        ELSEIF(LAST_NOD(FIRST) == 0)THEN
c         cell containing one node
c         add as next node
          NEXT_NOD(FIRST) = I ! next
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(I)     = 0 ! last one
        ELSE
c
c         jump to the last node of the cell
          LAST = LAST_NOD(FIRST) ! last node in this voxel
          NEXT_NOD(LAST)  = I ! next
          LAST_NOD(FIRST) = I ! last
          NEXT_NOD(I)     = 0 ! last one
        ENDIF
      ENDDO
C=======================================================================
C 2   recherche des boites concernant chaque facette
C     et creation des candidats
C=======================================================================
      TAGNOD(1:NUMNOD) = 0
C-----------------------------------------------
      J_STOK = 0


      DO NE=1,NRTM
c
c     il est possible d'ameliorer l'algo en decoupant la facette
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee

        M1 = IRECT(1,NE)
        M2 = IRECT(2,NE)
        M3 = IRECT(3,NE)
        M4 = IRECT(4,NE)

        IF(FLAGREMNODE==2.AND.IREMNODE==2)THEN
          K = KREMNODE(NE)+1
          L = KREMNODE(NE+1)
          DO M=K,L
            TAGNOD(REMNODE(M)) = 1
          ENDDO
        ENDIF

        IF (MSEGTYP(NE)==0 .OR. MSEGTYP(NE)>NRTMT)THEN
          ! LEDGMAX /=0 <=> INACTI=5 or -1 and IDDLEVEL=1 !
          AAA = MAX(MARGE+MAX(BGAPSMX+GAP_M(NE)+DGAPLOAD,DRAD),LEDGMAX+BGAPSMX+GAP_M(NE)+DGAPLOAD)
        ELSE
          AAA = MARGE+MAX(BGAPSMX+GAP_M(NE)+DGAPLOAD,DRAD)
        END IF


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

        IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))

        IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))

        IF (MSEGTYP(NE)==0 .OR. MSEGTYP(NE)>NRTMT)THEN
C Check for "large" nodes separately
C if the current segment belongs to a solid or a coating shell
          DO I = 1, NB_LARGE_NODES
            JJ = LARGE_NODE(I)
            NN=NSV(JJ)
            IF(NN == M1)GOTO 400
            IF(NN == M2)GOTO 400
            IF(NN == M3)GOTO 400
            IF(NN == M4)GOTO 400
            IF(TAGNOD(NN) == 1)GOTO 400

            XS = X(1,NN)
            YS = X(2,NN)
            ZS = X(3,NN)
c PMAX_GAP is a global overestimate penetration
c NEED to communicate in SPMD
c VMAXDT is a local overestimate of relative incremental displacement
c NO need to communicate in SPMD

            AAA = MAX(MARGE+MAX(GAP_S(JJ)+GAP_M(NE)+DGAPLOAD,DRAD)+DGAPLOAD,EDGE_L2(JJ)+GAP_S(JJ)+GAP_M(NE)+DGAPLOAD)
            IF(XS<=XMINE-AAA)GOTO 400
            IF(XS>=XMAXE+AAA)GOTO 400
            IF(YS<=YMINE-AAA)GOTO 400
            IF(YS>=YMAXE+AAA)GOTO 400
            IF(ZS<=ZMINE-AAA)GOTO 400
            IF(ZS>=ZMAXE+AAA)GOTO 400

c               IF(INACTI==5.OR.INACTI==-1)THEN ! Do it for all inacti
            IEM=IELEM(NE)
            IF(IEM/=0)THEN
              IPM=IPARTS(IEM)
              IPS=0
              DO J=NPARTNS(JJ)+1,NPARTNS(JJ+1)
                IF(LPARTNS(J)==IPM)THEN
                  IPS=IPM
                END IF
              END DO
              IF(IPM==IPS) GOTO 400
            END IF
c               END IF
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
              IF(D2 > A2)GOTO 400
            ENDIF
            J_STOK = J_STOK + 1
            PROV_N(J_STOK) = JJ
            PROV_E(J_STOK) = NE
            IF(J_STOK == NVSIZ)THEN
              CALL I25STO(
     1           NVSIZ ,IRECT  ,X     ,NSV   ,II_STOK,
     2           CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3           I_MEM ,PROV_N ,PROV_E,ESHIFT,NSN   ,
     4           NRTM  ,GAP_S  ,GAP_M ,NBINFLG  ,MBINFLG,
     5           ILEV,MSEGTYP,ITAB  ,IGAP ,GAP_S_L,
     6           GAP_M_L,EDGE_L2,ICODE,ISKEW,DRAD ,
     7           DGAPLOAD,NRTMT)
              IF(I_MEM==2)GOTO 100
              J_STOK = 0
            ENDIF
  400       CONTINUE
          ENDDO ! WHILE(JJ /= 0)
        ENDIF ! solid or coating shell

        DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
            DO IX = IX1,IX2

cc             nbpelem = nbpelem + 1

              JJ = VOXEL(IX,IY,IZ)

              DO WHILE(JJ /= 0)

cc             nnpelem = nnpelem + 1

                NN=NSV(JJ)
                IF(NN == M1)GOTO 300
                IF(NN == M2)GOTO 300
                IF(NN == M3)GOTO 300
                IF(NN == M4)GOTO 300
                IF(TAGNOD(NN) == 1)GOTO 300

                XS = X(1,NN)
                YS = X(2,NN)
                ZS = X(3,NN)
c PMAX_GAP is a global overestimate penetration
c NEED to communicate in SPMD
c VMAXDT is a local overestimate of relative incremental displacement
c NO need to communicate in SPMD

                IF (MSEGTYP(NE)==0 .OR. MSEGTYP(NE)>NRTMT)THEN
                  IF(IS_LARGE_NODE(JJ)==1) GOTO 300 ! node already checked before
                  ! LEDGMAX /=0 <=> INACTI=5 or -1 and IDDLEVEL=1 !
                  AAA = MAX(MARGE+MAX(GAP_S(JJ)+GAP_M(NE)+DGAPLOAD,DRAD),EDGE_L2(JJ)+GAP_S(JJ)+GAP_M(NE)+DGAPLOAD)
                ELSE
                  AAA = MARGE+MAX(GAP_S(JJ)+GAP_M(NE)+DGAPLOAD,DRAD)
                END IF

                IF(XS<=XMINE-AAA)GOTO 300
                IF(XS>=XMAXE+AAA)GOTO 300
                IF(YS<=YMINE-AAA)GOTO 300
                IF(YS>=YMAXE+AAA)GOTO 300
                IF(ZS<=ZMINE-AAA)GOTO 300
                IF(ZS>=ZMAXE+AAA)GOTO 300

c               IF(INACTI==5.OR.INACTI==-1)THEN Do it for all inacti
                IEM=IELEM(NE)
                IF(IEM/=0)THEN
                  IPM=IPARTS(IEM)
                  IPS=0
                  DO J=NPARTNS(JJ)+1,NPARTNS(JJ+1)
                    IF(LPARTNS(J)==IPM)THEN
                      IPS=IPM
                    END IF
                  END DO
                  IF(IPM==IPS) GOTO 300
                END IF
c               END IF



c    sousestimation de la distance**2 pour elimination de candidats

cc             nnr0pelem = nnr0pelem + 1

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
                IF(J_STOK == NVSIZ)THEN

                  CALL I25STO(
     1               NVSIZ ,IRECT  ,X     ,NSV   ,II_STOK,
     2               CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3               I_MEM ,PROV_N ,PROV_E,ESHIFT,NSN   ,
     4               NRTM  ,GAP_S  ,GAP_M ,NBINFLG  ,MBINFLG,
     5               ILEV,MSEGTYP,ITAB  ,IGAP ,GAP_S_L,
     6               GAP_M_L,EDGE_L2,ICODE,ISKEW,DRAD ,
     7               DGAPLOAD,NRTMT)
                  IF(I_MEM==2)GOTO 100
                  J_STOK = 0
                ENDIF

  300           CONTINUE

                JJ = NEXT_NOD(JJ)

              ENDDO ! WHILE(JJ /= 0)

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
C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
      IF(J_STOK/=0)CALL I25STO(
     1                J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2                CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3                I_MEM ,PROV_N ,PROV_E,ESHIFT,NSN   ,
     4                NRTM  ,GAP_S  ,GAP_M ,NBINFLG  ,MBINFLG,
     5                ILEV,MSEGTYP,ITAB  ,IGAP ,GAP_S_L,
     6                GAP_M_L,EDGE_L2,ICODE,ISKEW,DRAD ,
     7                DGAPLOAD,NRTMT)
C 4   remise a zero des noeuds dans les boites
C=======================================================================
  100 CONTINUE

      DO I=1,NSN
        IF(IIX(I)/=0)THEN
          VOXEL(IIX(I),IIY(I),IIZ(I))=0
        ENDIF
      ENDDO
C
      DEALLOCATE(NEXT_NOD)
      DEALLOCATE(IIX)
      DEALLOCATE(IIY)
      DEALLOCATE(IIZ)

      RETURN
      END

