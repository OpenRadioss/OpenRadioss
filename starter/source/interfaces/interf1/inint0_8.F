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
C
Chd|====================================================================
Chd|  ININT0_8                      source/interfaces/interf1/inint0_8.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ININT0_8(X      ,IRECT  ,NSEG   ,LCSEG  ,NSV   ,
     1                    MSR    ,ILOC   ,NMN    ,NSN    ,NRT   , NUMNOD )
C-----------------------------------------------------------------------
C     INTERFACE TYPE 8 (DRAWBEAD)
C-----------------------------------------------------------------------
C=======================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: NMN, NSN, NRT, NUMNOD
      INTEGER, INTENT(IN)  :: IRECT(4,*), NSEG(*), NSV(*), MSR(*) 
      INTEGER, INTENT(OUT) :: LCSEG(*), ILOC(*)
      my_real, INTENT(IN)  :: X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, JJ, J, K, L, N, SEG1
      my_real
     .   CMS, DMS,XMAX_M,YMAX_M,ZMAX_M,XMAX_S,YMAX_S,ZMAX_S,
     .   XMIN_M,YMIN_M,ZMIN_M,XMIN_S,YMIN_S,ZMIN_S,
     .   XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,XS,YS,ZS,DIST,MINDIST,
     .   X1,Y1,Z1,AAA
      INTEGER  IIX(NSN),IIY(NSN),IIZ(NSN)
      INTEGER  LAST_NOD(NSN),NEXT_NOD(NSN)
      INTEGER  NBX,NBY,NBZ,NE,FOUND
      INTEGER  FIRST,LAST,IX1,IY1,IZ1,NN,IX,IY,IZ,IX2,IY2,IZ2
      INTEGER, DIMENSION(:,:,:),ALLOCATABLE :: VOXEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: KNOD2SEG
      INTEGER, DIMENSION(:), ALLOCATABLE :: NOD2SEG
C-----------------------------------------------
      IF(NRT.EQ.0) RETURN
C================================================================
      NEXT_NOD = 0
      IIX = 0
      IIY = 0
      IIZ = 0
C----- BORNES DU DOMAINE
      XMAX_M=-EP30
      YMAX_M=-EP30
      ZMAX_M=-EP30 
      XMIN_M= EP30
      YMIN_M= EP30
      ZMIN_M= EP30

      DO I=1,NMN
         J=MSR(I) 
         XMAX_M= MAX(XMAX_M,X(1,J))
         YMAX_M= MAX(YMAX_M,X(2,J))
         ZMAX_M= MAX(ZMAX_M,X(3,J))
         XMIN_M= MIN(XMIN_M,X(1,J))
         YMIN_M= MIN(YMIN_M,X(2,J))
         ZMIN_M= MIN(ZMIN_M,X(3,J))
      ENDDO

      XMIN_S= EP30
      XMAX_S=-EP30
      YMIN_S= EP30
      YMAX_S=-EP30
      ZMIN_S= EP30
      ZMAX_S=-EP30 

      DO I=1,NSN
       J=NSV(I)
       XMIN_S= MIN(XMIN_S,X(1,J))
       YMIN_S= MIN(YMIN_S,X(2,J))
       ZMIN_S= MIN(ZMIN_S,X(3,J))
       XMAX_S= MAX(XMAX_S,X(1,J))
       YMAX_S= MAX(YMAX_S,X(2,J))
       ZMAX_S= MAX(ZMAX_S,X(3,J))
      ENDDO
      XMIN=MIN(XMIN_M,XMIN_S)
      YMIN=MIN(YMIN_M,YMIN_S)
      ZMIN=MIN(ZMIN_M,ZMIN_S)
      XMAX=MAX(XMAX_M,XMAX_S)
      YMAX=MAX(YMAX_M,YMAX_S)
      ZMAX=MAX(ZMAX_M,ZMAX_S)
C

      AAA = SQRT(NSN /
     .           ((XMAX-XMIN)*(YMAX-YMIN)
     .           +(YMAX-YMIN)*(ZMAX-ZMIN)
     .           +(ZMAX-ZMIN)*(XMAX-XMIN)))

      AAA = TQUART*AAA

      NBX = NINT(AAA*(XMAX-XMIN))
      NBY = NINT(AAA*(YMAX-YMIN))
      NBZ = NINT(AAA*(ZMAX-ZMIN))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)
C================================================================
C 1   mise des noeuds dans les boites
C================================================================
      ALLOCATE(VOXEL(NBX,NBY,NBZ))
      VOXEL=0
       DO I=1,NSN
        IIX(I)=0
        IIY(I)=0
        IIZ(I)=0
        J=NSV(I)
         
        IIX(I)=INT(NBX*(X(1,J)-XMIN)/(XMAX-XMIN))
        IIY(I)=INT(NBY*(X(2,J)-YMIN)/(YMAX-YMIN))
        IIZ(I)=INT(NBZ*(X(3,J)-ZMIN)/(ZMAX-ZMIN))

        IIX(I)=MAX(1,MIN(NBX,IIX(I)))
        IIY(I)=MAX(1,MIN(NBY,IIY(I)))
        IIZ(I)=MAX(1,MIN(NBZ,IIZ(I)))

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
C 2   recherche noeud second. le plus proche de chaque main
C================================================================
      DO NE=1,NMN
        MINDIST = EP30
        X1 = X(1,MSR(NE))
        Y1 = X(2,MSR(NE))
        Z1 = X(3,MSR(NE))
        IX1=INT(NBX*(X1-XMIN)/(XMAX-XMIN))
        IY1=INT(NBY*(Y1-YMIN)/(YMAX-YMIN))
        IZ1=INT(NBZ*(Z1-ZMIN)/(ZMAX-ZMIN))

        IX1=MAX(1,MIN(NBX,IX1))
        IY1=MAX(1,MIN(NBY,IY1))
        IZ1=MAX(1,MIN(NBZ,IZ1))

        IX2=IX1
        IY2=IY1
        IZ2=IZ1

        FOUND = 0
        DO WHILE(FOUND == 0)
         DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
           DO IX = IX1,IX2

             JJ = VOXEL(IX,IY,IZ)

             DO WHILE(JJ /= 0)
               NN=NSV(JJ)
               XS = X(1,NN)
               YS = X(2,NN)
               ZS = X(3,NN)
               DIST = (X1-XS)**2+(Y1-YS)**2+(Z1-ZS)**2
               IF( DIST .LT. MINDIST )  THEN
                 ILOC(NE)=JJ
                 MINDIST = DIST
               ENDIF
               FOUND = 1
               JJ = NEXT_NOD(JJ)
             ENDDO ! WHILE(JJ /= 0)
           ENDDO
          ENDDO
         ENDDO
         IX1 = IX1-1
         IY1 = IY1-1
         IZ1 = IZ1-1
         IX1 = MAX(1,IX1)
         IY1 = MAX(1,IY1)
         IZ1 = MAX(1,IZ1)
         IX2 = IX2+1
         IY2 = IY2+1
         IZ2 = IZ2+1
         IX2 = MIN(NBX,IX2)
         IY2 = MIN(NBY,IY2)
         IZ2 = MIN(NBZ,IZ2)
        ENDDO ! WHILE(FOUND == 0)


        X1 = X(1,MSR(NE))
        Y1 = X(2,MSR(NE))
        Z1 = X(3,MSR(NE))
        IX1=INT(NBX*(X1-MINDIST-XMIN)/(XMAX-XMIN))
        IY1=INT(NBY*(Y1-MINDIST-YMIN)/(YMAX-YMIN))
        IZ1=INT(NBZ*(Z1-MINDIST-ZMIN)/(ZMAX-ZMIN))

        IX1=MAX(1,MIN(NBX,IX1))
        IY1=MAX(1,MIN(NBY,IY1))
        IZ1=MAX(1,MIN(NBZ,IZ1))

        IX2=INT(NBX*(X1+MINDIST-XMIN)/(XMAX-XMIN))
        IY2=INT(NBY*(Y1+MINDIST-YMIN)/(YMAX-YMIN))
        IZ2=INT(NBZ*(Z1+MINDIST-ZMIN)/(ZMAX-ZMIN))

        IX2=MAX(1,MIN(NBX,IX2))
        IY2=MAX(1,MIN(NBY,IY2))
        IZ2=MAX(1,MIN(NBZ,IZ2))

         DO IZ = IZ1,IZ2
          DO IY = IY1,IY2
           DO IX = IX1,IX2

             JJ = VOXEL(IX,IY,IZ)

             DO WHILE(JJ /= 0)
               NN=NSV(JJ)
               XS = X(1,NN)
               YS = X(2,NN)
               ZS = X(3,NN)
               DIST = (X1-XS)**2+(Y1-YS)**2+(Z1-ZS)**2
               IF( DIST .LT. MINDIST )  THEN
                 ILOC(NE)=JJ
                 MINDIST = DIST
               ENDIF
               JJ = NEXT_NOD(JJ)
             ENDDO ! WHILE(JJ /= 0)
           ENDDO
          ENDDO
         ENDDO
      ENDDO
      DEALLOCATE(VOXEL)
C-----------------------------------------------
c 3 Build inverse connectivity for segments
C-----------------------------------------------
      ALLOCATE(KNOD2SEG(NUMNOD+1))
      ALLOCATE(NOD2SEG(4*NRT))
      NOD2SEG(1:4*NRT) = 0
      KNOD2SEG(1:NUMNOD+1) = 0
      DO I=1,NRT
        DO K=1,4
          N = IRECT(K,I)
          KNOD2SEG(N) = KNOD2SEG(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2SEG(I+1) = KNOD2SEG(I+1) + KNOD2SEG(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2SEG(N+1)=KNOD2SEG(N)
      END DO
      KNOD2SEG(1)=0
C
      DO I=1,NRT
        DO K=1,4
          N = IRECT(K,I)
          KNOD2SEG(N) = KNOD2SEG(N) + 1
          NOD2SEG(KNOD2SEG(N)) = I
        END DO
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2SEG(N+1)=KNOD2SEG(N)
      END DO
      KNOD2SEG(1)=0
C----------------------------------------------------------------
c 4 Remplissage LCSEG : List of Connected Segments to secnd nodes
C----------------------------------------------------------------
      II=0
      DO I=1,NSN
        K = NSV(I)
        DO J=KNOD2SEG(K)+1,KNOD2SEG(K+1)
          SEG1 = NOD2SEG(J)
          DO L=1,4
            IF (IRECT(L,SEG1) .NE. K)THEN
              II=II+1
              LCSEG(II)=SEG1
              EXIT
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      DEALLOCATE(KNOD2SEG)
      DEALLOCATE(NOD2SEG)
      RETURN
      END