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
Chd|  I11TRIVOX                     source/interfaces/intsort/i11trivox.F
Chd|-- called by -----------
Chd|        I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|-- calls ---------------
Chd|        I11STO_VOX                    source/interfaces/intsort/i11sto.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        REALLOC_MOD                   share/modules/realloc_mod.F   
Chd|        TRI11                         share/modules/tri11_mod.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11TRIVOX(
     1      IRECTS, IRECTM , X      , NRTM      ,NRTSR ,
     2      XYZM  , II_STOK, CAND_S , CAND_M    ,NSN4  ,
     3      NOINT , TZINF  , I_MEM  , ESHIFT    ,ADDCM ,
     5      CHAINE, NRTS   , ITAB   , STFS      ,STFM  ,
     6      IAUTO , VOXEL  , NBX    , NBY       ,NBZ   ,
     7      ITASK , IFPEN  , IFORM  , GAPMIN    ,DRAD  ,
     8      MARGE  ,GAP_S  , GAP_M  , GAP_S_L   ,GAP_M_L,
     9      BGAPSMX, IGAP  ,GAP     ,FLAGREMNODE,KREMNODE,
     1      REMNODE)    
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE REALLOC_MOD
      USE TRI7BOX
      USE TRI11
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
!#include      "r4r8_p.inc"
#include      "comlock.inc"
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
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
#include "mpif.h"
#endif
!-----------------------------------------------
!  SUBROUTINE AIM
! ==============
! VOXEL SEARCH to find couple (edge,edge) with penetration among all possible couples defined by secnd and main side.
! Temporary found candidate are written in Temporary array PROV_S and PROV_M in order to optimise OpenMP performances.
! There is no order.
! PROV_S(i),PROV_M(i) : is a potential candidate couple because edges are geometrically near each other.
! I11STO subroutine will compute if penetration is positive and if couple was not already stoked, in this case, candidate is stoked in CAND(S(i), CAND_M(i)
!
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     ADD(2,*)     TABLEAU DES ADRESSES              E/S 
C          1.........ADRESSES NOEUDS
C          2.........ADRESSES ELEMENTS
C     ZYZM(6,*)    TABLEAU DES XYZMIN                E/S 
C          1.........XMIN BOITE
C          2.........YMIN BOITE
C          3.........ZMIN BOITE
C          4.........XMAX BOITE
C          5.........YMAX BOITE
C          6.........ZMAX BOITE
C     IRECTM(2,*)  TABLEAU DES CONEC                 E
C          1.........NODE 1 main EDGE
C          2.........NODE 2 main EDGE
C     IRECTS(2,*)  TABLEAU DES CONEC                 E
C          1.........NODE 1 SECND EDGE
C          2.........NODE 2 SECND EDGE
C     X(3,*)       COORDONNEES NODALES               E
C     II_STOK       niveau de stockage des couples
C                                candidats impact    E/S
C     CAND_S       boites resultats noeuds
C     CAND_M       adresses des boites resultat elements
C     NOINT        NUMERO USER DE L'INTERFACE
C     TZINF        TAILLE ZONE INFLUENCE
C     VOXEL(*,*,*) VOXEL PARTIONNEMENT DE l'ESPACE (NBX+2,NBY+2,NBZ+2) 
C                  Stocke dans chaque voxel les edge secnd.
C                  En pratique designe la premier arrete d'une liste chainee
C     MAX_ADD      adresse maximum pour les tableaux chaines
C     NSN4         4*NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::
     .         NRTM,NRTSR,ESHIFT,NRTS,IGAP,
     .         NSN4,NOINT,ITAB(*),NBX,NBY,NBZ,IAUTO,
     .         IRECTS(2,NRTS),IRECTM(2,NRTM)
      INTEGER ITASK,IFORM
      INTEGER, INTENT(INOUT) ::
     .         CAND_S(*),CAND_M(*),ADDCM(*),CHAINE(2,*), 
     .         VOXEL(1:NBX+2,1:NBY+2,1:NBZ+2), I_MEM,IFPEN(*),II_STOK,
     .         FLAGREMNODE,KREMNODE(*),REMNODE(*)
      my_real
     .       ,INTENT(IN) ::
     .         X(3,*),XYZM(6,*),
     .        STFS(NRTS),STFM(NRTM), TZINF, GAP
      my_real
     .        GAPMIN,DRAD,MARGE,BGAPSMX,
     .        GAP_S(*),GAP_M(*), GAP_S_L(*), GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,J,SS1,SS2,IBUG,
     .        N1,N2,MM1,MM2, iN1, iN2, iM1, iM2, K,L,
     .        PROV_S(2*MVSIZ),PROV_M(2*MVSIZ),    !tableau provisoire de candidats envoye a I11STOK
     .        IX1,IY1,IZ1,IX2,IY2,IZ2, 
     .        IX,IY,IZ, FIRST_ADD,
     .        I_STOK, I_STOK_BAK, IEDG,
     .        PREV_ADD, CHAIN_ADD, CURRENT_ADD,   !pour le balayage des tableaux chaines
     .        NEDG, DEJA , MAX_ADD ,II_STOK0, M,REMOVE_REMOTE
      INTEGER, DIMENSION(3) :: TMIN,TMAX
      my_real
     .        XX1, XX2,
     .        XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, 
     .        YY1,YY2,ZZ1,ZZ2,
     .        AAA, DD,
     .        XMAX_EDGS(NRTS+NRTSR), XMIN_EDGS(NRTS+NRTSR),   !cotes min/max des arrete seconds et mains
     .        YMAX_EDGS(NRTS+NRTSR), YMIN_EDGS(NRTS+NRTSR),
     .        ZMAX_EDGS(NRTS+NRTSR), ZMIN_EDGS(NRTS+NRTSR),
     .        XMAX_EDGM(NRTM), XMIN_EDGM(NRTM),
     .        YMAX_EDGM(NRTM), YMIN_EDGM(NRTM),
     .        ZMAX_EDGM(NRTM), ZMIN_EDGM(NRTM),     
     .        XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB    
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGREMLINE    
C-----------------------------------------------
C
      IF(FLAGREMNODE==2) THEN
        ALLOCATE(TAGREMLINE(NRTS))
        TAGREMLINE(1:NRTS) = 0
      ENDIF
C  
      AAA = ZERO 
      !ATTENTION A POPTIONMISER POUR NE PAS FAIRE ONE RAZ COMPLET SI NRTS ==0
      MIN_IX=NBX+2
      MIN_IY=NBY+2
      MIN_IZ=NBZ+2
      MAX_IX=1
      MAX_IY=1
      MAX_IZ=1      
      
      !---------------------------------------------------------!
      ! Allocation des tableaux chaines                         !
      !---------------------------------------------------------!
      IF(ITASK == 0)THEN
        MAX_ADD = max(1,4*(NRTS+NRTSR))
        ALLOCATE(LCHAIN_ELEM(1:MAX_ADD))
        ALLOCATE(LCHAIN_NEXT(1:MAX_ADD))
        ALLOCATE(LCHAIN_LAST(1:MAX_ADD))
      END IF   
      
	  CALL MY_BARRIER      !all threads wait for allocation       

      IF(NRTM==0.OR.NRTS==0)THEN
       !ne pas reinitiliser tout le voxel s'il n'y a pas de candidat
        MIN_IX=1
        MIN_IY=1
        MIN_IZ=1        
      END IF

      !---------------------------------------------------------!
      ! recuperation des bornes du domaine                      !
      !---------------------------------------------------------!
      XMIN = XYZM(1,1)
      YMIN = XYZM(2,1)
      ZMIN = XYZM(3,1)
      XMAX = XYZM(4,1)
      YMAX = XYZM(5,1)
      ZMAX = XYZM(6,1)      
c     dev future: xminb plus grand que xmin...
      XMINB = XMIN
      YMINB = YMIN
      ZMINB = ZMIN
      XMAXB = XMAX
      YMAXB = YMAX
      ZMAXB = ZMAX  
C================================================================
C 1   Pour chaque edge, on marque les voxels occupes.
C     Le nombre d edge dans un voxels etant variable, on
C     utilise un tableau chaine.
C     Ces voxels representent le voisinage de l edge.
C     On cherchera ensuite toutes les entites interfacees
C     dans ce voisinage.
C================================================================
      IF(ITASK == 0)THEN
        
        CURRENT_ADD=1 ! premiere adresse

        DO I = 1,NRTS  !si besoin on peut inverser Main/Secnd
        
            IF(STFS(I).EQ.ZERO)CYCLE !on ne retient pas les facettes detruites
                 
            !-------------------------------------------!
            ! Nodes ID for edge (N1,N2)                      !
            !-------------------------------------------!
            N1=IRECTS(1,I)
            N2=IRECTS(2,I)             
            !-------------------------------------------!
            ! Coordinates of the two nodes              !
            !  +Optimisation // recherche les noeuds    !
            !compris dans xmin xmax des elements du proc!           
            !-------------------------------------------!
            XX1=X(1,N1)
            XX2=X(1,N2)
            XMAX_EDGS(I)=MAX(XX1,XX2); IF(XMAX_EDGS(I) < XMIN) CYCLE
            XMIN_EDGS(I)=MIN(XX1,XX2); IF(XMIN_EDGS(I) > XMAX) CYCLE             
            YY1=X(2,N1)
            YY2=X(2,N2) 
            YMAX_EDGS(I)=MAX(YY1,YY2); IF(YMAX_EDGS(I) < YMIN) CYCLE
            YMIN_EDGS(I)=MIN(YY1,YY2); IF(YMIN_EDGS(I) > YMAX) CYCLE         
            ZZ1=X(3,N1)
            ZZ2=X(3,N2)
            ZMAX_EDGS(I)=MAX(ZZ1,ZZ2); IF(ZMAX_EDGS(I) < ZMIN) CYCLE
            ZMIN_EDGS(I)=MIN(ZZ1,ZZ2); IF(ZMIN_EDGS(I) > ZMAX) CYCLE

            !-------------------------------------------!
            !  VOXEL OCCUPIED BY THE EDGE               !
            !-------------------------------------------!
            !Voxel_lower_left_bound for this edge
            IX1=INT(NBX*(XMIN_EDGS(I)-XMINB)/(XMAXB-XMINB))
            IY1=INT(NBY*(YMIN_EDGS(I)-YMINB)/(YMAXB-YMINB))
            IZ1=INT(NBZ*(ZMIN_EDGS(I)-ZMINB)/(ZMAXB-ZMINB))
            IX1=MAX(1,2+MIN(NBX,IX1))
            IY1=MAX(1,2+MIN(NBY,IY1))
            IZ1=MAX(1,2+MIN(NBZ,IZ1))
            !Voxel_upper_right_bound for this edge
            IX2=INT(NBX*(XMAX_EDGS(I)-XMINB)/(XMAXB-XMINB))
            IY2=INT(NBY*(YMAX_EDGS(I)-YMINB)/(YMAXB-YMINB))
            IZ2=INT(NBZ*(ZMAX_EDGS(I)-ZMINB)/(ZMAXB-ZMINB))
            IX2=MAX(1,2+MIN(NBX,IX2))
            IY2=MAX(1,2+MIN(NBY,IY2))
            IZ2=MAX(1,2+MIN(NBZ,IZ2))
          
            !pour reset des voxel
            MIN_IX = MIN(MIN_IX,IX1)
            MIN_IY = MIN(MIN_IY,IY1)
            MIN_IZ = MIN(MIN_IZ,IZ1)          
            MAX_IX = MAX(MAX_IX,IX2)
            MAX_IY = MAX(MAX_IY,IY2)
            MAX_IZ = MAX(MAX_IZ,IZ2)                                                  

            !----------------------------------------------!
            ! EDGE STORAGE FOR EACH VOXEL (CHAINED ARRAY)  !
            !----------------------------------------------!
C
C           VOXEL(i,j,k) LCHAIN_LAST(FIRST)
C           +-----------+------------+ 
C           |  =>FIRST  |   =>LAST   |                    
C           +--+--------+--+---------+
C              |           |
C              |           |
C              |           |
C              |           |   LCHAIN_ELEM(*) LCHAIN_NEXT(*) 
C              |           |   +------------+-----------+
C              +-------------->| edge_id    |   iadd 3  |  1:FIRST --+
C                          |   +------------+-----------+            |
C                          |   |            |              |  2           |
C                          |   +------------+-----------+            |
C                          |   | edge_id    |   iadd 4  |  3 <-------+
C                          |   +------------+-----------+            |
C                          |   | edge_id    |   iadd 6  |  4 <-------+
C                          |   +------------+-----------+            |
C                        |   |            |              |  5           |
C                          |   +------------+-----------+            |
C                          +-->| edge_id    |   0       |  6:LAST <--+
C                            +------------+-----------+
C                              |            |              |  MAX_ADD
C                              +------------+-----------+
C 
            !Pour tous les voxels qu'occupe la brique
            DO IZ = IZ1,IZ2
             DO IY = IY1,IY2
              DO IX = IX1,IX2
 
                FIRST_ADD = VOXEL(IX,IY,IZ)

                IF(FIRST_ADD == 0)THEN
                !voxel encore vide
                 VOXEL(IX,IY,IZ)          = CURRENT_ADD ! adresse dans le tableau chaine de la premiere eddge trouvee occupant le voxel
                 LCHAIN_LAST(CURRENT_ADD) = CURRENT_ADD ! dernier=adresse pour l edge courante
                 LCHAIN_ELEM(CURRENT_ADD) = I               ! edge ID
                 LCHAIN_NEXT(CURRENT_ADD) = 0               ! pas de suivant car dernier de la liste   ! 
                ELSE
                 !voxel contenant deja une edge
                 PREV_ADD                 = LCHAIN_LAST(FIRST_ADD) ! devient l'avant-dernier
                 LCHAIN_LAST(FIRST_ADD)         = CURRENT_ADD                  ! maj du dernier
                 LCHAIN_ELEM(CURRENT_ADD) = I                          ! edge ID
                 LCHAIN_NEXT(PREV_ADD)         = CURRENT_ADD                  ! maj du suivant 0 -> CURRENT_ADD
                 LCHAIN_NEXT(CURRENT_ADD) = 0                          ! pas de suivant car dernier de la liste
                ENDIF

                CURRENT_ADD = CURRENT_ADD+1 

                IF( CURRENT_ADD.GE.MAX_ADD)THEN
                 !OPTIMISATION : suprresion du deallocate/GOTO debut.
                 !REALLOCATE SI PAS ASSEZ DE PLACE : inutile de recommencer de 1 a MAX_ADD-1, on poursuit de MAX_ADD a 2*MAX_ADD
                 MAX_ADD = 2 * MAX_ADD        
                 !print *, "reallocate"
                 LCHAIN_NEXT => IREALLOCATE(LCHAIN_NEXT, MAX_ADD)
                 LCHAIN_ELEM => IREALLOCATE(LCHAIN_ELEM, MAX_ADD)
                 LCHAIN_LAST => IREALLOCATE(LCHAIN_LAST, MAX_ADD)                               
                ENDIF                         

              ENDDO !IX
             ENDDO !IY
            ENDDO !IZ

        ENDDO !DO I=1,NRTS

C================================================================
C 2   Traiter les edges remote. Recuperer les 2 noeuds des
C    edge remotes qui sont dans les memes voxels
C
C           a faire
C
C================================================================
        DO I = NRTS+1,NRTS+NRTSR  !si besoin on peut inverser Main/Secnd       
c            IF(STFS(I).EQ.ZERO)CYCLE !on ne retient pas les facettes detruites, deja fait dans SPMD_MACH::spmd_tri11vox
          J=I-NRTS          
          !-------------------------------------------!
          ! Coordinates of the two nodes              !
          !  +Optimisation // recherche les noeuds    !
          !compris dans xmin xmax des elements du proc!           
          !-------------------------------------------!
          XX1=XREM(1,J)
          XX2=XREM(8,J)
          XMAX_EDGS(I)=MAX(XX1,XX2)      ; IF(XMAX_EDGS(I) < XMIN) CYCLE
          XMIN_EDGS(I)=MIN(XX1,XX2)      ; IF(XMIN_EDGS(I) > XMAX) CYCLE             
          YY1=XREM(2,J)
          YY2=XREM(9,J) 
          YMAX_EDGS(I)=MAX(YY1,YY2)      ; IF(YMAX_EDGS(I) < YMIN) CYCLE
          YMIN_EDGS(I)=MIN(YY1,YY2)      ; IF(YMIN_EDGS(I) > YMAX) CYCLE         
          ZZ1=XREM(3,J)
          ZZ2=XREM(10,J)
          ZMAX_EDGS(I)=MAX(ZZ1,ZZ2)      ; IF(ZMAX_EDGS(I) < ZMIN) CYCLE
          ZMIN_EDGS(I)=MIN(ZZ1,ZZ2)      ; IF(ZMIN_EDGS(I) > ZMAX) CYCLE

          !-------------------------------------------!
          !  VOXEL OCCUPIED BY THE EDGE               !
          !-------------------------------------------!
          !Voxel_lower_left_bound for this edge
          IX1=INT(NBX*(XMIN_EDGS(I)-XMINB)/(XMAXB-XMINB))
          IY1=INT(NBY*(YMIN_EDGS(I)-YMINB)/(YMAXB-YMINB))
          IZ1=INT(NBZ*(ZMIN_EDGS(I)-ZMINB)/(ZMAXB-ZMINB))
          IX1=MAX(1,2+MIN(NBX,IX1))
          IY1=MAX(1,2+MIN(NBY,IY1))
          IZ1=MAX(1,2+MIN(NBZ,IZ1))
          !Voxel_upper_right_bound for this edge
          IX2=INT(NBX*(XMAX_EDGS(I)-XMINB)/(XMAXB-XMINB))
          IY2=INT(NBY*(YMAX_EDGS(I)-YMINB)/(YMAXB-YMINB))
          IZ2=INT(NBZ*(ZMAX_EDGS(I)-ZMINB)/(ZMAXB-ZMINB))
          IX2=MAX(1,2+MIN(NBX,IX2))
          IY2=MAX(1,2+MIN(NBY,IY2))
          IZ2=MAX(1,2+MIN(NBZ,IZ2))
          
          !pour reset des voxel
          MIN_IX = MIN(MIN_IX,IX1)
          MIN_IY = MIN(MIN_IY,IY1)
          MIN_IZ = MIN(MIN_IZ,IZ1)          
          MAX_IX = MAX(MAX_IX,IX2)
          MAX_IY = MAX(MAX_IY,IY2)
          MAX_IZ = MAX(MAX_IZ,IZ2)                                                  

          !----------------------------------------------!
          ! EDGE STORAGE FOR EACH VOXEL (CHAINED ARRAY)  !
          !----------------------------------------------!
C
C           VOXEL(i,j,k) LCHAIN_LAST(FIRST)
C           +-----------+------------+ 
C           |  =>FIRST  |   =>LAST   |                    
C           +--+--------+--+---------+
C              |           |
C              |           |
C              |           |
C              |           |   LCHAIN_ELEM(*) LCHAIN_NEXT(*) 
C              |           |   +------------+-----------+
C              +-------------->| edge_id    |   iadd 3  |  1:FIRST --+
C                          |   +------------+-----------+            |
C                          |   |            |              |  2           |
C                          |   +------------+-----------+            |
C                          |   | edge_id    |   iadd 4  |  3 <-------+
C                          |   +------------+-----------+            |
C                          |   | edge_id    |   iadd 6  |  4 <-------+
C                          |   +------------+-----------+            |
C                        |   |            |              |  5           |
C                          |   +------------+-----------+            |
C                          +-->| edge_id    |   0       |  6:LAST <--+
C                            +------------+-----------+
C                              |            |              |  MAX_ADD
C                              +------------+-----------+
C 
            !Pour tous les voxels qu'occupe la brique
          DO IZ = IZ1,IZ2
           DO IY = IY1,IY2
            DO IX = IX1,IX2           
              FIRST_ADD = VOXEL(IX,IY,IZ)
              IF(FIRST_ADD == 0)THEN
               !voxel encore vide
                VOXEL(IX,IY,IZ)          = CURRENT_ADD ! adresse dans le tableau chaine de la premiere eddge trouvee occupant le voxel
                LCHAIN_LAST(CURRENT_ADD) = CURRENT_ADD ! dernier=adresse pour l edge courante
                LCHAIN_ELEM(CURRENT_ADD) = I               ! edge ID
                LCHAIN_NEXT(CURRENT_ADD) = 0               ! pas de suivant car dernier de la liste   ! 
              ELSE
                !voxel contenant deja une edge
                PREV_ADD                 = LCHAIN_LAST(FIRST_ADD) ! devient l'avant-dernier
                LCHAIN_LAST(FIRST_ADD)         = CURRENT_ADD                  ! maj du dernier
                LCHAIN_ELEM(CURRENT_ADD) = I                          ! edge ID
                LCHAIN_NEXT(PREV_ADD)         = CURRENT_ADD                  ! maj du suivant 0 -> CURRENT_ADD
                LCHAIN_NEXT(CURRENT_ADD) = 0                          ! pas de suivant car dernier de la liste
              ENDIF
              CURRENT_ADD = CURRENT_ADD+1 
              IF( CURRENT_ADD.GE.MAX_ADD)THEN
                !OPTIMISATION : suprresion du deallocate/GOTO debut.
                !REALLOCATE SI PAS ASSEZ DE PLACE : inutile de recommencer de 1 a MAX_ADD-1, on poursuit de MAX_ADD a 2*MAX_ADD
                MAX_ADD = 2 * MAX_ADD        
                !print *, "reallocate remote"
                LCHAIN_NEXT => IREALLOCATE(LCHAIN_NEXT, MAX_ADD)
                LCHAIN_ELEM => IREALLOCATE(LCHAIN_ELEM, MAX_ADD)
                LCHAIN_LAST => IREALLOCATE(LCHAIN_LAST, MAX_ADD)                               
              ENDIF                         
            ENDDO !IX
           ENDDO !IY
          ENDDO !IZ

        ENDDO !DO NRTS+1,NRTS+NRTSR


      END IF !(ITASK==0)
      
      CALL MY_BARRIER  !le tableau voxel doit etre rempli avant de continuer 
                       !max_add doit etre le meme pour tout le monde.
C================================================================
C 3   A partir des voxels occupes par une edge main, on est en
C     mesure de connaitre toutes les edges escalves dans ce voisinage.
C     Ce qui permet de creer des couples cancidats pour le contact
C     Si la penetration est positive.
C================================================================
      NEDG   = 0 
      I_STOK = 0
      MARGE = TZINF - MAX(GAP,DRAD)

      DO IEDG=1,NRTM

        IF(STFM(IEDG) == ZERO)CYCLE   ! on ne retient pas les facettes detruites

c       AAA = ZERO !MARGE
        AAA = TZINF
        IF(IGAP == 0)THEN
          AAA = TZINF          
        ELSE
          AAA = MARGE+
     .        MAX(GAPMIN,BGAPSMX+GAP_M(IEDG),DRAD)
        ENDIF

        
        !-------------------------------------------!
        !    (N1,N2) is the current main edge     !
        !-------------------------------------------!
        N1 = IRECTM(1,IEDG)
        N2 = IRECTM(2,IEDG)
        MM1 = ITAB(N1)
        MM2 = ITAB(N2)

        !-------------------------------------------!
        !     X-coordinates of the four nodes       !
        !-------------------------------------------!
        XX1=X(1,N1)
        XX2=X(1,N2)
        YY1=X(2,N1)
        YY2=X(2,N2)
        ZZ1=X(3,N1)
        ZZ2=X(3,N2)

        XMAX_EDGM(IEDG)=MAX(XX1,XX2) ! +TZINF
        XMIN_EDGM(IEDG)=MIN(XX1,XX2) ! -TZINF
        YMAX_EDGM(IEDG)=MAX(YY1,YY2) ! +TZINF
        YMIN_EDGM(IEDG)=MIN(YY1,YY2) ! -TZINF
        ZMAX_EDGM(IEDG)=MAX(ZZ1,ZZ2) ! +TZINF
        ZMIN_EDGM(IEDG)=MIN(ZZ1,ZZ2) ! -TZINF

        !-------------------------------------------!
        !  VOXEL OCCUPIED BY THE BRICK              !
        !-------------------------------------------!
        !Voxel_lower_left_bound for this element---+
        IX1=INT(NBX*(XMIN_EDGM(IEDG)-AAA-XMINB)/(XMAXB-XMINB))
        IY1=INT(NBY*(YMIN_EDGM(IEDG)-AAA-YMINB)/(YMAXB-YMINB))
        IZ1=INT(NBZ*(ZMIN_EDGM(IEDG)-AAA-ZMINB)/(ZMAXB-ZMINB))
        IX1=MAX(1,2+MIN(NBX,IX1))
        IY1=MAX(1,2+MIN(NBY,IY1))
        IZ1=MAX(1,2+MIN(NBZ,IZ1))
        !Voxel_upper_right_bound for this element---+
        IX2=INT(NBX*(XMAX_EDGM(IEDG)+AAA-XMINB)/(XMAXB-XMINB))
        IY2=INT(NBY*(YMAX_EDGM(IEDG)+AAA-YMINB)/(YMAXB-YMINB))
        IZ2=INT(NBZ*(ZMAX_EDGM(IEDG)+AAA-ZMINB)/(ZMAXB-ZMINB))
        IX2=MAX(1,2+MIN(NBX,IX2))
        IY2=MAX(1,2+MIN(NBY,IY2))
        IZ2=MAX(1,2+MIN(NBZ,IZ2))

        DEJA = 0     ! l edge n est pas encore candidate.
        I_STOK_BAK = I_STOK
C
C--- IREMGAP - tag of deactivated lines
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE(2*(IEDG-1)+1)
          L = KREMNODE(2*(IEDG-1)+2)-1
          DO M=K,L
            TAGREMLINE(REMNODE(M)) = 1
          ENDDO
        ENDIF
C
        !ON PARCOURS A NOUVEAU LES EDGES SECND DANS LE VOISINAGE DE L EDGE main IEDG
        !ON CONSTITUE ICI ONE COUPLE

        DO IZ = IZ1,IZ2
         DO IY = IY1,IY2
          DO IX = IX1,IX2        
          
            CHAIN_ADD = VOXEL(IX,IY,IZ)       ! adresse dans le tableau chaine de la premiere edge stoquee dans le voxel
            DO WHILE(CHAIN_ADD /= 0)          ! BOUCLE SUR LES EDGES DU VOXEL COURANT
              I = LCHAIN_ELEM(CHAIN_ADD)       ! numeros des edge_id balayes dans le voxel courant

              !secnd edge nodes, exclure couples avec noeud commun
              IF (I.LE.NRTS)THEN
                SS1=ITAB(IRECTS(1,I))
                SS2=ITAB(IRECTS(2,I))              
              ELSE 
                SS1=IREM(2,I-NRTS)
                SS2=IREM(3,I-NRTS)                                                        
              END IF
              
              IF( (SS1.EQ.MM1).OR.(SS1.EQ.MM2).OR.
     .            (SS2.EQ.MM1).OR.(SS2.EQ.MM2)    )THEN
                CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                CYCLE
              END IF
              
              !unicite des couples        
              IF(IAUTO==1 .AND. MM1<SS1 )THEN
                CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                CYCLE
              END IF                      

C             IREMPGAP
              IF (FLAGREMNODE == 2) THEN
                IF (I <= NRTS) THEN
C-                Local Taged lines are removed
                  IF(TAGREMLINE(I)==1) THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  ENDIF
                ELSE
C-                Remote lines are identified by nodes
                  K = KREMNODE(2*(IEDG-1)+2)
                  L = KREMNODE(2*(IEDG-1)+3)-1
                  REMOVE_REMOTE = 0
                  DO M=K,L,2
                    IF ((SS1==REMNODE(M)).AND.(SS2==REMNODE(M+1))) REMOVE_REMOTE = 1
                  ENDDO
                  IF (REMOVE_REMOTE==1) THEN
                    CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                    CYCLE
                  ENDIF
                ENDIF
              ENDIF  
  
              I_STOK = I_STOK + 1 !on dispose d'un candidat
              PROV_S(I_STOK) = I    !edge secnd
              PROV_M(I_STOK) = IEDG !edge main
                                                            
              !print *, "candidat:",  IEDG, I
              IF(DEJA==0) NEDG = NEDG + 1     !nombre d edges candidate au calcul de contact (debug)                 
              DEJA=1                          !l edge main IEDG fait l'objet d'une ecriture de candidat. On compte les edges main faisant l'objet dun couple candidate : on ne doit plus incrementer NEDG pour les autres edge secnd testees.
              CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
C-----------------------------------------------------
              IF(I_STOK.GE.NVSIZ)THEN
                CALL I11STO_VOX(
     1               NVSIZ ,IRECTS,IRECTM,X     ,II_STOK,
     2               CAND_S,CAND_M,NSN4  ,NOINT ,MARGE,
     3               I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM ,
     4               CHAINE,NRTS  ,ITAB  ,IFPEN ,IFORM,
     5               GAPMIN,DRAD  ,IGAP, GAP_S, GAP_M,
     7               GAP_S_L, GAP_M_L )

                IF(I_MEM.EQ.2) THEN
                  !print *, "too much candidates"
c                    IF (ITASK==0)II_STOK=ZERO       
                  GOTO 1000
                END IF!(I_MEM.EQ.2)
                I_STOK = I_STOK-NVSIZ
C  !DIR$ ASSUME (I_STOK < NVSIZ) 
                DO J=1,I_STOK
                  PROV_S(J) = PROV_S(J+NVSIZ)
                  PROV_M(J) = PROV_M(J+NVSIZ)
                ENDDO           
              ENDIF                 
C-----------------------------------------------------                   

            ENDDO !NEXT WHILE(CHAIN_ADD /= 0)
          ENDDO   !NEXT IZ
         ENDDO    !NEXT IY
        ENDDO     !NEXT IZ
 
C--- IREMGAP - clean of tagremline
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE(2*(IEDG-1)+1)
          L = KREMNODE(2*(IEDG-1)+2)-1
          DO M=K,L
            TAGREMLINE(REMNODE(M)) = 0
          ENDDO
        ENDIF

      ENDDO       !NEXT IEDG

C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
 
      IF(I_STOK.NE.0)CALL I11STO_VOX(
     1              I_STOK,IRECTS,IRECTM,X     ,II_STOK,
     2              CAND_S,CAND_M,NSN4  ,NOINT ,MARGE  ,
     3              I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM  ,
     4              CHAINE,NRTS  ,ITAB  ,IFPEN ,IFORM  ,
     5              GAPMIN,DRAD  ,IGAP, GAP_S  ,GAP_M  ,
     7              GAP_S_L, GAP_M_L )

    
C================================================================
C 4   remise a zero des noeuds dans les boites et desallocation
C================================================================

 1000 CONTINUE            

      CALL MY_BARRIER  !ne pas desalloue tant que les autres threads n'ont pas fini de travailler  
       

      ! peut etre otpimisee : ne pas reinitialiser tout le tableau (plusieurs solutions possibles)  
      !! VOXEL( MIN_IX:MAX_IX, MIN_IY:MAX_IY, MIN_IZ:MAX_IZ ) = 0        
      TMIN(1) = MIN_IX
      TMIN(2) = MIN_IY
      TMIN(3) = MIN_IZ

      TMAX(1) = MAX_IX
      TMAX(2) = MAX_IY
      TMAX(3) = MAX_IZ
      
      IF (ITASK==0)THEN   
        !RESET VOXEL WITHIN USED RANGE ONLY
        DO K= TMIN(3),TMAX(3)
          DO J= TMIN(2),TMAX(2)
             DO I= TMIN(1),TMAX(1)
                VOXEL(I,J,K) = 0
             END DO
          END DO
        END DO   
        !CHAINED LIST DEALLOCATION
        DEALLOCATE(LCHAIN_NEXT)
        DEALLOCATE(LCHAIN_ELEM)
        DEALLOCATE(LCHAIN_LAST)
        IF(FLAGREMNODE==2) DEALLOCATE(TAGREMLINE)          
      END IF    
  
C___________________________________________________________________________________________________________

      RETURN   
      END



