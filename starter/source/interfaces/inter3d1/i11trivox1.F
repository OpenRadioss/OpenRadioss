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
Chd|  I11TRIVOX1                    source/interfaces/inter3d1/i11trivox1.F
Chd|-- called by -----------
Chd|        I11BUC_VOX1                   source/interfaces/inter3d1/i11buc1.F
Chd|-- calls ---------------
Chd|        I11RESETVOXEL1                source/interfaces/inter3d1/i11trivox1.F
Chd|        I11STO_VOX1                   source/interfaces/inter3d1/i11sto.F
Chd|        REALLOC_MOD                   share/modules1/realloc_mod.F  
Chd|        TRI11                         share/modules1/tri11_mod.F    
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I11TRIVOX1(
     1      IRECTS, IRECTM , X     , NRTM     ,
     2      XYZM  , II_STOK, CAND_S, CAND_M   ,NSN    ,
     3      NOINT , TZINF  , I_MEM , ADDCM ,  IADFIN  ,
     5      CHAINE, NRTS   , ITAB  , MULTIMP, 
     6      IAUTO , VOXEL  , NBX   , NBY      ,NBZ    ,
     7      GAPMIN, DRAD   , MARGE , GAP_S    , GAP_M ,
     8      GAP_S_L,GAP_M_L,IGAP   ,FLAGREMNODE,KREMNODE,
     3      REMNODE,DGAPLOAD)
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
#include      "param_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
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
C     NSN         NSN TAILLE MAX ADMISE MAINTENANT POUR LES
C                  COUPLES NOEUDS,ELT CANDIDATS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::
     .         NRTM,NRTS,MULTIMP,IADFIN,IGAP,
     .         NSN,NOINT,ITAB(*),NBX,NBY,NBZ,IAUTO,
     .         IRECTS(2,NRTS),IRECTM(2,NRTM),FLAGREMNODE
      INTEGER ITASK
      INTEGER, INTENT(INOUT) ::
     .         CAND_S(*),CAND_M(*),ADDCM(*),CHAINE(2,*), 
     .         VOXEL(1:NBX+2,1:NBY+2,1:NBZ+2), I_MEM,II_STOK,
     .         KREMNODE(*),REMNODE(*)
      my_real
     .       ,INTENT(IN) ::
     .         X(3,*),XYZM(6,*),
     .         GAPMIN, DRAD, MARGE, TZINF, DGAPLOAD,
     .         GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
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
     .        NEDG, DEJA , MAX_ADD ,II_STOK0, M
      my_real
     .        XX1, XX2,
     .        XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, 
     .        YY1,YY2,ZZ1,ZZ2,
     .        AAA, DD,
     .        XMAX_EDGS(NRTS), XMIN_EDGS(NRTS),   !cotes min/max des arrete seconds et mains
     .        YMAX_EDGS(NRTS), YMIN_EDGS(NRTS),
     .        ZMAX_EDGS(NRTS), ZMIN_EDGS(NRTS),
     .        XMAX_EDGM(NRTM), XMIN_EDGM(NRTM),
     .        YMAX_EDGM(NRTM), YMIN_EDGM(NRTM),
     .        ZMAX_EDGM(NRTM), ZMIN_EDGM(NRTM),     
     .        XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB
C
      INTEGER, DIMENSION(:), ALLOCATABLE ::  TAGREMLINE    
C-----------------------------------------------
C
C--- IREMGAP - array for tag of deactivated lines
      IF(FLAGREMNODE==2)THEN
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
      MAX_ADD = max(1,4*(NRTS))
      ALLOCATE(LCHAIN_ELEM(1:MAX_ADD))
      ALLOCATE(LCHAIN_NEXT(1:MAX_ADD))
      ALLOCATE(LCHAIN_LAST(1:MAX_ADD))
      

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

C=======================================================================
C 1   Pour chaque edge, on marque les voxels occupes.
C     Le nombre d edge dans un voxels etant variable, on
C     utilise un tableau chaine.
C     Ces voxels representent le voisinage de l edge.
C     On cherchera ensuite toutes les entites interfacees
C     dans ce voisinage.
C=======================================================================
        
        CURRENT_ADD=1 ! premiere adresse

        DO I = 1,NRTS  !si besoin on peut inverser Main/Secnd
        
c           IF(STFS(I).EQ.ZERO)CYCLE !on ne retient pas les facettes detruites
                 
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
C                          |   |            |              |  5           |
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


C=======================================================================
C 3   A partir des voxels occupes par une edge main, on est en
C     mesure de connaitre toutes les edges escalves dans ce voisinage.
C     Ce qui permet de creer des couples cancidats pour le contact
C     Si la penetration est positive.
C=======================================================================
      NEDG   = 0 
      I_STOK = 0
                      
      DO IEDG=1,NRTM

        AAA = ZERO !MARGE
        
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
        XMAX_EDGM(IEDG)=MAX(XX1,XX2)+TZINF
        XMIN_EDGM(IEDG)=MIN(XX1,XX2)-TZINF
        YY1=X(2,N1)
        YY2=X(2,N2)
        YMAX_EDGM(IEDG)=MAX(YY1,YY2)+TZINF
        YMIN_EDGM(IEDG)=MIN(YY1,YY2)-TZINF
        ZZ1=X(3,N1)
        ZZ2=X(3,N2)
        ZMAX_EDGM(IEDG)=MAX(ZZ1,ZZ2)+TZINF
        ZMIN_EDGM(IEDG)=MIN(ZZ1,ZZ2)-TZINF

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

C--- IREMGAP - tag of deactivated lines
        IF(FLAGREMNODE==2)THEN
          K = KREMNODE(IEDG)
          L = KREMNODE(IEDG+1)-1
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
              SS1=ITAB(IRECTS(1,I))
              SS2=ITAB(IRECTS(2,I))              
              
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

C             IREMPGAP - Taged lines are removded
              IF(FLAGREMNODE==2)THEN
                IF(TAGREMLINE(I)==1) THEN
                  CHAIN_ADD = LCHAIN_NEXT(CHAIN_ADD)
                  CYCLE
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
                  CALL I11STO_VOX1(
     1              NVSIZ,IRECTS,IRECTM,X     ,II_STOK ,
     2              CAND_S,CAND_M,NSN   ,NOINT ,MARGE  ,
     3              I_MEM ,PROV_S,PROV_M,MULTIMP,ADDCM ,
     4              CHAINE,IADFIN,GAPMIN,DRAD   ,IGAP  ,
     5              GAP_S ,GAP_M ,GAP_S_L,GAP_M_L,DGAPLOAD)

                IF(I_MEM.EQ.2) THEN
                      II_STOK=ZERO                                            
                  GOTO 1000
                END IF!(I_MEM.EQ.2)
                I_STOK = I_STOK-NVSIZ
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
          K = KREMNODE(IEDG)
          L = KREMNODE(IEDG+1)-1
          DO M=K,L
            TAGREMLINE(REMNODE(M)) = 0
          ENDDO
        ENDIF

      ENDDO       !NEXT IEDG


C-------------------------------------------------------------------------
C     FIN DU TRI
C-------------------------------------------------------------------------
 
      IF(I_STOK.NE.0)CALL I11STO_VOX1(
     1              I_STOK,IRECTS,IRECTM,X     ,II_STOK,
     2              CAND_S,CAND_M,NSN   ,NOINT ,MARGE  ,
     3              I_MEM ,PROV_S,PROV_M,MULTIMP,ADDCM ,
     4              CHAINE,IADFIN,GAPMIN,DRAD   ,IGAP  ,
     5              GAP_S ,GAP_M ,GAP_S_L,GAP_M_L,DGAPLOAD)

    
c      WRITE(6,*) __FILE__,__LINE__,I_STOK,II_STOK
C=======================================================================
C 4   remise a zero des noeuds dans les boites et desallocation
C=======================================================================

 1000 CONTINUE            

       

      ! peut etre otpimisee : ne pas reinitialiser tout le tableau (plusieurs solutions possibles)  
      !! VOXEL( MIN_IX:MAX_IX, MIN_IY:MAX_IY, MIN_IZ:MAX_IZ ) = 0        

      CALL I11RESETVOXEL1(
     1      (/MIN_IX, MIN_IY, MIN_IZ/), 
     .                               (/MAX_IX, MAX_IY, MAX_IZ/),
     2      NBX, NBY, NBZ, VOXEL  )
   
      DEALLOCATE(LCHAIN_NEXT)
      DEALLOCATE(LCHAIN_ELEM)
      DEALLOCATE(LCHAIN_LAST)
      IF(FLAGREMNODE==2) DEALLOCATE(TAGREMLINE)       
C___________________________________________________________________________________________________________

      RETURN   
      END



Chd|====================================================================
Chd|  I11RESETVOXEL1                source/interfaces/inter3d1/i11trivox1.F
Chd|-- called by -----------
Chd|        I11TRIVOX1                    source/interfaces/inter3d1/i11trivox1.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I11RESETVOXEL1(
     1      tMIN, tMAX ,
     2      NBX  , NBY  , NBZ, VOXEL)
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
!#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, intent(in) :: tMIN(3), tMAX(3),
     .                       NBX, NBY, NBZ
     
      INTEGER, INTENT(INOUT) ::
     .                       VOXEL(1:NBX+2,1:NBY+2,1:NBZ+2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,Z_FIRST,Z_LAST,SIZE
C-----------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------

       SIZE = tMAX(3) - tMIN(3) +1
       Z_FIRST = tMIN(3)
       Z_LAST  = SIZE + tMIN(3)-1
       
       DO K= Z_FIRST , Z_LAST 
          DO J= tMIN(2),tMAX(2)
             DO I= tMIN(1),tMAX(1)
                VOXEL(I,J,K) = 0
             END DO
          END DO
       END DO

       RETURN

      END
