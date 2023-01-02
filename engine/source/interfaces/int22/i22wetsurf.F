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
Chd|  I22WETSURF                    source/interfaces/int22/i22wetsurf.F
Chd|-- called by -----------
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22WETSURF(
     1                  JLT    ,CAND_B ,CAND_E  ,CB_LOC ,CE_LOC ,
     2                  X1     ,X2     ,X3      ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4      ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI      ,YI     ,ZI     ,
     5                  NX1    ,NX2    ,NX3     ,NX4    ,NY1    ,
     6                  NY2    ,NY3    ,NY4     ,NZ1    ,NZ2    ,
     7                  NZ3    ,NZ4    ,LB1     ,LB2    ,LB3    ,
     8                  LB4    ,LC1    ,LC2     ,LC3    ,LC4    ,
     9                  P1     ,P2     ,P3      ,P4     ,IX1    ,
     A                  IX2    ,IX3    ,IX4     ,NSVG   ,STIF   ,
     B                  JLT_NEW,GAPV   ,INACTI  ,CAND_P ,N_SCUT ,
     C                  INDEX  ,VXI    ,VYI    ,
     D                  VZI    ,MSI    ,KINI    ,SURF   ,IBAG   ,
     E                  ITAB   ,IRECT  ,I_STOK  ,IXS    ,NFT    ,
     F                  CoG    ,Seff   ,Delta   ,X)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "inter22.inc"
#include      "i22edges.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, JLT_NEW,INACTI, CAND_B(*),CB_LOC(MVSIZ),NFT,
     .        CAND_E(*),CE_LOC(MVSIZ), NSVG(MVSIZ), KINI(*), IXS(NIXS,*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INDEX(MVSIZ),IBAG, ITAB(*),IRECT(4,*),I_STOK
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), 
     .     GAPV(MVSIZ), CAND_P(*),
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .     SURF(3,*), N_SCUT(1:3,NBCUT_MAX,MVSIZ),COG(3,NBCUT_MAX,MVSIZ),
     .     Seff(NBCUT_MAX,MVSIZ),delta(4,NBCUT_MAX,MVSIZ),X(3,NUMNOD)
     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      
      TYPE POLYGON
        my_real :: NODE(16,2)  !index1 : numpt, index2:coor
        INTEGER :: NumNodes          
      END TYPE

      INTEGER :: I, J, NIN, IB, IFT,ILT, K, Q, L
      INTEGER :: NBCUT    , IE, IEDG_LAG, IEDG_SCUT
      INTEGER :: ICUT, IDX_LAG(7), IDX_SCUT(7)
      INTEGER :: TAG_SCUT(4,NBCUT_MAX), TAG_NOD(4,NBCUT_MAX) !taging lagrangian node regarding Pcut (proj criteria), 4 nodes, 2= max(nbcut)
      INTEGER :: IS_ON_SCUT(NBCUT_MAX), NP, Snod_On_Lag(6)
C      INTEGER :: TAG_EL(NBCUT_MAX)
      INTEGER :: IADD(4,6), NCUT(NBCUT_MAX), NP_RECT(MVSIZ),ITMP, NN
      INTEGER :: HULL_PT, HULL_IDX(24)
      INTEGER :: NP_INTER, NP_LAG, NP_SURF, NODE_NUM,brickID, ListP(4), NBCUT_ADD
      my_real :: NODE_XY(2,24), NODE_XYZ(3,24), PS, MARGE
      my_real :: DIST_PCUT(NBCUT_MAX, 4,I_STOK)  !2 PCUT, 4 POINTS, NCANDE couples
      my_real :: DMIN
      my_real :: NORMAL(3), BASIS(3), PH(3,NBCUT_MAX,4),P(3,4),  A,B,C,D , A2,B2,C2 ,A2B2, L2, DIST(4), PPh(3,4)
      my_real :: distance(4), edge(4), Area_tria(4)
      my_real :: SH(3,NBCUT_MAX,8), S(3,8)
      my_real :: SCUT_Pts(3,6) !Scut points
      my_real :: K1(2),K2(2),K3(2),K4(2)
      my_real :: POINT(3),RATIO,COG3,P_Grav(3,NBCUT_MAX,MVSIZ),ss2(4), s2, Sel(MVSIZ)
      my_real
     .     X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ)
      my_real
     .    XI0,SX0,
     .    YI0,SY0,
     .    ZI0,SZ0
      my_real
     .     NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .     NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .     NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
      my_real
     .     VAL1,VAL2,VAL3, V1(3), V2(3), V3(3), M(3,3,NBCUT_MAX),VEC(3), ! basis vector for each intersectionp plane
     .     POSj, POS, VAL,        !position sur le segment : produit scalaire non normalise
     .     InterP(2), WET_RATIO(0:4), Z(3), Stria(4),  Dsum, NumGrav, Pgrav(3),
     .     Cumul_VAL1, Cumul_VAL2

      my_real,DIMENSION(3) :: pt1,pt2,pt3,pt4
     
      INTEGER :: N_INTP_LAG(1:4,NBCUT_MAX)    ! number of intersection point on lagrandian edge id=index1 !index3:ICUT
!      TYPE(LAGRANGIAN_EDGE) :: EDG_LAG(1:4, 1:6 , NBCUT_MAX)  !index1: id  !index2: up to 6 intersection points (exact value is N_INTP_LAG(index1)) is Scut edge id  !index3:ICUT
      
 
      LOGICAL BOOL, ATYP1, ATYP2, ATYP3
      
      INTEGER :: ITYP
      
      my_real :: XP1(3), XP2(3), XP3(3), XP4(3)
      
       

      INTEGER,EXTERNAL :: ISONPOLYG
      
!      INTEGER IDX(7,6)
      INTEGER IAD1,ILAG0,ISCUT0,ILAGk,ISCUTk,IIB,NTRIA,
     .        JPOS(6) !index des poitsions non nulles sur IADD(id_lagrangian_edge,:)
    
      TYPE(POLYGON) :: PolySCUT, PolyTria(4), POLYresult
     
      INTERFACE   
        FUNCTION I22AERA(NPTS,P, C)
          INTEGER :: NPTS
          my_real :: P(3,NPTS), I22AERA(3), C(3)
        END FUNCTION           
      END INTERFACE  
      
      integer,external :: GetProjectedFaceType   
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C  Aim    : compute Wet surface of a lagrangian face
C  method : lagrangian segment and Cut Surface are projected on Cut surface associated plane. Polygonal clipping also enables
C           to estimate wet ratio VAL1/VAL2 where VAL1 is clipped polygon and VAL2 is full 2D segment
C
C           +--------------+
C         /                 \
C       /              oooooooooooooo
C      +                 o    \     o
C      |                   o   \    o
C      |    SCUT (2D)        o  +   o
C      |                       o|   o
C      |                        | o o
C      +------------------------+   o
C                                   o  o
C                                   o    o 
C                                   o      o   Projected 3D-Segment (2D)
C                                   o        o
C                                   oooooooooooo
C
C
C
C
C
           ! CE_LOC(1:MVSIZ) = CAND_E(NFT+(1:MVSIZ))   (local)
           ! CB_LOC(1:MVSIZ) = CAND_B(NFT+(1:MVSIZ))   (local) 

           !              3         3
           ! Proj_ICUT : R  -----> R
           !             P |-----> Ph = Proj_ICUT(P)

           !                3  3          1
           ! DIST_ICUT : ( R ,R ) -----> R
           !               P,Ph  |-----> <P-Ph, P-Ph>
 
           ! PROJECTION CRITERIA
           !   based on shortest distances.
           !   Each point of lagrangian face are projected to all cut planes.
           !   If one plane has the shortest distance, then the full face will be projected to the Cut Plane 
           ! (NBCUT = 2 but could be extended later):
           !             +---+---+---+---+---+---+---+---+
           !             | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | -> ICUT : cut plane id
           !    +--------+---+---+---+---+---+---+---+---+
           !    | Node_1 |   | T |   |   |   |   |   |   |
           !    +--------+---+---+---+---+---+---+---+---+           
           !    | Node_2 |   | T |   |   |   |   |   |   |
           !    +--------+---+---+---+---+---+---+---+---+
           !    | Node_3 |   |   |   |   |   |   |   | T |
           !    +--------+---+---+---+---+---+---+---+---+
           !    | Node_4 |   |   |   |   |   |   |   | T |
           !    +--------+---+---+---+---+---+---+---+---+                                 
           !                   |                       |
           !               IE going to be              |
           !               projected on Pcut_2         |
           !                                           |  
           !                                   IE also going to be
           !                                   projected on Pcut_8    
           
           ! EXAMPLE
           !        
           !       Pcut_1
           !        |
           !        |
           !   +----0--------------------+   Pcut_2
           !   |    |                    | /
           !   |    |                    |/
           !   |    |                    0
           !   |    |                   /|
           !   |    |                  / |               
           !   |    |                 /  |
           !   |    |                /   |
           !   |    |               /    |
           !   |    |              /     |
           ! Node_1 |             /      |
           !   |  + |   Pcut>8   /       |
           !   |   \|     |     /        |                                                                                                                         
           !   +----0----------0---------+    
           !         \        /
           !       IE \      /    
           !           \
           !            \
           !             + 
           !           Node_2                                 
C--------------------------------------------------------
C   S o u r c e    L i n e s
C--------------------------------------------------------
      NIN  = 1
      CE_LOC(1:JLT) = CAND_E(1:JLT)
      CB_LOC(1:JLT) = CAND_B(1:JLT)      

C--------------------------------------------------------
C  REFERENCE POINT : QUADRAGLE CENTROID OR TRIA THIRD NODE
C--------------------------------------------------------

       DO I=1,JLT
        IF(IX3(I)/=IX4(I))THEN
         NP_RECT(I) = 4
         X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
         Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
         Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
        ELSE
         NP_RECT(I) = 3
         X0(I) = X3(I)
         Y0(I) = Y3(I)
         Z0(I) = Z3(I)
        ENDIF
       ENDDO
C--------------------------------------------------------
C  CALCUL DES NORMALES SUR LES FACETTES LAGRANGIENNE
C--------------------------------------------------------
C
       DO I=1,JLT

         IF(NP_RECT(I)==4)THEN
         
          Sel(I)  = ZERO
         
          X01(I)  = X1(I) - X0(I)
          Y01(I)  = Y1(I) - Y0(I)
          Z01(I)  = Z1(I) - Z0(I)

          X02(I)  = X2(I) - X0(I)
          Y02(I)  = Y2(I) - Y0(I)
          Z02(I)  = Z2(I) - Z0(I)

          X03(I)  = X3(I) - X0(I)
          Y03(I)  = Y3(I) - Y0(I)
          Z03(I)  = Z3(I) - Z0(I)

          X04(I)  = X4(I) - X0(I)
          Y04(I)  = Y4(I) - Y0(I)
          Z04(I)  = Z4(I) - Z0(I)

          SX0     = Y01(I)*Z02(I) - Z01(I)*Y02(I)
          SY0     = Z01(I)*X02(I) - X01(I)*Z02(I)
          SZ0     = X01(I)*Y02(I) - Y01(I)*X02(I)
          SS2(1)  = SQRT(MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0))   !2S_tria1
          Sel(I)  = Sel(I) + SS2(1)
          SS2(1)  = ONE/SS2(1)

          NNX1(I) = SX0 * SS2(1)
          NNY1(I) = SY0 * SS2(1)
          NNZ1(I) = SZ0 * SS2(1)

          SX0     = Y02(I)*Z03(I) - Z02(I)*Y03(I)
          SY0     = Z02(I)*X03(I) - X02(I)*Z03(I)
          SZ0     = X02(I)*Y03(I) - Y02(I)*X03(I)
          SS2(2)  = SQRT(MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0))   !2S_tria2
          Sel(I)  = Sel(I) + SS2(2)  
          SS2(2)  = ONE/SS2(2)        

          NNX2(I) = SX0 * SS2(2)
          NNY2(I) = SY0 * SS2(2)
          NNZ2(I) = SZ0 * SS2(2)

          SX0     = Y03(I)*Z04(I) - Z03(I)*Y04(I)
          SY0     = Z03(I)*X04(I) - X03(I)*Z04(I)
          SZ0     = X03(I)*Y04(I) - Y03(I)*X04(I)
          SS2(3)  = SQRT(MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0))   !2S_tria3
          Sel(I)  = Sel(I) + SS2(3)  
          SS2(3)  = ONE/SS2(3)           

          NNX3(I) = SX0 * SS2(3)
          NNY3(I) = SY0 * SS2(3)
          NNZ3(I) = SZ0 * SS2(3)

          SX0     = Y04(I)*Z01(I) - Z04(I)*Y01(I)
          SY0     = Z04(I)*X01(I) - X04(I)*Z01(I)
          SZ0     = X04(I)*Y01(I) - Y04(I)*X01(I)
          SS2(4)  = SQRT(MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0))   !2S_tria4
          Sel(I)  = Sel(I) + SS2(4)  
          SS2(4)  = ONE/SS2(4)           

          NNX4(I) = SX0 * SS2(4)
          NNY4(I) = SY0 * SS2(4)
          NNZ4(I) = SZ0 * SS2(4)
          
          Sel(I)  = HALF * Sel(I)   !(2*1/4 = 1/2)
          
         ELSE ! => NP_RECT(I)==3
         
          X01(I)  = X1(I) - X0(I)
          Y01(I)  = Y1(I) - Y0(I)
          Z01(I)  = Z1(I) - Z0(I)

          X02(I)  = X2(I) - X0(I)
          Y02(I)  = Y2(I) - Y0(I)
          Z02(I)  = Z2(I) - Z0(I)

          SX0     = Y01(I)*Z02(I) - Z01(I)*Y02(I)
          SY0     = Z01(I)*X02(I) - X01(I)*Z02(I)
          SZ0     = X01(I)*Y02(I) - Y01(I)*X02(I)
          SS2(1)  = SQRT(MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0))
          Sel(I)  = SS2(1)
          SS2(1)  = ONE/SS2(1)

          NNX1(I) = SX0 * SS2(1)
          NNY1(I) = SY0 * SS2(1)
          NNZ1(I) = SZ0 * SS2(1)
          
          Sel(I)  = HALF*Sel(I)         
         
         ENDIF

       ENDDO



C--------------------------------------------------------
       !NORMAL BACKUP
       !  for a quadrangle, normal vector is 
       !  averaged from its 4 trias.
C--------------------------------------------------------
#include "lockon.inc"
        DO I=1,JLT
          IF(NP_RECT(I)==4)THEN
            SURF(1,IABS(CAND_E(I))) = FOURTH*(NNX1(I) + NNX2(I) + NNX3(I) + NNX4(I))
            SURF(2,IABS(CAND_E(I))) = FOURTH*(NNY1(I) + NNY2(I) + NNY3(I) + NNY4(I))
            SURF(3,IABS(CAND_E(I))) = FOURTH*(NNZ1(I) + NNZ2(I) + NNZ3(I) + NNZ4(I))
          ELSE
            SURF(1,IABS(CAND_E(I))) = NNX1(I) 
            SURF(2,IABS(CAND_E(I))) = NNY1(I) 
            SURF(3,IABS(CAND_E(I))) = NNZ1(I)          
          ENDIF
        ENDDO
#include "lockoff.inc"

C--------------------------------------------------------
       !SUBROUTINE OUTPUT
C--------------------------------------------------------
c      DO I=1,JLT
c        IF(IX3(I)/=IX4(I))THEN
c          !moyenne issue des 4 triangles
c          NX1(I)  = FOURTH*(NNX1(I)+NNX2(I)+NNX3(I)+NNX4(I))
c          NY1(I)  = FOURTH*(NNY1(I)+NNY2(I)+NNY3(I)+NNY4(I))          
c          NZ1(I)  = FOURTH*(NNZ1(I)+NNZ2(I)+NNZ3(I)+NNZ4(I))          
c        ELSE
c          NX1(I)  =      (NNX1(I)+NNX2(I)+NNX3(I)+NNX4(I))
c          NY1(I)  =      (NNY1(I)+NNY2(I)+NNY3(I)+NNY4(I))          
c          NZ1(I)  =      (NNZ1(I)+NNZ2(I)+NNZ3(I)+NNZ4(I))         
c        ENDIF
c      ENDDO

!      if(IBUG22_Swet==-11 .AND. INT22>0 )then  
!        print *,   "   |----------i22wetsurf.F---------|"
!        print *,   "   |     LAGRANGIAN PROJECTIONS    |"
!        print *,   "   |-------------------------------|"
!          write (*,FMT='(A,I10)')              "    N_CAND_B       =",NCANDB          
!          write (*,FMT='(A,I10)')              "    N_CAND_E       =",NCANDE           
!          write (*,FMT='(A,I10)')              "    #COUPLES       =",I_STOK        
!          write (*,FMT='(A,I10)')              "    NB             =",NB     
!        do I=1, JLT ! en bouclant sur NB il faut utiliser les tableaux globaux (moins performant, mais cest pour le post/debug)
!          IB  = CB_LOC(I)
!          IFT = BRICK_LIST(NIN,IB)%Seg_add_LFT
!          ILT = BRICK_LIST(NIN,IB)%Seg_add_LLT
!          ILT = MIN(ILT, JLT) !boucle MVSIZ
!!          IF(ILT<IFT)CYCLE  !on boucle ici sur les brique pas sur les candidats ne devrait pas arriver
!          
!          !IFT : [1,NBRIC] |-> 1st    :si =I_stok alors pas candidate
!          !ILT : [1,NBRIC] |-> last   :si =0      alors idem
!          
!          write (*,FMT='(A,I10)')              "    IB             =",IB
!          write (*,FMT='(A,I10,I10)')          "    IFT,ILT        =",IFT,ILT            
!          write (*,FMT='(A,I5,A,100I10)')"            CE_LOC(",I,")=",  CE_LOC(I)
!          write (*,FMT='(A,100I10)')           "                 N1=",ITAB(IRECT(1,CE_LOC(I)))
!          write (*,FMT='(A,100I10)')           "                 N2=",ITAB(IRECT(2,CE_LOC(I)))
!          write (*,FMT='(A,100I10)')           "                 N3=",ITAB(IRECT(3,CE_LOC(I)))                    
!          write (*,FMT='(A,100I10)')           "                 N4=",ITAB(IRECT(4,CE_LOC(I)))                             
!          print *, "" 
!        enddo       
!      endif
      
      DIST_PCUT(1:NBCUT_MAX , 1:4 , 1:NCANDE) = EP30
      

!                         +------------------------------------+
!                         +                                    +
!                         +                                    +
!                         +\                                   +
!                         + \                                  +
!                         +  \                                 +
!                         +   \                                +
!                         +    \                               +             8 real cuts from polyedra
!                         +     \ ICUT=1                       +            +6 fictious cuts from closed surface hypothesis
!                         +      \                             +
!                         +       \                            +
!                         +        \                           +
!                         +         \                          +
!                         +          \                 ICUT=2 /+
!                         +           \                     /  +
!                         +            \                  /    +
!                         +             \  ICUT=8+1     /      +
!                         +--------------+------------+--------+
!                                            ^
!                                            |_  
!                                                brick_list()%Pol9woNodes(J) = 1 (cell 9 does not have nodes on this face = closed surface hypothesis)
!                                                  These closed surfaces must be taken into account for contact forces (and also internal forces)


      DO I=1,JLT 
C--------------------------------------------------------
       !PROJECTION DES NOEUDS SUR LE PLAN DE COUPE
C--------------------------------------------------------      
        IB                      = CB_LOC(I)                                       
        IFT                     = BRICK_LIST(NIN,IB)%Seg_add_LFT                                                  
        ILT                     = BRICK_LIST(NIN,IB)%Seg_add_LLT                                                  
        NBCUT                   = BRICK_LIST(NIN,IB)%NBCUT 
        IE                      = I
          
        !face coordinates                                                                                           
        P(1:3,1)                = (/ X1(IE), Y1(IE), Z1(IE) /)                                             
        P(1:3,2)                = (/ X2(IE), Y2(IE), Z2(IE) /)                                             
        P(1:3,3)                = (/ X3(IE), Y3(IE), Z3(IE) /)                                             
        P(1:3,4)                = (/ X4(IE), Y4(IE), Z4(IE) /)
        
        DO ICUT = 1,NBCUT 
          delta(1:4,ICUT,I)     =  ZERO
          NORMAL(1:3)           =  BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(1:3)                                        
          BASIS(1:3)            =  BRICK_LIST(NIN,IB)%PCUT(ICUT)%B(1:3)                                      
          N_SCUT(1:3,ICUT,I)    =  NORMAL(1:3)                                                                
          !(/a,b,c,d/) = (/ NORMAL(1:3), -NORMAL(1:3)*BASIS(1:3) /)  !Equation du plan de section (Pcut)   
          a                     = NORMAL(1)                                                                                   
          b                     = NORMAL(2)                                                                                   
          c                     = NORMAL(3)                                                                                   
          d                     = -a*BASIS(1) -b*BASIS(2) -c*BASIS(3)                                                         
          a2                    = a*a                                                                                         
          b2                    = b*b                                                                                         
          c2                    = c*c                                                                                         
          L2                    = ONE / MAX(EM30,a2+b2+c2)                                                                     
          !projected face coordinates
          DO J=1,4    
            Ph(  1,ICUT,J)      = (b2+c2)*P(1,J) -          a*b*P(2,J) -     a*c*P(3,J) - d*a                      
            Ph(  2,ICUT,J)      =        -a*b*P(1,J) +  (a2+c2)*P(2,J) -     b*c*P(3,J) - d*b                      
            Ph(  3,ICUT,J)      =        -a*c*P(1,J) -      b*c*P(2,J) + (a2+b2)*P(3,J) - d*c                      
            Ph(1:3,ICUT,J)      = L2 * Ph(1:3,ICUT,J)
            PPh(1:3,J)          = P(1:3,J) - Ph(1:3,ICUT,J)                                                  
            DIST(J)             = SUM( PPh(1:3,J) * PPh(1:3,J) )                                           
            DIST_PCUT(ICUT,J,I) = DIST(J) !L2 Norm ** 2                                                           
          ENDDO 
          !projected cut surface nodes (coplanarity is assumption but not reality)
          NP                    = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP
          DO J=1,NP
            S(1:3,J)            = BRICK_LIST(NIN,IB)%PCUT(ICUT)%P(1:3,J)
          ENDDO
          DO J=1,NP
            Sh(  1,ICUT,J)      =      (b2+c2)*S(1,J) -      a*b*S(2,J) -     a*c*S(3,J) - d*a                      
            Sh(  2,ICUT,J)      =         -a*b*S(1,J) +  (a2+c2)*S(2,J) -     b*c*S(3,J) - d*b                      
            Sh(  3,ICUT,J)      =         -a*c*S(1,J) -      b*c*S(2,J) + (a2+b2)*S(3,J) - d*c                      
            Sh(1:3,ICUT,J)      = L2 * Sh(1:3,ICUT,J)
          ENDDO
        ENDDO !next ICUT 
         
        !Additional cut planes for closed surface hypothesis
        NBCUT_ADD               = 0
        DO ICUT = 8+1,8+6
          J                     =  ICUT - 8
          NP                    = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP 
          IF(NP<=0)CYCLE 
          NBCUT_ADD             =  NBCUT_ADD + 1
          delta(1:4,ICUT,I)     =  ZERO
          NORMAL(1:3)           =  BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(1:3)                                        
          BASIS(1:3)            =  BRICK_LIST(NIN,IB)%PCUT(ICUT)%B(1:3)                                      
          N_SCUT(1:3,ICUT,I)    =  NORMAL(1:3)                                                                  
          !(/a,b,c,d/) = (/ NORMAL(1:3), -NORMAL(1:3)*BASIS(1:3) /)  !Equation du plan de section (Pcut)   
          a                     = NORMAL(1)                                                                 
          b                     = NORMAL(2)                                                                 
          c                     = NORMAL(3)                                                                 
          d                     = -a*BASIS(1) -b*BASIS(2) -c*BASIS(3)                                        
          a2                    = a*a                                                                        
          b2                    = b*b                                                                        
          c2                    = c*c                                                                        
          L2                    = ONE / MAX(EM30,a2+b2+c2)                                                  
          !projected face coordinates
          DO J=1,4    
            Ph(  1,ICUT,J)      = (b2+c2)*P(1,J) -          a*b*P(2,J) -     a*c*P(3,J) - d*a                      
            Ph(  2,ICUT,J)      =        -a*b*P(1,J) +  (a2+c2)*P(2,J) -     b*c*P(3,J) - d*b                      
            Ph(  3,ICUT,J)      =        -a*c*P(1,J) -      b*c*P(2,J) + (a2+b2)*P(3,J) - d*c                      
            Ph(1:3,ICUT,J)      = L2 * Ph(1:3,ICUT,J)
            PPh(1:3,J)          = P(1:3,J) - Ph(1:3,ICUT,J)                                                      
            DIST(J)             = SUM( PPh(1:3,J) * PPh(1:3,J) )                                                    
            DIST_PCUT(ICUT,J,I) = DIST(J) !L2 Norm ** 2                                                           
          ENDDO 
          !projected cut surface nodes (coplanarity is assumption but not reality)
          NP                    = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP
          DO J=1,NP
            S(1:3,J)            = BRICK_LIST(NIN,IB)%PCUT(ICUT)%P(1:3,J)
          ENDDO
          DO J=1,NP
            Sh(  1,ICUT,J)      =      (b2+c2)*S(1,J) -      a*b*S(2,J) -     a*c*S(3,J) - d*a                      
            Sh(  2,ICUT,J)      =         -a*b*S(1,J) +  (a2+c2)*S(2,J) -     b*c*S(3,J) - d*b                      
            Sh(  3,ICUT,J)      =         -a*c*S(1,J) -      b*c*S(2,J) + (a2+b2)*S(3,J) - d*c                      
            Sh(1:3,ICUT,J)      = L2 * Sh(1:3,ICUT,J)
          ENDDO
        ENDDO !next ICUT                      
                                                                                                  

C        TAG_NOD(1:4,1:NBCUT) = 0                                                                                                                                                                                                                                     
C        DO J=1,4    
C          !init before entering loop
C          ICUT                  = 1 
C          VAL                   =  DIST_PCUT(ICUT,J,I)
C          !loop on cut planes in current cut cell IB = CB_LOC(I)
C          DO ICUT=1,NBCUT                                                                                   
C            IF(DIST_PCUT(ICUT,J,I) <= VAL)THEN                                                           
C              TAG_NOD(J, ICUT)  = 1                                                                       
C              VAL               =   DIST_PCUT(ICUT,J,I)                                                                
C            ENDIF                                                                                            
C          ENDDO!next ICUT                                                                                   
C        ENDDO !next J     
C        !tag for additional cuts (closed surface hypothesis)
C        IF(NBCUT_ADD > 0)THEN
C          DO J=1,4    
C            !init before entering loop
C            ICUT                = 8+1 
C            VAL                 =  DIST_PCUT(ICUT,J,I)
C            !loop on cut planes in current cut cell IB = CB_LOC(I)
C            DO ICUT=8+1,8+NBCUT_ADD
C              IF(DIST_PCUT(ICUT,J,I) <= VAL)THEN                                                           
C                TAG_NOD(J, ICUT)= 1                                                                       
C                VAL             =   DIST_PCUT(ICUT,J,I)                                                                
C              ENDIF                                                                                           
C            ENDDO!next ICUT                                                                                   
C          ENDDO !next J                            
C        ENDIF!(NBCUT_ADD > 0)

        !TAG DES ELEMENTS LAGRANGIENS                                                                           
C        TAG_EL(1:NBCUT_MAX) = 0  
C        IF(NBCUT>0)THEN                                                                                  
C          DO ICUT=1,NBCUT                                                                                                                                                     
C            TAG_EL(ICUT) = SUM(TAG_NOD(:,ICUT)) 
C               if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,I10,A)')   "   |---COUPLE:",I,"-----------|"             
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=1)print *, "   TAG_EL(ICUT=1)=" ,TAG_EL(1)
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=2)print *, "   TAG_EL(ICUT=2)=" ,TAG_EL(2)     
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=3)print *, "   TAG_EL(ICUT=3)=" ,TAG_EL(3)
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=4)print *, "   TAG_EL(ICUT=4)=" ,TAG_EL(4)
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=5)print *, "   TAG_EL(ICUT=5)=" ,TAG_EL(5)
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=6)print *, "   TAG_EL(ICUT=6)=" ,TAG_EL(6)
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=7)print *, "   TAG_EL(ICUT=7)=" ,TAG_EL(7)
C               if(IBUG22_Swet==-1 .AND. INT22>0 .AND. NBCUT<=8)print *, "   TAG_EL(ICUT=8)=" ,TAG_EL(8)                                                                                          
C          ENDDO!next ICUT 
C       ENDIF

                                                                             
       
C--------------------------------------------------------
       !INJECTION DANS LESPACE PROPRE DU PLAN DE PROJECTION (IE PLAN DINTERSECTION) 
C-------------------------------------------------------- 

        !DIM = 2
        !Base = Projection matrix Eingenvectors (eigenval = 1)
        ! B1=(-b,a,0) B2=(-c,0,a)

        DO ICUT=1,NBCUT
C          if(TAG_EL(ICUT)==0)CYCLE
          IB                 = CB_LOC(I)
          NORMAL(1:3)        = BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(1:3)
          a                  = NORMAL(1)
          b                  = NORMAL(2)
          c                  = NORMAL(3)
          IF    (a==ZERO . AND. b==ZERO)THEN
            V1               = (/ ONE,ZERO,ZERO /)
            V2               = (/ ZERO,ONE,ZERO /)     
          ELSEIF(a==ZERO . AND. c==ZERO)THEN
            V1               = (/ ONE,ZERO,ZERO /)
            V2               = (/ ZERO,ZERO,ONE /)      
          ELSEIF(b==ZERO . AND. c==ZERO)THEN                 
            V1               = (/ ZERO,ONE,ZERO /)
            V2               = (/ ZERO,ZERO,ONE /) 
          ELSE
           IF(a/=ZERO)THEN
             V1              = (/ -b, a   , ZERO /)
             V2              = (/ -c, ZERO, a    /)       
           ELSE
             V1              = (/ ONE, ZERO, ZERO /)
             V2              = (/ ZERO, -c, b    /)
           ENDIF
           !ORTHONORMAL
           !hyopothesis : V1, V2 are non colinear, ||V1||=1
           !V2~ = labmda1 * V1 + lambda2 * V2    (V2~ is generated by (V1,V2) )
           !let set lambda1 = <V1,V2>, then lambda2=-1
           V1                = V1 / SQRT(SUM(V1*V1))           
           PS                = V1(1)*V2(1) + V1(2)*V2(2) +V1(3)*V2(3)
           V2                = PS*V1 -V2
           V2                = V2 / SQRT(SUM(V2*V2))
          ENDIF 
          V3                 = NORMAL / SQRT(SUM(NORMAL*NORMAL)) 
          !BASIS SWITCH
          !third direction is Pcut normal
          !matrice de changement de base
          M(1:3,1,ICUT)      = V1
          M(1:3,2,ICUT)      = V2
          M(1:3,3,ICUT)      = V3
          if(IBUG22_Swet==-1 .AND. INT22>0 )then
            print *,"   ORTHONORMAL BASIS :"
            write (*,FMT='(A,F20.10,A1,F20.10,A1,F20.10)')"     V1=", V1(1),",",V1(2),",",V1(3)
            write (*,FMT='(A,F20.10,A1,F20.10,A1,F20.10)')"     V2=", V2(1),",",V2(2),",",V2(3)
            write (*,FMT='(A,F20.10,A1,F20.10,A1,F20.10)')"     V3=", V3(1),",",V3(2),",",V3(3)        
          endif  
          !nouvelles coordonnees dans le plan de projection (dim=2)
          !Inutile dinverser la matrice car elle est orthogonale
          DO J=1,4
            VAL1             = M(1,1,ICUT)*Ph(1,ICUT,J) + M(2,1,ICUT)*Ph(2,ICUT,J) + M(3,1,ICUT)*Ph(3,ICUT,J)
            VAL2             = M(1,2,ICUT)*Ph(1,ICUT,J) + M(2,2,ICUT)*Ph(2,ICUT,J) + M(3,2,ICUT)*Ph(3,ICUT,J)
            VAL3             = ZERO 
            Ph(1:3,ICUT,J)   = (/VAL1,VAL2,VAL3/)
          ENDDO
          NP                 = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP
          DO J=1,NP
            VAL1             = M(1,1,ICUT)*Sh(1,ICUT,J) + M(2,1,ICUT)*Sh(2,ICUT,J) + M(3,1,ICUT)*Sh(3,ICUT,J)
            VAL2             = M(1,2,ICUT)*Sh(1,ICUT,J) + M(2,2,ICUT)*Sh(2,ICUT,J) + M(3,2,ICUT)*Sh(3,ICUT,J)
            VAL3             = ZERO 
            Sh(1:3,ICUT,J)   = (/VAL1,VAL2,VAL3/)
          ENDDO!next NP
        ENDDO!next ICUT

 
        IF(NBCUT_ADD > 0)THEN       
          DO ICUT=8+1,8+6
            J                  =  ICUT - 8
            NP                 = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP         
            IF(NP<=0)CYCLE 
C            if(TAG_EL(ICUT)==0)CYCLE
            IB                 = CB_LOC(I)
            NORMAL(1:3)        = BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(1:3) 
            a                  = NORMAL(1)
            b                  = NORMAL(2)
            c                  = NORMAL(3)
            IF    (a==ZERO . AND. b==ZERO)THEN
              V1               = (/ ONE,ZERO,ZERO /)
              V2               = (/ ZERO,ONE,ZERO /)     
            ELSEIF(a==ZERO . AND. c==ZERO)THEN
              V1               = (/ ONE,ZERO,ZERO /)
              V2               = (/ ZERO,ZERO,ONE /)      
            ELSEIF(b==ZERO . AND. c==ZERO)THEN                 
              V1               = (/ ZERO,ONE,ZERO /)
              V2               = (/ ZERO,ZERO,ONE /) 
            ELSE
             IF(a/=ZERO)THEN
               V1              = (/ -b, a   , ZERO /)
               V2              = (/ -c, ZERO, a    /)       
             ELSE
               V1              = (/ ONE, ZERO, ZERO /)
               V2              = (/ ZERO, -c, b    /)
             ENDIF
             !ORTHONORMAL
             !hyopothesis : V1, V2 are non colinear, ||V1||=1
             !V2~ = labmda1 * V1 + lambda2 * V2    (V2~ is generated by (V1,V2) )
             !let set lambda1 = <V1,V2>, then lambda2=-1
             V1                = V1 / SQRT(SUM(V1*V1))           
             PS                = V1(1)*V2(1) + V1(2)*V2(2) +V1(3)*V2(3)
             V2                = PS*V1 -V2
             V2                = V2 / SQRT(SUM(V2*V2))
            ENDIF 
            V3                 = NORMAL / SQRT(SUM(NORMAL*NORMAL)) 
            !BASIS SWITCH
            !third direction is Pcut normal
            !matrice de changement de base
            M(1:3,1,ICUT)      = V1
            M(1:3,2,ICUT)      = V2
            M(1:3,3,ICUT)      = V3
            if(IBUG22_Swet==-1 .AND. INT22>0 )then
              print *,"   ORTHONORMAL BASIS _additional Scut (closed surf hypothesis) :"
              write (*,FMT='(A,F20.10,A1,F20.10,A1,F20.10)')"     V1=", V1(1),",",V1(2),",",V1(3)
              write (*,FMT='(A,F20.10,A1,F20.10,A1,F20.10)')"     V2=", V2(1),",",V2(2),",",V2(3)
              write (*,FMT='(A,F20.10,A1,F20.10,A1,F20.10)')"     V3=", V3(1),",",V3(2),",",V3(3)        
            endif  
            !nouvelles coordonnees dans le plan de projection (dim=2)
            !Inutile dinverser la matrice car elle est orthogonale
            DO J=1,4
              VAL1             = M(1,1,ICUT)*Ph(1,ICUT,J) + M(2,1,ICUT)*Ph(2,ICUT,J) + M(3,1,ICUT)*Ph(3,ICUT,J)
              VAL2             = M(1,2,ICUT)*Ph(1,ICUT,J) + M(2,2,ICUT)*Ph(2,ICUT,J) + M(3,2,ICUT)*Ph(3,ICUT,J)
              VAL3             = ZERO 
              Ph(1:3,ICUT,J)   = (/VAL1,VAL2,VAL3/)
            ENDDO
            NP                 =  BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP
            DO J=1,NP
              VAL1             = M(1,1,ICUT)*Sh(1,ICUT,J) + M(2,1,ICUT)*Sh(2,ICUT,J) + M(3,1,ICUT)*Sh(3,ICUT,J)
              VAL2             = M(1,2,ICUT)*Sh(1,ICUT,J) + M(2,2,ICUT)*Sh(2,ICUT,J) + M(3,2,ICUT)*Sh(3,ICUT,J)
              VAL3             = ZERO 
              Sh(1:3,ICUT,J)   = (/VAL1,VAL2,VAL3/)
            ENDDO!next NP
          ENDDO!next ICUT       
        ENDIF !(NBCUT_ADD > 0)



C--------------------------------------------------------
       !POLYGONAL CLIPPING (SCUT with projected segment)
C-------------------------------------------------------- 
! Projected Segment might be non convex. In this case it is decomposed into 2 triangles (hourglass shape)
! If projected segment is convex then it is splitted into 4 triangles (required for parith on)
!
        DO ICUT = 1, NBCUT                                                                                        
          SEFF(ICUT,I)              = ZERO 
          P_Grav(1:3,ICUT,I)        = ZERO 
          NumGrav                   = ZERO
          NP                        = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP     !BUG line was missing !
C          if(TAG_EL(ICUT)==0)CYCLE 
          PolySCUT%NumNodes         = NP+1  !
          DO J=1,NP
            PolySCUT%node(J,1:2)    = Sh(1:2,ICUT,J)
          ENDDO
          PolySCUT%node(NP+1,:)     = PolySCUT%node(1,:)
          CALL SetCounterClockWisePolyg( PolySCUT, VAL2 )
          IF(VAL2==ZERO)CYCLE
          ITYP                      = GetProjectedFaceType( 
     .                                 Ph(1:2,ICUT,1), Ph(1:2,ICUT,2), Ph(1:2,ICUT,3), Ph(1:2,ICUT,4), InterP, ListP    ) 
          NTRIA                     = 4
          IF(ITYP/=0)NTRIA        = 2 
          IF(NP_RECT(I)==3) THEN
            ITYP                    = 0
            NTRIA                   = 1
          ENDIF
          
          Stria(1:4)                = ZERO
           
          !first triangle
          PolyTria(1)%NumNodes      = 3+1
          PolyTria(1)%node(1,1:2)   = Ph(1:2,ICUT,ListP(1))
          PolyTria(1)%node(2,1:2)   = Ph(1:2,ICUT,ListP(2))
          PolyTria(1)%node(3,1:2)   = InterP(1:2)
          PolyTria(1)%node(4,:)     = PolyTria(1)%node(1,:)  
          CALL SetCounterClockWisePolyg( PolyTria(1), Stria(1) )   
          
          !second triangle
          PolyTria(2)%NumNodes      = 3+1
          PolyTria(2)%node(1,1:2)   = Ph(1:2,ICUT,ListP(3))
          PolyTria(2)%node(2,1:2)   = Ph(1:2,ICUT,ListP(4))
          PolyTria(2)%node(3,1:2)   = InterP(1:2)
          PolyTria(2)%node(4,:)     = PolyTria(2)%node(1,:)    
          CALL SetCounterClockWisePolyg( PolyTria(2), Stria(2) )                 
          
          IF(NTRIA == 4)THEN
            !third triangle
            PolyTria(3)%NumNodes = 3+1
            PolyTria(3)%node(1,1:2) = Ph(1:2,ICUT,ListP(2))
            PolyTria(3)%node(2,1:2) = Ph(1:2,ICUT,ListP(3))
            PolyTria(3)%node(3,1:2) = InterP(1:2) 
            PolyTria(3)%node(4,:)   = PolyTria(3)%node(1,:) 
            CALL SetCounterClockWisePolyg( PolyTria(3), Stria(3) )                                            

            !fourth triangle
            PolyTria(4)%NumNodes = 3+1
            PolyTria(4)%node(1,1:2) = Ph(1:2,ICUT,ListP(1))
            PolyTria(4)%node(2,1:2) = Ph(1:2,ICUT,ListP(4))
            PolyTria(4)%node(3,1:2) = InterP(1:2) 
            PolyTria(4)%node(4,:)   = PolyTria(4)%node(1,:)
            CALL SetCounterClockWisePolyg( PolyTria(4), Stria(4) )                                                  
          ENDIF
          
          Cumul_VAL2                = SUM(Stria(1:4))
          
          !-------OUTPUT------
          if(IBUG22_Swet==-1 .AND. INT22>0 )then  
            print *,   "   |-----------i22wetsurf.F--------|"
            print *,   "   |     LAGRANGIAN PROJECTIONS    |"
            print *,   "   |-------------------------------|"
             IB      = CB_LOC(I)
             brickID = BRICK_LISt(NIN,IB)%ID
              write (*,FMT='(A,1I10,A,I2)')      "    --------brickID=",ixs(11,brickID), "  ICUT=", ICUT
              K = ABS(CE_LOC(I))
              write (*,FMT='(A,1I10)')           "                 N1=",ITAB(IRECT(1,K))
              write (*,FMT='(A,1I10)')           "                 N2=",ITAB(IRECT(2,K))
              write (*,FMT='(A,1I10)')           "                 N3=",ITAB(IRECT(3,K))                    
              write (*,FMT='(A,1I10)')           "                 N4=",ITAB(IRECT(4,K))
              write (*,FMT='(A)'     )           "                   Projected Tria"
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,1),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,2),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,3),ZERO                                            
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,4),ZERO
              write (*,FMT='(A,1I1)' )           "               ityp=",ITYP                              
              write (*,FMT='(A)'     )           "                   PolySCUT"
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(1,1:2),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(2,1:2),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(3,1:2),ZERO                                            
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(4,1:2),ZERO               
              write (*,FMT='(A,1I10)')           "              NTRIA=",NTRIA               
              DO J=1,NTRIA
                                                                        
              ENDDO
              print *, ""     
          endif           
          
          POLYresult%NumNodes = 0

          WET_RATIO(:) = ZERO
          VAL1         = ZERO
          VAL2         = ZERO
          Cumul_VAL1   = ZERO
          
          DO J=1,NTRIA                                                                                              
            CALL PolygonalClipping( PolyTria(J),PolySCUT, POLYresult)
            IF(POLYresult%NumNodes==0)CYCLE
            !full area
            pt1(1:2) = PolyTria(J)%node(1,1:2) ; pt1(3)=ZERO
            pt2(1:2) = PolyTria(J)%node(2,1:2) ; pt2(3)=ZERO
            pt3(1:2) = PolyTria(J)%node(3,1:2) ; pt3(3)=ZERO                        
            !wet area
            if(IBUG22_Swet==-1 .AND. INT22>0 )then
            write (*,FMT='(A,1I10)')           "            --TRIA #", J
            write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolyTria(J)%node(1,1:2),ZERO                                             
            write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolyTria(J)%node(2,1:2),ZERO                                             
            write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolyTria(J)%node(3,1:2),ZERO             
            write (*,FMT='(A,1I10)')           "                   ->clip nodes", POLYresult%NumNodes  
            do k=1,POLYresult%NumNodes-1
            write (*,FMT='(A,3F45.35)')        "                     *createnode ",POLYresult%node(k,1:2),ZERO                                             
            enddo                    
            endif
            
            PGrav(1:3) = ZERO
            !Gravity center estimation / To be updated later / must also be Parith ON
            IF(POLYresult%NumNodes-1>0)THEN
              DO k=1,POLYresult%NumNodes-1
c                P_Grav(1,ICUT,I) = P_Grav(1,ICUT,I) + POLYresult%node(k,1)
c                P_Grav(2,ICUT,I) = P_Grav(2,ICUT,I) + POLYresult%node(k,2)         
                PGrav(1) = PGrav(1) + POLYresult%node(k,1)
                PGrav(2) = PGrav(2) + POLYresult%node(k,2)         
              ENDDO 
              PGrav(1) =  PGrav(1) / (POLYresult%NumNodes-1) 
              PGrav(2) =  PGrav(2) / (POLYresult%NumNodes-1)
            ENDIF
            
            P_Grav(1,ICUT,I) = P_Grav(1,ICUT,I) + PGrav(1)
            P_Grav(2,ICUT,I) = P_Grav(2,ICUT,I) + PGrav(2)            
            NumGrav          = Numgrav + ONE
           
            IF(POLYresult%NumNodes > 0 .AND. STria(J)/=ZERO)THEN          
              pt1(1:2)   = PolyTria(J)%node(1,1:2) ; pt1(3)=ZERO
              pt2(1:2)   = PolyTria(J)%node(2,1:2) ; pt2(3)=ZERO
              pt3(1:2)   = PolyTria(J)%node(3,1:2) ; pt3(3)=ZERO          
              CALL SetCounterClockWisePolyg(POLYresult ,VAL)
              VAL        = MAX(ZERO,MIN(VAL,Stria(J)))
              Cumul_VAL1 = Cumul_VAL1 + VAL               
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             ->      Tria Wet Surf=", VAL  
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             ->     Tria     Surf =", Stria(J)                          
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             -> cumulated Wet Surf=", Cumul_VAL1
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             ->   Sum of tria Surf=", Cumul_VAL2
              
              !post-condition testing
              WET_RATIO(J) = VAL/Stria(J)
              IF(WET_RATIO(J)<-EM06 .OR. WET_RATIO(J)-ONE>EM06)THEN
                print *, "***error: inter22 topology issue while computing wet surface",WET_RATIO(J),VAL1,VAL2
              ENDIF   
            ENDIF
          ENDDO !next J=1,NTRIA 

          WET_RATIO(0) = Cumul_VAL1 !sum of wet surfaces for all triangles composing the lagrangian face.
                   
          IF(WET_RATIO(0) == ZERO .OR. Cumul_VAL2==ZERO)THEN
            SEFF(ICUT,I) = ZERO
            CYCLE
          ELSE
            WET_RATIO(0) = WET_RATIO(0) / Cumul_VAL2          
          ENDIF
                   
          WET_RATIO(0) = MAX(ZERO,MIN(WET_RATIO(0),ONE))
          SEFF(ICUT,I) = Cumul_VAL1
          
          SEFF(ICUT,I) = WET_RATIO(0) * Sel(I)
          
          !-------OUTPUT------
          if(IBUG22_Swet==-1 .AND. INT22>0 )then              
              write (*,FMT='(A,1F30.16)')        "        Sum Wet Surf =",Cumul_VAL1
              write (*,FMT='(A,1F30.16)')        "          Total Surf =",Cumul_VAL2                                     
              write (*,FMT='(A,1F30.16)')        "               RATIO =",WET_RATIO(0)
              print *, ""     
          endif 
          
          IF(NumGrav==0)CYCLE
          
          P_Grav(1,ICUT,I) = P_Grav(1,ICUT,I) / NumGrav
          P_Grav(2,ICUT,I) = P_Grav(2,ICUT,I) / NumGrav
                    
          !---sWeighting factors using pressure load center
          !InterPolation using gravity centers of clipped polygons
          !NP_RECT(I) = 3 or 4

          !2d projected segment
          pt1(1:2)     = Ph(1:2,ICUT,ListP(1))
          pt2(1:2)     = Ph(1:2,ICUT,ListP(2))
          pt3(1:2)     = Ph(1:2,ICUT,ListP(3))
          pt4(1:2)     = Ph(1:2,ICUT,ListP(4))
          
C          write (*,FMT='(A          )')  "Segment    ="         
C          write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(1)) ," 0.0"
C          write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(2)) ," 0.0"
C          write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(3)) ," 0.0"
C          write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(4)) ," 0.0"                              

C          write (*,FMT='(A          )')  "Scut    ="  
C          do k=1,PolyScut%NumNodes-1       
C            write (*,FMT='(A,2E30.16,A)')"*createnode ", PolyScut%node(k,1:2) ," 0.0"
C          enddo
   
          
          !distance of each point from wet polygon gravity center
          distance(1)  = SQRT( (P_grav(1,ICUT,I)-Pt1(1))**2 + (P_grav(2,ICUT,I)-Pt1(2))**2 )
          distance(2)  = SQRT( (P_grav(1,ICUT,I)-Pt2(1))**2 + (P_grav(2,ICUT,I)-Pt2(2))**2 )
          distance(3)  = SQRT( (P_grav(1,ICUT,I)-Pt3(1))**2 + (P_grav(2,ICUT,I)-Pt3(2))**2 )
          distance(4)  = SQRT( (P_grav(1,ICUT,I)-Pt4(1))**2 + (P_grav(2,ICUT,I)-Pt4(2))**2 )

C          write (*,FMT='(A          )')  "Pgrav      ="         
C          write (*,FMT='(A,2E30.16,A)')  "*createnode ", P_grav(1:2,ICUT,I) ," 0.0"
   
          
          !segment edges  
          edge(1)      = SQRT ( (Pt1(1)-Pt2(1))**2 + (Pt1(2)-Pt2(2))**2 )
          edge(2)      = SQRT ( (Pt2(1)-Pt3(1))**2 + (Pt2(2)-Pt3(2))**2 )
          edge(3)      = SQRT ( (Pt3(1)-Pt4(1))**2 + (Pt3(2)-Pt4(2))**2 )
          edge(4)      = SQRT ( (Pt4(1)-Pt1(1))**2 + (Pt4(2)-Pt1(2))**2 )
          IF(NP_RECT(I)==3)THEN
            edge(3)      = SQRT ( (Pt3(1)-Pt1(1))**2 + (Pt3(2)-Pt1(2))**2 )
            edge(4)      = ZERO         
          ENDIF
          
          a            = edge(1)
          b            = distance(1)
          c            = distance(2)          
          s2           = HALF*(a+b+c)
          Area_tria(1) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) ) 
          
          a            = edge(2)
          b            = distance(2)
          c            = distance(3)          
          s2           = HALF*(a+b+c)
          Area_tria(2) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )  
          
          IF(NP_RECT(I)==4)THEN
            a            = edge(3)
            b            = distance(3)
            c            = distance(4)          
            s2           = HALF*(a+b+c)
            Area_tria(3) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )  

            a            = edge(4)
            b            = distance(4)
            c            = distance(1)          
            s2           = HALF*(a+b+c)
            Area_tria(4) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )                                                             
          ELSE
            a            = edge(3)
            b            = distance(3)
            c            = distance(1)          
            s2           = HALF*(a+b+c)
            Area_tria(3) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )  

            Area_tria(4) = ZERO          
          ENDIF
                                         
          Dsum         = SUM(Area_tria(1:NP_RECT(I)))
          
c22          IF(Dsum==ZERO)then
c22            print *, "Warning: inter22 - nodal forces computation"
c22          endif
          
          IF(NP_RECT(I)==4)THEN
            !delta(1,ICUT,I)   = HALF*(Area_tria(2)+Area_tria(3))/Dsum
            !delta(2,ICUT,I)   = HALF*(Area_tria(3)+Area_tria(4))/Dsum
            !delta(3,ICUT,I)   = HALF*(Area_tria(1)+Area_tria(4))/Dsum
            !delta(4,ICUT,I)   = HALF*(Area_tria(1)+Area_tria(2))/Dsum 
            !uniform loading: same weight
            delta(1:4,ICUT,I) = FOURTH                            
          ELSE
            !delta(1,ICUT,I)   = (Area_tria(2))/Dsum
            !delta(2,ICUT,I)   = (Area_tria(3))/Dsum
            !delta(3,ICUT,I)   = (Area_tria(1))/Dsum
            !delta(4,ICUT,I)   = ZERO 
            !uniform loading : same weight
            delta(1:4,ICUT,I) = THIRD
            delta(4  ,ICUT,I) = ZERO                                      
          ENDIF

          !write (*,FMT='(A,3E30.16)')  "CoG  = ", P_grav(1:2,ICUT,I)
          !write (*,FMT='(A,4E30.16)')  "dist = ", distance(1:4)

c          write (*,FMT='(A,4E30.16,A,1E30.16)')  "A_tria= ", Area_tria(1:4),"  sum=  ",SUM(Area_tria)
c          write (*,FMT='(A,4E30.16)')  "delta= ", delta(1:4,ICUT,I)
          
          
        ENDDO!next ICUT                                                                                                






        DO ICUT = 8+1,8+6  
          J                  =  ICUT - 8
          SEFF(ICUT,I)       = ZERO 
C          print *, "I=", I, "  ICUT= ",ICUT  
          NP                 = BRICK_LIST(NIN,IB)%PCUT(ICUT)%NP 
          IF(NP<=0)CYCLE           
          P_Grav(1:3,ICUT,I) = ZERO 
          NumGrav            = ZERO
            
C          if(TAG_EL(ICUT)==0)CYCLE 
          PolySCUT%NumNodes  = NP+1
          DO J=1,NP
            PolySCUT%node(J,1:2) = Sh(1:2,ICUT,J)
          ENDDO
          PolySCUT%node(NP+1,:)  = PolySCUT%node(1,:)
          CALL SetCounterClockWisePolyg( PolySCUT, VAL2 )
          
          IF(VAL2==ZERO)CYCLE

          ITYP = GetProjectedFaceType(Ph(1:2,ICUT,1), Ph(1:2,ICUT,2), Ph(1:2,ICUT,3), Ph(1:2,ICUT,4), InterP, ListP) 
          
          NTRIA              = 4
          IF(ITYP/=0)NTRIA = 2 
          IF(NP_RECT(I)==3) THEN
            ITYP  = 0
            NTRIA = 1
          ENDIF
          
          Stria(1:4)         = ZERO
           
          !first triangle
          PolyTria(1)%NumNodes = 3+1
          PolyTria(1)%node(1,1:2) = Ph(1:2,ICUT,ListP(1))
          PolyTria(1)%node(2,1:2) = Ph(1:2,ICUT,ListP(2))
          PolyTria(1)%node(3,1:2) = InterP(1:2)
          PolyTria(1)%node(4,:)   = PolyTria(1)%node(1,:)  
          CALL SetCounterClockWisePolyg( PolyTria(1), Stria(1) )   
          
          !second triangle
          PolyTria(2)%NumNodes = 3+1
          PolyTria(2)%node(1,1:2) = Ph(1:2,ICUT,ListP(3))
          PolyTria(2)%node(2,1:2) = Ph(1:2,ICUT,ListP(4))
          PolyTria(2)%node(3,1:2) = InterP(1:2)
          PolyTria(2)%node(4,:)   = PolyTria(2)%node(1,:)    
          CALL SetCounterClockWisePolyg( PolyTria(2), Stria(2) )                 
          
          IF(NTRIA == 4)THEN
            !third triangle
            PolyTria(3)%NumNodes = 3+1
            PolyTria(3)%node(1,1:2) = Ph(1:2,ICUT,ListP(2))
            PolyTria(3)%node(2,1:2) = Ph(1:2,ICUT,ListP(3))
            PolyTria(3)%node(3,1:2) = InterP(1:2) 
            PolyTria(3)%node(4,:)   = PolyTria(3)%node(1,:) 
            CALL SetCounterClockWisePolyg( PolyTria(3), Stria(3) )                                            

            !fourth triangle
            PolyTria(4)%NumNodes = 3+1
            PolyTria(4)%node(1,1:2) = Ph(1:2,ICUT,ListP(1))
            PolyTria(4)%node(2,1:2) = Ph(1:2,ICUT,ListP(4))
            PolyTria(4)%node(3,1:2) = InterP(1:2) 
            PolyTria(4)%node(4,:)   = PolyTria(4)%node(1,:)
            CALL SetCounterClockWisePolyg( PolyTria(4), Stria(4) )                                                  
          ENDIF
          
          Cumul_VAL2 = SUM(Stria(1:4))
          
          !-------OUTPUT------
          if(IBUG22_Swet==-1 .AND. INT22>0 )then  
            print *,   "   |-----------i22wetsurf.F--------|"
            print *,   "   |     LAGRANGIAN PROJECTIONS    |"
            print *,   "   |-------------------------------|"
             IB      = CB_LOC(I)
             brickID = BRICK_LISt(NIN,IB)%ID
              write (*,FMT='(A,1I10,A,I2)')      "    --------brickID=",ixs(11,brickID), "  ICUT=", ICUT
              write (*,FMT='(A     )')           "               additional Scut (closed surf. hypothesis." 
              write (*,FMT='(A,1I10)')           "                 N1=",ITAB(IRECT(1,CE_LOC(I)))
              write (*,FMT='(A,1I10)')           "                 N2=",ITAB(IRECT(2,CE_LOC(I)))
              write (*,FMT='(A,1I10)')           "                 N3=",ITAB(IRECT(3,CE_LOC(I)))                    
              write (*,FMT='(A,1I10)')           "                 N4=",ITAB(IRECT(4,CE_LOC(I)))
              write (*,FMT='(A)'     )           "                   Projected Tria"
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,1),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,2),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,3),ZERO                                            
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",Ph(1:2,ICUT,4),ZERO
              write (*,FMT='(A,1I1)' )           "               ityp=",ITYP                              
              write (*,FMT='(A)'     )           "                   PolySCUT"
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(1,1:2),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(2,1:2),ZERO                                             
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(3,1:2),ZERO                                            
              write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolySCUT%node(4,1:2),ZERO               
              write (*,FMT='(A,1I10)')           "              NTRIA=",NTRIA               
              DO J=1,NTRIA
                                                                        
              ENDDO
              print *, ""     
          endif           
          
          POLYresult%NumNodes = 0

          WET_RATIO(:) = ZERO
          VAL1         = ZERO
          VAL2         = ZERO
          Cumul_VAL1   = ZERO
          
          DO J=1,NTRIA                                                                                              
            CALL PolygonalClipping( PolyTria(J),PolySCUT, POLYresult)
            IF(POLYresult%NumNodes==0)CYCLE
            !full area
            pt1(1:2) = PolyTria(J)%node(1,1:2) ; pt1(3)=ZERO
            pt2(1:2) = PolyTria(J)%node(2,1:2) ; pt2(3)=ZERO
            pt3(1:2) = PolyTria(J)%node(3,1:2) ; pt3(3)=ZERO                        
            !wet area
            if(IBUG22_Swet==-1 .AND. INT22>0 )then
            write (*,FMT='(A,1I10)')           "            --TRIA #", J
            write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolyTria(J)%node(1,1:2),ZERO                                             
            write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolyTria(J)%node(2,1:2),ZERO                                             
            write (*,FMT='(A,3F30.16)')        "                   *createnode ",PolyTria(J)%node(3,1:2),ZERO             
            write (*,FMT='(A,1I10)')           "                   ->clip nodes", POLYresult%NumNodes  
            do k=1,POLYresult%NumNodes-1
            write (*,FMT='(A,3F45.35)')        "                     *createnode ",POLYresult%node(k,1:2),ZERO                                             
            enddo                    
            endif
            
            PGrav(1:3) = ZERO
            !Gravity center estimation / To be updated later / must also be Parith ON
            IF(POLYresult%NumNodes-1>0)THEN
              DO k=1,POLYresult%NumNodes-1
                 P_Grav(1,ICUT,I) = P_Grav(1,ICUT,I) + POLYresult%node(k,1)
                 P_Grav(2,ICUT,I) = P_Grav(2,ICUT,I) + POLYresult%node(k,2)         
                PGrav(1) = PGrav(1) + POLYresult%node(k,1)
                PGrav(2) = PGrav(2) + POLYresult%node(k,2)         
              ENDDO 
              PGrav(1) =  PGrav(1) / (POLYresult%NumNodes-1) 
              PGrav(2) =  PGrav(2) / (POLYresult%NumNodes-1)
            ENDIF
            
            P_Grav(1,ICUT,I) = P_Grav(1,ICUT,I) + PGrav(1)
            P_Grav(2,ICUT,I) = P_Grav(2,ICUT,I) + PGrav(2)            
            NumGrav          = Numgrav + ONE
           
            IF(POLYresult%NumNodes > 0 .AND. STria(J)/=ZERO)THEN          
              pt1(1:2)   = PolyTria(J)%node(1,1:2) ; pt1(3)=ZERO
              pt2(1:2)   = PolyTria(J)%node(2,1:2) ; pt2(3)=ZERO
              pt3(1:2)   = PolyTria(J)%node(3,1:2) ; pt3(3)=ZERO          
              CALL SetCounterClockWisePolyg(POLYresult ,VAL)
              VAL        = MAX(ZERO,MIN(VAL,Stria(J)))
              Cumul_VAL1 = Cumul_VAL1 + VAL               
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             ->      Tria Wet Surf=", VAL  
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             ->     Tria     Surf =", Stria(J)                          
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             -> cumulated Wet Surf=", Cumul_VAL1
              if(IBUG22_Swet==-1 .AND. INT22>0 )write (*,FMT='(A,1F30.16)')      "             ->   Sum of tria Surf=", Cumul_VAL2
              
              !post-condition testing
              WET_RATIO(J) = VAL/Stria(J)
              IF(WET_RATIO(J)<-EM06 .OR. WET_RATIO(J)-ONE>EM06)THEN
                print *, "***error: inter22 topology issue while computing wet surface",WET_RATIO(J),VAL1,VAL2
              ENDIF   
            ENDIF
          ENDDO !next J=1,NTRIA 

          WET_RATIO(0) = Cumul_VAL1 !sum of wet surfaces for all triangles composing the lagrangian face.
                   
          IF(WET_RATIO(0) == ZERO .OR. Cumul_VAL2==ZERO)THEN
            SEFF(ICUT,I) = ZERO
            CYCLE
          ELSE
            WET_RATIO(0) = WET_RATIO(0) / Cumul_VAL2          
          ENDIF
                   
          WET_RATIO(0) = MAX(ZERO,MIN(WET_RATIO(0),ONE))
          SEFF(ICUT,I) = Cumul_VAL1  
          
          SEFF(ICUT,I) = WET_RATIO(0) * Sel(I)   
          
          !-------OUTPUT------
          if(IBUG22_Swet==-1 .AND. INT22>0 )then              
              write (*,FMT='(A,1F30.16)')        "        Sum Wet Surf =",Cumul_VAL1
              write (*,FMT='(A,1F30.16)')        "          Total Surf =",Cumul_VAL2                                     
              write (*,FMT='(A,1F30.16)')        "               RATIO =",WET_RATIO(0)
              print *, ""     
          endif 
          
          IF(NumGrav==0)CYCLE
          
          P_Grav(1,ICUT,I) = P_Grav(1,ICUT,I) /NumGrav
          P_Grav(2,ICUT,I) = P_Grav(2,ICUT,I) /NumGrav
                    
          !---sWeighting factors using pressure load center
          !InterPolation using gravity centers of clipped polygons
          !NP_RECT(I) = 3 or 4

          !2d projected segment
          pt1(1:2)     = Ph(1:2,ICUT,ListP(1))
          pt2(1:2)     = Ph(1:2,ICUT,ListP(2))
          pt3(1:2)     = Ph(1:2,ICUT,ListP(3))
          pt4(1:2)     = Ph(1:2,ICUT,ListP(4))
          
C           write (*,FMT='(A          )')  "Segment    ="         
C           write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(1)) ," 0.0"
C           write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(2)) ," 0.0"
C           write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(3)) ," 0.0"
C           write (*,FMT='(A,2E30.16,A)')  "*createnode ", Ph(1:2,ICUT,ListP(4)) ," 0.0"                              

C           write (*,FMT='(A          )')  "Scut    ="  
C           do k=1,PolyScut%NumNodes-1       
C             write (*,FMT='(A,2E30.16,A)')"*createnode ", PolyScut%node(k,1:2) ," 0.0"
C           enddo

          
          !distance of each point from wet polygon gravity center
          distance(1)  = SQRT( (P_grav(1,ICUT,I)-Pt1(1))**2 + (P_grav(2,ICUT,I)-Pt1(2))**2 )
          distance(2)  = SQRT( (P_grav(1,ICUT,I)-Pt2(1))**2 + (P_grav(2,ICUT,I)-Pt2(2))**2 )
          distance(3)  = SQRT( (P_grav(1,ICUT,I)-Pt3(1))**2 + (P_grav(2,ICUT,I)-Pt3(2))**2 )
          distance(4)  = SQRT( (P_grav(1,ICUT,I)-Pt4(1))**2 + (P_grav(2,ICUT,I)-Pt4(2))**2 )

C           write (*,FMT='(A          )')  "Pgrav      ="         
C           write (*,FMT='(A,2E30.16,A)')  "*createnode ", P_grav(1:2,ICUT,I) ," 0.0"

          
          !segment edges  
          edge(1)      = SQRT ( (Pt1(1)-Pt2(1))**2 + (Pt1(2)-Pt2(2))**2 )
          edge(2)      = SQRT ( (Pt2(1)-Pt3(1))**2 + (Pt2(2)-Pt3(2))**2 )
          edge(3)      = SQRT ( (Pt3(1)-Pt4(1))**2 + (Pt3(2)-Pt4(2))**2 )
          edge(4)      = SQRT ( (Pt4(1)-Pt1(1))**2 + (Pt4(2)-Pt1(2))**2 )
          IF(NP_RECT(I)==3)THEN
            edge(3)      = SQRT ( (Pt3(1)-Pt1(1))**2 + (Pt3(2)-Pt1(2))**2 )
            edge(4)      = ZERO         
          ENDIF
          
          a            = edge(1)
          b            = distance(1)
          c            = distance(2)          
          s2           = HALF*(a+b+c)
          Area_tria(1) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) ) 
          
          a            = edge(2)
          b            = distance(2)
          c            = distance(3)          
          s2           = HALF*(a+b+c)
          Area_tria(2) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )  
          
          IF(NP_RECT(I)==4)THEN
            a            = edge(3)
            b            = distance(3)
            c            = distance(4)          
            s2           = HALF*(a+b+c)
            Area_tria(3) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )  

            a            = edge(4)
            b            = distance(4)
            c            = distance(1)          
            s2           = HALF*(a+b+c)
            Area_tria(4) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )                                                             
          ELSE
            a            = edge(3)
            b            = distance(3)
            c            = distance(1)          
            s2           = HALF*(a+b+c)
            Area_tria(3) = SQRT( ABS(s2*(s2-a)*(s2-b)*(s2-c)) )  

            Area_tria(4) = ZERO          
          ENDIF
                                         
          Dsum         = SUM(Area_tria(1:NP_RECT(I)))
          
          IF(Dsum==ZERO)then
            print *, "Warning: inter22 - nodal forces computation"
          endif
          
          IF(NP_RECT(I)==4)THEN
            delta(1,ICUT,I)   = HALF*(Area_tria(2)+Area_tria(3))/Dsum
            delta(2,ICUT,I)   = HALF*(Area_tria(3)+Area_tria(4))/Dsum
            delta(3,ICUT,I)   = HALF*(Area_tria(1)+Area_tria(4))/Dsum
            delta(4,ICUT,I)   = HALF*(Area_tria(1)+Area_tria(2))/Dsum  
            !uniform loading : same weight
            delta(1:4,ICUT,I) = FOURTH                            
          ELSE
            delta(1,ICUT,I)   = (Area_tria(2))/Dsum
            delta(2,ICUT,I)   = (Area_tria(3))/Dsum
            delta(3,ICUT,I)   = (Area_tria(1))/Dsum
            delta(4,ICUT,I)   = ZERO
            !uniform loading : same weight 
            delta(1:4,ICUT,I) = THIRD
            delta(4,ICUT,I)   = ZERO                                      
          ENDIF

          !write (*,FMT='(A,3E30.16)')  "CoG  = ", P_grav(1:2,ICUT,I)
          !write (*,FMT='(A,4E30.16)')  "dist = ", distance(1:4)

C           write (*,FMT='(A,4E30.16,A,1E30.16)')  "A_tria= ", Area_tria(1:4),"  sum=  ",SUM(Area_tria)
C           write (*,FMT='(A,4E30.16,A,I2,A,I6)')  "delta= ", delta(1:4,ICUT,I), "ICUT=", ICUT, " I=", I
          
        ENDDO!next ICUT
          
      ENDDO!next I (couple)
                     
C--------------------------------------------------------     
C COMMENTAIRES
C  -2- Dans le cas ou NBCUT > 1 (SCUT non connexe) il faut traiter les elements qui nont pas de projection directed mais sont dans la continuite de la surface.(celle ci a en general une partie connexe dans lelement voisin)
C      Solution : etendre SCUT par continuite en fermant la facette
C      (Inutile si taille de maille similaire entre lagrane et euler : example SMAES ciculaire)
C--------------------------------------------------------     
      
            
C
      RETURN
      END
C===============================================================================


      FUNCTION GetProjectedFaceType(P1,P2,P3,P4,I,ListP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real               :: P1(2),P2(2),P3(2),P4(2)
      my_real,intent(inout) :: I(2)      
      INTEGER,intent(inout) :: ListP(4)
      INTEGER               :: GetProjectedFaceType
C----------------------------------------------- 
C   L o c a l   A r g u m e n t s
C-----------------------------------------------
      my_real :: PA(2), PB(2)
C-----------------------------------------------
C   I n t e r f  a c e   B l o c k s
C----------------------------------------------- 
      INTERFACE
        FUNCTION intersectP(a,b,c,d,iflg)
          my_real, intent(inout) :: a(2),b(2),c(2),d(2)
          my_real                :: intersectP(2)
          integer,intent(in)     :: iflg
        END FUNCTION        
      END INTERFACE       
C-----------------------------------------------
C   S o u r c e   L i n e s
C----------------------------------------------- 
C
C
C         3          2                    3          4                       4          3
C          +---------+                     +---------+                       +----------+
C           \       /                       \       /                        |          |
C            \     /                         \     /                         |          |
C             \   /                           \   /                          |          |
C              \ /                             \ /                           |          |
C               + I                             + I                          |    + I   |
C              / \                             / \                           |          |
C             /   \                           /   \                          |          |
C            /     \                         /     \                         |          |
C           /       \                       /       \                        |          |
C          +---------+                     +---------+                       +----------+
C         1          4                    1          2                       1          2
C
C    GetProjectedFaceType = 2         GetProjectedFaceType = 1           GetProjectedFaceType = 0
C           PA /= EP30                       PA  = EP30                        PA  = EP30      
C           PB  = EP30                       PB /= EP30                        PB  = EP30      
C
C-----------------------------------------------
             
       PA(1:2)               = intersectP( P1, P2, P3, P4 ,0) ! [P1P2] & (P3P4)    
       PB(1:2)               = intersectP( P1, P4, P2, P3 ,0) ! [P1P4] & (P2P3)  
             
       IF    (PA(1)/=EP30 .AND. PB(1)==EP30)THEN
        GetProjectedFaceType = 2
        I(:)                 = PA(:)
        ListP(1:2)           = (/1,4/) !defining tiangles
        ListP(3:4)           = (/2,3/)        
       ELSEIF(PA(1)==EP30 .AND. PB(1)/=EP30)THEN
        GetProjectedFaceType = 1
        I(:)                 = PB(:) 
        ListP(1:2)           = (/1,2/) !defining tiangles
        ListP(3:4)           = (/3,4/)         
       ELSE
        GetProjectedFaceType = 0        
        I(1)                 = FOURTH*(P1(1)+P2(1)+P3(1)+P4(1))
        I(2)                 = FOURTH*(P1(2)+P2(2)+P3(2)+P4(2))
        ListP(1:4)           = (/1,2,3,4/)
       ENDIF
       
       
      END FUNCTION

