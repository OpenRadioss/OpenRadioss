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
Chd|  I22INTERSECT                  source/interfaces/int22/i22intersect.F
Chd|-- called by -----------
Chd|        I22BUCE                       source/interfaces/intsort/i22buce.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ISONSH3N                      source/interfaces/int22/i22intersect.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        REALLOC_MOD                   share/modules/realloc_mod.F   
Chd|====================================================================
      SUBROUTINE I22INTERSECT(
     1      X      ,II_STOK, CAND_B  ,CAND_E   ,ITASK,
     2      NBRIC  ,ITAB   , BUFBRIC  ,NCAND,   
     3      IXS    ,NIN )
C============================================================================    
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C  X(3,*) is used for local nodes (cells) for surface nodes use IRECTL  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE I22EDGE_MOD  
      USE REALLOC_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C=======================================================================
C 6   creation of edge list and addresses for each candidate cell
C================================================================     
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_B(NCAND),CAND_E(NCAND), NCAND, NIN, 
     .        ITASK, NBRIC, ITAB(*),
     .        BUFBRIC(NBRIC), IXS(NIXS,*), II_STOK
      my_real :: X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_NCN,NB_NCN1,NB_ECN,I,J,K,L,DIR,NB_NC,NB_EC,
     .        N1,N2,N3,N4,NN,NE,NCAND_PROV,J_STOK,II,JJ,TT,
     .        NSNF, NSNL,  TANGENT(12),
     .        PROV_B(2*MVSIZ), PROV_E(2*MVSIZ), LAST_NE,
     .        VOXBND(2*MVSIZ,0:1,1:3)                           !voxel bounds storage for shell: comp1=id,  comp2=lbound/ubound, comp3=direction.

      my_real
     .   DX,DY,DZ,XS,YS,ZS,XX,SX,SY,SZ,S2,
     .   XMIN, XMAX,YMIN, YMAX,ZMIN, ZMAX, TZ, GAPSMX, GAPL,
     .   D1X,D1Y,D1Z,D2X,D2Y,D2Z,DD1,DD2,D2,A2,GS, POINT(3),POINT2(3),d_1,d_2,d_3,
     .   ON1(3),N1N2(3)

      INTEGER  IX,IY,IZ,NEXT,M(8),
     .         IX1,IY1,IZ1,IX2,IY2,IZ2,IBUG,IBUG2,I_LOC,
     .         BIX1(NBRIC),BIY1(NBRIC),BIZ1(NBRIC),
     .         BIX2(NBRIC),BIY2(NBRIC),BIZ2(NBRIC),   
     .         FIRST_ADD, PREV_ADD, LCHAIN_ADD, I_STOK , idb_ID

      INTEGER,  DIMENSION(1)      :: SHELL_ADD  !address of surface face which is candidate in CAND_E (temp) 
                                                         !possible to decrease allocated size later
      INTEGER :: NC, I_STOK_BAK, IPA,IPB
      my_real
     .   XMINB,YMINB,ZMINB,XMAXB,YMAXB,ZMAXB,
     .   DXB,DYB,DZB,
     .   AAA, BasisCONST2,NS,
     .   POWER(8), CUTCOOR,CUTCOOR2,CUT(2),
     .   POW(2), OLD_CUTCOOR, OLD_CUTSHELL, CUTNODE(2)

      LOGICAL :: IsSH3N
     
      LOGICAL, DIMENSION(NBRIC) :: COUNT
!      LOGICAL, DIMENSION(12*NBRIC) :: LEDGE
      LOGICAL :: BOOL(NIRECT_L)
      INTEGER NBCUT, NBCUT2,DEJA, ISONSHELL, ISONSH3N
      INTEGER :: COUNTER, NEDGE, NFACE, NODES8(8), COUNTER_BRICK(NBRIC)
     
      INTEGER :: iN(2), iN1a, iN2a, iN1b, iN2b , iN3, iN4
      INTEGER :: POS, IAD, IADE, IB ,IBG    , NBF, NBL
      INTEGER :: I_12bits, nbits, npqts, pqts(4), SOM, SECTION
      INTEGER :: I_bits(12), MAX_ADD, IMIN_LOC, IMAX_LOC
      
      my_real :: AERADIAG, debugTab(24*NCAND,3)
      LOGICAL db_FLAG, TAGnode(8), debug_outp
     
      CHARACTER*12 :: sectype
      CHARACTER*12 ::filename
      LOGICAL      :: IsSecDouble
      
      CHARACTER(LEN=1) filenum
      
      INTEGER ::
     .        MIN_IX_LOC, MIN_IY_LOC, MIN_IZ_LOC,            !indice voxel min used
     .        MAX_IX_LOC, MAX_IY_LOC, MAX_IZ_LOC             !indice voxel max used      

      INTEGER :: ISHIFT, IDX

      my_real, dimension(:),     allocatable :: POWB

      INTEGER                        :: A(5), IE, N_CUT_EDGE
     
      INTEGER :: TAG_INDEX(NBRIC), I8(8,NBRIC),IFLG_DB
      my_real :: R8(8,NBRIC), DENOM, NORM, TOLCRIT,TOL
      
      TYPE(LIST_SECND) :: OLD_SECND_LIST
     
C----------------------------------------------------------------
      A(1:5)=((/1,2,3,4,1/))

      idb_ID = 0
                             

C--------------
C
C
C            60          61          62                    
C           +-----------+-----------+  
C          /|          /|          /|   
C         / |         / |         / |  
C        /  |        /  |        /  |   
C     50+---------51+---------52+   |   
C       |   |-------|---+-------|---+42  
C       |  /|       |  /|       |  /|    
C       | / |       | / |       | / | 
C       |/  |       |/  |       |/  |    
C     30+---------31+---------32+   |    
C       |   |-------|---+-------|---+ 
C       |  / 20     |  / 21     |  / 22    
C       | /         | /         | / 
C       |/          |/          |/     
C       +-----------+-----------+                                     
C     10          11          12   
C                    .     
C          
C-----------------------------------------------
     
      NB  = NCANDB
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NIN = 1
      
      cutcoor = -EP30

C=======================================================================
C 1   Allocation/INIT list of edges
C================================================================    

      !---------------------------------------------------------!
      ! DEBUG OUTPUT                                            !
      !---------------------------------------------------------!
      !INTERFACE 22 ONLY - OUTPUT---------------!
       debug_outp = .false.
       if(ibug22_intersect/=0)then
         if(ibug22_intersect>0)then
           do ib=nbf,nbl
             ie=BUFBRIC(LIST_B(Ib))
             if(ixs(11,ie)==ibug22_intersect)then
               debug_outp=.true.
               exit
             endif
           enddo
         elseif(ibug22_intersect==-1)then
           debug_outp = .true.
         endif
       endif
      if(itask==0.AND.debug_outp)then
        print *, ""
        print *, "================================="             
        print *, "====  BRICK INTERSECTIONS   ====="
        print *, "=================================" 
      endif

      NB  = NCANDB ! decreasing list of cells which are really intersected. Above we needed also uncut cells
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NIN = 1
      
      IF (ITASK==0)THEN
        ALLOCATE(vNE(NCANDE,3,4))
        ALLOCATE(BasisCONST(NCANDE,4))
        ALLOCATE(NbSubTriangles(NCANDE))
        ALLOCATE(diag22(NB))
        ALLOCATE(ptZ(NCANDE,3)) 
        ALLOCATE(vZA(NCANDE,3,4))
        ALLOCATE(vZB(NCANDE,3,4))
        ALLOCATE(ptA(NCANDE,3,4))
      END IF   
      CALL MY_BARRIER

!internal cell number       : BRICK_LIST(NIN,I)%ID
!local number in group      : LIST_B(I)


      !temp array (init)
      DO I=NBF, NBL ! 1,NB (threads)      
        BRICK_LIST(NIN,I)%ID = BUFBRIC(LIST_B(I))  ! internal cell id
        d_1 = zero
        d_2 = zero
        d_3 = zero
        DO J=1,12
          K=(I-1)*12+J
          EDGE_LIST(NIN,K)%NODE(1:2)    = IXS(INT22_BUF%iEDGE(1:2,J)+1,BRICK_LIST(NIN,I)%ID)                        ! ids cell nodes
          EDGE_LIST(NIN,K)%NBCUT        = 0
          EDGE_LIST(NIN,K)%CUTSHELL     = 0
          EDGE_LIST(NIN,K)%CUTCOOR(1:2) = ZERO
          EDGE_LIST(NIN,K)%VECTOR(1:3)  = X(1:3,EDGE_LIST(NIN,K)%NODE(2))-X(1:3,EDGE_LIST(NIN,K)%NODE(1)) ! vector (iN1->iN2) to be store to compute intersections
          d_1 = max(d_1, ABS(EDGE_LIST(NIN,K)%VECTOR(1)))
          d_2 = max(d_2, ABS(EDGE_LIST(NIN,K)%VECTOR(2)))
          d_3 = max(d_3, ABS(EDGE_LIST(NIN,K)%VECTOR(3)))
        END DO  
          diag22(I) = MAX ( d_1,d_2,d_3 )  
      END DO  
      CALL MY_BARRIER !waiting for the list to be created before going on

      if(itask==0 .AND. debug_outp)print *, ""


   
C=======================================================================
C 2   a.Normal & basis point
C       Storage in vNE(1:NCANDE,1:3,TT)
C                     -->   1st arg : candidate Face (structure)
C                     -->   2nd arg : components x,y,z
C                     -->   3rd arg : normal number (1shell=4tria)
C     b.plane equation (constant value)
C                      BasisCONST(TT)
C                     -->   1st arg : normal number (1shell=4tria)
C================================================================        
      NBF = 1+ITASK*NCANDE/NTHREAD
      NBL = (ITASK+1)*NCANDE/NTHREAD
            
      DO I=NBF,NBL!1,NCANDE
        NE = LIST_E(I)                                                !shell number  IRECT(1:4,*)
        M(3:4)=IRECT_L(3:4,NE)
        IF(M(3)==M(4))THEN                                            !type : sh4n or sh3n
          IsSH3N=.true.
          NbSubTriangles(I)=1
        ELSE
          IsSH3N=.false.   
          NbSubTriangles(I)=4 ; 
        END IF       
        IF(.NOT.IsSH3N)THEN                                       !centre
          !ptZ(I,1:3)=(/(FOURTH*SUM(  IRECT_L( 4*J+(/1:4/),NE) ),J=1,3)/)  ! Z = mean X(5:8), mean Y(9:12), mean Z(13:16)
          ptZ(I,1)=FOURTH*SUM( IRECT_L( 05:08,NE) )
          ptZ(I,2)=FOURTH*SUM( IRECT_L( 09:12,NE) )
          ptZ(I,3)=FOURTH*SUM( IRECT_L( 13:16,NE) )
        ELSE
          !ptZ(I,1:3)=IRECT_L((/8,12,16/),NE)  !M(4)
          ptZ(I,1)=IRECT_L(08,NE)  !M(4)
          ptZ(I,2)=IRECT_L(12,NE)  !M(4)
          ptZ(I,3)=IRECT_L(16,NE)  !M(4)
        END IF        
        DO TT=1,NbSubTriangles(I)                                    !composing triangle (Z,IPA,IPB) = (Z,1,2) ou (Z,2,3) ou (Z,3,4) ou (Z,4,1)
          IPA=A(TT)                                                  ! Z is centroid of 4node-shell.
          IPB=A(TT+1) !; IF(IPB==5) IPB=1,for cyclic permutation
          !IPA=IRECT_L(IPA,NE)
          !sIPB=IRECT_L(IPB,NE)
          ptA(I,1:3,TT) = IRECT_L((/4,8,12/)+IPA,NE)                  !A coordinates (for intersection computation ISONSH3N)    <=> X(1:3,IPA)
          vZA(I,1:3,TT) = IRECT_L((/4,8,12/)+IPA,NE)-ptZ(I,1:3)       !vector ZA et ZB for each triangle.
          vZB(I,1:3,TT) = IRECT_L((/4,8,12/)+IPB,NE)-ptZ(I,1:3)       !vector ZA et ZB for each triangle.         
          vNE(I,1:3,TT) = vZA(I,(/2,3,1/),TT)*vZB(I,(/3,1,2/),TT) -   ! vNE = vZA (x) vZB
     .                    vZA(I,(/3,1,2/),TT)*vZB(I,(/2,3,1/),TT)
          !normalizing the normal vector to normalize the cartesian plane equation
          NORM = vNE(I,1,TT)*vNE(I,1,TT)+vNE(I,2,TT)*vNE(I,2,TT)+vNE(I,3,TT)*vNE(I,3,TT)
          NORM = SQRT(NORM)
          IF(NORM/=ZERO)THEN
          vNE(I,1,TT) = vNE(I,1,TT) / NORM
          vNE(I,2,TT) = vNE(I,2,TT) / NORM
          vNE(I,3,TT) = vNE(I,3,TT) / NORM
          ENDIF
          BasisCONST(I,TT) = SUM(ptZ(i,1:3)*vNE(I,1:3,TT))            !basis constant (surface equation)
                                                                      !basis point is Zentrum, it is common for each triangle
          !PLANE EQUATION : nx.X + nY.Y + nZ.Z + k = 0
          ! where n is vNE
          ! k such as ptZ satisfying equation => k = -nx.Zx - ny.Zy - nz.Zz   is BasisCONST
        END DO !next TT
      END DO !next I

      CALL MY_BARRIER !wait for complete initialization 

C=======================================================================
C 3   Calcul Intersections
C================================================================    
      if(itask==0 .AND. debug_outp)then
          print *,"  Calcul des Intersections sur Proc=", ITASK+1
      endif

 !to be optimized : loop over cells from list_b (need to get corresponding addresses)
      NBF = 1+ITASK*NCANDB/NTHREAD
      NBL = (ITASK+1)*NCANDB/NTHREAD

      !NCANDB bricks are present in candidates. These NCANDB bricks are multi-threaded. Each thread will handle ( (/CAND_B(IADF(I):IADL(I)) , CAND_B(IADF(I):IADL(I))/), I=NBF,NBL)

      DO I=NBF,NBL !each thread handle its bricks (no duplicate work)
        BRICK_LIST(NIN,I)%Seg_add_LFT = IADF(I)    ! corresponding window in CAND_B(i)  : CAND_B(IADFL(I):IADL(I)) = I
        BRICK_LIST(NIN,I)%Seg_add_LLT = IADL(I) 
	CAND_B(IADF(I):IADL(I)) = I        !TRANSFORM CAND_B(I) into ADD into brick_list()
c        print *, "IB           =", I
c        print *, "....IADF,IADL=", IADF(I), IADL(I)   
        IB  = LIST_B(I)            !local ID in group IBUFSSG(IAD+(/1:NBRIC/))
        IBG = BUFBRIC(IB)          !global ID
        TAGnode=.false.
            TANGENT(1:12) = 0 !detect tangent edge : keep them uncut, its adjacent connected edges will be cut

              if (ixs(11,brick_list(nin,i)%id)==idb_ID)then
                print *, "idb_ID====="
                print *, "CAND_E =", CAND_E(IADF(I):IADL(I))
              endif
             ! if (ixs(11,brick_list(nin,i)%id)==0)then
             !   print *, "481====="
             !   print *, "CAND_E =", CAND_E(IADF(I):IADL(I))
             ! endif

        DO IAD=IADF(I), IADL(I)
          IE    = CAND_E(IAD)
          IF(IE<=0)CYCLE
          IADE  = GET_LIST_E_POS_FROM_CAND_E_POS(IAD)
C          !non intersection criterion (couple is retained anyway in CAND_B/CAND_E because we may not search/sort on each cycle)
C          IF((XMAXS(IB)<IRECT_L(17,IE)).OR. ! ignore if intersection with cell (without margin)is null, IRECT_L <=> xmine,xmaxe,...zmine,zmaxe
C     .    (XMINS(IB)>IRECT_L(20,IE)).OR.
C     .    (YMAXS(IB)<IRECT_L(18,IE)).OR.
C     .    (YMINS(IB)>IRECT_L(21,IE)).OR.
C     .    (ZMAXS(IB)<IRECT_L(19,IE)).OR.
C     .    (ZMINS(IB)>IRECT_L(22,IE)) ) THEN
C!            CAND_E(IAD) = 0
C            CYCLE !next couple candidat
C          END IF
          DO TT=1,NbSubTriangles(IADE)                                  !shell decomposition to get rid of warping errors
            POWER(1:8)=(/(SUM(vNE(IADE,1:3,TT) * X(1:3,IXS(II,IBG)))- BasisCONST(IADE,TT),II=2,9)/)    !power of nodes for plane generated by current triangle : POW = distance = origin ordinate in working system
            N_CUT_EDGE    = 0
            DO J=1,12                                                   !12 edges !opposite sign for nodes power ?
              K       = (I-1)*12+J                                      !k : address number in EDGE_LIST
              POW(1:2)= POWER(INT22_BUF%iEDGE(1:2, J))
              iN(1:2) = EDGE_LIST(NIN,K)%NODE(1:2)                      !IXS(1+iEDGE(1, J),NS)
              DEJA    = EDGE_LIST(NIN,K)%NBCUT                                 !is edge already cut by another shell ?
              NBCUT   = -1
              NBCUT2  = -1  
              
              if(ixs(11,brick_list(nin,i)%id)==idb_ID)then
                print *, "J=", J, ITAB(iN(1:2))
                write(*,FMT='(A,4I20)')    "shell N1-N2-N3 :",INT(IRECT_L(01:04,IE))
                write(*,FMT='(A,3F20.12)') "      shell N1 :",IRECT_L( (/05,09,13/),IE)
                write(*,FMT='(A,3F20.12)') "      shell N2 :",IRECT_L( (/06,10,14/),IE)
                write(*,FMT='(A,3F20.12)') "      shell N3 :",IRECT_L( (/07,11,15/),IE) 
                write(*,FMT='(A,2F40.20)')"        POW(1:2)=", POW(1:2)
                print *,""
              endif
                                                          !(2 intersection points : if edge is on the plane)
              if (ixs(11,brick_list(nin,i)%id)==idb_ID )then
                print *, "idb_ID====="
                write(*,FMT='(A,4I20)')    "shell N1-N2-N3-N4 :",INT(IRECT_L(01:04,IE))
                print *, "IE =", IE
                print *, "TT =", TT
                print *, "J  =", J
                print *, "POW1,POW2", POW(1:2)
              endif
              !if (ixs(11,brick_list(nin,i)%id)==5882 .AND. J==12)then
              !  print *, "5882====="
              !  write(*,FMT='(A,4I20)')    "shell N1-N2-N3-N4 :",INT(IRECT_L(01:04,IE))
              !  print *, "IE =", IE
              !  print *, "TT =", TT
              !  print *, "J  =", J
              !  print *, "POW1,POW2", POW(1:2)
              !endif
              
              TOLCRIT = EM06
              TOL = (ONE+EM04)*TOLCRIT*diag22(I)  !EM04 in ISONSH3N
              ! POWER is in mm corresponds to the shifted distance of the intercept (ordonnee a lorigine) it must be normalized
              IF((ABS(POW(1))<=TOL).AND.(ABS(POW(2))<=TOL))THEN         !edge is on the shell then no intersection with the face
                TANGENT(J) = 1
                !print *, "TANGENT=1",ixs(11,brick_list(nin,i)%id)
                cycle
              ENDIF                
              IF(DEJA==2)CYCLE                                          !hypothesis: 2 intersections max. Warning possible dpedency of elem numbering
              IF(  ((POW(1)<-TOL).AND.(POW(2)>TOL)) .OR.((POW(1)>TOL).AND.(POW(2)<-TOL)) )THEN    !intersection with current shell     
                ON1(1:3) = X(1:3,iN(1))
                N1N2(1:3)= EDGE_LIST(NIN,K)%VECTOR(1:3)
                DENOM    = SUM( vNE(IADE,1:3,TT) * N1N2(1:3) )
                IF(ABS(DENOM)>EM12)THEN
                CUTCOOR = ( BasisCONST(IADE,TT) - SUM( vNE(IADE,1:3,TT) * ON1(1:3) )    ) / DENOM                     !N.N1 change all 3 cycles (possible optimization, cf definition of iedge)        
                ELSE
                  !in this case edge is intersected with a plane which is quasi tangent. possibly infinite solutions. We choose cutcoor=0.5, error related to this description is neglictible
                  CUTCOOR = HALF
                  CALL ARRET(2)
                ENDIF    
                
                IF((CUTCOOR<=ONE+TOL).AND.(CUTCOOR>=-TOL))THEN           !check if intersection point is one on the shell
                  CUTCOOR = MIN(ONE-EM06,CUTCOOR)
                  CUTCOOR = MAX(EM06   ,CUTCOOR)
                  POINT(1:3)=ON1(1:3) + CUTCOOR * N1N2(1:3)
                ELSE
                  print *, " CUTCOOR =", CUTCOOR
                  CALL ARRET(2)
                END IF
                
                NBCUT2 = 0
              ELSEIF((ABS(POW(1))<=TOL).AND.(ABS(POW(2))<=TOL))THEN         !edge is on the shell then no intersection with the face
              !DON'T CUT EDGE WHICH IS LAYING ON THE CUT PLANE. ITS ADJACENT EDGES WILL BE CUT.
                ! specific case : edge is on the structural face
                !IF(J==1 .OR. J==5 .OR. J==8 .OR. J==12)THEN
                !  ON1(1:3) = X(1:3,iN(1))
                !  N1N2(1:3)= EDGE_LIST(NIN,K)%VECTOR(1:3)
                !  CUTCOOR     = ONE-EM06                                      !intserection on edge nodes1,5,8 ou 12 (necessary case to get a corresponding topology without risking to loose the lagrangian surface while it moves)
                !  CUTCOOR2    = EM06
                
                
                !ELSE
                  !-!ON1(1:3) = X(1:3,iN(1))
                  !-!N1N2(1:3)= EDGE_LIST(NIN,K)%VECTOR(1:3)
                  !-!CUTCOOR     = HALF                       
                  !-!POINT(1:3)  = ON1(1:3) + CUTCOOR  * N1N2(1:3)
                  !-!NBCUT2 = 0                  
                !ENDIF
                TANGENT(J) = 1
              ELSEIF (ABS(POW(1))<=TOL)THEN                               !intersection with summit #1
              !SET CUTCOOR = EM06 TO HAVE A EPSILON CUT SURFACE. OTHERWISE CRITERIA FOR AMBIGUS COMBINATION WILL GET A DIVISION BY 0 (double penta check in i22subvol)
                !IF(J==1 .OR. J==5 .OR. J==8 .OR. J==12)THEN
                  ON1(1:3) = X(1:3,iN(1))
                  N1N2(1:3)= EDGE_LIST(NIN,K)%VECTOR(1:3)
                  CUTCOOR     = EM06                                     !intserection on edge nodes 1,5,8 ou 12 (necessary case to get a corresponding topology without risking to loose the lagrangian surface while it moves)
                  POINT(1:3)  = ON1(1:3) + ZERO * N1N2(1:3)              
                  NBCUT2 = 0             
                !ELSE
                !  NBCUT  = 0
                !  NBCUT2 = 0                  
                !ENDIF
!                IF(TAGnode(iEDGE(1,J))==.FALSE.)THEN
!                  POINT(1:3)=X(1:3,iN(1))
!                  CUTCOOR=ZERO
!                  TAGnode(iEDGE(1,J))=.TRUE.
!                ELSE
!                  NBCUT  = 0
!                ENDIF
!                NBCUT2 = 0                  
              ELSEIF (ABS(POW(2))<=TOL)THEN                               !intersection with summit #2
              !SET CUTCOOR = ONE-EM06 TO HAVE A EPSILON CUT SURFACE. OTHERWISE CRITERIA FOR AMBIGUS COMBINATION WILL GET A DIVISION BY 0 (double penta check in i22subvol)
                !IF(J==1 .OR. J==5 .OR. J==8 .OR. J==12)THEN
                  ON1(1:3) = X(1:3,iN(1))
                  N1N2(1:3)= EDGE_LIST(NIN,K)%VECTOR(1:3)
                  CUTCOOR     = ONE-EM06                                     !intserection with edge nodes 1,5,8 ou 12 (necessary case to get a corresponding topology without risking to loose the lagrangian surface while it moves)
                  POINT(1:3)  = ON1(1:3) + ONE * N1N2(1:3)              
                  NBCUT2 = 0             
                !ELSE
                !  NBCUT  = 0
                !  NBCUT2 = 0                  
                !ENDIF                  
!                IF(TAGnode(iEDGE(2,J))==.FALSE.)THEN
!                  POINT(1:3)=X(1:3,iN(2))
!                  CUTCOOR=ONE
!                  TAGnode(iEDGE(2,J))=.TRUE.
!                ELSE
!                  NBCUT = 0               
!                ENDIF
!                NBCUT2 = 0
              ELSE                                                      !2 nodes of the edges are on the same side of the intersection plane
                NBCUT  = 0
                NBCUT2 = 0
              END IF   
              
              if (ixs(11,brick_list(nin,i)%id)==idb_ID )then
                print *, "cutcoor=", cutcoor
                IFLG_DB=1
              else
                IFLG_DB=0
              endif
               
             IF(NBCUT==-1) NBCUT =ISONSH3N( ptZ(IADE,1:3),ptA(IADE,1:3,TT),vZA(IADE,1:3,TT),vZB(IADE,1:3,TT),POINT(1:3) ,IFLG_DB)                                                           ! verifie si POINT(:) appartient au triangle           
             IF(NBCUT2==-1)NBCUT2=ISONSH3N( ptZ(IADE,1:3),ptA(IADE,1:3,TT),vZA(IADE,1:3,TT),vZB(IADE,1:3,TT),POINT2(1:3),IFLG_DB)                                                             ! verifie si POINT(:) appartient au triangle
             
              if (ixs(11,brick_list(nin,i)%id)==idb_ID )then
                print *, "NBCUT, NBCUT2=", NBCUT,NBCUT2
              endif                      

              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              !If intersection with infinite plane, we calculate coordinates on the edge : COOR = N1+ t N1N2
              ! if t is not within [0,1] we are not on the segment but on the open entity (N1N2)\[N1N2]
              IF(NBCUT>0) THEN !if we detect intersection on current shell          
                IF(DEJA==0)THEN !edge is not yet cut : first intersection detected
                  !print *, "nouveau point", CUTCOOR                
                  NBCUT=1
                  EDGE_LIST(NIN,K)%CUTCOOR(1)  = CUTCOOR          ! storage
                  EDGE_LIST(NIN,K)%CUTSHELL(1) = IE
                ELSEIF (DEJA==1)THEN                              ! already cut => 2nd intersection
                  OLD_CUTCOOR  = EDGE_LIST(NIN,K)%CUTCOOR(1)      ! first store, the ordering the 2 intersections
                  OLD_CUTSHELL = EDGE_LIST(NIN,K)%CUTSHELL(1)
                  IF (ABS(CUTCOOR-OLD_CUTCOOR)>EM6) THEN
                    NBCUT=2
                    IF(CUTCOOR>OLD_CUTCOOR)THEN
                      EDGE_LIST(NIN,K)%CUTCOOR(1)  = OLD_CUTCOOR
                      EDGE_LIST(NIN,K)%CUTCOOR(2)  = CUTCOOR
                      EDGE_LIST(NIN,K)%CUTSHELL(1) = OLD_CUTSHELL
                      EDGE_LIST(NIN,K)%CUTSHELL(2) = IE
                      !print *, "second points point",OLD_CUTCOOR,CUTCOOR
                    ELSEIF(CUTCOOR<OLD_CUTCOOR)THEN
                      EDGE_LIST(NIN,K)%CUTCOOR(1)  = CUTCOOR
                      EDGE_LIST(NIN,K)%CUTCOOR(2)  = OLD_CUTCOOR
                      EDGE_LIST(NIN,K)%CUTSHELL(1) = IE
                      EDGE_LIST(NIN,K)%CUTSHELL(2) = OLD_CUTSHELL
                      !print *, "second points point",CUTCOOR,OLD_CUTCOOR
                    ELSE
                      !print *, "point deja trouve", CUTCOOR
                      NBCUT=1 ! le point est le meme, inutile de l'enregistrer.
                    END IF
                  END IF              
                ELSEIF(DEJA==2)THEN ! already 2 intersections, no more expected (hypothesis)
                  if(itask==0 .AND. debug_outp)then
                    if(ibug22_intersect==-1 .or. ibug22_intersect==ixs(11,brick_list(nin,i)%id))then
                      print *, "THREE INTERSECTION SUR UNE ARRETE - STOP"
                      CALL ARRET(2)
                    endif
                  endif
                END IF
                EDGE_LIST(NIN,K)%NBCUT = NBCUT 
                EDGE_LIST(NIN,K)%LEN   = SQRT(N1N2(1)*N1N2(1)+N1N2(2)*N1N2(2)+N1N2(3)*N1N2(3)) 
              END IF !NBCUT>0
              
              IF(NBCUT2>0  .AND. DEJA==0)THEN !if already intersected
                  NBCUT=2
                  EDGE_LIST(NIN,K)%CUTCOOR(1)=ZERO ! storage
                  EDGE_LIST(NIN,K)%CUTCOOR(2)=ONE   ! storage              
                  if(itask==0 .AND. debug_outp)then
                    if(ibug22_intersect==-1 .or. ibug22_intersect==ixs(11,brick_list(nin,i)%id))then
                      print *, "edge fully on intersection plane",J
                    endif
                  endif
                  EDGE_LIST(NIN,K)%CUTSHELL(1) = IE                
                  EDGE_LIST(NIN,K)%CUTSHELL(2) = IE 
                  EDGE_LIST(NIN,K)%NBCUT       = 2                  
              ENDIF

            END DO !next J=1,12 // next edge
          END DO !next TT=1,NbSubTriangles // next sub triangles
        END DO !next IAD=IADF(I),IADL(I)
        
          if (ixs(11,brick_list(nin,i)%id)==idb_ID )print *, "TANGENT 1-12=", TANGENT(1:12)
        
        DO J=1,12
          IF(TANGENT(J)==1)THEN
            K =(I-1)*12+J 
            NBCUT = EDGE_LIST(NIN,K)%NBCUT 
            !IF(NBCUT==0)THEN              
            ! EDGE_LIST(NIN,K)%CUTSHELL(1) = 0
            ! EDGE_LIST(NIN,K)%CUTSHELL(2) = 0
            ! EDGE_LIST(NIN,K)%NBCUT       = 0
            ! EDGE_LIST(NIN,K)%CUTCOOR(1)  = 0
            ! EDGE_LIST(NIN,K)%CUTCOOR(2)  = 0
            !ELSEIF(NBCUT==1)THEN
            !
            !        +-------------+                               +-------------+
            !        |             |                               |             |
            !        |             |                               |             |
            !        | o-----------------1----o          dble  ->  O             O
            !        | |           |                               |             |
            !        | |           |                               |             |
            !        +-|-----------+                               +-O-----------+
            !          |                                         
            !          o                                         
            !  face1 cuts edge within tolerance, intersection point must be doubled to get PENTA  HEXA, otherwise, max(secID) = HEXA
            ! 
            !cleaning if cutcoor = EM06 ou CUTCOOR = ONE-EM06   (CASE : lagrangian face == euler face => HEXA cutting)
             IF(NBCUT>=1)THEN
              CUTCOOR = EDGE_LIST(NIN,K)%CUTCOOR(1)
              IF(CUTCOOR==EM06 .OR. CUTCOOR==ONE-EM06)THEN
                EDGE_LIST(NIN,K)%CUTSHELL(1)   = EDGE_LIST(NIN,K)%CUTSHELL(2)
                EDGE_LIST(NIN,K)%NBCUT         = MAX(0,EDGE_LIST(NIN,K)%NBCUT-1)
                EDGE_LIST(NIN,K)%CUTCOOR(1)    = EDGE_LIST(NIN,K)%CUTCOOR(2) 
                NBCUT                          = EDGE_LIST(NIN,K)%NBCUT 
                IF(NBCUT==1)THEN
                  EDGE_LIST(NIN,K)%CUTSHELL(2) = 0
                  EDGE_LIST(NIN,K)%CUTCOOR(2)  = ZERO                           
                ENDIF              
              ENDIF 
             ENDIF
             !duplicated nodes
             NBCUT                             = EDGE_LIST(NIN,K)%NBCUT 
             CUTCOOR                           =  EDGE_LIST(NIN,K)%CUTCOOR(1)
             IF(NBCUT==1 .AND. CUTCOOR>EM06 .AND. CUTCOOR<ONE-EM06)THEN
               EDGE_LIST(NIN,K)%CUTSHELL(2) = EDGE_LIST(NIN,K)%CUTSHELL(1)
               EDGE_LIST(NIN,K)%NBCUT       = 2
               EDGE_LIST(NIN,K)%CUTCOOR(2)  = EDGE_LIST(NIN,K)%CUTCOOR(1)               
             ENDIF

            !ENDIF
          ENDIF
        ENDDO
        
      END DO !next I=NBF,NBL

C=======================================================================
C 4   Calcalculation of intersection points
C      + HM script (*createnode) to visualize them with HM
C================================================================  

      CALL MY_BARRIER !wait for all nodes to be computed

      if(ITASK==0 .AND. IBUG22_OUTP_IntPoint==1)THEN
      
        if(debug_outp)then
            print *, "  ===== intersection_nodes.txt ======="  
        endif
      IPA = 100+ISPMD 
      filename(1:12) =  "cut_nod0.txt"
      write(filename(8:8),'(i1.1)')ISPMD+1
    !===script HM=====!
      open( unit=IPA, file = filename(1:12) ) 
   
      !===script HM=====!      
      SOM=0      
      DO I=1,NB
       !===script HM=====!                                                                                               
       if(ibug22_intersect==ixs(11,brick_list(nin,i)%id) .or. ibug22_intersect==-1 .or. IBUG22_OUTP_IntPoint == 1)then   
         write (unit=IPA,fmt='(A,I10)') "cell ID = ", ixs(11,brick_list(nin,i)%id)                                     
         write (*       ,fmt='(A,I10)') "cell ID = ", ixs(11,brick_list(nin,i)%id)                                     
       endif                                                                                                             
        DO J=1,12
        IAD          = (I-1)*12+J
        NBCUT        = EDGE_LIST(NIN,IAD)%NBCUT
        CUT(1:2)     = EDGE_LIST(NIN,IAD)%CUTCOOR(1:2)
        iN(1:2)      = EDGE_LIST(NIN,IAD)%NODE(1:2)          
        N1N2(1:3)    = EDGE_LIST(NIN,IAD)%VECTOR(1:3)
        ON1(1:3)     = X(1:3,iN(1)) 
          DO K=1,NBCUT
            !intersection coordinates
            POINT(1:3)= ON1(1:3) + CUT(K) * N1N2(1:3)
            ! EDGE_LIST(NIN,IAD)%CUTPOINT(1:3) = POINT(1:3)              
           !===script HM=====!
           if(ibug22_intersect==ixs(11,brick_list(nin,i)%id) .or. ibug22_intersect==-1 .or. IBUG22_OUTP_IntPoint == 1)then
             write(unit=IPA, 
     .         fmt='(A12,F20.14,A1,F20.14,A1,F20.14,A13)')"*createnode ",POINT(1) ," ", POINT(2)," ",POINT(3),"  0   0   0  " 
             write(*, 
     .         fmt='(A12,F20.14,A1,F20.14,A1,F20.14,A13)')"*createnode ",POINT(1) ," ", POINT(2)," ",POINT(3),"  0   0   0  "        
           endif
           !===script HM=====!
   
          db_FLAG=.true.
          if (som>0)then
            do L=1,SOM
              if(SUM(ABS(point(1:3)-debugTab(L,1:3)))<EM06)
     .                               db_FLAG=.false.
            end do
          end if
          if(db_FLAG)then
            som=som+1 !debug
            debugTab(som,1:3)    =point(1:3)
          end if

          END DO ! (DO K=1,NBCUT <=> NBCUT>0)
        END DO !next J=1,12
      END DO! next I=1,NB      
      !===script HM=====!      
      CLOSE(IPA)    
      !===script HM=====!
      end if !ITASK==0

!      if(debug_outp)then
!          print *, ""
!          print *, "  |--------i22intersect.F---------|"
!          print *, "  |    WRITE :  SCRIPT HM NODES   |"
!          print *, "  |-------------------------------|"
!          print *, SOM , "nodes written" 
!          do L=1,SOM
!            print *, debugTAB(L,1:3)
!          end do
!          print *, ""
!      endif

C=======================================================================
C 5   ***
C================================================================ 

      if(debug_outp)then
        if(itask==0)then
          print *, ""
          print *, "  |--------i22intersect.F---------|"
          print *, "  |           EDGES               |"
          print *, "  |-------------------------------|"
          print *, 12*NB , "  edges (12*NBRIC)" 
        DO I=1, NB ! 1,NB fractionne sur les differents threads 
          if(ibug22_intersect/=-1 .and. ibug22_intersect/=ixs(11,brick_list(nin,i)%id))cycle           
          print *, "  ** CELL **", IXS(11,BRICK_LIST(NIN,I)%ID)
          DO J=1,12
            K=(I-1)*12+J
            IF( EDGE_LIST(NIN,K)%NBCUT==0)THEN
              WRITE(*,FMT='(A10,I10,A1,I12,I12,A8)') "   edge ",K,":",
     .          ITAB(EDGE_LIST(NIN,K)%NODE(1)),
     .          ITAB(EDGE_LIST(NIN,K)%NODE(2)),"        "   
            ELSEIF( EDGE_LIST(NIN,K)%NBCUT==1)THEN
              WRITE(*,FMT='(A10,I10,A1,I12,I12,A8,1F30.16)') "   edge ",K,":",
     .          ITAB(EDGE_LIST(NIN,K)%NODE(1)),
     .          ITAB(EDGE_LIST(NIN,K)%NODE(2)),"  CUTTED   :" ,EDGE_LIST(NIN,K)%CUTCOOR(1)
            ELSE
              WRITE(*,FMT='(A10,I10,A1,I12,I12,A8,2F30.16)') "   edge ",K,":",
     .          ITAB(EDGE_LIST(NIN,K)%NODE(1)),
     .          ITAB(EDGE_LIST(NIN,K)%NODE(2))," 2CUTTED   :" ,EDGE_LIST(NIN,K)%CUTCOOR(1:2)
            END IF
          END DO  
        END DO  
        end if
      end if


C=======================================================================
C 5   DEALLOCATE
C================================================================ 

      IF (ITASK==0)THEN
        DEALLOCATE(vNE)
        DEALLOCATE(BasisCONST)
        DEALLOCATE(NbSubTriangles)
        DEALLOCATE(ptZ) 
        DEALLOCATE(vZA)
        DEALLOCATE(vZB)
        DEALLOCATE(ptA)
        DEALLOCATE(diag22)
      END IF 

      

      RETURN
      END






Chd|====================================================================
Chd|  ISONSH3N                      source/interfaces/int22/i22intersect.F
Chd|-- called by -----------
Chd|        I22INTERSECT                  source/interfaces/int22/i22intersect.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION ISONSH3N(Z, A, ZA, ZB, P, IFLG_DB)
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
! Precondition : Point P is on the infinite plane generated by 3-node-shell (Z,A,B)
C-----------------------------------------------
C   D e s c r i p t  i o n
C-----------------------------------------------
!Is point in interior or exterior ?
!                   1 :interior
!                   0 :exterior
!
!             Z+-----+B     CRITERIA :
!              | P  /       ----------
!              | + /          (ZA^ZP).(ZP^ZB) & (AZ^AP).(AP^AB) > 0
!              |  /     <=> / (ZA^U ).( U^ZB) & (AZ^V ).( V^AB) > 0
!              | /          | U=ZP
!              |/           \ V=AP
!             A+
!
! CRITERION
!     coordinates of P in frame (ZA,ZB) must be
!        -TOL < x < 1+ TOL
!        -TOL < y < 1+ TOL
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real , intent(in) ::
     .                          Z(3), A(3), P(3)                        !points
        my_real , intent(in) ::
     .                          ZA(3),ZB(3)                             !vectors
        INTEGER , intent(in) :: IFLG_DB
C-----------------------------------------------
C   L o c a l   V a r i a  b l e s
C-----------------------------------------------
        my_real
     .                          U(3),V(3),AB(3),PV1(3),PV2(3),PS1,PS2,PS3,U2(3),ZP(3),AP(3),
     .                          PZ(3),PA(3),PB(3),
     .                          NORM,  K , NORM1, NORM2, eps1,eps2, Lref1,Lref2 , TOL,
     .                          NORM_PZ,NORM_PA,NORM_PB,NORM_ZA_2,NORM_ZB_2,SIN1,SIN2,TOL_SIN,NORM_AB,
     .                          PAB, PBZ, PAZ, ABZ,
     .                          COOR1, COOR2, COOR3,COEF,
     .                          t, s, TOLCRIT
C-----------------------------------------------
C   S o u r c e    L i n e s
C-----------------------------------------------
      ISONSH3N = 0 

      TOLCRIT = EM06
      TOL = TOLCRIT

      NORM_ZA_2 = (ZA(1)*ZA(1)+ZA(2)*ZA(2)+ZA(3)*ZA(3))
      NORM_ZB_2 = (ZB(1)*ZB(1)+ZB(2)*ZB(2)+ZB(3)*ZB(3))

      ZP(1) = P(1) - Z(1)
      ZP(2) = P(2) - Z(2)
      ZP(3) = P(3) - Z(3)      

      PS1 =  ZA(1)*ZP(1)+ZA(2)*ZP(2)+ZA(3)*ZP(3)
      
      PS2 =  ZB(1)*ZP(1)+ZB(2)*ZP(2)+ZB(3)*ZP(3)

      PS3 =  ZB(1)*ZA(1)+ZB(2)*ZA(2)+ZB(3)*ZA(3)
      
      COEF = ONE-PS3*PS3/NORM_ZA_2/NORM_ZB_2
      COEF = ONE / COEF
      
      t =  COEF * ( PS2/NORM_ZB_2 - PS3*PS1/NORM_ZA_2/NORM_ZB_2)
      s =  COEF * ( -PS3*PS2/NORM_ZA_2/NORM_ZB_2 + PS1/NORM_ZA_2)
  
      IF(IFLG_DB == 1)THEN
        print *, "coor ZA,ZB =", t,s
        write(*,fmt='(A12,3F30.16)')"*createnode ",Z(1:3)
        write(*,fmt='(A12,3F30.16)')"*createnode ",A(1:3)
        write(*,fmt='(A12,3F30.16)')"*createnode ",Z(1:3)+ZB(1:3)
        write(*,fmt='(A12,3F30.16)')"*createnode ",P(1:3)
      ENDIF
  
      IF(t>=-TOL)THEN
        IF(s>=-TOL )THEN
          IF(s+t<=ONE+TOL)THEN
            ISONSH3N = 1
          ENDIF
        ENDIF       
      ENDIF      

      RETURN

      END FUNCTION

